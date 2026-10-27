from __future__ import annotations

import codecs
import os
import re
import termios
from dataclasses import dataclass

import pyte
from pyte import modes

CURSOR_POSITION_QUERY = b"\x1b[6n"
TERMINAL_STATUS_QUERY = b"\x1b[5n"
MODE_25_QUERY = b"\x1b[?25$p"

MODE_STATUS_NAMES = {
    0: "unrecognized",
    1: "set",
    2: "reset",
    3: "permanently_set",
    4: "permanently_reset",
}
_DSR_6N_RESPONSE = re.compile(rb"\x1b\[[1-9][0-9]*;[1-9][0-9]*R")
CURSOR_QUERY_HELPER = """import os
import select
import sys
import time

fd = os.open("/dev/tty", os.O_RDWR)
try:
    os.write(fd, b"\\x1b[6n")
    response = bytearray()
    deadline = time.monotonic() + 3.0
    while not response.endswith(b"R"):
        remaining = deadline - time.monotonic()
        if remaining <= 0 or len(response) >= 32:
            raise SystemExit(124)
        if not select.select([fd], [], [], remaining)[0]:
            raise SystemExit(124)
        chunk = os.read(fd, 1)
        if not chunk:
            raise SystemExit(125)
        response.extend(chunk)
    sys.stdout.write(response.hex())
finally:
    os.close(fd)
"""


def has_dsr_6n_exchange(exchanges: list[dict[str, object]]) -> bool:
    for exchange in exchanges:
        if (exchange.get("responder") != "pyte terminal screen"
                or exchange.get("kind") != "DSR 6n"
                or exchange.get("query_hex") != CURSOR_POSITION_QUERY.hex(" ")):
            continue
        try:
            response = bytes.fromhex(str(exchange["response_hex"]))
        except (KeyError, ValueError):
            continue
        if _DSR_6N_RESPONSE.fullmatch(response):
            return True
    return False


@dataclass(frozen=True)
class TerminalReply:
    query: bytes
    response: bytes
    kind: str
    mode: int | None = None
    status: int | None = None

    def as_record(self) -> dict[str, object]:
        record: dict[str, object] = {
            "kind": self.kind,
            "query_hex": self.query.hex(" "),
            "response_hex": self.response.hex(" "),
            "responder": "pyte terminal screen",
        }
        if self.mode is not None:
            record["mode"] = self.mode
        if self.status is not None:
            record["status"] = self.status
            record["mode_state"] = MODE_STATUS_NAMES[self.status]
        return record


class _N6Screen(pyte.Screen):
    def __init__(self, columns: int, rows: int, terminal: N6Terminal) -> None:
        self.terminal = terminal
        self._active_query: bytes | None = None
        super().__init__(columns, rows)

    def report_device_status(self, mode: int) -> None:
        query = {
            5: TERMINAL_STATUS_QUERY,
            6: CURSOR_POSITION_QUERY,
        }.get(mode)
        self._active_query = query
        try:
            super().report_device_status(mode)
        finally:
            self._active_query = None

    def write_process_input(self, data: str) -> None:
        if self._active_query is None:
            return
        self.terminal._queue_reply(
            self._active_query,
            data.encode("ascii"),
            "DSR 5n" if self._active_query == TERMINAL_STATUS_QUERY else "DSR 6n",
        )


class N6Terminal:
    """A PTY peer that answers queries from its independently tracked VT screen."""

    def __init__(self, rows: int, columns: int) -> None:
        self._decoder = codecs.getincrementaldecoder("utf-8")("replace")
        self._query_tail = ""
        self._pending_replies: list[TerminalReply] = []
        self.screen = _N6Screen(columns, rows, self)
        self.stream = pyte.Stream(self.screen)

    @property
    def mode_25_status(self) -> int:
        return 1 if modes.DECTCEM in self.screen.mode else 2

    def feed(self, data: bytes) -> list[TerminalReply]:
        text = self._decoder.decode(data)
        self._feed_text(text)
        return self._drain_replies()

    def finish(self) -> list[TerminalReply]:
        text = self._decoder.decode(b"", final=True)
        self._feed_text(text)
        if self._query_tail:
            self.stream.feed(self._query_tail)
            self._query_tail = ""
        return self._drain_replies()

    def _feed_text(self, text: str) -> None:
        buffer = self._query_tail + text
        self._query_tail = ""
        while buffer:
            query_at = buffer.find(MODE_25_QUERY.decode("ascii"))
            if query_at >= 0:
                self.stream.feed(buffer[:query_at])
                self._reply_mode_25()
                query_end = query_at + len(MODE_25_QUERY)
                self.stream.feed(buffer[query_at:query_end])
                buffer = buffer[query_end:]
                continue

            tail_size = self._mode_query_tail_size(buffer)
            ready_end = len(buffer) - tail_size
            if ready_end:
                self.stream.feed(buffer[:ready_end])
            self._query_tail = buffer[ready_end:]
            return

    @staticmethod
    def _mode_query_tail_size(text: str) -> int:
        query = MODE_25_QUERY.decode("ascii")
        for size in range(min(len(query) - 1, len(text)), 0, -1):
            if text.endswith(query[:size]):
                return size
        return 0

    def _reply_mode_25(self) -> None:
        status = self.mode_25_status
        response = f"\x1b[?25;{status}$y".encode("ascii")
        self._queue_reply(MODE_25_QUERY, response, "DECRQM", mode=25, status=status)

    def _queue_reply(self, query: bytes, response: bytes, kind: str,
                     mode: int | None = None, status: int | None = None) -> None:
        self._pending_replies.append(
            TerminalReply(query, response, kind, mode, status)
        )

    def _drain_replies(self) -> list[TerminalReply]:
        replies, self._pending_replies = self._pending_replies, []
        return replies


def write_replies(master: int, replies: list[TerminalReply]) -> list[dict[str, object]]:
    records = []
    for reply in replies:
        remaining = memoryview(reply.response)
        while remaining:
            written = os.write(master, remaining)
            if written <= 0:
                raise OSError("PTY master accepted no terminal response bytes.")
            remaining = remaining[written:]
        records.append(reply.as_record())
    return records


def terminal_snapshot(fd: int) -> tuple[object, ...]:
    attributes = termios.tcgetattr(fd)
    return (*attributes[:6], tuple(attributes[6]))


def cursor_probe_record(
    output: bytes,
    exchanges: list[dict[str, object]],
    reply_marker: bytes,
    ready_marker: bytes,
) -> dict[str, object]:
    marker_at = output.find(reply_marker)
    line_end = output.find(b"\n", marker_at) if marker_at >= 0 else -1
    payload = output[marker_at + len(reply_marker):line_end] if line_end >= 0 else b""
    status_text, separator, reply_hex = payload.decode("ascii", "replace").partition(":")
    try:
        helper_status = int(status_text)
        response = bytes.fromhex(reply_hex) if separator else b""
    except ValueError:
        helper_status, response = None, b""
    peer_exchange = next(
        (
            exchange for exchange in reversed(exchanges)
            if exchange.get("kind") == "DSR 6n"
            and exchange.get("query_hex") == CURSOR_POSITION_QUERY.hex(" ")
        ),
        None,
    )
    ready_at = output.find(ready_marker)
    query_at = output.find(
        CURSOR_POSITION_QUERY, ready_at + len(ready_marker), marker_at
    ) if ready_at >= 0 and marker_at > ready_at else -1
    peer_response = None
    if peer_exchange is not None:
        try:
            peer_response = bytes.fromhex(str(peer_exchange["response_hex"]))
        except (KeyError, ValueError):
            pass
    return {
        "origin": "wrapper post-target PTY probe",
        "helper_exit_code": helper_status,
        "query_hex": CURSOR_POSITION_QUERY.hex(" "),
        "query_observed_hex": (
            str(peer_exchange["query_hex"]) if peer_exchange is not None else None
        ),
        "query_matches_pty": query_at >= 0 and peer_exchange is not None,
        "peer_response_hex": peer_response.hex(" ") if peer_response else None,
        "response_hex": response.hex(" "),
        "response_matches_peer": peer_response == response,
    }
