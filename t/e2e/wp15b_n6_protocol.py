from __future__ import annotations

import re
import time
from collections.abc import Callable

from wp15b_n6_terminal import MODE_25_QUERY, N6Terminal

MODE_REPLY_MARKER = b"__NTOP_N6_QUERY_REPLY__"
QUERY_TIMEOUT = 3.0


def query_mode_25_exchange(
    master: int,
    output: bytearray,
    terminal: N6Terminal,
    exchanges: list[dict[str, object]],
    read_once: Callable[
        [int, bytearray, float, N6Terminal, list[dict[str, object]]], bool
    ],
) -> tuple[list[int] | None, int | None, bool]:
    query = MODE_25_QUERY
    query_offset = bytes(output).rfind(query)
    if query_offset < 0:
        query_offset = len(output)
        deadline = time.monotonic() + QUERY_TIMEOUT
        while query not in output[query_offset:]:
            if time.monotonic() >= deadline:
                raise TimeoutError("DECRQM query was not visible on the PTY master.")
            read_once(master, output, 0.05, terminal, exchanges)
        query_offset = output.find(query, query_offset)
    deadline = time.monotonic() + QUERY_TIMEOUT
    while (MODE_REPLY_MARKER not in output[query_offset:] or
           output.find(b"\n", output.find(MODE_REPLY_MARKER, query_offset)) < 0):
        if time.monotonic() >= deadline:
            raise TimeoutError("DECRQM reply was not recorded by the PTY wrapper.")
        read_once(master, output, 0.05, terminal, exchanges)
    marker_offset = output.find(MODE_REPLY_MARKER, query_offset) + len(MODE_REPLY_MARKER)
    line_end = output.find(b"\n", marker_offset)
    if line_end < 0:
        raise RuntimeError("DECRQM reply marker was incomplete.")
    try:
        received = bytes.fromhex(output[marker_offset:line_end].decode("ascii").strip())
    except ValueError:
        return None, None, False
    match = re.fullmatch(rb"\x1b\[\?25;([0-4])\$y", received)
    emulator_reply = next(
        (
            bytes.fromhex(str(exchange["response_hex"]))
            for exchange in reversed(exchanges)
            if exchange.get("kind") == "DECRQM"
            and exchange.get("query_hex") == query.hex(" ")
        ),
        None,
    )
    return (
        list(received),
        int(match.group(1)) if match else None,
        emulator_reply is not None and received == emulator_reply,
    )
