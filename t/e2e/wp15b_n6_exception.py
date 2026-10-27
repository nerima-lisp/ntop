from __future__ import annotations

import argparse
import errno
import fcntl
import json
import os
import pty
import re
import select
import shutil
import signal
import struct
import subprocess
import sys
import termios
import tempfile
import time
from pathlib import Path

from wp15b_n6_terminal import (
    CURSOR_POSITION_QUERY as CURSOR_QUERY,
    CURSOR_QUERY_HELPER,
    MODE_25_QUERY as QUERY,
    MODE_STATUS_NAMES,
    N6Terminal,
    cursor_probe_record,
    has_dsr_6n_exchange,
    write_replies,
)
from wp15b_n6_protocol import query_mode_25_exchange
from wp15b_n6_environment import source_registry_environment

READY = b"__NTOP_N6_EXCEPTION_READY__"
BEFORE_BEGIN = b"__NTOP_N6_BEFORE_BEGIN__"
BEFORE_END = b"__NTOP_N6_BEFORE_END__"
AFTER_BEGIN = b"__NTOP_N6_AFTER_BEGIN__"
AFTER_END = b"__NTOP_N6_AFTER_END__"
TARGET_PID = b"__NTOP_N6_TARGET_PID__"
TARGET_EXIT = b"__NTOP_N6_TARGET_EXIT__"
CURSOR_REPLY_MARKER = b"__NTOP_N6_CURSOR_REPLY__"
TERMIOS_READY = b"__NTOP_N6_TERMIOS_READY__"
OUTPUT_LIMIT = 16 * 1024 * 1024
READINESS_TIMEOUT = 120.0
EXIT_TIMEOUT = 15.0

LISP_CHILD = """(require :asdf)
(asdf:load-asd (truename "ntop.asd"))
(asdf:load-system "ntop")
(ntop.app.infrastructure:call-with-terminal-session
 (lambda (poll-terminal-event)
   (declare (ignore poll-terminal-event))
   (write-line "__NTOP_N6_EXCEPTION_READY__")
   (finish-output)
   (unless (eql (read-char *standard-input* nil nil) #\\x)
     (error "terminal exception handshake failed"))
   (error "deliberate terminal restoration test error")))
"""

WRAPPER = r"""
printf '%s\n' '__NTOP_N6_BEFORE_BEGIN__'
stty -a </dev/tty
printf '%s\n' '__NTOP_N6_BEFORE_END__'
exec 3>&2
{
  /bin/sh -c 'printf "__NTOP_N6_TARGET_PID__%s\\n" "$$"; exec "$@"' \
    ntop-n6-source "$@" </dev/tty 2>&3
  target_status=$?
} 2>"$NTOP_N6_SHELL_STDERR"
printf '\n%s%s\n' '__NTOP_N6_TARGET_EXIT__' "$target_status"
printf '%s\n' '__NTOP_N6_AFTER_BEGIN__'
stty -a </dev/tty
printf '%s\n' '__NTOP_N6_AFTER_END__'
printf '%s\n' '__NTOP_N6_TERMIOS_READY__'
saved=$(stty -g </dev/tty) || exit 125
stty raw -echo </dev/tty || exit 125
cursor_reply=$("$NTOP_N6_PYTHON" "$NTOP_N6_CURSOR_QUERY_SCRIPT")
cursor_status=$?
printf '%s%s:%s\n' '__NTOP_N6_CURSOR_REPLY__' "$cursor_status" "$cursor_reply"
printf '\033[?25$p'
reply=$(dd bs=1 count=9 </dev/tty 2>/dev/null | od -An -tx1 | tr -d ' \n')
stty "$saved" </dev/tty || exit 125
printf '%s%s\n' '__NTOP_N6_QUERY_REPLY__' "$reply"
exit "$target_status"
"""


def _set_window_size(fd: int, rows: int, columns: int) -> None:
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, columns, 0, 0))


def _child_tty_setup() -> None:
    os.setsid()
    fcntl.ioctl(0, termios.TIOCSCTTY, 0)


def _raw_mode(fd: int) -> bool:
    flags = termios.tcgetattr(fd)[3]
    return not (flags & termios.ECHO) and not (flags & termios.ICANON)


def _read_once(
    master: int,
    output: bytearray,
    wait: float,
    terminal: N6Terminal,
    terminal_exchanges: list[dict[str, object]],
) -> bool:
    ready, _, _ = select.select([master], [], [], wait)
    if not ready:
        return False
    try:
        chunk = os.read(master, 65536)
    except OSError as error:
        if error.errno == errno.EIO:
            return False
        raise
    if not chunk:
        return False
    output.extend(chunk)
    terminal_exchanges.extend(write_replies(master, terminal.feed(chunk)))
    if len(output) > OUTPUT_LIMIT:
        raise RuntimeError("PTY output exceeded the 16 MiB evidence limit.")
    return True


def _marker_integer(output: bytes, marker: bytes) -> int | None:
    match = re.search(re.escape(marker) + rb"(-?[0-9]+)", output)
    return int(match.group(1)) if match else None


def _marked_section(output: bytes, start_marker: bytes,
                    end_marker: bytes) -> bytes | None:
    start = output.find(start_marker)
    if start < 0:
        return None
    start += len(start_marker)
    end = output.find(end_marker, start)
    if end < 0:
        return None
    return output[start:end].strip(b" \t\r\n")


def _wait_for_ready(process: subprocess.Popen[bytes], slave: int,
                    master: int, output: bytearray,
                    terminal: N6Terminal,
                    terminal_exchanges: list[dict[str, object]]) -> int:
    deadline = time.monotonic() + READINESS_TIMEOUT
    last_activity = time.monotonic()
    while True:
        target_pid = _marker_integer(bytes(output), TARGET_PID)
        cursor_queries = output.count(CURSOR_QUERY)
        cursor_replies = sum(
            exchange.get("query_hex") == CURSOR_QUERY.hex(" ")
            for exchange in terminal_exchanges
        )
        if (READY in output and target_pid is not None and _raw_mode(slave) and
                cursor_queries <= cursor_replies and
                time.monotonic() - last_activity >= 0.1):
            return target_pid
        if process.poll() is not None:
            raise RuntimeError("PTY wrapper exited before the exception seam was ready.")
        if time.monotonic() >= deadline:
            raise TimeoutError("Timed out waiting for raw-mode exception readiness.")
        if _read_once(master, output, 0.05, terminal, terminal_exchanges):
            last_activity = time.monotonic()


def _wait_for_marker(process: subprocess.Popen[bytes], master: int,
                     output: bytearray, marker: bytes, timeout: float,
                     terminal: N6Terminal,
                     terminal_exchanges: list[dict[str, object]]) -> None:
    deadline = time.monotonic() + timeout
    while marker not in output:
        if process.poll() is not None:
            raise RuntimeError("PTY wrapper exited before its after-stty marker.")
        if time.monotonic() >= deadline:
            raise TimeoutError("Timed out waiting for the after-stty marker.")
        _read_once(master, output, 0.05, terminal, terminal_exchanges)


def _pid_alive(pid: int, process: subprocess.Popen[bytes] | None = None) -> bool:
    if process is not None and pid == process.pid:
        return process.poll() is None
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def _cleanup(process: subprocess.Popen[bytes], target_pid: int | None,
             target_finished: bool) -> list[dict[str, object]]:
    actions = []
    if target_pid is not None and not target_finished:
        pids = [(target_pid, "target", None)]
    else:
        pids = []
    if process.poll() is None:
        pids.append((process.pid, "wrapper", process))
    for pid, role, owner in pids:
        for signum in (signal.SIGTERM, signal.SIGHUP, signal.SIGINT):
            if not _pid_alive(pid, owner):
                break
            try:
                os.kill(pid, signum)
                result = "sent"
            except ProcessLookupError:
                result = "already-exited"
            except PermissionError:
                result = "permission-denied"
            actions.append({"pid": pid, "role": role,
                            "signal": signal.Signals(signum).name,
                            "result": result})
            if result != "sent":
                break
            deadline = time.monotonic() + 0.5
            while _pid_alive(pid, owner) and time.monotonic() < deadline:
                if owner is not None:
                    owner.poll()
                time.sleep(0.02)
    return actions


def _save_case(directory: Path, raw: bytes,
               metadata: dict[str, object]) -> None:
    (directory / "ntop-source-lisp-exception.raw").write_bytes(raw)
    (directory / "ntop-source-lisp-exception.json").write_text(
        json.dumps(metadata, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )


def run_exception_case(directory: Path, root: Path,
                       sbcl: str,
                       source_registry: dict[str, str] | None = None
                       ) -> dict[str, object]:
    source_registry = source_registry or source_registry_environment(root)
    child_path = directory / "ntop-n6-exception-child.lisp"
    child_path.write_text(LISP_CHILD, encoding="utf-8")
    command = [
        sbcl,
        "--noinform",
        "--no-userinit",
        "--no-sysinit",
        "--disable-debugger",
        "--script",
        str(child_path),
    ]
    output = bytearray()
    process: subprocess.Popen[bytes] | None = None
    target_pid: int | None = None
    target_exit: int | None = None
    master = -1
    slave = -1
    before: bytes | None = None
    after: bytes | None = None
    reply_octets: list[int] | None = None
    reply_status: int | None = None
    cursor_probe: dict[str, object] | None = None
    query_response_matches_peer = False
    failure: str | None = None
    cleanup_actions: list[dict[str, object]] = []
    terminal = N6Terminal(rows=48, columns=160)
    terminal_exchanges: list[dict[str, object]] = []
    shell_stderr = directory / "ntop-source-lisp-exception.shell-job-control.stderr"
    metadata: dict[str, object] = {
        "target": "ntop-source-runtime",
        "termination": "lisp-exception",
        "runtime_scope": "source-runtime; not the built ntop executable",
        "test_seam_source": "t/app/infrastructure-terminal-test-support.lisp",
        "restoration_source": "src/app/infrastructure-terminal.lisp",
        "command": command,
        "launch_scope": "PTY session leader shell, synchronous foreground SBCL",
        "term": "xterm-256color",
        "window": {"rows": 48, "columns": 160},
        "stty_command": ["stty", "-a"],
        "query_hex": QUERY.hex(" "),
        "query_provider": "pyte terminal emulator peer",
        "raw_output": "ntop-source-lisp-exception.raw",
        "temporary_lisp_script": child_path.name,
        "shell_job_control_stderr": shell_stderr.name,
    }
    try:
        master, slave = pty.openpty()
        _set_window_size(slave, 48, 160)
        environment = os.environ.copy()
        environment.update(source_registry)
        environment["TERM"] = "xterm-256color"
        environment["NTOP_N6_SHELL_STDERR"] = str(shell_stderr)
        cursor_script = directory / "ntop-source-cursor-query.py"
        cursor_script.write_text(CURSOR_QUERY_HELPER, encoding="utf-8")
        environment["NTOP_N6_PYTHON"] = sys.executable
        environment["NTOP_N6_CURSOR_QUERY_SCRIPT"] = str(cursor_script)
        process = subprocess.Popen(
            ["/bin/sh", "-c", WRAPPER, "ntop-wp15b-n6-exception", *command],
            stdin=slave,
            stdout=slave,
            stderr=slave,
            cwd=root,
            env=environment,
            close_fds=True,
            preexec_fn=_child_tty_setup,
        )
        metadata["wrapper_pid"] = process.pid
        target_pid = _wait_for_ready(
            process, slave, master, output, terminal, terminal_exchanges
        )
        metadata["target_pid"] = target_pid
        metadata["raw_mode_at_ready"] = True
        os.write(master, b"x")
        deadline = time.monotonic() + EXIT_TIMEOUT
        while TARGET_EXIT not in output:
            if process.poll() is not None:
                raise RuntimeError("PTY wrapper exited before recording exception status.")
            if time.monotonic() >= deadline:
                raise TimeoutError("Timed out waiting for Lisp exception exit.")
            _read_once(master, output, 0.05, terminal, terminal_exchanges)
        target_exit = _marker_integer(bytes(output), TARGET_EXIT)
        if target_exit is None:
            raise RuntimeError("Target exit marker did not contain an integer status.")
        _wait_for_marker(
            process, master, output, AFTER_END, EXIT_TIMEOUT,
            terminal, terminal_exchanges,
        )
        before = _marked_section(bytes(output), BEFORE_BEGIN, BEFORE_END)
        after = _marked_section(bytes(output), AFTER_BEGIN, AFTER_END)
        reply_octets, reply_status, query_response_matches_peer = query_mode_25_exchange(
            master, output, terminal, terminal_exchanges, _read_once
        )
        process.wait(timeout=EXIT_TIMEOUT)
        cursor_probe = cursor_probe_record(
            bytes(output), terminal_exchanges, CURSOR_REPLY_MARKER, TERMIOS_READY
        )
    except Exception as error:
        failure = f"{type(error).__name__}: {error}"
        if process is not None:
            if target_pid is None:
                target_pid = _marker_integer(bytes(output), TARGET_PID)
            cleanup_actions = _cleanup(
                process, target_pid, TARGET_EXIT in output
            )
            try:
                process.wait(timeout=2.0)
            except subprocess.TimeoutExpired:
                metadata["wrapper_still_running_after_cleanup"] = True
            before = _marked_section(bytes(output), BEFORE_BEGIN, BEFORE_END)
            after = _marked_section(bytes(output), AFTER_BEGIN, AFTER_END)
    finally:
        if slave >= 0:
            os.close(slave)
        if master >= 0:
            os.close(master)

    raw = bytes(output)
    wrapper_code = process.returncode if process is not None else None
    checks = {
        "ready_marker_inside_raw_session": READY in raw and metadata.get(
            "raw_mode_at_ready"
        ) is True,
        "exception_message_seen": b"deliberate terminal restoration test error" in raw,
        "nonzero_exception_exit": target_exit is not None and target_exit != 0,
        "both_stty_snapshots": before is not None and after is not None,
        "stty_unchanged": before is not None and before == after,
        "query_seen": QUERY in raw,
        "recognized_query_reply": reply_status in (1, 2, 3, 4),
        "cursor_visible_after_run": reply_status == 1,
        "query_response_matches_peer": query_response_matches_peer,
        "cursor_status_reply_matches_peer": (
            cursor_probe is not None
            and cursor_probe["helper_exit_code"] == 0
            and cursor_probe["query_matches_pty"] is True
            and cursor_probe["response_matches_peer"] is True
        ),
        "dsr_6n_query_answered_by_pyte_peer": has_dsr_6n_exchange(
            terminal_exchanges
        ),
        "target_pid_recorded": target_pid is not None,
        "harness_completed": process is not None and wrapper_code is not None,
        "no_runner_error": failure is None,
        "no_target_or_wrapper_running": (
            (target_pid is None or not _pid_alive(target_pid))
            and (process is None or process.poll() is not None)
        ),
    }
    metadata.update(
        {
            "target_exit_code": target_exit,
            "wrapper_returncode": wrapper_code,
            "stty_before": before.decode("utf-8", errors="replace") if before else None,
            "stty_after": after.decode("utf-8", errors="replace") if after else None,
            "terminal_query_exchanges": terminal_exchanges,
            "reply_octets": reply_octets,
            "reply_hex": (
                bytes(reply_octets).hex(" ") if reply_octets is not None else None
            ),
            "reply_status": reply_status,
            "terminal_mode_25_reply_status": reply_status,
            "terminal_mode_25_state": (
                MODE_STATUS_NAMES.get(reply_status)
                if reply_status is not None else None
            ),
            "query_response_matches_peer": query_response_matches_peer,
            "cursor_status_probe": cursor_probe,
            "checks": checks,
            "cleanup_actions": cleanup_actions,
            "shell_job_control_stderr_bytes": (
                shell_stderr.stat().st_size if shell_stderr.exists() else 0
            ),
            "status": "PASS" if all(checks.values()) else "FAIL",
        }
    )
    if failure:
        metadata["error"] = failure
    _save_case(directory, raw, metadata)
    return metadata


def main() -> int:
    parser = argparse.ArgumentParser(description="Measure the Lisp N6 exception path.")
    parser.add_argument("--root", default=str(Path(__file__).resolve().parents[2]))
    parser.add_argument("--sbcl", default="sbcl")
    parser.add_argument("--evidence-dir")
    arguments = parser.parse_args()
    sbcl = shutil.which(arguments.sbcl)
    if sbcl is None:
        print(f"ERROR: SBCL not found: {arguments.sbcl}", file=sys.stderr)
        return 1
    directory = (
        Path(arguments.evidence_dir)
        if arguments.evidence_dir
        else Path(tempfile.mkdtemp(prefix="ntop-wp15b-n6-exception-", dir="/tmp"))
    )
    directory.mkdir(parents=True, exist_ok=True)
    result = run_exception_case(directory, Path(arguments.root).resolve(), sbcl)
    print(f"N6 exception evidence: {directory}")
    print(f"N6 exception result: {result['status']}")
    return 0 if result["status"] == "PASS" else 1


if __name__ == "__main__":
    raise SystemExit(main())
