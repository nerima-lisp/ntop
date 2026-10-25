from __future__ import annotations

import errno
import fcntl
import json
import os
import pty
import re
import select
import signal
import struct
import subprocess
import termios
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
QUERY = b"\x1b[?25$p"
CURSOR_QUERY = b"\x1b[6n"
CURSOR_REPLY = b"\x1b[1;1R"
BEFORE_BEGIN = b"__NTOP_N6_BEFORE_BEGIN__"
BEFORE_END = b"__NTOP_N6_BEFORE_END__"
AFTER_BEGIN = b"__NTOP_N6_AFTER_BEGIN__"
AFTER_END = b"__NTOP_N6_AFTER_END__"
TERMIOS_READY = b"__NTOP_N6_TERMIOS_READY__"
TARGET_PID = b"__NTOP_N6_TARGET_PID__"
TARGET_EXIT = b"__NTOP_N6_TARGET_EXIT__"
REPLY_MARKER = b"__NTOP_N6_QUERY_REPLY__"
EXCEPTION_ENV = "NTOP_N6_TEST_EXCEPTION"
EXCEPTION_ENV_VALUE = "raise-on-first-render"
EXCEPTION_READY = b"__NTOP_N6_EXCEPTION_READY__"
EXCEPTION_MESSAGE = b"Deliberate N6 test exception after terminal setup."
READINESS_TIMEOUT = 30.0
EXIT_TIMEOUT = 15.0
QUERY_TIMEOUT = 3.0
OUTPUT_LIMIT = 16 * 1024 * 1024

WRAPPER = r"""
printf '%s\n' '__NTOP_N6_BEFORE_BEGIN__'
stty -a </dev/tty
printf '%s\n' '__NTOP_N6_BEFORE_END__'
exec 3>&2
{
  /bin/sh -c 'printf "__NTOP_N6_TARGET_PID__%s\\n" "$$"; exec "$@"' \
    ntop-n6-target "$@" </dev/tty 2>&3
  target_status=$?
} 2>"$NTOP_N6_SHELL_STDERR"
printf '\n%s%s\n' '__NTOP_N6_TARGET_EXIT__' "$target_status"
printf '%s\n' '__NTOP_N6_AFTER_BEGIN__'
stty -a </dev/tty
printf '%s\n' '__NTOP_N6_AFTER_END__'
printf '%s\n' '__NTOP_N6_TERMIOS_READY__'
IFS= read -r _ </dev/tty || exit 125
saved=$(stty -g </dev/tty) || exit 125
stty raw -echo </dev/tty || exit 125
printf '\033[?25$p'
reply=$(dd bs=1 count=9 </dev/tty 2>/dev/null | od -An -tx1 | tr -d ' \n')
stty "$saved" </dev/tty || exit 125
printf '%s%s\n' '__NTOP_N6_QUERY_REPLY__' "$reply"
exit "$target_status"
"""

def _set_window_size(fd: int, rows: int, columns: int) -> None:
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, columns, 0, 0))


def _terminal_snapshot(fd: int) -> tuple[object, ...]:
    attributes = termios.tcgetattr(fd)
    return (*attributes[:6], tuple(attributes[6]))


def _child_tty_setup() -> None:
    os.setsid()
    fcntl.ioctl(0, termios.TIOCSCTTY, 0)


def _read_once(master: int, output: bytearray, wait: float) -> bool:
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
    if len(output) > OUTPUT_LIMIT:
        raise RuntimeError("PTY output exceeded the 16 MiB evidence limit.")
    return True


def _marker_integer(output: bytes, marker: bytes) -> int | None:
    pattern = re.escape(marker) + rb"(-?[0-9]+)"
    return next(map(int, re.findall(pattern, output)), None)


def _wait_for_raw(process: subprocess.Popen[bytes], slave: int, master: int,
                  output: bytearray,
                  probes: list[dict[str, object]]) -> int:
    deadline = time.monotonic() + READINESS_TIMEOUT
    query_offset = 0
    replied = False
    post_reply_output = 0
    last_activity = time.monotonic()
    while True:
        while True:
            query_position = bytes(output).find(CURSOR_QUERY, query_offset)
            if query_position < 0:
                break
            os.write(master, CURSOR_REPLY)
            probes.append(
                {"query_hex": CURSOR_QUERY.hex(" "),
                 "reply_hex": CURSOR_REPLY.hex(" ")}
            )
            query_offset = query_position + len(CURSOR_QUERY)
            replied = True
            post_reply_output = len(output)
        target_pid = _marker_integer(bytes(output), TARGET_PID)
        flags = termios.tcgetattr(slave)[3]
        raw = not flags & (termios.ECHO | termios.ICANON)
        response_seen = not replied or len(output) > post_reply_output
        if (target_pid is not None and raw and response_seen and
                time.monotonic() - last_activity >= 0.1):
            return target_pid
        if process.poll() is not None:
            raise RuntimeError("PTY wrapper exited before target entered raw mode.")
        if time.monotonic() >= deadline:
            raise TimeoutError("Timed out waiting for target raw mode.")
        if _read_once(master, output, 0.05):
            last_activity = time.monotonic()


def _wait_for_exception(process: subprocess.Popen[bytes], master: int,
                        output: bytearray) -> int:
    deadline = time.monotonic() + READINESS_TIMEOUT
    while EXCEPTION_READY not in output:
        if process.poll() is not None:
            raise RuntimeError("Target exited before the exception readiness marker.")
        if time.monotonic() >= deadline:
            raise TimeoutError("Timed out waiting for the exception readiness marker.")
        _read_once(master, output, 0.05)
    target_pid = _marker_integer(bytes(output), TARGET_PID)
    if target_pid is None:
        raise RuntimeError("Target PID marker was missing before the exception.")
    return target_pid


def _marked_section(output: bytes, start_marker: bytes,
                    end_marker: bytes) -> bytes | None:
    start = output.find(start_marker)
    if start < 0:
        return None
    end = output.find(end_marker, start + len(start_marker))
    return output[start + len(start_marker):end].strip(b" \t\r\n") if end >= 0 else None


def _query_exchange(master: int, output: bytearray,
                    status: int) -> tuple[list[int] | None, int | None]:
    query_offset = bytes(output).rfind(QUERY)
    if query_offset < 0:
        query_offset = len(output)
        deadline = time.monotonic() + QUERY_TIMEOUT
        while QUERY not in output[query_offset:]:
            if time.monotonic() >= deadline:
                raise TimeoutError("DECRQM query was not visible on the PTY master.")
            _read_once(master, output, 0.05)
        query_offset = output.find(QUERY, query_offset)
    reply = f"\x1b[?25;{status}$y".encode("ascii")
    os.write(master, reply)
    deadline = time.monotonic() + QUERY_TIMEOUT
    while (REPLY_MARKER not in output[query_offset:] or
           output.find(b"\n", output.find(REPLY_MARKER, query_offset)) < 0):
        if time.monotonic() >= deadline:
            raise TimeoutError("DECRQM reply was not recorded by the PTY wrapper.")
        _read_once(master, output, 0.05)
    marker_offset = output.find(REPLY_MARKER, query_offset) + len(REPLY_MARKER)
    line_end = output.find(b"\n", marker_offset)
    if line_end < 0:
        raise RuntimeError("DECRQM reply marker was incomplete.")
    try:
        received = bytes.fromhex(
            output[marker_offset:line_end].decode("ascii").strip()
        )
    except ValueError:
        return None, None
    match = re.fullmatch(rb"\x1b\[\?25;([0-4])\$y", received)
    return list(received), int(match.group(1)) if match else None


def _pid_alive(pid: int, wrapper: subprocess.Popen[bytes] | None = None) -> bool:
    if wrapper is not None and pid == wrapper.pid:
        return wrapper.poll() is None
    try:
        os.kill(pid, 0)
        return True
    except ProcessLookupError:
        return False
    except PermissionError:
        return True


def _cleanup(process: subprocess.Popen[bytes], target_pid: int | None,
             target_finished: bool) -> list[dict[str, object]]:
    actions = []
    pids = []
    if target_pid is not None and not target_finished:
        pids.append((target_pid, "target", None))
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


def _private_mode_events(output: bytes) -> list[dict[str, object]]:
    return [
        {"mode": int(value),
         "action": "set" if match.group(2) == b"h" else "reset"}
        for match in re.finditer(rb"\x1b\[\?([0-9;]+)([hl])", output)
        for value in match.group(1).split(b";")
    ]


def _cursor_status(events: list[dict[str, object]]) -> int:
    return next(
        (1 if event["action"] == "set" else 2
         for event in reversed(events) if event["mode"] == 25),
        1,
    )


def _save_case(directory: Path, name: str, raw: bytes,
               metadata: dict[str, object]) -> None:
    (directory / f"{name}.raw").write_bytes(raw)
    (directory / f"{name}.json").write_text(
        json.dumps(metadata, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )


def _run_case(directory: Path, target: str, command: list[str],
              termination: str, signum: int | None) -> dict[str, object]:
    name = f"{target}-{termination.lower()}"
    exception_case = termination == "lisp-exception"
    sigkill_case = termination == "SIGKILL"
    output = bytearray()
    process: subprocess.Popen[bytes] | None = None
    master = -1
    slave = -1
    target_pid: int | None = None
    target_exit: int | None = None
    before: bytes | None = None
    after: bytes | None = None
    termios_before: tuple[object, ...] | None = None
    termios_after: tuple[object, ...] | None = None
    reply_octets: list[int] | None = None
    reply_status: int | None = None
    failure: str | None = None
    cleanup_actions: list[dict[str, object]] = []
    terminal_probes: list[dict[str, object]] = []
    shell_stderr = directory / f"{name}.shell-job-control.stderr"
    metadata: dict[str, object] = {
        "target": target,
        "termination": termination,
        "acceptance_scope": (
            "reference" if target == "btm"
            else "outside-acceptance" if sigkill_case
            else "acceptance"
        ),
        "command": command,
        "launch_scope": "PTY session leader shell, synchronous foreground target",
        "signal_scope": "captured target PID only" if signum is not None else None,
        "term": "xterm-256color",
        "window": {"rows": 48, "columns": 160},
        "stty_command": ["stty", "-a"],
        "query_hex": QUERY.hex(" "),
        "query_provider": "harness model from observed DEC private-mode events",
        "raw_output": f"{name}.raw",
        "shell_job_control_stderr": shell_stderr.name,
    }
    try:
        master, slave = pty.openpty()
        _set_window_size(slave, 48, 160)
        termios_before = _terminal_snapshot(slave)
        environment = os.environ.copy()
        environment["TERM"] = "xterm-256color"
        environment["NTOP_N6_SHELL_STDERR"] = str(shell_stderr)
        environment.pop(EXCEPTION_ENV, None)
        if exception_case:
            environment[EXCEPTION_ENV] = EXCEPTION_ENV_VALUE
        process = subprocess.Popen(
            ["/bin/sh", "-c", WRAPPER, "ntop-wp15b-n6", *command],
            stdin=slave,
            stdout=slave,
            stderr=slave,
            cwd=ROOT,
            env=environment,
            close_fds=True,
            preexec_fn=_child_tty_setup,
        )
        metadata["wrapper_pid"] = process.pid
        if exception_case:
            target_pid = _wait_for_exception(process, master, output)
            metadata["termination_action"] = (
                "uncaught test exception on first application-loop render"
            )
        else:
            target_pid = _wait_for_raw(
                process, slave, master, output, terminal_probes
            )
        metadata["target_pid"] = target_pid
        if exception_case:
            metadata["exception_environment"] = {
                "name": EXCEPTION_ENV,
                "value": EXCEPTION_ENV_VALUE,
            }
        elif signum is None:
            os.write(master, b"q")
            metadata["termination_action"] = "write q to PTY input"
        else:
            os.kill(target_pid, signum)
            metadata["termination_action"] = f"os.kill({target_pid}, {termination})"
            metadata["signal_number"] = signum
        deadline = time.monotonic() + EXIT_TIMEOUT
        while (TARGET_EXIT not in output or AFTER_END not in output
               or TERMIOS_READY not in output):
            if process.poll() is not None:
                raise RuntimeError("PTY wrapper exited before the after-stty marker.")
            if time.monotonic() >= deadline:
                raise TimeoutError("Timed out waiting for target restoration markers.")
            _read_once(master, output, 0.05)
        try:
            termios_after = _terminal_snapshot(slave)
        except Exception as error:
            metadata["termios_snapshot_error"] = (
                f"{type(error).__name__}: {error}"
            )
            raise
        metadata["termios_snapshot_point"] = (
            "post-target-stty-before-cursor-query"
        )
        os.write(master, b"\n")
        target_exit = _marker_integer(bytes(output), TARGET_EXIT)
        if target_exit is None:
            raise RuntimeError("Target exit marker did not contain an integer status.")
        before = _marked_section(bytes(output), BEFORE_BEGIN, BEFORE_END)
        after = _marked_section(bytes(output), AFTER_BEGIN, AFTER_END)
        events = _private_mode_events(bytes(output))
        reply_octets, reply_status = _query_exchange(
            master, output, _cursor_status(events)
        )
        process.wait(timeout=EXIT_TIMEOUT)
    except Exception as error:
        failure = f"{type(error).__name__}: {error}"
        if process is not None:
            if target_pid is None:
                target_pid = _marker_integer(bytes(output), TARGET_PID)
            target_finished = TARGET_EXIT in output
            cleanup_actions = _cleanup(process, target_pid, target_finished)
            try:
                process.wait(timeout=2.0)
            except subprocess.TimeoutExpired:
                metadata["wrapper_still_running_after_cleanup"] = True
            try:
                before = _marked_section(bytes(output), BEFORE_BEGIN, BEFORE_END)
                after = _marked_section(bytes(output), AFTER_BEGIN, AFTER_END)
            except Exception as cleanup_error:
                metadata["post_failure_error"] = (
                    f"{type(cleanup_error).__name__}: {cleanup_error}"
                )
    finally:
        if slave >= 0:
            os.close(slave)
        if master >= 0:
            os.close(master)

    raw = bytes(output)
    events = _private_mode_events(raw)
    wrapper_code = process.returncode if process is not None else None
    expected_exit = (
        target_exit is not None and target_exit != 0 if exception_case
        else target_exit == 0 if signum is None
        else target_exit == 128 + signum
    )
    stty_unchanged = before is not None and before == after
    stty_changed = None if before is None or after is None else before != after
    termios_unchanged = (
        termios_before is not None
        and termios_after is not None
        and termios_before == termios_after
    )
    termios_changed = (
        None
        if termios_before is None or termios_after is None
        else termios_before != termios_after
    )
    exception_ready = EXCEPTION_READY in raw
    exception_message_seen = EXCEPTION_MESSAGE in raw
    checks = {
        "both_stty_snapshots": before is not None and after is not None,
        "both_termios_snapshots": (
            termios_before is not None and termios_after is not None
        ),
        "expected_exit_status": expected_exit,
        "target_pid_recorded": target_pid is not None,
        "harness_completed": process is not None and wrapper_code is not None,
        "no_runner_error": failure is None,
        "no_target_or_wrapper_running": (
            (target_pid is None or not _pid_alive(target_pid))
            and (process is None or process.poll() is not None)
        ),
    }
    if sigkill_case:
        checks["sigkill_sent_to_captured_target"] = (
            signum == signal.SIGKILL
            and metadata.get("signal_scope") == "captured target PID only"
        )
    else:
        checks.update(
            {
                "stty_unchanged": stty_unchanged,
                "termios_unchanged": termios_unchanged,
                "query_seen": QUERY in raw,
                "recognized_query_reply": reply_status in (1, 2, 3, 4),
                "cursor_visible_after_run": reply_status == 1,
                "dec_mode_reset_seen": any(
                    e["action"] == "reset" for e in events
                ),
            }
        )
    if exception_case:
        checks["exception_readiness_marker_seen"] = exception_ready
        checks["exception_message_seen"] = exception_message_seen
    metadata.update(
        {
            "target_exit_code": target_exit,
            "wrapper_returncode": wrapper_code,
            "stty_before": before.decode("utf-8", errors="replace") if before else None,
            "stty_after": after.decode("utf-8", errors="replace") if after else None,
            "termios_before": repr(termios_before) if termios_before is not None else None,
            "termios_after": repr(termios_after) if termios_after is not None else None,
            "stty_unchanged": stty_unchanged,
            "stty_remains_changed": stty_changed,
            "termios_unchanged": termios_unchanged,
            "terminal_mode_remains_changed": termios_changed,
            "private_mode_events": events,
            "terminal_probe_exchanges": terminal_probes,
            "cursor_status_before_reply": _cursor_status(events),
            "dec_mode_reset_seen": any(
                e["action"] == "reset" for e in events
            ),
            "query_seen": QUERY in raw,
            "reply_octets": reply_octets,
            "reply_status": reply_status,
            "checks": checks,
            "cleanup_actions": cleanup_actions,
            "shell_job_control_stderr_bytes": (
                shell_stderr.stat().st_size if shell_stderr.exists() else 0
            ),
        }
    )
    if sigkill_case:
        metadata["status"] = "outside-acceptance"
        metadata["diagnostic_status"] = (
            "PASS" if all(checks.values()) else "FAIL"
        )
    else:
        metadata["status"] = "PASS" if all(checks.values()) else "FAIL"
    if failure:
        metadata["error"] = failure
    _save_case(directory, name, raw, metadata)
    return metadata
