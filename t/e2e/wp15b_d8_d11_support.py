"""Shared evidence collection for the WP15b D8/D11 harness."""

from __future__ import annotations

import codecs
import ctypes
import datetime as dt
import fcntl
import json
import os
import pty
import select
import signal
import struct
import subprocess
import sys
import termios
import time
from functools import cache
from pathlib import Path
from typing import Any

import pyte

from wp15b_d8_d11_display import (
    AVG_PATTERN, HEIGHT, WIDTH, application_render_signature,
    cli_error_output_detected, complete_process_panel,
)

CPU_LOAD_INFO = 2
CPU_ACTIVE_STATES = (0, 1, 3)
MAX_LOAD_ATTEMPTS = 5

U32 = ctypes.c_uint32
PTR_U32 = ctypes.POINTER(U32)


@cache
def _libsystem() -> Any:
    library = ctypes.CDLL("/usr/lib/libSystem.B.dylib")
    library.mach_host_self.restype = U32
    library.mach_task_self.restype = U32
    library.host_processor_info.argtypes = [
        U32, ctypes.c_int, PTR_U32, ctypes.POINTER(PTR_U32), PTR_U32,
    ]
    library.host_processor_info.restype = ctypes.c_int
    library.vm_deallocate.argtypes = [U32, ctypes.c_ulong, ctypes.c_ulong]
    library.vm_deallocate.restype = ctypes.c_int
    return library


if sys.platform == "darwin":
    _libsystem()


def utc_now() -> str:
    return dt.datetime.now(dt.timezone.utc).isoformat()


def write_json(path: Path, value: Any) -> None:
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n")


def cpu_counters() -> list[list[int]]:
    library = _libsystem()
    processor_count, info_count, info = U32(), U32(), PTR_U32()
    result = library.host_processor_info(
        library.mach_host_self(), CPU_LOAD_INFO, ctypes.byref(processor_count),
        ctypes.byref(info), ctypes.byref(info_count),
    )
    if result != 0:
        raise RuntimeError(f"host_processor_info returned {result}")
    values = [int(info[index]) for index in range(info_count.value)]
    address = ctypes.cast(info, ctypes.c_void_p).value
    deallocated = library.vm_deallocate(
        library.mach_task_self(), address, info_count.value * ctypes.sizeof(U32)
    )
    if deallocated != 0:
        raise RuntimeError(f"vm_deallocate returned {deallocated}")
    if info_count.value != processor_count.value * 4:
        raise RuntimeError(
            "unexpected processor info size: "
            f"{processor_count.value}, {info_count.value}"
        )
    return [values[index:index + 4] for index in range(0, len(values), 4)]


def busy_percent(before: list[list[int]], after: list[list[int]]) -> list[dict[str, Any]]:
    rows = []
    for index, (old, new) in enumerate(zip(before, after)):
        ticks = [later - earlier for earlier, later in zip(old, new)]
        total = sum(ticks)
        active = sum(ticks[state] for state in CPU_ACTIVE_STATES)
        rows.append({"cpu": index, "busy_percent": 100 * active / total if total else None,
                     "delta_ticks_user_system_idle_nice": ticks})
    return rows


def load_window(duration_seconds: float) -> dict[str, Any]:
    before = cpu_counters()
    started = time.monotonic()
    time.sleep(max(0.0, started + duration_seconds - time.monotonic()))
    after = cpu_counters()
    ended = time.monotonic()
    per_cpu = busy_percent(before, after)
    return {
        "start_monotonic": started, "end_monotonic": ended,
        "duration_seconds": ended - started, "before_ticks": before,
        "after_ticks": after, "per_cpu": per_cpu,
        "minimum_busy_percent": min(
            (row["busy_percent"] for row in per_cpu if row["busy_percent"] is not None),
            default=None,
        ),
    }


def cpu_worker() -> None:
    value = 1
    while True:
        value = (value * 1103515245 + 12345) & 0x7FFFFFFF


def child_status(raw_status: int | None) -> int | None:
    if raw_status is None:
        return None
    if os.WIFEXITED(raw_status):
        return os.WEXITSTATUS(raw_status)
    if os.WIFSIGNALED(raw_status):
        return -os.WTERMSIG(raw_status)
    return None


def pid_absent(pid: int) -> bool:
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return True
    except PermissionError:
        return False
    return False


def wait_child(pid: int, timeout_seconds: float) -> tuple[bool, int | None]:
    deadline = time.monotonic() + timeout_seconds
    while True:
        try:
            done, status = os.waitpid(pid, os.WNOHANG)
        except ChildProcessError:
            return True, None
        if done:
            return True, child_status(status)
        if time.monotonic() >= deadline:
            return False, None
        time.sleep(0.05)

def kill_child(pid: int, sig: int) -> dict[str, Any]:
    try:
        os.kill(pid, sig)
    except OSError as error:
        return {
            "signal": sig.name,
            "sent": False,
            "error": type(error).__name__,
            "errno": error.errno,
        }
    return {"signal": sig.name, "sent": True}


def stop_pty_child(pid: int, master: int | None, already_done: bool,
                   return_code: int | None) -> dict[str, Any]:
    previous_handler = signal.signal(signal.SIGINT, signal.SIG_IGN)
    signals_sent: list[str] = []
    signal_results: list[dict[str, Any]] = []
    done = already_done
    master_closed = master is None
    master_close_error = None
    try:
        if not done and master is not None:
            try:
                bytes_written = os.write(master, b"q")
                q_sent = bytes_written == 1
                q_result = {
                    "signal": "q",
                    "sent": q_sent,
                    "bytes_written": bytes_written,
                }
                if not q_sent:
                    q_result["error"] = "ShortWrite"
                if q_sent:
                    signals_sent.append("q")
            except OSError as error:
                q_result = {
                    "signal": "q",
                    "sent": False,
                    "error": type(error).__name__,
                    "errno": error.errno,
                }
            signal_results.append(q_result)

        if master is not None:
            try:
                os.close(master)
                master_closed = True
            except OSError as error:
                master_close_error = {
                    "error": type(error).__name__,
                    "errno": error.errno,
                }

        for sig, label, wait in ((None, None, 0.5), (signal.SIGTERM, "SIGTERM", 1.0),
                                 (signal.SIGKILL, "SIGKILL", 1.5)):
            if done:
                break
            if sig is not None:
                signal_result = kill_child(pid, sig)
                signal_results.append(signal_result)
                if signal_result["sent"]:
                    signals_sent.append(label)
            done, return_code = wait_child(pid, wait)

        result = {
            "pid": pid,
            "return_code": return_code,
            "signals_sent": signals_sent,
            "signal_results": signal_results,
            "reaped": done,
            "pid_absent": pid_absent(pid),
            "pty_master_closed": master_closed,
        }
        if master_close_error is not None:
            result["pty_master_close_error"] = master_close_error
        return result
    finally:
        signal.signal(signal.SIGINT, previous_handler)

def screen_text(screen: pyte.Screen) -> str:
    return "\n".join(screen.display)

def capture_tui(binary: str, working_directory: Path, arguments: list[str],
                raw_path: Path, display_path: Path, mode: str) -> dict[str, Any]:
    raw_path.parent.mkdir(parents=True, exist_ok=True)
    argv, started_utc, started = [binary, *arguments], utc_now(), time.monotonic()
    pid, master = pty.fork()
    if pid == 0:
        try:
            fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack("HHHH", HEIGHT, WIDTH, 0, 0))
            os.chdir(working_directory)
            os.execvpe(binary, argv, dict(os.environ, TERM="xterm-256color", LC_ALL="C"))
        except BaseException:
            os._exit(127)
    screen = pyte.Screen(WIDTH, HEIGHT)
    stream = pyte.Stream(screen)
    decoder = codecs.getincrementaldecoder("utf-8")("replace")
    deadline = started + (20.0 if mode == "d8" else 8.0)
    first_avg = panel_snapshot = child_return_code = None
    complete_panel_seen = complete_panel_complete = child_done = False
    application_initial_text = None
    application_initial_signature = None
    application_snapshot_count = 0
    cli_error_detected = False
    last_output_at = None
    signature_checked = False
    try:
        fcntl.ioctl(master, termios.TIOCSWINSZ, struct.pack("HHHH", HEIGHT, WIDTH, 0, 0))
        os.set_blocking(master, False)
        with raw_path.open("wb") as raw_file:
            while time.monotonic() < deadline:
                ready, _, _ = select.select(
                    [master], [], [], min(0.1, max(0.0, deadline - time.monotonic()))
                )
                if ready:
                    try:
                        data = os.read(master, 65536)
                    except OSError:
                        break
                    if not data:
                        break
                    raw_file.write(data)
                    raw_file.flush()
                    stream.feed(decoder.decode(data))
                    text, now = screen_text(screen), time.monotonic()
                    last_output_at = now
                    signature_checked = False
                    if mode == "d8" and first_avg is None:
                        match = AVG_PATTERN.search(text)
                        if match:
                            first_avg = {"avg_percent": int(match.group(1)),
                                         "latency_seconds": now-started, "screen_text": text}
                            break
                    if mode == "d11":
                        cli_error_detected = (
                            cli_error_detected or cli_error_output_detected(text)
                        )
                        panel_snapshot = complete_process_panel(text) or panel_snapshot
                        if panel_snapshot is not None:
                            complete_panel_seen = complete_panel_complete = True
                child_done, child_return_code = wait_child(pid, 0.0)
                if child_done:
                    break
                now = time.monotonic()
                if (mode == "d11" and last_output_at is not None
                    and not signature_checked
                    and now - last_output_at >= 0.2
                    and not decoder.getstate()[0]):
                    signature_checked = True
                    text = screen_text(screen)
                    cli_error_detected = (
                        cli_error_detected or cli_error_output_detected(text)
                    )
                    signature = (
                        application_render_signature(text)
                        if not cli_error_detected else None
                    )
                    if signature is not None:
                        application_initial_text = text
                        application_initial_signature = signature
                        application_snapshot_count = 1
                        break
    finally:
        tail = decoder.decode(b"", final=True)
        if tail:
            stream.feed(tail)
        cleanup = stop_pty_child(pid, master, child_done, child_return_code)
    final_text = screen_text(screen)
    cli_error_detected = cli_error_detected or (
        mode == "d11" and cli_error_output_detected(final_text)
    )
    application_rendered = (
        mode == "d11" and application_initial_signature is not None
        and not cli_error_detected
    )
    display_text = (
        application_initial_text if application_rendered else final_text
    )
    display_path.write_text(display_text + ("\n" if mode == "d8" else ""))
    result = {
        "argv": argv,
        "pid": pid,
        "started_utc": started_utc,
        "started_monotonic": started,
        "elapsed_seconds": time.monotonic()-started,
        "first_displayed_avg": first_avg,
        "complete_panel_seen": complete_panel_seen,
        "complete_panel_complete": complete_panel_complete,
        "process_panel_snapshot": panel_snapshot,
        "application_rendered": application_rendered,
        "application_initial_signature": (
            application_initial_signature if application_rendered else None
        ),
        "cli_error_output_detected": cli_error_detected,
        "application_render_snapshot_count": application_snapshot_count,
        "cleanup": cleanup,
        "screen_dimensions": {"rows": HEIGHT, "columns": WIDTH},
        "raw_path": str(raw_path),
        "display_path": str(display_path),
    }
    return result


def stop_workers(workers: list[subprocess.Popen[bytes]]) -> dict[str, Any]:
    previous_handler = signal.signal(signal.SIGINT, signal.SIG_IGN)
    try:
        for worker in workers:
            if worker.poll() is None:
                worker.terminate()
        deadline = time.monotonic() + 1.0
        while time.monotonic() < deadline and any(w.poll() is None for w in workers):
            time.sleep(0.02)
        for worker in workers:
            if worker.poll() is None:
                worker.kill()
        deadline = time.monotonic() + 1.0
        while time.monotonic() < deadline and any(w.poll() is None for w in workers):
            time.sleep(0.02)
        pids = [worker.pid for worker in workers]
        checks = [{"pid": pid, "absent": pid_absent(pid)} for pid in pids]
        return {"pids": pids, "return_codes": [w.poll() for w in workers],
                "all_reaped": all(w.poll() is not None for w in workers),
                "pid_checks": checks, "all_pids_absent": all(c["absent"] for c in checks)}
    finally:
        signal.signal(signal.SIGINT, previous_handler)
