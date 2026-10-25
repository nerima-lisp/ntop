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
import termios
import time
from pathlib import Path
from typing import Any

import pyte

from wp15b_d8_d11_display import (
    AVG_PATTERN, BOX_GLYPHS, CLI_ERROR_PATTERN, FRAME_WORDS, HEIGHT, WIDTH,
    application_render_signature, cli_error_output_detected, complete_process_panel,
    memory_arc_display_signature, structure_signature,
)

CPU_LOAD_INFO = 2
CPU_ACTIVE_STATES = (0, 1, 3)
MAX_LOAD_ATTEMPTS = 5

LIBSYSTEM = ctypes.CDLL("/usr/lib/libSystem.B.dylib")
U32 = ctypes.c_uint32
PTR_U32 = ctypes.POINTER(U32)
LIBSYSTEM.mach_host_self.restype = U32
LIBSYSTEM.mach_task_self.restype = U32
LIBSYSTEM.host_processor_info.argtypes = [
    U32, ctypes.c_int, PTR_U32, ctypes.POINTER(PTR_U32), PTR_U32,
]
LIBSYSTEM.host_processor_info.restype = ctypes.c_int
LIBSYSTEM.vm_deallocate.argtypes = [U32, ctypes.c_ulong, ctypes.c_ulong]
LIBSYSTEM.vm_deallocate.restype = ctypes.c_int


def utc_now() -> str:
    return dt.datetime.now(dt.timezone.utc).isoformat()


def write_json(path: Path, value: Any) -> None:
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n")


def cpu_counters() -> list[list[int]]:
    processor_count, info_count, info = U32(), U32(), PTR_U32()
    result = LIBSYSTEM.host_processor_info(
        LIBSYSTEM.mach_host_self(), CPU_LOAD_INFO, ctypes.byref(processor_count),
        ctypes.byref(info), ctypes.byref(info_count),
    )
    if result != 0:
        raise RuntimeError(f"host_processor_info returned {result}")
    values = [int(info[index]) for index in range(info_count.value)]
    address = ctypes.cast(info, ctypes.c_void_p).value
    deallocated = LIBSYSTEM.vm_deallocate(
        LIBSYSTEM.mach_task_self(), address, info_count.value * ctypes.sizeof(U32)
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


def kill_child(pid: int, sig: int) -> None:
    try:
        os.kill(pid, sig)
    except ProcessLookupError:
        pass


def stop_pty_child(pid: int, master: int | None, already_done: bool,
                   return_code: int | None) -> dict[str, Any]:
    previous_handler = signal.signal(signal.SIGINT, signal.SIG_IGN)
    signals_sent: list[str] = []
    done = already_done
    try:
        if not done and master is not None:
            try:
                os.write(master, b"q")
                signals_sent.append("q")
            except OSError:
                pass
        for sig, label, wait in ((None, None, 0.5), (signal.SIGTERM, "SIGTERM", 1.0),
                                 (signal.SIGKILL, "SIGKILL", 1.5)):
            if done:
                break
            if sig is not None:
                kill_child(pid, sig)
                signals_sent.append(label)
            done, return_code = wait_child(pid, wait)
        return {"pid": pid, "return_code": return_code, "signals_sent": signals_sent,
                "reaped": done, "pid_absent": pid_absent(pid)}
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
    screen, stream = pyte.Screen(WIDTH, HEIGHT), None
    stream = pyte.Stream(screen)
    decoder = codecs.getincrementaldecoder("utf-8")("replace")
    deadline = started + (20.0 if mode == "d8" else 8.0)
    first_avg = panel_snapshot = layout_signature = stable_since = raw_status = None
    application_layout_signature = application_stable_since = None
    complete_panel_seen = complete_panel_complete = child_done = False
    application_rendered = cli_error_detected = False
    stable_snapshot_count = application_snapshot_count = 0
    application_signature_poll_limit = 128
    application_signature_poll_count = application_signature_polls_dropped = 0
    application_signature_polls: list[dict[str, Any]] = []
    application_quiet_gate_check_count = application_quiet_gate_wait_count = 0
    application_decoder_pending_count = 0
    application_max_quiet_duration_seconds = 0.0
    application_current_quiet_duration_seconds = None
    application_final_stable_duration_seconds = 0.0
    application_diagnostic_observed_elapsed_seconds = 0.0
    last_output_at = None
    layout_stable_seconds = application_render_stable_seconds = 0.0
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
                        snapshot = complete_process_panel(text)
                        if snapshot:
                            complete_panel_seen, panel_snapshot = True, snapshot
                            if snapshot["layout_signature"] == layout_signature:
                                stable_snapshot_count += 1
                            else:
                                layout_signature = snapshot["layout_signature"]
                                stable_since, stable_snapshot_count = now, 1
                        else:
                            layout_signature, stable_since, stable_snapshot_count = None, None, 0

                child_done, raw_status = wait_child(pid, 0.0)
                if child_done:
                    break
                now = time.monotonic()
                if mode == "d11" and last_output_at is not None:
                    quiet_duration = max(0.0, now-last_output_at)
                    application_quiet_gate_check_count += 1
                    application_current_quiet_duration_seconds = quiet_duration
                    application_max_quiet_duration_seconds = max(
                        application_max_quiet_duration_seconds, quiet_duration
                    )
                    if quiet_duration < 0.2:
                        application_quiet_gate_wait_count += 1
                    elif decoder.getstate()[0]:
                        application_decoder_pending_count += 1
                    else:
                        text = screen_text(screen)
                        cli_error_detected = (
                            cli_error_detected or cli_error_output_detected(text)
                        )
                        app_snapshot = (
                            None if cli_error_detected
                            else application_render_signature(text)
                        )
                        previous_signature = application_layout_signature
                        if app_snapshot:
                            if app_snapshot == previous_signature:
                                application_snapshot_count += 1
                                reset_reason = "same-signature"
                            else:
                                application_layout_signature = app_snapshot
                                application_stable_since = now
                                application_snapshot_count = 1
                                reset_reason = (
                                    "signature-changed"
                                    if previous_signature is not None
                                    else "initial-signature"
                                )
                        else:
                            application_layout_signature = None
                            application_stable_since = None
                            application_snapshot_count = 0
                            reset_reason = (
                                "cli-error" if cli_error_detected
                                else "invalid-signature"
                            )
                        application_signature_poll_count += 1
                        stable_duration = (
                            max(0.0, now-application_stable_since)
                            if app_snapshot and application_stable_since is not None
                            else 0.0
                        )
                        if len(application_signature_polls) < application_signature_poll_limit:
                            application_signature_polls.append({
                                "poll_index": application_signature_poll_count,
                                "timestamp_utc": utc_now(),
                                "elapsed_seconds": now-started,
                                "signature_valid": app_snapshot is not None,
                                "signature_changed": (
                                    app_snapshot is not None
                                    and previous_signature is not None
                                    and app_snapshot != previous_signature
                                ),
                                "reset_reason": reset_reason,
                                "quiet_duration_seconds": quiet_duration,
                                "stable_duration_seconds": stable_duration,
                                "stable_seconds_required": 1.2,
                                "stable_seconds_remaining": max(
                                    0.0, 1.2-stable_duration
                                ),
                                "snapshot_count": application_snapshot_count,
                                "required_snapshot_count": 2,
                                "cli_error_output_detected": cli_error_detected,
                            })
                        else:
                            application_signature_polls_dropped += 1
                if (mode == "d11" and stable_since is not None and stable_snapshot_count >= 2
                    and now-stable_since >= 1.2 and last_output_at is not None
                    and now-last_output_at >= 0.2 and not decoder.getstate()[0]):
                    complete_panel_complete = True
                    layout_stable_seconds = now-stable_since
                if (mode == "d11" and application_stable_since is not None
                    and application_snapshot_count >= 2
                    and now-application_stable_since >= 1.2 and last_output_at is not None
                    and now-last_output_at >= 0.2 and not decoder.getstate()[0]):
                    application_rendered = True
                    application_render_stable_seconds = now-application_stable_since
                    break
    finally:
        observed_at = time.monotonic()
        application_diagnostic_observed_elapsed_seconds = observed_at-started
        if mode == "d11":
            if last_output_at is not None:
                application_current_quiet_duration_seconds = max(
                    0.0, observed_at-last_output_at
                )
                application_max_quiet_duration_seconds = max(
                    application_max_quiet_duration_seconds,
                    application_current_quiet_duration_seconds,
                )
            if application_stable_since is not None:
                application_final_stable_duration_seconds = max(
                    0.0, observed_at-application_stable_since
                )
        tail = decoder.decode(b"", final=True)
        if tail:
            stream.feed(tail)
        cleanup = stop_pty_child(pid, master, child_done, child_status(raw_status))
        try:
            os.close(master)
        except OSError:
            pass
    text = screen_text(screen)
    cli_error_detected = cli_error_detected or (
        mode == "d11" and cli_error_output_detected(text)
    )
    application_rendered = application_rendered and not cli_error_detected
    display_path.write_text(text + "\n")
    result = {"argv": argv, "pid": pid, "started_utc": started_utc,
              "started_monotonic": started, "elapsed_seconds": time.monotonic()-started,
              "first_displayed_avg": first_avg, "complete_panel_seen": complete_panel_seen,
              "complete_panel_complete": complete_panel_complete,
              "process_panel_snapshot": panel_snapshot,
              "layout_snapshot_count": stable_snapshot_count,
              "layout_stable_seconds": layout_stable_seconds,
              "application_rendered": application_rendered,
              "cli_error_output_detected": cli_error_detected,
              "application_render_snapshot_count": application_snapshot_count,
              "application_render_stable_seconds": application_render_stable_seconds,
              "cleanup": cleanup,
              "screen_dimensions": {"rows": HEIGHT, "columns": WIDTH},
              "raw_path": str(raw_path), "display_path": str(display_path)}
    if mode == "d11":
        result["application_signature_diagnostics"] = {
            "poll_count": application_signature_poll_count,
            "polls": application_signature_polls,
            "polls_dropped": application_signature_polls_dropped,
            "poll_limit": application_signature_poll_limit,
            "quiet_gate_check_count": application_quiet_gate_check_count,
            "quiet_gate_wait_count": application_quiet_gate_wait_count,
            "decoder_pending_count": application_decoder_pending_count,
            "maximum_quiet_duration_seconds": (
                application_max_quiet_duration_seconds
            ),
            "final_quiet_duration_seconds": (
                application_current_quiet_duration_seconds
            ),
            "final_stable_duration_seconds": (
                application_final_stable_duration_seconds
            ),
            "measurement_loop_elapsed_seconds": (
                application_diagnostic_observed_elapsed_seconds
            ),
            "quiet_gate_seconds_required": 0.2,
            "stable_seconds_required": 1.2,
            "required_snapshot_count": 2,
            "deadline_seconds": 8.0,
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
