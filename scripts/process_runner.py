import os
import signal
import subprocess
from collections.abc import Mapping, Sequence
from typing import Optional


class ProcessTimeoutError(TimeoutError):
    pass


def _signal_group(process: subprocess.Popen, sig: int) -> None:
    try:
        os.killpg(process.pid, sig)
    except ProcessLookupError:
        pass


def _stop_group(
    process: subprocess.Popen,
    grace_seconds: float,
    capture_output: bool,
) -> None:
    _signal_group(process, signal.SIGTERM)
    try:
        if capture_output:
            process.communicate(timeout=grace_seconds)
        else:
            process.wait(timeout=grace_seconds)
    except subprocess.TimeoutExpired:
        _signal_group(process, signal.SIGKILL)
        if capture_output:
            process.communicate()
        else:
            process.wait()


def run_bounded(
    command: Sequence[str],
    *,
    timeout_seconds: float,
    kill_after_seconds: float = 15,
    cwd: Optional[str] = None,
    env: Optional[Mapping[str, str]] = None,
    capture_output: bool = False,
) -> subprocess.CompletedProcess:
    if os.name != "posix":
        raise RuntimeError("Process-group timeouts require a POSIX host.")
    if not command:
        raise ValueError("A command is required.")
    if timeout_seconds <= 0 or kill_after_seconds < 0:
        raise ValueError("Timeouts must be positive and the kill grace non-negative.")

    process = subprocess.Popen(
        command,
        cwd=cwd,
        env=env,
        start_new_session=True,
        stdout=subprocess.PIPE if capture_output else None,
        stderr=subprocess.PIPE if capture_output else None,
        text=capture_output,
    )
    try:
        if capture_output:
            stdout, stderr = process.communicate(timeout=timeout_seconds)
        else:
            stdout = None
            stderr = None
            process.wait(timeout=timeout_seconds)
    except subprocess.TimeoutExpired as condition:
        _stop_group(process, kill_after_seconds, capture_output)
        raise ProcessTimeoutError(
            f"{command[0]} exceeded its {timeout_seconds:g}s time limit."
        ) from condition
    except KeyboardInterrupt:
        _stop_group(process, kill_after_seconds, capture_output)
        raise

    return subprocess.CompletedProcess(command, process.returncode, stdout, stderr)
