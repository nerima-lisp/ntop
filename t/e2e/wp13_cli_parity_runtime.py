"""PTY and default-config execution helpers for the WP13 CLI comparison."""
from __future__ import annotations

import hashlib
import os
import stat
import tempfile
from pathlib import Path
from typing import Any

from wp15b_measure_support import PtySession, TerminalGrid

ROOT = Path(__file__).resolve().parents[2]
DEFAULT_CONFIG = ROOT / "src/cli/default-config.toml"
CONFIG_PATH_PLACEHOLDER = "<temporary-config-path>"
DEFAULT_CONFIG_SHA256 = "a079bd2f3bab33ca20079e2b159a9ad85cfec32feb4abed5c4f053f83d8b13e9"
PTY_ROWS, PTY_COLUMNS = 24, 80
PROCESS_TIMEOUT_SECONDS, PTY_Q_GRACE_SECONDS = 8.0, 5.0
PTY_OBSERVATION_SECONDS = 3.5
MAX_RETAINED_OUTPUT_BYTES = 8192


class CountingSink:
    def __init__(self):
        self.retained = bytearray()
        self.session: PtySession | None = None

    def write(self, data: bytes) -> int:
        self.retained.extend(
            data[:max(0, MAX_RETAINED_OUTPUT_BYTES - len(self.retained))]
        )
        if self.session is not None and b"\x1b[6n" in data:
            self.session.send(b"\x1b[1;1R")
        return len(data)

    def visible_cells(self) -> int:
        screen = TerminalGrid(PTY_ROWS, PTY_COLUMNS)
        screen.feed(bytes(self.retained))
        return sum(cell != " " for row in screen.cells for cell in row)

    def flush(self) -> None:
        pass


def run_tui(binary: str, argv: tuple[str, ...]) -> dict[str, Any]:
    sink = CountingSink()
    session: PtySession | None = None
    observation_error: str | None = None
    q_sent = False
    status_before_cleanup: int | None = None
    cleanup: dict[str, Any] = {"verified": False, "errors": []}
    screen_drawn = False
    try:
        session = PtySession([binary, *argv], PTY_ROWS, PTY_COLUMNS)
        sink.session = session
        try:
            session.read_idle(
                sink,
                timeout=PROCESS_TIMEOUT_SECONDS,
                first_not_before=session.started + PTY_OBSERVATION_SECONDS,
            )
        except (RuntimeError, TimeoutError, OSError) as error:
            observation_error = type(error).__name__
        screen_drawn = sink.visible_cells() > 0
        if session.poll() is None:
            try:
                session.send(b"q")
                q_sent = True
                session.read_for(PTY_Q_GRACE_SECONDS, sink)
            except RuntimeError:
                pass
            except OSError as error:
                observation_error = observation_error or type(error).__name__
        status_before_cleanup = session.poll()
    except OSError as error:
        observation_error = type(error).__name__
    finally:
        if session is not None:
            cleanup = session.close()
    return {
        "exit_status": status_before_cleanup,
        "timed_out": observation_error == "TimeoutError"
        or (q_sent and status_before_cleanup is None),
        "execution_error": {"type": observation_error}
        if observation_error
        else None,
        "q_sent": q_sent,
        "screen_drawn": screen_drawn,
        "cleanup_verified": bool(cleanup.get("verified")),
    }


def _run_config(
    binary: str,
    argv: tuple[str, ...],
    directory: str,
    expected_bytes: bytes,
    expected_mode: int,
) -> dict[str, Any]:
    path = Path(directory) / "config.toml"
    actual_argv = tuple(
        str(path) if value == CONFIG_PATH_PLACEHOLDER else value
        for value in argv
    )
    result = run_tui(binary, actual_argv)
    file_result: dict[str, Any] = {
        "exists": path.is_file(),
        "content_match": False,
        "mode": None,
        "expected_mode": f"{expected_mode:04o}",
        "mode_match": False,
        "size": None,
        "sha256": None,
    }
    try:
        data = path.read_bytes()
        mode = stat.S_IMODE(path.stat().st_mode)
        file_result.update(
            {
                "content_match": data == expected_bytes,
                "mode": f"{mode:04o}",
                "mode_match": mode == expected_mode,
                "size": len(data),
                "sha256": hashlib.sha256(data).hexdigest(),
            }
        )
    except OSError as error:
        file_result["inspection_error"] = type(error).__name__
    result["config_file"] = file_result
    return result


def run_config_case(
    argv: tuple[str, ...], ntop: str, btm: str
) -> tuple[dict[str, Any], dict[str, Any], bool]:
    expected_bytes = DEFAULT_CONFIG.read_bytes()
    digest = hashlib.sha256(expected_bytes).hexdigest()
    if digest != DEFAULT_CONFIG_SHA256:
        raise ValueError(f"default config resource checksum mismatch: {digest}")
    inherited_umask = os.umask(0)
    os.umask(inherited_umask)
    expected_mode = 0o666 & ~inherited_umask
    with tempfile.TemporaryDirectory(prefix="ntop-parity-ntop-") as ntop_dir, \
            tempfile.TemporaryDirectory(prefix="ntop-parity-btm-") as btm_dir:
        ntop_result = _run_config(
            ntop, argv, ntop_dir, expected_bytes, expected_mode
        )
        btm_result = _run_config(
            btm, argv, btm_dir, expected_bytes, expected_mode
        )
    files_match = all(
        result["config_file"][key]
        for result in (ntop_result, btm_result)
        for key in ("exists", "content_match", "mode_match")
    )
    return ntop_result, btm_result, files_match
