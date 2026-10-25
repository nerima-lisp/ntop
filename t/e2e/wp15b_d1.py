#!/usr/bin/env python3
"""Capture displayed process write rates for a controlled D1 writer."""

import argparse
import errno
import json
import os
import re
import select
import signal
import statistics
import subprocess
import sys
import time
from pathlib import Path

from wp15b_measure import (
    PtySession,
    TerminalGrid,
    executable as resolve_executable,
    prepare_raw_dir,
    write_json,
)

ROWS, COLUMNS = 48, 160
RATE_MS, TARGET_BPS = 700, 1_000_000
SAMPLE_LIMIT, MAX_SECONDS, QUIET_SECONDS = 20, 90, 0.25
WRITER_TOKEN = "WP15B_D1_WRITER"
RATE_TOKEN = re.compile(
    r"(?<![\w.])(N/A|\d+(?:\.\d+)?(?:[KMGT]?B)?/s)(?![\w])", re.I
)


def load_average():
    return [float(value) for value in os.getloadavg()]


def write_all(fd, data):
    view = memoryview(data)
    while view:
        view = view[os.write(fd, view):]


def writer_child(arguments):
    data_path, report_path, ready_path = map(Path, arguments[:3])
    rate = int(arguments[3])
    fd = os.open(data_path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    running = True

    def request_stop(_signum, _frame):
        nonlocal running
        running = False

    signal.signal(signal.SIGTERM, request_stop)
    ready_path.write_text("ready\n")
    block = b"x" * 100_000
    period_ns = int(len(block) * 1_000_000_000 / rate)
    started = time.monotonic_ns()
    due = started
    written = 0
    try:
        while running:
            delay_ns = due - time.monotonic_ns()
            if delay_ns > 0:
                time.sleep(delay_ns / 1_000_000_000)
            if not running:
                break
            write_all(fd, block)
            written += len(block)
            due += period_ns
        ended = time.monotonic_ns()
        os.fsync(fd)
    finally:
        os.close(fd)
    write_json(report_path, {
        "target_bytes_per_second": rate,
        "bytes_written": written,
        "started_monotonic_ns": started,
        "ended_monotonic_ns": ended,
        "duration_seconds": (ended - started) / 1_000_000_000,
    })





def wait_bounded(child, seconds):
    deadline = time.monotonic() + seconds
    while child.poll() is None and time.monotonic() < deadline:
        time.sleep(0.05)
    return child.poll()


def signal_owned_group(pid, pgid, poll, signum):
    if poll() is not None:
        return False
    try:
        group = os.getpgid(pid)
    except ProcessLookupError:
        return False
    if group != pgid:
        raise RuntimeError(f"Refusing signal: PID {pid} changed process group")
    try:
        os.killpg(pgid, signum)
    except ProcessLookupError:
        return False
    return True


def stop_child(child, timeout=1.0, quit_pty=False):
    signals_sent = []
    try:
        if child.poll() is None and quit_pty:
            try:
                os.write(child.fd, b"q")
            except OSError as exc:
                if exc.errno not in (errno.EIO, errno.EBADF, errno.EAGAIN):
                    raise
            wait_bounded(child, timeout)
        if child.poll() is None:
            if signal_owned_group(
                child.pid, child.group_id, child.poll, signal.SIGTERM
            ):
                signals_sent.append("SIGTERM")
            wait_bounded(child, timeout)
        if child.poll() is None:
            if signal_owned_group(
                child.pid, child.group_id, child.poll, signal.SIGKILL
            ):
                signals_sent.append("SIGKILL")
            wait_bounded(child, timeout)
        return {
            "pid": child.pid,
            "pgid": child.group_id,
            "returncode": child.poll(),
            "signals_sent": signals_sent,
            "reaped": child.poll() is not None,
        }
    finally:
        os.close(child.fd)


def stop_writer(writer, pgid, timeout=1.0):
    signals_sent = []
    if writer.poll() is None:
        if signal_owned_group(writer.pid, pgid, writer.poll, signal.SIGTERM):
            signals_sent.append("SIGTERM")
        deadline = time.monotonic() + timeout
        while writer.poll() is None and time.monotonic() < deadline:
            time.sleep(0.05)
    if writer.poll() is None:
        if signal_owned_group(writer.pid, pgid, writer.poll, signal.SIGKILL):
            signals_sent.append("SIGKILL")
        deadline = time.monotonic() + timeout
        while writer.poll() is None and time.monotonic() < deadline:
            time.sleep(0.05)
    return {
        "pid": writer.pid,
        "pgid": pgid,
        "returncode": writer.poll(),
        "signals_sent": signals_sent,
        "reaped": writer.poll() is not None,
    }


def start_writer(raw_dir):
    link = raw_dir / WRITER_TOKEN
    os.symlink(sys.executable, link)
    script = str(Path(__file__).resolve())
    argv = [WRITER_TOKEN, script, "--writer-child",
            str(raw_dir / "writer.data"), str(raw_dir / "writer.json"),
            str(raw_dir / "writer.ready"), str(TARGET_BPS)]
    log = (raw_dir / "writer.log").open("wb")
    try:
        writer = subprocess.Popen(
            argv, executable=str(link), stdin=subprocess.DEVNULL,
            stdout=log, stderr=subprocess.STDOUT, start_new_session=True,
        )
    except BaseException:
        log.close()
        raise
    writer.pgid = writer.pid
    return writer, log, raw_dir / "writer.ready"


def wait_writer_ready(writer, ready_path):
    deadline = time.monotonic() + 2.0
    while time.monotonic() < deadline:
        if ready_path.exists():
            return
        if writer.poll() is not None:
            raise RuntimeError(f"Writer exited before ready: {writer.returncode}")
        time.sleep(0.02)
    raise TimeoutError("Writer did not become ready within 2 seconds")


def parse_rate(text):
    if text.upper() == "N/A":
        return None
    match = re.fullmatch(r"(\d+(?:\.\d+)?)([KMGT]?B)/s", text, re.I)
    if not match:
        return None
    scale = {"B": 1, "KB": 1_000, "MB": 1_000_000,
             "GB": 1_000_000_000, "TB": 1_000_000_000_000}
    return float(match.group(1)) * scale[match.group(2).upper()]


def displayed_writer_rate(grid, writer_pid):
    lines = tuple(line.rstrip() for line in grid.signature())
    header_index = next(
        (index for index, line in enumerate(lines) if "W/s" in line), None
    )
    if header_index is None:
        return None, "process-table-header-missing", {
            "writer_pid": writer_pid,
            "process_header": None,
        }

    header = lines[header_index]
    grouped = "Count" in header
    if "PID" not in header:
        return None, "process-pid-column-missing", {
            "writer_pid": writer_pid,
            "process_header": header.strip(),
            "grouped": grouped,
        }
    if "Command(n)" not in header:
        return None, "process-command-column-missing", {
            "writer_pid": writer_pid,
            "process_header": header.strip(),
            "visible_process_rows": [],
        }

    process_lines = []
    visible_rows = []
    table_lines = lines[header_index + 1:]
    for line in table_lines:
        if not line.strip():
            continue
        pid_match = re.match(r"\s*[^0-9]*?(\d+)(?=\s)", line)
        if pid_match and len(RATE_TOKEN.findall(line)) >= 2:
            pid = int(pid_match.group(1))
            process_lines.append((pid, line))
            visible_rows.append({
                "pid": pid,
                "writer_token_found": WRITER_TOKEN in line,
            })

    evidence = {
        "writer_pid": writer_pid,
        "process_header": header.strip(),
        "visible_process_rows": visible_rows,
    }
    writer_rows = [line for pid, line in process_lines if pid == writer_pid]
    if not writer_rows:
        token_rows = [line for line in table_lines if WRITER_TOKEN in line]
        if token_rows:
            evidence["writer_token_rows"] = token_rows
            return None, "writer-pid-unmatched", evidence
        return None, "writer-row-absent", evidence
    if len(writer_rows) != 1:
        evidence["writer_rows"] = writer_rows
        return None, "writer-pid-row-not-unique", evidence

    writer_row = writer_rows[0]
    evidence["writer_row"] = writer_row
    evidence["writer_command_token_found"] = WRITER_TOKEN in writer_row

    values = RATE_TOKEN.findall(writer_row)
    evidence["rate_values"] = values
    if len(values) < 2:
        return None, "read-write-rate-fields-missing", evidence
    rate_text = values[1]
    evidence["text"] = rate_text
    rate = parse_rate(rate_text)
    if rate is None:
        return None, "writer-rate-unavailable", evidence
    return rate, None, evidence


def start_ntop(ntop):
    argv = [
        ntop, "--rate", f"{RATE_MS}ms",
        "--default-widget-type", "process",
        "--default-widget-count", "1",
        "--process-default-sort", "w/s",
        "--process-command",
        "--tree",
    ]
    return PtySession(argv, ROWS, COLUMNS)


def collect_samples(ntop_child, writer, raw_dir, started):
    samples, observations = [], []
    transcript = bytearray()
    last_output = None
    last_sample_at = None
    last_signature = None
    pid_filter_sent = False
    deadline = started + MAX_SECONDS
    with (raw_dir / "ntop.ansi").open("wb") as raw:
        while len(samples) < SAMPLE_LIMIT and time.monotonic() < deadline:
            if ntop_child.poll() is not None:
                raise RuntimeError(f"ntop exited early with status {ntop_child.status}")
            if writer.poll() is not None:
                raise RuntimeError(f"Writer exited early with status {writer.returncode}")
            now = time.monotonic()
            timeout = min(0.1, max(0.0, deadline - now))
            if last_output is not None:
                quiet_at = last_output + QUIET_SECONDS
                if now < quiet_at:
                    timeout = min(timeout, quiet_at - now)
            if last_sample_at is not None:
                sample_at = last_sample_at + RATE_MS / 1000
                if now < sample_at:
                    timeout = min(timeout, sample_at - now)
            readable, _, _ = select.select([ntop_child.fd], [], [], timeout)
            if readable:
                try:
                    data = os.read(ntop_child.fd, 65536)
                except OSError as exc:
                    if exc.errno == errno.EIO:
                        break
                    raise
                if data:
                    raw.write(data)
                    raw.flush()
                    transcript.extend(data)
                    last_output = time.monotonic()
                else:
                    break
                continue
            now = time.monotonic()
            if last_output is None or now - last_output < QUIET_SECONDS:
                continue
            if (last_sample_at is not None
                    and now - last_sample_at < RATE_MS / 1000):
                continue
            last_sample_at = now
            grid = TerminalGrid(ROWS, COLUMNS)
            if not grid.feed(bytes(transcript)):
                observations.append({
                    "elapsed_seconds": now - started,
                    "valid": False,
                    "reason": "ansi-grid-invalid",
                    "writer_pid": writer.pid,
                    "terminal_frame_repeated": None,
                    "load_average": load_average(),
                })
                continue
            signature = grid.signature()
            terminal_frame_repeated = signature == last_signature
            last_signature = signature
            rate, reason, detail = displayed_writer_rate(grid, writer.pid)
            observation = {
                "elapsed_seconds": now - started,
                "valid": rate is not None,
                "writer_pid": writer.pid,
                "terminal_frame_repeated": terminal_frame_repeated,
                "process_header": detail.get("process_header"),
                "visible_process_rows": detail.get("visible_process_rows"),
                "writer_command_token_found": detail.get(
                    "writer_command_token_found"
                ),
                "writer_row": detail.get("writer_row"),
                "w_s_text": detail.get("text"),
                "w_s_bytes_per_second": rate,
                "reason": reason,
                "load_average": load_average(),
            }
            observations.append(observation)
            if (not pid_filter_sent
                    and detail.get("process_header")
                    and "PID" in detail["process_header"]):
                ntop_child.send(f"/pid = {writer.pid}\r".encode())
                pid_filter_sent = True
                observation["pid_filter_sent"] = True
            if rate is not None:
                observation["sample"] = len(samples) + 1
                observation["within_target_10_percent"] = (
                    TARGET_BPS * 0.9 <= rate <= TARGET_BPS * 1.1
                )
                samples.append(observation.copy())
    return samples, observations


def run_d1(ntop, raw_dir):
    result = {
        "measurement": "D1",
        "status": "running",
        "settings": {
            "rows": ROWS,
            "columns": COLUMNS,
            "process_view": "tree",
            "rate_ms": RATE_MS,
            "target_bytes_per_second": TARGET_BPS,
            "samples_requested": SAMPLE_LIMIT,
            "max_seconds": MAX_SECONDS,
        },
        "raw": {
            "pty": "ntop.ansi",
            "writer_data": "writer.data",
            "writer_report": "writer.json",
            "writer_log": "writer.log",
        },
    }
    output = raw_dir / "d1.json"
    write_json(output, result)
    result["load_before"] = load_average()
    writer, writer_log, ready_path = start_writer(raw_dir)
    ntop_child = None
    samples, observations = [], []
    try:
        wait_writer_ready(writer, ready_path)
        result["writer_process"] = {"pid": writer.pid, "pgid": writer.pgid}
        ntop_child = start_ntop(ntop)
        result["ntop_process"] = {"pid": ntop_child.pid, "pgid": ntop_child.group_id}
        started = time.monotonic()
        samples, observations = collect_samples(
            ntop_child, writer, raw_dir, started
        )
    except Exception as exc:
        result["error"] = f"{type(exc).__name__}: {exc}"
        raise
    finally:
        try:
            if ntop_child is not None:
                result["ntop_cleanup"] = stop_child(ntop_child, quit_pty=True)
        finally:
            try:
                result["writer_cleanup"] = stop_writer(writer, writer.pgid)
            finally:
                writer_log.close()
                result["load_after"] = load_average()
                result["samples"] = samples
                result["observations"] = observations
                write_json(output, result)
    report_path = raw_dir / "writer.json"
    writer_path = raw_dir / "writer.data"
    if report_path.exists():
        writer_report = json.loads(report_path.read_text())
        actual_bytes = writer_path.stat().st_size
        duration = writer_report["duration_seconds"]
        actual_rate = actual_bytes / duration if duration > 0 else None
        result["writer"] = {
            **writer_report,
            "file_size_bytes": actual_bytes,
            "file_size_matches_child_count": actual_bytes == writer_report["bytes_written"],
            "actual_bytes_per_second": actual_rate,
        }
    else:
        result["writer"] = {"report_missing": True}
        actual_rate = None
    displayed = [sample["w_s_bytes_per_second"] for sample in samples]
    median = statistics.median(displayed) if displayed else None
    writer_in_range = (
        actual_rate is not None and TARGET_BPS * 0.9 <= actual_rate <= TARGET_BPS * 1.1
    )
    file_matches = result["writer"].get("file_size_matches_child_count", False)
    cleanup_complete = (
        result.get("ntop_cleanup", {}).get("reaped", False)
        and result.get("writer_cleanup", {}).get("reaped", False)
    )
    displayed_in_range = (
        median is not None and TARGET_BPS * 0.9 <= median <= TARGET_BPS * 1.1
    )
    complete = len(samples) == SAMPLE_LIMIT
    passed = complete and writer_in_range and displayed_in_range
    passed = passed and file_matches and cleanup_complete
    result["summary"] = {
        "valid_sample_count": len(samples),
        "median_displayed_bytes_per_second": median,
        "minimum_displayed_bytes_per_second": min(displayed) if displayed else None,
        "maximum_displayed_bytes_per_second": max(displayed) if displayed else None,
        "displayed_samples_in_10_percent_range": sum(
            sample["within_target_10_percent"] for sample in samples
        ),
        "writer_rate_within_10_percent": writer_in_range,
        "median_display_within_10_percent": displayed_in_range,
        "writer_file_count_matches": file_matches,
        "owned_children_reaped": cleanup_complete,
    }
    result["acceptance"] = {
        "target_bytes_per_second": TARGET_BPS,
        "tolerance_percent": 10,
        "requires_20_valid_display_samples": True,
        "passed": passed,
    }
    result["status"] = "pass" if passed else "fail" if complete else "incomplete"
    if not complete:
        result["incomplete_reason"] = (
            f"Collected {len(samples)} valid W/s samples before {MAX_SECONDS}s limit"
        )
    write_json(output, result)
    return result


def main(argv=None):
    parser = argparse.ArgumentParser(
        description="Measure displayed W/s for a controlled 1,000,000 B/s writer."
    )
    parser.add_argument("--ntop", required=True, help="Built native ntop executable")
    parser.add_argument(
        "--raw-dir",
        help="New temporary evidence directory with an ntop-wp15b prefix",
    )
    args = parser.parse_args(argv)
    try:
        temporary_root = Path(os.sep) / "tmp"
        raw_value = args.raw_dir or str(
            temporary_root / f"ntop-wp15b-d1-{os.getpid()}-{time.time_ns()}"
        )
        raw_dir = prepare_raw_dir(raw_value)
        ntop = resolve_executable(args.ntop)
        result = run_d1(ntop, raw_dir)
        print(json.dumps({"status": result["status"], "raw_dir": str(raw_dir)}))
        return 0 if result["status"] == "pass" else 1
    except (OSError, RuntimeError, TimeoutError, ValueError,
            subprocess.SubprocessError) as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--writer-child":
        writer_child(sys.argv[2:])
    else:
        raise SystemExit(main())
