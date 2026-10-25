#!/usr/bin/env python3
"""Capture displayed process write rates for a controlled D1 writer."""

import argparse
import errno
import json
import os
import signal
import statistics
import subprocess
import sys
import time
from pathlib import Path

import wp15b_d1_display as d1_display
from wp15b_d1_darwin import RusageCaptureClock, attach_rusage_samples
from wp15b_measure import (
    PtySession,
    TerminalGrid,
    executable as resolve_executable,
    prepare_raw_dir,
    write_json,
)

from wp15b_d1_display import (
    COLUMNS,
    MAX_SECONDS,
    QUIET_SECONDS,
    RATE_MS,
    RATE_TOKEN,
    ROWS,
    SAMPLE_LIMIT,
    TARGET_BPS,
    WRITER_TOKEN,
    collect_btm_samples,
    collect_samples,
    displayed_writer_rate,
    load_average,
    parse_rate,
    start_btm,
    start_ntop,
)


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
    chunk_bytes = 100_000
    block = b"x" * chunk_bytes
    period_ns = int(chunk_bytes * 1_000_000_000 / rate)
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
            written += chunk_bytes
            due = time.monotonic_ns() + period_ns
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


def process_exists(value, group=False):
    try:
        (os.killpg if group else os.kill)(value, 0)
        return True
    except ProcessLookupError:
        return False
    except PermissionError:
        return True


def stop_child(child, timeout=1.0, quit_pty=False):
    cleanup_deadline = time.monotonic() + 3 * timeout
    signals_sent = []
    if child.poll() is None and quit_pty:
        try:
            os.write(child.fd, b"q")
        except OSError as exc:
            if exc.errno not in (errno.EIO, errno.EBADF, errno.EAGAIN):
                raise
        wait_bounded(child, timeout)
    try:
        os.close(child.fd)
    finally:
        child.fd = -1
    if child.poll() is None and signal_owned_group(
        child.pid, child.group_id, child.poll, signal.SIGTERM
    ):
        signals_sent.append("SIGTERM")
    wait_bounded(child, timeout)
    if child.poll() is None and signal_owned_group(
        child.pid, child.group_id, child.poll, signal.SIGKILL
    ):
        signals_sent.append("SIGKILL")
    wait_bounded(child, timeout)
    while (process_exists(child.group_id, group=True)
           and time.monotonic() < cleanup_deadline):
        child.poll()
        time.sleep(min(0.05, cleanup_deadline - time.monotonic()))
    return {
        "pid": child.pid,
        "pgid": child.group_id,
        "returncode": child.poll(),
        "signals_sent": signals_sent,
        "reaped": child.poll() is not None,
        "pid_absent": not process_exists(child.pid),
        "pgid_absent": not process_exists(child.group_id, group=True),
    }


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


def start_ntop_with_sample_trace(binary, raw_dir, writer_pid):
    trace_path = raw_dir / "ntop-internal-samples.tsv"
    trace_path.write_text(
        "sampled_at_monotonic_ns\tpid\tread_bytes\twrite_bytes\n",
        encoding="utf-8",
    )
    names = ("NTOP_D1_SAMPLE_TRACE_PATH", "NTOP_D1_SAMPLE_TRACE_PID")
    previous = {name: os.environ.get(name) for name in names}
    os.environ[names[0]] = str(trace_path)
    os.environ[names[1]] = str(writer_pid)
    try:
        child = start_ntop(binary)
    finally:
        for name, value in previous.items():
            if value is None:
                os.environ.pop(name, None)
            else:
                os.environ[name] = value
    return child, trace_path


def read_internal_samples(trace_path, expected_pid):
    lines = trace_path.read_text(encoding="utf-8").splitlines()
    header = "sampled_at_monotonic_ns\tpid\tread_bytes\twrite_bytes"
    if not lines or lines[0] != header:
        raise RuntimeError("ntop internal sample trace has an invalid header")
    samples = []
    for line_number, line in enumerate(lines[1:], start=2):
        fields = line.split("\t")
        if len(fields) != 4:
            raise RuntimeError(f"Invalid ntop sample trace row {line_number}")
        sampled_at_ns, pid = map(int, fields[:2])
        if pid != expected_pid:
            raise RuntimeError(f"Unexpected PID in ntop sample trace row {line_number}")
        samples.append({
            "sampled_at_monotonic_ns": sampled_at_ns,
            "pid": pid,
            "read_bytes": None if fields[2] == "N/A" else int(fields[2]),
            "write_bytes": None if fields[3] == "N/A" else int(fields[3]),
        })
    return samples


def run_d1(binary, raw_dir, backend="ntop"):
    if backend not in ("ntop", "btm"):
        raise ValueError(f"Unsupported D1 backend: {backend}")
    is_btm = backend == "btm"
    result = {
        "measurement": "D1",
        "status": "running",
        "settings": {
            "rows": ROWS,
            "columns": COLUMNS,
            "process_view": "ungrouped" if is_btm else "tree",
            "rate_ms": RATE_MS,
            "target_bytes_per_second": TARGET_BPS,
            "samples_requested": SAMPLE_LIMIT,
            "max_seconds": MAX_SECONDS,
        },
        "raw": {
            "pty": "btm.ansi" if is_btm else "ntop.ansi",
            "writer_data": "writer.data",
            "writer_report": "writer.json",
            "writer_log": "writer.log",
        },
    }
    if is_btm:
        result["settings"]["backend"] = backend
        result["raw"]["btm_config"] = "btm.toml"
    else:
        result["raw"]["internal_process_samples"] = "ntop-internal-samples.tsv"
    output = raw_dir / "d1.json"
    write_json(output, result)
    result["load_before"] = load_average()
    writer, writer_log, ready_path = start_writer(raw_dir)
    display_child = None
    rusage_clock = None
    internal_samples_path = None
    samples, observations = [], []
    try:
        wait_writer_ready(writer, ready_path)
        result["writer_process"] = {"pid": writer.pid, "pgid": writer.pgid}
        if is_btm:
            display_child = start_btm(binary, raw_dir)
        else:
            display_child, internal_samples_path = start_ntop_with_sample_trace(
                binary, raw_dir, writer.pid
            )
        result[f"{backend}_process"] = {
            "pid": display_child.pid,
            "pgid": display_child.group_id,
        }
        started = time.monotonic()
        collect = collect_btm_samples if is_btm else collect_samples
        rusage_clock = RusageCaptureClock(writer.pid)
        original_display_time = d1_display.time
        d1_display.time = rusage_clock
        try:
            samples, observations = collect(
                display_child, writer, raw_dir, started
            )
        finally:
            d1_display.time = original_display_time
    except Exception as exc:
        result["error"] = f"{type(exc).__name__}: {exc}"
        raise
    finally:
        try:
            if display_child is not None:
                result[f"{backend}_cleanup"] = stop_child(
                    display_child, quit_pty=True
                )
        finally:
            try:
                result["writer_cleanup"] = stop_writer(writer, writer.pgid)
            finally:
                writer_log.close()
                result["load_after"] = load_average()
                rusage_samples = rusage_clock.samples if rusage_clock else []
                result["diagnostics"] = {
                    "darwin_proc_pid_rusage_v2": attach_rusage_samples(
                        observations, rusage_samples, samples
                    )
                }
                if internal_samples_path is not None:
                    result["internal_process_samples"] = read_internal_samples(
                        internal_samples_path, writer.pid
                    )
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
        result.get(f"{backend}_cleanup", {}).get("reaped", False)
        and result.get("writer_cleanup", {}).get("reaped", False)
        and result.get(f"{backend}_cleanup", {}).get("pid_absent", False)
        and result.get(f"{backend}_cleanup", {}).get("pgid_absent", False)
    )
    rusage_ok = (
        result["diagnostics"]["darwin_proc_pid_rusage_v2"]["status"] == "pass"
    )
    internal_samples = result.get("internal_process_samples", [])
    internal_trace_ok = is_btm or (
        len(internal_samples) >= 2
        and sum(sample["write_bytes"] is not None for sample in internal_samples) >= 2
    )
    displayed_in_range = (
        median is not None and TARGET_BPS * 0.9 <= median <= TARGET_BPS * 1.1
    )
    complete = len(samples) == SAMPLE_LIMIT
    acceptance_passed = complete and writer_in_range and displayed_in_range
    acceptance_passed = acceptance_passed and file_matches and cleanup_complete
    passed = acceptance_passed and rusage_ok and internal_trace_ok
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
        "darwin_proc_pid_rusage_v2_passed": rusage_ok,
        "internal_sample_trace_passed": internal_trace_ok,
        "internal_process_sample_count": len(internal_samples),
    }
    result["acceptance"] = {
        "target_bytes_per_second": TARGET_BPS,
        "tolerance_percent": 10,
        "requires_20_valid_display_samples": True,
        "passed": acceptance_passed,
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
    display = parser.add_mutually_exclusive_group(required=True)
    display.add_argument("--ntop", help="Built native ntop executable")
    display.add_argument("--btm", help="btm 0.14.9 executable for reference measurement")
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
        backend = "btm" if args.btm else "ntop"
        binary = resolve_executable(args.btm or args.ntop)
        result = run_d1(binary, raw_dir, backend=backend)
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
