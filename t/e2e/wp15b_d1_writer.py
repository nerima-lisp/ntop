"""Controlled writer and counter-boundary trace helpers."""

import errno
import fcntl
import math
import os
import signal
import time
from pathlib import Path

from wp15b_measure import write_json


WRITE_TRACE_FIELDS = (
    "write_sequence",
    "chunk",
    "call_in_chunk",
    "write_started_monotonic_ns",
    "write_completed_monotonic_ns",
    "write_started_clock_monotonic_ns",
    "write_completed_clock_monotonic_ns",
    "bytes_written",
    "cumulative_bytes_before",
    "cumulative_bytes_after",
)


def write_all(fd, data, chunk, cumulative_bytes, write_events):
    view = memoryview(data)
    call_in_chunk = 0
    while view:
        write_started_monotonic_ns = time.monotonic_ns()
        write_started_clock_monotonic_ns = time.clock_gettime_ns(
            time.CLOCK_MONOTONIC
        )
        try:
            bytes_written = os.write(fd, view)
        except InterruptedError:
            continue
        write_completed_clock_monotonic_ns = time.clock_gettime_ns(
            time.CLOCK_MONOTONIC
        )
        write_completed_monotonic_ns = time.monotonic_ns()
        if bytes_written <= 0:
            raise OSError(errno.EIO, "write returned no bytes")
        cumulative_before = cumulative_bytes
        cumulative_bytes += bytes_written
        write_events.append({
            "write_sequence": len(write_events) + 1,
            "chunk": chunk,
            "call_in_chunk": call_in_chunk + 1,
            "write_started_monotonic_ns": write_started_monotonic_ns,
            "write_completed_monotonic_ns": write_completed_monotonic_ns,
            "write_started_clock_monotonic_ns": write_started_clock_monotonic_ns,
            "write_completed_clock_monotonic_ns": write_completed_clock_monotonic_ns,
            "bytes_written": bytes_written,
            "cumulative_bytes_before": cumulative_before,
            "cumulative_bytes_after": cumulative_bytes,
        })
        view = view[bytes_written:]
        call_in_chunk += 1
    return cumulative_bytes


def writer_child(arguments):
    data_path, report_path, ready_path, trace_path = map(Path, arguments[:4])
    rate = int(arguments[4])
    write_trace_path = Path(arguments[5])
    fd = os.open(data_path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    running = True
    events = []
    write_events = []

    def request_stop(_signum, _frame):
        nonlocal running
        running = False

    chunk_bytes = 100_000
    period_ns = int(chunk_bytes * 1_000_000_000 / rate)
    try:
        signal.signal(signal.SIGTERM, request_stop)
        nocache_command = getattr(fcntl, "F_NOCACHE", None)
        if nocache_command is None:
            raise RuntimeError("Darwin F_NOCACHE is unavailable")
        fcntl.fcntl(fd, nocache_command, 1)
        started_clock_monotonic_ns = time.clock_gettime_ns(time.CLOCK_MONOTONIC)
        ready_path.write_text("ready\n")
        block = b"x" * chunk_bytes
        started = time.monotonic_ns()
        written = chunk_index = 0
        while running:
            deadline = started + chunk_index * period_ns
            delay_ns = deadline - time.monotonic_ns()
            if delay_ns > 0:
                time.sleep(delay_ns / 1_000_000_000)
            if not running:
                break
            write_started = time.monotonic_ns()
            written = write_all(
                fd, block, chunk_index + 1, written, write_events
            )
            write_completed = time.monotonic_ns()
            fsync_started = time.monotonic_ns()
            os.fsync(fd)
            fsync_completed = time.monotonic_ns()
            events.append((chunk_index + 1, write_started, write_completed,
                           fsync_started, fsync_completed, written))
            chunk_index += 1
        ended = time.monotonic_ns()
        ended_clock_monotonic_ns = time.clock_gettime_ns(time.CLOCK_MONOTONIC)
    finally:
        os.close(fd)

    trace_fields = ("chunk", "write_started_monotonic_ns",
                    "write_completed_monotonic_ns", "fsync_started_monotonic_ns",
                    "fsync_completed_monotonic_ns", "cumulative_bytes")
    trace_path.write_text(
        "\t".join(trace_fields) + "\n" + "".join(
            "\t".join(map(str, event)) + "\n" for event in events
        ),
        encoding="utf-8",
    )
    write_trace_path.write_text(
        "\t".join(WRITE_TRACE_FIELDS) + "\n" + "".join(
            "\t".join(str(event[field]) for field in WRITE_TRACE_FIELDS)
            + "\n"
            for event in write_events
        ),
        encoding="utf-8",
    )
    write_json(report_path, {
        "target_bytes_per_second": rate,
        "bytes_written": written,
        "started_monotonic_ns": started,
        "ended_monotonic_ns": ended,
        "clock_domain": "CLOCK_MONOTONIC",
        "clock_id": time.CLOCK_MONOTONIC,
        "started_clock_monotonic_ns": started_clock_monotonic_ns,
        "ended_clock_monotonic_ns": ended_clock_monotonic_ns,
        "duration_seconds": (ended - started) / 1_000_000_000,
        "chunk_count": len(events),
        "write_syscall_count": len(write_events),
        "io_settings": {
            "open_flags": ["O_WRONLY", "O_CREAT", "O_EXCL"],
            "file_mode": "0600",
            "f_nocache_command": nocache_command,
            "f_nocache_enabled": True,
            "fsync_per_chunk": True,
            "chunk_bytes": chunk_bytes,
            "period_ns": period_ns,
            "pacing": "absolute_from_start",
        },
    })


def read_writer_write_samples(trace_path):
    lines = trace_path.read_text(encoding="utf-8").splitlines()
    header = "\t".join(WRITE_TRACE_FIELDS)
    if not lines or lines[0] != header:
        raise RuntimeError("writer write-sample trace has an invalid header")
    samples = []
    previous_bytes = 0
    previous_clock_completed_ns = None
    previous_monotonic_completed_ns = None
    for line_number, line in enumerate(lines[1:], start=2):
        fields = line.split("\t")
        if len(fields) != len(WRITE_TRACE_FIELDS):
            raise RuntimeError(f"Invalid writer write-sample row {line_number}")
        event = dict(zip(WRITE_TRACE_FIELDS, map(int, fields)))
        if event["write_sequence"] != len(samples) + 1:
            raise RuntimeError(f"Invalid write sequence in row {line_number}")
        if event["cumulative_bytes_before"] != previous_bytes:
            raise RuntimeError(f"Discontinuous writer bytes in row {line_number}")
        if event["bytes_written"] <= 0 or (
            event["cumulative_bytes_after"] - previous_bytes
            != event["bytes_written"]
        ):
            raise RuntimeError(f"Invalid byte count in writer row {line_number}")
        if event["write_started_clock_monotonic_ns"] > (
            event["write_completed_clock_monotonic_ns"]
        ):
            raise RuntimeError(f"Invalid clock interval in writer row {line_number}")
        if event["write_started_monotonic_ns"] > (
            event["write_completed_monotonic_ns"]
        ):
            raise RuntimeError(f"Invalid monotonic interval in writer row {line_number}")
        if (previous_clock_completed_ns is not None
                and event["write_started_clock_monotonic_ns"]
                < previous_clock_completed_ns):
            raise RuntimeError(f"Out-of-order clock interval in writer row {line_number}")
        if (previous_monotonic_completed_ns is not None
                and event["write_started_monotonic_ns"]
                < previous_monotonic_completed_ns):
            raise RuntimeError(f"Out-of-order monotonic interval in writer row {line_number}")
        samples.append(event)
        previous_bytes = event["cumulative_bytes_after"]
        previous_clock_completed_ns = event["write_completed_clock_monotonic_ns"]
        previous_monotonic_completed_ns = event["write_completed_monotonic_ns"]
    return samples


def writer_byte_bounds_at(timestamp_ns, write_samples, session_started_ns,
                          session_ended_ns):
    if timestamp_ns < session_started_ns or timestamp_ns > session_ended_ns:
        return None
    cumulative_bytes = 0
    for event in write_samples:
        write_started_ns = event["write_started_clock_monotonic_ns"]
        write_completed_ns = event["write_completed_clock_monotonic_ns"]
        if timestamp_ns < write_started_ns:
            return cumulative_bytes, cumulative_bytes
        if timestamp_ns < write_completed_ns:
            return (
                event["cumulative_bytes_before"],
                event["cumulative_bytes_after"],
            )
        cumulative_bytes = event["cumulative_bytes_after"]
    return cumulative_bytes, cumulative_bytes


def _writer_bounds_for_timestamp(timestamp_ns, write_samples,
                                 session_started_ns, session_ended_ns):
    seconds = timestamp_ns / 1_000_000_000
    rounding_error_ns = math.ceil(math.ulp(float(seconds)) * 500_000_000 + 1)
    lower_timestamp_ns = timestamp_ns - rounding_error_ns
    upper_timestamp_ns = timestamp_ns + rounding_error_ns
    lower_bounds = writer_byte_bounds_at(
        lower_timestamp_ns, write_samples, session_started_ns, session_ended_ns
    )
    upper_bounds = writer_byte_bounds_at(
        upper_timestamp_ns, write_samples, session_started_ns, session_ended_ns
    )
    if lower_bounds is None or upper_bounds is None:
        return None
    return {
        "timestamp_error_bound_ns": rounding_error_ns,
        "writer_bytes_lower": lower_bounds[0],
        "writer_bytes_upper": upper_bounds[1],
    }


def writer_counter_interval_bounds(internal_samples, write_samples,
                                   writer_report):
    if (writer_report.get("clock_domain") != "CLOCK_MONOTONIC"
            or writer_report.get("clock_id") != time.CLOCK_MONOTONIC):
        return []
    started_ns = writer_report.get("started_clock_monotonic_ns")
    ended_ns = writer_report.get("ended_clock_monotonic_ns")
    if not isinstance(started_ns, int) or not isinstance(ended_ns, int):
        return []
    intervals = []
    for previous, current in zip(internal_samples, internal_samples[1:]):
        previous_at_ns = previous["sampled_at_monotonic_ns"]
        current_at_ns = current["sampled_at_monotonic_ns"]
        elapsed_ns = current_at_ns - previous_at_ns
        counter_delta = None
        if previous["write_bytes"] is not None and current["write_bytes"] is not None:
            counter_delta = current["write_bytes"] - previous["write_bytes"]
        interval = {
            "previous_sampled_at_monotonic_ns": previous_at_ns,
            "current_sampled_at_monotonic_ns": current_at_ns,
            "ntop_elapsed_ns": elapsed_ns,
            "ntop_write_delta_bytes": counter_delta,
            "available": False,
        }
        if elapsed_ns <= 0 or counter_delta is None or counter_delta < 0:
            interval["reason"] = "invalid-counter-interval"
            intervals.append(interval)
            continue
        start_bounds = _writer_bounds_for_timestamp(
            previous_at_ns, write_samples, started_ns, ended_ns
        )
        end_bounds = _writer_bounds_for_timestamp(
            current_at_ns, write_samples, started_ns, ended_ns
        )
        if start_bounds is None or end_bounds is None:
            interval["reason"] = "counter-boundary-outside-writer-clock-span"
            intervals.append(interval)
            continue
        lower_elapsed_ns = elapsed_ns - (
            start_bounds["timestamp_error_bound_ns"]
            + end_bounds["timestamp_error_bound_ns"]
        )
        upper_elapsed_ns = elapsed_ns + (
            start_bounds["timestamp_error_bound_ns"]
            + end_bounds["timestamp_error_bound_ns"]
        )
        if lower_elapsed_ns <= 0:
            interval["reason"] = "counter-interval-shorter-than-clock-error"
            intervals.append(interval)
            continue
        writer_bytes_lower = max(
            0,
            end_bounds["writer_bytes_lower"]
            - start_bounds["writer_bytes_upper"],
        )
        writer_bytes_upper = max(
            0,
            end_bounds["writer_bytes_upper"]
            - start_bounds["writer_bytes_lower"],
        )
        interval.update({
            "available": True,
            "ntop_bytes_per_second": counter_delta * 1_000_000_000 / elapsed_ns,
            "writer_bytes_lower": writer_bytes_lower,
            "writer_bytes_upper": writer_bytes_upper,
            "writer_bytes_per_second_lower": (
                writer_bytes_lower * 1_000_000_000 / upper_elapsed_ns
            ),
            "writer_bytes_per_second_upper": (
                writer_bytes_upper * 1_000_000_000 / lower_elapsed_ns
            ),
            "clock_domain": "CLOCK_MONOTONIC",
            "clock_id": time.CLOCK_MONOTONIC,
            "timestamp_error_bound_ns": max(
                start_bounds["timestamp_error_bound_ns"],
                end_bounds["timestamp_error_bound_ns"],
            ),
            "start_writer_byte_bounds": {
                "lower": start_bounds["writer_bytes_lower"],
                "upper": start_bounds["writer_bytes_upper"],
            },
            "end_writer_byte_bounds": {
                "lower": end_bounds["writer_bytes_lower"],
                "upper": end_bounds["writer_bytes_upper"],
            },
        })
        intervals.append(interval)
    return intervals
