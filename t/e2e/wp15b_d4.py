#!/usr/bin/env python3
"""Compare WindowServer User and Time-derived start time with macOS ps."""

import argparse
import json
import math
import os
import re
import subprocess
import sys
import time
from pathlib import Path

from wp15b_d1 import (
    ROWS,
    prepare_raw_dir,
    resolve_executable,
    stop_child,
    wait_bounded,
    write_json,
)
from wp15b_measure_support import PtySession, TerminalGrid


RATE_MS = 700
CAPTURE_INTERVAL_SECONDS = RATE_MS / 1000
CAPTURE_SECONDS = 60.0
D4_COLUMNS = 320
PS_START_TIME_RESOLUTION_SECONDS = 1.0
PS_COMMAND = ["ps", "-o", "user=,lstart="]


def start_ntop(ntop):
    return PtySession(
        [
            ntop,
            "--rate",
            f"{RATE_MS}ms",
            "--default-widget-type",
            "process",
            "--default-widget-count",
            "1",
            "--process-default-sort",
            "cpu%",
            "--tree",
        ],
        ROWS,
        D4_COLUMNS,
    )


def process_snapshot(pid):
    environment = os.environ.copy()
    environment["LC_ALL"] = "C"
    completed = subprocess.run(
        [*PS_COMMAND, "-p", str(pid)],
        capture_output=True,
        text=True,
        timeout=10,
        check=True,
        env=environment,
    )
    lines = [line.strip() for line in completed.stdout.splitlines() if line.strip()]
    if len(lines) != 1:
        raise RuntimeError(
            f"ps returned {len(lines)} rows for the WindowServer PID"
        )
    fields = lines[0].split(None, 1)
    if len(fields) != 2:
        raise RuntimeError("ps output did not contain User and lstart")
    return {
        "user": fields[0],
        "start_time": fields[1],
    }, completed.stdout


def parse_display_elapsed(value):
    value = value.strip()
    match = re.fullmatch(r"(\d+)d\s+(\d+)h\s+(\d+)m", value)
    if match:
        days, hours, minutes = map(int, match.groups())
        return (days * 86400 + hours * 3600 + minutes * 60, 60.0)
    match = re.fullmatch(r"(\d+)h\s+(\d+)m\s+(\d+)s", value)
    if match:
        hours, minutes, seconds = map(int, match.groups())
        return (hours * 3600 + minutes * 60 + seconds, 1.0)
    match = re.fullmatch(r"(\d+)m\s+(\d+(?:\.\d+)?)s", value)
    if match:
        minutes, seconds = match.groups()
        return (int(minutes) * 60 + float(seconds), 0.01)
    match = re.fullmatch(r"(\d+(?:\.\d+)?)s", value)
    if match:
        seconds = match.group(1)
        decimals = len(seconds.partition(".")[2])
        return (float(seconds), 10 ** -decimals)
    return None



def parse_ps_start_time(value):
    match = re.fullmatch(
        r"[A-Za-z]{3}\s+(?P<month>[A-Za-z]{3})\s+(?P<day>\d{1,2})\s+"
        r"(?P<hour>\d{2}):(?P<minute>\d{2}):(?P<second>\d{2})\s+"
        r"(?P<year>\d{4})",
        value.strip(),
    )
    if match is None:
        return None
    month = {
        "Jan": 1,
        "Feb": 2,
        "Mar": 3,
        "Apr": 4,
        "May": 5,
        "Jun": 6,
        "Jul": 7,
        "Aug": 8,
        "Sep": 9,
        "Oct": 10,
        "Nov": 11,
        "Dec": 12,
    }.get(match.group("month"))
    if month is None:
        return None
    return time.mktime((
        int(match.group("year")),
        month,
        int(match.group("day")),
        int(match.group("hour")),
        int(match.group("minute")),
        int(match.group("second")),
        0,
        0,
        -1,
    ))








def process_table_cells(grid):
    lines = grid.signature()
    header_index = None
    starts = None
    for index, line in enumerate(lines):
        pid_start = line.find("PID(p)")
        name_start = line.find("Name(n)")
        cpu_start = line.find("CPU%(c)")
        user_start = line.find("User")
        state_start = line.find("State")
        time_start = line.find("Time")
        priority_start = line.find("Priority")
        if (pid_start >= 0 and name_start > pid_start and cpu_start > name_start
                and user_start > cpu_start and state_start > user_start
                and time_start > state_start):
            header_index = index
            starts = (
                pid_start,
                name_start,
                cpu_start,
                user_start,
                state_start,
                time_start,
                priority_start,
            )
            break
    if header_index is None or starts is None:
        return {}, "process table header with PID, Name, CPU, User, State, and Time was not visible"

    pid_start, name_start, cpu_start, user_start, state_start, time_start, priority_start = starts
    time_end = priority_start if priority_start > time_start else len(lines[header_index])
    displayed = {}
    for line in lines[header_index + 1:]:
        pid_text = line[pid_start:name_start].strip()
        if not re.fullmatch(r"\d+", pid_text):
            continue
        pid = int(pid_text)
        name = line[name_start:cpu_start].strip().lstrip("│├└─ ")
        displayed[pid] = {
            "name": name,
            "user": line[user_start:state_start].strip(),
            "time": line[time_start:time_end].strip(),
            "row": line.rstrip(),
        }
    return displayed, None


def run_d4(ntop, raw_dir):
    output = raw_dir / "d4.json"
    result = {
        "measurement": "D4",
        "status": "FAIL",
        "settings": {
            "rows": ROWS,
            "columns": D4_COLUMNS,
            "rate_ms": RATE_MS,
            "capture_seconds": CAPTURE_SECONDS,
        },
        "target": "WindowServer",
        "comparator": {
            "command": "ps -o user=,lstart= -p <pid>",
            "user_field": "user",
            "start_time_field": "lstart",
            "start_time_resolution_seconds": PS_START_TIME_RESOLUTION_SECONDS,
            "tolerance_rationale": (
                "ceil(Time display truncation interval + 0.7-second render "
                "interval + 1-second ps lstart resolution)"
            ),
        },
        "raw": {"pty": "ntop.ansi", "ps_output": "d4.json"},
    }
    write_json(output, result)
    ntop_child = None
    try:
        ntop_child = start_ntop(ntop)
        result["ntop_process"] = {"pid": ntop_child.pid, "pgid": ntop_child.group_id}
        raw_path = raw_dir / "ntop.ansi"
        capture_started = time.monotonic()
        screen_observed_at = None
        screen_valid = False
        screen_error = None
        displayed = {}
        with raw_path.open("wb") as raw:
            while time.monotonic() - capture_started < CAPTURE_SECONDS:
                remaining = CAPTURE_SECONDS - (time.monotonic() - capture_started)
                ntop_child.read_for(
                    min(CAPTURE_INTERVAL_SECONDS, remaining), raw
                )
                grid = TerminalGrid(ROWS, D4_COLUMNS)
                screen_valid = grid.feed(raw_path.read_bytes())
                displayed, screen_error = process_table_cells(grid)
                screen_observed_at = time.time()
                if (screen_valid and screen_error is None
                        and any(row["name"] == "WindowServer"
                                for row in displayed.values())):
                    break
        result["capture_elapsed_seconds"] = time.monotonic() - capture_started
        windowserver_rows = {
            pid: actual
            for pid, actual in displayed.items()
            if actual["name"] == "WindowServer"
        }
        result["screen"] = {
            "grid_valid": screen_valid,
            "header_error": screen_error,
            "displayed_process_count": len(displayed),
            "windowserver_row_count": len(windowserver_rows),
        }
        comparisons = []
        ps_output = None
        failure_reason = None
        if not screen_valid:
            failure_reason = "The captured terminal stream was not a valid screen"
        elif screen_error:
            failure_reason = screen_error
        elif len(windowserver_rows) != 1:
            failure_reason = "WindowServer was not visible exactly once"
        else:
            pid, actual = next(iter(windowserver_rows.items()))
            expected, ps_output = process_snapshot(pid)
            display_parse = parse_display_elapsed(actual["time"])
            expected_start = parse_ps_start_time(expected["start_time"])
            if (display_parse is not None and expected_start is not None
                    and screen_observed_at is not None):
                displayed_elapsed, resolution = display_parse
                estimated_start = screen_observed_at - displayed_elapsed
                time_tolerance = math.ceil(
                    resolution
                    + CAPTURE_INTERVAL_SECONDS
                    + PS_START_TIME_RESOLUTION_SECONDS
                )
                time_delta = int(round(abs(expected_start - estimated_start)))
                time_matches = time_delta <= time_tolerance
            else:
                displayed_elapsed = None
                resolution = None
                estimated_start = None
                time_tolerance = None
                time_delta = None
                time_matches = False
            user_matches = actual["user"] == expected["user"]
            matched = user_matches and time_matches
            comparisons.append({
                "pid": pid,
                "expected_user": expected["user"],
                "actual_user": actual["user"],
                "expected_start_time": expected["start_time"],
                "estimated_start_epoch_seconds": estimated_start,
                "ps_start_epoch_seconds": expected_start,
                "actual_time": actual["time"],
                "displayed_elapsed_seconds": displayed_elapsed,
                "display_time_resolution_seconds": resolution,
                "start_time_delta_seconds": time_delta,
                "start_time_tolerance_seconds": time_tolerance,
                "user_matches": user_matches,
                "start_time_matches": time_matches,
                "matched": matched,
            })
            if not user_matches:
                failure_reason = "WindowServer User did not match ps user"
            elif not time_matches:
                failure_reason = "WindowServer start time did not match ps lstart"
        matched_count = sum(int(row["matched"]) for row in comparisons)
        result["comparator"]["raw_output"] = ps_output
        result["processes"] = comparisons
        result["summary"] = {
            "eligible_count": len(comparisons),
            "user_match_count": sum(int(row["user_matches"]) for row in comparisons),
            "start_time_match_count": sum(
                int(row["start_time_matches"]) for row in comparisons
            ),
            "matched_count": matched_count,
            "all_eligible_rows_match": (
                len(comparisons) == 1 and matched_count == len(comparisons)
            ),
        }
        result["status"] = (
            "PASS"
            if len(comparisons) == 1 and matched_count == len(comparisons)
            else "FAIL"
        )
        if failure_reason:
            result["failure_reason"] = failure_reason
        result["acceptance"] = {
            "requires_exactly_one_windowserver_row": True,
            "requires_exact_user_match": True,
            "requires_start_time_match": True,
            "passed": result["status"] == "PASS",
        }
    except Exception as exc:
        result["status"] = "ERROR"
        result["error"] = f"{type(exc).__name__}: {exc}"
        raise
    finally:
        try:
            if ntop_child is not None:
                cleanup = stop_child(ntop_child, timeout=20.0, quit_pty=True)
                if cleanup["returncode"] is None:
                    cleanup["returncode"] = wait_bounded(ntop_child, 10.0)
                    cleanup["reaped"] = cleanup["returncode"] is not None
                result["ntop_cleanup"] = cleanup
                if not cleanup["reaped"]:
                    result["status"] = "ERROR"
                    result["cleanup_error"] = "ntop was not reaped after PTY cleanup"
                    if "acceptance" in result:
                        result["acceptance"]["passed"] = False
        except Exception as exc:
            result["ntop_cleanup"] = {
                "verified": False,
                "error": f"{type(exc).__name__}: {exc}",
            }
            result["status"] = "ERROR"
            result["cleanup_error"] = f"{type(exc).__name__}: {exc}"
            if "acceptance" in result:
                result["acceptance"]["passed"] = False
        write_json(output, result)
    return result, output


def main(argv=None):
    parser = argparse.ArgumentParser(
        description="Compare WindowServer User and Time-derived start time with macOS ps."
    )
    parser.add_argument("--ntop", required=True, help="Built native ntop executable")
    parser.add_argument(
        "--raw-dir",
        help="New temporary evidence directory with an ntop-wp15b prefix",
    )
    args = parser.parse_args(argv)
    temporary_root = Path(os.sep) / "tmp"
    raw_value = args.raw_dir or str(
        temporary_root / f"ntop-wp15b-d4-{os.getpid()}-{time.time_ns()}"
    )
    raw_dir = prepare_raw_dir(raw_value)
    ntop = resolve_executable(args.ntop)
    try:
        result, output = run_d4(ntop, raw_dir)
    except (OSError, RuntimeError, TimeoutError, ValueError,
            subprocess.SubprocessError) as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        print(raw_dir / "d4.json")
        return 1
    print(output)
    return 0 if result["status"] == "PASS" else 1


if __name__ == "__main__":
    raise SystemExit(main())
