#!/usr/bin/env python3
"""Compare displayed User and Time values with the macOS ps comparator."""

import argparse
import json
import os
import pwd
import re
import subprocess
import sys
import time
from pathlib import Path

from wp15b_d1 import (
    COLUMNS,
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
TIME_TOLERANCE_SECONDS = 3.0
PS_COMMAND = ["ps", "-o", "pid=,ruser=,etime=", "-A"]


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
            "pid",
            "--tree",
        ],
        ROWS,
        COLUMNS,
    )


def process_snapshot():
    completed = subprocess.run(
        PS_COMMAND, capture_output=True, text=True, timeout=10, check=True
    )
    rows = {}
    for line in completed.stdout.splitlines():
        fields = line.split(None, 2)
        if len(fields) != 3:
            continue
        pid_text, real_user, elapsed = fields
        if pid_text.isdecimal():
            rows[int(pid_text)] = {
                "user": real_user,
                "time": elapsed.strip(),
            }
    return rows, completed.stdout


def parse_display_elapsed(value):
    value = value.strip()
    match = re.fullmatch(r"(\d+)d\s+(\d+)h\s+(\d+)m", value)
    if match:
        days, hours, minutes = map(int, match.groups())
        return (days * 86400 + hours * 3600 + minutes * 60, 60.0, False)
    match = re.fullmatch(r"(\d+)h\s+(\d+)m\s+(\d+)s", value)
    if match:
        hours, minutes, seconds = map(int, match.groups())
        return (hours * 3600 + minutes * 60 + seconds, 1.0, False)
    match = re.fullmatch(r"(\d+)m\s+(\d+(?:\.\d+)?)s", value)
    if match:
        minutes, seconds = match.groups()
        return (int(minutes) * 60 + float(seconds), 0.01, False)
    match = re.fullmatch(r"(\d+(?:\.\d+)?)s", value)
    if match:
        seconds = match.group(1)
        decimals = len(seconds.partition(".")[2])
        return float(seconds), 10 ** -decimals, False
    return None


def parse_ps_elapsed(value):
    value = value.strip()
    days = 0
    if "-" in value:
        day_text, value = value.split("-", 1)
        if not day_text.isdecimal():
            return None
        days = int(day_text)
    parts = value.split(":")
    try:
        if len(parts) == 2:
            minutes, seconds = int(parts[0]), int(parts[1])
            hours = 0
        elif len(parts) == 3:
            hours, minutes, seconds = map(int, parts)
        else:
            return None
    except ValueError:
        return None
    return float(days * 86400 + hours * 3600 + minutes * 60 + seconds)


def elapsed_delta(expected_seconds, displayed_seconds, resolution, rounded):
    half_resolution = resolution / 2 if rounded else 0.0
    lower = displayed_seconds - half_resolution
    upper = displayed_seconds + (half_resolution if rounded else resolution)
    return max(0.0, lower - expected_seconds, expected_seconds - upper)


def process_table_cells(grid):
    lines = grid.signature()
    header_index = None
    starts = None
    for index, line in enumerate(lines):
        pid_start = line.find("PID(p)")
        name_start = line.find("Name(n)")
        user_start = line.find("User")
        state_start = line.find("State")
        time_start = line.find("Time")
        priority_start = line.find("Priority")
        if (pid_start >= 0 and name_start > pid_start and user_start >= 0
                and state_start > user_start and time_start > state_start):
            header_index = index
            starts = (
                pid_start,
                name_start,
                user_start,
                state_start,
                time_start,
                priority_start,
            )
            break
    if header_index is None:
        return {}, "process table header with PID, User, State, and Time was not visible"

    pid_start, name_start, user_start, state_start, time_start, priority_start = starts
    time_end = priority_start if priority_start > time_start else len(lines[header_index])
    displayed = {}
    for line in lines[header_index + 1:]:
        pid_text = line[pid_start:name_start].strip()
        if not re.fullmatch(r"\d+", pid_text):
            continue
        pid = int(pid_text)
        displayed[pid] = {
            "user": line[user_start:state_start].strip(),
            "user_column_width": state_start - user_start,
            "time": line[time_start:time_end].strip(),
            "row": line.rstrip(),
        }
    return displayed, None


def run_d4(ntop, raw_dir):
    output = raw_dir / "d4.json"
    result = {
        "measurement": "D4",
        "status": "INCOMPLETE",
        "settings": {
            "rows": ROWS,
            "columns": COLUMNS,
            "rate_ms": RATE_MS,
            "capture_seconds": CAPTURE_SECONDS,
        },
        "comparator": {
            "command": PS_COMMAND,
            "real_user_field": "ruser",
            "elapsed_time_field": "etime",
            "elapsed_time_tolerance_seconds": TIME_TOLERANCE_SECONDS,
            "tolerance_rationale": (
                "0.7-second UI refresh plus the observed approximately 2-second "
                "all-process ps scan, rounded up to 3 seconds"
            ),
        },
        "raw": {"pty": "ntop.ansi"},
    }
    write_json(output, result)
    ntop_child = None
    try:
        ntop_child = start_ntop(ntop)
        result["ntop_process"] = {"pid": ntop_child.pid, "pgid": ntop_child.group_id}
        raw_path = raw_dir / "ntop.ansi"
        capture_started = time.monotonic()
        with raw_path.open("wb") as raw:
            while time.monotonic() - capture_started < CAPTURE_SECONDS:
                remaining = CAPTURE_SECONDS - (time.monotonic() - capture_started)
                ntop_child.read_for(
                    min(CAPTURE_INTERVAL_SECONDS, remaining), raw
                )
                grid = TerminalGrid(ROWS, COLUMNS)
                screen_valid = grid.feed(raw_path.read_bytes())
                displayed, screen_error = process_table_cells(grid)
                if screen_valid and screen_error is None and displayed:
                    break
        result["capture_elapsed_seconds"] = time.monotonic() - capture_started
        comparator_rows, comparator_raw = process_snapshot()
        current_user = pwd.getpwuid(os.getuid()).pw_name
        other_users = {
            pid: expected
            for pid, expected in comparator_rows.items()
            if expected["user"] != current_user
        }
        comparisons = []
        visible_other_users = {
            pid: expected
            for pid, expected in other_users.items()
            if pid in displayed
        }
        user_match_count = 0
        user_exact_match_count = 0
        user_truncated_match_count = 0
        time_match_count = 0
        matched_count = 0
        for pid, expected in sorted(visible_other_users.items()):
            actual = displayed[pid]
            actual_user = actual["user"]
            actual_time = actual["time"]
            expected_elapsed = parse_ps_elapsed(expected["time"])
            display_parse = parse_display_elapsed(actual_time)
            if expected_elapsed is not None and display_parse is not None:
                actual_elapsed, resolution, rounded = display_parse
                time_delta = elapsed_delta(
                    expected_elapsed, actual_elapsed, resolution, rounded
                )
                time_matches = time_delta <= TIME_TOLERANCE_SECONDS
            else:
                actual_elapsed = None
                resolution = None
                time_delta = None
                time_matches = False
            user_exact_matches = actual_user == expected["user"]
            user_truncated_matches = (
                not user_exact_matches
                and len(expected["user"]) > actual["user_column_width"]
                and len(actual_user) >= actual["user_column_width"] - 1
                and expected["user"].startswith(actual_user)
            )
            user_matches = user_exact_matches or user_truncated_matches
            matched = user_matches and time_matches
            user_match_count += int(user_matches)
            user_exact_match_count += int(user_exact_matches)
            user_truncated_match_count += int(user_truncated_matches)
            time_match_count += int(time_matches)
            matched_count += int(matched)
            comparisons.append({
                "pid": pid,
                "expected_user": expected["user"],
                "expected_time": expected["time"],
                "actual_user": actual_user,
                "actual_time": actual_time,
                "user_column_width": actual["user_column_width"],
                "expected_elapsed_seconds": expected_elapsed,
                "actual_elapsed_seconds": actual_elapsed,
                "actual_time_resolution_seconds": resolution,
                "time_delta_seconds": time_delta,
                "time_tolerance_seconds": TIME_TOLERANCE_SECONDS,
                "eligible": True,
                "user_matches": user_matches,
                "user_exact_matches": user_exact_matches,
                "user_truncated_matches": user_truncated_matches,
                "time_matches": time_matches,
                "matched": matched,
            })
        result.update({
            "comparator": {
                **result["comparator"],
                "current_user": current_user,
                "other_user_filter": "ruser != current_user",
                "raw_output": comparator_raw,
            },
            "screen": {
                "grid_valid": screen_valid,
                "header_error": screen_error,
                "displayed_process_count": len(displayed),
            },
            "processes": comparisons,
            "summary": {
                "comparator_other_user_count": len(other_users),
                "visible_other_user_count": len(visible_other_users),
                "eligible_count": len(visible_other_users),
                "user_match_count": user_match_count,
                "user_exact_match_count": user_exact_match_count,
                "user_truncated_match_count": user_truncated_match_count,
                "time_match_count": time_match_count,
                "matched_count": matched_count,
                "time_tolerance_seconds": TIME_TOLERANCE_SECONDS,
                "all_eligible_rows_match": (
                    bool(visible_other_users)
                    and matched_count == len(visible_other_users)
                ),
            },
        })
        if not screen_valid:
            result["status"] = "INCOMPLETE"
            result["incomplete_reason"] = (
                "The captured terminal stream could not be parsed as a valid screen"
            )
        elif screen_error:
            result["status"] = "INCOMPLETE"
            result["incomplete_reason"] = screen_error
        elif not visible_other_users:
            result["status"] = "INCOMPLETE"
            result["incomplete_reason"] = (
                "No other-user process row was visible for comparison"
            )
        else:
            result["status"] = (
                "PASS"
                if matched_count == len(visible_other_users)
                else "FAIL"
            )
        result["acceptance"] = {
            "requires_at_least_one_eligible_other_user_row": True,
            "requires_user_and_time_match_for_every_eligible_row": True,
            "time_tolerance_seconds": TIME_TOLERANCE_SECONDS,
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
        description="Compare displayed process User and Time with macOS ps values."
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
