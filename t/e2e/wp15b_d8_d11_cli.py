"""D8 and D11 acceptance criteria and command line."""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
import time
from pathlib import Path
from typing import Any

from wp15b_d8_d11_display import compare_d11_screens
from wp15b_d8_d11_support import (
    HEIGHT, WIDTH, MAX_LOAD_ATTEMPTS, capture_tui, cpu_counters, cpu_worker,
    load_window, stop_workers,
    utc_now, write_json,
)


D11_COMPARISON_PAIR_COUNT = 20


def run_d8(args: argparse.Namespace) -> int:
    output = args.output
    output.mkdir(parents=True, exist_ok=True)
    if any(output.iterdir()):
        raise FileExistsError(f"output directory is not empty: {output}")
    record: dict[str, Any] = {
        "requirement": "D8", "started_utc": utc_now(), "rate": "700ms",
        "terminal_size": {"rows": HEIGHT, "columns": WIDTH}, "load_windows": [],
    }
    workers: list[subprocess.Popen[bytes]] = []
    selected = None
    return_code = 1
    try:
        processor_count = len(cpu_counters())
        record["logical_processor_count"] = processor_count
        record["baseline_loadavg"] = os.getloadavg()
        script = str(Path(__file__).with_name("wp15b_d8_d11.py").resolve())
        workers.extend(subprocess.Popen(
            [sys.executable, script, "--cpu-worker"], stdin=subprocess.DEVNULL,
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, start_new_session=True,
        ) for _ in range(processor_count))
        record["worker_pids"] = [worker.pid for worker in workers]
        for attempt in range(1, MAX_LOAD_ATTEMPTS + 1):
            window = load_window(2.0)
            window["attempt"] = attempt
            record["load_windows"].append(window)
            per_cpu = window["per_cpu"]
            if (processor_count > 0 and window["duration_seconds"] >= 2.0
                and len(per_cpu) == processor_count
                and all(row["busy_percent"] is not None and row["busy_percent"] >= 90.0
                        for row in per_cpu)):
                selected = window
                break
        record["selected_load_window"] = selected
        record["all_cores_over_90_percent"] = selected is not None
        if selected is not None:
            record["load_window_ending_utc"] = utc_now()
            record["load_window_ending_monotonic"] = selected["end_monotonic"]
            stop_started = time.monotonic()
            pre_app_cleanup = stop_workers(workers)
            workers_stopped = time.monotonic()
            record["pre_app_worker_cleanup"] = pre_app_cleanup
            record["workers_stopped_monotonic"] = workers_stopped
            record["window_end_to_workers_stopped_seconds"] = (
                workers_stopped - selected["end_monotonic"]
            )
            record["worker_stop_duration_seconds"] = workers_stopped-stop_started
            record["workers_absent_before_app"] = (
                pre_app_cleanup["all_reaped"] and pre_app_cleanup["all_pids_absent"]
            )
            write_json(output / "d8-load-evidence.json", record)
            if record["workers_absent_before_app"]:
                app = capture_tui(
                    args.binary, args.working_directory,
                    ["--rate", "700ms", "--default_cpu_entry", "avg"],
                    output / "d8-terminal.raw", output / "d8-screen.txt", "d8",
                )
                record["app"] = app
                record["workers_stopped_to_app_start_seconds"] = (
                    app["started_monotonic"] - workers_stopped
                )
                record["window_end_to_app_start_seconds"] = (
                    app["started_monotonic"] - selected["end_monotonic"]
                )
            else:
                record["error"] = "owned CPU workers were not confirmed absent before app launch"
        else:
            record["error"] = "no two-second window reached 90% busy on every core"
    except KeyboardInterrupt:
        record["error"] = "interrupted"
        return_code = 130
    except Exception as error:
        record["error"] = f"{type(error).__name__}: {error}"
    finally:
        record["worker_pids"] = [worker.pid for worker in workers]
        record["worker_cleanup"] = stop_workers(workers)
        app = record.get("app")
        checks = list(record["worker_cleanup"]["pid_checks"])
        cleanup_confirmed = (record["worker_cleanup"]["all_reaped"]
                             and record["worker_cleanup"]["all_pids_absent"])
        if app:
            checks.append({"pid": app["pid"], "absent": app["cleanup"]["pid_absent"]})
            cleanup_confirmed &= app["cleanup"]["reaped"] and app["cleanup"]["pid_absent"]
        record["exact_pid_checks"] = checks
        record["cleanup_confirmed"] = cleanup_confirmed
        record["cleanup_warning"] = [] if cleanup_confirmed else [
            "one or more owned PIDs were not confirmed absent and reaped"
        ]
        avg = app.get("first_displayed_avg") if app else None
        has_observation = bool(app and avg)
        record["acceptance_pass"] = bool(
            record.get("all_cores_over_90_percent") and record.get("workers_absent_before_app")
            and record.get("window_end_to_app_start_seconds", 999.0) <= 2.0
            and has_observation and avg["avg_percent"] >= 90
        )
        record["acceptance_status"] = (
            "pass" if record["acceptance_pass"] else
            "incomplete" if selected is not None and not has_observation else "fail"
        )
        record["pass"] = record["acceptance_pass"]
        process_success = record["acceptance_pass"] and cleanup_confirmed
        record["process_success"] = process_success
        if process_success:
            return_code = 0
        elif return_code != 130:
            return_code = 1
        record["finished_utc"] = utc_now()
        write_json(output / "d8-result.json", record)
    print(json.dumps({"mode": "d8", "acceptance_status": record["acceptance_status"],
                      "cleanup_confirmed": record["cleanup_confirmed"], "output": str(output)}))
    return return_code


def capture_d11_pair(
    args: argparse.Namespace,
    output: Path,
    common: list[str],
    pair_index: int,
) -> tuple[dict[str, dict[str, Any]], dict[str, Any]]:
    runs: dict[str, dict[str, Any]] = {}
    for label, extra in (("without", []), ("with", ["--free_arc"])):
        stem = (
            f"d11-{label}-free-arc" if pair_index == 1
            else f"d11-pair-{pair_index:02d}-{label}-free-arc"
        )
        run = capture_tui(
            args.binary, args.working_directory, [*common, *extra],
            output / f"{stem}.raw", output / f"{stem}.screen.txt", "d11",
        )
        runs[label] = run
        cleanup = run["cleanup"]
        if not (cleanup["reaped"] and cleanup["pid_absent"]):
            break

    cleanup_confirmed = bool(runs) and all(
        run["cleanup"]["reaped"] and run["cleanup"]["pid_absent"]
        for run in runs.values()
    )
    cli_error_detected = any(
        run["cli_error_output_detected"] for run in runs.values()
    )
    if {"without", "with"}.issubset(runs):
        displays = {
            label: Path(run["display_path"]).read_text()
            for label, run in runs.items()
        }
        comparison = compare_d11_screens(
            displays["without"],
            displays["with"],
            without_rendered=runs["without"]["application_rendered"],
            with_rendered=runs["with"]["application_rendered"],
            free_arc_supplied="--free_arc" in runs["with"]["argv"],
            without_cli_error=runs["without"]["cli_error_output_detected"],
            with_cli_error=runs["with"]["cli_error_output_detected"],
        )
    else:
        comparison = {
            "without_signature": None,
            "with_signature": None,
            "comparison_complete": False,
            "display_signature_equal": None,
            "flag_accepted": False,
            "cli_error_output_detected": cli_error_detected,
            "acceptance_status": "incomplete",
            "pass": False,
        }

    if not cleanup_confirmed:
        comparison["acceptance_status"] = "incomplete"
        comparison["pass"] = False
    capture_summaries = {
        label: {
            "argv": run["argv"],
            "pid": run["pid"],
            "started_utc": run["started_utc"],
            "started_monotonic": run["started_monotonic"],
            "application_rendered": run["application_rendered"],
            "application_render_snapshot_count": (
                run["application_render_snapshot_count"]
            ),
            "stable_snapshot_monotonic_times": (
                run["stable_snapshot_monotonic_times"]
            ),
            "cli_error_output_detected": run["cli_error_output_detected"],
            "complete_panel_complete": run["complete_panel_complete"],
            "cleanup": run["cleanup"],
            "elapsed_seconds": run["elapsed_seconds"],
            "raw_path": run["raw_path"],
            "display_path": run["display_path"],
        }
        for label, run in runs.items()
    }
    pair = {
        "pair_index": pair_index,
        **comparison,
        "without_capture": capture_summaries.get("without"),
        "with_capture": capture_summaries.get("with"),
        "process_panel_complete": (
            len(runs) == 2 and all(
                run["complete_panel_complete"] for run in runs.values()
            )
        ),
        "cleanup_confirmed": cleanup_confirmed,
    }
    return runs, pair


def run_d11(args: argparse.Namespace) -> int:
    output = args.output
    output.mkdir(parents=True, exist_ok=True)
    if any(output.iterdir()):
        raise FileExistsError(f"output directory is not empty: {output}")
    d11_started_utc = utc_now()
    load_average_before = list(os.getloadavg())
    common = ["--rate", "700ms", "--default_cpu_entry", "avg"]
    comparison_pairs = []
    first_runs = None
    for pair_index in range(1, D11_COMPARISON_PAIR_COUNT + 1):
        runs, pair = capture_d11_pair(args, output, common, pair_index)
        if first_runs is None:
            first_runs = runs
        comparison_pairs.append(pair)
        if pair["cli_error_output_detected"] or not pair["cleanup_confirmed"]:
            break

    load_average_after = list(os.getloadavg())
    first_pair = comparison_pairs[0]
    completed_pair_count = len(comparison_pairs)
    captures = [
        (pair["pair_index"], label, pair[f"{label}_capture"])
        for pair in comparison_pairs
        for label in ("without", "with")
        if pair.get(f"{label}_capture") is not None
    ]
    comparison_complete = (
        completed_pair_count == D11_COMPARISON_PAIR_COUNT
        and all(
            pair["comparison_complete"] and pair["cleanup_confirmed"]
            for pair in comparison_pairs
        )
    )
    display_signature_equal = (
        all(pair["display_signature_equal"] for pair in comparison_pairs)
        if comparison_complete else None
    )
    flag_accepted = (
        completed_pair_count == D11_COMPARISON_PAIR_COUNT
        and all(pair["flag_accepted"] and pair["cleanup_confirmed"]
                for pair in comparison_pairs)
    )
    application_rendered = (
        completed_pair_count == D11_COMPARISON_PAIR_COUNT
        and len(captures) == D11_COMPARISON_PAIR_COUNT * 2
        and all(capture["application_rendered"] for _, _, capture in captures)
    )
    cli_error_detected = any(
        capture["cli_error_output_detected"] for _, _, capture in captures
    )
    process_panel_complete = (
        completed_pair_count == D11_COMPARISON_PAIR_COUNT
        and all(pair["process_panel_complete"] for pair in comparison_pairs)
    )
    acceptance_pass = (
        comparison_complete
        and display_signature_equal is True
        and flag_accepted
        and application_rendered
        and not cli_error_detected
    )
    status = (
        "pass" if acceptance_pass
        else "fail" if any(pair["acceptance_status"] == "fail"
                           for pair in comparison_pairs)
        else "incomplete"
    )
    exact_pid_checks = [
        {
            "pair_index": pair_index,
            "flag": label,
            "pid": capture["pid"],
            "absent": capture["cleanup"]["pid_absent"],
            "reaped": capture["cleanup"]["reaped"],
        }
        for pair_index, label, capture in captures
    ]
    cleanup_confirmed = bool(exact_pid_checks) and all(
        check["reaped"] and check["absent"] for check in exact_pid_checks
    )
    initial_signatures = {
        "without": first_pair["without_signature"],
        "with": first_pair["with_signature"],
        "equal": first_pair["display_signature_equal"],
    }
    record = {
        "requirement": "D11",
        "started_utc": d11_started_utc,
        "load_average_before": load_average_before,
        "common_arguments": common,
        "comparison_signature": "application_render_signature",
        "comparison_pair_count_target": D11_COMPARISON_PAIR_COUNT,
        "comparison_pair_count_completed": completed_pair_count,
        "comparison_pairs": comparison_pairs,
        "initial_signature_comparison": initial_signatures,
        "without_flag": first_runs.get("without"),
        "with_flag": first_runs.get("with"),
        "without_structure": initial_signatures["without"],
        "with_structure": initial_signatures["with"],
        "flag_accepted": flag_accepted,
        "application_rendered": application_rendered,
        "cli_error_output_detected": cli_error_detected,
        "process_panel_complete": process_panel_complete,
        "comparison_complete": comparison_complete,
        "display_structure_equal": display_signature_equal,
        "acceptance_pass": acceptance_pass,
        "acceptance_status": status,
        "pass": acceptance_pass,
        "exact_pid_checks": exact_pid_checks,
        "cleanup_confirmed": cleanup_confirmed,
        "capture_aborted_for_cleanup": any(
            not pair["cleanup_confirmed"] for pair in comparison_pairs
        ),
        "cleanup_warning": [] if cleanup_confirmed else [
            "one or more owned app PIDs were not confirmed absent and reaped"
        ],
        "process_success": acceptance_pass and cleanup_confirmed,
        "load_average_after": load_average_after,
        "finished_utc": utc_now(),
    }
    write_json(output / "d11-comparison.json", record)
    print(json.dumps({
        "mode": "d11",
        "acceptance_status": status,
        "cleanup_confirmed": cleanup_confirmed,
        "flag_accepted": flag_accepted,
        "application_rendered": application_rendered,
        "cli_error_output_detected": cli_error_detected,
        "process_panel_complete": process_panel_complete,
        "display_structure_equal": display_signature_equal,
        "comparison_pair_count_completed": completed_pair_count,
        "comparison_pair_count_target": D11_COMPARISON_PAIR_COUNT,
        "load_average_before": load_average_before,
        "load_average_after": load_average_after,
        "output": str(output),
    }))
    return 0 if record["process_success"] else 1


def main() -> int:
    if sys.argv[1:] == ["--cpu-worker"]:
        cpu_worker()
        return 0
    parser = argparse.ArgumentParser()
    parser.add_argument("--mode", choices=("d8", "d11"), required=True)
    parser.add_argument("--binary", required=True)
    parser.add_argument("--working-directory", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    return run_d8(args) if args.mode == "d8" else run_d11(args)


if __name__ == "__main__":
    raise SystemExit(main())
