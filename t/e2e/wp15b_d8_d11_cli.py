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

from wp15b_d8_d11_support import (
    HEIGHT, WIDTH, MAX_LOAD_ATTEMPTS, capture_tui, cpu_counters, cpu_worker,
    load_window, stop_workers, structure_signature, memory_arc_display_signature,
    utc_now, write_json,
)


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


def run_d11(args: argparse.Namespace) -> int:
    output = args.output
    output.mkdir(parents=True, exist_ok=True)
    if any(output.iterdir()):
        raise FileExistsError(f"output directory is not empty: {output}")
    common = ["--rate", "700ms", "--default_cpu_entry", "avg"]
    runs = {}
    for label, extra in (("without", []), ("with", ["--free_arc"])):
        runs[label] = capture_tui(
            args.binary, args.working_directory, [*common, *extra],
            output / f"d11-{label}-free-arc.raw",
            output / f"d11-{label}-free-arc.screen.txt", "d11",
        )
    displays = {label: Path(run["display_path"]).read_text()
                for label, run in runs.items()}
    signatures = {label: structure_signature(displays[label])
                  for label in runs}
    equal = signatures["without"] == signatures["with"]
    memory_arc_signatures = {
        label: memory_arc_display_signature(displays[label]) for label in runs
    }
    memory_arc_equal = memory_arc_signatures["without"] == memory_arc_signatures["with"]
    memory_arc_complete = all(
        signature["memory_panel_visible"] for signature in memory_arc_signatures.values()
    )
    memory_arc_absent = memory_arc_complete and all(
        not signature["arc_entry_present"] for signature in memory_arc_signatures.values()
    )
    application_rendered = all(
        run["application_rendered"] and not run["cli_error_output_detected"]
        for run in runs.values()
    )
    cli_error_detected = any(run["cli_error_output_detected"] for run in runs.values())
    process_panel_complete = all(
        run["complete_panel_complete"] for run in runs.values()
    )
    acceptance_pass = application_rendered and memory_arc_equal and memory_arc_absent
    status = (
        "pass" if acceptance_pass
        else "fail" if application_rendered and memory_arc_complete
        else "incomplete"
    )
    exact_pid_checks = [{"pid": run["pid"], "absent": run["cleanup"]["pid_absent"]}
                        for run in runs.values()]
    cleanup_confirmed = all(run["cleanup"]["reaped"] and run["cleanup"]["pid_absent"]
                            for run in runs.values())
    record = {
        "requirement": "D11", "started_utc": utc_now(), "common_arguments": common,
        "without_flag": runs["without"], "with_flag": runs["with"],
        "without_structure": signatures["without"], "with_structure": signatures["with"],
        "flag_accepted": (
            runs["with"]["application_rendered"]
            and not runs["with"]["cli_error_output_detected"]
        ),
        "application_rendered": application_rendered,
        "cli_error_output_detected": cli_error_detected,
        "process_panel_complete": process_panel_complete,
        "display_structure_equal": equal,
        "without_memory_arc_display": memory_arc_signatures["without"],
        "with_memory_arc_display": memory_arc_signatures["with"],
        "memory_arc_display_equal": memory_arc_equal,
        "memory_arc_display_absent": memory_arc_absent,
        "memory_arc_display_complete": memory_arc_complete,
        "acceptance_pass": acceptance_pass,
        "acceptance_status": status, "pass": acceptance_pass,
        "exact_pid_checks": exact_pid_checks, "cleanup_confirmed": cleanup_confirmed,
        "cleanup_warning": [] if cleanup_confirmed else [
            "one or more owned app PIDs were not confirmed absent and reaped"
        ], "process_success": acceptance_pass and cleanup_confirmed,
        "finished_utc": utc_now(),
    }
    write_json(output / "d11-comparison.json", record)
    print(json.dumps({"mode": "d11", "acceptance_status": status,
                      "cleanup_confirmed": cleanup_confirmed,
                      "flag_accepted": record["flag_accepted"],
                      "application_rendered": application_rendered,
                      "cli_error_output_detected": cli_error_detected,
                      "process_panel_complete": process_panel_complete,
                      "display_structure_equal": equal,
                      "memory_arc_display_equal": memory_arc_equal,
                      "memory_arc_display_absent": memory_arc_absent,
                      "output": str(output)}))
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
