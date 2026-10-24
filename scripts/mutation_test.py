from __future__ import annotations

import argparse
from concurrent.futures import ThreadPoolExecutor, as_completed
import json
from pathlib import Path
import sys

from mutation_exec import (
    MAX_WORKERS,
    RunResult,
    _execute_project,
    _mutant_record,
    _print_mutant,
    _print_run,
    _report_json,
    _run_mutant,
    _run_summary,
    _selected_files,
)
from mutation_lisp import Mutation, MutationRunnerError
from mutation_operators import discover_mutations


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Run source-level Common Lisp mutation tests in temporary project copies. "
            "Only explicitly selected cl-weave test files are loaded and run."
        )
    )
    parser.add_argument(
        "--root",
        type=Path,
        default=Path(__file__).resolve().parents[1],
        help="Project root (defaults to the repository containing this script).",
    )
    parser.add_argument(
        "--source",
        action="append",
        required=True,
        default=[],
        metavar="FILE",
        help="Production Lisp source file to mutate; repeat for multiple files.",
    )
    parser.add_argument(
        "--test",
        action="append",
        required=True,
        metavar="FILE",
        help="Selected cl-weave test file to load and run; repeat to select more files.",
    )
    parser.add_argument(
        "--support",
        action="append",
        default=[],
        metavar="FILE",
        help="Optional support Lisp file to load before the selected test files; repeatable.",
    )
    parser.add_argument("--sbcl", default="sbcl", help="SBCL executable (default: sbcl).")
    parser.add_argument(
        "--jobs",
        type=int,
        default=1,
        help=f"Maximum concurrent mutant runs (1-{MAX_WORKERS}, default: 1).",
    )
    parser.add_argument(
        "--report",
        type=Path,
        help="Optional path for the JSON report; the report is also printed to stdout.",
    )
    parser.add_argument(
        "--empty-test-control",
        action="store_true",
        help=(
            "Calibration only: empty selected test copies, require zero tests, "
            "allow the empty run-all, and run every generated mutant."
        ),
    )
    arguments = parser.parse_args()
    if not 1 <= arguments.jobs <= MAX_WORKERS:
        parser.error(f"--jobs must be between 1 and {MAX_WORKERS}.")
    return arguments


def main() -> int:
    arguments = _parse_args()
    root = arguments.root.expanduser().resolve()
    if not root.is_dir():
        raise MutationRunnerError(f"Project root does not exist: {root}")

    test_files = _selected_files(root, arguments.test, "Selected test file")
    support_files = _selected_files(root, arguments.support, "Support file")
    if not test_files:
        raise MutationRunnerError("At least one selected test file is required.")
    for path in test_files:
        if "t" not in path.relative_to(root).parts or path.suffix.lower() != ".lisp":
            raise MutationRunnerError(f"Selected tests must be Lisp files under t/: {path}")

    source_files = _selected_files(root, arguments.source, "Source file")
    for path in source_files:
        if "src" not in path.relative_to(root).parts or path.suffix.lower() != ".lisp":
            raise MutationRunnerError(f"Mutation sources must be Lisp files under src/: {path}")

    empty_control = arguments.empty_test_control
    mutations: list[Mutation] = []
    for path in source_files:
        relative_source = path.relative_to(root).as_posix()
        try:
            source = path.read_bytes().decode("utf-8")
        except (OSError, UnicodeDecodeError) as error:
            message = f"Unable to read UTF-8 source file {path}: {error}"
            raise MutationRunnerError(message) from error
        mutations.extend(discover_mutations(relative_source, source))
    baseline = _execute_project(
        root,
        test_files,
        support_files,
        arguments.sbcl,
        empty_control,
    )
    _print_run("BASELINE", baseline)

    test_counts_valid = (
        baseline.selected_count is not None
        and baseline.runnable_count is not None
        and baseline.selected_count > 0
        and baseline.runnable_count > 0
    )
    baseline_valid = (
        baseline.result == "empty-control-confirmed"
        and baseline.selected_count == 0
        and baseline.runnable_count == 0
        if empty_control
        else baseline.result == "passed" and test_counts_valid
    )

    if not baseline_valid:
        report = _report(
            root,
            test_files,
            support_files,
            baseline,
            [],
            "empty-test-control" if empty_control else "scoring",
            "error",
        )
        _report_json(report, arguments.report.expanduser().resolve() if arguments.report else None)
        if empty_control:
            print(
                "ERROR: the empty-test baseline was not confirmed; no mutants were run.",
                file=sys.stderr,
            )
        elif baseline.result == "failed":
            print("ERROR: the unmutated baseline failed; no mutants were scored.", file=sys.stderr)
        elif (
            baseline.result in {"empty-selection", "no-runnable-tests"}
            or baseline.selected_count == 0
            or baseline.runnable_count == 0
        ):
            print(
                "ERROR: baseline selected no runnable tests; normal scoring requires a "
                "nonempty selection.",
                file=sys.stderr,
            )
        else:
            print(
                f"ERROR: baseline runner failure at {baseline.error_stage}: "
                f"{baseline.error_message}",
                file=sys.stderr,
            )
        return 1

    if not mutations:
        report = {
            "schema_version": 1,
            "status": "error",
            "mode": "empty-test-control" if empty_control else "scoring",
            "selected_test_files": [str(path.relative_to(root)) for path in test_files],
            "support_files": [str(path.relative_to(root)) for path in support_files],
            "baseline": _run_summary(baseline),
            "mutants": [],
            "totals": {
                "generated": 0,
                "killed": 0,
                "killed_by_tests": 0,
                "killed_at_production_load": 0,
                "survived": 0,
                "runner_errors": 0,
            },
            "mutation_score_percent": None,
        }
        _report_json(report, arguments.report.expanduser().resolve() if arguments.report else None)
        print("ERROR: no supported mutation sites were found.", file=sys.stderr)
        return 1

    results: dict[int, RunResult] = {}
    with ThreadPoolExecutor(max_workers=arguments.jobs) as executor:
        futures = {
            executor.submit(
                _run_mutant,
                root,
                mutation,
                test_files,
                support_files,
                arguments.sbcl,
                baseline.selected_count,
                baseline.runnable_count,
                empty_control,
            ): index
            for index, mutation in enumerate(mutations, 1)
        }
        for future in as_completed(futures):
            index = futures[future]
            try:
                results[index] = future.result()
            except Exception as error:
                results[index] = RunResult(
                    result="runner-error",
                    error_stage="worker",
                    error_message=f"{type(error).__name__}: {error}",
                )

    records = [
        _mutant_record(index, mutation, results[index])
        for index, mutation in enumerate(mutations, 1)
    ]
    for record in records:
        _print_mutant(record, len(records))
    report = _report(
        root,
        test_files,
        support_files,
        baseline,
        records,
        "empty-test-control" if empty_control else "scoring",
        "success",
    )
    killed = report["totals"]["killed"]
    killed_at_load = report["totals"]["killed_at_production_load"]
    survived = report["totals"]["survived"]
    runner_errors = report["totals"]["runner_errors"]
    evaluated = killed + survived
    if empty_control:
        valid_control = (
            baseline_valid
            and len(records) > 0
            and killed == 0
            and survived == len(records)
            and runner_errors == 0
        )
        report["status"] = "success" if valid_control else "error"
        report["mutation_score_percent"] = 0.0 if survived else None
    else:
        report["status"] = "error" if runner_errors else "success"
    print(
        f"SUMMARY generated={len(records)} killed={killed} "
        f"killed_by_tests={killed - killed_at_load} "
        f"killed_at_production_load={killed_at_load} survived={survived} "
        f"runner_errors={runner_errors} mutation_score_percent="
        f"{report['mutation_score_percent']}"
    )
    _report_json(report, arguments.report.expanduser().resolve() if arguments.report else None)
    return 0 if report["status"] == "success" else 1


def _report(
    root: Path,
    tests: list[Path],
    support: list[Path],
    baseline: RunResult,
    mutants: list[dict[str, object]],
    mode: str,
    status: str,
) -> dict[str, object]:
    killed = sum(record["outcome"] == "killed" for record in mutants)
    killed_at_load = sum(
        record["outcome"] == "killed"
        and record["failure_stage"] == "production-load"
        for record in mutants
    )
    survived = sum(record["outcome"] == "survived" for record in mutants)
    runner_errors = sum(record["outcome"] == "runner_error" for record in mutants)
    evaluated = killed + survived
    score = round(100 * killed / evaluated, 2) if evaluated else None
    return {
        "schema_version": 1,
        "status": status,
        "mode": mode,
        "selected_test_files": [str(path.relative_to(root)) for path in tests],
        "support_files": [str(path.relative_to(root)) for path in support],
        "baseline": _run_summary(baseline),
        "mutants": mutants,
        "totals": {
            "generated": len(mutants),
            "killed": killed,
            "killed_by_tests": killed - killed_at_load,
            "killed_at_production_load": killed_at_load,
            "survived": survived,
            "runner_errors": runner_errors,
            "evaluated": evaluated,
        },
        "mutation_score_percent": score,
        "score_denominator": (
            "killed + survived; production-load compile errors count as killed; "
            "runner errors are excluded"
        ),
    }

if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except MutationRunnerError as error:
        print(f"ERROR: {error}", file=sys.stderr)
        raise SystemExit(1) from error
