from __future__ import annotations

from dataclasses import dataclass
from functools import lru_cache
import json
import os
from pathlib import Path
import re
from runpy import run_path
import shutil
import subprocess
import tempfile
from typing import Any

from mutation_driver import TEST_TIMEOUT_MS, _driver_source
from mutation_lisp import Mutation, MutationRunnerError


MAX_WORKERS = 4
PROCESS_TIMEOUT_SECONDS = TEST_TIMEOUT_MS // 1000 + 20
_CACHE_DIRECTORY: tempfile.TemporaryDirectory | None = None


def _mutation_cache_directory() -> Path:
    global _CACHE_DIRECTORY
    if _CACHE_DIRECTORY is None:
        _CACHE_DIRECTORY = tempfile.TemporaryDirectory(prefix="ntop-mutation-cache-")
    return Path(_CACHE_DIRECTORY.name)


@lru_cache(maxsize=4)
def _base_environment(root: Path) -> dict[str, str]:
    workflow_path = root / "scripts" / "darwin-workflow.py"
    resolve_source_registry = run_path(str(workflow_path))["resolve_source_registry"]
    environment, _ = resolve_source_registry()
    return environment


def _project_environment(root: Path, project_copy: Path, cache: Path) -> dict[str, str]:
    environment = _base_environment(root).copy()
    registry = environment["CL_SOURCE_REGISTRY"]
    original = f"(:directory {_lisp_string(str(root))})"
    copied = f"(:directory {_lisp_string(str(project_copy))})"
    if original not in registry:
        raise RuntimeError("The resolved ASDF registry omits the project root.")
    environment["CL_SOURCE_REGISTRY"] = registry.replace(original, copied, 1)
    environment["XDG_CACHE_HOME"] = str(cache)
    return environment


def _lisp_string(value: str) -> str:
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


@dataclass
class RunResult:
    selected_count: int | None = None
    runnable_count: int | None = None
    result: str | None = None
    error_stage: str | None = None
    error_message: str | None = None
    returncode: int | None = None
    stdout: str = ""
    stderr: str = ""


def _ignore_project_artifacts(_: str, names: list[str]) -> set[str]:
    ignored = {".git", ".jj", ".worktrees", "__pycache__", ".pytest_cache"}
    return ignored.intersection(names)


def _resolve_project_file(root: Path, value: str, description: str) -> Path:
    candidate = Path(value).expanduser()
    path = candidate.resolve() if candidate.is_absolute() else (root / candidate).resolve()
    try:
        path.relative_to(root)
    except ValueError as error:
        message = f"{description} must be inside the project root: {value}"
        raise MutationRunnerError(message) from error
    if not path.is_file():
        raise MutationRunnerError(f"{description} does not exist or is not a file: {value}")
    return path


def _selected_files(root: Path, values: list[str], description: str) -> list[Path]:
    files: list[Path] = []
    seen: set[Path] = set()
    for value in values:
        path = _resolve_project_file(root, value, description)
        if path not in seen:
            seen.add(path)
            files.append(path)
    return files


def _empty_test_copies(root: Path, project_copy: Path, tests: list[Path]) -> None:
    for path in tests:
        copied = project_copy / path.relative_to(root)
        copied.write_bytes(b"")


def _last_marker(pattern: str, output: str) -> int | None:
    matches = re.findall(pattern, output, flags=re.MULTILINE)
    return int(matches[-1]) if matches else None


def _partial_text(value: str | bytes | None) -> str:
    if isinstance(value, bytes):
        return value.decode("utf-8", errors="replace")
    return value or ""


def _execute_project(
    root: Path,
    tests: list[Path],
    support: list[Path],
    sbcl: str,
    empty_test_control: bool,
) -> RunResult:
    with tempfile.TemporaryDirectory(prefix="ntop-mutation-run-") as temporary:
        temporary_root = Path(temporary)
        project_copy = temporary_root / "project"
        try:
            shutil.copytree(root, project_copy, symlinks=True, ignore=_ignore_project_artifacts)
        except OSError as error:
            return RunResult(error_stage="project-copy", error_message=str(error))

        copied_tests = [project_copy / path.relative_to(root) for path in tests]
        copied_support = [project_copy / path.relative_to(root) for path in support]
        if empty_test_control:
            try:
                _empty_test_copies(root, project_copy, tests)
            except OSError as error:
                return RunResult(error_stage="empty-test-copy", error_message=str(error))
        driver = temporary_root / "mutation-driver.lisp"
        try:
            driver.write_text(
                _driver_source(project_copy, copied_tests, copied_support, empty_test_control),
                encoding="utf-8",
            )
        except OSError as error:
            return RunResult(error_stage="driver-write", error_message=str(error))
        try:
            environment = _project_environment(
                root, project_copy, _mutation_cache_directory()
            )
            completed = subprocess.run(
                [sbcl, "--script", str(driver)],
                cwd=project_copy,
                env=environment,
                capture_output=True,
                text=True,
                encoding="utf-8",
                errors="replace",
                check=False,
                timeout=PROCESS_TIMEOUT_SECONDS,
            )
        except subprocess.TimeoutExpired as error:
            stdout = _partial_text(error.stdout)
            stderr = _partial_text(error.stderr)
            output = stdout + "\n" + stderr
            return RunResult(
                selected_count=_last_marker(r"^MUTATION_SELECTED_COUNT=(\d+)$", output),
                runnable_count=_last_marker(r"^MUTATION_RUNNABLE_COUNT=(\d+)$", output),
                result="runner-error",
                error_stage="subprocess-timeout",
                error_message=f"SBCL exceeded {PROCESS_TIMEOUT_SECONDS} seconds.",
                stdout=stdout[-5000:],
                stderr=stderr[-5000:],
            )
        except OSError as error:
            return RunResult(error_stage="sbcl-start", error_message=str(error))
        except Exception as error:
            return RunResult(error_stage="source-registry", error_message=str(error))

        output = completed.stdout + "\n" + completed.stderr
        selected_count = _last_marker(r"^MUTATION_SELECTED_COUNT=(\d+)$", output)
        runnable_count = _last_marker(r"^MUTATION_RUNNABLE_COUNT=(\d+)$", output)
        stage_match = re.findall(
            r"^MUTATION_RUNNER_ERROR_STAGE=(.+)$", output, flags=re.MULTILINE
        )
        error_match = re.findall(r"^MUTATION_RUNNER_ERROR=(.*)$", output, flags=re.MULTILINE)
        result_match = re.findall(
            r"^MUTATION_TEST_RESULT=(passed|failed)$", output, flags=re.MULTILINE
        )
        empty_match = re.findall(
            r"^MUTATION_EMPTY_CONTROL=(confirmed|failed|not-empty)$",
            output,
            flags=re.MULTILINE,
        )
        result = RunResult(
            selected_count=selected_count,
            runnable_count=runnable_count,
            returncode=completed.returncode,
            stdout=completed.stdout[-5000:],
            stderr=completed.stderr[-5000:],
            error_stage=stage_match[-1] if stage_match else None,
            error_message=error_match[-1] if error_match else None,
        )
        if empty_test_control:
            if (
                empty_match
                and empty_match[-1] == "confirmed"
                and completed.returncode == 0
                and selected_count == 0
                and runnable_count == 0
            ):
                result.result = "empty-control-confirmed"
            elif empty_match and empty_match[-1] == "not-empty":
                result.result = "runner-error"
                result.error_stage = "empty-test-control"
                result.error_message = "The copied selected test files produced tests."
            elif empty_match and empty_match[-1] == "failed":
                result.result = "runner-error"
                result.error_stage = "empty-test-control"
                result.error_message = "cl-weave:run-all rejected an empty selection."
            elif result.error_stage is not None:
                result.result = "runner-error"
            else:
                result.result = "runner-error"
                result.error_stage = "driver-protocol"
                result.error_message = "The empty-test control emitted no confirmation."
        elif result.error_stage is not None:
            result.result = "runner-error"
        elif result_match and completed.returncode == (
            0 if result_match[-1] == "passed" else 10
        ):
            result.result = result_match[-1]
        elif selected_count == 0 and completed.returncode == 0:
            result.result = "empty-selection"
        elif runnable_count == 0 and selected_count is not None and completed.returncode == 0:
            result.result = "no-runnable-tests"
        else:
            result.result = "runner-error"
            result.error_stage = "driver-protocol"
            result.error_message = "The SBCL driver did not emit a recognized result marker."
        return result


def _run_summary(result: RunResult) -> dict[str, Any]:
    return {
        "selected_test_count": result.selected_count,
        "runnable_test_count": result.runnable_count,
        "result": result.result,
        "error_stage": result.error_stage,
        "error_message": result.error_message,
        "returncode": result.returncode,
        "stdout_tail": result.stdout,
        "stderr_tail": result.stderr,
    }


def _mutant_record(index: int, mutation: Mutation, result: RunResult) -> dict[str, Any]:
    outcome: str
    failure_stage = result.error_stage
    if result.result in {"passed", "empty-control-confirmed"}:
        outcome = "survived"
    elif result.result == "failed":
        outcome = "killed"
    else:
        outcome = "runner_error"
    return {
        "id": f"M{index:04d}",
        "source": mutation.source,
        "line": mutation.line,
        "column": mutation.column,
        "form": mutation.form,
        "operator": mutation.operator,
        "original": mutation.original,
        "replacement": mutation.replacement,
        "outcome": outcome,
        "failure_stage": failure_stage,
        "error_message": result.error_message,
        "returncode": result.returncode,
        "selected_test_count": result.selected_count,
        "runnable_test_count": result.runnable_count,
        "stdout_tail": result.stdout,
        "stderr_tail": result.stderr,
    }


def _print_run(label: str, result: RunResult) -> None:
    print(
        f"{label}: selected={result.selected_count} runnable={result.runnable_count} "
        f"result={result.result or 'runner_error'}"
    )


def _print_mutant(record: dict[str, Any], total: int) -> None:
    print(
        f"{record['id']}/{total}: {record['source']}:{record['line']}:{record['column']} "
        f"operator={record['operator']} outcome={record['outcome']}"
    )


def _report_json(report: dict[str, Any], output_path: Path | None) -> str:
    serialized = json.dumps(report, ensure_ascii=False, indent=2, sort_keys=True)
    if output_path is not None:
        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_text(serialized + "\n", encoding="utf-8")
        print(f"Report written: {output_path}")
    print(serialized)
    return serialized


def _run_mutant(
    root: Path,
    mutation: Mutation,
    tests: list[Path],
    support: list[Path],
    sbcl: str,
    baseline_selected_count: int | None,
    baseline_runnable_count: int | None,
    empty_test_control: bool,
) -> RunResult:
    with tempfile.TemporaryDirectory(prefix="ntop-mutation-mutant-") as temporary:
        project_copy = Path(temporary) / "project"
        try:
            shutil.copytree(root, project_copy, symlinks=True, ignore=_ignore_project_artifacts)
            source_copy = project_copy / mutation.source
            content = source_copy.read_bytes().decode("utf-8")
            if content[mutation.start : mutation.end] != mutation.original:
                return RunResult(
                    result="runner-error",
                    error_stage="source-drift",
                    error_message=f"Source changed after mutation discovery: {mutation.source}",
                )
            changed = (
                content[: mutation.start]
                + mutation.replacement
                + content[mutation.end :]
            )
            source_copy.write_bytes(changed.encode("utf-8"))
        except (OSError, UnicodeDecodeError) as error:
            return RunResult(
                result="runner-error",
                error_stage="mutation-copy",
                error_message=str(error),
            )

        copied_tests = [project_copy / path.relative_to(root) for path in tests]
        copied_support = [project_copy / path.relative_to(root) for path in support]
        if empty_test_control:
            try:
                _empty_test_copies(root, project_copy, tests)
            except OSError as error:
                return RunResult(
                    result="runner-error",
                    error_stage="empty-test-copy",
                    error_message=str(error),
                )
        driver = Path(temporary) / "mutation-driver.lisp"
        try:
            driver.write_text(
                _driver_source(project_copy, copied_tests, copied_support, empty_test_control),
                encoding="utf-8",
            )
        except OSError as error:
            return RunResult(
                result="runner-error", error_stage="driver-write", error_message=str(error)
            )
        try:
            environment = _project_environment(
                root, project_copy, _mutation_cache_directory()
            )
            completed = subprocess.run(
                [sbcl, "--script", str(driver)],
                cwd=project_copy,
                env=environment,
                capture_output=True,
                text=True,
                encoding="utf-8",
                errors="replace",
                check=False,
                timeout=PROCESS_TIMEOUT_SECONDS,
            )
        except subprocess.TimeoutExpired as error:
            stdout = _partial_text(error.stdout)
            stderr = _partial_text(error.stderr)
            output = stdout + "\n" + stderr
            return RunResult(
                selected_count=_last_marker(r"^MUTATION_SELECTED_COUNT=(\d+)$", output),
                runnable_count=_last_marker(r"^MUTATION_RUNNABLE_COUNT=(\d+)$", output),
                result="runner-error",
                error_stage="subprocess-timeout",
                error_message=f"SBCL exceeded {PROCESS_TIMEOUT_SECONDS} seconds.",
                stdout=stdout[-5000:],
                stderr=stderr[-5000:],
            )
        except OSError as error:
            return RunResult(
                result="runner-error", error_stage="sbcl-start", error_message=str(error)
            )
        except Exception as error:
            return RunResult(
                result="runner-error", error_stage="source-registry", error_message=str(error)
            )

        output = completed.stdout + "\n" + completed.stderr
        selected_count = _last_marker(r"^MUTATION_SELECTED_COUNT=(\d+)$", output)
        runnable_count = _last_marker(r"^MUTATION_RUNNABLE_COUNT=(\d+)$", output)
        stage_match = re.findall(
            r"^MUTATION_RUNNER_ERROR_STAGE=(.+)$", output, flags=re.MULTILINE
        )
        error_match = re.findall(r"^MUTATION_RUNNER_ERROR=(.*)$", output, flags=re.MULTILINE)
        result_match = re.findall(
            r"^MUTATION_TEST_RESULT=(passed|failed)$", output, flags=re.MULTILINE
        )
        result = RunResult(
            selected_count=selected_count,
            runnable_count=runnable_count,
            returncode=completed.returncode,
            stdout=completed.stdout[-5000:],
            stderr=completed.stderr[-5000:],
            error_stage=stage_match[-1] if stage_match else None,
            error_message=error_match[-1] if error_match else None,
        )
        if (
            not empty_test_control
            and result.error_stage == "production-load"
            and result.error_message is not None
            and "COMPILE-FILE-ERROR" in result.error_message
            and mutation.source in result.stderr
        ):
            result.result = "failed"
            return result
        empty_match = re.findall(
            r"^MUTATION_EMPTY_CONTROL=(confirmed|failed|not-empty)$",
            output,
            flags=re.MULTILINE,
        )
        if empty_test_control:
            if (
                empty_match
                and empty_match[-1] == "confirmed"
                and completed.returncode == 0
                and selected_count == 0
                and runnable_count == 0
            ):
                result.result = "empty-control-confirmed"
            elif empty_match and empty_match[-1] == "not-empty":
                result.result = "runner-error"
                result.error_stage = "empty-test-control"
                result.error_message = "The copied selected test files produced tests."
            elif empty_match and empty_match[-1] == "failed":
                result.result = "runner-error"
                result.error_stage = "empty-test-control"
                result.error_message = "cl-weave:run-all rejected an empty selection."
            else:
                result.result = "runner-error"
                if result.error_stage is None:
                    result.error_stage = "driver-protocol"
                    result.error_message = "The empty-test control emitted no confirmation."
            return result

        if result.error_stage is None and (
            result.selected_count != baseline_selected_count
            or result.runnable_count != baseline_runnable_count
        ):
            result.result = "runner-error"
            result.error_stage = "test-count-drift"
            result.error_message = (
                "Mutant selected-test counts differ from the passing baseline."
            )
        elif result_match and completed.returncode == (
            0 if result_match[-1] == "passed" else 10
        ):
            result.result = result_match[-1]
        else:
            result.result = "runner-error"
            if result.error_stage is None:
                result.error_stage = "driver-protocol"
                result.error_message = "The SBCL driver did not emit a recognized result marker."
        return result
