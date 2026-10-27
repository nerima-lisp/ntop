import argparse
import json
import os
import re
import subprocess
import sys
from pathlib import Path

from process_runner import ProcessTimeoutError, run_bounded


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW_TIMEOUT_SECONDS = 1800
KILL_GRACE_SECONDS = 15


def _collect_input_paths(value: object, paths: set[str]) -> None:
    if isinstance(value, dict):
        path = value.get("path")
        if isinstance(path, str):
            paths.add(path)
        for child in value.values():
            _collect_input_paths(child, paths)
    elif isinstance(value, list):
        for child in value:
            _collect_input_paths(child, paths)


def _lisp_string(value: str) -> str:
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def _archive_paths() -> set[str]:
    command = [
        "nix",
        "flake",
        "archive",
        "--json",
        "--no-update-lock-file",
        "--no-write-lock-file",
    ]
    command.append("path:.")
    result = run_bounded(
        command,
        timeout_seconds=WORKFLOW_TIMEOUT_SECONDS,
        kill_after_seconds=KILL_GRACE_SECONDS,
        cwd=str(ROOT),
        capture_output=True,
    )
    if result.returncode != 0:
        raise subprocess.CalledProcessError(result.returncode, "nix flake archive")

    paths: set[str] = set()
    _collect_input_paths(json.loads(result.stdout), paths)
    if not paths:
        raise RuntimeError("Nix returned no archived dependency source paths.")
    return paths


def resolve_source_registry() -> tuple[dict[str, str], int]:
    archived_paths = _archive_paths()
    missing_paths = sorted(path for path in archived_paths if not Path(path).is_dir())
    if missing_paths:
        raise RuntimeError("Nix did not materialize every archived dependency source.")

    source_directories = sorted(
        path
        for path in archived_paths
        if any(
            system.name != "ntop.asd" for system in Path(path).glob("*.asd")
        )
    )
    if not source_directories:
        raise RuntimeError("Nix archives contain no pinned ASDF dependency systems.")
    entries = " ".join(
        f"(:directory {_lisp_string(path)})" for path in source_directories
    )
    project_entry = f"(:directory {_lisp_string(str(ROOT))})"
    env = os.environ.copy()
    env["CL_SOURCE_REGISTRY"] = (
        "(:source-registry :ignore-inherited-configuration "
        f"{project_entry} {entries})"
    )
    return env, len(source_directories)


def _run_sbcl(script: str, env: dict[str, str], capture_output: bool = False):
    return run_bounded(
        ["sbcl", "--script", str(ROOT / script)],
        timeout_seconds=WORKFLOW_TIMEOUT_SECONDS,
        kill_after_seconds=KILL_GRACE_SECONDS,
        cwd=str(ROOT),
        env=env,
        capture_output=capture_output,
    )


def _require_success(label: str, result: subprocess.CompletedProcess) -> None:
    if result.returncode != 0:
        raise RuntimeError(f"{label} failed with exit code {result.returncode}.")


def _write_captured_output(result: subprocess.CompletedProcess) -> None:
    if result.stdout:
        sys.stdout.write(result.stdout)
    if result.stderr:
        sys.stderr.write(result.stderr)


def _run_dependencies(env: dict[str, str]) -> None:
    _require_success("Dependency report", _run_sbcl("scripts/dependency-depth.lisp", env))


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Resolve pinned Nix sources and run bounded macOS workflows."
    )
    parser.add_argument(
        "workflow",
        choices=("resolve", "test", "coverage", "build", "dependencies"),
    )
    parser.add_argument(
        "--coverage-dir",
        type=Path,
        default=ROOT / "coverage" / "mac",
    )
    parser.add_argument("--emit-environment-json", action="store_true")
    arguments = parser.parse_args()
    workflow = arguments.workflow

    env, source_count = resolve_source_registry()
    if workflow == "resolve":
        if arguments.emit_environment_json:
            print(json.dumps({"CL_SOURCE_REGISTRY": env["CL_SOURCE_REGISTRY"]}))
            return 0
        print(f"Resolved {source_count} pinned ASDF dependency source trees.")
        return 0
    if workflow == "dependencies":
        _run_dependencies(env)
        return 0
    if workflow == "test":
        _require_success("Mac test suite", _run_sbcl("run-tests.lisp", env))
        return 0
    if workflow == "coverage":
        arguments.coverage_dir.mkdir(parents=True, exist_ok=True)
        env["NTOP_COVERAGE_DIR"] = str(arguments.coverage_dir.resolve())
        _require_success("Mac coverage", _run_sbcl("scripts/coverage.lisp", env))
        report = arguments.coverage_dir / "cover-index.html"
        if not report.is_file() or report.stat().st_size == 0:
            raise RuntimeError("Coverage did not produce a non-empty HTML report.")
        print(f"Coverage report: {report}")
        return 0
    if workflow == "build":
        result = _run_sbcl("scripts/build.lisp", env, capture_output=True)
        _write_captured_output(result)
        _require_success("Mac executable build", result)
        match = re.search(r"^NTOP_BUILD_OUTPUT=(.+)$", result.stdout or "", re.MULTILINE)
        if match is None:
            raise RuntimeError("The ASDF build did not report its output path.")
        output = Path(match.group(1))
        if not output.is_file() or not os.access(output, os.X_OK):
            raise RuntimeError("ASDF did not produce an executable at its reported path.")
        print(f"Executable: {output}")
        return 0
    raise AssertionError(f"Unhandled workflow: {workflow}")


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except ProcessTimeoutError as condition:
        print(f"ERROR: {condition}", file=sys.stderr)
        raise SystemExit(124) from condition
