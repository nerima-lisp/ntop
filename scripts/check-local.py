import argparse
import sys
import unittest
from pathlib import Path

from process_runner import ProcessTimeoutError, run_bounded


MAX_LINES = 500
MAX_COLUMNS = 100
CODE_SUFFIXES = {".asd", ".lisp", ".py"}


def _code_files(root: Path) -> list[Path]:
    files = [root / "ntop.asd", root / "run-tests.lisp"]
    for directory in ("src", "t", "scripts"):
        files.extend(
            path for path in (root / directory).rglob("*")
            if path.is_file() and path.suffix in CODE_SUFFIXES
        )
    return sorted(set(path for path in files if path.is_file()))


def _check_file_limits(files: list[Path]) -> None:
    if not files:
        raise RuntimeError("No Common Lisp or Python source files were selected.")
    violations = []
    for path in files:
        lines = path.read_text(encoding="utf-8").splitlines()
        if len(lines) > MAX_LINES:
            violations.append(f"{path}: {len(lines)} lines exceeds {MAX_LINES}.")
        violations.extend(
            f"{path}:{number}: {len(line)} columns exceeds {MAX_COLUMNS}."
            for number, line in enumerate(lines, 1)
            if len(line) > MAX_COLUMNS
        )
    if violations:
        raise RuntimeError("\n".join(violations))
    print(f"Source limits passed for {len(files)} files.")


def _check_python_supervisor(root: Path) -> None:
    suite = unittest.defaultTestLoader.discover(
        str(root / "t" / "python"), pattern="test_*.py"
    )
    count = suite.countTestCases()
    if count == 0:
        raise RuntimeError("No process-supervisor tests were discovered.")
    result = unittest.TextTestRunner(verbosity=2).run(suite)
    if not result.wasSuccessful():
        raise RuntimeError(
            f"Process-supervisor tests failed: {len(result.failures)} failures, "
            f"{len(result.errors)} errors, {count} selected."
        )


def _check_package_use(root: Path, sbcl: str) -> None:
    result = run_bounded(
        [sbcl, "--script", str(root / "scripts" / "check-package-use.lisp")],
        timeout_seconds=60,
        cwd=str(root),
    )
    if result.returncode != 0:
        raise RuntimeError(
            f"Package-use check failed with exit code {result.returncode}."
        )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--sbcl", default="sbcl")
    arguments = parser.parse_args()
    root = arguments.root.resolve()
    _check_file_limits(_code_files(root))
    _check_python_supervisor(root)
    _check_package_use(root, arguments.sbcl)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (ProcessTimeoutError, RuntimeError) as condition:
        print(f"ERROR: {condition}", file=sys.stderr)
        raise SystemExit(1) from condition
