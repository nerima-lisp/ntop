from __future__ import annotations

import argparse
import json
import re
import shutil
import signal
import subprocess
import sys
import tempfile
from pathlib import Path

from wp15b_n6_case import ROOT, _run_case, _save_case

def _resolve_executable(value: str) -> str:
    path = shutil.which(value)
    if path is None:
        raise ValueError(f"Executable not found: {value}")
    return str(Path(path).resolve())


def _btm_version(command: list[str]) -> str:
    result = subprocess.run(
        [*command, "--version"],
        capture_output=True,
        check=False,
        timeout=5,
    )
    output = (result.stdout + result.stderr).decode("utf-8", errors="replace").strip()
    if result.returncode != 0 or re.search(r"\b0\.14\.9\b", output) is None:
        raise ValueError(f"Expected btm 0.14.9; reported {output!r}")
    return output


def _btm_config() -> str:
    return (
        "[flags]\n"
        "basic = false\n"
        "dot_marker = false\n"
        "rate = 700\n\n"
        "[processes]\n"
        "case_sensitive = false\n"
        "default_grouped = true\n\n"
        "[[row]]\n[[row.child]]\ntype = \"cpu\"\nratio = 2\n\n"
        "[[row.child]]\ntype = \"mem\"\n\n"
        "[[row]]\n[[row.child]]\ntype = \"net\"\nratio = 2\n\n"
        "[[row.child]]\ntype = \"disk\"\nratio = 2\n\n"
        "[[row.child]]\ntype = \"temp\"\n\n"
        "[[row]]\n[[row.child]]\ntype = \"proc\"\nratio = 2\n"
        "default = true\n"
    )


def main() -> int:
    parser = argparse.ArgumentParser(description="Measure N6 terminal restoration in PTYs.")
    parser.add_argument("--ntop", required=True, help="Path to the built ntop executable.")
    parser.add_argument("--btm", default="btm", help="btm 0.14.9 executable or PATH name.")
    parser.add_argument("--sbcl", default="sbcl", help="SBCL source-runtime executable.")
    arguments = parser.parse_args()
    try:
        ntop = [_resolve_executable(arguments.ntop)]
        btm = [_resolve_executable(arguments.btm)]
        sbcl = _resolve_executable(arguments.sbcl)
        btm_version = _btm_version(btm)
    except (OSError, ValueError, subprocess.SubprocessError) as error:
        print(f"ERROR: {error}", file=sys.stderr)
        return 1

    directory = Path(tempfile.mkdtemp(prefix="ntop-wp15b-n6-", dir="/tmp"))
    config = directory / "btm.toml"
    config.write_text(_btm_config(), encoding="utf-8")
    btm.extend(["-C", str(config)])
    results: list[dict[str, object]] = []
    unavailable = (
        ("btm-lisp-exception", "btm", "not-applicable",
         "btm is Rust; the source-runtime Lisp exception case is separate.", btm),
    )
    for name, target, status, reason, command in unavailable:
        record = {"target": target, "termination": "lisp-exception",
                  "acceptance_scope": "reference",
                  "status": status, "reason": reason, "command_not_run": command,
                  "raw_output": f"{name}.raw"}
        _save_case(directory, name, b"", record)
        results.append(record)
    for target, command in (("ntop", ntop), ("btm", btm)):
        for termination, signum in (
            ("normal", None),
            ("SIGINT", signal.SIGINT),
            ("SIGTERM", signal.SIGTERM),
            ("SIGHUP", signal.SIGHUP),
        ):
            result = _run_case(directory, target, command, termination, signum)
            if target == "btm":
                result["btm_version"] = btm_version
                raw_path = directory / str(result["raw_output"])
                _save_case(directory, raw_path.stem, raw_path.read_bytes(), result)
            results.append(result)

    results.append(
        _run_case(directory, "ntop", ntop, "lisp-exception", None)
    )
    results.append(
        _run_case(directory, "ntop", ntop, "SIGKILL", signal.SIGKILL)
    )

    from wp15b_n6_exception import run_exception_case

    source_exception = run_exception_case(directory, ROOT, sbcl)
    source_exception["acceptance_scope"] = "reference"
    results.append(source_exception)
    acceptance_results = [
        result for result in results
        if result.get("acceptance_scope") not in ("reference", "outside-acceptance")
    ]
    references = [
        result for result in results
        if result.get("acceptance_scope") == "reference"
    ]
    diagnostics = [
        result for result in results
        if result.get("acceptance_scope") == "outside-acceptance"
    ]
    failures = sum(result.get("status") == "FAIL" for result in acceptance_results)
    incomplete = sum(
        result.get("status") == "not-generated" for result in acceptance_results
    )
    summary = {
        "results": results,
        "counts": {
            "pass": sum(
                result.get("status") == "PASS" for result in acceptance_results
            ),
            "fail": failures,
            "not_applicable": sum(
                result.get("status") == "not-applicable" for result in results
            ),
            "not_generated": incomplete,
            "reference_cases": len(references),
            "reference_pass": sum(
                result.get("status") == "PASS" for result in references
            ),
            "reference_fail": sum(
                result.get("status") == "FAIL" for result in references
            ),
            "outside_acceptance_diagnostics": len(diagnostics),
            "diagnostic_fail": sum(
                result.get("diagnostic_status") == "FAIL"
                for result in diagnostics
            ),
        },
        "overall": "FAIL" if failures else "INCOMPLETE" if incomplete else "PASS",
    }
    (directory / "manifest.json").write_text(
        json.dumps(summary, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )
    print(f"N6 evidence: {directory}")
    print(f"N6 result: {summary['overall']}")
    return 1 if failures else 2 if incomplete else 0


if __name__ == "__main__":
    raise SystemExit(main())
