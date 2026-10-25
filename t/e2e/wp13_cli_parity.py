#!/usr/bin/env python3
"""Compare ntop and btm CLI status codes and diagnostic output channels."""
from __future__ import annotations
import argparse, json, os, platform, re, selectors, signal, subprocess, sys, time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any
from wp13_cli_parity_runtime import (
    CONFIG_PATH_PLACEHOLDER,
    MAX_RETAINED_OUTPUT_BYTES,
    PROCESS_TIMEOUT_SECONDS,
    run_config_case,
    run_tui,
)
ROOT = Path(__file__).resolve().parents[2]
FLAG_CATALOG = ROOT / "src/app/domain-parity-flags.lisp"
COLUMN_CATALOG = ROOT / "src/app/domain-parity-process.lisp"
READ_CHUNK_BYTES = 65536

@dataclass
class CliCase:
    argv: tuple[str, ...]
    expected_status: int
    execution: str
    groups: set[str] = field(default_factory=set)
    sources: list[str] = field(default_factory=list)

def _strings(text: str) -> list[str]:
    return re.findall(r'"([^"]*)"', text)
_FLAG_RECORD = re.compile(
    r'\(\s*"([^"]+)"\s+"([^"]*)"\s+\(([^()]*)\)\s+"([^"]+)"'
    r'\s+\(([^()]*)\)\s+"[^"]*"\s+\([^()]*\)\s*\)',
    re.DOTALL,
)
_COLUMN_RECORD = re.compile(
    r'\(\s*"[^"]+"\s+"[^"]+"\s+\(([^()]*)\)\s+"[^"]+"'
    r'\s+"[^"]+"\s+"[^"]+"\s+\([^()]*\)\s*\)',
    re.DOTALL,
)
def _flag_rows() -> list[list[Any]]:
    text = FLAG_CATALOG.read_text(encoding="utf-8")
    return [[name, short, _strings(aliases), value_type, _strings(values)]
            for name, short, aliases, value_type, values in _FLAG_RECORD.findall(text)]

def _column_aliases() -> list[str]:
    text = COLUMN_CATALOG.read_text(encoding="utf-8")
    aliases = [alias for match in _COLUMN_RECORD.finditer(text)
               for alias in _strings(match.group(1))]
    result = _unique_case_insensitive(aliases)
    if not result:
        raise ValueError("the CLI sample rules require at least one column alias")
    return result

def _case_spellings(row: list[Any]) -> list[str]:
    name, short, aliases = row[0], row[1], row[2]
    spellings = ["--" + name, "--" + name.replace("_", "-")]
    spellings.extend("--" + alias.lstrip("-") for alias in aliases)
    if short:
        spellings.append(short)
    result: list[str] = []
    seen: set[str] = set()
    for spelling in spellings:
        folded = spelling.casefold()
        if folded not in seen:
            result.append(spelling)
            seen.add(folded)
    return result

def _unique_case_insensitive(values: list[str]) -> list[str]:
    result: list[str] = []
    seen: set[str] = set()
    for value in values:
        folded = value.casefold()
        if folded not in seen:
            result.append(value)
            seen.add(folded)
    return result

def _sample_values(row: list[Any], columns: list[str]) -> list[str | None]:
    name, value_type = row[0], row[3]
    if value_type in ("boolean", "control"):
        return [None]
    if value_type == "duration":
        return [{"rate": "251ms", "default_time_value": "30s",
                 "retention": "60s", "time_delta": "1s"}[name]]
    if value_type == "integer":
        return ["2"]
    if value_type == "enum":
        return list(row[4])
    if value_type == "path":
        return [CONFIG_PATH_PLACEHOLDER]
    if value_type == "column-name":
        if not columns:
            raise ValueError("the CLI sample rules require at least one column alias")
        return columns
    raise ValueError(f"unsupported parity flag value type: {value_type}")

def _add_case(cases, index, argv, expected_status, execution, group, source):
    key = (argv, execution)
    case = index.get(key)
    if case is None:
        case = CliCase(argv, expected_status, execution)
        index[key] = case
        cases.append(case)
    elif case.expected_status != expected_status:
        raise ValueError(f"conflicting expected statuses for {argv!r}")
    case.groups.add(group)
    if source not in case.sources:
        case.sources.append(source)

def _generated_valid_case(row: list[Any]) -> tuple[int, str]:
    name, value_type = row[0], row[3]
    if value_type == "control":
        return 0, "capture"
    if value_type == "path":
        return 0, "config"
    if name == "default_widget_count":
        return 2, "capture"
    if name == "hide_k_threads" and not sys.platform.startswith("linux"):
        return 2, "capture"
    return 0, "tui"

def _build_cases() -> list[CliCase]:
    rows = _flag_rows()
    if len(rows) != 52 or len({row[0] for row in rows}) != 52:
        raise ValueError(f"expected 52 unique parity flags, found {len(rows)}")
    columns, cases, index = _column_aliases(), [], {}
    for row in rows:
        name, value_type = row[0], row[3]
        for spelling in _case_spellings(row):
            for value in _sample_values(row, columns):
                argv = (spelling,) if value is None else (spelling, value)
                status, execution = _generated_valid_case(row)
                _add_case(cases, index, argv, status, execution, "valid",
                          f"presentation-cli-test valid sample: {name}")
    for row in rows:
        name, value_type = row[0], row[3]
        spellings = _case_spellings(row)
        if value_type == "control":
            invalid = [((f"--{name}=invalid",), 2)]
        elif value_type == "boolean":
            invalid = [((f"--{name}=false",), 2)]
        elif value_type == "path":
            invalid = [((spelling,), 2) for spelling in spellings]
        elif value_type == "duration":
            boundaries = {"rate": "249ms", "default_time_value": "29999ms",
                          "time_delta": "999ms"}
            values = ["abc"] + ([boundaries[name]] if name in boundaries else [])
            status = 1
            invalid = [((spelling, value), status) for spelling in spellings
                       for value in values]
        else:
            value = {"integer": "not-an-integer", "enum": "not-a-choice",
                     "column-name": "not-a-column"}[value_type]
            status = 1 if value_type == "column-name" else 2
            invalid = [((spelling, value), status) for spelling in spellings]
        for argv, status in invalid:
            _add_case(cases, index, argv, status, "capture", "invalid",
                      f"presentation-cli-test invalid sample: {name}")
    explicit = [
        (("--bogus",), 2, "capture", "WP13/§9.4 --bogus"),
        (("--theme", "bad"), 2, "capture", "WP13/§9.4 --theme bad"),
        (("--default_widget_type", "foo"), 2, "capture", "WP13/§9.4 --default_widget_type foo"),
        (("--rate", "100"), 1, "capture", "presentation-cli-test --rate 100"),
        (("--rate", "250"), 0, "tui", "btm accepts --rate 250"),
        (("--rate", "250ms"), 0, "tui", "btm accepts --rate 250ms"),
        (("--rate", "abc"), 1, "capture", "WP13/§9.4 --rate abc"),
        (("--retention", "59999ms"), 0, "tui", "btm accepts --retention 59999ms"),
        (("-h",), 0, "capture", "WP13/§9.4 -h"),
        (("-V",), 0, "capture", "WP13/§9.4 -V"),
        (("--free_arc",), 0, "tui", "WP13/§9.4 --free_arc"),
        (("--battery",), 0, "tui", "WP13/§9.4 --battery"),
        (("--default_widget_count", "2"), 2, "capture", "CLI conditional config status"),
        (("--celsius", "--kelvin"), 2, "capture", "presentation-cli-test mutually exclusive units"),
        (
            ("--config_location", CONFIG_PATH_PLACEHOLDER),
            0,
            "config",
            "btm creates default config at a missing path",
        ),
    ]
    if sys.platform == "darwin":
        explicit.append((("--hide_k_threads",), 2, "capture", "WP13/§9.4 macOS --hide_k_threads"))
    for argv, status, execution, source in explicit:
        _add_case(cases, index, argv, status, execution, "explicit", source)
    return cases

def _select_cases(cases: list[CliCase], all_cases: bool) -> list[CliCase]:
    if all_cases:
        return cases
    focused_tui = {
        ("--free_arc",),
        ("--battery",),
        ("--rate", "251ms"),
        ("--default_widget_type", "cpu"),
    }
    return [case for case in cases
            if "invalid" in case.groups or "explicit" in case.groups
            or case.argv in focused_tui]

def _run_captured(binary: str, argv: tuple[str, ...]) -> dict[str, Any]:
    result: dict[str, Any] = {"exit_status": None, "timed_out": False,
                              "execution_error": None, "output_channels": {}}
    process: subprocess.Popen[bytes] | None = None
    selector = selectors.DefaultSelector()
    retained = {"stdout": 0, "stderr": 0}
    totals = {"stdout": 0, "stderr": 0}
    try:
        process = subprocess.Popen([binary, *argv], cwd=ROOT, stdin=subprocess.DEVNULL,
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                   start_new_session=True,
                                   env=dict(os.environ, TERM="xterm-256color", LC_ALL="C"))
        for name, pipe in (("stdout", process.stdout), ("stderr", process.stderr)):
            assert pipe is not None
            selector.register(pipe, selectors.EVENT_READ, name)
        deadline = time.monotonic() + PROCESS_TIMEOUT_SECONDS
        drain_deadline: float | None = None
        while selector.get_map():
            now = time.monotonic()
            if now >= deadline and not result["timed_out"]:
                result["timed_out"] = True
                drain_deadline = now + 2.0
                try:
                    os.killpg(process.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
            if drain_deadline is not None and now >= drain_deadline:
                for key in list(selector.get_map().values()):
                    selector.unregister(key.fileobj)
                    key.fileobj.close()
                break
            wait = 0.1 if drain_deadline is not None else max(0.0, min(0.1, deadline - now))
            for key, _ in selector.select(wait):
                name = key.data
                data = os.read(key.fd, READ_CHUNK_BYTES)
                if not data:
                    selector.unregister(key.fileobj)
                    key.fileobj.close()
                    continue
                totals[name] += len(data)
                retained[name] += min(len(data), max(0, MAX_RETAINED_OUTPUT_BYTES - retained[name]))
        if result["timed_out"]:
            try:
                process.wait(timeout=2.0)
            except subprocess.TimeoutExpired:
                try:
                    os.killpg(process.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                process.wait()
        else:
            process.wait()
        result["exit_status"] = process.returncode
    except OSError as error:
        result["execution_error"] = {"type": type(error).__name__, "errno": error.errno}
        if process is not None and process.poll() is None:
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            process.wait()
    finally:
        selector.close()
        if process is not None:
            for pipe in (process.stdout, process.stderr):
                if pipe is not None and not pipe.closed:
                    pipe.close()
    result["output_channels"] = {name: {"nonempty": totals[name] > 0, "bytes": totals[name],
        "retained_bytes": retained[name], "truncated": totals[name] > retained[name]}
        for name in ("stdout", "stderr")}
    return result

def _run_case(case: CliCase, ntop: str, btm: str) -> dict[str, Any]:
    config_files_match: bool | None = None
    if case.execution == "config":
        ntop_result, btm_result, config_files_match = run_config_case(
            case.argv, ntop, btm
        )
    else:
        runner = run_tui if case.execution == "tui" else _run_captured
        ntop_result = runner(ntop, case.argv)
        btm_result = runner(btm, case.argv)
    ntop_status = ntop_result["exit_status"]
    btm_status = btm_result["exit_status"]
    status_match = None if None in (ntop_status, btm_status) else ntop_status == btm_status
    channels_match: bool | None = None
    if case.execution == "capture":
        ntop_channels = ntop_result["output_channels"]
        btm_channels = btm_result["output_channels"]
        channels_match = all(
            ntop_channels[name]["nonempty"] == btm_channels[name]["nonempty"]
            for name in ("stdout", "stderr")
        )
    unexpected: list[str] = []
    for name, result in (("ntop", ntop_result), ("btm", btm_result)):
        if result["execution_error"] is not None:
            unexpected.append(f"{name} execution error")
        if result["timed_out"]:
            unexpected.append(f"{name} timed out")
        status = result["exit_status"]
        if status is not None and status != case.expected_status:
            unexpected.append(f"{name} expected status {case.expected_status}")
        if case.execution == "capture":
            channels = result["output_channels"]
            if not channels["stdout"]["nonempty"] and not channels["stderr"]["nonempty"]:
                unexpected.append(f"{name} produced no captured output")
        elif case.execution in ("tui", "config"):
            if not result["q_sent"]:
                unexpected.append(f"{name} did not receive q")
            if not result["screen_drawn"]:
                unexpected.append(f"{name} TUI screen draw was not observed")
            if not result["cleanup_verified"]:
                unexpected.append(f"{name} cleanup was not verified")
    if case.execution == "config" and config_files_match is not True:
        unexpected.append("default config content or permissions did not match")
    passed = (status_match is True and channels_match is not False
              and config_files_match is not False and not unexpected)
    return {
        "argv": list(case.argv),
        "expected_status": case.expected_status,
        "execution": case.execution,
        "groups": sorted(case.groups),
        "sources": case.sources,
        "ntop": ntop_result,
        "btm": btm_result,
        "status_match": status_match,
        "output_channels_match": channels_match,
        "config_files_match": config_files_match,
        "unexpected": unexpected,
        "pass": passed,
    }

def _summary(available_cases: list[CliCase], planned_cases: list[CliCase],
             results: list[dict[str, Any]]) -> dict[str, int]:
    return {
        "catalog_flags": 52,
        "available_cases": len(available_cases),
        "planned_cases": len(planned_cases),
        "planned_invalid_cases": sum("invalid" in case.groups for case in planned_cases),
        "planned_explicit_cases": sum("explicit" in case.groups for case in planned_cases),
        "planned_tui_cases": sum(case.execution in ("tui", "config")
                                  for case in planned_cases),
        "completed_cases": len(results),
        "passed_cases": sum(result["pass"] for result in results),
        "mismatched_statuses": sum(result["status_match"] is False for result in results),
        "unobserved_statuses": sum(result["status_match"] is None for result in results),
        "mismatched_output_channels": sum(
            result["output_channels_match"] is False for result in results),
        "mismatched_config_files": sum(
            result["config_files_match"] is False for result in results),
        "unexpected_cases": sum(bool(result["unexpected"]) for result in results),
    }

def _markdown(report: dict[str, Any]) -> str:
    summary = report["summary"]
    lines = [
        "# WP13 / §9.4 CLI parity",
        "",
        f"Status: **{report['status']}**",
        f"Case set: `{report['case_set']}`",
        f"Platform: `{report['platform']}`",
        "",
        "## Summary",
        "",
        f"- Catalog flags: {summary['catalog_flags']}",
        f"- Available cases: {summary['available_cases']}",
        f"- Planned cases: {summary['planned_cases']}",
        f"- Planned invalid cases: {summary['planned_invalid_cases']}",
        f"- Planned explicit cases: {summary['planned_explicit_cases']}",
        f"- Planned TUI cases: {summary['planned_tui_cases']}",
        f"- Passed cases: {summary['passed_cases']}",
        f"- Status mismatches: {summary['mismatched_statuses']}",
        f"- Unobserved statuses: {summary['unobserved_statuses']}",
        f"- Output channel mismatches: {summary['mismatched_output_channels']}",
        f"- Default config file mismatches: {summary['mismatched_config_files']}",
        f"- Unexpected cases: {summary['unexpected_cases']}",
        "",
        "## Cases",
        "",
        "| Case | Arguments | Expected | ntop | btm | Draw | Channels | Config | Result |",
        "|---|---|---:|---:|---:|---|---|---|---|",
    ]
    for number, case in enumerate(report["results"], start=1):
        ntop_status = case["ntop"]["exit_status"]
        btm_status = case["btm"]["exit_status"]
        channels = case["output_channels_match"]
        draw = "n/a"
        if case["execution"] in ("tui", "config"):
            drawn = [name for name in ("ntop", "btm") if case[name]["screen_drawn"]]
            draw = "both" if len(drawn) == 2 else drawn[0] if drawn else "none"
        config_files = case["config_files_match"]
        config_status = (
            "match" if config_files is True
            else "mismatch" if config_files is False
            else "n/a"
        )
        args = json.dumps(case["argv"], ensure_ascii=False)
        lines.append(
            f"| {number:03d} | `{args}` | {case['expected_status']} | "
            f"{ntop_status if ntop_status is not None else 'n/a'} | "
            f"{btm_status if btm_status is not None else 'n/a'} | "
            f"{draw} | "
            f"{'match' if channels is True else 'mismatch' if channels is False else 'n/a'} | "
            f"{config_status} | "
            f"{'pass' if case['pass'] else 'fail'} |"
        )
    return "\n".join(lines) + "\n"

def _binary_path(value: str, label: str) -> str:
    path = Path(value).expanduser().resolve(strict=True)
    if not path.is_file() or not os.access(path, os.X_OK):
        raise ValueError(f"{label} must name an executable file")
    return str(path)

def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ntop", help="path to the built ntop executable")
    parser.add_argument("--btm", help="path to the btm executable")
    parser.add_argument("--output", type=Path, help="report file stem")
    parser.add_argument("--all-cases", action="store_true", help="include generated valid cases")
    parser.add_argument("--dry-run", action="store_true", help="write a plan only")
    args = parser.parse_args()
    try:
        available = _build_cases()
        selected = _select_cases(available, args.all_cases)
    except (OSError, ValueError) as error:
        parser.error(str(error))
    if args.dry_run:
        summary = _summary(available, selected, [])
        report = {"requirement": "WP13 / §9.4", "status": "planned",
                  "case_set": "all" if args.all_cases else "focused",
                  "platform": platform.system().lower(), "summary": summary, "results": []}
        if args.output:
            output = args.output.expanduser()
            output.parent.mkdir(parents=True, exist_ok=True)
            output.with_suffix(".json").write_text(
                json.dumps(report, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
            output.with_suffix(".md").write_text(_markdown(report), encoding="utf-8")
        print(json.dumps({"case_set": report["case_set"], "summary": summary},
                         ensure_ascii=False, sort_keys=True))
        return 0
    if not args.ntop or not args.btm or not args.output:
        parser.error("--ntop, --btm, and --output are required unless --dry-run is used")
    try:
        ntop = _binary_path(args.ntop, "ntop")
        btm = _binary_path(args.btm, "btm")
    except (OSError, ValueError) as error:
        parser.error(str(error))
    results = [_run_case(case, ntop, btm) for case in selected]
    summary = _summary(available, selected, results)
    passed = summary["completed_cases"] == summary["passed_cases"] == summary["planned_cases"]
    report = {
        "requirement": "WP13 / §9.4",
        "status": "success" if passed else "error",
        "case_set": "all" if args.all_cases else "focused",
        "platform": platform.system().lower(),
        "case_generation": "src/app/domain-parity-flags.lisp and CLI test sample rules",
        "output_comparison": "captured streams by presence; PTY merged; text unchecked",
        "summary": summary,
        "results": results,
    }
    output = args.output.expanduser()
    json_path = output.with_suffix(".json")
    markdown_path = output.with_suffix(".md")
    json_path.parent.mkdir(parents=True, exist_ok=True)
    json_path.write_text(json.dumps(report, ensure_ascii=False, indent=2, sort_keys=True) + "\n",
                         encoding="utf-8")
    markdown_path.write_text(_markdown(report), encoding="utf-8")
    print(json.dumps({"status": report["status"], "summary": summary,
                      "json_report": str(json_path), "markdown_report": str(markdown_path)},
                     ensure_ascii=False, sort_keys=True))
    return 0 if passed else 1

if __name__ == "__main__":
    raise SystemExit(main())
