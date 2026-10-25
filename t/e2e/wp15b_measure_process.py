#!/usr/bin/env python3
import json
import os
import re
import shutil
import statistics
import subprocess
import time
from pathlib import Path

from wp15b_measure_support import PtySession
ROWS, COLUMNS, RATE_MS = 48, 160, 700

N1_RUNS, N1_SECONDS = 5, 60

N2_SECONDS, N2_CADENCE, RSS_LIMIT_KIB = 1800, 30, 200 * 1024

def write_json(path, value): path.write_text(json.dumps(value, indent=2, allow_nan=False) + "\n")

def executable(value):
    path = Path(shutil.which(value) or value).resolve(strict=True)
    if not path.is_file() or not os.access(path, os.X_OK):
        raise ValueError(f"Not an executable file: {value}")
    return str(path)

def prepare_raw_dir(value):
    path = Path(value)
    temporary_root = Path(os.sep) / "tmp"
    if (not path.is_absolute() or path.parent != temporary_root
            or not path.name.startswith("ntop-wp15b-")):
        raise ValueError("--raw-dir must be a new temporary directory with an ntop-wp15b prefix")
    path.mkdir(parents=True, exist_ok=False)
    return path

def command_output(argv):
    result = subprocess.run(argv, capture_output=True, text=True, timeout=10)
    if result.returncode != 0:
        raise RuntimeError(f"Command failed ({result.returncode}): {argv[0]}: "
                           f"{result.stderr}")
    return result.stdout

def parse_cpu_time(value):
    value = value.strip()
    days, value = (value.split("-", 1) if "-" in value else ("0", value))
    parts = value.split(":")
    if len(parts) == 2: hours, minutes, seconds = 0, int(parts[0]), float(parts[1])
    elif len(parts) == 3: hours, minutes, seconds = int(parts[0]), int(parts[1]), float(parts[2])
    else:
        raise ValueError(f"Unexpected ps CPU time: {value!r}")
    return int(days) * 86400 + hours * 3600 + minutes * 60 + seconds

def process_group(pid):
    raw = command_output(["ps", "-o", "pgid=", "-p", str(pid)])
    values = raw.split()
    if len(values) != 1: raise RuntimeError(f"Expected one process group id, got: {raw!r}")
    return int(values[0]), raw

def group_values(group_id, field, parser):
    raw = command_output(["ps", "-o", f"{field}=", "-g", str(group_id)])
    values = [parser(line) for line in raw.splitlines() if line.strip()]
    if not values:
        raise RuntimeError(f"No process {field} values for group {group_id}")
    return sum(values), len(values), raw

def system_load():
    raw = command_output(["sysctl", "-n", "vm.loadavg"])
    values = re.findall(r"(?:\d+\.\d+|\d+|\.\d+)", raw)
    if len(values) != 3: raise RuntimeError(f"Expected three load averages, got: {raw!r}")
    return {"values": [float(value) for value in values], "raw": raw}

def btm_config(path):
    rows = ("[flags]", "basic = false", "dot_marker = false", f"rate = {RATE_MS}",
        "[processes]", "case_sensitive = false", "default_grouped = true", "[[row]]",
        "[[row.child]]", 'type = "cpu"', "ratio = 2", "[[row.child]]", 'type = "mem"',
        "[[row]]", "[[row.child]]", 'type = "net"', "ratio = 2", "[[row.child]]",
        'type = "disk"', "ratio = 2", "[[row.child]]", 'type = "temp"', "[[row]]",
        "[[row.child]]", 'type = "proc"', "ratio = 2", "default = true")
    path.write_text("\n".join(rows) + "\n")

def terminal_command(target, ntop, btm, raw_dir, retention=None):
    if target == "btm":
        config = raw_dir / "btm-config.toml"
        btm_config(config)
        return [btm, "-C", str(config)]
    argv = [ntop, "--rate", "700ms"]
    if retention:
        argv.extend(["--retention", retention])
    return argv

def begin_session(argv, raw_path, rows=ROWS, columns=COLUMNS):
    session = None
    try:
        session = PtySession(argv, rows, columns)
        with raw_path.open("wb") as sink:
            session.read_idle(sink, timeout=20,
                              first_not_before=session.started + 3 * RATE_MS / 1000)
        return session
    except BaseException as exc:
        diagnostic = {"stage": "begin-session",
                      "exception_type": type(exc).__name__,
                      "errno": getattr(exc, "errno", None),
                      "message": str(exc)}
        if session is not None:
            diagnostic["pid"] = session.pid
            diagnostic["pgid"] = session.group_id
            try:
                diagnostic["cleanup"] = session.close()
            except BaseException as cleanup_exc:
                diagnostic["cleanup_error"] = {
                    "exception_type": type(cleanup_exc).__name__,
                    "errno": getattr(cleanup_exc, "errno", None),
                    "message": str(cleanup_exc)}
        write_json(raw_path.with_suffix(".begin-failure.json"), diagnostic)
        raise

def cpu_sample(elapsed, cpu, count, percent, raw):
    return {"elapsed_seconds": elapsed, "cpu_seconds": cpu, "process_count": count,
            "cpu_percent": percent, "raw_ps": raw}

def sample_cpu_run(target, pair, ntop, btm, raw_dir):
    argv = terminal_command(target, ntop, btm, raw_dir)
    stem = f"n1-{target}-{pair:02d}"
    before = {"load": system_load(), "uptime": command_output(["uptime"])}
    stage = "pty-session"
    session = None
    group = None
    result = None
    try:
        session = begin_session(argv, raw_dir / f"{stem}.ansi")
        stage = "process-group"
        group, group_raw = process_group(session.pid)
        stage = "initial-cpu-sample"
        started = time.perf_counter()
        cpu, count, raw = group_values(group, "time", parse_cpu_time)
        prior_at, prior_cpu = time.perf_counter(), cpu
        samples = [cpu_sample(0, cpu, count, None, raw)]
        next_at, end = started + RATE_MS / 1000, started + N1_SECONDS
        stage = "cpu-sampling"
        with (raw_dir / f"{stem}.ansi").open("ab") as sink:
            while time.perf_counter() < end:
                now = time.perf_counter()
                if now < next_at:
                    session.read_for(min(next_at, end) - now, sink)
                    continue
                cpu, count, raw = group_values(group, "time", parse_cpu_time)
                stamp = time.perf_counter()
                samples.append(cpu_sample(stamp - started, cpu, count,
                    100 * (cpu - prior_cpu) / (stamp - prior_at), raw))
                prior_at, prior_cpu = stamp, cpu
                next_at += RATE_MS / 1000
                while next_at <= stamp:
                    next_at += RATE_MS / 1000
        stage = "assemble-result"
        median = statistics.median(x["cpu_percent"] for x in samples
                                   if x["cpu_percent"] is not None)
        result = {"pair": pair, "target": target, "command": argv,
                  "process_group": group, "process_group_raw_ps": group_raw,
                  "duration_seconds": time.perf_counter() - started,
                  "median_interval_cpu_percent": median, "load_before": before,
                  "samples": samples, "raw_pty": f"{stem}.ansi"}
        write_json(raw_dir / f"{stem}-measurement.json", result)
        return result
    except BaseException as exc:
        diagnostic = {"target": target, "pair": pair, "stage": stage,
                      "exception_type": type(exc).__name__,
                      "errno": getattr(exc, "errno", None),
                      "message": str(exc)}
        if session is not None:
            diagnostic["pid"] = session.pid
            diagnostic["pgid"] = session.group_id
            try:
                diagnostic["cleanup"] = session.close()
            except BaseException as cleanup_exc:
                diagnostic["cleanup_error"] = {
                    "exception_type": type(cleanup_exc).__name__,
                    "errno": getattr(cleanup_exc, "errno", None),
                    "message": str(cleanup_exc)}
            session = None
        write_json(raw_dir / f"{stem}-failure.json", diagnostic)
        raise
    finally:
        if session is not None:
            cleanup = session.close()
            if result is not None:
                result["cleanup"] = cleanup
                write_json(raw_dir / f"{stem}-measurement.json", result)

def run_n1(ntop, btm, raw_dir):
    result = {"measurement": "N1", "status": "running", "runs": []}
    output = raw_dir / "n1.json"
    write_json(output, result)
    for pair in range(1, N1_RUNS + 1):
        for target in ("btm", "ntop"):
            try:
                run = sample_cpu_run(target, pair, ntop, btm, raw_dir)
            except BaseException as exc:
                result.update({"status": "failed",
                               "failure": {"pair": pair, "target": target,
                                           "exception_type": type(exc).__name__,
                                           "errno": getattr(exc, "errno", None),
                                           "message": str(exc)}})
                write_json(output, result)
                raise
            run["load_after"] = {
                "load": system_load(), "uptime": command_output(["uptime"])}
            result["runs"].append(run)
            write_json(output, result)
            if not run.get("cleanup", {}).get("verified"):
                result.update({"status": "cleanup_failed",
                               "failure": {"pair": pair, "target": target,
                                           "reason": (
                                               "owned process group was not confirmed absent"
                                           )}})
                write_json(output, result)
                raise RuntimeError("N1 cleanup could not verify the owned process group is absent")
    medians = {
        target: statistics.median(
            r["median_interval_cpu_percent"] for r in result["runs"]
            if r["target"] == target)
        for target in ("btm", "ntop")}
    delta = medians["ntop"] - medians["btm"]
    relative = delta / medians["btm"] * 100 if medians["btm"] else None
    result.update({
        "status": "complete",
        "settings": {"rows": ROWS, "columns": COLUMNS, "rate_ms": RATE_MS,
                     "seconds_per_run": N1_SECONDS, "pairs": N1_RUNS,
                     "order": ["btm", "ntop"]},
        "median_cpu_percent": medians,
        "ntop_minus_btm_percentage_points": delta,
        "relative_increase_percent": relative,
        "acceptance": {"rule": "relative <= 10% OR points <= 0.5",
                       "passed": (relative is not None and relative <= 10) or delta <= 0.5}})
    write_json(output, result)
    return result

def sample_rss(session, group_id, target, started):
    before = time.perf_counter(); load = system_load()
    rss, count, raw = group_values(group_id, "rss", int)
    after = time.perf_counter()
    return {"target_seconds": target, "sample_started_seconds": before - started,
            "sample_completed_seconds": after - started, "rss_kib": rss,
            "process_count": count, "raw_ps": raw, "load": load,
            "one_minute_load_average": load["values"][0]}

def run_n2(ntop, raw_dir):
    argv = terminal_command("ntop", ntop, None, raw_dir, retention="10m")
    session = begin_session(argv, raw_dir / "n2.ansi")
    result = {"measurement": "N2", "status": "running", "command": argv,
        "settings": {"rows": ROWS, "columns": COLUMNS, "rate_ms": RATE_MS,
        "retention": "10m", "duration_seconds": N2_SECONDS,
        "cadence_seconds": N2_CADENCE}, "samples": []}
    output = raw_dir / "n2.json"; write_json(output, result)
    try:
        group, raw = process_group(session.pid)
        result.update({"process_group": group, "process_group_raw_ps": raw})
        started = time.perf_counter()
        result["samples"].append(sample_rss(session, group, 0, started))
        write_json(output, result)
        with (raw_dir / "n2.ansi").open("ab") as sink:
            for target in range(N2_CADENCE, N2_SECONDS + 1, N2_CADENCE):
                delay = started + target - time.perf_counter()
                if delay > 0: session.read_for(delay, sink)
                result["samples"].append(sample_rss(session, group, target, started))
                write_json(output, result)
        samples = result["samples"]
        values = {sample["target_seconds"]: sample["rss_kib"]
                  for sample in samples}
        at_two, at_end = values.get(120), values.get(N2_SECONDS)
        maximum = max(values.values()) if values else None
        growth = (100 * (at_end - at_two) / at_two
                  if at_two not in (None, 0) and at_end is not None else None)
        endpoints_present = at_two is not None and at_end is not None
        passed = (endpoints_present
                  and maximum is not None and maximum <= RSS_LIMIT_KIB
                  and growth is not None and growth <= 5)
        result.update({"status": "complete" if endpoints_present else "incomplete",
            "rss_at_2_minutes_kib": at_two,
            "rss_at_30_minutes_kib": at_end,
            "maximum_rss_kib": maximum,
            "growth_from_2_to_30_minutes_percent": growth,
            "incomplete_reason": (None if endpoints_present else
                "Missing RSS sample at exactly 2 or 30 minutes."),
            "acceptance": {"sample_selection": (
                "all RSS samples, regardless of one-minute load"),
            "sample_count": len(samples),
            "endpoints_present": endpoints_present,
            "rss_limit_kib": RSS_LIMIT_KIB,
            "rss_always_at_or_below_limit": (maximum is not None
                and maximum <= RSS_LIMIT_KIB),
            "growth_limit_percent": 5, "growth_at_or_below_limit": growth is not None
            and growth <= 5, "passed": passed}, "raw_pty": "n2.ansi"})
        write_json(output, result); return result
    except Exception as exc:
        result.update({"status": "error", "error": str(exc)}); write_json(output, result)
        raise
    finally:
        session.close()

def run_n3(ntop, raw_dir, btm=None):
    runs = []
    output = raw_dir / "n3.json"
    command = [ntop, "--version"]
    paired_fallback = btm is not None
    write_json(output, {
        "measurement": "N3",
        "status": "running",
        "command": command,
        "fallback_mode": "paired_high_load" if paired_fallback else "load_gated",
        "runs": runs,
    })
    for pair in range(1, 4):
        targets = ("btm", "ntop") if paired_fallback else ("ntop",)
        for target in targets:
            executable_path = btm if target == "btm" else ntop
            run_command = [executable_path, "--version"]
            stem = (
                f"n3-{target}-run-{pair:02d}"
                if paired_fallback else f"n3-run-{pair:02d}"
            )
            out, err = (
                raw_dir / f"{stem}.{ext}" for ext in ("stdout", "stderr")
            )
            load_before = system_load()
            started = time.perf_counter()
            completed = subprocess.run(
                run_command, capture_output=True, timeout=10)
            elapsed = time.perf_counter() - started
            load_after = system_load()
            out.write_bytes(completed.stdout)
            err.write_bytes(completed.stderr)
            run = {
                "pair": pair,
                "target": target,
                "command": run_command,
                "wall_seconds": elapsed,
                "within_100ms": elapsed <= .1,
                "exit_code": completed.returncode,
                "load_before": load_before,
                "load_after": load_after,
                "load_eligible": (
                    load_before["values"][0] < 40
                    and load_after["values"][0] < 40
                ),
                "stdout": out.name,
                "stderr": err.name,
            }
            runs.append(run)
            write_json(output, {
                "measurement": "N3",
                "status": "running",
                "command": command,
                "fallback_mode": (
                    "paired_high_load" if paired_fallback else "load_gated"
                ),
                "runs": runs,
            })
            if completed.returncode:
                raise RuntimeError(
                    f"{target} --version exited with {completed.returncode}"
                )
    ntop_runs = [run for run in runs if run["target"] == "ntop"]
    btm_runs = [run for run in runs if run["target"] == "btm"]
    eligible_runs = [run for run in ntop_runs if run["load_eligible"]]
    evaluation_runs = ntop_runs if paired_fallback else eligible_runs
    passed = (
        len(evaluation_runs) == 3
        and all(run["within_100ms"] for run in evaluation_runs)
    )
    if paired_fallback:
        status = "pass" if passed else "incomplete"
    else:
        status = (
            "pass" if passed
            else "incomplete" if len(eligible_runs) < 3
            else "fail"
        )
    result = {
        "measurement": "N3",
        "status": status,
        "command": command,
        "fallback_mode": (
            "paired_high_load" if paired_fallback else "load_gated"
        ),
        "runs": runs,
        "median_wall_seconds": (
            statistics.median(run["wall_seconds"] for run in evaluation_runs)
            if evaluation_runs else None
        ),
        "acceptance": {
            "status": status,
            "policy": (
                "ntop_absolute_threshold_with_paired_btm_context"
                if paired_fallback else "low_load_ntop_runs"
            ),
            "limit_seconds": .1,
            "required_ntop_runs": 3,
            "evaluated_ntop_runs": len(evaluation_runs),
            "low_load_eligible_ntop_runs": len(eligible_runs),
            "excluded_ntop_runs": [
                run["pair"] for run in ntop_runs if not run["load_eligible"]
            ],
            "paired_btm_runs": len(btm_runs),
            "passed": passed,
        },
        "decomposition": {
            "status": "not_measured",
            "reason": "--version exposes no hooks to split core load and initialization.",
        },
    }
    write_json(output, result)
    return result
