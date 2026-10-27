#!/usr/bin/env python3
import signal

from wp15b_measure_process import (
    COLUMNS,
    RATE_MS,
    ROWS,
    begin_session,
    command_output,
    system_load,
    terminal_command,
    write_json,
)
from wp15b_measure_support import TerminalGrid, interaction_result
from wp15b_terminal_acceptance import (
    finalize_terminal_measurement,
    load_is_eligible,
    load_policy,
    reject_high_start_load,
)
def terminal_interaction_sample(session, raw_dir, name, number, measurement, screen):
    if measurement == "n4":
        return interaction_result(
            session, raw_dir, name, number,
            lambda: session.send(b"e"), screen,
            minimum_observation_seconds=5 * RATE_MS / 1000)
    rows, columns = (49, 161) if number % 2 else (48, 160)
    sample = interaction_result(
        session, raw_dir, name, number,
        lambda rows=rows, columns=columns: session.resize(rows, columns),
        screen, (rows, columns),
        minimum_observation_seconds=5 * RATE_MS / 1000)
    sample.update({"rows": rows, "columns": columns})
    return sample

def run_terminal_interactions(ntop, raw_dir, measurement, btm=None):
    name = measurement.lower()
    paired_fallback = btm is not None and measurement != "n5"
    load_limit_1min, load_limit_comparison = load_policy(measurement)

    argv = terminal_command("ntop", ntop, None, raw_dir)
    session = None
    initial_valid = None
    screen = None
    btm_context = {
        "status": "pending" if paired_fallback else "not_requested",
        "role": "context_only",
        "samples": [],
    }
    if paired_fallback:
        btm_context.update({
            "action_equivalence": (
                "same PTY resize action"
                if measurement == "n5"
                else "same e key byte; semantic equivalence unverified"
            ),
            "limitation": (
                None
                if measurement == "n5"
                else (
                    "The N4 acceptance check requires ntop's Network/Processes "
                    "layout transition. This harness has no verified btm action "
                    "mapped to that transition; btm output is retained as raw "
                    "context without a comparison metric."
                )
            ),
            "pair_order": (
                "odd: btm then ntop; even: ntop then btm; each process is "
                "closed and reaped before the next starts"
            ),
            "command": terminal_command("btm", ntop, btm, raw_dir),
        })
    result = {
        "measurement": measurement.upper(),
        "status": "running",
        "fallback_mode": (
            "paired_high_load" if paired_fallback else "load_gated"
        ),
        "command": argv,
        "process_lifecycle": (
            "one target process at a time; close and reap after each sample"
            if paired_fallback else "one persistent ntop process"
        ),
        "settings": {
            "rows": ROWS,
            "columns": COLUMNS,
            "rate_ms": RATE_MS,
            "samples_requested": 20,
            "minimum_observation_window_seconds": 5 * RATE_MS / 1000,
            "load_limit_1min": load_limit_1min,
            "load_limit_comparison": load_limit_comparison,
            "paired_initial_size_policy": (
                "N5 odd samples start at 48x160 and resize to 49x161; "
                "even samples start at 49x161 and resize to 48x160"
                if paired_fallback and measurement == "n5" else None
            ),
        },
        "samples": [],
        "btm_context": btm_context,
        "acceptance": {
            "status": "incomplete",
            "passed": False,
            "reason": (
                "N5 requires 20 requested-size complete repaint frames and quiet output."
                if measurement == "n5"
                else "N4 requires 20 verified layout toggles and quiet output."
            ),
        },
    }
    output = raw_dir / f"{name}.json"
    write_json(output, result)
    pair_orders = []

    def close_session(target_session):
        previous = signal.signal(signal.SIGINT, signal.SIG_IGN)
        try:
            return target_session.close()
        finally:
            signal.signal(signal.SIGINT, previous)

    def capture_paired_sample(target, number, order):
        target_name = name if target == "ntop" else f"{name}-btm"
        target_argv = argv if target == "ntop" else btm_context["command"]
        initial_path = raw_dir / (
            f"{name}-initial-{number:02d}.ansi"
            if target == "ntop"
            else f"{name}-btm-initial-{number:02d}.ansi"
        )
        if measurement == "n5":
            initial_rows, initial_columns = (
                (48, 160) if number % 2 else (49, 161)
            )
        else:
            initial_rows, initial_columns = ROWS, COLUMNS
        target_session = None
        sample = None
        context_sample = None
        try:
            target_session = begin_session(
                target_argv, initial_path, initial_rows, initial_columns)
            target_screen = TerminalGrid(initial_rows, initial_columns)
            target_initial_valid = target_screen.feed(initial_path.read_bytes())
            sample_load_before = system_load()
            sample = terminal_interaction_sample(
                target_session, raw_dir, target_name, number,
                measurement, target_screen)
            sample_load_after = system_load()
            sample.update({
                "target": target,
                "load_before": sample_load_before,
                "load_after": sample_load_after,
                "load_eligible": (
                    load_is_eligible(
                        measurement, sample_load_before["values"][0],
                        load_limit_1min)
                    and load_is_eligible(
                        measurement, sample_load_after["values"][0],
                        load_limit_1min)
                ),
                "pair_order": list(order),
                "initial_rows": initial_rows,
                "initial_columns": initial_columns,
                "initial_grid_parse_valid": target_initial_valid,
                "raw_initial_output": initial_path.name,
                "owned_process": {
                    "pid": target_session.pid,
                    "pgid": target_session.group_id,
                },
            })
            sample["grid_parse_valid"] &= target_initial_valid
            if target == "ntop":
                result["samples"].append(sample)
            else:
                context_keys = (
                    "sample",
                    "load_before",
                    "load_after",
                    "load_eligible",
                    "response_observed",
                    "first_byte_latency_seconds",
                    "last_byte_latency_seconds",
                    "quiet_complete_latency_seconds",
                    "raw_bytes",
                    "raw_output",
                    "raw_pre_output",
                    "grid_parse_valid",
                    "initial_grid_parse_valid",
                    "initial_rows",
                    "initial_columns",
                    "raw_initial_output",
                    "owned_process",
                )
                context_sample = {
                    key: sample[key] for key in context_keys if key in sample
                }
                context_sample.update({
                    "target": "btm",
                    "role": "context_only",
                    "pair_order": list(order),
                })
                if measurement == "n5":
                    for key in (
                        "rows",
                        "columns",
                        "redraw_completed",
                        "redraw_completion_latency_seconds",
                        "redraw_completion_reason",
                        "redraw_completion_evidence",
                    ):
                        context_sample[key] = sample[key]
                btm_context["samples"].append(context_sample)
            write_json(output, result)
        except BaseException as exc:
            result["status"] = "error"
            result["failure"] = {
                "target": target,
                "sample": number,
                "pair_order": list(order),
                "exception_type": type(exc).__name__,
                "errno": getattr(exc, "errno", None),
                "message": str(exc),
            }
            if target_session is not None:
                result["failure"]["owned_process"] = {
                    "pid": target_session.pid,
                    "pgid": target_session.group_id,
                }
            write_json(output, result)
            raise
        finally:
            if target_session is not None:
                try:
                    cleanup = close_session(target_session)
                except BaseException as exc:
                    if sample is not None:
                        sample["cleanup_error"] = {
                            "exception_type": type(exc).__name__,
                            "errno": getattr(exc, "errno", None),
                            "message": str(exc),
                        }
                    if context_sample is not None:
                        context_sample["cleanup_error"] = {
                            "exception_type": type(exc).__name__,
                            "errno": getattr(exc, "errno", None),
                            "message": str(exc),
                        }
                    result["status"] = "cleanup_failed"
                    result.setdefault("failure", {
                        "target": target,
                        "sample": number,
                    })["cleanup_error"] = {
                        "exception_type": type(exc).__name__,
                        "errno": getattr(exc, "errno", None),
                        "message": str(exc),
                    }
                    write_json(output, result)
                    raise
                if sample is not None:
                    sample["cleanup"] = cleanup
                if context_sample is not None:
                    context_sample["cleanup"] = cleanup
                if not cleanup["verified"]:
                    result["status"] = "cleanup_failed"
                    result["failure"] = {
                        "target": target,
                        "sample": number,
                        "cleanup": cleanup,
                    }
                    write_json(output, result)
                    raise RuntimeError(
                        "PTY child PID/PGID remained after bounded cleanup")
                write_json(output, result)

    try:
        result["uptime_before"] = command_output(["uptime"])
        result["load_before"] = system_load()
        if reject_high_start_load(
            result, measurement, result["load_before"]["values"][0],
            load_limit_1min, load_limit_comparison
        ):
            write_json(output, result)
            return result
        if paired_fallback:
            for number in range(1, 21):
                order = (
                    ("btm", "ntop") if number % 2
                    else ("ntop", "btm")
                )
                pair_orders.append(list(order))
                for target in order:
                    capture_paired_sample(target, number, order)
            btm_context["status"] = "complete"
            btm_context["samples_requested"] = 20
            btm_context["samples_collected"] = len(btm_context["samples"])
            btm_context["pair_orders"] = pair_orders
        else:
            session = begin_session(argv, raw_dir / f"{name}-initial.ansi")
            screen = TerminalGrid(ROWS, COLUMNS)
            initial_valid = screen.feed(
                (raw_dir / f"{name}-initial.ansi").read_bytes())
            result["owned_process"] = {
                "pid": session.pid,
                "pgid": session.group_id,
            }
            for number in range(1, 21):
                sample_load_before = system_load()
                sample = terminal_interaction_sample(
                    session, raw_dir, name, number, measurement, screen)
                sample_load_after = system_load()
                sample.update({
                    "target": "ntop",
                    "load_before": sample_load_before,
                    "load_after": sample_load_after,
                    "load_eligible": (
                        load_is_eligible(
                            measurement, sample_load_before["values"][0],
                            load_limit_1min)
                        and load_is_eligible(
                            measurement, sample_load_after["values"][0],
                            load_limit_1min)
                    ),
                    "initial_grid_parse_valid": initial_valid,
                    "raw_initial_output": f"{name}-initial.ansi",
                })
                sample["grid_parse_valid"] &= initial_valid
                result["samples"].append(sample)
                write_json(output, result)
        result["uptime_after"] = command_output(["uptime"])
        result["load_after"] = system_load()
        finalize_terminal_measurement(
            result,
            measurement,
            paired_fallback,
            btm_context,
            pair_orders,
            load_limit_1min,
            load_limit_comparison,
        )
        write_json(output, result)
        return result
    except BaseException as exc:
        result.setdefault("failure", {
            "exception_type": type(exc).__name__,
            "errno": getattr(exc, "errno", None),
            "message": str(exc),
        })
        if result["status"] == "running":
            result["status"] = "error"
        write_json(output, result)
        raise
    finally:
        if session is not None:
            try:
                cleanup = close_session(session)
            except BaseException as exc:
                result["status"] = "cleanup_failed"
                result.setdefault("failure", {})["cleanup_error"] = {
                    "exception_type": type(exc).__name__,
                    "errno": getattr(exc, "errno", None),
                    "message": str(exc),
                }
                write_json(output, result)
                raise
            result["cleanup"] = cleanup
            if not cleanup["verified"]:
                result["status"] = "cleanup_failed"
                write_json(output, result)
                raise RuntimeError(
                    "PTY child PID/PGID remained after bounded cleanup")
            write_json(output, result)
