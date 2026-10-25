#!/usr/bin/env python3
import argparse
import json
import subprocess
import sys
import traceback

from wp15b_measure_process import (
    COLUMNS,
    N1_RUNS,
    N1_SECONDS,
    N2_CADENCE,
    N2_SECONDS,
    RATE_MS,
    ROWS,
    RSS_LIMIT_KIB,
    begin_session,
    btm_config,
    command_output,
    cpu_sample,
    executable,
    group_values,
    parse_cpu_time,
    prepare_raw_dir,
    process_group,
    run_n1,
    run_n2,
    run_n3,
    sample_cpu_run,
    sample_rss,
    system_load,
    terminal_command,
    write_json,
)
from wp15b_measure_support import PtySession, QUIET_SECONDS, TerminalGrid, interaction_result
from wp15b_measure_terminal import run_terminal_interactions, terminal_interaction_sample


def main(argv=None):
    parser = argparse.ArgumentParser(
        description="Measure native ntop N1-N5; visual checks gate N4/N5.",
        epilog="Follows t/e2e/measure-process.lisp and measure-terminal.lisp.")
    parser.add_argument("measurement", choices=("n1", "n2", "n3", "n4", "n5"))
    parser.add_argument("--ntop", required=True, help="Built native ntop executable")
    parser.add_argument(
        "--btm",
        help="btm executable required for N1 or paired high-load fallback",
    )
    parser.add_argument(
        "--raw-dir", required=True,
        help="New temporary evidence directory with an ntop-wp15b prefix",
    )
    args = parser.parse_args(argv)
    try:
        raw = prepare_raw_dir(args.raw_dir)
        ntop = executable(args.ntop)
        btm = executable(args.btm) if args.btm else None
        if args.measurement == "n1" and btm is None:
            parser.error("n1 requires --btm")
        if args.measurement == "n1":
            result = run_n1(ntop, btm, raw)
        elif args.measurement == "n2":
            result = run_n2(ntop, raw)
        elif args.measurement == "n3":
            result = run_n3(ntop, raw, btm)
        else:
            result = run_terminal_interactions(
                ntop, raw, args.measurement, btm)
        print(json.dumps({"status": result["status"], "raw_dir": str(raw)}))
        return 0
    except (OSError, RuntimeError, TimeoutError, ValueError,
            subprocess.SubprocessError) as exc:
        print(f"ERROR: {type(exc).__name__}: {exc}", file=sys.stderr)
        traceback.print_exc(file=sys.stderr)
        return 1

if __name__ == "__main__":
    raise SystemExit(main())
