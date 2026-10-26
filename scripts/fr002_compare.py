import argparse
import asyncio
import datetime
import os
from runpy import run_path
import signal
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from fr002_compare_results import (
    MAX_LAUNCH_SKEW_SECONDS,
    SAMPLE_COUNT,
    TOLERANCE_PERCENT,
    compare_samples,
    parse_ntop_probe_output,
    write_json,
)
from fr002_powermetrics import parse_powermetrics_output


ROOT = Path(__file__).resolve().parents[1]
resolve_source_registry = run_path(
    str(Path(__file__).resolve().with_name("darwin-workflow.py"))
)["resolve_source_registry"]
PROCESS_TIMEOUT_SECONDS = 30
READY_TIMEOUT_SECONDS = 120


class MeasurementError(RuntimeError):
    pass


def _expected_core_count():
    try:
        result = subprocess.run(
            ["sysctl", "-n", "hw.ncpu"],
            check=True,
            capture_output=True,
            text=True,
        )
        core_count = int(result.stdout.strip())
    except (OSError, subprocess.CalledProcessError, ValueError) as condition:
        raise MeasurementError(f"Could not read hw.ncpu: {condition}") from condition
    if core_count < 1:
        raise MeasurementError(f"Invalid hw.ncpu value: {core_count}")
    return core_count


def _lisp_probe_form():
    return r'''(progn
  (format t "FR002_READY~%")
  (force-output)
  (read-line *standard-input* nil)
  (loop for sample-index from 1 to 5
        do (sleep 0.95d0)
        do (ntop.platform.darwin::read-darwin-cpu-performance-state-data
            (lambda (data)
              (format t
                      "FR002_STATUS~C~D~Cavailable~%"
                      #\Tab
                      sample-index
                      #\Tab)
              (dolist (row data)
                (destructuring-bind (core-index states frequency-table) row
                  (let* ((frequency
                          (ntop.cpu.domain:cpu-weighted-frequency-mhz states
                                                                      frequency-table))
                         (active-ticks
                          (loop for (state-name . residency) in states
                                unless (ntop.cpu.domain::%cpu-excluded-frequency-state-p
                                        state-name)
                                  sum residency))
                         (cluster
                          (cond
                            ((<= 0 core-index 3) "E-Cluster")
                            ((<= 4 core-index 9) "P0-Cluster")
                            ((<= 10 core-index 15) "P1-Cluster")
                            (t "unclassified")))
                         (reason
                          (cond
                            (frequency "none")
                            ((null frequency-table) "missing-dvfs-table")
                            ((plusp active-ticks)
                             "active-frequency-unavailable")
                            (t "no-active-residency"))))
                    (format t
                            "FR002_CORE~C~D~C~D~C~A~C~A~C~D~C~A~%"
                            #\Tab
                            sample-index
                            #\Tab
                            core-index
                            #\Tab
                            cluster
                            #\Tab
                            (if frequency
                                (format nil "~,6F" frequency)
                                "NIL")
                            #\Tab
                            active-ticks
                            #\Tab
                            reason)))))
            (lambda (reason)
              (format t
                      "FR002_STATUS~C~D~Cunavailable~C~S~%"
                      #\Tab
                      sample-index
                      #\Tab
                      reason)))
        do (force-output))
  (sb-ext:exit :code 0))'''


def _decode_output(value):
    return value.decode("utf-8", errors="replace")


async def _terminate_process(process):
    if process is None or process.returncode is not None:
        return
    try:
        os.killpg(process.pid, signal.SIGTERM)
    except ProcessLookupError:
        return
    try:
        await asyncio.wait_for(process.wait(), timeout=3)
    except asyncio.TimeoutError:
        try:
            os.killpg(process.pid, signal.SIGKILL)
        except ProcessLookupError:
            return
        await process.wait()


async def _wait_for_ready(process, stderr_task):
    pending = []
    loop = asyncio.get_running_loop()
    deadline = loop.time() + READY_TIMEOUT_SECONDS
    while loop.time() < deadline:
        try:
            line = await asyncio.wait_for(
                process.stdout.readline(), timeout=deadline - loop.time()
            )
        except asyncio.TimeoutError as condition:
            raise MeasurementError("SBCL did not finish loading ntop in time.") from condition
        if not line:
            stderr = _decode_output(await stderr_task)
            raise MeasurementError(
                "SBCL exited before the measurement was ready. "
                f"Exit code: {process.returncode}. {stderr[-2000:]}"
            )
        decoded = _decode_output(line).rstrip("\r\n")
        if decoded == "FR002_READY":
            return pending, stderr_task
        pending.append(decoded)
        if len(pending) > 100:
            raise MeasurementError("SBCL emitted unexpected output before readiness.")
    raise MeasurementError("SBCL did not finish loading ntop in time.")


async def _run_capture():
    env, _source_count = resolve_source_registry()
    expected_core_count = _expected_core_count()
    sbcl = None
    sbcl_stderr_task = None
    powermetrics = None
    try:
        sbcl = await asyncio.create_subprocess_exec(
            "sbcl",
            "--noinform",
            "--no-sysinit",
            "--no-userinit",
            "--disable-debugger",
            "--non-interactive",
            "--eval",
            "(require :asdf)",
            "--eval",
            '(asdf:load-asd (merge-pathnames "ntop.asd" (uiop:getcwd)))',
            "--eval",
            '(asdf:load-system "ntop")',
            "--eval",
            _lisp_probe_form(),
            cwd=str(ROOT),
            env=env,
            stdin=asyncio.subprocess.PIPE,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
            start_new_session=True,
        )
        sbcl_stderr_task = asyncio.create_task(sbcl.stderr.read())
        startup_output, _ = await _wait_for_ready(sbcl, sbcl_stderr_task)
        powermetrics = await asyncio.create_subprocess_exec(
            "sudo",
            "-n",
            "powermetrics",
            "--samplers",
            "cpu_power",
            "-i",
            "1000",
            "-n",
            str(SAMPLE_COUNT),
            cwd=str(ROOT),
            env=env,
            stdin=asyncio.subprocess.DEVNULL,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
            start_new_session=True,
        )
        powermetrics_started_at = asyncio.get_running_loop().time()
        sbcl.stdin.write(b"start\n")
        await sbcl.stdin.drain()
        ntop_released_at = asyncio.get_running_loop().time()
        launch_skew_seconds = ntop_released_at - powermetrics_started_at
        async def collect_ntop():
            stdout = await sbcl.stdout.read()
            await sbcl.wait()
            return stdout, await sbcl_stderr_task

        try:
            (ntop_stdout, ntop_stderr), (power_stdout, power_stderr) = await asyncio.wait_for(
                asyncio.gather(collect_ntop(), powermetrics.communicate()),
                timeout=PROCESS_TIMEOUT_SECONDS,
            )
        except asyncio.TimeoutError as condition:
            raise MeasurementError("The five-sample capture timed out.") from condition
        if sbcl.returncode != 0:
            raise MeasurementError(
                f"ntop measurement exited {sbcl.returncode}. "
                f"{_decode_output(ntop_stderr)[-2000:]}"
            )
        if powermetrics.returncode != 0:
            raise MeasurementError(
                f"sudo powermetrics exited {powermetrics.returncode}. "
                f"{_decode_output(power_stderr)[-2000:]}"
            )
        ntop_text = "\n".join(startup_output + [_decode_output(ntop_stdout)])
        return {
            "ntop_samples": parse_ntop_probe_output(
                ntop_text, expected_core_count=expected_core_count
            ),
            "powermetrics_samples": parse_powermetrics_output(
                _decode_output(power_stdout)
            ),
            "powermetrics_stderr": _decode_output(power_stderr)[-2000:],
            "expected_ntop_core_count": expected_core_count,
            "capture_start_skew_seconds": launch_skew_seconds,
        }
    finally:
        await asyncio.gather(
            _terminate_process(powermetrics), _terminate_process(sbcl)
        )
        if sbcl_stderr_task is not None:
            await asyncio.gather(sbcl_stderr_task, return_exceptions=True)


def _default_output_path():
    stamp = datetime.datetime.now().strftime("%Y%m%d-%H%M%S")
    return Path.cwd() / f"fr002-powermetrics-comparison-{stamp}-{os.getpid()}.json"


def main():
    parser = argparse.ArgumentParser(
        description="Compare ntop Darwin CPU frequency with powermetrics."
    )
    parser.add_argument("--output", type=Path, help="JSON result path.")
    arguments = parser.parse_args()
    output_path = arguments.output or _default_output_path()
    exit_code = 1
    payload = {
        "requirement": "FR-002 / WP15",
        "created_at": datetime.datetime.now().astimezone().isoformat(),
        "tolerance_percent": TOLERANCE_PERCENT,
        "sample_count": SAMPLE_COUNT,
        "status": "ERROR",
    }
    try:
        if sys.platform != "darwin":
            raise MeasurementError("This comparison requires macOS.")
        if os.geteuid() == 0:
            raise MeasurementError(
                "Run the script as a normal user; only powermetrics is elevated."
            )
        capture = asyncio.run(_run_capture())
        status, comparisons = compare_samples(
            capture["ntop_samples"],
            capture["powermetrics_samples"],
            launch_skew_seconds=capture["capture_start_skew_seconds"],
        )
        payload.update(capture)
        payload["comparisons"] = comparisons
        payload["status"] = status
        payload["difference_denominator"] = "powermetrics average frequency"
        payload["measurement"] = {
            "ntop": "SBCL reads IOReport as the invoking user",
            "powermetrics": "sudo -n powermetrics --samplers cpu_power -i 1000 -n 5",
            "ntop_samples": SAMPLE_COUNT,
            "idle_samples": "excluded from active-frequency averages",
            "maximum_capture_start_skew_ms": MAX_LAUNCH_SKEW_SECONDS * 1000,
        }
        exit_code = 0 if status == "PASS" else 1
    except (MeasurementError, OSError, ValueError, RuntimeError) as condition:
        payload["error"] = str(condition)
        print(f"FR-002 comparison error: {condition}", file=sys.stderr)
    try:
        saved_path = write_json(output_path, payload)
    except OSError as condition:
        print(f"Could not write JSON result: {condition}", file=sys.stderr)
        return 1
    print(f"FR-002 powermetrics comparison: {payload['status']}")
    for comparison in payload.get("comparisons", []):
        if comparison["difference_percent"] is None:
            detail = "N/A"
        else:
            detail = (
                f"ntop={comparison['ntop_average_mhz']:.1f} MHz, "
                f"powermetrics={comparison['powermetrics_average_mhz']:.1f} MHz, "
                f"difference={comparison['difference_percent']:.2f}%"
            )
        print(f"{comparison['cluster']}: {comparison['status']} ({detail})")
    print(f"JSON result: {saved_path}")
    return exit_code


if __name__ == "__main__":
    raise SystemExit(main())
