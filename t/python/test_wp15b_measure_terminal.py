import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "e2e"))

import wp15b_measure_terminal


class FakeSession:
    def __init__(self, pid):
        self.pid = pid
        self.group_id = pid

    def close(self):
        return {
            "pid": self.pid,
            "pgid": self.group_id,
            "pid_absent": True,
            "pgid_absent": True,
            "reaped": True,
            "verified": True,
            "errors": [],
        }


class FakeGrid:
    def __init__(self, rows, columns):
        self.rows = rows
        self.columns = columns

    def feed(self, _raw):
        return True


def measure_with_samples(measurement, missing_redraw=False, missing_quiet=False,
                         sample_4_latency=0.158582, btm="btm",
                         sample_boundary_loads=None, run_start_load=1.0):
    with tempfile.TemporaryDirectory(prefix="ntop-n5-") as directory:
        raw_dir = Path(directory)
        next_pid = 0
        load_calls = 0

        def system_load():
            nonlocal load_calls
            load_calls += 1
            if measurement == "n5" and sample_boundary_loads is None:
                value = 1.0
            elif measurement == "n5" or sample_boundary_loads is not None:
                if load_calls == 1 or load_calls == 42:
                    value = run_start_load
                else:
                    sample_number, boundary = divmod(load_calls - 2, 2)
                    value = sample_boundary_loads[sample_number][boundary]
            elif btm is not None:
                value = 120.0
            else:
                value = 1.0
            return {
                "values": [value, value, value],
                "raw": "mock load",
            }

        def begin_session(_argv, initial_path, *_dimensions):
            nonlocal next_pid
            next_pid += 1
            initial_path.write_bytes(b"initial screen")
            return FakeSession(next_pid)

        def interaction_sample(session, _raw_dir, name, number, _measurement,
                               _screen):
            is_ntop = name == measurement
            latency = sample_4_latency if is_ntop and number == 4 else 0.02
            redraw_complete = not (
                missing_redraw and is_ntop and number == 20
            )
            rows, columns = ((49, 161) if number % 2 else (48, 160))
            initial_rows, initial_columns = (
                (48, 160) if number % 2 else (49, 161)
            )
            return {
                "sample": number,
                "response_observed": not (
                    missing_quiet and is_ntop and number == 20
                ),
                "first_byte_latency_seconds": 0.01,
                "last_byte_latency_seconds": latency,
                "quiet_complete_latency_seconds": 0.27,
                "raw_bytes": 100,
                "raw_output": "sample.ansi",
                "raw_pre_output": "pre.ansi",
                "grid_parse_valid": True,
                "initial_grid_parse_valid": True,
                "initial_rows": initial_rows,
                "initial_columns": initial_columns,
                "raw_initial_output": "initial.ansi",
                "owned_process": {
                    "pid": session.pid,
                    "pgid": session.group_id,
                },
                "rows": rows,
                "columns": columns,
                "redraw_completed": redraw_complete,
                "redraw_completion_latency_seconds": (
                    latency if redraw_complete else None
                ),
                "redraw_completion_reason": (
                    None if redraw_complete else "no_full_frame_at_requested_dimensions"
                ),
                "redraw_completion_evidence": {
                    "status": "complete" if redraw_complete else "not_proven"
                },
                "visible_change_verified": True,
                "visible_change_latency_seconds": latency,
            }

        with (
            patch.object(
                wp15b_measure_terminal,
                "terminal_command",
                side_effect=lambda target, *_args: [target],
            ),
            patch.object(wp15b_measure_terminal, "command_output", return_value="uptime"),
            patch.object(
                wp15b_measure_terminal,
                "system_load",
                side_effect=system_load,
            ),
            patch.object(
                wp15b_measure_terminal, "begin_session", side_effect=begin_session
            ),
            patch.object(
                wp15b_measure_terminal,
                "terminal_interaction_sample",
                side_effect=interaction_sample,
            ),
            patch.object(wp15b_measure_terminal, "TerminalGrid", FakeGrid),
            patch.object(wp15b_measure_terminal, "write_json"),
        ):
            return wp15b_measure_terminal.run_terminal_interactions(
                "ntop", raw_dir, measurement, btm=btm
            )


class TerminalInteractionAcceptanceTests(unittest.TestCase):
    def test_n5_fails_when_any_verified_redraw_exceeds_100ms(self):
        result = measure_with_samples("n5")

        self.assertEqual(result["acceptance"]["latency_samples"], 20)
        self.assertEqual(result["acceptance"]["median_latency_seconds"], 0.02)
        self.assertFalse(result["acceptance"]["passed"])
        self.assertEqual(result["acceptance"]["status"], "fail")

    def test_n5_uses_load_gate_when_btm_argument_is_supplied(self):
        result = measure_with_samples("n5", btm="btm")

        self.assertEqual(result["fallback_mode"], "load_gated")
        self.assertEqual(result["btm_context"]["status"], "not_requested")
        self.assertEqual(result["acceptance"]["policy"], "low_load_ntop_samples")
        self.assertEqual(result["acceptance"]["status"], "fail")

    def test_n5_start_load_above_16_skips_processes_and_samples(self):
        start_load = {"values": [17.0, 2.0, 1.0], "raw": "load average: 17.00"}
        with tempfile.TemporaryDirectory(prefix="ntop-n5-") as directory:
            with (
                patch.object(
                    wp15b_measure_terminal,
                    "terminal_command",
                    side_effect=lambda target, *_args: [target],
                ) as command,
                patch.object(
                    wp15b_measure_terminal,
                    "command_output",
                    return_value="raw uptime output",
                ),
                patch.object(
                    wp15b_measure_terminal,
                    "system_load",
                    return_value=start_load,
                ),
                patch.object(wp15b_measure_terminal, "begin_session") as begin_session,
                patch.object(
                    wp15b_measure_terminal, "terminal_interaction_sample"
                ) as interaction_sample,
                patch.object(wp15b_measure_terminal, "write_json"),
            ):
                result = wp15b_measure_terminal.run_terminal_interactions(
                    "ntop", Path(directory), "n5", btm="btm"
                )

        begin_session.assert_not_called()
        interaction_sample.assert_not_called()
        self.assertEqual([call.args[0] for call in command.call_args_list], ["ntop"])
        self.assertEqual(result["load_before"], start_load)
        self.assertEqual(result["samples"], [])
        self.assertEqual(result["btm_context"]["status"], "not_requested")
        self.assertEqual(result["acceptance"]["status"], "incomplete")
        self.assertFalse(result["acceptance"]["passed"])

    def test_n5_accepts_a_redraw_at_the_100ms_limit(self):
        result = measure_with_samples("n5", sample_4_latency=0.1)

        self.assertEqual(result["acceptance"]["sample_limit_seconds"], 0.1)
        self.assertTrue(result["acceptance"]["passed"])
        self.assertEqual(result["acceptance"]["status"], "pass")

    def test_n5_load_gate_includes_boundary_value_16(self):
        result = measure_with_samples(
            "n5", btm=None, sample_boundary_loads=[(16.0, 16.0)] * 20,
            sample_4_latency=0.02, run_start_load=16.0,
        )

        self.assertEqual(result["acceptance"]["low_load_eligible_samples"], 20)
        self.assertEqual(result["acceptance"]["load_limit_1min"], 16)
        self.assertEqual(result["acceptance"]["load_limit_comparison"], "<=")
        self.assertEqual(result["acceptance"]["status"], "pass")

    def test_n5_load_above_16_leaves_acceptance_incomplete(self):
        result = measure_with_samples(
            "n5", btm=None, sample_boundary_loads=[(16.01, 16.01)] * 20,
            sample_4_latency=0.02,
        )

        self.assertEqual(result["acceptance"]["low_load_eligible_samples"], 0)
        self.assertEqual(result["acceptance"]["status"], "incomplete")
        self.assertTrue(any("at or below 16" in reason
                            for reason in result["acceptance"]["reasons"]))

    def test_n5_one_ineligible_sample_keeps_acceptance_incomplete(self):
        boundaries = [(16.0, 16.0)] * 20
        boundaries[19] = (16.0, 16.01)
        result = measure_with_samples(
            "n5", btm=None, sample_boundary_loads=boundaries,
            sample_4_latency=0.02,
        )

        self.assertEqual(result["acceptance"]["low_load_eligible_samples"], 19)
        self.assertEqual(result["acceptance"]["status"], "incomplete")

    def test_n5_over_limit_redraw_fails_with_ineligible_load_sample(self):
        boundaries = [(16.0, 16.0)] * 20
        boundaries[3] = (16.0, 16.01)
        result = measure_with_samples(
            "n5", btm=None, sample_boundary_loads=boundaries,
        )

        self.assertEqual(result["acceptance"]["low_load_eligible_samples"], 19)
        self.assertEqual(result["acceptance"]["status"], "fail")

    def test_n5_stays_incomplete_when_a_required_redraw_is_missing(self):
        result = measure_with_samples(
            "n5", missing_redraw=True, sample_4_latency=0.02
        )

        self.assertEqual(result["acceptance"]["latency_samples"], 19)
        self.assertFalse(result["acceptance"]["passed"])
        self.assertEqual(result["acceptance"]["status"], "incomplete")

    def test_n5_stays_incomplete_when_quiet_evidence_is_missing(self):
        result = measure_with_samples(
            "n5", missing_quiet=True, sample_4_latency=0.02
        )

        self.assertEqual(result["acceptance"]["quiet_complete_samples"], 19)
        self.assertFalse(result["acceptance"]["passed"])
        self.assertEqual(result["acceptance"]["status"], "incomplete")

    def test_n5_over_limit_redraw_fails_with_other_evidence_missing(self):
        cases = (
            ("missing-redraw", {"missing_redraw": True}),
            ("missing-quiet", {"missing_quiet": True}),
        )
        for mode, btm in (("load-gated", None), ("paired", "btm")):
            for missing_case, missing in cases:
                with self.subTest(mode=mode, missing=missing_case):
                    result = measure_with_samples("n5", btm=btm, **missing)

                    self.assertFalse(result["acceptance"]["passed"])
                    self.assertEqual(result["acceptance"]["status"], "fail")

    def test_n4_keeps_the_existing_median_latency_rule(self):
        result = measure_with_samples("n4")

        self.assertEqual(result["acceptance"]["median_latency_seconds"], 0.02)
        self.assertTrue(result["acceptance"]["passed"])
        self.assertEqual(result["acceptance"]["status"], "pass")

    def test_n4_keeps_the_exclusive_40_load_gate(self):
        result = measure_with_samples(
            "n4", btm=None, sample_boundary_loads=[(40.0, 40.0)] * 20,
            sample_4_latency=0.02,
        )

        self.assertEqual(result["acceptance"]["low_load_eligible_samples"], 0)
        self.assertEqual(result["acceptance"]["load_limit_1min"], 40)
        self.assertEqual(result["acceptance"]["load_limit_comparison"], "<")
        self.assertEqual(result["acceptance"]["status"], "incomplete")


if __name__ == "__main__":
    unittest.main()
