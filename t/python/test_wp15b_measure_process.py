import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "e2e"))

from wp15b_measure_process import (
    n3_evaluate_runs,
    n3_load_eligibility,
    n3_timing_decomposition,
)


def measured_run(load_before=1, load_after=1, elapsed=.05):
    return {
        "load_eligible": load_before <= 16 and load_after <= 16,
        "exit_code": 0,
        "error": None,
        "total_elapsed_seconds": elapsed,
        "timing_decomposition": {"status": "complete"},
    }


class N3TimingTests(unittest.TestCase):
    def test_marker_phases_and_total_are_decomposed(self):
        launch = 1_000_000_000
        stderr = (
            b"N3_PROFILE_MARKER kind=init clock=monotonic_ns value=1040000000\n"
            b"N3_PROFILE_MARKER kind=exit clock=monotonic_ns value=1070000000\n"
        )

        timing = n3_timing_decomposition(launch, stderr, .072)

        self.assertEqual(timing["status"], "complete")
        self.assertAlmostEqual(
            timing["process_start_to_init_hook_seconds"], .04)
        self.assertAlmostEqual(
            timing["post_init_hook_to_exit_hook_seconds"], .03)
        self.assertAlmostEqual(timing["marker_elapsed_seconds"], .07)
        self.assertAlmostEqual(
            timing["process_start_to_init_hook_seconds"]
            + timing["post_init_hook_to_exit_hook_seconds"],
            timing["marker_elapsed_seconds"],
        )
        self.assertEqual(timing["total_elapsed_seconds"], .072)
        self.assertLessEqual(
            timing["marker_elapsed_seconds"], timing["total_elapsed_seconds"])
        self.assertEqual(len(timing["raw_marker_evidence"]), 2)
        self.assertEqual(timing["raw_marker_evidence"][0]["raw"],
                         stderr.decode().splitlines()[0])

    def test_missing_or_duplicate_markers_are_incomplete(self):
        missing = n3_timing_decomposition(0, b"", .02)
        duplicate = n3_timing_decomposition(
            0,
            b"N3_PROFILE_MARKER kind=init clock=monotonic_ns value=1\n"
            b"N3_PROFILE_MARKER kind=init clock=monotonic_ns value=2\n"
            b"N3_PROFILE_MARKER kind=exit clock=monotonic_ns value=3\n",
            .02,
        )

        self.assertEqual(missing["status"], "incomplete")
        self.assertEqual(duplicate["status"], "incomplete")
        self.assertIn("duplicate init marker", duplicate["errors"])

    def test_load_limit_is_inclusive_and_checks_both_sides(self):
        self.assertTrue(n3_load_eligibility(
            {"values": [16.0]}, {"values": [16.0]})["eligible"])
        self.assertFalse(n3_load_eligibility(
            {"values": [16.01]}, {"values": [1.0]})["eligible"])
        self.assertFalse(n3_load_eligibility(
            {"values": [1.0]}, {"values": [16.01]})["eligible"])
        self.assertFalse(n3_load_eligibility(
            {"values": []}, {"values": [1.0]})["eligible"])

    def test_status_requires_three_eligible_runs_and_keeps_high_load_rows(self):
        runs = [
            measured_run(),
            measured_run(load_after=16.01),
            measured_run(),
        ]

        result = n3_evaluate_runs(runs)

        self.assertEqual(len(runs), 3)
        self.assertEqual(result["status"], "incomplete")
        self.assertEqual(result["acceptance"]["recorded_runs"], 3)
        self.assertEqual(result["acceptance"]["low_load_eligible_runs"], 2)
        self.assertEqual(result["acceptance"]["excluded_runs"], [])

    def test_status_passes_only_when_all_runs_meet_limit(self):
        passing = n3_evaluate_runs([measured_run() for _ in range(3)])
        boundary = n3_evaluate_runs(
            [measured_run(elapsed=.1) for _ in range(3)])
        slow = n3_evaluate_runs(
            [measured_run(elapsed=.101) for _ in range(3)])
        short = n3_evaluate_runs([measured_run() for _ in range(2)])

        self.assertEqual(passing["status"], "pass")
        self.assertEqual(boundary["status"], "pass")
        self.assertEqual(slow["status"], "fail")
        self.assertEqual(short["status"], "incomplete")


if __name__ == "__main__":
    unittest.main()
