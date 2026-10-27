import sys
import tempfile
import unittest
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import Mock, patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "e2e"))

import wp15b_d1
import wp15b_d1_display as display
import wp15b_d1_writer as writer


class D1TimingTests(unittest.TestCase):
    def test_writer_syncs_chunks_and_uses_absolute_deadlines(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            handlers, synced = [], []

            def sync(fd):
                synced.append(fd)
                if len(synced) == 2:
                    handlers[0](wp15b_d1.signal.SIGTERM, None)

            ticks = iter((0, 0, 0, 1_000, 1_000, 150_000_000, 150_000_000,
                          150_000_000, 150_001_000, 150_001_000,
                          300_000_000, 300_000_000))
            clock = SimpleNamespace(
                monotonic_ns=lambda: next(ticks),
                clock_gettime_ns=Mock(side_effect=(50, 400)),
                CLOCK_MONOTONIC=1,
                sleep=Mock(),
            )
            with (
                patch.object(wp15b_d1.os, "open", return_value=17),
                patch.object(wp15b_d1.os, "close"),
                patch.object(writer.fcntl, "F_NOCACHE", 73, create=True),
                patch.object(writer.fcntl, "fcntl") as set_cache,
                patch.object(wp15b_d1.signal, "signal",
                             side_effect=lambda _sig, h: handlers.append(h)),
                patch.object(writer, "write_all", side_effect=(100_000, 200_000)) as write,
                patch.object(wp15b_d1.os, "fsync", side_effect=sync),
                patch.object(writer, "time", clock),
            ):
                names = (
                    "writer.data", "writer.json", "ready",
                    "writer-io-samples.tsv", "writer-write-samples.tsv",
                )
                wp15b_d1.writer_child(
                    [str(root / name) for name in names[:4]]
                    + ["1000000", str(root / names[4])]
                )

            report = wp15b_d1.json.loads((root / "writer.json").read_text())
            rows = [line.split("\t") for line in
                    (root / "writer-io-samples.tsv").read_text().splitlines()[1:]]
            set_cache.assert_called_once_with(17, 73, 1)
            self.assertEqual((synced, write.call_count), ([17, 17], 2))
            io_settings = report["io_settings"]
            self.assertTrue(io_settings["f_nocache_enabled"] and io_settings["fsync_per_chunk"])
            self.assertEqual(report["clock_domain"], "CLOCK_MONOTONIC")
            self.assertEqual(report["clock_id"], 1)
            self.assertEqual(report["started_clock_monotonic_ns"], 50)
            self.assertEqual(report["ended_clock_monotonic_ns"], 400)
            chunks = [(row[0], row[-1]) for row in rows]
            self.assertEqual(chunks, [("1", "100000"), ("2", "200000")])
            self.assertEqual((int(rows[1][1]), int(rows[1][4])), (150_000_000, 300_000_000))
            clock.sleep.assert_not_called()
            failed_ready = root / "failed.ready"
            with (
                patch.object(wp15b_d1.os, "open", return_value=18),
                patch.object(wp15b_d1.os, "close") as close_failed,
                patch.object(writer.fcntl, "F_NOCACHE", 73, create=True),
                patch.object(writer.fcntl, "fcntl", side_effect=OSError("denied")),
                patch.object(wp15b_d1.signal, "signal"),
            ):
                failed_names = (
                    "failed.data", "failed.json", "failed.ready",
                    "failed.tsv", "failed-writes.tsv",
                )
                with self.assertRaises(OSError):
                    wp15b_d1.writer_child(
                        [str(root / name) for name in failed_names[:4]]
                        + ["1000000", str(root / failed_names[4])]
                    )
            self.assertFalse(failed_ready.exists())
            close_failed.assert_called_once_with(18)

    def test_write_all_records_each_successful_partial_write(self):
        monotonic_ticks = iter((10, 20, 30, 40))
        clock_ticks = iter((11, 21, 31, 41))
        clock = SimpleNamespace(
            monotonic_ns=lambda: next(monotonic_ticks),
            clock_gettime_ns=lambda _clock_id: next(clock_ticks),
            CLOCK_MONOTONIC=1,
        )
        events = []
        with (
            patch.object(writer, "time", clock),
            patch.object(wp15b_d1.os, "write", side_effect=(3, 2)) as write,
        ):
            total = wp15b_d1.write_all(17, b"hello", 4, 0, events)

        self.assertEqual(total, 5)
        self.assertEqual(write.call_count, 2)
        self.assertEqual([event["bytes_written"] for event in events], [3, 2])
        self.assertEqual(
            [(event["cumulative_bytes_before"], event["cumulative_bytes_after"])
             for event in events],
            [(0, 3), (3, 5)],
        )
        self.assertEqual([event["write_sequence"] for event in events], [1, 2])
        self.assertEqual([event["call_in_chunk"] for event in events], [1, 2])
        self.assertEqual(events[0]["write_started_monotonic_ns"], 10)
        self.assertEqual(events[1]["write_completed_clock_monotonic_ns"], 41)

    def test_d1_single_811k_observation_fails_all_sample_gate(self):
        rates = [1_000_000] * 19 + [811_000]
        observations = [
            {
                "valid": True,
                "w_s_bytes_per_second": rate,
                "load_average_before": [1.0, 1.0, 1.0],
                "load_average_after": [1.0, 1.0, 1.0],
                "low_load_1m_threshold": 16.0,
                "low_load_eligible": True,
            }
            for rate in rates
        ]
        samples = [observation.copy() for observation in observations]

        result = wp15b_d1.evaluate_d1_observations(
            samples, observations, writer_boundaries_complete=True
        )

        self.assertEqual(result["status"], "fail")
        self.assertTrue(result["complete"])
        self.assertFalse(result["passed"])
        self.assertFalse(result["all_samples_within_10_percent"])

    def test_d1_missing_or_ineligible_evidence_stays_incomplete(self):
        rates = [1_000_000] * display.SAMPLE_LIMIT

        def observation(rate):
            return {
                "valid": True,
                "w_s_bytes_per_second": rate,
                "load_average_before": [1.0, 1.0, 1.0],
                "load_average_after": [1.0, 1.0, 1.0],
                "low_load_1m_threshold": 16.0,
                "low_load_eligible": True,
            }

        samples = [
            {"w_s_bytes_per_second": rate}
            for rate in rates
        ]
        complete_observations = [observation(rate) for rate in rates]
        missing_load = [item.copy() for item in complete_observations]
        missing_load[-1].pop("low_load_eligible")
        missing_load[-2].pop("load_average_before")
        missing = wp15b_d1.evaluate_d1_observations(
            samples, missing_load, writer_boundaries_complete=True
        )
        self.assertEqual(missing["status"], "incomplete")
        self.assertIn(
            "low-load-evidence-missing-or-above-threshold",
            missing["incomplete_reasons"],
        )

        ineligible = [item.copy() for item in complete_observations]
        ineligible[-1]["load_average_before"] = [16.1, 2.0, 1.0]
        ineligible[-1]["low_load_eligible"] = False
        above_threshold = wp15b_d1.evaluate_d1_observations(
            samples, ineligible, writer_boundaries_complete=True
        )
        self.assertEqual(above_threshold["status"], "incomplete")
        self.assertTrue(
            display.load_evidence([16.0, 2.0, 1.0], [16.0, 2.0, 1.0])[
                "low_load_eligible"
            ]
        )
        self.assertFalse(
            display.load_evidence([16.1, 2.0, 1.0], [16.0, 2.0, 1.0])[
                "low_load_eligible"
            ]
        )

        invalid_observations = [
            item.copy() for item in complete_observations[:-1]
        ]
        invalid = observation(1_000_000)
        invalid["valid"] = False
        invalid["w_s_bytes_per_second"] = None
        invalid_observations.append(invalid)
        invalid_result = wp15b_d1.evaluate_d1_observations(
            samples[:-1], invalid_observations, writer_boundaries_complete=True
        )
        self.assertEqual(invalid_result["status"], "incomplete")
        self.assertIn(
            "invalid-or-missing-observation",
            invalid_result["incomplete_reasons"],
        )

        missing_boundaries = wp15b_d1.evaluate_d1_observations(
            samples,
            complete_observations,
            writer_boundaries_complete=False,
        )
        self.assertEqual(missing_boundaries["status"], "incomplete")
        self.assertIn(
            "writer-counter-boundary-evidence-incomplete",
            missing_boundaries["incomplete_reasons"],
        )

    def test_d1_twenty_in_range_observations_pass(self):
        observations = [
            {
                "valid": True,
                "w_s_bytes_per_second": 1_000_000,
                "load_average_before": [1.0, 1.0, 1.0],
                "load_average_after": [1.0, 1.0, 1.0],
                "low_load_1m_threshold": 16.0,
                "low_load_eligible": True,
            }
            for _ in range(display.SAMPLE_LIMIT)
        ]
        samples = [observation.copy() for observation in observations]

        result = wp15b_d1.evaluate_d1_observations(
            samples, observations, writer_boundaries_complete=True
        )

        self.assertEqual(result["status"], "pass")
        self.assertTrue(result["passed"])
        self.assertEqual(result["valid_sample_count"], 20)
        self.assertEqual(result["low_load_eligible_count"], 20)

    def test_writer_counter_interval_uses_shared_clock_boundary_bounds(self):
        with tempfile.TemporaryDirectory() as directory:
            trace_path = Path(directory) / "writer-write-samples.tsv"
            events = [
                {
                    "write_sequence": 1,
                    "chunk": 1,
                    "call_in_chunk": 1,
                    "write_started_monotonic_ns": 120_000_000,
                    "write_completed_monotonic_ns": 150_000_000,
                    "write_started_clock_monotonic_ns": 120_000_000,
                    "write_completed_clock_monotonic_ns": 150_000_000,
                    "bytes_written": 100,
                    "cumulative_bytes_before": 0,
                    "cumulative_bytes_after": 100,
                },
                {
                    "write_sequence": 2,
                    "chunk": 2,
                    "call_in_chunk": 1,
                    "write_started_monotonic_ns": 180_000_000,
                    "write_completed_monotonic_ns": 220_000_000,
                    "write_started_clock_monotonic_ns": 180_000_000,
                    "write_completed_clock_monotonic_ns": 220_000_000,
                    "bytes_written": 100,
                    "cumulative_bytes_before": 100,
                    "cumulative_bytes_after": 200,
                },
            ]
            trace_path.write_text(
                "\t".join(wp15b_d1.WRITE_TRACE_FIELDS) + "\n"
                + "".join(
                    "\t".join(str(event[field])
                                 for field in wp15b_d1.WRITE_TRACE_FIELDS)
                    + "\n"
                    for event in events
                ),
                encoding="utf-8",
            )
            write_samples = wp15b_d1.read_writer_write_samples(trace_path)

        internal_samples = [
            {"sampled_at_monotonic_ns": 140_000_000, "write_bytes": 500},
            {"sampled_at_monotonic_ns": 160_000_000, "write_bytes": 700},
            {"sampled_at_monotonic_ns": 230_000_000, "write_bytes": 100_800},
        ]
        intervals = wp15b_d1.writer_counter_interval_bounds(
            internal_samples,
            write_samples,
            {
                "clock_domain": "CLOCK_MONOTONIC",
                "clock_id": wp15b_d1.time.CLOCK_MONOTONIC,
                "started_clock_monotonic_ns": 100_000_000,
                "ended_clock_monotonic_ns": 250_000_000,
            },
        )

        self.assertEqual(len(intervals), 2)
        in_flight_interval, completed_interval = intervals
        self.assertTrue(in_flight_interval["available"])
        self.assertEqual(in_flight_interval["clock_domain"], "CLOCK_MONOTONIC")
        self.assertEqual(
            (
                in_flight_interval["start_writer_byte_bounds"]["lower"],
                in_flight_interval["start_writer_byte_bounds"]["upper"],
            ),
            (0, 100),
        )
        self.assertEqual(
            (
                in_flight_interval["writer_bytes_lower"],
                in_flight_interval["writer_bytes_upper"],
            ),
            (0, 100),
        )
        self.assertTrue(completed_interval["available"])
        self.assertEqual(
            (
                completed_interval["writer_bytes_lower"],
                completed_interval["writer_bytes_upper"],
            ),
            (100, 100),
        )
        self.assertEqual(completed_interval["ntop_write_delta_bytes"], 100_100)
        self.assertLessEqual(
            completed_interval["writer_bytes_per_second_lower"],
            completed_interval["writer_bytes_per_second_upper"],
        )
