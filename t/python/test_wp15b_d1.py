import ctypes
import sys
import tempfile
import unittest
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import Mock, patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "e2e"))

import wp15b_d1
import wp15b_d1_darwin as darwin
import wp15b_d1_display as display


class Grid:
    def __init__(self, lines):
        self.lines = lines

    def signature(self):
        return self.lines


def process_table(labels, rows):
    widths = (7, 24, 8, 8, 13, 13, 13, 13)
    header = "".join(label.ljust(width) for label, width in zip(labels, widths))
    rendered = [header]
    for values in rows:
        rendered.append(
            "".join(value.ljust(width) for value, width in zip(values, widths))
        )
    return rendered


class D1DisplayTests(unittest.TestCase):
    def test_ntop_internal_sample_trace_is_scoped_to_launch(self):
        with tempfile.TemporaryDirectory() as directory:
            raw_dir = Path(directory)
            observed = {}

            def launch(binary):
                observed["binary"] = binary
                observed["path"] = wp15b_d1.os.environ[
                    "NTOP_D1_SAMPLE_TRACE_PATH"
                ]
                observed["pid"] = wp15b_d1.os.environ[
                    "NTOP_D1_SAMPLE_TRACE_PID"
                ]
                return object()

            with (
                patch.dict(wp15b_d1.os.environ, {
                    "NTOP_D1_SAMPLE_TRACE_PATH": "previous-path",
                    "NTOP_D1_SAMPLE_TRACE_PID": "previous-pid",
                }),
                patch.object(wp15b_d1, "start_ntop", side_effect=launch),
            ):
                child, trace_path = wp15b_d1.start_ntop_with_sample_trace(
                    "/opt/ntop", raw_dir, 222
                )
                self.assertEqual(
                    wp15b_d1.os.environ["NTOP_D1_SAMPLE_TRACE_PATH"],
                    "previous-path",
                )
                self.assertEqual(
                    wp15b_d1.os.environ["NTOP_D1_SAMPLE_TRACE_PID"],
                    "previous-pid",
                )
                self.assertIsNotNone(child)

            self.assertEqual(observed["binary"], "/opt/ntop")
            self.assertEqual(observed["path"], str(trace_path))
            self.assertEqual(observed["pid"], "222")
            self.assertEqual(
                trace_path.read_text(encoding="utf-8"),
                "sampled_at_monotonic_ns\tpid\tread_bytes\twrite_bytes\n",
            )

    def test_read_internal_samples_parses_monotonic_times_and_counters(self):
        with tempfile.TemporaryDirectory() as directory:
            trace_path = Path(directory) / "samples.tsv"
            trace_path.write_text(
                "sampled_at_monotonic_ns\tpid\tread_bytes\twrite_bytes\n"
                "100\t222\tN/A\t4000\n"
                "800000100\t222\t12\t8004000\n",
                encoding="utf-8",
            )

            samples = wp15b_d1.read_internal_samples(trace_path, 222)

        self.assertEqual(samples, [
            {
                "sampled_at_monotonic_ns": 100,
                "pid": 222,
                "read_bytes": None,
                "write_bytes": 4000,
            },
            {
                "sampled_at_monotonic_ns": 800000100,
                "pid": 222,
                "read_bytes": 12,
                "write_bytes": 8004000,
            },
        ])


    def test_cli_keeps_ntop_arguments_and_selects_optional_btm_mode(self):
        raw_dir = Path("/tmp/ntop-wp15b-d1-test")
        for flag, binary, backend in (
            ("--ntop", "/opt/ntop", "ntop"),
            ("--btm", "/opt/btm", "btm"),
        ):
            with self.subTest(backend=backend):
                with (
                    patch.object(wp15b_d1, "prepare_raw_dir", return_value=raw_dir),
                    patch.object(wp15b_d1, "resolve_executable", return_value=binary),
                    patch.object(
                        wp15b_d1, "run_d1", return_value={"status": "pass"}
                    ) as run,
                ):
                    status = wp15b_d1.main(
                        [flag, binary, "--raw-dir", str(raw_dir)]
                    )

                self.assertEqual(status, 0)
                run.assert_called_once_with(binary, raw_dir, backend=backend)

    def test_parse_displayed_byte_units(self):
        cases = (
            ("2.4 MB", 2_400_000),
            ("1.5 MiB", 1_572_864),
            ("42 B", 42),
            ("N/A", None),
        )
        for text, expected in cases:
            with self.subTest(text=text):
                self.assertEqual(display.parse_bytes(text), expected)

    def test_ntop_rate_and_total_write_use_the_matching_pid_row(self):
        lines = process_table(
            ("PID", "Command(n)", "CPU%", "Mem%", "R/s", "W/s", "T.Read", "T.Write"),
            (
                ("111", "WP15B_D1_WRITER-other", "0%", "0%", "0B/s", "0B/s", "0B", "4MB"),
                ("222", display.WRITER_TOKEN, "0%", "0%", "0B/s", "1MB/s", "0B", "2.4MB"),
            ),
        )

        rate, reason, evidence = display.displayed_writer_rate(Grid(lines), 222)

        self.assertIsNone(reason)
        self.assertEqual(rate, 1_000_000)
        self.assertEqual(evidence["writer_row"].split()[0], "222")
        self.assertEqual(evidence["total_write_text"], "2.4MB")
        self.assertEqual(evidence["total_write_bytes"], 2_400_000)

    def test_collected_ntop_observation_keeps_monotonic_time_and_row_counter(self):
        lines = process_table(
            ("PID", "Command(n)", "CPU%", "Mem%", "R/s", "W/s", "T.Read", "T.Write"),
            (("222", display.WRITER_TOKEN, "0%", "0%", "0B/s", "1MB/s", "0B", "2.4MB"),),
        )
        clock = [0.0]
        first_read = [True]

        def fake_select(readers, _writers, _errors, timeout):
            if first_read[0]:
                first_read[0] = False
                return readers, [], []
            clock[0] += timeout
            return [], [], []

        class FakeGrid:
            def __init__(self, _rows, _columns):
                pass

            def feed(self, _data):
                return True

            def signature(self):
                return lines

        class Child:
            fd = 9
            pid = 222
            status = None

            def poll(self):
                return None

            def send(self, _data):
                pass

        class Writer:
            pid = 222
            returncode = None

            def poll(self):
                return None

        with tempfile.TemporaryDirectory() as directory:
            raw_dir = Path(directory)
            (raw_dir / "writer.data").write_bytes(b"x" * 400_000)
            fake_time = SimpleNamespace(
                monotonic=lambda: clock[0],
                monotonic_ns=lambda: 1_234_567_890,
            )
            with (
                patch.object(display, "SAMPLE_LIMIT", 1),
                patch.object(display, "MAX_SECONDS", 10),
                patch.object(display, "time", fake_time),
                patch.object(display, "TerminalGrid", FakeGrid),
                patch.object(display, "load_average", return_value=[1.0, 1.0, 1.0]),
                patch.object(display.select, "select", side_effect=fake_select),
                patch.object(display.os, "read", return_value=b"screen"),
            ):
                samples, observations = display.collect_samples(
                    Child(), Writer(), raw_dir, started=0.0
                )

        self.assertEqual(len(samples), 1)
        self.assertEqual(len(observations), 1)
        self.assertEqual(samples[0]["sampled_at_monotonic_ns"], 1_234_567_890)
        self.assertEqual(samples[0]["total_write_bytes"], 2_400_000)
        self.assertEqual(observations[0]["observation_index"], 1)
        self.assertEqual(
            observations[0]["load_average_before"], [1.0, 1.0, 1.0]
        )
        self.assertEqual(
            observations[0]["load_average_after"], [1.0, 1.0, 1.0]
        )
        self.assertTrue(observations[0]["low_load_eligible"])
        self.assertIn(display.WRITER_TOKEN, samples[0]["writer_row"])

    def test_btm_collector_filters_the_writer_pid_before_sampling(self):
        clock = [0.0]
        filtered = [False]
        initial_read = [False]
        filtered_read = [False]

        class FakeGrid:
            def __init__(self, _rows, _columns):
                pass

            def feed(self, _data):
                return True

        class Child:
            fd = 9

            def __init__(self):
                self.sent = []

            def poll(self):
                return None

            def send(self, data):
                self.sent.append(data)
                filtered[0] = True

        class Writer:
            pid = 222
            returncode = None

            def poll(self):
                return None

        child = Child()

        def fake_select(readers, _writers, _errors, timeout):
            if not initial_read[0]:
                initial_read[0] = True
                return readers, [], []
            if filtered[0] and not filtered_read[0]:
                filtered_read[0] = True
                return readers, [], []
            clock[0] += timeout
            return [], [], []

        with tempfile.TemporaryDirectory() as directory:
            raw_dir = Path(directory)
            (raw_dir / "writer.data").write_bytes(b"x" * 100_000)
            fake_time = SimpleNamespace(
                monotonic=lambda: clock[0],
                monotonic_ns=lambda: 1_234_567_890,
            )
            with (
                patch.object(display, "SAMPLE_LIMIT", 1),
                patch.object(display, "MAX_SECONDS", 5),
                patch.object(display, "time", fake_time),
                patch.object(display, "TerminalGrid", FakeGrid),
                patch.object(
                    display,
                    "displayed_btm_writer_rate",
                    return_value=(950_000, None, {"process_header": "PID W/s"}),
                ),
                patch.object(display, "load_average", return_value=[1.0, 1.0, 1.0]),
                patch.object(display.select, "select", side_effect=fake_select),
                patch.object(display.os, "read", return_value=b"screen"),
            ):
                samples, observations = display.collect_btm_samples(
                    child, Writer(), raw_dir, started=0.0
                )

        self.assertEqual(child.sent, [b"/pid 222\r"])
        self.assertEqual(len(samples), 1)
        self.assertEqual(len(observations), 1)

    def test_btm_collector_parses_write_rate_from_ungrouped_pid_row(self):
        lines = process_table(
            ("PID", "Name", "CPU%", "Mem%", "R/s", "W/s▼", "T.Read", "T.Write"),
            (("222", display.WRITER_TOKEN, "0%", "0%", "0B/s", "950KB/s", "0B", "2.4MB"),),
        )

        rate, reason, evidence = display.displayed_btm_writer_rate(Grid(lines), 222)

        self.assertIsNone(reason)
        self.assertEqual(rate, 950_000)
        self.assertEqual(evidence["text"], "950KB/s")
        self.assertEqual(evidence["writer_row"].split()[0], "222")

    def test_rusage_v2_layout_and_observation_fields(self):
        self.assertEqual(ctypes.sizeof(darwin.RusageInfoV2), 160)
        self.assertEqual(
            darwin.RusageInfoV2.ri_diskio_byteswritten.offset, 152
        )
        timestamps = iter((100, 110, 120))
        api = SimpleNamespace()

        def proc_pid_rusage(pid, flavor, buffer):
            self.assertEqual(pid, 222)
            self.assertEqual(flavor, 2)
            raw = ctypes.cast(
                buffer, ctypes.POINTER(darwin.RusageInfoV2)
            ).contents
            raw.ri_diskio_byteswritten = 7_654_321
            return 0

        api.proc_pid_rusage = Mock(side_effect=proc_pid_rusage)
        clock = darwin.RusageCaptureClock(
            222, api=api, monotonic_ns=lambda: next(timestamps)
        )
        screen_timestamp = clock.monotonic_ns()
        observation = {
            "sampled_at_monotonic_ns": screen_timestamp,
            "writer_pid": 222,
            "total_write_bytes": 7_600_000,
        }
        display_sample = observation.copy()
        summary = darwin.attach_rusage_samples(
            [observation], clock.samples, [display_sample]
        )

        record = observation["darwin_proc_pid_rusage_v2"]
        self.assertEqual(record["source"], darwin.SOURCE)
        self.assertEqual(record["screen_observation_monotonic_ns"], 100)
        self.assertEqual(record["api_read_started_monotonic_ns"], 110)
        self.assertEqual(record["api_read_finished_monotonic_ns"], 120)
        self.assertEqual(record["layout_size_bytes"], 160)
        self.assertEqual(record["write_offset_bytes"], 152)
        self.assertEqual(record["cumulative_write_bytes"], 7_654_321)
        self.assertEqual(observation["total_write_bytes"], 7_600_000)
        self.assertEqual(
            display_sample["darwin_proc_pid_rusage_v2"], record
        )
        self.assertEqual(summary["status"], "pass")
        api.proc_pid_rusage.assert_called_once()

    def test_rusage_api_error_fails_the_diagnostic(self):
        import errno

        timestamps = iter((200, 210, 220))
        api = SimpleNamespace()

        def fail_proc_pid_rusage(_pid, _flavor, _buffer):
            ctypes.set_errno(errno.EPERM)
            return -1

        api.proc_pid_rusage = Mock(side_effect=fail_proc_pid_rusage)
        clock = darwin.RusageCaptureClock(
            222, api=api, monotonic_ns=lambda: next(timestamps)
        )
        screen_timestamp = clock.monotonic_ns()
        observation = {
            "sampled_at_monotonic_ns": screen_timestamp,
            "writer_pid": 222,
            "total_write_bytes": 99,
        }
        summary = darwin.attach_rusage_samples(
            [observation], clock.samples, [observation.copy()]
        )

        record = observation["darwin_proc_pid_rusage_v2"]
        self.assertEqual(record["status"], "error")
        self.assertIsNone(record["cumulative_write_bytes"])
        self.assertEqual(record["errno"], errno.EPERM)
        self.assertEqual(summary["status"], "fail")
        self.assertTrue(summary["independent_of_ntop_internal_sample"])

    def test_btm_mode_writes_700ms_ungrouped_process_config_and_command(self):
        with tempfile.TemporaryDirectory() as directory:
            raw_dir = Path(directory)
            session = Mock()
            with patch.object(display, "PtySession", return_value=session) as pty:
                result = display.start_btm("/opt/btm", raw_dir)

            self.assertIs(result, session)
            command, rows, columns = pty.call_args.args
            self.assertEqual(command, ["/opt/btm", "-C", str(raw_dir / "btm.toml")])
            self.assertEqual((rows, columns), (display.ROWS, display.COLUMNS))
            config = (raw_dir / "btm.toml").read_text(encoding="utf-8")
            self.assertIn("rate = 700", config)
            self.assertIn('default_sort = "W/s"', config)
            self.assertIn('sort_order = "Descending"', config)
            self.assertIn("default_grouped = false", config)
            self.assertIn('type = "proc"', config)

    def test_stop_child_closes_pty_before_signaling_and_checks_reap(self):
        events = []

        class Child:
            fd = 10
            pid = 222
            group_id = 222
            returncode = None

            def poll(self):
                return self.returncode

        child = Child()

        def signal_group(_pid, _pgid, _poll, signum):
            events.append(signum.name)
            child.returncode = 128 + signum.value
            return True

        with (
            patch.object(display.os, "write"),
            patch.object(display.os, "close", side_effect=lambda _fd: events.append("close")),
            patch.object(wp15b_d1, "wait_bounded", side_effect=lambda *_args: None),
            patch.object(wp15b_d1, "signal_owned_group", side_effect=signal_group),
            patch.object(wp15b_d1, "process_exists", return_value=False),
        ):
            result = wp15b_d1.stop_child(child, quit_pty=True)

        self.assertEqual(events, ["close", "SIGTERM"])
        self.assertTrue(result["reaped"])
        self.assertTrue(result["pid_absent"])
        self.assertTrue(result["pgid_absent"])


if __name__ == "__main__":
    unittest.main()
