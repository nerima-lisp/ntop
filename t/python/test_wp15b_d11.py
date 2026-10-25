import importlib
import sys
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory
from types import SimpleNamespace
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "e2e"))

import wp15b_d8_d11_support as d11_support
from wp15b_d8_d11_cli import capture_d11_pair
from wp15b_d8_d11_display import (
    HEIGHT, WIDTH, application_render_signature, compare_d11_screens,
    structure_signature,
)


def memory_screen(
    *,
    used: int,
    free: int,
    swap_used: int,
    label: str = "Used",
    graph: str = "─│─",
    panel_width: int = 40,
    cpu_missing: bool = False,
    memory_values: bool = True,
) -> str:
    rows = [" " * WIDTH for _ in range(HEIGHT)]
    inner_width = panel_width - 2

    def top(title: str, values: str = "") -> str:
        content = f" {title} {values}".ljust(inner_width, "─")
        return ("┌" + content + "┐").ljust(WIDTH)

    def middle(content: str) -> str:
        return ("│" + content.ljust(inner_width) + "│").ljust(WIDTH)

    def bottom() -> str:
        return ("└" + "─" * inner_width + "┘").ljust(WIDTH)

    cpu_values = "N/A N/A N/A" if cpu_missing else f"{used}% {free}% {swap_used}%"
    rows[0] = top("CPU", cpu_values)
    rows[1] = middle(f"AVG {used}% graph {graph}")
    rows[2] = bottom()
    rows[4] = top("Memory")
    if memory_values:
        rows[5] = middle(f"RAM: {used}% 98.0GiB/128.0GiB {label} {free}%")
        rows[6] = middle(f"SWP: {swap_used}% 42.8GiB/44.0GiB")
    else:
        rows[5] = middle("RAM:")
        rows[6] = middle("SWP:")
    rows[7] = bottom()
    return "\n".join(rows)


class D11ScreenComparisonTests(unittest.TestCase):
    def test_module_import_skips_libsystem_off_macos(self):
        with patch.object(d11_support.sys, "platform", "linux"):
            with patch.object(d11_support.ctypes, "CDLL") as load_library:
                importlib.reload(d11_support)

        load_library.assert_not_called()
        importlib.reload(d11_support)

    def test_dynamic_values_and_graph_strokes_do_not_change_signature(self):
        without = memory_screen(used=24, free=75, swap_used=25, graph="─│─")
        with_flag = memory_screen(used=33, free=66, swap_used=42, graph="│─│")

        self.assertNotEqual(without, with_flag)
        without_initial_signature = application_render_signature(without)
        with_initial_signature = application_render_signature(with_flag)
        self.assertIsNotNone(without_initial_signature)
        self.assertEqual(without_initial_signature, with_initial_signature)

        result = compare_d11_screens(
            without,
            with_flag,
            without_rendered=True,
            with_rendered=True,
            free_arc_supplied=True,
        )

        self.assertEqual(structure_signature(without), structure_signature(with_flag))
        self.assertTrue(result["display_signature_equal"])
        self.assertTrue(result["pass"])

    def test_static_memory_label_change_fails_comparison(self):
        without = memory_screen(used=24, free=75, swap_used=25)
        with_flag = memory_screen(used=33, free=66, swap_used=42, label="Disk")

        result = compare_d11_screens(
            without,
            with_flag,
            without_rendered=True,
            with_rendered=True,
            free_arc_supplied=True,
        )

        self.assertNotEqual(structure_signature(without), structure_signature(with_flag))
        self.assertFalse(result["display_signature_equal"])
        self.assertEqual(result["acceptance_status"], "fail")
        self.assertFalse(result["pass"])

    def test_missing_memory_panel_fails_comparison(self):
        without = memory_screen(used=24, free=75, swap_used=25)
        with_flag = without.replace("┌ Memory", "┌ Other ")

        result = compare_d11_screens(
            without,
            with_flag,
            without_rendered=True,
            with_rendered=True,
            free_arc_supplied=True,
        )

        self.assertIsNone(result["with_signature"])
        self.assertIsNone(result["display_signature_equal"])
        self.assertEqual(result["acceptance_status"], "incomplete")
        self.assertFalse(result["pass"])

    def test_panel_geometry_change_fails_comparison(self):
        without = memory_screen(used=24, free=75, swap_used=25, panel_width=40)
        with_flag = memory_screen(used=24, free=75, swap_used=25, panel_width=44)

        result = compare_d11_screens(
            without,
            with_flag,
            without_rendered=True,
            with_rendered=True,
            free_arc_supplied=True,
        )

        self.assertNotEqual(structure_signature(without), structure_signature(with_flag))
        self.assertFalse(result["display_signature_equal"])
        self.assertFalse(result["pass"])

    def test_initial_signature_does_not_require_dynamic_metric_values(self):
        initial = memory_screen(
            used=24, free=75, swap_used=25,
            cpu_missing=True, memory_values=False,
        )

        signature = application_render_signature(initial)

        self.assertIsNotNone(signature)
        self.assertEqual(
            set(signature),
            {"rows", "columns", "panels", "screen_structure"},
        )
        self.assertEqual(signature["rows"], HEIGHT)
        self.assertEqual(signature["columns"], WIDTH)
        self.assertEqual(set(signature["panels"]), {"CPU", "Memory"})
        self.assertEqual(signature["panels"]["CPU"], (0, 0))
        self.assertEqual(signature["panels"]["Memory"], (4, 0))
        self.assertEqual(
            signature["screen_structure"]["value_labels"],
            [[5, 1, "RAM"], [6, 1, "SWP"]],
        )

    def test_panel_heading_positions_are_part_of_signature(self):
        rows = [" " * WIDTH for _ in range(HEIGHT)]
        rows[0] = "┌ CPU ─────────┐┌ Memory ─────────────┐".ljust(WIDTH)
        cpu_right = rows[0].index("┐")
        memory_left = rows[0].index("┌", cpu_right + 1)
        memory_right = rows[0].index("┐", memory_left + 1)
        for y in range(1, 5):
            row = [" "] * WIDTH
            row[0] = row[cpu_right] = "│"
            row[memory_left] = row[memory_right] = "│"
            rows[y] = "".join(row)
        bottom = [" "] * WIDTH
        bottom[0] = bottom[memory_left] = "└"
        bottom[cpu_right] = bottom[memory_right] = "┘"
        for x in range(1, cpu_right):
            bottom[x] = "─"
        for x in range(memory_left + 1, memory_right):
            bottom[x] = "─"
        rows[5] = "".join(bottom)

        signature = application_render_signature("\n".join(rows))

        self.assertIsNotNone(signature)
        self.assertEqual(signature["panels"]["CPU"], (0, 0))
        self.assertEqual(signature["panels"]["Memory"], (0, memory_left))

    def test_initial_terminal_grid_with_panel_titles_is_valid(self):
        rows = [" " * WIDTH for _ in range(HEIGHT)]
        rows[0] = "┌ CPU ─────────┐┌ Memory ─────────────┐".ljust(WIDTH)

        signature = application_render_signature("\n".join(rows))

        self.assertIsNotNone(signature)
        self.assertEqual(signature["panels"], {"CPU": (0, 0), "Memory": (0, 16)})

    def test_optional_lower_panel_changes_full_screen_signature(self):
        initial = memory_screen(used=24, free=75, swap_used=25)
        rows = initial.splitlines()
        rows[10] = ("┌ Disk " + "─" * 30 + "┐").ljust(WIDTH)
        rows[11] = ("│" + " Disk I/O".ljust(36) + "│").ljust(WIDTH)
        rows[12] = ("└" + "─" * 36 + "┘").ljust(WIDTH)
        with_later_panel = "\n".join(rows)

        self.assertNotEqual(
            application_render_signature(initial),
            application_render_signature(with_later_panel),
        )

    def test_missing_cpu_or_memory_panel_is_not_a_valid_initial_screen(self):
        visible = memory_screen(used=24, free=75, swap_used=25)
        no_memory_panel = visible.replace("┌ Memory", "┌ Other ")

        self.assertIsNone(application_render_signature(no_memory_panel))

    def test_cli_rejection_is_incomplete_and_never_passes(self):
        visible = memory_screen(used=24, free=75, swap_used=25)
        rejected = "error: unknown option --free_arc"

        result = compare_d11_screens(
            visible,
            rejected,
            without_rendered=True,
            with_rendered=False,
            free_arc_supplied=True,
        )

        self.assertTrue(result["cli_error_output_detected"])
        self.assertFalse(result["flag_accepted"])
        self.assertEqual(result["acceptance_status"], "incomplete")
        self.assertFalse(result["pass"])

    def test_stop_pty_child_closes_master_before_wait_and_records_actual_signals(self):
        events = []
        wait_results = iter(((False, None), (False, None), (True, -9)))

        def write_master(master, data):
            events.append(("write", master, data))
            return 1

        def close_master(master):
            events.append(("close", master))

        def send_signal(pid, sig):
            events.append(("kill", sig.name))
            if sig == d11_support.signal.SIGTERM:
                return {
                    "signal": sig.name,
                    "sent": False,
                    "error": "ProcessLookupError",
                    "errno": 3,
                }
            return {"signal": sig.name, "sent": True}

        def wait_child(pid, timeout):
            events.append(("wait", timeout))
            return next(wait_results)

        with (
            patch.object(d11_support.signal, "signal",
                         return_value=d11_support.signal.SIG_DFL),
            patch.object(d11_support.os, "write", side_effect=write_master),
            patch.object(d11_support.os, "close", side_effect=close_master),
            patch.object(d11_support, "kill_child", side_effect=send_signal),
            patch.object(d11_support, "wait_child", side_effect=wait_child),
            patch.object(d11_support, "pid_absent", return_value=True),
        ):
            result = d11_support.stop_pty_child(321, 17, False, None)

        self.assertEqual(
            [event[0] for event in events],
            ["write", "close", "wait", "kill", "wait", "kill", "wait"],
        )
        self.assertEqual(
            [event[1] for event in events if event[0] == "wait"],
            [0.5, 1.0, 1.5],
        )
        self.assertEqual(sum(event[1] for event in events if event[0] == "wait"), 3.0)
        self.assertEqual(result["signals_sent"], ["q", "SIGKILL"])
        self.assertEqual(
            result["signal_results"],
            [
                {"signal": "q", "sent": True, "bytes_written": 1},
                {
                    "signal": "SIGTERM",
                    "sent": False,
                    "error": "ProcessLookupError",
                    "errno": 3,
                },
                {"signal": "SIGKILL", "sent": True},
            ],
        )
        self.assertTrue(result["pty_master_closed"])
        self.assertTrue(result["reaped"])
        self.assertTrue(result["pid_absent"])
        self.assertEqual(result["return_code"], -9)

    def test_kill_child_records_only_successful_signal_sends(self):
        with patch.object(d11_support.os, "kill") as send_signal:
            sent = d11_support.kill_child(321, d11_support.signal.SIGTERM)
        send_signal.assert_called_once_with(321, d11_support.signal.SIGTERM)
        self.assertEqual(sent, {"signal": "SIGTERM", "sent": True})

        with patch.object(
            d11_support.os,
            "kill",
            side_effect=ProcessLookupError(3, "No such process"),
        ) as send_signal:
            absent = d11_support.kill_child(321, d11_support.signal.SIGTERM)
        send_signal.assert_called_once_with(321, d11_support.signal.SIGTERM)
        self.assertEqual(
            absent,
            {
                "signal": "SIGTERM",
                "sent": False,
                "error": "ProcessLookupError",
                "errno": 3,
            },
        )

    def test_unconfirmed_cleanup_stops_before_next_capture(self):
        with TemporaryDirectory() as temporary:
            root = Path(temporary)
            args = SimpleNamespace(binary="ntop", working_directory=root)

            def failed_capture(binary, working_directory, arguments,
                               raw_path, display_path, mode):
                return {
                    "argv": [binary, *arguments],
                    "pid": 12345,
                    "application_rendered": False,
                    "application_render_snapshot_count": 0,
                    "cli_error_output_detected": False,
                    "complete_panel_complete": False,
                    "cleanup": {"reaped": False, "pid_absent": False},
                    "elapsed_seconds": 1.0,
                    "raw_path": str(raw_path),
                    "display_path": str(display_path),
                }

            with patch("wp15b_d8_d11_cli.capture_tui", side_effect=failed_capture) as capture:
                runs, pair = capture_d11_pair(
                    args, root, ["--rate", "700ms"], pair_index=1
                )

        self.assertEqual(capture.call_count, 1)
        self.assertEqual(set(runs), {"without"})
        self.assertIsNone(pair["with_capture"])
        self.assertFalse(pair["cleanup_confirmed"])
        self.assertFalse(pair["comparison_complete"])
        self.assertEqual(pair["acceptance_status"], "incomplete")


if __name__ == "__main__":
    unittest.main()
