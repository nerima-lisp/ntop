import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "e2e"))

from wp15b_measure_support import (
    TerminalGrid,
    infer_layout,
    layout_marker_row,
)


class TerminalGridTests(unittest.TestCase):
    def test_device_status_request_does_not_invalidate_screen(self):
        grid = TerminalGrid(2, 8)

        self.assertTrue(grid.feed(b"screen\x1b[6n"))
        self.assertEqual(grid.signature()[0], "screen  ")


class LayoutMarkerTests(unittest.TestCase):
    def test_default_marker_row_is_found_from_the_screen(self):
        grid = TerminalGrid(48, 160)
        grid.feed("\x1b[1;1H┌ CPU".encode())
        grid.feed("\x1b[13;1H┌ Network".encode())
        grid.feed("\x1b[25;1H┌ Processes".encode())

        self.assertEqual(layout_marker_row(grid, "default"), 12)
        self.assertIsNone(layout_marker_row(grid, "expanded"))
        self.assertEqual(infer_layout(grid)[0], "default")

    def test_expanded_marker_row_is_found_from_the_screen(self):
        grid = TerminalGrid(48, 160)
        grid.feed("\x1b[7;1H┌ Processes".encode())

        self.assertEqual(layout_marker_row(grid, "expanded"), 6)
        self.assertEqual(infer_layout(grid)[0], "expanded")

    def test_duplicate_marker_rows_do_not_identify_a_layout(self):
        grid = TerminalGrid(48, 160)
        grid.feed("\x1b[13;1H┌ Network\x1b[20;1H┌ Network".encode())

        self.assertIsNone(layout_marker_row(grid, "default"))
        self.assertIsNone(infer_layout(grid)[0])


if __name__ == "__main__":
    unittest.main()
