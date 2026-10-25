import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "e2e"))

from wp15b_measure_support import TerminalGrid


class TerminalGridTests(unittest.TestCase):
    def test_device_status_request_does_not_invalidate_screen(self):
        grid = TerminalGrid(2, 8)

        self.assertTrue(grid.feed(b"screen\x1b[6n"))
        self.assertEqual(grid.signature()[0], "screen  ")


if __name__ == "__main__":
    unittest.main()
