import sys
import tempfile
import time
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from scripts.process_runner import ProcessTimeoutError, run_bounded


class ProcessRunnerTests(unittest.TestCase):
    def test_timeout_kills_ignoring_parent_and_child(self):
        with tempfile.TemporaryDirectory(prefix="ntop-timeout-") as directory:
            marker = Path(directory) / "survived"
            child_code = (
                "import signal,time; "
                "signal.signal(signal.SIGTERM, signal.SIG_IGN); "
                "time.sleep(0.5); "
                f"open({str(marker)!r}, 'w').write('alive')"
            )
            parent_code = (
                "import signal,subprocess,sys,time; "
                "signal.signal(signal.SIGTERM, signal.SIG_IGN); "
                f"subprocess.Popen([sys.executable, '-c', {child_code!r}]); "
                "time.sleep(5)"
            )
            with self.assertRaises(ProcessTimeoutError):
                run_bounded(
                    [sys.executable, "-c", parent_code],
                    timeout_seconds=0.15,
                    kill_after_seconds=0.15,
                )
            time.sleep(0.7)
            self.assertFalse(marker.exists())

    def test_success_returns_exit_status_and_captured_output(self):
        result = run_bounded(
            [sys.executable, "-c", "print('bounded')"],
            timeout_seconds=2,
            capture_output=True,
        )
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "bounded\n")
