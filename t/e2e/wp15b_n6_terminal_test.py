import os
import pty
import tty
import unittest

from wp15b_n6_terminal import (
    CURSOR_POSITION_QUERY,
    MODE_25_QUERY,
    N6Terminal,
    cursor_probe_record,
    has_dsr_6n_exchange,
    write_replies,
)


class N6TerminalTests(unittest.TestCase):
    def setUp(self):
        self.terminal = N6Terminal(rows=48, columns=160)

    def test_dsr_reply_uses_emulated_cursor_position(self):
        replies = self.terminal.feed(b"\x1b[4;9H\x1b[6n")

        self.assertEqual(len(replies), 1)
        self.assertEqual(replies[0].query, CURSOR_POSITION_QUERY)
        self.assertEqual(replies[0].response, b"\x1b[4;9R")
        self.assertEqual(replies[0].kind, "DSR 6n")
        self.assertEqual(replies[0].as_record()["query_hex"], "1b 5b 36 6e")
        self.assertEqual(replies[0].as_record()["response_hex"], "1b 5b 34 3b 39 52")
        self.assertTrue(has_dsr_6n_exchange([replies[0].as_record()]))

    def test_dsr_exchange_predicate_checks_recorded_query_and_reply(self):
        record = self.terminal.feed(b"\x1b[6n")[0].as_record()

        for field, invalid_value in (
            ("query_hex", "1b 5b 35 6e"),
            ("response_hex", "1b 5b 30 6e"),
            ("kind", "DECRQM"),
        ):
            altered = dict(record, **{field: invalid_value})
            self.assertFalse(has_dsr_6n_exchange([altered]))

    def test_peer_writes_exact_emulator_reply_to_pty(self):
        replies = self.terminal.feed(b"\x1b[?25l" + MODE_25_QUERY)
        master, slave = pty.openpty()
        try:
            tty.setraw(slave)
            records = write_replies(master, replies)

            self.assertEqual(os.read(slave, len(replies[0].response)),
                             replies[0].response)
            self.assertEqual(records[0]["query_hex"], "1b 5b 3f 32 35 24 70")
            self.assertEqual(records[0]["response_hex"], "1b 5b 3f 32 35 3b 32 24 79")
            self.assertEqual(records[0]["status"], 2)
            self.assertEqual(records[0]["mode_state"], "reset")
        finally:
            os.close(master)
            os.close(slave)

    def test_pty_query_receives_the_emulator_dsr_reply(self):
        master, slave = pty.openpty()
        self.terminal.feed(b"\x1b[4;9H")
        try:
            tty.setraw(slave)
            os.write(slave, CURSOR_POSITION_QUERY)
            query = os.read(master, len(CURSOR_POSITION_QUERY))
            replies = self.terminal.feed(query)
            exchanges = write_replies(master, replies)
            response = os.read(slave, len(replies[0].response))

            self.assertEqual(query, CURSOR_POSITION_QUERY)
            self.assertEqual(response, b"\x1b[4;9R")
            self.assertTrue(has_dsr_6n_exchange(exchanges))
        finally:
            os.close(master)
            os.close(slave)

    def test_cursor_probe_record_matches_query_and_received_bytes(self):
        exchanges = [reply.as_record() for reply in self.terminal.feed(
            b"\x1b[4;9H" + CURSOR_POSITION_QUERY
        )]
        output = (
            b"__NTOP_N6_TERMIOS_READY__\n"
            + CURSOR_POSITION_QUERY
            + b"__NTOP_N6_CURSOR_REPLY__0:1b 5b 34 3b 39 52\n"
        )

        record = cursor_probe_record(
            output, exchanges, b"__NTOP_N6_CURSOR_REPLY__",
            b"__NTOP_N6_TERMIOS_READY__",
        )

        self.assertEqual(record["helper_exit_code"], 0)
        self.assertTrue(record["query_matches_pty"])
        self.assertTrue(record["response_matches_peer"])

    def test_decrqm_reports_reset_mode_25(self):
        replies = self.terminal.feed(b"\x1b[?25l" + MODE_25_QUERY)

        self.assertEqual(len(replies), 1)
        self.assertEqual(replies[0].query, MODE_25_QUERY)
        self.assertEqual(replies[0].response, b"\x1b[?25;2$y")
        self.assertEqual(replies[0].as_record()["mode_state"], "reset")

    def test_decrqm_reports_set_mode_25_after_reset(self):
        replies = self.terminal.feed(
            b"\x1b[?25l\x1b[?25h" + MODE_25_QUERY
        )

        self.assertEqual(len(replies), 1)
        self.assertEqual(replies[0].response, b"\x1b[?25;1$y")
        self.assertEqual(replies[0].as_record()["status"], 1)
        self.assertEqual(replies[0].as_record()["mode_state"], "set")

    def test_split_mode_query_is_replied_at_its_position(self):
        self.assertEqual(self.terminal.feed(b"\x1b[?25l\x1b[?25$"), [])

        replies = self.terminal.feed(b"p\x1b[?25h")

        self.assertEqual(len(replies), 1)
        self.assertEqual(replies[0].response, b"\x1b[?25;2$y")
        self.assertEqual(self.terminal.mode_25_status, 1)

    def test_unrecognized_decrqm_mode_is_not_answered_as_mode_25(self):
        replies = self.terminal.feed(b"\x1b[?999$p")

        self.assertEqual(replies, [])


if __name__ == "__main__":
    unittest.main()
