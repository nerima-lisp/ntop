"""PTY session and terminal-grid support for WP15b measurements."""
import copy, errno, fcntl, os, pty, re, select, signal, struct, termios, time, unicodedata

QUIET_SECONDS = 0.25

def id_exists(value, group=False):
    try: (os.killpg if group else os.kill)(value, 0); return True
    except ProcessLookupError: return False
    except PermissionError: return True
class PtySession:
    def __init__(self, argv, rows, columns):
        self.started, self.status = time.perf_counter(), None
        self.pid, self.fd = pty.fork()
        if self.pid == 0:
            try:
                fcntl.ioctl(0, termios.TIOCSWINSZ, self._size(rows, columns))
                os.execvpe(argv[0], argv, dict(os.environ, TERM="xterm-256color"))
            except OSError as exc:
                os.write(2, f"exec failed: {exc}\n".encode()); os._exit(127)
        self.group_id = self.pid
        fcntl.ioctl(self.fd, termios.TIOCSWINSZ, self._size(rows, columns))
        os.set_blocking(self.fd, False)
    @staticmethod
    def _size(rows, columns):
        return struct.pack("HHHH", rows, columns, 0, 0)
    def poll(self):
        if self.status is not None:
            return self.status
        pid, status = os.waitpid(self.pid, os.WNOHANG)
        if not pid:
            return None
        self.status = os.WEXITSTATUS(status) if os.WIFEXITED(status) else 128 + os.WTERMSIG(status)
        return self.status
    def _read_once(self, sink):
        try:
            data = os.read(self.fd, 65536)
        except BlockingIOError: return b"", None
        except OSError as exc:
            if exc.errno == errno.EIO: return b"", None
            raise
        if data: now = time.perf_counter(); sink.write(data); sink.flush(); return data, now
        return b"", None
    def read_idle(self, sink, quiet=QUIET_SECONDS, timeout=15,
                  first_not_before=None, allow_empty=False,
                  chunk_records=None, latency_origin=None):
        started = time.perf_counter(); deadline = started + timeout
        chunks, first, last = 0, None, None
        while True:
            now = time.perf_counter()
            quiet_complete = (last if last is not None else started) + quiet
            minimum_end = max(
                quiet_complete,
                first_not_before if first_not_before is not None else quiet_complete)
            if now >= minimum_end and (last is not None or allow_empty):
                if not select.select([self.fd], [], [], 0)[0]:
                    return chunks, first, last, quiet_complete
                data, stamp = self._read_once(sink)
                if not data:
                    return chunks, first, last, quiet_complete
                if chunk_records is not None:
                    chunk_records.append({
                        "offset": chunks,
                        "byte_count": len(data),
                        "latency_seconds": (stamp - latency_origin
                                            if latency_origin is not None else None)})
                chunks += len(data); first = first or stamp; last = stamp
                continue
            if now >= deadline:
                raise TimeoutError("PTY did not reach the expected quiet interval")
            if self.poll() is not None and not select.select([self.fd], [], [], 0)[0]:
                if last is not None or allow_empty:
                    time.sleep(min(0.1, minimum_end - now))
                    continue
                raise RuntimeError(f"PTY exited with status {self.status}")
            wait = min(0.1, deadline - now)
            if last is not None:
                wait = min(wait, max(0, quiet - (now - last)))
            if first_not_before is not None and now < first_not_before:
                wait = min(wait, first_not_before - now)
            if select.select([self.fd], [], [], wait)[0]:
                data, stamp = self._read_once(sink)
                if data:
                    if chunk_records is not None:
                        chunk_records.append({
                            "offset": chunks,
                            "byte_count": len(data),
                            "latency_seconds": (stamp - latency_origin
                                                if latency_origin is not None else None)})
                    chunks += len(data); first = first or stamp; last = stamp
                elif self.poll() is not None:
                    if last is None and not allow_empty:
                        raise RuntimeError(f"PTY exited with status {self.status}")
                    time.sleep(min(0.1, max(0, minimum_end - now)))
    def read_for(self, seconds, sink):
        deadline = time.perf_counter() + seconds
        while time.perf_counter() < deadline:
            if self.poll() is not None:
                raise RuntimeError(f"PTY exited with status {self.status}")
            wait = min(0.1, deadline - time.perf_counter())
            if select.select([self.fd], [], [], max(0, wait))[0]:
                self._read_once(sink)
    def send(self, data):
        view = memoryview(data)
        while view: view = view[os.write(self.fd, view):]
    def resize(self, rows, columns):
        fcntl.ioctl(self.fd, termios.TIOCSWINSZ, self._size(rows, columns))
    def window_size(self):
        size = fcntl.ioctl(self.fd, termios.TIOCGWINSZ, self._size(0, 0))
        return struct.unpack("HHHH", size)[:2]
    def close(self):
        errors = []
        if self.poll() is None:
            try:
                self.send(b"q")
            except OSError as exc:
                errors.append({"operation": "send-q",
                               "exception_type": type(exc).__name__,
                               "errno": exc.errno})
            self._wait_group(5)
        for sig in (signal.SIGTERM, signal.SIGKILL):
            if not id_exists(self.group_id, True):
                break
            try:
                os.killpg(self.group_id, sig)
            except ProcessLookupError:
                break
            except OSError as exc:
                errors.append({"operation": "killpg",
                               "signal": sig.name,
                               "exception_type": type(exc).__name__,
                               "errno": exc.errno})
                break
            self._wait_group(2)
        try:
            os.close(self.fd)
        except OSError as exc:
            errors.append({"operation": "close-pty",
                           "exception_type": type(exc).__name__,
                           "errno": exc.errno})
        self.fd = -1
        self._wait_group(2)
        self.poll()
        pid_absent = not id_exists(self.pid)
        pgid_absent = not id_exists(self.group_id, True)
        return {"pid": self.pid, "pgid": self.group_id,
                "pid_absent": pid_absent, "pgid_absent": pgid_absent,
                "reaped": self.status is not None,
                "verified": pid_absent and pgid_absent,
                "errors": errors}
    def _wait_group(self, seconds):
        deadline = time.perf_counter() + seconds
        while id_exists(self.group_id, True) and time.perf_counter() < deadline:
            self.poll(); time.sleep(0.05)
class TerminalGrid:
    def __init__(self, rows, columns):
        self.rows, self.columns = rows, columns
        self.cells = [[" "] * columns for _ in range(rows)]
        self.row = self.column = 0; self.valid = True
    def resize(self, rows, columns):
        cells = [[" "] * columns for _ in range(rows)]
        for r in range(min(rows, self.rows)):
            cells[r][:min(columns, self.columns)] = self.cells[r][:columns]
        self.rows, self.columns, self.cells = rows, columns, cells
        self.row = min(self.row, rows - 1); self.column = min(self.column, columns - 1)
    def signature(self):
        return tuple(map("".join, self.cells))
    def _line_feed(self):
        if self.row < self.rows - 1: self.row += 1
        else: self.cells = self.cells[1:] + [[" "] * self.columns]
    def _put(self, char):
        if unicodedata.combining(char) and self.column:
            self.cells[self.row][self.column - 1] += char; return
        if self.column >= self.columns: self.column = 0; self._line_feed()
        self.cells[self.row][self.column] = char
        self.column += 2 if unicodedata.east_asian_width(char) in "WF" else 1
    def _erase(self, final, mode):
        if final == "J" and mode in (2, 3):
            self.cells = [[" "] * self.columns for _ in range(self.rows)]
        elif final == "J" and mode == 0:
            self.cells[self.row][self.column:] = [" "] * (self.columns - self.column)
            for r in range(self.row + 1, self.rows): self.cells[r] = [" "] * self.columns
        elif final == "J" and mode == 1:
            for r in range(self.row): self.cells[r] = [" "] * self.columns
            self.cells[self.row][:self.column + 1] = [" "] * (self.column + 1)
        elif final == "K":
            start, end = ((self.column, self.columns) if mode == 0
                          else (0, self.column + 1))
            self.cells[self.row][start:end] = [" "] * (end - start)
    def _csi(self, raw, final):
        private = raw[:1] in "?<>!"; params = raw[1:] if private else raw
        if params and not re.fullmatch(r"[0-9;]*", params): self.valid = False; return
        nums = [int(v or 0) for v in params.split(";")] if params else []
        n = nums[0] if nums else 0
        if private and final in "hl": return
        if final in "Hf":
            self.row = min(self.rows - 1, max(0, (n or 1) - 1))
            col = nums[1] if len(nums) > 1 else 1
            self.column = min(self.columns - 1, max(0, col - 1))
        elif final in "ABCD":
            axis = "row" if final in "AB" else "column"; sign = 1 if final in "BC" else -1
            size = self.rows if axis == "row" else self.columns
            value = getattr(self, axis) + sign * max(1, n)
            setattr(self, axis, max(0, min(size - 1, value)))
        elif final in "G`": self.column = max(0, min(self.columns - 1, (n or 1) - 1))
        elif final == "d": self.row = max(0, min(self.rows - 1, (n or 1) - 1))
        elif final in "JK": self._erase(final, n)
        elif final in "mn": return
        elif final in "su":
            self.saved = ((self.row, self.column) if final == "s"
                          else getattr(self, "saved", (0, 0)))
            if final == "u": self.row, self.column = self.saved
        elif final == "S":
            n = min(max(n, 1), self.rows)
            self.cells = self.cells[n:] + [[" "] * self.columns for _ in range(n)]
        elif final not in "hl": self.valid = False
    def feed(self, data):
        self.valid = True; text = data.decode("utf-8", "replace"); i = 0
        while i < len(text):
            char = text[i]
            if char == "\x1b":
                if i + 1 == len(text): self.valid = False; break
                kind = text[i + 1]
                if kind == "[":
                    end = i + 2
                    while end < len(text) and not "@" <= text[end] <= "~": end += 1
                    if end == len(text): self.valid = False; break
                    self._csi(text[i + 2:end], text[end]); i = end + 1; continue
                if kind in "]PX^_":
                    bel, st = text.find("\a", i + 2), text.find("\x1b\\", i + 2)
                    ends = [v for v in (bel, st) if v >= 0]
                    if not ends: self.valid = False; break
                    stop = min(ends); i = stop + (2 if stop == st else 1); continue
                if kind == "D": self._line_feed()
                elif kind == "M": self.row = max(0, self.row - 1)
                elif kind == "c": self.cells = [[" "] * self.columns for _ in range(self.rows)]
                elif kind not in "78": self.valid = False
                i += 2; continue
            if char == "\r": self.column = 0
            elif char == "\n": self._line_feed()
            elif char == "\b": self.column = max(0, self.column - 1)
            elif char == "\t": self.column = min(self.columns - 1, (self.column // 8 + 1) * 8)
            elif char >= " ": self._put(char)
            i += 1
        return self.valid

def redraw_completion(raw, chunks, rows, columns):
    evidence = {
        "status": "not_proven",
        "requested_rows": rows,
        "requested_columns": columns,
        "clear_sequences": len(re.findall(rb"\x1b\[2J", raw)),
    }
    total = 0
    for chunk in chunks:
        count = chunk.get("byte_count")
        if chunk.get("offset") != total or not isinstance(count, int) or count <= 0:
            evidence["reason"] = "raw_chunk_accounting_mismatch"
            return None, evidence
        total += count
    if total != len(raw):
        evidence["reason"] = "raw_chunk_accounting_mismatch"
        return None, evidence
    try:
        text = raw.decode("utf-8")
    except UnicodeDecodeError:
        evidence["reason"] = "raw_output_is_not_valid_utf8"
        return None, evidence
    offsets = [0]
    for char in text:
        offsets.append(offsets[-1] + len(char.encode("utf-8")))
    expected_offset = 0
    current = None
    index = 0
    while index < len(text):
        if text[index] == "\x1b":
            match = re.match(r"\x1b\[([0-?]*)([ -/]*)([@-~])", text[index:])
            if match is None:
                current = None
                index += 1
                continue
            params, intermediate, final = match.groups()
            if final == "J" and params == "2" and not intermediate:
                current = {"row": 0, "column": 0, "await_home": True}
            elif current is not None:
                if (current["await_home"] and final == "H"
                        and params == "1;1" and not intermediate):
                    current["await_home"] = False
                elif current["await_home"] or final != "m" or intermediate:
                    current = None
            index += match.end()
            continue
        if current is not None and not current["await_home"]:
            char = text[index]
            if char == "\r":
                if (index + 1 < len(text) and text[index + 1] == "\n"
                        and current["column"] == columns
                        and current["row"] < rows - 1):
                    current["row"] += 1
                    current["column"] = 0
                    index += 2
                    continue
                current = None
            elif char == "\n" or ord(char) < 32 or ord(char) == 127:
                current = None
            elif current["row"] >= rows:
                current = None
            elif current["row"] == 0 and current["column"] == 0 and char != "┌":
                current = None
            else:
                width = (0 if unicodedata.combining(char) else
                         2 if unicodedata.east_asian_width(char) in "WF" else 1)
                if current["column"] + width > columns:
                    current = None
                else:
                    current["column"] += width
                    if current["row"] == rows - 1 and current["column"] == columns:
                        if char == "┘":
                            expected_offset = offsets[index + 1]
                            break
                        current = None
        index += 1
    if not expected_offset:
        evidence["reason"] = "no_full_frame_at_requested_dimensions"
        return None, evidence
    for chunk in chunks:
        start = chunk["offset"]
        end = start + chunk["byte_count"]
        if start < expected_offset <= end:
            latency = chunk.get("latency_seconds")
            if not isinstance(latency, (int, float)):
                evidence["reason"] = "completion_chunk_has_no_latency"
                return None, evidence
            evidence.update({
                "status": "complete",
                "rendered_rows": rows,
                "rendered_columns": columns,
                "top_left_marker": "┌",
                "bottom_right_marker": "┘",
                "final_cell_byte_offset": expected_offset,
                "completion_chunk": {
                    "offset": start,
                    "byte_count": chunk["byte_count"],
                    "latency_seconds": latency,
                },
            })
            return latency, evidence
    evidence["reason"] = "completion_cell_not_mapped_to_read_chunk"
    return None, evidence

LAYOUT_MARKERS = {
    "default": {"text": "Network"},
    "expanded": {"text": "Processes"},
}

def layout_marker_row(screen, layout):
    marker = LAYOUT_MARKERS[layout]["text"]
    title_rows = [row for row, cells in enumerate(screen.cells)
                  if cells[0] == "┌" and marker in "".join(cells)]
    if len(title_rows) != 1: return None
    row = title_rows[0]
    if layout == "expanded" and any(
            cells[0] == "┌" for cells in screen.cells[:row]): return None
    return row

def layout_marker_visible(screen, layout):
    return layout_marker_row(screen, layout) is not None

def infer_layout(screen):
    markers = {
        layout: layout_marker_visible(screen, layout)
        for layout in LAYOUT_MARKERS
    }
    visible = [layout for layout, present in markers.items() if present]
    return (visible[0] if len(visible) == 1 else None), markers

def first_layout_marker_chunk(before, response, chunks, layout):
    for chunk in chunks:
        end = chunk["offset"] + chunk["byte_count"]
        probe = copy.deepcopy(before)
        probe.feed(response[:end])
        if layout_marker_visible(probe, layout):
            return chunk
    return None

def overlapping_grid_changed(before, after):
    rows = min(before.rows, after.rows)
    columns = min(before.columns, after.columns)
    return any(before.cells[row][:columns] != after.cells[row][:columns]
               for row in range(rows))

def interaction_result(session, raw_dir, name, number, action, screen, target=None,
                       minimum_observation_seconds=3.5):
    pre = raw_dir / f"{name}-pre-{number:02d}.ansi"
    raw = raw_dir / f"{name}-sample-{number:02d}.ansi"
    with pre.open("wb") as sink:
        session.read_idle(sink, timeout=10, allow_empty=True)
    before_valid = screen.feed(pre.read_bytes())
    before_grid = copy.deepcopy(screen)
    current_layout, markers_before = (
        infer_layout(before_grid) if name == "n4" else (None, None))
    chunks = []
    with raw.open("wb") as sink:
        sent = time.perf_counter()
        action()
        size, first, last, quiet = session.read_idle(
            sink, timeout=10, allow_empty=True,
            first_not_before=sent + minimum_observation_seconds,
            chunk_records=chunks, latency_origin=sent)
        window_end = time.perf_counter()
    if target is not None:
        screen.resize(*target)
    response = raw.read_bytes()
    parse_valid = screen.feed(response) and before_valid
    window = session.window_size()
    sample = {
        "sample": number,
        "action_at_seconds": sent - session.started,
        "first_byte_latency_seconds": first - sent if first else None,
        "last_byte_latency_seconds": last - sent if last else None,
        "quiet_complete_latency_seconds": quiet - sent,
        "observation_window_end_latency_seconds": window_end - sent,
        "observation_window_seconds": window_end - sent,
        "response_observed": size > 0,
        "raw_bytes": size,
        "raw_chunks": chunks,
        "raw_output": raw.name,
        "raw_pre_output": pre.name,
        "grid_parse_valid": parse_valid,
        "grid_rows": screen.rows,
        "grid_columns": screen.columns,
        "pty_rows": window[0],
        "pty_columns": window[1],
    }
    if target is not None:
        completion, evidence = redraw_completion(
            response, chunks, target[0], target[1])
        evidence["pty_rows"], evidence["pty_columns"] = window
        if window != target:
            completion = None
            evidence.update({
                "status": "not_proven",
                "reason": "pty_size_does_not_match_requested_size",
            })
        sample.update({
            "visible_change_verified": overlapping_grid_changed(before_grid, screen)
                and parse_valid,
            "redraw_completed": completion is not None,
            "redraw_completion_latency_seconds": completion,
            "redraw_completion_evidence": evidence,
            "redraw_completion_reason": evidence.get("reason"),
        })
    elif name == "n4":
        expected_layout = (
            "default" if current_layout == "expanded" else
            "expanded" if current_layout == "default" else None)
        current_visible_before = (
            markers_before[current_layout] if current_layout is not None else False)
        expected_visible_before = (
            markers_before[expected_layout] if expected_layout is not None else None)
        expected_marker_row = (
            layout_marker_row(screen, expected_layout)
            if expected_layout is not None else None)
        expected_visible_after = (
            expected_marker_row is not None if expected_layout is not None else None)
        visible = (
            current_visible_before and not expected_visible_before
            and expected_visible_after and parse_valid)
        visible_chunk = (
            first_layout_marker_chunk(
                before_grid, response, chunks, expected_layout)
            if visible else None)
        sample.update({
            "key": "e",
            "current_layout": current_layout,
            "expected_layout": expected_layout,
            "layout_marker_evidence": {
                "marker_visibility_before": markers_before,
                "marker_rows_before": {
                    layout: layout_marker_row(before_grid, layout)
                    for layout in LAYOUT_MARKERS
                },
                "current_marker_visible_before": current_visible_before,
                "expected_marker_visible_before": expected_visible_before,
                "expected_marker_visible_after": expected_visible_after,
                "expected_marker": (
                    LAYOUT_MARKERS[expected_layout]["text"]
                    if expected_layout is not None else None),
                "expected_marker_row": expected_marker_row,
                "reason": (
                    None if current_layout is not None
                    else "layout_markers_not_unique_before_action"),
            },
            "visible_change_verified": bool(visible),
            "visible_change_latency_seconds": (
                visible_chunk["latency_seconds"] if visible_chunk else None),
            "visible_change_chunk": visible_chunk,
        })
    else:
        sample["visible_change_verified"] = False
    return sample
