"""PTY session and terminal-grid support for WP15b measurements."""
import errno, fcntl, os, pty, re, select, signal, struct, termios, time, unicodedata

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
                  first_not_before=None, allow_empty=False):
        started = time.perf_counter(); deadline = started + timeout
        chunks, first, last = 0, None, None
        while True:
            now = time.perf_counter()
            if (last is not None and now - last >= quiet
                    and (first_not_before is None or now >= first_not_before)):
                return chunks, first, last, now
            if last is None and allow_empty and now - started >= quiet:
                return chunks, None, None, now
            if now >= deadline:
                raise TimeoutError("PTY did not reach the expected quiet interval")
            if self.poll() is not None and not select.select([self.fd], [], [], 0)[0]:
                if last is not None or allow_empty:
                    return chunks, first, last, time.perf_counter()
                raise RuntimeError(f"PTY exited with status {self.status}")
            wait = min(0.1, deadline - now)
            if last is not None:
                wait = min(wait, max(0, quiet - (now - last)))
            if first_not_before is not None and now < first_not_before:
                wait = min(wait, first_not_before - now)
            if select.select([self.fd], [], [], wait)[0]:
                data, stamp = self._read_once(sink)
                if data:
                    chunks += len(data); first = first or stamp; last = stamp
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
        elif final == "m": return
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
