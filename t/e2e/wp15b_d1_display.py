"""Parse displayed process write rates and collect D1 terminal samples."""

import errno
import os
import re
import select
import time

from wp15b_measure import PtySession, TerminalGrid


ROWS, COLUMNS = 48, 160
RATE_MS, TARGET_BPS = 700, 1_000_000
SAMPLE_LIMIT, MAX_SECONDS, QUIET_SECONDS = 20, 90, 0.25
WRITER_TOKEN = "WP15B_D1_WRITER"
RATE_TOKEN = re.compile(
    r"(?<![\w.])(N/A|\d+(?:\.\d+)?(?:[KMGT]?B)?/s)(?![\w])", re.I
)


def load_average():
    return [float(value) for value in os.getloadavg()]


def parse_rate(text):
    if text.upper() == "N/A":
        return None
    match = re.fullmatch(r"(\d+(?:\.\d+)?)([KMGT]?B)/s", text, re.I)
    if not match:
        return None
    scale = {"B": 1, "KB": 1_000, "MB": 1_000_000,
             "GB": 1_000_000_000, "TB": 1_000_000_000_000}
    return float(match.group(1)) * scale[match.group(2).upper()]


def displayed_writer_rate(grid, writer_pid):
    lines = tuple(line.rstrip() for line in grid.signature())
    header_index = next(
        (index for index, line in enumerate(lines) if "W/s" in line), None
    )
    if header_index is None:
        return None, "process-table-header-missing", {
            "writer_pid": writer_pid,
            "process_header": None,
        }

    header = lines[header_index]
    grouped = "Count" in header
    if "PID" not in header:
        return None, "process-pid-column-missing", {
            "writer_pid": writer_pid,
            "process_header": header.strip(),
            "grouped": grouped,
        }
    if "Command(n)" not in header:
        return None, "process-command-column-missing", {
            "writer_pid": writer_pid,
            "process_header": header.strip(),
            "visible_process_rows": [],
        }

    process_lines = []
    visible_rows = []
    table_lines = lines[header_index + 1:]
    for line in table_lines:
        if not line.strip():
            continue
        pid_match = re.match(r"\s*[^0-9]*?(\d+)(?=\s)", line)
        if pid_match and len(RATE_TOKEN.findall(line)) >= 2:
            pid = int(pid_match.group(1))
            process_lines.append((pid, line))
            visible_rows.append({
                "pid": pid,
                "writer_token_found": WRITER_TOKEN in line,
            })

    evidence = {
        "writer_pid": writer_pid,
        "process_header": header.strip(),
        "visible_process_rows": visible_rows,
    }
    writer_rows = [line for pid, line in process_lines if pid == writer_pid]
    if not writer_rows:
        token_rows = [line for line in table_lines if WRITER_TOKEN in line]
        if token_rows:
            evidence["writer_token_rows"] = token_rows
            return None, "writer-pid-unmatched", evidence
        return None, "writer-row-absent", evidence
    if len(writer_rows) != 1:
        evidence["writer_rows"] = writer_rows
        return None, "writer-pid-row-not-unique", evidence

    writer_row = writer_rows[0]
    evidence["writer_row"] = writer_row
    evidence["writer_command_token_found"] = WRITER_TOKEN in writer_row

    values = RATE_TOKEN.findall(writer_row)
    evidence["rate_values"] = values
    if len(values) < 2:
        return None, "read-write-rate-fields-missing", evidence
    rate_text = values[1]
    evidence["text"] = rate_text
    rate = parse_rate(rate_text)
    if rate is None:
        return None, "writer-rate-unavailable", evidence
    return rate, None, evidence


def start_ntop(ntop):
    argv = [
        ntop, "--rate", f"{RATE_MS}ms",
        "--default-widget-type", "process",
        "--default-widget-count", "1",
        "--process-default-sort", "w/s",
        "--process-command",
        "--tree",
    ]
    return PtySession(argv, ROWS, COLUMNS)


def collect_samples(ntop_child, writer, raw_dir, started):
    samples, observations = [], []
    transcript = bytearray()
    last_output = None
    last_sample_at = None
    last_signature = None
    pid_filter_sent = False
    deadline = started + MAX_SECONDS
    writer_data_path = raw_dir / "writer.data"
    last_writer_observation_at = time.monotonic()
    last_writer_bytes = writer_data_path.stat().st_size
    with (raw_dir / "ntop.ansi").open("wb") as raw:
        while len(samples) < SAMPLE_LIMIT and time.monotonic() < deadline:
            if ntop_child.poll() is not None:
                raise RuntimeError(f"ntop exited early with status {ntop_child.status}")
            if writer.poll() is not None:
                raise RuntimeError(f"Writer exited early with status {writer.returncode}")
            now = time.monotonic()
            timeout = min(0.1, max(0.0, deadline - now))
            if last_output is not None:
                quiet_at = last_output + QUIET_SECONDS
                if now < quiet_at:
                    timeout = min(timeout, quiet_at - now)
            if last_sample_at is not None:
                sample_at = last_sample_at + RATE_MS / 1000
                if now < sample_at:
                    timeout = min(timeout, sample_at - now)
            readable, _, _ = select.select([ntop_child.fd], [], [], timeout)
            if readable:
                try:
                    data = os.read(ntop_child.fd, 65536)
                except OSError as exc:
                    if exc.errno == errno.EIO:
                        break
                    raise
                if data:
                    raw.write(data)
                    raw.flush()
                    transcript.extend(data)
                    last_output = time.monotonic()
                else:
                    break
                continue
            now = time.monotonic()
            if last_output is None or now - last_output < QUIET_SECONDS:
                continue
            if (last_sample_at is not None
                    and now - last_sample_at < RATE_MS / 1000):
                continue
            last_sample_at = now
            writer_data_bytes = writer_data_path.stat().st_size
            writer_interval_bytes = writer_data_bytes - last_writer_bytes
            writer_interval_seconds = now - last_writer_observation_at
            writer_interval_rate = (
                writer_interval_bytes / writer_interval_seconds
                if writer_interval_seconds > 0 else None
            )
            last_writer_bytes = writer_data_bytes
            last_writer_observation_at = now
            writer_evidence = {
                "writer_data_bytes": writer_data_bytes,
                "writer_interval_bytes": writer_interval_bytes,
                "writer_interval_seconds": writer_interval_seconds,
                "writer_interval_bytes_per_second": writer_interval_rate,
            }
            grid = TerminalGrid(ROWS, COLUMNS)
            if not grid.feed(bytes(transcript)):
                observations.append({
                    "elapsed_seconds": now - started,
                    "valid": False,
                    "reason": "ansi-grid-invalid",
                    "writer_pid": writer.pid,
                    "terminal_frame_repeated": None,
                    **writer_evidence,
                    "load_average": load_average(),
                })
                continue
            signature = grid.signature()
            terminal_frame_repeated = signature == last_signature
            last_signature = signature
            rate, reason, detail = displayed_writer_rate(grid, writer.pid)
            observation = {
                "elapsed_seconds": now - started,
                "valid": rate is not None,
                "writer_pid": writer.pid,
                "terminal_frame_repeated": terminal_frame_repeated,
                "process_header": detail.get("process_header"),
                "visible_process_rows": detail.get("visible_process_rows"),
                "writer_command_token_found": detail.get(
                    "writer_command_token_found"
                ),
                "writer_row": detail.get("writer_row"),
                "w_s_text": detail.get("text"),
                "w_s_bytes_per_second": rate,
                "reason": reason,
                **writer_evidence,
                "load_average": load_average(),
            }
            observations.append(observation)
            if (not pid_filter_sent
                    and detail.get("process_header")
                    and "PID" in detail["process_header"]):
                ntop_child.send(f"/pid = {writer.pid}\r".encode())
                pid_filter_sent = True
                observation["pid_filter_sent"] = True
            if rate is not None:
                observation["sample"] = len(samples) + 1
                observation["within_target_10_percent"] = (
                    TARGET_BPS * 0.9 <= rate <= TARGET_BPS * 1.1
                )
                samples.append(observation.copy())
    return samples, observations
