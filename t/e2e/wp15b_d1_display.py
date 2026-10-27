"""Parse displayed process write rates and collect D1 terminal samples."""

import errno
import os
import re
import select
import time

from wp15b_measure import PtySession, TerminalGrid
from wp15b_d1_acceptance import (
    LOW_LOAD_1M_THRESHOLD, SAMPLE_LIMIT, TARGET_BPS, load_average, load_evidence,
)


ROWS, COLUMNS, RATE_MS = 48, 160, 700
MAX_SECONDS, QUIET_SECONDS = 90, 0.25
WRITER_TOKEN = "WP15B_D1_WRITER"
RATE_TOKEN = re.compile(
    r"(?<![\w.])(N/A|\d+(?:\.\d+)?(?:\s*[KMGT]?B)?/s)(?![\w])", re.I
)


def parse_rate(text):
    normalized = re.sub(r"\s+", "", text)
    if normalized.upper() == "N/A":
        return None
    match = re.fullmatch(r"(\d+(?:\.\d+)?)([KMGT]?B)/s", normalized, re.I)
    if not match:
        return None
    scale = {
        "B": 1,
        "KB": 1_000,
        "MB": 1_000_000,
        "GB": 1_000_000_000,
        "TB": 1_000_000_000_000,
    }
    return float(match.group(1)) * scale[match.group(2).upper()]


def parse_bytes(text):
    normalized = re.sub(r"\s+", "", text)
    match = re.fullmatch(r"(\d+(?:\.\d+)?)([KMGTPE]?i?B)", normalized, re.I)
    if not match:
        return None
    unit = match.group(2).upper()
    exponent = {
        "B": 0, "KB": 1, "MB": 2, "GB": 3, "TB": 4, "PB": 5, "EB": 6,
    }.get(unit)
    if unit.endswith("IB"):
        exponent = {
            "KIB": 1, "MIB": 2, "GIB": 3, "TIB": 4, "PIB": 5, "EIB": 6,
        }.get(unit)
    if exponent is None:
        return None
    base = 1024 if "I" in unit else 1000
    return int(round(float(match.group(1)) * base ** exponent))


def column_text(header, row, label):
    columns = list(re.finditer(r"\S+", header))
    target = next(
        (index for index, match in enumerate(columns)
         if match.group().rstrip("▼▲") == label),
        None,
    )
    if target is None:
        return None
    start = columns[target].start()
    end = columns[target + 1].start() if target + 1 < len(columns) else len(row)
    return row[start:end].strip()


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
    evidence["total_write_text"] = column_text(header, writer_row, "T.Write")
    evidence["total_write_bytes"] = parse_bytes(
        evidence["total_write_text"] or ""
    )

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


def btm_config_text():
    return (
        "[flags]\n"
        "basic = false\n"
        "dot_marker = false\n"
        f"rate = {RATE_MS}\n\n"
        "[processes]\n"
        'columns = ["PID", "Name", "CPU%", "Mem%", "R/s", "W/s", "T.Read", "T.Write"]\n'
        'default_sort = "W/s"\n'
        'sort_order = "Descending"\n'
        "case_sensitive = false\n"
        "default_grouped = false\n\n"
        "[[row]]\n"
        "[[row.child]]\n"
        'type = "proc"\n'
        "ratio = 2\n"
        "default = true\n"
    )


def btm_command(btm, config_path):
    return [btm, "-C", str(config_path)]


def start_btm(btm, raw_dir):
    config_path = raw_dir / "btm.toml"
    config_path.write_text(btm_config_text(), encoding="utf-8")
    return PtySession(btm_command(btm, config_path), ROWS, COLUMNS)


def displayed_btm_writer_rate(grid, writer_pid):
    lines = tuple(line.rstrip() for line in grid.signature())
    header_index = next(
        (
            index for index, line in enumerate(lines)
            if "PID" in line and re.search(r"\b(?:W/s|Write/s)\b", line)
        ),
        None,
    )
    if header_index is None:
        return None, "process-table-header-missing", {
            "writer_pid": writer_pid,
            "process_header": None,
        }

    header = lines[header_index]
    rate_label = next(
        (label for label in ("W/s", "Write/s")
         if label in {
             match.group().rstrip("▼▲")
             for match in re.finditer(r"\S+", header)
         }),
        None,
    )
    table_lines = lines[header_index + 1:]
    writer_rows = []
    visible_rows = []
    for line in table_lines:
        pid_match = re.match(r"\s*[^0-9]*?(\d+)(?=\s)", line)
        if pid_match is None:
            continue
        pid = int(pid_match.group(1))
        visible_rows.append({"pid": pid, "row": line})
        if pid == writer_pid:
            writer_rows.append(line)

    evidence = {
        "writer_pid": writer_pid,
        "process_header": header.strip(),
        "visible_process_rows": visible_rows,
    }
    if not writer_rows:
        evidence["writer_token_rows"] = [
            line for line in table_lines if WRITER_TOKEN in line
        ]
        return None, "writer-row-absent", evidence
    if len(writer_rows) != 1:
        evidence["writer_rows"] = writer_rows
        return None, "writer-pid-row-not-unique", evidence

    writer_row = writer_rows[0]
    evidence["writer_row"] = writer_row
    evidence["writer_command_token_found"] = WRITER_TOKEN in writer_row
    evidence["text"] = column_text(header, writer_row, rate_label)
    rate = parse_rate(evidence["text"] or "")
    if rate is None:
        return None, "writer-rate-unavailable", evidence
    return rate, None, evidence


def collect_samples(ntop_child, writer, raw_dir, started):
    samples, observations = [], []
    transcript = bytearray()
    last_output = None
    last_sample_at = None
    last_signature = None
    pid_filter_sent = False
    measurement_started = False
    deadline = started + MAX_SECONDS
    writer_data_path = raw_dir / "writer.data"
    last_writer_observation_at = time.monotonic()
    last_writer_bytes = writer_data_path.stat().st_size
    with (raw_dir / "ntop.ansi").open("wb") as raw:
        while len(observations) < SAMPLE_LIMIT and time.monotonic() < deadline:
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
            grid = TerminalGrid(ROWS, COLUMNS)
            if grid.feed(bytes(transcript)):
                signature = grid.signature()
                terminal_frame_repeated = signature == last_signature
                rate, reason, detail = displayed_writer_rate(grid, writer.pid)
            else:
                signature = terminal_frame_repeated = None
                rate, reason, detail = None, "ansi-grid-invalid", {}

            if not measurement_started:
                if (not pid_filter_sent
                        and detail.get("process_header")
                        and "PID" in detail["process_header"]):
                    ntop_child.send(f"/pid = {writer.pid}\r".encode())
                    pid_filter_sent = True
                    continue
                if rate is None:
                    continue
                measurement_started = True

            observation_index = len(observations) + 1
            load_before = load_average()
            sampled_at_monotonic_ns = time.monotonic_ns()
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
            if signature is not None:
                last_signature = signature
            load_after = load_average()
            observation = {
                "observation_index": observation_index,
                "elapsed_seconds": now - started,
                "sampled_at_monotonic_ns": sampled_at_monotonic_ns,
                "valid": rate is not None,
                "writer_pid": writer.pid,
                "terminal_frame_repeated": terminal_frame_repeated,
                "process_header": detail.get("process_header"),
                "visible_process_rows": detail.get("visible_process_rows"),
                "writer_command_token_found": detail.get(
                    "writer_command_token_found"
                ),
                "writer_row": detail.get("writer_row"),
                "total_write_text": detail.get("total_write_text"),
                "total_write_bytes": detail.get("total_write_bytes"),
                "w_s_text": detail.get("text"),
                "w_s_bytes_per_second": rate,
                "reason": reason,
                **writer_evidence,
                **load_evidence(load_before, load_after),
            }
            observations.append(observation)
            if rate is not None:
                observation["sample"] = len(samples) + 1
                observation["within_target_10_percent"] = (
                    TARGET_BPS * 0.9 <= rate <= TARGET_BPS * 1.1
                )
                samples.append(observation.copy())
    return samples, observations


def collect_btm_samples(btm_child, writer, raw_dir, started):
    samples, observations = [], []
    transcript = bytearray()
    last_output = None
    last_sample_at = None
    pid_filter_sent = False
    measurement_started = False
    deadline = started + MAX_SECONDS
    writer_data_path = raw_dir / "writer.data"
    last_writer_observation_at = time.monotonic()
    last_writer_bytes = writer_data_path.stat().st_size
    with (raw_dir / "btm.ansi").open("wb") as raw:
        while len(observations) < SAMPLE_LIMIT and time.monotonic() < deadline:
            if btm_child.poll() is not None:
                raise RuntimeError(
                    f"btm exited early with status {btm_child.status}"
                )
            if writer.poll() is not None:
                raise RuntimeError(
                    f"Writer exited early with status {writer.returncode}"
                )
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
            readable, _, _ = select.select([btm_child.fd], [], [], timeout)
            if readable:
                try:
                    data = os.read(btm_child.fd, 65536)
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
            if last_sample_at is not None and now - last_sample_at < RATE_MS / 1000:
                continue
            if not pid_filter_sent:
                btm_child.send(f"/pid {writer.pid}\r".encode())
                pid_filter_sent = True
                last_output = time.monotonic()
                continue
            last_sample_at = now
            observation_index = len(observations) + 1
            load_before = load_average()
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
            if grid.feed(bytes(transcript)):
                rate, reason, detail = displayed_btm_writer_rate(grid, writer.pid)
            else:
                rate, reason, detail = None, "ansi-grid-invalid", {}
            if rate is None and not measurement_started:
                continue
            measurement_started = True
            sampled_at_monotonic_ns = time.monotonic_ns()
            load_after = load_average()
            observation = {
                "observation_index": observation_index,
                "elapsed_seconds": now - started,
                "sampled_at_monotonic_ns": sampled_at_monotonic_ns,
                "valid": rate is not None,
                "writer_pid": writer.pid,
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
                **load_evidence(load_before, load_after),
            }
            observations.append(observation)
            if rate is not None:
                observation["sample"] = len(samples) + 1
                observation["within_target_10_percent"] = (
                    TARGET_BPS * 0.9 <= rate <= TARGET_BPS * 1.1
                )
                samples.append(observation.copy())
    return samples, observations
