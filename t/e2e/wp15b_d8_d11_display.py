"""Terminal display dimensions and semantic signatures for WP15b harnesses."""

from __future__ import annotations

import re
from typing import Any

WIDTH = 160
HEIGHT = 48
AVG_PATTERN = re.compile(r"\bAVG\s+([0-9]{1,3})%")
CLI_ERROR_PATTERN = re.compile(
    r"(?im)^\s*(?:[^:\n]+:\s*)?(?:error\b|usage\s*:|"
    r"(?:unknown|unrecognized|invalid|unexpected)\s+(?:option|argument)\b)"
)
FRAME_WORDS = frozenset(
    "CPU AVG Memory Disk Network Temperature GPU Battery Process Use User Time "
    "PID Mem Rx Tx Read Write Used Free Total All Core Cores Name Count Power RAM SWP".split()
)
BOX_GLYPHS = frozenset("┌┐└┘")


def structure_signature(text: str) -> dict[str, Any]:
    rows = text.splitlines()
    boxes = [[y, x, c] for y, row in enumerate(rows) for x, c in enumerate(row) if c in BOX_GLYPHS]
    labels = [[y, m.start(), m.group()] for y, row in enumerate(rows)
              for m in re.finditer(r"[A-Za-z]+", row) if m.group() in FRAME_WORDS]
    return {"rows": len(rows), "columns": max((len(row) for row in rows), default=0),
            "box_cells": boxes, "labels": labels}


def cli_error_output_detected(text: str) -> bool:
    return bool(CLI_ERROR_PATTERN.search(text))

def compare_d11_screens(
    without_text: str,
    with_text: str,
    *,
    without_rendered: bool,
    with_rendered: bool,
    free_arc_supplied: bool,
    without_cli_error: bool = False,
    with_cli_error: bool = False,
) -> dict[str, Any]:
    without_error = without_cli_error or cli_error_output_detected(without_text)
    with_error = with_cli_error or cli_error_output_detected(with_text)
    without_valid = without_rendered and not without_error
    with_valid = with_rendered and not with_error
    without_signature = application_render_signature(without_text) if without_valid else None
    with_signature = application_render_signature(with_text) if with_valid else None
    comparison_complete = (
        without_valid and with_valid
        and without_signature is not None and with_signature is not None
    )
    signature_equal = (
        without_signature == with_signature if comparison_complete else None
    )
    flag_accepted = free_arc_supplied and with_valid
    status = (
        "incomplete" if not comparison_complete or not flag_accepted
        else "pass" if signature_equal
        else "fail"
    )
    return {
        "without_signature": without_signature,
        "with_signature": with_signature,
        "comparison_complete": comparison_complete,
        "display_signature_equal": signature_equal,
        "flag_accepted": flag_accepted,
        "cli_error_output_detected": without_error or with_error,
        "acceptance_status": status,
        "pass": status == "pass",
    }


def application_render_signature(text: str) -> dict[str, Any] | None:
    rows = text.splitlines()
    if len(rows) != HEIGHT or max((len(row) for row in rows), default=0) != WIDTH:
        return None
    if cli_error_output_detected(text):
        return None

    panel_positions = {}
    for panel in ("CPU", "Memory"):
        for y, row in enumerate(rows):
            match = re.search(rf"┌\s*{panel}\b", row)
            if match:
                panel_positions[panel] = (y, match.start())
                break
    if set(panel_positions) != {"CPU", "Memory"}:
        return None

    screen_structure = structure_signature(text)
    return {
        "rows": HEIGHT,
        "columns": WIDTH,
        "panels": panel_positions,
        "screen_structure": {
            "box_cells": screen_structure["box_cells"],
            "labels": screen_structure["labels"],
            "value_labels": [
                [y, match.start(1), match.group(1)]
                for y, row in enumerate(rows)
                for match in re.finditer(r"\b([A-Za-z][A-Za-z0-9_-]*):", row)
            ],
        },
    }


def complete_process_panel(text: str) -> dict[str, Any] | None:
    rows = text.splitlines()
    if len(rows) != HEIGHT or max((len(row) for row in rows), default=0) != WIDTH:
        return None
    titles = [
        y for y, row in enumerate(rows)
        if re.search(r"┌\s*Processes\b", row)
    ]
    words = ("Count", "Name(n)", "CPU%", "Mem%", "R/s", "W/s", "T.Read", "T.Write",
             "User", "State", "Time", "Priority")
    headers = [y for y, row in enumerate(rows) if all(word in row for word in words)]
    if not titles or not headers:
        return None
    horizontal = set("─━┄┈")
    for title_y in titles:
        top_row = rows[title_y]
        left = top_row.find("┌")
        right = top_row.rfind("┐")
        header_y = next((y for y in headers if y > title_y), None)
        if left < 0 or right <= left or header_y is None:
            continue
        if rows[header_y][left] != "│" or rows[header_y][right] != "│":
            continue
        bottom = next((
            y for y in range(header_y + 1, HEIGHT)
            if rows[y][left] == "└" and rows[y][right] == "┘"
        ), None)
        if bottom is None or not all(
            char in horizontal for char in rows[bottom][left + 1:right]
        ):
            continue
        framed_rows = [
            y for y in range(header_y, bottom)
            if rows[y][left] == "│" and rows[y][right] == "│"
        ]
        visible_rows = [
            y for y in framed_rows
            if y > header_y and rows[y][left + 1:right].strip()
        ]
        if not visible_rows:
            continue
        return {
            "title_row": title_y,
            "header_row": header_y,
            "visible_row_count": len(visible_rows),
            "layout_signature": {
                "process_box": [title_y, left, bottom, right],
                "title_row": title_y,
                "header_row": header_y,
                "process_header": rows[header_y],
            },
        }
    return None
