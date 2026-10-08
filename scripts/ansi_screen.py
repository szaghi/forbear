#!/usr/bin/env python3
"""Reduce a terminal capture to what the terminal finally shows.

A progress bar redraws its line many times: every frame ends with "erase to the end of the line" and a carriage return,
and the next frame overwrites it; bars at a position below the current line move the cursor down and back up. A capture
of the raw stream therefore holds every frame; this filter replays it on a minimal terminal (carriage return, line
feed, cursor up and down, erase in line and in display, SGR colours; other control sequences are dropped) and prints the
final screen, colours kept as SGR sequences. With --frame K it stops at the end of the K-th frame instead (frame 1 is
the one drawn by `start`), showing the screen while that frame is displayed.

The output of the documentation examples must not change from a run to the next: the progress speed, the estimated
time of arrival, the elapsed time, the summary and the dates, which depend on the clock, are replaced by placeholders.

Usage: ansi_screen.py [--frame K] < CAPTURE > SCREEN
"""

from __future__ import annotations

import re
import sys

CSI = re.compile(r"\x1b\[([0-9;?]*)([A-Za-z])")
SPEED = re.compile(r"\(\s*[^()%\s]+%/s\)")
ETA = re.compile(r"ETA (?:\d\d:\d\d:\d\d|\s*\S+ d)")
# an ETA or an elapsed time of a template: its colour code may touch it, so no \b
CLOCK = re.compile(r"(?<!\d)\d\d:\d\d:\d\d(?!\d)")
SUMMARY = re.compile(r"\[done in [^,\]]+, [^\]]+/s\]")
DATE = re.compile(r"\d{4}/\d{2}/\d{2} \d{2}:\d{2}:\d{2}")

Cell = tuple[str, tuple[str, ...]]
BLANK: Cell = (" ", ())


def render(text: str, frame: int = 0) -> list[str]:
    """Replay `text` on a minimal terminal and return its lines, with SGR sequences; stop after the `frame`-th frame."""
    rows: list[list[Cell]] = [[]]
    row = col = 0
    sgr: tuple[str, ...] = ()
    frames = 0
    i = 0
    while i < len(text):
        match = CSI.match(text, i)
        if match:
            params, final = match.groups()
            count = int(params) if params.isdigit() else 1
            if final == "m":
                for code in params.split(";"):
                    sgr = () if code in ("", "0") else (*sgr, code)
            elif final == "A":
                row = max(0, row - count)
            elif final == "B":
                row += count
            elif final == "K":
                rows.extend([] for _ in range(row + 1 - len(rows)))
                rows[row] = [] if params == "2" else rows[row][:col]
            elif final == "J":
                rows.extend([] for _ in range(row + 1 - len(rows)))
                rows[row] = rows[row][:col]
                del rows[row + 1 :]
            i = match.end()
            continue
        char = text[i]
        i += 1
        if char == "\n":
            row += 1
            col = 0
        elif char == "\r":
            col = 0
            # a frame ends with "erase to the end of the line" and a carriage return
            if text.endswith("\x1b[K", 0, i - 1):
                frames += 1
                if frames == frame:
                    break
        else:
            rows.extend([] for _ in range(row + 1 - len(rows)))
            line = rows[row]
            line.extend([BLANK] * (col + 1 - len(line)))
            line[col] = (char, sgr)
            col += 1
    out = []
    for line in rows:
        while line and line[-1] == BLANK:
            line.pop()
        buffer, current = "", ()
        for char, cell_sgr in line:
            if cell_sgr != current:
                if current:
                    buffer += "\x1b[0m"
                if cell_sgr:
                    buffer += "\x1b[" + ";".join(cell_sgr) + "m"
                current = cell_sgr
            buffer += char
        if current:
            buffer += "\x1b[0m"
        out.append(buffer)
    while out and not out[-1]:
        out.pop()
    return out


def main() -> None:
    frame = int(sys.argv[2]) if len(sys.argv) == 3 and sys.argv[1] == "--frame" else 0
    text = sys.stdin.buffer.read().decode("utf-8", errors="replace")
    for line in render(text, frame):
        line = SPEED.sub("( nn.nn%/s)", line)
        line = ETA.sub("ETA hh:mm:ss", line)
        line = SUMMARY.sub("[done in n.nn s, nn.nn/s]", line)
        line = DATE.sub("yyyy/mm/dd hh:mm:ss", line)
        line = CLOCK.sub("hh:mm:ss", line)
        sys.stdout.write(line + "\n")


if __name__ == "__main__":
    main()
