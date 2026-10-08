#!/usr/bin/env python3
"""Reduce a terminal capture to what the terminal finally shows.

A progress bar redraws its line many times: every frame ends with a carriage return and the next one overwrites it,
inside escape sequences that hide and restore the cursor. A capture of the raw stream therefore holds every frame;
this filter replays it on a minimal terminal (carriage return, line feed, SGR colours; other control sequences are
dropped) and prints the final screen, colours kept as SGR sequences. With --frame K it stops at the K-th carriage
return instead, showing the screen while the K-th frame is displayed (frame 1 is the one drawn by `start`).

The output of the documentation examples must not change from a run to the next: the progress speed and the start/end
dates, which depend on the clock, are replaced by placeholders.

Usage: ansi_screen.py [--frame K] < CAPTURE > SCREEN
"""

from __future__ import annotations

import re
import sys

CSI = re.compile(r"\x1b\[([0-9;?]*)([A-Za-z])")
SPEED = re.compile(r"\(\s*[^()%\s]+%/s\)")
DATE = re.compile(r"\d{4}/\d{2}/\d{2} \d{2}:\d{2}:\d{2}")


def render(text: str, frame: int = 0) -> list[str]:
    """Replay `text` on a minimal terminal and return its lines, with SGR sequences; stop at the `frame`-th CR if > 0."""
    lines: list[list[tuple[str, tuple[str, ...]]]] = [[]]
    col = 0
    sgr: tuple[str, ...] = ()
    returns = 0
    i = 0
    while i < len(text):
        match = CSI.match(text, i)
        if match:
            params, final = match.groups()
            if final == "m":
                for code in params.split(";"):
                    sgr = () if code in ("", "0") else (*sgr, code)
            i = match.end()
            continue
        char = text[i]
        i += 1
        if char == "\n":
            lines.append([])
            col = 0
        elif char == "\r":
            returns += 1
            if returns == frame:
                break
            col = 0
        else:
            line = lines[-1]
            line.extend([(" ", ())] * (col + 1 - len(line)))
            line[col] = (char, sgr)
            col += 1
    out = []
    for line in lines:
        while line and line[-1] == (" ", ()):
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
        line = DATE.sub("yyyy/mm/dd hh:mm:ss", line)
        sys.stdout.write(line + "\n")


if __name__ == "__main__":
    main()
