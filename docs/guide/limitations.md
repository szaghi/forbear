---
title: Behaviour and limitations
---

# Behaviour and limitations

What forbear does at the edges, and how to stay away from them. Every behaviour described here is that of the current
source; the workarounds are in your program, not in the library.

## The range

The progress is the fraction of the range done, `(current - min_value) / (max_value - min_value)`, clamped to [0, 1]
and truncated to an integer percent: 99.9% shows as 99%, and the bar is complete only when `current` reaches
`max_value` (a tolerance of 10⁻⁹ percent absorbs the round-off of a sum such as twenty steps of 0.05).

- A `current` outside the range is clamped: below `min_value` the bar shows 0%, above `max_value` 100%.
- The bar completes once: it ends its line at 100%, and further updates do nothing until the next `start`.
- An empty range (`max_value <= min_value`) completes the bar at `start`.
- A loop left before its end leaves the bar running: call `finish` after it, which ends the bar where it is.
- A loop of unknown length takes an `indeterminate` bar: `current` counts what is done, and `finish` ends it. Such a bar
  has no percent, ETA or scale.

## How often the bar is drawn

On a terminal, the bar is drawn at 0%, at 100%, and in between at most once every `min_interval` seconds (0.1 by
default): a loop of a million fast iterations is not slowed down by its bar. With `frequency=1` (the default) every
update due is drawn, even when the percent has not changed: the spinner moves at every drawing. With `frequency=f`
larger than 1 the bar is drawn when the progress enters a new multiple of `f`, and at 100%: a loop of 7 steps (14%,
28%, 42%, ...) with `frequency=10` is drawn at every step, since each one enters a new ten. A message passed to an
update that is not drawn is shown at the next drawing.

In a log, a line is written at every 10% (every `frequency`% if larger than 1), whatever the time between them. A long
job can go for hours between two tens, and an indeterminate bar writes only its first and last lines: `log_interval=600`
(or `FORBEAR_LOG_INTERVAL=600` in the job script) adds a line whenever ten minutes have passed since the last one.

## Number formats

The progress speed and the values of the scale have fixed widths, six and five characters: a line that got shorter
would leave the end of the previous drawing on screen. A number takes the most precise form that fits, right-aligned:

| Form | Speed (6 characters) | Scale (5 characters) |
|---|---|---|
| two decimals | `  0.00` to `999.99` | ` 0.00` to `99.99` |
| one decimal | `1000.0` to `9999.9` | `100.0` to `999.9` |
| integer | `10000` to `999999` | `1000` to `99999` |
| one decimal and exponent | `1.2e6`, `2.5e9`, `1.0e10` | `1.2e5`, `2.5e9` |
| one digit and exponent | `1e100` | `1e10`, `1e300` |

Negative numbers take one more character for the sign. Only a scale value from about −1e100 down does not fit, and shows as
`*****`. The ETA has eight characters, `hh:mm:ss` below 100 hours and days beyond (`12.5 d`); the duration of the
summary is in seconds below a minute (`2.53 s`), `hh:mm:ss` beyond.

`add_scale_bar` needs `width` of at least 22: a narrower bar stops the program in `initialize`, with an `error stop`.

## Several bars, one terminal

Every bar keeps its own state (timer, speed, spinner, start time): bars one after another, nested or interleaved, do
not disturb each other's numbers. Two bars running at the same time need their own lines: give each one a `position`
(see [Nested loops](/manual/tutorial/07-nested)). A bar at a position larger than 0 is drawn with "cursor down" and
"cursor up" sequences, so the lines below the current one must belong to the bars: print through the `write` of the
bar at position 0.

## initialize resets everything

`initialize` starts by resetting the bar to its defaults: every setting not passed is lost, also when the bar was
configured before or copied from another bar (see
[Several bars one after another](/manual/cookbook#several-bars-one-after-another)).

## If the program stops

On a terminal, a running bar hides the cursor and shows it again at 100% or at `finish`. A program that stops before, with an
`error stop`, a crash or Ctrl-C, leaves the terminal without a cursor: Fortran has no portable way to run code when a
program is interrupted. `reset` or `tput cnorm` brings the cursor back. For programs that may stop half way, as a
long run killed by its user, pass `hide_cursor=.false.`: the bar is drawn the same, with the cursor visible at the start
of its line.

## Mistakes stop the program

A colour or style name that is not in [the lists](./styling) (or a malformed `#rrggbb`), wrong
[`bar_zones`](./styling#zones), an unknown [`bar_profile`](./bar#profiles) or a ramp with `partial_blocks`, a
`spinner_string` that is not the key of a [spinner](./spinners), a wrong [template](./templates): `initialize` stops the
program (`error stop`), after a message on standard error that names the mistake. Before forbear 1.6 the names were ignored without a message, and a typo left an
element without its colour.

## Other output while the bar runs

The bar line ends with a carriage return: anything written to the same terminal by `print` or `write` statements is
drawn over the bar. Print through `bar%write` instead, wrap output you do not control (a library, the MPI runtime) in
`bar%suspend` and `bar%resume`, or send the bar to standard error with `output_unit=error_unit`: see
[Talking while the bar runs](/manual/tutorial/06-terminal).

## Terminals, files and batch jobs

On a terminal the bar animates; anywhere else it writes a plain line every 10%, with no control sequences: see
[Terminals and logs](./terminals). A terminal must understand the carriage return and the ANSI sequences (colours,
cursor movement, erase in line): every modern terminal does, Windows Terminal included. 24-bit `#rrggbb` colours need
a terminal with true colour; most modern ones have it, some (macOS Terminal.app, old consoles) approximate or drop them.

## Lines wider than the terminal

forbear does not know how wide the terminal is: Fortran has no portable way to ask. A bar line wider than the terminal
is cut at its right edge: forbear turns the terminal's line wrapping off while it writes a drawing, and on again right
after, so that the text printed by `bar%write` still wraps. The line gets too wide with a long prefix or message, a
large `width`, or East Asian wide characters and emoji, which take two columns each: `width=32` with
`filled_char_string='㊂'` is a bar of 64 columns. To see the whole line, keep it within the terminal. Before forbear
1.6.1, such a line wrapped, and every drawing left a copy of the bar on the line above.

## Unicode prefixes and the scale

The scale is indented by the columns of the prefix, so that it sits above the bar: forbear counts the characters of the
prefix, without the bytes that continue a UTF-8 character (`'Größe '` is 8 bytes and 6 columns). East Asian wide
characters and emoji take two columns on a terminal, and are counted as one: with such a prefix, the scale is shifted
to the left by one column per wide character.
