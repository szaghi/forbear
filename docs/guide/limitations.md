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

## Frequency

With `frequency=1` (the default) the bar is drawn at every update, even when the percent has not changed: the spinner
moves at every update. With `frequency=f` larger than 1 it is drawn when the progress enters a new multiple of `f`,
and at 100%: a loop of 7 steps (14%, 28%, 42%, ...) with `frequency=10` is drawn at every step, since each one enters a
new ten. The first drawing, at `start`, is always made.

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
`*****`. The speed is the one between the last two drawings, not an average.

`add_scale_bar` needs `width` of at least 22: a narrower bar stops the program in `initialize`, with an `error stop`.

## Several bars, one terminal

Every bar keeps its own state (timer, spinner, start time): bars one after another, nested or interleaved, do not
disturb each other's numbers. They do share the terminal: two bars drawn on the same unit at the same time overwrite
each other's line. Draw them on different units, or one after another.

A bar is not thread safe: update it from one thread. Under MPI, every process that updates a bar draws it on the same
terminal: draw from one process only.

## initialize resets everything

`initialize` starts by resetting the bar to its defaults: every setting not passed is lost, also when the bar was
configured before or copied from another bar (see
[Several bars one after another](/manual/cookbook#several-bars-one-after-another)).

## Silent defaults

A colour or style name that is not in [the lists](./styling), or a `spinner_string` that is not the key of a
[spinner](./spinners), is ignored without a message.

## Other output while the bar runs

The bar line ends with a carriage return: anything written to the same terminal before the bar is complete is drawn
over it. Send the bar to standard error with `output_unit=error_unit`, or hold the messages while `is_stdout_locked()`
is true: see [Sharing the terminal](/manual/tutorial/06-terminal).

## Terminals, files and batch jobs

The bar needs a terminal that understands the carriage return and the ANSI escape sequences (colours, hidden cursor).
Written to a file, as in the log of a batch job, the bar is every frame one after another, carriage returns and
escape sequences included: in a batch job, send it to standard error and discard it, or do not draw it.

## Unicode prefixes and the scale

The scale is indented by the length of the prefix string, in characters of the string. gfortran stores every UTF-8 byte
of a literal of the source as one character, in a `UCS4` literal too: `'Größe '` is 8 characters for 6 shown, and the
scale is shifted by two columns. The bar itself is not affected. With the scale, use an ASCII prefix.
