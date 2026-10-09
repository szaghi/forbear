---
title: The bar object
---

# The bar object

forbear exports the bar, the two types of the fields of a program, and two character kinds:

```fortran
use forbear, only : bar_object, field_object, progress_object, ASCII, UCS4
```

| Method | What it does |
|---|---|
| [`initialize`](#initialize) | Set up the bar: its elements, its range, what it reports, where and how it draws. Resets every other setting. |
| [`start`](#start) | Print the scale (if asked), draw the bar at 0%, take over the terminal line. |
| [`update`](#update) | Compute the progress of the current value, redraw the bar when due; at 100% end its line. |
| [`finish`](#finish) | End the bar where it is: an indeterminate bar, a loop left before its end. |
| [`write`](#write) | Print a line above the running bar. |
| [`suspend`, `resume`](#suspend-and-resume) | Clear the bar while other code prints freely, then draw it again. |
| [`add_field`](./templates#fields-of-the-program) | Add a field of the program, for the template. |
| [`is_stdout_locked`](#is-stdout-locked) | True while the bar is running on a terminal, from `start` to 100% or `finish`. |
| [`destroy`](#destroy) | Reset the bar to its defaults. |
| `=` | Copy a bar (intrinsic assignment). |

## initialize

```fortran
call bar%initialize([keyword=value, ...])
```

Every argument is optional and passed by keyword. `initialize` first resets the bar (see [`destroy`](#destroy)): every
setting not passed takes its default, whatever the bar had before.

**Elements.** Each element has a string and three colour keywords; the strings are `class(*)`, of default, `ASCII` or
`UCS4` kind. The colours and the styles are names: see [Colours and styles](./styling).

| Element | String (default) | Colour keywords |
|---|---|---|
| Prefix | `prefix_string` (none) | `prefix_color_fg`, `prefix_color_bg`, `prefix_style` |
| Suffix | `suffix_string` (none) | `suffix_color_fg`, `suffix_color_bg`, `suffix_style` |
| Left bracket | `bracket_left_string` (none) | `bracket_left_color_fg`, `bracket_left_color_bg`, `bracket_left_style` |
| Right bracket | `bracket_right_string` (none) | `bracket_right_color_fg`, `bracket_right_color_bg`, `bracket_right_style` |
| Done part | `filled_char_string` (`*`; `█` with `partial_blocks`) | `filled_char_color_fg`, `filled_char_color_bg`, `filled_char_style` |
| Remaining part | `empty_char_string` (`-`; a space with `partial_blocks`) | `empty_char_color_fg`, `empty_char_color_bg`, `empty_char_style` |
| Spinner | `spinner_string` (none): the key of a [spinner](./spinners) | `spinner_color_fg`, `spinner_color_bg`, `spinner_style` |
| Message | given by [`update`](#update) | `message_color_fg`, `message_color_bg`, `message_style` |

**Reports.** Each one is off by default and has the same three colour keywords.

| Switch | Adds | Colour keywords |
|---|---|---|
| `add_progress_percent` | the progress, ` nnn%` | `progress_percent_color_fg`, `progress_percent_color_bg`, `progress_percent_style` |
| `add_progress_count` | the count, ` current/max`, integers with whole-number bounds | `progress_count_color_fg`, `progress_count_color_bg`, `progress_count_style` |
| `add_progress_speed` | the smoothed speed, ` (nnn.nn%/s)`, the number in [six characters](./limitations#number-formats) | `progress_speed_color_fg`, `progress_speed_color_bg`, `progress_speed_style` |
| `add_eta` | the estimated time to the end, ` ETA hh:mm:ss` | `eta_color_fg`, `eta_color_bg`, `eta_style` |
| `add_scale_bar` | at the start, a line with `min_value` and `max_value` above the bar | `scale_bar_color_fg`, `scale_bar_color_bg`, `scale_bar_style` |
| `add_date_time` | at the end, a line `[start - end]`, `yyyy/mm/dd hh:mm:ss` each | `date_time_color_fg`, `date_time_color_bg`, `date_time_style` |
| `add_summary` | at the end, a line `[done in <duration>, <mean speed>/s]` | `summary_color_fg`, `summary_color_bg`, `summary_style` |

**Numbers and switches.**

| Keyword | Type | Default | Meaning |
|---|---|---|---|
| `width` | `integer(int32)` | 32 | Characters between the brackets; 0 for no bar body. At least 22 with `add_scale_bar`, or the program stops (`error stop`). |
| `min_value` | `real(real64)` | 0 | Start of the range. |
| `max_value` | `real(real64)` | 1 | End of the range. |
| `partial_blocks` | `logical` | `.false.` | Draw the done part with full and partial blocks, eight steps per character. |
| `bar_profile` | `character(*)` | `'flat'` | The glyph of each cell: `'flat'`, the filled and empty strings; `'ramp'`, blocks rising along the body. See [Profiles](#profiles). |
| `pulse_trail` | `character(*)` | none | With `indeterminate`: the moving block becomes a one-cell head with a trail of fading colours, e.g. `'red_intense red black_intense'`. See [Pulse trail](#pulse-trail). |
| `bar_zones` | `character(*)` | none | Colour each filled cell by its position: `limit:colour` items, e.g. `'0.7:green 0.9:yellow 1:red'`. See [Zones](./styling#zones). |
| `min_interval` | `real(real64)` | 0.1 | Minimum time between two drawings on a terminal, in seconds; `FORBEAR_MIN_INTERVAL` replaces the default. |
| `log_interval` | `real(real64)` | 0 | In a log, also write a line when this many seconds have passed since the last one; 0 for none. `FORBEAR_LOG_INTERVAL` replaces the default. On a terminal it does nothing. |
| `frequency` | `integer(int32)` | 1 | With 1, draw at every update due; with `f > 1`, only when the progress enters a new multiple of `f`% (and at 100%). In a log, a line every `f`% (every 10% with 1). |
| `smoothing` | `real(real64)` | 0.3 | Weight of the last speed in its moving average: 1 the momentary speed, 0 the average since the start. |
| `position` | `integer(int32)` | 0 | Line of the bar, counted below the current one; a bar at a position larger than 0 is cleared when it completes. |
| `interactive` | `logical` | detected | Draw for a terminal (`.true.`) or write a plain log (`.false.`). Not passed: `FORBEAR_INTERACTIVE`, else whether `output_unit` is a terminal. See [Terminals and logs](./terminals). |
| `disabled` | `logical` | `.false.` | Draw nothing; `write` still prints. `FORBEAR_DISABLE` turns every bar off. |
| `template` | `character(*)` | none | The layout of the bar line: see [Layout templates](./templates). Without it, the keywords above describe the line. |
| `indeterminate` | `logical` | `.false.` | The total is unknown: `current` counts what is done from `min_value`, the bar body is a block going back and forth, and only [`finish`](#finish) ends the bar. `max_value` and `frequency` are ignored; a percent, an ETA or a scale stops the program (`error stop`). |
| `hide_cursor` | `logical` | `.true.` | Hide the cursor while the bar runs on a terminal, show it again at 100%. `.false.` leaves it visible: see [If the program stops](./limitations#if-the-program-stops). |
| `output_unit` | `integer(int32)` | standard output | The unit the bar is written to, e.g. `error_unit`. |

## The line

Without a [template](./templates), `update` draws one line, in this order:

```
prefix  bracket_left  done part  remaining part  bracket_right  suffix  spinner  percent  count  speed  ETA  message
```

The done part has `nint(progress / 100 * width)` characters; with `partial_blocks`, `int(fraction * width)` full blocks
and a partial block for the eighths of the next character, in the foreground of `filled_char` and the background of
`empty_char`. With [`bar_zones`](./styling#zones), each filled cell, the partial one included, takes the foreground of
its zone. The filled and empty strings are repeated as they are: with more than one character each, the bar is
wider than `width`. With `width=0` the bar body is empty and the line is the rest: a spinner or a percentage alone.

### Profiles

`bar_profile='ramp'` draws every cell of the body as a block rising from `▁` (one eighth high) in the first cell to `█`
in the last, as the bar graph of a dashboard tachometer. Done cells are lit, in the colours of `filled_char` (or of
their [zone](./styling#zones)); remaining cells keep their blocks, unlit, in the colours of `empty_char`, whose
foreground is `black_intense` unless `empty_char_color_fg` is given. The filled and empty strings are not used. A log
has no colours, so its remaining cells are blank: `▁▃  ` for 4 cells half done. `partial_blocks` draws blocks of
its own: together with a ramp, it stops the program; so does a profile that is not `flat` or `ramp`.

<<< @/examples/snippets/ramp-init.f90

At 80%:

<<< @/examples/output/ramp.ansi{ansi}

### Pulse trail

The body of an [indeterminate](#initialize) bar is a block going back and forth, one cell per drawing.
`pulse_trail` turns it into a scanner: a head of one cell, in the colours of `filled_char` (or of its
[zone](./styling#zones)), and behind it the cells where the head was on the last drawings, one colour per drawing
back, from the first colour of the list (the brightest, as a rule) to the last. The colours are names or `#rrggbb`,
separated by blanks; they replace the foreground of `filled_char`. At each end the head turns back over its own trail,
as a real scanner does; where two drawings meet in a cell, the later one is drawn. The 17 named colours give two or
three shades of one hue (`'red_intense red black_intense'`); `#rrggbb` gives as many as wanted. A log draws no pulse,
so the trail changes nothing there. On a bar that is not indeterminate, or with an unknown colour, `pulse_trail` stops
the program.

<<< @/examples/snippets/scanner-init.f90

On the way back:

<<< @/examples/output/scanner.ansi{ansi}

The prefix and the suffix can change while the bar runs: assign `bar%prefix%string` or `bar%suffix%string` (any
string, as `prefix_string`), and the next drawing shows it, in the colours set by `initialize` (or by the template).
For instance, the name of the phase of a solver: `bar%prefix%string = 'assemble '`.

On a terminal, every drawing starts with the ANSI sequence that hides the cursor (unless `hide_cursor=.false.`) and
ends with "erase to the end of the line" and a carriage return: the next drawing overwrites it, and a shorter line
leaves nothing behind. Line wrapping is off while a drawing is written, so a line wider than the terminal is cut at its
right edge (see [Lines wider than the terminal](./limitations#lines-wider-than-the-terminal)). The end of the bar shows
the cursor again. In a log, the line is written as it is, with no colours and no control sequences.

## start

```fortran
call bar%start
```

Resets the run state of the bar (timer, speed, spinner, message, completion) and takes the start time. Unless the bar
is disabled, it prints the scale line (with `add_scale_bar`, at position 0 only), marks the terminal as taken
(`is_stdout_locked` becomes true, on a terminal) and draws the bar at `min_value`. A bar can be started again after it
has completed.

## update

```fortran
call bar%update(current=value [, message=text])   ! value: real(real64); text: any string
```

1. If the bar is disabled or complete, nothing happens.
2. A `message`, if passed, replaces the one shown at the end of the line. A [suspended](#suspend-and-resume) bar stops
   here: it records `current` and `message`, and `resume` draws them.
3. The progress is `(current - min_value) / (max_value - min_value)`, clamped to [0, 1], truncated to an integer
   percent: 100% only when `current` reaches `max_value`.
4. On a terminal, the bar is drawn when the update is due: always at 0% and 100%; otherwise if at least `min_interval`
   seconds have passed since the last drawing and, with `frequency > 1`, the progress has entered a new multiple of
   `frequency`. In a log, a line is written when the progress enters a new multiple of 10% (of `frequency`%), or when
   `log_interval` seconds have passed since the last line.
5. At each drawing, the spinner moves one frame and the speed is updated: the progress made since the previous drawing,
   over the time elapsed since then, averaged with `smoothing`.
6. At 100% the bar is complete: on a terminal, the cursor is shown again and the line ended (a bar at a position larger
   than 0 is cleared instead); the start and end line and the summary are printed, if asked; `is_stdout_locked` becomes
   false.

An [indeterminate](#initialize) bar counts `current - min_value` (not below 0) and never reaches 100%: it is drawn at
most once every `min_interval` seconds on a terminal; in a log, at the start, at `finish`, and every `log_interval`
seconds if set.

## finish

```fortran
call bar%finish([message=text])   ! text: any string
```

Ends a running bar where it is, as 100% would: it draws the last update (an indeterminate bar with its track full), with
the `message` if passed, then ends the line, shows the cursor and writes the date and summary lines, if asked. The rate
of the summary is that of what was done. In a log, it writes a last line unless the previous one already shows that
progress. A suspended bar is drawn again to end. After `finish`, updates do nothing until the next `start`. On a bar not running (not started, already
complete, disabled) it does nothing. Call it after a loop that may `exit` early, and to end an indeterminate bar.

## write

```fortran
call bar%write(text)   ! text: any string
```

While the bar runs on a terminal, `write` clears the bar line, writes the text in its place and draws the bar again on
the line below: the text scrolls up with the output, the bar stays at the bottom. Otherwise (before `start`, after
100%, in a log, with the bar disabled) it writes the text as a line of the unit of the bar. With nested bars, call the
`write` of the bar at position 0.

## suspend and resume

```fortran
call bar%suspend
call solver_library_step   ! prints with print, write, or from C: anything
call bar%resume
```

`write` covers the lines your program prints. Output you do not control, from a library, the MPI runtime or a `print`
deep in a call, would be drawn over the bar: `suspend` steps aside first. It clears the bar line, shows the cursor at
its start and frees the terminal (`is_stdout_locked` becomes false); the other output goes there and below. Meanwhile
updates record the progress without drawing it, and `resume` draws the bar again, on the line where the cursor is,
with the last update; if that update reached 100%, the bar completes there. A bar at a position larger than 0 clears
its own line: suspend every running bar of the terminal, nested ones included, and resume them after.

In a log, on a disabled bar, or on a bar not running, `suspend` and `resume` do nothing: there, other output and the
lines of the bar never overlap. `finish` ends a suspended bar too.

## is_stdout_locked

```fortran
logical :: running
running = bar%is_stdout_locked()
```

True from `start` until the bar reaches 100% or is finished, on a terminal, except while it is suspended: while it is true, anything written to the terminal but
through `write` is drawn over the bar. It reports the state of the bar, and locks nothing.

## destroy

```fortran
call bar%destroy
```

Frees the strings and resets the defaults: `width=32`, range [0, 1], `frequency=1`, `min_interval=0.1`,
`smoothing=0.3`, position 0, standard output, no reports. `initialize` calls it first.

## Character kinds

`ASCII` and `UCS4` are the kinds of the strings of the elements. With the GNU modes of the `fobos` file they are the
ASCII and ISO 10646 kinds; with the Intel and NVIDIA modes, and with fpm, they fall back to the default kind. Plain string
literals, Unicode ones included in a UTF-8 source, work in every case: forbear converts every string to `UCS4`
internally.
