---
title: The bar object
---

# The bar object

forbear exports one class and two character kinds:

```fortran
use forbear, only : bar_object, ASCII, UCS4
```

| Method | What it does |
|---|---|
| [`initialize`](#initialize) | Set up the bar: its elements, its range, what it reports. Resets every other setting. |
| [`start`](#start) | Print the scale (if asked), draw the bar at 0%, take over the terminal line. |
| [`update`](#update) | Compute the progress of the current value, redraw the bar; at 100% end its line. |
| [`is_stdout_locked`](#is-stdout-locked) | True while the bar is running, from `start` to 100%. |
| [`destroy`](#destroy) | Reset the bar to its defaults. |
| `=` | Copy a bar. |

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
| Done part | `filled_char_string` (`*`) | `filled_char_color_fg`, `filled_char_color_bg`, `filled_char_style` |
| Remaining part | `empty_char_string` (`-`) | `empty_char_color_fg`, `empty_char_color_bg`, `empty_char_style` |
| Spinner | `spinner_string` (none): the key of a [spinner](./spinners) | `spinner_color_fg`, `spinner_color_bg`, `spinner_style` |

**Reports.** Each one is off by default and has the same three colour keywords.

| Switch | Adds | Colour keywords |
|---|---|---|
| `add_progress_percent` | the progress, `nnn%` | `progress_percent_color_fg`, `progress_percent_color_bg`, `progress_percent_style` |
| `add_progress_speed` | the progress speed, ` (nnn.nn%/s)` | `progress_speed_color_fg`, `progress_speed_color_bg`, `progress_speed_style` |
| `add_date_time` | at the end, a line `[start - end]`, `yyyy/mm/dd hh:mm:ss` each | `date_time_color_fg`, `date_time_color_bg`, `date_time_style` |
| `add_scale_bar` | at the start, a line with `min_value` and `max_value` above the bar | `scale_bar_color_fg`, `scale_bar_color_bg`, `scale_bar_style` |

**Numbers.**

| Keyword | Type | Default | Meaning |
|---|---|---|---|
| `width` | `integer(int32)` | 32 | Characters between the brackets; 0 for no bar body. At least 22 with `add_scale_bar`, or the program stops (`error stop`). |
| `min_value` | `real(real64)` | 0 | Start of the range. |
| `max_value` | `real(real64)` | 1 | End of the range. |
| `frequency` | `integer(int32)` | 1 | With 1, draw at every update; with `f > 1`, draw when the progress, in percent, enters a new multiple of `f` (and at 100%). |
| `output_unit` | `integer(int32)` | standard output | The unit the bar is written to, e.g. `error_unit`. |

## The line

`update` draws one line, in this order, and ends it with a carriage return:

```
prefix  bracket_left  filled × n  empty × (width − n)  bracket_right  suffix  spinner  percent  speed
```

`n` is `nint(progress / 100 * width)`. The filled and empty strings are repeated as they are: with more than one
character each, the bar is wider than `width`. With `width=0` the bar body is empty and the line is the rest: a spinner
or a percentage alone. Every drawing starts with the ANSI sequence that hides the cursor; the end of the bar shows it
again.

## start

```fortran
call bar%start
```

Resets the run state of the bar (timer, spinner, completion), prints the scale line (with `add_scale_bar`), marks the
terminal as taken (`is_stdout_locked` becomes true) and draws the bar at `min_value`. The timer of the speed and the
start time of `add_date_time` are taken at this first drawing. A bar can be started again after it has completed.

## update

```fortran
call bar%update(current=value)   ! value: real(real64)
```

1. If the bar is complete, nothing happens.
2. The progress is `(current - min_value) / (max_value - min_value)`, clamped to [0, 1], truncated to an integer
   percent: 100% only when `current` reaches `max_value`.
3. The line is redrawn according to `frequency` (and always at 100%): the spinner moves one frame, the speed is the
   progress made since the previous drawing over the time elapsed since then.
4. At 100% the cursor is shown again and the line ended; with `add_date_time` the start and end line is printed; the
   bar is complete and `is_stdout_locked` becomes false.

See [Behaviour and limitations](./limitations) for the details.

## is_stdout_locked

```fortran
logical :: running
running = bar%is_stdout_locked()
```

True from `start` until the bar reaches 100%: while it is true, anything written to the terminal is drawn over the bar
(see [Sharing the terminal](/manual/tutorial/06-terminal)). It reports the state of the bar, and locks nothing.

## destroy

```fortran
call bar%destroy
```

Frees the strings and resets the defaults: `width=32`, range [0, 1], `frequency=1`, standard output, no reports.
`initialize` calls it first.

## Character kinds

`ASCII` and `UCS4` are the kinds of the strings of the elements. With the GNU modes of the `fobos` file they are the
ASCII and ISO 10646 kinds; with the Intel and PGI modes, and with fpm, they fall back to the default kind. Plain string
literals, Unicode ones included in a UTF-8 source, work in every case: forbear converts every string to `UCS4`
internally.
