---
title: Cookbook
---

# Cookbook

Short answers to "how do I ...?". Each recipe shows the code and its real output; the [reference](/guide/features) has
the details. The outputs show the terminal at the end of the run, or *while running* when the recipe says so.

[[toc]]

## The smallest bar

<<< @/examples/snippets/minimal.f90

<<< @/examples/output/minimal.ansi{ansi}

The default range is [0, 1]: pass the fraction done.

## A bar over the steps of a loop

<<< @/examples/snippets/march_1-loop.f90

<<< @/examples/output/march_1.ansi{ansi}

`max_value` is the number of steps, `current` the step: both `real(real64)`.

## A loop of many iterations {#a-loop-of-many-iterations}

<<< @/examples/snippets/many-range.f90

<<< @/examples/output/many.ansi{ansi}

The range starts one before the first record, so that the bar starts at 0%. Updating at every iteration is cheap: the
bar is drawn at most ten times a second (`min_interval`), whatever the number of iterations. `frequency=5` draws it
only at every 5%, which also keeps the log of a batch job to 21 lines.

## Fewer drawings, without recompiling

`export FORBEAR_MIN_INTERVAL=1` makes every bar of a program wait one second between two drawings on a terminal,
unless the program passes `min_interval` itself; `0` draws at every update. It is the knob for a slow remote terminal,
and the docs set it to 0 so that their outputs do not depend on the speed of the machine. See
[How often the bar is drawn](/guide/limitations#how-often-the-bar-is-drawn).

## A smooth bar

<<< @/examples/snippets/march_2s-init.f90

<<< @/examples/output/march_2s.ansi{ansi}

*While running.* Eight steps per character. For a visible track, give the empty part a background colour,
`empty_char_color_bg`.

## A Unicode bar

<<< @/examples/snippets/march_2u-init.f90

<<< @/examples/output/march_2u.ansi{ansi}

*While running.* Save the source as UTF-8; one character for `filled_char_string` and `empty_char_string`.

## A coloured, solid bar

<<< @/examples/snippets/march_3-init.f90

<<< @/examples/output/march_3.ansi{ansi}

*While running.* Full blocks in two foreground colours. The colour and style names are in [Colours and styles](/guide/styling).

## Exact colours

<<< @/examples/snippets/hex-init.f90

<<< @/examples/output/hex.ansi{ansi}

*While running.* `#rrggbb` in any colour keyword: the same colour on every terminal palette (here Catppuccin Mocha's
blue, green, yellow and surface). The terminal must have true colour, as most modern ones do. See
[24-bit colours](/guide/styling#_24-bit-colours).

## A redline: colours by position

<<< @/examples/snippets/zones-init.f90

<<< @/examples/output/zones.ansi{ansi}

*While running.* Each `limit:colour` of `bar_zones` colours the filled cells up to that fraction of the bar; the dark
empty cells are the unlit segments of a display. See [Zones](/guide/styling#zones).

## A rising ramp

<<< @/examples/snippets/ramp-init.f90

<<< @/examples/output/ramp.ansi{ansi}

*While running.* `bar_profile='ramp'`: blocks rising from one eighth to the full height, lit or unlit; with zones, a
tachometer. See [Profiles](/guide/bar#profiles).

## Seven-segment numbers

<<< @/examples/snippets/digits-init.f90

<<< @/examples/output/digits.ansi{ansi}

*While running.* `digits='segment'` for the percent, count, ETA and elapsed time; `digits_unlit_color` pads them with
unlit 8s. **Check that the font of your terminal has these characters** (Cascadia Code, Iosevka, JuliaMono do; most
defaults do not): see [Segment digits](/guide/bar#segment-digits).

## A dashboard look in one keyword

<<< @/examples/snippets/themes-init.f90

<<< @/examples/output/themes.ansi{ansi}

*While running.* `theme='vfd'`, `'amber'` or `'kitt'` sets the segments and the colours of every element; anything
passed explicitly wins, and zones, a ramp or segment digits go on top. See [Themes](/guide/styling#themes).

## Percent, count, speed, ETA, scale, times, summary

<<< @/examples/snippets/march_4-init.f90

<<< @/examples/output/march_4.ansi{ansi}

The scale needs `width` of at least 22 (see [Number formats](/guide/limitations#number-formats)). With a narrower
bar the program stops:

<<< @/examples/snippets/scale_narrow.f90

<<< @/examples/output/scale_narrow.ansi{ansi}

## The speed of now, not the average

<<< @/examples/snippets/tuning-speed.f90

The speed and the ETA follow an exponential moving average, with weight `smoothing` (0.3) on the last speed: 1 shows
the momentary speed, 0 the average since the start. See [update](/guide/bar#update).

## A prefix that names the phase

<<< @/examples/snippets/phases-loop.f90

<<< @/examples/output/phases.ansi{ansi}

*While running*, in the third phase. Assign `bar%prefix%string` (or `bar%suffix%string`) while the bar runs: the next
drawing shows it, in the colours of `initialize`. Keep the phases the same width, or the line jumps.

## A spinner next to the bar

<<< @/examples/snippets/march_5-bar.f90

<<< @/examples/output/march_5-bar.ansi{ansi}

*While running.* Pick the key of a spinner from the [catalogue](/guide/spinners).

## A spinner alone

<<< @/examples/snippets/march_5-spinner.f90

<<< @/examples/output/march_5-spinner.ansi{ansi}

*While running.* `width=0`: no bar, only the prefix and the spinner.

## A percentage alone

<<< @/examples/snippets/march_5-counter.f90

<<< @/examples/output/march_5-counter.ansi{ansi}

## Lines above the bar, a message at its end

<<< @/examples/snippets/march_6-loop.f90

<<< @/examples/output/march_6.ansi{ansi}

`bar%write` prints above the running bar; `message=` shows the text at the end of the bar line until the next one. A
plain `print` would be drawn over the bar.

## Output of a library while the bar runs

<<< @/examples/snippets/march_6p-loop.f90

<<< @/examples/output/march_6p.ansi{ansi}

`suspend` clears the bar and frees the terminal, `resume` draws it again below what was printed: see
[suspend and resume](/guide/bar#suspend-and-resume).

## A bar for each loop of a nest

<<< @/examples/snippets/march_7-init.f90

<<< @/examples/snippets/march_7-loop.f90

<<< @/examples/output/march_7-running.ansi{ansi}

*While running.* The inner bar, at `position=1`, is cleared when it completes and drawn again by its next `start`.

## The bar on standard error

<<< @/examples/snippets/stderr-init.f90

Standard output keeps only the results of the program,

<<< @/examples/output/stderr-results.ansi{ansi}

and standard error only the bar:

<<< @/examples/output/stderr-bar.ansi{ansi}

## A clean log in a batch job

Nothing to do: when the output of the bar is not a terminal, the bar writes a plain line every 10%.

<<< @/examples/output/march_8-log.ansi{ansi}

`interactive=.false.` (or `FORBEAR_INTERACTIVE=0`) forces this mode on a terminal too; see
[Terminals and logs](/guide/terminals).

## A log line every ten minutes

```fortran
call bar%initialize(max_value=real(steps, R8P), log_interval=600._R8P)
```

A solver of unknown length, logging a line every so often (here every few iterations, from
[chapter 8](/manual/tutorial/08-logs#signs-of-life-in-long-jobs)):

<<< @/examples/output/march_8l.ansi{ansi}

Or, for every bar, without recompiling: `export FORBEAR_LOG_INTERVAL=600` in the job script. See
[Signs of life in long jobs](/manual/tutorial/08-logs#signs-of-life-in-long-jobs).

## No bars at all

Turn every bar of a program off from its environment, without recompiling; `write` still prints:

<<< @/examples/output/march_8-disabled.ansi{ansi}

In code, `disabled=.true.`; under MPI, draw the bar of one process only:

```fortran
call bar%initialize(max_value=real(steps, R8P), disabled=(rank /= 0)) ! rank from MPI_Comm_rank
```

## A layout of your own

<<< @/examples/snippets/layout-init.f90

<<< @/examples/output/layout-running.ansi{ansi}

*While running.* Any order, any words between the fields: see [Layout templates](/guide/templates). At the end:

<<< @/examples/output/layout.ansi{ansi}

## A field of your own

<<< @/examples/snippets/march_9-field.f90

<<< @/examples/snippets/march_9-init.f90

<<< @/examples/output/march_9.ansi{ansi}

The field reads the residual of the program through a pointer, at every drawing.

## A loop of unknown length

<<< @/examples/snippets/march_10-solve.f90

<<< @/examples/output/march_10-running.ansi{ansi}

*While running.* `current` counts the iterations; `finish` ends the bar. No percent, ETA or scale: see
[chapter 10](/manual/tutorial/10-unknown-ends).

## A scanner for a loop of unknown length

<<< @/examples/snippets/scanner-init.f90

<<< @/examples/output/scanner.ansi{ansi}

*While running.* `pulse_trail` turns the pulse of an indeterminate bar into a one-cell head with a fading trail; with
`#rrggbb` shades, as smooth as you like. See [Pulse trail](/guide/bar#pulse-trail).

## Leaving a loop early

<<< @/examples/snippets/march_10-steady.f90

`finish` ends the bar where the loop left it: the line ended, the cursor shown, the summary written if asked.

## Several bars one after another

<<< @/examples/snippets/sequence-keep.f90

<<< @/examples/output/sequence.ansi{ansi}

Every `initialize` resets the bar, so pass every setting each time. The first three lines of the output come from the
other loop of the program, which copies a configured bar (`bar = style`) and then calls `initialize` to change the
prefix: the copy is lost, and those bars have the defaults.

<<< @/examples/snippets/sequence-copy.f90

`bar%destroy` frees a bar and resets every default, as `initialize` does first.

Two bars running at the same time need their own lines: see [A bar for each loop of a nest](#a-bar-for-each-loop-of-a-nest).

## A cursor that stays visible

<<< @/examples/snippets/tuning-cursor.f90

On a terminal the bar hides the cursor while it runs and shows it again at 100%. If the program may stop before (an
`error stop`, a crash, Ctrl-C), `hide_cursor=.false.` leaves it visible. See
[If the program stops](/guide/limitations#if-the-program-stops).

## Printing only when no bar is running

<<< @/examples/snippets/tuning-locked.f90

`is_stdout_locked` is true while a bar runs on a terminal: anything printed then would be drawn over the bar (use
`bar%write` instead). Once the bar completes, the terminal is free. See [is_stdout_locked](/guide/bar#is-stdout-locked).
