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

## Percent, count, speed, ETA, scale, times, summary

<<< @/examples/snippets/march_4-init.f90

<<< @/examples/output/march_4.ansi{ansi}

The scale needs `width` of at least 22 (see [Number formats](/guide/limitations#number-formats)). With a narrower
bar the program stops:

<<< @/examples/snippets/scale_narrow.f90

<<< @/examples/output/scale_narrow.ansi{ansi}

## A spinner next to the bar

<<< @/examples/snippets/march_5-bar.f90

<<< @/examples/output/march_5-bar.ansi{ansi}

*While running.* Pick the key of a spinner from the [catalogue](/guide/spinners).

## A spinner alone

<<< @/examples/snippets/march_5-spinner.f90

`width=0`: no bar, only the prefix and the spinner.

## A percentage alone

<<< @/examples/snippets/march_5-counter.f90

## Lines above the bar, a message at its end

<<< @/examples/snippets/march_6-loop.f90

<<< @/examples/output/march_6.ansi{ansi}

`bar%write` prints above the running bar; `message=` shows the text at the end of the bar line until the next one. A
plain `print` would be drawn over the bar.

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

*While running.* Any order, any words between the fields: see [Layout templates](/guide/templates).

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

Two bars running at the same time need their own lines: see [A bar for each loop of a nest](#a-bar-for-each-loop-of-a-nest).
