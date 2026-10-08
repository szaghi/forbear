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

The progress is rounded to an integer percent, and from 99.5% on it already counts as 100%: in a loop of more than 200
iterations, the last ones each print the completed bar again, on a new line.

<<< @/examples/snippets/many_naive.f90

<<< @/examples/output/many_naive.ansi{ansi}

Pass the integer percent instead, computed by truncation, which reaches 100 only at the last iteration; and update the
bar only when it changes. The same lines handle a counter that does not start at 1 (`min_value` must stay 0):

<<< @/examples/snippets/many-percent.f90

<<< @/examples/output/many.ansi{ansi}

The product `100 * (i - first + 1)` is computed in 64-bit integers: in default integers it overflows from about 21
million iterations.

## A Unicode bar

<<< @/examples/snippets/march_2u-init.f90

<<< @/examples/output/march_2u.ansi{ansi}

*While running.* Save the source as UTF-8; one character for `filled_char_string` and `empty_char_string`.

## A coloured, solid bar

<<< @/examples/snippets/march_3-init.f90

<<< @/examples/output/march_3.ansi{ansi}

*While running.* Full blocks in two foreground colours. The colour and style names are in [Colours and styles](/guide/styling).

## Percent, speed, start and end time, scale

<<< @/examples/snippets/march_4-init.f90

<<< @/examples/output/march_4.ansi{ansi}

The scale needs `width` of at least 22, and `max_value` below 100 to show its value (see
[Behaviour and limitations](/guide/limitations#numbers-that-do-not-fit)). With a narrower bar the program stops:

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

## The bar on standard error

<<< @/examples/snippets/march_6-init.f90

Standard output keeps only the results of the program:

<<< @/examples/output/march_6-results.ansi{ansi}

`frequency=10` draws the bar only at every 10%: a log of standard error gets fewer frames.

## Messages while the bar runs

A message printed while the bar runs is drawn over it (see [Sharing the terminal](./tutorial/06-terminal)). Hold the
messages until the bar has finished:

<<< @/examples/snippets/deferred.f90

<<< @/examples/output/deferred.ansi{ansi}

## Several bars one after another

<<< @/examples/snippets/sequence-keep.f90

<<< @/examples/output/sequence.ansi{ansi}

Every `initialize` resets the bar, so pass every setting each time. The first three lines of the output come from the
other loop of the program, which copies a configured bar (`bar = style`) and then calls `initialize` to change the
prefix: the copy is lost, and those bars have the defaults.

<<< @/examples/snippets/sequence-copy.f90

Never run two bars at the same time, nested or interleaved: they share their timer and spinner state (see
[Behaviour and limitations](/guide/limitations#one-bar-at-a-time)).
