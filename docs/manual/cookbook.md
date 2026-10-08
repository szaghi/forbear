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

The range starts one before the first record, so that the bar starts at 0%. With the default `frequency=1` the bar is
drawn at every update: a million drawings cost more than the loop itself, while `frequency=5` draws it 21 times.

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

Every bar keeps its own state, but two bars drawn on the same terminal at the same time overwrite each other's line
(see [Behaviour and limitations](/guide/limitations#several-bars-one-terminal)).
