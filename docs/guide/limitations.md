---
title: Behaviour and limitations
---

# Behaviour and limitations

What forbear does at the edges, and how to stay away from them. Every behaviour described here is that of the current
source; the workarounds are in your program, not in the library.

## The range

The progress is `nint(current / (max_value - min_value) * 100)`: an integer percent, rounded to the nearest.

**Keep `min_value` at 0.** The formula does not subtract `min_value` from `current`: with a range [1000, 2000] the
first update already reports 100%. Measure your counter from its start instead, as in
[A loop of many iterations](/manual/cookbook#a-loop-of-many-iterations).

**Never pass `current` beyond `max_value`.** Past 100% (a little past: from `100 + 50 / width` percent) the done part
is longer than the bar, and the program stops with a run time error, also without run time checks:

```
Fortran runtime error: Argument NCOPIES of REPEAT intrinsic is negative (its value is -3)
```

**More than 200 updates.** Rounding makes the progress 100% from 99.5% on: in a loop of more than 200 iterations, the
last ones each end the bar again and print it on a new line.

<<< @/examples/snippets/many_naive.f90

<<< @/examples/output/many_naive.ansi{ansi}

Pass an integer percent computed by truncation, which reaches 100 only at the last iteration: see
[A loop of many iterations](/manual/cookbook#a-loop-of-many-iterations). For the same reason, the updates whose
progress rounds to 0% (the first iterations of a loop of more than 200) each reset the timer and the spinner.

## Frequency

With `frequency=f` the bar is drawn only when the progress is a multiple of `f`, or 100. The progress must hit those
multiples: a loop of 7 steps goes 14%, 29%, 43%, 57%, 71%, 86%, 100%, and with `frequency=10` its bar jumps from 0% to
100%.

## Numbers that do not fit

- The progress speed is written with two decimals in six characters: above 999.99%/s, a very fast loop, it shows as
  `******`. It is the speed between the last two drawings, not an average.
- The scale shows `min_value` and `max_value` with two decimals in five characters: from 100 on (and below −9.99) they
  show as `*****`. The scale is useful for ranges such as [0, 1] or [0, 50]; for a larger one, use the percentage.
- `add_scale_bar` needs `width` of at least 22: a narrower bar stops the program in `initialize`, with an `error stop`.

## One bar at a time

The timer of the speed, the progress at the previous drawing, the frame of the spinner and the start time live in
the `update` procedure, not in the bar: every `bar_object` of the program shares them. Bars one after another are
fine; two bars running at the same time (nested loops, interleaved updates) corrupt each other's speed and spinner.

For the same reason `update` is not thread safe: call it from one thread. Under MPI, every process that calls it draws
its own bar on the same terminal: draw from one process only.

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
