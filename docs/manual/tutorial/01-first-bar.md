# 1. A first bar

`march` advances its solution for 50 time steps, and each step takes a while: long enough for whoever runs it to
wonder how far it has come. A progress bar answers, in three steps: **initialize** the bar, **start** it, **update**
it at every time step.

<<< @/examples/snippets/march_1.f90

- `initialize` sets up the bar: here only its range, from `min_value` (default 0) to `max_value`. Everything not passed
  takes its default: a bar 32 characters wide, `*` for the done part and `-` for the rest.
- `start` draws the empty bar (0%) and takes over the terminal line.
- `update` takes the current value, between `min_value` and `max_value`, as a `real(real64)`, and redraws the line.

## Running it

While running, at step 25 of 50:

<<< @/examples/output/march_1-running.ansi{ansi}

At the end:

<<< @/examples/output/march_1.ansi{ansi}

Each update rewrites the same line: it ends with a carriage return instead of a new line, so the next one overwrites
it. When the progress reaches 100%, `update` ends the line and the program prints below it as usual: the message after
the loop needs no special care.

## The range

The progress is the current value over the length of the range, in percent, rounded to an integer. With the default
range [0, 1] you pass the fraction done; with `max_value=real(steps, R8P)` you pass the step itself. Two rules keep the
bar correct:

- keep `min_value` at 0, and shift your counter instead (see [A loop that does not start at 1](../cookbook#a-loop-of-many-iterations));
- never pass a value beyond `max_value`: past 100% the bar no longer fits its width and the program stops with a run
  time error.

A loop of more than 200 iterations needs one more line: see [the cookbook](../cookbook#a-loop-of-many-iterations) and
[Behaviour and limitations](/guide/limitations).

::: tip What you learned
The three steps `initialize`, `start`, `update`; the range of the bar.
Reference: [The bar object](/guide/bar).
:::

Next: [2. The look of the bar](./02-look).
