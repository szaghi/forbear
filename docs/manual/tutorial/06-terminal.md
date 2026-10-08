# 6. Sharing the terminal

`march` now prints the residual of its solution every 10 steps, and sends the bar to standard error, drawing it only at
every 10%:

<<< @/examples/snippets/march_6-init.f90

<<< @/examples/snippets/march_6-loop.f90

In a terminal, both streams end up on the screen:

<<< @/examples/output/march_6.ansi{ansi}

Each residual is printed over the bar: the bar line ends with a carriage return, so the cursor is at the beginning of
that line, and the next output overwrites it. The bar then goes on in the line below. The order matters too: the line
of step 10 is printed before the update of step 10, over the bar drawn last.

## Separate streams

`output_unit=error_unit` sends the bar to standard error (the default is standard output). The two streams can then be
split, and each one is clean: the results only,

<<< @/examples/output/march_6-results.ansi{ansi}

or the bar only, the results going to a file:

<<< @/examples/output/march_6-bar.ansi{ansi}

## Messages while the bar runs

On one terminal, a message must wait for the bar to finish. `is_stdout_locked` is true from `start` until the bar
reaches 100%; a small logger holds the messages until then:

<<< @/examples/snippets/deferred-log.f90

<<< @/examples/output/deferred.ansi{ansi}

The whole program is in [the cookbook](../cookbook#messages-while-the-bar-runs).

## Fewer drawings

`frequency=10` draws the bar only when the progress enters a new multiple of 10% (and at 100%). Drawing costs
little, but a loop of very short steps spends less time in it, and a log file of the bar (standard error redirected)
gets 11 frames instead of 51. With the default `frequency=1` the bar is drawn at every update.

::: tip What you learned
Why other output breaks the bar; standard error; holding messages with `is_stdout_locked`; `frequency`.
Reference: [Behaviour and limitations](/guide/limitations).
:::

That is the end of the tutorial: the [cookbook](../cookbook) has short recipes for everyday tasks.
