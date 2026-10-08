# 7. Nested loops

![march with a bar for the time steps and a bar for the iterations of each step](/gifs/march_7.gif){.gif}

Each time step of `march` now iterates a Newton method, ten iterations per step: a bar for the steps, a bar for the
iterations. The second one goes one line below the first one, with `position=1`:

<<< @/examples/snippets/march_7-init.f90

<<< @/examples/snippets/march_7-loop.f90

While running, at the sixth iteration of step 3:

<<< @/examples/output/march_7-running.ansi{ansi}

At the end:

<<< @/examples/output/march_7.ansi{ansi}

- `position=n` draws the bar `n` lines below the line of the cursor, and brings the cursor back: the bar at position 0
  stays where it is, the others are drawn below it. Give the outer loop position 0, the inner one position 1, the next
  one 2, ...
- A bar below the current line is *cleared* when it completes: the iterations bar disappears at the end of each step,
  and is drawn again by its next `start`. The bar at position 0 completes as usual, and ends its line.
- Each bar keeps its own state (timer, speed, spinner): the inner bar does not disturb the speed or the ETA of the outer
  one.
- `write` messages go above all the bars: call the `write` of the bar at position 0.

In a log, where there is no line to come back to, only the bar at position 0 is written (see
[chapter 8](./08-logs)).

::: tip What you learned
`position` for a bar per loop level; inner bars are cleared when done.
Reference: [The bar object](/guide/bar#initialize).
:::

Next: [8. Batch jobs and logs](./08-logs).
