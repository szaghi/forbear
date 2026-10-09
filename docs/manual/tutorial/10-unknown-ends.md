# 10. Unknown ends

![march with a solver of unknown length, then a time loop left at steady state](/gifs/march_10.gif){.gif}

Every bar so far knew its end: `max_value` steps, and the bar completed when `current` reached it. Two common loops do
not fit that. An iterative solver runs until its residual is small enough, and nobody knows in advance how many
iterations that takes. A time loop of 50 steps may reach a steady state at step 30 and leave early. `march` does both:

<<< @/examples/snippets/march_10-solve.f90

While the solver runs:

<<< @/examples/output/march_10-running.ansi{ansi}

- `indeterminate=.true.` says that the total is unknown. `current` counts what is done, from `min_value`, and the bar
  never completes by itself: a block goes back and forth along the track, one cell per drawing, as the spinner turns.
- `{count}` shows what is done (`8`), not `8/max`; `{speed}`, if you ask for it, is what is done per second.
- With nothing to measure against, an indeterminate bar has no percent, ETA or scale: asking for one stops the program
  in `initialize`, with a message.
- `bar%finish` ends the bar: it draws the last update with the track full, ends the line and writes the summary, if
  asked.

## Leaving a loop early

<<< @/examples/snippets/march_10-steady.f90

At the end:

<<< @/examples/output/march_10.ansi{ansi}

Without `finish`, the `exit` would leave the bar running at 60%: on a terminal its line not ended and the cursor still
hidden, the next output drawn over it. `finish` ends it where it is: the last update drawn, here with a message, the
line ended, the cursor shown, the date and summary lines written if asked (the summary rate counts the 60% done).
After `finish`, updates do nothing until the next `start`, as after 100%; on a bar not running, `finish` does nothing.

In a log, an indeterminate bar writes two lines, at the start and at the end: there is no 10% to wait for. A
determinate bar finished early writes one last line, unless its last line already shows where it stopped.

For a livelier pulse, `pulse_trail` gives it a one-cell head and a fading trail, as a scanner: see
[Pulse trail](/guide/bar#pulse-trail).

::: tip What you learned
`indeterminate` bars, ended by `finish`; `finish` for a loop left before its end.
Reference: [finish](/guide/bar#finish).
:::

Next: [11. A 1980s dashboard](./11-dashboard).
