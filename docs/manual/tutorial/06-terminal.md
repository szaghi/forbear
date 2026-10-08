# 6. Talking while the bar runs

![march printing checkpoints above the bar and the residual at its end](/gifs/march_6.gif){.gif}

`march` now saves its solution every 10 steps, and wants to say so; and it wants to show the residual of every step. A
`print` would not do: the bar line ends with a carriage return, so whatever is printed next overwrites the bar. forbear
has two ways to talk while the bar runs:

<<< @/examples/snippets/march_6-init.f90

<<< @/examples/snippets/march_6-loop.f90

While running:

<<< @/examples/output/march_6-running.ansi{ansi}

At the end:

<<< @/examples/output/march_6.ansi{ansi}

- `bar%write(text)` prints a line *above* the bar: it clears the bar line, writes the text there, and draws the bar
  again on the line below. The lines scroll up, the bar stays at the bottom. When the bar is not running (before
  `start`, after 100%), or in a log, `write` just prints the line.
- `update(current, message=text)` shows the text at the end of the bar line, in the colours of `message_color_fg`,
  `message_color_bg` and `message_style`, until the next message. A shorter message leaves nothing of the longer one:
  every drawing erases the rest of its line.

`write` writes to the unit of the bar. To keep the results of a program apart from its bar, send the bar to standard
error with `output_unit=error_unit` (see [The bar on standard error](../cookbook#the-bar-on-standard-error)).

## Output you do not control

![march suspending its bar while a library prints](/gifs/march_6p.gif){.gif}

`write` works for the lines `march` prints itself. Now `march` saves its solution through a library, which prints on
its own, with `print`: those lines would be drawn over the bar. `suspend` and `resume` let the library have the
terminal for a moment:

<<< @/examples/snippets/march_6p-loop.f90

While running, after the first save:

<<< @/examples/output/march_6p-running.ansi{ansi}

At the end:

<<< @/examples/output/march_6p.ansi{ansi}

- `bar%suspend` clears the bar line and shows the cursor at its start: what the library prints starts there, as if
  there was no bar.
- `bar%resume` draws the bar again, below those lines, with the last update. Updates made in between are recorded, not
  drawn (a long phase of the library may update the bar too).
- The library is any code: a solver, an I/O layer, the MPI runtime printing a warning. A program that cannot tell when
  such output comes should suspend the bar around every call that may print.

## How often the bar is drawn

A bar is not drawn at every update: at most once every `min_interval` seconds (0.1 by default, ten times a second), and
always at 0% and 100%. A loop of a million fast iterations therefore spends its time in the loop, not in its bar; a loop
of slow steps is drawn at every step. `frequency=f` draws only when the progress enters a new multiple of `f`%, for a
bar that should move in coarse steps. A message passed to an update that is not drawn is shown at the next drawing.

::: tip What you learned
`write` for lines above the bar, `message` for the end of the bar line, `suspend` and `resume` around output you do
not control; `min_interval` and `frequency`.
Reference: [The bar object](/guide/bar#write), [Behaviour and limitations](/guide/limitations#how-often-the-bar-is-drawn).
:::

Next: [7. Nested loops](./07-nested).
