# 5. Spinners and counters

![march with a spinner, then a spinner alone, then a counter](/gifs/march_5.gif){.gif}

A spinner is a character that changes at every drawing of the bar: it shows that the program is alive. You choose it
with `spinner_string`, the key of one of the [40 spinners](/guide/spinners) of forbear:

<<< @/examples/snippets/march_5-bar.f90

While running:

<<< @/examples/output/march_5-bar.ansi{ansi}

The spinner moves one frame at every drawing of the bar: it is not driven by a clock of its own, so it stands still
while the program spends a long time between two updates (Fortran has no portable threads to animate it meanwhile). A string that is not the key of a spinner gives no
spinner, without a message.

## Without the bar

With `width=0` the bar has no body: what remains is the prefix, the spinner, the percentage. A spinner alone:

<<< @/examples/snippets/march_5-spinner.f90

<<< @/examples/output/march_5-spinner.ansi{ansi}

A percentage alone, a counter:

<<< @/examples/snippets/march_5-counter.f90

<<< @/examples/output/march_5-counter.ansi{ansi}

Each output shows the line of the previous bars above it: this program runs three bars, one after another, with the
same `bar` object. Calling `initialize` again resets the bar: every setting not passed returns to its default.

::: tip What you learned
Spinners next to the bar and alone; a bare percentage; one object for several bars in a row.
Reference: [Spinners](/guide/spinners).
:::

Next: [6. Sharing the terminal](./06-terminal).
