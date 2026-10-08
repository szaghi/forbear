---
title: About forbear
---

# About forbear

forbear (the `e` is mute: say *forbar*) is a pure Fortran library that draws a progress bar for long-running
programs: a line that is redrawn in place while the program runs, showing how far it has come and, if you ask, the
progress in percent and in steps, the speed and the estimated time to the end, a message of the current step, the start
and end times, a scale and a closing summary. Messages can be printed above a running bar, nested loops get one bar
each, and when the output is not a terminal, as in the log of a batch job, the bar writes plain lines instead.

A bar is a `bar_object`: you `initialize` it (its look and its range), `start` it before the loop, and `update` it with
the current value at every step. The bar is made of *elements*, each one a string with an optional colour and style:
a prefix, the brackets, the filled and empty characters, a suffix, a spinner. forbear is Fortran 2008 and depends on
one small library by the same author, [FACE](https://github.com/szaghi/FACE), for the ANSI colours.

This documentation reads in order, and each page links to the next one:

1. [Installation](./install): get forbear into your project.
2. The [tutorial](/manual/): nine short chapters that grow the progress bar of one program.
3. The [cookbook](/manual/cookbook): short recipes, one for each "how do I ...?".
4. The reference, from the [feature map](./features) on: every keyword, every default, every limitation.

The [API](/api/) documents the source itself.

Every code sample of this documentation is part of a program that is compiled and run to produce the outputs shown (see
[`docs/examples`](https://github.com/szaghi/forbear/tree/master/docs/examples)). A bar redraws its line many times: the
outputs show what the terminal displays at the end of the run, or in the middle of it when the page says so; the
animations are recordings of the same programs in a real terminal.

## Authors

- Stefano Zaghi — [@szaghi](https://github.com/szaghi)

Contributions are welcome — see the [Contributing](contributing) page.

## Copyrights

forbear is distributed under a multi-licensing system:

| Use case | License |
|---|---|
| FOSS projects | [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html) |
| Closed source / commercial | [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause) |
| Closed source / commercial | [BSD 3-Clause](http://opensource.org/licenses/BSD-3-Clause) |
| Closed source / commercial | [MIT](http://opensource.org/licenses/MIT) |
