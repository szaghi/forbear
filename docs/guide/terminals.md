---
title: Terminals and logs
---

# Terminals and logs

A bar has two ways of drawing. On a terminal it is *interactive*: it redraws one line in place, with colours, and hides
the cursor while it runs. Anywhere else (a file, a pipe, the log of a batch job) it writes *plain lines*: one every 10%,
with no colours and no control sequences, so that the log reads as text.

## Which mode

`initialize` decides, in this order:

1. the `interactive` keyword, if passed;
2. else the environment variable `FORBEAR_INTERACTIVE`, if set: `0` for plain lines, anything else for a terminal;
3. else whether `output_unit` is a terminal: forbear asks the operating system (`isatty`) about the standard output and
   the standard error; any other unit, such as a file opened by the program, is not a terminal.

The same program therefore animates its bar when run by hand and writes a clean log when run by a scheduler, with no
change. `scripts/docs_examples.sh` runs the examples of this documentation in a pseudo-terminal for this reason.

## The plain log

<<< @/examples/output/march_8-log.ansi{ansi}

- A line at 0%, at every multiple of 10% (of `frequency`%, if larger than 1) and at 100%, then the start and end line
  and the summary, if asked. `finish` writes a last line where the bar stopped, if the previous one does not show it;
  an indeterminate bar writes a line at the start and one at `finish` only.
- The same elements as on a terminal, without colours and without the spinner, which has no meaning in a log.
- A bar at a position larger than 0 writes nothing: a log cannot come back to a line below.
- `write` prints its lines as they come, between the lines of the bar.

## Environment variables

They act on every bar of a program, without recompiling it; a keyword passed to `initialize` wins over them, except for
`FORBEAR_DISABLE`.

| Variable | Values | Effect |
|---|---|---|
| `FORBEAR_INTERACTIVE` | `1` / `0` | Draw for a terminal / write plain lines, when `interactive` is not passed. |
| `FORBEAR_DISABLE` | any but `0` | Turn every bar off; `write` still prints. Wins over `disabled=.false.`. |
| `FORBEAR_MIN_INTERVAL` | seconds, e.g. `0.5` | The minimum time between two drawings, when `min_interval` is not passed. |

## Batch jobs

- Nothing to do for a readable log: the output of a job is a file, and the bar writes plain lines.
- `frequency=5` (or 20, 25) gives a finer (coarser) log.
- `FORBEAR_DISABLE=1` in the job script removes the bars from the log altogether.

## MPI

Every process that draws a bar draws it on the same terminal, and the lines mix. Draw the bar of one process only:

```fortran
call bar%initialize(max_value=real(steps, R8P), disabled=(rank /= 0)) ! rank from MPI_Comm_rank
```

A disabled bar still prints the lines given to `write`: guard them with the rank as well, if every process calls it.

## Threads

A bar is not thread safe: update it from one thread. Fortran has no portable threads to animate a spinner on its own,
so a spinner moves only when the bar is drawn.
