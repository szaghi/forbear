# 8. Batch jobs and logs

![march on a terminal, then with its output sent to a log file](/gifs/march_8.gif){.gif}

`march` will also run as a batch job, its output written to a log file: there, a bar that redraws its line would leave
every frame in the file, carriage returns and escape sequences included. forbear knows where it writes:

<<< @/examples/snippets/march_8-init.f90

On a terminal:

<<< @/examples/output/march_8.ansi{ansi}

With the output sent to a file:

<<< @/examples/output/march_8-log.ansi{ansi}

- `initialize` looks at the unit of the bar: when it is the standard output or error and that is a terminal, the bar
  is *interactive*, and animates; otherwise it writes a plain line at every 10% (at every `frequency`% if larger than
  1), with no colours, no carriage returns and no escape sequences, then the date and time and the summary lines. The
  same program, the same calls, a readable log.
- `interactive=.true.` or `.false.` forces either mode; the environment variable `FORBEAR_INTERACTIVE=1` or `0` does
  the same for every bar, when the keyword is not passed.
- Bars below the current line (`position` larger than 0) write nothing in a log.

## Signs of life in long jobs

A line every 10% is a fine log for a job of an hour; for a job of two days, ten lines leave hours of silence, and an
indeterminate bar writes nothing between its first and last lines. `log_interval` adds a line whenever that many
seconds have passed since the last one:

<<< @/examples/snippets/march_8l-init.f90

<<< @/examples/output/march_8l.ansi{ansi}

Each iteration of this solver takes 0.2 s: a line every 0.3 s comes every other iteration. In a real job,
`log_interval=600` gives a line every ten minutes; `FORBEAR_LOG_INTERVAL=600` in the job script does it for every bar,
without recompiling. On a terminal, `log_interval` does nothing.

## Turning the bars off

`disabled=.true.` turns a bar off: `start` and `update` draw nothing, `write` still prints its lines. The environment
variable `FORBEAR_DISABLE=1` turns every bar of the program off, without recompiling:

<<< @/examples/output/march_8-disabled.ansi{ansi}

Under MPI every process would draw its own bar on the same terminal: draw the bar of one process only,

```fortran
call bar%initialize(max_value=real(steps, R8P), disabled=(rank /= 0)) ! rank from MPI_Comm_rank
```

::: tip What you learned
Interactive and log modes, `log_interval`, `interactive`, `disabled`, the `FORBEAR_*` environment variables, one bar
under MPI.
Reference: [Terminals and logs](/guide/terminals).
:::

Next: [9. Layout templates](./09-templates).
