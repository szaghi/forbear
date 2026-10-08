<div align="center">

# forbear
#### Fortran (progress) B(e)ar environment

[![GitHub tag](https://img.shields.io/github/v/tag/szaghi/forbear)](https://github.com/szaghi/forbear/tags)
[![GitHub issues](https://img.shields.io/github/issues/szaghi/forbear)](https://github.com/szaghi/forbear/issues)
[![CI](https://github.com/szaghi/forbear/actions/workflows/ci.yml/badge.svg)](https://github.com/szaghi/forbear/actions/workflows/ci.yml)
[![coverage](https://img.shields.io/endpoint?url=https://szaghi.github.io/forbear/coverage.json)](https://github.com/szaghi/forbear/actions/workflows/ci.yml)
[![License](https://img.shields.io/badge/license-GPLv3%20%7C%20BSD%20%7C%20MIT-blue.svg)](#copyrights)

> Progress bars, spinners, ETA and messages for long-running Fortran programs, in pure Fortran 2008:
> one object, three calls — `initialize`, `start`, `update`. On a terminal the bar animates in place;
> in the log of a batch job it writes clean lines. The `e` is mute: say *forbar*.

<img src="docs/public/gifs/hero.gif" alt="a pretend CFD run: a mesh bar, then nested time-step and Newton bars, with checkpoints printed above, a residual and the ETA, and a closing summary" width="760">

<sub>A (pretend) CFD run: nested bars, messages above the bar, ETA and summary. Every frame is drawn by forbear.</sub>

<div>
<table>
<tr>
<td width="50%"><b>📊 A bar in three calls</b><br><sub><code>initialize</code> sets its look and range, <code>start</code> draws it, <code>update</code> redraws it, at most ten times a second; at 100% it ends its line and gives the terminal back. <a href="https://szaghi.github.io/forbear/manual/tutorial/01-first-bar">A first bar</a></sub></td>
<td width="50%"><b>⏱️ ETA, speed, count, summary</b><br><sub>A smoothed speed and the time to the end, the count of steps done, start and end time, a closing line with duration and throughput. <a href="https://szaghi.github.io/forbear/manual/tutorial/04-reports">What the bar reports</a></sub></td>
</tr>
<tr>
<td width="50%"><b>💬 Talk while it runs</b><br><sub><code>bar%write</code> prints a line above the running bar; <code>update(message=)</code> shows the residual of the step at its end. No broken lines. <a href="https://szaghi.github.io/forbear/manual/tutorial/06-terminal">Talking while the bar runs</a></sub></td>
<td width="50%"><b>🪆 Nested loops</b><br><sub>One bar per loop level, each on its own line with <code>position</code>: time steps above, iterations below, cleared when done. <a href="https://szaghi.github.io/forbear/manual/tutorial/07-nested">Nested loops</a></sub></td>
</tr>
<tr>
<td width="50%"><b>📜 Batch jobs and logs</b><br><sub>Not on a terminal, the bar writes a plain line every 10%: no carriage returns, no escape codes in your SLURM log. <code>FORBEAR_DISABLE=1</code> turns every bar off; <code>disabled=(rank /= 0)</code> under MPI. <a href="https://szaghi.github.io/forbear/manual/tutorial/08-logs">Batch jobs and logs</a></sub></td>
<td width="50%"><b>🎨 Smooth, coloured, Unicode</b><br><sub>Eighth-of-a-cell partial blocks, 17 colours and 16 styles for every element through <a href="https://github.com/szaghi/FACE">FACE</a>, any Unicode string, 40 spinners. <a href="https://szaghi.github.io/forbear/guide/spinners">Spinners</a></sub></td>
</tr>
<tr>
<td width="50%"><b>🛠️ Standard Fortran, small</b><br><sub>Fortran 2008, four modules and one small dependency (FACE, ANSI colours) fetched by <code>fobis fetch</code> or fpm. <a href="https://szaghi.github.io/forbear/guide/install">Installation</a></sub></td>
<td width="50%"><b>🔓 Multi-licensed</b><br><sub>GPL v3 for FOSS projects; BSD 2-Clause, BSD 3-Clause or MIT for closed source and commercial ones: pick the license that fits. <a href="#copyrights">Copyrights</a></sub></td>
</tr>
</table>
</div>

**[Full documentation](https://szaghi.github.io/forbear/)** · [Tutorial](https://szaghi.github.io/forbear/manual/tutorial/01-first-bar) · [Cookbook](https://szaghi.github.io/forbear/manual/cookbook) · [API reference](https://szaghi.github.io/forbear/api/)

</div>

## Quick start

Three steps: initialize, start, update. This is a whole program:

```fortran
program minimal
!< The smallest progress bar: the default range is [0, 1].
use, intrinsic :: iso_fortran_env, only : R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar ! the progress bar
real(R8P)        :: x   ! progress, in [0, 1]
integer          :: i   ! counter
integer          :: j   ! counter
real(R8P)        :: y   ! work result

x = 0._R8P
call bar%initialize(filled_char_string='+', prefix_string='progress |', suffix_string='| ', &
                    add_progress_percent=.true.)
call bar%start
do i = 1, 20
   x = x + 0.05_R8P
   do j = 1, 1000000
      y = sqrt(x) ! just spend some time
   enddo
   call bar%update(current=x)
enddo
if (y < 0._R8P) print *, y
endprogram minimal
```

```console
$ minimal
progress |++++++++++++++++++++++++++++++++| 100%
```

## Grows with your program

The same calls scale to what a solver needs: an ETA, the residual of every step at the end of the bar, checkpoints
printed above it. From the [tutorial](https://szaghi.github.io/forbear/manual/tutorial/06-terminal):

```fortran
call bar%initialize(prefix_string='march ', bracket_left_string='[', bracket_right_string='] ', &
                    filled_char_string='#', empty_char_string='.', add_progress_percent=.true., &
                    message_color_fg='cyan', width=30, max_value=real(steps, R8P))
call bar%start
do step = 1, steps
   call advance
   residual = residual / 2._R8P
   if (mod(step, 10) == 0) then
      write(text, '(A,I0,A)') 'step ', step, ': solution saved'
      call bar%write(trim(text))                            ! a line above the bar
   endif
   write(text, '(A,ES9.2)') 'residual', residual
   call bar%update(current=real(step, R8P), message=trim(text)) ! the end of the bar line
enddo
```

<img src="docs/public/gifs/march_6.gif" alt="march printing checkpoints above the bar and the residual at its end" width="760">

New to forbear? The [tutorial](https://szaghi.github.io/forbear/manual/tutorial/01-first-bar) grows a progress bar
step by step; the [cookbook](https://szaghi.github.io/forbear/manual/cookbook) has short recipes. Every example is a
compiled, runnable program in [`docs/examples/src`](docs/examples/src), shown with its real output in the
documentation.

## Install

### FoBiS

Clone, fetch FACE, and build:

```bash
git clone https://github.com/szaghi/forbear && cd forbear
fobis fetch                           # FACE into src/third_party, pinned by its fobos.lock
fobis build --mode static-gnu         # static/libforbear.a, modules in static/mod
```

The archive contains FACE too: link a program with

```bash
gfortran -I static/mod my_program.f90 static/libforbear.a -o my_program
```

### fpm

Add to your `fpm.toml`:

```toml
[dependencies]
forbear = { git = "https://github.com/szaghi/forbear", tag = "v1.3.0" }
```

v1.2.0 and older releases have no `fpm.toml`. The ETA, the messages, nested bars, the log mode and the other
features added after v1.3.0 need `branch = "master"` until the next release.

A Fortran 2008 compiler is required: tested on every push with gfortran 13 to 15 (and the 16 trunk), Intel ifx 2025.3
and NVIDIA nvfortran 26.1 (see [Installation](https://szaghi.github.io/forbear/guide/install)).

## Authors

- Stefano Zaghi — [@szaghi](https://github.com/szaghi)

Contributions are welcome — see the [Contributing](https://szaghi.github.io/forbear/guide/contributing) page.

## Copyrights

This project is distributed under a multi-licensing system:

- **FOSS projects**: [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html)
- **Closed source / commercial**: [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause), [BSD 3-Clause](http://opensource.org/licenses/BSD-3-Clause), or [MIT](http://opensource.org/licenses/MIT)

> Anyone interested in using, developing, or contributing to this project is welcome — pick the license that best fits your needs.
