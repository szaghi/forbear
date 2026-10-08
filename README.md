<div align="center">

# forbear
#### Fortran (progress) B(e)ar environment

[![GitHub tag](https://img.shields.io/github/v/tag/szaghi/forbear)](https://github.com/szaghi/forbear/tags)
[![GitHub issues](https://img.shields.io/github/issues/szaghi/forbear)](https://github.com/szaghi/forbear/issues)
[![CI](https://github.com/szaghi/forbear/actions/workflows/ci.yml/badge.svg)](https://github.com/szaghi/forbear/actions/workflows/ci.yml)
[![coverage](https://img.shields.io/endpoint?url=https://szaghi.github.io/forbear/coverage.json)](https://github.com/szaghi/forbear/actions/workflows/ci.yml)
[![License](https://img.shields.io/badge/license-GPLv3%20%7C%20BSD%20%7C%20MIT-blue.svg)](#copyrights)

> Progress bars and spinners for long-running Fortran programs, in pure Fortran 2008:
> one object, three calls — `initialize`, `start`, `update` — and the bar redraws its line in place.
> The `e` is mute: say *forbar*.

<img src="media/taste.gif" alt="forbear progress bars and spinners in a terminal" width="760">

<sub>Bars, counters and spinners drawn by the test program of forbear.</sub>

<div>
<table>
<tr>
<td width="50%"><b>📊 A bar in three calls</b><br><sub><code>initialize</code> sets its look and range, <code>start</code> draws it, <code>update</code> redraws it at every step; at 100% it ends its line and gives the terminal back. <a href="https://szaghi.github.io/forbear/manual/tutorial/01-first-bar">A first bar</a></sub></td>
<td width="50%"><b>🧩 Built from elements</b><br><sub>Prefix, brackets, filled and empty characters, suffix: each element is a string of your choice, Unicode included (<code>█</code>, <code>░</code>, <code>▕</code>, ...). <a href="https://szaghi.github.io/forbear/guide/bar">The bar object</a></sub></td>
</tr>
<tr>
<td width="50%"><b>🎨 Colours and styles</b><br><sub>A foreground colour, a background colour and a style for every element, by name: 17 colours, 16 styles, through <a href="https://github.com/szaghi/FACE">FACE</a>. <a href="https://szaghi.github.io/forbear/guide/styling">Colours and styles</a></sub></td>
<td width="50%"><b>⏱️ What the bar reports</b><br><sub>Progress in percent, progress speed, start and end time, a scale with the range of the run: one switch each. <a href="https://szaghi.github.io/forbear/manual/tutorial/04-reports">What the bar reports</a></sub></td>
</tr>
<tr>
<td width="50%"><b>🌀 40 spinners</b><br><sub>Braille dots, blocks, arcs, moons: next to the bar, or alone with <code>width=0</code>; a bare percentage counter too. <a href="https://szaghi.github.io/forbear/guide/spinners">Spinners</a></sub></td>
<td width="50%"><b>🖥️ Sharing the terminal</b><br><sub>Send the bar to standard error with <code>output_unit</code>, draw it only every <i>n</i>% with <code>frequency</code>, hold your messages while <code>is_stdout_locked()</code>. <a href="https://szaghi.github.io/forbear/manual/tutorial/06-terminal">Sharing the terminal</a></sub></td>
</tr>
<tr>
<td width="50%"><b>🛠️ Standard Fortran, small</b><br><sub>Fortran 2008, four modules and one small dependency (FACE, ANSI colours); built with FoBiS or fpm. <a href="https://szaghi.github.io/forbear/guide/install">Installation</a></sub></td>
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

One `initialize` adds what the bar reports: here the percentage, the speed, the start and end time and a scale, each
in its own colour, for the `march` solver of the
[tutorial](https://szaghi.github.io/forbear/manual/tutorial/01-first-bar):

```fortran
call bar%initialize(prefix_string='march ', bracket_left_string='[', bracket_right_string='] ', &
                    filled_char_string='#', empty_char_string='.',                             &
                    add_progress_percent=.true., progress_percent_color_fg='yellow',           &
                    add_progress_speed=.true., progress_speed_color_fg='green',                &
                    add_date_time=.true., date_time_color_fg='magenta',                        &
                    add_scale_bar=.true., scale_bar_color_fg='blue',                           &
                    width=40, max_value=real(steps, R8P))
```

```console
$ march
      [ 0.00 (min)                  (max) 50.00]
march [########################################] 100% ( nn.nn%/s)
[yyyy/mm/dd hh:mm:ss - yyyy/mm/dd hh:mm:ss]
```

The speed and the dates change from a run to the next, and are shown as placeholders.

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
forbear = { git = "https://github.com/szaghi/forbear", branch = "master" }
```

v1.2.0 and older releases have no `fpm.toml`: use the branch until the next release, then pin its tag.

A Fortran 2008 compiler is required; the examples of the documentation are built with gfortran 16, and the `fobos`
file has modes for Intel and PGI too (see [Installation](https://szaghi.github.io/forbear/guide/install)).

## Authors

- Stefano Zaghi — [@szaghi](https://github.com/szaghi)

Contributions are welcome — see the [Contributing](https://szaghi.github.io/forbear/guide/contributing) page.

## Copyrights

This project is distributed under a multi-licensing system:

- **FOSS projects**: [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html)
- **Closed source / commercial**: [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause), [BSD 3-Clause](http://opensource.org/licenses/BSD-3-Clause), or [MIT](http://opensource.org/licenses/MIT)

> Anyone interested in using, developing, or contributing to this project is welcome — pick the license that best fits your needs.
