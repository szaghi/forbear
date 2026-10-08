# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

forbear is a small pure-Fortran (F2008+) library for drawing progress bars and spinners on a terminal.
The public API is a single class, `bar_object` (plus the `ASCII`/`UCS4` character kinds), re-exported by
`src/lib/forbear.f90`. The README carries the full user-facing API reference for `initialize`, `start`, `update`,
`destroy`, and `is_stdout_locked`.

## Build and test

FoBiS is the primary build tool; the `fobos` file defines GNU, Intel and PGI modes. List them with `fobis build --lmodes`.

```bash
fobis build --mode tests-gnu          # builds every program under src/ into exe/ (here: exe/forbear_test)
fobis build --mode tests-gnu-debug    # -O0, -fcheck=all, -std=f2008, -DDEBUG
fobis build --mode static-gnu         # libforbear.a in ./static/   (shared-gnu → libforbear.so in ./shared/)
bash scripts/run_tests.sh             # runs every executable in exe/; PASS = exit status 0
fobis rule --ex makecoverage          # clean + coverage build + run + gcov over src/lib/forbear*
```

`fpm build` / `fpm test` also work (`fpm.toml` pulls FACE from git; the library source dir is `src/lib`).

The single "test" (`src/tests/forbear_test.F90`) is a **visual demo**, not an assertion suite. It draws about 30 bars
and spinners and passes as long as it exits 0. A wrong rendering does not fail it, so check the output by eye, or add a
`<name>.result` file: `run_tests.sh` compares a test's trimmed stdout against `<name>.result` when one exists.
Executables named `*_xfail_*` must exit non-zero, and names containing `mpi` run under `mpirun -np N`.

## Documentation examples

Every code sample and output in the tutorial and cookbook comes from a real program in `docs/examples/src/*.f90`.
`bash scripts/docs_examples.sh` rebuilds `static-gnu`, compiles each program, runs it, and regenerates
`docs/examples/snippets/` and `docs/examples/output/*.ansi`. Never edit those two directories by hand.

- Marker comments in the sources drive the generator. `!run [-s] [-f K] ID COMMAND` records a run: `-s` adds the exit
  status, and `-f K` shows the screen at the K-th frame instead of the end. Frame 1 is the one drawn by `start`, frame
  k+1 is update k, and frames keep counting across several bars in one program. `!region NAME … !endregion NAME`
  marks a snippet, and `!as NAME` sets the command name.
- `scripts/ansi_screen.py` replays the captured stream on a minimal terminal and keeps the final (or K-th) screen. It
  handles carriage returns and SGR colours, and drops the cursor hide/show sequences. It also masks the progress
  speed (`nn.nn`) and the dates (`yyyy/mm/dd hh:mm:ss`), so the output is identical from run to run.
- Shiki's dual-theme ANSI renderer drops background colours, so examples that render in the docs must use foreground
  colours only.
- The pages document the library's real edge behaviour in `docs/guide/limitations.md` (the 200-step rounding, the
  `min_value` and `max_value` overflow, the shared `save` state, field overflows). If you fix one of these in
  `forbear_bar_object.F90`, update that page and the tutorial and cookbook passages that link to it.

## Dependencies

- **FACE** (ANSI colours, `colorize`) is the only dependency. It is a **git submodule** at `src/third_party/FACE`, not
  a `fobis fetch` dependency: there is no `[dependencies]` section in `fobos` and no `.deps_config.ini`. Run
  `git submodule update --init` on a fresh clone. The `$EXDIRS` setting in `fobos` excludes FACE's own tests and its
  nested PENF from the build.
- The `UCS4_SUPPORTED` / `ASCII_SUPPORTED` preprocessor macros are set only in the GNU templates. The Intel and PGI
  modes compile without them, so `ASCII` and `UCS4` both fall back to the default character kind. As a result, every
  `select type` over string kinds (`forbear_kinds.F90`, `forbear_element_object.F90`) has `#ifdef`'d branches. Keep
  new string-accepting procedures consistent with that pattern.

## Architecture

```
forbear (facade) ── bar_object (forbear_bar_object.F90) ── element_object ×N (forbear_element_object.F90) ── FACE.colorize
                 └─ forbear_kinds: ASCII, UCS4, ucs4_string(class(*)) → UCS4 string
```

- **`element_object`** is one coloured text fragment: a UCS4 string plus fg/bg/style. Its `output()` returns the
  string with the ANSI colours applied.
- **`bar_object`** is built from fixed `element_object` components (prefix, suffix, brackets, empty/filled chars,
  percent, speed, scale, date-time) and an allocatable `spinner(:)` array. `initialize` takes about 50 optional
  keyword arguments, one `<component>_string/_color_fg/_color_bg/_style` group per element. Strings are `class(*)`,
  so callers may pass default, ASCII or UCS4 literals.
- **Spinners** are chosen by the *first frame character* passed as `spinner_string`: `create_spinner` has a large
  `select case` that maps each character to a hard-coded frame sequence. Adding a spinner means adding a `case` there.
- **Rendering** happens in `update(current)`. It builds a whole line that starts with an ANSI sequence hiding the
  cursor and ends with `char(13)` (a carriage return, no newline), writes it with `advance='no'`, then flushes. At
  100 % it restores the cursor and, if requested, prints the start/end timestamp. `start` prints the optional scale
  line, then calls `update(min_value)`. `is_stdout_locked` reports whether a bar is in progress, so callers can hold
  back other output to the same unit until it finishes.

### Non-obvious behaviour in `update`

- The timer, previous progress, spinner counter and start date are **`save` locals**, not components of the bar. All
  `bar_object` instances share them, so two bars updated alternately corrupt each other's speed and spinner state.
  They are reset only when the computed progress is exactly 0.
- Progress is computed as `nint(current / (max_value - min_value) * 100)`, without subtracting `min_value`.
  If progress goes past about 100 + 50/width %, `REPEAT` gets a negative count and the program aborts. That happens
  when `min_value /= 0`, or when `current > max_value`. Because of the rounding, every update from 99.5 % on counts as
  100 %: in a loop of more than 200 steps, each of the last updates prints the completed bar again on a new line.
- `width=0` makes a spinner-only or counter-only display, with no bar body. `add_scale_bar` requires `width >= 22`
  and otherwise raises `error stop`.

## In-progress tooling migration (staged, not yet committed)

The index currently stages a move from Travis/FORD to GitHub Actions, VitePress and git-cliff. `.github/`,
`docs/.vitepress`, `docs/package.json`, `cliff.toml`, `scripts/release.sh`, `scripts/compute-coverage.sh`, and the
`CONTRIBUTING.md` → `docs/guide/contributing.md` symlink are all staged. The legacy pieces are still present:
`.travis.yml`, `doc/` (FORD), `wiki/`, and the `makedoc`/`makecoverage-analysis` rules in `fobos`. Current
state:

- The workflows check out with `submodules: true` to get FACE. Their `FoBiS.py fetch` step is a no-op, because
  there is no `.deps_config.ini`.
- `run-coverage-analysis` runs `fobis rule --ex makecoverage-analysis`. That rule removes FACE's coverage data, then
  calls `scripts/compute-coverage.sh`, which writes `docs/public/coverage.json` (`{"pct":"…"}`). That file is a
  generated artifact: do not commit it.
- The docs mirror FLAP's layout. "Start here" and the reference live in `docs/guide/`, the tutorial
  (`manual/tutorial/0N-*.md`) and the cookbook live in `docs/manual/`, and `docs/api/` is generated by `formal` from
  `docs/ford.md`. Build the site with `cd docs && npm install && npm run docs:build`; the `predocs` hook regenerates the
  API first.
- Apart from `makecoverage-analysis`, the `fobos` rules still use the deprecated `FoBiS.py rule -ex …` / `-mode` / `-coverage` forms.

Releases: run `scripts/release.sh --patch|--minor|--major|vX.Y.Z` from `master`. It regenerates `CHANGELOG.md`
with git-cliff, writes `VERSION` (and the `fpm.toml` version, if the manifest declares one), commits, tags and
pushes. The tag push triggers `release.yml`. Existing tags go up to `v1.2.0`.
