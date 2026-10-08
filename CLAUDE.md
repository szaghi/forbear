# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

forbear is a small pure-Fortran (F2008+) library for drawing progress bars and spinners: animated on a terminal,
plain lines in a log. The public API is a single class, `bar_object` (plus the `ASCII`/`UCS4` character kinds),
re-exported by `src/lib/forbear.f90`. The user-facing reference is the VitePress site (`docs/guide/bar.md` for the
API); the README is a landing page.

## Build and test

FoBiS is the primary build tool; the `fobos` file defines GNU, Intel (ifx, FoBiS `intel_nextgen`) and NVIDIA
(nvfortran) modes. List them with `fobis build --lmodes`. Run `fobis fetch` first on a fresh clone (FACE).

```bash
fobis build --mode tests-gnu-debug    # every program under src/ into exe/; -O0, -fcheck=all, -std=f2008, -DDEBUG
bash scripts/run_tests.sh             # runs every executable in exe/; PASS = exit status 0
fobis build --mode static-gnu         # libforbear.a in ./static/   (shared-gnu → libforbear.so in ./shared/)
fobis rule --ex makecoverage          # clean + coverage build + run + gcov over src/lib/forbear*
fpm test                              # the same tests with fpm (each one is a [[test]] of fpm.toml)
```

Locally, ifx and nvfortran are installed but not on PATH: `source /opt/intel/oneapi/compiler/2025.3/env/vars.sh`
then `--mode tests-intel-debug`; `PATH=/opt/nvidia/hpc_sdk/Linux_x86_64/26.5/compilers/bin:$PATH` then
`--mode tests-nvf-debug`. `exe/` is shared by all modes: `fobis clean --mode <mode>` when switching compiler.

The tests (`src/tests/forbear_test_*.F90`, helpers in `forbear_test_tools.F90`) pass a capture file as `output_unit`,
read back every byte, and `check` it; `report` ends with `error stop 1` on any failure. Log mode
(`interactive=.false.`) gives exact, deterministic lines to compare. Terminal mode (`interactive=.true.`) checks the
control-sequence protocol: `ESC[K`+CR per frame, `ESC[nA` for positions, the end sequence. A capture file gets one
extra LF on `close` after a non-advancing write. Mutation-checked: reverting the rounding, `min_value` or `ESC[K`
fixes makes them fail. `forbear_test.F90` is the old visual demo (passes if it exits 0). New behaviour needs a test
here, and a new test program needs a `[[test]]` entry in `fpm.toml`. Executables named `*_xfail_*` must exit
non-zero, and names containing `mpi` run under `mpirun -np N`.

Portability lessons from ifx and nvfortran:
- keep every line, comments included, within 132 columns;
- never write `'\'` (nvfortran treats backslash as an escape in literals): use `achar(92)`;
- never import an unused `R8P=>…` alias into a private module: nvfortran leaked `forbear_element_object`'s
  `R8P=>real32` into `forbear_bar_object`.

## Documentation examples and GIFs

Every code sample and output in the tutorial and cookbook comes from a real program in `docs/examples/src/*.f90`.
`bash scripts/docs_examples.sh` rebuilds `static-gnu`, compiles each program, runs it, and regenerates
`docs/examples/snippets/` and `docs/examples/output/*.ansi`. Never edit those two directories by hand. It takes about
a minute, because each `march_*` step waits 40 ms, so that the GIFs are watchable.

- Marker comments in the sources drive the generator. `!run [-s] [-f K] ID COMMAND` records a run: `-s` adds the exit
  status, and `-f K` shows the screen at the end of the K-th frame instead of the end of the run. A frame is a drawing
  that ends with `ESC[K` + CR: `start`'s first drawing, each drawn update, and each redraw after `write`. Frames keep
  counting across several bars in one program. `!region NAME … !endregion NAME` marks a snippet, and `!as NAME` sets
  the command name.
- Runs happen inside a pseudo-terminal (`script -qefc`, from util-linux), so forbear's auto-detection sees a terminal.
  A redirection inside COMMAND, such as `march > run.log`, really is not a terminal, which is how the log mode is
  shown. `FORBEAR_MIN_INTERVAL=0` is exported so that frame K does not depend on machine speed.
- `scripts/ansi_screen.py` replays the stream on a minimal terminal: CR, LF, cursor up and down, `K`/`2K`/`J` erase,
  and SGR colours. It keeps the final (or K-th) screen and masks the clock-dependent fields (speed `nn.nn`, ETA
  `hh:mm:ss`, summary, dates), so the outputs are identical from run to run (verified by regenerating twice).
- `bash scripts/docs_gifs.sh [name…]` records `docs/public/gifs/*.gif` with VHS, from `docs/gifs/<name>.tape`. Shared
  settings live in `settings.tape` (Catppuccin Mocha, the docs theme's palette). The recordings use the tutorial
  programs plus `docs/gifs/src/{hero,spinners}.f90`. GIFs are not reproducible byte for byte; commit them deliberately.
- Shiki's dual-theme ANSI renderer drops background colours, so examples whose `.ansi` output renders in the docs
  must use foreground colours only. The GIFs show backgrounds fine.
- `docs/guide/limitations.md`, `guide/bar.md` (the `update` steps), `guide/terminals.md` and the tutorial and cookbook
  describe the exact semantics of `update`. Any change to `update` must update them, then rerun both scripts.

## Dependencies

- **FACE** (ANSI colours, `colorize`) is the only dependency. `fobis fetch` puts it in `src/third_party/FACE`, as the
  `fobos` `[dependencies]` section declares, and checks it out at the commit pinned in `src/third_party/fobos.lock`.
  Run `fobis fetch` on a fresh clone before any build; `fobis fetch --update` moves the pin. In `src/third_party`,
  only `.gitignore`, `.deps_config.ini` and `fobos.lock` are tracked. The `.deps_config.ini` makes the CI
  `FoBiS.py fetch` steps run, and `install.sh` fetches whenever `fobos` has `[dependencies]`. `$EXDIRS` excludes
  FACE's own tests from the build. fpm resolves FACE separately from `fpm.toml`, so it is not pinned to the same
  commit.
- The `UCS4_SUPPORTED` / `ASCII_SUPPORTED` preprocessor macros are set only in the GNU templates. The Intel and NVIDIA
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
  percent, count, speed, ETA, message, scale, date-time, summary) and an allocatable `spinner(:)` array. `initialize`
  takes about 70 optional keyword arguments, one `<component>_string/_color_fg/_color_bg/_style` group per element
  plus the switches. Strings are `class(*)`, so callers may pass default, ASCII or UCS4 literals. There is no defined
  assignment: intrinsic assignment copies the bar, and the components' own defined assignment handles them.
- **Spinners** are chosen by a *key* passed as `spinner_string` (one of the frames, not always the first):
  `create_spinner` has a large `select case` mapping each key to a hard-coded frame sequence.
- **Rendering**: `update` decides whether the bar is due, then `update_rate` updates the smoothed rate and
  `build_frame` builds the line into `frame_` (UCS4, no control sequences). `render(element, plain)` drops the colours
  in log mode. `draw` writes `ESC[?25l` + frame + `ESC[K` + CR, wrapped for `position>0` in LF×p … `ESC[pA`.
  `complete` handles 100 %: at position 0 it restores the cursor, writes a newline and `ESC[J`, then the date and
  summary lines; at position>0 it clears its own line. `write_message` (bound as `write`) clears the line, prints the
  text and redraws `frame_`.
- **Modes**, resolved once in `initialize`: interactive comes from the `interactive` keyword, else
  `FORBEAR_INTERACTIVE`, else `is_terminal(output_unit)`. That calls C `isatty` through `iso_c_binding`, and only
  `output_unit`→fd 1 and `error_unit`→fd 2 can be terminals. `FORBEAR_DISABLE` (≠0) forces disabled. A
  non-interactive bar at position>0 is disabled.

### Non-obvious behaviour in `update`

- The run state lives in trailing-underscore components (`progress_drawn_`, `fraction_drawn_`, `rate_`,
  `rate_samples_`, `tic_`, `tic_start_`, `spinner_count_`, `date_time_start_`, `is_complete_`, `frame_`). `start`
  resets it and takes the start time. Never reintroduce `save` locals: they would be shared by every bar.
- Progress is `(current - min_value)/(max_value - min_value)`, clamped to [0, 1] and truncated with a 1e-9 % tolerance,
  so 100 % means done. Clamping keeps `REPEAT` counts non-negative. An empty range completes at `start`, and `start`
  locks *before* its first update so that this completion can unlock. Once complete, `update` returns until the next
  `start`.
- When a drawing is due, on a terminal:
  - 0 % and 100 % are always drawn;
  - otherwise at least `min_interval` must have passed since the last drawing (default 0.1 s; the
    `FORBEAR_MIN_INTERVAL` variable replaces the default);
  - with `frequency>1`, progress must also have entered a new multiple of it.

  In a log, a line is written at every new multiple of 10 % (of `frequency`, if >1), with no time throttle.
- The speed is an exponential moving average, weight `smoothing` (0.3) on the latest inter-drawing rate.
  `smoothing=0` gives the mean since the start. The ETA is `(1-fraction)/rate_`.
- With `partial_blocks`, the partial cell takes the filled fg colour and the *empty* bg colour, so a track drawn with
  `empty_char_color_bg` has no seam. The glyphs are UTF-8 byte literals (`PARTIAL_BLOCKS`). Do not build them with
  `char(code, UCS4)`: gfortran writes UCS4 characters above 255 as `?` to non-UTF-8 units. The byte strings work
  because every terminal just receives the bytes.
- Every number in the line has a fixed width, so the line never shrinks and `ESC[K` only matters for messages. The
  speed (6 characters) and the scale labels (5) go through `compact_real(x, w)`, which uses the first form that fits:
  two decimals, one decimal, an integer, `m.me<n>`, `me<n>`. It writes `F32.d` plus `adjustl`, because gfortran's
  `F0.d` drops the leading zero. The ETA uses `hms` (8 characters, days beyond 100 h); the summary uses `duration`.
- The percent is written `(A,I3,A)` as `' nnn%'`: it always has its own leading space (count, speed, ETA too), so an
  element before it needs no trailing space. `hide_cursor=.false.` drops the hide/show sequences; the end still
  writes a newline and `ESC[J`.
- `width=0` makes a spinner-only or counter-only display, with no bar body. `add_scale_bar` requires `width >= 22`
  and otherwise raises `error stop`.

## Tooling

- `.github/workflows/matrix.yml` (project-owned, as in FLAP) runs several jobs: gfortran 13/14/15 (16 trunk allowed to
  fail) with `tests-gnu-debug`; ifx 2025.3 and nvfortran 26.1 via `fortran-lang/setup-fortran` with
  `tests-intel-debug`/`tests-nvf-debug`; `fpm test`; and the docs-examples job, which reruns `docs_examples.sh` and
  fails if `docs/examples` changed.
- CI (`.github/workflows/ci.yml`) runs `FoBiS.py fetch` (gated on `src/third_party/.deps_config.ini`), then the
  coverage action. That action runs `fobis rule --ex makecoverage-analysis`, which removes FACE's coverage data and
  calls `scripts/compute-coverage.sh` to write `docs/public/coverage.json` (`{"pct":"…"}`). That file is a generated
  artifact: do not commit it. The release tarball does not contain FACE; `install.sh` fetches it.
- The docs mirror FLAP's layout. "Start here" and the reference live in `docs/guide/`, the tutorial
  (`manual/tutorial/0N-*.md`) and the cookbook live in `docs/manual/`, and `docs/api/` is generated by `formal` from
  `docs/ford.md` and committed. Build the site with `cd docs && npm install && npm run docs:build`; the `predocs` hook
  regenerates the API first. The custom theme (`docs/.vitepress/theme/`) uses the Catppuccin Latte palette in light
  mode and Mocha in dark mode, the GIFs' palette, plus the `img.gif` and `.showcase` styles.
- Apart from `makecoverage-analysis`, the `fobos` rules still use the deprecated `FoBiS.py rule -ex …` / `-mode` /
  `-coverage` forms. The legacy `.travis.yml`, `doc/` (FORD) and `wiki/` are still present.

Releases: run `scripts/release.sh --patch|--minor|--major|vX.Y.Z` from `master`. It regenerates `CHANGELOG.md`
with git-cliff, writes `VERSION` (and the `fpm.toml` version, if the manifest declares one), commits, tags and
pushes. The tag push triggers `release.yml`. Existing tags go up to `v1.4.0`, the first release with the seven 2026 features (write, logs, ETA, count, summary, partial blocks, positions) and the compiler matrix.
