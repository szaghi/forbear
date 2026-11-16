---
title: Installation
---

# Installation

## Requirements

- A **Fortran 2008** compiler with the C preprocessor (the sources are `.F90`). forbear is tested on every push with
  gfortran 16.2, Intel ifx 2026.2 and NVIDIA nvfortran 26.9, with FoBiS, and with
  fpm; it also builds and passes its tests with gfortran 11 and 12 and nvfortran 26.5.
- [FACE](https://github.com/szaghi/FACE) (ANSI colours), fetched into `src/third_party` by every build system.
- A terminal that understands ANSI escape sequences and the carriage return: see [Behaviour and
  limitations](./limitations#terminals-files-and-batch-jobs).

## FoBiS

[FoBiS](https://github.com/szaghi/FoBiS) (3.8+) is the reference build system of forbear.

```bash
git clone https://github.com/szaghi/forbear && cd forbear
fobis fetch                         # FACE master into src/third_party
fobis build --mode static-gnu       # static/libforbear.a, modules in static/mod
fobis build --mode shared-gnu       # shared/libforbear.so
fobis build --mode tests-gnu        # the test programs into exe/
bash scripts/run_tests.sh           # run them
fobis build --mode tests-intel      # the same with Intel ifx, tests-nvf with NVIDIA nvfortran
fobis build --lmodes                # every mode (GNU, Intel ifx, NVIDIA nvfortran; debug variants)
```

An existing clone of FACE stays where it is: `fobis fetch --update` moves it to the latest master.

The library archive contains FACE too, so a program needs only `libforbear.a` and the module directory:

```bash
gfortran -I static/mod my_program.f90 static/libforbear.a -o my_program
```

The GNU modes define `UCS4_SUPPORTED` and `ASCII_SUPPORTED`, which give the `UCS4` and `ASCII` character kinds their
own values; the Intel and NVIDIA modes do not, and both kinds fall back to the default character kind there. Plain
(default kind) string literals work everywhere, Unicode ones included, provided the source file is UTF-8.

## fpm

Add forbear as a dependency in your project's `fpm.toml`:

```toml
[dependencies]
forbear = { git = "https://github.com/szaghi/forbear", branch = "master" }
```

This follows `master`, which always has the features of this documentation. For a fixed version, use
`tag = "vX.Y.Z"` with a [release](https://github.com/szaghi/forbear/releases) (v1.3.0 or later: older ones have no
`fpm.toml`).

`fpm build` fetches forbear and FACE. To build and test forbear itself:

```bash
git clone https://github.com/szaghi/forbear && cd forbear
fpm test
```

## One-line install

```bash
curl -fsSL https://github.com/szaghi/forbear/releases/latest/download/install.sh | bash
```

It installs the latest release into `~/.local` with the build system you choose among those forbear ships
(`--build fobis|fpm`: a menu asks when you do not say), the compiler you name (`--fc`, `--gnu`, `--intel`), one install
per compiler (`lib/forbear/<compiler>-<version>/`, `include/forbear/<compiler>-<version>/`), and makes it usable through
pkg-config (`pkg-config --cflags --libs forbear`, with `PKG_CONFIG_PATH=$HOME/.local/lib/pkgconfig`). The release
carries FACE. Pass options after `bash -s --`, e.g. `... | bash -s -- --build fpm --prefix /opt/forbear`; `--help`
lists them all, `--uninstall` removes the install. The installer is FoBiS's
([One-line installer](https://szaghi.github.io/FoBiS/cookbook/installer)).
