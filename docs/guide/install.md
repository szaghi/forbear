---
title: Installation
---

# Installation

## Requirements

- A **Fortran 2008** compiler with the C preprocessor (the sources are `.F90`). forbear is tested on every push with
  gfortran 13, 14 and 15 (and the gfortran 16 trunk), Intel ifx 2025.3 and NVIDIA nvfortran 26.1, with FoBiS, and with
  fpm; it also builds and passes its tests with gfortran 11 and 12 and nvfortran 26.5.
- [FACE](https://github.com/szaghi/FACE) (ANSI colours), fetched into `src/third_party` by every build system.
- A terminal that understands ANSI escape sequences and the carriage return: see [Behaviour and
  limitations](./limitations#terminals-files-and-batch-jobs).

## FoBiS

[FoBiS](https://github.com/szaghi/FoBiS) (3.8+) is the reference build system of forbear.

```bash
git clone https://github.com/szaghi/forbear && cd forbear
fobis fetch                         # FACE into src/third_party, at the commit pinned by its fobos.lock
fobis build --mode static-gnu       # static/libforbear.a, modules in static/mod
fobis build --mode shared-gnu       # shared/libforbear.so
fobis build --mode tests-gnu        # the test programs into exe/
bash scripts/run_tests.sh           # run them
fobis build --mode tests-intel      # the same with Intel ifx, tests-nvf with NVIDIA nvfortran
fobis build --lmodes                # every mode (GNU, Intel ifx, NVIDIA nvfortran; debug variants)
```

`fobis fetch --update` moves FACE to its latest commit and updates `src/third_party/fobos.lock`.

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
forbear = { git = "https://github.com/szaghi/forbear", tag = "v1.6.1" }
```

v1.2.0 and older releases have no `fpm.toml`; the features of this documentation need v1.6.1 or later.

`fpm build` fetches forbear and FACE. To build and test forbear itself:

```bash
git clone https://github.com/szaghi/forbear && cd forbear
fpm test
```

## Install script

From v1.3.0 on, every release ships a tarball and an `install.sh`, which downloads forbear, fetches
FACE with `fobis fetch` and builds it with FoBiS:

```bash
./install.sh --download git --build fobis
./install.sh --download wget --build fobis    # the release tarball
```
