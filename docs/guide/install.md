---
title: Installation
---

# Installation

## Requirements

- A **Fortran 2008** compiler with the C preprocessor (the sources are `.F90`). The examples of this documentation are
  built and run with gfortran 16; the `fobos` file also has modes for Intel (`ifort`/`ifx`) and PGI/NVIDIA compilers.
- [FACE](https://github.com/szaghi/FACE) (ANSI colours), a git submodule in `src/third_party/FACE`.
- A terminal that understands ANSI escape sequences and the carriage return: see [Behaviour and
  limitations](./limitations#terminals-files-and-batch-jobs).

## FoBiS

[FoBiS](https://github.com/szaghi/FoBiS) (3.8+) is the reference build system of forbear.

```bash
git clone --recursive https://github.com/szaghi/forbear && cd forbear   # --recursive: FACE
fobis build --mode static-gnu       # static/libforbear.a, modules in static/mod
fobis build --mode shared-gnu       # shared/libforbear.so
fobis build --mode tests-gnu        # the test program into exe/
bash scripts/run_tests.sh           # run it
fobis build --lmodes                # every mode (GNU, Intel, PGI; debug variants)
```

On a clone made without `--recursive`, fetch FACE with `git submodule update --init`.

The library archive contains FACE too, so a program needs only `libforbear.a` and the module directory:

```bash
gfortran -I static/mod my_program.f90 static/libforbear.a -o my_program
```

The GNU modes define `UCS4_SUPPORTED` and `ASCII_SUPPORTED`, which give the `UCS4` and `ASCII` character kinds their
own values; the Intel and PGI modes do not, and both kinds fall back to the default character kind there. Plain
(default kind) string literals work everywhere, Unicode ones included, provided the source file is UTF-8.

## fpm

Add forbear as a dependency in your project's `fpm.toml`:

```toml
[dependencies]
forbear = { git = "https://github.com/szaghi/forbear", branch = "master" }
```

v1.2.0 and older releases have no `fpm.toml`: use the branch until the next release, then pin its tag.

`fpm build` fetches forbear and FACE. To build and test forbear itself:

```bash
git clone https://github.com/szaghi/forbear && cd forbear
fpm test
```

## Install script

From the release after v1.2.0 on, every release ships a tarball, which contains FACE, and an `install.sh`, which
downloads the tarball and builds it with FoBiS:

```bash
./install.sh --download wget --build fobis
```

`--download git` clones without the submodule, so the build cannot find FACE: clone with `git clone --recursive`
instead, as above.
