<a name="top"></a>

# forbear [![GitHub tag](https://img.shields.io/github/tag/szaghi/forbear.svg)]() [![Join the chat at https://gitter.im/szaghi/forbear](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/szaghi/forbear?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[![License](https://img.shields.io/badge/license-GNU%20GeneraL%20Public%20License%20v3,%20GPLv3-blue.svg)]()
[![License](https://img.shields.io/badge/license-BSD2-red.svg)]()
[![License](https://img.shields.io/badge/license-BSD3-red.svg)]()
[![License](https://img.shields.io/badge/license-MIT-red.svg)]()

[![Status](https://img.shields.io/badge/status-stable-brightgreen.svg)]()
[![Build Status](https://travis-ci.org/szaghi/forbear.svg?branch=master)](https://travis-ci.org/szaghi/forbear)
[![Coverage Status](https://img.shields.io/codecov/c/github/szaghi/forbear.svg)](http://codecov.io/github/szaghi/forbear?branch=master)

### forbear, Fortran (progress) B(~~e~~)ar environment

> *forbear* has `e` mute, pronce it like `forbar`.

A KISS pure Fortran Library for building and running fancy progress bar

- forbear is a pure Fortran (KISS) library for building and running fancy progress bar for modern Fortran projects;
- forbear is Fortran 2008+ standard compliant;
- forbear is OOP designed;
- forbear is TDD developed;
- forbear is a Free, Open Source Project.

#### Issues

[![GitHub issues](https://img.shields.io/github/issues/szaghi/forbear.svg)]()
[![Ready in backlog](https://badge.waffle.io/szaghi/forbear.png?label=ready&title=Ready)](https://waffle.io/szaghi/forbear)
[![In Progress](https://badge.waffle.io/szaghi/forbear.png?label=in%20progress&title=In%20Progress)](https://waffle.io/szaghi/forbear)
[![Open bugs](https://badge.waffle.io/szaghi/forbear.png?label=bug&title=Open%20Bugs)](https://waffle.io/szaghi/forbear)

#### Compiler Support

[![Compiler](https://img.shields.io/badge/GNU-v4.9.2+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/Intel-v12.x+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/IBM%20XL-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/g95-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/NAG-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/PGI-not%20tested-yellow.svg)]()

---

| [What is forbear?](#what-is-forbear) | [Main features](#main-features) | [Copyrights](#copyrights) | [Documentation](#documentation) | [Install](#install) |

---

## What is forbear?

In Executing long-time running programs it could be helpful to display a *progress bar* with some informative data. **forbear** is designed to perform just this task, handle the progress and display it as the user specifications.

Go to [Top](#top)

## Main features

forbear is inspired by the python great module [python-progressbar](https://github.com/WoLpH/python-progressbar), thus many features are taken from it. Here the main features are listed.

+ [ ] Timer
+ [ ] ETA
+ [ ] AdaptiveETA
+ [ ] FileTransferSpeed
+ [ ] AdaptiveTransferSpeed
+ [ ] AnimatedMarker
+ [ ] Counter
+ [ ] Percentage
+ [ ] FormatLabel
+ [ ] SimpleProgress
+ [ ] Bar
+ [ ] ReverseBar
+ [ ] BouncingBar
+ [ ] RotatingMarker
+ [ ] DynamicMessage

Any feature request is welcome.

Go to [Top](#top)

## Copyrights

forbear is a Free and Open Source Software (FOSS), it is distributed under a **very permissive** multi-licensing system: selectable licenses are [GPLv3](http://www.gnu.org/licenses/gpl-3.0.html), [BSD2-Clause](http://opensource.org/licenses/BSD-2-Clause), [BSD3-Clause](http://opensource.org/licenses/BSD-3-Clause) and [MIT](http://opensource.org/licenses/MIT), feel free to select the license that best matches your workflow.

> Anyone is interest to use, to develop or to contribute to FORESEER is welcome.

More details can be found on [wiki](https://github.com/szaghi/forbear/wiki/Copyrights).

Go to [Top](#top)

## Documentation

Besides this README file the forbear documentation is contained into its own [wiki](https://github.com/szaghi/forbear/wiki). Detailed documentation of the API is contained into the [GitHub Pages](http://szaghi.github.io/forbear/index.html) that can also be created locally by means of [ford tool](https://github.com/cmacmackin/ford).

### A Taste of forbear

A minimal *plate*:

```fortran
program forbear_minimal
!< **forbear** test.
use, intrinsic :: iso_fortran_env, only : I4P=>int32, R8P=>real32
use forbear, only : bar_object
implicit none

type(bar_object) :: bar
real(R8P)        :: x
real(R8P)        :: y
integer(I4P)     :: i
integer(I4P)     :: j

x = 0._R8P
call bar%initialize(filled_char='+', prefix='progress |', suffix='| ', add_progress_percent=.true.)
call bar%start
do i=1, 20
   x = x + 0.05_R8P
   do j=1, 100000000
      y = sqrt(x) ! just spend some times
   enddo
   call bar%update(current=x)
enddo
endprogram forbear_minimal
```

That *built and run* provides:

```shell
→ ./forbear_minimal
progress |++++++++++++++++++++++++++++    | 85%
```

Go to [Top](#top)

---

## Install

forbear is a Fortran library composed by several modules.

> Before download and compile the library you must check the [requirements](https://github.com/szaghi/forbear/wiki/Requirements).

To download and build the project two main ways are available:

+ exploit the [install script](#install-script) that can be downloaded [here](https://github.com/szaghi/forbear/releases/latest)
+ [manually download and build](#manually-download-and-build):
  + [download](#download)
  + [build](#build)

---

### install script

forbear ships a bash script (downloadable from [here](https://github.com/szaghi/forbear/releases/latest)) that is able to automatize the download and build steps. The script `install.sh` has the following usage:

```shell
→ ./install.sh
Install script of forbear
Usage:

install.sh --help|-?
    Print this usage output and exit

install.sh --download|-d <arg> [--verbose|-v]
    Download the project

    --download|-d [arg]  Download the project, arg=git|wget to download with git or wget respectively
    --verbose|-v         Output verbose mode activation

install.sh --build|-b <arg> [--verbose|-v]
    Build the project

    --build|-b [arg]  Build the project, arg=fobis|make|cmake to build with FoBiS.py, GNU Make or CMake respectively
    --verbose|-v      Output verbose mode activation

Examples:

install.sh --download git
install.sh --build make
install.sh --download wget --build cmake
```

> The script does not cover all possibilities.

The script operation modes are 2 (*collapsible* into one-single-mode):

+ download a new fresh-clone of the latest master-release by means of:
  + [git](https://git-scm.com/);
  + [wget](https://www.gnu.org/software/wget/) (also [curl](https://curl.haxx.se/) is necessary);
+ build a fresh-clone project as static-linked library by means of:
  + [FoBiS.py](https://github.com/szaghi/FoBiS);
  + [GNU Make](https://www.gnu.org/software/make/);
  + [CMake](https://cmake.org/);

> you can mix any of the above combinations accordingly to the tools available.

Typical usages are:

```shell
# download and prepare the project by means of git and build with GNU Make
install.sh --dowload git --build make
# download and prepare the project by means of wget (curl) and build with CMake
install.sh --dowload wget --build cmake
# download and prepare the project by means of git and build with FoBiS.py
install.sh --dowload git --build fobis
```

---

### manually download and build

#### download

To download all the available releases and utilities (fobos, license, readme, etc...), it can be convenient to _clone_ whole the project:

```shell
git clone https://github.com/szaghi/forbear
cd forbear
git submodule update --init
```

Alternatively, you can directly download a release from GitHub server, see the [ChangeLog](https://github.com/szaghi/forbear/wiki/ChangeLog).

#### build

The most easy way to compile forbear is to use [FoBiS.py](https://github.com/szaghi/FoBiS) within the provided fobos file.

Consequently, it is strongly encouraged to install [FoBiS.py](https://github.com/szaghi/FoBiS#install).

| [Build by means of FoBiS](#build-by-means-of-fobis) | [Build by means of GNU Make](#build-by-means-of-gnu-make) | [Build by means of CMake](#build-by-means-of-cmake) |

---

#### build by means of FoBiS

FoBiS.py is a KISS tool for automatic building of modern Fortran projects. Providing very few options, FoBiS.py is able to build almost automatically complex Fortran projects with cumbersome inter-modules dependency. This removes the necessity to write complex makefile. Moreover, providing a very simple options file (in the FoBiS.py nomenclature indicated as `fobos` file) FoBiS.py can substitute the (ab)use of makefile for other project stuffs (build documentations, make project archive, etc...). forbear is shipped with a fobos file that can build the library in both _static_ and _shared_ forms and also build the `Test_Driver` program. The provided fobos file has several building modes.

##### listing fobos building modes
Typing:
```bash
FoBiS.py build -lmodes
```
the following message should be printed:
```bash
The fobos file defines the following modes:
 - "shared-gnu"
  - "static-gnu"
  - "test-driver-gnu"
  - "shared-gnu-debug"
  - "static-gnu-debug"
  - "test-driver-gnu-debug"
  - "shared-intel"
  - "static-intel"
  - "test-driver-intel"
  - "shared-intel-debug"
  - "static-intel-debug"
  - "test-driver-intel-debug"
```
The modes should be self-explicative: `shared`, `static` and `test-driver` are the modes for building (in release, optimized form) the shared and static versions of the library and the Test Driver program, respectively. The other 3 modes are the same, but in debug form instead of release one. `-gnu` use the `GNU gfortran` compiler while `-intel` the Intel one.

##### building the library
The `shared` or `static` directories are created accordingly to the form of the library built. The compiled objects and mod files are placed inside this directory, as well as the linked library.
###### release shared library
```bash
FoBiS.py build -mode shared-gnu
```
###### release static library
```bash
FoBiS.py build -mode static-gnu
```
###### debug shared library
```bash
FoBiS.py build -mode shared-gnu-debug
```
###### debug static library
```bash
FoBiS.py build -mode static-gnu-debug
```

---

#### build by means of GNU Make

Not yet supported.

#### Build by means of CMake

Not yet supported.

Go to [Top](#top)
