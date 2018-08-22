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

![taste screencast](media/taste.gif)

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

[![Compiler](https://img.shields.io/badge/GNU-v6.3.1+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/Intel-v17.x+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/IBM%20XL-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/g95-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/NAG-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/PGI-not%20tested-yellow.svg)]()

---

| [What is forbear?](#what-is-forbear) | [Main features](#main-features) | [Copyrights](#copyrights) | [Documentation](#documentation) | [Install](#install) |

---

## What is forbear?

In Executing long-time running programs it could be helpful to display a *progress bar* with some informative data. **forbear** is designed to perform just this task, handle the progress and display it as the user specifications:

> forbear handles data related to the progress of a (long) time run giving an informative, pretty-formatted ouput for each time step (or accordingly a given frequency) as the user specifications.

Go to [Top](#top)

## Main features

+ [x] Bar Element-Based Structure:
  + fully customizable elements:
    + foreground color;
    + background color;
    + style;
+ [x] Bar
+ [x] Bar scale
+ [x] Progress Percentage
+ [x] Progress Speed
+ [x] Start-End Time
+ [ ] ETA
+ [ ] Adaptive ETA
+ [ ] Reverse Bar
+ [x] Spinners
+ [x] Static Prefix-Suffix Messages
+ [ ] Dynamic Message
+ [x] Well Documented
+ [x] Test Driven Developed
+ [x] FOSS

Any feature request is welcome.

Go to [Top](#top)

## Copyrights

forbear is a Free and Open Source Software (FOSS), it is distributed under a **very permissive** multi-licensing system: selectable licenses are [GPLv3](http://www.gnu.org/licenses/gpl-3.0.html), [BSD2-Clause](http://opensource.org/licenses/BSD-2-Clause), [BSD3-Clause](http://opensource.org/licenses/BSD-3-Clause) and [MIT](http://opensource.org/licenses/MIT), feel free to select the license that best matches your workflow.

> Anyone is interest to use, to develop or to contribute to FORESEER is welcome.

More details can be found on [wiki](https://github.com/szaghi/forbear/wiki/Copyrights).

Go to [Top](#top)

## Documentation

Besides this README file the forbear documentation is contained into its own [wiki](https://github.com/szaghi/forbear/wiki). Detailed documentation of the API is contained into the [GitHub Pages](http://szaghi.github.io/forbear/index.html) that can also be created locally by means of [ford tool](https://github.com/cmacmackin/ford).

| [A taster of forber](#a_taste_of_forbear) | [API documentation](#api_documentation)

---

### A Taste of forbear

A minimal *plate*:

```fortran
program forbear_minimal
!< **forbear** test.
use, intrinsic :: iso_fortran_env, only : I4P=>int32, R8P=>real64
use forbear, only : bar_object
implicit none

type(bar_object) :: bar
real(R8P)        :: x
real(R8P)        :: y
integer(I4P)     :: i
integer(I4P)     :: j

x = 0._R8P
call bar%initialize(filled_char_string='+', prefix_string='progress |', suffix_string='| ', add_progress_percent=.true.)
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

### API documentation

forbear library exposes only one class, namely the `bar_object` class, that is used to handle the progress of your runs. The `bar_object` class has the following public methods

+ [`destroy`](#destroy_method)
+ [`initialize`](#initialize_method)
+ [`is_stdout_locked`](#is_stdout_locked_method)
+ [`start`](#start_method)
+ [`update`](#update_method)

#### `destroy` method

Signature

```fortran
pure subroutine destroy(self)
```

It has not dummy arguments (except the `bar_object self` bound-passed). It destroys the bar, namely reset it to the minimal (safe) status.

Examples

```fortran
use forbear
type(bar_obejct) :: bar

call bar%destroy
```

#### `initialize` method

Signature

```fortran
subroutine initialize(self,                                                                                               &
                      prefix_string, prefix_color_fg, prefix_color_bg, prefix_style,                                      &
                      suffix_string, suffix_color_fg, suffix_color_bg, suffix_style,                                      &
                      bracket_left_string, bracket_left_color_fg, bracket_left_color_bg, bracket_left_style,              &
                      bracket_right_string, bracket_right_color_fg, bracket_right_color_bg, bracket_right_style,          &
                      empty_char_string, empty_char_color_fg, empty_char_color_bg, empty_char_style,                      &
                      filled_char_string, filled_char_color_fg, filled_char_color_bg, filled_char_style,                  &
                      add_scale_bar, scale_bar_color_fg, scale_bar_color_bg, scale_bar_style,                             &
                      add_progress_percent, progress_percent_color_fg, progress_percent_color_bg, progress_percent_style, &
                      add_progress_speed, progress_speed_color_fg, progress_speed_color_bg, progress_speed_style,         &
                      add_date_time, date_time_color_fg, date_time_color_bg, date_time_style,                             &
                      width, min_value, max_value, frequency)
```

This method initializes the bar accordingly to the user specifications. It has a huge list of dummy arguments because the bar is fully customizable. The meaning of the arguments (except the obvious passed `self`) are:

```fortran
   character(len=*),  intent(in), optional :: prefix_string             ! Prefix string.
   character(len=*),  intent(in), optional :: prefix_color_fg           ! Prefix foreground color.
   character(len=*),  intent(in), optional :: prefix_color_bg           ! Prefix background color.
   character(len=*),  intent(in), optional :: prefix_style              ! Prefix style.
   character(len=*),  intent(in), optional :: suffix_string             ! Suffix string.
   character(len=*),  intent(in), optional :: suffix_color_fg           ! Suffix foreground color.
   character(len=*),  intent(in), optional :: suffix_color_bg           ! Suffix background color.
   character(len=*),  intent(in), optional :: suffix_style              ! Suffix style.
   character(len=*),  intent(in), optional :: bracket_left_string       ! Left bracket string.
   character(len=*),  intent(in), optional :: bracket_left_color_fg     ! Left bracket foreground color.
   character(len=*),  intent(in), optional :: bracket_left_color_bg     ! Left bracket background color.
   character(len=*),  intent(in), optional :: bracket_left_style        ! Left bracket style.
   character(len=*),  intent(in), optional :: bracket_right_string      ! Right bracket string
   character(len=*),  intent(in), optional :: bracket_right_color_fg    ! Right bracket foreground color.
   character(len=*),  intent(in), optional :: bracket_right_color_bg    ! Right bracket background color.
   character(len=*),  intent(in), optional :: bracket_right_style       ! Right bracket style.
   character(len=1),  intent(in), optional :: empty_char_string         ! Empty char.
   character(len=*),  intent(in), optional :: empty_char_color_fg       ! Empty char foreground color.
   character(len=*),  intent(in), optional :: empty_char_color_bg       ! Empty char background color.
   character(len=*),  intent(in), optional :: empty_char_style          ! Empty char style.
   character(len=1),  intent(in), optional :: filled_char_string        ! Filled char.
   character(len=*),  intent(in), optional :: filled_char_color_fg      ! Filled char foreground color.
   character(len=*),  intent(in), optional :: filled_char_color_bg      ! Filled char background color.
   character(len=*),  intent(in), optional :: filled_char_style         ! Filled char style.
   logical,           intent(in), optional :: add_scale_bar             ! Add scale to the bar.
   character(len=*),  intent(in), optional :: scale_bar_color_fg        ! Scale bar foreground color.
   character(len=*),  intent(in), optional :: scale_bar_color_bg        ! Scale bar background color.
   character(len=*),  intent(in), optional :: scale_bar_style           ! Scale bar style.
   logical,           intent(in), optional :: add_progress_percent      ! Add progress in percent.
   character(len=*),  intent(in), optional :: progress_percent_color_fg ! Progress percent foreground color.
   character(len=*),  intent(in), optional :: progress_percent_color_bg ! Progress percent background color.
   character(len=*),  intent(in), optional :: progress_percent_style    ! Progress percent style.
   logical,           intent(in), optional :: add_progress_speed        ! Add progress in percent.
   character(len=*),  intent(in), optional :: progress_speed_color_fg   ! Progress speed foreground color.
   character(len=*),  intent(in), optional :: progress_speed_color_bg   ! Progress speed background color.
   character(len=*),  intent(in), optional :: progress_speed_style      ! Progress speed style.
   logical,           intent(in), optional :: add_date_time             ! Add date and time.
   character(len=*),  intent(in), optional :: date_time_color_fg        ! Date and time foreground color.
   character(len=*),  intent(in), optional :: date_time_color_bg        ! Date and time background color.
   character(len=*),  intent(in), optional :: date_time_style           ! Date and time style.
   integer(I4P),      intent(in), optional :: width                     ! With of the bar.
   real(R8P),         intent(in), optional :: min_value                 ! Minimum value.
   real(R8P),         intent(in), optional :: max_value                 ! Maximum value.
   integer(I4P),      intent(in), optional :: frequency                 ! Bar update frequency, in range `[1%,100%]`.
```

Examples

```fortran
use forbear
type(bar_obejct) :: bar

! initialize a bar that will have only the progress percentage counter
call bar%initialize(width=0, add_progress_percent=.true., progress_percent_color_fg='yellow')

! initialize a very fancy bar with "all batteries included"
call bar%initialize(width=32,                                                                       &
                    bracket_left_string='|', bracket_left_color_fg='blue',                          &
                    empty_char_string='o', empty_char_color_fg='blue', empty_char_color_bg='white', &
                    filled_char_string=' ', filled_char_color_bg='blue',                            &
                    bracket_right_string='|', bracket_right_color_fg='blue',                        &
                    prefix_string='progress ', prefix_color_fg='red',                               &
                    add_progress_percent=.true., progress_percent_color_fg='yellow',                &
                    add_progress_speed=.true., progress_speed_color_fg='green',                     &
                    add_date_time=.true., date_time_color_fg='magenta',                             &
                    add_scale_bar=.true., scale_bar_color_fg='blue', scale_bar_style='underline_on')
```

Note that if you initialize the bar to have also the scale over the progress bar the bar's length must be at least 22 characters.

#### `is_stdout_locked` method

Signature

```fortran
pure function is_stdout_locked(self) result(is_locked)
```

This functions return `.true.` if the bar had locked the standard output unit, namely if the bar had started to print progress data. As a matter of fact, forbear locks the standard output unit during its running because it *refreshes* the last terminal row where it has been started. The user should be use this function to check if printing on standard output is safe, otherwise the `look and feel` of the bar will be destroyed.

Examples

```fortran
use forbear
type(bar_obejct) :: bar

if (.not.bar%is_stdout_locked) then
   ! you can safely print to stdout...
endif
```

#### `start` method

Signature

```fortran
subroutine start(self)
```

This method must be invoked just before the *time consuming work* starts. It locks the standard output unit and initialize the bar output, e.g. if the user selected to add the bar scale it is printed into this method.

Note that there is not an equivalent *end* method: the bar is supposed to end when the progress reaches (or overcome) the 100%, that is handled directly into the `update` method.

```fortran
use forbear
type(bar_obejct) :: bar

call bar%initialize
call bar%start
! in the following the long time consuming work
```

#### `update` method

Signature

```fortran
subroutine update(self, current)
```

This method updates the bar each time it is called (accordingly to the frequency set into the `initialize` method, 1 by default). It takes only one argument, namely `current`: it is the current progress expressed in `real(real64)` where `real64` is the kind constant provided by the `iso_fortran_env` intrinsic module. The current progress is evaluated with respect the minimum and maximum values set into the `initialize` method, that by default are `0` and `1` respectively. Essentially, the progress is computed as

```fortran
progress = nint(current / (self%max_value - self%min_value) * 100)
```

The `update` method handles the bar update automatically: if it is the first call after the `start` method some useful data for the progress statistics are stored, while if the 100% progress is reached it automatically ends the bar smoothly.

Examples

```fortran
use, intrinsic :: iso_fortran_env
use forbear
type(bar_obejct) :: bar

call bar%initialize(max_value=2.1_real64)
call bar%start
! in the following the long time consuming work, e.g. increase "t" up a limit
   t = t + Dt
   call bar%update(current=t)
```

For a complete examples see [fobear_test](https://github.com/szaghi/forbear/blob/master/src/tests/forbear_test.f90)
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
