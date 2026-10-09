!< **forbear** test, expected to fail: bar_zones with limits not increasing.

program forbear_test_xfail_zones_order
!< **forbear** test, expected to fail: bar_zones with limits not increasing.
use, intrinsic :: iso_fortran_env, only : R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar !< Bar under test.

call bar%initialize(bar_zones='0.8:red 0.5:blue', interactive=.false.)
call bar%start
call bar%update(current=1._R8P)
endprogram forbear_test_xfail_zones_order
