!< **forbear** test, expected to fail: unknown digits.

program forbear_test_xfail_digits
!< **forbear** test, expected to fail: unknown digits.
use, intrinsic :: iso_fortran_env, only : R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar !< Bar under test.

call bar%initialize(digits='segments', interactive=.false.)
call bar%start
call bar%update(current=1._R8P)
endprogram forbear_test_xfail_digits
