!< **forbear** test, expected to fail: an unknown spinner key.

program forbear_test_xfail_spinner
!< **forbear** test, expected to fail: an unknown spinner key.
use, intrinsic :: iso_fortran_env, only : R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar !< Bar under test.

call bar%initialize(spinner_string='x', interactive=.false.)
call bar%start
call bar%update(current=1._R8P)
endprogram forbear_test_xfail_spinner
