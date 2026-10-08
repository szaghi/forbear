!< **forbear** test, expected to fail: an indeterminate bar with a percent.

program forbear_test_xfail_indeterminate
!< **forbear** test, expected to fail: an indeterminate bar with a percent.
use, intrinsic :: iso_fortran_env, only : R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar !< Bar under test.

call bar%initialize(indeterminate=.true., add_progress_percent=.true., interactive=.false.)
call bar%start
call bar%update(current=1._R8P)
endprogram forbear_test_xfail_indeterminate
