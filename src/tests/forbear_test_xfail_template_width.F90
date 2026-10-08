!< **forbear** test, expected to fail: a width for a field other than the bar.

program forbear_test_xfail_template_width
!< **forbear** test, expected to fail: a width for a field other than the bar.
use, intrinsic :: iso_fortran_env, only : R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar !< Bar under test.

call bar%initialize(template='{percent:30}', interactive=.false.)
call bar%start
call bar%update(current=1._R8P)
endprogram forbear_test_xfail_template_width
