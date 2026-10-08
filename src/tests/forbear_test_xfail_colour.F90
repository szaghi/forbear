!< **forbear** test, expected to fail: an unknown colour keyword.

program forbear_test_xfail_colour
!< **forbear** test, expected to fail: an unknown colour keyword.
use, intrinsic :: iso_fortran_env, only : R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar !< Bar under test.

call bar%initialize(prefix_string='p', prefix_color_fg='rde', interactive=.false.)
call bar%start
call bar%update(current=1._R8P)
endprogram forbear_test_xfail_colour
