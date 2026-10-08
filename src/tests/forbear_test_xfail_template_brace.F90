!< **forbear** test, expected to fail: an unclosed brace in a template.

program forbear_test_xfail_template_brace
!< **forbear** test, expected to fail: an unclosed brace in a template.
use, intrinsic :: iso_fortran_env, only : R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar !< Bar under test.

call bar%initialize(template='x {bar', interactive=.false.)
call bar%start
call bar%update(current=1._R8P)
endprogram forbear_test_xfail_template_brace
