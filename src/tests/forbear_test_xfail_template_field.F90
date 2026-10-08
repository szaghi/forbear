!< **forbear** test, expected to fail: a template field never added.

program forbear_test_xfail_template_field
!< **forbear** test, expected to fail: a template field never added.
use, intrinsic :: iso_fortran_env, only : R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar !< Bar under test.

call bar%initialize(template='{nope}', interactive=.false.)
call bar%start
call bar%update(current=1._R8P)
endprogram forbear_test_xfail_template_field
