!< **forbear** test.

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
