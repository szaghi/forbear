!run minimal minimal
program minimal
!< The smallest progress bar: the default range is [0, 1].
use, intrinsic :: iso_fortran_env, only : R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar ! the progress bar
real(R8P)        :: x   ! progress, in [0, 1]
integer          :: i   ! counter
integer          :: j   ! counter
real(R8P)        :: y   ! work result

x = 0._R8P
call bar%initialize(filled_char_string='+', prefix_string='progress |', suffix_string='| ', &
                    add_progress_percent=.true.)
call bar%start
do i = 1, 20
   x = x + 0.05_R8P
   do j = 1, 1000000
      y = sqrt(x) ! just spend some time
   enddo
   call bar%update(current=x)
enddo
if (y < 0._R8P) print *, y
endprogram minimal
