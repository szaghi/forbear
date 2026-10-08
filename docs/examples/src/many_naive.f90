!run many_naive many_naive
program many_naive
!< A loop of many iterations, updated at every iteration: the last ones already round to 100%.
use, intrinsic :: iso_fortran_env, only : R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar ! the progress bar
integer          :: i   ! current iteration

call bar%initialize(prefix_string='records ', add_progress_percent=.true., max_value=1000._R8P)
call bar%start
do i = 1, 1000
   call bar%update(current=real(i, R8P))
enddo
endprogram many_naive
