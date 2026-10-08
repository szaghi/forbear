program many
!< A loop of a million iterations, whose counter does not start at 1.
use, intrinsic :: iso_fortran_env, only : R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar   ! the progress bar
integer          :: first ! first record
integer          :: last  ! last record
integer          :: i     ! current record

first = 1001
last  = 1000000
call bar%initialize(prefix_string='records ', add_progress_percent=.true., frequency=5, &
                    min_value=real(first - 1, R8P), max_value=real(last, R8P))
call bar%start
do i = first, last
   call bar%update(current=real(i, R8P))
enddo
endprogram many
