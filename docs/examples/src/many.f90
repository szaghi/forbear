!run many many
program many
!< A loop of many iterations, whose counter does not start at one.
use, intrinsic :: iso_fortran_env, only : I8P=>int64, R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar      ! the progress bar
integer          :: first    ! first record
integer          :: last     ! last record
integer          :: i        ! current record
integer          :: percent  ! progress, in percent
integer          :: previous ! progress at the previous update

first = 1001
last  = 2000
!region percent
call bar%initialize(prefix_string='records ', add_progress_percent=.true., max_value=100._R8P)
call bar%start
previous = 0
do i = first, last
   ! integer division: 100 only at the last record
   percent = int((100_I8P * (i - first + 1)) / (last - first + 1))
   if (percent /= previous) call bar%update(current=real(percent, R8P))
   previous = percent
enddo
!endregion percent
endprogram many
