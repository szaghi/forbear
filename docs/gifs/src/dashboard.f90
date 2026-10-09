program dashboard
!< Recording of docs/public/gifs/dashboard.gif: the 1980s dashboard looks of forbear, three bars at once.
use, intrinsic :: iso_fortran_env, only : I8P=>int64, R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: speed  ! a blue-green display, with a redline and seven-segment numbers
type(bar_object) :: tach   ! an amber tachometer, rising
type(bar_object) :: scan   ! a red scanner, for a loop of unknown length
integer          :: steps  ! time steps
integer          :: step   ! current time step

steps = 90
call speed%initialize(theme='vfd', prefix_string='SPEED ', width=36, bar_zones='0.75:#2EF5C0 0.9:#FFB000 1:#FF3B30', &
                      add_progress_count=.true., digits='segment', max_value=real(steps, R8P))
call tach%initialize(theme='amber', prefix_string='RPM   ', width=36, bar_profile='ramp', &
                     add_progress_percent=.true., digits='segment', max_value=real(steps, R8P), position=1)
call scan%initialize(theme='kitt', prefix_string='SCAN  ', width=36, indeterminate=.true., add_progress_count=.true., &
                     digits='segment', position=2)
call speed%start
call tach%start
call scan%start
do step = 1, steps
   call advance
   call speed%update(current=real(step, R8P))
   call tach%update(current=real(step, R8P))
   call scan%update(current=real(step, R8P))
enddo
call scan%finish

contains
   subroutine advance
   !< Advance the solution of one time step (here: wait 40 ms, as a real computation would take).
   integer(I8P) :: start ! clock at the start
   integer(I8P) :: now   ! clock now
   integer(I8P) :: rate  ! clock counts per second
   call system_clock(start, rate)
   do
      call system_clock(now)
      if (now - start >= rate / 25) exit
   enddo
   endsubroutine advance
endprogram dashboard
