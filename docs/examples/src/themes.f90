!as march
!run -f 90 themes march
program march
!< Colours and styles: the three dashboard themes, one bar each.
use, intrinsic :: iso_fortran_env, only : I8P=>int64, R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: vfd   ! a vacuum fluorescent display
type(bar_object) :: amber ! an amber liquid crystal display
type(bar_object) :: kitt  ! the scanner of a talking car
integer          :: steps ! time steps
integer          :: step  ! current time step

steps = 50
!region init
call vfd%initialize(theme='vfd', prefix_string='vfd   ', add_progress_percent=.true., width=30, &
                    max_value=real(steps, R8P))
call amber%initialize(theme='amber', prefix_string='amber ', bar_profile='ramp', add_progress_percent=.true., &
                      width=30, max_value=real(steps, R8P), position=1)
call kitt%initialize(theme='kitt', prefix_string='kitt  ', indeterminate=.true., add_progress_count=.true., &
                     width=30, position=2)
!endregion init
call vfd%start
call amber%start
call kitt%start
do step = 1, steps
   call advance
   call vfd%update(current=real(step, R8P))
   call amber%update(current=real(step, R8P))
   call kitt%update(current=real(step, R8P))
enddo
call kitt%finish

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
endprogram march
