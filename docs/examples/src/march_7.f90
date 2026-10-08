!as march
!run -f 32 march_7-running march
!run march_7 march
program march
!< Tutorial, chapter 7: nested loops, two bars.
use, intrinsic :: iso_fortran_env, only : I8P=>int64, R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: steps_bar      ! the bar of the time steps
type(bar_object) :: iterations_bar ! the bar of the iterations of a time step
integer          :: steps          ! time steps
integer          :: step           ! current time step
integer          :: iterations     ! iterations of a time step
integer          :: iteration      ! current iteration

steps = 5
iterations = 10
!region init
call steps_bar%initialize(prefix_string='time step ', bracket_left_string='[', bracket_right_string='] ', &
                          partial_blocks=.true., filled_char_color_fg='cyan', add_progress_count=.true., &
                          width=30, max_value=real(steps, R8P))
call iterations_bar%initialize(prefix_string='  newton  ', bracket_left_string='[', bracket_right_string='] ', &
                               partial_blocks=.true., filled_char_color_fg='yellow', add_progress_count=.true., &
                               width=30, max_value=real(iterations, R8P), position=1)
!endregion init
!region loop
call steps_bar%start
do step = 1, steps
   call iterations_bar%start
   do iteration = 1, iterations
      call advance
      call iterations_bar%update(current=real(iteration, R8P))
   enddo
   call steps_bar%update(current=real(step, R8P))
enddo
print '(A)', 'march: done'
!endregion loop

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
