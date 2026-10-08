program march
!< Tutorial, chapter 10: a loop of unknown length, and a loop left before its end.
use, intrinsic :: iso_fortran_env, only : I8P=>int64, R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar        ! the progress bar
real(R8P)        :: residual   ! residual of the solution
integer          :: iterations ! iterations of the solver
integer          :: steps      ! time steps
integer          :: step       ! current time step

call bar%initialize(template='solve {spinner:cyan} [{bar:20}] {count:yellow} iterations, {elapsed:blue}', &
                    indeterminate=.true., spinner_string='⠋', filled_char_string='#', empty_char_string='.')
residual = 1._R8P
iterations = 0
call bar%start
do while (residual > 1.e-6_R8P) ! how many iterations: unknown until done
   call advance
   residual = residual / 2._R8P
   iterations = iterations + 1
   call bar%update(current=real(iterations, R8P))
enddo
call bar%finish

steps = 50
call bar%initialize(prefix_string='march ', bracket_left_string='[', bracket_right_string=']', &
                    filled_char_string='#', empty_char_string='.', add_progress_percent=.true., &
                    progress_percent_color_fg='yellow', message_color_fg='green', width=30,     &
                    max_value=real(steps, R8P))
residual = 1._R8P
call bar%start
do step = 1, steps
   call advance
   residual = residual / 2._R8P
   call bar%update(current=real(step, R8P))
   if (residual < 1.e-9_R8P) exit ! steady state: the remaining steps are not needed
enddo
call bar%finish(message='steady state')

contains
   subroutine advance
   !< Advance the solution of one iteration (here: wait 40 ms, as a real computation would take).
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
