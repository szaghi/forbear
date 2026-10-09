program march
!< Cookbook: a prefix that names the phase of the solver, changed while the bar runs.
use, intrinsic :: iso_fortran_env, only : I8P=>int64, R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar   ! the progress bar
integer          :: steps ! time steps
integer          :: step  ! current time step

steps = 50
call bar%initialize(prefix_string='mesh     ', prefix_color_fg='cyan', bracket_left_string='[', bracket_right_string=']', &
                    width=30, add_progress_percent=.true., max_value=real(steps, R8P))
call bar%start
do step = 1, steps
   if (step == 10) bar%prefix%string = 'assemble '
   if (step == 30) bar%prefix%string = 'solve    '
   call advance
   call bar%update(current=real(step, R8P))
enddo

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
