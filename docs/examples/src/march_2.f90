!as march
!run -f 26 march_2 march
program march
!< Tutorial, chapter 2: the look of the bar.
use, intrinsic :: iso_fortran_env, only : I8P=>int64, R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar   ! the progress bar
integer          :: steps ! time steps
integer          :: step  ! current time step

steps = 50
!region init
call bar%initialize(prefix_string='march ', suffix_string=' time steps', &
                    bracket_left_string='[', bracket_right_string=']',   &
                    filled_char_string='#', empty_char_string='.',       &
                    width=40, max_value=real(steps, R8P))
!endregion init
call bar%start
do step = 1, steps
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
