!as march
!run -f 26 march_2 march
program march
!< Tutorial, chapter 2: the look of the bar.
use, intrinsic :: iso_fortran_env, only : R8P=>real64
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
   !< Advance the solution of one time step (here: just spend some time).
   real(R8P) :: x
   integer   :: i
   x = 0._R8P
   do i = 1, 2000000
      x = x + sqrt(real(i, R8P))
   enddo
   if (x < 0._R8P) print *, x
   endsubroutine advance
endprogram march
