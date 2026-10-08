program march
!< Tutorial, chapter 6: sharing the terminal.
use, intrinsic :: iso_fortran_env, only : R8P=>real64, error_unit
use forbear, only : bar_object
implicit none
type(bar_object) :: bar      ! the progress bar
integer          :: steps    ! time steps
integer          :: step     ! current time step
real(R8P)        :: residual ! residual of the solution

steps = 50
call bar%initialize(prefix_string='march ', bracket_left_string='[', bracket_right_string='] ', &
                    filled_char_string='#', empty_char_string='.', add_progress_percent=.true., &
                    frequency=10, output_unit=error_unit,                                      &
                    width=40, max_value=real(steps, R8P))
call bar%start
residual = 1._R8P
do step = 1, steps
   call advance
   residual = residual / 2._R8P
   if (mod(step, 10) == 0) write(*, '(A,I3,A,ES9.2)') 'step ', step, ', residual ', residual
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
