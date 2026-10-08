!as march
!run -f 26 march_6-running march
!run march_6 march
program march
!< Tutorial, chapter 6: talking while the bar runs.
use, intrinsic :: iso_fortran_env, only : I8P=>int64, R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar      ! the progress bar
integer          :: steps    ! time steps
integer          :: step     ! current time step
real(R8P)        :: residual ! residual of the solution
character(64)    :: text     ! a message

steps = 50
!region init
call bar%initialize(prefix_string='march ', bracket_left_string='[', bracket_right_string=']',  &
                    filled_char_string='#', empty_char_string='.', add_progress_percent=.true., &
                    message_color_fg='cyan', width=30, max_value=real(steps, R8P))
!endregion init
!region loop
call bar%start
residual = 1._R8P
do step = 1, steps
   call advance
   residual = residual / 2._R8P
   if (mod(step, 10) == 0) then
      write(text, '(A,I0,A)') 'step ', step, ': solution saved'
      call bar%write(trim(text))                            ! a line above the bar
   endif
   write(text, '(A,ES9.2)') 'residual', residual
   call bar%update(current=real(step, R8P), message=trim(text)) ! the end of the bar line
enddo
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
