!as march
!run -f 26 march_5-bar march
!run -f 77 march_5-spinner march
!run -f 128 march_5-counter march
program march
!< Tutorial, chapter 5: spinners and counters.
use, intrinsic :: iso_fortran_env, only : I8P=>int64, R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar   ! the progress bar
integer          :: steps ! time steps

steps = 50
!region bar
call bar%initialize(prefix_string='march ', bracket_left_string='[', bracket_right_string='] ', &
                    filled_char_string='#', empty_char_string='.',                              &
                    spinner_string='⠋', spinner_color_fg='red',                                 &
                    width=40, max_value=real(steps, R8P))
!endregion bar
call run
!region spinner
call bar%initialize(prefix_string='march ', spinner_string='⠋', spinner_color_fg='red', &
                    width=0, max_value=real(steps, R8P))
!endregion spinner
call run
!region counter
call bar%initialize(prefix_string='march ', add_progress_percent=.true., &
                    width=0, max_value=real(steps, R8P))
!endregion counter
call run

contains
   subroutine run
   !< Run the time steps.
   integer :: step ! current time step
   call bar%start
   do step = 1, steps
      call advance
      call bar%update(current=real(step, R8P))
   enddo
   endsubroutine run

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
