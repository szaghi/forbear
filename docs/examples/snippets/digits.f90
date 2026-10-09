program march
!< The line: seven-segment digits, their padding as unlit 8s, as the speedometer of a 1980s dashboard.
use, intrinsic :: iso_fortran_env, only : I8P=>int64, R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar   ! the progress bar
integer          :: steps ! time steps
integer          :: step  ! current time step

steps = 120
call bar%initialize(prefix_string='march ', prefix_color_fg='#2EF5C0', width=30,                              &
                    filled_char_string='▌', filled_char_color_fg='#2EF5C0',                                     &
                    empty_char_string='▌', empty_char_color_fg='#0E342C',                                       &
                    add_progress_percent=.true., progress_percent_color_fg='#FFB000',                           &
                    add_progress_count=.true., progress_count_color_fg='#FFB000',                               &
                    digits='segment', digits_unlit_color='#3A2800', max_value=real(steps, R8P))
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
