!as tach
!run -f 41 ramp tach
program tach
!< The line: a ramp profile with colour zones, as the rising bars of a 1980s dashboard tachometer.
use, intrinsic :: iso_fortran_env, only : I8P=>int64, R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar   ! the progress bar
integer          :: steps ! time steps
integer          :: step  ! current time step

steps = 50
!region init
call bar%initialize(prefix_string='RPM ', prefix_color_fg='#2EF5C0', prefix_style='bold_on',               &
                    bar_profile='ramp', empty_char_color_fg='#0E342C',                                     &
                    bar_zones='0.7:#2EF5C0 0.88:#FFB000 1:#FF3B30',                                        &
                    add_progress_percent=.true., progress_percent_color_fg='#FFB000',                      &
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
endprogram tach
