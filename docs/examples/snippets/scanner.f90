program solve
!< The line: a pulse trail on an indeterminate bar, as the scanner of a 1980s talking car.
use, intrinsic :: iso_fortran_env, only : I8P=>int64, R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar        ! the progress bar
real(R8P)        :: residual   ! residual of the solution
integer          :: iterations ! iterations of the solver

call bar%initialize(prefix_string='solve ', prefix_color_fg='#FF3B30',                                       &
                    indeterminate=.true., width=24, add_progress_count=.true., progress_count_color_fg='#FFB000', &
                    filled_char_string='▬', filled_char_color_fg='#FF3B30',                                   &
                    empty_char_string='▬', empty_char_color_fg='#3C0C0A',                                     &
                    pulse_trail='#C0281E #7A1912 #4A0F0B')
residual = 1._R8P
iterations = 0
call bar%start
do while (residual > 1.e-12_R8P) ! how many iterations: unknown until done
   call advance
   residual = residual / 2._R8P
   iterations = iterations + 1
   call bar%update(current=real(iterations, R8P))
enddo
call bar%finish

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
endprogram solve
