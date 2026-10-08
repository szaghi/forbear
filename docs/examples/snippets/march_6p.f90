module io_library
!< A library that prints on its own: here, with print; in real life, an I/O or a solver library, the MPI runtime, ...
implicit none
contains
   subroutine save_solution(step)
   !< Save the solution of a step, and say so.
   integer, intent(in) :: step ! time step
   print '(A,I4.4,A)', 'io_library: writing solution_', step, '.h5'
   print '(A)', 'io_library: done, 2.1 MB'
   endsubroutine save_solution
endmodule io_library

program march
!< Tutorial, chapter 6: a library printing while the bar runs.
use, intrinsic :: iso_fortran_env, only : I8P=>int64, R8P=>real64
use forbear, only : bar_object
use io_library, only : save_solution
implicit none
type(bar_object) :: bar   ! the progress bar
integer          :: steps ! time steps
integer          :: step  ! current time step

steps = 50
call bar%initialize(prefix_string='march ', bracket_left_string='[', bracket_right_string=']',  &
                    filled_char_string='#', empty_char_string='.', add_progress_percent=.true., &
                    progress_percent_color_fg='yellow', width=30, max_value=real(steps, R8P))
call bar%start
do step = 1, steps
   call advance
   call bar%update(current=real(step, R8P))
   if (mod(step, 20) == 0) then
      call bar%suspend           ! the bar leaves the terminal...
      call save_solution(step)   ! ...to a library that prints on its own...
      call bar%resume            ! ...and comes back below its lines
   endif
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
