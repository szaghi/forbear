!as march
!run march_8l march > run.log; cat run.log
program march
!< Tutorial, chapter 8: signs of life in the log of a long job.
use, intrinsic :: iso_fortran_env, only : I8P=>int64, R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar        ! the progress bar
real(R8P)        :: residual   ! residual of the solution
integer          :: iterations ! iterations of the solver

!region init
call bar%initialize(template='solve {count} iterations, {elapsed} elapsed', indeterminate=.true., &
                    log_interval=0.3_R8P) ! in a log, a line at least every 0.3 s (in a real job: minutes)
!endregion init
residual = 1._R8P
iterations = 0
call bar%start
do while (residual > 1.e-3_R8P)
   call advance
   residual = residual / 2._R8P
   iterations = iterations + 1
   call bar%update(current=real(iterations, R8P))
enddo
call bar%finish

contains
   subroutine advance
   !< Advance the solution of one iteration (here: wait 200 ms, as a long computation would take).
   integer(I8P) :: start ! clock at the start
   integer(I8P) :: now   ! clock now
   integer(I8P) :: rate  ! clock counts per second
   call system_clock(start, rate)
   do
      call system_clock(now)
      if (now - start >= rate / 5) exit
   enddo
   endsubroutine advance
endprogram march
