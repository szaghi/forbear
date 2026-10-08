program march
!< Tutorial, chapter 1: a first progress bar.
use, intrinsic :: iso_fortran_env, only : I8P=>int64, R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar   ! the progress bar
integer          :: steps ! time steps
integer          :: step  ! current time step

steps = 50
call bar%initialize(max_value=real(steps, R8P))    ! 1. initialize
call bar%start                                     ! 2. start
do step = 1, steps
   call advance                                    ! the real work
   call bar%update(current=real(step, R8P))        ! 3. update
enddo
print '(A,I0,A)', 'march: ', steps, ' time steps done'

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
