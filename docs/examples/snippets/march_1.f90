program march
!< Tutorial, chapter 1: a first progress bar.
use, intrinsic :: iso_fortran_env, only : R8P=>real64
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
