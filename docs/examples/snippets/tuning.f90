program march
!< Cookbook: keywords that tune a bar: the cursor, the speed, the state of the terminal.
use, intrinsic :: iso_fortran_env, only : R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar   ! the progress bar
integer          :: steps ! time steps
integer          :: step  ! current time step

steps = 50
call bar%initialize(hide_cursor=.false., max_value=real(steps, R8P)) ! the cursor stays visible, even if the program stops
call bar%initialize(add_progress_speed=.true., add_eta=.true., smoothing=1._R8P, max_value=real(steps, R8P)) ! momentary
call bar%start
do step = 1, steps
   call bar%update(current=real(step, R8P))
   if (.not.bar%is_stdout_locked()) print '(A)', 'the bar is done: the terminal is free again'
enddo
endprogram march
