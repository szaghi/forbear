program hero
!< The showcase of the documentation: a (pretend) CFD run, with a mesh bar, nested solver bars, messages and a summary.
use, intrinsic :: iso_fortran_env, only : I8P=>int64, R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: mesh       ! the bar of the mesh generation
type(bar_object) :: steps_bar  ! the bar of the time steps
type(bar_object) :: newton     ! the bar of the iterations of a time step
integer          :: blocks     ! mesh blocks
integer          :: steps      ! time steps
integer          :: iterations ! iterations of a time step
integer          :: i          ! counter
integer          :: step       ! current time step
real(R8P)        :: residual   ! residual of the solution
character(64)    :: text       ! a message

blocks = 40
call mesh%initialize(prefix_string='mesh   ', bracket_left_string='▕', bracket_right_string='▏ ',                 &
                     partial_blocks=.true., filled_char_color_fg='magenta', add_progress_percent=.true.,          &
                     progress_percent_color_fg='yellow', add_progress_count=.true., progress_count_color_fg='blue', &
                     empty_char_string=' ', empty_char_color_bg='black_intense', width=36, max_value=real(blocks, R8P))
call mesh%start
do i = 1, blocks
   call wait(30)
   call mesh%update(current=real(i, R8P))
enddo

steps = 30
iterations = 8
call steps_bar%initialize(prefix_string='solve  ', bracket_left_string='▕', bracket_right_string='▏ ',              &
                          partial_blocks=.true., filled_char_color_fg='cyan', add_progress_percent=.true., &
                          empty_char_string=' ', empty_char_color_bg='black_intense', &
                          progress_percent_color_fg='yellow', add_progress_count=.true.,                         &
                          progress_count_color_fg='blue', add_eta=.true., eta_color_fg='green',                   &
                          message_color_fg='magenta_intense', add_summary=.true., summary_color_fg='green',        &
                          width=36, max_value=real(steps, R8P))
call newton%initialize(prefix_string='newton ', bracket_left_string='▕', bracket_right_string='▏ ',                 &
                       partial_blocks=.true., filled_char_color_fg='yellow', add_progress_count=.true., &
                       empty_char_string=' ', empty_char_color_bg='black_intense', &
                       progress_count_color_fg='blue', width=36, max_value=real(iterations, R8P), position=1)
call steps_bar%start
residual = 1._R8P
do step = 1, steps
   call newton%start
   do i = 1, iterations
      call wait(25)
      call newton%update(current=real(i, R8P))
   enddo
   residual = residual * 0.5_R8P
   if (mod(step, 10) == 0) then
      write(text, '(A,I0,A)') '✔ checkpoint saved at step ', step
      call steps_bar%write(trim(text))
   endif
   write(text, '(A,ES8.1)') 'residual', residual
   call steps_bar%update(current=real(step, R8P), message=trim(text))
enddo

contains
   subroutine wait(milliseconds)
   !< Wait, as a real computation would take.
   integer, intent(in) :: milliseconds ! time to wait
   integer(I8P)        :: start        ! clock at the start
   integer(I8P)        :: now          ! clock now
   integer(I8P)        :: rate         ! clock counts per second
   call system_clock(start, rate)
   do
      call system_clock(now)
      if ((now - start) * 1000 >= milliseconds * rate) exit
   enddo
   endsubroutine wait
endprogram hero
