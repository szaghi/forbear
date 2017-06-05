!< **forbear** test.

program forbear_minimal
!< **forbear** test.
use, intrinsic :: iso_fortran_env, only : I4P=>int32, R8P=>real64
use forbear, only : bar_object
implicit none

type(bar_object) :: bar
real(R8P)        :: x
real(R8P)        :: y
integer(I4P)     :: i
integer(I4P)     :: j

print '(A)', 'Minimal bar'
call bar%initialize()
call worker

print '(A)', 'Only counter bar'
call bar%initialize(width=0, add_progress_percent=.true., progress_percent_color_fg='yellow')
call worker

print '(A)', 'Fancy bar'
call bar%initialize(width=32,                                                                       &
                    bracket_left_string='|', bracket_left_color_fg='blue',                          &
                    empty_char_string='o', empty_char_color_fg='blue', empty_char_color_bg='white', &
                    filled_char_string=' ', filled_char_color_bg='blue',                            &
                    bracket_right_string='|', bracket_right_color_fg='blue',                        &
                    prefix_string='progress ', prefix_color_fg='red',                               &
                    add_progress_percent=.true., progress_percent_color_fg='yellow',                &
                    add_progress_speed=.true., progress_speed_color_fg='green',                     &
                    add_date_time=.true., date_time_color_fg='magenta',                             &
                    add_scale_bar=.true., scale_bar_color_fg='blue', scale_bar_style='underline_on')
call worker

contains
   subroutine worker
   !< The worker.
   call bar%start
   x = 0._R8P
   do i=1, 20
      x = x + 0.05_R8P
      do j=1, 100000000
         y = sqrt(x) ! just spend some times
      enddo
      call bar%update(current=x)
   enddo
   endsubroutine worker
endprogram forbear_minimal
