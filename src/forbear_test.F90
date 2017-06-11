!< **forbear** test.

program forbear_minimal
!< **forbear** test.
use, intrinsic :: iso_fortran_env, only : I4P=>int32, R8P=>real64
use forbear, only : bar_object, UCS4
implicit none

type(bar_object) :: bar

print*

call bar%initialize(prefix_string='Counter ', width=0, add_progress_percent=.true., progress_percent_color_fg='yellow')
call worker

call bar%initialize(prefix_string='Spinner 1 : ', width=0, spinner_string='|', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 2 : ', width=0, spinner_string='⠋', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 3 : ', width=0, spinner_string='⣾', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 4 : ', width=0, spinner_string='⠓', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 5 : ', width=0, spinner_string='⠄', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 6 : ', width=0, spinner_string='⠐', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 7 : ', width=0, spinner_string='⠒', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 8 : ', width=0, spinner_string='⠁', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 9 : ', width=0, spinner_string='⡃⢐', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 10: ', width=0, spinner_string='⡀', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 11: ', width=0, spinner_string='⡐', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 12: ', width=0, spinner_string='⣸', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 13: ', width=0, spinner_string='┤', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 14: ', width=0, spinner_string='✶', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 15: ', width=0, spinner_string='_', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 16: ', width=0, spinner_string='▃', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 17: ', width=0, spinner_string='▉', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 18: ', width=0, spinner_string='@', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 19: ', width=0, spinner_string='°', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 20: ', width=0, spinner_string='▒', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 21: ', width=0, spinner_string='⠂', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 22: ', width=0, spinner_string='▖', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 23: ', width=0, spinner_string='◢', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 24: ', width=0, spinner_string='◜', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 25: ', width=0, spinner_string='⊙', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 26: ', width=0, spinner_string='◰', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 27: ', width=0, spinner_string='◴', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 28: ', width=0, spinner_string='◐', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 29: ', width=0, spinner_string='⊶', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 30: ', width=0, spinner_string='▫', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 31: ', width=0, spinner_string='□', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 32: ', width=0, spinner_string='▪', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 33: ', width=0, spinner_string='▯', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 34: ', width=0, spinner_string='⦿', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 35: ', width=0, spinner_string='◍', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 36: ', width=0, spinner_string='◉', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 37: ', width=0, spinner_string='㊂', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 38: ', width=0, spinner_string='(  ●   )', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 39: ', width=0, spinner_string='🌔 ', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Spinner 40: ', width=0, spinner_string='🚶 ', spinner_color_fg='red')
call worker

call bar%initialize(prefix_string='Minimal bar ')
call worker

call bar%initialize(width=32,                                                                       &
                    bracket_left_string='|', bracket_left_color_fg='blue',                          &
                    empty_char_string='o', empty_char_color_fg='blue', empty_char_color_bg='white', &
                    filled_char_string=' ', filled_char_color_bg='blue',                            &
                    bracket_right_string='|', bracket_right_color_fg='blue',                        &
                    prefix_string='Fancy ASCII bar, progress ', prefix_color_fg='red',              &
                    add_progress_percent=.true., progress_percent_color_fg='yellow',                &
                    add_progress_speed=.true., progress_speed_color_fg='green',                     &
                    add_scale_bar=.true., scale_bar_color_fg='blue', scale_bar_style='underline_on')
call worker

#ifdef UCS4_SUPPORTED
call bar%initialize(width=32,                                                              &
                    bracket_left_string='|', bracket_left_color_fg='blue',                 &
                    empty_char_string=UCS4_'⬜', empty_char_color_fg='blue',                &
                    filled_char_string=UCS4_'⬛', filled_char_color_fg='blue',              &
                    bracket_right_string='|', bracket_right_color_fg='blue',               &
                    prefix_string=UCS4_'Fancy UCS4 bar, ƥƦōƔƦĘşş ', prefix_color_fg='red', &
                    add_progress_percent=.true., progress_percent_color_fg='yellow',       &
                    add_date_time=.true., date_time_color_fg='magenta',                    &
                    add_progress_speed=.true., progress_speed_color_fg='green')
call worker
#endif

contains
   subroutine worker
   !< The worker.
   real(R8P)        :: x
   real(R8P)        :: y
   integer(I4P)     :: i
   integer(I4P)     :: j
   call bar%start
   x = 0._R8P
   do i=1, 20
      x = x + 0.05_R8P
      do j=1, 10000000
         y = sqrt(x) ! just spend some times
      enddo
      call bar%update(current=x)
   enddo
   endsubroutine worker
endprogram forbear_minimal
