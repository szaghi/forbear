steps = 50
call bar%initialize(prefix_string='march ', bracket_left_string='[', bracket_right_string=']', &
                    filled_char_string='#', empty_char_string='.', add_progress_percent=.true., &
                    progress_percent_color_fg='yellow', message_color_fg='green', width=30,     &
                    max_value=real(steps, R8P))
residual = 1._R8P
call bar%start
do step = 1, steps
   call advance
   residual = residual / 2._R8P
   call bar%update(current=real(step, R8P))
   if (residual < 1.e-9_R8P) exit ! steady state: the remaining steps are not needed
enddo
call bar%finish(message='steady state')
