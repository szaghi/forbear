call steps_bar%initialize(prefix_string='time step ', bracket_left_string='[', bracket_right_string='] ', &
                          partial_blocks=.true., filled_char_color_fg='cyan', add_progress_count=.true., &
                          width=30, max_value=real(steps, R8P))
call iterations_bar%initialize(prefix_string='  newton  ', bracket_left_string='[', bracket_right_string='] ', &
                               partial_blocks=.true., filled_char_color_fg='yellow', add_progress_count=.true., &
                               width=30, max_value=real(iterations, R8P), position=1)
