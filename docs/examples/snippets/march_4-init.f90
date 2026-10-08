call bar%initialize(prefix_string='march ', bracket_left_string='[', bracket_right_string='] ', &
                    filled_char_string='#', empty_char_string='.',                             &
                    add_progress_percent=.true., progress_percent_color_fg='yellow',           &
                    add_progress_count=.true., progress_count_color_fg='cyan',                 &
                    add_progress_speed=.true., progress_speed_color_fg='green',                &
                    add_eta=.true., eta_color_fg='blue',                                       &
                    add_scale_bar=.true., scale_bar_color_fg='blue',                           &
                    add_date_time=.true., date_time_color_fg='magenta',                        &
                    add_summary=.true., summary_color_fg='green',                              &
                    width=30, max_value=real(steps, R8P))
