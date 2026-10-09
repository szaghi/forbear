call bar%initialize(prefix_string='RPM ', prefix_color_fg='#2EF5C0', prefix_style='bold_on',               &
                    filled_char_string='▌', empty_char_string='▌', empty_char_color_fg='#0E342C',          &
                    bar_zones='0.7:#2EF5C0 0.88:#FFB000 1:#FF3B30',                                        &
                    add_progress_percent=.true., progress_percent_color_fg='#FFB000',                      &
                    width=40, max_value=real(steps, R8P))
