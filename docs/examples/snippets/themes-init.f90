call vfd%initialize(theme='vfd', prefix_string='vfd   ', add_progress_percent=.true., width=30, &
                    max_value=real(steps, R8P))
call amber%initialize(theme='amber', prefix_string='amber ', bar_profile='ramp', add_progress_percent=.true., &
                      width=30, max_value=real(steps, R8P), position=1)
call kitt%initialize(theme='kitt', prefix_string='kitt  ', indeterminate=.true., add_progress_count=.true., &
                     width=30, position=2)
