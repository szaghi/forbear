do p = 1, size(phases)
   call bar%initialize(prefix_string=phases(p), bracket_left_string='[', bracket_right_string='] ', &
                       filled_char_string='=', empty_char_string=' ', add_progress_percent=.true., width=30)
   call run_phase
enddo
