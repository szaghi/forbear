call style%initialize(bracket_left_string='[', bracket_right_string='] ', filled_char_string='=', &
                      empty_char_string=' ', add_progress_percent=.true., width=30)
do p = 1, size(phases)
   bar = style                                       ! copy the configuration
   call bar%initialize(prefix_string=phases(p))      ! re-initialize: everything else is reset!
   call run_phase
enddo
