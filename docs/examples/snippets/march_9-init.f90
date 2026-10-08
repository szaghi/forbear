call bar%initialize(template='march {bar:30} {percent:yellow} step {count:cyan} ETA {eta:blue} res {residual:magenta}', &
                    partial_blocks=.true., filled_char_color_fg='cyan', empty_char_color_bg='black_intense', &
                    max_value=real(steps, R8P))
field%value => residual
call bar%add_field('residual', field) ! after initialize, before start
