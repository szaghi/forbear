call bar%initialize(template='solve {spinner:cyan} [{bar:20}] {count:yellow} iterations, {elapsed:blue}', &
                    indeterminate=.true., spinner_string='⠋', filled_char_string='#', empty_char_string='.')
residual = 1._R8P
iterations = 0
call bar%start
do while (residual > 1.e-6_R8P) ! how many iterations: unknown until done
   call advance
   residual = residual / 2._R8P
   iterations = iterations + 1
   call bar%update(current=real(iterations, R8P))
enddo
call bar%finish
