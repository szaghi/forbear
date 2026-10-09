call bar%start
do step = 1, steps
   call bar%update(current=real(step, R8P))
   if (.not.bar%is_stdout_locked()) print '(A)', 'the bar is done: the terminal is free again'
enddo
