call steps_bar%start
do step = 1, steps
   call iterations_bar%start
   do iteration = 1, iterations
      call advance
      call iterations_bar%update(current=real(iteration, R8P))
   enddo
   call steps_bar%update(current=real(step, R8P))
enddo
print '(A)', 'march: done'
