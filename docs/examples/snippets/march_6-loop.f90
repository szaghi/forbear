call bar%start
residual = 1._R8P
do step = 1, steps
   call advance
   residual = residual / 2._R8P
   if (mod(step, 10) == 0) write(*, '(A,I3,A,ES9.2)') 'step ', step, ', residual ', residual
   call bar%update(current=real(step, R8P))
enddo
