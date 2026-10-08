residual = 1._R8P
call bar%start
do step = 1, steps
   call advance
   residual = residual / 2._R8P
   call bar%update(current=real(step, R8P))
enddo
