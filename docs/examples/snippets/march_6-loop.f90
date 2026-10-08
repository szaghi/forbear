call bar%start
residual = 1._R8P
do step = 1, steps
   call advance
   residual = residual / 2._R8P
   if (mod(step, 10) == 0) then
      write(text, '(A,I0,A)') 'step ', step, ': solution saved'
      call bar%write(trim(text))                            ! a line above the bar
   endif
   write(text, '(A,ES9.2)') 'residual', residual
   call bar%update(current=real(step, R8P), message=trim(text)) ! the end of the bar line
enddo
