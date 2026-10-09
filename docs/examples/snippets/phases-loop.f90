call bar%start
do step = 1, steps
   if (step == 10) bar%prefix%string = 'assemble '
   if (step == 30) bar%prefix%string = 'solve    '
   call advance
   call bar%update(current=real(step, R8P))
enddo
