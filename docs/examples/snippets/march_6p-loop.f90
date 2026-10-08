call bar%start
do step = 1, steps
   call advance
   call bar%update(current=real(step, R8P))
   if (mod(step, 20) == 0) then
      call bar%suspend           ! the bar leaves the terminal...
      call save_solution(step)   ! ...to a library that prints on its own...
      call bar%resume            ! ...and comes back below its lines
   endif
enddo
