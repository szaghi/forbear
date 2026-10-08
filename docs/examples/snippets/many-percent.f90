call bar%initialize(prefix_string='records ', add_progress_percent=.true., max_value=100._R8P)
call bar%start
previous = 0
do i = first, last
   ! integer division: 100 only at the last record
   percent = int((100_I8P * (i - first + 1)) / (last - first + 1))
   if (percent /= previous) call bar%update(current=real(percent, R8P))
   previous = percent
enddo
