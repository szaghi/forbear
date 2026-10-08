call bar%initialize(prefix_string='records ', add_progress_percent=.true., frequency=5, &
                    min_value=real(first - 1, R8P), max_value=real(last, R8P))
call bar%start
do i = first, last
   call bar%update(current=real(i, R8P))
enddo
