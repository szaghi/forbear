call bar%initialize(max_value=real(steps, R8P))    ! 1. initialize
call bar%start                                     ! 2. start
do step = 1, steps
   call advance                                    ! the real work
   call bar%update(current=real(step, R8P))        ! 3. update
enddo
print '(A,I0,A)', 'march: ', steps, ' time steps done'
