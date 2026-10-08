!< **forbear** test: the range of the bar, its completion, the 200-update rounding, values out of range.

program forbear_test_range
!< **forbear** test: the range of the bar, its completion, the 200-update rounding, values out of range.
!<
!< In a log (`interactive=.false.`) a bar writes a line at 0%, at every 10% and at 100%: the lines tell exactly what
!< the bar computed.
use, intrinsic :: iso_fortran_env, only : I4P=>int32, R8P=>real64
use forbear, only : bar_object
use forbear_test_tools, only : capture_close, capture_open, check, count_text, line, lines_number, report
implicit none
type(bar_object)              :: bar  !< Bar under test.
character(len=:), allocatable :: text !< What the bar wrote.
integer(I4P)                  :: u    !< Capture unit.
integer(I4P)                  :: i    !< Counter.
real(R8P)                     :: x    !< Accumulated value.

! a range that does not start at 0
u = capture_open('test_range_1.txt')
call bar%initialize(width=10, add_progress_percent=.true., add_progress_count=.true., interactive=.false., &
                    min_value=1000._R8P, max_value=2000._R8P, output_unit=u)
call bar%start
call check(.not.bar%is_stdout_locked(), 'a log does not lock the terminal')
do i = 1001, 2000
   call bar%update(current=real(i, R8P))
enddo
text = capture_close(u, 'test_range_1.txt')
call check(lines_number(text) == 11, 'range [1000, 2000]: a line at 0%, every 10% and 100%')
call check(line(text, 1) == '----------   0% 1000/2000', 'range [1000, 2000]: starts at 0%')
call check(line(text, 6) == '*****-----  50% 1500/2000', 'range [1000, 2000]: 50% at 1500')
call check(line(text, -1) == '********** 100% 2000/2000', 'range [1000, 2000]: ends at 100%')

! many updates: only the last one is 100% (rounding made it 100% from 99.5% on)
u = capture_open('test_range_2.txt')
call bar%initialize(width=10, add_progress_percent=.true., interactive=.false., max_value=1000._R8P, output_unit=u)
call bar%start
do i = 1, 1000
   call bar%update(current=real(i, R8P))
enddo
text = capture_close(u, 'test_range_2.txt')
call check(lines_number(text) == 11, '1000 updates: 11 lines')
call check(count_text(text, '100%') == 1, '1000 updates: one line at 100%')

! values beyond the range: clamped, no crash; updates after 100% do nothing
u = capture_open('test_range_3.txt')
call bar%initialize(width=10, add_progress_percent=.true., add_progress_count=.true., interactive=.false., &
                    max_value=10._R8P, output_unit=u)
call bar%start
call bar%update(current=-5._R8P)
do i = 1, 15
   call bar%update(current=real(i, R8P))
enddo
text = capture_close(u, 'test_range_3.txt')
call check(lines_number(text) == 11, 'beyond max: 11 lines, the updates after 100% do nothing')
call check(line(text, 1) == '----------   0%  0/10', 'below min: clamped to 0%')
call check(line(text, -1) == '********** 100% 10/10', 'beyond max: clamped to 100%')

! round-off: 20 sums of 0.05 make 0.999..., still 100%
u = capture_open('test_range_4.txt')
call bar%initialize(width=10, add_progress_percent=.true., interactive=.false., output_unit=u)
call bar%start
x = 0._R8P
do i = 1, 20
   x = x + 0.05_R8P
   call bar%update(current=x)
enddo
text = capture_close(u, 'test_range_4.txt')
call check(line(text, -1) == '********** 100%', '20 sums of 0.05: the bar completes')

! an empty range completes at start, and leaves the terminal free
u = capture_open('test_range_5.txt')
call bar%initialize(width=10, add_progress_percent=.true., interactive=.true., min_value=1._R8P, max_value=1._R8P, &
                    output_unit=u)
call bar%start
text = capture_close(u, 'test_range_5.txt')
call check(.not.bar%is_stdout_locked(), 'empty range: the terminal is free after start')
call check(count_text(text, '100%') == 1, 'empty range: one drawing, at 100%')

! a bar can be started again after it completes
u = capture_open('test_range_6.txt')
call bar%initialize(width=10, add_progress_percent=.true., interactive=.false., output_unit=u)
do i = 1, 2
   call bar%start
   call bar%update(current=1._R8P)
enddo
text = capture_close(u, 'test_range_6.txt')
call check(count_text(text, '100%') == 2, 'restart: two runs, two completions')

call report
endprogram forbear_test_range
