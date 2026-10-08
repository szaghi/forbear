!< **forbear** test: when the bar is drawn, with `frequency` and `min_interval`.

program forbear_test_frequency
!< **forbear** test: when the bar is drawn, with `frequency` and `min_interval`.
!<
!< On a terminal every drawing ends with "erase to the end of the line" and a carriage return: counting them counts the
!< drawings.
use, intrinsic :: iso_fortran_env, only : I4P=>int32, R8P=>real64
use forbear, only : bar_object
use forbear_test_tools, only : capture_close, capture_open, check, count_text, CR, ESC, line, lines_number, report
implicit none
type(bar_object)              :: bar  !< Bar under test.
character(len=:), allocatable :: text !< What the bar wrote.
integer(I4P)                  :: u    !< Capture unit.

! a log: a line at every new ten, also when the progress jumps over the multiples
u = capture_open('test_frequency_1.txt')
call bar%initialize(width=7, add_progress_percent=.true., interactive=.false., max_value=7._R8P, output_unit=u)
call run(7)
text = capture_close(u, 'test_frequency_1.txt')
call check(lines_number(text) == 8, 'log, 7 steps: 0% and one line per step (each enters a new ten)')
call check(line(text, 3) == '**-----  28%', 'log, 7 steps: 2/7 is 28%, truncated')

! a log with frequency=25
u = capture_open('test_frequency_2.txt')
call bar%initialize(width=7, add_progress_percent=.true., interactive=.false., max_value=7._R8P, frequency=25, &
                    output_unit=u)
call run(7)
text = capture_close(u, 'test_frequency_2.txt')
call check(lines_number(text) == 5, 'log, frequency=25: 0%, 28%, 57%, 85%, 100%')

! a terminal, no throttle: one drawing per update, plus the one of start
u = capture_open('test_frequency_3.txt')
call bar%initialize(interactive=.true., min_interval=0._R8P, max_value=7._R8P, output_unit=u)
call run(7)
text = capture_close(u, 'test_frequency_3.txt')
call check(count_text(text, ESC//'[K'//CR) == 8, 'terminal, min_interval=0: 8 drawings')

! a terminal, throttled: only 0% and 100% are drawn within a long interval
u = capture_open('test_frequency_4.txt')
call bar%initialize(interactive=.true., min_interval=1000._R8P, max_value=7._R8P, output_unit=u)
call run(7)
text = capture_close(u, 'test_frequency_4.txt')
call check(count_text(text, ESC//'[K'//CR) == 2, 'terminal, min_interval=1000: only 0% and 100%')

! a terminal with frequency=25: the multiples entered
u = capture_open('test_frequency_5.txt')
call bar%initialize(interactive=.true., min_interval=0._R8P, frequency=25, max_value=7._R8P, output_unit=u)
call run(7)
text = capture_close(u, 'test_frequency_5.txt')
call check(count_text(text, ESC//'[K'//CR) == 5, 'terminal, frequency=25: 5 drawings')

call report
contains
   subroutine run(steps)
   !< Run the bar over its steps.
   integer(I4P), intent(in) :: steps !< Steps.
   integer(I4P)             :: s     !< Counter.

   call bar%start
   do s = 1, steps
      call bar%update(current=real(s, R8P))
   enddo
   endsubroutine run
endprogram forbear_test_frequency
