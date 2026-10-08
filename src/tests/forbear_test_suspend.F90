!< **forbear** test: suspend and resume, the lines of a log every log_interval seconds.

program forbear_test_suspend
!< **forbear** test: suspend and resume, the lines of a log every log_interval seconds.
use, intrinsic :: iso_fortran_env, only : I4P=>int32, I8P=>int64, R8P=>real64
use forbear, only : bar_object
use forbear_test_tools, only : capture_close, capture_open, check, count_text, CR, ESC, LF, line, lines_number, report
implicit none
type(bar_object)              :: bar  !< Bar under test.
character(len=:), allocatable :: text !< What the bar wrote.
integer(I4P)                  :: u    !< Capture unit.
integer(I4P)                  :: i    !< Counter.

! suspended: the line cleared, the cursor shown, the updates recorded; resumed: drawn with the last update
u = capture_open('test_suspend_1.txt')
call bar%initialize(width=10, max_value=10._R8P, add_progress_percent=.true., interactive=.true., min_interval=0._R8P, &
                    output_unit=u)
call bar%resume ! not suspended: nothing
call bar%start
call bar%update(current=1._R8P)
call bar%suspend
call bar%suspend ! already suspended: nothing
call check(.not.bar%is_stdout_locked(), 'suspend: the terminal is free')
write(u, '(A)') 'library output'
call bar%update(current=4._R8P)
call bar%resume
call check(bar%is_stdout_locked(), 'resume: the terminal is taken again')
text = capture_close(u, 'test_suspend_1.txt')
call check(count_text(text, CR//ESC//'[2K'//ESC//'[?25h') == 1, 'suspend: clear the line, show the cursor, once')
call check(count_text(text, ESC//'[K'//CR) == 3, 'suspend: start, one update, the drawing of resume')
call check(index(text, ESC//'[?25h'//'library output'//LF) > 0, 'suspend: the program writes at the start of the line')
call check(index(text, 'library output'//LF//ESC//'[?25l'//ESC//'[?7l'//'****------  40%') > 0, &
           'resume: drawn below, with the update made while suspended')

! resume draws at once, also when an update would not be due yet
u = capture_open('test_suspend_8.txt')
call bar%initialize(width=10, max_value=10._R8P, interactive=.true., min_interval=100._R8P, output_unit=u)
call bar%start
call bar%suspend
call bar%update(current=5._R8P)
call bar%resume
text = capture_close(u, 'test_suspend_8.txt')
call check(count_text(text, ESC//'[K'//CR) == 2 .and. index(text, '*****-----') > 0, 'resume: drawn at once')

! 100% while suspended: the bar completes when resumed
u = capture_open('test_suspend_2.txt')
call bar%initialize(width=4, max_value=2._R8P, interactive=.true., min_interval=0._R8P, output_unit=u)
call bar%start
call bar%suspend
call bar%update(current=2._R8P)
call check(.not.bar%is_stdout_locked(), 'suspend: not complete while suspended')
call bar%resume
call check(.not.bar%is_stdout_locked(), 'resume: complete at 100%')
text = capture_close(u, 'test_suspend_2.txt')
call check(index(text, '****'//ESC//'[K'//CR//ESC//'[?7h'//ESC//'[?25h'//LF//ESC//'[J') > 0, 'resume: 100%, then the end')

! a bar below the current line clears its own line; the cursor left visible is not shown again
u = capture_open('test_suspend_3.txt')
call bar%initialize(width=4, max_value=2._R8P, interactive=.true., min_interval=0._R8P, position=1, output_unit=u)
call bar%start
call bar%suspend
text = capture_close(u, 'test_suspend_3.txt')
call check(index(text, LF//ESC//'[2K'//CR//ESC//'[1A') > 0, 'suspend at position 1: its line cleared')
u = capture_open('test_suspend_4.txt')
call bar%initialize(width=4, max_value=2._R8P, interactive=.true., min_interval=0._R8P, hide_cursor=.false., &
                    output_unit=u)
call bar%start
call bar%suspend
call bar%finish ! a suspended bar ends drawn
text = capture_close(u, 'test_suspend_4.txt')
call check(index(text, ESC//'[?25') == 0, 'suspend with hide_cursor=.false.: the cursor untouched')
call check(count_text(text, ESC//'[K'//CR) == 2, 'finish of a suspended bar: drawn')

! in a log, suspend does nothing: the lines go on
u = capture_open('test_suspend_5.txt')
call bar%initialize(width=10, max_value=10._R8P, interactive=.false., output_unit=u)
call bar%start
call bar%suspend
do i = 1, 10
   call bar%update(current=real(i, R8P))
enddo
call bar%resume
text = capture_close(u, 'test_suspend_5.txt')
call check(lines_number(text) == 11, 'suspend in a log: nothing changes')

! log_interval: a line when that much time passed since the last one, also between two tens
u = capture_open('test_suspend_6.txt')
call bar%initialize(width=10, max_value=100._R8P, add_progress_percent=.true., interactive=.false., &
                    log_interval=0.001_R8P, output_unit=u)
call bar%start
do i = 1, 3
   call wait
   call bar%update(current=real(i, R8P))
enddo
call bar%update(current=4._R8P) ! too soon after the line of 3%
text = capture_close(u, 'test_suspend_6.txt')
call check(lines_number(text) == 4, 'log_interval: 0%, then 1%, 2%, 3%, not 4%')
call check(line(text, -1) == '----------   3%', 'log_interval: the line of 3%')

! log_interval with an indeterminate bar: its only lines between the start and the end
u = capture_open('test_suspend_7.txt')
call bar%initialize(width=4, indeterminate=.true., add_progress_count=.true., interactive=.false., &
                    log_interval=0.001_R8P, output_unit=u)
call bar%start
do i = 1, 3
   call wait
   call bar%update(current=real(i, R8P))
enddo
call bar%finish
text = capture_close(u, 'test_suspend_7.txt')
call check(lines_number(text) == 5, 'log_interval, indeterminate: the start, three lines, the end')
call check(line(text, 3) == '---- 2', 'log_interval, indeterminate: an empty track and the count')
call check(line(text, -1) == '**** 3', 'log_interval, indeterminate: the end')

call report
contains
   subroutine wait()
   !< Wait 3 ms, longer than the log_interval of the tests.
   integer(I8P) :: start !< Clock at the start.
   integer(I8P) :: now   !< Clock now.
   integer(I8P) :: rate  !< Clock counts per second.

   call system_clock(start, rate)
   do
      call system_clock(now)
      if (real(now - start, R8P) / real(rate, R8P) >= 0.003_R8P) exit
   enddo
   endsubroutine wait
endprogram forbear_test_suspend
