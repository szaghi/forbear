!< **forbear** test: the control sequences of a bar on a terminal: frames, end, write, position.

program forbear_test_terminal
!< **forbear** test: the control sequences of a bar on a terminal: frames, end, write, position.
use, intrinsic :: iso_fortran_env, only : I4P=>int32, R8P=>real64
use forbear, only : bar_object
use forbear_test_tools, only : capture_close, capture_open, check, count_text, CR, ESC, LF, report
implicit none
type(bar_object)              :: bar  !< Bar under test.
type(bar_object)              :: copy !< Copy of the bar.
character(len=:), allocatable :: text !< What the bar wrote.
integer(I4P)                  :: u    !< Capture unit.
integer(I4P)                  :: n    !< Length of the text.

! frames: hide the cursor, draw, erase the rest of the line, carriage return; the end shows the cursor
u = capture_open('test_terminal_1.txt')
call bar%initialize(width=4, prefix_string='p', prefix_color_fg='red', interactive=.true., min_interval=0._R8P, &
                    max_value=2._R8P, output_unit=u)
call bar%start
call check(bar%is_stdout_locked(), 'terminal: locked while running')
call bar%write('note')
call bar%update(current=1._R8P, message='a long message')
call bar%update(current=2._R8P, message='short')
call check(.not.bar%is_stdout_locked(), 'terminal: free after 100%')
text = capture_close(u, 'test_terminal_1.txt')
n = len(text)
call check(count_text(text, ESC//'[?25l') == 4, 'frames: the cursor is hidden at every drawing')
call check(count_text(text, ESC//'[K'//CR) == 4, 'frames: start, the redraw after write, two updates')
call check(index(text, ESC//'[31m') > 0, 'terminal: colours')
call check(index(text, CR//ESC//'[2K'//'note'//LF//ESC//'[?25l') > 0, 'write: clear the line, write, draw again')
call check(index(text, 'short'//ESC//'[K'//CR) > 0, 'message: erased to the end of the line')
! closing the capture ends the last record (ESC[J is written without advancing): one line feed may follow
call check(index(text, ESC//'[?25h'//LF//ESC//'[J') >= n - 10, 'end: show the cursor, next line, clear below')

! a bar at position 1: drawn one line below, cleared when complete
u = capture_open('test_terminal_2.txt')
call bar%initialize(width=4, interactive=.true., min_interval=0._R8P, position=1, max_value=1._R8P, output_unit=u)
call bar%start
call bar%update(current=1._R8P)
text = capture_close(u, 'test_terminal_2.txt')
call check(count_text(text, ESC//'[?25l'//LF) == 2, 'position 1: every drawing goes one line down')
call check(count_text(text, ESC//'[K'//CR//ESC//'[1A') == 2, 'position 1: and comes back up')
call check(index(text, LF//ESC//'[2K'//CR//ESC//'[1A') > 0, 'position 1: cleared when complete')
call check(index(text, ESC//'[?25h') == 0, 'position 1: the cursor stays hidden, the outer bar shows it')

! the cursor left visible: no hide, no show, the end still goes to the next line
u = capture_open('test_terminal_3.txt')
call bar%initialize(width=4, interactive=.true., min_interval=0._R8P, hide_cursor=.false., max_value=1._R8P, &
                    output_unit=u)
call bar%start
call bar%update(current=1._R8P)
text = capture_close(u, 'test_terminal_3.txt')
n = len(text)
call check(index(text, ESC//'[?25') == 0, 'hide_cursor=.false.: the cursor is neither hidden nor shown')
call check(count_text(text, ESC//'[K'//CR) == 2, 'hide_cursor=.false.: still two drawings')
call check(index(text, CR//LF//ESC//'[J') >= n - 5, 'hide_cursor=.false.: the end goes to the next line')

! the percent is always apart from what precedes it, also at 100%
u = capture_open('test_terminal_4.txt')
call bar%initialize(width=0, spinner_string='|', add_progress_percent=.true., interactive=.true., &
                    min_interval=0._R8P, max_value=1._R8P, output_unit=u)
call bar%start
call bar%update(current=1._R8P)
text = capture_close(u, 'test_terminal_4.txt')
call check(index(text, '/ 100%') > 0, 'percent: a space between the spinner and 100%')

! a copy is an independent bar with the same settings
call bar%initialize(width=7, interactive=.false., max_value=3._R8P, output_unit=99)
copy = bar
call bar%initialize(width=1)
call check(copy%width == 7 .and. copy%max_value == 3._R8P .and. copy%output_unit == 99, 'copy: settings kept')

call report
endprogram forbear_test_terminal
