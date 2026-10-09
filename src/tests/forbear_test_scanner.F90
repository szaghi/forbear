!< **forbear** test: the pulse trail of an indeterminate bar, `pulse_trail`: a one-cell head and the cells it left.

program forbear_test_scanner
!< **forbear** test: the pulse trail of an indeterminate bar, `pulse_trail`: a one-cell head and the cells it left.
use, intrinsic :: iso_fortran_env, only : I4P=>int32, R8P=>real64
use forbear, only : bar_object
use forbear_test_tools, only : capture_close, capture_open, check, ESC, line, report
implicit none
type(bar_object)              :: bar    !< Bar under test.
character(len=:), allocatable :: text   !< What the bar wrote.
character(len=:), allocatable :: head   !< The head, in the filled colour.
character(len=:), allocatable :: red    !< One drawing back.
character(len=:), allocatable :: yellow !< Two drawings back.
integer(I4P)                  :: u      !< Capture unit.
integer(I4P)                  :: i      !< Counter.

head   = ESC//'[34m#'//ESC//'[0m'
red    = ESC//'[31m#'//ESC//'[0m'
yellow = ESC//'[38;2;255;176;0m#'//ESC//'[0m'

! 5 cells: the head goes 1 2 3 4 5 4 3 ..., one cell per drawing (start is the first); the trail is where it was
u = capture_open('test_scanner_1.txt')
call bar%initialize(width=5, indeterminate=.true., filled_char_string='#', filled_char_color_fg='blue', &
                    empty_char_string='-', pulse_trail='red #FFB000', interactive=.true., min_interval=0._R8P, &
                    bracket_left_string='[', bracket_right_string=']', output_unit=u)
call bar%start
do i = 1, 7
   call bar%update(current=real(i, R8P))
enddo
call bar%finish
text = capture_close(u, 'test_scanner_1.txt')
call check(index(text, '['//head//'----]') > 0, 'scanner: the first drawing, the head alone')
call check(index(text, '['//yellow//red//head//'--]') > 0, 'scanner: the third drawing, a trail of two behind the head')
call check(index(text, '['//'---'//head//red//']') > 0, 'scanner: back from the end, the trail folds under the head')
call check(index(text, '['//'--'//head//red//yellow//']') > 0, 'scanner: going back, the trail follows')
call check(index(text, '['//repeat(head, 5)//']') > 0, 'scanner: finished, the body full')

! a log has no trail: the empty track, then the full body
u = capture_open('test_scanner_2.txt')
call bar%initialize(width=5, indeterminate=.true., filled_char_string='#', empty_char_string='-', pulse_trail='red', &
                    interactive=.false., output_unit=u)
call bar%start
call bar%update(current=3._R8P)
call bar%finish
text = capture_close(u, 'test_scanner_2.txt')
call check(line(text, 1) == '-----', 'scanner in a log: the empty track')
call check(line(text, -1) == '#####', 'scanner in a log: full at the end')

call report
endprogram forbear_test_scanner
