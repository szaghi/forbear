!< **forbear** test: the elements of a bar in a log: partial blocks, count, message, write, disabled bars.

program forbear_test_elements
!< **forbear** test: the elements of a bar in a log: partial blocks, count, message, write, disabled bars.
use, intrinsic :: iso_fortran_env, only : I4P=>int32, R8P=>real64
use forbear, only : bar_object
use forbear_test_tools, only : capture_close, capture_open, check, count_text, ESC, line, lines_number, report
implicit none
type(bar_object)              :: bar  !< Bar under test.
character(len=:), allocatable :: text !< What the bar wrote.
integer(I4P)                  :: u    !< Capture unit.
integer(I4P)                  :: i    !< Counter.

! partial blocks: eight steps per cell; a log has no colours
u = capture_open('test_elements_1.txt')
call bar%initialize(width=8, partial_blocks=.true., bracket_left_string='[', bracket_right_string=']',       &
                    filled_char_color_fg='red', prefix_string='p ', prefix_color_fg='blue', spinner_string='|', &
                    interactive=.false., max_value=10._R8P, output_unit=u)
call run(10)
text = capture_close(u, 'test_elements_1.txt')
call check(line(text, 2) == 'p [▊       ]', 'partial blocks: 10% of 8 cells is 6/8 of a cell')
call check(line(text, 6) == 'p [████    ]', 'partial blocks: 50% of 8 cells is 4 cells')
call check(line(text, -1) == 'p [████████]', 'partial blocks: 100% is full')
call check(index(text, ESC) == 0, 'a log has no control sequences, no colours')
call check(index(text, '|') == 0, 'a log has no spinner')

! the count, right-aligned to the width of the maximum; the message, until the next one
u = capture_open('test_elements_2.txt')
call bar%initialize(width=4, add_progress_count=.true., interactive=.false., max_value=100._R8P, output_unit=u)
call bar%start
do i = 1, 100
   if (i == 30) then
      call bar%update(current=real(i, R8P), message='hello')
   else
      call bar%update(current=real(i, R8P))
   endif
enddo
text = capture_close(u, 'test_elements_2.txt')
call check(line(text, 2) == '----  10/100', 'count: right-aligned to the width of the maximum')
call check(line(text, 4) == '*---  30/100 hello', 'message: at the end of the line')
call check(line(text, 5) == '**--  40/100 hello', 'message: kept until the next one')

! write in a log: a line among the lines of the bar; a disabled bar draws nothing, but writes
u = capture_open('test_elements_3.txt')
call bar%initialize(width=4, interactive=.false., max_value=2._R8P, output_unit=u)
call bar%start
call bar%write('note')
call bar%update(current=2._R8P)
call bar%initialize(width=4, interactive=.false., disabled=.true., output_unit=u)
call run(2)
call bar%write('disabled but written')
call bar%initialize(width=4, interactive=.false., position=1, output_unit=u)
call run(2)
text = capture_close(u, 'test_elements_3.txt')
call check(lines_number(text) == 4, 'log: bar, note, bar, then only the line written by the disabled bar')
call check(line(text, 2) == 'note', 'write: a line among the lines of the bar')
call check(line(text, -1) == 'disabled but written', 'disabled bar: nothing but its writes; a log has no position 1')
call check(count_text(text, '----') == 1, 'disabled bar and bar at position 1: no lines of theirs')

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
endprogram forbear_test_elements
