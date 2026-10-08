!< **forbear** test: finish, and the indeterminate bars it ends.

module forbear_test_finish_fields
!< A field of a program, for the indeterminate test.
use forbear, only : field_object, progress_object
implicit none
private
public :: done_field

type, extends(field_object) :: done_field
   !< What the bar knows of an indeterminate progress.
   contains
      procedure, pass(self) :: render => render_done !< Return the text of the field.
endtype done_field

contains
   function render_done(self, progress) result(text)
   !< Return the current value, and whether the progress is indeterminate.
   class(done_field),     intent(in) :: self     !< Field.
   type(progress_object), intent(in) :: progress !< Progress of the bar.
   character(len=:), allocatable     :: text     !< Text of the field.
   character(len=32)                 :: buffer   !< Buffer.

   write(buffer, '(I0,A,L1,A,I0)') nint(progress%current), ' ', progress%indeterminate, ' ', progress%percent
   text = trim(buffer)
   endfunction render_done
endmodule forbear_test_finish_fields

program forbear_test_finish
!< **forbear** test: finish, and the indeterminate bars it ends.
use, intrinsic :: iso_fortran_env, only : I4P=>int32, R8P=>real64
use forbear, only : bar_object
use forbear_test_finish_fields, only : done_field
use forbear_test_tools, only : capture_close, capture_open, check, count_text, CR, ESC, line, lines_number, report
implicit none
type(bar_object)              :: bar  !< Bar under test.
type(done_field)              :: done !< Field of the program.
character(len=:), allocatable :: text !< What the bar wrote.
integer(I4P)                  :: u    !< Capture unit.
integer(I4P)                  :: i    !< Counter.

! a loop left at 65%: finish logs where the bar stopped, and ends it
u = capture_open('test_finish_1.txt')
call bar%initialize(width=10, max_value=10._R8P, add_progress_percent=.true., add_progress_count=.true., &
                    interactive=.false., output_unit=u)
call bar%finish ! not started: nothing to end
call bar%start
do i = 1, 6
   call bar%update(current=real(i, R8P))
enddo
call bar%update(current=6.5_R8P)
call bar%finish
call bar%update(current=10._R8P)
call bar%finish
text = capture_close(u, 'test_finish_1.txt')
call check(lines_number(text) == 8, 'finish: 0% to 60%, then one line at 65%, then nothing')
call check(line(text, -1) == '*******---  65%  6/10', 'finish: the line of the last update')

! finished where the log already is: no second line
u = capture_open('test_finish_2.txt')
call bar%initialize(width=10, max_value=10._R8P, add_progress_percent=.true., interactive=.false., add_summary=.true., &
                    output_unit=u)
call bar%start
do i = 1, 6
   call bar%update(current=real(i, R8P))
enddo
call bar%finish
text = capture_close(u, 'test_finish_2.txt')
call check(lines_number(text) == 8, 'finish: 0% to 60%, and the summary')
call check(line(text, -2) == '******----  60%', 'finish: the 60% line, once')
call check(index(line(text, -1), '[done in ') == 1 .and. index(line(text, -1), '%/s]') > 0, 'finish: the summary')

! on a terminal: the last frame, then the end of the bar
u = capture_open('test_finish_3.txt')
call bar%initialize(width=10, max_value=10._R8P, add_progress_percent=.true., interactive=.true., min_interval=0._R8P, &
                    output_unit=u)
call bar%start
call bar%update(current=4._R8P)
call check(bar%is_stdout_locked(), 'finish: locked while running')
call bar%finish
call check(.not.bar%is_stdout_locked(), 'finish: free after finish')
text = capture_close(u, 'test_finish_3.txt')
call check(count_text(text, ESC//'[K'//CR) == 3, 'finish: start, update, the last frame')
call check(count_text(text, '****------  40%') == 2, 'finish: the last frame draws the last update')
call check(index(text, ESC//'[?25h') > 0, 'finish: the cursor shown again')

! indeterminate, in a log: the start and the end only, what is done and its rate
u = capture_open('test_finish_4.txt')
call bar%initialize(width=8, indeterminate=.true., add_progress_count=.true., add_progress_speed=.true., &
                    prefix_string='files ', interactive=.false., output_unit=u)
call bar%start
do i = 1, 5
   call bar%update(current=real(i, R8P))
enddo
call bar%finish(message='done')
text = capture_close(u, 'test_finish_4.txt')
call check(lines_number(text) == 2, 'indeterminate log: the start and the end')
call check(line(text, 1) == 'files -------- 0 (  0.00/s)', 'indeterminate log: an empty track at the start')
call check(index(line(text, 2), 'files ******** 5 (') == 1, 'indeterminate log: a full bar and the count at the end')
call check(index(line(text, 2), '/s) done') > 0, 'indeterminate log: what is done per second, the message')

! indeterminate, on a terminal: a block going back and forth, one cell per drawing; full at the end
u = capture_open('test_finish_5.txt')
call bar%initialize(width=8, indeterminate=.true., interactive=.true., min_interval=0._R8P, output_unit=u)
call bar%start
do i = 1, 14
   call bar%update(current=real(i, R8P))
enddo
call check(bar%is_stdout_locked(), 'indeterminate: never complete by itself')
call bar%finish
text = capture_close(u, 'test_finish_5.txt')
call check(count_text(text, ESC//'[K'//CR) == 16, 'indeterminate: start, 14 updates, finish')
call check(index(text, '**------') > 0 .and. index(text, '-**-----') > 0, 'indeterminate: the block moves right')
call check(count_text(text, '------**') == 1, 'indeterminate: up to the end of the track, once')
call check(count_text(text, '-----**-') == 2, 'indeterminate: and back')
call check(index(text, '********') > 0, 'indeterminate: full at the end')

! indeterminate, with a template and a field of the program: the count does not stop at max_value
u = capture_open('test_finish_6.txt')
call bar%initialize(template='{prefix}[{bar:6}] {count} items {n}', indeterminate=.true., prefix_string='scan ', &
                    interactive=.false., output_unit=u)
call bar%add_field('n', done)
call bar%start
call bar%update(current=1234._R8P)
call bar%finish
text = capture_close(u, 'test_finish_6.txt')
call check(line(text, -1) == 'scan [******] 1234 items 1234 T 0', 'indeterminate template: count and field')

call report
endprogram forbear_test_finish
