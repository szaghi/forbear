!< **forbear** test: layout templates and the fields of a program.

module forbear_test_template_fields
!< Fields of a program, for the template test.
use, intrinsic :: iso_fortran_env, only : R8P=>real64
use forbear, only : field_object, progress_object
implicit none
private
public :: steps_field
public :: value_field

type, extends(field_object) :: steps_field
   !< The current step and the steps, as words.
   contains
      procedure, pass(self) :: render => render_steps !< Return the text of the field.
endtype steps_field

type, extends(field_object) :: value_field
   !< A value of the program, read through a pointer at every drawing.
   real(R8P), pointer :: value => null() !< Value.
   contains
      procedure, pass(self) :: render => render_value !< Return the text of the field.
endtype value_field

contains
   function render_steps(self, progress) result(text)
   !< Return the current step and the steps.
   class(steps_field),    intent(in) :: self     !< Field.
   type(progress_object), intent(in) :: progress !< Progress of the bar.
   character(len=:), allocatable     :: text     !< Text of the field.
   character(len=32)                 :: buffer   !< Buffer.

   write(buffer, '(A,I2,A,I2)') 'step ', nint(progress%current), ' of ', nint(progress%max_value)
   text = trim(buffer)
   endfunction render_steps

   function render_value(self, progress) result(text)
   !< Return the value, in a fixed width.
   class(value_field),    intent(in) :: self     !< Field.
   type(progress_object), intent(in) :: progress !< Progress of the bar.
   character(len=:), allocatable     :: text     !< Text of the field.
   character(len=9)                  :: buffer   !< Buffer.

   write(buffer, '(ES9.2)') self%value
   text = buffer
   endfunction render_value
endmodule forbear_test_template_fields

program forbear_test_template
!< **forbear** test: layout templates and the fields of a program.
use, intrinsic :: iso_fortran_env, only : I4P=>int32, R8P=>real64
use forbear, only : bar_object
use forbear_test_template_fields, only : steps_field, value_field
use forbear_test_tools, only : capture_close, capture_open, check, ESC, line, lines_number, report
implicit none
type(bar_object)              :: bar      !< Bar under test.
type(steps_field)             :: steps    !< A field of the program.
type(value_field)             :: residual !< A field of the program, reading a value of it.
real(R8P), target             :: r        !< The value read by the field.
character(len=:), allocatable :: text     !< What the bar wrote.
integer(I4P)                  :: u        !< Capture unit.
integer(I4P)                  :: i        !< Counter.

! fields, literal text, the width of the bar from the template
u = capture_open('test_template_1.txt')
call bar%initialize(template='p [{bar:10}] {percent} {count}', interactive=.false., max_value=10._R8P, output_unit=u)
call run(10)
text = capture_close(u, 'test_template_1.txt')
call check(lines_number(text) == 11, 'template: a line every 10%')
call check(line(text, 1) == 'p [----------]   0%  0/10', 'template: fields without separators of their own')
call check(line(text, 6) == 'p [*****-----]  50%  5/10', 'template: 50%')
call check(line(text, -1) == 'p [**********] 100% 10/10', 'template: 100%')

! the words around the times are the template's; escaped braces
u = capture_open('test_template_2.txt')
call bar%initialize(template='{{{percent}}} left {eta} after {elapsed}', interactive=.false., max_value=2._R8P, &
                    output_unit=u)
call run(2)
text = capture_close(u, 'test_template_2.txt')
call check(line(text, 1) == '{  0%} left --:--:-- after 00:00:00', 'template: escaped braces, ETA unknown at the start')
call check(index(line(text, -1), '{100%} left 00:00:00') == 1, 'template: ETA 0 at the end')

! the message, without separators: an empty message is empty
u = capture_open('test_template_3.txt')
call bar%initialize(template='[{bar:4}]{message}|', interactive=.false., max_value=2._R8P, output_unit=u)
call bar%start
call bar%update(current=1._R8P, message='hi')
call bar%update(current=2._R8P)
text = capture_close(u, 'test_template_3.txt')
call check(line(text, 1) == '[----]|', 'template: no message yet')
call check(line(text, 2) == '[**--]hi|', 'template: the message, as it is')
call check(line(text, 3) == '[****]hi|', 'template: the message kept')

! fields of the program: one computed from the progress, one reading a value of the program
u = capture_open('test_template_4.txt')
call bar%initialize(template='{steps}: {residual}', interactive=.false., max_value=10._R8P, output_unit=u)
call bar%add_field('steps', steps)
residual%value => r
call bar%add_field('residual', residual)
r = 1._R8P
call bar%start
do i = 1, 10
   r = r / 10._R8P
   call bar%update(current=real(i, R8P))
enddo
text = capture_close(u, 'test_template_4.txt')
call check(line(text, 1) == 'step  0 of 10:  1.00E+00', 'program fields: at the start')
call check(line(text, 6) == 'step  5 of 10:  1.00E-05', 'program fields: the value of the program, at every drawing')

! colours, background and style of a field, on a terminal
u = capture_open('test_template_5.txt')
call bar%initialize(template='{percent:yellow,on_blue,bold_on}', interactive=.true., min_interval=0._R8P, &
                    max_value=1._R8P, output_unit=u)
call run(1)
text = capture_close(u, 'test_template_5.txt')
call check(index(text, ESC//'[33m') > 0 .and. index(text, ESC//'[44m') > 0 .and. index(text, ESC//'[1m') > 0, &
           'template: foreground, background and style of a field')

! the scale above the bar body; the summary counts in the units of the range with {count}
u = capture_open('test_template_6.txt')
call bar%initialize(template='ab [{bar:22}] {count}', add_scale_bar=.true., add_summary=.true., interactive=.false., &
                    max_value=10._R8P, output_unit=u)
call run(10)
text = capture_close(u, 'test_template_6.txt')
call check(line(text, 1) == '     0.00 (min)(max) 10.00', 'template: the scale above the bar body')
call check(index(line(text, -1), '%/s]') == 0 .and. index(line(text, -1), '/s]') > 0, 'template: summary in units')

call report
contains
   subroutine run(steps_number)
   !< Run the bar over its steps.
   integer(I4P), intent(in) :: steps_number !< Steps.
   integer(I4P)             :: s            !< Counter.

   call bar%start
   do s = 1, steps_number
      call bar%update(current=real(s, R8P))
   enddo
   endsubroutine run
endprogram forbear_test_template
