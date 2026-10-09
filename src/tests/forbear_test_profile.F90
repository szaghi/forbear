!< **forbear** test: the profile of the bar body, `bar_profile='ramp'`: blocks rising along the body, lit or unlit.

program forbear_test_profile
!< **forbear** test: the profile of the bar body, `bar_profile='ramp'`: blocks rising along the body, lit or unlit.
use, intrinsic :: iso_fortran_env, only : I4P=>int32, R8P=>real64
use forbear, only : bar_object
use forbear_test_tools, only : capture_close, capture_open, check, ESC, line, report
implicit none
type(bar_object)              :: bar  !< Bar under test.
character(len=:), allocatable :: text !< What the bar wrote.
integer(I4P)                  :: u    !< Capture unit.

! 8 cells rise one eighth each; half done: four lit in the filled colour, four unlit in black_intense, the default
u = capture_open('test_profile_1.txt')
call bar%initialize(width=8, bar_profile='ramp', filled_char_color_fg='green', &
                    interactive=.true., min_interval=0._R8P, max_value=1._R8P, output_unit=u)
call bar%start
call bar%update(current=0.5_R8P)
text = capture_close(u, 'test_profile_1.txt')
call check(index(text, cell('32', '▁')//cell('32', '▂')//cell('32', '▃')//cell('32', '▄')// &
                       cell('90', '▅')//cell('90', '▆')//cell('90', '▇')//cell('90', '█')) > 0, &
           'ramp: rising blocks, lit in the filled colour, unlit in black_intense')

! the zones colour the lit blocks; an empty colour given is kept
u = capture_open('test_profile_2.txt')
call bar%initialize(width=8, bar_profile='ramp', bar_zones='0.5:blue 1:#FF3B30', empty_char_color_fg='red', &
                    interactive=.true., min_interval=0._R8P, max_value=1._R8P, output_unit=u)
call bar%start
call bar%update(current=0.75_R8P)
text = capture_close(u, 'test_profile_2.txt')
call check(index(text, cell('34', '▁')//cell('34', '▂')//cell('34', '▃')//cell('34', '▄')// &
                       cell('38;2;255;59;48', '▅')//cell('38;2;255;59;48', '▆')//cell('31', '▇')//cell('31', '█')) > 0, &
           'ramp: lit blocks in the colours of their zones, unlit ones in the empty colour given')

! a log has no colours: unlit cells are blank; 4 cells rise 1, 3, 5, 8 eighths
u = capture_open('test_profile_3.txt')
call bar%initialize(width=4, bar_profile='ramp', interactive=.false., max_value=1._R8P, output_unit=u)
call bar%start
call bar%update(current=0.5_R8P)
call bar%update(current=1._R8P)
text = capture_close(u, 'test_profile_3.txt')
call check(line(text, 1) == '    ', 'ramp in a log: nothing lit at the start')
call check(line(text, 2) == '▁▃  ', 'ramp in a log: half done, the unlit cells blank')
call check(line(text, 3) == '▁▃▅█', 'ramp in a log: done')

! one cell: the full block; flat stays the default
u = capture_open('test_profile_4.txt')
call bar%initialize(width=1, bar_profile='ramp', interactive=.false., max_value=1._R8P, output_unit=u)
call bar%start
call bar%update(current=1._R8P)
text = capture_close(u, 'test_profile_4.txt')
call check(line(text, -1) == '█', 'ramp of one cell: the full block')
u = capture_open('test_profile_5.txt')
call bar%initialize(width=4, bar_profile='flat', interactive=.false., max_value=1._R8P, output_unit=u)
call bar%start
call bar%update(current=1._R8P)
text = capture_close(u, 'test_profile_5.txt')
call check(line(text, -1) == '****', 'flat: the filled string')

call report

contains
   pure function cell(code, glyph) result(text)
   !< Return a cell in the colour of an SGR code.
   character(len=*), intent(in)  :: code  !< SGR code.
   character(len=*), intent(in)  :: glyph !< Glyph.
   character(len=:), allocatable :: text  !< Cell.

   text = ESC//'['//code//'m'//glyph//ESC//'[0m'
   endfunction cell
endprogram forbear_test_profile
