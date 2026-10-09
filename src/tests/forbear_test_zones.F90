!< **forbear** test: the colour zones of the bar body, `bar_zones`: a filled cell takes the colour of its position.

program forbear_test_zones
!< **forbear** test: the colour zones of the bar body, `bar_zones`: a filled cell takes the colour of its position.
use, intrinsic :: iso_fortran_env, only : I4P=>int32, R8P=>real64
use forbear, only : bar_object
use forbear_test_tools, only : capture_close, capture_open, check, ESC, line, report
implicit none
type(bar_object)              :: bar    !< Bar under test.
type(bar_object)              :: copy   !< Copy of the bar.
character(len=:), allocatable :: text   !< What the bar wrote.
character(len=:), allocatable :: blue   !< A blue cell.
character(len=:), allocatable :: yellow !< A yellow cell.
character(len=:), allocatable :: rgb    !< A 24-bit red cell.
character(len=:), allocatable :: green  !< A cell in the filled_char colour.
integer(I4P)                  :: u      !< Capture unit.

blue   = ESC//'[34m#'//ESC//'[0m'
yellow = ESC//'[33m#'//ESC//'[0m'
rgb    = ESC//'[38;2;255;59;48m#'//ESC//'[0m'
green  = ESC//'[32m#'//ESC//'[0m'

! three zones over 10 cells: cells 1-5 blue, 6-8 yellow, 9-10 #FF3B30; the empty cells keep their own colours
u = capture_open('test_zones_1.txt')
call bar%initialize(width=10, filled_char_string='#', empty_char_string='-', bar_zones='0.5:blue 0.8:Yellow  1:#FF3B30', &
                    interactive=.true., min_interval=0._R8P, max_value=1._R8P, output_unit=u)
call bar%start
call bar%update(current=0.5_R8P)
call bar%update(current=1._R8P)
text = capture_close(u, 'test_zones_1.txt')
call check(index(text, repeat(blue, 5)//'-----') > 0, 'zones: half done, the lit cells all in the first zone')
call check(index(text, repeat(blue, 5)//repeat(yellow, 3)//repeat(rgb, 2)) > 0, &
           'zones: done, each cell in the colour of its position, names in any case, #rrggbb')

! zones not reaching the end: the cells beyond the last limit keep the filled_char colour
u = capture_open('test_zones_2.txt')
call bar%initialize(width=10, filled_char_string='#', filled_char_color_fg='green', bar_zones='0.3:blue', &
                    interactive=.true., min_interval=0._R8P, max_value=1._R8P, output_unit=u)
call bar%start
call bar%update(current=1._R8P)
text = capture_close(u, 'test_zones_2.txt')
call check(index(text, repeat(blue, 3)//repeat(green, 7)) > 0, 'zones: beyond the last limit, the filled_char colour')

! partial blocks: the partial cell takes the colour of its zone too
u = capture_open('test_zones_3.txt')
call bar%initialize(width=4, partial_blocks=.true., bar_zones='0.5:blue 1:yellow', &
                    interactive=.true., min_interval=0._R8P, max_value=1._R8P, output_unit=u)
call bar%start
call bar%update(current=0.6_R8P) ! 2.4 cells: two full blue ones, 3/8 of a yellow one
text = capture_close(u, 'test_zones_3.txt')
call check(index(text, repeat(ESC//'[34m█'//ESC//'[0m', 2)//ESC//'[33m▍'//ESC//'[0m') > 0, &
           'zones: the partial block in the colour of its zone')

! a copy draws the same zones; an indeterminate bar, once finished, is full and coloured by position
u = capture_open('test_zones_4.txt')
call bar%initialize(width=8, filled_char_string='#', bar_zones='0.5:blue 1:yellow', indeterminate=.true., &
                    interactive=.true., min_interval=0._R8P, output_unit=u)
copy = bar
call copy%start
call copy%update(current=3._R8P)
call copy%finish
text = capture_close(u, 'test_zones_4.txt')
call check(index(text, repeat(blue, 4)//repeat(yellow, 4)) > 0, 'zones: a copy, indeterminate and finished, by position')

! a log has no colours: the zones change nothing
u = capture_open('test_zones_5.txt')
call bar%initialize(width=4, filled_char_string='#', bar_zones='0.5:blue 1:yellow', interactive=.false., &
                    max_value=1._R8P, output_unit=u)
call bar%start
call bar%update(current=1._R8P)
text = capture_close(u, 'test_zones_5.txt')
call check(line(text, 2) == '####', 'zones: a log has no colours')
call check(index(text, ESC) == 0, 'zones: a log has no control sequences')

call report
endprogram forbear_test_zones
