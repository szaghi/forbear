!< **forbear** test: seven-segment digits (`digits`, `digits_unlit_color`) and dashboard themes (`theme`).

program forbear_test_dashboard
!< **forbear** test: seven-segment digits (`digits`, `digits_unlit_color`) and dashboard themes (`theme`).
use, intrinsic :: iso_fortran_env, only : I4P=>int32, R8P=>real64
use forbear, only : bar_object
use forbear_test_tools, only : capture_close, capture_open, check, ESC, line, report
implicit none
type(bar_object)              :: bar  !< Bar under test.
character(len=:), allocatable :: text !< What the bar wrote.
integer(I4P)                  :: u    !< Capture unit.

! segment digits: the digits of the percent and the count; without digits_unlit_color the padding stays blank
u = capture_open('test_dashboard_1.txt')
call bar%initialize(width=2, add_progress_percent=.true., progress_percent_color_fg='yellow', add_progress_count=.true., &
                    digits='segment', interactive=.true., min_interval=0._R8P, max_value=100._R8P, output_unit=u)
call bar%start
call bar%update(current=7._R8P)
call bar%update(current=50._R8P)
text = capture_close(u, 'test_dashboard_1.txt')
call check(index(text, ESC//'[33m 🯵🯰%'//ESC//'[0m') == 0 .and. index(text, ESC//'[33m🯵🯰%'//ESC//'[0m') > 0, &
           'segment digits: the percent, its padding apart')
call check(index(text, ESC//'[33m '//ESC//'[0m'//ESC//'[33m🯵🯰%'//ESC//'[0m') > 0, 'segment digits: padding blank')
call check(index(text, '🯷/🯱🯰🯰') > 0, 'segment digits: the count, its slash as it is')

! unlit 8s pad the numbers in digits_unlit_color; the elapsed time of a template too
u = capture_open('test_dashboard_2.txt')
call bar%initialize(template='{percent:yellow} {elapsed}', digits='segment', digits_unlit_color='#3A2800', &
                    interactive=.true., min_interval=0._R8P, max_value=100._R8P, output_unit=u)
call bar%start
call bar%update(current=5._R8P)
text = capture_close(u, 'test_dashboard_2.txt')
call check(index(text, ESC//'[38;2;58;40;0m🯸🯸'//ESC//'[0m'//ESC//'[33m🯵%'//ESC//'[0m') > 0, &
           'segment digits: the padding as unlit 8s')
call check(index(text, '🯰🯰:🯰🯰:🯰🯰') > 0, 'segment digits: the elapsed time')

! a log keeps plain digits
u = capture_open('test_dashboard_3.txt')
call bar%initialize(width=2, add_progress_percent=.true., digits='segment', digits_unlit_color='red', &
                    interactive=.false., max_value=1._R8P, output_unit=u)
call bar%start
call bar%update(current=1._R8P)
text = capture_close(u, 'test_dashboard_3.txt')
call check(line(text, -1) == '** 100%', 'segment digits in a log: plain digits')

! theme vfd: segments, lit and unlit, a bold lit prefix, numbers in the accent; a keyword passed wins
u = capture_open('test_dashboard_4.txt')
call bar%initialize(width=2, theme='vfd', prefix_string='p', add_progress_percent=.true., &
                    interactive=.true., min_interval=0._R8P, max_value=1._R8P, output_unit=u)
call bar%start
call bar%update(current=0.5_R8P)
text = capture_close(u, 'test_dashboard_4.txt')
call check(index(text, ESC//'[1m'//ESC//'[38;2;46;245;192mp'//ESC//'[0m'//ESC//'[0m') > 0, 'theme: the prefix')
call check(index(text, ESC//'[38;2;46;245;192m▌'//ESC//'[0m'//ESC//'[38;2;14;52;44m▌'//ESC//'[0m') > 0, &
           'theme: lit and unlit segments')
call check(index(text, ESC//'[38;2;255;176;0m 50%') > 0, 'theme: the percent in the accent')
u = capture_open('test_dashboard_5.txt')
call bar%initialize(width=2, theme='vfd', filled_char_color_fg='red', filled_char_string='#', digits='segment', &
                    add_progress_percent=.true., interactive=.true., min_interval=0._R8P, max_value=1._R8P, output_unit=u)
call bar%start
call bar%update(current=0.05_R8P)
text = capture_close(u, 'test_dashboard_5.txt')
call check(index(text, ESC//'[38;2;14;52;44m▌'//ESC//'[0m') > 0 .and. index(text, ESC//'[31m#') == 0, &
           'theme: the unlit segments kept')
call check(index(text, ESC//'[38;2;58;40;0m🯸🯸'//ESC//'[0m') > 0, 'theme: the unlit 8s of the numbers')
u = capture_open('test_dashboard_6.txt')
call bar%initialize(width=2, theme='vfd', filled_char_color_fg='red', filled_char_string='#', &
                    interactive=.true., min_interval=0._R8P, max_value=1._R8P, output_unit=u)
call bar%start
call bar%update(current=1._R8P)
text = capture_close(u, 'test_dashboard_6.txt')
call check(index(text, ESC//'[31m#'//ESC//'[0m') > 0, 'theme: a keyword passed wins')

! theme kitt on an indeterminate bar: its own pulse trail
u = capture_open('test_dashboard_7.txt')
call bar%initialize(width=4, theme='kitt', indeterminate=.true., add_progress_count=.true., digits='segment', &
                    interactive=.true., min_interval=0._R8P, output_unit=u)
call bar%start
call bar%update(current=1._R8P)
text = capture_close(u, 'test_dashboard_7.txt')
call check(index(text, ESC//'[38;2;192;40;30m▌'//ESC//'[0m'//ESC//'[38;2;255;59;48m▌'//ESC//'[0m') > 0, &
           'theme kitt: the trail behind the head')

call report
endprogram forbear_test_dashboard
