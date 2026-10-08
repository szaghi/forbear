!< **forbear** test: the number formats, fixed width: scale labels, speed, ETA, summary, date and time.

program forbear_test_formats
!< **forbear** test: the number formats, fixed width: scale labels, speed, ETA, summary, date and time.
use, intrinsic :: iso_fortran_env, only : I4P=>int32, R8P=>real64
use forbear, only : bar_object
use forbear_test_tools, only : capture_close, capture_open, check, line, report
implicit none
type(bar_object)              :: bar  !< Bar under test.
character(len=:), allocatable :: text !< What the bar wrote.
character(len=:), allocatable :: last !< A line.
integer(I4P)                  :: u    !< Capture unit.

! the scale labels: the most precise form that fits five characters
call check_scale(0._R8P, 50._R8P, ' 0.00 (min)', '(max) 50.00')
call check_scale(0._R8P, 100._R8P, ' 0.00 (min)', '(max) 100.0')
call check_scale(0._R8P, 1000._R8P, ' 0.00 (min)', '(max)  1000')
call check_scale(0._R8P, 123456._R8P, ' 0.00 (min)', '(max) 1.2e5')
call check_scale(0._R8P, 2.5e7_R8P, ' 0.00 (min)', '(max) 2.5e7')
call check_scale(0._R8P, 1.e300_R8P, ' 0.00 (min)', '(max) 1e300')
call check_scale(-1000._R8P, 0._R8P, '-1000 (min)', '(max)  0.00')
call check_scale(-1.e100_R8P, 0._R8P, '***** (min)', '(max)  0.00')

! a UTF-8 prefix: the scale is indented by its columns (6), not by its bytes (8)
u = capture_open('test_formats_prefix.txt')
call bar%initialize(width=22, add_scale_bar=.true., prefix_string='Größe ', interactive=.false., output_unit=u)
call bar%start
text = capture_close(u, 'test_formats_prefix.txt')
call check(line(text, 1) == repeat(' ', 6)//' 0.00 (min)(max)  1.00', 'UTF-8 prefix: the scale indented by 6 columns')
call check(index(line(text, 2), 'Größe ') == 1, 'UTF-8 prefix: the bar line starts with the prefix')

! speed and ETA: unknown at the first drawing, 0 left at the last; summary, date and time
u = capture_open('test_formats.txt')
call bar%initialize(width=10, add_progress_speed=.true., add_eta=.true., add_summary=.true., add_date_time=.true., &
                    interactive=.false., max_value=2._R8P, output_unit=u)
call bar%start
call bar%update(current=1._R8P)
call bar%update(current=2._R8P)
text = capture_close(u, 'test_formats.txt')
call check(line(text, 1) == '---------- (  0.00%/s) ETA --:--:--', 'first line: no speed, no ETA yet')
last = line(text, 3)
call check(last(len(last) - 12:) == ' ETA 00:00:00', 'last bar line: nothing left')
last = line(text, 4)
call check(len(last) == 43 .and. last(1:1) == '[' .and. last(21:23) == ' - ' .and. last(43:43) == ']', &
           'date and time: [yyyy/mm/dd hh:mm:ss - yyyy/mm/dd hh:mm:ss]')
last = line(text, 5)
call check(index(last, '[done in ') == 1 .and. index(last, ' s, ') > 0 .and. index(last, '%/s]') == len(last) - 3, &
           'summary: [done in <seconds> s, <speed>%/s]')

call report
contains
   subroutine check_scale(min_value, max_value, min_label, max_label)
   !< Check the labels of the scale of a bar.
   real(R8P),        intent(in)  :: min_value !< Minimum value.
   real(R8P),        intent(in)  :: max_value !< Maximum value.
   character(len=*), intent(in)  :: min_label !< Expected label of the minimum.
   character(len=*), intent(in)  :: max_label !< Expected label of the maximum.
   character(len=:), allocatable :: scale     !< Scale line.

   u = capture_open('test_formats_scale.txt')
   call bar%initialize(width=22, add_scale_bar=.true., interactive=.false., min_value=min_value, max_value=max_value, &
                       output_unit=u)
   call bar%start
   text = capture_close(u, 'test_formats_scale.txt')
   scale = line(text, 1)
   call check(len(scale) == 22 .and. scale(1:11) == min_label .and. scale(12:22) == max_label, &
              'scale "'//min_label//'"..."'//max_label//'", found "'//scale//'"')
   endsubroutine check_scale
endprogram forbear_test_formats
