!< **forbear** tests, tools: capture what a bar writes, check conditions.

module forbear_test_tools
!< **forbear** tests, tools: capture what a bar writes, check conditions.
!<
!< A test passes a capture unit to the bars it checks (`output_unit=`), reads back every byte written, and checks it;
!< `report` ends the test with `error stop` if any check failed, so that `scripts/run_tests.sh` counts it as failed.
use, intrinsic :: iso_fortran_env, only : I4P=>int32, I8P=>int64, error_unit
implicit none
private
public :: capture_close
public :: capture_open
public :: check
public :: count_text
public :: ESC, CR, LF
public :: line
public :: lines_number
public :: report

character(len=1), parameter :: ESC = achar(27) !< Escape.
character(len=1), parameter :: CR  = achar(13) !< Carriage return.
character(len=1), parameter :: LF  = achar(10) !< Line feed.
integer(I4P)                :: failures = 0    !< Number of failed checks.

contains
   function capture_open(file) result(unit)
   !< Open a capture file, return its unit.
   character(len=*), intent(in) :: file !< Capture file.
   integer(I4P)                 :: unit !< Unit of the capture.

   open(newunit=unit, file=file, access='stream', form='formatted', status='replace', action='write')
   endfunction capture_open

   function capture_close(unit, file) result(text)
   !< Close a capture and return every byte written to it; the file is deleted.
   integer(I4P),     intent(in)  :: unit   !< Unit of the capture.
   character(len=*), intent(in)  :: file   !< Capture file.
   character(len=:), allocatable :: text   !< Bytes written.
   integer(I8P)                  :: bytes  !< Size of the capture.
   integer(I4P)                  :: reader !< Unit reading the capture.

   close(unit)
   open(newunit=reader, file=file, access='stream', form='unformatted', status='old', action='read')
   inquire(unit=reader, size=bytes)
   allocate(character(len=bytes) :: text)
   if (bytes > 0) read(reader) text
   close(reader, status='delete')
   endfunction capture_close

   subroutine check(condition, label)
   !< Check a condition: on failure, print its label on standard error and count it.
   logical,          intent(in) :: condition !< Condition that must hold.
   character(len=*), intent(in) :: label     !< What is checked.

   if (.not.condition) then
      failures = failures + 1
      write(error_unit, '(A)') 'FAIL: '//label
   endif
   endsubroutine check

   pure function count_text(text, pattern) result(n)
   !< Return the number of occurrences, not overlapping, of a pattern in a text.
   character(len=*), intent(in) :: text    !< Text.
   character(len=*), intent(in) :: pattern !< Pattern.
   integer(I4P)                 :: n       !< Occurrences.
   integer(I4P)                 :: start   !< Start of the search.
   integer(I4P)                 :: found   !< Position of an occurrence.

   n = 0
   start = 1
   do
      found = index(text(start:), pattern)
      if (found == 0) exit
      n = n + 1
      start = start + found - 1 + len(pattern)
      if (start > len(text)) exit
   enddo
   endfunction count_text

   pure function lines_number(text) result(n)
   !< Return the number of lines of a text, each one ended by a line feed.
   character(len=*), intent(in) :: text !< Text.
   integer(I4P)                 :: n    !< Lines.

   n = count_text(text, LF)
   endfunction lines_number

   pure function line(text, n) result(content)
   !< Return the n-th line of a text (negative: counted from the last one, -1 the last), without its line feed.
   character(len=*), intent(in)  :: text    !< Text.
   integer(I4P),     intent(in)  :: n       !< Line number.
   character(len=:), allocatable :: content !< Line.
   integer(I4P)                  :: target  !< Line number, from the first.
   integer(I4P)                  :: l       !< Counter.
   integer(I4P)                  :: start   !< Start of the line.
   integer(I4P)                  :: finish  !< End of the line.

   target = n ; if (n < 0) target = lines_number(text) + n + 1
   content = ''
   start = 1
   do l = 1, target
      finish = index(text(start:), LF)
      if (finish == 0) return
      if (l == target) content = text(start:start + finish - 2)
      start = start + finish
   enddo
   endfunction line

   subroutine report()
   !< End the test: `error stop` if any check failed.
   character(len=12) :: n !< Failures, as a string.

   if (failures > 0) then
      write(n, '(I0)') failures
      write(error_unit, '(A)') 'forbear test: '//trim(n)//' check(s) failed'
      error stop 1 ! a constant stop code: Fortran 2008
   endif
   print '(A)', 'all checks passed'
   endsubroutine report
endmodule forbear_test_tools
