!< **forbear** project, definition of [[bar_object]].

module forbear_bar_object
!< **forbear** project, definition of [[bar_object]].
use, intrinsic :: iso_fortran_env, only : I4P=>int32, R8P=>real32, stdout=>output_unit
implicit none
private
public :: bar_object

type :: bar_object
   !< Progress **bar** class.
   integer(I4P)                  :: width=32                     !< With of the bar.
   character(len=:), allocatable :: prefix                       !< Message prefixing the bar.
   character(len=:), allocatable :: suffix                       !< Message suffixing the bar.
   character(len=1)              :: empty_char=' '               !< Characters used for empty bar.
   character(len=1)              :: filled_char='='              !< Characters used for filled bar.
   real(R8P)                     :: max_value=1._R8P             !< Maximum value.
   integer(I4P)                  :: frequency=1                  !< Bar update frequency, in range `[1%,100%]`.
   logical                       :: add_progress_percent=.false. !< Add numerical progress percent to the bar.
   contains
      ! public methods
      procedure, pass(self) :: destroy    !< Destroy bar.
      procedure, pass(self) :: initialize !< Initialize bar.
      procedure, pass(self) :: start      !< Start bar.
      procedure, pass(self) :: update     !< Update bar.
      ! public operators
      generic :: assignment(=) => assign_bar !< Overload `=` operator.
      ! private methods
      procedure, pass(lhs), private :: assign_bar !< Operator `=`.
endtype bar_object

contains
   ! public methods
   pure subroutine destroy(self)
   !< Destroy bar.
   class(bar_object), intent(inout) :: self  !< Bar.
   type(bar_object)                 :: fresh !< Fresh instance of bar.

   self = fresh
   endsubroutine destroy

   subroutine initialize(self, width, prefix, suffix, empty_char, filled_char, max_value, frequency, add_progress_percent)
   !< Initialize bar.
   class(bar_object), intent(inout)        :: self                 !< Bar.
   integer(I4P),      intent(in), optional :: width                !< With of the bar.
   character(len=*),  intent(in), optional :: prefix               !< Message prefixing the bar.
   character(len=*),  intent(in), optional :: suffix               !< Message suffixing the bar.
   character(len=*),  intent(in), optional :: empty_char           !< Characters used for empty bar.
   character(len=*),  intent(in), optional :: filled_char          !< Characters used for filled bar.
   real(R8P),         intent(in), optional :: max_value            !< Maximum value.
   integer(I4P),      intent(in), optional :: frequency            !< Bar update frequency, in range `[1%,100%]`.
   logical,           intent(in), optional :: add_progress_percent !< Add numerical progress percent to the bar.

   call self%destroy
   self%prefix = ''
   self%suffix = ''
   if (present(width)) self%width = width
   if (present(prefix)) self%prefix = prefix
   if (present(suffix)) self%suffix = suffix
   if (present(empty_char)) self%empty_char = empty_char
   if (present(filled_char)) self%filled_char = filled_char
   if (present(max_value)) self%max_value = max_value
   if (present(frequency)) self%frequency = frequency
   if (present(add_progress_percent)) self%add_progress_percent = add_progress_percent
   endsubroutine initialize

   subroutine start(self)
   !< Start bar.
   !<
   !< @note This is only *syntax sugar*, it is equivalent to call `update(current=0)`.
   class(bar_object), intent(inout) :: self !< Bar.

   call self%update(current=0._R8P)
   endsubroutine start

   subroutine update(self, current)
   !< Update bar.
   class(bar_object), intent(inout) :: self             !< Bar.
   real(R8P),         intent(in)    :: current          !< Current value.
   integer(I4P)                     :: progress         !< Progress, in percent.
   integer(I4P)                     :: f_chars          !< Number of filled characters of bar.
   character(len=4)                 :: progress_percent !< Progress in percent.
   character(len=:), allocatable    :: bar              !< Bar line.

   progress = nint(current / self%max_value * 100)
   if (mod(progress, self%frequency) == 0 .or. progress == 100) then
      f_chars = nint(progress / 100._R8P * self%width)
      bar = self%prefix//                                   &
            repeat(self%filled_char, f_chars)//             &
            repeat(self%empty_char, self%width - f_chars)// &
            self%suffix
      if (self%add_progress_percent) then
         write(progress_percent, '(I3,A)') progress, '%'
         bar = bar//progress_percent
      endif
      bar = bar//char(13)
      write(stdout, '(A)', advance='no') bar
      flush(stdout)
   endif
   endsubroutine update

   ! private methods
   pure subroutine assign_bar(lhs, rhs)
   !< Initialize bar.
   class(bar_object), intent(inout) :: lhs !< Left hand side.
   type(bar_object),  intent(in)    :: rhs !< Right hand side.

   lhs%width = rhs%width
   if (allocated(lhs%prefix)) deallocate(lhs%prefix)
   if (allocated(rhs%prefix)) lhs%prefix = rhs%prefix
   if (allocated(lhs%suffix)) deallocate(lhs%suffix)
   if (allocated(rhs%suffix)) lhs%suffix = rhs%suffix
   lhs%empty_char = rhs%empty_char
   lhs%filled_char = rhs%filled_char
   lhs%max_value = rhs%max_value
   lhs%frequency = rhs%frequency
   lhs%add_progress_percent = rhs%add_progress_percent
   endsubroutine assign_bar
endmodule forbear_bar_object
