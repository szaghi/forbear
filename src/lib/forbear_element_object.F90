!< **forbear** project, definition of [[element_object]].

module forbear_element_object
!< **forbear** project, definition of [[element_object]].
use, intrinsic :: iso_fortran_env, only : error_unit
use face, only : colorize
use forbear_kinds, only : ASCII, UCS4
implicit none
private
public :: element_object
public :: is_color
public :: is_style

type :: element_object
   !< Bar element class.
   character(len=:, kind=UCS4), allocatable :: string   !< Element string.
   character(len=:),            allocatable :: color_fg !< Foreground color.
   character(len=:),            allocatable :: color_bg !< Background color.
   character(len=:),            allocatable :: style    !< Style.
   contains
      ! public methods
      procedure, pass(self) :: destroy    !< Destroy element.
      procedure, pass(self) :: initialize !< Initialize element.
      procedure, pass(self) :: output     !< Return formatted output of element.
      ! public operators
      generic :: assignment(=) => assign_element !< Overload `=`.
      ! private methods
      procedure, pass(lhs), private :: assign_element !< Operator `=`.
endtype element_object

contains
   ! public methods
   pure subroutine destroy(self)
   !< Destroy element.
   class(element_object), intent(inout) :: self !< element.

   if (allocated(self%string)) deallocate(self%string)
   if (allocated(self%color_fg)) deallocate(self%color_fg)
   if (allocated(self%color_bg)) deallocate(self%color_bg)
   if (allocated(self%style)) deallocate(self%style)
   endsubroutine destroy

   subroutine initialize(self, string, color_fg, color_bg, style)
   !< Initialize element; an unknown colour or style name stops the program.
   class(element_object), intent(inout)        :: self     !< element.
   class(*),              intent(in), optional :: string   !< Element string.
   character(len=*),      intent(in), optional :: color_fg !< Foreground color.
   character(len=*),      intent(in), optional :: color_bg !< Background color.
   character(len=*),      intent(in), optional :: style    !< Style.

   call self%destroy
   self%string = UCS4_''
   if (present(string)) then
      select type(string)
#if defined ASCII_SUPPORTED && defined ASCII_NEQ_DEFAULT
      type is(character(len=*, kind=ASCII))
         self%string = string
#endif
#ifdef UCS4_SUPPORTED
      type is(character(len=*, kind=UCS4))
         self%string = string
#endif
      type is(character(len=*))
         self%string = string
      endselect
   endif
   self%color_fg = '' ; if (present(color_fg)) self%color_fg = color_fg
   self%color_bg = '' ; if (present(color_bg)) self%color_bg = color_bg
   self%style = '' ; if (present(style)) self%style = style
   if (.not.is_color(self%color_fg)) call stop_unknown('colour', self%color_fg)
   if (.not.is_color(self%color_bg)) call stop_unknown('colour', self%color_bg)
   if (.not.is_style(self%style)) call stop_unknown('style', self%style)
   endsubroutine initialize

   pure function output(self)
   !< Return formatted output of element.
   class(element_object), intent(in)        :: self   !< element.
   character(len=:, kind=UCS4), allocatable :: output !< Formatted output.

   output = colorize(self%string, color_fg=self%color_fg, color_bg=self%color_bg, style=self%style)
   endfunction output

   ! non type-bound procedures
   pure function is_color(name)
   !< Return true if the name is a colour of FACE, or empty (no colour), in any case.
   character(len=*), intent(in) :: name     !< Colour name.
   logical                      :: is_color !< The name is a colour.

   ! FACE writes a known colour as an escape sequence around the string, and leaves it as it is otherwise
   is_color = len_trim(name) == 0
   if (.not.is_color) is_color = colorize('x', color_fg=name) /= 'x'
   endfunction is_color

   pure function is_style(name)
   !< Return true if the name is a style of FACE, or empty (no style), in any case.
   character(len=*), intent(in) :: name     !< Style name.
   logical                      :: is_style !< The name is a style.

   is_style = len_trim(name) == 0
   if (.not.is_style) is_style = colorize('x', style=name) /= 'x'
   endfunction is_style

   subroutine stop_unknown(what, name)
   !< Stop the program on an unknown colour or style name.
   character(len=*), intent(in) :: what !< What the name is: colour, style.
   character(len=*), intent(in) :: name !< Name.

   write(error_unit, '(A)') 'forbear: unknown '//what//' "'//name//'", see https://szaghi.github.io/forbear/guide/styling'
   error stop 'forbear: unknown colour or style name'
   endsubroutine stop_unknown

   ! private methods
   pure subroutine assign_element(lhs, rhs)
   !< Operator `=`.
   class(element_object), intent(inout) :: lhs !< Left hand side.
   type(element_object),  intent(in)    :: rhs !< Right hand side.

   if (allocated(rhs%string)) lhs%string = rhs%string
   if (allocated(rhs%color_fg)) lhs%color_fg = rhs%color_fg
   if (allocated(rhs%color_bg)) lhs%color_bg = rhs%color_bg
   if (allocated(rhs%style)) lhs%style = rhs%style
   endsubroutine assign_element
endmodule forbear_element_object
