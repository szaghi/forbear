!< **forbear** project, definition of parametric kinds.

module forbear_kinds
!< **forbear** project, definition of parametric kinds.
implicit none
private
public :: ASCII
public :: UCS4
public :: ucs4_string

#ifdef ASCII_SUPPORTED
integer, parameter :: ASCII = selected_char_kind('ascii')      !< ASCII character set kind.
#else
integer, parameter :: ASCII = selected_char_kind('default')    !< ASCII character set kind.
#endif
#ifdef UCS4_SUPPORTED
integer, parameter :: UCS4  = selected_char_kind('iso_10646')  !< Unicode character set kind.
#else
integer, parameter :: UCS4  = selected_char_kind('default')    !< Unicode character set kind.
#endif

contains
   pure function ucs4_string(input) result(output)
   !< Convert string of any kind to UCS4 string.
   class(*), intent(in)                     :: input  !< Input string of any kind.
   character(len=:, kind=UCS4), allocatable :: output !< Output string of UCS4 kind.

   select type(input)
#if defined ASCII_SUPPORTED && defined ASCII_NEQ_DEFAULT
   type is(character(len=*, kind=ASCII))
      output = input
#endif
#ifdef UCS4_SUPPORTED
   type is(character(len=*, kind=UCS4))
      output = input
#endif
   type is(character(len=*))
      output = input
   endselect
   endfunction ucs4_string
endmodule forbear_kinds
