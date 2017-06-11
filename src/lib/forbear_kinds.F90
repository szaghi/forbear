!< **forbear** project, definition of parametric kinds.

module forbear_kinds
!< **forbear** project, definition of parametric kinds.
implicit none
private
public :: ASCII
public :: UCS4

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
endmodule forbear_kinds
