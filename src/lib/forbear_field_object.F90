!< **forbear** project, definition of [[field_object]], the fields a program adds to the template of a bar.

module forbear_field_object
!< **forbear** project, definition of [[field_object]], the fields a program adds to the template of a bar.
!<
!< A program extends `field_object`, overriding `render`, and adds an instance to a bar with `bar%add_field(name, field)`:
!< the template of the bar shows it where it writes `{name}`, at every drawing, with the colours of its spec.
use, intrinsic :: iso_fortran_env, only : I4P=>int32, R8P=>real64
implicit none
private
public :: field_object
public :: progress_object

type :: progress_object
   !< What a bar knows of its progress when it draws: what a field can show.
   real(R8P)    :: current = 0._R8P   !< Current value, clamped to the range.
   real(R8P)    :: min_value = 0._R8P !< Minimum value.
   real(R8P)    :: max_value = 1._R8P !< Maximum value.
   real(R8P)    :: fraction = 0._R8P  !< Fraction of the range done, in [0, 1].
   integer(I4P) :: percent = 0        !< Progress, in percent, truncated.
   real(R8P)    :: rate = 0._R8P      !< Smoothed rate, fraction of the range per second; 0 until known.
   real(R8P)    :: elapsed = 0._R8P   !< Time since the start, in seconds.
   real(R8P)    :: eta = -1._R8P      !< Estimated time to the end, in seconds; negative until known.
endtype progress_object

type, abstract :: field_object
   !< A field of a template, defined by the program.
   contains
      procedure(render_interface), pass(self), deferred :: render !< Return the text of the field.
endtype field_object

abstract interface
   function render_interface(self, progress) result(text)
   !< Return the text of the field for the progress of the bar; keep its width constant, as the bar line must not shrink.
   import :: field_object, progress_object
   class(field_object),   intent(in) :: self     !< Field.
   type(progress_object), intent(in) :: progress !< Progress of the bar.
   character(len=:), allocatable     :: text     !< Text of the field.
   endfunction render_interface
endinterface
endmodule forbear_field_object
