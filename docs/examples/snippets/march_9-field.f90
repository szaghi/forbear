module residual_fields
!< A field of the program: the residual of the solution, read at every drawing.
use, intrinsic :: iso_fortran_env, only : R8P=>real64
use forbear, only : field_object, progress_object
implicit none
private
public :: residual_field

type, extends(field_object) :: residual_field
   real(R8P), pointer :: value => null() ! the residual, a variable of the program
   contains
      procedure, pass(self) :: render
endtype residual_field

contains
   function render(self, progress) result(text)
   !< Return the residual, always 7 characters wide.
   class(residual_field), intent(in) :: self
   type(progress_object), intent(in) :: progress
   character(len=:), allocatable     :: text
   character(len=7)                  :: buffer
   write(buffer, '(ES7.1)') self%value
   text = buffer
   endfunction render
endmodule residual_fields
