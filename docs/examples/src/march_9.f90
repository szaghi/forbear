!as march
!run -f 26 march_9-running march
!run march_9 march
!region field
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
!endregion field

program march
!< Tutorial, chapter 9: a layout template, with a field of the program.
use, intrinsic :: iso_fortran_env, only : I8P=>int64, R8P=>real64
use forbear, only : bar_object
use residual_fields, only : residual_field
implicit none
type(bar_object)     :: bar      ! the progress bar
type(residual_field) :: field    ! the residual, as a field of the bar
real(R8P), target    :: residual ! residual of the solution
integer              :: steps    ! time steps
integer              :: step     ! current time step

steps = 50
!region init
call bar%initialize(template='march {bar:30} {percent:yellow} step {count:cyan} ETA {eta:blue} res {residual:magenta}', &
                    partial_blocks=.true., filled_char_color_fg='cyan', empty_char_color_bg='black_intense', &
                    max_value=real(steps, R8P))
field%value => residual
call bar%add_field('residual', field) ! after initialize, before start
!endregion init
!region loop
residual = 1._R8P
call bar%start
do step = 1, steps
   call advance
   residual = residual / 2._R8P
   call bar%update(current=real(step, R8P))
enddo
!endregion loop

contains
   subroutine advance
   !< Advance the solution of one time step (here: wait 40 ms, as a real computation would take).
   integer(I8P) :: start ! clock at the start
   integer(I8P) :: now   ! clock now
   integer(I8P) :: rate  ! clock counts per second
   call system_clock(start, rate)
   do
      call system_clock(now)
      if (now - start >= rate / 25) exit
   enddo
   endsubroutine advance
endprogram march
