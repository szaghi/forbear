!run deferred deferred
program deferred
!< Messages produced while a bar is running, printed when it has finished.
use, intrinsic :: iso_fortran_env, only : R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object)  :: bar          ! the progress bar
character(64)     :: pending(10)  ! messages waiting for the bar to finish
integer           :: n_pending    ! number of messages waiting
integer           :: step         ! current step
character(64)     :: message      ! a message

n_pending = 0
call bar%initialize(prefix_string='solve ', add_progress_percent=.true., width=30, max_value=40._R8P)
call bar%start
do step = 1, 40
   if (mod(step, 15) == 0) then
      write(message, '(A,I0)') 'warning: CFL limit reached at step ', step
      call log_message(message)
   endif
   call bar%update(current=real(step, R8P))
enddo
call log_message('solve: done')

contains
   !region log
   subroutine log_message(message)
   !< Print a message, or keep it while the bar owns the terminal.
   character(*), intent(in) :: message ! the message
   integer                  :: m       ! counter
   if (bar%is_stdout_locked()) then
      n_pending = n_pending + 1
      pending(n_pending) = message
   else
      do m = 1, n_pending
         print '(A)', trim(pending(m))
      enddo
      n_pending = 0
      print '(A)', trim(message)
   endif
   endsubroutine log_message
   !endregion log
endprogram deferred
