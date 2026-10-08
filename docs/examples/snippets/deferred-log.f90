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
