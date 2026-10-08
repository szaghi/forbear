!run sequence sequence
program sequence
!< Several bars one after another, configured once.
use, intrinsic :: iso_fortran_env, only : R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: style ! the configuration shared by the bars
type(bar_object) :: bar   ! the progress bar of a phase
character(9)     :: phases(3) = [character(9) :: 'mesh     ', 'solve    ', 'write    ']
integer          :: p     ! current phase

!region copy
call style%initialize(bracket_left_string='[', bracket_right_string='] ', filled_char_string='=', &
                      empty_char_string=' ', add_progress_percent=.true., width=30)
do p = 1, size(phases)
   bar = style                                       ! copy the configuration
   call bar%initialize(prefix_string=phases(p))      ! re-initialize: everything else is reset!
   call run_phase
enddo
!endregion copy
!region keep
do p = 1, size(phases)
   call bar%initialize(prefix_string=phases(p), bracket_left_string='[', bracket_right_string='] ', &
                       filled_char_string='=', empty_char_string=' ', add_progress_percent=.true., width=30)
   call run_phase
enddo
!endregion keep

contains
   subroutine run_phase
   !< Run a phase of 20 steps.
   integer :: step ! current step
   call bar%start
   do step = 1, 20
      call bar%update(current=step / 20._R8P)
   enddo
   endsubroutine run_phase
endprogram sequence
