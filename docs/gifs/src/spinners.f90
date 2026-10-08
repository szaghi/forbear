program spinners
!< A gallery of spinners, one bar per line: each one is a bar at its own position.
use, intrinsic :: iso_fortran_env, only : I8P=>int64, R8P=>real64
use forbear, only : bar_object
implicit none
integer, parameter :: n = 18                                  ! number of spinners
character(len=12)  :: keys(n) = [character(len=12) ::                                    &
                                 '⠋', '⣾', '⠓', '⠄', '⡃⢐', '|', '┤', '✶', '▃', '▉', &
                                 '◢', '◜', '◐', '◰', '▖', '⊶', '▒', '(  ●   )']  ! spinner keys
character(len=10)  :: colors(6) = [character(len=10) :: 'cyan', 'magenta', 'yellow', 'green', 'blue', 'red'] ! colours
type(bar_object)   :: bars(n)                                 ! one bar per spinner
integer            :: s                                       ! counter
integer            :: tick                                    ! counter

do s = 1, n
   call bars(s)%initialize(prefix_string='spinner_string='''//trim(keys(s))//'''  ', spinner_string=trim(keys(s)), &
                           spinner_color_fg=trim(colors(mod(s - 1, 6) + 1)), width=0, max_value=1.e6_R8P,        &
                           position=s - 1, min_interval=0._R8P)
   call bars(s)%start
enddo
do tick = 1, 1000 ! longer than the recording
   call wait(90)
   do s = 1, n
      call bars(s)%update(current=real(tick, R8P))
   enddo
enddo

contains
   subroutine wait(milliseconds)
   !< Wait.
   integer, intent(in) :: milliseconds ! time to wait
   integer(I8P)        :: start        ! clock at the start
   integer(I8P)        :: now          ! clock now
   integer(I8P)        :: rate         ! clock counts per second
   call system_clock(start, rate)
   do
      call system_clock(now)
      if ((now - start) * 1000 >= milliseconds * rate) exit
   enddo
   endsubroutine wait
endprogram spinners
