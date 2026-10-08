program layout
!< A layout of your own: the percent first, words between the fields.
use, intrinsic :: iso_fortran_env, only : R8P=>real64
use forbear, only : bar_object
implicit none
type(bar_object) :: bar   ! the progress bar
integer          :: n     ! files
integer          :: i     ! current file

n = 12
call bar%initialize(template='{prefix} {percent:yellow} [{bar:20}] {count} files, {eta:blue} left', &
                    prefix_string='load', max_value=real(n, R8P))
call bar%start
do i = 1, n
   call bar%update(current=real(i, R8P))
enddo
endprogram layout
