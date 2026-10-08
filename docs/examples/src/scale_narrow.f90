!run -s scale_narrow scale_narrow
program scale_narrow
!< The scale needs a bar at least 22 characters wide.
use forbear, only : bar_object
implicit none
type(bar_object) :: bar ! the progress bar

call bar%initialize(width=20, add_scale_bar=.true.)
endprogram scale_narrow
