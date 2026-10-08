!< **forbear** project, definition of [[bar_object]].

module forbear_bar_object
!< **forbear** project, definition of [[bar_object]].
use, intrinsic :: iso_c_binding, only : c_int
use, intrinsic :: iso_fortran_env, only : I4P=>int32, I8P=>int64, R8P=>real64, stdout=>output_unit, stderr=>error_unit
use, intrinsic :: ieee_arithmetic, only : ieee_is_finite, ieee_is_nan
use forbear_element_object, only : element_object
use forbear_kinds, only : ASCII, UCS4, ucs4_string
implicit none
private
public :: bar_object

character(len=1), parameter :: ESC = achar(27) !< Escape, start of the ANSI control sequences.
character(len=1), parameter :: CR  = achar(13) !< Carriage return.
character(len=1), parameter :: LF  = achar(10) !< Line feed.
! UTF-8 encoded, as every literal of the sources: written byte by byte, the terminal shows the characters
character(len=*), parameter :: FULL_BLOCK = '█'                                          !< Full block.
character(len=*), parameter :: PARTIAL_BLOCKS(1:7) = ['▏', '▎', '▍', '▌', '▋', '▊', '▉'] !< Blocks of 1/8 to 7/8.

interface
   function isatty(fd) bind(c, name='isatty')
   !< POSIX `isatty`: non zero if the file descriptor refers to a terminal.
   import :: c_int
   integer(c_int), value :: fd     !< File descriptor.
   integer(c_int)        :: isatty !< Non zero if a terminal.
   endfunction isatty
endinterface

type :: bar_object
   !< Progress **bar** class.
   type(element_object)              :: prefix               !< Message prefixing the bar.
   type(element_object)              :: suffix               !< Message suffixing the bar.
   type(element_object)              :: bracket_left         !< Left bracket surrounding the bar.
   type(element_object)              :: bracket_right        !< Right bracket surrounding the bar.
   type(element_object)              :: empty_char           !< Characters used for empty bar.
   type(element_object)              :: filled_char          !< Characters used for filled bar.
   type(element_object)              :: progress_percent     !< Progress in percent.
   type(element_object)              :: progress_count       !< Progress count, current and maximum value.
   type(element_object)              :: progress_speed       !< Progress speed in percent.
   type(element_object)              :: eta                  !< Estimated time of arrival.
   type(element_object)              :: message              !< Message given by the last update.
   type(element_object)              :: scale_bar            !< Scale bar.
   type(element_object)              :: date_time            !< Date and time.
   type(element_object)              :: summary              !< Summary printed when the bar completes.
   type(element_object), allocatable :: spinner(:)           !< Spinner.
   integer(I4P)                      :: width                !< With of the bar.
   real(R8P)                         :: min_value            !< Minimum value.
   real(R8P)                         :: max_value            !< Maximum value.
   integer(I4P)                      :: frequency            !< Bar update frequency, in range `[1%,100%]`.
   real(R8P)                         :: min_interval         !< Minimum time between two drawings, in seconds.
   real(R8P)                         :: smoothing            !< Smoothing of the speed, in [0, 1]: 0 average, 1 momentary.
   integer(I4P)                      :: position             !< Line of the bar, counted below the current one.
   logical                           :: add_scale_bar        !< Add scale to the bar.
   logical                           :: add_progress_percent !< Add progress in percent.
   logical                           :: add_progress_count   !< Add progress count.
   logical                           :: add_progress_speed   !< Add progress speed in percent.
   logical                           :: add_eta              !< Add estimated time of arrival.
   logical                           :: add_date_time        !< Add date and time.
   logical                           :: add_summary          !< Add a summary line at the end.
   logical                           :: partial_blocks       !< Draw the bar with partial blocks, 8 steps per character.
   logical                           :: is_interactive_      !< Flag set when the bar is drawn on a terminal.
   logical                           :: is_disabled_         !< Flag set when the bar draws nothing.
   logical                           :: is_stdout_locked_    !< Flag to store standard output status.
   integer(I4P)                      :: output_unit = stdout !< Output unit to display bar
   ! run state, reset by start
   integer(I4P)                             :: progress_drawn_ = -1     !< Progress at the last drawing, %; -1 before.
   real(R8P)                                :: fraction_drawn_ = 0._R8P !< Fraction of the range done at the last drawing.
   real(R8P)                                :: rate_ = 0._R8P           !< Smoothed rate, fraction of the range per second.
   integer(I4P)                             :: rate_samples_ = 0        !< Number of rate samples.
   integer(I8P)                             :: tic_ = 0_I8P             !< Timer count at the last drawing.
   integer(I8P)                             :: tic_start_ = 0_I8P       !< Timer count at the start.
   integer(I4P)                             :: spinner_count_ = 0       !< Spinner frame at the last drawing.
   character(len=18)                        :: date_time_start_ = ''    !< Start date/time.
   logical                                  :: is_complete_ = .false.   !< Flag set when the bar has reached 100%.
   character(len=:, kind=UCS4), allocatable :: frame_                   !< Last frame drawn, without control sequences.
   contains
      ! public methods
      procedure, pass(self) :: destroy                !< Destroy bar.
      procedure, pass(self) :: initialize             !< Initialize bar.
      procedure, pass(self) :: is_stdout_locked       !< Return status of standard output unit.
      procedure, pass(self) :: start                  !< Start bar.
      procedure, pass(self) :: update                 !< Update bar.
      procedure, pass(self) :: write => write_message !< Write a message above the bar.
      ! private methods
      procedure, pass(self), private :: build_frame    !< Build the frame of the current progress.
      procedure, pass(self), private :: complete       !< Complete the bar.
      procedure, pass(self), private :: create_spinner !< Create spinner.
      procedure, pass(self), private :: draw           !< Draw the last frame on the terminal.
      procedure, pass(self), private :: update_rate    !< Update the smoothed rate.
endtype bar_object

contains
   ! public methods
   pure subroutine destroy(self)
   !< Destroy bar.
   class(bar_object), intent(inout) :: self !< Bar.
   integer(I4P)                     :: s    !< Counter.

   call self%prefix%destroy
   call self%suffix%destroy
   call self%bracket_left%destroy
   call self%bracket_right%destroy
   call self%empty_char%destroy
   call self%filled_char%destroy
   call self%progress_percent%destroy
   call self%progress_count%destroy
   call self%progress_speed%destroy
   call self%eta%destroy
   call self%message%destroy
   call self%scale_bar%destroy
   call self%date_time%destroy
   call self%summary%destroy
   if (allocated(self%spinner)) then
      do s=1, size(self%spinner, dim=1)
         call self%spinner(s)%destroy
      enddo
      deallocate (self%spinner)
   endif
   self%width = 32
   self%min_value = 0._R8P
   self%max_value = 1._R8P
   self%frequency = 1_I4P
   self%min_interval = 0.1_R8P
   self%smoothing = 0.3_R8P
   self%position = 0_I4P
   self%output_unit = stdout
   self%add_scale_bar = .false.
   self%add_progress_percent = .false.
   self%add_progress_count = .false.
   self%add_progress_speed = .false.
   self%add_eta = .false.
   self%add_date_time = .false.
   self%add_summary = .false.
   self%partial_blocks = .false.
   self%is_interactive_ = .false.
   self%is_disabled_ = .false.
   self%is_stdout_locked_ = .false.
   self%progress_drawn_ = -1_I4P
   self%fraction_drawn_ = 0._R8P
   self%rate_ = 0._R8P
   self%rate_samples_ = 0_I4P
   self%tic_ = 0_I8P
   self%tic_start_ = 0_I8P
   self%spinner_count_ = 0_I4P
   self%date_time_start_ = ''
   self%is_complete_ = .false.
   if (allocated(self%frame_)) deallocate(self%frame_)
   endsubroutine destroy

   subroutine initialize(self,                                                                                               &
                         prefix_string, prefix_color_fg, prefix_color_bg, prefix_style,                                      &
                         suffix_string, suffix_color_fg, suffix_color_bg, suffix_style,                                      &
                         bracket_left_string, bracket_left_color_fg, bracket_left_color_bg, bracket_left_style,              &
                         bracket_right_string, bracket_right_color_fg, bracket_right_color_bg, bracket_right_style,          &
                         empty_char_string, empty_char_color_fg, empty_char_color_bg, empty_char_style,                      &
                         filled_char_string, filled_char_color_fg, filled_char_color_bg, filled_char_style,                  &
                         spinner_string, spinner_color_fg, spinner_color_bg, spinner_style,                                  &
                         add_scale_bar, scale_bar_color_fg, scale_bar_color_bg, scale_bar_style,                             &
                         add_progress_percent, progress_percent_color_fg, progress_percent_color_bg, progress_percent_style, &
                         add_progress_count, progress_count_color_fg, progress_count_color_bg, progress_count_style,         &
                         add_progress_speed, progress_speed_color_fg, progress_speed_color_bg, progress_speed_style,         &
                         add_eta, eta_color_fg, eta_color_bg, eta_style,                                                     &
                         add_date_time, date_time_color_fg, date_time_color_bg, date_time_style,                             &
                         add_summary, summary_color_fg, summary_color_bg, summary_style,                                     &
                         message_color_fg, message_color_bg, message_style,                                                  &
                         width, min_value, max_value, frequency, min_interval, smoothing, partial_blocks, position,          &
                         interactive, disabled, output_unit)
   !< Initialize bar.
   !<
   !< Every setting not passed takes its default. The display mode is resolved here: `interactive` if passed, else the
   !< environment variable `FORBEAR_INTERACTIVE` (0 or 1) if set, else whether `output_unit` is a terminal. The
   !< environment variable `FORBEAR_DISABLE` (any value but 0) disables every bar; `FORBEAR_MIN_INTERVAL` replaces the
   !< default of `min_interval`.
   class(bar_object), intent(inout)         :: self                      !< Bar.
   class(*),          intent(in), optional  :: prefix_string             !< Prefix string.
   character(len=*),  intent(in), optional  :: prefix_color_fg           !< Prefix foreground color.
   character(len=*),  intent(in), optional  :: prefix_color_bg           !< Prefix background color.
   character(len=*),  intent(in), optional  :: prefix_style              !< Prefix style.
   class(*),          intent(in), optional  :: suffix_string             !< Suffix string.
   character(len=*),  intent(in), optional  :: suffix_color_fg           !< Suffix foreground color.
   character(len=*),  intent(in), optional  :: suffix_color_bg           !< Suffix background color.
   character(len=*),  intent(in), optional  :: suffix_style              !< Suffix style.
   class(*),          intent(in), optional  :: bracket_left_string       !< Left bracket string.
   character(len=*),  intent(in), optional  :: bracket_left_color_fg     !< Left bracket foreground color.
   character(len=*),  intent(in), optional  :: bracket_left_color_bg     !< Left bracket background color.
   character(len=*),  intent(in), optional  :: bracket_left_style        !< Left bracket style.
   class(*),          intent(in), optional  :: bracket_right_string      !< Right bracket string
   character(len=*),  intent(in), optional  :: bracket_right_color_fg    !< Right bracket foreground color.
   character(len=*),  intent(in), optional  :: bracket_right_color_bg    !< Right bracket background color.
   character(len=*),  intent(in), optional  :: bracket_right_style       !< Right bracket style.
   class(*),          intent(in), optional  :: empty_char_string         !< Empty char.
   character(len=*),  intent(in), optional  :: empty_char_color_fg       !< Empty char foreground color.
   character(len=*),  intent(in), optional  :: empty_char_color_bg       !< Empty char background color.
   character(len=*),  intent(in), optional  :: empty_char_style          !< Empty char style.
   class(*),          intent(in), optional  :: filled_char_string        !< Filled char.
   character(len=*),  intent(in), optional  :: filled_char_color_fg      !< Filled char foreground color.
   character(len=*),  intent(in), optional  :: filled_char_color_bg      !< Filled char background color.
   character(len=*),  intent(in), optional  :: filled_char_style         !< Filled char style.
   class(*),          intent(in), optional  :: spinner_string            !< Spinner char.
   character(len=*),  intent(in), optional  :: spinner_color_fg          !< Spinner char foreground color.
   character(len=*),  intent(in), optional  :: spinner_color_bg          !< Spinner char background color.
   character(len=*),  intent(in), optional  :: spinner_style             !< Spinner char style.
   logical,           intent(in), optional  :: add_scale_bar             !< Add scale to the bar.
   character(len=*),  intent(in), optional  :: scale_bar_color_fg        !< Scale bar foreground color.
   character(len=*),  intent(in), optional  :: scale_bar_color_bg        !< Scale bar background color.
   character(len=*),  intent(in), optional  :: scale_bar_style           !< Scale bar style.
   logical,           intent(in), optional  :: add_progress_percent      !< Add progress in percent.
   character(len=*),  intent(in), optional  :: progress_percent_color_fg !< Progress percent foreground color.
   character(len=*),  intent(in), optional  :: progress_percent_color_bg !< Progress percent background color.
   character(len=*),  intent(in), optional  :: progress_percent_style    !< Progress percent style.
   logical,           intent(in), optional  :: add_progress_count        !< Add progress count, current/maximum.
   character(len=*),  intent(in), optional  :: progress_count_color_fg   !< Progress count foreground color.
   character(len=*),  intent(in), optional  :: progress_count_color_bg   !< Progress count background color.
   character(len=*),  intent(in), optional  :: progress_count_style      !< Progress count style.
   logical,           intent(in), optional  :: add_progress_speed        !< Add progress speed in percent.
   character(len=*),  intent(in), optional  :: progress_speed_color_fg   !< Progress speed foreground color.
   character(len=*),  intent(in), optional  :: progress_speed_color_bg   !< Progress speed background color.
   character(len=*),  intent(in), optional  :: progress_speed_style      !< Progress speed style.
   logical,           intent(in), optional  :: add_eta                   !< Add estimated time of arrival.
   character(len=*),  intent(in), optional  :: eta_color_fg              !< ETA foreground color.
   character(len=*),  intent(in), optional  :: eta_color_bg              !< ETA background color.
   character(len=*),  intent(in), optional  :: eta_style                 !< ETA style.
   logical,           intent(in), optional  :: add_date_time             !< Add date and time.
   character(len=*),  intent(in), optional  :: date_time_color_fg        !< Date and time foreground color.
   character(len=*),  intent(in), optional  :: date_time_color_bg        !< Date and time background color.
   character(len=*),  intent(in), optional  :: date_time_style           !< Date and time style.
   logical,           intent(in), optional  :: add_summary               !< Add a summary line at the end.
   character(len=*),  intent(in), optional  :: summary_color_fg          !< Summary foreground color.
   character(len=*),  intent(in), optional  :: summary_color_bg          !< Summary background color.
   character(len=*),  intent(in), optional  :: summary_style             !< Summary style.
   character(len=*),  intent(in), optional  :: message_color_fg          !< Message foreground color.
   character(len=*),  intent(in), optional  :: message_color_bg          !< Message background color.
   character(len=*),  intent(in), optional  :: message_style             !< Message style.
   integer(I4P),      intent(in), optional  :: width                     !< With of the bar.
   real(R8P),         intent(in), optional  :: min_value                 !< Minimum value.
   real(R8P),         intent(in), optional  :: max_value                 !< Maximum value.
   integer(I4P),      intent(in), optional  :: frequency                 !< Bar update frequency, in range `[1%,100%]`.
   real(R8P),         intent(in), optional  :: min_interval              !< Minimum time between two drawings, in seconds.
   real(R8P),         intent(in), optional  :: smoothing                 !< Smoothing of the speed, in [0, 1].
   logical,           intent(in), optional  :: partial_blocks            !< Draw the bar with partial blocks.
   integer(I4P),      intent(in), optional  :: position                  !< Line of the bar, below the current one.
   logical,           intent(in), optional  :: interactive               !< Draw for a terminal (else for a log).
   logical,           intent(in), optional  :: disabled                  !< Draw nothing.
   integer(I4P),      intent(in), optional  :: output_unit               !< Output unit to display bar
   character(len=:, kind=UCS4), allocatable :: empty_char_string_        !< Characters used for empty bar, local variable.
   character(len=:, kind=UCS4), allocatable :: filled_char_string_       !< Characters used for filled bar, local variable.
   character(len=:),            allocatable :: env                       !< Value of an environment variable.
   real(R8P)                                :: env_real                  !< Real value of an environment variable.
   integer(I4P)                             :: iostat                    !< Status of a read.
   logical                                  :: is_partial                !< Draw with partial blocks.

   is_partial = .false. ; if (present(partial_blocks)) is_partial = partial_blocks
   if (present(empty_char_string)) then
      empty_char_string_ = ucs4_string(input=empty_char_string)
   elseif (is_partial) then
      empty_char_string_ = UCS4_' '
   else
      empty_char_string_ = UCS4_'-'
   endif
   if (is_partial) then
      filled_char_string_ = ucs4_string(input=FULL_BLOCK)
   elseif (present(filled_char_string)) then
      filled_char_string_ = ucs4_string(input=filled_char_string)
   else
      filled_char_string_ = UCS4_'*'
   endif

   call self%destroy
   call self%prefix%initialize(string=prefix_string, color_fg=prefix_color_fg, color_bg=prefix_color_bg, style=prefix_style)
   call self%suffix%initialize(string=suffix_string, color_fg=suffix_color_fg, color_bg=suffix_color_bg, style=suffix_style)
   call self%bracket_left%initialize(string=bracket_left_string, color_fg=bracket_left_color_fg, color_bg=bracket_left_color_bg,&
                                     style=bracket_left_style)
   call self%bracket_right%initialize(string=bracket_right_string, color_fg=bracket_right_color_fg,&
                                      color_bg=bracket_right_color_bg, style=bracket_right_style)
   call self%empty_char%initialize(string=empty_char_string_, color_fg=empty_char_color_fg, color_bg=empty_char_color_bg,&
                                   style=empty_char_style)
   call self%filled_char%initialize(string=filled_char_string_, color_fg=filled_char_color_fg, color_bg=filled_char_color_bg,&
                                    style=filled_char_style)
   call self%create_spinner(string=spinner_string, color_fg=spinner_color_fg, color_bg=spinner_color_bg, style=spinner_style)
   if (present(add_scale_bar)) self%add_scale_bar = add_scale_bar
   call self%scale_bar%initialize(color_fg=scale_bar_color_fg, color_bg=scale_bar_color_bg, style=scale_bar_style)
   if (present(add_progress_percent)) self%add_progress_percent = add_progress_percent
   call self%progress_percent%initialize(color_fg=progress_percent_color_fg, color_bg=progress_percent_color_bg,&
                                         style=progress_percent_style)
   if (present(add_progress_count)) self%add_progress_count = add_progress_count
   call self%progress_count%initialize(color_fg=progress_count_color_fg, color_bg=progress_count_color_bg,&
                                       style=progress_count_style)
   if (present(add_progress_speed)) self%add_progress_speed = add_progress_speed
   call self%progress_speed%initialize(color_fg=progress_speed_color_fg, color_bg=progress_speed_color_bg,&
                                       style=progress_speed_style)
   if (present(add_eta)) self%add_eta = add_eta
   call self%eta%initialize(color_fg=eta_color_fg, color_bg=eta_color_bg, style=eta_style)
   if (present(add_date_time)) self%add_date_time = add_date_time
   call self%date_time%initialize(color_fg=date_time_color_fg, color_bg=date_time_color_bg, style=date_time_style)
   if (present(add_summary)) self%add_summary = add_summary
   call self%summary%initialize(color_fg=summary_color_fg, color_bg=summary_color_bg, style=summary_style)
   call self%message%initialize(color_fg=message_color_fg, color_bg=message_color_bg, style=message_style)
   if (present(width)) self%width = width
   if (present(min_value))    self%min_value = min_value
   if (present(max_value))    self%max_value = max_value
   if (present(frequency))    self%frequency = frequency
   if (present(smoothing))    self%smoothing = max(0._R8P, min(1._R8P, smoothing))
   self%partial_blocks = is_partial
   if (present(position))     self%position = max(0_I4P, position)
   if (present(output_unit))  self%output_unit = output_unit
   if (present(min_interval)) then
      self%min_interval = min_interval
   elseif (get_environment('FORBEAR_MIN_INTERVAL', env)) then
      read(env, *, iostat=iostat) env_real
      if (iostat == 0) self%min_interval = env_real
   endif
   if (present(interactive)) then
      self%is_interactive_ = interactive
   elseif (get_environment('FORBEAR_INTERACTIVE', env)) then
      self%is_interactive_ = trim(env) /= '0'
   else
      self%is_interactive_ = is_terminal(self%output_unit)
   endif
   if (present(disabled)) self%is_disabled_ = disabled
   if (get_environment('FORBEAR_DISABLE', env)) then
      if (trim(env) /= '0') self%is_disabled_ = .true.
   endif
   ! a log cannot come back to a line below: only the bar on the current line is logged
   if (.not.self%is_interactive_ .and. self%position > 0) self%is_disabled_ = .true.

   if (self%add_scale_bar .and. self%width < 22) error stop 'error: for adding scale bar the bar width must be at least 22 chars'
   endsubroutine initialize

   pure function is_stdout_locked(self) result(is_locked)
   !< Return status of standard output unit.
   class(bar_object), intent(in) :: self      !< Bar.
   logical                       :: is_locked !< Standard output status.

   is_locked = self%is_stdout_locked_
   endfunction is_stdout_locked

   subroutine start(self)
   !< Start bar.
   class(bar_object), intent(inout) :: self !< Bar.

   self%progress_drawn_ = -1_I4P
   self%fraction_drawn_ = 0._R8P
   self%rate_ = 0._R8P
   self%rate_samples_ = 0_I4P
   self%spinner_count_ = 0_I4P
   self%is_complete_ = .false.
   self%message%string = UCS4_''
   if (allocated(self%frame_)) deallocate(self%frame_)
   call system_clock(self%tic_start_)
   self%tic_ = self%tic_start_
   if (self%add_date_time) call date_and_time(date=self%date_time_start_(1:8), time=self%date_time_start_(9:))
   if (self%is_disabled_) return
   if (self%add_scale_bar .and. self%position == 0) call add_scale_bar
   ! lock before the first update: an empty range completes the bar at once, and the update must unlock it
   self%is_stdout_locked_ = self%is_interactive_
   call self%update(current=self%min_value)
   contains
      subroutine add_scale_bar()
      !< Add scale to the bar.
      character(len=:, kind=UCS4), allocatable :: bar       !< Bar line.
      character(len=11)                        :: min_value !< Minimum_value.
      character(len=11)                        :: max_value !< Maximum_value.
      logical                                  :: plain     !< Write without colors.

      plain = .not.self%is_interactive_
      min_value = compact_real(self%min_value, 5_I4P)//' (min)'
      max_value = '(max) '//compact_real(self%max_value, 5_I4P)
      self%scale_bar%string = ucs4_string(min_value//repeat(' ', self%width - len(min_value) - len(max_value))//max_value)
      bar = repeat(UCS4_' ', len(self%prefix%string))//render(self%bracket_left, plain)//render(self%scale_bar, plain)//&
            render(self%bracket_right, plain)
      write(self%output_unit, '(A)') bar
      endsubroutine add_scale_bar
   endsubroutine start

   subroutine update(self, current, message)
   !< Update bar.
   !<
   !< The progress is the fraction of the range done, clamped to [0, 1], truncated to an integer percent: it reaches
   !< 100% only when `current` reaches `max_value`. On a terminal the bar is drawn when the progress enters a new
   !< multiple of `frequency` (at every update with `frequency=1`), at most once every `min_interval` seconds, and
   !< always at 0% and 100%; in a log, a line is written at every 10% (every `frequency` percent if larger than 1).
   !< Once at 100%, the bar is complete and further updates do nothing until the next `start`.
   class(bar_object), intent(inout)        :: self       !< Bar.
   real(R8P),         intent(in)           :: current    !< Current value.
   class(*),          intent(in), optional :: message    !< Message shown at the end of the bar, until the next one.
   integer(I4P)                            :: progress   !< Progress, in percent.
   integer(I4P)                            :: step       !< Progress between two lines of a log, in percent.
   real(R8P)                               :: fraction   !< Fraction of the range done, in [0, 1].
   real(R8P)                               :: elapsed    !< Time elapsed since the last drawing, in seconds.
   integer(I8P)                            :: tic        !< Timer count.
   integer(I8P)                            :: count_rate !< Timer count rate.
   logical                                 :: is_due     !< The bar must be drawn.

   if (self%is_disabled_ .or. self%is_complete_) return
   if (present(message)) self%message%string = ucs4_string(input=message)
   if (self%max_value > self%min_value) then
      fraction = max(0._R8P, min(1._R8P, (current - self%min_value) / (self%max_value - self%min_value)))
   else
      fraction = 1._R8P ! empty range: nothing to do
   endif
   ! truncate, so that 100% means done; the tolerance absorbs round-off, e.g. 20 sums of 0.05 giving 0.999...
   progress = int(fraction * 100._R8P + 1.e-9_R8P, I4P)
   call system_clock(tic, count_rate)
   elapsed = real(tic - self%tic_, kind=R8P) / real(count_rate, kind=R8P)
   if (self%progress_drawn_ < 0 .or. progress == 100) then
      is_due = .true.
   elseif (self%is_interactive_) then
      is_due = (self%frequency <= 1 .or. progress / self%frequency > self%progress_drawn_ / self%frequency) .and. &
               elapsed >= self%min_interval
   else
      step = 10_I4P ; if (self%frequency > 1) step = self%frequency
      is_due = progress / step > self%progress_drawn_ / step
   endif
   if (.not.is_due) return
   call self%update_rate(fraction=fraction, tic=tic, count_rate=count_rate)
   call self%build_frame(progress=progress, fraction=fraction)
   if (self%is_interactive_) then
      call self%draw
   else
      write(self%output_unit, '(A)') self%frame_
      flush(self%output_unit)
   endif
   self%progress_drawn_ = progress
   self%fraction_drawn_ = fraction
   self%tic_ = tic
   if (progress == 100) call self%complete(tic=tic, count_rate=count_rate)
   endsubroutine update

   subroutine write_message(self, message)
   !< Write a message above the bar.
   !<
   !< On a terminal, while the bar runs, the bar line is cleared, the message written in its place and the bar drawn again
   !< on the line below: the message scrolls up with the output, the bar stays at the bottom. Otherwise, the message is
   !< written as a line of the output unit of the bar, also when the bar is disabled.
   class(bar_object), intent(inout) :: self    !< Bar.
   class(*),          intent(in)    :: message !< Message.

   if (self%is_interactive_ .and. self%is_stdout_locked_ .and. allocated(self%frame_)) then
      write(self%output_unit, '(A)', advance='no') ucs4_string(input=CR//ESC//'[2K')//ucs4_string(input=message)//&
                                                   ucs4_string(input=LF)
      call self%draw
   else
      write(self%output_unit, '(A)') ucs4_string(input=message)
      flush(self%output_unit)
   endif
   endsubroutine write_message

   ! private methods
   subroutine build_frame(self, progress, fraction)
   !< Build the frame of the current progress, without control sequences, in `frame_`.
   class(bar_object), intent(inout)         :: self     !< Bar.
   integer(I4P),      intent(in)            :: progress !< Progress, in percent.
   real(R8P),         intent(in)            :: fraction !< Fraction of the range done, in [0, 1].
   character(len=:, kind=UCS4), allocatable :: frame    !< Frame.
   character(len=4)                         :: percent  !< Progress in percent.
   type(element_object)                     :: glyph    !< Partial block.
   logical                                  :: plain    !< Write without colors.
   real(R8P)                                :: cells    !< Filled cells, with their fraction.
   integer(I4P)                             :: full     !< Filled cells.
   integer(I4P)                             :: eighths  !< Eighths of the partially filled cell.
   integer(I4P)                             :: rest     !< Empty cells.

   plain = .not.self%is_interactive_
   frame = render(self%prefix, plain)//render(self%bracket_left, plain)
   if (self%partial_blocks) then
      cells = fraction * self%width
      full = min(self%width, int(cells, I4P))
      eighths = int((cells - full) * 8._R8P, I4P)
      frame = frame//repeat(render(self%filled_char, plain), full)
      rest = self%width - full
      if (eighths > 0 .and. rest > 0) then
         glyph = self%filled_char
         glyph%string = ucs4_string(input=PARTIAL_BLOCKS(eighths))
         glyph%color_bg = self%empty_char%color_bg ! the rest of the cell looks as an empty one
         frame = frame//render(glyph, plain)
         rest = rest - 1
      endif
      frame = frame//repeat(render(self%empty_char, plain), rest)
   else
      full = nint(progress / 100._R8P * self%width)
      frame = frame//repeat(render(self%filled_char, plain), full)//repeat(render(self%empty_char, plain), self%width - full)
   endif
   frame = frame//render(self%bracket_right, plain)//render(self%suffix, plain)
   if (allocated(self%spinner) .and. self%is_interactive_) then
      self%spinner_count_ = self%spinner_count_ + 1
      if (self%spinner_count_ > size(self%spinner, dim=1)) self%spinner_count_ = 1
      frame = frame//render(self%spinner(self%spinner_count_), plain)
   endif
   if (self%add_progress_percent) then
      write(percent, '(I3,A)') progress, '%'
      self%progress_percent%string = ucs4_string(input=percent)
      frame = frame//render(self%progress_percent, plain)
   endif
   if (self%add_progress_count) then
      self%progress_count%string = ucs4_string(input=' '//count_text(self%min_value, self%max_value, fraction))
      frame = frame//render(self%progress_count, plain)
   endif
   if (self%add_progress_speed) then
      self%progress_speed%string = ucs4_string(input=' ('//compact_real(100._R8P * self%rate_, 6_I4P)//'%/s)')
      frame = frame//render(self%progress_speed, plain)
   endif
   if (self%add_eta) then
      if (progress == 100) then
         self%eta%string = ucs4_string(input=' ETA '//hms(0._R8P))
      elseif (self%rate_ > 0._R8P) then
         self%eta%string = ucs4_string(input=' ETA '//hms((1._R8P - fraction) / self%rate_))
      else
         self%eta%string = ucs4_string(input=' ETA --:--:--')
      endif
      frame = frame//render(self%eta, plain)
   endif
   if (len(self%message%string) > 0) frame = frame//UCS4_' '//render(self%message, plain)
   self%frame_ = frame
   endsubroutine build_frame

   subroutine complete(self, tic, count_rate)
   !< Complete the bar: end its line (clear it, at a position below the current line), write date and time and summary.
   class(bar_object), intent(inout) :: self       !< Bar.
   integer(I8P),      intent(in)    :: tic        !< Timer count.
   integer(I8P),      intent(in)    :: count_rate !< Timer count rate.
   character(len=18)                :: date_time  !< Current date/time.
   character(len=12)                :: position   !< Position, as a string.
   real(R8P)                        :: elapsed    !< Time elapsed since the start, in seconds.
   logical                          :: plain      !< Write without colors.

   self%is_complete_ = .true.
   self%is_stdout_locked_ = .false.
   plain = .not.self%is_interactive_
   if (self%is_interactive_) then
      if (self%position > 0) then ! a bar below the current line leaves nothing behind
         write(position, '(I0)') self%position
         write(self%output_unit, '(A)', advance='no') repeat(LF, self%position)//ESC//'[2K'//CR//ESC//'['//trim(position)//'A'
         flush(self%output_unit)
         return
      endif
      write(self%output_unit, '(A)') ESC//'[?25h' ! restore cursor, go to the next line
      write(self%output_unit, '(A)', advance='no') ESC//'[J' ! clear what bars below the current line left
   endif
   if (self%add_date_time) then
      call date_and_time(date=date_time(1:8), time=date_time(9:))
      self%date_time%string = ucs4_string(input='['//self%date_time_start_(1:4)//'/'//self%date_time_start_(5:6)//'/'//  &
                                          self%date_time_start_(7:8)//' '//self%date_time_start_(9:10)//':'//            &
                                          self%date_time_start_(11:12)//':'//self%date_time_start_(13:14)//              &
                                          ' - '//date_time(1:4)//'/'//date_time(5:6)//'/'//date_time(7:8)//              &
                                          ' '//date_time(9:10)//':'//date_time(11:12)//':'//date_time(13:14)//']')
      write(self%output_unit, '(A)') render(self%date_time, plain)
   endif
   if (self%add_summary) then
      elapsed = real(tic - self%tic_start_, kind=R8P) / real(count_rate, kind=R8P)
      if (self%add_progress_count) then
         self%summary%string = ucs4_string(input='[done in '//duration(elapsed)//', '//                                     &
                               trim(adjustl(compact_real(abs(self%max_value - self%min_value) / elapsed, 6_I4P)))//'/s]')
      else
         self%summary%string = ucs4_string(input='[done in '//duration(elapsed)//', '//                                     &
                               trim(adjustl(compact_real(100._R8P / elapsed, 6_I4P)))//'%/s]')
      endif
      write(self%output_unit, '(A)') render(self%summary, plain)
   endif
   flush(self%output_unit)
   endsubroutine complete

   subroutine draw(self)
   !< Draw the last frame on the terminal, on its line, and come back to the current line.
   class(bar_object), intent(inout) :: self     !< Bar.
   character(len=12)                :: position !< Position, as a string.

   if (self%position > 0) then
      write(position, '(I0)') self%position
      write(self%output_unit, '(A)', advance='no') ucs4_string(input=ESC//'[?25l'//repeat(LF, self%position))//self%frame_// &
                                                   ucs4_string(input=ESC//'[K'//CR//ESC//'['//trim(position)//'A')
   else
      write(self%output_unit, '(A)', advance='no') ucs4_string(input=ESC//'[?25l')//self%frame_//ucs4_string(input=ESC//'[K'//CR)
   endif
   flush(self%output_unit)
   endsubroutine draw

   subroutine update_rate(self, fraction, tic, count_rate)
   !< Update the smoothed rate: an exponential moving average of the rate between two drawings, weighted by `smoothing`;
   !< with `smoothing=0`, the average rate since the start.
   class(bar_object), intent(inout) :: self       !< Bar.
   real(R8P),         intent(in)    :: fraction   !< Fraction of the range done, in [0, 1].
   integer(I8P),      intent(in)    :: tic        !< Timer count.
   integer(I8P),      intent(in)    :: count_rate !< Timer count rate.
   real(R8P)                        :: dt         !< Time since the last drawing, in seconds.
   real(R8P)                        :: rate       !< Rate since the last drawing.

   if (self%progress_drawn_ < 0) return ! first drawing: no rate yet
   if (self%smoothing <= 0._R8P) then
      dt = real(tic - self%tic_start_, kind=R8P) / real(count_rate, kind=R8P)
      if (dt > 0._R8P) self%rate_ = fraction / dt
   else
      dt = real(tic - self%tic_, kind=R8P) / real(count_rate, kind=R8P)
      if (dt <= 0._R8P) return
      rate = (fraction - self%fraction_drawn_) / dt
      if (self%rate_samples_ == 0) then
         self%rate_ = rate
      else
         self%rate_ = self%smoothing * rate + (1._R8P - self%smoothing) * self%rate_
      endif
      self%rate_samples_ = self%rate_samples_ + 1
   endif
   endsubroutine update_rate

   subroutine create_spinner(self, string, color_fg, color_bg, style)
   !< Create spinner.
   class(bar_object), intent(inout)         :: self     !< Bar.
   class(*),          intent(in), optional  :: string   !< Spinner char.
   character(len=*),  intent(in), optional  :: color_fg !< Spinner char foreground color.
   character(len=*),  intent(in), optional  :: color_bg !< Spinner char background color.
   character(len=*),  intent(in), optional  :: style    !< Spinner char style.
   character(len=:, kind=UCS4), allocatable :: string_  !< Spinner char, local variable
   ! integer(I4P)                             :: s        !< Counter.

   if (present(string)) then
      string_ = ucs4_string(input=string)
      select case(string_)
      case(UCS4_'|')
         allocate(self%spinner(1:4))
         call self%spinner(1)%initialize(string='|', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='/', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='-', color_fg=color_fg, color_bg=color_bg, style=style)
         ! backslash as achar(92): some compilers (nvfortran) read '\' as an escape in a literal
         call self%spinner(4)%initialize(string=achar(92), color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'⠋')
         allocate(self%spinner(1:10))
         call self%spinner(1 )%initialize(string='⠋', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2 )%initialize(string='⠙', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3 )%initialize(string='⠹', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4 )%initialize(string='⠸', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5 )%initialize(string='⠼', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6 )%initialize(string='⠴', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(7 )%initialize(string='⠦', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(8 )%initialize(string='⠧', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(9 )%initialize(string='⠇', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(10)%initialize(string='⠏', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'⣾')
         allocate(self%spinner(1:8))
         call self%spinner(1)%initialize(string='⣾', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='⣽', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='⣻', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4)%initialize(string='⢿', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5)%initialize(string='⡿', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6)%initialize(string='⣟', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(7)%initialize(string='⣯', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(8)%initialize(string='⣷', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'⠓')
         allocate(self%spinner(1:10))
         call self%spinner(1 )%initialize(string='⠋', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2 )%initialize(string='⠙', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3 )%initialize(string='⠚', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4 )%initialize(string='⠞', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5 )%initialize(string='⠖', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6 )%initialize(string='⠦', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(7 )%initialize(string='⠴', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(8 )%initialize(string='⠲', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(9 )%initialize(string='⠳', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(10)%initialize(string='⠓', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'⠄')
         allocate(self%spinner(1:14))
         call self%spinner(1 )%initialize(string='⠄', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2 )%initialize(string='⠆', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3 )%initialize(string='⠇', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4 )%initialize(string='⠋', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5 )%initialize(string='⠙', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6 )%initialize(string='⠸', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(7 )%initialize(string='⠰', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(8 )%initialize(string='⠠', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(9 )%initialize(string='⠰', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(10)%initialize(string='⠸', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(11)%initialize(string='⠙', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(12)%initialize(string='⠋', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(13)%initialize(string='⠇', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(14)%initialize(string='⠆', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'⠐')
         allocate(self%spinner(1:17))
         call self%spinner(1 )%initialize(string='⠋', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2 )%initialize(string='⠙', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3 )%initialize(string='⠚', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4 )%initialize(string='⠒', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5 )%initialize(string='⠂', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6 )%initialize(string='⠂', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(7 )%initialize(string='⠒', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(8 )%initialize(string='⠲', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(9 )%initialize(string='⠴', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(10)%initialize(string='⠦', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(11)%initialize(string='⠖', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(12)%initialize(string='⠒', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(13)%initialize(string='⠐', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(14)%initialize(string='⠐', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(15)%initialize(string='⠒', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(16)%initialize(string='⠓', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(17)%initialize(string='⠋', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'⠒')
         allocate(self%spinner(1:24))
         call self%spinner(1 )%initialize(string='⠈', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2 )%initialize(string='⠉', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3 )%initialize(string='⠋', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4 )%initialize(string='⠓', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5 )%initialize(string='⠒', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6 )%initialize(string='⠐', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(7 )%initialize(string='⠐', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(8 )%initialize(string='⠒', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(9 )%initialize(string='⠖', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(10)%initialize(string='⠦', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(11)%initialize(string='⠤', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(12)%initialize(string='⠠', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(13)%initialize(string='⠠', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(14)%initialize(string='⠤', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(15)%initialize(string='⠦', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(16)%initialize(string='⠖', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(17)%initialize(string='⠒', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(18)%initialize(string='⠐', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(19)%initialize(string='⠐', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(20)%initialize(string='⠒', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(21)%initialize(string='⠓', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(22)%initialize(string='⠋', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(23)%initialize(string='⠉', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(24)%initialize(string='⠈', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'⠁')
         allocate(self%spinner(1:29))
         call self%spinner(1 )%initialize(string='⠁', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2 )%initialize(string='⠁', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3 )%initialize(string='⠉', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4 )%initialize(string='⠙', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5 )%initialize(string='⠚', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6 )%initialize(string='⠒', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(7 )%initialize(string='⠂', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(8 )%initialize(string='⠂', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(9 )%initialize(string='⠒', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(10)%initialize(string='⠲', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(11)%initialize(string='⠴', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(12)%initialize(string='⠤', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(13)%initialize(string='⠄', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(14)%initialize(string='⠄', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(15)%initialize(string='⠤', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(16)%initialize(string='⠠', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(17)%initialize(string='⠠', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(18)%initialize(string='⠤', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(19)%initialize(string='⠦', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(20)%initialize(string='⠖', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(21)%initialize(string='⠒', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(22)%initialize(string='⠐', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(23)%initialize(string='⠐', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(24)%initialize(string='⠒', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(25)%initialize(string='⠓', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(26)%initialize(string='⠋', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(27)%initialize(string='⠉', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(28)%initialize(string='⠈', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(29)%initialize(string='⠈', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'⣸')
         allocate(self%spinner(1:8))
         call self%spinner(1)%initialize(string='⢹', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='⢺', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='⢼', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4)%initialize(string='⣸', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5)%initialize(string='⣇', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6)%initialize(string='⡧', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(7)%initialize(string='⡗', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(8)%initialize(string='⡏', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'⡐')
         allocate(self%spinner(1:7))
         call self%spinner(1)%initialize(string='⢄', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='⢂', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='⢁', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4)%initialize(string='⡁', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5)%initialize(string='⡈', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6)%initialize(string='⡐', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(7)%initialize(string='⡠', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'⡀')
         allocate(self%spinner(1:8))
         call self%spinner(1)%initialize(string='⠁', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='⠂', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='⠄', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4)%initialize(string='⡀', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5)%initialize(string='⢀', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6)%initialize(string='⠠', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(7)%initialize(string='⠐', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(8)%initialize(string='⠈', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'⡃⢐')
         allocate(self%spinner(1:56))
         call self%spinner(1 )%initialize(string='⢀⠀', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2 )%initialize(string='⡀⠀', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3 )%initialize(string='⠄⠀', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4 )%initialize(string='⢂⠀', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5 )%initialize(string='⡂⠀', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6 )%initialize(string='⠅⠀', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(7 )%initialize(string='⢃⠀', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(8 )%initialize(string='⡃⠀', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(9 )%initialize(string='⠍⠀', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(10)%initialize(string='⢋⠀', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(11)%initialize(string='⡋⠀', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(12)%initialize(string='⠍⠁', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(13)%initialize(string='⢋⠁', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(14)%initialize(string='⡋⠁', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(15)%initialize(string='⠍⠉', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(16)%initialize(string='⠋⠉', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(17)%initialize(string='⠋⠉', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(18)%initialize(string='⠉⠙', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(19)%initialize(string='⠉⠙', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(20)%initialize(string='⠉⠩', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(21)%initialize(string='⠈⢙', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(22)%initialize(string='⠈⡙', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(23)%initialize(string='⢈⠩', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(24)%initialize(string='⡀⢙', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(25)%initialize(string='⠄⡙', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(26)%initialize(string='⢂⠩', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(27)%initialize(string='⡂⢘', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(28)%initialize(string='⠅⡘', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(29)%initialize(string='⢃⠨', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(30)%initialize(string='⡃⢐', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(31)%initialize(string='⠍⡐', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(32)%initialize(string='⢋⠠', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(33)%initialize(string='⡋⢀', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(34)%initialize(string='⠍⡁', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(35)%initialize(string='⢋⠁', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(36)%initialize(string='⡋⠁', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(37)%initialize(string='⠍⠉', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(38)%initialize(string='⠋⠉', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(39)%initialize(string='⠋⠉', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(40)%initialize(string='⠉⠙', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(41)%initialize(string='⠉⠙', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(42)%initialize(string='⠉⠩', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(43)%initialize(string='⠈⢙', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(44)%initialize(string='⠈⡙', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(45)%initialize(string='⠈⠩', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(46)%initialize(string='⠀⢙', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(47)%initialize(string='⠀⡙', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(48)%initialize(string='⠀⠩', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(49)%initialize(string='⠀⢘', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(50)%initialize(string='⠀⡘', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(51)%initialize(string='⠀⠨', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(52)%initialize(string='⠀⢐', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(53)%initialize(string='⠀⡐', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(54)%initialize(string='⠀⠠', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(55)%initialize(string='⠀⢀', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(56)%initialize(string='⠀⡀', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'┤')
         allocate(self%spinner(1:8))
         call self%spinner(1)%initialize(string='┤', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='┘', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='┴', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4)%initialize(string='└', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5)%initialize(string='├', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6)%initialize(string='┌', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(7)%initialize(string='┬', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(8)%initialize(string='┐', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'✶')
         allocate(self%spinner(1:6))
         call self%spinner(1)%initialize(string='✶', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='✸', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='✹', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4)%initialize(string='✺', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5)%initialize(string='✹', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6)%initialize(string='✷', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'_')
         allocate(self%spinner(1:12))
         call self%spinner(1 )%initialize(string='_', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2 )%initialize(string='_', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3 )%initialize(string='_', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4 )%initialize(string='-', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5 )%initialize(string='`', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6 )%initialize(string='`', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(7 )%initialize(string="'", color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(8 )%initialize(string='´', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(9 )%initialize(string='-', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(10)%initialize(string='_', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(11)%initialize(string='_', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(12)%initialize(string='_', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'▃')
         allocate(self%spinner(1:10))
         call self%spinner(1 )%initialize(string='▁', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2 )%initialize(string='▃', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3 )%initialize(string='▄', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4 )%initialize(string='▅', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5 )%initialize(string='▆', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6 )%initialize(string='▇', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(7 )%initialize(string='▆', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(8 )%initialize(string='▅', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(9 )%initialize(string='▄', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(10)%initialize(string='▃', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'▉')
         allocate(self%spinner(1:12))
         call self%spinner(1 )%initialize(string='▏', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2 )%initialize(string='▎', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3 )%initialize(string='▍', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4 )%initialize(string='▌', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5 )%initialize(string='▋', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6 )%initialize(string='▊', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(7 )%initialize(string='▉', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(8 )%initialize(string='▊', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(9 )%initialize(string='▋', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(10)%initialize(string='▌', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(11)%initialize(string='▍', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(12)%initialize(string='▎', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'@')
         allocate(self%spinner(1:7))
         call self%spinner(1)%initialize(string=' ', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='.', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='o', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4)%initialize(string='O', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5)%initialize(string='@', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6)%initialize(string='*', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(7)%initialize(string=' ', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'°')
         allocate(self%spinner(1:7))
         call self%spinner(1)%initialize(string='.', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='o', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='O', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4)%initialize(string='°', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5)%initialize(string='O', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6)%initialize(string='o', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(7)%initialize(string='.', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'▒')
         allocate(self%spinner(1:3))
         call self%spinner(1)%initialize(string='▓', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='▒', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='░', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'⠂')
         allocate(self%spinner(1:4))
         call self%spinner(1)%initialize(string='⠁', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='⠂', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='⠄', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4)%initialize(string='⠂', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'▖')
         allocate(self%spinner(1:4))
         call self%spinner(1)%initialize(string='▖', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='▘', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='▝', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4)%initialize(string='▗', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'◢')
         allocate(self%spinner(1:4))
         call self%spinner(1)%initialize(string='◢', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='◣', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='◤', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4)%initialize(string='◥', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'◜')
         allocate(self%spinner(1:6))
         call self%spinner(1)%initialize(string='◜', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='◠', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='◝', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4)%initialize(string='◞', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5)%initialize(string='◡', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6)%initialize(string='◟', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'⊙')
         allocate(self%spinner(1:3))
         call self%spinner(1)%initialize(string='◡', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='⊙', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='◠', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'◰')
         allocate(self%spinner(1:4))
         call self%spinner(1)%initialize(string='◰', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='◳', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='◲', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4)%initialize(string='◱', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'◴')
         allocate(self%spinner(1:4))
         call self%spinner(1)%initialize(string='◴', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='◷', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='◶', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4)%initialize(string='◵', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'◐')
         allocate(self%spinner(1:4))
         call self%spinner(1)%initialize(string='◐', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='◓', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='◑', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4)%initialize(string='◒', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'⊶')
         allocate(self%spinner(1:2))
         call self%spinner(1)%initialize(string='⊶', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='⊷', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'▫')
         allocate(self%spinner(1:2))
         call self%spinner(1)%initialize(string='▫', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='▪', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'□')
         allocate(self%spinner(1:2))
         call self%spinner(1)%initialize(string='□', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='■', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'▪')
         allocate(self%spinner(1:4))
         call self%spinner(1)%initialize(string='■', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='□', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='▪', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4)%initialize(string='▫', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'▯')
         allocate(self%spinner(1:2))
         call self%spinner(1)%initialize(string='▮', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='▯', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'⦿')
         allocate(self%spinner(1:2))
         call self%spinner(1)%initialize(string='⦾', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='⦿', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'◍')
         allocate(self%spinner(1:2))
         call self%spinner(1)%initialize(string='◍', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='◌', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'◉')
         allocate(self%spinner(1:2))
         call self%spinner(1)%initialize(string='◉', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='◎', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'㊂')
         allocate(self%spinner(1:3))
         call self%spinner(1)%initialize(string='㊂', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='㊀', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='㊁', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'(  ●   )')
         allocate(self%spinner(1:10))
         call self%spinner(1 )%initialize(string='( ●    )', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2 )%initialize(string='(  ●   )', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3 )%initialize(string='(   ●  )', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4 )%initialize(string='(    ● )', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5 )%initialize(string='(     ●)', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6 )%initialize(string='(    ● )', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(7 )%initialize(string='(   ●  )', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(8 )%initialize(string='(  ●   )', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(9 )%initialize(string='( ●    )', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(10)%initialize(string='(●     )', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'🌔 ')
         allocate(self%spinner(1:8))
         call self%spinner(1)%initialize(string='🌑 ', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='🌒 ', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(3)%initialize(string='🌓 ', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(4)%initialize(string='🌔 ', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(5)%initialize(string='🌕 ', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(6)%initialize(string='🌖 ', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(7)%initialize(string='🌗 ', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(8)%initialize(string='🌘 ', color_fg=color_fg, color_bg=color_bg, style=style)
      case(UCS4_'🚶 ')
         allocate(self%spinner(1:2))
         call self%spinner(1)%initialize(string='🚶 ', color_fg=color_fg, color_bg=color_bg, style=style)
         call self%spinner(2)%initialize(string='🏃 ', color_fg=color_fg, color_bg=color_bg, style=style)
      endselect
   endif
   endsubroutine create_spinner

   ! non type-bound procedures
   pure function compact_real(x, w) result(compact)
   !< Return a real number in exactly `w` characters, right-aligned: with two decimals while they fit, then one, then
   !< as an integer, then as `<m.m>e<n>` or `<m>e<n>`; `nan` and `inf` for non finite numbers.
   !<
   !< A fixed width keeps the bar line from shrinking: a shorter drawing would leave the end of the previous one on
   !< screen. With `w >= 6` every finite number fits; with `w = 5`, every one above -1e100 (else `*****`).
   real(R8P),    intent(in) :: x       !< Number.
   integer(I4P), intent(in) :: w       !< Width.
   character(len=w)         :: compact !< Number in `w` characters.
   character(len=32)        :: buffer  !< Formatting buffer.
   real(R8P)                :: mantissa !< Mantissa of the scientific form.
   integer(I4P)             :: m        !< Mantissa of the scientific form, one significant digit.
   integer(I4P)             :: e        !< Exponent of the scientific form.

   if (ieee_is_nan(x)) then
      buffer = 'nan'
   elseif (.not.ieee_is_finite(x)) then
      buffer = 'inf'
      if (x < 0._R8P) buffer = '-inf'
   else
      buffer = repeat('*', len(buffer)) ! not fitting, unless a form below fits
      if (abs(x) < 1.e9_R8P) then      ! the fixed forms fit the buffer: F32.d, not F0.d, keeps the leading zero
         write(buffer, '(F32.2)') x ; buffer = adjustl(buffer)
         if (len_trim(buffer) > w) then
            write(buffer, '(F32.1)') x ; buffer = adjustl(buffer)
         endif
         if (len_trim(buffer) > w) write(buffer, '(I0)') nint(x, I8P)
      endif
      if (len_trim(buffer) > w) then
         e = floor(log10(abs(x)), I4P)
         mantissa = x / 10._R8P**e
         if (abs(nint(mantissa * 10._R8P)) >= 100) then ! e.g. 9.96e6 rounds to 10.0e6
            mantissa = mantissa / 10._R8P
            e = e + 1
         endif
         write(buffer, '(F0.1,A,I0)') mantissa, 'e', e
         if (len_trim(buffer) > w) then ! one significant digit only
            m = nint(mantissa, I4P)
            if (abs(m) == 10) then
               m = sign(1_I4P, m)
               e = e + 1
            endif
            write(buffer, '(I0,A,I0)') m, 'e', e
         endif
      endif
   endif
   if (len_trim(buffer) > w) buffer = repeat('*', w) ! never truncate a number into a wrong one
   compact = adjustr(buffer(1:w))
   endfunction compact_real

   pure function count_text(min_value, max_value, fraction) result(text)
   !< Return the progress count, `current/maximum`: integers when the range bounds are, else in the compact real form.
   real(R8P), intent(in)         :: min_value !< Minimum value.
   real(R8P), intent(in)         :: max_value !< Maximum value.
   real(R8P), intent(in)         :: fraction  !< Fraction of the range done, in [0, 1].
   character(len=:), allocatable :: text      !< Progress count.
   character(len=24)             :: current   !< Current value.
   character(len=24)             :: maximum   !< Maximum value.
   character(len=24)             :: minimum   !< Minimum value.
   real(R8P)                     :: value     !< Current value, clamped to the range.
   integer(I4P)                  :: w         !< Width of the current value.

   value = min_value + fraction * (max_value - min_value)
   if (abs(min_value) < 1.e9_R8P .and. abs(max_value) < 1.e9_R8P .and. &
       min_value == aint(min_value) .and. max_value == aint(max_value)) then
      write(maximum, '(I0)') nint(max_value, I8P)
      write(minimum, '(I0)') nint(min_value, I8P)
      write(current, '(I0)') floor(value + 1.e-9_R8P, I8P) ! truncated, as the percent
      w = max(len_trim(maximum), len_trim(minimum))
      text = repeat(' ', max(0, w - len_trim(current)))//trim(current)//'/'//trim(maximum)
   else
      text = compact_real(value, 6_I4P)//'/'//trim(adjustl(compact_real(max_value, 6_I4P)))
   endif
   endfunction count_text

   pure function duration(seconds) result(text)
   !< Return a duration for people: seconds below a minute (`2.53 s`), else `hh:mm:ss`.
   real(R8P), intent(in)         :: seconds !< Duration, in seconds.
   character(len=:), allocatable :: text    !< Duration.

   if (seconds < 60._R8P) then
      text = trim(adjustl(compact_real(seconds, 6_I4P)))//' s'
   else
      text = hms(seconds)
   endif
   endfunction duration

   function get_environment(name, value) result(is_set)
   !< Return true if the environment variable is set to a non empty value, and its value.
   character(len=*),              intent(in)  :: name   !< Name of the variable.
   character(len=:), allocatable, intent(out) :: value  !< Value of the variable.
   logical                                    :: is_set !< The variable is set, not empty.
   integer                                    :: length !< Length of the value.
   integer                                    :: status !< Status of the query.

   call get_environment_variable(name, length=length, status=status)
   is_set = status == 0 .and. length > 0
   if (is_set) then
      allocate(character(len=length) :: value)
      call get_environment_variable(name, value=value)
   else
      value = ''
   endif
   endfunction get_environment

   pure function hms(seconds) result(text)
   !< Return a duration in exactly 8 characters: `hh:mm:ss` below 100 hours, else days (`12.5 d`), right-aligned.
   real(R8P), intent(in) :: seconds !< Duration, in seconds.
   character(len=8)      :: text    !< Duration.
   integer(I8P)          :: s       !< Whole seconds.

   if (.not.ieee_is_finite(seconds) .or. seconds < 0._R8P) then
      text = '--:--:--'
   elseif (seconds < 360000._R8P) then
      s = nint(seconds, I8P)
      write(text, '(I2.2,A,I2.2,A,I2.2)') s / 3600, ':', mod(s / 60, 60_I8P), ':', mod(s, 60_I8P)
   else
      text = adjustr(trim(adjustl(compact_real(seconds / 86400._R8P, 5_I4P)))//' d')
   endif
   endfunction hms

   function is_terminal(unit)
   !< Return true if the unit is the standard output or error and that is a terminal.
   integer(I4P), intent(in) :: unit        !< Unit.
   logical                  :: is_terminal !< The unit is a terminal.

   is_terminal = .false.
   if (unit == stdout) then
      is_terminal = isatty(1_c_int) /= 0
   elseif (unit == stderr) then
      is_terminal = isatty(2_c_int) /= 0
   endif
   endfunction is_terminal

   pure function render(element, plain) result(text)
   !< Return an element, with its colors and style unless plain.
   type(element_object), intent(in)         :: element !< Element.
   logical,              intent(in)         :: plain   !< Without colors and style.
   character(len=:, kind=UCS4), allocatable :: text    !< Rendered element.

   if (.not.allocated(element%string)) then
      text = UCS4_''
   elseif (plain) then
      text = element%string
   else
      text = element%output()
   endif
   endfunction render
endmodule forbear_bar_object
