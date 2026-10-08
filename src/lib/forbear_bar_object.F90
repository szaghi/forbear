!< **forbear** project, definition of [[bar_object]].

module forbear_bar_object
!< **forbear** project, definition of [[bar_object]].
use, intrinsic :: iso_c_binding, only : c_int
use, intrinsic :: iso_fortran_env, only : I4P=>int32, I8P=>int64, R8P=>real64, stdout=>output_unit, stderr=>error_unit
use, intrinsic :: ieee_arithmetic, only : ieee_is_finite, ieee_is_nan
use forbear_element_object, only : element_object, is_color, is_style
use forbear_field_object, only : field_object, progress_object
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

! kinds of the tokens of a template: text, or one of the fields
integer(I4P), parameter :: TOKEN_TEXT    = 0  !< Literal text.
integer(I4P), parameter :: TOKEN_BAR     = 1  !< The bar body.
integer(I4P), parameter :: TOKEN_SPINNER = 2  !< The spinner.
integer(I4P), parameter :: TOKEN_PERCENT = 3  !< The progress in percent.
integer(I4P), parameter :: TOKEN_COUNT   = 4  !< The progress count.
integer(I4P), parameter :: TOKEN_SPEED   = 5  !< The progress speed.
integer(I4P), parameter :: TOKEN_ETA     = 6  !< The estimated time of arrival.
integer(I4P), parameter :: TOKEN_ELAPSED = 7  !< The time elapsed since the start.
integer(I4P), parameter :: TOKEN_MESSAGE = 8  !< The message of the last update.
integer(I4P), parameter :: TOKEN_PREFIX  = 9  !< The prefix string.
integer(I4P), parameter :: TOKEN_SUFFIX  = 10 !< The suffix string.
integer(I4P), parameter :: TOKEN_FIELD   = 11 !< A field added by the program.

type :: token_object
   !< A piece of the bar line: literal text, or a field with its colours.
   integer(I4P)                  :: kind = TOKEN_TEXT !< Kind of token.
   logical                       :: decorated = .false. !< Field with its own separators (layout without template).
   character(len=:), allocatable :: name               !< Name of a field added by the program.
   integer(I4P)                  :: field = 0          !< Index of a field added by the program.
   type(element_object)          :: style              !< Text (of a literal) and colours.
endtype token_object

type :: field_entry
   !< A field added by the program, with its name.
   character(len=:),    allocatable :: name  !< Name, as written in the template.
   class(field_object), allocatable :: field !< Field.
endtype field_entry

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
   logical                           :: indeterminate        !< The total is unknown: `current` counts what is done.
   logical                           :: is_interactive_      !< Flag set when the bar is drawn on a terminal.
   logical                           :: is_disabled_         !< Flag set when the bar draws nothing.
   logical                           :: hide_cursor          !< Hide the cursor while the bar runs on a terminal.
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
   logical                                  :: is_complete_ = .false.   !< Flag set when the bar has ended: 100%, finish.
   real(R8P)                                :: current_ = 0._R8P        !< Current value of the last update.
   integer(I4P)                             :: pulse_ = 0               !< Drawings of an indeterminate bar body.
   character(len=:, kind=UCS4), allocatable :: frame_                   !< Last frame drawn, without control sequences.
   ! layout
   type(token_object), allocatable          :: tokens_(:)                !< Tokens of the bar line.
   type(field_entry),  allocatable          :: fields_(:)                !< Fields added by the program.
   logical                                  :: has_template_ = .false.   !< The layout comes from a template.
   character(len=:),   allocatable          :: template_                 !< Template, as given.
   contains
      ! public methods
      procedure, pass(self) :: add_field              !< Add a field defined by the program.
      procedure, pass(self) :: destroy                !< Destroy bar.
      procedure, pass(self) :: finish                 !< End the bar where it is.
      procedure, pass(self) :: initialize             !< Initialize bar.
      procedure, pass(self) :: is_stdout_locked       !< Return status of standard output unit.
      procedure, pass(self) :: start                  !< Start bar.
      procedure, pass(self) :: update                 !< Update bar.
      procedure, pass(self) :: write => write_message !< Write a message above the bar.
      ! private methods
      procedure, pass(self), private :: add_token      !< Add a token to the layout.
      procedure, pass(self), private :: bar_body       !< Return the body of the bar.
      procedure, pass(self), private :: build_frame    !< Build the frame of the current progress.
      procedure, pass(self), private :: default_layout !< Build the layout the keywords describe.
      procedure, pass(self), private :: parse_template !< Build the layout of a template.
      procedure, pass(self), private :: measure        !< Return the progress of a current value.
      procedure, pass(self), private :: progress_state !< Return the progress, for the fields of the program.
      procedure, pass(self), private :: resolve_fields !< Find the fields of the program named by the template.
      procedure, pass(self), private :: width_before_bar !< Return the columns of the layout before the bar body.
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
   self%indeterminate = .false.
   self%is_interactive_ = .false.
   self%is_disabled_ = .false.
   self%hide_cursor = .true.
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
   self%current_ = 0._R8P
   self%pulse_ = 0_I4P
   if (allocated(self%frame_)) deallocate(self%frame_)
   if (allocated(self%tokens_)) deallocate(self%tokens_)
   if (allocated(self%fields_)) deallocate(self%fields_)
   self%has_template_ = .false.
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
                         interactive, disabled, hide_cursor, template, indeterminate, output_unit)
   !< Initialize bar.
   !<
   !< Every setting not passed takes its default. The display mode is resolved here: `interactive` if passed, else the
   !< environment variable `FORBEAR_INTERACTIVE` (0 or 1) if set, else whether `output_unit` is a terminal. The
   !< environment variable `FORBEAR_DISABLE` (any value but 0) disables every bar; `FORBEAR_MIN_INTERVAL` replaces the
   !< default of `min_interval`. With `indeterminate`, the total is unknown: `current` counts what is done, from
   !< `min_value`, and the bar ends with `finish`; it has no percent, ETA or scale (asking for them stops the program).
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
   logical,           intent(in), optional  :: hide_cursor               !< Hide the cursor while the bar runs.
   character(len=*),  intent(in), optional  :: template                  !< Layout of the bar line.
   logical,           intent(in), optional  :: indeterminate             !< The total is unknown.
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
   if (present(hide_cursor)) self%hide_cursor = hide_cursor
   if (present(indeterminate)) self%indeterminate = indeterminate
   if (get_environment('FORBEAR_DISABLE', env)) then
      if (trim(env) /= '0') self%is_disabled_ = .true.
   endif
   ! a log cannot come back to a line below: only the bar on the current line is logged
   if (.not.self%is_interactive_ .and. self%position > 0) self%is_disabled_ = .true.

   if (present(template)) then
      call self%parse_template(template)
   else
      call self%default_layout
   endif

   if (self%indeterminate) then ! nothing to measure against: no percent, no ETA, no scale
      if (self%add_scale_bar .or. any(self%tokens_%kind == TOKEN_PERCENT) .or. any(self%tokens_%kind == TOKEN_ETA)) then
         write(stderr, '(A)') 'forbear: an indeterminate bar has no percent, ETA or scale, '// &
                              'see https://szaghi.github.io/forbear/guide/bar#initialize'
         error stop 'forbear: indeterminate bar with a percent, an ETA or a scale'
      endif
   endif
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

   call self%resolve_fields
   self%progress_drawn_ = -1_I4P
   self%fraction_drawn_ = 0._R8P
   self%rate_ = 0._R8P
   self%rate_samples_ = 0_I4P
   self%spinner_count_ = 0_I4P
   self%pulse_ = 0_I4P
   self%is_complete_ = .false.
   self%current_ = self%min_value
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
      if (self%has_template_) then ! the scale right above the bar body
         bar = repeat(UCS4_' ', self%width_before_bar())//render(self%scale_bar, plain)
      else
         bar = repeat(UCS4_' ', display_width(self%prefix%string))//render(self%bracket_left, plain)//&
               render(self%scale_bar, plain)//render(self%bracket_right, plain)
      endif
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
   !< Once at 100%, the bar is complete and further updates do nothing until the next `start`. An indeterminate bar is
   !< drawn at most once every `min_interval` seconds on a terminal, at the start and the end only in a log, and never
   !< completes: `finish` ends it.
   class(bar_object), intent(inout)        :: self       !< Bar.
   real(R8P),         intent(in)           :: current    !< Current value.
   class(*),          intent(in), optional :: message    !< Message shown at the end of the bar, until the next one.
   integer(I4P)                            :: progress   !< Progress, in percent.
   integer(I4P)                            :: step       !< Progress between two lines of a log, in percent.
   real(R8P)                               :: fraction   !< Fraction of the range done, in [0, 1]; done, if indeterminate.
   real(R8P)                               :: elapsed    !< Time elapsed since the last drawing, in seconds.
   integer(I8P)                            :: tic        !< Timer count.
   integer(I8P)                            :: count_rate !< Timer count rate.
   logical                                 :: is_due     !< The bar must be drawn.

   if (self%is_disabled_ .or. self%is_complete_) return
   if (present(message)) self%message%string = ucs4_string(input=message)
   self%current_ = current
   call self%measure(current=current, fraction=fraction, progress=progress)
   call system_clock(tic, count_rate)
   elapsed = real(tic - self%tic_, kind=R8P) / real(count_rate, kind=R8P)
   if (self%progress_drawn_ < 0 .or. progress == 100) then
      is_due = .true.
   elseif (self%indeterminate) then
      is_due = self%is_interactive_ .and. elapsed >= self%min_interval
   elseif (self%is_interactive_) then
      is_due = (self%frequency <= 1 .or. progress / self%frequency > self%progress_drawn_ / self%frequency) .and. &
               elapsed >= self%min_interval
   else
      step = 10_I4P ; if (self%frequency > 1) step = self%frequency
      is_due = progress / step > self%progress_drawn_ / step
   endif
   if (.not.is_due) return
   call self%update_rate(fraction=fraction, tic=tic, count_rate=count_rate)
   call self%build_frame(progress=progress, fraction=fraction, &
                         elapsed=real(tic - self%tic_start_, kind=R8P) / real(count_rate, kind=R8P))
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

   subroutine finish(self, message)
   !< End the bar where it is: draw the progress of the last update, end the line, write date and time and summary, as
   !< at 100%. It ends an indeterminate bar, or a loop left before its end; further updates do nothing until the next
   !< `start`. A bar not running (not started, complete, disabled) is left as it is.
   class(bar_object), intent(inout)        :: self       !< Bar.
   class(*),          intent(in), optional :: message    !< Message shown at the end of the bar.
   integer(I4P)                            :: progress   !< Progress, in percent.
   real(R8P)                               :: fraction   !< Fraction of the range done, in [0, 1]; done, if indeterminate.
   integer(I8P)                            :: tic        !< Timer count.
   integer(I8P)                            :: count_rate !< Timer count rate.

   if (self%is_disabled_ .or. self%is_complete_ .or. .not.allocated(self%frame_)) return
   if (present(message)) self%message%string = ucs4_string(input=message)
   call self%measure(current=self%current_, fraction=fraction, progress=progress)
   call system_clock(tic, count_rate)
   self%is_complete_ = .true. ! the last frame: an indeterminate bar body is drawn full
   call self%update_rate(fraction=fraction, tic=tic, count_rate=count_rate)
   call self%build_frame(progress=progress, fraction=fraction, &
                         elapsed=real(tic - self%tic_start_, kind=R8P) / real(count_rate, kind=R8P))
   if (self%is_interactive_) then
      call self%draw
   elseif (fraction /= self%fraction_drawn_ .or. present(message)) then ! a log line, unless the last one says it
      write(self%output_unit, '(A)') self%frame_
   endif
   self%progress_drawn_ = progress
   self%fraction_drawn_ = fraction
   self%tic_ = tic
   call self%complete(tic=tic, count_rate=count_rate)
   endsubroutine finish

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
   subroutine build_frame(self, progress, fraction, elapsed)
   !< Build the frame of the current progress, without control sequences, in `frame_`: the tokens of the layout, in order.
   class(bar_object), intent(inout)         :: self     !< Bar.
   integer(I4P),      intent(in)            :: progress !< Progress, in percent.
   real(R8P),         intent(in)            :: fraction !< Fraction of the range done, in [0, 1].
   real(R8P),         intent(in)            :: elapsed  !< Time since the start, in seconds.
   character(len=:, kind=UCS4), allocatable :: frame    !< Frame.
   character(len=:),            allocatable :: text     !< Text of a field.
   character(len=4)                         :: percent  !< Progress in percent.
   logical                                  :: plain    !< Write without colors.
   integer(I4P)                             :: t        !< Counter.

   plain = .not.self%is_interactive_
   frame = UCS4_''
   do t = 1, size(self%tokens_, dim=1)
      associate(token => self%tokens_(t))
      select case(token%kind)
      case(TOKEN_TEXT, TOKEN_PREFIX, TOKEN_SUFFIX)
         frame = frame//render(token%style, plain)
      case(TOKEN_BAR)
         if (self%indeterminate .and. self%is_interactive_) self%pulse_ = self%pulse_ + 1
         frame = frame//self%bar_body(progress=progress, fraction=fraction, plain=plain)
      case(TOKEN_SPINNER)
         if (self%is_interactive_) then ! a spinner has no meaning in a log
            self%spinner_count_ = self%spinner_count_ + 1
            if (self%spinner_count_ > size(self%spinner, dim=1)) self%spinner_count_ = 1
            token%style%string = self%spinner(self%spinner_count_)%string
            frame = frame//render(token%style, plain)
         endif
      case(TOKEN_PERCENT)
         write(percent, '(I3,A)') progress, '%'
         text = percent ; if (token%decorated) text = ' '//text ! the space keeps 100% apart from what precedes it
         frame = frame//styled(token, text, plain)
      case(TOKEN_COUNT)
         if (self%indeterminate) then
            text = done_text(self%min_value, fraction)
         else
            text = count_text(self%min_value, self%max_value, fraction)
         endif
         if (token%decorated) text = ' '//text
         frame = frame//styled(token, text, plain)
      case(TOKEN_SPEED)
         if (self%indeterminate) then ! what is done per second
            text = compact_real(self%rate_, 6_I4P) ; if (token%decorated) text = ' ('//text//'/s)'
         else
            text = compact_real(100._R8P * self%rate_, 6_I4P) ; if (token%decorated) text = ' ('//text//'%/s)'
         endif
         frame = frame//styled(token, text, plain)
      case(TOKEN_ETA)
         if (progress == 100) then
            text = hms(0._R8P)
         elseif (self%rate_ > 0._R8P) then
            text = hms((1._R8P - fraction) / self%rate_)
         else
            text = '--:--:--'
         endif
         if (token%decorated) text = ' ETA '//text
         frame = frame//styled(token, text, plain)
      case(TOKEN_ELAPSED)
         frame = frame//styled(token, hms(elapsed), plain)
      case(TOKEN_MESSAGE)
         token%style%string = self%message%string
         if (.not.token%decorated) then
            frame = frame//render(token%style, plain)
         elseif (len(self%message%string) > 0) then
            frame = frame//UCS4_' '//render(token%style, plain)
         endif
      case(TOKEN_FIELD)
         text = self%fields_(token%field)%field%render(self%progress_state(fraction=fraction, progress=progress, &
                                                                             elapsed=elapsed))
         frame = frame//styled(token, text, plain)
      endselect
      endassociate
   enddo
   self%frame_ = frame
   endsubroutine build_frame

   function bar_body(self, progress, fraction, plain) result(body)
   !< Return the body of the bar: the done part, the partial block, the remaining part.
   class(bar_object), intent(in)            :: self     !< Bar.
   integer(I4P),      intent(in)            :: progress !< Progress, in percent.
   real(R8P),         intent(in)            :: fraction !< Fraction of the range done, in [0, 1].
   logical,           intent(in)            :: plain    !< Write without colors.
   character(len=:, kind=UCS4), allocatable :: body     !< Body of the bar.
   type(element_object)                     :: glyph    !< Partial block.
   real(R8P)                                :: cells    !< Filled cells, with their fraction.
   integer(I4P)                             :: full     !< Filled cells.
   integer(I4P)                             :: eighths  !< Eighths of the partially filled cell.
   integer(I4P)                             :: rest     !< Empty cells.
   integer(I4P)                             :: block    !< Cells of the block of an indeterminate bar.
   integer(I4P)                             :: travel   !< Positions of the block, after the first.
   integer(I4P)                             :: k        !< Phase of the block, in a back and forth.

   if (self%indeterminate) then ! a block going back and forth, one cell per drawing; full at the end, empty in a log
      if (self%is_complete_) then
         body = repeat(render(self%filled_char, plain), self%width)
      elseif (plain) then
         body = repeat(render(self%empty_char, plain), self%width)
      else
         block = min(self%width, max(1_I4P, self%width / 4_I4P))
         travel = self%width - block
         k = 0 ; if (travel > 0) k = mod(self%pulse_ - 1_I4P, 2_I4P * travel)
         if (k > travel) k = 2_I4P * travel - k
         body = repeat(render(self%empty_char, plain), k)//repeat(render(self%filled_char, plain), block)// &
                repeat(render(self%empty_char, plain), travel - k)
      endif
   elseif (self%partial_blocks) then
      cells = fraction * self%width
      full = min(self%width, int(cells, I4P))
      eighths = int((cells - full) * 8._R8P, I4P)
      body = repeat(render(self%filled_char, plain), full)
      rest = self%width - full
      if (eighths > 0 .and. rest > 0) then
         glyph = self%filled_char
         glyph%string = ucs4_string(input=PARTIAL_BLOCKS(eighths))
         glyph%color_bg = self%empty_char%color_bg ! the rest of the cell looks as an empty one
         body = body//render(glyph, plain)
         rest = rest - 1
      endif
      body = body//repeat(render(self%empty_char, plain), rest)
   else
      full = nint(progress / 100._R8P * self%width)
      body = repeat(render(self%filled_char, plain), full)//repeat(render(self%empty_char, plain), self%width - full)
   endif
   endfunction bar_body

   pure subroutine measure(self, current, fraction, progress)
   !< Return the progress of a current value: the fraction of the range done, clamped to [0, 1], and the percent,
   !< truncated; for an indeterminate bar, what is done since `min_value` (not below 0), and 0%.
   class(bar_object), intent(in)  :: self     !< Bar.
   real(R8P),         intent(in)  :: current  !< Current value.
   real(R8P),         intent(out) :: fraction !< Fraction of the range done; done, if indeterminate.
   integer(I4P),      intent(out) :: progress !< Progress, in percent.

   if (self%indeterminate) then
      fraction = max(0._R8P, current - self%min_value)
      progress = 0_I4P
      return
   endif
   if (self%max_value > self%min_value) then
      fraction = max(0._R8P, min(1._R8P, (current - self%min_value) / (self%max_value - self%min_value)))
   else
      fraction = 1._R8P ! empty range: nothing to do
   endif
   ! truncate, so that 100% means done; the tolerance absorbs round-off, e.g. 20 sums of 0.05 giving 0.999...
   progress = int(fraction * 100._R8P + 1.e-9_R8P, I4P)
   endsubroutine measure

   function progress_state(self, fraction, progress, elapsed) result(state)
   !< Return the progress of the bar, for the fields of the program.
   class(bar_object), intent(in) :: self     !< Bar.
   real(R8P),         intent(in) :: fraction !< Fraction of the range done, in [0, 1].
   integer(I4P),      intent(in) :: progress !< Progress, in percent.
   real(R8P),         intent(in) :: elapsed  !< Time since the start, in seconds.
   type(progress_object)         :: state    !< Progress.

   if (self%indeterminate) then ! the rate is what is done per second; no fraction, no ETA
      state%indeterminate = .true.
      state%current = self%min_value + fraction
      state%min_value = self%min_value
      state%max_value = self%max_value
      state%rate = self%rate_
      state%elapsed = elapsed
      return
   endif
   state%current = self%min_value + fraction * (self%max_value - self%min_value)
   state%min_value = self%min_value
   state%max_value = self%max_value
   state%fraction = fraction
   state%percent = progress
   state%rate = self%rate_
   state%elapsed = elapsed
   if (progress == 100) then
      state%eta = 0._R8P
   elseif (self%rate_ > 0._R8P) then
      state%eta = (1._R8P - fraction) / self%rate_
   endif
   endfunction progress_state

   subroutine add_field(self, name, field)
   !< Add a field defined by the program: the template shows it where it writes `{name}`. Call it after `initialize`,
   !< which removes the fields, and before `start`, which looks for the fields the template names.
   class(bar_object),   intent(inout) :: self  !< Bar.
   character(len=*),    intent(in)    :: name  !< Name of the field in the template.
   class(field_object), intent(in)    :: field !< Field.
   type(field_entry),   allocatable   :: fields(:) !< Fields, with the new one.
   integer(I4P)                       :: f     !< Counter.

   if (token_kind(name) /= TOKEN_FIELD) call template_error('"'//name//'" is the name of a field of forbear', name)
   if (.not.allocated(self%fields_)) allocate(self%fields_(0))
   do f = 1, size(self%fields_, dim=1)
      if (self%fields_(f)%name == name) then ! the same name again: the new field replaces the old one
         deallocate(self%fields_(f)%field)
         allocate(self%fields_(f)%field, source=field)
         return
      endif
   enddo
   allocate(fields(size(self%fields_, dim=1) + 1))
   do f = 1, size(self%fields_, dim=1)
      fields(f)%name = self%fields_(f)%name
      call move_alloc(self%fields_(f)%field, fields(f)%field)
   enddo
   fields(size(fields, dim=1))%name = name
   allocate(fields(size(fields, dim=1))%field, source=field)
   call move_alloc(fields, self%fields_)
   endsubroutine add_field

   subroutine add_token(self, kind, style, decorated, name)
   !< Add a token to the layout.
   class(bar_object),    intent(inout)        :: self      !< Bar.
   integer(I4P),         intent(in)           :: kind      !< Kind of token.
   type(element_object), intent(in), optional :: style     !< Text and colours.
   logical,              intent(in), optional :: decorated !< Field with its own separators.
   character(len=*),     intent(in), optional :: name      !< Name of a field of the program.
   type(token_object),   allocatable          :: tokens(:) !< Tokens, with the new one.
   integer(I4P)                               :: n         !< Number of tokens.

   if (.not.allocated(self%tokens_)) allocate(self%tokens_(0))
   n = size(self%tokens_, dim=1)
   allocate(tokens(n + 1))
   tokens(1:n) = self%tokens_
   tokens(n + 1)%kind = kind
   if (present(style)) then
      tokens(n + 1)%style = style
   else
      call tokens(n + 1)%style%initialize
   endif
   if (present(decorated)) tokens(n + 1)%decorated = decorated
   if (present(name)) tokens(n + 1)%name = name
   call move_alloc(tokens, self%tokens_)
   endsubroutine add_token

   subroutine default_layout(self)
   !< Build the layout the keywords describe, without a template: the line of forbear 1.x, byte for byte.
   class(bar_object), intent(inout) :: self !< Bar.

   if (allocated(self%tokens_)) deallocate(self%tokens_)
   self%has_template_ = .false.
   call self%add_token(TOKEN_PREFIX, style=self%prefix)
   call self%add_token(TOKEN_TEXT, style=self%bracket_left)
   call self%add_token(TOKEN_BAR)
   call self%add_token(TOKEN_TEXT, style=self%bracket_right)
   call self%add_token(TOKEN_SUFFIX, style=self%suffix)
   if (allocated(self%spinner)) call self%add_token(TOKEN_SPINNER, style=self%spinner(1))
   if (self%add_progress_percent) call self%add_token(TOKEN_PERCENT, style=self%progress_percent, decorated=.true.)
   if (self%add_progress_count) call self%add_token(TOKEN_COUNT, style=self%progress_count, decorated=.true.)
   if (self%add_progress_speed) call self%add_token(TOKEN_SPEED, style=self%progress_speed, decorated=.true.)
   if (self%add_eta) call self%add_token(TOKEN_ETA, style=self%eta, decorated=.true.)
   call self%add_token(TOKEN_MESSAGE, style=self%message, decorated=.true.)
   endsubroutine default_layout

   subroutine parse_template(self, template)
   !< Build the layout of a template: literal text and `{name[:spec]}` fields; `{{` and `}}` write a brace.
   class(bar_object), intent(inout) :: self     !< Bar.
   character(len=*),  intent(in)    :: template !< Template.
   character(len=:),  allocatable   :: literal  !< Literal text being read.
   type(element_object)             :: text     !< Literal text, as a token style.
   integer(I4P)                     :: i        !< Position in the template.
   integer(I4P)                     :: closing  !< Position of the closing brace, from the opening one.

   if (allocated(self%tokens_)) deallocate(self%tokens_)
   allocate(self%tokens_(0))
   self%has_template_ = .true.
   self%template_ = template
   literal = ''
   i = 1
   do while (i <= len(template))
      select case(template(i:i))
      case('{')
         if (i < len(template)) then
            if (template(i + 1:i + 1) == '{') then
               literal = literal//'{'
               i = i + 2
               cycle
            endif
         endif
         closing = index(template(i + 1:), '}')
         if (closing == 0) call template_error('"{" not closed', template)
         call flush_literal
         call parse_field(template(i + 1:i + closing - 1))
         i = i + closing + 1
      case('}')
         if (i < len(template)) then
            if (template(i + 1:i + 1) == '}') then
               literal = literal//'}'
               i = i + 2
               cycle
            endif
         endif
         call template_error('"}" not opened', template)
      case default
         literal = literal//template(i:i)
         i = i + 1
      endselect
   enddo
   call flush_literal
   contains
      subroutine flush_literal()
      !< Add the literal text read so far, if any, as a token.
      if (len(literal) > 0) then
         call text%initialize(string=literal)
         call self%add_token(TOKEN_TEXT, style=text)
         literal = ''
      endif
      endsubroutine flush_literal

      subroutine parse_field(content)
      !< Add the token of a field, `name[:spec]`; the spec is a comma-separated list of a width (the bar only), colours
      !< (foreground), `on_` colours (background) and a style.
      character(len=*), intent(in)  :: content !< Content of the braces.
      character(len=:), allocatable :: name    !< Name of the field.
      character(len=:), allocatable :: spec    !< Spec of the field.
      character(len=:), allocatable :: item    !< Item of the spec.
      type(element_object)          :: style   !< Colours of the field.
      integer(I4P)                  :: colon   !< Position of the colon.
      integer(I4P)                  :: comma   !< Position of a comma.
      integer(I4P)                  :: kind    !< Kind of token.
      integer(I4P)                  :: width   !< Width of the bar.
      integer(I4P)                  :: iostat  !< Status of a read.

      colon = index(content, ':')
      if (colon > 0) then
         name = trim(adjustl(content(:colon - 1)))
         spec = content(colon + 1:)
      else
         name = trim(adjustl(content))
         spec = ''
      endif
      if (len(name) == 0) call template_error('a field without a name', template)
      kind = token_kind(name)
      ! the colours of the field: those of its keywords, then those of the spec
      select case(kind)
      case(TOKEN_PREFIX)  ; style = self%prefix
      case(TOKEN_SUFFIX)  ; style = self%suffix
      case(TOKEN_PERCENT) ; style = self%progress_percent
      case(TOKEN_COUNT)   ; style = self%progress_count ; self%add_progress_count = .true. ! the summary counts too
      case(TOKEN_SPEED)   ; style = self%progress_speed
      case(TOKEN_ETA)     ; style = self%eta
      case(TOKEN_MESSAGE) ; style = self%message
      case(TOKEN_SPINNER)
         if (.not.allocated(self%spinner)) call template_error('{spinner} without spinner_string', template)
         style = self%spinner(1)
      case(TOKEN_BAR)
         if (any(self%tokens_%kind == TOKEN_BAR)) call template_error('two {bar} fields', template)
         call style%initialize
      case default
         call style%initialize
      endselect
      do while (len(spec) > 0)
         comma = index(spec, ',')
         if (comma > 0) then
            item = trim(adjustl(spec(:comma - 1)))
            spec = spec(comma + 1:)
         else
            item = trim(adjustl(spec))
            spec = ''
         endif
         if (len(item) == 0) cycle
         if (verify(item, '0123456789') == 0) then
            if (kind /= TOKEN_BAR) call template_error('a width "'//item//'" for {'//name//'}, only {bar} has one', template)
            read(item, *, iostat=iostat) width
            if (iostat /= 0 .or. width < 0) call template_error('a wrong width "'//item//'"', template)
            self%width = width
         elseif (kind == TOKEN_BAR) then
            call template_error('a colour "'//item//'" for {bar}, coloured by the filled_char and empty_char keywords', &
                                template)
         elseif (len(item) > 3 .and. item(1:min(3, len(item))) == 'on_') then
            if (.not.is_color(item(4:))) call template_error('an unknown colour "'//item(4:)//'"', template)
            style%color_bg = item(4:)
         elseif (is_color(item)) then
            style%color_fg = item
         elseif (is_style(item)) then
            style%style = item
         else
            call template_error('an unknown colour or style "'//item//'"', template)
         endif
      enddo
      call self%add_token(kind, style=style, name=name)
      endsubroutine parse_field
   endsubroutine parse_template

   subroutine resolve_fields(self)
   !< Find the fields of the program that the template names: a name with no field stops the program.
   class(bar_object), intent(inout) :: self !< Bar.
   integer(I4P)                     :: t    !< Counter.
   integer(I4P)                     :: f    !< Counter.

   if (.not.allocated(self%tokens_)) call self%default_layout
   do t = 1, size(self%tokens_, dim=1)
      if (self%tokens_(t)%kind /= TOKEN_FIELD) cycle
      self%tokens_(t)%field = 0
      if (allocated(self%fields_)) then
         do f = 1, size(self%fields_, dim=1)
            if (self%fields_(f)%name == self%tokens_(t)%name) self%tokens_(t)%field = f
         enddo
      endif
      if (self%tokens_(t)%field == 0) &
         call template_error('{'//self%tokens_(t)%name//'} never added (add_field, after initialize, before start)', &
                             self%template_)
   enddo
   endsubroutine resolve_fields

   function width_before_bar(self) result(width)
   !< Return the columns of the layout before the bar body, with the values of the start: the scale is drawn above the bar.
   class(bar_object), intent(in) :: self  !< Bar.
   integer(I4P)                  :: width !< Columns.
   integer(I4P)                  :: t     !< Counter.

   width = 0
   do t = 1, size(self%tokens_, dim=1)
      select case(self%tokens_(t)%kind)
      case(TOKEN_BAR)
         return
      case(TOKEN_TEXT, TOKEN_PREFIX, TOKEN_SUFFIX)
         width = width + display_width(self%tokens_(t)%style%string)
      case(TOKEN_SPINNER)
         width = width + display_width(self%spinner(1)%string)
      case(TOKEN_PERCENT)
         width = width + 4
      case(TOKEN_COUNT)
         width = width + len(count_text(self%min_value, self%max_value, 0._R8P))
      case(TOKEN_SPEED)
         width = width + 6
      case(TOKEN_ETA, TOKEN_ELAPSED)
         width = width + 8
      case(TOKEN_FIELD)
         width = width + len(self%fields_(self%tokens_(t)%field)%field%render(progress_object()))
      endselect
   enddo
   endfunction width_before_bar


   subroutine complete(self, tic, count_rate)
   !< Complete the bar: end its line (clear it, at a position below the current line), write date and time and summary;
   !< the rate of the summary is that of what was done, the whole range unless the bar was finished before.
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
      if (self%hide_cursor) then
         write(self%output_unit, '(A)') ESC//'[?25h' ! restore cursor, go to the next line
      else
         write(self%output_unit, '(A)') ''           ! go to the next line
      endif
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
      if (self%indeterminate) then
         self%summary%string = ucs4_string(input='[done in '//duration(elapsed)//', '//                                     &
                               trim(adjustl(compact_real(self%fraction_drawn_ / elapsed, 6_I4P)))//'/s]')
      elseif (self%add_progress_count) then
         self%summary%string = ucs4_string(input='[done in '//duration(elapsed)//', '//                                     &
                               trim(adjustl(compact_real(abs(self%max_value - self%min_value) * self%fraction_drawn_ /     &
                                                         elapsed, 6_I4P)))//'/s]')
      else
         self%summary%string = ucs4_string(input='[done in '//duration(elapsed)//', '//                                     &
                               trim(adjustl(compact_real(100._R8P * self%fraction_drawn_ / elapsed, 6_I4P)))//'%/s]')
      endif
      write(self%output_unit, '(A)') render(self%summary, plain)
   endif
   flush(self%output_unit)
   endsubroutine complete

   subroutine draw(self)
   !< Draw the last frame on the terminal, on its line, and come back to the current line.
   !<
   !< Autowrap is off while the frame is written (`ESC[?7l` ... `ESC[?7h`): a frame wider than the terminal is cut at its
   !< right edge, instead of wrapping and leaving a copy of the bar on the line above at every drawing.
   class(bar_object), intent(inout) :: self     !< Bar.
   character(len=12)                :: position !< Position, as a string.
   character(len=:), allocatable    :: hide     !< Sequence hiding the cursor, if asked.

   hide = '' ; if (self%hide_cursor) hide = ESC//'[?25l'
   if (self%position > 0) then
      write(position, '(I0)') self%position
      write(self%output_unit, '(A)', advance='no') ucs4_string(input=hide//repeat(LF, self%position)//ESC//'[?7l')// &
                                                   self%frame_//                                                   &
                                                   ucs4_string(input=ESC//'[K'//CR//ESC//'['//trim(position)//'A'//   &
                                                               ESC//'[?7h')
   else
      write(self%output_unit, '(A)', advance='no') ucs4_string(input=hide//ESC//'[?7l')//self%frame_// &
                                                   ucs4_string(input=ESC//'[K'//CR//ESC//'[?7h')
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
      case default
         write(stderr, '(3A)') 'forbear: unknown spinner "', string_, &
                               '", see https://szaghi.github.io/forbear/guide/spinners'
         error stop 'forbear: unknown spinner'
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

   pure function done_text(min_value, done) result(text)
   !< Return what an indeterminate bar has done: an integer when `min_value` and `done` are whole, else compact.
   real(R8P), intent(in)         :: min_value !< Minimum value.
   real(R8P), intent(in)         :: done      !< Done since `min_value`.
   character(len=:), allocatable :: text      !< Done.
   character(len=24)             :: buffer    !< Formatting buffer.

   if (abs(min_value) < 1.e9_R8P .and. done < 1.e18_R8P .and. min_value == aint(min_value) .and. done == aint(done)) then
      write(buffer, '(I0)') nint(done, I8P)
      text = trim(buffer)
   else
      text = trim(adjustl(compact_real(done, 6_I4P)))
   endif
   endfunction done_text

   pure function display_width(string) result(width)
   !< Return the columns a string takes on a terminal: its characters, UTF-8 continuation bytes excluded.
   !<
   !< Strings hold UTF-8 text byte by byte (`ucs4_string` keeps every byte as one character), so `'Größe'` has 7
   !< characters for 5 columns: the bytes 128 to 191 continue a character and take no column. Wide characters (East
   !< Asian ideographs, emoji) take two columns and are counted as one.
   character(len=*, kind=UCS4), intent(in) :: string !< String.
   integer(I4P)                            :: width  !< Columns.
   integer(I4P)                            :: c      !< Counter.
   integer(I4P)                            :: code   !< Code of a character.

   width = 0
   do c = 1, len(string)
      code = ichar(string(c:c))
      if (code < 128 .or. code > 191) width = width + 1
   enddo
   endfunction display_width

   function styled(token, text, plain) result(output)
   !< Return the text of a field in the colours of its token.
   type(token_object), intent(inout)        :: token  !< Token.
   character(len=*),   intent(in)           :: text   !< Text.
   logical,            intent(in)           :: plain  !< Without colours.
   character(len=:, kind=UCS4), allocatable :: output !< Rendered text.

   token%style%string = ucs4_string(input=text)
   output = render(token%style, plain)
   endfunction styled

   subroutine template_error(what, template)
   !< Stop the program on a wrong template.
   character(len=*), intent(in) :: what     !< What is wrong.
   character(len=*), intent(in) :: template !< Template, or a hint.

   write(stderr, '(A)') 'forbear: '//what//' in template "'//template//'", see https://szaghi.github.io/forbear/guide/templates'
   error stop 'forbear: wrong template'
   endsubroutine template_error

   pure function token_kind(name) result(kind)
   !< Return the kind of token of a field name: a field of forbear, or one of the program.
   character(len=*), intent(in) :: name !< Name.
   integer(I4P)                 :: kind !< Kind of token.

   select case(name)
   case('bar')     ; kind = TOKEN_BAR
   case('spinner') ; kind = TOKEN_SPINNER
   case('percent') ; kind = TOKEN_PERCENT
   case('count')   ; kind = TOKEN_COUNT
   case('speed')   ; kind = TOKEN_SPEED
   case('eta')     ; kind = TOKEN_ETA
   case('elapsed') ; kind = TOKEN_ELAPSED
   case('message') ; kind = TOKEN_MESSAGE
   case('prefix')  ; kind = TOKEN_PREFIX
   case('suffix')  ; kind = TOKEN_SUFFIX
   case default    ; kind = TOKEN_FIELD
   endselect
   endfunction token_kind

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
