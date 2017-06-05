!< **forbear** project, definition of [[bar_object]].

module forbear_bar_object
!< **forbear** project, definition of [[bar_object]].
use, intrinsic :: iso_fortran_env, only : I4P=>int32, I8P=>int64, R8P=>real64, stdout=>output_unit
use forbear_element_object, only : element_object
implicit none
private
public :: bar_object

type :: bar_object
   !< Progress **bar** class.
   type(element_object) :: prefix               !< Message prefixing the bar.
   type(element_object) :: suffix               !< Message suffixing the bar.
   type(element_object) :: bracket_left         !< Left bracket surrounding the bar.
   type(element_object) :: bracket_right        !< Right bracket surrounding the bar.
   type(element_object) :: empty_char           !< Characters used for empty bar.
   type(element_object) :: filled_char          !< Characters used for filled bar.
   type(element_object) :: progress_percent     !< Progress in percent.
   type(element_object) :: progress_speed       !< Progress speed in percent.
   type(element_object) :: scale_bar            !< Scale bar.
   type(element_object) :: date_time            !< Date and time.
   integer(I4P)         :: width                !< With of the bar.
   real(R8P)            :: min_value            !< Minimum value.
   real(R8P)            :: max_value            !< Maximum value.
   integer(I4P)         :: frequency            !< Bar update frequency, in range `[1%,100%]`.
   logical              :: add_scale_bar        !< Add scale to the bar.
   logical              :: add_progress_percent !< Add progress in percent.
   logical              :: add_progress_speed   !< Add progress speed in percent.
   logical              :: add_date_time        !< Add date and time.
   logical              :: is_stdout_locked_    !< Flag to store standard output status.
   contains
      ! public methods
      procedure, pass(self) :: destroy          !< Destroy bar.
      procedure, pass(self) :: initialize       !< Initialize bar.
      procedure, pass(self) :: is_stdout_locked !< Return status of standard output unit.
      procedure, pass(self) :: start            !< Start bar.
      procedure, pass(self) :: update           !< Update bar.
      ! public operators
      generic :: assignment(=) => assign_bar !< Overload `=` operator.
      ! private methods
      procedure, pass(lhs),  private :: assign_bar !< Operator `=`.
endtype bar_object

contains
   ! public methods
   pure subroutine destroy(self)
   !< Destroy bar.
   class(bar_object), intent(inout) :: self !< Bar.

   call self%prefix%destroy
   call self%suffix%destroy
   call self%bracket_left%destroy
   call self%bracket_right%destroy
   call self%empty_char%destroy
   call self%filled_char%destroy
   call self%progress_percent%destroy
   call self%progress_speed%destroy
   call self%scale_bar%destroy
   call self%date_time%destroy
   self%width = 32
   self%min_value = 0._R8P
   self%max_value = 1._R8P
   self%frequency = 1_I4P
   self%add_scale_bar = .false.
   self%add_progress_percent = .false.
   self%add_progress_speed = .false.
   self%add_date_time = .false.
   self%is_stdout_locked_ = .false.
   endsubroutine destroy

   subroutine initialize(self,                                                                                               &
                         prefix_string, prefix_color_fg, prefix_color_bg, prefix_style,                                      &
                         suffix_string, suffix_color_fg, suffix_color_bg, suffix_style,                                      &
                         bracket_left_string, bracket_left_color_fg, bracket_left_color_bg, bracket_left_style,              &
                         bracket_right_string, bracket_right_color_fg, bracket_right_color_bg, bracket_right_style,          &
                         empty_char_string, empty_char_color_fg, empty_char_color_bg, empty_char_style,                      &
                         filled_char_string, filled_char_color_fg, filled_char_color_bg, filled_char_style,                  &
                         add_scale_bar, scale_bar_color_fg, scale_bar_color_bg, scale_bar_style,                             &
                         add_progress_percent, progress_percent_color_fg, progress_percent_color_bg, progress_percent_style, &
                         add_progress_speed, progress_speed_color_fg, progress_speed_color_bg, progress_speed_style,         &
                         add_date_time, date_time_color_fg, date_time_color_bg, date_time_style,                             &
                         width, min_value, max_value, frequency)
   !< Initialize bar.
   class(bar_object), intent(inout)        :: self                      !< Bar.
   character(len=*),  intent(in), optional :: prefix_string             !< Prefix string.
   character(len=*),  intent(in), optional :: prefix_color_fg           !< Prefix foreground color.
   character(len=*),  intent(in), optional :: prefix_color_bg           !< Prefix background color.
   character(len=*),  intent(in), optional :: prefix_style              !< Prefix style.
   character(len=*),  intent(in), optional :: suffix_string             !< Suffix string.
   character(len=*),  intent(in), optional :: suffix_color_fg           !< Suffix foreground color.
   character(len=*),  intent(in), optional :: suffix_color_bg           !< Suffix background color.
   character(len=*),  intent(in), optional :: suffix_style              !< Suffix style.
   character(len=*),  intent(in), optional :: bracket_left_string       !< Left bracket string.
   character(len=*),  intent(in), optional :: bracket_left_color_fg     !< Left bracket foreground color.
   character(len=*),  intent(in), optional :: bracket_left_color_bg     !< Left bracket background color.
   character(len=*),  intent(in), optional :: bracket_left_style        !< Left bracket style.
   character(len=*),  intent(in), optional :: bracket_right_string      !< Right bracket string
   character(len=*),  intent(in), optional :: bracket_right_color_fg    !< Right bracket foreground color.
   character(len=*),  intent(in), optional :: bracket_right_color_bg    !< Right bracket background color.
   character(len=*),  intent(in), optional :: bracket_right_style       !< Right bracket style.
   character(len=1),  intent(in), optional :: empty_char_string         !< Empty char.
   character(len=*),  intent(in), optional :: empty_char_color_fg       !< Empty char foreground color.
   character(len=*),  intent(in), optional :: empty_char_color_bg       !< Empty char background color.
   character(len=*),  intent(in), optional :: empty_char_style          !< Empty char style.
   character(len=1),  intent(in), optional :: filled_char_string        !< Filled char.
   character(len=*),  intent(in), optional :: filled_char_color_fg      !< Filled char foreground color.
   character(len=*),  intent(in), optional :: filled_char_color_bg      !< Filled char background color.
   character(len=*),  intent(in), optional :: filled_char_style         !< Filled char style.
   logical,           intent(in), optional :: add_scale_bar             !< Add scale to the bar.
   character(len=*),  intent(in), optional :: scale_bar_color_fg        !< Scale bar foreground color.
   character(len=*),  intent(in), optional :: scale_bar_color_bg        !< Scale bar background color.
   character(len=*),  intent(in), optional :: scale_bar_style           !< Scale bar style.
   logical,           intent(in), optional :: add_progress_percent      !< Add progress in percent.
   character(len=*),  intent(in), optional :: progress_percent_color_fg !< Progress percent foreground color.
   character(len=*),  intent(in), optional :: progress_percent_color_bg !< Progress percent background color.
   character(len=*),  intent(in), optional :: progress_percent_style    !< Progress percent style.
   logical,           intent(in), optional :: add_progress_speed        !< Add progress in percent.
   character(len=*),  intent(in), optional :: progress_speed_color_fg   !< Progress speed foreground color.
   character(len=*),  intent(in), optional :: progress_speed_color_bg   !< Progress speed background color.
   character(len=*),  intent(in), optional :: progress_speed_style      !< Progress speed style.
   logical,           intent(in), optional :: add_date_time             !< Add date and time.
   character(len=*),  intent(in), optional :: date_time_color_fg        !< Date and time foreground color.
   character(len=*),  intent(in), optional :: date_time_color_bg        !< Date and time background color.
   character(len=*),  intent(in), optional :: date_time_style           !< Date and time style.
   integer(I4P),      intent(in), optional :: width                     !< With of the bar.
   real(R8P),         intent(in), optional :: min_value                 !< Minimum value.
   real(R8P),         intent(in), optional :: max_value                 !< Maximum value.
   integer(I4P),      intent(in), optional :: frequency                 !< Bar update frequency, in range `[1%,100%]`.
   character(len=1)                        :: empty_char_string_        !< Characters used for empty bar, local variable.
   character(len=1)                        :: filled_char_string_       !< Characters used for filled bar, local variable.

   empty_char_string_ = ' ' ; if (present(empty_char_string)) empty_char_string_ = empty_char_string
   filled_char_string_ = '*' ; if (present(filled_char_string)) filled_char_string_ = filled_char_string
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
   if (present(add_scale_bar)) self%add_scale_bar = add_scale_bar
   call self%scale_bar%initialize(color_fg=scale_bar_color_fg, color_bg=scale_bar_color_bg, style=scale_bar_style)
   if (present(add_progress_percent)) self%add_progress_percent = add_progress_percent
   call self%progress_percent%initialize(color_fg=progress_percent_color_fg, color_bg=progress_percent_color_bg,&
                                         style=progress_percent_style)
   if (present(add_progress_speed)) self%add_progress_speed = add_progress_speed
   call self%progress_speed%initialize(color_fg=progress_speed_color_fg, color_bg=progress_speed_color_bg,&
                                       style=progress_speed_style)
   if (present(add_date_time)) self%add_date_time = add_date_time
   call self%date_time%initialize(color_fg=date_time_color_fg, color_bg=date_time_color_bg, style=date_time_style)
   if (present(width)) self%width = width
   if (present(min_value)) self%min_value = min_value
   if (present(max_value)) self%max_value = max_value
   if (present(frequency)) self%frequency = frequency

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

   if (self%add_scale_bar) call add_scale_bar
   call self%update(current=self%min_value)
   self%is_stdout_locked_ = .true.
   contains
      subroutine add_scale_bar()
      !< Add scale to the bar.
      character(len=:), allocatable :: bar       !< Bar line.
      character(len=11)             :: min_value !< Minimum_value.
      character(len=11)             :: max_value !< Maximum_value.

      write(min_value, '(F5.2)') self%min_value ; min_value = trim(min_value)//' (min)'
      write(max_value, '(F5.2)') self%max_value ; max_value = '(max) '//trim(max_value)
      self%scale_bar%string = min_value//repeat(' ', self%width - len(min_value) - len(max_value))//max_value
      bar = repeat(' ', len(self%prefix%string))//self%bracket_left%output()//self%scale_bar%output()//self%bracket_right%output()
      write(stdout, '(A)') bar
      endsubroutine add_scale_bar
   endsubroutine start

   subroutine update(self, current)
   !< Update bar.
   class(bar_object), intent(inout) :: self              !< Bar.
   real(R8P),         intent(in)    :: current           !< Current value.
   integer(I4P)                     :: progress          !< Progress, in percent.
   integer(I4P), save               :: progress_previous !< Previous progress, in percent.
   integer(I8P), save               :: tic_toc(1:2)      !< Tic-toc timer.
   character(len=18), save          :: date_time_start   !< Start date/time.
   character(len=18)                :: date_time         !< Current date/time.
   integer(I8P)                     :: count_rate        !< Timer count rate.
   integer(I4P)                     :: f_chars           !< Number of filled characters of bar.
   character(len=4)                 :: progress_percent  !< Progress in percent.
   character(len=12)                :: progress_speed    !< Progress speed in percent.
   character(len=:), allocatable    :: bar               !< Bar line.

   progress = nint(current / (self%max_value - self%min_value) * 100)
   if (progress == 0) then
      progress_previous = 0
      call system_clock(tic_toc(1), count_rate)
      call date_and_time(date=date_time_start(1:8), time=date_time_start(9:))
   endif
   if (mod(progress, self%frequency) == 0 .or. progress == 100) then
      call system_clock(tic_toc(2), count_rate)
      call date_and_time(date=date_time(1:8), time=date_time(9:))
      f_chars = nint(progress / 100._R8P * self%width)
      bar = self%prefix%output()
      bar = bar//self%bracket_left%output()
      bar = bar//repeat(self%filled_char%output(), f_chars)
      bar = bar//repeat(self%empty_char%output(), self%width - f_chars)
      bar = bar//self%bracket_right%output()
      bar = bar//self%suffix%output()
      if (self%add_progress_percent) then
         write(progress_percent, '(I3,A)') progress, '%'
         self%progress_percent%string = progress_percent
         bar = bar//self%progress_percent%output()
      endif
      if (self%add_progress_speed) then
         write(progress_speed, '(A,F6.2,A)') ' (', (progress - progress_previous) / &
                                                  (real(tic_toc(2) - tic_toc(1), kind=R8P) / count_rate), '%/s)'
         self%progress_speed%string = progress_speed
         bar = bar//self%progress_speed%output()
      endif
      if (self%add_date_time) then
         self%date_time%string = ' ['//date_time_start(1:4)//'/'//date_time_start(5:6)//'/'//date_time_start(7:8)//      &
                                  ' '//date_time_start(9:10)//':'//date_time_start(11:12)//':'//date_time_start(13:14)// &
                                ' - '//date_time(1:4)//'/'//date_time(5:6)//'/'//date_time(7:8)//                        &
                                  ' '//date_time(9:10)//':'//date_time(11:12)//':'//date_time(13:14)//']'
         bar = bar//self%date_time%output()
      endif
      bar = bar//char(13)
      write(stdout, '(A)', advance='no') bar
      flush(stdout)
      progress_previous = progress
      tic_toc(1) = tic_toc(2)
   endif
   if (progress == 100) then
      self%is_stdout_locked_ = .false.
      write(stdout, '(A)')
   endif
   endsubroutine update

   ! private methods
   pure subroutine assign_bar(lhs, rhs)
   !< Initialize bar.
   class(bar_object), intent(inout) :: lhs !< Left hand side.
   type(bar_object),  intent(in)    :: rhs !< Right hand side.

   lhs%prefix = rhs%prefix
   lhs%suffix = rhs%suffix
   lhs%bracket_left = rhs%bracket_left
   lhs%bracket_right = rhs%bracket_right
   lhs%empty_char = rhs%empty_char
   lhs%filled_char = rhs%filled_char
   lhs%progress_percent = rhs%progress_percent
   lhs%progress_speed = rhs%progress_speed
   lhs%scale_bar = rhs%scale_bar
   lhs%date_time = rhs%date_time
   lhs%width = rhs%width
   lhs%min_value = rhs%min_value
   lhs%max_value = rhs%max_value
   lhs%frequency = rhs%frequency
   lhs%add_scale_bar = rhs%add_scale_bar
   lhs%add_progress_percent = rhs%add_progress_percent
   lhs%add_progress_speed = rhs%add_progress_speed
   lhs%add_date_time = rhs%add_date_time
   lhs%is_stdout_locked_ = rhs%is_stdout_locked_
   endsubroutine assign_bar
endmodule forbear_bar_object