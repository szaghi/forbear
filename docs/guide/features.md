---
title: Feature map
---

# Feature map

Every feature of forbear, where the tutorial teaches it and where the reference describes it.

| Feature | How | Tutorial | Reference |
|---|---|---|---|
| A progress bar | `initialize`, `start`, `update` | [1](/manual/tutorial/01-first-bar) | [The bar object](./bar) |
| Range of the bar | `min_value`, `max_value` | [1](/manual/tutorial/01-first-bar#the-range) | [Limitations](./limitations#the-range) |
| Prefix and suffix | `prefix_string`, `suffix_string` | [2](/manual/tutorial/02-look) | [The line](./bar#the-line) |
| Prefix or suffix changed while running | `bar%prefix%string`, `bar%suffix%string` | [cookbook](/manual/cookbook#a-prefix-that-names-the-phase) | [The line](./bar#the-line) |
| Brackets | `bracket_left_string`, `bracket_right_string` | [2](/manual/tutorial/02-look) | [The line](./bar#the-line) |
| Bar characters and width | `filled_char_string`, `empty_char_string`, `width` | [2](/manual/tutorial/02-look) | [The line](./bar#the-line) |
| Unicode strings | UTF-8 literals or `UCS4` kind | [2](/manual/tutorial/02-look#unicode) | [Character kinds](./bar#character-kinds) |
| Colours and styles | `<element>_color_fg`, `_color_bg`, `_style` | [3](/manual/tutorial/03-colours) | [Colours and styles](./styling) |
| 24-bit colours | `'#rrggbb'` for any colour keyword | [3](/manual/tutorial/03-colours) | [24-bit colours](./styling#_24-bit-colours) |
| Colour zones | `bar_zones` | [11](/manual/tutorial/11-dashboard#zones) | [Zones](./styling#zones) |
| Rising ramp | `bar_profile='ramp'` | [11](/manual/tutorial/11-dashboard#a-ramp) | [Profiles](./bar#profiles) |
| Seven-segment digits | `digits`, `digits_unlit_color` | [11](/manual/tutorial/11-dashboard#segment-digits) | [Segment digits](./bar#segment-digits) |
| Dashboard themes | `theme` | [11](/manual/tutorial/11-dashboard#themes) | [Themes](./styling#themes) |
| Scanner (pulse with a trail) | `pulse_trail` | [11](/manual/tutorial/11-dashboard#a-scanner) | [Pulse trail](./bar#pulse-trail) |
| Smooth bar | `partial_blocks` | [2](/manual/tutorial/02-look#a-smooth-bar) | [The line](./bar#the-line) |
| Progress in percent | `add_progress_percent` | [4](/manual/tutorial/04-reports) | [initialize](./bar#initialize) |
| Progress count | `add_progress_count` | [4](/manual/tutorial/04-reports) | [initialize](./bar#initialize) |
| Progress speed | `add_progress_speed` | [4](/manual/tutorial/04-reports) | [Number formats](./limitations#number-formats) |
| Estimated time of arrival | `add_eta`, `smoothing` | [4](/manual/tutorial/04-reports) | [update](./bar#update) |
| Start and end time | `add_date_time` | [4](/manual/tutorial/04-reports) | [initialize](./bar#initialize) |
| Summary at the end | `add_summary` | [4](/manual/tutorial/04-reports) | [initialize](./bar#initialize) |
| Scale | `add_scale_bar` | [4](/manual/tutorial/04-reports) | [Number formats](./limitations#number-formats) |
| Spinners | `spinner_string` | [5](/manual/tutorial/05-spinners) | [Spinners](./spinners) |
| Spinner or percentage alone | `width=0` | [5](/manual/tutorial/05-spinners#without-the-bar) | [The line](./bar#the-line) |
| Lines above the bar | `write` | [6](/manual/tutorial/06-terminal) | [write](./bar#write) |
| Output of a library | `suspend`, `resume` | [6](/manual/tutorial/06-terminal#output-you-do-not-control) | [suspend and resume](./bar#suspend-and-resume) |
| A message at the end of the bar | `update(message=)` | [6](/manual/tutorial/06-terminal) | [update](./bar#update) |
| Drawing rate | `min_interval`, `frequency`, `FORBEAR_MIN_INTERVAL` | [6](/manual/tutorial/06-terminal#how-often-the-bar-is-drawn) | [update](./bar#update) |
| Output unit | `output_unit` | [cookbook](/manual/cookbook#the-bar-on-standard-error) | [initialize](./bar#initialize) |
| Nested bars | `position` | [7](/manual/tutorial/07-nested) | [initialize](./bar#initialize) |
| Plain log off a terminal | `interactive`, `FORBEAR_INTERACTIVE` | [8](/manual/tutorial/08-logs) | [Terminals and logs](./terminals) |
| A log line every so often | `log_interval`, `FORBEAR_LOG_INTERVAL` | [8](/manual/tutorial/08-logs#signs-of-life-in-long-jobs) | [How often the bar is drawn](./limitations#how-often-the-bar-is-drawn) |
| Cursor left visible | `hide_cursor` | | [If the program stops](./limitations#if-the-program-stops) |
| Bars off | `disabled`, `FORBEAR_DISABLE` | [8](/manual/tutorial/08-logs#turning-the-bars-off) | [Terminals and logs](./terminals#environment-variables) |
| Terminal state | `is_stdout_locked` | | [is_stdout_locked](./bar#is-stdout-locked) |
| Reusing or copying a bar | `destroy`, `initialize` again, `=` | [cookbook](/manual/cookbook#several-bars-one-after-another) | [destroy](./bar#destroy) |
| Layout template | `template` | [9](/manual/tutorial/09-templates) | [Layout templates](./templates) |
| Fields of the program | `field_object`, `add_field` | [9](/manual/tutorial/09-templates#a-field-of-your-own) | [Fields of the program](./templates#fields-of-the-program) |
| Loops of unknown length | `indeterminate`, `finish` | [10](/manual/tutorial/10-unknown-ends) | [finish](./bar#finish) |
| A loop left before its end | `finish` | [10](/manual/tutorial/10-unknown-ends#leaving-a-loop-early) | [finish](./bar#finish) |

Not available: a bar that runs backwards, a spinner animated by a clock of its own (Fortran has no portable threads).
