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
| Brackets | `bracket_left_string`, `bracket_right_string` | [2](/manual/tutorial/02-look) | [The line](./bar#the-line) |
| Bar characters and width | `filled_char_string`, `empty_char_string`, `width` | [2](/manual/tutorial/02-look) | [The line](./bar#the-line) |
| Unicode strings | UTF-8 literals or `UCS4` kind | [2](/manual/tutorial/02-look#unicode) | [Character kinds](./bar#character-kinds) |
| Colours and styles | `<element>_color_fg`, `_color_bg`, `_style` | [3](/manual/tutorial/03-colours) | [Colours and styles](./styling) |
| Progress in percent | `add_progress_percent` | [4](/manual/tutorial/04-reports) | [initialize](./bar#initialize) |
| Progress speed | `add_progress_speed` | [4](/manual/tutorial/04-reports) | [Numbers that do not fit](./limitations#numbers-that-do-not-fit) |
| Start and end time | `add_date_time` | [4](/manual/tutorial/04-reports) | [initialize](./bar#initialize) |
| Scale | `add_scale_bar` | [4](/manual/tutorial/04-reports) | [Numbers that do not fit](./limitations#numbers-that-do-not-fit) |
| Spinners | `spinner_string` | [5](/manual/tutorial/05-spinners) | [Spinners](./spinners) |
| Spinner or percentage alone | `width=0` | [5](/manual/tutorial/05-spinners#without-the-bar) | [The line](./bar#the-line) |
| Output unit | `output_unit` | [6](/manual/tutorial/06-terminal#separate-streams) | [initialize](./bar#initialize) |
| Holding other output | `is_stdout_locked` | [6](/manual/tutorial/06-terminal#messages-while-the-bar-runs) | [is_stdout_locked](./bar#is-stdout-locked) |
| Drawing frequency | `frequency` | [6](/manual/tutorial/06-terminal#fewer-drawings) | [update](./bar#update) |

Not available: an estimated time of arrival (ETA), a message that changes during the run, a bar that runs backwards.
