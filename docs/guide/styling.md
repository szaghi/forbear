---
title: Colours and styles
---

# Colours and styles

Every element of the bar takes a foreground colour (`<element>_color_fg`), a background colour (`<element>_color_bg`)
and a style (`<element>_style`), by name. The names are those of [FACE](https://github.com/szaghi/FACE), which writes
them as ANSI escape sequences; they are case insensitive (`red`, `RED`, `Red`).

A name that is not in these lists is ignored, without a message.

## Colours

The same 17 names for the foreground and the background:

| Name | Foreground code | Background code |
|---|---|---|
| `black` | 30 | 40 |
| `red` | 31 | 41 |
| `green` | 32 | 42 |
| `yellow` | 33 | 43 |
| `blue` | 34 | 44 |
| `magenta` | 35 | 45 |
| `cyan` | 36 | 46 |
| `white` | 37 | 47 |
| `default` | 39 | 49 |
| `black_intense` | 90 | 100 |
| `red_intense` | 91 | 101 |
| `green_intense` | 92 | 102 |
| `yellow_intense` | 93 | 103 |
| `blue_intense` | 94 | 104 |
| `magenta_intense` | 95 | 105 |
| `cyan_intense` | 96 | 106 |
| `white_intense` | 97 | 107 |

How a colour looks depends on the palette of the terminal.

## Styles

One style for each element:

| Name | Code |
|---|---|
| `bold_on` | 1 |
| `italics_on` | 3 |
| `underline_on` | 4 |
| `inverse_on` (foreground and background swapped) | 7 |
| `strikethrough_on` | 9 |
| `framed_on` | 51 |
| `encircled_on` | 52 |
| `overlined_on` | 53 |

The `_off` names (`bold_off`, `italics_off`, `underline_off`, `inverse_off`, `strikethrough_off`, `framed_off`,
`encircled_off`, `overlined_off`) exist too, but have no use here: every element ends with a reset of all colours and
styles. Many terminals do not support `framed_on`, `encircled_on` and `overlined_on`.

## Example

<<< @/examples/snippets/march_3-init.f90

<<< @/examples/output/march_3.ansi{ansi}
