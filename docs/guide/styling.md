---
title: Colours and styles
---

# Colours and styles

Every element of the bar takes a foreground colour (`<element>_color_fg`), a background colour (`<element>_color_bg`)
and a style (`<element>_style`), by name. The names are those of [FACE](https://github.com/szaghi/FACE), which writes
them as ANSI escape sequences; they are case insensitive (`red`, `RED`, `Red`). A colour may also be a 24-bit value,
`#rrggbb`.

A name that is not in these lists, or a malformed `#rrggbb`, stops the program in `initialize`, with a message that
names it.

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

How a named colour looks depends on the palette of the terminal.

### 24-bit colours

`#rrggbb`, three hexadecimal pairs in any case (`#2EF5C0`, `#2ef5c0`), is written as `ESC[38;2;r;g;bm` in the
foreground and `ESC[48;2;r;g;bm` in the background: the exact colour, whatever the palette of the terminal. Most modern
terminals support it; one that does not shows an approximation, or no colour. The short form `#rgb` is not accepted.

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

## Zones

`bar_zones` colours each filled cell of the bar body by its position, as the tachometer of a car turns amber, then red,
towards the end of its scale. It is a list of `limit:colour` items separated by blanks:

- the limit is a fraction of the body, in (0, 1], increasing from item to item;
- the colour is a name or `#rrggbb`, and replaces the foreground colour of `filled_char` (its background and style
  stay).

A filled cell takes the colour of the first zone whose limit reaches the end of the cell: with `width=40` and
`'0.7:… 0.88:… 1:…'`, cells 1–28, 29–35 and 36–40. Cells beyond the last limit keep the `filled_char` colour; empty
cells keep the `empty_char` colours. The partial block of `partial_blocks` takes the colour of its cell, and so do the
block of an [indeterminate](./bar#initialize) bar and its full body at the end. A log has no colours: the zones change
nothing there.

A wrong item stops the program in `initialize`, with a message that names it: no colon, a limit that is not a number,
not in (0, 1] or not above the previous one, an unknown colour.

<<< @/examples/snippets/zones-init.f90

At 80%, the dark `empty_char` cells are the unlit segments:

<<< @/examples/output/zones.ansi{ansi}

## Themes

`theme` gives a bar the look of a 1980s dashboard display in one keyword: the segments of the bar, lit and unlit, and
the colours of every element. Every keyword passed explicitly wins over the theme, so a theme is a starting point:

| Theme | Looks like | Lit | Unlit | Numbers | Pulse trail |
|---|---|---|---|---|---|
| `vfd` | a vacuum fluorescent display, blue-green | `#2EF5C0` | `#0E342C` | `#FFB000` | `#1FB08A #137057 #0B3F31` |
| `amber` | an amber liquid crystal display | `#FFB000` | `#3A2800` | `#FFB000` | `#C08400 #7A5400 #3F2B00` |
| `kitt` | the red scanner of a talking car | `#FF3B30` | `#3C0C0A` | `#FFB000` | `#C0281E #7A1912 #4A0F0B` |

A theme sets:

- the filled and empty strings to `▌`, a segment with the gap of its right half (unless `partial_blocks`);
- the filled colour to *lit* and the empty colour to *unlit*, which a [ramp](./bar#profiles) uses too;
- the prefix, suffix and spinner to *lit*, the prefix in bold;
- the percent, count, speed, ETA, scale, date and summary to *numbers*;
- the unlit 8s of [segment digits](./bar#segment-digits) to `#3A2800`;
- on an [indeterminate](./bar#initialize) bar, the [pulse trail](./bar#pulse-trail).

It sets no [zones](#zones), no profile and no segment digits: add them as you like. An unknown theme stops the program.

<<< @/examples/snippets/themes-init.f90

Three bars, one per theme, the second with a ramp:

<<< @/examples/output/themes.ansi{ansi}

## Example

<<< @/examples/snippets/march_3-init.f90

<<< @/examples/output/march_3.ansi{ansi}
