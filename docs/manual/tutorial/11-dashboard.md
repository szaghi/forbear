# 11. A 1980s dashboard

![the dashboard: a segmented bar in blue-green, an amber ramp, a red scanner, seven-segment numbers](/gifs/dashboard.gif){.gif}

The digital dashboards of the 1980s drew their gauges with a few tricks that still work on a terminal. Unlit segments
stay faintly visible. A gauge turns amber, then red, near the end of its scale. The bars of a tachometer rise in height.
A red light sweeps back and forth, and numbers are drawn in seven segments. This chapter dresses the bar of `march` in
each of them, then shows how one keyword does it all.

## 24-bit colours

Every colour keyword also takes `#rrggbb`: an exact colour, the same whatever the palette of the terminal. The dark
blue-green of an unlit segment is not among the 17 named colours. With `#0E342C` the empty part becomes the unlit
segments of a display, drawn with the same glyph as the lit ones: `▌`, a block whose right half is the gap between
two segments.

## Zones

`bar_zones` colours each filled cell by its position along the bar, as a tachometer turns amber and then red towards
its redline. Each `limit:colour` item covers the cells up to that fraction of the bar:

<<< @/examples/snippets/zones-init.f90

At 80%, past the first zone:

<<< @/examples/output/zones.ansi{ansi}

## A ramp

`bar_profile='ramp'` draws the cells as blocks rising from one eighth to the full height. The cells still to do keep
their blocks, unlit, so the whole ramp is always visible:

<<< @/examples/snippets/ramp-init.f90

<<< @/examples/output/ramp.ansi{ansi}

## A scanner

An [indeterminate](./10-unknown-ends) bar shows a block going back and forth. `pulse_trail` turns it into a scanner: a
head of one cell, followed by the cells it lit on the last drawings, in darker and darker shades:

<<< @/examples/snippets/scanner-init.f90

On its way back, with the trail behind the head:

<<< @/examples/output/scanner.ansi{ansi}

## Segment digits

`digits='segment'` writes the numbers in seven segments, and `digits_unlit_color` turns their padding into unlit 8s:

<<< @/examples/snippets/digits-init.f90

<<< @/examples/output/digits.ansi{ansi}

::: warning Check the font first
The seven-segment digits are characters that most terminal fonts do not have: Cascadia Code, Iosevka, JuliaMono and
GNU Unifont have them; DejaVu Sans Mono, Menlo and Consolas do not. Try `digits='segment'` in your terminal before
you rely on it. A log always gets plain digits.
:::

## Themes

`theme` sets all of the above, except the zones, the ramp and the digits, in one keyword: `vfd` (a vacuum fluorescent
display, blue-green), `amber` (a liquid crystal display) and `kitt` (a red scanner). Any keyword you pass wins over the
theme:

<<< @/examples/snippets/themes-init.f90

<<< @/examples/output/themes.ansi{ansi}

- A theme sets segments, colours, a bold prefix and, on an indeterminate bar, a pulse trail.
- Add zones, a ramp or segment digits on top of a theme, as the `amber` bar adds a ramp.
- In a log there are no colours. Zones and trails change nothing there, and a ramp shows its lit blocks only.

::: tip What you learned
`#rrggbb` colours, `bar_zones`, `bar_profile='ramp'`, `pulse_trail`, `digits='segment'`, `theme`.
Reference: [Themes](/guide/styling#themes), [Zones](/guide/styling#zones), [Profiles](/guide/bar#profiles),
[Pulse trail](/guide/bar#pulse-trail), [Segment digits](/guide/bar#segment-digits).
:::

That is the end of the tutorial: the [cookbook](../cookbook) has short recipes for everyday tasks.
