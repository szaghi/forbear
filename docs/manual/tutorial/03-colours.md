# 3. Colours and styles

![march with a coloured bar](/gifs/march_3.gif){.gif}

Every element takes three more keywords: `<element>_color_fg` (the colour of the characters), `<element>_color_bg` (the
colour behind them) and `<element>_style` (bold, italics, underline, ...). With the full block `█` for both parts and
two colours, the bar becomes solid:

<<< @/examples/snippets/march_3-init.f90

While running:

<<< @/examples/output/march_3.ansi{ansi}

- Colours and styles are names: `red`, `green_intense`, `bold_on`, ... in any case. There are 17 colours, the same for
  the foreground and the background, and 16 styles: the full list is in [Colours and styles](/guide/styling). A colour
  may also be a 24-bit `#rrggbb`, the same on every terminal palette.
- A name that is not in the list stops the program in `initialize`, with a message that names it: a typo cannot go
  unnoticed.
- Each element takes one style.
- A background colour fills the cell behind the characters: `filled_char_string=' ', filled_char_color_bg='green'`
  makes a solid bar too. The outputs of this documentation cannot show background colours, so the examples use
  foreground ones.

Colours are ANSI escape sequences, written with the bar: the terminal must understand them (every modern terminal
does). Sent to a file, they are written into the file as they are.

::: tip What you learned
The `_color_fg`, `_color_bg` and `_style` keywords of every element; a solid bar.
Reference: [Colours and styles](/guide/styling).
:::

Next: [4. What the bar reports](./04-reports).
