# 2. The look of the bar

The bar of chapter 1 is a row of stars. Its look is made of *elements*, each one a string you choose:

<<< @/examples/snippets/march_2-init.f90

While running:

<<< @/examples/output/march_2.ansi{ansi}

The line is drawn in this order:

```
prefix  bracket_left  filled × n  empty × (width − n)  bracket_right  suffix
march   [             ##########  ..........           ]              time steps
```

- `width` is the number of characters between the brackets; `n` is the progress times `width`, rounded.
- `filled_char_string` and `empty_char_string` are repeated `n` and `width − n` times: use one character each, or the
  bar becomes wider than `width`.
- The prefix, the brackets and the suffix are drawn as given, spaces included: `prefix_string='march '` ends with a
  space to keep the name apart from the bracket.

## Unicode

Every string can be Unicode: write it in the source, saved as UTF-8.

<<< @/examples/snippets/march_2u-init.f90

<<< @/examples/output/march_2u.ansi{ansi}

The strings are `class(*)`: plain literals as above, or literals of the `UCS4` kind that forbear exports
(`UCS4_'█'`), both work.

::: tip What you learned
The elements of the line, their order, the width; Unicode strings.
Reference: [The bar object](/guide/bar#the-line).
:::

Next: [3. Colours and styles](./03-colours).
