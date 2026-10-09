# 9. Layout templates

![march with a layout of its own: words between the fields, and the residual as a field](/gifs/march_9.gif){.gif}

Chapters 1 to 8 built every line from keywords: the elements come in a fixed order, and each one carries its own spaces
and words (`ETA`, `%/s`). A *template* lays the line out instead: literal text, and fields in braces where the values
go. `march` writes its line as it wants it, with the residual of the solution as a field of its own:

<<< @/examples/snippets/march_9-init.f90

While running:

<<< @/examples/output/march_9-running.ansi{ansi}

At the end:

<<< @/examples/output/march_9.ansi{ansi}

- Everything outside the braces is written as it is: `march `, ` step `, ` ETA `, ` res `. The fields write their value
  only: `{percent}` is `nnn%`, `{count}` is `25/50`, `{eta}` is `hh:mm:ss`; the spacing is the template's.
- After a colon, a field takes colours and a style: `{percent:yellow}`, `{eta:white,on_blue,bold_on}`. The bar takes its
  width instead, `{bar:30}`; its colours stay the keywords of the filled and empty parts.
- A mistake stops the program with a message that names it: an unknown colour or style, an unclosed brace, in
  `initialize`; a field that is neither forbear's nor added by the program, in `start`, so that `add_field` can come in
  between.

## A field of your own

`{residual}` is not a field of forbear: the program adds it. A field is a type that extends `field_object` and returns
its text from `render`:

<<< @/examples/snippets/march_9-field.f90

- `render` gets the progress of the bar (`progress_object`: current value, fraction, percent, speed, elapsed time,
  ETA), and may read anything else through its components: here, a pointer to the residual of the program.
- `bar%add_field('residual', field)`, after `initialize` (which removes the fields) and before `start`, names it for the
  template. The bar keeps a copy of the field; the pointer in the copy still points to the variable of the program.
- Keep the text of a field the same width at every drawing, as the fields of forbear do: a line that shrinks is erased
  to its end, but a line whose fields move around is hard to read.

The keywords of chapters 1 to 8 still work, and draw exactly what they drew: a template is another way to describe
the same line, for when the order, the words or the spacing are yours.

::: tip What you learned
`template=`, its fields and their colours; fields of the program with `field_object` and `add_field`.
Reference: [Layout templates](/guide/templates).
:::

Next: [10. Unknown ends](./10-unknown-ends).
