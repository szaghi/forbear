# Design note: a layout template for forbear

Status: implemented, 2026-10-08, as an additive release (v1.6): the keywords build the same layout as before.

Decisions (section 9): 1. inline styles, option A; 2. a wrong template stops the program (`error stop`); 3. unknown
colour and style names stop the program also as keywords (and so do unknown spinner keys); 4. only fields take styles,
not literal text (open: may be added later, without breaking); 5. fields of the program now (`field_object`,
`add_field`).

## 1. The problem

`initialize` takes 71 keywords. 45 of them (63%) are colour and style keywords: every element costs a string keyword
and three colour keywords (`_color_fg`, `_color_bg`, `_style`), and every report also an `add_` switch. The four
reports added in v1.4.0 (count, ETA, summary, message) cost 15 keywords.

Every bar line has the same layout, hard-coded in `build_frame`:

```
prefix bracket_left bar bracket_right suffix spinner percent count speed ETA message
```

What this rules out, today:

- **Order.** A count before the percent, the message before the ETA, the percent before the bar: not possible.
- **Text between elements.** Brackets are the only free text around the bar; anything else needs the `prefix_string`
  and `suffix_string` slots. The words are fixed: the ETA is always `ETA hh:mm:ss`, the speed always `(nnn.nn%/s)`.
- **Spacing.** It is baked into the elements. v1.5.0 had to give the percent a leading space so that it would not touch
  a spinner, and the examples lost their `'] '` brackets in return. That fix was a symptom: when the library owns the
  spacing, every new element combination is a new spacing bug.
- **Growth.** A new field (elapsed time, a rate in steps per second) costs four more keywords and one more fixed slot.

## 2. Constraints

- Fortran 2008, no variadic arguments and no dictionaries. A layout has to be data: a string, or an array of objects.
- Parse once, in `initialize`. `update` runs in hot loops, and must keep doing only arithmetic and concatenation.
- Plain mode stays: a log gets the same layout, without colours and control sequences.
- Fixed-width fields stay fixed-width (`compact_real`, `hms`): a line that shrinks leaves characters on screen.
- Strings remain UTF-8 bytes in UCS4 strings (see `display_width`). The template itself is a string like any other.
- Mistakes must be loud. Today an unknown colour or spinner key is silently ignored, which the docs list as a
  limitation. A template is new code, so it can fail in `initialize` from the start.

## 3. Three use cases, from the tutorial

**U1, chapter 4: every report.** Today, 14 keywords for the bar line, plus the switches of the other lines:

```fortran
call bar%initialize(prefix_string='march ', bracket_left_string='[', bracket_right_string=']',  &
                    filled_char_string='#', empty_char_string='.',                             &
                    add_progress_percent=.true., progress_percent_color_fg='yellow',           &
                    add_progress_count=.true., progress_count_color_fg='cyan',                 &
                    add_progress_speed=.true., progress_speed_color_fg='green',                &
                    add_eta=.true., eta_color_fg='blue',                                       &
                    add_scale_bar=.true., scale_bar_color_fg='blue',                           &
                    add_date_time=.true., date_time_color_fg='magenta',                        &
                    add_summary=.true., summary_color_fg='green',                              &
                    width=30, max_value=real(steps, R8P))
```

**U2, chapter 6: a message at the end of the line.**

```fortran
call bar%initialize(prefix_string='march ', bracket_left_string='[', bracket_right_string=']',  &
                    filled_char_string='#', empty_char_string='.', add_progress_percent=.true., &
                    message_color_fg='cyan', width=30, max_value=real(steps, R8P))
```

**U3, the showcase: a smooth bar with a coloured track.**

```fortran
call steps_bar%initialize(prefix_string='solve  ', bracket_left_string='▕', bracket_right_string='▏',                &
                          partial_blocks=.true., filled_char_color_fg='cyan', add_progress_percent=.true.,       &
                          empty_char_string=' ', empty_char_color_bg='black_intense',                            &
                          progress_percent_color_fg='yellow', add_progress_count=.true.,                         &
                          progress_count_color_fg='blue', add_eta=.true., eta_color_fg='green',                   &
                          message_color_fg='magenta_intense', add_summary=.true., summary_color_fg='green',        &
                          width=36, max_value=real(steps, R8P))
```

## 4. Three options

### A. A template with inline styles (as indicatif)

The bar line is a string: literal text and fields in braces, each field with an optional style after a colon.

```fortran
! U1: the bar line in 3 keywords instead of 14
call bar%initialize(template='march [{bar:30}] {percent:yellow} {count:cyan} ({speed:green}%/s) ETA {eta:blue}', &
                    filled_char_string='#', empty_char_string='.',                                             &
                    add_scale_bar=.true., add_date_time=.true., add_summary=.true., max_value=real(steps, R8P))
! U2
call bar%initialize(template='march [{bar:30}] {percent} {message:cyan}', filled_char_string='#', &
                    empty_char_string='.', max_value=real(steps, R8P))
! U3
call steps_bar%initialize(template='solve  ▕{bar:36}▏ {percent:yellow} {count:blue} '//                   &
                                   'ETA {eta:green} {message:magenta_intense}',                          &
                          partial_blocks=.true., filled_char_color_fg='cyan', empty_char_color_bg='black_intense', &
                          add_summary=.true., max_value=real(steps, R8P))
```

The layout reads as what it draws, and the spacing and the words (`ETA`, `%/s`) belong to the user. The cost is a
small parser, and typos are found when the program runs (by `initialize`), not when it compiles.

### B. A plain template, styles set apart

The same template without colours, which are set by a method, one call per field:

```fortran
call bar%initialize(template='march [{bar:30}] {percent} {count} ({speed}%/s) ETA {eta}', max_value=real(steps, R8P))
call bar%style('percent', color_fg='yellow')
call bar%style('count', color_fg='cyan')
call bar%style('eta', color_fg='blue')
```

The template stays short and readable, but one bar needs several statements, and the field names are repeated as
strings in two places: a renamed field must be changed in both.

### C. Column objects (as Rich)

A layout is a list of objects, one type per field, with typed keywords:

```fortran
call bar%initialize(max_value=real(steps, R8P))
call bar%add(text('march ['))
call bar%add(bar_column(width=30, filled='#', empty='.'))
call bar%add(text('] '))
call bar%add(percent_column(color_fg='yellow'))
call bar%add(eta_column(color_fg='blue', label='ETA '))
```

The compiler checks every keyword, and a program can extend `column_object` with a column of its own (say, the
residual formatted its way) by overriding a deferred `render`. The cost: one derived type and one constructor per field,
five to ten statements per bar, and the layout no longer readable at a glance.

### Comparison

| | A: inline | B: apart | C: columns |
|---|---|---|---|
| Reads as the line it draws | yes | yes | no |
| Statements per bar (U1) | 1 | 1 + one per coloured field | one per field |
| Typos found | at `initialize` | at `initialize` | at compile time |
| User-defined fields | no (`{message}` only) | no | yes |
| New field costs | a name in the parser | a name in the parser | a type and a constructor |
| Implementation | parser + token list, ~250 lines | the same + `style` | ~10 types, ~400 lines |
| Fit with Fortran | a string: natural | a string: natural | OOP: natural, verbose |

## 5. Recommendation: A

Option A gives the most for the least: one statement per bar, the layout readable as it is drawn, and a parser that
runs once. Compile-time checking (C) is the one thing it loses; an `error stop` in `initialize` with a precise message
recovers most of it, since every program builds its bars at the start. User-defined fields (C's real strength) are
mostly covered by `{message}`, which takes any text the program formats; if a need appears, a C-style extension can
come later beneath the same template, as a user field type the parser looks up.

### Grammar

```
template := { literal | field }
literal  := any text but '{' and '}' ; '{{' and '}}' write a brace
field    := '{' name [ ':' spec ] '}'
spec     := item { ',' item }
item     := width | colour | 'on_' colour | style
```

- `width`: an integer, for `bar` only (`{bar:30}`); the `width` keyword stays as its default.
- `colour`: a FACE colour name, the foreground (`{percent:yellow}`); `on_` + a colour name, the background
  (`{eta:white,on_blue}`); `style`: a FACE style name (`{prefix:bold_on}`). Several items combine: `{eta:blue,bold_on}`.
- Fields: `bar`, `spinner`, `percent`, `count`, `speed`, `eta`, `message`, `prefix`, `suffix` (the text of
  `prefix_string`, `suffix_string`, which a program may change), and a new `elapsed`. Each field writes its value
  only: `{percent}` is `nnn%`, `{speed}` the number, `{eta}` the time. Words and spacing are the template's.
- An unknown field, colour or style, an unclosed brace: `error stop` in `initialize`, naming the template and the
  column.

The bar glyphs and their colours stay keywords (`filled_char_*`, `empty_char_*`, `partial_blocks`): a bar has two or
three parts, each with foreground and background, which a one-line spec would compress into something unreadable. The
other lines (scale, date and time, summary) stay switches; a `summary_template` can follow if wanted.

## 6. Implementation sketch

- `initialize` parses the template into an array of tokens. A token is either a literal (a UCS4 string) or a field (an
  enumerator and an `element_object` holding the field's colours). The parser runs once per `initialize`.
- `build_frame` becomes a loop over the tokens: a literal is copied, a field is rendered as today. The field
  renderers already exist (the bar body, `compact_real`, `hms`, `count_text`); only the order and the separators move
  out of the code.
- Without a template, `initialize` builds the tokens the v1 keywords describe: the same fields, in today's order, with
  today's spacing. The old keywords become a front end to the new mechanism instead of a second code path.

The acceptance test is already written: the 24 outputs of `docs/examples` must stay byte-identical with the v1 keywords,
and CI checks them on every push. New tests cover the parser: every field, every spec item, escaped braces, and every
error.

## 7. Versioning: additive, not breaking

The request was a "v2.0 template", but nothing in option A requires breaking v1. If the v1 keywords build a default
template, existing programs draw the same bytes, and the template is a new keyword. That is a minor release: **v1.6**.

A v2.0 would only be needed to *remove* the colour keywords. That removal buys a shorter `initialize` signature and
costs every existing program a rewrite. Recommendation: do not remove them; present the template as the way to lay out
a bar in the docs, keep the keywords documented as the compact form, and decide on removal only if they become a
maintenance burden in practice.

## 8. Effort

| Work | Size |
|---|---|
| Parser, tokens, `build_frame` loop, default template from the keywords | ~250 lines; 1 day |
| Tests: parser, errors, the 24 docs outputs unchanged | ~1/2 day |
| Docs: a tutorial chapter "Layout templates", reference page, cookbook recipes, GIF | ~1 day |

## 9. Open questions

1. Inline styles (A) or styles apart (B)? This note recommends A.
2. Unknown field, colour or style in a template: `error stop` (recommended), or a warning and ignore, as colours today?
3. Should colours in a template fail loudly, while the same colour names given as keywords keep failing silently? A
   v1.6 could make both loud; that is a behaviour change for programs with a typo in a colour, so a judgement call.
4. Styled literal text (`{"solve":cyan}`), or only fields take styles? Recommendation: fields only; `{prefix:cyan}`
   covers the common case.
5. User-defined fields (option C beneath A): now, later, or never? Recommendation: later, only if a real program needs
   it.
