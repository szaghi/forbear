---
title: Layout templates
---

# Layout templates

A template lays out the bar line: literal text, and fields in braces.

```fortran
call bar%initialize(template='march [{bar:30}] {percent:yellow} {count:cyan} ETA {eta:blue} {message}', &
                    max_value=real(steps, R8P))
```

Without a template, the keywords of [initialize](./bar#initialize) describe the line, in the fixed order of
[The line](./bar#the-line). The two are equivalent: forbear builds the same internal layout from either, and the
keywords draw exactly what they drew before templates existed.

## Syntax

```
template := { text | field }
text     := any characters but braces; {{ and }} write a brace
field    := { name [ : spec ] }
spec     := item { , item }
item     := width (bar only) | colour | on_colour | style
```

## Fields

| Field | Shows | Width |
|---|---|---|
| `{bar}` | the bar body: done part, partial block, remaining part | `width` keyword, or `{bar:n}` |
| `{percent}` | the progress, `nnn%` | 4 |
| `{count}` | the current value and `max_value`, `25/50`; what is done, `25`, if [indeterminate](./bar#initialize) | that of `max_value`, twice, and the slash; growing, if indeterminate |
| `{speed}` | the smoothed speed, in percent per second, `nnn.nn`; what is done per second, if indeterminate | 6 |
| `{eta}` | the estimated time to the end, `hh:mm:ss`, `--:--:--` until known | 8 |
| `{elapsed}` | the time since the start, `hh:mm:ss` | 8 |
| `{spinner}` | the current frame of the spinner of `spinner_string` | that of its frames |
| `{message}` | the message of the last `update` | variable |
| `{prefix}`, `{suffix}` | the strings of `prefix_string`, `suffix_string` | as given |
| `{name}` | a field of the program, added with [`add_field`](#fields-of-the-program) | its own |

A field takes the colours of its keywords (`progress_percent_color_fg`, ...) unless its spec gives others.

## Spec

| Item | Example | Meaning |
|---|---|---|
| an integer | `{bar:30}` | the width of the bar; `{bar}` only |
| a colour | `{percent:yellow}` | foreground colour |
| `on_` + a colour | `{eta:on_blue}` | background colour |
| a style | `{prefix:bold_on}` | style |

Items combine, separated by commas: `{eta:white,on_blue,bold_on}`. The names are those of
[Colours and styles](./styling). The bar has no colours in its spec: its done and remaining parts are coloured by the
`filled_char_*` and `empty_char_*` keywords.

## Errors

A wrong template stops the program, with a message on standard error that names the mistake and the template:

| Mistake | Found by |
|---|---|
| `{` not closed, `}` not opened | `initialize` |
| an unknown colour or style in a spec | `initialize` |
| a width for a field other than `{bar}`, two `{bar}` | `initialize` |
| `{spinner}` without `spinner_string` | `initialize` |
| `{percent}` or `{eta}` in an indeterminate bar | `initialize` |
| a field neither of forbear nor added with `add_field` | `start` |

## Fields of the program

A program adds fields of its own: a type that extends `field_object`, with a `render` function.

```fortran
use forbear, only : field_object, progress_object

type, extends(field_object) :: residual_field
   real(R8P), pointer :: value => null()
   contains
      procedure, pass(self) :: render
endtype residual_field
```

```fortran
function render(self, progress) result(text)
class(residual_field), intent(in) :: self
type(progress_object), intent(in) :: progress
character(len=:), allocatable     :: text
```

`render` is called at every drawing. `progress` holds what the bar knows:

| Component | Type | Meaning |
|---|---|---|
| `current` | `real(real64)` | current value, clamped to the range |
| `min_value`, `max_value` | `real(real64)` | the range |
| `fraction` | `real(real64)` | fraction of the range done, in [0, 1] |
| `percent` | `integer(int32)` | progress in percent, truncated |
| `rate` | `real(real64)` | smoothed rate, fraction of the range per second; 0 until known |
| `elapsed` | `real(real64)` | seconds since the start |
| `eta` | `real(real64)` | seconds to the end; negative until known |
| `indeterminate` | `logical` | the total is unknown: `current` is not clamped, `rate` is what is done per second, `fraction` and `percent` are 0, `eta` negative |

```fortran
call bar%initialize(template='... {residual} ...', ...)
call bar%add_field('residual', field)   ! after initialize, before start
call bar%start
```

`add_field` stores a copy of the field; a pointer in it still points where it did. `initialize` removes the fields;
adding a field with the name of an existing one replaces it; a name of a field of forbear stops the program. The
complete program is [chapter 9](/manual/tutorial/09-templates) of the tutorial.

## The other lines

The template is the bar line. The scale, the start and end time and the summary keep their switches (`add_scale_bar`,
`add_date_time`, `add_summary`); with a template the scale is drawn right above the bar body, and the summary counts in
the units of the range when the template has `{count}`.
