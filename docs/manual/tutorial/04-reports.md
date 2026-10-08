# 4. What the bar reports

Four switches add information to the bar, each with the colour keywords of an element:

<<< @/examples/snippets/march_4-init.f90

While running:

<<< @/examples/output/march_4-running.ansi{ansi}

At the end:

<<< @/examples/output/march_4.ansi{ansi}

- `add_progress_percent` adds the progress in percent, right after the suffix (here the right bracket ends with a
  space, `'] '`, to keep it apart).
- `add_progress_speed` adds the progress speed, in percent per second: the progress made since the previous drawing of
  the bar, over the time elapsed since then. It is a momentary speed, not an average, written in six characters:
  `12.50`, then `1234.5`, `123456`, `2.5e7` as it grows.
- `add_date_time` prints, when the bar is complete, a line with the date and time of the start and of the end.
- `add_scale_bar` prints, when the bar starts, a scale above it with `min_value` and `max_value`. It needs a bar at
  least 22 characters wide, and shows the values in five characters: `0.00`, `50.00`, `100.0`, `2000`, `2.5e7`.

The speed and the dates change from a run to the next: the outputs of this documentation show them as `nn.nn` and
`yyyy/mm/dd hh:mm:ss`.

::: tip What you learned
Progress in percent and speed, start and end time, the scale.
Reference: [The bar object](/guide/bar#initialize), [Behaviour and limitations](/guide/limitations).
:::

Next: [5. Spinners and counters](./05-spinners).
