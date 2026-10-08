# 4. What the bar reports

![march with percent, count, speed, ETA, scale, times and summary](/gifs/march_4.gif){.gif}

Seven switches add information to the bar, each with the colour keywords of an element:

<<< @/examples/snippets/march_4-init.f90

While running:

<<< @/examples/output/march_4-running.ansi{ansi}

At the end:

<<< @/examples/output/march_4.ansi{ansi}

On the bar line, in this order:

- `add_progress_percent`: the progress in percent, right after the suffix, always one space apart from it:
  ` 50%`, ` 100%`.
- `add_progress_count`: the current value and `max_value`, `25/50`. With whole-number bounds the count is an integer,
  right-aligned to the width of `max_value`, so the line does not shift; otherwise it is a real in six characters.
- `add_progress_speed`: the speed, in percent per second. It is smoothed: an exponential moving average of the speed
  between two drawings, which weights the last one by `smoothing` (0.3 by default, as tqdm does). `smoothing=1` gives
  the momentary speed, `smoothing=0` the average since the start.
- `add_eta`: the estimated time to the end, `ETA hh:mm:ss`: what is left of the range, over the smoothed speed. It is
  `--:--:--` until the speed is known, at the first drawing.

On their own lines:

- `add_scale_bar` prints, when the bar starts, a scale above it with `min_value` and `max_value`. It needs a bar at
  least 22 characters wide, and shows the values in five characters: `0.00`, `50.00`, `100.0`, `2000`, `2.5e7`.
- `add_date_time` prints, when the bar is complete, a line with the date and time of the start and of the end.
- `add_summary` prints, when the bar is complete, how long the run took and its mean speed: per second in the units
  of the range with `add_progress_count`, in percent per second without.

The speed, the ETA, the summary and the dates change from a run to the next: the outputs of this documentation show
them as `nn.nn`, `hh:mm:ss`, `n.nn s` and `yyyy/mm/dd hh:mm:ss`.

::: tip What you learned
Percent, count, smoothed speed, ETA; scale, start and end time, summary.
Reference: [The bar object](/guide/bar#initialize), [Number formats](/guide/limitations#number-formats).
:::

Next: [5. Spinners and counters](./05-spinners).
