---
layout: home

hero:
  name: forbear
  text: Progress bars for Fortran
  tagline: "Bars, spinners, ETA and messages for long-running Fortran programs: one object, three calls, pure Fortran 2008. On a terminal it animates; in a batch log it writes clean lines."
  image:
    src: /logo.svg
    alt: forbear
  actions:
    - theme: brand
      text: Tutorial
      link: /manual/tutorial/01-first-bar
    - theme: alt
      text: Cookbook
      link: /manual/cookbook
    - theme: alt
      text: Reference
      link: /guide/features
    - theme: alt
      text: API
      link: /api/
    - theme: alt
      text: View on GitHub
      link: https://github.com/szaghi/forbear

features:
  - icon: 📊
    title: A bar in three calls
    details: "initialize, start, update: the bar redraws its line in place, at most ten times a second, and gives the terminal back at 100%."
    link: /manual/tutorial/01-first-bar
    linkText: A first bar
  - icon: ⏱️
    title: ETA, speed, count, summary
    details: "A smoothed speed and the time to the end, the count of steps done, start and end time, a closing line with duration and throughput."
    link: /manual/tutorial/04-reports
    linkText: What the bar reports
  - icon: 💬
    title: Talk while it runs
    details: "bar%write prints a line above the running bar, update(message=) shows the residual of the step at its end: no broken lines."
    link: /manual/tutorial/06-terminal
    linkText: Talking while the bar runs
  - icon: 🪆
    title: Nested loops
    details: "One bar per loop level, each on its own line: time steps above, iterations below, cleared when done."
    link: /manual/tutorial/07-nested
    linkText: Nested loops
  - icon: 📜
    title: Batch jobs and logs
    details: "Not on a terminal, the bar writes a plain line every 10%: no carriage returns, no escape codes in your SLURM log. One variable turns it off."
    link: /manual/tutorial/08-logs
    linkText: Batch jobs and logs
  - icon: 🎨
    title: Smooth, coloured, Unicode
    details: "Eighth-of-a-cell partial blocks, 17 colours and 16 styles for every element, any Unicode string, 40 spinners."
    link: /guide/styling
    linkText: Colours and styles
---

<div class="showcase">

![a pretend CFD run: a mesh bar, then nested time-step and Newton bars, with checkpoints printed above, a residual and the ETA, and a closing summary](/gifs/hero.gif){.gif}

<p class="caption">A (pretend) CFD run: <a href="/forbear/manual/tutorial/07-nested">nested bars</a>, <a href="/forbear/manual/tutorial/06-terminal">messages above the bar</a>, <a href="/forbear/manual/tutorial/04-reports">ETA and summary</a>. Every frame is drawn by forbear.</p>

</div>

## Quick start

Initialize the bar, start it, update it at every step: this is a whole program.

<<< @/examples/snippets/minimal.f90

<<< @/examples/output/minimal.ansi{ansi}

## 40 spinners

Every spinner of forbear is a bar too: here eighteen of them, each one a bar on its own line.

![eighteen spinners animating, one per line](/gifs/spinners.gif){.gif}

The whole catalogue is in [Spinners](/guide/spinners).

## Where next

Learn forbear step by step in the [tutorial](/manual/tutorial/01-first-bar), find quick answers in the
[cookbook](/manual/cookbook), look up every keyword in the [reference](/guide/features).

## Authors

- Stefano Zaghi — [@szaghi](https://github.com/szaghi)

Contributions are welcome — see the [Contributing](/guide/contributing) page.
