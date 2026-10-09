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
    title: Your layout, your fields
    details: "One template lays the line out, '{bar:30} {percent:yellow} ETA {eta}'; a field of your own shows the residual of your solver, or anything else."
    link: /manual/tutorial/09-templates
    linkText: Layout templates
  - icon: ⏱️
    title: ETA, speed, count, summary
    details: "A smoothed speed and the time to the end, the count of steps done, start and end time, a closing line with duration and throughput."
    link: /manual/tutorial/04-reports
    linkText: What the bar reports
  - icon: 💬
    title: Talk while it runs
    details: "bar%write prints a line above the running bar, update(message=) shows the residual of the step at its end, suspend and resume let a library print freely: no broken lines."
    link: /manual/tutorial/06-terminal
    linkText: Talking while the bar runs
  - icon: 🪆
    title: Nested and open-ended loops
    details: "One bar per loop level, each on its own line; a bar for a solver that runs until it converges; finish for a loop left early."
    link: /manual/tutorial/07-nested
    linkText: Nested loops
  - icon: 📜
    title: Batch jobs and logs
    details: "Not on a terminal, the bar writes a plain line every 10%: no carriage returns, no escape codes in your SLURM log; a line every ten minutes, if asked. One variable turns it off."
    link: /manual/tutorial/08-logs
    linkText: Batch jobs and logs
  - icon: 🎨
    title: Smooth, coloured, Unicode
    details: "Eighth-of-a-cell partial blocks, 17 colours, 24-bit #rrggbb and 16 styles for every element, colour zones, rising ramps, a scanner pulse, seven-segment digits and dashboard themes, any Unicode string, 40 spinners."
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

## A 1980s dashboard

Segments lit and unlit, a redline, a rising tachometer, a scanner and seven-segment numbers: `theme`, `bar_zones`,
`bar_profile`, `pulse_trail` and `digits`.

![three bars at once: a blue-green segmented bar with a redline, an amber rising ramp, a red scanner, seven-segment numbers](/gifs/dashboard.gif){.gif}

All of it in [chapter 11 of the tutorial](/manual/tutorial/11-dashboard).

## 40 spinners

Every spinner of forbear is a bar too: here eighteen of them, each one a bar on its own line.

![eighteen spinners animating, one per line](/gifs/spinners.gif){.gif}

The whole catalogue is in [Spinners](/guide/spinners).

## Gallery

Every kind of output forbear draws, each the real output of a program of the docs (on a terminal, at the end of the
run or while running); the title leads to its recipe in the [cookbook](/manual/cookbook).

### The bar

**[The smallest bar](/manual/cookbook#the-smallest-bar)**

<<< @/examples/output/minimal.ansi{ansi}

**[A bar over the steps of a loop](/manual/cookbook#a-bar-over-the-steps-of-a-loop)**

<<< @/examples/output/march_1.ansi{ansi}

**[A loop of many iterations](/manual/cookbook#a-loop-of-many-iterations)**

<<< @/examples/output/many.ansi{ansi}

**[A smooth bar](/manual/cookbook#a-smooth-bar)**

<<< @/examples/output/march_2s.ansi{ansi}

**[A Unicode bar](/manual/cookbook#a-unicode-bar)**

<<< @/examples/output/march_2u.ansi{ansi}

### Colours and dashboard looks

**[A coloured, solid bar](/manual/cookbook#a-coloured-solid-bar)**

<<< @/examples/output/march_3.ansi{ansi}

**[Exact colours, #rrggbb](/manual/cookbook#exact-colours)**

<<< @/examples/output/hex.ansi{ansi}

**[A redline: colours by position](/manual/cookbook#a-redline-colours-by-position)**

<<< @/examples/output/zones.ansi{ansi}

**[A rising ramp](/manual/cookbook#a-rising-ramp)**

<<< @/examples/output/ramp.ansi{ansi}

**[Seven-segment numbers](/manual/cookbook#seven-segment-numbers)**

<<< @/examples/output/digits.ansi{ansi}

**[Three dashboard themes](/manual/cookbook#a-dashboard-look-in-one-keyword)**

<<< @/examples/output/themes.ansi{ansi}

### What the bar reports

**[Percent, count, speed, ETA, scale, times, summary](/manual/cookbook#percent-count-speed-eta-scale-times-summary)**

<<< @/examples/output/march_4.ansi{ansi}

**[A prefix that names the phase](/manual/cookbook#a-prefix-that-names-the-phase)**

<<< @/examples/output/phases.ansi{ansi}

### Spinners and counters

**[A spinner next to the bar](/manual/cookbook#a-spinner-next-to-the-bar)**

<<< @/examples/output/march_5-bar.ansi{ansi}

**[A spinner alone](/manual/cookbook#a-spinner-alone)**

<<< @/examples/output/march_5-spinner.ansi{ansi}

**[A percentage alone](/manual/cookbook#a-percentage-alone)**

<<< @/examples/output/march_5-counter.ansi{ansi}

### Talking while the bar runs

**[Lines above the bar, a message at its end](/manual/cookbook#lines-above-the-bar-a-message-at-its-end)**

<<< @/examples/output/march_6.ansi{ansi}

**[Output of a library while the bar runs](/manual/cookbook#output-of-a-library-while-the-bar-runs)**

<<< @/examples/output/march_6p.ansi{ansi}

### Many bars

**[A bar for each loop of a nest](/manual/cookbook#a-bar-for-each-loop-of-a-nest)**

<<< @/examples/output/march_7-running.ansi{ansi}

**[Several bars one after another](/manual/cookbook#several-bars-one-after-another)**

<<< @/examples/output/sequence.ansi{ansi}

### Layouts and fields

**[A layout of your own](/manual/cookbook#a-layout-of-your-own)**

<<< @/examples/output/layout-running.ansi{ansi}

**[A field of your own](/manual/cookbook#a-field-of-your-own)**

<<< @/examples/output/march_9.ansi{ansi}

### Unknown ends

**[A loop of unknown length](/manual/cookbook#a-loop-of-unknown-length)**

<<< @/examples/output/march_10-running.ansi{ansi}

**[A scanner for a loop of unknown length](/manual/cookbook#a-scanner-for-a-loop-of-unknown-length)**

<<< @/examples/output/scanner.ansi{ansi}

**[Leaving a loop early](/manual/cookbook#leaving-a-loop-early)**

<<< @/examples/output/march_10.ansi{ansi}

### Logs, batch jobs and other units

**[A clean log in a batch job](/manual/cookbook#a-clean-log-in-a-batch-job)**

<<< @/examples/output/march_8-log.ansi{ansi}

**[A log line every so often](/manual/cookbook#a-log-line-every-ten-minutes)**

<<< @/examples/output/march_8l.ansi{ansi}

**[No bars at all](/manual/cookbook#no-bars-at-all)**

<<< @/examples/output/march_8-disabled.ansi{ansi}

**[The bar on standard error](/manual/cookbook#the-bar-on-standard-error)**

<<< @/examples/output/stderr-bar.ansi{ansi}

### Mistakes stop the program

**[A scale on a narrow bar](/manual/cookbook#percent-count-speed-eta-scale-times-summary)**

<<< @/examples/output/scale_narrow.ansi{ansi}

## Where next

Learn forbear step by step in the [tutorial](/manual/tutorial/01-first-bar), find quick answers in the
[cookbook](/manual/cookbook), look up every keyword in the [reference](/guide/features).

## Authors

- Stefano Zaghi — [@szaghi](https://github.com/szaghi)

Contributions are welcome — see the [Contributing](/guide/contributing) page.
