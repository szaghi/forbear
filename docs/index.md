---
layout: home

hero:
  name: forbear
  text: Fortran (progress) B(e)ar environment
  tagline: "Progress bars and spinners for long-running Fortran programs: one object, three calls, pure Fortran 2008."
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
    details: "initialize, start, update: the bar redraws its line in place, then gives the terminal back."
    link: /manual/tutorial/01-first-bar
    linkText: A first bar
  - icon: 🧩
    title: Built from elements
    details: "Prefix, brackets, filled and empty characters, suffix: each element is a string of your choice, Unicode included."
    link: /guide/bar
    linkText: The bar object
  - icon: 🎨
    title: Colours and styles
    details: "A foreground colour, a background colour and a style for every element, by name: 17 colours, 16 styles."
    link: /guide/styling
    linkText: Colours and styles
  - icon: ⏱️
    title: What the bar reports
    details: "Progress in percent, progress speed, start and end time, a scale with the range of the run."
    link: /manual/tutorial/04-reports
    linkText: What the bar reports
  - icon: 🌀
    title: 40 spinners
    details: "Braille dots, blocks, arcs, moons: chosen by a key, alone or next to the bar."
    link: /guide/spinners
    linkText: Spinners
  - icon: 🔓
    title: Multi-licensed
    details: "GPL v3 for FOSS projects; BSD 2-Clause, BSD 3-Clause or MIT for closed source and commercial ones."
    link: /guide/#copyrights
    linkText: Copyrights
---

## Quick start

<p align="center"><img src="/taste.gif" alt="forbear progress bars and spinners in a terminal"></p>

This is a whole program: initialize the bar, start it, update it at every step.

<<< @/examples/snippets/minimal.f90

<<< @/examples/output/minimal.ansi{ansi}

Learn forbear step by step in the [tutorial](/manual/tutorial/01-first-bar), find quick answers in the
[cookbook](/manual/cookbook), look up every keyword in the [reference](/guide/features). Read
[Behaviour and limitations](/guide/limitations) before using forbear in a loop of more than 200 iterations.

## Authors

- Stefano Zaghi — [@szaghi](https://github.com/szaghi)

Contributions are welcome — see the [Contributing](/guide/contributing) page.
