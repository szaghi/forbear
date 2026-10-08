---
title: Spinners
---

# Spinners

![eighteen spinners animating, one per line](/gifs/spinners.gif){.gif}

`spinner_string` chooses a spinner by its key: pass one of the strings of the first column. The key is one of the
frames of the spinner, not always the first. The spinner moves one frame at every drawing of the bar, and starts again
after the last frame. A string that is not a key stops the program, with a message that names it.

How a spinner looks depends on the font of the terminal: the Braille patterns (`⠋`, `⣾`, ...) and the emoji need a
font that has them.

| `spinner_string` | Frames | Count |
|---|---|---|
| <code>&#124;</code> | <code>&#124;</code> <code>/</code> <code>-</code> <code>&#92;</code> | 4 |
| <code>⠋</code> | <code>⠋</code> <code>⠙</code> <code>⠹</code> <code>⠸</code> <code>⠼</code> <code>⠴</code> <code>⠦</code> <code>⠧</code> <code>⠇</code> <code>⠏</code> | 10 |
| <code>⣾</code> | <code>⣾</code> <code>⣽</code> <code>⣻</code> <code>⢿</code> <code>⡿</code> <code>⣟</code> <code>⣯</code> <code>⣷</code> | 8 |
| <code>⠓</code> | <code>⠋</code> <code>⠙</code> <code>⠚</code> <code>⠞</code> <code>⠖</code> <code>⠦</code> <code>⠴</code> <code>⠲</code> <code>⠳</code> <code>⠓</code> | 10 |
| <code>⠄</code> | <code>⠄</code> <code>⠆</code> <code>⠇</code> <code>⠋</code> <code>⠙</code> <code>⠸</code> <code>⠰</code> <code>⠠</code> <code>⠰</code> <code>⠸</code> <code>⠙</code> <code>⠋</code> <code>⠇</code> <code>⠆</code> | 14 |
| <code>⠐</code> | <code>⠋</code> <code>⠙</code> <code>⠚</code> <code>⠒</code> <code>⠂</code> <code>⠂</code> <code>⠒</code> <code>⠲</code> <code>⠴</code> <code>⠦</code> <code>⠖</code> <code>⠒</code> <code>⠐</code> <code>⠐</code> <code>⠒</code> <code>⠓</code> <code>⠋</code> | 17 |
| <code>⠒</code> | <code>⠈</code> <code>⠉</code> <code>⠋</code> <code>⠓</code> <code>⠒</code> <code>⠐</code> <code>⠐</code> <code>⠒</code> <code>⠖</code> <code>⠦</code> <code>⠤</code> <code>⠠</code> <code>⠠</code> <code>⠤</code> <code>⠦</code> <code>⠖</code> <code>⠒</code> <code>⠐</code> <code>⠐</code> <code>⠒</code> <code>⠓</code> <code>⠋</code> <code>⠉</code> <code>⠈</code> | 24 |
| <code>⠁</code> | <code>⠁</code> <code>⠁</code> <code>⠉</code> <code>⠙</code> <code>⠚</code> <code>⠒</code> <code>⠂</code> <code>⠂</code> <code>⠒</code> <code>⠲</code> <code>⠴</code> <code>⠤</code> <code>⠄</code> <code>⠄</code> <code>⠤</code> <code>⠠</code> <code>⠠</code> <code>⠤</code> <code>⠦</code> <code>⠖</code> <code>⠒</code> <code>⠐</code> <code>⠐</code> <code>⠒</code> <code>⠓</code> <code>⠋</code> <code>⠉</code> <code>⠈</code> <code>⠈</code> | 29 |
| <code>⣸</code> | <code>⢹</code> <code>⢺</code> <code>⢼</code> <code>⣸</code> <code>⣇</code> <code>⡧</code> <code>⡗</code> <code>⡏</code> | 8 |
| <code>⡐</code> | <code>⢄</code> <code>⢂</code> <code>⢁</code> <code>⡁</code> <code>⡈</code> <code>⡐</code> <code>⡠</code> | 7 |
| <code>⡀</code> | <code>⠁</code> <code>⠂</code> <code>⠄</code> <code>⡀</code> <code>⢀</code> <code>⠠</code> <code>⠐</code> <code>⠈</code> | 8 |
| <code>⡃⢐</code> | <code>⢀⠀</code> <code>⡀⠀</code> <code>⠄⠀</code> <code>⢂⠀</code> <code>⡂⠀</code> <code>⠅⠀</code> <code>⢃⠀</code> <code>⡃⠀</code> <code>⠍⠀</code> <code>⢋⠀</code> <code>⡋⠀</code> <code>⠍⠁</code> <code>⢋⠁</code> <code>⡋⠁</code> <code>⠍⠉</code> <code>⠋⠉</code> <code>⠋⠉</code> <code>⠉⠙</code> <code>⠉⠙</code> <code>⠉⠩</code> <code>⠈⢙</code> <code>⠈⡙</code> <code>⢈⠩</code> <code>⡀⢙</code> <code>⠄⡙</code> <code>⢂⠩</code> <code>⡂⢘</code> <code>⠅⡘</code> <code>⢃⠨</code> <code>⡃⢐</code> <code>⠍⡐</code> <code>⢋⠠</code> <code>⡋⢀</code> <code>⠍⡁</code> <code>⢋⠁</code> <code>⡋⠁</code> <code>⠍⠉</code> <code>⠋⠉</code> <code>⠋⠉</code> <code>⠉⠙</code> <code>⠉⠙</code> <code>⠉⠩</code> <code>⠈⢙</code> <code>⠈⡙</code> <code>⠈⠩</code> <code>⠀⢙</code> <code>⠀⡙</code> <code>⠀⠩</code> <code>⠀⢘</code> <code>⠀⡘</code> <code>⠀⠨</code> <code>⠀⢐</code> <code>⠀⡐</code> <code>⠀⠠</code> <code>⠀⢀</code> <code>⠀⡀</code> | 56 |
| <code>┤</code> | <code>┤</code> <code>┘</code> <code>┴</code> <code>└</code> <code>├</code> <code>┌</code> <code>┬</code> <code>┐</code> | 8 |
| <code>✶</code> | <code>✶</code> <code>✸</code> <code>✹</code> <code>✺</code> <code>✹</code> <code>✷</code> | 6 |
| <code>&#95;</code> | <code>&#95;</code> <code>&#95;</code> <code>&#95;</code> <code>-</code> <code>&#96;</code> <code>&#96;</code> <code>´</code> <code>-</code> <code>&#95;</code> <code>&#95;</code> <code>&#95;</code> | 11 |
| <code>▃</code> | <code>▁</code> <code>▃</code> <code>▄</code> <code>▅</code> <code>▆</code> <code>▇</code> <code>▆</code> <code>▅</code> <code>▄</code> <code>▃</code> | 10 |
| <code>▉</code> | <code>▏</code> <code>▎</code> <code>▍</code> <code>▌</code> <code>▋</code> <code>▊</code> <code>▉</code> <code>▊</code> <code>▋</code> <code>▌</code> <code>▍</code> <code>▎</code> | 12 |
| <code>@</code> | <code>&nbsp;</code> <code>.</code> <code>o</code> <code>O</code> <code>@</code> <code>&#42;</code> <code>&nbsp;</code> | 7 |
| <code>°</code> | <code>.</code> <code>o</code> <code>O</code> <code>°</code> <code>O</code> <code>o</code> <code>.</code> | 7 |
| <code>▒</code> | <code>▓</code> <code>▒</code> <code>░</code> | 3 |
| <code>⠂</code> | <code>⠁</code> <code>⠂</code> <code>⠄</code> <code>⠂</code> | 4 |
| <code>▖</code> | <code>▖</code> <code>▘</code> <code>▝</code> <code>▗</code> | 4 |
| <code>◢</code> | <code>◢</code> <code>◣</code> <code>◤</code> <code>◥</code> | 4 |
| <code>◜</code> | <code>◜</code> <code>◠</code> <code>◝</code> <code>◞</code> <code>◡</code> <code>◟</code> | 6 |
| <code>⊙</code> | <code>◡</code> <code>⊙</code> <code>◠</code> | 3 |
| <code>◰</code> | <code>◰</code> <code>◳</code> <code>◲</code> <code>◱</code> | 4 |
| <code>◴</code> | <code>◴</code> <code>◷</code> <code>◶</code> <code>◵</code> | 4 |
| <code>◐</code> | <code>◐</code> <code>◓</code> <code>◑</code> <code>◒</code> | 4 |
| <code>⊶</code> | <code>⊶</code> <code>⊷</code> | 2 |
| <code>▫</code> | <code>▫</code> <code>▪</code> | 2 |
| <code>□</code> | <code>□</code> <code>■</code> | 2 |
| <code>▪</code> | <code>■</code> <code>□</code> <code>▪</code> <code>▫</code> | 4 |
| <code>▯</code> | <code>▮</code> <code>▯</code> | 2 |
| <code>⦿</code> | <code>⦾</code> <code>⦿</code> | 2 |
| <code>◍</code> | <code>◍</code> <code>◌</code> | 2 |
| <code>◉</code> | <code>◉</code> <code>◎</code> | 2 |
| <code>㊂</code> | <code>㊂</code> <code>㊀</code> <code>㊁</code> | 3 |
| <code>(&nbsp;&nbsp;●&nbsp;&nbsp;&nbsp;)</code> | <code>(&nbsp;●&nbsp;&nbsp;&nbsp;&nbsp;)</code> <code>(&nbsp;&nbsp;●&nbsp;&nbsp;&nbsp;)</code> <code>(&nbsp;&nbsp;&nbsp;●&nbsp;&nbsp;)</code> <code>(&nbsp;&nbsp;&nbsp;&nbsp;●&nbsp;)</code> <code>(&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;●)</code> <code>(&nbsp;&nbsp;&nbsp;&nbsp;●&nbsp;)</code> <code>(&nbsp;&nbsp;&nbsp;●&nbsp;&nbsp;)</code> <code>(&nbsp;&nbsp;●&nbsp;&nbsp;&nbsp;)</code> <code>(&nbsp;●&nbsp;&nbsp;&nbsp;&nbsp;)</code> <code>(●&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;)</code> | 10 |
| <code>🌔</code> | <code>🌑&nbsp;</code> <code>🌒&nbsp;</code> <code>🌓&nbsp;</code> <code>🌔&nbsp;</code> <code>🌕&nbsp;</code> <code>🌖&nbsp;</code> <code>🌗&nbsp;</code> <code>🌘&nbsp;</code> | 8 |
| <code>🚶</code> | <code>🚶&nbsp;</code> <code>🏃&nbsp;</code> | 2 |

## Example

<<< @/examples/snippets/march_5-bar.f90

<<< @/examples/output/march_5-bar.ansi{ansi}
