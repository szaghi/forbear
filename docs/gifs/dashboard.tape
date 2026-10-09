# Recording of docs/public/gifs/dashboard.gif: run by scripts/docs_gifs.sh from the root of the repository.
# The seven-segment digits need a font that has them: the subset the docs ship, installed for the recording
# (docs/public/fonts/forbear-segments.woff2, as a TTF in ~/.local/share/fonts).
Output docs/public/gifs/dashboard.gif
Source docs/gifs/settings.tape
Set FontFamily "DejaVu Sans Mono, Forbear Segments"
Set Height 210
Hide
Type "cd build/docs-gifs/run/dashboard && export PATH=$PWD:$PATH PS1='$ ' && clear"
Enter
Sleep 500ms
Show
Type "dashboard"
Sleep 400ms
Enter
Sleep 5s
