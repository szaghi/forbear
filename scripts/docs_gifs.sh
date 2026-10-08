#!/usr/bin/env bash
# Record the animated GIFs of the documentation, in docs/public/gifs/, with VHS (https://github.com/charmbracelet/vhs).
#
#   docs/gifs/<name>.tape      one recording each: typed commands in a real terminal (ttyd + headless Chrome), so the
#                              bars are those a user sees; docs/gifs/settings.tape has the settings they share
#   docs/gifs/src/*.f90        programs recorded only (the showcase, the spinner gallery); the tutorial chapters are
#                              recorded from their own programs, docs/examples/src/march_*.f90
#
# Every program runs in its own directory, build/docs-gifs/run/<name> (git-ignored), under the name the documentation
# gives it (march for the tutorial chapters). The library is rebuilt by FoBiS (mode static-gnu). Times and speeds are
# those of the recording machine: unlike the outputs of scripts/docs_examples.sh, the GIFs change at every recording.
#
# Usage: bash scripts/docs_gifs.sh [name ...]      (no name: every tape)
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
build=$root/build/docs-gifs # git-ignored
fc=${FC:-gfortran}
command -v vhs > /dev/null || { echo "docs_gifs: vhs not found, see https://github.com/charmbracelet/vhs" >&2; exit 1; }

mkdir -p "$root/build"
(cd "$root" && fobis build --mode static-gnu --fc "$fc") > "$root/build/docs-gifs.log" 2>&1 || {
  cat "$root/build/docs-gifs.log"; echo "docs_gifs: library build failed" >&2; exit 1; }
rm -rf -- "$build"
mkdir -p "$build/bin" "$root/docs/public/gifs"
for src in "$root"/docs/examples/src/march_*.f90 "$root"/docs/gifs/src/*.f90; do
  name=$(basename "$src" .f90)
  "$fc" -I"$root/static/mod" -o "$build/bin/$name" "$src" "$root/static/libforbear.a"
  mkdir -p "$build/run/$name"
  case "$name" in march_*) as=march ;; *) as=$name ;; esac
  ln -s "$build/bin/$name" "$build/run/$name/$as"
done

if [ $# -eq 0 ]; then
  set -- $(cd "$root/docs/gifs" && ls *.tape | sed 's/\.tape$//' | grep -v '^settings$')
fi
cd "$root"
for name in "$@"; do
  echo "docs_gifs: recording $name"
  vhs "docs/gifs/$name.tape" > "$build/$name.log" 2>&1 || { cat "$build/$name.log"; exit 1; }
done
echo "docs_gifs: $# GIFs in docs/public/gifs"
