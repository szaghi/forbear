#!/usr/bin/env bash
# Build and run the documentation examples, regenerating everything the pages include from them.
#
#   docs/examples/src/*.f90    the example programs (hand-written), with marker comments:
#                                !run ID COMMAND        a run shown in the pages (!run -s: with its exit status;
#                                                       !run -f K: the screen while the K-th frame of the bar is shown,
#                                                       frame 1 being the one drawn by start)
#                                !region NAME ... !endregion NAME   a part of the program included on its own
#                                !as NAME               its runs call it NAME (the chapters of the tutorial are all march)
#   docs/examples/snippets/    generated: <program>.f90 without the markers, and <program>-<region>.f90
#   docs/examples/output/      generated: <ID>.ansi, "$ COMMAND" then what the terminal finally shows (standard output
#                              and error, colours kept): a bar redraws its line many times, scripts/ansi_screen.py
#                              replays the frames and keeps the last one, and replaces the progress speed, the ETA,
#                              the summary and the dates, which depend on the clock, with placeholders
#
# The library is rebuilt from scratch by FoBiS (mode static-gnu) and the examples are built by the same compiler, gfortran
# or $FC. A run happens in a pseudo-terminal (`script`, from util-linux) in a scratch directory, with HOME pointing there
# and a minimal environment, so nothing outside it is read or written.
#
# Usage: bash scripts/docs_examples.sh            (FC=gfortran-14 bash scripts/docs_examples.sh: another compiler)
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
ex=$root/docs/examples
build=$root/build/docs-examples # git-ignored
run_dir=$build/run
home=/home/user                 # how the run directory is shown

fc=${FC:-gfortran}
mkdir -p "$root/build"
(cd "$root" && fobis clean --mode static-gnu && fobis build --mode static-gnu --fc "$fc") \
  > "$root/build/docs-examples.log" 2>&1 || {
  cat "$root/build/docs-examples.log"; echo "docs_examples: library build failed" >&2; exit 1; }
rm -rf -- "$build"
mkdir -p "$build/bin" "$run_dir" "$ex/snippets" "$ex/output"
rm -f -- "$ex"/snippets/*.f90 "$ex"/output/*.ansi

# snippets: the whole program and each region, without the markers, dedented
dedent() { awk '{l[NR]=$0; if ($0 ~ /[^ ]/) {match($0, /^ */); if (m == "" || RLENGTH < m) m = RLENGTH}}
                END {for (i = 1; i <= NR; i++) print substr(l[i], m + 1)}' "$1"; }
for src in "$ex"/src/*.f90; do
  name=$(basename "$src" .f90)
  grep -Ev '^ *!(run|region|endregion|as) ' "$src" > "$ex/snippets/$name.f90" || true
  for region in $(sed -n 's/^ *!region \([A-Za-z0-9_-]*\).*/\1/p' "$src"); do
    awk -v r="$region" '$1 == "!endregion" && $2 == r {on = 0}
                        on && $0 !~ /^ *!(run|region|endregion|as) / {print}
                        $1 == "!region" && $2 == r {on = 1}' "$src" > "$build/region.f90"
    dedent "$build/region.f90" > "$ex/snippets/$name-$region.f90"
  done
done

# programs
for src in "$ex"/src/*.f90; do
  "$fc" -I"$root/static/mod" -o "$build/bin/$(basename "$src" .f90)" "$src" "$root/static/libforbear.a"
done

# runs, in the order of the files and of the lines
path=$build/bin
run() { # run [-s] [-f K] ID COMMAND
  local show=0 status=0 frame=()
  while :; do
    case "$1" in
      -s ) show=1; shift ;;
      -f ) frame=(--frame "$2"); shift 2 ;;
      * ) break ;;
    esac
  done
  local id=$1; shift
  local cmd="$*"
  # in a pseudo-terminal, as in a real one: forbear detects it (and a redirection in COMMAND is not a terminal);
  # FORBEAR_MIN_INTERVAL=0 draws at every update, so that the K-th frame does not depend on the speed of the machine
  (cd "$run_dir" && env -i HOME="$run_dir" PATH="$path:/usr/bin:/bin" SHELL=/bin/bash LC_ALL=C.UTF-8 \
                   GFORTRAN_ERROR_BACKTRACE=0 FORBEAR_MIN_INTERVAL=0 \
                   script -qefc "$cmd" /dev/null < /dev/null > "$build/capture" 2>&1) || status=$?
  {
    printf '$ %s\n' "$cmd"
    python3 "$root/scripts/ansi_screen.py" "${frame[@]}" < "$build/capture"
    if [ $show = 1 ]; then printf '[exit status %d]\n' "$status"; fi
  } | sed -e "s|$run_dir|$home|g" > "$ex/output/$id.ansi"
}
for src in "$ex"/src/*.f90; do
  path=$build/bin
  as=$(sed -n 's/^ *!as \([A-Za-z0-9_-]*\).*/\1/p' "$src" | head -n 1)
  if [ -n "$as" ]; then
    path=$build/as/$(basename "$src" .f90)
    mkdir -p "$path" && ln -sf "$build/bin/$(basename "$src" .f90)" "$path/$as"
    path=$path:$build/bin
  fi
  while IFS= read -r line; do
    line=${line#*!run }
    opts=()
    while :; do
      case "${line%% *}" in
        -s ) opts+=(-s); line=${line#-s } ;;
        -f ) line=${line#-f }; opts+=(-f "${line%% *}"); line=${line#* } ;;
        * ) break ;;
      esac
    done
    run "${opts[@]}" "${line%% *}" "${line#* }"
  done < <(grep -E '^ *!run ' "$src" || true)
done
echo "docs_examples: $(ls "$ex"/src/*.f90 | wc -l) programs, $(ls "$ex"/output/*.ansi | wc -l) runs"
