#!/bin/sh
# run-wine.sh - test the Windows hosted CLI (modus.exe) under Wine.
#
# Usage: test/win64/run-wine.sh MODUS_EXE
#
# WINE_RUN names the command that runs one Windows program under Wine, with the
# program path as its argument.  Default: `wine64' on PATH.  The Wine tree built
# here without root is run through /home/claude/tools/wine-run.sh, which
# overlays it on /usr (see docs/win64.md).
#
# The cases are test/win64/cases.sh, which run-vm.sh runs in a real Windows
# guest too.  The run starts with a deliberately WRONG expectation: it must
# fail, or the checks are not evidence of anything.

set -u
exe=${1:?usage: run-wine.sh MODUS_EXE}
run=${WINE_RUN:-wine64}
here=$(cd "$(dirname "$0")" && pwd)
pass=0; fail=0

run_form() {  # run_form FORM -> the image's stdout, prompts stripped, one value per line
  printf '%s\n' "$1" | $run "$exe" 2>/dev/null | sed 's/^> //'
}

verdict() {   # verdict NAME WANT OUTPUT
  if printf '%s\n' "$3" | grep -qxF -- "$2"; then
    echo "ok   $1 => $2"; pass=$((pass+1))
  else
    echo "FAIL $1: wanted line '$2', got:"; printf '%s\n' "$3" | sed 's/^/     | /' | head -20
    fail=$((fail+1))
  fi
}

windows=no; case "$exe" in *.exe) windows=yes ;; esac
check() {     # drive-letter cases are Windows semantics; elsewhere C:/x is relative
  case "$1" in drive*) [ $windows = yes ] || return 0 ;; esac
  verdict "$1" "$3" "$(run_form "$2")"
}
script() { verdict "$1" "$3" "$($run "$exe" --script "$2" 2>/dev/null)"; }
put() { printf '%s\n' "$2" > "$1"; }

echo "== control: a wrong expectation must be caught"
if run_form '(+ 1 2)' | grep -qxF -- '4'; then
  echo "CONTROL FAILED: the check accepted 4 for (+ 1 2); the harness cannot fail"
  exit 2
fi
echo "ok   control rejects 4 for (+ 1 2)"

echo "== modus.exe under Wine: $exe"
scratch=$(mktemp -d) || exit 1
exe=$(cd "$(dirname "$exe")" && pwd)/$(basename "$exe")
cd "$scratch" || exit 1
WDIR="Z:$scratch"                 # Wine's Z: is /
. "$here/cases.sh"
cd / && rm -rf "$scratch"

echo "== $pass passed, $fail failed"
[ "$fail" -eq 0 ]
