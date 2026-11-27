#!/bin/sh
# test/run-uiop-gaps.sh [MODUS] -- test/uiop-gaps.lisp must print exactly what SBCL prints.
M=${1:-./modus}
here=$(cd "$(dirname "$0")" && pwd)
w=$(mktemp -d "${TMPDIR:-/tmp}/uiop-gaps.XXXXXX") || exit 2
mkdir "$w/s" "$w/m"
export XDG_CACHE_HOME=/cache/ XDG_CONFIG_HOME=/conf/ XDG_CONFIG_DIRS=/etc/xdg
sbcl --script "$here/uiop-gaps.lisp" "$w/s/" > "$w/sbcl.out" 2>&1
MODUS_FASL_DIR="$w/fasl" "$M" --script "$here/uiop-gaps.lisp" "$w/m/" > "$w/modus.out" 2>&1
if diff "$w/sbcl.out" "$w/modus.out"; then
  echo "uiop-gaps: PASS ($(wc -l < "$w/sbcl.out") lines identical to SBCL)"; rc=0
else
  echo "uiop-gaps: FAIL (diff above: < SBCL, > modus)"; rc=1
fi
rm -r "$w"
exit $rc
