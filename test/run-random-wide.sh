#!/bin/sh
# test/run-random-wide.sh [MODUS] -- test/random-wide.lisp must print exactly what SBCL prints.
M=${1:-./modus}; here=$(cd "$(dirname "$0")" && pwd)
w=$(mktemp -d "${TMPDIR:-/tmp}/random-wide.XXXXXX") || exit 2
sbcl --script "$here/random-wide.lisp" > "$w/sbcl" 2>&1
MODUS_FASL_DIR="$w/fasl" timeout 300 "$M" --script "$here/random-wide.lisp" 2>&1 | grep -v '^;' > "$w/modus"
if diff "$w/sbcl" "$w/modus"; then echo "random-wide: PASS ($(wc -l < "$w/sbcl") lines identical to SBCL)"; rc=0
else echo "random-wide: FAIL (diff above: < SBCL, > modus)"; rc=1; fi
rm -r "$w"; exit $rc
