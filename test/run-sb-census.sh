#!/bin/sh
# test/run-sb-census.sh [MODUS] -- test/sb-census.lisp must print exactly what SBCL prints.
M=${1:-./modus}
here=$(cd "$(dirname "$0")" && pwd)
w=$(mktemp -d "${TMPDIR:-/tmp}/sb-census.XXXXXX") || exit 2
mkdir "$w/s" "$w/m"
sbcl --script "$here/sb-census.lisp" "$w/s/" > "$w/sbcl.out" 2>&1
MODUS_FASL_DIR="$w/fasl" timeout 300 "$M" --script "$here/sb-census.lisp" "$w/m/" 2>&1 | grep -v '^;' > "$w/modus.out"
if diff "$w/sbcl.out" "$w/modus.out"; then
  echo "sb-census: PASS ($(wc -l < "$w/sbcl.out") lines identical to SBCL)"; rc=0
else
  echo "sb-census: FAIL (diff above: < SBCL, > modus)"; rc=1
fi
rm -r "$w"
exit $rc
