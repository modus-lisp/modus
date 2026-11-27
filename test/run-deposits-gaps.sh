#!/bin/sh
# test/run-deposits-gaps.sh [MODUS] -- test/deposits-gaps.lisp under SBCL and modus; identical.
M=${1:-./modus}; here=$(cd "$(dirname "$0")" && pwd)
t=$(mktemp -d "${TMPDIR:-/tmp}/depgaps.XXXXXX") || exit 2
sbcl --script "$here/deposits-gaps.lisp" > "$t/sbcl" 2>&1
timeout 300 "$M" --script "$here/deposits-gaps.lisp" 2>&1 | grep -v "^;" > "$t/modus"
if diff "$t/sbcl" "$t/modus"; then echo "deposits-gaps: $(wc -l < "$t/sbcl") lines identical to SBCL"; rc=0
else echo "FAIL: deposits-gaps differs from SBCL (above: < SBCL, > modus)"; rc=1; fi
rm -rf "$t"; exit $rc
