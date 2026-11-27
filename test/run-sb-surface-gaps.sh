#!/bin/sh
# test/run-sb-surface-gaps.sh [MODUS] -- test/sb-surface-gaps.lisp under SBCL and modus; identical.
M=${1:-./modus}; here=$(cd "$(dirname "$0")" && pwd)
t=$(mktemp -d "${TMPDIR:-/tmp}/sbgaps.XXXXXX") || exit 2
mkdir -p "$t/reg/sr-probe" && echo '(asdf:defsystem "sr-probe")' > "$t/reg/sr-probe/sr-probe.asd"
export SR_TEST_DIR="$t/reg/" CL_SOURCE_REGISTRY="(:source-registry (:tree \"$t/reg/\") :ignore-inherited-configuration)"
sbcl --script "$here/sb-surface-gaps.lisp" > "$t/sbcl" 2>&1
timeout 300 "$M" --script "$here/sb-surface-gaps.lisp" 2>&1 | grep -v "^;" > "$t/modus"
if diff "$t/sbcl" "$t/modus"; then echo "sb-surface-gaps: $(wc -l < "$t/sbcl") lines identical to SBCL"; rc=0
else echo "FAIL: sb-surface-gaps differs from SBCL (above: < SBCL, > modus)"; rc=1; fi
rm -rf "$t"; exit $rc
