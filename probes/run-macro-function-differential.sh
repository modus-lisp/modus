#!/bin/bash
# run-macro-function-differential.sh <modus-binary> [outdir]
#
# Runs probes/macro-function-differential.lisp under SBCL (the conformance
# ORACLE) and under a Modus CLI, then diffs the two transcripts.
#
#   left  column (<) = SBCL / ANSI-correct
#   right column (>) = Modus
#
# Exit status 0 means Modus agrees with SBCL on every case.
#
# A MISSING line is a FAILED case, not a negative result: the runner checks
# for the DONE marker on both sides and shouts if either is absent.
set -u
BIN="${1:?usage: run-macro-function-differential.sh <modus-binary> [outdir]}"
HERE="$(cd "$(dirname "$0")" && pwd)"
PROBE="$HERE/macro-function-differential.lisp"
OUT="${2:-${TMPDIR:-/tmp}/macro-function-diff.$$}"
mkdir -p "$OUT"

sbcl --noinform --disable-debugger --script "$PROBE" 2>"$OUT/sbcl.err" \
  | grep -aE '^(mf|expand|call|fboundp|walker)\.|^MACRO-FUNCTION-DIFFERENTIAL-DONE' > "$OUT/sbcl.txt"
timeout 600 "$BIN" --load "$PROBE" --quit 2>"$OUT/modus.err" \
  | grep -aE '^(mf|expand|call|fboundp|walker)\.|^MACRO-FUNCTION-DIFFERENTIAL-DONE' > "$OUT/modus.txt"

fail=0
for side in sbcl modus; do
  if ! grep -q 'MACRO-FUNCTION-DIFFERENTIAL-DONE' "$OUT/$side.txt"; then
    echo "!! $side transcript is TRUNCATED (no DONE marker) -- see $OUT/$side.err"
    fail=1
  fi
done

echo "=== macro-function differential: SBCL(<) vs Modus(>) ==="
if diff "$OUT/sbcl.txt" "$OUT/modus.txt"; then
  echo "IDENTICAL -- $(grep -c . < "$OUT/sbcl.txt") lines agree"
else
  fail=1
fi
echo "(transcripts in $OUT)"
exit $fail
