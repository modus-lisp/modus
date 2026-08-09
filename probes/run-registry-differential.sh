#!/bin/bash
# run-registry-differential.sh <modus-binary> [outdir]
#
# Runs probes/registry-differential.lisp under SBCL (the conformance ORACLE)
# and under a Modus CLI, then diffs the two transcripts.
#
#   left  column (<) = SBCL / ANSI-correct
#   right column (>) = Modus
#
# Exit status 0 means Modus agrees with SBCL on every case.
#
# Both runs print LABEL<TAB>VALUE per case and REGISTRY-DIFFERENTIAL-DONE at
# the end.  A MISSING line is a FAILED case, not a negative result: the runner
# checks for the DONE marker on both sides and shouts if either is absent.
set -u
BIN="${1:?usage: run-registry-differential.sh <modus-binary> [outdir]}"
HERE="$(cd "$(dirname "$0")" && pwd)"
PROBE="$HERE/registry-differential.lisp"
OUT="${2:-${TMPDIR:-/tmp}/registry-diff.$$}"
mkdir -p "$OUT"

sbcl --noinform --disable-debugger --script "$PROBE" 2>"$OUT/sbcl.err" \
  | grep -aE '^[a-z]|^REGISTRY-DIFFERENTIAL-DONE' > "$OUT/sbcl.txt"
timeout 600 "$BIN" --load "$PROBE" --quit 2>"$OUT/modus.err" \
  | grep -aE '^[a-z]|^REGISTRY-DIFFERENTIAL-DONE' > "$OUT/modus.txt"

fail=0
for side in sbcl modus; do
  if ! grep -q 'REGISTRY-DIFFERENTIAL-DONE' "$OUT/$side.txt"; then
    echo "!! $side transcript is TRUNCATED (no DONE marker) -- see $OUT/$side.err"
    fail=1
  fi
done

echo "=== registry differential: SBCL(<) vs Modus(>) ==="
if diff "$OUT/sbcl.txt" "$OUT/modus.txt"; then
  echo "IDENTICAL -- $(grep -c . < "$OUT/sbcl.txt") lines agree"
else
  fail=1
fi
echo "(transcripts in $OUT)"
exit $fail
