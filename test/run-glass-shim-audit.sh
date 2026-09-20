#!/bin/bash
# run-glass-shim-audit.sh — THE WHOLE SBCL SURFACE GLASS USES, IN ONE RUN.
#
#   test/run-glass-shim-audit.sh [MODUS-BINARY]
#
# The shim is baked into the image, so each gap found by running glass costs a
# REBUILD, and running glass only ever finds the gaps on the path the first
# client takes.  This asks the whole surface at once, in the shape glass writes
# it — see the header of test/glass-shim-audit.lisp for the call sites.
#
# SBCL RUNS FIRST AND MUST SCORE 100%.  Every probe is a transcription of a real
# glass call site, so SBCL failing one means the TRANSCRIPTION is wrong, and the
# script says so rather than reporting a modus gap that is not there.
set -u
BIN=${1:-./modus}
cd "$(dirname "$0")/.." || exit 1
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }

WORK=$(mktemp -d) || exit 1
trap 'rm -rf "$WORK"' EXIT

echo "=== THE GLASS COMPATIBILITY SURFACE, AUDITED ==="
echo "modus: $BIN"
echo

echo "-- sbcl (validating the probes themselves) --"
if timeout 300 sbcl --script test/glass-shim-audit.lisp > "$WORK/sbcl.txt" 2>"$WORK/sbcl.err"; then
  echo "   $(grep -c '^ok ' "$WORK/sbcl.txt") ok, 0 gaps — the probes are right"
else
  echo "HARNESS FAILURE: the ORACLE does not pass its own probes.  A probe below"
  echo "is not the shape glass actually writes; fix the probe, not the shim."
  grep '^GAP' "$WORK/sbcl.txt"
  sed -n '1,15p' "$WORK/sbcl.err"
  exit 2
fi
echo

echo "-- modus --"
timeout 600 "$BIN" --script test/glass-shim-audit.lisp > "$WORK/modus.txt" 2>"$WORK/modus.err"
RC=$?
grep -E '^(===|ok |GAP |[0-9]+ ok|  - |PASS|FAIL|THE GAPS)' "$WORK/modus.txt"
echo

if [ "$RC" -eq 0 ] && grep -q '^PASS' "$WORK/modus.txt"; then
  exit 0
else
  echo "(rc=$RC)"
  if ! grep -q 'ok\|GAP' "$WORK/modus.txt"; then
    echo "modus produced no probe output at all — stderr follows:"
    sed -n '1,30p' "$WORK/modus.err"
  fi
  exit 1
fi
