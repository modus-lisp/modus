#!/bin/bash
# run-worker-intern.sh — INTERNING A FRESH SYMBOL ON A WORKER THREAD KILLS THE PROCESS.
#
#   test/run-worker-intern.sh [MODUS-BINARY] [RUNS] [MAIN_ROUNDS]
#
# Runs the four arms of test/hosted-worker-intern.lisp, EACH IN ITS OWN PROCESS
# — which is not tidiness: the failing arm takes the process down, so a single
# process could not report the arms after it.
#
# EXPECTED ON THIS TREE: cons / format / intern-same are clean and
# `intern-fresh' is not, so this EXITS NON-ZERO.  It is a reproducer, and a
# reproducer that passes has stopped reproducing.  When the defect is fixed all
# four arms are clean and this exits 0.
#
# RUNS defaults to 3 because the arms are layout-sensitive and a single clean
# run means "not reproduced in this shape", never "correct".  The rate is the
# result; read it, not one line.
#
# MAIN_ROUNDS defaults to 0 — the main thread allocates NOTHING while the
# worker runs.  That is deliberate and it is the control that rules out a
# two-thread heap race: the failure survives it.
set -u
BIN=${1:-./modus}
RUNS=${2:-3}
MAIN_ROUNDS=${3:-0}
cd "$(dirname "$0")/.." || exit 1
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }

RC=0
for arm in cons format intern-same intern-fresh; do
  pass=0; fail=0; sig=""
  for _ in $(seq 1 "$RUNS"); do
    out=$(WORKER_ARM="$arm" MAIN_ROUNDS="$MAIN_ROUNDS" \
          "$BIN" --script test/hosted-worker-intern.lisp 2>/dev/null)
    if [ $? -eq 0 ]; then
      pass=$((pass + 1))
    else
      fail=$((fail + 1))
      [ -z "$sig" ] && sig=$(printf '%s' "$out" | grep -m1 -oE 'MVM LONGJMP[^,]*|TYPE-ERROR|SIMPLE-ERROR' )
    fi
  done
  printf '  %-13s clean %d / %d' "$arm" "$pass" "$RUNS"
  [ -n "$sig" ] && printf '   first failure: %s' "$sig"
  printf '\n'
  [ "$fail" -gt 0 ] && RC=1
done

echo
if [ "$RC" -eq 0 ]; then
  echo "PASS: every arm survived."
else
  echo "FAIL: an arm did not — the defect is reproduced."
fi
exit $RC
