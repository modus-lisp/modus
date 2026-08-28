#!/bin/bash
# run-region0-frontier.sh — DOES THE MAIN THREAD ALLOCATE OVER A WORKER'S
# OBJECTS IN REGION 0?
#
#   test/run-region0-frontier.sh [MODUS-BINARY] [RUNS] [TIMEOUT]
#
# test/hosted-region0-frontier.lisp is a MEASUREMENT, not a stress: one worker,
# one intern, two marked conses.  The two conses are the whole point — the same
# worker allocates them in the same function microseconds apart, and they differ
# in exactly one thing:
#
#   hot   allocated UNDER the runtime lock, so it lives in REGION 0, which is
#         also the MAIN thread's heap and which main bump-allocates from its own
#         registers, outside the lock
#   own   allocated outside the lock, in the worker's own region, which nothing
#         else touches
#
# Both hold 123456789 . 987654321 when the worker makes them.  The driver reads
# both back after the join.  `own' coming back intact while `hot' does not is
# the finding, and the control is inside the same run: it is not the worker,
# not the thread and not the timing, because both conses share all three.
#
# THE CORRUPTION IS INTERMITTENT — a single cons is a narrow window, where the
# intern LOOP that dies half the time makes many.  So this reports a RATE, and
# a run with `hot' intact means "not caught in this shape", never "sound".
#
# THIS IS AN INSTRUMENT, NOT A GATE: it exits 0 whenever it could measure.
# Nothing here opens a socket, binds a port or leaves anything listening.
set -u
BIN=${1:-./modus}
RUNS=${2:-20}
TMO=${3:-45}
cd "$(dirname "$0")/.." || exit 1
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }

TEST=test/hosted-region0-frontier.lisp
hotbad=0; ownbad=0; both=0; ran=0; died=0; hung=0; inside=0; example=""
for _ in $(seq 1 "$RUNS"); do
  out=$(timeout -k 5 "$TMO" "$BIN" --script "$TEST" 2>/dev/null)
  rc=$?
  case "$rc" in
    124|137) hung=$((hung + 1)); continue ;;
  esac
  # A run that never printed the two conses died before it could report.
  if ! printf '%s' "$out" | grep -q '^  hot @'; then
    died=$((died + 1)); continue
  fi
  ran=$((ran + 1))
  h=$(printf '%s' "$out" | grep -m1 '^  hot @')
  o=$(printf '%s' "$out" | grep -m1 '^  own @')
  hb=0; ob=0
  printf '%s' "$h" | grep -q 'car 123456789   cdr 987654321' || hb=1
  printf '%s' "$o" | grep -q 'car 123456789   cdr 987654321' || ob=1
  [ "$hb" -eq 1 ] && { hotbad=$((hotbad + 1)); [ -z "$example" ] && example="$h"; }
  [ "$ob" -eq 1 ] && ownbad=$((ownbad + 1))
  [ "$hb" -eq 1 ] && [ "$ob" -eq 1 ] && both=$((both + 1))
  printf '%s' "$out" | grep -q 'the symbol is INSIDE' && inside=$((inside + 1))
done

echo "  runs that reported            $ran of $RUNS   (died $died, hung $hung)"
echo "  the worker's REGION 0 cons came back CHANGED   $hotbad of $ran"
echo "  the CONTROL — its OWN-region cons changed      $ownbad of $ran"
echo "  both changed (would mean the worker, not the region)   $both of $ran"
echo "  the worker's symbol landed inside main's span  $inside of $ran"
[ -n "$example" ] && echo "  first corrupted region-0 cons: $example"
echo
if [ "$hotbad" -gt 0 ] && [ "$ownbad" -eq 0 ]; then
  echo "REPRODUCED: main allocates over objects a worker made in region 0,"
  echo "and the same worker's own-region objects are untouched."
else
  echo "NOT REPRODUCED IN THIS SHAPE.  That is not the same as sound —"
  echo "read the rate, and note the window is one cons wide."
fi
exit 0
