#!/bin/bash
# run-atomics.sh — %ATOMIC-INCF / %ATOMIC-DECF under four real threads, RATED.
#
#   test/run-atomics.sh [./modus] [runs]
#
# A single run is a sample, not a property: the negative control is a RACE, and
# a race that happens not to interleave on one run comes back clean.  So both
# arms are run N times and reported as `N of M', and the control is graded on
# the RATE at which it loses updates rather than on any one result.
#
# TWO ARMS, ONE BINARY, ONE BSS WORD APART:
#   sync    — the shipping configuration.  Every arm must be EXACT.
#   unsync  — MODUS_ATOMICS_MODE=unsync clears the threads-live gate, so the
#             same macro takes the pre-SMP branch.  It must LOSE updates; a run
#             in which it came out exact would mean the sync arm proved nothing.
#
# Nothing here opens a socket, and nothing is left running.

set -u
MODUS="${1:-./modus}"
RUNS="${2:-10}"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

if [ ! -x "$MODUS" ]; then
  echo "run-atomics.sh: no such binary: $MODUS" >&2
  exit 2
fi

sync_pass=0; sync_fail=0; sync_dead=0
uns_pass=0;  uns_fail=0;  uns_dead=0
uns_lost_min=""; uns_lost_max=""

echo
echo "=== ARM: sync (the shipping configuration) — $RUNS runs ================"
for i in $(seq 1 "$RUNS"); do
  out="$TMP/sync.$i"
  timeout 300 "$MODUS" --script test/hosted-atomics.lisp >"$out" 2>/dev/null
  rc=$?
  if [ "$rc" -ne 0 ]; then
    sync_dead=$((sync_dead+1))
    echo "  run $i: rc=$rc (process did not exit cleanly)"
  elif grep -q "ATOMICS UNDER FOUR THREADS: PASS" "$out"; then
    sync_pass=$((sync_pass+1))
    lost=$(grep -m1 "LOST UPDATES" "$out" | tr -dc '0-9')
    echo "  run $i: PASS   (in-process control lost ${lost:-?} of 80000)"
  else
    sync_fail=$((sync_fail+1))
    echo "  run $i: FAIL"
    grep "  FAIL" "$out" | head -5
  fi
done

echo
echo "=== ARM: unsync (THE NEGATIVE CONTROL) — $RUNS runs ===================="
for i in $(seq 1 "$RUNS"); do
  out="$TMP/unsync.$i"
  MODUS_ATOMICS_MODE=unsync timeout 300 "$MODUS" --script test/hosted-atomics.lisp \
    >"$out" 2>/dev/null
  rc=$?
  lost=$(grep -m1 "LOST UPDATES" "$out" | tr -dc '0-9')
  if [ "$rc" -ne 0 ]; then
    uns_dead=$((uns_dead+1))
    echo "  run $i: rc=$rc (process did not exit cleanly)"
  elif grep -q "ATOMICS UNDER FOUR THREADS: PASS" "$out"; then
    # PASS here means "it lost updates as required of a control".
    uns_pass=$((uns_pass+1))
    echo "  run $i: lost ${lost:-?} of 80000 through the UNPROTECTED path"
    if [ -n "${lost:-}" ]; then
      [ -z "$uns_lost_min" ] && uns_lost_min="$lost"
      [ -z "$uns_lost_max" ] && uns_lost_max="$lost"
      [ "$lost" -lt "$uns_lost_min" ] && uns_lost_min="$lost"
      [ "$lost" -gt "$uns_lost_max" ] && uns_lost_max="$lost"
    fi
  else
    uns_fail=$((uns_fail+1))
    echo "  run $i: THE CONTROL DID NOT LOSE AN UPDATE — the test proves nothing"
    grep "  FAIL" "$out" | head -5
  fi
done

echo
echo "=== RATES ============================================================="
echo "  sync   PASS $sync_pass of $RUNS   (fail $sync_fail, unclean exit $sync_dead)"
echo "  unsync lost-updates-as-required $uns_pass of $RUNS   (came out exact $uns_fail, unclean exit $uns_dead)"
[ -n "$uns_lost_min" ] && echo "  unsync lost between $uns_lost_min and $uns_lost_max of 80000"
echo

if [ "$sync_pass" -eq "$RUNS" ] && [ "$uns_pass" -eq "$RUNS" ]; then
  echo "ATOMICS: PASS ($sync_pass of $RUNS protected exact; control lost updates $uns_pass of $RUNS)"
  exit 0
fi
echo "ATOMICS: FAIL"
exit 1
