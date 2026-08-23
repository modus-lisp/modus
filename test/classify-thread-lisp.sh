#!/bin/bash
# classify-thread-lisp.sh — RUN test/hosted-thread-lisp.lisp N TIMES AND CLASSIFY EVERY RUN.
#
#   test/classify-thread-lisp.sh [BINARY] [RUNS] [TIMEOUT]
#
# run-thread-exit.sh asserts only that nothing hangs.  This one reports the
# three-way split the campaign asks for — pass / clean-death / hang — because
# "the hang is gone" is a claim about a rate, and a rate needs a denominator.
#
# A hang is rc=124 (the timeout's).  A clean death is any other non-zero rc:
# the process ended and the pipe saw EOF, which is the property the teardown
# fix bought.  A pass is rc=0 AND the test's own PASS line.
set -u
BIN=${1:-./modus}
RUNS=${2:-100}
TIMEOUT=${3:-30}
TEST=test/hosted-thread-lisp.lisp

cd "$(dirname "$0")/.." || exit 1
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }

pass=0; death=0; hang=0; failed=0
declare -A rcs
for i in $(seq 1 "$RUNS"); do
  out=$(timeout -k 5 "$TIMEOUT" "$BIN" --script "$TEST" 2>&1)
  rc=$?
  rcs[$rc]=$(( ${rcs[$rc]:-0} + 1 ))
  case $rc in
    0)   if printf '%s' "$out" | grep -q "REAL LISP: PASS"; then pass=$((pass+1))
         else failed=$((failed+1))
              echo "  run $i: rc=0 but NOT a pass: $(printf '%s' "$out" | tail -3 | tr '\n' ' ')"; fi ;;
    124) hang=$((hang+1)); echo "  run $i: HANG" ;;
    *)   death=$((death+1))
         echo "  run $i: clean death rc=$rc: $(printf '%s' "$out" | tail -2 | tr '\n' ' ')" ;;
  esac
done

echo
echo "runs            $RUNS"
echo "pass            $pass"
echo "clean-death     $death"
echo "ran-but-failed  $failed"
echo "HANG            $hang"
echo -n "rc histogram   "
for k in "${!rcs[@]}"; do echo -n " rc=$k:${rcs[$k]}"; done
echo
[ "$hang" -eq 0 ] || exit 1
