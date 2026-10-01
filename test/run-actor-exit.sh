#!/bin/bash
# run-actor-exit.sh — EXITING WITH AN ACTOR ALIVE MUST END THE PROCESS.
#
#   test/run-actor-exit.sh [BINARY]
#
# Same shape as run-thread-exit.sh, for a deliberate exit rather than a fault:
# the failure is that the process never finishes, so only something outside it,
# holding a clock, can see it.  PASS iff the run ends within the timeout with
# the status the program asked for (7).
set -u
BIN=${1:-./modus}
cd "$(dirname "$0")/.." || exit 1
if [ ! -x "$BIN" ]; then echo "no such binary: $BIN" >&2; exit 1; fi
timeout 60 "$BIN" --script test/actor-exit.lisp > /dev/null 2>&1
rc=$?
if [ "$rc" -eq 7 ]; then echo "ACTOR EXIT: PASS"; exit 0; fi
if [ "$rc" -eq 124 ]; then echo "ACTOR EXIT: FAIL (hung: exit ended only the main thread)"; else echo "ACTOR EXIT: FAIL (status $rc, wanted 7)"; fi
exit 1
