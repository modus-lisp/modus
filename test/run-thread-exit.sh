#!/bin/bash
# run-thread-exit.sh — A FAULTING THREADED PROGRAM MUST DIE, NOT HANG.
#
#   test/run-thread-exit.sh [BINARY] [RUNS]
#
# WHAT THIS TESTS, and why it is a shell script rather than a Lisp one.
#
# The bug this pins is a TEARDOWN bug: when a thread takes an unhandled
# hardware fault, the signal stub used to end the CALLING THREAD (SYS_exit, 60
# on x86-64 / 93 on aarch64) instead of the PROCESS (exit_group, 231 / 94).  In
# a single-threaded image those two are indistinguishable, which is how it
# survived.  With a second thread alive it is a permanent hang: the kernel
# leaves the group leader an unreapable ZOMBIE because the thread group is not
# empty, the sibling stays parked in futex_wait with nobody left to wake it,
# and a parent reading the process's output through a pipe never sees EOF.
#
# A test written in Lisp CANNOT observe this.  The failure is precisely that
# the process never finishes, so the thing that must do the observing is
# something OUTSIDE the process, holding a clock.  That is this script.
#
# THE ASSERTION IS NOT "the run passed".  test/hosted-thread-lisp.lisp still
# fails a minority of runs on a genuine, separate, UNFIXED defect (a fault
# during a forced collection concurrent with the other thread's Lisp work —
# see CLAUDE.md).  This script deliberately does not care whether a given run
# passed or faulted.  It asserts only that EVERY run TERMINATED: exit status
# 124 (the timeout's) must never appear.  A faulting run scoring 139 is a
# reportable crash and passes here; a run that hangs is the bug.
#
# So this stays honest while the remaining defect is outstanding, and it keeps
# being the right test after that defect is fixed.

set -u
BIN=${1:-./modus}
RUNS=${2:-20}
TEST=test/hosted-thread-lisp.lisp
TIMEOUT=60

cd "$(dirname "$0")/.." || exit 1

if [ ! -x "$BIN" ]; then echo "no such binary: $BIN" >&2; exit 1; fi

echo "=== A FAULTING THREADED PROGRAM MUST DIE, NOT HANG ==="
echo "binary:  $BIN"
echo "runs:    $RUNS   (timeout ${TIMEOUT}s each)"
echo "test:    $TEST"
echo

hung=0; passed=0; crashed=0; other=0
for i in $(seq 1 "$RUNS"); do
  # The pipe is not incidental: reading the output through one is exactly the
  # thing the zombie-leader bug made block forever.
  out=$(timeout -k 5 "$TIMEOUT" "$BIN" --script "$TEST" 2>&1)
  rc=$?
  case $rc in
    0)   passed=$((passed+1)) ;;
    124) hung=$((hung+1));    echo "  run $i: HUNG (rc=124) — the teardown bug" ;;
    139) crashed=$((crashed+1)); echo "  run $i: crashed rc=139 (reportable; not this test's subject)" ;;
    *)   other=$((other+1));  echo "  run $i: rc=$rc" ;;
  esac
  # Silence an unused-variable warning while keeping the pipe read above real.
  : "${out:+read}"
done

echo
echo "passed          $passed"
echo "crashed (139)   $crashed"
echo "other rc        $other"
echo "HUNG (124)      $hung"
echo
if [ "$hung" -eq 0 ]; then
  echo "PASS: every one of $RUNS runs terminated; nothing became an unreapable zombie."
  exit 0
else
  echo "FAIL: $hung of $RUNS runs never terminated."
  exit 1
fi
