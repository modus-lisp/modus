#!/bin/bash
# run-worker-xregion.sh — THE ROOT CAUSE, AUDITED RATHER THAN ARGUED.
#
#   test/run-worker-xregion.sh [MODUS-BINARY] [K]
#
# Runs both arms of test/hosted-worker-xregion.lisp:
#
#   strings   the worker allocates and KEEPS K strings          — must audit 0
#   intern    the worker INTERNS K fresh symbols                — audits 516 here
#
# The arms are the same loop of the same length over the same strings; the only
# difference is whether the result is put in the shared table.  So the control
# arm passing and the subject arm failing localises the violation to interning
# and to nothing else.
#
# EXPECTED ON THIS TREE: `strings' passes and `intern' does not, so this exits
# NON-ZERO.  It is a reproducer of a soundness violation, and it becomes a
# PASSING regression test the moment a worker's intern allocates in region 0.
#
# BOTH ARMS PASSING IS ALSO A FAILURE MODE worth knowing about: if the subject
# ever reports 0 while the crash in test/run-worker-intern.sh persists, the
# audit is not looking where it thinks it is.
set -u
BIN=${1:-./modus}
K=${2:-1500}
cd "$(dirname "$0")/.." || exit 1
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }

RC=0
for arm in strings intern; do
  echo "=== ARM $arm ==="
  XREGION_ARM="$arm" XREGION_K="$K" "$BIN" --script test/hosted-worker-xregion.lisp 2>/dev/null \
    | grep -E '^(ok|FAIL|===|last object|its name|FOREIGN|worker region|[0-9]+ checks|WORKER CROSS)'
  # shellcheck disable=SC2181
  [ "${PIPESTATUS[0]}" -eq 0 ] || RC=1
  echo
done

if [ "$RC" -eq 0 ]; then
  echo "PASS: no region 0 -> worker-region pointers in either arm."
else
  echo "FAIL: the cross-region invariant is violated — see the FOREIGN REFS line."
fi
exit $RC
