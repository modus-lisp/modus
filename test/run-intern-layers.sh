#!/bin/bash
# run-intern-layers.sh — WHICH INTERN LEAVES THE FORBIDDEN POINTER?
#
#   test/run-intern-layers.sh [MODUS-BINARY] [RUNS] [K]
#
# Three arms of test/hosted-intern-layers.lisp, EACH IN ITS OWN PROCESS, RUNS
# times each.  They differ only in what the worker's loop body does:
#
#   str   allocate K strings                  — the allocation control
#   low   K fresh %INTERN-SYMBOL-PKG          — the runtime's low-level intern
#   cl    K fresh CL:INTERN                   — the CL package-system intern
#
# EXPECTED ON THIS TREE, and these are RATES, not verdicts (K=20):
#
#   str   10 of 10 clean
#   low    5 of 10 clean   — dies with MVM LONGJMP (TRAP #x0511); its AUDIT is
#                            zero on every run that completes, so this is a
#                            SECOND defect, not the cross-region one
#   cl     0 of  8 clean   — audit +44 every single run, process never dies
#
# So this EXITS NON-ZERO.  It is a reproducer, and a reproducer that passes has
# stopped reproducing.  Both arms have to go clean before this exits 0, and they
# are different problems: see THERE ARE TWO DEFECTS HERE in the .lisp file.
#
# RUNS defaults to 3 because everything in this campaign is layout-sensitive and
# a single clean run means "not reproduced in this shape", never "correct".  The
# RATE is the result; read it, not one line.
#
# Nothing here opens a socket, binds a port or leaves anything listening.
set -u
BIN=${1:-./modus}
RUNS=${2:-3}
K=${3:-20}
cd "$(dirname "$0")/.." || exit 1
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }

RC=0
for arm in str low cl; do
  pass=0; fail=0; detail=""
  for _ in $(seq 1 "$RUNS"); do
    out=$(ARM="$arm" K="$K" "$BIN" --script test/hosted-intern-layers.lisp 2>/dev/null)
    rc=$?
    if [ "$rc" -eq 0 ] && printf '%s' "$out" | grep -q "INTERN LAYERS $arm: PASS"; then
      pass=$((pass + 1))
    else
      fail=$((fail + 1))
      [ -z "$detail" ] && detail=$(printf '%s' "$out" | grep -m1 -E 'region 0 -> worker|MVM LONGJMP[^,]*|TYPE-ERROR')
    fi
  done
  printf '  %-4s clean %d / %d' "$arm" "$pass" "$RUNS"
  [ -n "$detail" ] && printf '   first failure: %s' "$detail"
  printf '\n'
  [ "$fail" -gt 0 ] && RC=1
done

echo
if [ "$RC" -eq 0 ]; then
  echo "PASS: every arm survived and left the forbidden direction at zero."
else
  echo "FAIL: an arm did not — the defect is reproduced."
fi
exit $RC
