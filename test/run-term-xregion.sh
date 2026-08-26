#!/bin/bash
# run-term-xregion.sh — THE CROSS-REGION MESSAGE AUDIT, ONE CASE PER PROCESS.
#
#   test/run-term-xregion.sh [MODUS-BINARY] [RUNS]
#
# Why per-process: the audit sweeps region 0's live span GARBAGE INCLUDED, and
# a later case counts an earlier case's leftovers — decomposed in the header of
# test/hosted-term-xregion.lisp (the historic single-process "29" was 2 real +
# 27 pollution).  Three cases:
#
#   string     must audit 0 (since the serialiser copy fix)
#   symbol     must audit 0 (since the B-LITE arena + locked CL:INTERN copy)
#   joinshare  EXPECTED NON-ZERO — the residual join-by-pointer channel,
#              demonstrated on purpose; also the audit's in-vivo positive
#              control
#
# Nothing here opens a socket, binds a port or leaves anything listening.
set -u
BIN=${1:-./modus}
RUNS=${2:-3}
TMO=60
cd "$(dirname "$0")/.." || exit 1
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }

RC=0
for c in string symbol joinshare; do
  pass=0; fail=0; note=""
  for _ in $(seq 1 "$RUNS"); do
    out=$(XREGION_CASE="$c" timeout -k 5 "$TMO" "$BIN" --script test/hosted-term-xregion.lisp 2>&1)
    if [ $? -eq 0 ] && printf '%s' "$out" | grep -q "CROSS-REGION MESSAGE AUDIT: PASS"; then
      pass=$((pass + 1))
    else
      fail=$((fail + 1))
      [ -z "$note" ] && note=$(printf '%s' "$out" | grep -m1 -aE "FAIL |LONGJMP|TYPE-ERROR")
    fi
  done
  printf '  %-9s pass %d / %d' "$c" "$pass" "$RUNS"
  [ -n "$note" ] && printf '   first failure: %s' "$note"
  printf '\n'
  [ "$fail" -gt 0 ] && RC=1
done

echo
if [ "$RC" -eq 0 ]; then
  echo "PASS: string and symbol audit 0 in their own processes, and the"
  echo "join-by-pointer channel is still visible where it is expected."
else
  echo "FAIL"
fi
exit $RC
