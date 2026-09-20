#!/bin/bash
# run-intern-shape.sh — IF IT IS NOT THE COMPILATION MODE, WHAT IS IT?
#
#   test/run-intern-shape.sh [MODUS-BINARY] [RUNS] [K] [TIMEOUT]
#
# Seven arms of test/hosted-intern-shape.lisp, EACH IN ITS OWN PROCESS, RUNS
# times each.  Every arm is the same worker running the same K
# %INTERN-SYMBOL-PKG calls; they differ by exactly one named thing, in a chain,
# so a rate that moves between two adjacent arms names that thing:
#
#   bare    the loop and nothing else — the floor
#   count   bare + the shared table's COUNT before and after
#   audit   count + the O(heap) %GC-COUNT-FOREIGN-REFS sweep (today's `low')
#   list    bare, returning a list consed in the worker's own region
#   outer   bare, with ONE %RT-ENTER/%RT-LEAVE around the whole loop
#   main    the same loop on the MAIN thread — the "is it a worker?" control
#   str     K strings on the worker — the allocation control
#
# EVERY RUN IS CLASSIFIED THREE WAYS: survived / died / HUNG.  A hang is a lost
# wakeup or a held lock, not a flavour of fault, and folding it into "died"
# would hide it.  A normal run takes seconds; TIMEOUT defaults to 45.
#
# THIS IS AN INSTRUMENT, NOT A GATE.  It prints a table and exits 0 whenever it
# could measure — an arm dying is the finding.  It exits non-zero only if there
# is no binary to run.
#
# Nothing here opens a socket, binds a port or leaves anything listening.
set -u
BIN=${1:-./modus}
RUNS=${2:-10}
K=${3:-20}
TMO=${4:-45}
cd "$(dirname "$0")/.." || exit 1
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }

TEST=test/hosted-intern-shape.lisp

printf '  %-6s  %8s %6s %6s   %s\n' arm survived died hung 'first death'
for arm in bare count audit list outer main str; do
  surv=0; died=0; hung=0; note=""
  for _ in $(seq 1 "$RUNS"); do
    out=$(ARM="$arm" K="$K" timeout -k 5 "$TMO" "$BIN" --script "$TEST" 2>&1)
    rc=$?
    case "$rc" in
      124|137) hung=$((hung + 1)) ;;
      0)       surv=$((surv + 1)) ;;
      *)       died=$((died + 1))
               [ -z "$note" ] && note=$(printf '%s' "$out" \
                  | grep -m1 -oE 'MVM LONGJMP[^,]*|TYPE-ERROR|SIMPLE-ERROR|FAIL [^:]*') ;;
    esac
  done
  printf '  %-6s  %8d %6d %6d   %s\n' "$arm" "$surv" "$died" "$hung" "$note"
done

echo
echo "Read the RATES.  bare -> count -> audit adds one measurement at a time;"
echo "bare -> list adds the returned list; bare -> outer moves the lock;"
echo "bare -> main removes the worker."
exit 0
