#!/bin/bash
# run-intern-aot.sh — AOT VERSUS RUNTIME-COMPILED, ON THE SAME LOOP.
#
#   test/run-intern-aot.sh [MODUS-BINARY] [RUNS] [K] [TIMEOUT]
#
# The `low' arm of test/hosted-intern-layers.lisp — a worker interning fresh
# symbols through %INTERN-SYMBOL-PKG — dies about half the time while its
# cross-region audit reads ZERO, and test/hosted-thread-lisp.lisp interns fresh
# symbols through the SAME function on two threads and scores 100 of 100.  The
# last difference standing between them is that the green one's loop is
# AOT-compiled inside the image.  This runs both, in one binary, on one loop.
#
# SIX CELLS: {aot, rt} x {str, low, cl}, each in its OWN PROCESS, RUNS times.
#
#   str   the allocation control — must be clean in both modes
#   low   the subject
#   cl    the OTHER defect (D1), here as the audit's positive control: it must
#         report a non-zero `region 0 -> worker', or a zero from `low' means
#         the instrument is looking at nothing
#
# EVERY RUN IS CLASSIFIED THREE WAYS, not two.  survived / died / HUNG are
# distinct outcomes: a hang is a lost wakeup or a held lock, not a flavour of
# fault, and counting it as a death would hide it.  A normal run takes
# seconds; TIMEOUT defaults to 45.
#
# THIS PRINTS A TABLE AND EXITS 0 IF IT COULD MEASURE.  It is an instrument,
# not a gate — `low' dying is the finding, not a harness failure.  It exits
# non-zero only if it could not measure at all (no binary, or a binary with no
# %IP-WORKER, which every `aot' cell reports as SKIP).
#
# Nothing here opens a socket, binds a port or leaves anything listening.
set -u
BIN=${1:-./modus}
RUNS=${2:-10}
K=${3:-20}
TMO=${4:-45}
cd "$(dirname "$0")/.." || exit 1
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }

TEST=test/hosted-intern-aot.lisp
RC=0
SKIPPED=0

printf '  %-4s %-4s  %8s %6s %6s %6s   %s\n' \
       mode arm survived died hung skip 'first death / audit'
for mode in aot rt; do
  for arm in str low cl; do
    surv=0; died=0; hung=0; skip=0; note=""
    for _ in $(seq 1 "$RUNS"); do
      out=$(MODE="$mode" ARM="$arm" K="$K" \
            timeout -k 5 "$TMO" "$BIN" --script "$TEST" 2>&1)
      rc=$?
      case "$rc" in
        124|137) hung=$((hung + 1)) ;;
        0)       surv=$((surv + 1))
                 [ -z "$note" ] && note=$(printf '%s' "$out" \
                    | grep -m1 -oE 'region 0 -> worker +[0-9]+ -> [0-9]+ +\(grew by [0-9-]+\)') ;;
        2)       skip=$((skip + 1)); SKIPPED=1; note="no %IP-WORKER in this binary" ;;
        *)       died=$((died + 1))
                 [ -z "$note" ] && note=$(printf '%s' "$out" \
                    | grep -m1 -oE 'MVM LONGJMP[^,]*|TYPE-ERROR|SIMPLE-ERROR|FAIL [^:]*') ;;
      esac
    done
    printf '  %-4s %-4s  %8d %6d %6d %6d   %s\n' \
           "$mode" "$arm" "$surv" "$died" "$hung" "$skip" "$note"
  done
done

echo
if [ "$SKIPPED" -eq 1 ]; then
  echo "COULD NOT MEASURE: rebuild with net/hosted-intern-probe.lisp baked in."
  RC=1
else
  echo "Read the RATES.  A difference between the two MODE rows on the same"
  echo "arm is the compilation mode; no difference sends the search back to"
  echo "the harness shape."
fi
exit $RC
