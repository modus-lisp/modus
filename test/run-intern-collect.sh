#!/bin/bash
# run-intern-collect.sh — THE SIGSEGV SUBJECT AS A RATE.
#
#   test/run-intern-collect.sh [MODUS-BINARY] [RUNS] [K]
#
# test/hosted-intern-collect.lisp: a worker interns K fresh CL symbols, then
# forces TWO collections of its own region with a live chain, re-resolving
# every symbol EQ after each.  Pre-fix this shape was SIGSEGV 3 of 3 and could
# not be a test at all.  Survived / died / hung classified separately, as
# everywhere in this campaign.
#
# Nothing here opens a socket, binds a port or leaves anything listening.
set -u
BIN=${1:-./modus}
RUNS=${2:-10}
K=${3:-200}
TMO=45
cd "$(dirname "$0")/.." || exit 1
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }

surv=0; died=0; hung=0; sig=""
for _ in $(seq 1 "$RUNS"); do
  out=$(INTC_K="$K" timeout -k 5 "$TMO" "$BIN" --script test/hosted-intern-collect.lisp 2>&1)
  rc=$?
  case "$rc" in
    124|137) hung=$((hung + 1)) ;;
    0) if printf '%s' "$out" | grep -q "INTERN THEN COLLECT: PASS"; then
         surv=$((surv + 1))
       else
         died=$((died + 1))
       fi ;;
    *) died=$((died + 1))
       [ -z "$sig" ] && sig=$(printf '%s' "$out" \
          | grep -m1 -oE 'MVM LONGJMP[^,]*|TYPE-ERROR|SIMPLE-ERROR|FAIL [^:]*') ;;
  esac
done
echo "  survived $surv  died $died  hung $hung   of $RUNS   (K=$K)"
[ -n "$sig" ] && echo "  first death: $sig"
[ "$surv" -eq "$RUNS" ] && { echo "PASS"; exit 0; }
echo "FAIL"; exit 1
