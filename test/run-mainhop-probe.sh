#!/bin/bash
# run-mainhop-probe.sh — SHAPE A'S GATE, AS RATES.
#
#   test/run-mainhop-probe.sh [MODUS-BINARY] [RUNS] [TIMEOUT]
#
# Runs test/hosted-mainhop-probe.lisp N times and reports, per phase, how many
# runs got there and what each phase answered.  The probe prints and flushes
# each phase before starting the next, so a death localizes to the first
# missing banner.
#
# WHAT THE PHASES MEAN is documented in the .lisp file.  In brief: P1/P2 are
# shape A's viability (allocation routing and lock discipline after main hops
# out of region 0), P3 is the publication audit (region 0 -> main's region
# references, by site), P4 is a forced collection of main's region (the three
# dangle probes), P5a/P5b are natural collection under compiled / evaluated
# allocation — the recorded killer.
#
# THIS IS AN INSTRUMENT, NOT A GATE-KEEPER SCRIPT: it exits 0 whenever it
# could measure.  The probe dying at P5 IS the measurement.
#
# Nothing here opens a socket, binds a port or leaves anything listening.
set -u
BIN=${1:-./modus}
RUNS=${2:-10}
TMO=${3:-120}
cd "$(dirname "$0")/.." || exit 1
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }

TEST=test/hosted-mainhop-probe.lisp
p1=0; p2=0; p3=0; p4=0; p4dangle=0; p4defun=0; p4sym=0
p5a=0; p5asurv=0; p5b=0; p5bsurv=0
surv=0; died=0; hung=0; sig=""
for _ in $(seq 1 "$RUNS"); do
  out=$(timeout -k 5 "$TMO" "$BIN" --script "$TEST" 2>&1)
  rc=$?
  case "$rc" in
    124|137) hung=$((hung + 1)) ;;
    0)       surv=$((surv + 1)) ;;
    *)       died=$((died + 1))
             [ -z "$sig" ] && sig=$(printf '%s' "$out" \
                | grep -m1 -oE 'MVM LONGJMP[^,]*|TYPE-ERROR|SIMPLE-ERROR') ;;
  esac
  printf '%s' "$out" | grep -q '=== P1:' && p1=$((p1 + 1))
  printf '%s' "$out" | grep -q 'ok   P2 ' && p2=$((p2 + 1))
  printf '%s' "$out" | grep -q 'after ONE CL:INTERN' && p3=$((p3 + 1))
  if printf '%s' "$out" | grep -q "ok   P4 main's region really collected"; then
    p4=$((p4 + 1))
    printf '%s' "$out" | grep -q "FAIL P4 the DEFVAR'd list" && p4dangle=$((p4dangle + 1))
    printf '%s' "$out" | grep -q "FAIL P4 the post-hop DEFUN" && p4defun=$((p4defun + 1))
    printf '%s' "$out" | grep -q "FAIL P4 the CL:INTERN'd" && p4sym=$((p4sym + 1))
  fi
  printf '%s' "$out" | grep -q '=== P5a:' && p5a=$((p5a + 1))
  printf '%s' "$out" | grep -q 'ok   P5a the compiled loop ran' && p5asurv=$((p5asurv + 1))
  printf '%s' "$out" | grep -q '=== P5b:' && p5b=$((p5b + 1))
  printf '%s' "$out" | grep -q 'ok   P5b the form ran to its count' && p5bsurv=$((p5bsurv + 1))
done

echo "  runs: $RUNS   survived $surv   died $died   hung $hung"
[ -n "$sig" ] && echo "  first death signature: $sig"
echo
echo "  reached P1 (hop viability)               $p1 of $RUNS"
echo "  P2 clean (no unlocked region-0 alloc)    $p2 of $RUNS"
echo "  completed P3 (publication audit)         $p3 of $RUNS"
echo "  completed P4's forced collection         $p4 of $RUNS"
echo "    DEFVAR'd heap value DANGLED            $p4dangle of $p4"
echo "    post-hop DEFUN broke                   $p4defun of $p4"
echo "    CL:INTERN'd symbol broke               $p4sym of $p4"
echo "  reached P5a (compiled natural pressure)  $p5a of $RUNS"
echo "    ... and survived it                    $p5asurv of $p5a"
echo "  reached P5b (evaluated natural pressure) $p5b of $RUNS"
echo "    ... and survived it                    $p5bsurv of $p5b"
exit 0
