#!/bin/bash
# arch-ladder-gate.sh — does every architecture still RUN COMPILED LISP?
#
# Usage:
#   scripts/arch-ladder-gate.sh                 # every arch, every rung
#   scripts/arch-ladder-gate.sh riscv64 68k     # only these architectures
#   ARCH_LADDER_JOBS=8 scripts/arch-ladder-gate.sh
#   ARCH_LADDER_CELL_TIMEOUT=900 scripts/arch-ladder-gate.sh   # per-cell cap
#
# PER-CELL TIMEOUT.  Each cell is capped (default 1800s) because the harness's
# own build timeout is an hour: without this, one wedged build stalls a whole
# 16-wide batch and the run looks stuck rather than failed.  A cell that
# produces no output is reported TIMEOUT, not left blank — a blank line would be
# counted as neither pass nor skip and would quietly shrink the denominator.
#
# Each rung boots a real image in QEMU and reads its answer back out of guest
# memory over QMP (see scripts/arch-ladder.py for why memory and not serial).
# An architecture passes only when every rung returns the RIGHT VALUE.
#
# WHY THIS EXISTS.  For months this repo claimed "all 9 architectures compile
# and produce correct output (factorial 3628800) in QEMU" while five of them
# emitted a binary that nothing ever observed — none of those images even had a
# serial output path to print with.  The bugs that had accumulated behind that
# non-check were not subtle: a translator that emitted a NOP for every opcode
# it did not implement, a GC check that trapped whenever memory WAS available,
# a register that was both scratch and allocatable, an encoder that shifted
# away displacement bits it could not hold.  Every one was invisible because
# nothing asked a question it could fail.
#
# THE GATE PROVES IT CAN FAIL.  Before running the ladder it runs one rung with
# a deliberately wrong expected value and requires that to FAIL.  A gate that
# has never been seen to fail is not evidence; if the positive control passes,
# this exits non-zero without running anything else.

set -u
cd "$(dirname "$0")/.."

JOBS=${ARCH_LADDER_JOBS:-8}
ALL_ARCHES="i386 x64 aarch64 arm32 riscv64 ppc64 ppc32 68k"
ARCHES="${*:-$ALL_ARCHES}"
RUNGS_DIR=test/arch-rungs
OUT=$(mktemp -d)/results
: > "$OUT"

# rung -> expected value.  Everything answers 42 except the factorial.
expected_for() {
  case "$(basename "$1")" in
    r07-factorial.lisp) echo 3628800 ;;
    *)                  echo 42 ;;
  esac
}

# ---- positive control: the gate must be able to fail -----------------------
CONTROL_ARCH=$(echo "$ARCHES" | tr ' ' '\n' | head -1)
echo "positive control: $CONTROL_ARCH r01-call expecting the WRONG answer (43)"
if scripts/arch-ladder.py "$CONTROL_ARCH" "$RUNGS_DIR/r01-call.lisp" --expect=43 >/dev/null 2>&1; then
  echo "GATE BROKEN: the positive control PASSED with a wrong expected value."
  echo "Nothing below would mean anything; refusing to run the ladder."
  exit 2
fi
echo "positive control failed as required — the gate can fail"
echo

# ---- the ladder -----------------------------------------------------------
for a in $ARCHES; do
  for r in "$RUNGS_DIR"/*.lisp; do
    echo "$a $r $(expected_for "$r")"
  done
done | xargs -P "$JOBS" -n 3 bash -c '
  line=$(timeout "${ARCH_LADDER_CELL_TIMEOUT:-1800}" \
           scripts/arch-ladder.py "$0" "$1" --expect="$2" 2>&1 | tail -1)
  # A cell that produced nothing timed out: say so rather than writing a blank
  # line that the verdict would then count as neither pass nor skip.
  [ -z "$line" ] && line="$(printf "%-8s TIMEOUT   no result within %ss" "$0" "${ARCH_LADDER_CELL_TIMEOUT:-1800}")"
  printf "%-22s %s\n" "$(basename "$1" .lisp)" "$line" >> '"$OUT"'
'

sort "$OUT"
echo
total=$(wc -l < "$OUT")
pass=$(grep -c ' PASS ' "$OUT")
skip=$(grep -c ' SKIP ' "$OUT")
fail=$((total - pass - skip))

echo "ladder: $pass passed, $fail failed, $skip skipped (of $total)"
if [ "$fail" -ne 0 ]; then
  echo "FAIL"
  exit 1
fi
if [ "$skip" -ne 0 ]; then
  echo "PASS (with $skip skipped — install the missing qemu-system-* to cover them)"
else
  echo "PASS"
fi
