#!/bin/bash
# fragility-fuzzer.sh — build the ANSI-test binary with several values of
# *fuzz-funcall-nops* and report which tests flip across builds.
#
# Each :nop is 1 byte in MVM bytecode; the x64 translator emits a single
# 0x90 NOP per :nop, so N maps directly to bytes added per funcall site.
# At thousands of funcalls in the binary, N=1 → ~thousands of bytes
# shift; N=4 → 4× that; etc.
#
# A test that passes with some N but fails with others is layout-fragile
# — its result depends on something that shifts with bytecode size.
# The PATTERN of flipping vs N tells us what mechanism is sensitive
# (alignment-period, branch displacement, GC scan finding a specific
# value on the stack, etc.).

set -eu

cd "$(dirname "$0")/.."

FUZZ_VALUES="${1:-0 1 2 3 4 8}"
RESULTS_DIR=/tmp/fragility-fuzzer
mkdir -p "$RESULTS_DIR"

for n in $FUZZ_VALUES; do
  echo "=== Building with MODUS_FUZZ_FUNCALL_NOPS = $n ==="
  rm -f /tmp/modus-ansi-test
  MODUS_FUZZ_FUNCALL_NOPS="$n" \
    sbcl --dynamic-space-size 2048 \
         --script mvm/build-ansi-test.lisp 2>&1 | tail -2
  if [ ! -f /tmp/modus-ansi-test ]; then
    echo "  BUILD FAILED for n=$n; aborting"
    exit 1
  fi
  cp /tmp/modus-ansi-test "$RESULTS_DIR/binary-n$n"
  /tmp/modus-ansi-test > "$RESULTS_DIR/run-n$n.log" 2>&1
  passed=$(grep -c "^P:" "$RESULTS_DIR/run-n$n.log" || true)
  failed=$(grep -c "^FAIL " "$RESULTS_DIR/run-n$n.log" || true)
  echo "  N=$n  passed=$passed  failed=$failed"
done

echo
echo "=== Flip analysis (tests that change pass/fail across N) ==="
for n in $FUZZ_VALUES; do
  awk '/^P:/ {print substr($0,3)+0}' "$RESULTS_DIR/run-n$n.log" \
    | LC_ALL=C sort -n -u > "$RESULTS_DIR/passed-n$n.txt"
done

# Tests that pass for some N but fail for others.
first=$(echo $FUZZ_VALUES | cut -d' ' -f1)
cp "$RESULTS_DIR/passed-n$first.txt" "$RESULTS_DIR/union.txt"
cp "$RESULTS_DIR/passed-n$first.txt" "$RESULTS_DIR/inter.txt"
for n in $FUZZ_VALUES; do
  if [ "$n" != "$first" ]; then
    LC_ALL=C sort -n -u "$RESULTS_DIR/union.txt" "$RESULTS_DIR/passed-n$n.txt" \
      > "$RESULTS_DIR/union.tmp"
    mv "$RESULTS_DIR/union.tmp" "$RESULTS_DIR/union.txt"
    LC_ALL=C comm -12 "$RESULTS_DIR/inter.txt" "$RESULTS_DIR/passed-n$n.txt" \
      > "$RESULTS_DIR/inter.tmp"
    mv "$RESULTS_DIR/inter.tmp" "$RESULTS_DIR/inter.txt"
  fi
done

LC_ALL=C comm -23 "$RESULTS_DIR/union.txt" "$RESULTS_DIR/inter.txt" \
  > "$RESULTS_DIR/flippy.txt"

echo "Stable across all N: $(wc -l < "$RESULTS_DIR/inter.txt")"
echo "Flippy (P for some N, F for others): $(wc -l < "$RESULTS_DIR/flippy.txt")"
echo
echo "Per-test flip pattern (first 30):"
echo -n "  test  "
for n in $FUZZ_VALUES; do printf " %3s" "n$n"; done
echo
head -30 "$RESULTS_DIR/flippy.txt" | while read tid; do
  printf "  %5d " "$tid"
  for n in $FUZZ_VALUES; do
    if grep -q "^$tid\$" "$RESULTS_DIR/passed-n$n.txt"; then
      printf " %3s" "P"
    else
      printf " %3s" "."
    fi
  done
  echo
done
