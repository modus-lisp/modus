#!/bin/bash
# run-ansi-all.sh — generate + run one ANSI test runner per subdir,
# each in its own /tmp/modus process so a crash in one subdir doesn't
# clobber the others' counts.  Prints a per-subdir table and a total.
#
# Usage: scripts/run-ansi-all.sh

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
MODUS_BIN=/tmp/modus
TESTS_ROOT=/home/claude/modus-ref/ansi-test/tests
RUNNER=$HERE/build-ansi-runner.sh
OUT_DIR=${OUT_DIR:-/tmp/ansi-runs}
mkdir -p "$OUT_DIR"

SUBDIRS=$(ls "$TESTS_ROOT" | sort)
printf "%-25s %8s %8s %8s\n" "subdir" "PASS" "FAIL" "CRASH"
printf "%-25s %8s %8s %8s\n" "------" "----" "----" "-----"

total_p=0; total_f=0; total_c=0
for sd in $SUBDIRS; do
  runner="$OUT_DIR/run-$sd.lisp"
  out="$OUT_DIR/run-$sd.out"
  bash "$RUNNER" "$runner" "$sd" > /dev/null 2>&1
  timeout 240 "$MODUS_BIN" "$runner" > "$out" 2>&1 || true
  read p f c <<<$(awk '/^P:/{p++} /^FAIL/{f++} /^CRASH/{c++} END {print p+0, f+0, c+0}' "$out")
  printf "%-25s %8d %8d %8d\n" "$sd" "$p" "$f" "$c"
  total_p=$((total_p + p))
  total_f=$((total_f + f))
  total_c=$((total_c + c))
done

printf "%-25s %8s %8s %8s\n" "------" "----" "----" "-----"
printf "%-25s %8d %8d %8d\n" "TOTAL" "$total_p" "$total_f" "$total_c"
echo
echo "Tests reached: $((total_p + total_f + total_c))"
echo "PASS rate of reached: $(awk -v p=$total_p -v t=$((total_p+total_f+total_c)) 'BEGIN{printf "%.1f%%\n",100*p/t}')"
