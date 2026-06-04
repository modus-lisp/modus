#!/bin/bash
# run-ansi-per-file.sh — run every ANSI test file in its OWN /tmp/modus
# process.  Each process loads aux + one test file + runs.  Files run
# in parallel up to $JOBS at a time (default 4).
#
# Rationale: the single-process per-subdir sweep accumulates ~2000-3000
# test registrations and their associated state in one image.  Some
# test failures cascade — a test that signals unwinds through state
# that the next test's evaluation can't tolerate.  Each file having
# its own process makes runs deterministic and parallel-safe.
#
# Aux files reload per process, adding ~25s overhead × N files = a lot,
# but parallelism amortises and the result is much more honest.
#
# Usage:
#   scripts/run-ansi-per-file.sh [JOBS]
# Output: $OUT_DIR/$subdir/$file.out per file, plus per-subdir and
# overall summary tables.

set -u
JOBS="${1:-4}"
HERE="$(cd "$(dirname "$0")" && pwd)"
MODUS_BIN=/tmp/modus
TESTS_ROOT=/home/claude/modus-ref/ansi-test/tests
RUNNER=$HERE/build-ansi-file-runner.sh
OUT_DIR=${OUT_DIR:-/tmp/ansi-per-file}
mkdir -p "$OUT_DIR"

# Collect all test files (non-aux, non-load)
FILES=()
for sd in $(ls "$TESTS_ROOT" | sort); do
  for f in "$TESTS_ROOT/$sd"/*.lsp; do
    bn=$(basename "$f")
    if [ "$bn" != "load.lsp" ] && [[ "$bn" != *-aux.lsp ]]; then
      FILES+=("$sd/$bn")
    fi
  done
done

echo "Total test files: ${#FILES[@]}"
echo "Parallel jobs: $JOBS"

run_one () {
  local rel="$1"
  local sd="${rel%/*}"
  local bn="${rel##*/}"
  local out_sub="$OUT_DIR/$sd"
  mkdir -p "$out_sub"
  local runner="$out_sub/${bn%.lsp}.lisp"
  local out="$out_sub/${bn%.lsp}.out"
  bash "$RUNNER" "$runner" "$TESTS_ROOT/$rel" > /dev/null 2>&1
  timeout 120 "$MODUS_BIN" "$runner" > "$out" 2>&1 || true
}

export -f run_one
export OUT_DIR MODUS_BIN TESTS_ROOT RUNNER

# Use xargs for parallel execution
printf "%s\n" "${FILES[@]}" | xargs -n 1 -P "$JOBS" -I {} bash -c 'run_one "$@"' _ {}

# Aggregate per-subdir
printf "%-25s %8s %8s %8s\n" "subdir" "PASS" "FAIL" "CRASH"
printf "%-25s %8s %8s %8s\n" "------" "----" "----" "-----"
total_p=0; total_f=0; total_c=0
for sd in $(ls "$OUT_DIR"); do
  p=0; f=0; c=0
  for out in "$OUT_DIR/$sd"/*.out; do
    [ -f "$out" ] || continue
    p=$((p + $(grep -c "^P:" "$out") ))
    f=$((f + $(grep -c "^FAIL" "$out") ))
    c=$((c + $(grep -c "^CRASH" "$out") ))
  done
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
