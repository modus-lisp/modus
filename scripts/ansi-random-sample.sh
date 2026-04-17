#!/bin/bash
# ansi-random-sample.sh — run N random ANSI tests each in ISOLATION and classify.
#
# Each test runs as `./modus-ansi-test ID ID+1` — the binary only forks for that
# single ID. Classification per sample:
#   PASS    — stdout contains exactly `^P:ID$`, exit 0
#   FAIL    — stdout contains exactly `^FAIL ID$` (child's handler-case caught),
#             exit 0 or 139 (parent SEGV after children finish)
#   CRASH   — no P:ID / FAIL ID line at all (child forked but died silently,
#             OR parent SEGV'd before running that test)
#   TIMEOUT — per-sample timeout fired
#   NOFORK  — `# nofork` marker absent (binary didn't even reach run-real-ansi-tests)
#
# Usage: N=200 SHARDS=116 scripts/ansi-random-sample.sh > report.txt
#   N        number of random test IDs to sample (default 200)
#   SHARDS   how many samples to run concurrently (default 116)
#   BINARY   path to modus-ansi-test (default /tmp/modus-ansi-test)
#   SEED     RNG seed for reproducibility (default: date-derived)
#   TIMEOUT  per-test wall timeout in seconds (default 10)

set -u

N=${N:-200}
SHARDS=${SHARDS:-116}
BINARY=${BINARY:-/tmp/modus-ansi-test}
SEED=${SEED:-$(date +%s)}
TIMEOUT=${TIMEOUT:-10}

if [ ! -x "$BINARY" ]; then
  echo "no binary at $BINARY" >&2
  exit 2
fi

OUTDIR=$(mktemp -d -t ansi-sample.XXXX)
echo "# sampling $N tests, seed=$SEED, parallel=$SHARDS"
echo "# output dir: $OUTDIR"

# Emit N distinct random IDs in [10001, 27708].
awk -v n="$N" -v seed="$SEED" '
BEGIN {
  srand(seed)
  lo = 10001; hi = 27708
  count = 0
  while (count < n) {
    id = lo + int(rand() * (hi - lo + 1))
    if (!(id in seen)) {
      seen[id] = 1
      print id
      count++
    }
  }
}' > "$OUTDIR/ids.txt"

# Run one test in isolation: `binary ID ID+1`, classify, print one line.
run_one() {
  local id=$1
  local out="$OUTDIR/t-$id.out"
  timeout "$TIMEOUT" "$BINARY" "$id" "$((id + 1))" > "$out" 2>&1
  local status=$?

  # Normalize: extract the verdict line we expect.
  # FAIL may appear as "FAIL <id>" or "FAIL <id> GOT:... EXP:..." (rt-run-test
  # prints GOT/EXP for the first few mismatches).
  if grep -qE "^P:$id\$" "$out"; then
    echo "$id PASS"
  elif grep -qE "^FAIL $id(\$| )" "$out"; then
    echo "$id FAIL"
  elif [ "$status" -eq 124 ]; then
    echo "$id TIMEOUT"
  else
    echo "$id CRASH exit=$status"
  fi
}
export -f run_one
export OUTDIR TIMEOUT BINARY

# Run up to SHARDS in parallel. xargs -P handles the throttling.
start_time=$(date +%s)
xargs -a "$OUTDIR/ids.txt" -I{} -P "$SHARDS" bash -c 'run_one "$@"' _ {} > "$OUTDIR/verdicts.txt"
elapsed=$(( $(date +%s) - start_time ))

echo "# all samples done in ${elapsed}s"
echo ""

# Summarize.
awk '
  { verdicts[$2]++ }
  END {
    total = 0
    for (v in verdicts) total += verdicts[v]
    printf "Random-sample ANSI summary\n"
    printf "  sample size: %d\n", total
    for (v in verdicts) printf "  %-8s %6d  (%.2f%%)\n", v, verdicts[v], 100.0 * verdicts[v] / total
    pass = verdicts["PASS"] + 0
    if (total > 0) {
      # Wilson 95% CI for proportion
      p = pass / total
      z = 1.96
      denom = 1 + z*z / total
      center = (p + z*z / (2 * total)) / denom
      margin = z * sqrt(p * (1 - p) / total + z*z / (4 * total * total)) / denom
      lo = center - margin; hi = center + margin
      if (lo < 0) lo = 0; if (hi > 1) hi = 1
      printf "\nEstimated pass rate: %.2f%% (95%% CI %.2f%%..%.2f%%)\n", 100.0*p, 100.0*lo, 100.0*hi
    }
  }
' "$OUTDIR/verdicts.txt"

echo ""
echo "# First 10 crashes (for targeted debugging):"
grep "CRASH" "$OUTDIR/verdicts.txt" | head -10
echo ""
echo "# Full verdict list: $OUTDIR/verdicts.txt"
