#!/bin/bash
# ansi-shard.sh — shard the ANSI suite across N parallel processes.
#
# Each shard runs `./modus-ansi-test START END` which executes only tests
# with id in [START, END). Outputs are merged and summarized.
#
# Total ANSI test ID range: 10001 .. 27708 (17708 tests).
# Custom test ids 9935, 99999 — leave those to shard 1 (start = 0).

set -u

SHARDS=${SHARDS:-32}
BINARY=${BINARY:-/tmp/modus-ansi-test}
FIRST=10001
LAST=27709   # one past last test id

if [ ! -x "$BINARY" ]; then
  echo "no binary at $BINARY" >&2
  exit 2
fi

TOTAL=$(( LAST - FIRST ))
STEP=$(( (TOTAL + SHARDS - 1) / SHARDS ))

OUTDIR=$(mktemp -d -t ansi-shard.XXXX)
# Keep the shard output dir for post-mortem; delete manually when done.

echo "# sharding $TOTAL tests across $SHARDS shards of ~$STEP each"
echo "# output dir: $OUTDIR"

start_time=$(date +%s)

# Launch all shards in parallel. Shard 0 also runs the pre-fork custom tests
# (it uses start=0 to not skip them).
for i in $(seq 0 $(( SHARDS - 1 ))); do
  lo=$(( FIRST + i * STEP ))
  hi=$(( lo + STEP ))
  if [ $hi -gt $LAST ]; then hi=$LAST; fi
  if [ $i -eq 0 ]; then lo=0; fi     # shard 0 gets pre-fork custom tests too
  "$BINARY" "$lo" "$hi" > "$OUTDIR/shard-$i.out" 2>&1 &
done

# Wait for all.
wait

elapsed=$(( $(date +%s) - start_time ))
echo "# all shards done in ${elapsed}s"

# Aggregate.
awk '
  # Custom/pre-ANSI tests (IDs < 10001) run in every shard — dedupe by ID.
  # ANSI fork-tests (IDs 10001-27708) are sharded exclusively — accumulate.
  # Symbol-named deftest init-forms run in every shard — dedupe by text.
  match($0, /ANSI-TOTAL=[0-9]+/) { tot = substr($0, RSTART+11, RLENGTH-11)+0 }
  /^P:/ {
    key = $0
    if (match(key, /^P:[0-9]+$/)) {
      id = substr(key, 3) + 0
      if (id >= 10001) { ansi_pass[id] = 1 }
      else             { custom_pass[id] = 1 }
    } else {
      sym_pass[key] = 1
    }
    next
  }
  /^FAIL / {
    rest = substr($0, 6)
    if (match(rest, /^[0-9]+$/)) {
      id = rest + 0
      if (id >= 10001) { ansi_fail[id] = 1 }
      else             { custom_fail[id] = 1 }
    } else {
      sym_fail[rest] = 1
    }
    next
  }
  END {
    for (id in ansi_pass)   ansi_p++
    for (id in ansi_fail)   ansi_f++
    for (id in custom_pass) cust_p++
    for (id in custom_fail) cust_f++
    for (k in sym_pass)     sym_p++
    for (k in sym_fail)     sym_f++
    ansi_ran = ansi_p + ansi_f
    lost = tot - ansi_ran
    printf "ANSI sharded summary (deduped)\n"
    printf "  ANSI expected:      %d\n", tot
    printf "  ANSI passed:        %d\n", ansi_p
    printf "  ANSI failed:        %d\n", ansi_f
    printf "  ANSI lost to crash: %d\n", lost
    if (ansi_ran > 0) printf "  ANSI pass (of run): %.2f%%  (%d / %d)\n", 100.0*ansi_p/ansi_ran, ansi_p, ansi_ran
    if (tot > 0)      printf "  ANSI pass (overall):%.2f%%  (%d / %d)\n", 100.0*ansi_p/tot, ansi_p, tot
    printf "  Custom pass/fail:   %d / %d\n", cust_p, cust_f
    printf "  Symbol pass/fail:   %d / %d   (define-condition init-forms)\n", sym_p, sym_f
  }
' "$OUTDIR"/shard-*.out
