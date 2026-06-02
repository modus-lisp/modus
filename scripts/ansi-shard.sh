#!/bin/bash
# ansi-shard.sh — shard the ANSI suite across N parallel processes.
#
# Each shard runs `./modus-ansi-test START END` which executes only tests
# with id in [START, END). Outputs are merged and summarized.
#
# ID classification used by the post-run summary:
#   1     ..  9999  — Modus's pre-ANSI custom tests (probes, smoke).
#   10001 .. 27708  — the ANSI suite (the headline number).
#   27709 .. 99999  — Modus probe tests (eval / suite-load probes, IDs
#                     like 56491, 57001 etc.).
#   100000+         — runtime-loaded suite tests with hash-based IDs from
#                     %load-suite-file's auto-numbering.
#
# Prior to 2026-05-31 the summary counted every P:<id> with id >= 10001
# as an ANSI pass — including the runtime-suite hash IDs in the
# trillions and the 5x000-range Modus probes.  That inflated the
# headline by ~1300 passes (true 14k → reported 15.3k).  Buckets 3 and
# 4 are now tracked under "Extra pass/fail" and NOT included in the
# ANSI total.

set -u

SHARDS=${SHARDS:-32}
BINARY=${BINARY:-/home/claude/modus/tmp/modus-ansi-test}
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

# Per-shard wall-clock cap (seconds). Each shard processes ~150 tests; even at
# 30s/file with our per-file alarm, no shard should run past ~10 min. The cap
# protects against everything outside the per-file alarm (e.g. a hang in the
# parent's bookkeeping between fork-files, or a runaway in custom tests before
# the ANSI section starts).
SHARD_TIMEOUT=${SHARD_TIMEOUT:-600}

# Launch all shards in parallel. Shard 0 also runs the pre-fork custom tests
# (it uses start=0 to not skip them). `timeout` lets the shard binary write
# its output normally but kills it if it runs over.
for i in $(seq 0 $(( SHARDS - 1 ))); do
  lo=$(( FIRST + i * STEP ))
  hi=$(( lo + STEP ))
  if [ $hi -gt $LAST ]; then hi=$LAST; fi
  if [ $i -eq 0 ]; then lo=0; fi     # shard 0 gets pre-fork custom tests too
  timeout --kill-after=5s "$SHARD_TIMEOUT" \
    "$BINARY" "$lo" "$hi" > "$OUTDIR/shard-$i.out" 2>&1 &
done

# Wait for all.
wait

elapsed=$(( $(date +%s) - start_time ))
echo "# all shards done in ${elapsed}s"

# Aggregate.
grep -ah '' "$OUTDIR"/shard-*.out | awk '
  # ID buckets:
  #   1     ..  9999  — Modus pre-ANSI custom tests (probes, smoke).
  #   10001 .. 27708  — the ANSI suite (the headline number).
  #   27709 .. 99999  — extra Modus probes that landed in 5x000 ranges
  #                     (eval / suite-load probes, IDs like 56491, 57001).
  #   100000+         — runtime-loaded suite tests with hash-based IDs
  #                     from %load-suite-file auto-numbering.  Real
  #                     passes, but NOT part of the 17352-test ANSI
  #                     denominator — bucketing them into ansi_pass
  #                     inflated the headline by ~1300 historically.
  # Symbol-named deftest init-forms (FAIL <name> with no leading digits)
  # are tracked separately by text and deduped.
  match($0, /ANSI-TOTAL=[0-9]+/) { tot = substr($0, RSTART+11, RLENGTH-11)+0 }
  /^P:/ {
    key = $0
    if (match(key, /^P:[0-9]+$/)) {
      id = substr(key, 3) + 0
      if      (id <= 9999)                 { custom_pass[id] = 1 }
      else if (id >= 10001 && id <= 27708) { ansi_pass[id]   = 1 }
      else                                 { extra_pass[id]  = 1 }
    } else {
      sym_pass[key] = 1
    }
    next
  }
  /^FAIL / {
    rest = substr($0, 6)
    # ID-tagged fails may have a trailing " GOT:... EXP:..." annotation
    # (rt-run-test prints details for the first few failures of each chunk).
    # Match leading integer id either with or without trailing text.
    if (match(rest, /^[0-9]+( |$)/)) {
      id = rest + 0
      if      (id <= 9999)                 { custom_fail[id] = 1 }
      else if (id >= 10001 && id <= 27708) { ansi_fail[id]   = 1 }
      else                                 { extra_fail[id]  = 1 }
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
    for (id in extra_pass)  ex_p++
    for (id in extra_fail)  ex_f++
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
    printf "  Extra pass/fail:    %d / %d   (Modus probes + runtime-suite-load tests; NOT in ANSI total)\n", ex_p, ex_f
    printf "  Symbol pass/fail:   %d / %d   (define-condition init-forms)\n", sym_p, sym_f
  }
'
