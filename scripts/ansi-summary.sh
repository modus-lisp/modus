#!/bin/bash
# ansi-summary.sh — run /home/claude/modus/tmp/modus-ansi-test and print a real pass/fail summary.
#
# The harness emits, for every ANSI test:
#   "+"                           — one byte per passing test (survives fork crashes)
#   "FAIL <id> [GOT:... EXP:...]" — one line per failing test
# Plus per-fork on clean exit:
#   "P:chunk/passes/fails/total"  — only if the fork didn't crash
# And once at startup:
#   "EXP:N"                       — total tests we expected to run
#
# Pass count = number of "+" bytes (definitive — written before any subsequent
# work could crash). Fail count = number of FAIL lines. Lost = expected -
# (passes + fails). Crashed forks contribute zeros to the gap.

set -u

# Keep raw per-test output around for post-mortem grep (the previous
# mktemp + trap rm pattern wiped it at script exit and we lost data
# every time the sweep was interrupted or someone wanted to re-summarise).
TMPDIR="$(cd "$(dirname "$0")/.." && pwd)/tmp"
mkdir -p "$TMPDIR"
OUT="$TMPDIR/ansi-summary-raw.log"

/home/claude/modus/tmp/modus-ansi-test > "$OUT" 2>&1
status=$?

awk '
  # ID buckets (same scheme as ansi-shard.sh):
  #   1     ..  9999  — custom pre-ANSI Modus tests
  #   10001 .. 27708  — ANSI suite (the headline)
  #   27709 .. 99999  — Modus probe tests (56491, 57001, …)
  #   100000+         — runtime-loaded suite tests (hash IDs)
  # The pre-fix awk counted ALL P: lines as "passed", which inflated the
  # headline by ~1300 because %load-suite-file emits P:<huge-id> lines
  # for every test it runs.  Without ID classification a Modus probe or
  # suite-load pass falsely advances the ANSI pass count.
  match($0, /ANSI-TOTAL=[0-9]+/) { tot = substr($0, RSTART+11, RLENGTH-11)+0 }
  /^P:/ {
    if (match($0, /^P:[0-9]+$/)) {
      id = substr($0, 3) + 0
      if      (id <= 9999)                 { cust_p++ }
      else if (id >= 10001 && id <= 27708) { pass++ }
      else                                 { ex_p++ }
    }
  }
  /^FAIL/ {
    rest = substr($0, 6)
    if (match(rest, /^[0-9]+( |$)/)) {
      id = rest + 0
      if      (id <= 9999)                 { cust_f++ }
      else if (id >= 10001 && id <= 27708) { fails++ }
      else                                 { ex_f++ }
    }
  }
  END {
    ran = pass + fails
    lost = tot - ran
    printf "ANSI summary (per-test fork isolation)\n"
    printf "  expected:           %d\n", tot
    printf "  ran:                %d\n", ran
    printf "  passed:             %d\n", pass
    printf "  failed:             %d\n", fails
    printf "  lost to test crash: %d  (test process died mid-body)\n", lost
    if (ran > 0) {
      printf "  pass rate of run:   %.2f%%  (%d / %d)\n", 100.0*pass/ran, pass, ran
    }
    if (tot > 0) {
      printf "  pass rate overall:  %.2f%%  (%d / %d)\n", 100.0*pass/tot, pass, tot
    }
    printf "  Custom pass/fail:   %d / %d\n", cust_p+0, cust_f+0
    printf "  Extra pass/fail:    %d / %d   (Modus probes + runtime-suite-load; NOT in ANSI total)\n", ex_p+0, ex_f+0
  }
' "$OUT"

exit $status
