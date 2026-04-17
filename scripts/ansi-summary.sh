#!/bin/bash
# ansi-summary.sh — run /tmp/modus-ansi-test and print a real pass/fail summary.
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

OUT=$(mktemp)
trap 'rm -f "$OUT"' EXIT

/tmp/modus-ansi-test > "$OUT" 2>&1
status=$?

awk '
  match($0, /ANSI-TOTAL=[0-9]+/)       { tot = substr($0, RSTART+11, RLENGTH-11)+0 }
  match($0, /P:[0-9]+\/[0-9]+\/[0-9]+\/[0-9]+/) { pdone++ }
  /^FAIL/                              { fails++ }
  {
    # Count "+" bytes anywhere in the line — rt-run-test writes one per pass,
    # with no newline, so they may be packed onto lines with other content.
    n = gsub(/\+/, "+")
    pass += n
  }
  END {
    ran = pass + fails
    lost = tot - ran
    printf "ANSI summary\n"
    printf "  expected:               %d\n", tot
    printf "  forks finished cleanly: %d\n", pdone
    printf "  ran:                    %d\n", ran
    printf "  passed:                 %d  (includes a few non-ANSI rt-run-test calls)\n", pass
    printf "  failed:                 %d\n", fails
    printf "  lost (fork crash):      %d\n", lost
    if (ran > 0) {
      printf "  pass rate of run tests: %.2f%%  (%d / %d)\n", 100.0*pass/ran, pass, ran
    }
    if (tot > 0) {
      printf "  pass rate of expected:  %.2f%%  (%d / %d)\n", 100.0*pass/tot, pass, tot
    }
  }
' "$OUT"

exit $status
