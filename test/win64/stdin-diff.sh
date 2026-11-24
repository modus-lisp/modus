#!/bin/sh
# stdin-diff.sh - run test files on the Windows image and on a Linux build of
# the same source, and compare what they print.
#
# Usage: test/win64/stdin-diff.sh WIN_EXE LINUX_BIN OUTDIR TEST.lisp...
#
# The Windows image has no file I/O yet, so a test cannot be --script'ed; each
# file is fed to the REPL on stdin instead, on BOTH images, so the two runs see
# the same thing.  Prompts are stripped.  A file is SAME when the outputs are
# identical, DIFF otherwise; either way the FAIL lines of each side are counted,
# because a test that prints timings differs without being wrong.
# WINE_RUN: the command that runs a Windows program (default wine64).

set -u
win=${1:?usage: stdin-diff.sh WIN_EXE LINUX_BIN OUTDIR TEST...}; shift
lin=${1:?}; shift
out=${1:?}; shift
run=${WINE_RUN:-wine64}
mkdir -p "$out"
for t in "$@"; do
  n=$(basename "$t" .lisp)
  timeout 1200 $run "$win" < "$t" 2>"$out/$n.win.err" | sed 's/^> //' > "$out/$n.win"
  wrc=$?
  timeout 1200 "$lin" < "$t" 2>"$out/$n.lin.err" | sed 's/^> //' > "$out/$n.lin"
  lrc=$?
  wf=$(grep -ci "fail" "$out/$n.win"); lf=$(grep -ci "fail" "$out/$n.lin")
  wcrash=$(grep -c "page fault" "$out/$n.win.err")
  if cmp -s "$out/$n.win" "$out/$n.lin"; then same=SAME; else same=DIFF; fi
  printf '%-32s %s  fail-lines win=%s linux=%s  rc win=%s linux=%s  win-fault=%s\n' \
    "$n" "$same" "$wf" "$lf" "$wrc" "$lrc" "$wcrash"
done
