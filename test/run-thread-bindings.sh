#!/bin/sh
# test/run-thread-bindings.sh [MODUS] -- test/thread-bindings.lisp must print
# exactly what SBCL prints: dynamic bindings belong to the thread that made
# them, the main thread's included.  MODUS may be a wrapper script (the
# aarch64 CLI under qemu-aarch64-static); TIMEOUT (s) defaults to 300.
# Dropped from modus's output as well as ';' lines: the AArch64 CLI's
# 'WARNING: undefined function: %...' lines on stderr, printed when the SB-*
# surfaces are first installed (x86-64-only helpers they name: pre-existing,
# unrelated to bindings).
M=${1:-./modus}
here=$(cd "$(dirname "$0")" && pwd)
w=$(mktemp -d "${TMPDIR:-/tmp}/thread-bindings.XXXXXX") || exit 2
sbcl --script "$here/thread-bindings.lisp" > "$w/sbcl.out" 2>&1
MODUS_FASL_DIR="$w/fasl" timeout "${TIMEOUT:-300}" "$M" --script "$here/thread-bindings.lisp" 2>&1 \
  | grep -v '^;' | grep -v '^WARNING: undefined function: %' > "$w/modus.out"
if diff "$w/sbcl.out" "$w/modus.out"; then
  echo "thread-bindings: PASS ($(wc -l < "$w/sbcl.out") lines identical to SBCL)"; rc=0
else
  echo "thread-bindings: FAIL (diff above: < SBCL, > modus)"; rc=1
fi
rm -r "$w"
exit $rc
