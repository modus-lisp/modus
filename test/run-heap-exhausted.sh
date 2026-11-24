#!/bin/bash
# run-heap-exhausted.sh BINARY -- 256 MB semispaces, 128 MB live, then one 104 MB
# allocation.  PASS only if the process says "an allocation does not fit" and exits 137
# BEFORE making it (no "got" line): the sized gc-check's re-test, translate-x64.
BIN=${1:-./modus}
cd "$(dirname "$0")/.."
out=$(timeout 300 "$BIN" --dynamic-space-size 512 --script test/heap-exhausted.lisp </dev/null 2>&1); rc=$?
echo "$out" | tail -3
if [ $rc -eq 137 ] && echo "$out" | grep -q "an allocation does not fit" && ! echo "$out" | grep -q "^got"; then
  echo "HEAP-EXHAUSTED: PASS"; exit 0
fi
echo "HEAP-EXHAUSTED: FAIL (rc=$rc)"; exit 1
