#!/bin/sh
# test/run-thread-heap-and-wait.sh [MODUS] -- both parts of test/thread-heap-and-wait.lisp, each in
# the heap it needs; exit 0 only if both print PASS.
M=${1:-./modus}; here=$(dirname "$0"); rc=0
for part in wait heap; do
  if [ $part = heap ]; then size="--dynamic-space-size 8GB"; else size=; fi
  out=$(THW_PART=$part timeout 300 "$M" $size --script "$here/thread-heap-and-wait.lisp" 2>&1)
  printf '%s\n' "$out" | grep -a "^ok \|^FAIL\|exhausted"
  printf '%s\n' "$out" | grep -qa "thread-heap-and-wait: PASS" || rc=1
done
[ $rc = 0 ] && echo "thread-heap-and-wait: PASS" || echo "thread-heap-and-wait: FAIL"
exit $rc
