#!/bin/sh
# run-concurrent-interp.sh [MODUS] [RUNS] — the RATE, because one run is noise.
#
# Prints pass/fail over RUNS for the shim arm and the raw arm side by side.
# The shim arm is expected to FAIL most runs and the raw arm to pass all of
# them; that difference is the finding, and a single clean shim run means
# "not reproduced in this shape", never "fixed".
set -u
MODUS=${1:-./modus}
RUNS=${2:-6}
HERE=$(cd "$(dirname "$0")" && pwd)
arm () {
    p=0; f=0; i=0
    while [ $i -lt "$RUNS" ]; do
        if env $1 NT=2 K=2000 timeout 200 "$MODUS" --script "$HERE/concurrent-interp-probe.lisp" 2>/dev/null \
             | grep -q SURVIVED; then p=$((p+1)); else f=$((f+1)); fi
        i=$((i+1))
    done
    printf "  %-28s pass=%-3s fail=%-3s of %s\n" "$2" "$p" "$f" "$RUNS"
}
echo "== concurrent interpretation, $RUNS runs per arm =="
arm "RAW=0" "sb-thread:make-thread"
arm "RAW=1" "%make-native-thread"
arm "RAW=0 EAGER=1" "shim, but jit-eager first"
