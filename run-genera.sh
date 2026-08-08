#!/bin/bash
# lf/run.sh with the Genera compat preludes loaded first.
BIN="$1"; TAG="$2"; PRE="${3:-/home/claude/ws-genera}"
D=/home/claude/lf/logs/$TAG
mkdir -p "$D"
run_one() {
  lib="$1"
  s=$(date +%s)
  timeout 1500 "$BIN" --load $PRE/net/cooperative-atomics.lisp \
     --load $PRE/net/genera-compat.lisp \
     --load /home/claude/lf/drivers/$lib.lisp --quit > "$D/$lib.log" 2>&1
  rc=$?
  e=$(date +%s)
  echo "EXIT=$rc SECS=$((e-s))" >> "$D/$lib.log"
}
export -f run_one; export BIN D PRE
( cat /home/claude/lf/ladder.txt; echo alexandria-ql; echo sha1-ql ) \
  | xargs -P 8 -I{} bash -c 'run_one {}'
echo "LADDER-DONE $TAG"
