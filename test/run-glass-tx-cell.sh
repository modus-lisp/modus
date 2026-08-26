#!/bin/bash
# run-glass-tx-cell.sh — THE WALL, NAMED.  See test/glass-tx-cell.lisp's header.
#
#   test/run-glass-tx-cell.sh [MODUS-BINARY] [MODE] [GLASS-DIR] [CRAM-DIR]
#
# MODE is one arm, or `all' (the default), or `key' (the four that carry the
# argument: lock, bothnil, bothpre, lockalloc).  Environment knobs, both
# documented in the .lisp header:
#
#   GLASS_TX_MAINMODE=join   main does not run a reader at all
#   GLASS_TX_FBW=48          rect 1's raw buffer becomes FBW*64*4 bytes
#   REPS=5                   run each arm REPS times and report N of M
#
# A failing arm costs the client timeout (30 s); a delivering arm is immediate.
#
# EXPECTED, TODAY: `both' does not deliver — 0 of 5, stopping at 32785 of 49180
# and printing the CAR of GLASS::*TX* turning into a CHARACTER.  Every other arm
# delivers.  So `all' EXITS NON-ZERO on a healthy tree: this is a reproducer,
# and a reproducer that passes has stopped reproducing.  Read the per-arm N-of-M
# lines, not the exit status.
#
# AND DELIVERY IS NOT HEALTH.  `lockalloc-keep' delivers all 49180 bytes with
# `e=CONS/NOTINT:CHARACTER' — the same overwrite, landing after the last read of
# the counter instead of before it.  THE LINE TO READ IS THE a=/b=/c=/d=/e=
# GRADE, not the byte count: any NOTINT: is the defect, whatever the client got.
#
# NOTHING IS LEFT LISTENING: loopback, a kernel-chosen port, 5900-5920 refused
# by the server itself, and `ss' asked afterwards.
set -u
BIN=${1:-./modus}
MODE=${2:-all}
GLASS=${3:-/home/claude/glass}
CRAM=${4:-/home/claude/cram}
REPS=${REPS:-1}
cd "$(dirname "$0")/.." || exit 1
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }

WORK=$(mktemp -d) || exit 1
SERVER_PID=""

# NO `timeout CMD &' WRAPPER.  `$!' after that is the PID of TIMEOUT, not of
# modus, so `kill "$SERVER_PID"' reaps the wrapper and leaves modus orphaned
# still holding its listener — which is exactly what "nothing left listening"
# exists to prevent.  The budget lives in the wait loops below instead, and
# TERM/INT/HUP are trapped as well as EXIT because an external kill is the case
# that leaks.
cleanup() {
  if [ -n "${SERVER_PID:-}" ] && kill -0 "$SERVER_PID" 2>/dev/null; then
    kill "$SERVER_PID" 2>/dev/null
    for _ in 1 2 3; do kill -0 "$SERVER_PID" 2>/dev/null || break; sleep 1; done
    kill -9 "$SERVER_PID" 2>/dev/null
    wait "$SERVER_PID" 2>/dev/null
  fi
  rm -rf "$WORK"
}
trap cleanup EXIT
trap 'cleanup; exit 143' TERM INT HUP

sbcl --script test/glass-manifest.lisp "$GLASS/" "$CRAM/" "$WORK/manifest.lisp" || {
  echo "FAIL: could not build the manifest from the .asd files" >&2; exit 1; }
cat "$WORK/manifest.lisp" > "$WORK/runner.lisp"
cat test/glass-tx-cell.lisp >> "$WORK/runner.lisp"

case "$MODE" in
  all) MODES="plain tx lock both bothnil othersp lockalloc bothpre lockset mymtx lockalloc-keep" ;;
  key) MODES="lock bothnil bothpre lockalloc" ;;
  *)   MODES="$MODE" ;;
esac

RC=0
for m in $MODES; do
  echo "=== MODE $m ==="
  PASS=0; N=0; CORRUPT=0
  for _ in $(seq 1 "$REPS"); do
    N=$((N + 1))
    GLASS_TX_MODE="$m" "$BIN" --script "$WORK/runner.lisp" \
        > "$WORK/server.out" 2> "$WORK/server.err" &
    SERVER_PID=$!

    PORT=""; EXPECT=""
    for _ in $(seq 1 400); do
      PORT=$(grep -m1 '^PORT ' "$WORK/server.out" 2>/dev/null | awk '{print $2}')
      EXPECT=$(grep -m1 '^MODE ' "$WORK/server.out" 2>/dev/null | awk '{print $6}')
      [ -n "$PORT" ] && [ -n "$EXPECT" ] && break
      kill -0 "$SERVER_PID" 2>/dev/null || break
      sleep 1
    done
    if [ -z "$PORT" ] || [ -z "$EXPECT" ]; then
      echo "  FAIL: the server never announced a port"
      tail -5 "$WORK/server.err"
      SERVER_PID=""; RC=1; continue
    fi

    python3 test/glass-tx-cell.py "$PORT" "$EXPECT"
    CRC=$?

    for _ in $(seq 1 60); do kill -0 "$SERVER_PID" 2>/dev/null || break; sleep 1; done
    if kill -0 "$SERVER_PID" 2>/dev/null; then
      kill -9 "$SERVER_PID" 2>/dev/null; wait "$SERVER_PID" 2>/dev/null; SRC=124
    else
      wait "$SERVER_PID"; SRC=$?
    fi
    SERVER_PID=""

    # THE GRADE, and it is of memory rather than of the wire: a=CONS/0 b=CONS/4
    # then NOTINT:CHARACTER is the cell being handed out again while live.
    grep -E 'a=CONS|worker:|main:|reader got|SERVER DONE|keep=' "$WORK/server.out" \
      | sed 's/^/  /'
    echo "  server rc=$SRC"

    if ss -ltnH "sport = :$PORT" 2>/dev/null | grep -q .; then
      echo "  FAIL: something is still listening on $PORT"; RC=1
    fi

    # THE BYTE COUNT IS NOT THE ORACLE — `lockalloc-keep' delivers all 49180
    # bytes with the counter's CAR already a CHARACTER, because the overwrite
    # landed after the last TX+ that would have read it.  An arm is CLEAN only
    # if it delivered AND no grade point saw a non-integer CAR.
    if grep -q 'NOTINT:\|=BROKEN' "$WORK/server.out"; then
      echo "  !! COUNTER OVERWRITTEN (a NOTINT:/BROKEN grade above)"
      CORRUPT=$((CORRUPT + 1))
    fi
    [ "$CRC" -eq 0 ] && [ "$SRC" -eq 0 ] && PASS=$((PASS + 1))
  done
  echo "  ARM $m: $PASS of $N delivered, $CORRUPT of $N with the counter overwritten"
  { [ "$PASS" -eq "$N" ] && [ "$CORRUPT" -eq 0 ]; } || RC=1
  echo
done

if [ "$RC" -eq 0 ]; then
  echo "PASS: every arm delivered — the overwrite has stopped reproducing."
else
  echo "FAIL: at least one arm did not — the overwrite, reproduced."
fi
exit $RC
