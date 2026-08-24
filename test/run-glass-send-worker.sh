#!/bin/bash
# run-glass-send-worker.sh — THE BISECT UNDER test/run-glass-serve.sh's WALL.
#
#   test/run-glass-send-worker.sh [MODUS-BINARY] [MODE] [GLASS-DIR] [CRAM-DIR]
#
# MODE is plain | tx | lock | both | all (default all).  See the header of
# test/glass-send-worker.lisp: the four arms differ ONLY by which wrappers
# RFB-SENDER-LOOP puts around GLASS:SEND-RECTS, and the Python peer counts the
# bytes that reach it.
#
# EXPECTED: `both' does not deliver, on every machine tried, so `all' is
# EXPECTED TO REPORT A FAILING ARM and this script exits non-zero when it does
# — it is a reproducer, and a reproducer that passes has stopped reproducing.
#
# THE OTHER THREE ARMS ARE ENVIRONMENT-DEPENDENT: they complete the transfer
# everywhere, but on some machines exactly one pixel arrives as 0x000000, at an
# index that MOVES between environments (2591 on one, 2528 on another).  If you
# see that, the line to report is FB-SELFCHECK — it grades the framebuffer
# before a client can connect, so it says whether the PAINT lost the pixel or
# the WIRE did.  GLASS_SEND_SELFCHECK=0 turns the readback off.
#
# NOTHING IS LEFT LISTENING: loopback, a kernel-chosen port, 5900-5920 refused,
# and `ss' asked afterwards.
set -u
BIN=${1:-./modus}
MODE=${2:-all}
GLASS=${3:-/home/claude/glass}
CRAM=${4:-/home/claude/cram}
cd "$(dirname "$0")/.." || exit 1
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }

WORK=$(mktemp -d) || exit 1
SERVER_PID=""
cleanup() {
  if [ -n "$SERVER_PID" ] && kill -0 "$SERVER_PID" 2>/dev/null; then
    kill -9 "$SERVER_PID" 2>/dev/null
  fi
  rm -rf "$WORK"
}
trap cleanup EXIT

sbcl --script test/glass-manifest.lisp "$GLASS/" "$CRAM/" "$WORK/manifest.lisp" || {
  echo "FAIL: could not build the manifest from the .asd files" >&2; exit 1; }
cat "$WORK/manifest.lisp" > "$WORK/runner.lisp"
cat test/glass-send-worker.lisp >> "$WORK/runner.lisp"

[ "$MODE" = "all" ] && MODES="plain tx lock both" || MODES="$MODE"

RC=0
for m in $MODES; do
  echo "=== MODE $m ==="
  GLASS_SEND_MODE="$m" timeout 400 "$BIN" --script "$WORK/runner.lisp" \
      > "$WORK/server.$m.out" 2> "$WORK/server.$m.err" &
  SERVER_PID=$!

  PORT=""
  for _ in $(seq 1 400); do
    PORT=$(grep -m1 '^PORT ' "$WORK/server.$m.out" 2>/dev/null | awk '{print $2}')
    [ -n "$PORT" ] && break
    kill -0 "$SERVER_PID" 2>/dev/null || break
    sleep 1
  done
  if [ -z "$PORT" ]; then
    echo "  FAIL: the server never announced a port"
    tail -5 "$WORK/server.$m.err"
    RC=1; SERVER_PID=""; continue
  fi

  python3 test/glass-send-worker.py "$PORT"
  CRC=$?

  for _ in $(seq 1 60); do
    kill -0 "$SERVER_PID" 2>/dev/null || break
    sleep 1
  done
  if kill -0 "$SERVER_PID" 2>/dev/null; then
    kill -9 "$SERVER_PID" 2>/dev/null; wait "$SERVER_PID" 2>/dev/null; SRC=124
  else
    wait "$SERVER_PID"; SRC=$?
  fi
  SERVER_PID=""

  echo "  server rc=$SRC"
  grep -E 'MVM LONGJMP|UNHANDLED-ESCAPE|worker:|SERVER DONE|FB-SELFCHECK' "$WORK/server.$m.out" | sed 's/^/  /'

  if ss -ltnH "sport = :$PORT" 2>/dev/null | grep -q .; then
    echo "  FAIL: something is still listening on $PORT"; RC=1
  fi
  [ "$CRC" -eq 0 ] && [ "$SRC" -eq 0 ] || { echo "  ARM $m FAILED"; RC=1; }
  echo
done

if [ "$RC" -eq 0 ]; then
  echo "PASS: every arm delivered the whole update."
else
  echo "FAIL: at least one arm did not — that is the wall, reproduced."
fi
exit $RC
