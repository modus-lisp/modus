#!/bin/bash
# run-glass-serve.sh — GLASS'S OWN RFB SERVER, ON MODUS, SERVING A REAL VNC CLIENT.
#
#   test/run-glass-serve.sh [MODUS-BINARY] [CLIENTS] [GLASS-DIR] [CRAM-DIR]
#
# Starts test/glass-serve.lisp under modus.  That script loads the :glass system
# from the glass tree, paints a framebuffer, and hands glass's own GLASS:SERVE a
# loopback listener on a kernel-chosen port.  It prints `PORT n'; this script
# reads that line and runs test/glass-rfb-client.py against it.
#
# THE CLIENT IS THE JUDGE.  It is Python, it is not modus, and it generates the
# image it expects rather than being handed one.  Both sides must pass: the
# server side checks what only it can see (the listener was loopback, the port
# was outside the VNC range, no descriptor leaked), the client side checks the
# wire.
#
# NOTHING IS LEFT LISTENING: the modus process is waited for, killed if it
# overruns, and `ss' is asked afterwards whether the port is still bound.
set -u
BIN=${1:-./modus}
CLIENTS=${2:-1}
GLASS=${3:-/home/claude/glass}
CRAM=${4:-/home/claude/cram}
cd "$(dirname "$0")/.." || exit 1
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }

WORK=$(mktemp -d) || exit 1
SERVER_PID=""
cleanup() {
  if [ -n "$SERVER_PID" ] && kill -0 "$SERVER_PID" 2>/dev/null; then
    kill "$SERVER_PID" 2>/dev/null
    sleep 1
    kill -9 "$SERVER_PID" 2>/dev/null
  fi
  rm -rf "$WORK"
}
trap cleanup EXIT

sbcl --script test/glass-manifest.lisp "$GLASS/" "$CRAM/" "$WORK/manifest.lisp" || {
  echo "FAIL: could not build the manifest from the .asd files" >&2; exit 1; }
cat "$WORK/manifest.lisp" > "$WORK/runner.lisp"
cat test/glass-serve.lisp >> "$WORK/runner.lisp"
# ---- GLASS_SERVE_TRACE: an optional diagnostic overlay ----------------------
#
# A file of Lisp REDEFINITIONS spliced in after :glass has loaded and before
# anything is served.  It exists because the interesting failures here are on a
# WORKER thread inside code glass owns, and glass is read-only: redefining one
# of its functions IN THE IMAGE is how you get a window onto what the sender
# thread sees without editing the tree.
#
# TWO TRAPS, both paid for once already.  (1) `#\'name' resolves LATE on this
# image, so a "wrapper" that saves #'f and then redefines f calls ITSELF and
# dies of stack exhaustion — replace outright, do not wrap.  (2) Have the
# overlay RECORD into a global and let the MAIN thread print it after SERVE
# returns; a worker thread that prints has added its own variable to the
# experiment.
if [ -n "${GLASS_SERVE_TRACE:-}" ]; then
  python3 - "$WORK/runner.lisp" "$GLASS_SERVE_TRACE" <<'PYEOF'
import sys
r,t=sys.argv[1],sys.argv[2]
s=open(r).read()
mark="(format t \"~&=== loaded ===~%\")"
s=s.replace(mark, open(t).read()+"\n(in-package \"COMMON-LISP-USER\")\n"+mark,1)
open(r,"w").write(s)
PYEOF
fi

echo "=== GLASS'S OWN RFB SERVER, ON MODUS ==="
echo "modus:   $BIN"
echo "glass:   $GLASS  (read-only)"
echo "clients: $CLIENTS"
echo

GLASS_SERVE_CLIENTS="$CLIENTS" "$BIN" --script "$WORK/runner.lisp" \
    > "$WORK/server.out" 2> "$WORK/server.err" &
SERVER_PID=$!

# Wait for the PORT line.  A budget, so a server that never gets there ends the
# run instead of hanging it.
PORT=""
for _ in $(seq 1 600); do
  PORT=$(grep -m1 '^PORT ' "$WORK/server.out" 2>/dev/null | awk '{print $2}')
  [ -n "$PORT" ] && break
  kill -0 "$SERVER_PID" 2>/dev/null || break
  sleep 1
done

if [ -z "$PORT" ]; then
  echo "FAIL: the server never announced a port."
  echo "--- server stdout ---"; tail -30 "$WORK/server.out"
  echo "--- server stderr ---"; tail -30 "$WORK/server.err"
  exit 1
fi

if [ "$PORT" -ge 5900 ] && [ "$PORT" -le 5920 ]; then
  echo "FAIL: refusing to test on $PORT — inside the 5900-5920 VNC range."
  exit 1
fi

echo "=== THE CLIENT (python, not modus) — port $PORT ==="
python3 test/glass-rfb-client.py "$PORT" --clients "$CLIENTS" \
        > "$WORK/client.out" 2>&1
CLIENT_RC=$?
cat "$WORK/client.out"
echo

# The server should now come back by itself.
for _ in $(seq 1 120); do
  kill -0 "$SERVER_PID" 2>/dev/null || break
  sleep 1
done
if kill -0 "$SERVER_PID" 2>/dev/null; then
  echo "FAIL: the server did not return after the client(s) finished; killing it."
  kill "$SERVER_PID" 2>/dev/null
  wait "$SERVER_PID" 2>/dev/null
  SERVER_RC=124
else
  wait "$SERVER_PID"
  SERVER_RC=$?
fi
SERVER_PID=""

echo "=== THE SERVER (modus) ==="
grep -vE '^PORT |^CLIENTS ' "$WORK/server.out" | grep -E '^(ok|FAIL|===|[0-9]+ checks|GLASS)'
echo

echo "=== NOTHING LEFT LISTENING ==="
if ss -ltnH "sport = :$PORT" 2>/dev/null | grep -q .; then
  echo "FAIL: something is still listening on $PORT:"
  ss -ltn "sport = :$PORT"
  LISTEN_RC=1
else
  echo "ok   ss reports nothing listening on $PORT"
  LISTEN_RC=0
fi
echo

if [ "$CLIENT_RC" -eq 0 ] && [ "$SERVER_RC" -eq 0 ] && [ "$LISTEN_RC" -eq 0 ]; then
  echo "PASS: glass's RFB server, on modus, served $CLIENTS real VNC client(s)."
  exit 0
fi
echo "FAIL: client rc=$CLIENT_RC server rc=$SERVER_RC listening rc=$LISTEN_RC"
echo "--- server stderr (last 40) ---"
tail -40 "$WORK/server.err"
exit 1
