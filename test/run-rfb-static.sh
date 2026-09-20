#!/bin/bash
# run-rfb-static.sh — MODUS SERVES A FRAMEBUFFER; SOMETHING THAT IS NOT MODUS LOOKS AT IT.
#
#   test/run-rfb-static.sh [MODUS-BINARY]
#
# Starts test/rfb-static.lisp (a minimal RFB 3.8 server written against modus's
# own socket layer), reads the ephemeral port it prints, points
# test/rfb-client.py at it, and requires BOTH ends to pass.
#
# THE CLIENT IS THE JUDGE.  A modus-to-modus check cannot see a protocol bug
# both ends share, so the Python client does the handshake itself, checks every
# field of the advertised pixel format, and regenerates the expected image from
# the rule rather than being handed it.
#
# NETWORK RULES, and they are checked rather than asserted:
#   * 127.0.0.1 only — the server calls SOCKET-LISTEN, the loopback-only
#     spelling, and never SOCKET-LISTEN-ON.
#   * the port is bind(0) + getsockname, so no number is chosen or raced for.
#   * 5900-5920 is refused outright at BOTH ends — that is where this box's real
#     desktops live.
#   * the listener is torn down, and `ss` is asked afterwards whether anything
#     is still listening on that port.

set -u
BIN=${1:-./modus}
cd "$(dirname "$0")/.." || exit 1
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }

WORK=$(mktemp -d)
SRV_OUT="$WORK/server.txt"
cleanup() {
  [ -n "${SRV_PID:-}" ] && kill -9 "$SRV_PID" 2>/dev/null
  rm -rf "$WORK"
}
trap cleanup EXIT

echo "=== MODUS SERVES A FRAMEBUFFER OVER RFB ==="
echo "binary: $BIN"
echo

"$BIN" --script test/rfb-static.lisp > "$SRV_OUT" 2>"$WORK/server.err" &
SRV_PID=$!

# Wait for the server to publish its port (it prints "PORT <n>" and flushes).
PORT=""
for _ in $(seq 1 300); do
  PORT=$(grep -oE '^PORT [0-9]+' "$SRV_OUT" 2>/dev/null | head -1 | awk '{print $2}')
  [ -n "$PORT" ] && break
  kill -0 "$SRV_PID" 2>/dev/null || break
  sleep 1
done

if [ -z "$PORT" ]; then
  echo "FAIL: the server never published a port."
  echo "--- server stdout ---"; tail -20 "$SRV_OUT"
  echo "--- server stderr ---"; grep -vE "trampoline|Handler-stack|WARN:" "$WORK/server.err" | tail -20
  exit 1
fi

echo "server is listening on 127.0.0.1:$PORT"
if [ "$PORT" -ge 5900 ] && [ "$PORT" -le 5920 ]; then
  echo "FAIL: port $PORT is inside the forbidden 5900-5920 range."
  exit 1
fi
echo "  (outside 5900-5920: ok)"
echo

echo "--- what a REAL VNC client makes of it -------------------"
python3 test/rfb-client.py "$PORT"
CLIENT_RC=$?
echo

wait "$SRV_PID"; SRV_RC=$?
SRV_PID=""

echo "--- what the server itself reported ----------------------"
grep -vE "trampoline|Handler-stack|WARN:" "$SRV_OUT" | sed '/^[[:space:]]*$/d'
echo

echo "--- and is anything still listening? ---------------------"
LEFT=$(ss -Hltn 2>/dev/null | awk -v p=":$PORT" '$4 ~ p"$" {print}')
if [ -n "$LEFT" ]; then
  echo "FAIL: something is STILL LISTENING on $PORT:"
  echo "$LEFT"
  LEFT_RC=1
else
  echo "ss: nothing is listening on 127.0.0.1:$PORT   (ok)"
  LEFT_RC=0
fi
echo

SRV_VERDICT=$(grep -c "STATIC FRAMEBUFFER OVER RFB: PASS" "$SRV_OUT")
echo "=== VERDICT ==============================================="
echo "client rc=$CLIENT_RC   server rc=$SRV_RC   server verdict lines=$SRV_VERDICT   listener left=$LEFT_RC"
if [ "$CLIENT_RC" -eq 0 ] && [ "$SRV_VERDICT" -ge 1 ] && [ "$LEFT_RC" -eq 0 ]; then
  echo "PASS: modus served a framebuffer to a real VNC client, and closed up after itself."
  exit 0
fi
echo "FAIL"
exit 1
