#!/bin/sh
# run-socket-server.sh — drive test/hosted-sockets-server.lisp against a client
# that is NOT modus.
#
#   test/run-socket-server.sh [MODUS] [OUTDIR] [NCONN] [NBYTES] [CHUNK]
#
# The modus side binds 127.0.0.1 PORT 0 and prints the port the kernel chose;
# this script waits for that line and then runs test/socket-client.py against
# it.  Nothing here picks a port number, so nothing here can collide with a
# desktop or with another run.
#
# NETWORK HYGIENE, because this is a shared box:
#   * 127.0.0.1 only — the modus side calls SOCKET-LISTEN-ON with
#     (%SOCK-LOOPBACK) written out in full and has no other spelling available.
#   * an EPHEMERAL port only, chosen by the kernel via bind(0) + getsockname.
#   * the server holds a DEADLINE, so a run whose client never arrives still
#     ends by itself and still closes its listener.
#   * `timeout' bounds the modus process as a second line of defence, and this
#     script verifies at the end that the port is no longer listening.
#
# Environment passed through to the modus side:
#   MODUS_SK_SPIN    per-event spin (widens the two-CPU race window)
#   MODUS_SK_SHARED  1 = THE NEGATIVE CONTROL (every CPU back on one page)
#   MODUS_SK_NCPU    1 or 2 serving threads
#   MODUS_SK_SECS    server deadline in seconds
#
# Exit status 0 only if BOTH the modus checks and the client comparison pass.

set -u

MODUS=${1:-./modus}
OUTDIR=${2:-/tmp/modus-socket-test}
NCONN=${3:-4}
NBYTES=${4:-1048576}
CHUNK=${5:-16384}

HERE=$(cd "$(dirname "$0")" && pwd)
mkdir -p "$OUTDIR" || exit 2
SRV="$OUTDIR/server.out"
CLI="$OUTDIR/client.out"
: > "$SRV"
: > "$CLI"

MODUS_SK_CONNS=$NCONN
MODUS_SK_BYTES=$NBYTES
export MODUS_SK_CONNS MODUS_SK_BYTES

# The deadline is the outer bound on how long anything can be listening.
SECS=${MODUS_SK_SECS:-40}
export MODUS_SK_SECS=$SECS

timeout $((SECS + 25)) "$MODUS" --script "$HERE/hosted-sockets-server.lisp" \
    > "$SRV" 2>&1 &
SRVPID=$!

# Wait for the port line.  If the server dies first, stop waiting.
PORT=""
i=0
while [ $i -lt 600 ]; do
    PORT=$(sed -n 's/^PORT \([0-9][0-9]*\)$/\1/p' "$SRV" | head -1)
    [ -n "$PORT" ] && break
    kill -0 "$SRVPID" 2>/dev/null || break
    sleep 0.1
    i=$((i + 1))
done

if [ -z "$PORT" ]; then
    echo "run-socket-server.sh: the server never announced a port"
    wait "$SRVPID" 2>/dev/null
    sed -n '1,80p' "$SRV"
    exit 2
fi

echo "run-socket-server.sh: server on 127.0.0.1:$PORT — $NCONN connections x $NBYTES bytes"
timeout 120 python3 "$HERE/socket-client.py" "$PORT" "$NCONN" "$NBYTES" "$CHUNK" > "$CLI" 2>&1
CLIRC=$?

wait "$SRVPID"
SRVRC=$?

echo "---- client ----"
cat "$CLI"
echo "---- server ----"
cat "$SRV"

# NOTHING MAY BE LEFT LISTENING.  Checked, not assumed.
STILL=$(ss -ltn "sport = :$PORT" 2>/dev/null | grep -c LISTEN)
echo "---- after ----"
echo "listeners still on port $PORT: $STILL"

VERDICT=0
grep -q "SOCKET SERVER: PASS" "$SRV" || VERDICT=1
grep -q "CLIENT-VERDICT OK" "$CLI" || VERDICT=1
[ "$STILL" = "0" ] || VERDICT=1
[ "$SRVRC" = "0" ] || VERDICT=1
[ "$CLIRC" = "0" ] || VERDICT=1

if [ $VERDICT = 0 ]; then
    echo "RUN-VERDICT OK  (server rc=$SRVRC client rc=$CLIRC listeners=$STILL)"
else
    echo "RUN-VERDICT FAIL  (server rc=$SRVRC client rc=$CLIRC listeners=$STILL)"
fi
exit $VERDICT
