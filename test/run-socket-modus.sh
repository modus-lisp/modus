#!/bin/sh
# run-socket-modus.sh — THE MODUS-TO-MODUS ARM: one modus process serving,
# another modus process as the client.
#
#   test/run-socket-modus.sh [MODUS] [OUTDIR] [NCONN] [ROUNDS] [CHUNK]
#
# The server is the same test/hosted-sockets-server.lisp the Python arm uses,
# so this is not a second server written to agree with a second client.  It
# runs with TWO serving threads and expects NCONN+1 connections: NCONN held
# open at once in phase A, and one more in phase B for the single send that is
# larger than the staging buffer.
#
# Same network hygiene as run-socket-server.sh: 127.0.0.1 only, an ephemeral
# port the kernel chooses, a deadline on the server, `timeout' on both
# processes, and a check at the end that nothing is still listening.

set -u

MODUS=${1:-./modus}
OUTDIR=${2:-/tmp/modus-socket-modus}
NCONN=${3:-4}
ROUNDS=${4:-16}
CHUNK=${5:-8192}

HERE=$(cd "$(dirname "$0")" && pwd)
mkdir -p "$OUTDIR" || exit 2
SRV="$OUTDIR/server.out"
CLI="$OUTDIR/client.out"
: > "$SRV"
: > "$CLI"

SECS=${MODUS_SK_SECS:-40}
BIG=${MODUS_SK_BIG:-98304}

# THE SPIN IS HIGHER HERE THAN IN THE PYTHON ARM, AND THAT IS THE CLIENT'S
# FAULT, NOT THE SERVER'S.  The Python client streams 1 MB per connection from
# four threads, so the server is saturated and two handlers are inside at once
# by themselves.  The modus client is ONE thread in LOCK STEP — send a chunk on
# every connection, then read a chunk back from every connection — so between
# rounds the server has nothing to do and the two handlers' lifetimes barely
# meet.  The spin lengthens a handler so the overlap the server is capable of
# actually happens; it does not manufacture one (two handlers really are inside
# the table at the same instant, counted under the lock).  Measured: at 20000
# the overlap witness reads 0, at 200000 it reads 1, at 2000000 it reads ~48.
export MODUS_SK_SPIN=${MODUS_SK_SPIN:-2000000}
EXPECT=$((NCONN + 1))
TOTAL=$((NCONN * ROUNDS * CHUNK + BIG))

MODUS_SK_CONNS=$EXPECT MODUS_SK_BYTES=0 MODUS_SK_SECS=$SECS \
    timeout $((SECS + 25)) "$MODUS" --script "$HERE/hosted-sockets-server.lisp" \
    > "$SRV" 2>&1 &
SRVPID=$!

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
    echo "run-socket-modus.sh: the server never announced a port"
    wait "$SRVPID" 2>/dev/null
    sed -n '1,80p' "$SRV"
    exit 2
fi

echo "run-socket-modus.sh: server on 127.0.0.1:$PORT — modus client, $NCONN+1 connections"
MODUS_SK_PORT=$PORT MODUS_SK_CONNS=$NCONN MODUS_SK_ROUNDS=$ROUNDS \
MODUS_SK_CCHUNK=$CHUNK MODUS_SK_BIG=$BIG \
    timeout 120 "$MODUS" --script "$HERE/hosted-sockets-client.lisp" > "$CLI" 2>&1
CLIRC=$?

wait "$SRVPID"
SRVRC=$?

echo "---- client ----"
grep -E "^(  ok|  FAIL|  sent|  first|  127|  [0-9]+ rounds|=== |MODUS-TO-MODUS)" "$CLI"
echo "---- server ----"
grep -E "^(  ok|  FAIL|  CPU|  total|  max handlers|  overlap|  handoffs|  log entries|HOSTED)" "$SRV"

STILL=$(ss -ltn "sport = :$PORT" 2>/dev/null | grep -c LISTEN)
echo "---- after ----"
echo "server echoed (want $TOTAL): $(sed -n 's/^  total *\([0-9]*\) bytes$/\1/p' "$SRV" | head -1)"
echo "listeners still on port $PORT: $STILL"

VERDICT=0
grep -q "SOCKET SERVER: PASS" "$SRV" || VERDICT=1
grep -q "MODUS-TO-MODUS SOCKETS: PASS" "$CLI" || VERDICT=1
[ "$STILL" = "0" ] || VERDICT=1
[ "$SRVRC" = "0" ] || VERDICT=1
[ "$CLIRC" = "0" ] || VERDICT=1

if [ $VERDICT = 0 ]; then
    echo "RUN-VERDICT OK  (server rc=$SRVRC client rc=$CLIRC listeners=$STILL)"
else
    echo "RUN-VERDICT FAIL  (server rc=$SRVRC client rc=$CLIRC listeners=$STILL)"
fi
exit $VERDICT
