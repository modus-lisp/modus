#!/bin/bash
# run-x64-ssh.sh — Build and run Modus x64 SSH server via MVM
# Connect with: ssh -p $PORT -o StrictHostKeyChecking=no test@localhost

set -e
cd "$(dirname "$0")/.."

PORT=${1:-2222}
EXPR="${2:-}"
LOGFILE="/tmp/modus-ssh.log"
BIN="/tmp/modus-x64-ssh.bin"
QEMU_PID=""

cleanup() {
    if [ -n "$QEMU_PID" ] && kill -0 "$QEMU_PID" 2>/dev/null; then
        kill "$QEMU_PID" 2>/dev/null
        wait "$QEMU_PID" 2>/dev/null
    fi
    exit 0
}
trap cleanup INT TERM

# Build kernel via MVM pipeline
if [ ! -f "$BIN" ] || [ mvm/build.lisp -nt "$BIN" ]; then
    echo "Building x64 SSH kernel..." >&2
    sbcl --script mvm/build.lisp x64/bare/qemu/ssh >&2
fi

pkill -9 -f "qemu.*modus-x64-ssh" 2>/dev/null || true
sleep 0.3

# Start QEMU (disable THP to avoid compaction hangs)
SCRIPTDIR="$(cd "$(dirname "$0")" && pwd)"
NO_THP=""
[ -x "$SCRIPTDIR/no-thp-exec" ] && NO_THP="$SCRIPTDIR/no-thp-exec"

> "$LOGFILE"
$NO_THP qemu-system-x86_64 \
    -kernel "$BIN" -m 512 \
    -nographic -no-reboot \
    -device 'e1000,netdev=net0,romfile=,rombar=0' \
    -netdev "user,id=net0,hostfwd=tcp::${PORT}-:22" \
    > "$LOGFILE" 2>&1 &
QEMU_PID=$!

# Wait for SSH server to be ready
echo "Booting..."
TRIES=0
while [ $TRIES -lt 120 ]; do
    if ! kill -0 "$QEMU_PID" 2>/dev/null; then
        echo "QEMU exited. Log:" >&2
        cat "$LOGFILE" >&2
        exit 1
    fi
    if grep -q "SSH:" "$LOGFILE" 2>/dev/null; then
        break
    fi
    sleep 0.5
    TRIES=$((TRIES + 1))
done

if ! grep -q "SSH:" "$LOGFILE" 2>/dev/null; then
    echo "Timed out waiting for SSH. Log:" >&2
    cat "$LOGFILE" >&2
    cleanup
    exit 1
fi

if [ -n "$EXPR" ]; then
    sleep 5
    ssh -o StrictHostKeyChecking=no -o ConnectTimeout=30 -p "$PORT" test@localhost "$EXPR" 2>/dev/null || true
    cleanup
else
    echo ""
    echo "Modus SSH server ready on port $PORT."
    echo "  ssh -p $PORT -o StrictHostKeyChecking=no test@localhost"
    echo ""
    echo "Press Ctrl-C to stop."

    wait "$QEMU_PID" 2>/dev/null
    QEMU_PID=""
fi
