#!/bin/bash
# run-aarch64-ssh.sh — Build and run AArch64 SSH server in QEMU (virt + E1000)
# The kernel auto-starts networking + SSH on AArch64.
# Connect with: ssh -p $PORT -o StrictHostKeyChecking=no test@localhost

set -e
cd "$(dirname "$0")/.."

PORT=${1:-2222}
EXPR="${2:-}"
LOGFILE="/tmp/modus-aarch64-ssh.log"
QEMU_PID=""

cleanup() {
    if [ -n "$QEMU_PID" ] && kill -0 "$QEMU_PID" 2>/dev/null; then
        kill "$QEMU_PID" 2>/dev/null
        wait "$QEMU_PID" 2>/dev/null
    fi
    exit 0
}
trap cleanup INT TERM

# Build kernel
echo "Building AArch64 SSH kernel..."
sbcl --script mvm/build.lisp aarch64/bare/qemu/ssh
echo "Build complete."

pkill -9 -f 'qemu-system-aarch64.*modus-aarch64-ssh' 2>/dev/null || true
sleep 0.5

# Start QEMU in background
> "$LOGFILE"
qemu-system-aarch64 \
    -machine virt \
    -cpu cortex-a57 \
    -m 512 \
    -kernel /tmp/modus-aarch64-ssh.bin \
    -nographic \
    -semihosting \
    -device 'e1000,netdev=net0,romfile=,rombar=0' \
    -netdev "user,id=net0,hostfwd=tcp::${PORT}-:22" \
    > "$LOGFILE" 2>&1 &
QEMU_PID=$!

# Wait for SSH server to be ready
echo "Booting..."
TRIES=0
while [ $TRIES -lt 120 ]; do
    if ! kill -0 "$QEMU_PID" 2>/dev/null; then
        echo "QEMU exited unexpectedly. Log:"
        cat "$LOGFILE"
        exit 1
    fi
    if grep -q "SSH:" "$LOGFILE" 2>/dev/null; then
        break
    fi
    sleep 0.5
    TRIES=$((TRIES + 1))
done

if ! grep -q "SSH:" "$LOGFILE" 2>/dev/null; then
    echo "Timed out waiting for SSH server. Log:"
    cat "$LOGFILE"
    cleanup
    exit 1
fi

if [ -n "$EXPR" ]; then
    # Batch mode: run expression, print result, exit
    sleep 5
    ssh -o StrictHostKeyChecking=no -o ConnectTimeout=30 -p "$PORT" test@localhost "$EXPR" 2>/dev/null || true
    cleanup
else
    echo ""
    echo "Modus AArch64 SSH server ready on port $PORT."
    echo "  ssh -p $PORT -o StrictHostKeyChecking=no test@localhost"
    echo ""
    echo "Press Ctrl-C to stop."

    # Wait for QEMU to exit or Ctrl-C
    wait "$QEMU_PID" 2>/dev/null
    QEMU_PID=""
fi
