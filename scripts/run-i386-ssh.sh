#!/bin/bash
# run-i386-ssh.sh — Build and run i386 SSH server in QEMU (NE2000 ISA NIC)
# Connect with: ssh -p $PORT -o StrictHostKeyChecking=no test@localhost

set -e
cd "$(dirname "$0")/.."

PORT=${1:-2222}
EXPR="${2:-}"
LOGFILE="/tmp/modus-i386-ssh.log"
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
echo "Building i386 SSH kernel..."
sbcl --script mvm/build.lisp i386/bare/qemu/ssh
echo "Build complete."

pkill -9 -f 'qemu-system-i386.*modus-i386-ssh' 2>/dev/null || true
sleep 0.5

# Start QEMU in background
> "$LOGFILE"
qemu-system-i386 \
    -m 256 \
    -nographic \
    -no-reboot \
    -kernel /tmp/modus-i386-ssh.bin \
    -device ne2k_isa,netdev=net0,iobase=0x300,irq=9 \
    -netdev "user,id=net0,hostfwd=tcp::${PORT}-:22" \
    > "$LOGFILE" 2>&1 &
QEMU_PID=$!

# Wait for DHCP + SSH to be ready
echo "Booting..."
TRIES=0
while [ $TRIES -lt 120 ]; do
    if ! kill -0 "$QEMU_PID" 2>/dev/null; then
        echo "QEMU exited unexpectedly. Log:"
        cat "$LOGFILE"
        exit 1
    fi
    if grep -q "DHCP:IP=" "$LOGFILE" 2>/dev/null; then
        break
    fi
    sleep 0.5
    TRIES=$((TRIES + 1))
done

if ! grep -q "DHCP:IP=" "$LOGFILE" 2>/dev/null; then
    echo "Timed out waiting for DHCP. Log:"
    cat "$LOGFILE"
    cleanup
    exit 1
fi

if [ -n "$EXPR" ]; then
    sleep 5
    ssh -o StrictHostKeyChecking=no -o ConnectTimeout=30 -p "$PORT" test@localhost "$EXPR" 2>/dev/null || true
    cleanup
else
    echo ""
    echo "Modus i386 SSH server ready on port $PORT."
    echo "  ssh -p $PORT -o StrictHostKeyChecking=no test@localhost"
    echo ""
    echo "Press Ctrl-C to stop."

    # Wait for QEMU to exit or Ctrl-C
    wait "$QEMU_PID" 2>/dev/null
    QEMU_PID=""
fi
