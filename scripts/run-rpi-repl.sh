#!/bin/bash
# run-rpi-repl.sh — Build and run RPi REPL image in QEMU (AArch64, raspi3b)
#
# Usage:
#   ./run-rpi-repl.sh
#
# Ctrl-A X to quit QEMU

set -e
cd "$(dirname "$0")/.."

echo "Building RPi REPL image..."
sbcl --script mvm/build.lisp aarch64/bare/rpi/repl
echo "Build OK"

exec qemu-system-aarch64 \
    -machine raspi3b \
    -kernel /tmp/kernel8.img \
    -display none \
    -serial stdio \
    -semihosting
