#!/bin/bash
# Test: RPi 3B QEMU — Serial REPL boots and shows prompt
TEST_NAME="rpi-repl"
source "$(dirname "$0")/lib.sh"

echo "Building RPi REPL kernel..."
sbcl --script mvm/build.lisp aarch64/bare/rpi/repl > /dev/null 2>&1

boot_rpi /tmp/kernel8.img
wait_for ">"
pass
