#!/bin/bash
# Test: AArch64 QEMU virt — Qubes-like actor isolation boots
TEST_NAME="aarch64-isolated"
source "$(dirname "$0")/lib.sh"

TIMEOUT=120

echo "Building AArch64 isolated kernel..."
sbcl --script mvm/build.lisp aarch64/bare/qemu/isolated > /dev/null 2>&1

boot_aarch64_virt /tmp/modus-aarch64-isolated.bin \
    -device 'e1000,netdev=net0,romfile=,rombar=0' \
    -netdev "user,id=net0"
wait_for "SSH:" 120
pass
