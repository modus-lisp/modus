#!/bin/bash
# Test: AArch64 QEMU virt — Actor system boots (SSH server starts)
TEST_NAME="aarch64-actors"
source "$(dirname "$0")/lib.sh"

TIMEOUT=120

echo "Building AArch64 actors kernel..."
sbcl --script mvm/build.lisp aarch64/bare/qemu/actors > /dev/null 2>&1

boot_aarch64_virt /tmp/modus-aarch64-actors.bin \
    -device 'e1000,netdev=net0,romfile=,rombar=0' \
    -netdev "user,id=net0"
wait_for "SSH:" 120
pass
