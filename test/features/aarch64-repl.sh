#!/bin/bash
# Test: AArch64 QEMU virt — Serial REPL boots and shows prompt
TEST_NAME="aarch64-repl"
source "$(dirname "$0")/lib.sh"

echo "Building AArch64 REPL kernel..."
sbcl --script mvm/build.lisp aarch64/bare/qemu/repl > /dev/null 2>&1

boot_aarch64_virt /tmp/modus-aarch64.bin
wait_for ">"
pass
