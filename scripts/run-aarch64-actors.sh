#!/bin/bash
# Run AArch64 Actors+SSH kernel on QEMU virt
# Build first: sbcl --script mvm/build.lisp aarch64/bare/qemu/actors

qemu-system-aarch64 -machine virt -cpu cortex-a57 -m 512 \
    -kernel /tmp/modus-aarch64-actors.bin -nographic -semihosting \
    -device 'e1000,netdev=net0,romfile=,rombar=0' \
    -netdev 'user,id=net0,hostfwd=tcp::2222-:22'
