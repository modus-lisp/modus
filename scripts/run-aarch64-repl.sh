#!/bin/bash
# run-aarch64-repl.sh — Build and run AArch64 REPL image in QEMU (virt)
#
# Usage:
#   ./scripts/run-aarch64-repl.sh              # interactive REPL
#   ./scripts/run-aarch64-repl.sh "(+ 1 2)"    # eval expression, print result, exit
#
# Ctrl-A X to quit QEMU (interactive mode)

set -e
cd "$(dirname "$0")/.."

BIN=/tmp/modus-aarch64.bin

if [ ! -f "$BIN" ] || [ mvm/build.lisp -nt "$BIN" ]; then
    echo "Building AArch64 REPL image..." >&2
    sbcl --script mvm/build.lisp aarch64/bare/qemu/repl >&2
fi

if [ -n "$1" ]; then
    exec "$(dirname "$0")/run-repl-eval.sh" aarch64 "$BIN" "$1"
fi

exec qemu-system-aarch64 \
    -machine virt \
    -cpu cortex-a57 \
    -m 512 \
    -kernel "$BIN" \
    -nographic \
    -semihosting
