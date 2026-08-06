#!/bin/bash
# run-x64-repl.sh — Build and run Modus x64 serial REPL via MVM
#
# Usage:
#   ./scripts/run-x64-repl.sh              # interactive REPL
#   ./scripts/run-x64-repl.sh "(+ 1 2)"    # eval expression, print result, exit
#
# Ctrl-A X to quit QEMU (interactive mode)

set -e
cd "$(dirname "$0")/.."

BIN=/tmp/modus-x64.bin

if [ ! -f "$BIN" ] || [ mvm/build.lisp -nt "$BIN" ]; then
    echo "Building x64 REPL..." >&2
    sbcl --script mvm/build.lisp x64/bare/qemu/repl >&2
fi

if [ -n "$1" ]; then
    exec "$(dirname "$0")/run-repl-eval.sh" x64 "$BIN" "$1"
fi

SCRIPTDIR="$(cd "$(dirname "$0")" && pwd)"
NO_THP=""
[ -x "$SCRIPTDIR/no-thp-exec" ] && NO_THP="$SCRIPTDIR/no-thp-exec"

exec $NO_THP qemu-system-x86_64 \
    -kernel "$BIN" -m 512 \
    -nographic \
    -no-reboot
