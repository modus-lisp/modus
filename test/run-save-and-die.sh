#!/bin/bash
# run-save-and-die.sh — A CORE RESTORES WITH ITS JIT CODE (lib/save-image.lisp).
#
#   test/run-save-and-die.sh [BINARY]
#
# Phase 1 JITs two functions and snapshots; phase 2 restores with --core and
# checks the functions are still native, the data intact, and a new function
# JITs from the arena.  PASS iff both phases exit 0.
set -u
BIN=${1:-./modus}
cd "$(dirname "$0")/.." || exit 1
if [ ! -x "$BIN" ]; then echo "no such binary: $BIN" >&2; exit 1; fi
CORE=$(mktemp -t sad.XXXXXX.core)
trap 'rm -f "$CORE"' EXIT
timeout 120 "$BIN" --eval "(defvar *core* \"$CORE\")" --script test/save-and-die.lisp > /dev/null 2>&1
rc=$?
if [ "$rc" -ne 0 ] || [ ! -s "$CORE" ]; then echo "SAVE-AND-DIE: FAIL (save status $rc)"; exit 1; fi
out=$(timeout 60 "$BIN" --core "$CORE" --script test/save-and-die-restore.lisp 2>/dev/null)
rc=$?
if [ "$rc" -eq 0 ]; then echo "SAVE-AND-DIE: PASS ($(stat -c %s "$CORE") byte core)"; exit 0; fi
echo "SAVE-AND-DIE: FAIL (restore status $rc: $(echo "$out" | grep SAD | tail -1))"
exit 1
