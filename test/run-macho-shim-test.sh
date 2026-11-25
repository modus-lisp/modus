#!/bin/sh
# run-macho-shim-test.sh -- the macOS host code's logic, tested on Linux.
#
# test/macho-shim-test.lisp compiles host/macos/shim.lisp with test/macho-shim-cases.lisp
# into a static Linux/aarch64 ELF, libSystem replaced by test doubles; qemu-aarch64 runs it.
# It checks the compiler (mvm/macho-cc.lisp) and the shim's translation code; it cannot check
# libSystem itself, which only a Mac has (host/macos/mac-test.sh).
set -eu
HERE=$(cd "$(dirname "$0")/.." && pwd)
OUT=${1:-${TMPDIR:-/tmp}/macho-shim-test.$$.elf}
QEMU=${QEMU_AARCH64:-qemu-aarch64-static}
MODUS_MACHO_TEST_OUT=$OUT sbcl --script "$HERE/test/macho-shim-test.lisp"
chmod 755 "$OUT"
set +e
"$QEMU" "$OUT"
rc=$?
set -e
[ $# -ge 1 ] || rm -f "$OUT"
echo "run-macho-shim-test: exit $rc"
exit $rc
