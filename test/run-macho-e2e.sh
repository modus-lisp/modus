#!/bin/sh
# run-macho-e2e.sh IMAGE [WORKDIR] -- the macOS CLI's host code running the REAL
# Darwin-layout image, on Linux, under qemu-aarch64.
#
# test/macho-shim-test.lisp links host/macos/shim.lisp with test/macho-e2e-doubles.lisp
# (libSystem played by Linux syscalls, Darwin's layouts and numbering on the call side) and
# embeds IMAGE as __TEXT,__modus would hold it.  qemu runs it with a guest base (-B) so the
# doubles can map page 0 as the main thread's TSD: the image reads its per-thread delta at
# TPIDRRO_EL0 + 2400, and TPIDRRO_EL0 is 0 in Linux user space.  Then it evaluates a form
# and runs test/macho-e2e-script.lisp (files, directories, clocks, the collector, a caught
# fault through the signal path, the JIT, a loopback socket).  Threads are not covered.
set -eu
IMAGE=$1
WORK=${2:-${TMPDIR:-/tmp}/macho-e2e.$$}
HERE=$(cd "$(dirname "$0")/.." && pwd)
QEMU=${QEMU_AARCH64:-qemu-aarch64-static}
mkdir -p "$WORK"
MODUS_MACHO_E2E_IMAGE=$IMAGE MODUS_MACHO_TEST_OUT=$WORK/e2e.elf sbcl --script "$HERE/test/macho-shim-test.lisp"
chmod 755 "$WORK/e2e.elf"          # qemu-user refuses a non-executable file silently (exit 1)
out=$(cd "$WORK" && timeout 600 "$QEMU" -B 0x1000000000 ./e2e.elf --eval '(print (+ 1 2))' --quit </dev/null)
echo "eval: $out"
[ "$(echo $out)" = 3 ] || { echo "run-macho-e2e: FAIL eval"; exit 1; }
(cd "$WORK" && MODUS_TEST_DIR=$WORK timeout 1200 "$QEMU" -B 0x1000000000 ./e2e.elf \
   --script "$HERE/test/macho-e2e-script.lisp" </dev/null) | tee "$WORK/script.out"
grep -q "checks, 0 failed" "$WORK/script.out" || { echo "run-macho-e2e: FAIL script"; exit 1; }
echo "run-macho-e2e: PASS"
