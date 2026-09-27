#!/bin/sh
# build-macos.sh IMAGE.elf OUT — wrap a Darwin-layout modus image in a native,
# signed macOS executable (docs/macos-hosting.md, M0).
#
# Build the image first, e.g.:
#   MODUS_DARWIN=1 MODUS_NO_JIT=1 MODUS_CODE_BASE=7000000000 \
#   MODUS_CONV_DELTA=7040000000 MODUS_HEAP_BASE=7080000000 \
#   MODUS_JIT_ARENA_BASE=70C0000000 MODUS_CLI_OUT=/tmp/modus-darwin.elf \
#   sbcl --dynamic-space-size 16384 --script mvm/build-aarch64-cli.lisp
set -eu
IMAGE=$1
OUT=$2
HERE=$(cd "$(dirname "$0")" && pwd)
# The image goes into __TEXT (r-x, covered by the code signature) on a 16 KB
# boundary, so the shim can mach_vm_remap it page for page.  ld signs arm64
# executables ad hoc.
cc -O2 -Wall -o "$OUT" "$HERE/modus-shim.c" "$HERE/syscall-stub.S" \
   -Wl,-sectcreate,__TEXT,__modus,"$IMAGE" \
   -Wl,-sectalign,__TEXT,__modus,0x4000
codesign --force --sign - "$OUT" >/dev/null 2>&1 || true
echo "wrote $OUT ($(wc -c < "$OUT" | tr -d ' ') bytes)"
