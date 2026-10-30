#!/bin/bash
# run-68k-fpu-scan.sh IMAGE ARGS...
# Run a 68k image under qemu with in_asm logging and count the FPU
# instructions it actually EXECUTES (every translated block is logged; m68k
# mnemonics starting with `f' are FPU ops).  qemu 7.2 has no FPU-less
# 68020+ CPU model, so this is how a soft-float image is checked dynamically.
# A soft-float image must report fpu=0; a hard image is the positive control.
img=$1; shift
fasl=$(mktemp -d)
MODUS_FASL_DIR=$fasl qemu-m68k-static -cpu m68040 -d in_asm -D /dev/stderr "$img" "$@" \
  2> >(awk '/^0x/ {n++; if ($2 ~ /^f/) {f++; if (f < 4) print "FPU " $0}}
            END {print "translated-insns=" n " fpu=" f+0}' >&2)
rc=$?
rm -rf "$fasl"
exit $rc
