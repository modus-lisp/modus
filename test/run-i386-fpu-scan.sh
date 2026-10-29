#!/bin/bash
# run-i386-fpu-scan.sh IMAGE CPU ARGS...
# Run an i386 image under qemu with in_asm logging and count the x87 (f*) and
# SSE (xmm) instructions it actually EXECUTES.  A soft-float image must report
# fp=0; a hard image is the positive control.  CAVEAT: qemu's log
# disassembler sometimes restarts mid-instruction (`.byte' lines) and reads an
# immediate as an opcode -- `c7 45 e8 da 00 00 00' (movl $0xda,-0x18(%ebp))
# shows up as `fiaddl'.  Check any hit's bytes before believing it.
img=$1; cpu=$2; shift 2
fasl=$(mktemp -d)
MODUS_FASL_DIR=$fasl qemu-i386-static -cpu "$cpu" -d in_asm -D /dev/stderr "$img" "$@" 2>&1 >/dev/null |
  awk -F'  +' '/^0x/ {n++; m=$3; if (m ~ /^f/ || m ~ /xmm/) {f++; if (f < 6) print "FP " $0}}
               END {print "translated-insns=" n " fp=" f+0}'
rm -rf "$fasl"
