#!/bin/bash
# test/run-riscv32-ddc.sh -- the RV32 DDC (W32 M1, docs/w32.md).
#
#   test/run-riscv32-ddc.sh UEFI-DDC-WORKDIR
#
# The third fast DDC, beside test/run-aarch64-ddc.sh and with its shape: the
# same dumped source (run-uefi-ddc.sh's full-source.lisp) compiled for
# Linux/RISC-V 32 by SBCL (test/ddc-riscv32-host.lisp) and by the self-hosted
# x64 compiler (modus-sh --compile-riscv32), and the two ELFs compared byte
# for byte.  The other two DDCs compile for a 64-bit little-endian target --
# the self-hosted compiler's own shape -- so a decision it takes from ITS OWN
# widths instead of the compilation's is invisible to them.  This one sees it:
# the target's fixnums are 30 bits.
set -uo pipefail
cd "$(dirname "$0")/.."
W=${1:?usage: run-riscv32-ddc.sh UEFI-DDC-WORKDIR}; W=$(cd "$W" && pwd)
DSS=${DSS:-12288}
say() { echo "[rv32-ddc] $(date +%H:%M:%S) $*"; }
for f in full-source.lisp modus-sh; do
  [ -s "$W/$f" ] || { say "FAIL: no $W/$f (run test/run-uefi-ddc.sh first)"; exit 1; }
done
t0=$(date +%s)
sbcl --dynamic-space-size "$DSS" --script test/ddc-riscv32-host.lisp \
     "$W/full-source.lisp" "$W/sbcl-rv32.elf" > "$W/rv32-sbcl.log" 2>&1 & p1=$!
( cd "$W" && ./modus-sh --compile-riscv32 full-source.lisp sh-rv32.elf ) > "$W/rv32-sh.log" 2>&1 & p2=$!
wait $p1; r1=$?; wait $p2; r2=$?
[ $r1 = 0 ] && [ -s "$W/sbcl-rv32.elf" ] || { say "FAIL: SBCL side rc=$r1 (see $W/rv32-sbcl.log)"; exit 1; }
[ $r2 = 0 ] && [ -s "$W/sh-rv32.elf" ] || { say "FAIL: modus-sh side rc=$r2 (see $W/rv32-sh.log)"; exit 1; }
md5sum "$W/sbcl-rv32.elf" "$W/sh-rv32.elf"
say "done in $(( $(date +%s) - t0 )) s"
if cmp -s "$W/sbcl-rv32.elf" "$W/sh-rv32.elf"; then
  say "ok: FIXPOINT — SBCL and modus-sh emit the same RV32 image"; say "PASS"; exit 0
fi
say "DIFF: $(cmp "$W/sbcl-rv32.elf" "$W/sh-rv32.elf" | head -1); sizes $(stat -c %s "$W/sbcl-rv32.elf") / $(stat -c %s "$W/sh-rv32.elf")"
say "FAIL"; exit 1
