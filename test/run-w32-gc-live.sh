#!/bin/bash
# test/run-w32-gc-live.sh -- the W32 collector rung and its negative control
#
#   test/run-w32-gc-live.sh IMGDIR
#
# IMGDIR holds, for each of riscv32 arm32 i386, two CLI images built with
#   MODUS_EXTRA_SOURCE=test/w32-gc-live.lisp
#   ARCH-safe    plain
#   ARCH-unsafe  + MODUS_W32_UNSAFE_LIVE=1 (raw words NOT split around GC points)
# (build-riscv32-cli / build-arm32-cli take MODUS_CLI_OUT, build-i386-cli
# MODUS_I386_OUT).  Each safe image must answer 15 and each unsafe one 11 --
# everything but bit 4, "the raw word read back unchanged".  The control must
# FAIL FOR THE RIGHT REASON: 11, not a crash or another number.  Exit 0 iff
# all six answer as they must.
set -u
IMG=${1:?usage: run-w32-gc-live.sh IMGDIR}
declare -A QEMU=([riscv32]=qemu-riscv32-static [arm32]=qemu-arm-static [i386]=qemu-i386-static)
fail=0
for a in riscv32 arm32 i386; do
  for k in safe unsafe; do
    want=$([ $k = safe ] && echo 15 || echo 11)
    f="$IMG/$a-$k"
    chmod +x "$f" 2>/dev/null   # qemu-user rejects a non-executable file silently
    out=$(timeout 900 "${QEMU[$a]}" "$f" --eval '(modus-int::w32-gc-live)' --quit 2>&1)
    got=$(printf '%s\n' "$out" | sed -n 's/^w32-gc-live \([0-9]*\).*/\1/p' | tail -1)
    if [ "$got" = "$want" ]; then
      echo "$a $k: $got (want $want) ok"
    else
      echo "$a $k: '${got:-none}' (want $want) FAIL"; printf '%s\n' "$out" | tail -5; fail=1
    fi
  done
done
exit $fail
