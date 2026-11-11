#!/bin/bash
# x64-kernels.sh -- assemble mvm/x64-kernels/*.s with GNU as and print the Lisp byte tables
# translate-x64.lisp embeds (*X64-KERNEL-AES-CTR* ...).  The build does not run an assembler;
# rerun this after editing a .s and paste its output over the tables (it also checks that
# every branch in a kernel stays inside it, since the bytes are copied in place).
set -e
here="$(cd "$(dirname "$0")/.." && pwd)/mvm/x64-kernels"
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
for k in aes-ctr ghash cpuid; do
  as "$here/$k.s" -o "$tmp/$k.o"
  if objdump -dr "$tmp/$k.o" | grep -q 'R_X86_64'; then echo "$k: has relocations" >&2; exit 1; fi
  objcopy -O binary -j .text "$tmp/$k.o" "$tmp/$k.bin"
  name=$k
  printf '(defparameter *x64-kernel-%s*\n  ;; %s bytes, from mvm/x64-kernels/%s.s (scripts/x64-kernels.sh)\n  #(' \
    "$name" "$(stat -c %s "$tmp/$k.bin")" "$k"
  od -An -v -tx1 "$tmp/$k.bin" | tr -s ' \n' ' ' | sed 's/^ //; s/ $//' | \
    awk '{for(i=1;i<=NF;i++){printf "#x%s%s", toupper($i), (i<NF? (i%16==0?"\n    ":" ") : "")}}'
  printf '))\n'
done
