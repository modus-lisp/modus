#!/bin/sh
# dos386-com.sh OUT -- assemble scripts/dos386-chain.s into MODUS.COM.
set -e
here=$(cd "$(dirname "$0")" && pwd)
out=${1:-MODUS.COM}
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
as --32 -o "$tmp/c.o" "$here/dos386-chain.s"
ld -m elf_i386 -Ttext=0x100 --oformat binary -o "$out" "$tmp/c.o"
size=$(wc -c < "$out")
if [ "$size" -gt $((0x1800 - 0x100)) ]; then
    echo "dos386-com.sh: MODUS.COM is $size bytes and reaches its data area (MBI at 1800h)" >&2
    exit 1
fi
