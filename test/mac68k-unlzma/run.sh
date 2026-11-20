#!/bin/sh
# run.sh [IMAGE] -- check scripts/mac68k-lzma.inc (the Mac floppy's LZMA decoder)
# against xz's encoder under qemu-m68k-static (68040): edge cases, random data,
# text, and IMAGE if given (a Mac image, the thing the floppy carries).
# (cram's encoder, which the floppy uses, was checked the same way.)
set -e
here=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
m68k-linux-gnu-as -m68020 -I "$here/../../scripts" -o "$tmp/h.o" "$here/harness.s"
m68k-linux-gnu-ld -o "$tmp/harness" "$tmp/h.o"
printf a > "$tmp/one"
head -c 100000 /dev/zero > "$tmp/zeros"
head -c 300000 /dev/urandom > "$tmp/rand"
seq 1 200000 > "$tmp/text"
set -- one zeros rand text ${1:+"$1"}
fail=0
for f; do
  case $f in /*) src=$f ;; *) src=$tmp/$f ;; esac
  xz --format=raw --lzma1=preset=9e,lc=2,lp=1,pb=2,dict=8MiB -c "$src" > "$tmp/s"
  n=$(stat -c %s "$src")
  if qemu-m68k-static -cpu m68040 "$tmp/harness" "$n" < "$tmp/s" | cmp -s - "$src"
  then echo "ok   $f ($n bytes)"; else echo "FAIL $f ($n bytes)"; fail=1; fi
done
exit $fail
