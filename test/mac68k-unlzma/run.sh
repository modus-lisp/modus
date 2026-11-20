#!/bin/sh
# run.sh [IMAGE] -- check scripts/mac68k-lzma.inc (the Mac floppy's LZMA decoder)
# against xz's encoder under qemu-m68k-static (68040): edge cases, random data,
# text, and IMAGE if given (a Mac image, the thing the floppy carries).
# (cram's encoder, which the floppy uses, was checked the same way.)  Then
# PRESET DICTIONARIES, from cram's encoder (dict.lisp; liblzma's decoder reads
# its streams too): text against earlier text, and with IMAGE, 64 KB blocks of
# its second half against its first -- the floppy driver's cold-code blocks.
set -e
here=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
m68k-linux-gnu-as -m68020 -I "$here/../../scripts" -o "$tmp/h.o" "$here/harness.s"
m68k-linux-gnu-ld -o "$tmp/harness" "$tmp/h.o"
printf a > "$tmp/one"
head -c 100000 /dev/zero > "$tmp/zeros"
head -c 300000 /dev/urandom > "$tmp/rand"
seq 1 200000 > "$tmp/text"
image=${1:-}
set -- one zeros rand text ${1:+"$1"}
fail=0
for f; do
  case $f in /*) src=$f ;; *) src=$tmp/$f ;; esac
  xz --format=raw --lzma1=preset=9e,lc=2,lp=1,pb=2,dict=8MiB -c "$src" > "$tmp/s"
  n=$(stat -c %s "$src")
  if qemu-m68k-static -cpu m68040 "$tmp/harness" "$n" < "$tmp/s" | cmp -s - "$src"
  then echo "ok   $f ($n bytes)"; else echo "FAIL $f ($n bytes)"; fail=1; fi
done
# with a dictionary: the harness reads DICT then the stream; DICTEND is the
# dictionary's end in the input buffer, far from the output
dict() {  # NAME DICTFILE INPUTFILE
  sbcl --script "$here/dict.lisp" "$2" "$3" "$tmp/ds" > /dev/null 2>&1
  n=$(stat -c %s "$3"); dn=$(stat -c %s "$2")
  if cat "$2" "$tmp/ds" | qemu-m68k-static -cpu m68040 "$tmp/harness" "$n" "$dn" | cmp -s - "$3"
  then echo "ok   $1 ($n bytes, $(stat -c %s "$tmp/ds") packed, dictionary $dn)"
  else echo "FAIL $1"; fail=1; fi
}
head -c 700001 "$tmp/text" > "$tmp/d1"; tail -c +700002 "$tmp/text" | head -c 200000 > "$tmp/i1"
dict "text, odd dictionary" "$tmp/d1" "$tmp/i1"
printf 'aaaa' > "$tmp/d2"; head -c 5000 /dev/zero | tr '\0' a > "$tmp/i2"
dict "a first byte that is a rep into it" "$tmp/d2" "$tmp/i2"
if [ -n "$image" ]; then
  half=$(( $(stat -c %s "$image") / 2 / 4 * 4 ))
  head -c $half "$image" > "$tmp/d3"
  for k in 0 7 31; do
    tail -c +$((half + 1 + k * 65536)) "$image" | head -c 65536 > "$tmp/i3"
    [ -s "$tmp/i3" ] && dict "IMAGE block $k against its first half" "$tmp/d3" "$tmp/i3"
  done
fi
exit $fail
