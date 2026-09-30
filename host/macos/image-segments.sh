#!/bin/sh
# image-segments.sh IMAGE.elf SCRATCH-DIR — print the linker flags that put a
# PC-relative (MODUS_PCREL=1) image in its own Mach-O segments, where it runs
# IN PLACE (docs/macos-hosting.md, "Running in place"):
#
#   __MODUSS  rw-  16 KB   the syscall slot, one page below the code base
#   __MODUS   r-x          the image, at its link address
#   __MODUSB  rw-          the ELF's BSS tail (p_memsz past p_filesz)
#   __MODUSR, __MODUSH, __MODUSA  rw-, ZERO-FILL: the runtime-data region, the
#             heap and the JIT arena, RESERVED at their link addresses when the
#             image names them (MODUS-LAYOUT-* symbols, boot-linux-aarch64
#             LINUX-AARCH64-LAYOUT-SYMS).  Nothing else can land there first;
#             the image's boot stub maps over them in place (modus-shim.c).
#
# The loader slides all three with the rest of the executable, by the same
# amount, so the distances the image's ADRP+ADD sites encode hold.  Writes the
# two zero-filled section files into SCRATCH-DIR.
set -eu
IMAGE=$1
DIR=$2
u64() { od -An -t u8 -j "$1" -N 8 "$IMAGE" | tr -d ' '; }
PHOFF=$(u64 32)
VADDR=$(u64 $((PHOFF + 16)))
FILESZ=$(u64 $((PHOFF + 32)))
MEMSZ=$(u64 $((PHOFF + 40)))
P=16384
round() { echo $(( ($1 + P - 1) / P * P )); }
FILESPAN=$(round "$FILESZ")
TAIL=$(( $(round "$MEMSZ") - FILESPAN ))
[ $((VADDR % P)) -eq 0 ] || { echo "image-segments: p_vaddr is not 16 KB aligned" >&2; exit 1; }
mkdir -p "$DIR"
# Only what is loaded: the ELF's section headers and symbol table follow
# p_filesz in the file, and would push __MODUS into the BSS tail's place.
head -c "$FILESZ" "$IMAGE" > "$DIR/image.bin"
# The data layout, if the image names it: zero-fill segments cost no file
# bytes, only address space, and slide with everything else.
sym() { xcrun nm "$IMAGE" 2>/dev/null | awk -v n="$1" '$3 == n { print $1 }'; }
# EVERY segment comes from one assembly file, in ASCENDING ADDRESS ORDER: ld
# lays segments out in the order it first meets them and refuses one that is
# out of order, and -sectcreate segments always come after an object's.
S="$DIR/segments.s"
{
  echo ".section __MODUSS,__slot"
  echo ".space $P"
  echo ".section __MODUS,__image"
  echo ".incbin \"$DIR/image.bin\""
  [ "$TAIL" -gt 0 ] && echo ".zerofill __MODUSB,__bss,_modus_image_bss,$TAIL,14"
} > "$S"
FLAGS="-Wl,-segprot,__MODUSS,rw,rw -Wl,-segaddr,__MODUSS,$(printf '%#x' $((VADDR - P)))"
FLAGS="$FLAGS -Wl,-segprot,__MODUS,rx,rx -Wl,-segaddr,__MODUS,$(printf '%#x' "$VADDR")"
[ "$TAIL" -gt 0 ] &&
  FLAGS="$FLAGS -Wl,-segprot,__MODUSB,rw,rw -Wl,-segaddr,__MODUSB,$(printf '%#x' $((VADDR + FILESPAN)))"
if [ -n "$(sym MODUS-LAYOUT-REGION-LO)" ]; then
  for part in REGION:__MODUSR HEAP:__MODUSH ARENA:__MODUSA; do
    name=${part%%:*}; seg=${part##*:}
    lo=$((0x$(sym MODUS-LAYOUT-$name-LO))); hi=$((0x$(sym MODUS-LAYOUT-$name-HI)))
    [ $((lo % P)) -eq 0 ] && [ $((hi % P)) -eq 0 ] ||
      { echo "image-segments: $name is not 16 KB aligned" >&2; exit 1; }
    echo ".zerofill $seg,__reserve,_modus_reserve_$name,$((hi - lo)),14" >> "$S"
    FLAGS="$FLAGS -Wl,-segprot,$seg,rw,rw -Wl,-segaddr,$seg,$(printf '%#x' "$lo")"
  done
fi
echo "$FLAGS $S"
