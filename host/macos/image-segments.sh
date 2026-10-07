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
#
# MODUS_CORE=FILE (a save-and-die snapshot of this image, lib/save-image.lisp):
# the snapshot's JIT CODE becomes the first part of the arena, as
#   __MODUSC  r-x  the arena's code pages, byte for byte, at the arena base
# and __MODUSA reserves only the rest.  That code is position-independent for
# the layout, so signed and read-only it runs wherever the loader slides it --
# which is how an iOS app, which can never write code, carries native code
# compiled on the Mac.  The restore finds those bytes already in place.
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
    if [ "$name" = ARENA ] && [ -n "${MODUS_CORE:-}" ]; then
      # The core's header words are stored as fixnums (value << 1).
      cw() { echo $(( $(od -An -t u8 -j "$1" -N 8 "$MODUS_CORE" | tr -d ' ') / 2 )); }
      FROM=$(cw 8); FREE=$(cw 32); BLEN=$(cw 48); ALO=$(cw 64); ABUMP=$(cw 72); CONV=$(cw 80)
      # The saving process ran at some slide: its region base (0x10000000 in
      # the image's terms) was CONV, and the region's link base is 16 MB
      # above REGION-LO.  The code itself is position-independent.
      SLIDE=$((CONV - 0x$(sym MODUS-LAYOUT-REGION-LO) - 0x1000000))
      [ "$ALO" -eq $((lo + SLIDE)) ] || { echo "image-segments: the core's arena is not this image's" >&2; exit 1; }
      CODE=$((ABUMP - ALO))
      if [ "$CODE" -gt 0 ]; then
        OFF=$((128 + 4096 + FREE - FROM + 2 * BLEN))
        tail -c +$((OFF + 1)) "$MODUS_CORE" | head -c "$CODE" > "$DIR/arena-code.bin"
        CSPAN=$(round "$CODE")
        echo ".section __MODUSC,__code" >> "$S"
        echo ".incbin \"$DIR/arena-code.bin\"" >> "$S"
        [ "$CSPAN" -gt "$CODE" ] && echo ".space $((CSPAN - CODE))" >> "$S"
        FLAGS="$FLAGS -Wl,-segprot,__MODUSC,rx,rx -Wl,-segaddr,__MODUSC,$(printf '%#x' "$lo")"
        lo=$((lo + CSPAN))
      fi
    fi
    echo ".zerofill $seg,__reserve,_modus_reserve_$name,$((hi - lo)),14" >> "$S"
    FLAGS="$FLAGS -Wl,-segprot,$seg,rw,rw -Wl,-segaddr,$seg,$(printf '%#x' "$lo")"
  done
fi
echo "$FLAGS $S"
