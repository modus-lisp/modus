#!/bin/sh
# image-segments.sh IMAGE.elf SCRATCH-DIR — print the linker flags that put a
# PC-relative (MODUS_PCREL=1) image in its own Mach-O segments, where it runs
# IN PLACE (docs/macos-hosting.md, "Running in place"):
#
#   __MODUSS  rw-  16 KB   the syscall slot, one page below the code base
#   __MODUS   r-x          the image, at its link address
#   __MODUSB  rw-          the ELF's BSS tail (p_memsz past p_filesz)
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
head -c $P /dev/zero > "$DIR/slot.bin"
FLAGS="-Wl,-sectcreate,__MODUSS,__slot,$DIR/slot.bin -Wl,-segprot,__MODUSS,rw,rw"
FLAGS="$FLAGS -Wl,-segaddr,__MODUSS,$(printf '%#x' $((VADDR - P)))"
FLAGS="$FLAGS -Wl,-sectcreate,__MODUS,__image,$DIR/image.bin -Wl,-segprot,__MODUS,rx,rx"
FLAGS="$FLAGS -Wl,-segaddr,__MODUS,$(printf '%#x' "$VADDR")"
if [ "$TAIL" -gt 0 ]; then
  head -c "$TAIL" /dev/zero > "$DIR/bss.bin"
  FLAGS="$FLAGS -Wl,-sectcreate,__MODUSB,__bss,$DIR/bss.bin -Wl,-segprot,__MODUSB,rw,rw"
  FLAGS="$FLAGS -Wl,-segaddr,__MODUSB,$(printf '%#x' $((VADDR + FILESPAN)))"
fi
echo "$FLAGS"
