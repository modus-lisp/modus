#!/bin/sh
# uefi-disk.sh EFI CORE OUT [SECTORS] -- a GPT disk that boots a bare x64 UEFI image: one FAT32
# EFI System Partition (120 MB, at sector 2048) holding \EFI\BOOT\BOOTX64.EFI and, when CORE is
# not empty, \CORE.BIN (boot/boot-uefi-x64.lisp's stub reads it to 0x20000000 and the image
# restores it).  SECTORS (default: the partition's end) sizes the disk, so a region after the
# partition can be kept for a desk's log and command channel (vessel src/t420.lisp).  Every file
# is read back and compared.  make-uefi-usb.sh's recipe; needs sfdisk and mtools.
set -e
EFI=$1 CORE=$2 OUT=$3 SECTORS=${4:-0}
O=1048576
MB=128
if [ "$SECTORS" -gt 0 ]; then
  MB=$(( (SECTORS * 512 + 1048575) / 1048576 ))
  [ "$MB" -ge 128 ] || MB=128
fi
rm -f "$OUT"
dd if=/dev/zero of="$OUT" bs=1M count=0 seek="$MB" status=none
printf 'label: gpt\ntype=C12A7328-F81F-11D2-BA4B-00A0C93EC93B, size=120M\n' | /sbin/sfdisk "$OUT" >/dev/null 2>&1
mformat -i "$OUT@@$O" -F ::
mmd -i "$OUT@@$O" ::/EFI
mmd -i "$OUT@@$O" ::/EFI/BOOT
mcopy -i "$OUT@@$O" "$EFI" ::/EFI/BOOT/BOOTX64.EFI
[ -z "$CORE" ] || mcopy -i "$OUT@@$O" "$CORE" ::/CORE.BIN
T=$(mktemp)
mcopy -o -i "$OUT@@$O" ::/EFI/BOOT/BOOTX64.EFI "$T" && cmp "$T" "$EFI"
[ -z "$CORE" ] || { mcopy -o -i "$OUT@@$O" ::/CORE.BIN "$T" && cmp "$T" "$CORE"; }
rm -f "$T"
echo "uefi-disk: $OUT (${MB} MB${CORE:+, with a core})"
