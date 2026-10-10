#!/bin/bash
# make-raw-uefi-disk.sh -- wrap a UEFI application in a raw GPT disk image with one EFI System
# Partition, the layout a custom server image needs to boot under UEFI.
#   scripts/make-raw-uefi-disk.sh EFI_APP OUT.raw [SIZE_MB]      (default 256 MB)
# The application is installed as /EFI/BOOT/BOOTX64.EFI, the UEFI fallback path.  The GPT is
# written by the Python below (no sgdisk/sfdisk needed); the filesystem by mtools.
set -euo pipefail
APP=${1:?EFI application}; OUT=${2:?output .raw}; MB=${3:-256}
python3 - "$OUT" "$MB" <<'PY'
import sys, struct, uuid, zlib
path, mb = sys.argv[1], int(sys.argv[2])
SECTOR = 512
total = mb * 1024 * 1024 // SECTOR            # sectors on the disk
first, last = 2048, total - 34                 # usable range (1 MiB start, backup GPT at the end)
esp_type = uuid.UUID("C12A7328-F81F-11D2-BA4B-00A0C93EC93B").bytes_le
guid = uuid.UUID(int=0x6d6f6475732d6573702d30303031).bytes_le   # a fixed partition GUID
entry = struct.pack("<16s16sQQQ72s", esp_type, guid, first, last, 0,
                    "EFI".encode("utf-16-le").ljust(72, b"\0"))
entries = entry + b"\0" * (128 * 128 - len(entry))
entries_crc = zlib.crc32(entries) & 0xffffffff
disk_guid = uuid.UUID(int=0x6d6f6475732d6469736b2d3030303031).bytes_le
def header(this, other, entries_lba):
    h = struct.pack("<8sIIIIQQQQ16sQIII", b"EFI PART", 0x10000, 92, 0, 0,
                    this, other, 34, last, disk_guid, entries_lba, 128, 128, entries_crc)
    return h + b"\0" * (SECTOR - len(h))
primary = header(1, total - 1, 2)
backup_h = bytearray(header(total - 1, 1, total - 33))
backup_h[16:20] = b"\0\0\0\0"
crc = zlib.crc32(bytes(backup_h[:92])) & 0xffffffff
backup_h[16:20] = struct.pack("<I", crc)
primary = bytearray(primary); primary[16:20] = b"\0\0\0\0"
primary[16:20] = struct.pack("<I", zlib.crc32(bytes(primary[:92])) & 0xffffffff)
# protective MBR
mbr = bytearray(SECTOR)
mbr[446:462] = struct.pack("<BBBBBBBBII", 0, 0, 2, 0, 0xEE, 0xFF, 0xFF, 0xFF, 1, total - 1)
mbr[510:512] = b"\x55\xaa"
with open(path, "wb") as f:
    f.truncate(total * SECTOR)
with open(path, "r+b") as f:
    f.write(bytes(mbr))
    f.seek(1 * SECTOR); f.write(bytes(primary))
    f.seek(2 * SECTOR); f.write(entries)
    f.seek((total - 33) * SECTOR); f.write(entries)
    f.seek((total - 1) * SECTOR); f.write(bytes(backup_h))
print(f"GPT written: ESP sectors {first}..{last}")
PY
PART="$OUT@@1048576"
PART_SECTORS=$(( (MB * 1024 * 1024 / 512) - 2048 - 34 ))
mformat -i "$PART" -F -h 64 -s 32 -t $(( PART_SECTORS / 32 / 64 )) -n 64 :: 2>/dev/null || mformat -i "$PART" -F ::
mmd -i "$PART" ::/EFI ::/EFI/BOOT
mcopy -i "$PART" "$APP" ::/EFI/BOOT/BOOTX64.EFI
echo "wrote $OUT ($MB MB): EFI System Partition at 1 MiB, BOOTX64.EFI from $APP"
