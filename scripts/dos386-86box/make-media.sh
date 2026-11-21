#!/bin/sh
# make-media.sh IMAGE VMDIR FREEDOS [SFIMAGE] -- boot media for an 86Box (or real) PC:
#   VMDIR/hdd.img     83 MB IDE disk (162 x 16 x 63) that boots FreeDOS and
#                     runs C:\MODUS.COM C:\MODUS.BIN: an MBR (mbr.asm), one
#                     active FAT16 partition from sector 63 with FreeDOS's
#                     boot sector, KERNEL.SYS, COMMAND.COM, CONFIG.SYS,
#                     AUTOEXEC.BAT, MODUS.COM, IMAGE as MODUS.BIN and
#                     SFIMAGE, the soft-float image MODUS.COM picks on a
#                     machine with no FPU, as MODUS.SF.  Written to a USB
#                     stick it is the same thing a BIOS boots as a hard disk.
#   VMDIR/floppy.img  1.44 MB FreeDOS floppy that runs A:\MODUS.COM C:\MODUS.BIN
#                     (for a machine that will not boot the disk).
# FREEDOS is a directory with kernel.sys (FDOS/kernel ke2046_386f16 bin/),
# COMMAND.COM (FDOS/freecom), and boot12.bin and boot16.bin, the kernel's
# boot/boot.asm assembled with nasm -f bin -DISFAT12 / -DISFAT16 (SYS.COM
# is a DOS program; this splices the boot code around mformat's BPB the way
# SYS would).  matrix.py runs 86Box on these.
set -e
here=$(cd "$(dirname "$0")" && pwd)
img=$1 vm=$2 fd=$3 sfimg=$4
mkdir -p "$vm"; tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
rm -f "$vm/floppy.img" "$vm/hdd.img"
mformat -i "$vm/floppy.img" -f 1440 -C -v MODUSBOOT ::
splice() {   # splice BOOT's code around the BPB of the FAT boot sector at OFFSET in IMG
python3 -I - "$1" "$2" "$3" <<'PY'
import sys
boot = open(sys.argv[1], 'rb').read(); off = int(sys.argv[3])
f = open(sys.argv[2], 'r+b'); f.seek(off); sec = bytearray(f.read(512))
sec[0:3] = boot[0:3]; sec[0x3E:0x200] = boot[0x3E:0x200]   # keep the BPB (3..0x3D)
if off: sec[0x24] = 0x80                                    # drive: the first hard disk
f.seek(off); f.write(sec)
PY
}
splice "$fd/boot12.bin" "$vm/floppy.img" 0
printf 'FILES=20\r\nBUFFERS=20\r\n' > "$tmp/CONFIG.SYS"
printf '@ECHO OFF\r\nA:\\MODUS.COM C:\\MODUS.BIN\r\n' > "$tmp/AUTOEXEC.BAT"
printf '@ECHO OFF\r\nC:\\MODUS.COM C:\\MODUS.BIN\r\n' > "$tmp/AUTOEXEC.C"
"$here/../dos386-com.sh" "$tmp/MODUS.COM"
mcopy -i "$vm/floppy.img" "$fd/kernel.sys" ::KERNEL.SYS
mcopy -i "$vm/floppy.img" "$fd/COMMAND.COM" ::COMMAND.COM
for f in CONFIG.SYS AUTOEXEC.BAT MODUS.COM; do mcopy -i "$vm/floppy.img" "$tmp/$f" ::$f; done
nasm -f bin -o "$tmp/mbr.bin" "$here/mbr.asm"
python3 -I - "$vm/hdd.img" "$tmp/mbr.bin" <<'PY'
import sys, struct
C, H, S = 162, 16, 63; total = C * H * S; start = 63
img = bytearray(total * 512)
def chs(lba):
    c = lba // (H * S); h = (lba // S) % H; s = lba % S + 1
    return bytes([h, (s & 0x3F) | ((c >> 2) & 0xC0), c & 0xFF])
img[0x1BE:0x1CE] = bytes([0x80]) + chs(start) + bytes([0x06]) + chs(total - 1) + struct.pack('<II', start, total - start)
code = open(sys.argv[2], 'rb').read(); assert len(code) <= 0x1BE
img[0:len(code)] = code
img[0x1FE:0x200] = b'\x55\xAA'
open(sys.argv[1], 'wb').write(img)
PY
mformat -i "$vm/hdd.img@@32256" -T $((162 * 16 * 63 - 63)) -h 16 -s 63 -H 63 -v MODUS ::
splice "$fd/boot16.bin" "$vm/hdd.img" 32256
mcopy -i "$vm/hdd.img@@32256" "$fd/kernel.sys" ::KERNEL.SYS
mcopy -i "$vm/hdd.img@@32256" "$fd/COMMAND.COM" ::COMMAND.COM
mcopy -i "$vm/hdd.img@@32256" "$tmp/CONFIG.SYS" ::CONFIG.SYS
mcopy -i "$vm/hdd.img@@32256" "$tmp/AUTOEXEC.C" ::AUTOEXEC.BAT
mcopy -i "$vm/hdd.img@@32256" "$tmp/MODUS.COM" ::MODUS.COM
mcopy -i "$vm/hdd.img@@32256" "$img" ::MODUS.BIN
if [ -n "$sfimg" ]; then mcopy -i "$vm/hdd.img@@32256" "$sfimg" ::MODUS.SF; fi
