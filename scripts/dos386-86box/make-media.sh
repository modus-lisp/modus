#!/bin/sh
# make-media.sh IMAGE VMDIR FREEDOS [SFIMAGE] -- the boot media for an 86Box (or real) PC:
#   VMDIR/floppy.img  1.44 MB FreeDOS boot floppy: KERNEL.SYS, COMMAND.COM,
#                     CONFIG.SYS, MODUS.COM and an AUTOEXEC.BAT that runs
#                     A:\MODUS.COM C:\MODUS.BIN
#   VMDIR/hdd.img     83 MB IDE disk (162 x 16 x 63), one FAT16 partition from
#                     sector 63 holding IMAGE as MODUS.BIN and SFIMAGE, the
#                     soft-float image MODUS.COM picks on a machine with no
#                     FPU, as MODUS.SF.  Its MBR (mbr.asm) boots A:, so a
#                     BIOS that tries C: first still starts the floppy.
# FREEDOS is a directory with kernel.sys (FDOS/kernel ke2046_386f16 bin/),
# COMMAND.COM (FDOS/freecom) and boot12.bin, the kernel's boot/boot.asm
# assembled with nasm -f bin -DISFAT12 (SYS.COM is a DOS program; this
# splices the boot code around mformat's BPB the way SYS would).
# Then: sed s|@VMDIR@|VMDIR| 486vipio2.cfg > VMDIR/86box.cfg and run 86Box
# with -P VMDIR --automation unix:SOCKET (built with -DAUTOMATION=ON).
set -e
here=$(cd "$(dirname "$0")" && pwd)
img=$1 vm=$2 fd=$3 sfimg=$4
mkdir -p "$vm"; tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
rm -f "$vm/floppy.img" "$vm/hdd.img"
mformat -i "$vm/floppy.img" -f 1440 -C -v MODUSBOOT ::
python3 -I - "$fd/boot12.bin" "$vm/floppy.img" <<'PY'
import sys
boot = open(sys.argv[1], 'rb').read(); img = bytearray(open(sys.argv[2], 'rb').read())
img[0:3] = boot[0:3]; img[0x3E:0x200] = boot[0x3E:0x200]   # keep the BPB (3..0x3D)
open(sys.argv[2], 'wb').write(img)
PY
printf 'FILES=20\r\nBUFFERS=20\r\n' > "$tmp/CONFIG.SYS"
printf '@ECHO OFF\r\nA:\\MODUS.COM C:\\MODUS.BIN\r\n' > "$tmp/AUTOEXEC.BAT"
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
code = open(sys.argv[2], 'rb').read(); assert len(code) <= 0x1B8
img[0:len(code)] = code
img[0x1FE:0x200] = b'\x55\xAA'
open(sys.argv[1], 'wb').write(img)
PY
mformat -i "$vm/hdd.img@@32256" -T $((162 * 16 * 63 - 63)) -h 16 -s 63 -H 63 -v MODUS ::
mcopy -i "$vm/hdd.img@@32256" "$img" ::MODUS.BIN
if [ -n "$sfimg" ]; then mcopy -i "$vm/hdd.img@@32256" "$sfimg" ::MODUS.SF; fi
