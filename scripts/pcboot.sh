#!/bin/sh
# pcboot.sh OUT IMAGE [SFIMAGE] [--sectors N] -- a disk that boots modus with no DOS.
#
# Assembles the boot sector (pcboot1.s), the shim (pcboot2.s) and MODUS.COM
# (dos386-chain.s, via dos386-com.sh), and writes OUT with
# scripts/pcboot-disk.lisp: MODUS.COM, IMAGE as MODUS.BIN and SFIMAGE (the
# soft-float image MODUS.COM picks on a machine with no FPU) as MODUS.SF.
# Write OUT to a USB stick, a hard disk or (when it fits) a floppy; the BIOS
# boots it and MODUS.COM runs as it does under DOS, minus DOS.
#
#   --sectors N   pad the disk to N sectors (163296: the matrix's 162x16x63)
set -e
here=$(cd "$(dirname "$0")" && pwd)
out=$1; image=$2; shift 2
sf=""
if [ $# -gt 0 ] && [ "$1" != "--sectors" ]; then sf=$1; shift; fi
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
as --32 -o "$tmp/b1.o" "$here/pcboot1.s"
ld -m elf_i386 -Ttext=0x7C00 --oformat binary -o "$tmp/pcboot1.bin" "$tmp/b1.o"
as --32 -o "$tmp/b2.o" "$here/pcboot2.s"
ld -m elf_i386 -Ttext=0x0800 --oformat binary -o "$tmp/pcboot2.bin" "$tmp/b2.o"
"$here/dos386-com.sh" "$tmp/MODUS.COM"
set -- "$@" MODUS.COM="$tmp/MODUS.COM" MODUS.BIN="$image"
[ -n "$sf" ] && set -- "$@" MODUS.SF="$sf"
sbcl --script "$here/pcboot-disk.lisp" "$out" "$tmp/pcboot1.bin" "$tmp/pcboot2.bin" "$@"
