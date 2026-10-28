#!/bin/bash
# make-grub-usb.sh — Create a bootable USB image with GRUB + the Modus CL image
#
# Usage:
#   ./scripts/make-grub-usb.sh              # create /tmp/modus-grub-usb.iso
#   ./scripts/make-grub-usb.sh /dev/sdX     # write directly to USB stick (DANGEROUS)
#
# The image can also be written with:
#   sudo dd if=/tmp/modus-grub-usb.iso of=/dev/sdX bs=1M status=progress
#
# For ThinkPad T420: boot from USB (F12 boot menu), select GRUB entry.
# Works in both Legacy BIOS and UEFI-CSM modes.
# No sudo needed to build the image.
#
# Requires: grub-pc-bin xorriso mtools (apt install grub-pc-bin xorriso mtools)

set -e
cd "$(dirname "$0")/.."

BIN=/tmp/modus-x64-cl-pc.bin
IMG=/tmp/modus-grub-usb.iso
STAGING=$(mktemp -d /tmp/modus-grub-staging.XXXXXX)
trap 'rm -rf "$STAGING"' EXIT

# Build kernel if needed
NEED_BUILD=0
if [ ! -f "$BIN" ]; then NEED_BUILD=1; fi
[ -n "$(find mvm boot net lib runtime -name '*.lisp' -newer "$BIN" -print -quit 2>/dev/null)" ] && NEED_BUILD=1
if [ "$NEED_BUILD" = 1 ]; then
    # the multiboot CL image with the PC console (VGA text + PS/2, beside COM1)
    echo "Building the x64 CL image with its console..." >&2
    MODUS_PC_CONSOLE=1 MODUS_CL_REPL_OUT=$BIN sbcl --dynamic-space-size 12288 --script mvm/build-x64-cl-repl.lisp >&2
fi

echo "Creating GRUB bootable image..." >&2

# Stage files for grub-mkrescue
mkdir -p "$STAGING/boot/grub"
cp "$BIN" "$STAGING/boot/modus.bin"

cat > "$STAGING/boot/grub/grub.cfg" <<'GRUB'
set timeout=3
set default=0

menuentry "Modus v31e - fix BDSM + fill BDSM+DSPASURF" {
    multiboot /boot/modus.bin
    boot
}
GRUB

# Create hybrid ISO (BIOS-bootable, dd-able to USB)
grub-mkrescue -o "$IMG" "$STAGING" 2>&1 | grep -v "^NOTE:" >&2

echo "Created: $IMG ($(du -h "$IMG" | cut -f1))" >&2
echo "  Hybrid ISO — dd to USB or burn to CD" >&2
echo "  sudo dd if=$IMG of=/dev/sdX bs=1M status=progress" >&2

# If a device was specified, write to it
if [ -n "$1" ]; then
    DEV="$1"
    if [ ! -b "$DEV" ]; then
        echo "Error: $DEV is not a block device" >&2
        exit 1
    fi
    echo "WARNING: About to overwrite $DEV" >&2
    echo "Press Enter to continue or Ctrl-C to abort..." >&2
    read
    sudo dd if="$IMG" of="$DEV" bs=1M status=progress
    sync
    echo "Done. Boot from $DEV and select 'Modus Lisp REPL' in GRUB menu." >&2
fi
