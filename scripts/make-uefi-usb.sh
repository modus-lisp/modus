#!/bin/bash
# make-uefi-usb.sh — Create a bootable USB image for UEFI x64 hardware
#
# Usage:
#   ./scripts/make-uefi-usb.sh              # create /tmp/modus-usb.img
#   ./scripts/make-uefi-usb.sh /dev/sdX     # write directly to USB stick (DANGEROUS)
#
# The image can also be written with:
#   sudo dd if=/tmp/modus-usb.img of=/dev/sdX bs=1M status=progress
#
# For ThinkPad T420: boot from USB in UEFI mode (F12 boot menu).
# Includes VGA text mode fallback + PC speaker beep diagnostic.

set -e
cd "$(dirname "$0")/.."

EFI=/tmp/modus-uefi-cl.efi
IMG=/tmp/modus-usb.img

# Build the EFI binary: the CL image, with its screen and keyboard console
# (net/pc-console.lisp: GOP framebuffer + PS/2, beside COM1)
echo "Building the UEFI CL image..." >&2
MODUS_CL_REPL_OUT=$EFI sbcl --dynamic-space-size 12288 --script mvm/build-uefi-cl-repl.lisp >&2

# Create 128MB GPT-partitioned image with EFI System Partition
echo "Creating GPT image with EFI System Partition..." >&2
dd if=/dev/zero of="$IMG" bs=1M count=128 status=none

# GPT partition table with one EFI System Partition
echo 'label: gpt
type=C12A7328-F81F-11D2-BA4B-00A0C93EC93B, size=120M' | /sbin/sfdisk "$IMG" >/dev/null 2>&1

# ESP starts at sector 2048 = byte offset 1048576
PART_OFFSET=1048576

# Format ESP as FAT32 and install EFI binary
mformat -i "$IMG@@$PART_OFFSET" -F ::
mmd -i "$IMG@@$PART_OFFSET" ::/EFI
mmd -i "$IMG@@$PART_OFFSET" ::/EFI/BOOT
mcopy -i "$IMG@@$PART_OFFSET" "$EFI" ::/EFI/BOOT/BOOTX64.EFI

echo "Created: $IMG ($(du -h "$IMG" | cut -f1))" >&2
echo "  GPT + EFI System Partition + FAT32 + /EFI/BOOT/BOOTX64.EFI" >&2

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
    echo "Done. Boot from $DEV in UEFI mode." >&2
fi
