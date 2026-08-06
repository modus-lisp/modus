#!/bin/bash
# run-uefi-repl.sh — Build and run Modus UEFI x64 REPL via OVMF
#
# Usage:
#   ./scripts/run-uefi-repl.sh              # interactive serial REPL
#   ./scripts/run-uefi-repl.sh "(+ 1 2)"    # eval expression, print result, exit
#
# Ctrl-A X to quit QEMU (interactive mode)
#
# Includes GOP framebuffer output and PS/2 keyboard input.
# Serial I/O always active (for QEMU -nographic testing).
#
# Requires: ovmf (apt install ovmf), mtools (apt install mtools)

set -e
cd "$(dirname "$0")/.."

EFI=/tmp/modus-uefi.efi
FAT=/tmp/modus-uefi.img
OVMF=/usr/share/OVMF/OVMF_CODE_4M.fd
VARS=/tmp/modus-ovmf-vars.fd
TIMEOUT=${REPL_TIMEOUT:-60}

# Build EFI binary if needed (check all source files)
NEED_BUILD=0
if [ ! -f "$EFI" ]; then NEED_BUILD=1; fi
for src in mvm/build-uefi-repl.lisp mvm/repl-source.lisp net/uefi-console.lisp \
           boot/boot-uefi-x64.lisp mvm/compiler.lisp mvm/translate-x64.lisp; do
    [ -f "$src" ] && [ "$src" -nt "$EFI" ] && NEED_BUILD=1
done
if [ "$NEED_BUILD" = 1 ]; then
    echo "Building UEFI REPL..." >&2
    sbcl --script mvm/build-uefi-repl.lisp >&2
fi

# Create FAT32 disk image with EFI binary
echo "Creating FAT32 boot image..." >&2
dd if=/dev/zero of="$FAT" bs=1M count=64 status=none
mformat -i "$FAT" -F ::
mmd -i "$FAT" ::/EFI
mmd -i "$FAT" ::/EFI/BOOT
mcopy -i "$FAT" "$EFI" ::/EFI/BOOT/BOOTX64.EFI

# Copy OVMF vars (writable copy needed)
cp /usr/share/OVMF/OVMF_VARS_4M.fd "$VARS" 2>/dev/null || true

QEMU_ARGS="-drive if=pflash,format=raw,readonly=on,file=$OVMF \
    -drive if=pflash,format=raw,file=$VARS \
    -drive format=raw,file=$FAT \
    -m 512 -nographic -no-reboot"

# Batch mode: eval expression and print result
if [ -n "$1" ]; then
    FIFO=$(mktemp -u /tmp/modus-fifo.XXXXXX)
    OUT=$(mktemp /tmp/modus-eval.XXXXXX)
    mkfifo "$FIFO"
    trap 'rm -f "$OUT" "$FIFO"; kill $QPID 2>/dev/null; wait $QPID 2>/dev/null' EXIT

    timeout "$TIMEOUT" qemu-system-x86_64 $QEMU_ARGS < "$FIFO" > "$OUT" 2>/dev/null &
    QPID=$!
    exec 3>"$FIFO"

    for i in $(seq 1 300); do
        kill -0 $QPID 2>/dev/null || { echo "QEMU died" >&2; exit 1; }
        if tail -1 "$OUT" 2>/dev/null | grep -q '^> $'; then break; fi
        sleep 0.1
    done
    if ! tail -1 "$OUT" 2>/dev/null | grep -q '^> $'; then
        echo "Timeout waiting for prompt" >&2; exit 1
    fi

    sleep 0.3
    # CR, not LF: the bare-metal serial line editor terminates a line on
    # CR (0x0D) and ignores LF.  With "\n" the expression echoes at the
    # prompt and is never evaluated, so this script produced NO output and
    # exited 0 — a silent false pass that made OVMF look broken.  Same bug
    # was fixed in run-repl-eval.sh; this script has its own inline copy.
    printf '%s\r' "$1" >&3
    sleep 1
    exec 3>&-
    sleep 0.5

    tr -d '\r' < "$OUT" | sed -n '/^> .*[^ ]/{n; /^>/!{/^$/!p;};}' | head -1
    exit 0
fi

# Interactive mode
echo "Booting UEFI REPL (Ctrl-A X to quit)..." >&2
exec qemu-system-x86_64 $QEMU_ARGS
