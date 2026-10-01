#!/bin/bash
# run-uefi-cl.sh — boot a Modus PE32+ image (build-uefi-repl or build-uefi-cl-repl)
# under OVMF and either evaluate expressions or attach interactively.
#
#   scripts/run-uefi-cl.sh IMAGE.efi                 interactive (Ctrl-A X quits)
#   scripts/run-uefi-cl.sh IMAGE.efi '(+ 1 2)' ...   eval each form, print results
#   OUT=file  keep the raw serial capture       NET=1  add an E1000 (user net)
#   HOSTFWD=port  with NET=1, forward host tcp:port to the guest's 22 (SSH)
#   TIMEOUT=s (default 120)                     MEM=m  (default 512)
#   FORM_WAIT=s  per-form wait (default 50)     KEEP=1  leave QEMU running (prints its pid)
#   QMP=path     add a QMP unix socket there     LOADER=file@addr  place FILE in guest RAM
#                (save-and-die cores: QMP pmemsave gets the core out, LOADER puts it back)
#
# THE PAYLOAD IS DELIVERED BY A FAT DISK HERE, WHICH AN SEV-SNP LAUNCH DOES NOT
# MEASURE.  For attestation the image must go in via `-kernel` under the AmdSev
# OVMF build (see docs/snp-guest.md); this script is the plain-OVMF bring-up.
#
# Forms end in CR: the bare-metal line editor terminates on 0x0D and ignores LF
# (see run-repl-eval.sh).  The result extraction prints the line after the echo.
set -u
EFI=${1:?usage: run-uefi-cl.sh IMAGE.efi [form ...]}; shift
OVMF=${OVMF:-/usr/share/OVMF/OVMF_CODE_4M.fd}
W=$(mktemp -d /tmp/modus-uefi-cl.XXXXXX)
IMG=$W/boot.img; VARS=$W/vars.fd; OUT=${OUT:-$W/serial.txt}
SZ=$(( ( $(stat -c %s "$EFI") / 1048576 ) + 8 ))
dd if=/dev/zero of=$IMG bs=1M count=$SZ status=none
mformat -i $IMG -F :: && mmd -i $IMG ::/EFI && mmd -i $IMG ::/EFI/BOOT && mcopy -i $IMG "$EFI" ::/EFI/BOOT/BOOTX64.EFI || exit 1
cp /usr/share/OVMF/OVMF_VARS_4M.fd $VARS
NETARGS=""; [ "${NET:-0}" = 1 ] && NETARGS="-device e1000,netdev=net0,romfile=,rombar=0 -netdev user,id=net0${HOSTFWD:+,hostfwd=tcp::${HOSTFWD}-:22}"
EXTRA=""; [ -n "${QMP:-}" ] && EXTRA="$EXTRA -qmp unix:${QMP},server=on,wait=off"
[ -n "${GDB:-}" ] && EXTRA="$EXTRA -gdb tcp::${GDB}"      # GDB=port: gdbstub, for a RIP when the guest goes silent
[ -n "${LOADER:-}" ] && EXTRA="$EXTRA -device loader,file=${LOADER%@*},addr=${LOADER#*@}"
QEMU="qemu-system-x86_64 -drive if=pflash,format=raw,readonly=on,file=$OVMF -drive if=pflash,format=raw,file=$VARS -drive format=raw,file=$IMG -m ${MEM:-512} -nographic -no-reboot $NETARGS $EXTRA"
# KERNEL=1: IMAGE is a multiboot kernel (build-x64-cl-repl), booted with -kernel instead of OVMF + FAT.
[ "${KERNEL:-0}" = 1 ] && QEMU="qemu-system-x86_64 -kernel $EFI -m ${MEM:-512} -nographic -no-reboot $NETARGS $EXTRA"
if [ $# = 0 ]; then echo "Booting $EFI (Ctrl-A X to quit)" >&2; exec $QEMU; fi
FIFO=$W/fifo; mkfifo $FIFO
timeout ${TIMEOUT:-120} $QEMU < $FIFO > $OUT 2>&1 &
QP=$!
exec 3>$FIFO
for i in $(seq 1 400); do
  kill -0 $QP 2>/dev/null || { echo "QEMU exited before the prompt" >&2; tr -d '\r' < $OUT | tail -5 >&2; exit 1; }
  tr -d '\r' < $OUT | tail -1 | grep -q '^> $' && break
  sleep 0.25
done
tr -d '\r' < $OUT | tail -1 | grep -q '^> $' || { echo "Timeout waiting for prompt" >&2; tr -d '\r' < $OUT | tail -5 >&2; kill $QP 2>/dev/null; exit 1; }
rc=0
for f in "$@"; do
  before=$(tr -d '\r' < $OUT | wc -l)
  printf '%s\r' "$f" >&3
  for i in $(seq 1 $(( ${FORM_WAIT:-50} * 4 ))); do
    kill -0 $QP 2>/dev/null || break
    tr -d '\r' < $OUT | tail -1 | grep -q '^> $' && [ "$(tr -d '\r' < $OUT | wc -l)" -gt "$before" ] && break
    # a form that halts the machine (save-and-die) never reprompts; CORE-END is its last word
    tr -d '\r' < $OUT | tail -n +$((before+1)) | grep -q '^CORE-END=' && break
    sleep 0.25
  done
  # result = lines after the echo line, before the next prompt
  tr -d '\r' < $OUT | tail -n +$((before+1)) | sed -n '2,$p' | grep -v '^> $'
done
if [ -n "${KEEP:-}" ]; then echo "KEEP: QEMU pid $QP serial $OUT" >&2; exit $rc; fi
exec 3>&-
kill $QP 2>/dev/null; wait $QP 2>/dev/null
exit $rc
