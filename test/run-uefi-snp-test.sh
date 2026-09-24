#!/bin/bash
# run-uefi-snp-test.sh — the SEV-SNP boot path, as far as a plain machine can take it.
#
#   1. MODUS_UEFI_SNP=test build: boots under plain OVMF, and the stub raises the
#      #VC handler with INT 29 in front of real IN/OUT instructions.  The serial
#      line "VC+wi5" is the witness (see emit-snp-selftest in
#      boot/boot-uefi-snp.lisp); the handler's fatal path prints '!'.  The REPL
#      prompt must still appear afterwards.
#   2. Flag-off build: must contain NO trace of the handler (the movabs of the
#      GHCB address), and the :test build must.  Byte-identity of the flag-off
#      image with the pre-change tree was measured once (md5 56f4213f…, 2026-09-24)
#      and is not re-measured here because any compiler change moves it.
#
# The real-SNP arm (MODUS_UEFI_SNP=1) needs an SNP host: kernel >= 6.11 with
# /dev/sev, QEMU >= 9.1, and the AmdSev OVMF build.  Not runnable here yet.
set -u
cd "$(dirname "$0")/.."
W=${MODUS_SNP_WORK:-tmp/snp-test}; mkdir -p "$W"
fail=0
say() { echo "[snp-test] $*"; }

if [ "${MODUS_SNP_SKIP_BUILD:-0}" = 1 ] && [ -f $W/uefi-test.efi ] && [ -f $W/uefi-off.efi ]; then
  say "reusing images in $W"
else
say "building :test image"
MODUS_UEFI_SNP=test MODUS_UEFI_OUT=$W/uefi-test.efi sbcl --script mvm/build-uefi-repl.lisp > $W/build-test.log 2>&1 || { say "FAIL: :test build"; exit 1; }
say "building flag-off image"
MODUS_UEFI_SNP=0 MODUS_UEFI_OUT=$W/uefi-off.efi sbcl --script mvm/build-uefi-repl.lisp > $W/build-off.log 2>&1 || { say "FAIL: flag-off build"; exit 1; }
fi

# Signature: the handler's `mov rbx, GHCB` (48 BB 00 F0 1F 05 00 00 00 00).  Counted
# with Python — grep -P miscounts patterns containing NUL bytes.
has_sig() { python3 -c "import sys; sys.exit(0 if open(sys.argv[1],'rb').read().count(bytes.fromhex('48bb00f01f0500000000'))>0 else 1)" "$1"; }
if has_sig $W/uefi-off.efi; then say "FAIL: flag-off image contains the #VC handler"; fail=1; else say "ok: flag-off image has no handler"; fi
if has_sig $W/uefi-test.efi; then say "ok: :test image contains the handler"; else say "FAIL: :test image lacks the handler"; fail=1; fi

# Boot the :test image under OVMF and read the serial line
IMG=$W/boot.img; VARS=$W/vars.fd; OUT=$W/serial.txt
dd if=/dev/zero of=$IMG bs=1M count=64 status=none; mformat -i $IMG -F ::; mmd -i $IMG ::/EFI; mmd -i $IMG ::/EFI/BOOT
mcopy -i $IMG $W/uefi-test.efi ::/EFI/BOOT/BOOTX64.EFI
cp /usr/share/OVMF/OVMF_VARS_4M.fd $VARS
timeout 60 qemu-system-x86_64 -drive if=pflash,format=raw,readonly=on,file=/usr/share/OVMF/OVMF_CODE_4M.fd \
  -drive if=pflash,format=raw,file=$VARS -drive format=raw,file=$IMG -m 512 -nographic -no-reboot </dev/null > $OUT 2>&1 &
QP=$!
for i in $(seq 1 200); do tr -d '\r' < $OUT | grep -q '^> ' && break; sleep 0.25; done
kill $QP 2>/dev/null; wait $QP 2>/dev/null
LINE=$(tr -d '\r' < $OUT | sed 's/\x1b\[[0-9;=]*[A-Za-z]//g' | grep -o 'VC[^>]*' | head -1)
say "serial witness: '${LINE}'"
case "$LINE" in
  VC+wi5) say "ok: #VC path — OUT dx, IN dx, IN ax (66), IN imm8; 5 entries" ;;
  *!*)    say "FAIL: handler took its fatal path"; fail=1 ;;
  *)      say "FAIL: witness missing or wrong (expected VC+wi5)"; fail=1 ;;
esac
tr -d '\r' < $OUT | grep -q '^> ' && say "ok: REPL prompt reached after the self-test" || { say "FAIL: no REPL prompt"; fail=1; }
[ $fail = 0 ] && say "PASS" || say "FAIL"
exit $fail
