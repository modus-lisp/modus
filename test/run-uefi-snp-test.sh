#!/bin/bash
# run-uefi-snp-test.sh — the SEV-SNP boot path, as far as a plain machine can take it.
#
#   1. MODUS_UEFI_SNP=test build of the UEFI CL image (mvm/build-uefi-cl-repl.lisp):
#      boots under plain OVMF, and the stub raises the
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
W=${MODUS_SNP_WORK:-tmp/snp-test}; mkdir -p "$W"; W=$(cd "$W" && pwd)
fail=0
say() { echo "[snp-test] $*"; }

if [ "${MODUS_SNP_SKIP_BUILD:-0}" = 1 ] && [ -f $W/uefi-test.efi ] && [ -f $W/uefi-off.efi ]; then
  say "reusing images in $W"
else
say "building :test image (the UEFI CL image, ~3 min)"
MODUS_UEFI_SNP=test MODUS_CL_REPL_OUT=$W/uefi-test.efi sbcl --dynamic-space-size 12288 --script mvm/build-uefi-cl-repl.lisp > $W/build-test.log 2>&1 || { say "FAIL: :test build"; exit 1; }
say "building flag-off image"
MODUS_UEFI_SNP=0 MODUS_CL_REPL_OUT=$W/uefi-off.efi sbcl --dynamic-space-size 12288 --script mvm/build-uefi-cl-repl.lisp > $W/build-off.log 2>&1 || { say "FAIL: flag-off build"; exit 1; }
fi

# Signature: the handler's `mov rbx, GHCB` (48 BB 00 F0 1F 0C 00 00 00 00: the CL
# image's GHCB is the last page of its shared 2 MB at 0x0C000000).  Counted
# with Python — grep -P miscounts patterns containing NUL bytes.
has_sig() { python3 -c "import sys; sys.exit(0 if open(sys.argv[1],'rb').read().count(bytes.fromhex('48bb00f01f0c00000000'))>0 else 1)" "$1"; }
if has_sig $W/uefi-off.efi; then say "FAIL: flag-off image contains the #VC handler"; fail=1; else say "ok: flag-off image has no handler"; fi
if has_sig $W/uefi-test.efi; then say "ok: :test image contains the handler"; else say "FAIL: :test image lacks the handler"; fail=1; fi

# Boot the :test image under plain OVMF: the witness before the banner, then a form
R=$(OUT=$W/serial.txt TIMEOUT=240 scripts/run-uefi-cl.sh $W/uefi-test.efi '(* 6 7)' 2>/dev/null | tail -1)
LINE=$(tr -d '\r' < $W/serial.txt | sed 's/\x1b\[[0-9;=]*[A-Za-z]//g' | grep -o 'VC[^>]*' | head -1)
say "serial witness: '${LINE}', (* 6 7) => '$R'"
case "$LINE" in
  VC+wi5) say "ok: #VC path — OUT dx, IN dx, IN ax (66), IN imm8; 5 entries" ;;
  *!*)    say "FAIL: handler took its fatal path"; fail=1 ;;
  *)      say "FAIL: witness missing or wrong (expected VC+wi5)"; fail=1 ;;
esac
[ "$R" = "42" ] && say "ok: the REPL evaluates after the self-test" || { say "FAIL: no evaluation after the self-test"; fail=1; }
[ $fail = 0 ] && say "PASS" || say "FAIL"
exit $fail
