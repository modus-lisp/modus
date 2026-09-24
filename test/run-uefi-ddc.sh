#!/bin/bash
# run-uefi-ddc.sh — is the UEFI CL image a self-host FIXPOINT?
#
#   1. SBCL builds mvm/build-uefi-cl-repl.lisp -> sbcl.efi and dumps the exact
#      source text it compiled (MODUS_DDC_DUMP_SOURCE).
#   2. SBCL builds the self-hosting image (mvm/build-modus-selfhost.lisp -> modus-sh)
#      unless MODUS_SH names one already built (any host: scripts/run-fixpoint-hosts.sh).
#   3. modus-sh --compile-uefi <dump> sh.efi <mode>   — Modus compiles the image.
#   4. Byte comparison sbcl.efi vs sh.efi, and sh.efi vs a second run (per-run
#      reproducibility).  md5 of sh.efi is the DDC'd hash an SNP launch should
#      measure (via AmdSev OVMF -kernel; see docs/snp-guest.md).
#
# Env: MODUS_UEFI_SNP (0|test|1, default 0), MODUS_NET_BUILD (passed through),
#      MODUS_SH (reuse a modus-sh), MODUS_DDC_WORK (default tmp/uefi-ddc), DSS (12288).
set -uo pipefail
cd "$(dirname "$0")/.."
W=${MODUS_DDC_WORK:-tmp/uefi-ddc}; mkdir -p "$W"; W=$(cd "$W" && pwd)
MODE=${MODUS_UEFI_SNP:-0}; DSS=${DSS:-12288}
say() { echo "[uefi-ddc] $(date +%H:%M:%S) $*"; }
t0=$(date +%s)

say "1. SBCL build of the UEFI CL image (snp=$MODE)"
MODUS_UEFI_SNP=$MODE MODUS_CL_REPL_OUT=$W/sbcl.efi MODUS_DDC_DUMP_SOURCE=$W/full-source.lisp \
  sbcl --dynamic-space-size $DSS --script mvm/build-uefi-cl-repl.lisp > $W/build-sbcl.log 2>&1 \
  || { say "FAIL: SBCL build (see $W/build-sbcl.log)"; exit 1; }
say "   $(stat -c %s $W/sbcl.efi) bytes, source $(stat -c %s $W/full-source.lisp) chars"

if [ -n "${MODUS_SH:-}" ] && [ -x "$MODUS_SH" ]; then
  SH=$MODUS_SH; say "2. reusing modus-sh at $SH"
else
  SH=$W/modus-sh; say "2. SBCL build of modus-sh"
  MODUS_CLI_OUT=$SH sbcl --dynamic-space-size $DSS --script mvm/build-modus-selfhost.lisp > $W/build-sh.log 2>&1 \
    || { say "FAIL: modus-sh build (see $W/build-sh.log)"; exit 1; }
fi

say "3. modus-sh --compile-uefi (in-image compile of $(stat -c %s $W/full-source.lisp) chars)"
for i in 1 2; do
  ( cd $W && "$SH" --compile-uefi $W/full-source.lisp $W/sh$i.efi $MODE ) > $W/compile-$i.log 2>&1
  rc=$?; say "   run $i rc=$rc: $(grep -a 'modus' $W/compile-$i.log | tail -1)"
  [ -s $W/sh$i.efi ] || { say "FAIL: no output from run $i (see $W/compile-$i.log)"; exit 1; }
done

say "4. bytes"
md5sum $W/sbcl.efi $W/sh1.efi $W/sh2.efi | tee $W/md5.txt
fail=0
cmp -s $W/sh1.efi $W/sh2.efi && say "ok: in-image compile is reproducible run-to-run" || { say "FAIL: sh1 != sh2"; fail=1; }
if cmp -s $W/sbcl.efi $W/sh1.efi; then say "ok: FIXPOINT — SBCL and modus-sh produce the same image"
else
  say "DIFF: SBCL and modus-sh images differ ($(cmp $W/sbcl.efi $W/sh1.efi 2>&1 | head -1)); first bytes:"
  cmp -l $W/sbcl.efi $W/sh1.efi 2>/dev/null | head -5; fail=1
fi
say "done in $(( $(date +%s) - t0 )) s"; [ $fail = 0 ] && say "PASS" || say "FAIL"; exit $fail
