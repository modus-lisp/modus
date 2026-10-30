#!/bin/bash
# run-aarch64-ddc.sh — the fast AArch64 DDC: does the self-hosted compiler
# emit the same Linux/AArch64 image as SBCL from the same source?
#
#   test/run-aarch64-ddc.sh UEFI-DDC-WORKDIR
#
# Reuses what test/run-uefi-ddc.sh left in its work directory -- the source
# dump (full-source.lisp) and the self-hosting image (modus-sh) -- so the
# acceptance gate pays for one modus-sh build, not two.  Then, in parallel:
#   SBCL     test/ddc-aarch64-host.lisp     full-source.lisp -> sbcl-aa64.elf
#   modus-sh --compile-aarch64              full-source.lisp -> sh-aa64.elf
# and requires the two byte-identical.  Both sides set the same emitter
# settings (the SBCL script mirrors %INIT-AARCH64-TRANSLATOR), so a
# difference is a difference between the two compilers -- the shared front
# end or the AArch64 translator.  The images are not meant to run.
set -uo pipefail
cd "$(dirname "$0")/.."
W=${1:?usage: run-aarch64-ddc.sh UEFI-DDC-WORKDIR}; W=$(cd "$W" && pwd)
DSS=${DSS:-12288}
say() { echo "[aa64-ddc] $(date +%H:%M:%S) $*"; }
for f in full-source.lisp modus-sh; do
  [ -s "$W/$f" ] || { say "FAIL: no $W/$f (run test/run-uefi-ddc.sh first)"; exit 1; }
done
t0=$(date +%s)
sbcl --dynamic-space-size "$DSS" --script test/ddc-aarch64-host.lisp \
     "$W/full-source.lisp" "$W/sbcl-aa64.elf" > "$W/aa64-sbcl.log" 2>&1 & p1=$!
( cd "$W" && ./modus-sh --compile-aarch64 full-source.lisp sh-aa64.elf ) > "$W/aa64-sh.log" 2>&1 & p2=$!
wait $p1; r1=$?; wait $p2; r2=$?
[ $r1 = 0 ] && [ -s "$W/sbcl-aa64.elf" ] || { say "FAIL: SBCL side rc=$r1 (see $W/aa64-sbcl.log)"; exit 1; }
[ $r2 = 0 ] && [ -s "$W/sh-aa64.elf" ] || { say "FAIL: modus-sh side rc=$r2 (see $W/aa64-sh.log)"; exit 1; }
md5sum "$W/sbcl-aa64.elf" "$W/sh-aa64.elf"
say "done in $(( $(date +%s) - t0 )) s"
if cmp -s "$W/sbcl-aa64.elf" "$W/sh-aa64.elf"; then
  say "ok: FIXPOINT — SBCL and modus-sh emit the same AArch64 image"; say "PASS"; exit 0
fi
say "DIFF: $(cmp "$W/sbcl-aa64.elf" "$W/sh-aa64.elf" | head -1); sizes $(stat -c %s "$W/sbcl-aa64.elf") / $(stat -c %s "$W/sh-aa64.elf")"
say "FAIL"; exit 1
