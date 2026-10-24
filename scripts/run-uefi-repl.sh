#!/bin/bash
# run-uefi-repl.sh — build (if needed) and boot the UEFI x64 CL image under OVMF.
#
#   ./scripts/run-uefi-repl.sh              # interactive (Ctrl-A X quits)
#   ./scripts/run-uefi-repl.sh "(+ 1 2)"    # evaluate forms, print the results
#
# The image is mvm/build-uefi-cl-repl.lisp: the CL REPL on COM1 and, through
# net/pc-console.lisp, on the GOP framebuffer with PS/2 keyboard input.  It is
# rebuilt when it is missing or any .lisp under mvm/ boot/ net/ lib/ runtime/
# is newer.  Running it is scripts/run-uefi-cl.sh, which takes its knobs (NET=1,
# QMP=, TIMEOUT=, ...).
#
# Requires: ovmf, mtools.
set -e
cd "$(dirname "$0")/.."
EFI=${MODUS_UEFI_EFI:-/tmp/modus-uefi-cl.efi}
if [ ! -f "$EFI" ] || \
   [ -n "$(find mvm boot net lib runtime -name '*.lisp' -newer "$EFI" -print -quit 2>/dev/null)" ]; then
  echo "Building the UEFI CL image (a few minutes)..." >&2
  MODUS_CL_REPL_OUT=$EFI sbcl --dynamic-space-size 12288 --script mvm/build-uefi-cl-repl.lisp >&2
fi
exec scripts/run-uefi-cl.sh "$EFI" "$@"
