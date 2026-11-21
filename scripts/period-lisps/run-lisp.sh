#!/bin/bash
# run-lisp.sh WHICH OUTDIR [MACHINE] [RAM] -- boot $LISPS/disks/WHICH.hd.chd under MAME and type
# forms at the Lisp's Listener, with a screenshot OUTDIR/shot-NN.png before each form (so
# shot-(k+1) shows form k's result) and one at the end.
#   FORMS_FILE  one form per line (default: $HERE/bench-forms.lisp)
#   MAC_FORMS   |-separated forms; overrides FORMS_FILE
#   TIMED=1     (default) wrap each form as (time (eval (quote FORM)))
#   DELAY, GAP  emulated seconds before the first form / between forms (default 90, 45)
#   DISK        another .chd to boot instead
#   NVRAM       a directory copied to OUTDIR/nvram first (MAME's PRAM), e.g.
#               $LISPS/nvram/q950-32bit: Quadra 950 PRAM with 32-bit addressing on
#   MAMEBIN     MAME binary; default: 0.251 for 68000/030 Macs, the 0.289 Quadra-only build
#               ($RSS/msrc/mame/qd) for macqd* (0.251 has no macqd950)
# MAME runs -nothrottle with no video; timings are the emulated machine's own clock.
set -eu
LISPS=${LISPS:-/mnt/data-fast/claude/rss/lisps}
HERE=$(cd "$(dirname "$0")" && pwd)
RSS=/mnt/data-fast/claude/rss
MAME=${MAME:-$RSS/mame}
W=$1; D=$(realpath -m "$2"); MACH=${3:-macclasc}; RAM=${4:-4M}
DISK=${DISK:-$LISPS/disks/$W.hd.chd}
if [ -z "${MAC_FORMS:-}" ]; then
  F=""
  while IFS= read -r l; do
    [ -z "$l" ] && continue
    case "$l" in \;*) continue ;; esac
    [ "${TIMED:-1}" = 1 ] && l="(time (eval (quote $l)))"
    F="${F:+$F|}$l"
  done < "${FORMS_FILE:-$HERE/bench-forms.lisp}"
  MAC_FORMS=$F
fi
mkdir -p "$D"; rm -f "$D"/shot-*.png
if [ -n "${NVRAM:-}" ]; then rm -rf "$D/nvram"; cp -r "$NVRAM" "$D/nvram"; fi
cp "$DISK" "$D/disk.chd"          # MAME may write to the CHD; keep the built one pristine
case $MACH in
  macqd*) MAMEBIN=${MAMEBIN:-$RSS/msrc/mame/qd}; LIBS=$RSS/mdev/root/usr/lib/x86_64-linux-gnu ;;
  *)      MAMEBIN=${MAMEBIN:-$MAME/root/usr/games/mame}; LIBS=$MAME/root/usr/lib/x86_64-linux-gnu ;;
esac
export LD_LIBRARY_PATH=$LIBS HOME=$MAME
cd "$D"
SNAPDIR="$D" DELAY=${DELAY:-90} GAP=${GAP:-45} MAC_FORMS="$MAC_FORMS" \
  timeout ${TIMEOUT:-3600} $MAMEBIN $MACH -rompath $MAME/roms -ramsize $RAM \
  -hard "$D/disk.chd" -video none -sound none -nothrottle -skip_gameinfo \
  -autoboot_script $HERE/lisp.lua ${MAMEARGS:-} > "$D/mame.log" 2>&1 || echo "mame exit $? (see $D/mame.log)"
rm -f "$D/disk.chd"
ls "$D"
