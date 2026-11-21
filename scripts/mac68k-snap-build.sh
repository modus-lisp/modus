#!/bin/bash
# mac68k-snap-build.sh SNAPDIR OUTDIR -- the classicos Mac image with the runtime JIT, built from
# a snapshot of the tree (never edit files a running build reads), its saved core taken on
# Basilisk II, and a MAME hard disk (OUTDIR/c.hd.chd).  Tool layout: $RSS (default /tmp/rss):
#   b2/  Basilisk II (basilisk-headless.sh, BasiliskII-jit-lo from scripts/basilisk-unpie.lisp, lc475.rom)
#   mame/ldpath, mame/root/usr/bin/chdman  (Debian bookworm's mame, mame-tools unpacked)
set -u
S=$1; D=$2; RSS=${RSS:-/tmp/rss}
# MACHINE=quadra: the big map, all native and unpaged (boot-mac68k :QUADRA); default classicos
MACHINE=${MACHINE:-classicos}
# SHIFT=HEX: the whole map moved (boot-mac68k MODUS_MAC68K_SHIFT); always six
# digits, so builds a shift apart record strings of one length (mac68k-reloc.lisp)
export MODUS_MAC68K_SHIFT=$(printf '%06X' $((16#${SHIFT:-0})))
if [ $MACHINE = quadra ]; then KNOBS="MODUS_68K_PAGED=0 MODUS_68K_THREADED=0 MODUS_SMALL_ACCESS=${MODUS_SMALL_ACCESS:-0} MODUS_68K_SHARED=${MODUS_68K_SHARED:-cons,alloc-obj,array-len} MODUS_SHAKE=${MODUS_SHAKE:-1} MODUS_68K_COLD_KEEP=${MODUS_68K_COLD_KEEP-mvm/68k-keep-quadra.txt}"
else KNOBS="MODUS_68K_PAGED=1 MODUS_68K_THREADED=1 MODUS_68K_HOT=mvm/68k-hot-mac.txt MODUS_HOT_ORDER=mvm/68k-hotorder-repl.txt"; fi
rm -rf $D && mkdir -p $D && cd $S
env MODUS_VESSEL=${MODUS_VESSEL:-/home/claude/modus-mac/.vessel} MODUS_68K_JIT=1 MODUS_MAC68K_MACHINE=$MACHINE \
  MODUS_68K_CPU=68000 MODUS_SMALL_IMAGE=1 MODUS_NARROW_STRINGS=1 $KNOBS MODUS_68K_REGIONS=$D/regions.txt \
  MODUS_SYMMAP=$D/cl.map MODUS_CL_REPL_OUT=$D/cl.bin sbcl --dynamic-space-size 12288 --script mvm/build-mac68k-cl-repl.lisp > $D/build.log 2>&1
echo "BUILD-EXIT $?"; grep -E "Unhandled|68k paged:" $D/build.log | head -5
for i in 1 2 3; do MODUS_MAC68K_MACHINE=$MACHINE MALLOC_MMAP_MAX_=0 MAC68K_JIT=true MAC68K_ROM=$RSS/b2/lc475.rom \
  MAC68K_BASILISK="env MAC_BASILISK_BIN=BasiliskII-jit-lo $RSS/b2/basilisk-headless.sh" \
  timeout 1500 env FS_APPLE=${FS_APPLE:?set FS_APPLE} sbcl --script scripts/mac68k-core.lisp $D/cl.bin $D/c.dsk > $D/core.log 2>&1 && break; done
echo "CORE-EXIT $?"; grep "mac68k-core: /" $D/core.log | cut -c1-100
FS_APPLE=${FS_APPLE:?mac68k-snap-build: set FS_APPLE to an fs-apple checkout (a snapshot has no sibling one)} sbcl --script scripts/mac68k-hd.lisp $D/c.dsk $D/c.hd --chd "env LD_LIBRARY_PATH=$(cat $RSS/mame/ldpath) $RSS/mame/root/usr/bin/chdman" | tail -1
