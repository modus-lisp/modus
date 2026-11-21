#!/bin/bash
# mac68k-mame-run.sh DIR MACHINE RAM TAG FORMS [SECONDS] -- a REPL session on DIR/c.hd.chd by the
# screen console (scripts/mac68k-mame-screen.lua); transcript in $RSS/qd/TAG.txt.  macclasc and
# friends run Debian bookworm's MAME 0.251; macqd950/macqd700 a MAME 0.289 built from source
# (make SUBTARGET=qd SOURCES=src/mame/apple/macquadra700.cpp), which also needs FAKESCREEN=0x600000.
D=$(readlink -f $1); M=$2; RAM=$3; T=$4; F=$5; S=${6:-300}; RSS=${RSS:-/tmp/rss}
HERE=$(dirname "$(readlink -f "$0")")
mkdir -p $RSS/qd; cd $RSS/qd
case $M in
  macqd700|macqd950) export LD_LIBRARY_PATH=$RSS/mdev/root/usr/lib/x86_64-linux-gnu; BIN=$RSS/msrc/mame/qd
                     export FAKESCREEN=${FAKESCREEN:-0x600000} ;;
  *) export LD_LIBRARY_PATH=$(cat $RSS/mame/ldpath); BIN=$RSS/mame/root/usr/games/mame ;;
esac
export HOME=$RSS/mame
REGION=${REGION:-0x2B0000} MAC_OUT=$RSS/qd/$T.txt MAC_SNAP=$RSS/qd/$T.png MAC_FORMS="$F" MAC_SECONDS=$S \
  timeout 3000 $BIN $M -rompath $RSS/mame/roms -ramsize $RAM -hard $D/c.hd.chd -video none -sound none \
  -nothrottle -skip_gameinfo -autoboot_script $HERE/mac68k-mame-screen.lua > $RSS/qd/$T.log 2>&1
cat $RSS/qd/$T.txt
