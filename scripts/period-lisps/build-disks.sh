#!/bin/bash
# build-disks.sh [pearl mcl acl] -- rebuild the period-Lisp MAME hard disks from scratch.
# Everything lands under $LISPS (default /mnt/data-fast/claude/rss/lisps):
#   sys6/   System 6.0.8 HD image (archive.org)      unar/  Debian 12 unar/lsar, private prefix
#   in/     copies of the uploaded archives
#   x/      extracted archives and floppies          disks/ WHICH.hfv, WHICH.hd, WHICH.hd.chd
set -eu
LISPS=${LISPS:-/mnt/data-fast/claude/rss/lisps}; export LISPS
HERE=$(cd "$(dirname "$0")" && pwd)
REPO=${REPO:-/home/claude/modus-mac}
UP=${UP:-/mnt/data-fast/claude/rss/romsrc/uploads}
MAME=${MAME:-/mnt/data-fast/claude/rss/mame}
L=$MAME/root/usr/lib/x86_64-linux-gnu
WHICH=${*:-pearl mcl}
mkdir -p $LISPS/{in,x,disks,sys6,unar/debs}

# 1. System 6.0.8 (the same archive.org image the original session used)
if [ ! -f $LISPS/sys6/MacintoshSystem6.0.8HD.img ]; then
  (cd $LISPS/sys6 && curl -sSL -m 600 -o MacintoshSystem6.0.8HD.tar.gz \
     "https://archive.org/download/macintosh-system-6.0.8-hd.tar/MacintoshSystem6.0.8HD.tar.gz" &&
   tar xzf MacintoshSystem6.0.8HD.tar.gz)
fi

# 2. unar/lsar (Debian's `unar` package; it reads StuffIt, Compact Pro and BinHex and
#    writes resource forks as AppleDouble ._ files): UNAR, else the one on PATH, else
#    the private prefix an earlier run of this script unpacked under $LISPS/unar/root
R=$LISPS/unar/root
if [ -n "${UNAR:-}" ]; then unar() { "$UNAR" "$@"; }
elif command -v unar > /dev/null; then :
elif [ -x $R/usr/bin/unar ]; then
  unar() { LD_LIBRARY_PATH=$R/usr/lib:$R/usr/lib/x86_64-linux-gnu:$R/lib/x86_64-linux-gnu $R/usr/bin/unar "$@"; }
else echo "build-disks: no unar: install Debian's unar, or set UNAR"; exit 1; fi
LISP="sbcl --script"; export WORKSPACE=${WORKSPACE:-$REPO/..}

# 3. the uploads, unpacked with resource forks as AppleDouble ._ files
cp -n $UP/e5caa33d-pearllisp11_0.sit $LISPS/in/pearl.sit
cp -n $UP/771caece-MCL_2.0_Images_0.sit $LISPS/in/mcl20.sit
[ -f $UP/efac7ea4-AllegroCommonLISP1.2.2.hqx ] && cp -n $UP/efac7ea4-AllegroCommonLISP1.2.2.hqx $LISPS/in/acl122.hqx || true
cd $LISPS/x
for w in $WHICH; do
  case $w in
  pearl) [ -d pearl ] || unar -q -k hidden -o pearl ../in/pearl.sit ;;
  mcl)
    [ -d mcl20 ] || unar -q -k hidden -o mcl20 ../in/mcl20.sit
    # Disk Copy 4.2 images: strip the 84-byte header -> raw 800K HFS floppies
    for n in 1 2 3; do
      dd if="mcl20/MCL 2.0 Images/MCL 2.0 #$n.image" of=mcl$n.dsk bs=84 skip=1 2>/dev/null; truncate -s 819200 mcl$n.dsk
    done
    $LISP $HERE/mcl-join-cpt.lisp mcl1.dsk mcl2.dsk mcl3.dsk mcl20.cpt
    rm -rf mcl; unar -q -k hidden -o mcl mcl20.cpt > mcl-unar.log 2>&1 || true
    grep -c Failed mcl-unar.log && { echo "unar failures, see $LISPS/x/mcl-unar.log"; exit 1; } || true
    ;;
  acl)
    [ -d acl122 ] || unar -q -k hidden -o acl122 ../in/acl122.hqx
    for n in 1 2; do $LISP $HERE/ndif.lisp "acl122/acl122/Allegro Common LISP $n.img" "acl122/acl122/._Allegro Common LISP $n.img" acl$n.dsk; done ;;
  esac
done

# 4. one System 6 volume per Lisp, then our SCSI-driver HD wrapper and a CHD for MAME
cd $LISPS/disks
for w in $WHICH; do
  $LISP $HERE/mkvol.lisp $w $w.hfv
  rm -f $w.hd $w.hd.chd
  sbcl --script $REPO/scripts/mac68k-hd.lisp $w.hfv $w.hd --chd "env LD_LIBRARY_PATH=$L $MAME/root/usr/bin/chdman" | tail -1
done
ls -la $LISPS/disks
