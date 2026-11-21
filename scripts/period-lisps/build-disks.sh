#!/bin/bash
# build-disks.sh [pearl mcl acl] -- rebuild the period-Lisp MAME hard disks from scratch.
# Everything lands under $LISPS (default /mnt/data-fast/claude/rss/lisps):
#   sys6/   System 6.0.8 HD image (archive.org)      unar/  Debian 12 unar/lsar, private prefix
#   venv/   python venv with machfs                  in/    copies of the uploaded archives
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

# 2. machfs
[ -x $LISPS/venv/bin/python ] || { python3 -m venv $LISPS/venv && $LISPS/venv/bin/pip install -q machfs; }
PY="$LISPS/venv/bin/python -I"

# 3. unar/lsar: Debian 12's package plus whatever dependencies the host lacks
R=$LISPS/unar/root
if [ ! -x $R/usr/bin/unar ]; then
  (cd $LISPS/unar && curl -sSL -o Packages.xz https://deb.debian.org/debian/dists/bookworm/main/binary-amd64/Packages.xz &&
   python3 -I - $LISPS/unar <<'PY'
import lzma, re, subprocess, sys
B = sys.argv[1]
txt = lzma.open(B + '/Packages.xz').read().decode('utf-8', 'replace')
pk = {}
for blk in txt.split('\n\n'):
    f = {}
    for m in re.finditer(r'^([A-Za-z-]+): (.*(?:\n .*)*)', blk, re.M): f[m.group(1)] = m.group(2)
    if 'Package' in f: pk.setdefault(f['Package'], f)
inst = set(subprocess.run(['dpkg-query', '-W', '-f=${Package}\n'], capture_output=True, text=True).stdout.split())
need = ['unar']; seen = set(); files = []
while need:
    n = need.pop()
    if n in seen: continue
    seen.add(n)
    if n in inst and n != 'unar': continue
    f = pk.get(n)
    if not f: print('missing', n); continue
    files.append(f['Filename'])
    for dep in f.get('Depends', '').split(','):
        dep = dep.strip()
        if dep: need.append(dep.split('|')[0].strip().split(' ')[0].split(':')[0])
open(B + '/files.txt', 'w').write('\n'.join(files))
PY
   cd debs && xargs -P 8 -I{} sh -c 'curl -s --retry 3 -O "https://deb.debian.org/debian/{}"' < ../files.txt &&
   for d in *.deb; do dpkg-deb -x $d ../root; done)
fi
unar() { LD_LIBRARY_PATH=$R/usr/lib:$R/usr/lib/x86_64-linux-gnu:$R/lib/x86_64-linux-gnu $R/usr/bin/unar "$@"; }

# 4. the uploads, unpacked with resource forks as AppleDouble ._ files
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
    $PY $HERE/mcl-join-cpt.py mcl1.dsk mcl2.dsk mcl3.dsk mcl20.cpt
    rm -rf mcl; unar -q -k hidden -o mcl mcl20.cpt > mcl-unar.log 2>&1 || true
    grep -c Failed mcl-unar.log && { echo "unar failures, see $LISPS/x/mcl-unar.log"; exit 1; } || true
    ;;
  acl)
    [ -d acl122 ] || unar -q -k hidden -o acl122 ../in/acl122.hqx
    for n in 1 2; do $PY $HERE/ndif.py "acl122/acl122/Allegro Common LISP $n.img" "acl122/acl122/._Allegro Common LISP $n.img" acl$n.dsk; done ;;
  esac
done

# 5. one System 6 volume per Lisp, then our SCSI-driver HD wrapper and a CHD for MAME
cd $LISPS/disks
for w in $WHICH; do
  $PY $HERE/mkvol.py $w $w.hfv
  rm -f $w.hd $w.hd.chd
  sbcl --script $REPO/scripts/mac68k-hd.lisp $w.hfv $w.hd --chd "env LD_LIBRARY_PATH=$L $MAME/root/usr/bin/chdman" | tail -1
done
ls -la $LISPS/disks
