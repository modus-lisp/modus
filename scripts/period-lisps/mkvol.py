"""mkvol.py APPS OUT.hfv [SIZE_MB] [START] -- a System volume holding one or more period
Lisps and/or Modus, set to launch START (default: the first of APPS) at startup.

  APPS   comma-separated: pearl, mcl, acl, modus     e.g.  mcl,modus
  START  which of APPS launches at boot               e.g.  modus
         ("none": none, the Finder alone)

Run with the venv python (needs machfs).  Environment:
  LISPS        base dir (default /mnt/data-fast/claude/rss/lisps): LISPS/sys6/... holds
               System 6.0.8, LISPS/x/... what build-disks.sh extracted
  SYSTEM       608 (default): System 6.0.8; startup app via the boot blocks and the
               volume's Finder info ("Set Startup").
               753: the System Folder of the user's System 7.5.3 image
               ($UPLOADS/857557ab-System7_5_3.img) -- System 6.0.8 refuses to boot on
               68040 Quadras.  System 7's Finder ignores "Set Startup", so a copy of
               START's application also goes into System Folder:Startup Items.
  MODUS_FORKS  directory with data and rsrc of the Modus application, as written by
               scripts/mac68k-chain.py DISK OUT.bin --forks DIR (required for modus)
  VOLNAME      the volume's name (default Macintosh HD)
  MODUS_DESKTOP  V,H: Modus on the desktop (System 7), its icon there, not in a folder
"""
import sys, os, machfs
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__))); import adouble
B = os.environ.get('LISPS', '/mnt/data-fast/claude/rss/lisps')
UP = os.environ.get('UPLOADS', '/mnt/data-fast/claude/rss/romsrc/uploads')
apps, out = sys.argv[1].split(','), sys.argv[2]
size = int(sys.argv[3]) if len(sys.argv) > 3 else 24
startname = sys.argv[4] if len(sys.argv) > 4 else apps[0]
system = os.environ.get('SYSTEM', '608')
sysimg = {'608': B + '/sys6/MacintoshSystem6.0.8HD.img', '753': UP + '/857557ab-System7_5_3.img'}[system]
src = machfs.Volume(); src.read(open(sysimg, 'rb').read())
v = machfs.Volume(); v.name = os.environ.get('VOLNAME', 'Macintosh HD')
v['System Folder'] = src['System Folder']


def add_tree(dst, path):
    """A directory unar wrote with -k hidden (AppleDouble ._ files) into machfs folder DST."""
    for n in sorted(os.listdir(path)):
        if n.startswith('._'): continue
        p = os.path.join(path, n)
        if os.path.isdir(p):
            f = machfs.Folder(); add_tree(f, p); dst[n] = f
        else:
            f = machfs.File(); f.data = open(p, 'rb').read()
            ad = os.path.join(path, '._' + n)
            if os.path.exists(ad):
                m = adouble.parse(open(ad, 'rb').read())
                f.rsrc, f.type, f.creator = m['rsrc'], m['type'], m['creator']
                f.flags = m['flags'] & ~0x0100      # not "inited": the Finder places it
            dst[n] = f


starts = {}
for which in apps:
    if which == 'pearl':
        folder = machfs.Folder(); add_tree(folder, B + '/x/pearl/pearl/Pearl Lisp/Pearl Lisp Disk 1')
        v['Pearl'] = folder; starts[which] = ('Pearl', 'Pearl')
    elif which == 'acl':
        folder = machfs.Folder()
        for d in ('acl1.dsk', 'acl2.dsk'):
            s = machfs.Volume(); s.read(open(B + '/x/' + d, 'rb').read())
            for k, x in s.items(): folder[k] = x
        v['Allegro'] = folder; starts[which] = ('Allegro', 'Allegro Common LISP')
    elif which == 'mcl':
        folder = machfs.Folder(); add_tree(folder, B + '/x/mcl/MCL 2.0')
        v['MCL 2.0'] = folder; starts[which] = ('MCL 2.0', 'MCL 2.0')
    elif which == 'modus':
        d = os.environ['MODUS_FORKS']
        f = machfs.File(); f.type, f.creator = b'APPL', b'MDUS'
        f.flags = 0x2400                    # hasBundle, hasCustomIcon: its "(m)" icon
        f.data = open(d + '/data', 'rb').read(); f.rsrc = open(d + '/rsrc', 'rb').read()
        if os.environ.get('MODUS_DESKTOP'):
            # on the desktop (System 7's Desktop Folder), its icon at V,H
            f.x, f.y = (int(n) for n in os.environ['MODUS_DESKTOP'].split(','))
            folder = machfs.Folder(); folder.flags = 0x4000; folder['Modus'] = f
            v['Desktop Folder'] = folder; starts[which] = ('Desktop Folder', 'Modus')
        else:
            folder = machfs.Folder(); folder['Modus'] = f
            v['Modus'] = folder; starts[which] = ('Modus', 'Modus')
    else:
        raise SystemExit('unknown app %r: pearl, mcl, acl, modus' % which)

start = starts.get(startname)                # START "none": just the Finder
app = v
for part in start or (): app = app[part]
if start and system != '608':
    si = machfs.Folder(); v['System Folder']['Startup Items'] = si
    c = machfs.File(); c.type, c.creator, c.flags = app.type, app.creator, app.flags
    c.data, c.rsrc = app.data, app.rsrc
    si[start[-1]] = c
# System 7: no machfs Desktop DB.  Its empty one is not a B-tree the Finder
# can use: "The disk Macintosh cannot be used, because an error of type -127
# occurred" (fsDSIntErr) the moment the Finder came forward on a Quadra 950
# under 7.5.3.  Without it the Finder builds its own.
open(out, 'wb').write(v.write(size=size * 1024 * 1024, startapp=start, desktopdb=(system == '608')))
if not start: sys.exit(print(out, 'System', system, 'apps', ','.join(apps), 'no startup application'))
print(out, 'System', system, 'apps', ','.join(apps), 'start', '/'.join(start), app.type, app.creator,
      len(app.data), len(app.rsrc))
