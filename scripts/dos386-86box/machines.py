"""machines.py [SRC [ROMS]] -- the 86Box machine table as JSON, for matrix.py.

Reads SRC/src/machine/machine_table.c (default ~/src/86Box) and prints one
object per machine: internal name, display name, type (MACHINE_TYPE_*), bus
and feature flags, RAM limits in KB, whether it has its own video, IDE and
FDC, the ROM files its init function loads (from SRC/src/machine/*.c), and
whether they are all present under ROMS (default ~/86Box-roms).
"""
import glob, json, os, re, sys

src = os.path.expanduser(sys.argv[1] if len(sys.argv) > 1 else '~/src/86Box')
roms = os.path.expanduser(sys.argv[2] if len(sys.argv) > 2 else '~/86Box-roms')
table = open(os.path.join(src, 'src/machine/machine_table.c')).read()

# init function -> ROM paths it names
rom_of = {}
for fn in glob.glob(os.path.join(src, 'src/machine/*.c')):
    text = open(fn, errors='replace').read()
    for m in re.finditer(r'\n(?:int|static int)\s*\n?(machine_\w+_init)\s*\(([^)]*)\)\s*\{(.*?)\n\}', text, re.S):
        rom_of[m.group(1)] = re.findall(r'"(roms/[^"]+)"', m.group(3))


def field(entry, name):
    m = re.search(r'\.' + name + r'\s*=\s*([^,\n]+)', entry)
    return m.group(1).strip() if m else None


out = []
for entry in re.split(r'\n    \{\n', table):
    name = re.search(r'\.name\s*=\s*"([^"]*)"', entry)
    iname = re.search(r'\.internal_name\s*=\s*"([^"]*)"', entry)
    if not (name and iname) or iname.group(1) == 'none':
        continue
    ram = re.search(r'\.ram\s*=\s*\{(.*?)\}', entry, re.S)
    rmin = rmax = None
    if ram:
        lo = re.search(r'\.min\s*=\s*([0-9x* ]+)', ram.group(1))
        hi = re.search(r'\.max\s*=\s*([0-9x* ]+)', ram.group(1))
        rmin = eval(lo.group(1)) if lo else None
        rmax = eval(hi.group(1)) if hi else None
    flags = field(entry, 'flags') or ''
    init = field(entry, 'init')
    files = rom_of.get(init, [])
    present = None if not files else all(os.path.exists(os.path.join(roms, f[len('roms/'):])) for f in files)
    out.append({'iname': iname.group(1), 'name': name.group(1),
                'type': (field(entry, 'type') or '').replace('MACHINE_TYPE_', ''),
                'bus': field(entry, 'bus_flags'), 'flags': flags,
                'ide': 'MACHINE_IDE' in flags, 'video': 'MACHINE_VIDEO' in flags,
                'fdc': 'MACHINE_FDC' in flags or 'MACHINE_SUPER_IO' in flags,
                'ram_min': rmin, 'ram_max': rmax, 'init': init, 'roms': files, 'present': present})
json.dump(out, sys.stdout, indent=1)
