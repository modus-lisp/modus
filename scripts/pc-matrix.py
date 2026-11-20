#!/usr/bin/env python3
"""pc-matrix.py DISK [CELL-PATTERN ...] [-j N] [--list] [--keep]

Boot a pcboot disk (scripts/pcboot.sh) on modern PC models and check that the
REPL ANSWERS, not merely that it starts.  The 86Box matrix
(scripts/dos386-86box/) covers real 1990s BIOSes; this covers what came after
them, as far as an emulator can:

  QEMU   pc (i440FX + PIIX) and q35 (ICH9) chipsets; CPUs from the n270 Atom and
         Core 2 through Sandy Bridge (the ThinkPad T420), Haswell, Skylake, Ice
         Lake and EPYC; the BIOS (SeaBIOS) booting from IDE, AHCI, NVMe,
         virtio, SCSI and a USB stick on UHCI, EHCI or xHCI; RAM from 6 MB to
         8 GB (above 4 GB the PCI hole splits it); 4 CPUs.  Input is the PS/2
         keyboard (QMP send-key) and output is read out of VGA text memory --
         a laptop's own console, no serial port.
  Bochs  its own CPU models, Pentium to Tiger Lake and Ryzen, under its own
         legacy BIOS.  Input is the PS/2 keyboard through Bochs's VNC server
         (key events, so its 8042 model is in the loop), output is COM1.

Every cell runs the same forms and must produce each one's expected answer.
It also reports what the stub made of the machine: the RAM it found, the
heap's semispace S and the code pool (stub variables at 0x8008/0x8010/0x8070).

  --list   print the cells and exit
  -j N     cells in parallel (default: half the CPUs)
  --keep   keep each cell's transcript in /dev/shm/pcm-CELL.txt

Exit 0 only if every selected cell passes."""
import fnmatch, json, os, shutil, socket, struct, subprocess, sys, tempfile, threading, time

QEMU = shutil.which('qemu-system-x86_64') or '/usr/bin/qemu-system-x86_64'
BOCHS = shutil.which('bochs')
BOCHS_SHARE = '/usr/share/bochs'

# (form, expected text in its answer).  Short enough not to wrap on 80 columns.
FORMS = [
    ('(+ 1 2)', '3'),
    ('(length (make-list 10000))', '10000'),
    ('(expt 2 70)', '1180591620717411303424'),
    ('(string-upcase "modus")', '"MODUS"'),
    ('(* 1.5d0 2)', '3.0d0'),
    ('(dotimes (i 100) (make-list 10000))', 'NIL'),
    ('(let ((h (make-hash-table))) (setf (gethash 1 h) 2) (gethash 1 h))', '2'),
]
PROBE = '(list (mem-ref #x8008 :u32) (mem-ref #x8010 :u32) (mem-ref #x8070 :u32))'

# ---------------------------------------------------------------- the cells
def qcell(name, machine='q35', cpu='SandyBridge', mem='64', disk='sata', smp=1, note=''):
    return dict(name=name, emu='qemu', machine=machine, cpu=cpu, mem=mem, disk=disk, smp=smp, note=note)
def bcell(name, cpu, mem=32, note=''):
    return dict(name=name, emu='bochs', cpu=cpu, mem=mem, note=note)

CELLS = [
    # the ThinkPad T420: Sandy Bridge, ICH9-class chipset, booted off a USB 2 stick, 8 GB, 4 threads
    qcell('t420', cpu='SandyBridge', mem='8G', disk='usb-ehci', smp=4, note='ThinkPad T420 profile'),
    # boot devices (SandyBridge, 64 MB)
    qcell('dev-ide',      machine='pc', disk='ide'),
    qcell('dev-sata',     disk='sata'),
    qcell('dev-nvme',     disk='nvme'),
    qcell('dev-virtio',   disk='virtio'),
    qcell('dev-scsi',     disk='lsi'),
    qcell('dev-usb-uhci', machine='pc', disk='usb-uhci'),
    qcell('dev-usb-ehci', disk='usb-ehci'),
    qcell('dev-usb-xhci', disk='usb-xhci'),
    # CPUs (q35, SATA, 64 MB)
    qcell('cpu-n270',      cpu='n270',  note='Atom netbook'),
    qcell('cpu-core2duo',  cpu='core2duo'),
    qcell('cpu-penryn',    cpu='Penryn'),
    qcell('cpu-nehalem',   cpu='Nehalem'),
    qcell('cpu-westmere',  cpu='Westmere'),
    qcell('cpu-haswell',   cpu='Haswell-noTSX'),
    qcell('cpu-skylake',   cpu='Skylake-Client-noTSX-IBRS'),
    qcell('cpu-icelake',   cpu='Icelake-Server-noTSX'),
    qcell('cpu-athlon',    cpu='athlon'),
    qcell('cpu-phenom',    cpu='phenom'),
    qcell('cpu-opteron5',  cpu='Opteron_G5'),
    qcell('cpu-epyc',      cpu='EPYC-Milan'),
    # RAM (q35, SandyBridge, SATA)
    qcell('ram-6m',   mem='6'),
    qcell('ram-8m',   mem='8'),
    qcell('ram-16m',  mem='16'),
    qcell('ram-512m', mem='512'),
    qcell('ram-2g',   mem='2G'),
    qcell('ram-3g',   mem='3G', note='q35 lowmem ends at 2 GB'),
    qcell('ram-8g',   mem='8G', note='above 4 GB'),
    qcell('ram-8g-pc', machine='pc', mem='8G', disk='ide', note='i440FX lowmem 3.5 GB'),
    qcell('smp-4',    smp=4),
    # Bochs's own CPU models and BIOS
    bcell('bochs-p2',       'p2_klamath'),
    bcell('bochs-p4',       'p4_prescott_celeron_336'),
    bcell('bochs-atom',     'atom_n270'),
    bcell('bochs-athlon64', 'athlon64_venice'),
    bcell('bochs-core2',    'core2_penryn_t9600'),
    bcell('bochs-arrandale','corei5_arrandale_m520'),
    bcell('bochs-sandy',    'corei7_sandy_bridge_2600k', note='T420 generation'),
    bcell('bochs-sandy-8m', 'corei7_sandy_bridge_2600k', mem=8),
    bcell('bochs-ivy',      'corei7_ivy_bridge_3770k'),
    bcell('bochs-haswell',  'corei7_haswell_4770'),
    bcell('bochs-skylake',  'corei7_skylake_x'),
    bcell('bochs-icelake',  'corei7_icelake_u'),
    bcell('bochs-tigerlake','tigerlake'),
    bcell('bochs-ryzen',    'ryzen'),
    bcell('bochs-sandy-512m','corei7_sandy_bridge_2600k', mem=512),
]

# ------------------------------------------------------------ the keyboard
SHIFTED = {'!': '1', '@': '2', '#': '3', '$': '4', '%': '5', '^': '6', '&': '7', '*': '8',
           '(': '9', ')': '0', '_': '-', '+': '=', '{': '[', '}': ']', '|': '\\', ':': ';',
           '"': "'", '<': ',', '>': '.', '?': '/', '~': '`'}
QCODE = {'-': 'minus', '=': 'equal', '[': 'bracket_left', ']': 'bracket_right', '\\': 'backslash',
         ';': 'semicolon', "'": 'apostrophe', ',': 'comma', '.': 'dot', '/': 'slash',
         '`': 'grave_accent', ' ': 'spc', '\n': 'ret'}

def keystrokes(ch):
    """(shift?, base character) for a US layout."""
    if ch in SHIFTED: return True, SHIFTED[ch]
    if ch.isupper(): return True, ch.lower()
    return False, ch

# -------------------------------------------------------------- the answer
def answer_after(text, form):
    """The REPL's answer to FORM in TEXT: what follows the form's last echo, up to the next prompt."""
    at = text.rfind(form)
    if at < 0: return None
    rest = text[at + len(form):]
    end = rest.find('\n>')
    return (rest if end < 0 else rest[:end]).strip()

# ------------------------------------------------------------------- QEMU
class Qemu:
    def __init__(self, cell, disk, tmp):
        self.sock = os.path.join(tmp, 'qmp')
        self.vga = os.path.join(tmp, 'vga.bin')
        drive = 'file=%s,format=raw,snapshot=on' % disk
        d = cell['disk']
        if d == 'ide':        dev = ['-drive', drive + ',if=ide']
        elif d == 'sata':     dev = ['-drive', drive + ',if=none,id=d0', '-device', 'ide-hd,drive=d0,bus=ide.0']
        elif d == 'nvme':     dev = ['-drive', drive + ',if=none,id=d0', '-device', 'nvme,drive=d0,serial=modus']
        elif d == 'virtio':   dev = ['-drive', drive + ',if=virtio']
        elif d == 'lsi':      dev = ['-device', 'lsi53c895a,id=scsi0', '-drive', drive + ',if=none,id=d0',
                                     '-device', 'scsi-hd,drive=d0,bus=scsi0.0']
        elif d.startswith('usb-'):
            hc = {'usb-uhci': 'piix3-usb-uhci', 'usb-ehci': 'ich9-usb-ehci1', 'usb-xhci': 'qemu-xhci'}[d]
            dev = ['-device', hc + ',id=hc', '-drive', drive + ',if=none,id=d0',
                   '-device', 'usb-storage,drive=d0,bus=hc.0,bootindex=0']
        else: raise ValueError(d)
        args = [QEMU, '-machine', cell['machine'], '-cpu', cell['cpu'], '-m', cell['mem'],
                '-smp', str(cell['smp']), '-vga', 'std', '-display', 'none', '-serial', 'null',
                '-no-reboot', '-qmp', 'unix:%s,server=on,wait=off' % self.sock] + dev
        self.p = subprocess.Popen(args, stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL,
                                  stderr=open(os.path.join(tmp, 'stderr'), 'w'))
        t0 = time.time()
        while not os.path.exists(self.sock):
            if self.p.poll() is not None or time.time() - t0 > 20:
                raise RuntimeError('qemu did not start: ' + open(os.path.join(tmp, 'stderr')).read()[-300:])
            time.sleep(0.05)
        self.s = socket.socket(socket.AF_UNIX); self.s.connect(self.sock); self.f = self.s.makefile('rw')
        self.f.readline(); self.cmd('qmp_capabilities')
    def cmd(self, c, **a):
        self.f.write(json.dumps({'execute': c, 'arguments': a} if a else {'execute': c}) + '\n'); self.f.flush()
        while True:
            r = json.loads(self.f.readline())
            if 'return' in r or 'error' in r: return r
    def screen(self):
        self.cmd('pmemsave', val=0xB8000, size=4000, filename=self.vga)
        d = open(self.vga, 'rb').read()
        return '\n'.join(''.join(chr(d[(r * 80 + c) * 2]) if 32 <= d[(r * 80 + c) * 2] < 127 else ' '
                                 for c in range(80)).rstrip() for r in range(25))
    def text(self): return self.screen()
    def type(self, t):
        for ch in t:
            shift, base = keystrokes(ch)
            k = QCODE.get(base, base)
            self.cmd('send-key', keys=([{'type': 'qcode', 'data': 'shift'}] if shift else []) +
                     [{'type': 'qcode', 'data': k}])
            time.sleep(0.03)
    def at_prompt(self):
        return self.screen().rstrip().endswith('>')
    def close(self):
        try: self.cmd('quit')
        except Exception: pass
        try: self.p.wait(5)
        except Exception: self.p.kill()

# ------------------------------------------------------------------ Bochs
XK = {'\n': 0xFF0D, ' ': 0x20}
XK_SHIFT = 0xFFE1

class Bochs:
    def __init__(self, cell, disk, tmp, port):
        size = os.path.getsize(disk)
        rc = os.path.join(tmp, 'bochsrc')
        open(rc, 'w').write('\n'.join([
            'megs: %d' % cell['mem'],
            'cpu: model=%s, ips=200000000' % cell['cpu'],
            'romimage: file=%s/BIOS-bochs-latest' % BOCHS_SHARE,
            'vgaromimage: file=%s/VGABIOS-lgpl-latest' % BOCHS_SHARE,
            'ata0-master: type=disk, path=%s, mode=flat, cylinders=%d, heads=16, spt=63'
            % (disk, size // (16 * 63 * 512)),
            'boot: disk',
            'com1: enabled=1, mode=socket-server, dev=localhost:%d' % port,
            'display_library: rfb, options="timeout=0"',
            'clock: sync=none',
            'speaker: enabled=0',
            'log: %s' % os.path.join(tmp, 'bochs.log'),
            'panic: action=fatal', 'error: action=report', '']))
        cmds = os.path.join(tmp, 'cmds'); open(cmds, 'w').write('c\n')
        # the disk is opened read-write: give each cell its own copy
        self.p = subprocess.Popen([BOCHS, '-q', '-f', rc, '-rc', cmds], cwd=tmp,
                                  stdin=subprocess.DEVNULL, stdout=open(os.path.join(tmp, 'stdout'), 'w'),
                                  stderr=subprocess.STDOUT)
        t0 = time.time(); self.s = None
        while self.s is None:
            try: self.s = socket.create_connection(('localhost', port))
            except OSError:
                if self.p.poll() is not None or time.time() - t0 > 30:
                    out = open(os.path.join(tmp, 'stdout'), errors='replace').read()
                    why = [l for l in out.splitlines() if l.startswith('[') or '>>PANIC<<' in l]
                    raise RuntimeError('bochs exited: ' + (why[-1].strip() if why else out[-200:]))
                time.sleep(0.2)
        self.s.setblocking(False); self.buf = b''
        self.vnc = None; self.vnc_t0 = time.time()
    def pump(self):
        try:
            while True:
                b = self.s.recv(65536)
                if not b: break
                self.buf += b
        except (BlockingIOError, InterruptedError): pass
    def text(self):
        self.pump(); return self.buf.decode('latin1').replace('\r', '')
    def at_prompt(self):
        return self.text().rstrip().endswith('>')
    def rfb(self):
        """Connect to Bochs's VNC server once the guest is up: its port is the
           one the bochs process listens on (5900 and up, the first free one)."""
        if self.vnc: return self.vnc
        out = subprocess.run(['ss', '-ltnpH'], capture_output=True, text=True).stdout
        ports = [int(l.split()[3].rsplit(':', 1)[1]) for l in out.splitlines() if 'pid=%d,' % self.p.pid in l]
        ports = [p for p in ports if 5900 <= p < 6000]
        if not ports: raise RuntimeError('no VNC port for bochs pid %d' % self.p.pid)
        v = socket.create_connection(('localhost', ports[0])); v.settimeout(10)
        ver = v.recv(12); v.sendall(ver[:12])
        sec = v.recv(4)
        if struct.unpack('>I', sec)[0] != 1: raise RuntimeError('VNC security %r' % sec)
        v.sendall(b'\x01')                       # ClientInit: shared
        hdr = b''
        while len(hdr) < 24: hdr += v.recv(24 - len(hdr))
        nlen = struct.unpack('>I', hdr[20:24])[0]
        name = b''
        while len(name) < nlen: name += v.recv(nlen - len(name))
        v.setblocking(False)                     # the framebuffer updates it may send are not read
        self.vnc = v; return v
    def key(self, sym, down):
        self.vnc.sendall(struct.pack('>BBHI', 4, 1 if down else 0, 0, sym))
    def type(self, t):
        self.rfb()
        for ch in t:
            shift, base = keystrokes(ch)
            sym = XK.get(ch, ord(ch))
            if shift: self.key(XK_SHIFT, True)
            self.key(sym, True); self.key(sym, False)
            if shift: self.key(XK_SHIFT, False)
            time.sleep(0.03)
            try: self.vnc.recv(1 << 20)          # drain framebuffer traffic
            except (BlockingIOError, InterruptedError): pass
    def close(self):
        self.p.kill()
        try: self.p.wait(5)
        except Exception: pass

# ---------------------------------------------------------------- one cell
def run_cell(cell, disk, idx, keep, forms=None):
    tmp = tempfile.mkdtemp(prefix='pcm-', dir='/dev/shm')
    res = dict(name=cell['name'], ok=False, detail='', boot=None, probe='')
    m = None; log = []
    try:
        t0 = time.time()
        if cell['emu'] == 'qemu':
            m = Qemu(cell, disk, tmp)
        else:
            # its own copy (Bochs opens the disk read-write), padded to whole
            # cylinders of 16 x 63, the only geometry it takes for a flat image
            own = os.path.join(tmp, 'disk.img'); shutil.copyfile(disk, own)
            cyl = 16 * 63 * 512
            with open(own, 'r+b') as f: f.truncate(-(-os.path.getsize(own) // cyl) * cyl)
            m = Bochs(cell, own, tmp, 24000 + idx)
        while not m.at_prompt():
            if time.time() - t0 > 180:
                res['detail'] = 'no prompt in 180 s: ' + ' / '.join(m.text().strip().splitlines()[-3:])
                return res
            time.sleep(1)
        res['boot'] = time.time() - t0
        bad = []
        for form, want in (forms or FORMS) + [(PROBE, None)]:
            m.type(form + '\n'); t1 = time.time()
            while True:
                time.sleep(0.5)
                txt = m.text(); a = answer_after(txt, form)
                if a is not None and txt.rstrip().endswith('>') and a != '': break
                if time.time() - t1 > 300: a = '(no answer in 300 s)'; break
            log.append('%s => %s  [%.1f s]' % (form, a, time.time() - t1))
            if want is None:
                res['probe'] = a
            elif want not in (a or '').split():
                bad.append('%s => %s' % (form, a))
        res['ok'] = not bad
        res['detail'] = '; '.join(bad)
        return res
    except Exception as e:
        if m and getattr(m, 'p', None) and m.p.poll() is not None:
            res['detail'] = 'the emulator exited (code %s): the guest reset or halted' % m.p.returncode
        else:
            res['detail'] = 'harness: %s' % e
        return res
    finally:
        last = ''
        if m:
            try: last = m.text()
            except Exception: pass
            m.close()
        res['screen'] = last
        if keep:
            open('/dev/shm/pcm-%s.txt' % cell['name'], 'w').write('\n'.join(log) + '\n\n' + last)
        res['log'] = log
        shutil.rmtree(tmp, ignore_errors=True)

def probe_text(p):
    try:
        ram, s, pool = [int(x) for x in p.strip('()').split()]
        return 'RAM %s, S %.1f MB, pool %.2f MB' % (('%.0f MB' % (ram / 1048576)), s / 1048576, pool / 1048576)
    except Exception:
        return p or ''

def main():
    argv = sys.argv[1:]
    if '--list' in argv:
        for c in CELLS:
            print('%-18s %-5s %s' % (c['name'], c['emu'], ' '.join('%s=%s' % (k, v) for k, v in c.items()
                                                               if k not in ('name', 'emu', 'note') and v != '')
                                      + ('   # ' + c['note'] if c.get('note') else '')))
        return 0
    keep = '--keep' in argv; argv = [a for a in argv if a != '--keep']
    jobs = max(1, (os.cpu_count() or 2) // 2)
    if '-j' in argv:
        i = argv.index('-j'); jobs = int(argv[i + 1]); del argv[i:i + 2]
    if not argv: print(__doc__); return 2
    disk, pats = argv[0], argv[1:] or ['*']
    cells = [c for c in CELLS if any(fnmatch.fnmatch(c['name'], p) for p in pats)]
    if any(c['emu'] == 'bochs' for c in cells) and not BOCHS:
        print('bochs is not installed: skipping its cells'); cells = [c for c in cells if c['emu'] != 'bochs']
    if not cells: print('no cell matches'); return 2
    # THE HARNESS MUST BE ABLE TO FAIL: one cell, with (+ 1 2) expected to be 4,
    # must come back FAIL on exactly that form, or nothing below means anything.
    ctl = run_cell(cells[0], os.path.abspath(disk), 99, False, [('(+ 1 2)', '4')] + FORMS[1:2])
    if ctl['ok'] or not ctl['detail'].startswith('(+ 1 2) => 3'):
        print('CONTROL DID NOT FAIL AS REQUIRED (%s: %s): refusing the matrix'
              % (cells[0]['name'], ctl['detail'] or 'passed')); return 2
    print('control: %s answered (+ 1 2) => 3 against an expected 4, FAIL as required' % cells[0]['name'])
    results = [None] * len(cells); lock = threading.Lock(); nxt = [0]
    def worker():
        while True:
            with lock:
                if nxt[0] >= len(cells): return
                i = nxt[0]; nxt[0] += 1
            r = run_cell(cells[i], os.path.abspath(disk), i, keep); results[i] = r
            with lock:
                print('%-18s %-4s %6s  %-40s %s' % (r['name'], 'pass' if r['ok'] else 'FAIL',
                      '%.0fs' % r['boot'] if r['boot'] else '-', probe_text(r['probe']), r['detail']), flush=True)
    ts = [threading.Thread(target=worker) for _ in range(min(jobs, len(cells)))]
    for t in ts: t.start()
    for t in ts: t.join()
    n = sum(1 for r in results if r['ok'])
    print('%d / %d cells pass' % (n, len(results)))
    return 0 if n == len(results) else 1

if __name__ == '__main__':
    sys.exit(main())
