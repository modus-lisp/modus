"""box.py -- drive one 86Box machine through the automation socket.

Shared by matrix.py and golden.py: start 86Box on a VM directory, send
requests, read the text screen with its attributes, press keys by name, and
find and move the highlight in a BIOS setup menu.
"""
import collections, json, os, re, socket, subprocess, time

EXE = os.environ.get('EIGHTYSIX', os.path.expanduser('~/src/86Box/build/src/86Box'))
ROMS = os.environ.get('ROMS', os.path.expanduser('~/86Box-roms'))

# Set-1 make codes; 0x100 marks an E0-prefixed key.
KEYS = {'esc': 0x01, 'enter': 0x1C, 'space': 0x39, 'tab': 0x0F, 'y': 0x15, 'n': 0x31,
        'f1': 0x3B, 'f2': 0x3C, 'f3': 0x3D, 'f4': 0x3E, 'f5': 0x3F, 'f6': 0x40,
        'f7': 0x41, 'f8': 0x42, 'f9': 0x43, 'f10': 0x44, 'f11': 0x57, 'f12': 0x58, 'del': 0x153,
        'up': 0x148, 'down': 0x150, 'left': 0x14B, 'right': 0x14D,
        'pgup': 0x149, 'pgdn': 0x151, 'home': 0x147, 'end': 0x14F,
        'plus': 0x4E, 'minus': 0x4A, 'lshift': 0x2A}
for i, ch in enumerate('1234567890'):
    KEYS[ch] = 0x02 + i
for row, base in (('qwertyuiop', 0x10), ('asdfghjkl', 0x1E), ('zxcvbnm', 0x2C)):
    for i, ch in enumerate(row):
        KEYS.setdefault(ch, base + i)


# POST prompts that let the boot go on, and the key each names.
CONTINUE = [(re.compile(r'<?ESC>?\s*to\s*(Boot|continue|skip)', re.I), 'esc'),
            (re.compile(r'<?F1>?\s*(key\s*)?to\s*(continue|resume)', re.I), 'f1'),
            (re.compile(r'<?F2>?\s*to\s*(continue|resume|load default)', re.I), 'f2')]


# MODUS.COM's refusals (scripts/dos386-chain.s: each error is "MODUS: " and
# one of these); "MODUS: no FPU, so ..." is not one.
MODUS_REFUSED = re.compile(r'MODUS: (needs|the CPU|cannot|no multiboot|the multiboot|the image|the BIOS|not enough)')


def continue_key(scr):
    """The key a POST error screen asks for to boot on, or None."""
    if scr.ok:
        t = scr.text()
        for pat, key in CONTINUE:
            if pat.search(t):
                return key
    return None


class Screen:
    def __init__(self, r):
        self.ok = bool(r.get('ok'))
        self.error = r.get('error')
        self.rows = [bytes.fromhex(x).decode('cp437') for x in r.get('chars', [])]
        self.attrs = [bytes.fromhex(x) for x in r.get('attrs', [])]
        self.cursor = r.get('cursor')

    def text(self):
        if not self.ok:
            return '(not a text mode: %s)' % self.error
        return '\n'.join(l.rstrip() for l in self.rows)

    def __contains__(self, s):
        return s in self.text()

    def search(self, pattern, flags=re.I):
        return re.search(pattern, self.text(), flags)

    def highlight(self):
        """The highlighted item: (row, col0, col1, text).  Among the runs of
        cells with a background other than the screen's usual one and with
        letters in them, the one whose attribute is rarest on the screen (a
        title bar has the same odd background, but over whole rows)."""
        if not self.ok:
            return None
        cells = collections.Counter(a for row in self.attrs for a in row)
        # All four background bits: AMI's WinBIOS turns blink off and marks
        # its item with a bright background (F0h against 0Fh's black).
        bg = collections.Counter(a >> 4 for row in self.attrs for a in row).most_common(1)[0][0]
        best, key = None, None
        for y, (row, at) in enumerate(zip(self.rows, self.attrs)):
            x = 0
            while x < len(at):
                if (at[x] >> 4) != bg:
                    x0 = x
                    while x < len(at) and at[x] == at[x0]:
                        x += 1
                    text = row[x0:x].strip()
                    k = (cells[at[x0]], -len(text))
                    if re.search('[A-Za-z]{2}', text) and (key is None or k < key):
                        best, key = (y, x0, x, text), k
                else:
                    x += 1
        return best

    def find(self, pattern):
        """(row, col) of the first match of PATTERN on a single row."""
        for y, row in enumerate(self.rows):
            m = re.search(pattern, row, re.I)
            if m:
                return y, m.start()
        return None


class Box:
    def __init__(self, vm, cfg='86box.cfg', hold=False, log=None):
        self.vm = os.path.abspath(vm)
        self.sock = os.path.join(self.vm, 'ctl.sock')
        if os.path.exists(self.sock):
            os.unlink(self.sock)
        env = dict(os.environ, SDL_VIDEODRIVER='dummy', SDL_AUDIODRIVER='dummy')
        args = [EXE, '-R', ROMS, '-P', self.vm, '-N', '--automation', 'unix:' + self.sock]
        if hold:
            args.append('--automation-hold')
        self.p = subprocess.Popen(args + [os.path.join(self.vm, cfg)],
                                  stdout=open(os.path.join(self.vm, '86box.out'), 'w'),
                                  stderr=subprocess.STDOUT, env=env, start_new_session=True)
        self.log = log
        self.s = socket.socket(socket.AF_UNIX)
        for _ in range(150):
            try:
                self.s.connect(self.sock)
                break
            except OSError:
                if self.p.poll() is not None:
                    raise RuntimeError('86Box exited %s' % self.p.returncode)
                time.sleep(0.2)
        else:
            raise RuntimeError('no automation socket')
        self.f = self.s.makefile('rw')

    def __call__(self, cmd):
        self.f.write(cmd + '\n')
        self.f.flush()
        line = self.f.readline()
        if not line:
            raise RuntimeError('86Box went away')
        return json.loads(line)

    def screen(self):
        r = self('text')
        if r.get('ok') and not any(int(x, 16) for x in r['chars']):
            # Blank, as `text` reads it (the bus's view of B8000h): on some
            # chipsets (Intel 450KX) only the processor's view reaches the
            # VGA there.  Read that, at the CRTC's start address.
            self('iow 0x3d4 1 0x0c')
            hi = self('ior 0x3d5 1')['values'][0]
            self('iow 0x3d4 1 0x0d')
            lo = self('ior 0x3d5 1')['values'][0]
            cols, rows = r.get('cols', 80), r.get('rows', 25)
            m = self('memr 0x%x %d cpu' % (0xB8000 + 2 * (hi << 8 | lo), 2 * cols * rows))
            if m.get('ok'):
                d = bytes.fromhex(m['data'])
                r['chars'] = [d[2 * cols * y:2 * cols * (y + 1):2].hex() for y in range(rows)]
                r['attrs'] = [d[2 * cols * y + 1:2 * cols * (y + 1):2].hex() for y in range(rows)]
        return Screen(r)

    def step(self, ms):
        return self('step %d' % ms)

    def keys(self, *names, gap=150):
        """Press and release each key in turn, GAP emulated ms apart."""
        for n in names:
            code = KEYS[n.lower()] if isinstance(n, str) else n
            self('key down 0x%x' % code)
            self('key up 0x%x' % code)
            if gap:
                self.step(gap)
        if self.log:
            self.log('keys ' + ' '.join(map(str, names)))

    def wait_screen(self, pred, timeout_ms, poll_ms=500):
        """Run until PRED(screen) is true; the screen, or None at the timeout."""
        t_end = self('status')['ms'] + timeout_ms
        while True:
            scr = self.screen()
            if pred(scr):
                return scr
            if self.step(poll_ms)['ms'] > t_end:
                return None

    def goto(self, pattern, tries=40):
        """Move a setup menu's highlight onto the item matching PATTERN.
        False if it is not on the screen, or the highlight stops moving."""
        for _ in range(tries):
            scr = self.screen()
            hl = scr.highlight()
            if hl and re.search(pattern, hl[3], re.I):
                return True
            at = scr.find(pattern)
            if not at or not hl:
                return False
            y, x = at
            if y < hl[0]:
                self.keys('up')
            elif y > hl[0]:
                self.keys('down')
            elif x < hl[1]:
                self.keys('left')
            else:
                self.keys('right')
            if self.screen().highlight() == hl:
                return False
        return False

    def com1(self, frm=0):
        r = self('serial read 1 %d' % frm)
        return bytes.fromhex(r.get('data', '')).decode('latin1'), r.get('next', frm)

    def close(self):
        try:
            self('quit')
        except Exception:
            pass
        try:
            self.p.wait(timeout=30)
        except subprocess.TimeoutExpired:
            self.p.kill()
