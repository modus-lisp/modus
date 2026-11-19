"""golden.py VMDIR -- configure a machine's BIOS once, the way a person would.

A machine with no saved CMOS boots with its BIOS's power-on defaults, which on
most boards before about 1997 means no hard disk.  This enters the BIOS's own
setup program, loads its defaults, has it detect the IDE disk, and saves, then
keeps going until DOS has run MODUS.COM.  VMDIR/nvr/ then holds the machine's CMOS
as an owner would have left it; matrix.py copies it into each run.

Recognised setup programs: Award (4.x menu grid; 6.0 "Phoenix - AwardBIOS"),
AMI Hi-Flex and WinBIOS, and the prompt-driven POST screens in front of them;
any other screen with a save-and-exit key gets F9 (defaults) and F10.  With no
setup found it stops with stage 'unknown-setup' and the screen in VMDIR/golden.log.

Prints one JSON line: stage ('dos': MODUS.COM started the image), what was done, the last screen.
"""
import json, os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from box import Box, continue_key, MODUS_REFUSED

DOS_UP = re.compile(r'MODUS: starting|' + MODUS_REFUSED.pattern)


class Golden:
    def __init__(self, vm):
        self.vm = vm
        self.logf = open(os.path.join(vm, 'golden.log'), 'w')
        self.b = Box(vm, hold=True, log=self.log)
        self.seed_cmos()
        self.done = []

    def log(self, msg):
        self.logf.write(msg + '\n')
        self.logf.flush()

    def snap(self, why):
        s = self.b.screen()
        self.log('---- %s @%s ms' % (why, self.b('status')['ms']))
        self.log(s.text())
        return s

    def settle(self, ms=1500):
        self.b.step(ms)
        return self.b.screen()

    def answer_yes(self):
        """A (Y/N) prompt: type Y, then Enter if the prompt is still there."""
        self.b.keys('y', gap=300)
        s = self.b.screen()
        if s.search(r'\(Y/N\)'):
            self.b.keys('enter', gap=1000)
        for _ in range(3):
            if not self.b.screen().search(r'press any key'):
                break
            self.b.keys('space', gap=1000)

    def seed_cmos(self):
        """Before the first instruction: the standard AT CMOS bytes that say
        what is plugged in -- one 1.44 MB drive A:, a VGA, 640 KB of base
        memory, the disk as drive C: -- with the 10h-2Dh checksum over them.  A BIOS whose own
        checks still fail loads its defaults, as without this; the AMI BIOSes
        take these and so know about the floppy."""
        b = self.b

        def rd(r):
            b('iow 0x70 1 %d' % r)
            return b('ior 0x71 1')['values'][0]

        def wr(r, v):
            b('iow 0x70 1 %d' % r)
            b('iow 0x71 1 %d' % v)

        wr(0x10, 0x40)                          # A: 1.44 MB, B: none
        wr(0x14, (rd(0x14) & 0x0E) | 0x01)      # one floppy drive, EGA/VGA
        wr(0x15, 0x80)
        wr(0x16, 0x02)
        wr(0x0E, 0x00)
        # Drive C: as type 47 (user-defined) with the disk's own geometry, in
        # the AMI / Award layout: BIOSes without auto-detection (AMI 1990)
        # keep it, the rest replace it with what they detect.
        m = re.search(r'hdd_01_parameters\s*=\s*(\d+),\s*(\d+),\s*(\d+)',
                      open(os.path.join(self.vm, '86box.cfg')).read())
        if m:
            spt, heads, cyl = map(int, m.groups())
            wr(0x12, (rd(0x12) & 0x0F) | 0xF0)
            wr(0x19, 47)
            for i, v in enumerate([cyl & 0xFF, cyl >> 8, heads, 0xFF, 0xFF, 0x08 if heads > 8 else 0,
                                   cyl & 0xFF, cyl >> 8, spt]):
                wr(0x1B + i, v)
        s = sum(rd(r) for r in range(0x10, 0x2E))
        wr(0x2E, s >> 8 & 0xFF)
        wr(0x2F, s & 0xFF)
        b('release')

    # --- POST -------------------------------------------------------------

    def enter_setup(self, timeout_ms=60000):
        """Get from power-on into a setup program.  The screen, or None.
        Press the key the POST screen names; press DEL blind only after 4 s
        with no prompt, once a second (a key held during the keyboard test
        is a "stuck key" error)."""
        b = self.b
        t0 = b('status')['ms']
        last_blind = 0
        while True:
            now = b('status')['ms']
            if now > t0 + timeout_ms:
                return None
            s = b.screen()
            if self.family(s):
                return s
            t = s.text() if s.ok else ''
            m = re.search(r'<?(DEL|F1|F2|ESC|F10)>?\s*(?:key\s*)?(?:to|for|if you want to)\s*(?:enter\s*|run\s*)?(?:the\s*)?SETUP', t, re.I)
            if m:
                self.log('POST asks for %s' % m.group(1))
                b.keys(m.group(1).lower(), gap=300)
            elif re.search(r'RUN SETUP UTILITY', t) and re.search(r'F1>?\s*to\s*RESUME', t):
                self.log('AMI: F1 runs setup')
                b.keys('f1', gap=1500)
            elif s.ok and s.find(r'RUN CMOS SETUP'):
                b.goto(r'RUN CMOS SETUP')
                b.keys('enter', gap=1500)
            elif now - t0 > 4000 and now - last_blind > 1000:
                last_blind = now
                b.keys('del', gap=300)
            else:
                b.step(200)

    def family(self, s):
        if not s.ok:
            return None
        t = s.text()
        if 'IDE HDD AUTO DETECTION' in t and 'SAVE & EXIT SETUP' in t:
            return 'award4'
        if re.search(r'Load (Optimized|Fail-Safe|Setup) Defaults', t) and 'Standard CMOS Features' in t:
            return 'award6'
        if 'WRITE TO CMOS AND EXIT' in t:
            return 'amihiflex'
        if 'Alt+H' in t and 'Optimal' in t:
            return 'winbios'
        if re.search(r'F10\s*:?\s*(Save|Exit)|Save\s*&\s*Exit|Exit Saving Changes|Save Values', t, re.I):
            return 'generic'
        return None

    # --- setup programs ---------------------------------------------------

    def award4(self):
        b = self.b
        for item in (r'LOAD SETUP DEFAULTS', r'LOAD BIOS DEFAULTS'):
            if b.screen().find(item) and b.goto(item):
                b.keys('enter', gap=800)
                self.snap('defaults prompt')
                self.answer_yes()
                self.done.append('defaults')
                break
        b.goto(r'IDE HDD AUTO DETECTION')
        b.keys('enter', gap=3000)
        for _ in range(8):
            s = self.snap('detect')
            if self.family(s):
                break
            m = s.search(r'(Primary Master|Primary Slave|Secondary Master|Secondary Slave|Drive [CDEF])\b.*?(\(Y/N\)|N=Skip)')
            if not m:
                b.keys('esc', gap=1500)
                continue
            if m.group(1).lower() in ('primary master', 'drive c'):
                b.keys('y', gap=500)
                self.done.append('detect')
                if b.screen().search(r'\(Y/N\)|N=Skip'):
                    b.keys('enter', gap=3000)
            else:
                b.keys('n', gap=500)
                if b.screen().search(r'N=Skip|\(Y/N\)'):
                    b.keys('enter', gap=3000)
        self.save_f10()

    def award6(self):
        b = self.b
        s = b.screen()
        item = r'Load Optimized Defaults' if s.find(r'Load Optimized Defaults') else r'Load (Fail-Safe|Setup) Defaults'
        if b.goto(item):
            b.keys('enter', gap=800)
            self.snap('defaults prompt')
            self.answer_yes()
            self.done.append('defaults')
        self.save_f10()

    def amihiflex(self):
        b = self.b
        if b.goto(r'AUTO CONFIGURATION WITH BIOS DEFAULTS'):
            b.keys('enter', gap=800)
            self.snap('defaults prompt')
            self.answer_yes()
            self.done.append('defaults')
        if b.goto(r'AUTO.DETECT HARD DIS'):
            b.keys('enter', gap=3000)
            for _ in range(6):
                s = self.snap('detect')
                if self.family(s):
                    break
                m = s.search(r'(?:for|Hard Disk|Disk|Drive)\s*([CD]):?[^\n]*?\(Y/N\)')
                if m and m.group(1) == 'C':
                    self.answer_yes()
                    self.done.append('detect')
                elif m:
                    b.keys('n', gap=500)
                    if b.screen().search(r'\(Y/N\)'):
                        b.keys('enter', gap=1500)
                else:
                    b.keys('esc', gap=1500)
        b.goto(r'WRITE TO CMOS AND EXIT')
        b.keys('enter', gap=800)
        self.snap('save prompt')
        self.answer_yes()
        self.done.append('saved')

    def winbios(self):
        """AMI WinBIOS: icons in windows.  Tab moves between windows, the
        arrows within one, and the bottom row says what the item does."""
        b = self.b

        def help_line():
            for row in b.screen().rows:
                if 'Alt+H' in row:
                    return row
            return ''

        def seek(pattern):
            for _ in range(6):
                for _ in range(10):
                    h = help_line()
                    if re.search(pattern, h, re.I):
                        return True
                    b.keys('right', gap=200)
                    if help_line() == h:
                        break
                b.keys('tab', gap=200)
            return False

        if seek(r'Load Optimal Default'):
            b.keys('enter', gap=1500)
            self.snap('optimal prompt')
            b.keys('enter', gap=1500)          # Yes is the default
            self.done.append('defaults')
        if seek(r'Auto Detect Hard Disk|Detect IDE|Auto.?Detect'):
            b.keys('enter', gap=4000)
            self.snap('detect C')
            b.keys('enter', gap=2000)          # Accept Parameters? Yes
            self.done.append('detect')
        b.keys('esc', gap=1500)
        self.snap('exit dialog')
        b.keys('enter', gap=1000)              # Save changes and Exit
        self.done.append('saved')

    def generic(self):
        """A setup this does not know.  Its key legend usually names a
        "load defaults" key (F5 on Intel's, F9 on Phoenix's) and a "save and
        exit" key (F10 nearly everywhere); each asks first.  IDE drives are
        "Auto" in those defaults."""
        b = self.b
        t = b.screen().text()
        m = re.search(r'\b(F\d+)\s*:?\s*(?:Load\s*)?(?:Setup|Optimal|BIOS|Fail.?Safe)?\s*Defaults', t, re.I)
        keys = [m.group(1).lower() if m else 'f9']
        m = re.search(r'\b(F\d+)\s*:?\s*Save\s*(?:&|and)?\s*Exit', t, re.I)
        keys.append(m.group(1).lower() if m else 'f10')
        for key in keys:
            before = b.screen().text()
            b.keys(key, gap=1000)
            s = self.snap('after ' + key)
            if s.search(r'\(Y/N\)'):
                self.answer_yes()
            elif s.text() != before:
                b.keys('enter', gap=1500)
            self.done.append(key)

    def save_f10(self):
        self.b.keys('f10', gap=800)
        self.snap('save prompt')
        self.answer_yes()
        self.done.append('saved')

    # --- the whole thing --------------------------------------------------

    def run(self):
        try:
            return self.run1()
        finally:
            self.b.close()

    def run1(self):
        b = self.b
        b('ff on')
        res = {'machine': os.path.basename(self.vm.rstrip('/'))}
        s = self.enter_setup()
        fam = self.family(s) if s else None
        res['family'] = fam
        if not fam:
            res['stage'] = 'unknown-setup'
        else:
            self.snap('setup ' + fam)
            getattr(self, fam)()
            ok = None
            for _ in range(4):
                ok = b.wait_screen(lambda s: s.ok and (DOS_UP.search(s.text()) or continue_key(s)), 180000, 1000)
                if not ok or DOS_UP.search(ok.text()):
                    break
                self.log('POST: ' + continue_key(ok))
                b.keys(continue_key(ok), gap=2000)
            res['stage'] = 'no-dos' if not ok else 'dos' if 'MODUS: starting' in ok.text() else 'modus-refused'
        res['done'] = self.done
        res['screen'] = [l for l in self.snap('end').text().split('\n') if l.strip()][-10:]
        return res


if __name__ == '__main__':
    print(json.dumps(Golden(sys.argv[1]).run()))
