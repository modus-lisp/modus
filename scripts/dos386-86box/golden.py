"""golden.py VMDIR -- configure a machine's BIOS once, the way a person would.

A machine with no saved CMOS boots with its BIOS's power-on defaults, which on
most boards before about 1997 means no hard disk.  This enters the BIOS's own
setup program, loads its defaults, has it detect the IDE disk, and saves, then
keeps going until DOS has run MODUS.COM.  VMDIR/nvr/ then holds the machine's CMOS
as an owner would have left it; matrix.py copies it into each run.

Recognised setup programs: Award (4.x menu grid; 6.0 "Phoenix - AwardBIOS"),
AMI Hi-Flex and WinBIOS, and the prompt-driven POST screens in front of them.  Anything
else stops with stage 'unknown-setup' and the screen in VMDIR/golden.log.

Prints one JSON line: stage ('dos': MODUS.COM started the image), what was done, the last screen.
"""
import json, os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from box import Box, continue_key

DOS_UP = re.compile(r'MODUS: (starting|cannot|BIOS|this|no )')


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
        memory -- with the 10h-2Dh checksum over them.  A BIOS whose own
        checks still fail loads its defaults, as without this; the AMI BIOSes
        take these and so know about the floppy."""
        b = self.b

        def rd(r):
            b('iow 0x70 1 %d' % r)
            return b('ior 0x71 1')['values'][0]

        def wr(r, v):
            b('iow 0x70 1 %d' % r)
            b('iow 0x71 1 %d' % v)

        wr(0x10, (rd(0x10) & 0x0F) | 0x40)
        wr(0x14, (rd(0x14) & 0x0E) | 0x01)
        wr(0x15, 0x80)
        wr(0x16, 0x02)
        wr(0x0E, 0x00)
        s = sum(rd(r) for r in range(0x10, 0x2E))
        wr(0x2E, s >> 8 & 0xFF)
        wr(0x2F, s & 0xFF)
        b('release')

    # --- POST -------------------------------------------------------------

    def enter_setup(self, timeout_ms=60000):
        """Get from power-on into a setup program.  The screen, or None."""
        b = self.b
        t_end = b('status')['ms'] + timeout_ms
        while b('status')['ms'] < t_end:
            s = b.screen()
            if self.family(s):
                return s
            m = s.search(r'(?:press|hit)\s*<?(DEL|F1|F2|ESC|F10)>?\s*(?:key\s*)?(?:to|for)\s*(?:enter\s*|run\s*)?SETUP') if s.ok else None
            if m:
                self.log('POST asks for %s' % m.group(1))
                b.keys(m.group(1).lower(), gap=300)
            elif s.ok and s.search(r'RUN SETUP UTILITY') and s.search(r'F1>?\s*to\s*RESUME'):
                self.log('AMI: F1 runs setup')
                b.keys('f1', gap=1500)
            elif s.ok and s.find(r'RUN CMOS SETUP'):
                b.goto(r'RUN CMOS SETUP')
                b.keys('enter', gap=1500)
            else:
                b.keys('del', gap=300)
        return None

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
        if 'AMIBIOS Setup' in t and 'Detect C' in t and 'Optimal' in t:
            return 'winbios'
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
            if m.group(1) in ('Primary Master', 'Drive C'):
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
        if seek(r'Auto Detect Hard Disk'):
            b.keys('enter', gap=4000)
            self.snap('detect C')
            b.keys('enter', gap=2000)          # Accept Parameters? Yes
            self.done.append('detect')
        b.keys('esc', gap=1500)
        self.snap('exit dialog')
        b.keys('enter', gap=1000)              # Save changes and Exit
        self.done.append('saved')

    def save_f10(self):
        self.b.keys('f10', gap=800)
        self.snap('save prompt')
        self.answer_yes()
        self.done.append('saved')

    # --- the whole thing --------------------------------------------------

    def run(self):
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
        b.close()
        return res


if __name__ == '__main__':
    print(json.dumps(Golden(sys.argv[1]).run()))
