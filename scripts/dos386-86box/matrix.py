"""matrix.py OUTDIR MEDIA MACHINE... -- boot the DOS386 media on many 86Box machines at once.

For each 86Box machine (internal name, e.g. 486vipio2): a VM directory under
OUTDIR with its own copies of MEDIA/floppy.img and MEDIA/hdd.img
(make-media.sh), a config (the machine's default CPU, MEM MB of RAM capped at
what the board takes, ISA VGA, the disks, COM1 on the automation backend, the
default -- fast, host-double -- x87), and 86Box (EIGHTYSIX, built with
-DAUTOMATION=ON) started headless and fast-forwarded.  The run presses F1 at a
CMOS error, then waits for the REPL's prompt on COM1, evaluates FORMS one
character at a time, and records one line: how far it got, the answers, and on
a failure the text screen where it stopped.  Results go to OUTDIR/results.json.

  EIGHTYSIX  86Box binary            (default ~/src/86Box/build/src/86Box)
  ROMS       ROM set                 (default ~/86Box-roms)
  JOBS       machines at once        (default 8)
  BOOT_MS    emulated ms to the REPL (default 900000)
  MEM        RAM in MB               (default 48)
"""
import concurrent.futures, json, os, shutil, socket, subprocess, sys, time

FORMS = [('(+ 1 2)', '3'),
         ('(list (* 1.5d0 2.25d0) (sqrt 2d0) (expt 2 70))',
          '(3.375d0 1.4142135623730951d0 1180591620717411303424)')]

def cfg_text(machine, vm, mem_kb):
    return f"""[Machine]
machine = {machine}
mem_size = {mem_kb}

[Video]
gfxcard = vga

[Storage controllers]
hdc = internal

[Hard disks]
hdd_01_parameters = 63, 16, 81, 0, ide
hdd_01_fn = {vm}/hdd.img
hdd_01_ide_channel = 0:0

[Floppy and CD-ROM drives]
fdd_01_type = 35_2hd
fdd_01_fn = {vm}/floppy.img
fdd_02_type = none

[Ports (COM & LPT)]
serial1_enabled = 1
serial1_device = automation
"""

class Ctl:
    def __init__(self, path):
        self.s = socket.socket(socket.AF_UNIX)
        for _ in range(150):
            try:
                self.s.connect(path); break
            except OSError:
                time.sleep(0.2)
        else:
            raise RuntimeError('no automation socket')
        self.f = self.s.makefile('rw')
    def __call__(self, cmd):
        self.f.write(cmd + '\n'); self.f.flush()
        line = self.f.readline()
        if not line:
            raise RuntimeError('86Box went away')
        return json.loads(line)
    def screen(self):
        r = self('text')
        if not r.get('ok'):
            return ['(not a text mode: %s)' % r.get('error')]
        return [bytes.fromhex(x).decode('cp437').rstrip() for x in r['chars']]
    def com1(self, frm=0):
        r = self('serial read 1 %d' % frm)
        return bytes.fromhex(r.get('data', '')).decode('latin1'), r.get('next', frm)

def run(machine, outdir, media, mem_kb, boot_ms):
    vm = os.path.join(outdir, machine)
    shutil.rmtree(vm, ignore_errors=True); os.makedirs(vm)
    for f in ('floppy.img', 'hdd.img'):
        shutil.copy(os.path.join(media, f), vm)
    open(os.path.join(vm, '86box.cfg'), 'w').write(cfg_text(machine, vm, mem_kb))
    sock = os.path.join(vm, 'ctl.sock')
    env = dict(os.environ, SDL_VIDEODRIVER='dummy', SDL_AUDIODRIVER='dummy')
    exe = os.environ.get('EIGHTYSIX', os.path.expanduser('~/src/86Box/build/src/86Box'))
    roms = os.environ.get('ROMS', os.path.expanduser('~/86Box-roms'))
    p = subprocess.Popen([exe, '-R', roms, '-P', vm, '-N', '--automation', 'unix:' + sock,
                          os.path.join(vm, '86box.cfg')],
                         stdout=open(os.path.join(vm, '86box.out'), 'w'), stderr=subprocess.STDOUT,
                         env=env, start_new_session=True)
    res = {'machine': machine, 'stage': 'start'}
    try:
        c = Ctl(sock)
        info = c('info')
        res.update(cpu=f"{info.get('cpu_family')} {info.get('cpu')}", mem_mb=info.get('mem_kb', 0) // 1024,
                   name=info.get('machine_name'))
        c('ff on')
        res['stage'] = 'post'
        t_end = info['ms'] + boot_ms
        f1 = 0
        while True:
            r = c('wait serial 20000 1 0 > ')
            if r.get('matched'):
                res['stage'] = 'repl'; break
            scr = '\n'.join(c.screen())
            if 'FreeDOS kernel' in scr and res['stage'] == 'post':
                res['stage'] = 'dos'
            if 'MODUS: starting' in scr:
                res['stage'] = 'modus-started'
            if ('F1' in scr and ('continue' in scr.lower() or 'resume' in scr.lower())) and f1 < 3:
                c('key down 0x3b'); c('key up 0x3b'); f1 += 1
            if 'MODUS: ' in scr and 'MODUS: loading' not in scr and 'MODUS: DOS loader' in scr \
               and 'MODUS: starting' not in scr and '\nA:\\>' in scr:
                res['stage'] = 'modus-refused'; break
            if r['ms'] > t_end:
                break
        res['ms_to_stop'] = r.get('ms')
        if res['stage'] == 'repl':
            text, pos = c.com1()
            answers = []
            for form, want in FORMS:
                for ch in form + '\r':
                    c('serial write 1 ' + ch.encode().hex()); c('step 3')
                r = c('wait serial 600000 1 %d > ' % pos)
                out, pos = c.com1(pos)
                got = out.replace('\r', '').split('\n')
                ans = got[1] if len(got) > 1 else ''
                answers.append(ans)
                if not r.get('matched'):
                    break
            res['answers'] = answers
            res['ok'] = [a for a in answers] == [w for _, w in FORMS]
            res['stage'] = 'pass' if res['ok'] else 'wrong'
        else:
            res['screen'] = [l for l in c.screen() if l.strip()][-12:]
            res['com1'] = c.com1()[0][-300:]
            res['resets'] = c('info').get('hard_resets')
        try:
            c('quit')
        except Exception:
            pass
    except Exception as e:
        res['error'] = repr(e)
    finally:
        try:
            p.wait(timeout=20)
        except subprocess.TimeoutExpired:
            p.kill()
    return res

def main():
    outdir, media, machines = sys.argv[1], sys.argv[2], sys.argv[3:]
    os.makedirs(outdir, exist_ok=True)
    jobs = int(os.environ.get('JOBS', '8'))
    boot_ms = int(os.environ.get('BOOT_MS', '900000'))
    mem_kb = int(os.environ.get('MEM', '48')) * 1024
    caps = {}
    try:
        for t, iname, pkg, rmax, name in json.load(open(os.environ.get('MACHINES_JSON', '/dev/null'))):
            caps[iname] = rmax
    except Exception:
        pass
    results = []
    with concurrent.futures.ThreadPoolExecutor(jobs) as ex:
        futs = {ex.submit(run, m, outdir, media, min(mem_kb, caps.get(m, mem_kb)), boot_ms): m
                for m in machines}
        for f in concurrent.futures.as_completed(futs):
            r = f.result(); results.append(r)
            print(f"{r['machine']:14} {r['stage']:14} {r.get('cpu','')[:24]:24} {r.get('mem_mb','')!s:>4} MB  "
                  f"{r.get('name','')[:46]}", flush=True)
    json.dump(sorted(results, key=lambda r: r['machine']), open(os.path.join(outdir, 'results.json'), 'w'), indent=1)

if __name__ == '__main__':
    main()
