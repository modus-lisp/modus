"""matrix.py OUTDIR MEDIA MACHINE... -- boot the DOS386 media on many 86Box machines at once.

For each 86Box machine (internal name, e.g. 486vipio2): a VM directory under
OUTDIR with MEDIA/hdd.img (make-media.sh: it boots FreeDOS, which runs
MODUS.COM; a hard link, as nothing writes it) and a config: the machine's
default CPU (on 486 boards a DX2-66, see cpu_lines), MEM MB of RAM within
what the board takes, its own video or an ISA VGA, its own IDE or an ISA IDE
card, the disk, an empty 1.44 MB drive A:, COM1 on the automation backend,
the default (fast, host-double) x87.  The machine table comes from
machines.py (MACHINES_JSON).

CMOS: a board's power-on defaults usually have no hard disk, where an owner's
machine would.  GOLDEN/MACHINE.nvr is the CMOS as golden.py left it (the BIOS's
defaults, its IDE auto-detection, saved); a machine without one gets a golden
pass first, and its result is kept for later runs.

Then the test boot: the run answers POST prompts with the key they name
(F1 to continue, ESC to boot), waits for the REPL's prompt on COM1, evaluates
FORMS one character at a time, and records one line: how far it got, the
answers, and on a failure the text screen and a screenshot.  Results go to
OUTDIR/results.json.

  EIGHTYSIX     86Box binary            (default ~/src/86Box/build/src/86Box)
  ROMS          ROM set                 (default ~/86Box-roms)
  MACHINES_JSON machines.py output      (default ~/dos386-work/86box/machines.json)
  GOLDEN        saved CMOS images       (default ~/dos386-work/86box/nvr-162x16x63: per disk geometry)
  JOBS          machines at once        (default 8)
  BOOT_MS       emulated ms to the REPL (default 1800000: a 486SX-25
                reads the 20 MB image through DOS at about 30 KB/s)
  MEM           RAM in MB               (default 48)
  CPU486        dx2 or default          (default dx2: see cpu_lines)
"""
import concurrent.futures, json, os, re, shutil, sys, traceback
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from box import Box, continue_key, MODUS_REFUSED
from golden import Golden

FORMS = [('(+ 1 2)', '3'),
         ('(list (* 1.5d0 2.25d0) (sqrt 2d0) (expt 2 70))',
          '(3.375d0 1.4142135623730951d0 1180591620717411303424)')]

# 486 boards: 86Box defaults most of them to a 486SX, which has no FPU and so
# runs MODUS.SF (slow to start on a 25 MHz SX).  CPU486=dx2 (the default)
# asks for a 486DX2-66 instead; 86Box falls back to the board's default when
# it cannot take one.  CPU486=default keeps the board's own default.
SOCKET_486 = {'SOCKET1', 'SOCKET2', 'SOCKET3', 'SOCKET3_PCI', 'SOCKET3_4', '386DX_486', '486SLC'}


def cpu_lines(m):
    if m.get('type') in SOCKET_486 and os.environ.get('CPU486', 'dx2') == 'dx2':
        return 'cpu_family = i486dx2\ncpu_speed = 66666666\ncpu_multi = 2\n'
    return ''


def cfg_text(m, mem_kb):
    return f"""[Machine]
machine = {m['iname']}
mem_size = {mem_kb}
{cpu_lines(m)}
[Video]
gfxcard = {'internal' if m.get('video') else 'vga'}

[Storage controllers]
hdc_1 = {'internal' if m.get('ide') else 'ide_isa'}

[Hard disks]
hdd_01_parameters = 63, 16, 162, 0, ide
hdd_01_fn = hdd.img
hdd_01_ide_channel = 0:0

[Floppy and CD-ROM drives]
fdd_01_type = 35_2hd
fdd_02_type = none

[Ports (COM & LPT)]
serial1_enabled = 1
serial1_device = automation
"""


def test_boot(vm, res, boot_ms):
    b = Box(vm)
    try:
        info = b('info')
        res.update(cpu=f"{info.get('cpu_family')} {info.get('cpu')}", mem_mb=info.get('mem_kb', 0) // 1024,
                   name=info.get('machine_name'))
        b('ff on')
        res['stage'] = 'post'
        t_end = info['ms'] + boot_ms
        pressed = []
        while True:
            r = b('wait serial 20000 1 0 > ')
            if r.get('matched'):
                res['stage'] = 'repl'
                res.setdefault('ms_at', {})['repl'] = r['ms']
                break
            scr = b.screen()
            t = scr.text()
            if 'FreeDOS kernel' in t and res['stage'] == 'post':
                res['stage'] = 'dos'
            if 'MODUS: loading' in t and res['stage'] in ('post', 'dos'):
                res['stage'] = 'loading'
            if 'MODUS: starting' in t:
                res['stage'] = 'modus-started'
            res.setdefault('ms_at', {}).setdefault(res['stage'], r['ms'])
            key = continue_key(scr)
            if key and len(pressed) < 6:
                b.keys(key, gap=500)
                pressed.append(key)
            if MODUS_REFUSED.search(t) and 'MODUS: starting' not in t:
                res['stage'] = 'modus-refused'
                break
            if r['ms'] > t_end:
                break
        res['ms_to_stop'] = r.get('ms')
        if pressed:
            res['pressed'] = pressed
        if res['stage'] == 'repl':
            text, pos = b.com1()
            answers = []
            for form, want in FORMS:
                for ch in form + '\r':
                    b('serial write 1 ' + ch.encode().hex())
                    b.step(3)
                r = b('wait serial 600000 1 %d > ' % pos)
                out, pos = b.com1(pos)
                got = out.replace('\r', '').split('\n')
                answers.append(got[1] if len(got) > 1 else '')
                if not r.get('matched'):
                    break
            res['answers'] = answers
            res['ok'] = answers == [w for _, w in FORMS]
            res['stage'] = 'pass' if res['ok'] else 'wrong'
        else:
            res['screen'] = [l for l in b.screen().text().split('\n') if l.strip()][-12:]
            res['com1'] = b.com1()[0][-300:]
            res['resets'] = b('info').get('hard_resets')
            b('screenshot ' + os.path.join(vm, 'stop.png'))
    finally:
        b.close()


def run(m, outdir, media, mem_kb, boot_ms, golden_dir):
    name = m['iname']
    vm = os.path.join(outdir, name)
    shutil.rmtree(vm, ignore_errors=True)
    os.makedirs(os.path.join(vm, 'nvr'))
    # The hard disk is only read (the image file), so every VM shares one copy.
    os.link(os.path.join(media, 'hdd.img'), os.path.join(vm, 'hdd.img'))
    open(os.path.join(vm, '86box.cfg'), 'w').write(cfg_text(m, mem_kb))
    res = {'machine': name, 'stage': 'start'}
    nvr = os.path.join(vm, 'nvr', name + '.nvr')
    gold = os.path.join(golden_dir, name + '.nvr')
    try:
        if os.path.exists(gold):
            shutil.copy(gold, nvr)
        else:
            g = Golden(vm).run()
            res['golden'] = {k: g[k] for k in ('stage', 'family', 'done')}
            if g['stage'] == 'dos' and os.path.exists(nvr):
                os.makedirs(golden_dir, exist_ok=True)
                shutil.copy(nvr, gold)
            elif g['stage'] != 'dos':
                res['golden']['screen'] = g['screen']
        test_boot(vm, res, boot_ms)
    except Exception as e:
        res['error'] = repr(e)
        res['trace'] = traceback.format_exc()[-800:]
    return res


def main():
    outdir, media, names = sys.argv[1], sys.argv[2], sys.argv[3:]
    os.makedirs(outdir, exist_ok=True)
    jobs = int(os.environ.get('JOBS', '8'))
    boot_ms = int(os.environ.get('BOOT_MS', '1800000'))
    mem_kb = int(os.environ.get('MEM', '48')) * 1024
    golden_dir = os.path.expanduser(os.environ.get('GOLDEN', '~/dos386-work/86box/nvr-162x16x63'))
    table = {m['iname']: m for m in json.load(open(os.path.expanduser(
        os.environ.get('MACHINES_JSON', '~/dos386-work/86box/machines.json'))))}
    results = []
    with concurrent.futures.ThreadPoolExecutor(jobs) as ex:
        futs = []
        for n in names:
            m = table.get(n, {'iname': n, 'ide': True})
            kb = mem_kb
            if m.get('ram_max'):
                kb = min(kb, m['ram_max'])
            if m.get('ram_min'):
                kb = max(kb, m['ram_min'])
            futs.append(ex.submit(run, m, outdir, media, kb, boot_ms, golden_dir))
        for f in concurrent.futures.as_completed(futs):
            r = f.result()
            results.append(r)
            g = r.get('golden', {})
            print(f"{r['machine']:14} {r['stage']:14} {r.get('cpu','')[:24]:24} {r.get('mem_mb','')!s:>4} MB  "
                  f"{(g.get('stage','') + ':' + str(g.get('family'))) if g else '':22} {r.get('name','')[:40]}"
                  + ('  ' + r['error'] if 'error' in r else ''), flush=True)
            json.dump(sorted(results, key=lambda r: r['machine']),
                      open(os.path.join(outdir, 'results.json'), 'w'), indent=1)


if __name__ == '__main__':
    main()
