#!/usr/bin/env python3
"""uefi-core.py EFI OUT -- boot a bare x64 UEFI image (mvm/build-uefi-cl-repl.lisp) under QEMU +
OVMF, evaluate the forms in $CORE_FORMS (one per line) at its REPL, and when the last of them
SAVE-AND-DIEs, write the RAM-range core it left at 0x20000000 to OUT.

The image's SAVE-AND-DIE (lib/save-image.lisp, the bare x64 seams in build-cl-repl-common.lisp)
writes the core into RAM at %CORE-ADDR and prints CORE-END=<address>; QEMU is told to pause
rather than reset on the halt that follows, and the range is read out over QMP (pmemsave).  The
core boots from the ESP as \\CORE.BIN: boot/boot-uefi-x64.lisp's stub reads it back to
0x20000000 before ExitBootServices (scripts/uefi-disk.sh makes such a disk).

  CORE_FORMS   the forms (required); the last should be (save-and-die nil)
  CORE_LOG     the serial transcript (default OUT.serial.log)
  QEMU_MEM     MB (default 1024)    QEMU_CPU (default SandyBridge)
  QEMU_EXTRA   more QEMU arguments (a NIC: "-device e1000,netdev=n0 -netdev user,id=n0")
  OVMF_CODE / OVMF_VARS   (default /usr/share/OVMF/OVMF_CODE.fd / OVMF_VARS.fd)
  CORE_TIMEOUT seconds for the whole run (default 3600)
"""
import json, os, re, select, shutil, socket, subprocess, sys, tempfile, time

BASE = 0x20000000


def main():
    efi, out = sys.argv[1], sys.argv[2]
    forms = [l.rstrip('\n') for l in open(os.environ['CORE_FORMS']) if l.strip()]
    log_path = os.environ.get('CORE_LOG', out + '.serial.log')
    # beside OUT: the scratch ESP is 128 MB (sparse), and /tmp is often small
    tmp = tempfile.mkdtemp(prefix='uefi-core-', dir=os.path.dirname(os.path.abspath(out)))
    try:
        disk = os.path.join(tmp, 'esp.img')
        here = os.path.dirname(os.path.abspath(__file__))
        subprocess.run(['sh', os.path.join(here, 'uefi-disk.sh'), efi, '', disk, '0'], check=True,
                       stdout=subprocess.DEVNULL)
        vars_fd = os.path.join(tmp, 'vars.fd')
        shutil.copy(os.environ.get('OVMF_VARS', '/usr/share/OVMF/OVMF_VARS.fd'), vars_fd)
        sock = os.path.join(tmp, 'qmp.sock')
        cmd = ['qemu-system-x86_64', '-m', os.environ.get('QEMU_MEM', '1024'),
               '-cpu', os.environ.get('QEMU_CPU', 'SandyBridge'),
               '-drive', 'if=pflash,format=raw,readonly=on,file=' +
               os.environ.get('OVMF_CODE', '/usr/share/OVMF/OVMF_CODE.fd'),
               '-drive', 'if=pflash,format=raw,file=' + vars_fd,
               '-drive', 'file=%s,format=raw' % disk, '-display', 'none', '-serial', 'stdio',
               '-action', 'reboot=shutdown', '-action', 'shutdown=pause',
               '-qmp', 'unix:%s,server,nowait' % sock] + os.environ.get('QEMU_EXTRA', '').split()
        p = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT)
        buf, sent, end = b'', 0, None
        deadline = time.time() + float(os.environ.get('CORE_TIMEOUT', '3600'))
        log = open(log_path, 'wb')
        while time.time() < deadline:
            r, _, _ = select.select([p.stdout], [], [], 1)
            if r:
                d = os.read(p.stdout.fileno(), 65536)
                if not d:
                    break
                buf += d
                log.write(d)
                log.flush()
            m = re.search(rb'CORE-END=(\d+)\r?\n', buf)
            if m:
                end = int(m.group(1))
                time.sleep(2)
                break
            if buf.rstrip().endswith(b'>') and sent < len(forms):
                print('uefi-core: form %d/%d' % (sent + 1, len(forms)), flush=True)
                p.stdin.write(forms[sent].encode() + b'\n')
                p.stdin.flush()
                sent += 1
                buf = b''
        log.close()
        if end is None:
            p.kill()
            sys.exit('uefi-core: no CORE-END (sent %d of %d forms; see %s)' % (sent, len(forms), log_path))
        s = socket.socket(socket.AF_UNIX)
        s.connect(sock)
        f = s.makefile('rw')
        f.readline()
        for c, a in (('qmp_capabilities', {}),
                     ('pmemsave', {'val': BASE, 'size': end - BASE, 'filename': os.path.abspath(out)})):
            f.write(json.dumps({'execute': c, 'arguments': a}) + '\n')
            f.flush()
            while True:
                resp = json.loads(f.readline())
                if 'return' in resp or 'error' in resp:
                    break
            if 'error' in resp:
                sys.exit('uefi-core: %s: %s' % (c, resp['error']))
        s.close()
        p.kill()
        p.wait()
        print('uefi-core: %s (%d bytes)' % (out, end - BASE))
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


if __name__ == '__main__':
    main()
