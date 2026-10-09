#!/usr/bin/env python3
"""Netboot the Pi Zero 2 W with a TFTP that RETRIES over the flaky RTL8153.
Reset -> U-Boot -> usb start -> bind -> tftpboot (retry w/ usb reset on Rx fail)
-> go -> send REPL forms.  Written because the 64MB image intermittently hits
'Rx: failed to receive: -5' and the stock netboot does not retry the transfer.

Run on the board host (the Pi 5 with the Zero on its serial and eth0).  --img and
--core are names under /srv/tftp; --img is gzipped and U-Boot unzips it to 0x300000.
A core (save-and-die, docs/save-and-die.md) loads at 0x18000000; the transfer is
waited for (up to 300 s) rather than given a fixed window, because a native media
core is ~74 MB and U-Boot's TFTP moves ~1.8 MiB/s.  Such a core only fits below the
RAM top when the Zero's config.txt has gpu_mem=32 (480 MiB; the default 448 MiB
leaves a 64 MB slot)."""
import argparse, serial, subprocess, sys, time
ap = argparse.ArgumentParser()
ap.add_argument('--img', required=True)
ap.add_argument('--core', default=None)
ap.add_argument('--addr', default='0x300000')
ap.add_argument('--send', action='append', default=[])
ap.add_argument('--send-delay', type=float, default=12.0)
ap.add_argument('--tftp-tries', type=int, default=5)
ap.add_argument('--port', default='/dev/ttyAMA0')
ap.add_argument('--pre-go', action='append', default=[])
ap.add_argument('--unzip-addr', default='0x300000')
ap.add_argument('--reset-cmd', default='/home/modus/pi5-reset-zero.sh')
ap.add_argument('--boot-cmd', default=None)
a = ap.parse_args()
ser = serial.Serial(a.port, 115200, timeout=0.1)
ser.reset_input_buffer()
_w=ser.write
def _paced(b):
    for i in range(len(b)): _w(b[i:i+1]); time.sleep(0.004)
    return len(b)
ser.write=_paced
def pump(dur):
    end = time.time() + dur; buf = b''
    while time.time() < end:
        b = ser.read(8192)
        if b: sys.stdout.write(b.decode('utf-8','replace')); sys.stdout.flush(); buf += b
    return buf
print('=== reset ===', flush=True)
subprocess.run(['bash', a.reset_cmd], capture_output=True)
end = time.time()+9
while time.time()<end:
    ser.write(b'\r'); b=ser.read(512)
    if b: sys.stdout.write(b.decode('utf-8','replace')); sys.stdout.flush()
    time.sleep(0.1)
print('\n=== U-Boot ===', flush=True)
ser.write(b'setenv ipaddr 10.0.0.2\r'); pump(1)
ser.write(b'setenv serverip 10.0.0.1\r'); pump(1)
ser.write(b'usb start\r'); pump(12)
# bind: ping until alive, usb reset between
for _ in range(6):
    ser.write(b'ping 10.0.0.1\r'); b=pump(8)
    if b'is alive' in b: break
    ser.write(b'usb reset\r'); pump(14)
# tftp with retries
ok=False
for t in range(a.tftp_tries):
    print(f'\n=== tftpboot try {t} ===', flush=True)
    if t==0: ser.write(b'setenv tftpblocksize 512\r'); pump(1)
    ser.write(f'tftpboot 0x08000000 {a.img}\r'.encode()); b=pump(30)
    if b'Bytes transferred' in b: ok=True; break
    print(f'\n[tftp retry {t}: usb reset]', flush=True)
    ser.write(b'\x03'); pump(2); ser.write(b'\x03\r'); pump(3)   # abort U-Boot's own tftp restarts first
    ser.write(b'usb stop\r'); pump(4); ser.write(b'usb start\r'); pump(14)
    ser.write(b'ping 10.0.0.1\r'); pump(8)
if not ok:
    print('\n=== TFTP FAILED after retries ===', flush=True); sys.exit(2)
print('\n=== unzip 0x08000000 -> 0x300000 ===', flush=True)
ser.write(f'unzip 0x08000000 {a.unzip_addr}\r'.encode()); b=pump(25)
if b'Uncompressed size' not in b:
    print('\n=== UNZIP FAILED ===', flush=True); sys.exit(3)
if a.core:
    print(f'\n=== tftpboot core {a.core} -> 0x18000000 ===', flush=True)
    ok=False
    for t2 in range(4):
        ser.write(f'tftpboot 0x18000000 {a.core}\r'.encode()); b=b''; end=time.time()+300
        while time.time()<end and b'Bytes transferred' not in b and b'Retry count exceeded' not in b: b+=pump(5)
        if b'Bytes transferred' in b: ok=True; break
        ser.write(b'\x03\r'); pump(2); ser.write(b'usb stop\r'); pump(4); ser.write(b'usb start\r'); pump(14)
    print(f'core tftp ok={ok}', flush=True)
else:
    ser.write(b'mw.q 0x18000000 0\r'); pump(1)
for cmd in a.pre_go:
    print(f'\n=== pre-go: {cmd} ===', flush=True); ser.write((cmd+'\r').encode()); pump(2)
bootcmd = a.boot_cmd or f'go {a.addr}'
print(f'\n=== {bootcmd} ===', flush=True)
ser.write((bootcmd+'\r').encode())
b=b''; end=time.time()+120
while time.time()<end:
    c=ser.read(8192)
    if c: sys.stdout.write(c.decode('utf-8','replace')); sys.stdout.flush(); b+=c
    if b'Modus CL REPL' in b: break
pump(3)
for line in a.send:
    print(f'\n--- send: {line} ---', flush=True)
    for ch in (line+'\r\n').encode(): ser.write(bytes([ch])); time.sleep(0.02)
    pump(a.send_delay)
pump(5)
print('\n=== NETBOOT-DONE ===', flush=True)
