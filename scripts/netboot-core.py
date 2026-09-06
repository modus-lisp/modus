#!/usr/bin/env python3
"""Reset the Pi Zero 2 W, TFTP a save-and-die CORE to 0x18000000 and the kernel
to 0x300000, then `go`.  The image restores from the core before boot init."""
import argparse, serial, subprocess, sys, time
ap = argparse.ArgumentParser()
ap.add_argument('--img', required=True)
ap.add_argument('--core', default=None)
ap.add_argument('--capture', type=float, default=120.0)
ap.add_argument('--addr', default='0x300000')
ap.add_argument('--send', action='append', default=[])
ap.add_argument('--send-delay', type=float, default=6.0)
ap.add_argument('--port', default='/dev/ttyAMA0')
a = ap.parse_args()
ser = serial.Serial(a.port, 115200, timeout=0.1)
ser.reset_input_buffer()
def pump(dur):
    end = time.time() + dur
    while time.time() < end:
        b = ser.read(8192)
        if b:
            sys.stdout.write(b.decode('utf-8', 'replace')); sys.stdout.flush()
print('=== reset ===', flush=True)
subprocess.run(['bash', '/home/modus/pi5-reset-zero.sh'], capture_output=True)
end = time.time() + 9
while time.time() < end:
    ser.write(b'\r'); b = ser.read(512)
    if b:
        sys.stdout.write(b.decode('utf-8', 'replace')); sys.stdout.flush()
    time.sleep(0.1)
print('\n=== driving U-Boot ===', flush=True)
ser.write(b'setenv ipaddr 10.0.0.2\r'); pump(1)
ser.write(b'setenv serverip 10.0.0.1\r'); pump(1)
ser.write(b'usb start\r'); pump(12)
for attempt in range(6):
    ser.write(b'ping 10.0.0.1\r')
    end = time.time() + 8; buf = b''
    while time.time() < end:
        b = ser.read(4096)
        if b:
            sys.stdout.write(b.decode('utf-8','replace')); sys.stdout.flush(); buf += b
    if b'is alive' in buf: break
    print(f'\n[bind retry {attempt}]', flush=True)
    ser.write(b'usb reset\r'); pump(14)
if a.core:
    print('\n=== tftp core -> 0x18000000 ===', flush=True)
    ser.write(f'tftpboot 0x18000000 {a.core}\r'.encode()); pump(30)
ser.write(f'tftpboot {a.addr} {a.img}\r'.encode()); pump(40)
print(f'\n=== go {a.addr} ===', flush=True)
ser.write(f'go {a.addr}\r'.encode())
if a.send:
    buf = b''; end = time.time() + 1500
    while time.time() < end:
        b = ser.read(8192)
        if b:
            sys.stdout.write(b.decode('utf-8', 'replace')); sys.stdout.flush(); buf += b
            if b'Modus CL REPL' in buf or b'CORE-RESTORED' in buf: break
    pump(3)
else:
    pump(a.capture)
for line in a.send:
    print(f'\n--- send: {line} ---', flush=True)
    for ch in (line + '\r\n').encode():
        ser.write(bytes([ch])); time.sleep(0.02)
    pump(a.send_delay)
pump(5)
print('\n=== end ===', flush=True)
