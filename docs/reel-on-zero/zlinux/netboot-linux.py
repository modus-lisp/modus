#!/usr/bin/env python3
"""Netboot a Linux kernel + initramfs on the Pi Zero 2 W over the U-Boot/TFTP rig
(same reset / autoboot-stop / USB bind / TFTP-retry logic as netboot-core-gz.py),
then capture the serial console until the bench prints its end marker."""
import argparse, serial, subprocess, sys, time
ap = argparse.ArgumentParser()
ap.add_argument('--kernel', default='linux.img.gz')    # gzip'd arm64 Image (RPi OS kernel8.img)
ap.add_argument('--initrd', default='initrd.gz')
ap.add_argument('--dtb', default='zero2w.dtb')          # stock dtb; memory node patched from bdinfo
ap.add_argument('--daddr', default='0x02f00000')
ap.add_argument('--kaddr', default='0x01000000')       # 2 MB aligned, below the initrd
ap.add_argument('--iaddr', default='0x03000000')
ap.add_argument('--bootargs', default='console=ttyS0,115200 earlycon=uart8250,mmio32,0x3f215040 keep_bootcon clk_ignore_unused rdinit=/init loglevel=7')
ap.add_argument('--wait', type=float, default=900.0)
ap.add_argument('--marker', default='ZLINUX END')
ap.add_argument('--tftp-tries', type=int, default=5)
ap.add_argument('--port', default='/dev/ttyAMA0')
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
def tftp(addr, name, tries, per):
    for t in range(tries):
        print(f'\n=== tftpboot {name} -> {addr} try {t} ===', flush=True)
        ser.write(f'tftpboot {addr} {name}\r'.encode()); b=pump(per)
        if b'Bytes transferred' in b: return True
        ser.write(b'\x03'); pump(2); ser.write(b'\x03\r'); pump(3)
        ser.write(b'usb stop\r'); pump(4); ser.write(b'usb start\r'); pump(14)
        ser.write(b'ping 10.0.0.1\r'); pump(8)
    return False
print('=== reset ===', flush=True)
subprocess.run(['bash','/home/modus/pi5-reset-zero.sh'], capture_output=True)
end = time.time()+9
while time.time()<end:
    ser.write(b'\r'); b=ser.read(512)
    if b: sys.stdout.write(b.decode('utf-8','replace')); sys.stdout.flush()
    time.sleep(0.1)
print('\n=== U-Boot ===', flush=True)
ser.write(b'setenv ipaddr 10.0.0.2\r'); pump(1)
ser.write(b'setenv serverip 10.0.0.1\r'); pump(1)
ser.write(b'setenv tftpblocksize 512\r'); pump(1)
ser.write(b'usb start\r'); pump(12)
for _ in range(6):
    ser.write(b'ping 10.0.0.1\r'); b=pump(8)
    if b'is alive' in b: break
    ser.write(b'usb reset\r'); pump(14)
if not tftp('0x08000000', a.kernel, a.tftp_tries, 40): print('\n=== KERNEL TFTP FAILED ===', flush=True); sys.exit(2)
print(f'\n=== unzip 0x08000000 -> {a.kaddr} ===', flush=True)
ser.write(f'unzip 0x08000000 {a.kaddr}\r'.encode()); b=pump(30)
if b'Uncompressed size' not in b: print('\n=== UNZIP FAILED ===', flush=True); sys.exit(3)
if not tftp(a.iaddr, a.initrd, a.tftp_tries, 60): print('\n=== INITRD TFTP FAILED ===', flush=True); sys.exit(2)
ser.write(b'printenv filesize\r'); b=pump(2)
import re
m=re.search(rb'filesize=([0-9a-f]+)', b); isize=m.group(1).decode() if m else None
print(f'\n=== initrd size 0x{isize} ===', flush=True)
if not tftp(a.daddr, a.dtb, a.tftp_tries, 20): print('\n=== DTB TFTP FAILED ===', flush=True); sys.exit(2)
ser.write(b'bdinfo\r'); b=pump(3)
m=re.search(rb'-> size\s*=\s*0x([0-9a-f]+)', b); dram=m.group(1).decode() if m else '1c000000'
print(f'\n=== DRAM size 0x{dram} ===', flush=True)
ser.write(f'fdt addr {a.daddr}\r'.encode()); pump(1)
ser.write(b'fdt resize 0x8000\r'); pump(1)
ser.write(f'fdt memory 0x0 0x{dram}\r'.encode()); pump(1)
ser.write(b'fdt print /memory@0\r'); pump(2)
# Leave the mini-UART DISABLED (stock): the kernel's aux-uart probe fails without the firmware's
# clock fixups and its error path disables the AUX clock, killing the earlycon too.  U-Boot's UART
# setup then survives, earlycon (raw MMIO 8250) + keep_bootcon carry printk, and /init writes to
# /dev/kmsg.  The PL011 is disabled so it cannot re-mux GPIO14/15 away from us.
ser.write(b'fdt set /soc/serial@7e201000 status disabled\r'); pump(1)
ser.write(b'fdt set /soc/serial@7e215040 status disabled\r'); pump(1)   # stock says okay; its probe kills the AUX clock
ser.write(b'fdt print /soc/serial@7e215040 status\r'); pump(2)
ser.write(b'fdt print /aliases\r'); pump(2)
ser.write(b'fdt header\r'); pump(2)
ser.write(f'setenv bootargs {a.bootargs}\r'.encode()); pump(1)
print(f'\n=== booti {a.kaddr} {a.iaddr}:0x{isize} {a.daddr} ===', flush=True)
ser.write(f'booti {a.kaddr} {a.iaddr}:0x{isize} {a.daddr}\r'.encode())
b=b''; end=time.time()+a.wait
while time.time()<end:
    c=ser.read(8192)
    if c: sys.stdout.write(c.decode('utf-8','replace')); sys.stdout.flush(); b+=c
    if a.marker.encode() in b: break
print('\n=== ZLINUX-NETBOOT DONE ===', flush=True)
