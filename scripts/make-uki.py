#!/usr/bin/env python3
"""make-uki.py IN.efi OUT.efi OSREL CMDLINE -- add the UKI sections .osrel and .cmdline to a PE/COFF
EFI application, the way systemd-ukify lays them out: each section gets a virtual address after the
last existing section, its data is appended to the file, and the section table, NumberOfSections and
SizeOfImage are updated.  objcopy is NOT used: it placed the new sections at virtual address 0 (over
the headers) on this image, and OVMF refused to load the result.  The code is not touched."""
import struct, sys

def align(x, a): return (x + a - 1) // a * a

def add_sections(data, sections):
    pe = struct.unpack_from('<I', data, 0x3c)[0]
    coff = pe + 4
    nsec, optsz = struct.unpack_from('<HH', data, coff + 2), struct.unpack_from('<H', data, coff + 16)[0]
    nsec = nsec[0]
    opt = coff + 20
    sec_table = opt + optsz
    raw_first = min(struct.unpack_from('<I', data, sec_table + 40 * i + 20)[0] for i in range(nsec))
    if sec_table + 40 * (nsec + len(sections)) > raw_first:
        raise SystemExit("no room in the header for the new section entries")
    sec_align = 0x1000
    last_end = 0
    for i in range(nsec):
        vsize, va = struct.unpack_from('<II', data, sec_table + 40 * i + 8)
        last_end = max(last_end, va + vsize)
    out = bytearray(data)
    file_end = align(len(out), 0x200)
    out += b'\0' * (file_end - len(out))
    va = align(last_end, sec_align)
    for k, (name, payload) in enumerate(sections):
        entry = sec_table + 40 * (nsec + k)
        raw_ptr = len(out)
        size = len(payload)
        padded = align(size, 0x200)
        out += payload + b'\0' * (padded - size)
        hdr = struct.pack('<8sIIIIIIHHI', name.encode().ljust(8, b'\0'), size, va, padded, raw_ptr,
                          0, 0, 0, 0, 0x40000040)   # INITIALIZED_DATA | READ
        out[entry:entry + 40] = hdr
        va = align(va + size, sec_align)
    struct.pack_into('<H', out, coff + 2, nsec + len(sections))
    struct.pack_into('<I', out, opt + 56, va)      # SizeOfImage
    return bytes(out)

if __name__ == '__main__':
    src, dst, osrel, cmdline = sys.argv[1:5]
    sections = [('.osrel', open(osrel, 'rb').read()), ('.cmdline', open(cmdline, 'rb').read())]
    open(dst, 'wb').write(add_sections(open(src, 'rb').read(), sections))
    print('wrote', dst)
