#!/usr/bin/env python3
"""mkcpio.py OUT.cpio [--file SRC:DEST[:MODE] ...] [--text DEST:CONTENT ...]
Deterministic newc cpio (mtime 0, uid/gid 0, ino by order) -- the ramdisk format
the Nitro boot kernel unpacks.  No Docker, no host filesystem state in the image."""
import sys, os
def newc(entries):
    out = bytearray(); ino = 1
    def rec(name, mode, data):
        nonlocal ino
        nb = name.encode() + b"\0"
        hdr = b"070701" + b"".join(b"%08X" % v for v in (ino, mode, 0, 0, 1, 0, len(data), 0, 0, 0, 0, len(nb), 0))
        buf = hdr + nb; buf += b"\0" * (-len(buf) % 4); buf += data; buf += b"\0" * (-len(buf) % 4)
        out.extend(buf); ino += 1
    dirs = set()
    for name, mode, data in entries:
        d = os.path.dirname(name)
        while d and d not in dirs: dirs.add(d); d = os.path.dirname(d)
    for d in sorted(dirs): rec(d, 0o040755, b"")
    for name, mode, data in entries: rec(name, mode, data)
    rec("TRAILER!!!", 0, b"")
    out += b"\0" * (-len(out) % 512)
    return bytes(out)
a = sys.argv[1:]; outp = a[0]; ents = []; i = 1
while i < len(a):
    if a[i] == "--file":
        p = a[i+1].split(":"); src, dst = p[0], p[1]; mode = int(p[2], 8) if len(p) > 2 else 0o100755
        ents.append((dst, mode, open(src, "rb").read()))
    elif a[i] == "--text":
        dst, content = a[i+1].split(":", 1); ents.append((dst, 0o100644, content.encode()))
    i += 2
open(outp, "wb").write(newc(ents)); print(outp, os.path.getsize(outp), "bytes")
