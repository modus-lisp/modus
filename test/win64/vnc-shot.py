#!/usr/bin/env python3
"""vnc-shot.py - one screenshot from an RFB (VNC) server, as PNG.

Usage: test/win64/vnc-shot.py HOST PORT OUT.png [--click X Y]

Speaks RFB 3.8 with security type None, asks for 32-bit BGRX (glass's own
format), requests one full update and assembles its Raw rectangles.  --click
sends a left click at X,Y first and waits a moment, so a second shot can show
that input reached the desktop.  Standard library only.
"""
import socket, struct, sys, time, zlib


def recvn(s, n):
    b = b""
    while len(b) < n:
        c = s.recv(n - len(b))
        if not c:
            raise EOFError("server closed after %d of %d bytes" % (len(b), n))
        b += c
    return b


def png(path, w, h, rgb):
    def chunk(kind, body):
        c = struct.pack(">I", len(body)) + kind + body
        return c + struct.pack(">I", zlib.crc32(kind + body) & 0xFFFFFFFF)
    rows = b"".join(b"\x00" + rgb[y * w * 3:(y + 1) * w * 3] for y in range(h))
    with open(path, "wb") as o:
        o.write(b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", struct.pack(">IIBBBBB", w, h, 8, 2, 0, 0, 0))
                + chunk(b"IDAT", zlib.compress(rows, 6)) + chunk(b"IEND", b""))


def main():
    host, port, out = sys.argv[1], int(sys.argv[2]), sys.argv[3]
    click = None
    if "--click" in sys.argv:
        i = sys.argv.index("--click")
        click = (int(sys.argv[i + 1]), int(sys.argv[i + 2]))
    s = socket.create_connection((host, port), timeout=60)
    recvn(s, 12)
    s.sendall(b"RFB 003.008\n")
    n = recvn(s, 1)[0]
    types = recvn(s, n)
    if 1 not in types:
        sys.exit("server offers no None security: %r" % types)
    s.sendall(b"\x01")
    if struct.unpack(">I", recvn(s, 4))[0] != 0:
        sys.exit("security failed")
    s.sendall(b"\x01")                                   # shared
    w, h = struct.unpack(">HH", recvn(s, 4))
    recvn(s, 16)
    recvn(s, struct.unpack(">I", recvn(s, 4))[0])
    # SetPixelFormat: 32 bpp, depth 24, little-endian, true colour, BGRX.
    s.sendall(struct.pack(">BxxxBBBBHHHBBBxxx", 0, 32, 24, 0, 1, 255, 255, 255, 16, 8, 0))
    s.sendall(struct.pack(">BxHi", 2, 1, 0))             # SetEncodings: Raw
    if click:
        x, y = click
        s.sendall(struct.pack(">BBHH", 5, 0, x, y))      # move
        s.sendall(struct.pack(">BBHH", 5, 1, x, y))      # press
        s.sendall(struct.pack(">BBHH", 5, 0, x, y))      # release
        time.sleep(1.5)
    s.sendall(struct.pack(">BBHHHH", 3, 0, 0, 0, w, h))   # full update
    fb = bytearray(w * h * 3)
    covered = 0
    while covered < w * h:
        t = recvn(s, 1)[0]
        if t != 0:                                       # not a FramebufferUpdate: skip what we know
            if t == 2:
                continue                                 # Bell
            if t == 3:
                recvn(s, 3); recvn(s, struct.unpack(">I", recvn(s, 4))[0]); continue
            sys.exit("unexpected server message %d" % t)
        recvn(s, 1)
        for _ in range(struct.unpack(">H", recvn(s, 2))[0]):
            x, y, rw, rh, enc = struct.unpack(">HHHHi", recvn(s, 12))
            if enc != 0:
                sys.exit("non-Raw rectangle (encoding %d)" % enc)
            data = recvn(s, rw * rh * 4)
            for r in range(rh):
                for c in range(rw):
                    p = (r * rw + c) * 4
                    q = ((y + r) * w + x + c) * 3
                    fb[q:q + 3] = bytes((data[p + 2], data[p + 1], data[p]))
            covered += rw * rh
    png(out, w, h, bytes(fb))
    print("%s: %dx%d" % (out, w, h))


if __name__ == "__main__":
    main()
