#!/usr/bin/env python3
"""glass-send-worker.py — count the bytes of ONE glass FramebufferUpdate.

The peer for test/glass-send-worker.lisp.  It is not modus and it is not Lisp:
it connects, sends one byte so the server's reader thread unparks, and then
counts what arrives until the update is complete or the socket goes quiet.

WHAT IT KNOWS INDEPENDENTLY.  The size of a two-band Raw update of a 128x96
screen is arithmetic, not something the server tells it:

    4                      FramebufferUpdate header (type, pad, n-rects)
  + 12 + 128*64*4          rect 1 header + pixels   (banded at 64 rows)
  + 12 + 128*32*4          rect 2 header + pixels
  = 49180

so "it stopped early" is a fact about the wire and not an opinion.  It also
checks the first rectangle's header fields and the first pixels against the
rule in test/glass-send-worker.lisp, so a run that delivers 49180 bytes of
garbage is not a pass.

PORT SAFETY: 5900-5920 is refused outright.
"""

import socket
import struct
import sys

FBW, FBH = 128, 96
BAND = 64
NEED = 4 + (12 + FBW * BAND * 4) + (12 + FBW * (FBH - BAND) * 4)


def expected_pixel(x, y):
    return ((x * 2) & 0xFF) << 16 | ((y * 3) & 0xFF) << 8 | 0x10


def main():
    if len(sys.argv) < 2:
        print("usage: glass-send-worker.py PORT")
        return 2
    port = int(sys.argv[1])
    if 5900 <= port <= 5920:
        print(f"REFUSING to connect to {port}: inside the 5900-5920 VNC range.")
        return 2

    sock = socket.create_connection(("127.0.0.1", port), timeout=180)
    sock.settimeout(90)
    try:
        sock.sendall(b"\x01")            # unpark the server's reader
        buf = b""
        while len(buf) < NEED:
            try:
                chunk = sock.recv(65536)
            except socket.timeout:
                print("  the socket went quiet")
                break
            if not chunk:
                print("  the peer closed")
                break
            buf += chunk
    finally:
        sock.close()

    print(f"  bytes received: {len(buf)} of {NEED}")
    if len(buf) < NEED:
        print(f"CLIENT: INCOMPLETE — stopped {NEED - len(buf)} bytes short")
        return 1

    msg, _pad, nrects = struct.unpack(">BBH", buf[:4])
    ok = True
    if msg != 0:
        print(f"  FAIL message type {msg}, expected 0"); ok = False
    if nrects != 2:
        print(f"  FAIL {nrects} rectangles, expected 2"); ok = False

    off = 4
    for want in ((0, 0, FBW, BAND), (0, BAND, FBW, FBH - BAND)):
        rx, ry, rw, rh, enc = struct.unpack(">HHHHi", buf[off:off + 12])
        if (rx, ry, rw, rh) != want or enc != 0:
            print(f"  FAIL rect ({rx},{ry},{rw},{rh}) enc={enc}, expected {want} enc=0")
            ok = False
        off += 12
        for i in range(0, rw * rh):
            v = struct.unpack("<I", buf[off + i * 4:off + i * 4 + 4])[0] & 0xFFFFFF
            w = expected_pixel(rx + (i % rw), ry + (i // rw))
            if v != w:
                print(f"  FAIL pixel {i} of rect at y={ry}: got {v:#08x} want {w:#08x}")
                ok = False
                break
        off += rw * rh * 4

    print("CLIENT: COMPLETE AND CORRECT" if ok else "CLIENT: COMPLETE BUT WRONG")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
