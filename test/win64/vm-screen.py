#!/usr/bin/env python3
"""vm-screen.py - screenshot a running test VM through its QMP socket.

Usage: test/win64/vm-screen.py QMP_SOCKET OUT.png

QEMU writes a PPM (screendump); this converts it to PNG with the standard
library only, so a headless VM can be looked at without a display or socat.
"""
import json, os, socket, struct, sys, tempfile, time, zlib


def qmp(path, *commands):
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.connect(path)
    f = s.makefile("rw")
    f.readline()                                  # the greeting
    out = []
    for c in [{"execute": "qmp_capabilities"}] + list(commands):
        f.write(json.dumps(c) + "\n")
        f.flush()
        while True:                               # skip asynchronous events
            r = json.loads(f.readline())
            if "return" in r or "error" in r:
                out.append(r)
                break
    s.close()
    return out


def ppm_to_png(ppm, png):
    data = open(ppm, "rb").read()
    parts = data.split(maxsplit=4)                # P6 width height maxval pixels
    assert parts[0] == b"P6", "not a binary PPM"
    w, h = int(parts[1]), int(parts[2])
    pixels = parts[4][: w * h * 3]
    rows = b"".join(b"\x00" + pixels[y * w * 3:(y + 1) * w * 3] for y in range(h))

    def chunk(kind, body):
        c = struct.pack(">I", len(body)) + kind + body
        return c + struct.pack(">I", zlib.crc32(kind + body) & 0xFFFFFFFF)

    with open(png, "wb") as o:
        o.write(b"\x89PNG\r\n\x1a\n")
        o.write(chunk(b"IHDR", struct.pack(">IIBBBBB", w, h, 8, 2, 0, 0, 0)))
        o.write(chunk(b"IDAT", zlib.compress(rows, 6)))
        o.write(chunk(b"IEND", b""))


def main():
    sock, png = sys.argv[1], sys.argv[2]
    ppm = tempfile.mktemp(suffix=".ppm")
    r = qmp(sock, {"execute": "screendump", "arguments": {"filename": ppm}})
    for _ in range(50):
        if os.path.exists(ppm) and os.path.getsize(ppm) > 0:
            break
        time.sleep(0.1)
    if "error" in r[-1]:
        sys.exit("screendump failed: %s" % r[-1]["error"])
    ppm_to_png(ppm, png)
    os.remove(ppm)
    print(png)


if __name__ == "__main__":
    main()
