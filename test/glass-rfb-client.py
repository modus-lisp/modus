#!/usr/bin/env python3
"""glass-rfb-client.py — a REAL VNC client against GLASS'S OWN RFB SERVER on modus.

This is the thing that decides whether test/glass-serve.lisp worked.  It is not
modus, it is not glass, and it is not Lisp: it speaks RFB 3.8 (RFC 6143) over a
TCP socket and it takes the server's word for nothing.

  * it does the handshake itself and checks EVERY field of ServerInit — the
    width, the height, and all ten fields of the PIXEL FORMAT — because a
    server that advertises one format and sends another is exactly the bug that
    a same-implementation test on both ends cannot see;

  * it GENERATES THE EXPECTED IMAGE ITSELF, from the rule written out in
    test/glass-serve.lisp's header, and compares pixel by pixel.  It is never
    handed the answer;

  * it ASSEMBLES THE FRAME FROM HOWEVER MANY RECTANGLES ARRIVE.  glass bands a
    tall rectangle at *MAX-BAND-ROWS* = 64 rows, so a 96-row screen arrives as
    two rects and a client that assumed one would either fail a working server
    or — worse — pass on half a screen.  Coverage is tracked per pixel and a
    pixel nobody sent is a failure, not a zero that happens to match.

WHY THE PIXEL FORMAT DIFFERS FROM test/rfb-client.py's.  That client grades
test/rfb-static.lisp, modus's own minimal server, which advertises
big-endian-flag 1.  GLASS advertises big-endian-flag 0 and writes BGRX
(src/rfb.lisp write-pixel-format and write-rect-raw).  Both are legal RFB and
they are different wires; one client that accepted either would be checking
nothing.  So this file asserts glass's format exactly, and the existing client
is left alone rather than loosened.

CONCURRENCY.  With --clients N, N threads connect AT THE SAME TIME and each does
the whole exchange independently.  That is the point of the flag: glass is
thread-per-client times two (a reader thread and a sender thread each), so N
viewers is 2N threads on the server, and a shared buffer or an unlocked table
shows up as one client receiving another's pixels.  Each client compares against
the SAME expected image, so cross-talk is a mismatch and not a coincidence.

PORT SAFETY.  5900-5920 — the VNC range, where this box's real desktops live —
is refused outright.
"""

import argparse
import socket
import struct
import sys
import threading

# ---------------------------------------------------------------------------
# THE IMAGE, generated here and nowhere else.
#
# The same rule as test/glass-serve.lisp's GLASS-TEST-PIXEL, written out
# independently.  It is asymmetric in x and in y and uses all three channels at
# different rates, so a red/blue swap, a transposed frame, a wrong stride and a
# band emitted in the wrong order each show up as a mismatch rather than as
# "some picture arrived".  The off-centre block breaks the pure-gradient
# symmetry that would let a transpose slip through on a square screen.
# ---------------------------------------------------------------------------


def expected_pixel(x, y):
    r = (x * 2) & 0xFF
    g = (y * 3) & 0xFF
    b = 0x40 if (16 <= x < 48 and 8 <= y < 24) else 0x10
    return (r << 16) | (g << 8) | b


def recv_exactly(sock, n):
    buf = bytearray()
    while len(buf) < n:
        chunk = sock.recv(n - len(buf))
        if not chunk:
            raise EOFError(f"peer closed after {len(buf)} of {n} bytes")
        buf += chunk
    return bytes(buf)


class Result:
    def __init__(self, label):
        self.label = label
        self.lines = []
        self.failures = []

    def check(self, name, got, want):
        if got == want:
            self.lines.append(f"  ok   [{self.label}] {name} = {got}")
        else:
            self.failures.append(name)
            self.lines.append(f"  FAIL [{self.label}] {name} = {got} (expected {want})")

    def note(self, text):
        self.lines.append(f"       [{self.label}] {text}")


def run_one(port, fbw, fbh, label, res):
    sock = socket.create_connection(("127.0.0.1", port), timeout=120)
    sock.settimeout(120)
    try:
        # ---- ProtocolVersion ----------------------------------------------
        res.check("ProtocolVersion", recv_exactly(sock, 12), b"RFB 003.008\n")
        sock.sendall(b"RFB 003.008\n")

        # ---- Security ------------------------------------------------------
        count = recv_exactly(sock, 1)[0]
        res.check("security types offered", count, 1)
        types = recv_exactly(sock, count)
        res.check("the type offered is None(1)", list(types), [1])
        sock.sendall(bytes([1]))
        res.check("SecurityResult", struct.unpack(">I", recv_exactly(sock, 4))[0], 0)

        # ---- ClientInit / ServerInit ---------------------------------------
        sock.sendall(bytes([1]))                       # shared-flag
        init = recv_exactly(sock, 24)
        width, height = struct.unpack(">HH", init[:4])
        res.check("ServerInit width", width, fbw)
        res.check("ServerInit height", height, fbh)

        (bpp, depth, big_endian, true_colour,
         rmax, gmax, bmax, rshift, gshift, bshift) = struct.unpack(">BBBBHHHBBB", init[4:17])
        res.check("bits-per-pixel", bpp, 32)
        res.check("depth", depth, 24)
        res.check("big-endian-flag", big_endian, 0)     # GLASS's value, not modus's
        res.check("true-colour-flag", true_colour, 1)
        res.check("red-max", rmax, 255)
        res.check("green-max", gmax, 255)
        res.check("blue-max", bmax, 255)
        res.check("red-shift", rshift, 16)
        res.check("green-shift", gshift, 8)
        res.check("blue-shift", bshift, 0)

        name_len = struct.unpack(">I", init[20:24])[0]
        name = recv_exactly(sock, name_len).decode("latin-1")
        res.note(f"desktop name: {name!r}")

        # ---- ask for the screen --------------------------------------------
        # Raw only.  A server that answered with an encoding we did not offer
        # would otherwise be free to pass by sending something we never checked.
        sock.sendall(struct.pack(">BBHi", 2, 0, 1, 0))
        # FramebufferUpdateRequest, NON-incremental, whole screen.
        sock.sendall(struct.pack(">BBHHHH", 3, 0, 0, 0, fbw, fbh))

        # ---- collect rectangles until the screen is covered -----------------
        image = [None] * (fbw * fbh)
        rect_count = 0
        update_count = 0
        while any(p is None for p in image):
            header = recv_exactly(sock, 4)
            msg, _pad, nrects = struct.unpack(">BBH", header)
            if msg != 0:
                res.check("message type is FramebufferUpdate(0)", msg, 0)
                break
            update_count += 1
            for _ in range(nrects):
                rx, ry, rw, rh, encoding = struct.unpack(">HHHHi", recv_exactly(sock, 12))
                if encoding != 0:
                    res.check("encoding is Raw(0)", encoding, 0)
                    return
                payload = recv_exactly(sock, rw * rh * 4)
                rect_count += 1
                for yy in range(rh):
                    for xx in range(rw):
                        off = (yy * rw + xx) * 4
                        # big-endian-flag 0 => the four bytes are little-endian,
                        # so the u32 is B | G<<8 | R<<16 (glass writes BGRX).
                        v = struct.unpack("<I", payload[off:off + 4])[0] & 0xFFFFFF
                        image[(ry + yy) * fbw + (rx + xx)] = v

        res.check("every pixel of the screen was sent", sum(1 for p in image if p is None), 0)
        res.note(f"{rect_count} rectangle(s) in {update_count} update(s) "
                 f"— banding at 64 rows means {(fbh + 63) // 64} for a {fbh}-row screen")

        # ---- compare against the image WE generated -------------------------
        mismatches = 0
        first_bad = None
        for y in range(fbh):
            for x in range(fbw):
                got = image[y * fbw + x]
                want = expected_pixel(x, y)
                if got != want:
                    mismatches += 1
                    if first_bad is None:
                        first_bad = (x, y, got, want)
        res.check("pixels differing from the independently generated image", mismatches, 0)
        if first_bad:
            x, y, got, want = first_bad
            res.note(f"first at ({x},{y}): got {got:#08x} want {want:#08x}")

        distinct = len({p for p in image if p is not None})
        res.note(f"distinct pixel values received: {distinct}")
        res.check("the image is not a flat colour", distinct > 64, True)
    finally:
        sock.close()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("port", type=int)
    ap.add_argument("--width", type=int, default=128)
    ap.add_argument("--height", type=int, default=96)
    ap.add_argument("--clients", type=int, default=1,
                    help="how many viewers connect AT ONCE (glass is 2 threads per viewer)")
    args = ap.parse_args()

    if 5900 <= args.port <= 5920:
        print(f"REFUSING to connect to {args.port}: inside the 5900-5920 VNC range.")
        return 2

    results = [Result(f"client{i + 1}") for i in range(args.clients)]
    errors = {}

    def body(i):
        try:
            run_one(args.port, args.width, args.height, results[i].label, results[i])
        except Exception as exc:                       # noqa: BLE001 — reported, not swallowed
            errors[results[i].label] = f"{type(exc).__name__}: {exc}"

    if args.clients == 1:
        body(0)
    else:
        # STARTED TOGETHER ON PURPOSE.  Clients that connect one after another
        # never have two senders alive at once, which is the state being tested.
        threads = [threading.Thread(target=body, args=(i,)) for i in range(args.clients)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()

    for r in results:
        for line in r.lines:
            print(line)

    failed = [r for r in results if r.failures]
    print()
    print("=== VERDICT =============================================")
    if errors:
        for label, msg in sorted(errors.items()):
            print(f"   {label} RAISED: {msg}")
    if failed or errors:
        for r in failed:
            print(f"   {r.label}: {len(r.failures)} failed check(s): {', '.join(r.failures)}")
        print(f"GLASS'S RFB SERVER ON MODUS, {args.clients} CLIENT(S): FAIL")
        return 1
    print(f"GLASS'S RFB SERVER ON MODUS, {args.clients} CLIENT(S): PASS")
    return 0


if __name__ == "__main__":
    sys.exit(main())
