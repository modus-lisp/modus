#!/usr/bin/env python3
"""rfb-client.py — a REAL RFB (VNC) client, so that modus is not grading itself.

This is the thing that decides whether test/rfb-static.lisp worked.  It speaks
RFB 3.8 (RFC 6143) against the modus server, and it does not take the server's
word for anything:

  * it does the handshake itself and checks every field of ServerInit —
    the width, the height, and all sixteen bytes of the PIXEL FORMAT, because a
    server that advertises one format and sends another is exactly the bug a
    modus-to-modus test cannot see;
  * it sends a FramebufferUpdateRequest and decodes the rectangle;
  * it GENERATES THE EXPECTED IMAGE ITSELF from the rule written in the test's
    header, and compares pixel by pixel.  It is not handed the answer.

The image is asymmetric in x and y and uses all three channels at different
values, so a red/blue swap, a transposed frame or a wrong stride each fail here
rather than passing as "some picture arrived".
"""

import socket
import struct
import sys

FBW, FBH = 64, 40


def expected_pixel(x, y):
    """The SAME rule as test/rfb-static.lisp's RFB-PIXEL, written out
    independently.  red varies only with x, green only with y, and blue marks a
    rectangle that is offset from centre in both axes."""
    r = (x * 4) & 0xFF
    g = (y * 8) & 0xFF
    b = 0x40 if (8 <= x < 24 and 4 <= y < 12) else 0x10
    return (r << 16) | (g << 8) | b


def recv_exactly(sock, n):
    buf = b""
    while len(buf) < n:
        chunk = sock.recv(n - len(buf))
        if not chunk:
            raise EOFError(f"peer closed after {len(buf)} of {n} bytes")
        buf += chunk
    return buf


def main():
    if len(sys.argv) < 2:
        print("usage: rfb-client.py PORT", file=sys.stderr)
        return 2
    port = int(sys.argv[1])

    if 5900 <= port <= 5920:
        print(f"REFUSING to connect to {port}: inside the 5900-5920 VNC range.")
        return 2

    failures = []

    def check(name, got, want):
        if got == want:
            print(f"  ok   {name} = {got}")
        else:
            failures.append(name)
            print(f"  FAIL {name} = {got} (expected {want})")

    sock = socket.create_connection(("127.0.0.1", port), timeout=30)
    sock.settimeout(30)

    print("=== HANDSHAKE ===========================================")

    # ---- ProtocolVersion ------------------------------------------------
    server_version = recv_exactly(sock, 12)
    check("ProtocolVersion from the server", server_version, b"RFB 003.008\n")
    sock.sendall(b"RFB 003.008\n")

    # ---- Security -------------------------------------------------------
    count = recv_exactly(sock, 1)[0]
    check("number of security types offered", count, 1)
    types = recv_exactly(sock, count)
    check("the security type offered is None(1)", list(types), [1])
    sock.sendall(bytes([1]))

    result = struct.unpack(">I", recv_exactly(sock, 4))[0]
    check("SecurityResult", result, 0)

    # ---- ClientInit / ServerInit ---------------------------------------
    sock.sendall(bytes([1]))  # shared

    init = recv_exactly(sock, 24)
    width, height = struct.unpack(">HH", init[:4])
    check("ServerInit width", width, FBW)
    check("ServerInit height", height, FBH)

    # The pixel format is 16 bytes: 13 of fields then 3 of padding (RFC 6143 7.4).
    (bpp, depth, big_endian, true_colour,
     rmax, gmax, bmax, rshift, gshift, bshift) = struct.unpack(">BBBBHHHBBB", init[4:17])
    print("  --- the advertised pixel format, field by field ---")
    check("bits-per-pixel", bpp, 32)
    check("depth", depth, 24)
    check("big-endian-flag", big_endian, 1)
    check("true-colour-flag", true_colour, 1)
    check("red-max", rmax, 255)
    check("green-max", gmax, 255)
    check("blue-max", bmax, 255)
    check("red-shift", rshift, 16)
    check("green-shift", gshift, 8)
    check("blue-shift", bshift, 0)

    name_len = struct.unpack(">I", init[20:24])[0]
    name = recv_exactly(sock, name_len).decode("latin-1")
    print(f"  desktop name: {name!r} ({name_len} bytes)")

    # ---- ask for the screen --------------------------------------------
    print()
    print("=== THE FRAME ===========================================")
    # SetEncodings: Raw only, so the server cannot answer with something we did
    # not ask for and have that count as a pass.
    sock.sendall(struct.pack(">BBHi", 2, 0, 1, 0))
    # FramebufferUpdateRequest, non-incremental, whole screen
    sock.sendall(struct.pack(">BBHHHH", 3, 0, 0, 0, FBW, FBH))

    header = recv_exactly(sock, 4)
    msg, _pad, nrects = struct.unpack(">BBH", header)
    check("message type is FramebufferUpdate(0)", msg, 0)
    check("number of rectangles", nrects, 1)

    rx, ry, rw, rh, encoding = struct.unpack(">HHHHi", recv_exactly(sock, 12))
    check("rect x", rx, 0)
    check("rect y", ry, 0)
    check("rect width", rw, FBW)
    check("rect height", rh, FBH)
    check("encoding is Raw(0)", encoding, 0)

    payload = recv_exactly(sock, rw * rh * 4)
    check("pixel payload length", len(payload), rw * rh * 4)

    # ---- compare against the image we generated ourselves ---------------
    mismatches = 0
    first_bad = None
    for y in range(rh):
        for x in range(rw):
            off = (y * rw + x) * 4
            got = struct.unpack(">I", payload[off:off + 4])[0] & 0xFFFFFF
            want = expected_pixel(x, y)
            if got != want:
                mismatches += 1
                if first_bad is None:
                    first_bad = (x, y, got, want)

    print("  --- the pixels, against an image this client generated ---")
    check("pixels that differ from the expected image", mismatches, 0)
    if first_bad:
        x, y, got, want = first_bad
        print(f"       first at ({x},{y}): got {got:#08x} want {want:#08x}")

    # A flat image would pass a byte-order bug; say out loud that it is not flat.
    distinct = len({struct.unpack(">I", payload[i:i + 4])[0] for i in range(0, len(payload), 4)})
    print(f"  distinct pixel values received: {distinct}")
    check("the image is not a flat colour", distinct > 64, True)

    sock.close()

    print()
    print("=== VERDICT =============================================")
    if failures:
        print(f"REAL VNC CLIENT AGAINST MODUS: FAIL ({len(failures)} checks)")
        for f in failures:
            print(f"   - {f}")
        return 1
    print("REAL VNC CLIENT AGAINST MODUS: PASS")
    return 0


if __name__ == "__main__":
    sys.exit(main())
