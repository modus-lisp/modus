#!/usr/bin/env python3
"""glass-tx-cell.py — the far side of the kernel, and it counts.

    python3 test/glass-tx-cell.py PORT EXPECT

Drains the FramebufferUpdate the server writes and reports how many bytes
actually arrived.  It deliberately knows nothing about the encoding: the
evidence this test turns on is the Lisp-side grade of GLASS::*TX*, and the byte
count is here only so that "the transfer completed" is never asserted by the
process under test.

Exit 0 when EXPECT bytes arrived, 1 otherwise.
"""
import socket
import sys

TIMEOUT = 30.0


def main():
    if len(sys.argv) != 3:
        print("usage: glass-tx-cell.py PORT EXPECT", file=sys.stderr)
        return 2
    port = int(sys.argv[1])
    expect = int(sys.argv[2])

    s = socket.create_connection(("127.0.0.1", port), timeout=TIMEOUT)
    s.settimeout(TIMEOUT)
    got = 0
    try:
        while got < expect:
            chunk = s.recv(65536)
            if not chunk:
                print("CLIENT: the peer closed")
                break
            got += len(chunk)
    except socket.timeout:
        print("CLIENT: timed out waiting for the rest")
    finally:
        s.close()

    print("CLIENT: bytes received: %d of %d" % (got, expect))
    if got == expect:
        print("CLIENT: COMPLETE")
        return 0
    print("CLIENT: INCOMPLETE — stopped %d bytes short" % (expect - got))
    return 1


if __name__ == "__main__":
    sys.exit(main())
