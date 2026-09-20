#!/usr/bin/env python3
"""socket-client.py — THE CLIENT THAT IS NOT MODUS.

    python3 test/socket-client.py PORT NCONN NBYTES CHUNK

Opens NCONN connections to 127.0.0.1:PORT *at the same time*, holds them all
open at a barrier so their handling on the server genuinely overlaps, then on
each one sends NBYTES of a per-connection byte pattern in CHUNK-sized writes
while a second thread reads the echo back, and compares byte for byte.

Why this exists rather than a modus client: a modus-to-modus test cannot see a
protocol bug that both ends share.  Python's socket layer is not modus's, so a
byte order, a length field or an off-by-one that modus is self-consistent about
shows up here as a mismatch.

Why send and receive are on separate threads: the server echoes, so a client
that sent NBYTES before reading anything would fill both socket buffers and
deadlock against a server doing exactly the same thing.  That is a property of
echo, not a bug in either side, and the fix is to read while writing.

Prints one line per connection and a final OK/FAIL summary.  Exit status 0 only
if every connection round-tripped exactly.

Binds nothing.  Connects to 127.0.0.1 only.
"""
import socket
import sys
import threading


def payload(k, n):
    # A pattern that differs per connection AND per offset, so a mismatch tells
    # you whether bytes were lost, duplicated, or came from another connection.
    return bytes(((i * 31 + k * 97 + 11) & 0xFF) for i in range(n))


def first_difference(got, want):
    n = min(len(got), len(want))
    for i in range(n):
        if got[i] != want[i]:
            return "offset %d: got %d want %d" % (i, got[i], want[i])
    if len(got) != len(want):
        return "length %d vs %d" % (len(got), len(want))
    return "none"


def main():
    port = int(sys.argv[1])
    nconn = int(sys.argv[2])
    nbytes = int(sys.argv[3])
    chunk = int(sys.argv[4])

    ready = threading.Barrier(nconn)
    results = [None] * nconn

    def one(k):
        want = payload(k, nbytes)
        try:
            s = socket.create_connection(("127.0.0.1", port), timeout=60)
        except Exception as exc:                       # noqa: BLE001
            results[k] = ("CONNECT-FAILED", repr(exc), 0)
            try:
                ready.wait(timeout=30)
            except Exception:                          # noqa: BLE001
                pass
            return
        s.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
        s.settimeout(60)
        # Every connection is open before any of them sends, so the server is
        # holding all of them at once rather than serving a queue.
        try:
            ready.wait(timeout=30)
        except Exception:                              # noqa: BLE001
            pass

        senderr = []

        def send():
            try:
                for off in range(0, nbytes, chunk):
                    s.sendall(want[off:off + chunk])
                s.shutdown(socket.SHUT_WR)
            except Exception as exc:                   # noqa: BLE001
                senderr.append(repr(exc))

        t = threading.Thread(target=send)
        t.start()
        got = bytearray()
        try:
            while len(got) < nbytes:
                b = s.recv(65536)
                if not b:
                    break
                got += b
        except Exception as exc:                       # noqa: BLE001
            senderr.append("recv: " + repr(exc))
        t.join()
        s.close()
        got = bytes(got)
        if senderr:
            results[k] = ("ERROR", "; ".join(senderr), len(got))
        elif got == want:
            results[k] = ("OK", "", len(got))
        else:
            results[k] = ("MISMATCH", first_difference(got, want), len(got))

    threads = [threading.Thread(target=one, args=(k,)) for k in range(nconn)]
    for t in threads:
        t.start()
    for t in threads:
        t.join()

    bad = 0
    total = 0
    for k, r in enumerate(results):
        if r is None:
            r = ("NO-RESULT", "", 0)
        total += r[2]
        if r[0] != "OK":
            bad += 1
        print("  conn %d: %s %s (%d bytes back)" % (k, r[0], r[1], r[2]))
    print("CLIENT-TOTAL %d bytes over %d connections" % (total, nconn))
    if bad == 0:
        print("CLIENT-VERDICT OK")
        return 0
    print("CLIENT-VERDICT FAIL (%d of %d connections)" % (bad, nconn))
    return 1


if __name__ == "__main__":
    sys.exit(main())
