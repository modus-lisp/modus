#!/usr/bin/env python3
"""vsock-proxy.py TCP_PORT CID VSOCK_PORT -- on an enclave's PARENT: forward every
TCP connection on 127.0.0.1:TCP_PORT to the enclave's vsock CID:VSOCK_PORT, both
directions, one thread per direction per connection.  The enclave's SSH listens
on vsock; this is what lets a plain `ssh -p TCP_PORT test@127.0.0.1' reach it."""
import socket, sys, threading
tcp_port, cid, vport = int(sys.argv[1]), int(sys.argv[2]), int(sys.argv[3])
def pump(a, b):
    try:
        while True:
            d = a.recv(65536)
            if not d: break
            b.sendall(d)
    except OSError: pass
    finally:
        try: b.shutdown(socket.SHUT_WR)
        except OSError: pass
def serve(c):
    v = socket.socket(socket.AF_VSOCK, socket.SOCK_STREAM); v.connect((cid, vport))
    threading.Thread(target=pump, args=(c, v), daemon=True).start(); pump(v, c)
    c.close(); v.close()
l = socket.socket(); l.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1); l.bind(("127.0.0.1", tcp_port)); l.listen(8)
print(f"vsock-proxy: 127.0.0.1:{tcp_port} -> vsock {cid}:{vport}", flush=True)
while True:
    c, _ = l.accept(); threading.Thread(target=serve, args=(c,), daemon=True).start()
