#!/usr/bin/env python3
"""vsock-client.py CID PORT FORM...   -- talk to a modus vsock console (CID 1 = this host's loopback)"""
import socket, sys
cid, port = int(sys.argv[1]), int(sys.argv[2])
s = socket.socket(socket.AF_VSOCK, socket.SOCK_STREAM); s.settimeout(30); s.connect((cid, port))
f = s.makefile("rwb", buffering=0); print(f.readline().decode().strip())
for form in sys.argv[3:]:
    f.write((form + "\n").encode()); print(form, "->", f.readline().decode().strip())
