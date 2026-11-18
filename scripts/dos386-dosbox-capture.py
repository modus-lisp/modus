"""dos386-dosbox-capture.py PORT TIMEOUT UNTIL DOSBOX-ARGV...

Listen on 127.0.0.1:PORT, start DOSBox (whose COM1 connects there), copy what
arrives to stdout, and stop DOSBox at TIMEOUT seconds, when UNTIL (if not
empty) has been seen, or when DOSBox exits.  Exit 0 if UNTIL was seen (or
none was asked for), 1 on a timeout, 3 if DOSBox exited first."""
import os, select, socket, subprocess, sys, time

port, timeout, until = int(sys.argv[1]), float(sys.argv[2]), sys.argv[3].encode()
argv = sys.argv[4:]
srv = socket.socket()
srv.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
srv.bind(("127.0.0.1", port))
srv.listen(1)
env = dict(os.environ, SDL_VIDEODRIVER="dummy", SDL_AUDIODRIVER="dummy")
log = open(os.devnull, "w") if not os.environ.get("DOS_LOG") else open(os.environ["DOS_LOG"], "w")
proc = subprocess.Popen(argv, env=env, stdout=log, stderr=subprocess.STDOUT)
deadline = time.time() + timeout
conn, seen, rc = None, b"", 1
try:
    while time.time() < deadline:
        socks = [conn] if conn else [srv]
        r, _, _ = select.select(socks, [], [], 0.2)
        if srv in r:
            conn, _ = srv.accept()
        elif conn in r:
            d = conn.recv(4096)
            if not d:
                conn = None
                continue
            sys.stdout.buffer.write(d)
            sys.stdout.flush()
            seen = (seen + d)[-4096:]
            if until and until in seen:
                rc = 0
                break
        if proc.poll() is not None:
            rc = 0 if not until else 3
            break
    else:
        if not until:
            rc = 0
finally:
    if proc.poll() is None:
        proc.kill()
        proc.wait()
sys.exit(rc)
