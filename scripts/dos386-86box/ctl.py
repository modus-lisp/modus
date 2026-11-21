"""ctl.py SOCKET CMD... -- send 86Box automation commands, one per argument; print each reply."""
import socket, sys, json, time
s = socket.socket(socket.AF_UNIX)
for _ in range(100):
    try: s.connect(sys.argv[1]); break
    except OSError: time.sleep(0.2)
f = s.makefile('rw')
for cmd in sys.argv[2:]:
    f.write(cmd + '\n'); f.flush()
    line = f.readline()
    try:
        r = json.loads(line)
        if 'data' in r and cmd.startswith('serial read'):
            r['text'] = bytes.fromhex(r['data']).decode('latin1'); del r['data']
        if 'chars' in r:
            r['screen'] = [bytes.fromhex(x).decode('cp437').rstrip() for x in r['chars']]; del r['chars']; r.pop('attrs', None)
        print(json.dumps(r, indent=None)[:4000])
    except Exception:
        print(line.strip()[:2000])
