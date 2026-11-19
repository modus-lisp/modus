"""repl.py SOCKET FORM... -- send each FORM to the guest's COM1 and print the answer (waits for the next prompt)."""
import socket, sys, json, time
s = socket.socket(socket.AF_UNIX); s.connect(sys.argv[1]); f = s.makefile('rw')
def cmd(c):
    f.write(c + '\n'); f.flush(); return json.loads(f.readline())
pos = cmd('serial read 1 0 1')['pos']
for form in sys.argv[2:]:
    # a character at a time with a pause: the REPL's line editor runs in Lisp,
    # and a 486 cannot take a 115200-baud burst (the UART overruns)
    for ch in form + '\r':
        cmd('serial write 1 ' + ch.encode().hex()); cmd('step 3')
    r = cmd('wait serial 600000 1 %d > ' % pos)
    out = cmd('serial read 1 %d' % pos)
    pos = out['next']
    print(bytes.fromhex(out['data']).decode('latin1').replace('\r', '').rstrip(), '   [%s ms emulated]' % r.get('ms'))
