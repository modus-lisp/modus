#!/usr/bin/env python3
"""push.py <file.lisp> [file2.lisp ...] — load Lisp into the running Modus over
the serial REPL, no SD swap, no dongle.  Modus is a live image: this reads
top-level forms from each file (paren-balanced, comments + blank lines stripped),
flattens each to one line, sends it to the serial REPL, and waits for the prompt,
printing the result.  This IS the no-swap dev loop — edit locally, push, it's
live.  (A new *base* image still needs a reflash; everything else loads here.)"""
import serial, time, sys, re

def forms(src):
    # strip line comments (;) outside strings, then split into paren-balanced
    # top-level forms.
    out, i, n = [], 0, len(src)
    depth, cur, instr, esc = 0, [], False, False
    while i < n:
        c = src[i]
        if instr:
            cur.append(c)
            if esc: esc = False
            elif c == '\\': esc = True
            elif c == '"': instr = False
        elif c == ';':                       # comment to EOL
            while i < n and src[i] != '\n': i += 1
            continue
        elif c == '"':
            instr = True; cur.append(c)
        elif c == '(':
            depth += 1; cur.append(c)
        elif c == ')':
            depth -= 1; cur.append(c)
            if depth == 0:
                out.append(''.join(cur).strip()); cur = []
        elif depth == 0 and c in ' \t\r\n':
            pass                              # ignore whitespace between forms
        else:
            cur.append(c)
        i += 1
    return [re.sub(r'\s+', ' ', f) for f in out if f.strip()]

def main():
    files = sys.argv[1:]
    if not files:
        print("usage: push.py <file.lisp> ..."); sys.exit(1)
    s = serial.Serial("/dev/ttyAMA0", 115200, timeout=0.3)
    s.reset_input_buffer()
    s.write(b"\r\n"); time.sleep(0.3); s.read(4096)   # nudge to a prompt
    total = ok = 0
    for fn in files:
        for form in forms(open(fn).read()):
            total += 1
            s.write((form + "\r\n").encode()); s.flush()
            b = b""; t = time.time()
            while time.time() - t < 30:
                d = s.read(8192)
                if d: b += d
                if b.rstrip().endswith(b">"): break
                time.sleep(0.02)
            done = b.rstrip().endswith(b">")
            ok += 1 if done else 0
            # last non-echo line = the result
            res = b.replace(b"\r", b"").decode("latin1").strip().split("\n")
            res = res[-2] if len(res) >= 2 else (res[-1] if res else "")
            print(("ok " if done else "?? ") + form[:52] + "  -> " + res[:40], flush=True)
    print("== %d/%d forms evaluated ==" % (ok, total), flush=True)
    s.close()

main()
