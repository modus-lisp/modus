#!/usr/bin/env python3
"""wm-drive.py - drive glass's window manager in a native window in the guest (VM_DURING).

Usage: wm-drive.py QMP_SOCKET OUTDIR [ACTION...]

Waits for the workspace (teal, glass/wm's *WM-BG*), takes its client origin as the
smallest x and y of any teal pixel (the Clock window covers the corner), and runs
ACTIONs relative to that origin, a screenshot after each:
  click:X:Y      left click            type:TEXT   type TEXT (a-z 0-9 space ( ) + - . ")
  key:QCODE      press a QEMU qcode    wait:SECS
  drag:X1:Y1:X2:Y2[:STEPS]  press at X1,Y1, move to X2,Y2 in STEPS (20) moves, release
Ends with Alt+F4, which closes the window and so ends the program.
"""
import os, sys, time, importlib.util
here = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location("wtd", os.path.join(here, "window-test-drive.py"))
d = importlib.util.module_from_spec(spec); spec.loader.exec_module(d)
TEAL = (61, 122, 138)
QCODES = {" ": "spc", "(": ("shift", "9"), ")": ("shift", "0"), "+": ("shift", "equal"),
          "-": "minus", ".": "dot", '"': ("shift", "apostrophe"), "*": ("shift", "8")}


def origin(px, w, h):
    t = bytes(TEAL); xs = ys = None
    for y in range(h):
        row = px[y * w * 3:(y + 1) * w * 3]
        i = row.find(t)
        while i >= 0:
            if i % 3 == 0:
                x = i // 3
                xs = x if xs is None else min(xs, x)
                ys = y if ys is None else min(ys, y)
                break
            i = row.find(t, i + 1)
    return None if xs is None else (xs, ys)


def press(sock, keys):
    keys = keys if isinstance(keys, tuple) else (keys,)
    d.inp(sock, [{"type": "key", "data": {"down": True, "key": {"type": "qcode", "data": k}}} for k in keys])
    time.sleep(0.08)
    d.inp(sock, [{"type": "key", "data": {"down": False, "key": {"type": "qcode", "data": k}}} for k in reversed(keys)])
    time.sleep(0.08)


def main():
    sock, out, actions = sys.argv[1], sys.argv[2], sys.argv[3:]
    t0 = time.time()
    while True:
        ppm, w, h, px = d.grab(sock)
        os.remove(ppm)
        at = origin(px, w, h)
        if at:
            break
        if time.time() - t0 > 400:
            print("no workspace after %d s" % (time.time() - t0)); return
        time.sleep(5)
    print("workspace origin %s after %d s" % (at, time.time() - t0))
    time.sleep(3)
    n = 0
    def shot():
        nonlocal n
        n += 1
        ppm, w2, h2, _ = d.grab(sock)
        d.vs.ppm_to_png(ppm, os.path.join(out, "wm%02d.png" % n)); os.remove(ppm)
    shot()
    for a in actions:
        kind, _, arg = a.partition(":")
        if kind == "click":
            x, y = (int(v) for v in arg.split(":"))
            d.click(sock, at[0] + x, at[1] + y, w, h)
        elif kind == "type":
            for c in arg:
                press(sock, QCODES.get(c, c))
        elif kind == "key":
            press(sock, tuple(arg.split("+")))
        elif kind == "drag":
            v = [int(x) for x in arg.split(":")]
            x1, y1, x2, y2 = v[:4]; steps = v[4] if len(v) > 4 else 20
            def to(x, y):
                d.inp(sock, [{"type": "abs", "data": {"axis": "x", "value": (at[0] + x) * 32767 // (w - 1)}},
                             {"type": "abs", "data": {"axis": "y", "value": (at[1] + y) * 32767 // (h - 1)}}])
            to(x1, y1); time.sleep(0.2)
            d.inp(sock, [{"type": "btn", "data": {"down": True, "button": "left"}}])
            for i in range(1, steps + 1):
                to(x1 + (x2 - x1) * i // steps, y1 + (y2 - y1) * i // steps); time.sleep(0.05)
            d.inp(sock, [{"type": "btn", "data": {"down": False, "button": "left"}}])
        elif kind == "wait":
            time.sleep(float(arg))
        time.sleep(1.5)
        shot()
        print("did %s" % a)
    press(sock, ("alt", "f4"))
    print("sent alt-f4")


if __name__ == "__main__":
    main()
