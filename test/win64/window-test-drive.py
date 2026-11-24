#!/usr/bin/env python3
"""window-test-drive.py - drive window-test.lisp in the guest from the host (VM_DURING).

Usage: window-test-drive.py QMP_SOCKET OUTDIR

Waits for the window (its magenta marker) on the guest's screen, screenshots it,
clicks at client (160, 120), types "hi", screenshots again, and presses q.  Needs
the usb-tablet run-vm.sh adds with VM_UI=1.  Prints what it did.
"""
import json, os, socket, sys, tempfile, time
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import importlib.util
spec = importlib.util.spec_from_file_location("vmscreen", os.path.join(os.path.dirname(os.path.abspath(__file__)), "vm-screen.py"))
vs = importlib.util.module_from_spec(spec); spec.loader.exec_module(vs)


def grab(sock):
    ppm = tempfile.mktemp(suffix=".ppm")
    vs.qmp(sock, {"execute": "screendump", "arguments": {"filename": ppm}})
    for _ in range(50):
        if os.path.exists(ppm) and os.path.getsize(ppm) > 0:
            break
        time.sleep(0.1)
    data = open(ppm, "rb").read()
    parts = data.split(maxsplit=4)
    w, h = int(parts[1]), int(parts[2])
    return ppm, w, h, parts[4][:w * h * 3]


def find(px, w, h, rgb):
    for y in range(h):
        row = px[y * w * 3:(y + 1) * w * 3]
        i = row.find(bytes(rgb))
        while i >= 0:
            if i % 3 == 0:
                return i // 3, y
            i = row.find(bytes(rgb), i + 1)
    return None


def inp(sock, events):
    vs.qmp(sock, {"execute": "input-send-event", "arguments": {"events": events}})


def click(sock, x, y, w, h):
    inp(sock, [{"type": "abs", "data": {"axis": "x", "value": x * 32767 // (w - 1)}},
               {"type": "abs", "data": {"axis": "y", "value": y * 32767 // (h - 1)}}])
    time.sleep(0.3)
    inp(sock, [{"type": "btn", "data": {"down": True, "button": "left"}}])
    time.sleep(0.2)
    inp(sock, [{"type": "btn", "data": {"down": False, "button": "left"}}])


def key(sock, q):
    inp(sock, [{"type": "key", "data": {"down": True, "key": {"type": "qcode", "data": q}}}])
    time.sleep(0.1)
    inp(sock, [{"type": "key", "data": {"down": False, "key": {"type": "qcode", "data": q}}}])
    time.sleep(0.2)


def main():
    sock, out = sys.argv[1], sys.argv[2]
    t0 = time.time()
    while True:
        ppm, w, h, px = grab(sock)
        at = find(px, w, h, (255, 0, 255))
        if at:
            break
        os.remove(ppm)
        if time.time() - t0 > 400:
            vs.ppm_to_png(ppm, os.path.join(out, "timeout.png")) if os.path.exists(ppm) else None
            print("no window after %d s" % (time.time() - t0)); return
        time.sleep(5)
    print("window marker at screen %s after %d s (screen %dx%d)" % (at, time.time() - t0, w, h))
    time.sleep(2)
    ppm, w, h, px = grab(sock); vs.ppm_to_png(ppm, os.path.join(out, "shot1.png"))
    cx, cy = at[0] + 160, at[1] + 120
    click(sock, cx, cy, w, h)
    time.sleep(1)
    key(sock, "h"); key(sock, "i")
    time.sleep(2)
    ppm, w, h, px = grab(sock); vs.ppm_to_png(ppm, os.path.join(out, "shot2.png"))
    print("pixel at the click after it: %s" % (tuple(px[(cy * w + cx) * 3:(cy * w + cx) * 3 + 3]),))
    key(sock, "q")
    print("sent q")


if __name__ == "__main__":
    main()
