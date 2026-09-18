#!/bin/bash
Z() { printf "$1\n" | timeout ${2:-20} nc -w ${3:-8} 10.0.0.2 2323 2>/dev/null; }
echo "=== vpx foreground sanity:"; Z "cd /reel; VPX_PASSES=3 vpxbench /reel/cam.ivf 2>&1; echo rc=\$?; exit" 60 40
echo "=== vpx steady state:"
Z "cd /reel; (VPX_PASSES=800 /usr/bin/vpxbench /reel/cam.ivf > /tmp/vpx.out 2>&1 &) ; sleep 2; ps | grep -v grep | grep vpxb; exit" 30 20
Z "PID=\$(ps | grep -v grep | grep vpxb | awk '{print \$1}' | head -1); echo pid=\$PID; pstat \$PID 8 | tail -2; exit" 60 45
echo "=== modus: wait for eager=, then sample:"
Z "cd /reel; (modus --load /reel/modus-bench-perf.lisp --quit > /tmp/modus.out 2>&1 &); exit" >/dev/null
for i in $(seq 1 40); do sleep 10; E=$(Z "grep -ac eager= /tmp/modus.out; exit"); [ "$E" = "1" ] && break; done; echo "eager after ~$((i*10)) s"; sleep 12
Z "PID=\$(ps | grep -v grep | grep ' modus' | awk '{print \$1}' | head -1); echo pid=\$PID; grep -a eager= /tmp/modus.out; pstat \$PID 15 | tail -2; exit" 60 45
sleep 1; Z "ps | grep -v grep | grep -E 'vpxb| modus' | awk '{print \$1}' | xargs -r kill 2>/dev/null; exit" >/dev/null
echo "=== MEASURE3 DONE ==="
