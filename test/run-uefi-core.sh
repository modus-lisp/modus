#!/bin/bash
# run-uefi-core.sh — SAVE-AND-DIE on the bare x64 UEFI CL image, under QEMU:
# boot a GENERIC image, load packages over the network (ql:quickload from a
# tarball directory served to the guest), snapshot the heap with save-and-die,
# pull the core out of guest RAM over QMP, then boot the SAME image again with
# the core placed back in RAM and prove the packages are live without a reload.
#
#   test/run-uefi-core.sh IMAGE.efi OUT.core [--load=NAME ...] [--probe=FORM --expect=TEXT]
#
#   TARS=dir     tarballs served as http://10.0.2.2:8086/<name>.tar (default tmp/tars)
#   MEM=m        guest RAM (default 1024: the core slot is 0x20000000, above a 512 MB guest)
#   CORE_ADDR    default 0x20000000 (must match the image's %core-addr)
#   LOAD_WAIT=s  per-quickload wait (default 600)
# Exit 0 = restored core answered the probe as expected.
set -u
EFI=${1:?usage: run-uefi-core.sh IMAGE.efi OUT.core [--load=NAME ...] [--probe=FORM --expect=TEXT]}; CORE=${2:?OUT.core}; shift 2
cd "$(dirname "$0")/.."
TARS=${TARS:-tmp/tars}; MEM=${MEM:-1024}; CORE_ADDR=${CORE_ADDR:-0x20000000}
loads=(); probe=""; expect=""
for a in "$@"; do case $a in --load=*) loads+=("${a#*=}");; --probe=*) probe=${a#*=};; --expect=*) expect=${a#*=};; *) echo "unknown arg $a" >&2; exit 2;; esac; done
W=$(mktemp -d /tmp/modus-uefi-core.XXXXXX)
# the tarball server, loopback only; the guest reaches the host as 10.0.2.2
( cd "$TARS" && exec python3 -m http.server 8086 --bind 127.0.0.1 > $W/http.log 2>&1 ) & HP=$!
trap 'kill $HP 2>/dev/null; [ -n "${QP:-}" ] && kill $QP 2>/dev/null' EXIT
sleep 0.5
forms=("(list (%gc-from-start) (%gc-bitmap-page-base) (%gc-bitmap-base) (%gc-cons-bitmap-base) (%core-addr))")
for l in "${loads[@]}"; do forms+=("(ql:quickload \"$l\")"); done
[ -n "$probe" ] && forms+=("$probe")
forms+=("(save-and-die \"core\")")
echo "== phase 1: boot, load, save"
OUT1=$W/serial1.txt
KEEP=1 OUT=$OUT1 NET=1 MEM=$MEM QMP=$W/qmp.sock FORM_WAIT=${LOAD_WAIT:-600} TIMEOUT=${TIMEOUT:-1800} \
  scripts/run-uefi-cl.sh "$EFI" "${forms[@]}" > $W/phase1.txt 2> $W/phase1.err
QP=$(grep -o 'QEMU pid [0-9]*' $W/phase1.err | awk '{print $3}')
sed 's/^/   /' $W/phase1.txt | cut -c1-160
end=$(tr -d '\r' < $OUT1 | grep -a '^CORE-END=' | tail -1 | cut -d= -f2)
[ -n "$end" ] || { echo "FAIL: no CORE-END on serial"; tr -d '\r' < $OUT1 | tail -5; exit 1; }
base=$((CORE_ADDR)); size=$((end - base))
echo "   core: [$CORE_ADDR, $end) = $size bytes"
python3 - "$W/qmp.sock" $base $size "$(readlink -f $W)/core.bin" <<'PY'
import socket, json, sys
sock, base, size, path = sys.argv[1], int(sys.argv[2]), int(sys.argv[3]), sys.argv[4]
s = socket.socket(socket.AF_UNIX); s.connect(sock)
def recv():
    d = b""
    while True:
        d += s.recv(65536)
        if b"\n" in d: return json.loads(d.split(b"\n")[0])
recv(); s.sendall(json.dumps({"execute": "qmp_capabilities"}).encode() + b"\n"); recv()
s.sendall(json.dumps({"execute": "pmemsave", "arguments": {"val": base, "size": size, "filename": path}}).encode() + b"\n"); print("   pmemsave:", recv())
s.sendall(json.dumps({"execute": "quit"}).encode() + b"\n")
PY
sleep 1; kill $QP 2>/dev/null; wait $QP 2>/dev/null; QP=""
[ -s $W/core.bin ] && [ "$(stat -c %s $W/core.bin)" = "$size" ] || { echo "FAIL: core dump missing or short"; exit 1; }
cp $W/core.bin "$CORE"; echo "   wrote $CORE sha256 $(sha256sum "$CORE" | cut -c1-16)"
echo "== phase 2: boot the same image with the core in RAM"
OUT2=$W/serial2.txt
reply=$(OUT=$OUT2 NET=1 MEM=$MEM LOADER="$(readlink -f "$CORE")@$CORE_ADDR" FORM_WAIT=60 TIMEOUT=${TIMEOUT:-1800} \
  scripts/run-uefi-cl.sh "$EFI" ${probe:+"$probe"} 2> $W/phase2.err)
grep -aq 'CORE-RESTORED' $OUT2 && echo "   CORE-RESTORED: yes" || { echo "FAIL: no CORE-RESTORED (serial tail:)"; tr -d '\r' < $OUT2 | tail -6; exit 1; }
grep -aq 'E2SMOKE' $OUT2 && { echo "FAIL: E2SMOKE ran, so boot init ran too: the core was not what came up"; exit 1; }
echo "   probe reply: $reply"
if [ -n "$expect" ]; then
  printf '%s' "$reply" | grep -qF -- "$expect" && echo "PASS: restored core answers $expect" || { echo "FAIL: expected $expect"; exit 1; }
else echo "PASS: restored"; fi
echo "   work $W"
