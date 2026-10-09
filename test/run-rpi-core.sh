#!/bin/bash
# run-rpi-core.sh -- SAVE-AND-DIE on the Pi 3B / Zero 2 W CL image under QEMU raspi3b:
# boot the kernel, install tarballs QEMU placed in RAM (no network under QEMU),
# snapshot with save-and-die, dump [0x18000000, CORE-END) over the gdbstub, boot
# the same kernel again with the core back in RAM and require CORE-RESTORED + probe.
#
#   test/run-rpi-core.sh kernel8.img OUT.core [--load=NAME ...] [--blob=PATH@ADDR ...]
#                        [--form=FORM ...] [--probe=FORM --expect=TEXT]
#
#   --blob   another file QEMU places in guest RAM (fonts, data) for a --form to read
#            with (ramv ADDR LEN); keep it clear of the tarballs at 0x1A000000 + 16 MB*k
#   --form   evaluated after the installs, before the probe and the save, in order;
#            (jit-eager) here makes the core NATIVE -- saved hot-only, it restores every
#            DEFUN interpreted (hot = a form evaluated twice, never a function called often)
#
#   TARS=dir   tarballs <name>.tar (default tmp/tars); each is placed at 0x1A000000 + 16 MB * k (inside the 448 MB the image maps; the core may overwrite them after they are consumed)
#              (the board never sees these)
#   GDBPORT    default 1234
# The mini-UART is QEMU raspi3b's SECOND -serial; the first is wired to null.
set -u
IMG=${1:?usage: run-rpi-core.sh kernel8.img OUT.core [--load=NAME ...] [--probe=FORM --expect=TEXT]}; CORE=${2:?OUT.core}; shift 2
cd "$(dirname "$0")/.."
TARS=${TARS:-tmp/tars}; GDBPORT=${GDBPORT:-1234}
loads=(); probe=""; expect=""; blobs=(); forms=()
for a in "$@"; do case $a in --load=*) loads+=("${a#*=}");; --blob=*) blobs+=("${a#*=}");; --form=*) forms+=(--form "${a#*=}");;
  --probe=*) probe=${a#*=};; --expect=*) expect=${a#*=};; *) echo "unknown arg $a" >&2; exit 2;; esac; done
W=$(mktemp -d /tmp/modus-rpi-core.XXXXXX); SOCK=$W/uart.sock
LOADERS=(); TARSPEC=(); k=0
for n in "${loads[@]}"; do f=$(readlink -f "$TARS/$n.tar"); [ -s "$f" ] || { echo "FAIL: no tarball $TARS/$n.tar"; exit 1; }
  addr=$((0x1A000000 + k*16777216)); LOADERS+=(-device "loader,file=$f,addr=$addr"); TARSPEC+=(--tar "$addr:$(stat -c %s "$f"):$n"); k=$((k+1)); done
for b in "${blobs[@]}"; do f=$(readlink -f "${b%@*}"); [ -s "$f" ] || { echo "FAIL: no blob ${b%@*}"; exit 1; }
  LOADERS+=(-device "loader,file=$f,addr=${b##*@}"); done
boot() { # extra qemu args...
  # The mini-UART socket WAITS for the driver (no nowait): a restore prints CORE-RESTORED
  # first thing, and a nowait socket drops what the guest says before the driver connects.
  qemu-system-aarch64 -M raspi3b -kernel "$IMG" -serial null -serial "unix:$SOCK,server" -display none -no-reboot -gdb tcp::$GDBPORT \
    -monitor "unix:$W/mon.sock,server,nowait" "$@" > $W/qemu.log 2>&1 & QP=$!; }
# The dump: gdb-multiarch over the gdbstub, or -- where it is not installed -- the QEMU
# monitor's pmemsave.  HMP takes "\r" (a "\n" never submits) and answers with a second
# "(qemu)" prompt once the file is written; QEMU must still be running (it holds the core).
dump() { # start end out
  if command -v gdb-multiarch >/dev/null; then
    gdb-multiarch -batch -ex "target remote :$GDBPORT" -ex "dump binary memory $3 $1 $2" > $W/gdb.log 2>&1
  else
    MON=$W/mon.sock START=$1 LEN=$(($2 - $1)) OUT=$3 python3 - > $W/gdb.log 2>&1 <<'PY'
import os, socket, time
m = socket.socket(socket.AF_UNIX); m.connect(os.environ["MON"]); m.settimeout(1)
m.sendall(b'pmemsave %s %s "%s"\r' % tuple(os.environ[k].encode() for k in ("START", "LEN", "OUT")))
b = b""; e = time.time() + 600
while time.time() < e and b.count(b"(qemu)") < 2:
    try: b += m.recv(65536)
    except socket.timeout: pass
PY
  fi; }
trap '[ -n "${QP:-}" ] && kill $QP 2>/dev/null' EXIT
echo "== phase 1: boot, install from RAM, save ($W)"
boot "${LOADERS[@]}"
python3 test/rpi-core-driver.py $SOCK "${TARSPEC[@]}" "${forms[@]}" ${probe:+--probe "$probe"} --save | tee $W/phase1.txt | sed 's/^/   /'
end=$(grep -a '^CORE-END=' $W/phase1.txt | tail -1 | cut -d= -f2)
[ -n "$end" ] || { echo "FAIL: no CORE-END"; exit 1; }
size=$((end - 0x18000000)); echo "   core: [0x18000000, $end) = $size bytes"
[ $size -gt 1048576 ] || { echo "FAIL: core smaller than 1 MB, not staging it"; exit 1; }
dump 0x18000000 $end $W/core.bin
kill $QP 2>/dev/null; wait $QP 2>/dev/null; QP=""
[ "$(stat -c %s $W/core.bin 2>/dev/null)" = "$size" ] || { echo "FAIL: gdb dump missing or short"; cat $W/gdb.log | tail -3; exit 1; }
cp $W/core.bin "$CORE"; echo "   wrote $CORE sha256 $(sha256sum "$CORE" | cut -c1-16)"
echo "== phase 2: boot the same kernel with the core in RAM"
rm -f $SOCK; boot -device "loader,file=$(readlink -f "$CORE"),addr=0x18000000"
python3 test/rpi-core-driver.py $SOCK --expect-restored --banner-wait 900 ${probe:+--probe "$probe"} | tee $W/phase2.txt | sed 's/^/   /'
grep -q "^BANNER yes RESTORED=yes" $W/phase2.txt || { echo "FAIL: no CORE-RESTORED"; exit 1; }
reply=$(grep -a '^PROBE=' $W/phase2.txt | cut -d= -f2-)
if [ -n "$expect" ]; then printf '%s' "$reply" | grep -qF -- "$expect" && echo "PASS: restored core answers $expect" || { echo "FAIL: probe answered '$reply', expected $expect"; exit 1; }
else echo "PASS: restored"; fi
echo "   work $W"
