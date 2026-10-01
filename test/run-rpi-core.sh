#!/bin/bash
# run-rpi-core.sh -- SAVE-AND-DIE on the Pi 3B / Zero 2 W CL image under QEMU raspi3b:
# boot the kernel, install tarballs QEMU placed in RAM (no network under QEMU),
# snapshot with save-and-die, dump [0x18000000, CORE-END) over the gdbstub, boot
# the same kernel again with the core back in RAM and require CORE-RESTORED + probe.
#
#   test/run-rpi-core.sh kernel8.img OUT.core [--load=NAME ...] [--probe=FORM --expect=TEXT]
#
#   TARS=dir   tarballs <name>.tar (default tmp/tars); each is placed at 0x1A000000 + 16 MB * k (inside the 448 MB the image maps; the core may overwrite them after they are consumed)
#              (the board never sees these)
#   GDBPORT    default 1234
# The mini-UART is QEMU raspi3b's SECOND -serial; the first is wired to null.
set -u
IMG=${1:?usage: run-rpi-core.sh kernel8.img OUT.core [--load=NAME ...] [--probe=FORM --expect=TEXT]}; CORE=${2:?OUT.core}; shift 2
cd "$(dirname "$0")/.."
TARS=${TARS:-tmp/tars}; GDBPORT=${GDBPORT:-1234}
loads=(); probe=""; expect=""
for a in "$@"; do case $a in --load=*) loads+=("${a#*=}");; --probe=*) probe=${a#*=};; --expect=*) expect=${a#*=};; *) echo "unknown arg $a" >&2; exit 2;; esac; done
W=$(mktemp -d /tmp/modus-rpi-core.XXXXXX); SOCK=$W/uart.sock
LOADERS=(); TARSPEC=(); k=0
for n in "${loads[@]}"; do f=$(readlink -f "$TARS/$n.tar"); [ -s "$f" ] || { echo "FAIL: no tarball $TARS/$n.tar"; exit 1; }
  addr=$((0x1A000000 + k*16777216)); LOADERS+=(-device "loader,file=$f,addr=$addr"); TARSPEC+=(--tar "$addr:$(stat -c %s "$f"):$n"); k=$((k+1)); done
boot() { # extra qemu args...
  qemu-system-aarch64 -M raspi3b -kernel "$IMG" -serial null -serial "unix:$SOCK,server,nowait" -display none -no-reboot -gdb tcp::$GDBPORT "$@" > $W/qemu.log 2>&1 & QP=$!; }
trap '[ -n "${QP:-}" ] && kill $QP 2>/dev/null' EXIT
echo "== phase 1: boot, install from RAM, save ($W)"
boot "${LOADERS[@]}"
python3 test/rpi-core-driver.py $SOCK "${TARSPEC[@]}" ${probe:+--probe "$probe"} --save | tee $W/phase1.txt | sed 's/^/   /'
end=$(grep -a '^CORE-END=' $W/phase1.txt | tail -1 | cut -d= -f2)
[ -n "$end" ] || { echo "FAIL: no CORE-END"; exit 1; }
size=$((end - 0x18000000)); echo "   core: [0x18000000, $end) = $size bytes"
[ $size -gt 1048576 ] || { echo "FAIL: core smaller than 1 MB, not staging it"; exit 1; }
gdb-multiarch -batch -ex "target remote :$GDBPORT" -ex "dump binary memory $W/core.bin 0x18000000 $end" > $W/gdb.log 2>&1
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
