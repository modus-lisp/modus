#!/bin/sh
# run-vm.sh - test modus.exe in a real Windows guest (KVM), with the same cases
# as run-wine.sh (test/win64/cases.sh).
#
# Usage: test/win64/run-vm.sh BASE.qcow2 MODUS_EXE [EXTRA_FILE...]
#
# BASE.qcow2 is a disk made by vm-install.sh; it is never written (each run
# boots a throwaway overlay).  The run puts modus.exe, every case's input and a
# RUN.CMD on a small FAT disk.  At boot the guest's task finds RUN.CMD, runs
# it, and powers off; the outputs are then read back off the FAT disk with
# mtools and judged here.  EXTRA_FILEs are copied onto the disk too (a probe
# program, say); RUN.CMD does not run them unless EXTRA_CMD is set, a line of
# cmd that RUN.CMD runs after the cases.
#
# VM_WORK keeps the work directory (default: a temporary one, removed);
# VM_RUN_TIMEOUT bounds the whole boot-run-shutdown (default 1200 s).  While it
# runs, test/win64/vm-screen.py WORK/vm.qmp OUT.png shows the screen.
set -u
base=${1:?usage: run-vm.sh BASE.qcow2 MODUS_EXE [EXTRA_FILE...]}
exe=${2:?usage: run-vm.sh BASE.qcow2 MODUS_EXE [EXTRA_FILE...]}
shift 2
here=$(cd "$(dirname "$0")" && pwd)
base=$(cd "$(dirname "$base")" && pwd)/$(basename "$base")
if [ -n "${VM_WORK:-}" ]; then work=$VM_WORK; mkdir -p "$work"
else work=$(mktemp -d); trap 'rm -rf "$work"' EXIT; fi
disk="$work/data.img"; part="$disk@@1M"; stage="$work/stage"
rm -rf "$stage" "$work/out"; mkdir -p "$stage" "$work/out"

# ---- the cases, as files for the guest -----------------------------------
WDIR="D:"                     # the FAT disk's letter in the guest (checked below)
n=0; : > "$work/manifest"
emit() { printf '%s\r\n' "$1" >> "$stage/RUN.CMD"; }
next() { n=$((n+1)); id=$(printf 'c%02d' $n); }
check() {
  next; printf '%s\n' "$2" > "$stage/$id.in"
  emit "modus.exe < $id.in > $id.out 2> $id.err"
  printf '%s\t%s\t%s\n' "$id" "$1" "$3" >> "$work/manifest"
}
script() {
  next
  emit "modus.exe --script $2 > $id.out 2> $id.err"
  printf '%s\t%s\t%s\n' "$id" "$1" "$3" >> "$work/manifest"
}
put() { printf '%s\n' "$2" > "$stage/$1"; }

: > "$stage/RUN.CMD"
emit "@echo off"
emit "echo %~d0> DRIVE.TXT"
emit "ver > VER.TXT"
emit "modus.exe < c00.in > c00.out 2> c00.err"      # the control
printf '(+ 1 2)\n' > "$stage/c00.in"
. "$here/cases.sh"
[ -n "${EXTRA_CMD:-}" ] && emit "$EXTRA_CMD"
emit "echo done> DONE.TXT"
cp "$exe" "$stage/modus.exe"
for f in "$@"; do cp "$f" "$stage/"; done

# ---- the FAT disk: an MBR with one FAT32 partition at 1 MiB ---------------
mb=256
rm -f "$disk"; truncate -s ${mb}M "$disk"
python3 - "$disk" $mb <<'PY'
import struct, sys
path, mb = sys.argv[1], int(sys.argv[2])
start, count = 2048, mb * 2048 - 2048
entry = struct.pack("<B3sB3sII", 0, b"\xfe\xff\xff", 0x0C, b"\xfe\xff\xff", start, count)
with open(path, "r+b") as f:
    f.seek(440); f.write(struct.pack("<I", 0x4D4F4455))    # disk signature
    f.seek(446); f.write(entry)
    f.seek(510); f.write(b"\x55\xaa")
PY
mformat -i "$part" -F -T $((mb * 2048 - 2048)) -h 64 -s 32 -H 2048 -v MODUS :: || exit 1
mcopy -i "$part" "$stage"/* :: || exit 1

# ---- boot a throwaway overlay of the installed disk -----------------------
qemu-img create -q -f qcow2 -b "$base" -F qcow2 "$work/run.qcow2" || exit 1
echo "booting $(basename "$base") with $n cases (typically 2-5 minutes)"
t0=$(date +%s)
timeout ${VM_RUN_TIMEOUT:-1200} qemu-system-x86_64 \
  -enable-kvm -machine q35 -cpu host -smp 4 -m 4096 \
  -drive file="$work/run.qcow2",if=none,id=hd,format=qcow2 \
  -drive file="$disk",if=none,id=data,format=raw \
  -device ahci,id=ahci \
  -device ide-hd,drive=hd,bus=ahci.0 \
  -device ide-hd,drive=data,bus=ahci.1 \
  -nic none -display none -vga std \
  -qmp unix:"$work/vm.qmp",server=on,wait=off
rc=$?
echo "guest ran $(( $(date +%s) - t0 )) s (qemu exit $rc)"
mcopy -n -i "$part" '::*' "$work/out/" 2>/dev/null

# ---- judge -----------------------------------------------------------------
get() { # get NAME -> the file's text, whatever case the guest gave its name
  f=$(find "$work/out" -maxdepth 1 -iname "$1" | head -1)
  [ -n "$f" ] && tr -d '\r' < "$f"
}
if [ -z "$(get DONE.TXT)" ]; then
  echo "RUN.CMD did not finish (qemu exit $rc): the guest never found it, or a case hung"
  [ $rc -eq 124 ] && echo "  (timed out; keep VM_WORK and use vm-screen.py on the next run)"
fi
echo "guest: $(get VER.TXT | grep -v '^$')"
drive=$(get DRIVE.TXT | tr -d ' ')
if [ -n "$drive" ] && [ "$drive" != "$WDIR" ]; then
  echo "NOTE: the FAT disk was $drive in the guest, not $WDIR: drive cases will fail"
fi
pass=0; fail=0
if get c00.out | sed 's/^> //' | grep -qxF 3 && ! get c00.out | sed 's/^> //' | grep -qxF 4; then
  echo "ok   control: (+ 1 2) answered 3, and the check rejects 4"
else
  echo "CONTROL FAILED: (+ 1 2) did not answer 3 in the guest; nothing below is evidence"
  get c00.out | head -5 | sed 's/^/     | /'; get c00.err | head -5 | sed 's/^/   ! /'
  exit 2
fi
tab=$(printf '\t')
while IFS="$tab" read -r id name want; do
  out=$(get "$id.out" | sed 's/^> //')
  if printf '%s\n' "$out" | grep -qxF -- "$want"; then
    echo "ok   $name => $want"; pass=$((pass+1))
  else
    echo "FAIL $name: wanted line '$want', got:"
    printf '%s\n' "$out" | head -20 | sed 's/^/     | /'
    get "$id.err" | head -10 | sed 's/^/   ! /'
    fail=$((fail+1))
  fi
done < "$work/manifest"
echo "== $pass passed, $fail failed (real Windows)"
[ "$fail" -eq 0 ]
