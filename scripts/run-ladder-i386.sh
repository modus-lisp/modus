#!/bin/bash
# run-ladder-i386.sh <i386-cli-binary> <tag> [parallel] [timeout-secs]
#
# Runs the 22-library ladder (/home/claude/lf/drivers/*-ql.lisp) against an
# i386 Modus CLI under qemu-i386-static, writing logs where
# /home/claude/lf/score.py expects them.
#
# /home/claude/lf/run-ql.sh cannot be used directly: it execs the binary, and a
# 32-bit ELF needs the qemu shim -- binfmt_misc is NOT registered here, so a
# bare ./binary silently fails to exec instead of reporting an error.  A wrapper
# is generated per run so nothing outside this script needs to know about qemu.
# Exact counterpart of scripts/run-ladder-aarch64.sh.
#
# Score with:  python3 /home/claude/lf/score.py /home/claude/lf/logs/<tag>
# Compare to an x64 run of the SAME tree -- never a stale binary.
set -u
BIN="$1"; TAG="$2"; PAR="${3:-8}"; TMO="${4:-6000}"
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }
BIN=$(readlink -f "$BIN")
QEMU="${QEMU_I386:-qemu-i386-static}"
command -v "$QEMU" >/dev/null 2>&1 || { echo "$QEMU not found (install qemu-user-static or set QEMU_I386)" >&2; exit 1; }
D=/home/claude/lf/logs/$TAG
mkdir -p "$D"
SHIM=$(mktemp)
printf '#!/bin/bash\nexec %s %s "$@"\n' "$QEMU" "$BIN" > "$SHIM"
chmod +x "$SHIM"
run_one() {
  lib="$1"
  s=$(date +%s)
  timeout "$TMO" "$SHIM" --load /home/claude/lf/drivers/$lib.lisp --quit \
      > "$D/$lib.log" 2>&1
  rc=$?
  e=$(date +%s)
  echo "EXIT=$rc SECS=$((e-s))" >> "$D/$lib.log"
}
export -f run_one; export SHIM D TMO
xargs -P "$PAR" -I{} bash -c 'run_one {}' < /home/claude/lf/ladder-ql.txt
rm -f "$SHIM"
echo "LADDER-QL-DONE $TAG"
