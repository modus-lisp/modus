#!/bin/bash
# test/ladder/run.sh <modus-cli-binary> <tag> [parallel] [timeout-secs]
#
# Runs the 22-library ladder against a Modus CLI image: each library's
# UNMODIFIED quicklisp tarball (tars/) through Modus's own INSTALL-TARBALL,
# then use-probes before and after a forced collection.  See README.md.
#
# Logs:    $LADDER_LOGS/<tag>/<lib>-ql.log   (default LADDER_LOGS = <repo>/tmp/ladder-logs)
# Drivers: $LADDER_LOGS/<tag>/drivers/       (generated for this run; kept so a
#                                             log can always be read against
#                                             exactly what produced it)
# Score:   python3 test/ladder/score.py $LADDER_LOGS/<tag>
#
# The binary's architecture is read from its ELF header.  A foreign one (i386
# anywhere, aarch64 on a non-aarch64 host) runs under qemu-user through a
# generated wrapper -- binfmt_misc is NOT registered on the dev box, and a bare
# ./binary of a 32-bit ELF silently fails to exec instead of reporting an
# error.  Override the emulator with QEMU_I386 / QEMU_AARCH64.
#
# Compare against a run of the SAME tree on another arch -- never a stale binary.
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$HERE/../.." && pwd)
[ $# -ge 2 ] || { sed -n '2,19p' "$0" | sed 's/^# \{0,1\}//' >&2; exit 2; }
BIN="$1"; TAG="$2"; PAR="${3:-8}"; TMO="${4:-6000}"
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }
BIN=$(readlink -f "$BIN")

( cd "$HERE/tars" && sha256sum --quiet -c SHA256SUMS ) \
  || { echo "tars/ does not match tars/SHA256SUMS" >&2; exit 1; }

HOSTARCH=$(uname -m)
case "$(file -b "$BIN")" in
  *"Intel 80386"*)
    EMU="${QEMU_I386:-qemu-i386-static}" ;;
  *"ARM aarch64"*)
    if [ "$HOSTARCH" = aarch64 ]; then EMU=""; else EMU="${QEMU_AARCH64:-qemu-aarch64-static}"; fi ;;
  *"x86-64"*)
    if [ "$HOSTARCH" = x86_64 ]; then EMU=""; else EMU="${QEMU_X86_64:-qemu-x86_64-static}"; fi ;;
  *) echo "unrecognised binary: $(file -b "$BIN")" >&2; exit 1 ;;
esac
if [ -n "$EMU" ]; then
  command -v "$EMU" >/dev/null 2>&1 \
    || { echo "$EMU not found (install qemu-user-static or set QEMU_*)" >&2; exit 1; }
fi

LOGS="${LADDER_LOGS:-$ROOT/tmp/ladder-logs}"
D="$LOGS/$TAG"
mkdir -p "$D"
python3 "$HERE/gen-drivers.py" --tars "$HERE/tars" --out "$D/drivers" || exit 1

SHIM="$D/.shim"
if [ -n "$EMU" ]; then
  printf '#!/bin/bash\nexec %s %s "$@"\n' "$EMU" "$BIN" > "$SHIM"
else
  printf '#!/bin/bash\nexec %s "$@"\n' "$BIN" > "$SHIM"
fi
chmod +x "$SHIM"

echo "ladder: $BIN${EMU:+ (via $EMU)} -> $D"
run_one() {
  lib="$1"
  s=$(date +%s)
  timeout "$TMO" "$SHIM" --load "$D/drivers/$lib.lisp" --quit > "$D/$lib.log" 2>&1
  rc=$?
  e=$(date +%s)
  echo "EXIT=$rc SECS=$((e-s))" >> "$D/$lib.log"
}
export -f run_one; export SHIM D TMO
xargs -P "$PAR" -I{} bash -c 'run_one {}' < "$D/drivers/ladder.txt"
rm -f "$SHIM"
echo "LADDER-QL-DONE $TAG"
