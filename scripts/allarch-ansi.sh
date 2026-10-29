#!/bin/bash
# allarch-ansi.sh — the upstream ansi-test suite on every non-x64 hosted CLI.
#
#   scripts/allarch-ansi.sh [WORKDIR] [ARCH...]
#
# The SECONDARY suite: NOT part of scripts/acceptance-gate.sh, which must stay
# under half an hour and sweeps x64 only.  Under qemu-user a port with no
# runtime JIT interprets everything the suite loads, so one full sweep takes
# 1-2 hours there.  Run this on demand and before merges that touch a
# translator, the runtime or the compiler.  A port moves into the gate once it
# has a JIT and its sweep fits the budget.
#
# Builds each CLI from THIS tree (default ARCHs: aarch64 i386 arm32 riscv32
# riscv64) into WORKDIR (default /var/tmp/modus-allarch), sweeps them all in
# parallel, and prints one line per port.  Exit 0 only if every port passed
# every test and every shard finished.
#
# Per-port settings, each measured (2026-10-29, all six CLIs 21681/21681):
#   i386                    56 shards: at 28, three shards of slow random
#                           printing tests hit ansi-sweep's 2-hour shard limit
#   arm32 riscv32 riscv64   MODUS_FASL=0: with ~28 processes sharing one fasl
#                           cache these ports hit a read error while loading
#                           the suite (open; private caches and a disabled
#                           cache are both clean)
set -u
REPO=$(cd "$(dirname "$0")/.." && pwd)
WORKDIR=${1:-/var/tmp/modus-allarch}
shift || true
ARCHES=${*:-"aarch64 i386 arm32 riscv32 riscv64"}
mkdir -p "$WORKDIR" || exit 2
WORKDIR=$(cd "$WORKDIR" && pwd)

qemu_for() { case $1 in arm32) echo arm ;; *) echo "$1" ;; esac; }

port() { # arch: build, wrap, sweep
  local a=$1 bin=$WORKDIR/modus-$1 wrap=$WORKDIR/run-$1
  ( cd "$REPO" && MODUS_CLI_OUT=$bin sbcl --dynamic-space-size 12288 \
      --script "mvm/build-$a-cli.lisp" > "$WORKDIR/build-$a.log" 2>&1 ) \
    || { echo "BUILD-FAILED" > "$WORKDIR/sweep-$a.result"; return; }
  printf '#!/bin/sh\nexec /usr/bin/qemu-%s-static %s "$@"\n' "$(qemu_for "$a")" "$bin" > "$wrap"
  chmod +x "$bin" "$wrap"
  local n=28 env=
  case $a in
    i386) n=56 ;;
    arm32|riscv32|riscv64) env="MODUS_FASL=0" ;;
  esac
  ( cd "$REPO" && env $env scripts/ansi-sweep.sh -n "$n" "$wrap" "$WORKDIR/sweep-$a" \
      > "$WORKDIR/sweep-$a.log" 2>&1 )
  echo "rc=$?" > "$WORKDIR/sweep-$a.result"
}

for a in $ARCHES; do port "$a" & done
wait

FAIL=0
for a in $ARCHES; do
  s=$WORKDIR/sweep-$a/summary
  if [ -s "$s" ]; then
    inc=$(grep -c '^INCOMPLETE' "$s")
    printf '%-8s %s  incomplete-shards=%s\n' "$a" "$(head -1 "$s")" "$inc"
    grep -q 'failed=0' "$s" && [ "$inc" -eq 0 ] || FAIL=1
  else
    printf '%-8s %s\n' "$a" "$(cat "$WORKDIR/sweep-$a.result" 2>/dev/null || echo NO-RESULT)"
    FAIL=1
  fi
done
[ $FAIL -eq 0 ] && echo "ALLARCH: PASS" || echo "ALLARCH: FAIL"
exit $FAIL
