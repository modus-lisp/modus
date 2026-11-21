#!/bin/bash
# test/run-w32-bench.sh -- the W32 throughput measurements (docs/w32-spec.md 9.2)
#
#   test/run-w32-bench.sh IMGDIR [REPS] [KB]
#
# IMGDIR holds riscv32-cli, arm32-cli and i386-cli, each built with
#   MODUS_EXTRA_SOURCE=test/w32-bench.lisp
# Every run's output lines are compared with SBCL's for the same sizes
# (test/w32-bench-check.lisp, which first checks the copies against natrium,
# FIPS 180-2, Python hashlib and RFC 8439); a mismatch fails the script.
# Prints one line per (arch, rep) and a median per arch, MB/s from the
# in-image clock.  All runs go in parallel: one process each, nothing shared.
set -u
IMG=${1:?usage: run-w32-bench.sh IMGDIR [REPS] [KB]}
REPS=${2:-5}
KB=${3:-64}
HERE=$(cd "$(dirname "$0")" && pwd)
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT

sbcl --script "$HERE/w32-bench-check.lisp" "$KB" "$KB" > "$T/sbcl.out" 2>&1 \
  || { echo "SBCL reference check FAILED:"; cat "$T/sbcl.out"; exit 1; }
# The reference lines: everything but the timings.
strip() { sed -n -e 's/ us=[0-9]*//' -e '/^abc=\|^rfc8439\|^sha256 \|^chacha20 /p' "$1"; }
strip "$T/sbcl.out" > "$T/want"
echo "SBCL: $(grep -E '^(sha256|chacha20) ' "$T/sbcl.out" | sed 's/ digest=.*\| sha256-of-ct=.*//' | tr '\n' ' ')"

declare -A QEMU=([riscv32]=qemu-riscv32-static [arm32]=qemu-arm-static [i386]=qemu-i386-static)
for a in riscv32 arm32 i386; do
  for r in $(seq 1 "$REPS"); do
    ( timeout 3600 "${QEMU[$a]}" "$IMG/$a-cli" \
        --eval "(modus-int::w32b-run $KB $KB)" --quit > "$T/$a.$r" 2>&1 ) &
  done
done
wait

fail=0
for a in riscv32 arm32 i386; do
  sha=(); cha=()
  for r in $(seq 1 "$REPS"); do
    if ! diff -q <(strip "$T/$a.$r") "$T/want" > /dev/null; then
      echo "$a rep $r: OUTPUT MISMATCH"; diff <(strip "$T/$a.$r") "$T/want" | head; fail=1; continue
    fi
    s=$(sed -n 's/^sha256 .* us=\([0-9]*\).*/\1/p' "$T/$a.$r")
    c=$(sed -n 's/^chacha20 .* us=\([0-9]*\).*/\1/p' "$T/$a.$r")
    sha+=("$s"); cha+=("$c")
    echo "$a rep $r: sha256 ${s} us  chacha20 ${c} us  (outputs match SBCL)"
  done
  [ ${#sha[@]} -gt 0 ] || continue
  med() { printf '%s\n' "$@" | sort -n | awk '{a[NR]=$1} END {print a[int((NR+1)/2)]}'; }
  ms=$(med "${sha[@]}"); mc=$(med "${cha[@]}")
  awk -v a="$a" -v kb="$KB" -v s="$ms" -v c="$mc" -v n="${#sha[@]}" 'BEGIN {
    printf "%-8s median of %d: sha256 %.4f MB/s (%d us)  chacha20 %.4f MB/s (%d us)\n",
           a, n, kb*1024/s, s, kb*1024/c, c }'
done
exit $fail
