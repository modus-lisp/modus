#!/bin/sh
# run-gc-straddle-natrium.sh MODUS NATRIUM_DIR [N]
#
# Regression for the semispace-straddle corruption (an allocation that passed
# :gc-check just below the end of the LOWER semispace ran into the to-space,
# and the next collection copied survivors over its tail).  Found through
# natrium's Ed25519 verify: TYPE-ERROR DATUM=NIL from BIGNUM-ASH, a field
# element holding a symbol.  It is LAYOUT-sensitive -- whether an object
# straddles the boundary depends on everything allocated before it, including
# the process ENVIRONMENT, which modus copies into the heap at boot -- so this
# runs natrium's suite + Wycheproof under N padded environments (PAD of 0, 8,
# 16 ... bytes) with seeded entropy.  Each environment is deterministic.
#
# Measured on the same tree: 24 of 32 environments failed before the fix
# (TYPE-ERROR or MEMORY-FAULT-ERROR, in the suite or in Wycheproof), 0 of 32
# after.  natrium: https://github.com/modus-lisp/natrium
set -u
MODUS=${1:?modus binary}; NAT=${2:?natrium checkout}; N=${3:-32}
HERE=$(cd "$(dirname "$0")" && pwd); OUT=$(mktemp -d "${TMPDIR:-/tmp}/gc-straddle.XXXXXX")
i=0
while [ $i -lt $N ]; do
  p=$((i * 8)); pad=$(head -c $p /dev/zero | tr '\0' 'x')
  ( PAD="$pad" NATRIUM_DIR="$NAT" timeout 5000 "$MODUS" --script "$HERE/gc-straddle-natrium.lisp" > "$OUT/r-$p.txt" 2>&1 ) &
  i=$((i + 1))
done
wait
fail=0
for f in "$OUT"/r-*.txt; do
  if ! grep -q "^RUNALL-RESULT OK" "$f" || ! grep -q "^WP-RESULT OK" "$f"; then
    fail=$((fail + 1)); echo "FAIL $(basename "$f"): $(grep -E '^(RUNALL|WP)-RESULT' "$f" | tr '\n' ' ')"
  fi
done
echo "GC-STRADDLE-NATRIUM: $((N - fail)) of $N environments clean"
[ $fail -eq 0 ]
