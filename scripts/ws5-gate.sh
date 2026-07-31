#!/usr/bin/env bash
# ws5-gate.sh — the standing gate for the WS5 width-parameterization workstream.
#
# Whole-image md5 was RETIRED as the instrument: the image embeds a symbol-name
# table derived by textually scanning the baked source, so ANY refactor that
# introduces a NAME changes the image without changing a single instruction.
# The gate is now per-function.
#
#   usage: ws5-gate.sh <baseline-commit> <new-commit> [outdir]
#
# Builds both commits in FRESH DETACHED WORKTREES (never the tree you are
# editing) and reports:
#   1. x64      per-function native comparison  (scripts/fndiff.py)
#   2. aarch64  per-function native comparison
#   3. relocation proof for any differences     (scripts/reloproof.py)
#   4. functional smoke on both targets
#   5. SELF-HOST: --compile works, output is correct on large-|value|
#      literals, and is BIT-EXACT REPRODUCIBLE across runs.
#
# Why (5) is in the standing gate: compiler.lisp itself carries the
# 2^62-magnitude constants, so self-compiling exercises emit-li-tagged's
# large-|value| path directly — it is not a corner nobody reaches.  And
# self-compile has an unusually strong known-answer test: bit-exactness.
# A past non-determinism bug there corrupted whole modus3 rolls
# (reference_selfcompile_nondeterminism).
set -euo pipefail

BASE=${1:?baseline commit}
NEW=${2:?new commit}
OUT=${3:-/home/claude/ws5-gate-out}
REPO=$(git rev-parse --show-toplevel)
mkdir -p "$OUT"

build() {  # tag commit
  local tag=$1 rev=$2 wt="$OUT/wt-$tag"
  rm -rf "$wt"; git -C "$REPO" worktree add --detach "$wt" "$rev" >/dev/null
  ( cd "$wt"
    MODUS_DUMP_NATIVE=1 MODUS_SYMMAP="$OUT/$tag-x64.symmap" MODUS_CLI_OUT="$OUT/$tag-x64" \
      sbcl --dynamic-space-size 12288 --script mvm/build-generic-cli.lisp  >"$OUT/$tag-x64.log"  2>&1
    MODUS_DUMP_NATIVE=1 MODUS_SYMMAP="$OUT/$tag-aa64.symmap" MODUS_CLI_OUT="$OUT/$tag-aa64" \
      sbcl --dynamic-space-size 12288 --script mvm/build-aarch64-cli.lisp >"$OUT/$tag-aa64.log" 2>&1 )
}

build base "$BASE"
build new  "$NEW"

for a in x64 aa64; do
  echo "######## per-function comparison: $a"
  python3 "$REPO/scripts/fndiff.py" \
      "$OUT/base-$a.symmap" "$OUT/base-$a.native" \
      "$OUT/new-$a.symmap"  "$OUT/new-$a.native" "$a" || true
  python3 "$REPO/scripts/reloproof.py" \
      "$OUT/base-$a.symmap" "$OUT/base-$a.native" \
      "$OUT/new-$a.symmap"  "$OUT/new-$a.native" || true
done

echo "######## functional"
echo '(print (+ 1 2))' | "$OUT/new-x64"            # expect 3
for p in 33333 44444 55555; do                     # walked=100000 / poison-test=0 / SURVIVED-4000
  qemu-aarch64-static "$OUT/new-aa64" $p | tail -2
done

echo "######## self-host (--compile + bit-exact reproducibility)"
( cd "$OUT/wt-new"
  MODUS_CLI_OUT="$OUT/modus-sh" sbcl --dynamic-space-size 12288 \
      --script mvm/build-modus-selfhost.lisp >"$OUT/selfhost.log" 2>&1 )
cat > "$OUT/gate-big.lisp" <<'LISP'
(defun putc (c) (write-char-serial c))
(defun sxit (code) (syscall3 60 code 0 0))
(defun kernel-main ()
  ;; Every check drives emit-li-tagged's large-|value| path (>= 2^61) through
  ;; the IN-IMAGE compiler.  Expect YYYYYY.  NB a bare --compile'd program has
  ;; NO runtime library, so avoid anything that lowers to a call: a POSITIVE
  ;; (ash x n>30) becomes a BIGNUM-ASH call and would report a false N.
  ;; Negative counts inline ("<= 30 bits left, ANY right").
  (if (= (- 4611686018427387903 4611686018427387902) 1) (putc 89) (putc 78))
  (if (= (+ 2305843009213693952 2305843009213693951) 4611686018427387903) (putc 89) (putc 78))
  (if (= (+ -4611686018427387903 4611686018427387903) 0) (putc 89) (putc 78))
  (if (= (ash 2305843009213693952 -1) 1152921504606846976) (putc 89) (putc 78))
  (if (= (ash 4611686018427387903 -61) 1) (putc 89) (putc 78))
  (if (= (* 2305843009213693951 2) 4611686018427387902) (putc 89) (putc 78))
  (putc 10) (sxit 0))
LISP
for i in 1 2 3; do
  "$OUT/modus-sh" --compile "$OUT/gate-big.lisp" "$OUT/gate-big-$i" >/dev/null 2>&1
done
echo -n "  compiled program says (expect YYYYYY): "; "$OUT/gate-big-1"
echo "  reproducibility (all three md5s must match):"
md5sum "$OUT/gate-big-1" "$OUT/gate-big-2" "$OUT/gate-big-3" | sed 's/^/    /'
