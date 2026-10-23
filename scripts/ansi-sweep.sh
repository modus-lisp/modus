#!/bin/bash
# ansi-sweep.sh — run the upstream ansi-test suite on a Modus CLI binary.
#
#   scripts/ansi-sweep.sh [-n NSHARDS] BINARY OUTDIR [NAMES-FILE]
#
# Runs the whole suite in NSHARDS parallel processes (default 32), or with
# NAMES-FILE only the tests it names (one process).  Each shard gets its own
# copy of the suite under OUTDIR/trees.  Results:
#   OUTDIR/sN.out      each shard's transcript
#   OUTDIR/pass        names of passing tests, sorted
#   OUTDIR/fail        names of failing tests, sorted
#   OUTDIR/summary     counts, and every shard that did not finish
#
# The suite is upstream ansi-test at a pinned commit, cloned once into
# $MODUS_ANSI_UPSTREAM (default ~/.cache/modus/ansi-test).  Point
# MODUS_ANSI_UPSTREAM at an existing clone to reuse it; the sweep copies the
# commit's tracked files only, so leftovers in a working copy do not leak in.
set -u
ANSI_COMMIT=ca06bd919661af162c67407c9d994e881870bdb3
ANSI_URL=https://gitlab.common-lisp.net/ansi-test/ansi-test.git
here=$(cd "$(dirname "$0")/.." && pwd)
n=32
while getopts "n:" o; do
  case $o in
    n) n=$OPTARG ;;
    *) echo "usage: $0 [-n NSHARDS] BINARY OUTDIR [NAMES-FILE]" >&2; exit 2 ;;
  esac
done
shift $((OPTIND - 1))
[ $# -ge 2 ] || { echo "usage: $0 [-n NSHARDS] BINARY OUTDIR [NAMES-FILE]" >&2; exit 2; }
bin=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
mkdir -p "$2"; out=$(cd "$2" && pwd)
names=
[ $# -ge 3 ] && names=$(cd "$(dirname "$3")" && pwd)/$(basename "$3")
[ -x "$bin" ] || { echo "not executable: $bin" >&2; exit 2; }

src=${MODUS_ANSI_UPSTREAM:-$HOME/.cache/modus/ansi-test}
mkdir -p "$(dirname "$src")"
# Two sweeps started together (the gate runs base and fix at once) must not
# both clone into an empty $src: hold a lock while making sure it is there.
(
  flock 9
  if [ ! -d "$src/.git" ]; then
    git clone -q "$ANSI_URL" "$src" || { echo "cannot clone $ANSI_URL" >&2; exit 2; }
  fi
  git -C "$src" cat-file -e "$ANSI_COMMIT^{commit}" 2>/dev/null \
    || git -C "$src" fetch -q origin || { echo "cannot fetch $ANSI_URL" >&2; exit 2; }
) 9> "$src.lock" || exit 2

rm -rf "$out/trees" "$out/fasl" "$out"/s*.out "$out/pass" "$out/fail" "$out/summary"
mkdir -p "$out/trees"
# A private fasl cache, removed at the end: the shared one in ~/.cache would
# otherwise collect entries keyed by these throwaway tree paths, and a sweep
# should not depend on what earlier runs left there.
export MODUS_FASL_DIR=${MODUS_FASL_DIR:-$out/fasl/}
mkdir -p "$MODUS_FASL_DIR"
pristine=$out/trees/pristine
mkdir -p "$pristine"
git -C "$src" archive "$ANSI_COMMIT" | tar -x -C "$pristine"
cp "$here/test/ansi-upstream/shard.lisp" "$pristine/zz-shard.lisp"

run_shard() { # index lo hi
  local t=$out/trees/t$1
  cp -a "$pristine" "$t"
  ( cd "$t" && SHARD_LO=$2 SHARD_HI=$3 ${names:+SHARD_NAMES=$names} \
      timeout 7200 "$bin" --script zz-shard.lisp < /dev/null > "$out/s$1.out" 2>&1
    echo "rc=$?" >> "$out/s$1.out" )
}

if [ -n "$names" ]; then
  run_shard 0 0 0
else
  total=21676    # tests in the pinned suite; the last shard runs to the end
  per=$(( (total + n - 1) / n ))
  for i in $(seq 0 $((n - 1))); do
    hi=$(( (i + 1) * per )); [ "$i" -eq $((n - 1)) ] && hi=1000000
    run_shard "$i" $((i * per)) "$hi" &
  done
  wait
fi

cat "$out"/s*.out | grep -a "^PASS "  | awk '{print $2}' | sort -u > "$out/pass"
cat "$out"/s*.out | grep -a "^FAILX " | awk '{print $2}' | sort -u > "$out/fail"
{
  echo "passed=$(wc -l < "$out/pass") failed=$(wc -l < "$out/fail")"
  for f in "$out"/s*.out; do
    grep -qa "==SHARD-END" "$f" || \
      echo "INCOMPLETE $(basename "$f" .out) $(grep -a '^rc=' "$f" | tail -1) last=$(grep -a '^>>' "$f" | tail -1)"
  done
} > "$out/summary"
rm -rf "$out/trees" "$out/fasl"
cat "$out/summary"
# exit 1 when nothing ran or a shard did not finish, so a caller cannot read an
# empty or partial result as a measurement
[ -s "$out/pass" ] || exit 1
! grep -q "^INCOMPLETE" "$out/summary"
