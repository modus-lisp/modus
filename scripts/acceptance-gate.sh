#!/bin/bash
# acceptance-gate.sh — the formal merge gate for Modus shared-source changes.
#
# Usage: scripts/acceptance-gate.sh <base-ref> <fix-ref> [workdir]
#
# What it does (encoding the practice from CLAUDE.md "ANSI Conformance"):
#   1. Adds a git worktree per ref under WORKDIR (default /tmp/modus-gate).
#   2. Builds the x64-Linux ANSI gate runner (mvm/build-x64-linux.lisp) in
#      EACH worktree, with a per-worktree MODUS_ANSI_OUT (the build's default
#      output path is shared — two concurrent builds would clobber it).
#   3. Builds ONE clean image (build-generic-cli) at the fix ref — the build
#      taxonomy check: a gate runner building says nothing about the 25
#      shipping images.
#   4. Runs the 64-shard sweep (same method BOTH sides — ID sets are only
#      comparable same-run/same-method) via the inlined n-shard runner.
#   5. Verdict:
#        NET      = fix.passed - base.passed   (the reliable metric;
#                   the per-file ABSOLUTE count has ~±200 run-to-run jitter)
#        CC / FW  = CHUNK-CRASH / FILE-WEDGE counts — the timing-IMMUNE
#                   markers; they must NOT increase.
#      PASS  = NET >= -15 (sweep noise band) AND CC/FW not increased AND
#              both builds + the clean image built.
#      On a NET in the noise band or a marker flip, do NOT merge on this
#      script alone: sub-shard the disagreeing range (NSH=128+) and diff the
#      per-ID sets — "judge GC/alloc-heavy changes by SUB-SHARDED comm-diff,
#      never a single coarse shard."
#
# Exit codes: 0 = PASS, 1 = FAIL (verdict), 2 = infrastructure error.
#
# Judgment doctrine (do not weaken in this script):
#   - Judge by CRASH MARKERS + passed, never raw lost-to-crash.
#   - Correctness over regression-avoidance: a small NET loss from a
#     conformance FIX can still merge — but only after the per-ID diff is
#     read and each lost ID is explained.  This script only automates the
#     honest measurement; the explanation is on you.
set -u
BASE_REF="${1:?usage: acceptance-gate.sh <base-ref> <fix-ref> [workdir]}"
FIX_REF="${2:?usage: acceptance-gate.sh <base-ref> <fix-ref> [workdir]}"
WORKDIR="${3:-/tmp/modus-gate}"
REPO="$(cd "$(dirname "$0")/.." && pwd)"
NSH="${NSH:-64}"
START=10001; END=27800

mkdir -p "$WORKDIR" || exit 2

wt_for () { # ref -> worktree dir (idempotent)
  local ref="$1" dir="$WORKDIR/wt-$(git -C "$REPO" rev-parse --short "$1")"
  if [ ! -d "$dir" ]; then
    git -C "$REPO" worktree add --detach "$dir" "$ref" >/dev/null || return 1
  fi
  echo "$dir"
}

build_gate () { # worktree-dir -> builds tmp/ansi-gate-bin inside it
  local dir="$1"
  ( cd "$dir" && mkdir -p tmp && \
    MODUS_ANSI_OUT="$dir/tmp/ansi-gate-bin" \
    sbcl --dynamic-space-size 8192 --script mvm/build-x64-linux.lisp \
    > "$dir/tmp/gate-build.log" 2>&1 )
}

run_shards () { # bin out tag -> prints "tag: passed=N CHUNK-CRASH=c FILE-WEDGE=f"
  local BIN="$1" OUT="$2" TAG="$3"
  local SPAN=$(( (END-START+NSH)/NSH ))
  rm -f "$OUT".shard.* "$OUT"
  local pids=()
  for ((i=0;i<NSH;i++)); do
    local s=$((START + i*SPAN)) e=$((START + i*SPAN + SPAN - 1))
    [ $e -gt $END ] && e=$END
    ( timeout 600 "$BIN" $s $e 2>&1 \
        | grep -aE "^(P:[0-9]+|CHUNK-CRASH|FILE-WEDGE)" > "$OUT".shard.$i ) &
    pids+=($!)
  done
  wait "${pids[@]}" 2>/dev/null
  cat "$OUT".shard.* | grep -aE "^P:" | sort -u > "$OUT"
  local CC FW
  CC=$(cat "$OUT".shard.* | grep -ac "CHUNK-CRASH")
  FW=$(cat "$OUT".shard.* | grep -ac "FILE-WEDGE")
  echo "$TAG: passed=$(wc -l < "$OUT") CHUNK-CRASH=$CC FILE-WEDGE=$FW (NSH=$NSH)"
}

echo "== acceptance gate: base=$BASE_REF fix=$FIX_REF workdir=$WORKDIR =="

BASE_WT=$(wt_for "$BASE_REF") || exit 2
FIX_WT=$(wt_for "$FIX_REF") || exit 2

echo "-- building gate runners (parallel) --"
build_gate "$BASE_WT" & BP=$!
build_gate "$FIX_WT"  & FP=$!
wait $BP; BRC=$?
wait $FP; FRC=$?
if [ $BRC -ne 0 ] || [ ! -x "$BASE_WT/tmp/ansi-gate-bin" ]; then
  echo "FAIL: base gate build failed (see $BASE_WT/tmp/gate-build.log)"; exit 2; fi
if [ $FRC -ne 0 ] || [ ! -x "$FIX_WT/tmp/ansi-gate-bin" ]; then
  echo "FAIL: fix gate build failed (see $FIX_WT/tmp/gate-build.log)"; exit 1; fi

echo "-- building one clean image at fix ref (build taxonomy check) --"
( cd "$FIX_WT" && MODUS_CLI_OUT="$FIX_WT/tmp/modus-cli" \
  sbcl --dynamic-space-size 12288 --script mvm/build-generic-cli.lisp \
  > "$FIX_WT/tmp/cli-build.log" 2>&1 )
if [ ! -x "$FIX_WT/tmp/modus-cli" ]; then
  echo "FAIL: clean image (build-generic-cli) failed at fix ref"; exit 1; fi

echo "-- running 64-shard sweeps (both sides, same method) --"
BLINE=$(run_shards "$BASE_WT/tmp/ansi-gate-bin" "$WORKDIR/base.pass" base)
FLINE=$(run_shards "$FIX_WT/tmp/ansi-gate-bin"  "$WORKDIR/fix.pass"  fix)
echo "$BLINE"; echo "$FLINE"

BP=$(echo "$BLINE" | sed -n 's/.*passed=\([0-9]*\).*/\1/p')
FPASS=$(echo "$FLINE" | sed -n 's/.*passed=\([0-9]*\).*/\1/p')
BCC=$(echo "$BLINE" | sed -n 's/.*CHUNK-CRASH=\([0-9]*\).*/\1/p')
FCC=$(echo "$FLINE" | sed -n 's/.*CHUNK-CRASH=\([0-9]*\).*/\1/p')
BFW=$(echo "$BLINE" | sed -n 's/.*FILE-WEDGE=\([0-9]*\).*/\1/p')
FFW=$(echo "$FLINE" | sed -n 's/.*FILE-WEDGE=\([0-9]*\).*/\1/p')
NET=$((FPASS - BP))
comm -23 "$WORKDIR/base.pass" "$WORKDIR/fix.pass" > "$WORKDIR/lost.ids"
comm -13 "$WORKDIR/base.pass" "$WORKDIR/fix.pass" > "$WORKDIR/gained.ids"
echo "NET=$NET lost=$(wc -l < "$WORKDIR/lost.ids") gained=$(wc -l < "$WORKDIR/gained.ids")"
echo "  (per-ID sets: $WORKDIR/lost.ids / $WORKDIR/gained.ids — read the"
echo "   lost set before merging on any negative NET)"

FAIL=0
[ "$NET" -lt -15 ] && { echo "VERDICT: NET below noise band ($NET < -15)"; FAIL=1; }
[ "$FCC" -gt "$BCC" ] && { echo "VERDICT: CHUNK-CRASH increased ($BCC -> $FCC)"; FAIL=1; }
[ "$FFW" -gt "$BFW" ] && { echo "VERDICT: FILE-WEDGE increased ($BFW -> $FFW)"; FAIL=1; }
if [ $FAIL -eq 0 ]; then echo "VERDICT: PASS"; exit 0; else echo "VERDICT: FAIL"; exit 1; fi
