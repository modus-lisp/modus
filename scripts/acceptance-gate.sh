#!/bin/bash
# acceptance-gate.sh — may FIX merge?  Compares it against BASE three ways.
#
#   scripts/acceptance-gate.sh BASE-REF FIX-REF [WORKDIR]
#
# 1. BUILDS.  scripts/build-matrix.sh builds every image at both refs.  A
#    build that succeeds at BASE and fails at FIX fails the gate.  (Images
#    are expected to change; their bytes are not compared here — use
#    build-matrix-compare.sh for a change that claims to be byte-identical.)
# 2. WEB.  FIX's JS-MVM image must boot under node and evaluate (* 6 7).
# 3. CONFORMANCE.  scripts/ansi-sweep.sh runs the upstream ansi-test suite
#    on both refs' x64 CLI.  Pass sets are compared BY TEST NAME.  Every test
#    lost is re-run alone on both binaries: one that passes on FIX the second
#    time is reported as a flake; one that passes on BASE and fails on FIX
#    again fails the gate.  A shard that does not finish at FIX but did at
#    BASE fails it too.
#
# Worktrees, images and transcripts go in WORKDIR (default /var/tmp/modus-gate),
# which is left in place for inspection.  Exit 0 = PASS.
set -u
BASE_REF=${1:?usage: acceptance-gate.sh BASE-REF FIX-REF [WORKDIR]}
FIX_REF=${2:?usage: acceptance-gate.sh BASE-REF FIX-REF [WORKDIR]}
WORKDIR=${3:-/var/tmp/modus-gate}
# NOT UNDER /tmp: build-matrix.sh runs each build in a namespace that
# bind-mounts a private /tmp, which hides a worktree that lives under /tmp --
# every build then fails in 0 s with "can't cd", at both refs, and the gate
# reports "no x64 CLI at fix".
case "$WORKDIR" in /tmp|/tmp/*)
  echo "acceptance-gate: WORKDIR must not be under /tmp (build-matrix hides it); got $WORKDIR" >&2
  exit 2 ;;
esac
REPO=$(cd "$(dirname "$0")/.." && pwd)
mkdir -p "$WORKDIR" || exit 2
WORKDIR=$(cd "$WORKDIR" && pwd)
JOBS=${GATE_JOBS:-48}

wt_for() { # ref -> worktree dir (idempotent)
  local dir="$WORKDIR/wt-$(git -C "$REPO" rev-parse --short "$1")"
  [ -d "$dir" ] || git -C "$REPO" worktree add -q --detach "$dir" "$1" >/dev/null || return 1
  echo "$dir"
}

echo "== acceptance gate: base=$BASE_REF fix=$FIX_REF workdir=$WORKDIR =="
# Two image matrices and two sweeps need several GB; a full disk shows up as
# shards dying mid-test (write failed, -28), not as a clear error.
# A run peaks around 4 GB (two image matrices before trimming, then two
# sweeps' trees); GATE_MIN_FREE_GB lowers the 8 GB floor on a tight disk.
free_kb=$(df -Pk "$WORKDIR" | awk 'NR==2 {print $4}')
need_gb=${GATE_MIN_FREE_GB:-8}
if [ "$free_kb" -lt $((need_gb * 1024 * 1024)) ]; then
  echo "VERDICT: NOT RUN (only $((free_kb / 1024)) MB free under $WORKDIR; need $need_gb GB)"; exit 2
fi
BASE_WT=$(wt_for "$BASE_REF") || exit 2
FIX_WT=$(wt_for "$FIX_REF") || exit 2
FAIL=0

# ---- 1. builds --------------------------------------------------------------
# FIX's harness and list drive both sides: the list names what must build.
echo "-- building every image at both refs --"
"$FIX_WT/scripts/build-matrix.sh" -j "$JOBS" -t "$BASE_WT" "$WORKDIR/mx-base" > "$WORKDIR/mx-base.log" 2>&1 &
"$FIX_WT/scripts/build-matrix.sh" -j "$JOBS" -t "$FIX_WT" "$WORKDIR/mx-fix" > "$WORKDIR/mx-fix.log" 2>&1 &
wait
nb=0 nbad=0
for d in "$WORKDIR"/mx-fix/*/; do
  n=$(basename "$d"); rf=$(cat "$d/rc" 2>/dev/null || echo X)
  rb=$(cat "$WORKDIR/mx-base/$n/rc" 2>/dev/null || echo X)
  nb=$((nb + 1))
  if [ "$rf" != 0 ]; then
    if [ "$rb" = 0 ]; then
      echo "  BUILD BROKEN: $n (rc $rf; see $d/build.log)"; nbad=$((nbad + 1)); FAIL=1
    else
      echo "  build fails at both refs: $n (rc base $rb / fix $rf)"
    fi
  fi
done
echo "builds: $nb at fix, $nbad newly broken"

# Keep the build logs and the two images the gate still needs; the rest is
# about a gigabyte per side.
for side in mx-base mx-fix; do
  for d in "$WORKDIR/$side"/*/; do
    case $(basename "$d") in x64-cli|web) ;; *) rm -rf "$d/tmp" ;; esac
  done
done

CLI_BASE=$WORKDIR/mx-base/x64-cli/tmp/modus
CLI_FIX=$WORKDIR/mx-fix/x64-cli/tmp/modus
if [ ! -x "$CLI_FIX" ]; then echo "VERDICT: FAIL (no x64 CLI at fix)"; exit 1; fi
if [ ! -x "$CLI_BASE" ]; then echo "VERDICT: FAIL (no x64 CLI at base; cannot compare)"; exit 2; fi

# ---- 2. web -------------------------------------------------------------------
WEB=$WORKDIR/mx-fix/web/tmp/modus-web
if [ -z "${MODUS_GATE_NO_WEB:-}" ] && command -v node > /dev/null 2>&1 && [ -s "$WEB" ]; then
  WEBOUT=$( cd "$FIX_WT" && timeout 900 node web/run-node.js --mvmw "$WEB" \
              --no-compile --eval '(print (* 6 7))' --quit 2>&1 )
  if echo "$WEBOUT" | grep -q '^42'; then
    echo "web: BOOT-OK (JS MVM evaluated (* 6 7))"
  else
    echo "web: FAIL — the JS-MVM image did not boot"; FAIL=1
    echo "$WEBOUT" | grep -aE 'FAULT|Error|unknown opcode|relocation' | head -3
  fi
else
  echo "web: skipped (MODUS_GATE_NO_WEB, no node, or no web image)"
fi

# ---- 3. conformance -----------------------------------------------------------
echo "-- upstream ansi-test sweep on both CLIs --"
SWEEP=$FIX_WT/scripts/ansi-sweep.sh
"$SWEEP" "$CLI_BASE" "$WORKDIR/sw-base" > /dev/null 2>&1 &
"$SWEEP" "$CLI_FIX" "$WORKDIR/sw-fix" > /dev/null 2>&1 &
wait
for side in base fix; do
  if [ ! -s "$WORKDIR/sw-$side/summary" ] || [ ! -s "$WORKDIR/sw-$side/pass" ]; then
    echo "VERDICT: FAIL (the $side sweep produced no results; see $WORKDIR/sw-$side)"; exit 1
  fi
done
echo "base: $(head -1 "$WORKDIR/sw-base/summary")"
echo "fix:  $(head -1 "$WORKDIR/sw-fix/summary")"
# A base that did not run most of the suite is no baseline: the comparison
# would only see the tests both sides happened to reach.
floor=${GATE_MIN_PASSED:-20000}
for side in base fix; do
  np=$(wc -l < "$WORKDIR/sw-$side/pass")
  if [ "$np" -lt "$floor" ]; then
    echo "VERDICT: FAIL ($side passed only $np tests, below $floor; see $WORKDIR/sw-$side/summary)"; exit 1
  fi
done
inc_b=$(grep -c "^INCOMPLETE" "$WORKDIR/sw-base/summary")
inc_f=$(grep -c "^INCOMPLETE" "$WORKDIR/sw-fix/summary")
if [ "$inc_f" -gt "$inc_b" ]; then
  echo "unfinished shards: $inc_b -> $inc_f"; grep "^INCOMPLETE" "$WORKDIR/sw-fix/summary"; FAIL=1
fi
comm -23 "$WORKDIR/sw-base/pass" "$WORKDIR/sw-fix/pass" > "$WORKDIR/lost"
comm -13 "$WORKDIR/sw-base/pass" "$WORKDIR/sw-fix/pass" > "$WORKDIR/gained"
echo "lost=$(wc -l < "$WORKDIR/lost") gained=$(wc -l < "$WORKDIR/gained")"
if [ -s "$WORKDIR/lost" ]; then
  echo "-- re-running the lost tests alone on both CLIs --"
  "$SWEEP" "$CLI_BASE" "$WORKDIR/re-base" "$WORKDIR/lost" > /dev/null 2>&1 &
  "$SWEEP" "$CLI_FIX" "$WORKDIR/re-fix" "$WORKDIR/lost" > /dev/null 2>&1 &
  wait
  # lost for real: passes on base again and fails on fix again
  comm -12 "$WORKDIR/re-base/pass" "$WORKDIR/re-fix/fail" > "$WORKDIR/lost-real"
  comm -23 "$WORKDIR/lost" "$WORKDIR/lost-real" > "$WORKDIR/lost-flaky"
  [ -s "$WORKDIR/lost-flaky" ] && echo "flaky (passed on re-run): $(tr '\n' ' ' < "$WORKDIR/lost-flaky")"
  if [ -s "$WORKDIR/lost-real" ]; then
    echo "LOST: $(tr '\n' ' ' < "$WORKDIR/lost-real")"; FAIL=1
  fi
fi
[ -s "$WORKDIR/gained" ] && echo "gained: $(tr '\n' ' ' < "$WORKDIR/gained" | cut -c1-600)"

if [ $FAIL -eq 0 ]; then echo "VERDICT: PASS"; exit 0; else echo "VERDICT: FAIL"; exit 1; fi
