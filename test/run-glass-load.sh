#!/bin/bash
# run-glass-load.sh — THE `:glass` SYSTEM LOADS ON MODUS, RE-RUNNABLY.
#
#   test/run-glass-load.sh [MODUS-BINARY] [GLASS-SRC-DIR] [CRAM-SRC-DIR]
#
# `:glass' is the RFB server: glass/fb's three files, glass/clipboard's one,
# and its own four, over cram's five (its zlib).  Loading all of them under
# modus was claimed once with nothing behind it; this is the claim as a command
# anybody can run.
#
# THE FILE LIST COMES OUT OF THE .asd FILES, NOT OUT OF THIS SCRIPT.  A
# component added to :glass appears here automatically.  The extraction is a
# tiny Lisp reader run under SBCL over `glass.asd' and `cram.asd' — SBCL is
# already the oracle for the other glass test in this tree, and using a real
# reader means a component list written across several lines, or reordered, is
# still read correctly.
#
# WHAT WOULD MAKE THIS A LIE:
#
#   "LOAD DID NOT SIGNAL" IS NOT "IT LOADED".  modus's LOAD swallows a toplevel
#   form that dies, so the test is that a named function from each file is
#   FBOUND afterwards.  The witness for each file is chosen near the END of that
#   file, so a form swallowed in the middle still fails the check.
#
#   A MODUS-ONLY RUN PROVES THE WITNESS LIST, NOT THE LOAD.  So the identical
#   program is also run under SBCL, where :glass is known to load, and the
#   witness lines must match.  If SBCL cannot find a witness either, the witness
#   is wrong and the script says so instead of blaming modus.
#
# glass and cram are READ-ONLY: nothing is written into either tree.
set -u
BIN=${1:-./modus}
GLASS=${2:-/home/claude/glass}
CRAM=${3:-/home/claude/cram}
cd "$(dirname "$0")/.." || exit 1
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }
[ -f "$GLASS/glass.asd" ] || { echo "no glass at $GLASS" >&2; exit 1; }
[ -f "$CRAM/cram.asd" ]   || { echo "no cram at $CRAM" >&2; exit 1; }

WORK=$(mktemp -d) || exit 1
trap 'rm -rf "$WORK"' EXIT

# ---- the manifest, read out of the .asd files -------------------------------
# test/glass-manifest.lisp has the file list and the witness list; it is a
# separate file because the later runners (serve one client, serve two) load
# :glass the same way and must not carry a second copy of it.
sbcl --script test/glass-manifest.lisp "$GLASS/" "$CRAM/" "$WORK/manifest.lisp"
if [ $? -ne 0 ]; then
  echo "FAIL: could not build the manifest from the .asd files" >&2; exit 1
fi

NFILES=$(grep -c '^  (' "$WORK/manifest.lisp")

echo "=== :glass LOADS ON MODUS ==="
echo "modus:  $BIN"
echo "glass:  $GLASS  (read-only)"
echo "cram:   $CRAM   (read-only)"
echo "files:  $NFILES, from glass.asd + cram.asd"
echo

cat "$WORK/manifest.lisp" > "$WORK/runner.lisp"
cat test/glass-load.lisp >> "$WORK/runner.lisp"

# ---- SBCL first: it validates the WITNESS LIST -------------------------------
echo "-- sbcl (validating the witnesses) --"
if timeout 300 sbcl --script "$WORK/runner.lisp" > "$WORK/sbcl.txt" 2>"$WORK/sbcl.err"; then
  echo "   $(grep -c '^ok ' "$WORK/sbcl.txt") ok"
else
  echo "FAIL: the oracle itself did not load :glass — the witness list is wrong,"
  echo "      or glass/cram moved.  This is a HARNESS failure, not a modus one."
  sed -n '1,15p' "$WORK/sbcl.err"
  grep '^FAIL' "$WORK/sbcl.txt" | head -10
  exit 1
fi

# ---- modus ------------------------------------------------------------------
echo "-- modus --"
# stderr is dropped on purpose: modus writes compile chatter there.
timeout 900 "$BIN" --script "$WORK/runner.lisp" > "$WORK/modus.txt" 2>"$WORK/modus.err"
RC=$?
grep -E '^(ok|FAIL|LOADED|PASS)' "$WORK/modus.txt" | sed 's/^/   /'
echo

if [ "$RC" -eq 0 ] && grep -q '^PASS' "$WORK/modus.txt"; then
  echo "PASS: $(grep '^LOADED' "$WORK/modus.txt")"
  exit 0
else
  echo "FAIL: rc=$RC"
  grep '^FAIL' "$WORK/modus.txt" | head -20
  sed -n '1,20p' "$WORK/modus.err"
  exit 1
fi
