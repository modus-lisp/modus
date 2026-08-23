#!/bin/bash
# run-glass-fb.sh — glass/fb's DRAWING PRIMITIVES UNDER MODUS, WITH SBCL AS THE ORACLE.
#
#   test/run-glass-fb.sh [MODUS-BINARY] [GLASS-SRC-DIR]
#
# :glass/fb is the pure display core of the glass VNC server — an in-memory
# 32-bit framebuffer and its clipped drawing primitives.  It depends on NOTHING
# (no FFI, no sockets, no threads: its one platform seam is an sb-thread lock
# already feature-gated to a no-op where sb-thread is absent, which is modus),
# so it is the first real piece of the glass desktop that can run here.
#
# WHAT WOULD MAKE THIS A LIE, and what stops it.
#
#   A MODUS-ONLY SELF-CHECK PROVES NOTHING.  "The rectangle looks right" is the
#   framebuffer agreeing with itself.  So the identical program is run under
#   SBCL and the two transcripts are compared BYTE FOR BYTE.  SBCL is the
#   oracle; modus has to match it exactly, not plausibly.
#
#   AND THE TRANSCRIPT IS THE PIXELS, not a summary.  Every scene is dumped in
#   full — every pixel, six hex digits — so a difference of one pixel in one
#   rectangle is a diff, not a rounding-off.  The bookkeeping (generation,
#   frameno, accumulated damage, composed CopyRect hint) is dumped too, and a
#   320x200 buffer is folded to a checksum so a large surface is covered as
#   well as the small readable ones.
#
#   NOTHING IN THE PROGRAM READS A CLOCK OR AN ADDRESS, so two correct
#   implementations MUST produce identical bytes.  (fb-mark-frame does stamp
#   get-internal-real-time; that field is deliberately never printed.)
#
# glass is READ-ONLY here: this loads its three source files where they sit and
# writes nothing into that tree.
set -u
BIN=${1:-./modus}
GLASS=${2:-/home/claude/glass}
cd "$(dirname "$0")/.." || exit 1
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }
[ -f "$GLASS/src/framebuffer.lisp" ] || { echo "no glass at $GLASS" >&2; exit 1; }

WORK=$(mktemp -d); trap 'rm -rf "$WORK"' EXIT
DRAW=$PWD/test/glass-fb-draw.lisp

cat > "$WORK/runner.lisp" <<LISP
(load "$GLASS/src/packages.lisp")
(load "$GLASS/src/record.lisp")
(load "$GLASS/src/framebuffer.lisp")
(load "$DRAW")
LISP

echo "=== glass/fb UNDER MODUS, SBCL AS THE ORACLE ==="
echo "modus:  $BIN"
echo "glass:  $GLASS  (read-only)"
echo

echo "-- sbcl --"
if ! timeout 300 sbcl --script "$WORK/runner.lisp" > "$WORK/sbcl.txt" 2>"$WORK/sbcl.err"; then
  echo "FAIL: the oracle itself did not run"; sed -n '1,20p' "$WORK/sbcl.err"; exit 1
fi
echo "   $(wc -c < "$WORK/sbcl.txt") bytes"

echo "-- modus --"
# stderr is dropped on purpose: modus writes compile chatter (trampoline /
# handler-helper / implicit-global notes) there, and the transcript is stdout.
if ! timeout 600 "$BIN" --script "$WORK/runner.lisp" > "$WORK/modus.txt" 2>"$WORK/modus.err"; then
  echo "FAIL: modus did not run the program"; sed -n '1,20p' "$WORK/modus.err"; exit 1
fi
echo "   $(wc -c < "$WORK/modus.txt") bytes"
echo

if cmp -s "$WORK/sbcl.txt" "$WORK/modus.txt"; then
  echo "md5 $(md5sum < "$WORK/sbcl.txt" | cut -d' ' -f1)   $(wc -l < "$WORK/sbcl.txt") lines"
  echo "PASS: BYTE-IDENTICAL to SBCL."
  exit 0
else
  echo "FAIL: modus and SBCL disagree."
  diff "$WORK/sbcl.txt" "$WORK/modus.txt" | head -40
  exit 1
fi
