#!/bin/bash
# get-ansi-corpus.sh — fetch the ANSI CL conformance corpus the gate measures.
#
# WHY THIS EXISTS: five scripts and mvm/build-ansi-common.lisp hard-coded two
# absolute corpus paths with NO statement of where the corpus came from.  A
# conformance number whose corpus cannot be reconstructed is not reproducible,
# and scripts/acceptance-gate.sh PASSES on a machine with no corpus at all —
# it compares two zeroes and reports NET = 0.  See docs/handler-stack-collision.md.
#
# THE SUITE is the ANSI test suite maintained at common-lisp.net (the one GCL
# shipped; SBCL and others test against it).  This script does not vendor it —
# it clones upstream, so the revision is whatever master is on the day you run.
# THAT MATTERS: pass counts are only comparable across runs of the SAME corpus
# revision.  Record the revision this prints alongside any number you quote.
#
#   Usage: scripts/get-ansi-corpus.sh [--force]
#
# TWO PATHS, TWO LAYOUTS, both required by different consumers:
#
#   /home/claude/modus/tmp/ansi-test/        <- THE GATE
#       upstream's own layout, verbatim: chapter dirs (printer/, symbols/,
#       objects/ …) and auxiliary/ at the top.  mvm/build-ansi-common.lisp
#       names these paths absolutely (~line 805) and bakes the corpus into the
#       x64-Linux gate runner.  This is what acceptance-gate.sh measures.
#
#   /home/claude/modus-ref/ansi-test/        <- the per-file scripts
#       a REORGANISED view: chapter dirs moved under tests/, and the *-aux.lsp
#       files moved down into auxiliary/ansi_aux/ (ansi-aux-macros.lsp stays at
#       auxiliary/).  scripts/build-ansi-runner.sh, run-ansi-all.sh and
#       run-ansi-per-file.sh want this shape.
#
# The reorganisation below is RECONSTRUCTED from what those scripts open — it
# was not documented anywhere.  If the per-file runners misbehave, suspect this
# mapping first.
set -eu

UP=https://gitlab.common-lisp.net/ansi-test/ansi-test.git
GATE=/home/claude/modus/tmp/ansi-test
REF=/home/claude/modus-ref/ansi-test
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

if [ -d "$GATE" ] && [ "${1:-}" != "--force" ]; then
  echo "$GATE exists; pass --force to refetch." >&2
  exit 0
fi

echo "cloning $UP ..."
git clone --depth 1 -q "$UP" "$TMP/src"
REV=$(git -C "$TMP/src" rev-parse --short HEAD)
DATE=$(git -C "$TMP/src" log -1 --format=%ad --date=short)

# ---- 1. the gate: upstream layout, verbatim -------------------------------
rm -rf "$GATE"; mkdir -p "$GATE"
( cd "$TMP/src" && cp -r [a-z]*/ "$GATE/" 2>/dev/null || true; cp ./*.lsp "$GATE/" 2>/dev/null || true )

# ---- 2. the per-file scripts: reorganised view ----------------------------
rm -rf "$REF"; mkdir -p "$REF/tests" "$REF/auxiliary/ansi_aux"
cp "$TMP/src/auxiliary/ansi-aux-macros.lsp" "$REF/auxiliary/"
cp "$TMP/src"/auxiliary/*-aux.lsp           "$REF/auxiliary/ansi_aux/"
for d in $(cd "$TMP/src" && ls -d */ | tr -d '/'); do
  case "$d" in auxiliary|doc|sandbox|expected-failures|beyond-ansi|rctest|random) continue;; esac
  if ls "$TMP/src/$d"/*.lsp >/dev/null 2>&1; then
    mkdir -p "$REF/tests/$d"; cp "$TMP/src/$d"/*.lsp "$REF/tests/$d/"
  fi
done

# ---- 3. say what landed, so a quoted number can name its corpus ----------
echo
echo "upstream revision : $REV  ($DATE)"
echo "gate  $GATE"
echo "      $(find "$GATE" -name '*.lsp' | wc -l) .lsp, $(grep -rho '(deftest' "$GATE" --include=*.lsp | wc -l) deftests"
echo "ref   $REF"
echo "      $(find "$REF/tests" -name '*.lsp' | wc -l) .lsp in tests/, $(ls "$REF/auxiliary/ansi_aux"/*.lsp | wc -l) aux"
echo
echo "NOTE: CLAUDE.md records 16,489/17,465 = 94.4%.  If the deftest count above"
echo "      differs materially from 17,465 you are measuring a DIFFERENT corpus"
echo "      than that number, and the two are not comparable."
