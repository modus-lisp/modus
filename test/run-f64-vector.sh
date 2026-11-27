#!/bin/sh
# test/run-f64-vector.sh [MODUS] -- test/f64-vector.lisp under SBCL and modus: every "=>"
# line must be identical (SBCL is the reference), and modus's own "#M" checks must pass.
M=${1:-./modus}
here=$(dirname "$0")
tmp=${TMPDIR:-/tmp}/f64-vector.$$
mkdir -p "$tmp" || exit 2
sbcl --script "$here/f64-vector.lisp" > "$tmp/sbcl" 2>/dev/null || { echo "FAIL: SBCL run"; exit 2; }
"$M" --script "$here/f64-vector.lisp" > "$tmp/modus.all" 2>&1
mrc=$?
grep -v '^#M' "$tmp/modus.all" > "$tmp/modus"
rc=0
if diff "$tmp/sbcl" "$tmp/modus" > "$tmp/diff"; then
  echo "f64-vector: $(wc -l < "$tmp/sbcl") lines identical to SBCL"
else
  echo "FAIL: f64-vector differs from SBCL:"; head -40 "$tmp/diff"; rc=1
fi
grep '^#M' "$tmp/modus.all"
[ $mrc -eq 0 ] || { echo "FAIL: modus checks (exit $mrc)"; rc=1; }
rm -rf "$tmp"
exit $rc
