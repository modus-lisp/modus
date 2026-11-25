#!/bin/sh
# test/run-unicode-case.sh [MODUS] -- test/unicode-case.lisp under SBCL and modus; the
# outputs must be identical.  Also checks that modus decodes argv as UTF-8.
M=${1:-./modus}
here=$(dirname "$0")
tmp=${TMPDIR:-/tmp}/unicode-case.$$
mkdir -p "$tmp" || exit 2
sbcl --script "$here/unicode-case.lisp" > "$tmp/sbcl" 2>&1 || { echo "FAIL: SBCL run"; exit 2; }
"$M" --script "$here/unicode-case.lisp" > "$tmp/modus" 2>&1
rc=0
if diff "$tmp/sbcl" "$tmp/modus" > "$tmp/diff"; then
  echo "unicode-case: $(wc -l < "$tmp/sbcl") lines identical to SBCL"
else
  echo "FAIL: unicode-case differs from SBCL:"; head -20 "$tmp/diff"; rc=1
fi
e='(progn (princ (map (quote list) (function char-code) "été")) (terpri))'
want=$(sbcl --noinform --non-interactive --eval "$e" 2>/dev/null | tail -1)
got=$("$M" --eval "$e" --quit 2>/dev/null | tail -1)
if [ "$want" = "$got" ]; then echo "argv: UTF-8 decoded ($got)"; else echo "FAIL: argv: SBCL $want, modus $got"; rc=1; fi
rm -rf "$tmp"
exit $rc
