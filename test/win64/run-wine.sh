#!/bin/sh
# run-wine.sh - test the Windows hosted CLI (modus.exe) under Wine.
#
# Usage: test/win64/run-wine.sh MODUS_EXE
#
# WINE_RUN names the command that runs one Windows program under Wine, with the
# program path as its argument.  Default: `wine64' on PATH.  The Wine tree built
# here without root is run through /home/claude/tools/wine-run.sh, which
# overlays it on /usr (see the notes in boot/boot-win64.lisp).
#
# Every case feeds a form on stdin and requires a value line on stdout.  The
# run starts with a deliberately WRONG expectation: it must fail, or the
# checks below are not evidence of anything.

set -u
exe=${1:?usage: run-wine.sh MODUS_EXE}
run=${WINE_RUN:-wine64}
here=$(cd "$(dirname "$0")" && pwd)
pass=0; fail=0

run_form() {  # run_form FORM -> the image's stdout, prompts stripped, one value per line
  printf '%s\n' "$1" | $run "$exe" 2>/dev/null | sed 's/^> //'
}

expect() {    # expect NAME FORM WANT
  out=$(run_form "$2")
  rc=$?
  if printf '%s\n' "$out" | grep -qx -- "$3"; then
    echo "ok   $1 ($2 => $3)"; pass=$((pass+1))
  else
    echo "FAIL $1 ($2): wanted line '$3', got:"; printf '%s\n' "$out" | sed 's/^/     | /' | head -20
    echo "     exit=$rc"; fail=$((fail+1))
  fi
}

echo "== control: a wrong expectation must be caught"
if run_form '(+ 1 2)' | grep -qx -- '4'; then
  echo "CONTROL FAILED: the check accepted 4 for (+ 1 2); the harness cannot fail"
  exit 2
fi
echo "ok   control rejects 4 for (+ 1 2)"

echo "== modus.exe under Wine: $exe"
expect "addition"        '(+ 1 2)'                 '3'
expect "multiplication"  '(* 6 7)'                 '42'
expect "bignum"          '(expt 2 100)'            '1267650600228229401496703205376'
expect "defun + call"    '(progn (defun sq (x) (* x x)) (sq 12))' '144'
expect "list printing"   "(list 1 2 3)"            '(1 2 3)'
# Allocation churn: on a small-semispace build (MODUS_X64_MIDPOINT=16777216) these
# force many collections, so they check the copying collector under the shim.
expect "gc churn"        '(let ((s 0)) (dotimes (i 200000) (setq s (+ s (length (list i i i))))) s)' '600000'
expect "gc live list"    '(let ((l nil)) (dotimes (i 100000) (push i l)) (+ (car l) (length l)))' '199999'

# The runtime JIT's calling paths (a MODUS_WIN64_JIT=1 image; the interpreter
# image must give the same answers).  A DEFUN goes native after 64 calls; a
# native caller reaches a still-interpreted callee through a bridge thunk (the
# any-arity one for 4+ arguments and &rest), and a redefinition reaches native
# callers through the callee's linkage stub.
expect "jit hot defun"   '(progn (defun jf (n) (let ((s 0)) (dotimes (i n) (setq s (+ s i))) s)) (dotimes (k 100) (jf 10)) (jf 1000))' '499500'
expect "jit 5-arg bridge" '(progn (defun j5 (a b c d e) (+ a b c d e)) (defun cj5 () (j5 1 2 3 4 5)) (dotimes (k 100) (cj5)) (cj5))' '15'
expect "jit &rest bridge" '(progn (defun jr (&rest xs) (length xs)) (defun cjr () (jr 1 2 3 4 5 6)) (dotimes (k 100) (cjr)) (cjr))' '6'
# Separate top-level forms: inside one PROGN the module binds JG early (and says so).
expect "jit redefinition" '(defun jg () 1)
(defun jh () (jg))
(dotimes (k 100) (jh))
(defun jg () 2)
(list :redef (jh))' '(REDEF 2)'

# Files, through relative paths in a scratch directory.
scratch=$(mktemp -d) || exit 1
exe_abs=$(cd "$(dirname "$exe")" && pwd)/$(basename "$exe")
cd "$scratch" || exit 1
exe=$exe_abs
expect "file write+read" '(progn (with-open-file (s "w64-io.txt" :direction :output :if-exists :supersede) (write-line "hello file" s)) (with-open-file (s "w64-io.txt") (list (read-line s) (file-length s))))' '("hello file" 11)'
expect "probe+delete"    '(list (not (null (probe-file "w64-io.txt"))) (progn (delete-file "w64-io.txt") (probe-file "w64-io.txt")))' '(T NIL)'
printf '(defun w64-loaded () 42)\n' > w64-load.lisp
expect "load a file"     '(progn (load "w64-load.lisp") (w64-loaded))' '42'
printf '(write-line (princ-to-string (* 6 7)))\n' > w64-script.lisp
if $run "$exe" --script w64-script.lisp 2>/dev/null | grep -qx -- '42'; then
  echo "ok   --script a file"; pass=$((pass+1))
else
  echo "FAIL --script a file"; fail=$((fail+1))
fi
# A drive-letter path: absolute, the drive its first component (Wine's Z: is /).
# Windows semantics only: elsewhere "C:/x" is a relative path, as it should be.
case "$exe" in *.exe)
expect "drive path parse" '(list (namestring (parse-namestring "C:/x/y.lisp")) (pathname-directory "C:/x/y.lisp"))' '("C:/x/y.lisp" (ABSOLUTE "C:" "x"))'
expect "drive path file" "(progn (with-open-file (s \"Z:$scratch/d.txt\" :direction :output :if-exists :supersede) (write-line \"drive\" s)) (with-open-file (s \"Z:$scratch/d.txt\") (read-line s)))" '"drive"'
expect "drive truename"  "(namestring (truename \"Z:$scratch/d.txt\"))" "\"Z:$scratch/d.txt\""
esac
cd / && rm -rf "$scratch"

echo "== $pass passed, $fail failed"
[ "$fail" -eq 0 ]
