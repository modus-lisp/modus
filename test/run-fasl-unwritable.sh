#!/bin/bash
# The FASL cache directory may be unwritable (a read-only or missing $HOME).
# %FASL-PUT then fails to write its record inside IGNORE-ERRORS, which the
# AOT compiler used to compile as a bare PROGN: the FILE-ERROR escaped, so
# every --load ended in "error during --eval/--load" and %DEFER-BOOT-SURFACES
# aborted at boot, leaving no SB-EXT / SB-THREAD.  Run both ways: an
# unwritable cache directory, and a missing $HOME (test/run-env-parity.sh's).
#   test/run-fasl-unwritable.sh [./modus]   => FASL-UNWRITABLE PASS
BIN=${1:-./modus}
d=$(mktemp -d "${TMPDIR:-/tmp}/fasl-unwritable.XXXXXX"); trap 'rm -rf "$d"' EXIT
printf '(defun fu-f () 42)\n(format t "LOADED ~a~%%" (fu-f))\n' > "$d/f.lisp"
fail=0
probe='(progn (format t "PKG ~a~%" (if (find-package "SB-EXT") 1 0)) (sys-exit 0))'
for env in "MODUS_FASL_DIR=/nonexistent/fasl" "HOME=/nonexistent/home"; do
  out=$(env -i HOME=/nonexistent/home $env "$BIN" --load "$d/f.lisp" --eval "$probe" 2>&1 </dev/null)
  if echo "$out" | grep -q "^LOADED 42" && echo "$out" | grep -q "^PKG 1" \
     && ! echo "$out" | grep -q "error during"; then
    echo "ok   $env"
  else
    echo "FAIL $env:"; echo "$out" | grep -v "WARN\|^ *$" | tail -4; fail=1
  fi
done
[ $fail = 0 ] && echo "FASL-UNWRITABLE PASS" || { echo "FASL-UNWRITABLE FAIL"; exit 1; }
