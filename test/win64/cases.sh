# cases.sh - the modus.exe checks, shared by run-wine.sh (Wine) and run-vm.sh
# (a real Windows guest).  Sourced, not run.  The sourcing script defines:
#
#   check NAME FORM WANT     feed FORM on stdin; some output line must be WANT
#   script NAME FILE WANT    run `modus.exe --script FILE'; a line must be WANT
#   put FILE TEXT            create FILE (in the working directory) holding TEXT
#
# and sets WDIR to the working directory as a drive-letter path (Z:/tmp/x under
# Wine, D: in the guest).  Cases run in order, in that working directory, and
# later ones may use files earlier ones made.  WANT is a fixed string.

check "addition"        '(+ 1 2)'                 '3'
check "multiplication"  '(* 6 7)'                 '42'
check "bignum"          '(expt 2 100)'            '1267650600228229401496703205376'
check "defun + call"    '(progn (defun sq (x) (* x x)) (sq 12))' '144'
check "list printing"   "(list 1 2 3)"            '(1 2 3)'
# Allocation churn: on a small-semispace build (MODUS_X64_MIDPOINT=16777216) these
# force many collections, so they check the copying collector under the shim.
check "gc churn"        '(let ((s 0)) (dotimes (i 200000) (setq s (+ s (length (list i i i))))) s)' '600000'
check "gc live list"    '(let ((l nil)) (dotimes (i 100000) (push i l)) (+ (car l) (length l)))' '199999'

# The runtime JIT's calling paths.  A DEFUN goes native after 64 calls; a native
# caller reaches a still-interpreted callee through a bridge thunk (the any-arity
# one for 4+ arguments and &rest), and a redefinition reaches native callers
# through the callee's linkage stub.
check "jit hot defun"   '(progn (defun jf (n) (let ((s 0)) (dotimes (i n) (setq s (+ s i))) s)) (dotimes (k 100) (jf 10)) (jf 1000))' '499500'
check "jit 5-arg bridge" '(progn (defun j5 (a b c d e) (+ a b c d e)) (defun cj5 () (j5 1 2 3 4 5)) (dotimes (k 100) (cj5)) (cj5))' '15'
check "jit &rest bridge" '(progn (defun jr (&rest xs) (length xs)) (defun cjr () (jr 1 2 3 4 5 6)) (dotimes (k 100) (cjr)) (cjr))' '6'
# Separate top-level forms: inside one PROGN the module binds JG early (and says so).
check "jit redefinition" '(defun jg () 1)
(defun jh () (jg))
(dotimes (k 100) (jh))
(defun jg () 2)
(list :redef (jh))' '(REDEF 2)'

# Sockets: a loopback listener, connect, accept, send, receive, close -- one
# thread, so the connect completes through the backlog.  Same answer as Linux.
check "socket loopback" '(let* ((l (modus-int::socket-listen 0 4)) (port (modus-int::socket-local-port l)) (c (modus-int::socket-connect 2130706433 port t)) (a (modus-int::socket-accept l)) (out (make-array 5 :element-type (quote (unsigned-byte 8)) :initial-contents (quote (104 101 108 108 111)))) (in (make-array 5 :element-type (quote (unsigned-byte 8))))) (modus-int::socket-send c out 5) (list :sock (> port 0) (modus-int::socket-recv a in 5) (map (quote string) (function code-char) in) (modus-int::socket-close a) (modus-int::socket-close c) (modus-int::socket-close l)))' '(SOCK T 5 "hello" 0 0 0)'

# Files, through relative paths in the working directory.
check "file write+read" '(progn (with-open-file (s "w64-io.txt" :direction :output :if-exists :supersede) (write-line "hello file" s)) (with-open-file (s "w64-io.txt") (list (read-line s) (file-length s))))' '("hello file" 11)'
check "probe+delete"    '(list (not (null (probe-file "w64-io.txt"))) (progn (delete-file "w64-io.txt") (probe-file "w64-io.txt")))' '(T NIL)'
put w64-load.lisp '(defun w64-loaded () 42)'
check "load a file"     '(progn (load "w64-load.lisp") (w64-loaded))' '42'
put w64-script.lisp '(write-line (princ-to-string (* 6 7)))'
script "--script a file" w64-script.lisp '42'

# A drive-letter path: absolute, the drive its first component.  Windows
# semantics only: elsewhere "C:/x" is a relative path, as it should be.
check "drive path parse" '(list (namestring (parse-namestring "C:/x/y.lisp")) (pathname-directory "C:/x/y.lisp"))' '("C:/x/y.lisp" (ABSOLUTE "C:" "x"))'
check "drive path file" "(progn (with-open-file (s \"$WDIR/d.txt\" :direction :output :if-exists :supersede) (write-line \"drive\" s)) (with-open-file (s \"$WDIR/d.txt\") (read-line s)))" '"drive"'
check "drive truename"  "(namestring (truename \"$WDIR/d.txt\"))" "\"$WDIR/d.txt\""
