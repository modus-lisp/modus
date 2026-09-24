;;;; test/gc-epoch-eval.lisp -- EVAL must survive every collection count.
;;;;
;;;; The macroexpansion memo and the JIT page cache stamp themselves with "the
;;;; collection count" and compare stamps with EQL.  They used %GC-COUNT, which
;;;; on x64 hands the raw count word back AS A TAGGED VALUE: a count of 9 is an
;;;; OBJECT pointer to address 0, EQL went numeric, %IEEE-FLOAT-P faulted, and
;;;; every EVAL of a macro form failed while the count stayed 9 -- which it
;;;; does, because failing forms allocate almost nothing.  The real ansi-test
;;;; died in DO-TESTS exactly there.  Now they use %GC-EPOCH.
;;;;
;;;;   ./modus --script test/gc-epoch-eval.lisp     ; exit 0 = PASS
;;;;
;;;; Each round allocates enough to collect at least once, then EVALs a FRESH
;;;; macro form (a repeated form is served from the eval cache and never
;;;; reaches the macroexpander).  The count must pass 9 for this to mean
;;;; anything, so that is asserted too.

(defun gee-burn (n) (let ((x nil)) (dotimes (i n) (setq x (make-array 1000))) (length x)))
(defun gee-count () (%gc-read64 (+ (%gc-region) #x20)))

(defvar *gee-fails* 0)
(dotimes (k 16)
  (gee-burn 150000)
  (let ((v (handler-case (eval (list 'when t (list '+ k 2))) (error () :fail))))
    (unless (eql v (+ k 2))
      (setq *gee-fails* (+ *gee-fails* 1))
      (format t "~&FAIL round ~d gc=~d eval=~s~%" k (gee-count) v))))
(format t "~&collections=~d eval-failures=~d~%" (gee-count) *gee-fails*)
(defvar *gee-ok* (and (zerop *gee-fails*) (> (gee-count) 9)))
(format t "~a~%" (if *gee-ok* "PASS" "FAIL"))
;; At TOPLEVEL: a SYS-EXIT nested inside an IF in a --script does not take
;; effect (CLAUDE.md), and every runner reads the exit code, not the prose.
(sys-exit (if *gee-ok* 0 1))
