;;;; ccl-host-compat.lisp — HOST compatibility shim so the Modus build runs
;;;; under Clozure CL (CCL) instead of SBCL.
;;;;
;;;; The Modus build scripts and compiler are ordinary host CL programs that
;;;; reach into a handful of SBCL-internal packages.  This file provides those
;;;; symbols under CCL so the exact same first-party source loads unmodified.
;;;;
;;;; Load this BEFORE any Modus source (the sb-* package-qualified symbols must
;;;; exist at READ time).  Guarded to CCL only — a no-op elsewhere.
;;;;
;;;; The one design fact that makes this small: CCL's reader FULLY EXPANDS
;;;; backquote at read time (`` `(a ,b) `` reads as `(list* 'a (list b))`),
;;;; so the compiler's SBCL-comma decoding path (bq-comma-p / sb-impl::comma /
;;;; sb-int:quasiquote) never fires — the pre-expanded list*/append/cons/quote
;;;; forms are compiled directly.  We only need those symbols to EXIST so the
;;;; source reads; they are inert stubs under CCL.
;;;;
;;;; NOTE ON STRUCTURE: the package definitions MUST be their own top-level
;;;; form, evaluated before the second form (which uses sb-*: qualified
;;;; symbols) is even READ.  Do not merge the two progns.

;;; ---- Form 1: create the SBCL-internal packages -------------------------
#+ccl
(progn
  (defpackage :sb-ext (:use)
    (:export #:posix-getenv #:run-program #:*posix-argv* #:exit))
  (defpackage :sb-int (:use) (:export #:quasiquote))
  (defpackage :sb-impl (:use)
    (:export #:comma #:comma-p #:comma-expr #:comma-kind))
  (defpackage :sb-kernel (:use)
    (:export #:double-float-high-bits #:double-float-low-bits))
  (defpackage :sb-gray (:use)
    (:import-from :ccl
                  #:fundamental-character-output-stream
                  #:stream-write-char #:stream-write-string
                  #:stream-line-column #:stream-force-output
                  #:stream-finish-output)
    (:export #:fundamental-character-output-stream
             #:stream-write-char #:stream-write-string
             #:stream-line-column #:stream-force-output
             #:stream-finish-output))
  (defpackage :sb-debug (:use) (:export #:print-backtrace #:list-backtrace)))

;;; ---- Form 2: define the shimmed operators ------------------------------
#+ccl
(progn

  ;; --- SB-EXT: process/environment ---------------------------------------
  (defun sb-ext:posix-getenv (name) (ccl:getenv name))

  (defun sb-ext:exit (&key (code 0) &allow-other-keys) (ccl:quit code))

  ;; SBCL's run-program returns a process object; only the chmod call uses it
  ;; and that is #+sbcl-guarded, so behaviour parity is not required.
  (defun sb-ext:run-program (program args &key wait &allow-other-keys)
    (ccl:run-program program args :wait wait))

  ;; argv[0] plus the args CCL hands the script.  Kept as a plain list so the
  ;; symbol resolves; Modus supplies its own *posix-argv* in-image at runtime.
  (defparameter sb-ext:*posix-argv*
    (cons "modus" (rest ccl:*command-line-argument-list*)))

  ;; --- SB-INT / SB-IMPL: backquote representation (inert under CCL) -------
  ;; sb-int:quasiquote is only ever compared against with EQ; it never appears
  ;; in CCL-read data, so the bare interned symbol above is all that is needed.
  ;; A real (never-matched) struct type makes (typep x 'sb-impl::comma) => NIL
  ;; for every CCL value — exactly the pre-expanded-backquote semantics.
  (defstruct (sb-impl::comma (:predicate sb-impl::comma-p)) expr kind)

  ;; --- SB-KERNEL: raw IEEE-754 double bits -------------------------------
  (defun sb-kernel:double-float-high-bits (f)
    ;; SBCL returns a SIGNED (signed-byte 32); CCL's double-float-bits is
    ;; unsigned.  Sign-convert so the emitted float word is byte-identical
    ;; to an SBCL build.
    (let ((hi (nth-value 0 (ccl::double-float-bits (float f 1.0d0)))))
      (if (>= hi #x80000000) (- hi #x100000000) hi)))

  (defun sb-kernel:double-float-low-bits (f)
    (nth-value 1 (ccl::double-float-bits (float f 1.0d0))))

  ;; --- SB-DEBUG: backtraces (harness diagnostics) ------------------------
  (defun sb-debug:print-backtrace (&rest args)
    (declare (ignore args)) (ccl:print-call-history))
  (defun sb-debug:list-backtrace (&rest args) (declare (ignore args)) nil)

  (format t "~&[ccl-host-compat] SBCL compatibility shim installed under ~A.~%"
          (lisp-implementation-type)))
