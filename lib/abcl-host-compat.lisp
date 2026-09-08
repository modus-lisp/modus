;;;; abcl-host-compat.lisp — HOST compatibility shim so the Modus build runs
;;;; under Armed Bear Common Lisp (ABCL, on the JVM) instead of SBCL.
;;;;
;;;; Same idea as lib/ccl-host-compat.lisp: the build scripts and compiler are
;;;; ordinary host-CL programs that reach into a handful of SBCL-internal
;;;; packages; we provide those symbols so the exact same first-party source
;;;; builds unmodified.  Load this BEFORE any Modus source.
;;;;
;;;; The one ABCL-specific wrinkle is BACKQUOTE.  Like CCL, ABCL expands
;;;; backquote at READ time — but into its OWN operators (SYSTEM::BACKQ-LIST*,
;;;; BACKQ-APPEND, …) rather than the standard LIST*/APPEND that CCL emits.
;;;; The Modus compiler reads the assembled image source with the HOST reader
;;;; (cross.lisp: `(read stream …)`), so those BACKQ-* symbols would reach the
;;;; compiler as calls to undefined functions.  Rather than hand-roll a
;;;; backquote reader (nesting/dotted/vector correctness is treacherous), we
;;;; REUSE ABCL's own battle-tested expansion and merely RENAME the operators
;;;; back to their standard-CL equivalents (BACKQ-LIST → LIST, etc.), which
;;;; Modus compiles natively.  This leaves the compiler's SBCL-comma decoding
;;;; path inert, exactly as under CCL.

#+abcl (require :gray-streams)

;;; ---- Form 1: create the SBCL-internal packages -------------------------
#+abcl
(progn
  (defpackage :sb-ext (:use)
    (:export #:posix-getenv #:run-program #:*posix-argv* #:exit))
  (defpackage :sb-int (:use) (:export #:quasiquote))
  (defpackage :sb-impl (:use)
    (:export #:comma #:comma-p #:comma-expr #:comma-kind))
  (defpackage :sb-kernel (:use)
    (:export #:double-float-high-bits #:double-float-low-bits))
  (defpackage :sb-gray (:use)
    (:import-from :gray-streams
                  #:fundamental-character-output-stream
                  #:stream-write-char #:stream-write-string
                  #:stream-line-column #:stream-force-output
                  #:stream-finish-output)
    (:export #:fundamental-character-output-stream
             #:stream-write-char #:stream-write-string
             #:stream-line-column #:stream-force-output
             #:stream-finish-output))
  (defpackage :sb-debug (:use) (:export #:print-backtrace #:list-backtrace)))

;; NOTE — check-source-parses under ABCL.  Two baked-as-source files
;; (net/asdf-interface.lisp, net/genera-compat.lisp) reference symbols in
;; in-image-only packages (UIOP, SCL, ASDF, QL-DIST, PROCESS, SI, …).  Those
;; files are never host-executed — they are baked as raw SOURCE strings for
;; Modus's own in-image reader, which has those packages.  SBCL and CCL happen
;; to READ the files anyway (their readers tolerate a missing package in a
;; token); ABCL's reader is stricter and errors, so the host-side sweep reports
;; false positives.  Run the ABCL build with MODUS_GLOBAL_CHECK=warn (the knob
;; the check itself documents) — the image is byte-for-byte unaffected either
;; way.  build-abcl.sh sets it.

;;; ---- Form 2: define the shimmed operators ------------------------------
#+abcl
(progn

  ;; --- SB-EXT: process/environment ---------------------------------------
  (defun sb-ext:posix-getenv (name) (ext:getenv name))

  (defun sb-ext:exit (&key (code 0) &allow-other-keys) (ext:quit :status code))

  ;; Only the #+sbcl-guarded chmod uses run-program, so a best-effort stub
  ;; via the shell is enough.
  (defun sb-ext:run-program (program args &key wait &allow-other-keys)
    (declare (ignore wait))
    (ext:run-shell-command
     (format nil "~A~{ ~A~}" program args)))

  (defparameter sb-ext:*posix-argv*
    (cons "modus" (copy-list ext:*command-line-argument-list*)))

  ;; --- SB-INT / SB-IMPL: backquote representation (inert under ABCL) ------
  (defstruct (sb-impl::comma (:predicate sb-impl::comma-p)) expr kind)

  ;; --- SB-KERNEL: raw IEEE-754 double bits (via java.lang.Double) ---------
  (defun %df-bits (f)
    (logand (java:jstatic "doubleToRawLongBits" "java.lang.Double" (float f 1.0d0))
            #xFFFFFFFFFFFFFFFF))
  (defun sb-kernel:double-float-high-bits (f)
    ;; SBCL returns a SIGNED (signed-byte 32); sign-convert to match.
    (let ((hi (ldb (byte 32 32) (%df-bits f))))
      (if (>= hi #x80000000) (- hi #x100000000) hi)))
  (defun sb-kernel:double-float-low-bits (f)
    (ldb (byte 32 0) (%df-bits f)))

  ;; --- SB-DEBUG: backtraces (harness diagnostics) ------------------------
  (defun sb-debug:print-backtrace (&rest args) (declare (ignore args)) nil)
  (defun sb-debug:list-backtrace (&rest args) (declare (ignore args)) nil)

  ;; --- BACKQUOTE: rename ABCL's BACKQ-* operators to standard CL ----------
  ;; Wrap the `#\`` reader macro; substitute the operator symbols in whatever
  ;; ABCL's own (correct) backquote reader produces.  SUBLIS descends every
  ;; car/cdr — including quoted nested-backquote data — so the rename is
  ;; consistent at every level.  BACKQ-VECTOR is deliberately NOT mapped:
  ;; backquoted-vector semantics do not rename 1:1, and no first-party source
  ;; uses them, so an appearance should flag loudly (unresolved BACKQ-VECTOR)
  ;; rather than silently miscompile.  BACKQ-COMMA* never survive to the final
  ;; expansion.
  (defparameter *backq-rename*
    (loop for (name . std) in '(("BACKQ-LIST"   . list)  ("BACKQ-LIST*" . list*)
                                ("BACKQ-APPEND" . append)("BACKQ-CONS"  . cons)
                                ("BACKQ-NCONC"  . nconc))
          for sym = (find-symbol name :system)
          when sym collect (cons sym std)))

  (let ((orig (get-macro-character #\`)))
    (set-macro-character
     #\`
     (lambda (stream char)
       (sublis *backq-rename* (funcall orig stream char)))))

  (format t "~&[abcl-host-compat] SBCL compatibility shim installed under ~A ~A.~%"
          (lisp-implementation-type) (lisp-implementation-version)))
