;;;; static-symbols.lisp -- quoted symbols in image code are cached in the
;;;; literal vector (docs/static-literals.md phase 2) and must stay EQ to the
;;;; runtime's symbols across real collections.
;;;;
;;;; Run: ./modus --script test/static-symbols.lisp
;;;;
;;;; TYPE-OF and CLASS-NAME return symbols quoted in IMAGE code, so each
;;;; result comes out of the vector root at #x10000FB0.  No boxed float
;;;; literal here: on aarch64 a runtime defun's 1.5d0 reads back as a string
;;;; after collections, with or without this cache (a separate bug).  Collections are
;;;; forced by allocating (not %GC-COLLECT-HERE at toplevel, which proved
;;;; nothing); %GC-EPOCH confirms they happened.  A collector missing the
;;;; root leaves the vector's slots pointing at from-space: the symbols stop
;;;; being EQ, or the process faults.

(defvar *ss-fail* 0)
(defun ss (name got want)
  (unless (eql got want)
    (setq *ss-fail* (+ *ss-fail* 1))
    (format t "~&FAIL ~A: got ~S want ~S~%" name got want)))

(defun ss-same (round name sym)
  ;; Identity, not correctness: the symbol an image function hands back must
  ;; be the one the package system hands back for the same name.
  (ss (list round name) (and (symbolp sym) sym (symbol-package sym)
                             (find-symbol (symbol-name sym) (symbol-package sym)))
      sym))

(defun ss-check (round)
  (ss-same round 'type-of-char (type-of #\a))
  (ss-same round 'type-of-cons (type-of (cons 1 2)))
  (ss-same round 'type-of-symbol (type-of 'foo))
  (ss-same round 'class-name (class-name (class-of 5)))
  (ss (list round 'subtypep) (subtypep 'fixnum 'integer) t)
  (ss (list round 'typep) (typep 5 'integer) t)
  ;; The symbols INSIDE a quoted list in image code (%TYPE-DIRECT-SUPERS'
  ;; '(symbol list boolean)) come from the cache too.  This was NIL when the
  ;; cache kept an early-boot name-less CL:LIST that cl-packages later
  ;; replaced (named-readtables' DEFREADTABLE failed on it).
  (ss (list round 'subtypep-null-list) (subtypep 'null 'list) t)
  (ss (list round 'subtypep-cons-list) (subtypep 'cons 'list) t)
  ;; A cached quoted symbol is ONE value (a leaked MV-COUNT from the fill
  ;; made ANSI's (multiple-value-list (defsetf ...)) return (NAME NIL)).
  (ss (list round 'one-value) (length (multiple-value-list (type-of #\a))) 1)
  (ss (list round 'stale-slots) (ss-stale-slots) nil))

(defun ss-stale-slots ()
  "Every filled slot of the vector must be the symbol FIND-SYMBOL returns
   now.  CL-USER is skipped: build-host packages without a runtime
   counterpart (MODUS.MVM, ...) are filed there by %INTERN-SYMBOL-PKG and
   FIND-SYMBOL cannot see them, cached or not."
  (let ((v (mem-ref #x10000FB0 :u64)) (bad nil))
    (unless (eql v 0)
      (dotimes (i (length v))
        (let* ((s (svref v i)) (p (and (not (eql s 0)) (symbol-package s))))
          (when (and p (not (string= (package-name p) "COMMON-LISP-USER")))
            (multiple-value-bind (f st) (find-symbol (symbol-name s) p)
              (unless (and st (eq f s))
                (push (list i (symbol-name s) (package-name p)) bad)))))))
    bad))

(defun ss-churn ()
  (let ((keep nil))
    (dotimes (i 400000) (push (make-list 4) keep) (when (> (length keep) 1000) (setq keep nil)))
    keep))

(ss-check 0)
(let ((e0 (%gc-epoch)))
  (dotimes (r 5) (ss-churn) (ss-check (+ r 1)))
  (format t "~&static-symbols: collections ~D~%" (- (%gc-epoch) e0))
  (ss 'collected (> (%gc-epoch) e0) t))
(format t "~&static-symbols: ~D failure(s)~%" *ss-fail*)
(when (> *ss-fail* 0) (error "static-symbols: ~D failure(s)" *ss-fail*))
