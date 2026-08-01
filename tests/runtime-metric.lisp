;;;; runtime-metric.lisp — does loaded code actually RUN?
;;;;
;;;; WS5 #203.  "alexandria loads 22/22 files clean" was recorded for weeks as
;;;; the marker that a build was nearly drop-in.  It is a DEFINE-time metric:
;;;; loading executes defun/defmacro/defparameter and barely exercises calls
;;;; BETWEEN them.  It stayed green on x64 while `(k3 21)` — where k3 calls k2,
;;;; both defined by earlier top-level forms — produced nothing at all.  That is
;;;; the interior of every library.
;;;;
;;;; Every check below therefore DEFINES something in one top-level form and
;;;; USES it from a LATER one, which is the only shape that matters and the one
;;;; "loads clean" cannot see.  Portable CL: runs unmodified on SBCL, which is
;;;; the reference — diff this file's output, don't eyeball it.
;;;;
;;;;   sbcl --noinform --no-sysinit --no-userinit --load tests/runtime-metric.lisp --quit
;;;;   ./modus --load tests/runtime-metric.lisp --quit
;;;;   qemu-aarch64-static ./modus-aa64 --load tests/runtime-metric.lisp --quit
;;;;
;;;; Each line is `NAME=VALUE'.  A conforming run prints RT-OK at the end and
;;;; every value matches SBCL's.  Guard each use with handler-case so ONE gap
;;;; reports as :ERR instead of aborting the file and hiding the rest.

;; --- definitions (each its own top-level form) ---------------------------
(defun rt-inc (x) (+ x 1))
(defun rt-double (x) (* x 2))
(defun rt-nested (x) (rt-double (rt-inc x)))          ; fn calling fn, both earlier
(defmacro rt-plus1 (x) `(+ ,x 1))                     ; runtime macro + backquote
(defmacro rt-splice (&rest xs) `(+ ,@xs))             ; ,@ splicing
(defparameter *rt-fn* (lambda (x) (* x 3)))           ; closure in a global
(defparameter *rt-cap* (let ((n 40)) (lambda (x) (+ n x))))  ; capturing closure
(defparameter *rt-tbl* (list (lambda () 7)))          ; closure inside a structure
(setf (symbol-function 'rt-sf) (lambda (x) (- x 1)))  ; closure as a function cell
(defvar *rt-counter* 0)
(defun rt-bump () (setq *rt-counter* (+ *rt-counter* 1)) *rt-counter*)

;; --- uses (every one a LATER top-level form) -----------------------------
(defmacro rt-check (name form)
  `(progn (princ ,name) (princ "=")
          (princ (handler-case ,form (error (c) :ERR) (condition (c) :ERR)))
          (terpri)))

(rt-check "call-by-name"    (rt-inc 41))
(rt-check "nested-call"     (rt-nested 20))
(rt-check "macro-use"       (rt-plus1 41))
(rt-check "macro-splice"    (rt-splice 1 2 3 4))
(rt-check "stored-closure"  (funcall *rt-fn* 14))
(rt-check "capturing"       (funcall *rt-cap* 2))
(rt-check "closure-in-list" (funcall (car *rt-tbl*)))
(rt-check "symbol-function" (rt-sf 43))
(rt-check "sharp-quote"     (funcall #'rt-inc 41))
(rt-check "apply"           (apply *rt-fn* (list 14)))
(rt-check "mapcar"          (mapcar #'rt-double (list 1 2 3)))
(rt-check "mapcar-stored"   (mapcar *rt-fn* (list 1 2 3)))
(rt-check "special-mutate"  (progn (rt-bump) (rt-bump) *rt-counter*))
(rt-check "higher-order"    (funcall (lambda (f) (funcall f 20)) #'rt-double))
(rt-check "recursion"       (labels ((f (n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2)))))) (f 10)))
;; --- re-execution check ------------------------------------------------
;; A form can produce the CORRECT value while executing TWICE, which no
;; value-only check can see.  x64 does exactly this for a call by name to a
;; runtime-defined function: the call lands at the module thunk entry instead
;; of the callee, so the form re-enters FROM THE TOP, and only then resolves
;; and completes.  The signature is asymmetric — a side effect placed BEFORE
;; the call runs twice, one placed AFTER it runs once:
;;     pre=2 post=1        (x64)        pre=1 post=1   (SBCL, aarch64)
;; Count with real counters, never by eyeballing duplicated output: the
;; duplicated text is easy to mistake for a printing artifact, and a filter
;; like `grep -v' can delete it along with the value on the same line.
(defvar *rt-pre* 0)
(defvar *rt-post* 0)
(defun rt-touch (x) (+ x 1))
(progn (setq *rt-pre* (+ *rt-pre* 1))
       (rt-touch 1)
       (setq *rt-post* (+ *rt-post* 1)))
;; Value is 1/0 rather than a keyword ON PURPOSE: this file is meant to be
;; DIFFED against SBCL, and princ of a keyword still prints the leading colon
;; on Modus (:YES vs SBCL's YES), which would make the row diverge textually
;; forever for a reason that has nothing to do with what it measures.
(rt-check "form-ran-once" (if (and (= *rt-pre* 1) (= *rt-post* 1))
                              1
                              (list 0 *rt-pre* *rt-post*)))
(princ "RT-OK") (terpri)
