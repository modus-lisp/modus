;;;; hosted-bringup-bare.lisp — %SB-THREADS-UP ARMS THE SEAM, AND THE PROBE THAT
;;;; SAID OTHERWISE WAS READING THE WORDS WRONG.
;;;;
;;;;   ./modus --script test/hosted-bringup-bare.lisp
;;;;
;;;; ================= WHAT THIS FILE USED TO CLAIM =================
;;;;
;;;; It printed, 4 of 4 and later 3 of 3 on a second build:
;;;;
;;;;   PRE  gate=0 mode=0 flag=NIL
;;;;   POST ret=T gate=0 mode=0 flag=T
;;;;
;;;; and concluded that %SB-THREADS-UP latched success while leaving the
;;;; threads-live gate (#x10000DB8) and the per-CPU mode word (#x10000FF8) at
;;;; zero — i.e. that the runtime-table lock was inert and B-lite's arena never
;;;; carved in every process that did not happen to have the "right" script
;;;; shape.  A whole round was spent on "what decides the shape".
;;;;
;;;; ================= IT IS A READ ARTIFACT.  MEASURED. =================
;;;;
;;;; THE WORDS WERE ARMED THE WHOLE TIME.  Add ONE more toplevel form to the old
;;;; script — reading the same two addresses, three lines later, in the same
;;;; process, with nothing in between — and it reports gate=1 mode=1.
;;;;
;;;; THE TRIGGER, isolated one difference at a time, deterministic:
;;;;
;;;;   an inline (MEM-REF <literal address> :U32), in a TOPLEVEL FORM that also
;;;;   contains a call to a function defined by net/sb-thread-shim.lisp,
;;;;   reads the value that address held BEFORE the form began.
;;;;
;;;; It is not about arming and not about %SB-THREADS-UP.  A form whose only
;;;; shim call is `(sb-thread:threadp 5)' — which arms nothing and does nothing
;;;; — misreads an ALREADY-armed gate the same way, in the same argument list
;;;; in which (%RT-THREADS-LIVE-P) correctly answers 1.  The same expression
;;;; inside an ordinary compiled DEFUN is honest; so is the same expression in a
;;;; toplevel form with no shim call in it.  That is why the "shape dependence"
;;;; looked inexplicable: the two shapes differed in whether the READ was in the
;;;; same form as the CALL, not in what the call did.
;;;;
;;;; ***SO: NEVER GRADE THIS SEAM WITH AN INLINE MEM-REF OF A LITERAL ADDRESS.***
;;;; Use %RT-THREADS-LIVE-P and %HA-PERCPU-MODE, which are ordinary compiled
;;;; functions in the image, or better, use the FUNCTIONAL oracle below.
;;;;
;;;; ================= THE ORACLE THAT CANNOT LIE =================
;;;;
;;;; Reading a word to ask "is the lock on" is one indirection too many when the
;;;; read itself is the thing in doubt.  %RT-THREADS-ON zeroes the acquisition
;;;; counter at #x10000DE0 and every locked section increments it, so:
;;;;
;;;;   SYMBOL-VALUE in a loop BEFORE bringup  -> the counter does not move
;;;;   the same loop AFTER bringup            -> the counter moves by thousands
;;;;
;;;; That is the runtime-table lock actually engaging, observed without reading
;;;; the gate at all.  It is the check this file now makes.
;;;;
;;;; No threads, no sockets, no glass, nothing listening.

(defvar *bb-fail* 0)
(defvar *bb-checks* 0)
(defvar *bb-g* 5)

(defun bb-check (name got want)
  (setq *bb-checks* (+ *bb-checks* 1))
  (if (equal got want)
      (format t "~&  ok   ~a = ~s~%" name got)
      (progn (setq *bb-fail* (+ *bb-fail* 1))
             (format t "~&  FAIL ~a = ~s (wanted ~s)~%" name got want))))

(defun bb-check-true (name got)
  (setq *bb-checks* (+ *bb-checks* 1))
  (if got
      (format t "~&  ok   ~a = ~s~%" name got)
      (progn (setq *bb-fail* (+ *bb-fail* 1))
             (format t "~&  FAIL ~a = ~s (wanted non-nil)~%" name got))))

;;; 200 SYMBOL-VALUEs.  Compiled, in a DEFUN, so the loop itself is not the
;;; thing under measurement.
(defun bb-spin ()
  (let ((n 0) (i 0))
    (loop
      (when (>= i 200) (return n))
      (setq n (+ n (symbol-value '*bb-g*)))
      (setq i (+ i 1)))))

(format t "~&=== hosted-bringup-bare ===~%")

;;; ---- BEFORE BRINGUP: the lock must be INERT ---------------------------------
;;; This is also the positive control for the oracle: if the counter moved here,
;;; something armed the seam before we did and every number below is meaningless.
(defvar *bb-acq0* (%rt-acquisitions))
(defvar *bb-spin0* (bb-spin))
(defvar *bb-acq1* (%rt-acquisitions))

(format t "~&-- before bringup --~%")
(bb-check "spin result" *bb-spin0* 1000)
(bb-check "acquisitions before any bringup" *bb-acq0* 0)
(bb-check "acquisitions unmoved by 200 SYMBOL-VALUEs" *bb-acq1* 0)
(bb-check "gate reads 0 (%RT-THREADS-LIVE-P)" (%rt-threads-live-p) 0)
(bb-check "mode reads 0 (%HA-PERCPU-MODE)" (%ha-percpu-mode) 0)
(bb-check "flag not latched" *sb-threads-up* nil)
(finish-output)

;;; ---- BRINGUP ----------------------------------------------------------------
(defvar *bb-ret* (%sb-threads-up))
(defvar *bb-acq2* (%rt-acquisitions))
(defvar *bb-spin1* (bb-spin))
(defvar *bb-acq3* (%rt-acquisitions))

(format t "~&-- after bringup --~%")
(bb-check "%SB-THREADS-UP returned" *bb-ret* t)
(bb-check "flag latched" *sb-threads-up* t)

;;; The words, read HONESTLY — through the compiled accessors.
(bb-check "gate armed (%RT-THREADS-LIVE-P)" (%rt-threads-live-p) 1)
(bb-check "mode armed (%HA-PERCPU-MODE)" (%ha-percpu-mode) 1)

;;; The FUNCTIONAL oracle: the lock is not merely flagged on, it is engaging.
(bb-check-true "acquisitions non-zero right after bringup" (> *bb-acq2* 0))
(bb-check "spin result still correct" *bb-spin1* 1000)
(bb-check-true "200 more SYMBOL-VALUEs moved the counter"
               (> *bb-acq3* *bb-acq2*))
(finish-output)

;;; ---- THE ARTIFACT ITSELF, AS A REGRESSION -----------------------------------
;;;
;;; Both reads below happen with the gate ARMED, at the same instant, in one
;;; argument list.  The function read is correct.  The inline read is the bug.
;;; This section asserts the CORRECT one and merely REPORTS the inline one, so
;;; that the day the underlying evaluator defect is fixed this test does not
;;; start failing for a good reason — but the line still prints, so the
;;; regression is visible either way.
(format t "~&-- the inline-MEM-REF read artifact (reported, not asserted) --~%")
(let ((r (sb-thread:threadp 5)))
  (format t "~&  shim-form: threadp=~s fn-gate=~s fn-mode=~s inline-gate=~s inline-mode=~s~%"
          r (%rt-threads-live-p) (%ha-percpu-mode)
          (mem-ref #x10000DB8 :u32) (mem-ref #x10000FF8 :u32)))
(format t "~&  (a form with NO shim call in it reads inline gate=~s mode=~s)~%"
        (mem-ref #x10000DB8 :u32) (mem-ref #x10000FF8 :u32))
(finish-output)

;;; The honest accessors must still agree from a form that DOES contain a shim
;;; call — that is the property the shim's own bringup guard depends on.
(let ((r (sb-thread:threadp 5)))
  (bb-check "threadp of a non-thread" r nil)
  (bb-check "fn-gate inside a shim-calling form" (%rt-threads-live-p) 1)
  (bb-check "fn-mode inside a shim-calling form" (%ha-percpu-mode) 1))

(format t "~&=== hosted-bringup-bare: ~d checks, ~d failed ===~%"
        *bb-checks* *bb-fail*)
(finish-output)
(sys-exit (if (= *bb-fail* 0) 0 1))
