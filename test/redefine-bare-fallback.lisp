;;;; test/redefine-bare-fallback.lisp -- a JIT bridge call that resolves its
;;;; callee through the BARE-name fallback still sees every redefinition.
;;;;
;;;;   ./modus --script test/redefine-bare-fallback.lisp     -> BAREFB PASS / FAIL
;;;;
;;;; Native code calls MODUS-INT::BBF-INNER under the folded key
;;;; "MODUS-INT::BBF-INNER"; a MODUS-INT DEFUN is keyed bare ("BBF-INNER"), so
;;;; the bridge falls back to the bare entry (%MVM-RUNTIME-FALLBACK-NAME).  That
;;;; native caller used to link its stub / cell under the QUALIFIED name while
;;;; a redefinition retargets the BARE one, so on x64 every redefinition below
;;;; was invisible (3f6dd2c: 6 FAIL).  Callers now link under the key that
;;;; holds the function (%MVM-RUNTIME-KEY).  A callee not defined yet when its
;;;; caller links goes through a bridge thunk, whose bare fallback (SUBSEQ +
;;;; FIND-SYMBOL + two EQUAL hashes per call) is now cached per thunk under
;;;; *JIT-BRIDGE-GEN*, which every DEFUN / (SETF FDEFINITION) / FMAKUNBOUND
;;;; bumps; the "forward" checks are that path.

(defvar *bbf-fail* 0)
(defun bbf-check (what got want)
  (format t "~&  ~a ~a: ~s (want ~s)~%" (if (equal got want) "ok  " "FAIL") what got want)
  (unless (equal got want) (incf *bbf-fail*)))

(defun modus-int::bbf-inner (x) (+ x 1))
(defun bbf-outer (x) (modus-int::bbf-inner x))
;; A NON-NATIVE callee (a closure: every runtime DEFCLASS accessor and generic
;; function is one) is reached through a late-binding thunk on every call.
(setf (fdefinition 'modus-int::bbf-clo) (let ((k 3)) (lambda (x) (+ x k))))
(defun bbf-via-clo (x) (modus-int::bbf-clo x))
(defun bbf-clo-loop (n) (let ((s 0)) (dotimes (i n s) (setq s (+ s (bbf-via-clo 1))))))
;; A FORWARD reference: native before its callee exists, so the call goes
;; through a bridge thunk under the qualified name, and the bridge falls back.
(defun bbf-fwd (x) (modus-int::bbf-later x))
(defun bbf-loop (n) (let ((s 0)) (dotimes (i n s) (setq s (+ s (bbf-fwd 1))))))
(ignore-errors (jit-eager))

(bbf-check "native outer, original inner" (bbf-outer 1) 2)
(bbf-check "again (cached path)" (bbf-outer 1) 2)
(defun modus-int::bbf-inner (x) (* x 9))
(bbf-check "after a DEFUN of the bare name" (bbf-outer 1) 9)
(let ((orig (fdefinition 'modus-int::bbf-inner)))
  (setf (fdefinition 'modus-int::bbf-inner) (lambda (x) (* x 100)))
  (bbf-check "after (setf fdefinition) of a closure" (bbf-outer 1) 100)
  (setf (fdefinition 'modus-int::bbf-inner) orig)
  (bbf-check "after restoring the original" (bbf-outer 1) 9))
(setf (symbol-function 'modus-int::bbf-inner) (lambda (x) (* x 7)))
(bbf-check "after (setf symbol-function)" (bbf-outer 1) 7)
(fmakunbound 'modus-int::bbf-inner)
(bbf-check "after FMAKUNBOUND" (handler-case (bbf-outer 1) (undefined-function () :undefined)) :undefined)
(defun modus-int::bbf-inner (x) (- x 5))
(bbf-check "defined again" (bbf-outer 1) -4)

(bbf-check "closure callee" (bbf-via-clo 1) 4)
(bbf-check "closure callee again" (bbf-via-clo 1) 4)
(setf (fdefinition 'modus-int::bbf-clo) (lambda (x) (* x 11)))
(bbf-check "closure callee replaced" (bbf-via-clo 2) 22)

;; The forward-referenced callee, defined after its caller went native.
(bbf-check "forward: undefined" (handler-case (bbf-fwd 1) (undefined-function () :undefined)) :undefined)
(defun modus-int::bbf-later (x) (+ x 10))
(bbf-check "forward: defined" (bbf-fwd 1) 11)
(bbf-check "forward: again (cached)" (bbf-fwd 1) 11)
(defun modus-int::bbf-later (x) (+ x 20))
(bbf-check "forward: redefined" (bbf-fwd 1) 21)
(setf (fdefinition 'modus-int::bbf-later) (lambda (x) (* x 3)))
(bbf-check "forward: (setf fdefinition)" (bbf-fwd 2) 6)
(fmakunbound 'modus-int::bbf-later)
(bbf-check "forward: FMAKUNBOUND" (handler-case (bbf-fwd 1) (undefined-function () :undefined)) :undefined)
(defun modus-int::bbf-later (x) (- x 1))
(bbf-check "forward: defined again" (bbf-fwd 1) 0)

;; Rough cost of one fallback-resolved call, for the record.
(let ((t0 (get-internal-real-time)))
  (bbf-loop 200000)
  (format t "~&  200000 forward-referenced calls: ~d ms~%"
          (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second)))
(let ((t0 (get-internal-real-time)))
  (bbf-clo-loop 200000)
  (format t "~&  200000 calls to a closure callee: ~d ms~%"
          (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second)))
(format t "~&BAREFB ~a~%" (if (zerop *bbf-fail*) "PASS" "FAIL"))
