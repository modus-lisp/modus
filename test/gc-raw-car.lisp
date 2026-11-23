;;;; test/gc-raw-car.lisp -- a cons whose CAR is an odd RAW word ending in #xF
;;;; must survive collections.
;;;;
;;;; The collectors mark a moved object by overwriting its first word with
;;;; (new-address | #xF), and decided "already moved" from that low nibble
;;;; alone.  A cons's car is a value slot, and the interpreter keeps RAW machine
;;;; words as values (interp.lisp REG-SET; its operand stack is a Lisp list), so
;;;; a raw odd word can be a car.  The old test took it for a forward, returned
;;;; (word & ~15) | 1 -- an address in neither semispace -- and never copied the
;;;; cons; its memory was reused and the list read garbage.  Measured on the
;;;; 32-bit CLIs (i386, arm32, 68k), 2026-11-23: the MVM operand stack held
;;;; (#x19FFFFFF . NIL), and the ANSI sweep's intermittent `N is not of type
;;;; LIST' / memory fault was this.
;;;;   modus --script test/gc-raw-car.lisp      prints GC-RAW-CAR PASS / FAIL
;;;; Run it with a small heap so it collects (--dynamic-space-size 12MB on the
;;;; 32-bit CLIs); %GC-EPOCH is the positive control.
(defparameter *rc-list*
  (eval '(list (%word->val #x0BFFFFFF) 1 2 3)))     ; raw car, nibble F, off-heap
(defparameter *rc-sink* nil)
(defun rc-churn ()
  (dotimes (k (floor (* 6 (modus-int::%core-space-size)) (* 5 144)))
    (setq *rc-sink* (make-array 16))))
(defun rc-held-on-stack ()
  ;; the same shape held only by this frame across the collections
  (let ((l (eval '(list (%word->val #x0BFFFFFF) :a :b))))
    (dotimes (i 3) (rc-churn) (modus-int::%gc-force))
    (and (consp l) (consp (cdr l)) (eq (cadr l) :a) (eq (caddr l) :b) (null (cdddr l)))))
(let* ((e0 (modus-int::%gc-epoch))
       (stack-ok (rc-held-on-stack))
       (ok (progn (dotimes (i 3) (rc-churn) (modus-int::%gc-force))
                  (and stack-ok
                       (consp *rc-list*)
                       (equal (cdr *rc-list*) '(1 2 3))))))
  (format t "~&GC-RAW-CAR ~:[FAIL~;PASS~] (collections ~a)~%"
          (and ok (> (modus-int::%gc-epoch) e0)) (- (modus-int::%gc-epoch) e0)))
