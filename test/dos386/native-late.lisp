;;;; native-late.lisp -- the i386 native tier's late-bound calls (mvm/jit-i386-native.lisp).
;;;;
;;;; Run on an image built with MODUS_I386_JIT=1:
;;;;   qemu-i386-static IMAGE --non-interactive --load test/dos386/native-late.lisp
;;;; Each line prints its answer and the one wanted: a redefinition seen by a
;;;; native caller, an undefined callee signalling and then defined, a generic
;;;; function, a closure callee, a tail call and multiple values through a cell.

(defun native-p (f) (let ((w (modus-int::%val->word f))) (and (= (logand w 15) 3) (>= w #x12000000) (< w #x12800000))))
(defun g (x) (+ x 1))
(defun f (x) (* 10 (g x)))
(format t "~&f native ~S g native ~S (f 4) = ~S (want 50)~%" (native-p #'f) (native-p #'g) (f 4))
(defun g (x) (+ x 2))
(format t "after redefining g: (f 4) = ~S (want 60)~%" (f 4))
(defun h (x) (no-such-function-yet x))
(format t "undefined: ~S~%" (handler-case (h 1) (undefined-function (c) (list :undefined-function-signalled)) (error (c) (list :other (type-of c)))))
(defun no-such-function-yet (x) (* x 100))
(format t "defined later: (h 3) = ~S (want 300)~%" (h 3))
(defgeneric area (s))
(defclass sq () ((s :initarg :s :reader side)))
(defmethod area ((x sq)) (* (side x) (side x)))
(defun total (n) (let ((a (make-instance 'sq :s 3)) (s 0)) (dotimes (i n s) (incf s (area a)))))
(format t "clos: total native ~S (total 1000) = ~S (want 9000)~%" (native-p #'total) (total 1000))
(let ((k 5)) (defun adder (x) (+ x k)))
(defun use-adder (x) (adder x))
(format t "closure callee: (use-adder 1) = ~S (want 6), use-adder native ~S~%" (use-adder 1) (native-p #'use-adder))
(defun tc (x) (g x))
(format t "tail call through cell: (tc 1) = ~S (want 3)~%" (tc 1))
(defun mv () (values 1 2 3))
(defun use-mv () (multiple-value-list (mv)))
(format t "values across a late call: ~S (want (1 2 3))~%" (use-mv))
(format t "translated ~D pending ~D~%" modus-int::*n386-translated* (length modus-int::*n386-pending*))
