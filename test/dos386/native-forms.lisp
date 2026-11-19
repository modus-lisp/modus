;;;; native-forms.lisp -- top-level forms that build closures, run natively by the
;;;; i386 native tier's form seam (mvm/jit-i386-native.lisp %RUNTIME-FORM-NATIVE).
;;;;
;;;;   qemu-i386-static IMAGE --non-interactive --load test/dos386/native-forms.lisp
;;;; on an image built with MODUS_I386_JIT=1.  Each line prints its answer and the
;;;; one wanted: methods (with :AROUND), a closure and a hook made by a form,
;;;; multiple values and an error out of a form's lambda; then the census.

(defgeneric area (s))
(defclass sq () ((s :initarg :s :reader side)))
(defclass circ () ((r :initarg :r :reader rad)))
(defmethod area ((x sq)) (* (side x) (side x)))
(defmethod area ((x circ)) (* 3 (rad x) (rad x)))
(defmethod area :around ((x sq)) (+ 1000 (call-next-method)))
(format t "~&methods: ~S ~S (want 1009 12)~%" (area (make-instance 'sq :s 3)) (area (make-instance 'circ :r 2)))
(defparameter *k* 7)
(defparameter *adder* (let ((n 5)) (lambda (x) (+ x n *k*))))
(format t "closure from a form: ~S (want 22)~%" (funcall *adder* 10))
(defvar *hooks* nil)
(push (lambda (x) (* x 2)) *hooks*)
(format t "hook: ~S (want 42)~%" (funcall (first *hooks*) 21))
(format t "mapcar lambda form: ~S~%" (mapcar (lambda (x) (* x x)) '(1 2 3)))
(format t "values form: ~S~%" (multiple-value-list (let ((f (lambda () (values 1 2 3)))) (funcall f))))
(format t "error in form: ~S~%" (handler-case (funcall (lambda () (error "boom ~D" 1))) (error (c) (princ-to-string c))))
(format t "forms native ~D, refused ~S~%" modus-int::*n386-forms-translated* (remove-if-not (lambda (r) (eq (car r) :form)) modus-int::*n386-refused*))
