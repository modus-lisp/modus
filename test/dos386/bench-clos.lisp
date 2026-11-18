;;;; bench-clos.lisp -- the CLOS workload of test/dos386/bench.lisp, loaded at
;;;; run time (see bench-defs.lisp for why it is not baked).  Uses RUN and
;;;; *SCALE* from bench-defs.lisp.

;; CLOS dispatch
(defclass shape () ())
(defclass sq (shape) ((s :initarg :s :reader side)))
(defclass circ (shape) ((r :initarg :r :reader rad)))
(defgeneric area (x))
(defmethod area ((x sq)) (* (side x) (side x)))
(defmethod area ((x circ)) (* 3 (rad x) (rad x)))
(defun clos-work (n)
  (let ((a (make-instance 'sq :s 3)) (b (make-instance 'circ :r 2)) (s 0))
    (dotimes (i n s) (incf s (+ (area a) (area b))))))

(run "clos 100k" 1 (lambda () (clos-work 100000)) 2100000)
