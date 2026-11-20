;;;; test/interp-closure-slot-escape.lisp -- a closure the INTERPRETER makes and
;;;; stores in a structure slot must be callable from native code.
;;;;
;;;;   MODUS_NO_JIT=1 ./modus --script test/interp-closure-slot-escape.lisp
;;;;
;;;; The interpreter's closures carry a bytecode offset, not code; it hands
;;;; native code a trampoline when one escapes -- as an argument to a native
;;;; function, a return value, a global.  An inlined structure writer stores
;;;; with a plain slot store, which did not wrap, so the slot held the raw
;;;; interpreter closure and a native FUNCALL of it jumped through a bytecode
;;;; offset.  Found as vessel's desk: (setf (desk-keyboard-fn desk) (lambda ...))
;;;; from interpreted code, then glass's native %FOCUS calling it -- opening
;;;; Notes faulted the Pi Zero.  Interpreted-only ports (i386, arm32, RISC-V)
;;;; run every runtime form this way.
;;;;
;;;; MAPCAR, FUNCALL from a COMPILE'd function, and SORT's key are all native.

(defvar *fails* 0)
(defvar *checks* 0)
(defun check (name ok &optional got)
  (incf *checks*)
  (unless ok (incf *fails*))
  (format t "~:[FAIL~;ok  ~] ~a~@[ ~s~]~%" ok name (unless ok got)))

(defstruct holder fn (other 0))

(defvar *h* (make-holder))
(defvar *k* 10)

;; Toplevel forms: interpreted under MODUS_NO_JIT=1.
(let ((k 7)) (setf (holder-fn *h*) (lambda (x) (+ x k))))

(check "interpreted call of the slot" (= (funcall (holder-fn *h*) 1) 8)
       (funcall (holder-fn *h*) 1))
(check "native MAPCAR of the slot" (equal (mapcar (holder-fn *h*) '(1 2 3)) '(8 9 10))
       (mapcar (holder-fn *h*) '(1 2 3)))
(check "funcall from compiled code"
       (= (funcall (compile nil '(lambda (h) (funcall (holder-fn h) 5))) *h*) 12))
(check "slot is EQ-stable across reads" (eq (holder-fn *h*) (holder-fn *h*)))

;; A captureless lambda, and a closure over a special, the same way.
(setf (holder-fn *h*) (lambda (x) (* x 3)))
(check "captureless lambda via MAPCAR" (equal (mapcar (holder-fn *h*) '(1 2)) '(3 6)))
(setf (holder-fn *h*) (lambda (x) (+ x *k*)))
(check "special-reading lambda via MAPCAR" (equal (mapcar (holder-fn *h*) '(1)) '(11)))

;; Ordinary data still goes in unchanged.
(setf (holder-other *h*) '(1 2))
(check "a list stays a list" (equal (holder-other *h*) '(1 2)))
(setf (holder-other *h*) #'car)
(check "#'CAR stays callable" (= (funcall (holder-other *h*) '(4 5)) 4))

(format t "~&interp-closure-slot-escape: ~d checks, ~d failed~%" *checks* *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
