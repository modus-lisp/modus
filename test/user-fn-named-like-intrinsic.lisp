;;; A user package's own function named like a compiler intrinsic is that
;;; package's function (compiler.lisp compile-compound, %USER-OWN-FUNCTION-P).
;;; The shape that broke a Zero card: vessel's KILN-ZERO-BOOT::%KERNEL, called
;;; as (funcall (%kernel "X") ...), compiled as modus's %KERNEL instruction, so
;;; the DEFUNs holding it never existed.  Run: ./modus --script THIS (also SBCL).
(defpackage :intrinsic-name-test (:use :cl))
(in-package :intrinsic-name-test)
(defvar *fails* 0)
(defvar *checks* 0)
(defmacro check (name form expect)
  `(let ((got (handler-case ,form (error (c) (list :error (princ-to-string c))))))
     (incf *checks*)
     (unless (equal got ,expect)
       (incf *fails*)
       (format t "FAIL ~a: got ~s want ~s~%" ,name got ,expect))))

(defun %kernel (name) (lambda (&rest args) (list* name args)))
(defun %bulk-move (a b) (+ a b))
(defun mem-ref (x) (* x 10))
(defun use-kernel (n) (funcall (%kernel "GETENV") n))
(defun use-bulk () (%bulk-move 2 3))
(defun use-mem-ref () (mem-ref 4))

(check "funcall of own %KERNEL" (use-kernel "DESK") '("GETENV" "DESK"))
(check "own %KERNEL direct" (funcall (%kernel "X")) '("X"))
(check "own %BULK-MOVE" (use-bulk) 5)
(check "own MEM-REF" (use-mem-ref) 40)

(format t "user-fn-named-like-intrinsic: ~d checks, ~d failed~%" *checks* *fails*)
(if (zerop *fails*) (format t "PASS~%") (format t "FAIL~%"))
