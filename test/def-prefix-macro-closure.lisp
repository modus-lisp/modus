;;;; def-prefix-macro-closure.lisp -- a closure hidden behind a user macro whose name starts with DEF
;;;; still shares the variables it mutates.
;;;;
;;;; %CFV-DEFINITION-HEAD-P kept the closure walkers from expanding every DEF... head, taking each for
;;;; a definition.  shuttle's (DEF-METHOD obj name (this args) body) expands to (PUT obj name (LAMBDA
;;;; ...)), so the LAMBDA was invisible to VARS-MUTATED-IN-LAMBDAS and a LET variable it INCFs was
;;;; never boxed: each call bumped a private copy.  Every JavaScript Array Iterator answered its first
;;;; element forever, so any for-of or spread ran to shuttle's 60M-step budget (example.com hung the
;;;; iOS browser).  Run: ./modus --script test/def-prefix-macro-closure.lisp  (modus-mac: MODUS_SLIDE)

(defpackage :dpmc (:use :cl))
(in-package :dpmc)

(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "~&PASS ~a~%" name)
      (progn (incf *fails*) (format t "~&FAIL ~a: got ~s want ~s~%" name got want))))

(defun put (o k v) (setf (getf (car o) k) v))
(defmacro def-method (target name (this args) &body body)
  `(put ,target ,name (lambda (,this ,args) (declare (ignorable ,this ,args)) ,@body)))
(defmacro wrap-method (target name (this args) &body body)   ; the same, without the DEF prefix
  `(put ,target ,name (lambda (,this ,args) (declare (ignorable ,this ,args)) ,@body)))
(defmacro define-counter (target &body body)
  `(put ,target :next (lambda () ,@body)))

(defun drive (it n) (loop repeat n collect (funcall (getf (car it) :next) nil nil)))

(defun incf-def () (let ((i 0) (it (list nil))) (def-method it :next (this args) (incf i)) it))
(defun incf-wrap () (let ((i 0) (it (list nil))) (wrap-method it :next (this args) (incf i)) it))
(defun setq-def () (let ((i 0) (it (list nil))) (def-method it :next (this args) (setq i (+ i 1))) it))
(defun define-prefix ()
  (let ((i 0) (it (list nil))) (define-counter it (incf i)) (loop repeat 3 collect (funcall (getf (car it) :next)))))

;; the shuttle Array Iterator shape: a flag and an index, both mutated by the method
(defun array-iterator (arr)
  (let ((i 0) (done nil) (it (list nil)))
    (def-method it :next (this args)
      (cond (done :done)
            ((< i (length arr)) (prog1 (nth i arr) (incf i)))
            (t (setf done t) :done)))
    it))

(check "incf behind DEF-METHOD" (drive (incf-def) 3) '(1 2 3))
(check "incf behind a non-DEF macro" (drive (incf-wrap) 3) '(1 2 3))
(check "setq behind DEF-METHOD" (drive (setq-def) 3) '(1 2 3))
(check "incf behind DEFINE-COUNTER" (define-prefix) '(1 2 3))
(check "array iterator ends" (drive (array-iterator '(10 20 30)) 5) '(10 20 30 :done :done))
(format t "~&~:[ALL PASS~;~:*~d FAILED~]~%" (and (plusp *fails*) *fails*))
