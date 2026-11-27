;;;; test/setf-place-known-p.lisp — %SETF-PLACE-KNOWN-P says no for a place
;;;; SETF would compile to the SET-NAME fallback (mvm/compiler.lisp).
;;;; Run: ./modus --load test/setf-place-known-p.lisp
;;;; Prints one line per check and ALL PASS or N FAILED last.
;;;;
;;;; Since the fallback learned to look up a late (SETF NAME) at run time it is
;;;; nested inside a LET*, and a test of the expansion's head answered T for
;;;; every name.  cortez asks this (CORTEZ.HOST:SETF-EXPANDER) to decide whether
;;;; a corpus accessor still needs its DEFSETF fetched, so a T for everything
;;;; meant none was fetched.

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*))
  (finish-output))

(defun known (name) (funcall (find-symbol "%SETF-PLACE-KNOWN-P" "MODUS-INT") name))

(declaim (inline sk-inline))
(defun sk-inline (a i) (sk-elsewhere a i))
(defun sk-plain (a) a)
(defun sk-with-setter (a) a)
(defun set-sk-with-setter (a v) (declare (ignore a)) v)
(defun sk-with-setf-fn (a) (car a))
(defun (setf sk-with-setf-fn) (v a) (setf (car a) v))
(defun sk-with-defsetf (a) (car a))
(defsetf sk-with-defsetf (a) (v) `(setf (car ,a) ,v))
(defstruct sk-point x)

(check "a name SETF has never heard of is not a known place" (not (known 'sk-never-heard)))
(check "nor is a plain function with no setter" (not (known 'sk-plain)))
(check "nor an inline one whose body names no setter either" (not (known 'sk-inline)))
(check "a function whose SET-NAME the image defines is" (known 'sk-with-setter))
(check "and one with a (SETF NAME) function" (known 'sk-with-setf-fn))
(check "and one with a DEFSETF" (known 'sk-with-defsetf))
(check "and a DEFSTRUCT accessor" (known 'sk-point-x))
(check "and the places SETF stores into itself: CAR GETF AREF GETHASH CHAR"
       (every #'known '(car getf aref gethash char)))

(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
