;;;; test/random-limit.lisp -- RANDOM's LIMIT must be a positive integer or a
;;;; positive float (CLHS); anything else signals TYPE-ERROR, as SBCL does.
;;;;
;;;;   ./modus --script test/random-limit.lisp      (also: sbcl --script)
;;;;
;;;; (random 0) used to return 0, so a caller indexing an empty vector with it
;;;; went on to read past the vector's end instead of failing.

(defvar *checks* 0)
(defvar *fails* 0)
(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (unless (equal got want)
    (setq *fails* (+ *fails* 1))
    (format t "~&FAIL ~a~%   got  ~s~%   want ~s~%" name got want)))

(defun outcome (limit)
  (handler-case (let ((r (random limit))) (list :value r))
    (type-error () :type-error)
    (error () :other-error)))

(dolist (bad (list 0 -1 -5 0.0 -1.0 1/2 nil :x "3"))
  (chk (format nil "(random ~s)" bad) (outcome bad) :type-error))

(defun in-range-p (limit n)
  (let ((ok t))
    (dotimes (i n ok)
      (let ((r (random limit)))
        (unless (and (typep r (if (integerp limit) 'integer 'float)) (>= r 0) (< r limit))
          (setq ok nil))))))
(dolist (good (list 1 2 7 1000 (expt 2 40) 1.0 0.5 1000.0))
  (chk (format nil "(random ~s) in range" good) (in-range-p good 2000) t))
(chk "(random 1) is 0" (random 1) 0)

(format t "~&RANDOM-LIMIT: ~a checks, ~a failed~%" *checks* *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
