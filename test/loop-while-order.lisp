;;;; test/loop-while-order.lisp -- a WHILE or UNTIL among the FOR clauses runs in its source place.
;;;;   modus --script test/loop-while-order.lisp   => LOOP-WHILE-ORDER PASS
;;;;
;;;; (loop while (< i end) for c = (char s i) ...) stepped C before testing I, so C read (char s end):
;;;; weft's CSS identifier reader, and with it every page the browser loaded.  SBCL, CCL and MIT LOOP
;;;; test first.  The other shapes here are the ones the LOOP compiler's comments name as having broken
;;;; under earlier orderings.
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))
(defmacro safely (form) `(handler-case ,form (error (e) (list :error (princ-to-string e)))))

(defun read-ident (s)
  (let ((i 0) (end (length s)) (out '()))
    (loop while (< i end) for c = (char s i) do (push c out) (incf i))
    (coerce (nreverse out) 'string)))
(check "while before for = reads no further than the test allows" (safely (read-ident "meta")) "meta")
(check "until before for =" (safely (let ((i 0) (s "ab") (out '()))
                                       (loop until (>= i (length s)) for c = (char s i) do (push c out) (incf i))
                                       (nreverse out)))
       '(#\a #\b))
(check "for x in list while x" (safely (loop for x in '(1 2 nil 4) while x collect x)) '(1 2))
(check "for from, then while on it" (safely (loop for i from 0 while (< i 3) collect i)) '(0 1 2))
(check "while alone" (safely (let ((n 0)) (loop while (< n 5) do (incf n)) n)) 5)
(check "for = then, while between fors"
       (safely (loop for i from 0 below 10 for sq = (* i i) while (< sq 20) collect sq))
       '(0 1 4 9 16))
(check "while after do stays in place"
       (safely (let ((k 0)) (loop do (incf k) while (< k 3)) k)) 3)
(check "with, while, for"
       (safely (loop with v = #(1 2 3) for i from 0 while (< i (length v)) for x = (aref v i) collect x))
       '(1 2 3))

(format t "~&LOOP-WHILE-ORDER ~:[FAIL (~D)~;PASS~]~%" (zerop *fails*) *fails*)
