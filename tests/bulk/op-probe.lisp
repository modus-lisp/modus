;;;; op-probe.lisp -- %BULK-MOVE / %BULK-FILL called directly, so a REPL-compiled
;;;; function reaches the in-image JIT arm and, under MODUS_NO_JIT=1, the
;;;; interpreter reference arm.  Expected on every build:
;;;;   OPS #(0 1 0 1 2 3 4 7 8 9) #(3 4 5 6 7 8 9 10 11 12 13 14 12 13 14 15 16 17 18 19)
;;;;   OPS2 #(0 (:X) (:X) (:X) 2 3 4 7 8 9) #(3 4 5 6 7 44 44 44 44 44 44 44 44 44 44 44 16 17 18 19)
(defun op-mv0 (d di s si n) (%bulk-move d di s si n 0))
(defun op-mv1 (d di s si n) (%bulk-move d di s si n 1))
(defun op-fl0 (a i n v) (%bulk-fill a i n v 0))
(defun op-fl1 (a i n v) (%bulk-fill a i n v 1))
(let ((w (vector 0 1 2 3 4 5 6 7 8 9)) (u (make-array 20 :element-type '(unsigned-byte 8))))
  (dotimes (i 20) (setf (aref u i) i))
  (op-mv0 w 2 w 0 5)
  (op-mv1 u 0 u 3 12)
  (format t "OPS ~S ~S~%" w u)
  (op-fl0 w 1 3 (list :x))
  (op-fl1 u 5 11 300)
  (format t "OPS2 ~S ~S~%" w u))
