;;;; test/map-coerce-fast.lisp -- MAP and COERCE of one list or simple-vector, walked directly.
;;;;   modus --script test/map-coerce-fast.lisp   => MAP-COERCE-FAST PASS
;;;;
;;;; MAP of one list / simple-vector into NIL / LIST / VECTOR / SIMPLE-VECTOR, and COERCE of a list or
;;;; simple-vector to LIST / VECTOR / SIMPLE-VECTOR, skip the general paths (type expansion, an APPLYed
;;;; MAPCAR).  They must still answer what those did, signal on a dotted list, and leave every other
;;;; shape (strings, several sequences, other result types) to the general path.
(defvar *fails* 0)
(defun check (name got want)
  (if (equalp got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))
(defvar *v* (vector 1 2 3))
(check "list->list" (map 'list #'1+ '(1 2 3)) '(2 3 4))
(check "list->vector" (map 'vector #'1+ '(1 2 3)) #(2 3 4))
(check "vector->simple-vector" (map 'simple-vector #'1+ *v*) #(2 3 4))
(check "vector->list" (map 'list #'1+ *v*) '(2 3 4))
(check "for-effect" (let ((acc nil)) (map nil (lambda (e) (push e acc)) *v*) acc) '(3 2 1))
(check "for-effect-value" (map nil #'identity '(1 2)) nil)
(check "empty" (list (map 'vector #'1+ nil) (map 'list #'1+ #())) (list #() nil))
(check "dotted" (handler-case (map 'list #'identity '(1 2 . 3)) (type-error () :te)) :te)
(check "string-general" (map 'string #'char-upcase "abc") "ABC")
(check "two-seqs-general" (map 'list #'list '(1 2) '(a b c)) '((1 a) (2 b)))
(check "fresh-result" (let ((r (map 'vector #'identity *v*))) (eq r *v*)) nil)
(check "coerce-list->vector" (coerce '(1 2 3) 'vector) #(1 2 3))
(check "coerce-list->sv" (coerce '(1 2 3) 'simple-vector) #(1 2 3))
(check "coerce-vector->list" (coerce *v* 'list) '(1 2 3))
(check "coerce-identity" (eq (coerce *v* 'vector) *v*) t)
(check "coerce-nil" (coerce nil 'vector) #())
(check "coerce-dotted" (handler-case (coerce '(1 . 2) 'vector) (type-error () :te)) :te)
(check "coerce-string-general" (coerce "ab" 'list) '(#\a #\b))
(format t "~&MAP-COERCE-FAST ~A~%" (if (zerop *fails*) "PASS" "FAIL"))
