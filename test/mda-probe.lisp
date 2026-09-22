;;; mda-probe.lisp -- what a compiled (%MDA-P x) must still answer, and what
;;; the operators built on it must still do.  Run on main's binary, on the
;;; inlined binary, and on SBCL (the last only for the CL-level half).
(defun p (label v) (format t "~a ~s~%" label v))

;; --- the predicate's own truth table, through the compiled call site ---
(defun mdap (x) (%mda-p x))
(p "nil"       (mdap nil))
(p "t"         (mdap t))
(p "fixnum"    (mdap 7))
(p "bignum"    (mdap (* 1234567890123 987654321)))
(p "cons"      (mdap (cons 1 2)))
(p "char"      (mdap #\a))
(p "string"    (mdap "abc"))
(p "symbol"    (mdap 'foo))
(p "keyword"   (mdap :foo))
(p "svec"      (mdap (make-array 3)))
(p "fn"        (mdap #'car))
(p "2d"        (mdap (make-array '(2 3))))
(p "adj"       (mdap (make-array 4 :adjustable t)))
(p "fp"        (mdap (make-array 4 :fill-pointer 0)))
(p "charmda"   (mdap (make-array 4 :element-type 'character :fill-pointer 0)))

;; --- the operators that sit on top of it ---
(let ((a (make-array '(2 3) :initial-element 0)))
  (setf (aref a 1 2) 41)
  (p "2d-aref" (aref a 1 2))
  (p "2d-dims" (array-dimensions a))
  (p "2d-rank" (array-rank a))
  (p "2d-total" (array-total-size a))
  (p "2d-rma" (row-major-aref a 5)))

(let ((v (make-array 4 :fill-pointer 0)))
  (vector-push 10 v) (vector-push 20 v)
  (p "fp-len" (length v))
  (p "fp-elt" (elt v 1))
  (p "fp-pop" (vector-pop v))
  (p "fp-len2" (length v))
  (p "fp-vpe" (progn (vector-push-extend 99 v) (coerce v 'list))))

(let ((s (make-array 5 :element-type 'character :fill-pointer 0)))
  (vector-push #\h s) (vector-push #\i s)
  (p "adjstr-stringp" (stringp s))
  (p "adjstr-val" (copy-seq s))
  (p "adjstr-len" (length s))
  (p "adjstr-char" (char s 1))
  (p "adjstr-concat" (concatenate 'string s "!")))

(let* ((base (make-array 6 :initial-contents '(0 1 2 3 4 5)))
       (d (make-array 3 :displaced-to base :displaced-index-offset 2)))
  (p "disp-elts" (coerce d 'list))
  (p "disp-aref" (aref d 0)))

(let ((a (make-array 3 :adjustable t :initial-contents '(1 2 3))))
  (p "adj-elts" (coerce a 'list))
  (p "adj-print" (format nil "~s" a))
  (p "adj-arrayp" (arrayp a))
  (p "adj-vectorp" (vectorp a))
  (p "adj-simple" (typep a 'simple-vector)))

;; sequence functions over an MDA vector
(let ((v (make-array 5 :fill-pointer 5 :initial-contents '(3 1 4 1 5))))
  (p "seq-find" (find 4 v))
  (p "seq-pos" (position 1 v))
  (p "seq-sort" (coerce (sort (copy-seq v) #'<) 'list))
  (p "seq-reduce" (reduce #'+ v))
  (p "seq-map" (map 'list #'1+ v))
  (p "seq-rev" (coerce (reverse v) 'list))
  (p "seq-sub" (coerce (subseq v 1 3) 'list)))

(p "str-eq" (string= (make-array 3 :element-type 'character
                                 :initial-contents '(#\a #\b #\c)) "abc"))
(p "done" t)
