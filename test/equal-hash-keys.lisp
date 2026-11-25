;;;; test/equal-hash-keys.lisp -- EQUAL on lists and atoms; keyword and list keys in hash tables.
;;;;   modus --script test/equal-hash-keys.lisp   => EQUAL-HASH-KEYS PASS
;;;;
;;;; EQUAL tests EQ and a cons before EQL's number tests and walks the CDR in a loop (a 200000-long
;;;; list); STRINGP of an ordinary list answers without %WRAPPER-STRINGP; a KEYWORD hashes by its
;;;; name hash (it was :NOHASH: the linear path, and (:F32 . 4) collided with (:I64 . 4)).  The
;;;; expected list is SBCL's.
(defvar *long* (make-list 200000 :initial-element 3))
(defvar *got*
  (list (equal 'a 'a) (equal 'a 'b) (equal 1 1) (equal 1 1.0) (equal 1.5 1.5) (equal #\a #\a) (equal #\a #\b)
        (equal (cons :f32 4) (cons :f32 4)) (equal (cons :f32 4) (cons :i64 4)) (equal "ab" "ab") (equal 'a "a")
        (equal (expt 2 70) (expt 2 70)) (equal #*01 (copy-seq #*01)) (equal nil nil) (equal nil '())
        (equal '(1 (2.5 "x") . 3) (list 1 (list 2.5 (copy-seq "x")) 3)) (equal '(1 2) '(1 2 3)) (equal '(1 2 3) '(1 2))
        (equal '(a . b) '(a . c)) (equal (list 1/2 #c(1 2)) (list 1/2 #c(1 2)))
        (equal "ab" (make-array 2 :element-type 'character :initial-contents "ab" :adjustable t))
        (equal *long* (copy-list *long*)) (equal (list "a" 'b) (list "A" 'b)) (equal 1d0 1d0) (equal 0.0 -0.0)
        (let ((h (make-hash-table :test 'equal)))
          (setf (gethash (cons :f32 4) h) 1 (gethash (cons :i64 4) h) 2 (gethash :kw h) 3 (gethash (list :a :b) h) 4)
          (list (gethash (cons :f32 4) h) (gethash (cons :i64 4) h) (gethash :kw h) (gethash (list :a :b) h)
                (gethash (list :a :c) h) (hash-table-count h)))
        (let ((h (make-hash-table :test 'eq)))
          (setf (gethash :x h) 1 (gethash :y h) 2)
          (list (gethash :x h) (gethash :y h) (gethash :z h)))
        (let ((h (make-hash-table :test 'equalp)))
          (setf (gethash (list :k "AB") h) 9)
          (gethash (list :k "ab") h))
        (stringp (cons :a 1)) (stringp (make-array 3 :element-type 'character :fill-pointer 2)) (stringp '(1 . 2))))
(defvar *want* '(T NIL T NIL T T NIL T NIL T NIL T T T T NIL NIL NIL NIL T T T NIL T NIL
                 (1 2 3 4 NIL 4) (1 2 NIL) 9 NIL T NIL))
(let ((bad 0))
  (loop for g in *got* for w in *want* for i from 0
        unless (equal g w) do (incf bad) (format t "FAIL item ~d: ~s, want ~s~%" i g w))
  (unless (= (length *got*) (length *want*)) (incf bad))
  (format t "~&EQUAL-HASH-KEYS ~A~%" (if (zerop bad) "PASS" "FAIL")))
