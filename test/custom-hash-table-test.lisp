;;;; test/custom-hash-table-test.lisp — a hash table whose :TEST is the program's own
;;;; function compares with it (SBCL's extension), instead of quietly becoming EQL.
;;;; Run: ./modus --load test/custom-hash-table-test.lisp   Prints ALL PASS or N FAILED last.

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*))
  (finish-output))

(defun ci= (a b) (string-equal a b))
(defun ci-hash (k) (sxhash (string-upcase k)))
(defun sym-ci= (a b) (string-equal (symbol-name a) (symbol-name b)))

(let ((h (make-hash-table :test #'ci= :hash-function #'ci-hash)))
  (setf (gethash "Abc" h) 1)
  (check "a function :TEST finds a key equal under it" (eql (gethash "aBC" h) 1))
  (setf (gethash "ABC" h) 2)
  (check "and storing under an equal key replaces, not adds"
         (and (eql (gethash "abc" h) 2) (= (hash-table-count h) 1)))
  (check "REMHASH by an equal key removes it" (and (remhash "aBc" h) (zerop (hash-table-count h)))))
(let ((h (make-hash-table :test 'ci=)))
  (setf (gethash "Abc" h) 1)
  (check "a symbol :TEST too" (eql (gethash "ABC" h) 1)))
(let ((h (make-hash-table :test 'sym-ci=)))
  (setf (gethash 'foo h) 1)
  (check "symbol keys go through the test, not EQ"
         (eql (gethash (make-symbol "foo") h) 1)))
(let ((h (make-hash-table :test 'ci=)))
  (dotimes (i 200) (setf (gethash (format nil "Key~d" i) h) i))
  (check "past the index threshold it still finds every key by the test"
         (loop for i below 200 always (eql (gethash (format nil "KEY~d" i) h) i)))
  (check "and counts them" (= (hash-table-count h) 200)))
(check "DEFINE-HASH-TABLE-TEST answers the name" (eq (define-hash-table-test 'ci= #'ci-hash) 'ci=))
(check "and refuses a name with no function"
       (handler-case (progn (define-hash-table-test 'chtt-none #'ci-hash) nil) (error () t)))
;; the standard tests are as they were
(let ((e (make-hash-table :test 'equal)) (q (make-hash-table :test 'eql)) (p (make-hash-table :test 'equalp)))
  (setf (gethash "ab" e) 1 (gethash (list 1 2) e) 2 (gethash 5 q) 3 (gethash "AB" p) 4)
  (check "EQUAL, EQL and EQUALP tables answer as before"
         (and (eql (gethash (copy-seq "ab") e) 1) (eql (gethash (list 1 2) e) 2)
              (eql (gethash 5 q) 3) (null (gethash (copy-seq "ab") q))
              (eql (gethash "ab" p) 4)))
  (check "and report their tests"
         (equal (list (hash-table-test e) (hash-table-test q) (hash-table-test p)) '(equal eql equalp))))
(let ((q (make-hash-table :test 'eql)))
  (dotimes (i 300) (setf (gethash i q) (* i i)))
  (check "a large EQL table still indexes and finds" (loop for i below 300 always (eql (gethash i q) (* i i)))))

(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
