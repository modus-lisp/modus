;;;; term-format.lisp -- the wire format's round trips: UTF-8 strings (tag 7), double-floats (tag 8), nil and
;;;; fixnums.  The codec is net/term.lisp, loaded as shipped (test/term-harness.lisp).
;;;;   ./modus --script test/term-format.lisp        (run from the modus checkout)

(load "test/term-harness.lisp")

(defvar *pass* 0)
(defvar *fail* 0)

(defun check (name ok)
  (if ok
      (progn (incf *pass*) (format t "~&PASS ~A~%" name))
      (progn (incf *fail*) (format t "~&FAIL ~A~%" name))))

(defun wire-round-trip (value)
  "VALUE encoded into the buffer, the bytes it took, and the value decoded back from them."
  (let ((n (term-encode value (%addr *buf*))))
    (setf (mem-ref (%addr *cell*) :u64) (%addr *buf*))
    (values (term-decode-step) n)))

(defun codes (&rest codes)
  (map 'string #'code-char codes))

;;; ---- UTF-8 strings ---------------------------------------------------------------------------

(let ((s "hello"))
  (multiple-value-bind (back n) (wire-round-trip s)
    (check "utf8: ASCII round trips" (equal back s))
    (check "utf8: size predicted for ASCII" (= n (term-size s)))))

(let ((s (codes 99 97 102 233)))                     ; "café": the e-acute is two bytes in UTF-8
  (multiple-value-bind (back n) (wire-round-trip s)
    (check "utf8: Latin-1 round trips (cafe with an e-acute)" (equal back s))
    (check "utf8: two-byte character sized" (= n (term-size s)))
    (check "utf8: the e-acute is two bytes on the wire" (= n (+ 5 5)))))

(let ((s (codes #x65e5 #x672c)))                     ; "日本": three bytes each
  (multiple-value-bind (back n) (wire-round-trip s)
    (check "utf8: CJK round trips" (equal back s))
    (check "utf8: three-byte characters sized" (= n (term-size s)))))

(let ((s (codes #x1f642)))                           ; a four-byte character
  (multiple-value-bind (back n) (wire-round-trip s)
    (check "utf8: four-byte character round trips" (equal back s))
    (check "utf8: four-byte character sized" (= n (term-size s)))))

(check "utf8: the empty string round trips" (equal (wire-round-trip "") ""))

;;; ---- double-floats ---------------------------------------------------------------------------

(dolist (x '(1.5d0 -0.25d0 1.0d300 3.141592653589793d0))
  (multiple-value-bind (back n) (wire-round-trip x)
    (check (format nil "float: ~A round trips" x) (and (floatp back) (= back x)))
    (check (format nil "float: ~A is nine bytes" x) (= n 9))))

(multiple-value-bind (back n) (wire-round-trip 0.0d0)
  (check "float: 0.0 is a float, not nil" (and (floatp back) (= back 0.0d0)))
  (check "float: 0.0 is nine bytes" (= n 9))
  (check "float: 0.0 sized as written" (= n (term-size 0.0d0))))

;;; ---- nil and zero: NIL is its own tag now, so a fixnum zero is not nil (hosted images) ---------

(check "nil: round trips as nil" (null (wire-round-trip nil)))
(check "zero: a fixnum 0 round trips as 0, not nil" (let ((z (wire-round-trip 0))) (and (numberp z) (= z 0))))
(check "nil: a list ends in nil and sizes" (= (nth-value 1 (wire-round-trip (list 1 2))) (term-size (list 1 2))))

;;; ---- records: a plist of keyword keys, strings, numbers and floats ----------------------------

(let* ((record (list :id "a1" :title "Apples" :count 3 :weight 2.5d0 :kind :fruit))
       (back (wire-round-trip record)))
  ;; positions: 0 :id 1 "a1" 2 :title 3 "Apples" 4 :count 5 3 6 :weight 7 2.5 8 :kind 9 :fruit
  (check "record: keys come back as keywords"
         (and (eq (first back) :id) (eq (third back) :title) (eq (fifth back) :count)
              (eq (seventh back) :weight) (eq (nth 8 back) :kind)))
  (check "record: text values round trip" (and (equal (second back) "a1") (equal (fourth back) "Apples")))
  (check "record: the integer round trips" (= (nth 5 back) 3))
  (check "record: the float round trips" (and (floatp (nth 7 back)) (= (nth 7 back) 2.5d0)))
  (check "record: the keyword value round trips" (eq (nth 9 back) :fruit))
  (check "record: sized as written" (= (nth-value 1 (wire-round-trip record)) (term-size record))))

(let* ((names (list "café" "日本" (codes #x1f642) 1.25d0))
       (back (wire-round-trip names)))
  (check "list: mixed strings and floats in one message"
         (and (equal (first back) "café") (equal (second back) "日本")
              (equal (third back) (codes #x1f642)) (= (fourth back) 1.25d0))))

(format t "~&~D passed, ~D failed~%" *pass* *fail*)
