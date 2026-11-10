;;;; term-format.lisp -- the wire format's round trips: UTF-8 strings (tag 7) and double-floats (tag 8).
;;;;
;;;;   ./modus --script test/term-format.lisp        (run from the modus checkout)
;;;;
;;;; The functions are READ FROM net/actors.lisp and evaluated, not copied, so what is tested is what
;;;; ships.  The forms are read into MODUS-INT, the package the image compiles the kernel in, so their
;;;; names resolve as they do there.  What is supplied here, because this runner's image lacks it:
;;;;   - DECODE-PTR-ADDR, which the live actor path provides with the staging buffer (a cell that
;;;;     points the decoder at the bytes);
;;;;   - %MAKE-FLOAT2 and %FLOAT-SET-BITS, the double constructor and its setter (prelude.lisp, which
;;;;     this image does not carry).  The stand-in builds the double from its IEEE bits by arithmetic,
;;;;     exact for every finite double and zero.  The encoder uses the real %FLOAT-HI32 and %FLOAT-LO32.
;;;; Every round trip asserts the DECODED VALUE, and that the byte count TERM-SIZE predicts is the
;;;; count TERM-ENCODE writes: a size that is wrong overruns or underfills the staging buffer.

(defparameter *wanted*
  '("SOFT-SUBTAG" "TERM-FLOAT-P" "%UTF8-LENGTH" "%UTF8-PUT" "%UTF8-GET"
    "TERM-SIZE" "TERM-SIZE-VALUE" "TERM-ENCODE" "TERM-ENCODE-VALUE" "TERM-DECODE-STEP"))

(defparameter *cell* (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
(defparameter *buf* (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0))

(defun %addr (array)
  (+ (modus-int::%gc-word-of array (modus-int::%conv-addr #x100050A0)) 7))

(defun bits-to-double (hi lo)
  "The double whose IEEE bits are HI (the high 32) and LO (the low 32).  Arithmetic: exact for every
   finite double and zero.  Infinity and NaN are not built here, and no test uses them."
  (let* ((sign (ash hi -31))
         (expo (logand (ash hi -20) #x7FF))
         (mant (+ (* (logand hi #xFFFFF) 4294967296) lo))
         (value (cond ((and (= expo 0) (= mant 0)) 0.0d0)
                      ((= expo 0) (* (float mant 1d0) (expt 2d0 -1074)))
                      (t (* (+ 1d0 (/ (float mant 1d0) 4503599627370496d0))
                            (expt 2d0 (- expo 1023)))))))
    (if (= sign 1) (- 0.0d0 value) value)))

;; the image's float helpers, under the names the forms in net/actors.lisp use
(setf (fdefinition '%float-lo32) (fdefinition 'modus-int::%float-lo32))
(setf (fdefinition '%float-hi32) (fdefinition 'modus-int::%float-hi32))

;; the stand-ins for what this image lacks (only where it lacks them)
(defun %standin (sym fn)
  (unless (fboundp sym) (setf (fdefinition sym) fn)))

(%standin 'decode-ptr-addr (lambda () (%addr *cell*)))
(%standin '%make-float2 (lambda () 0.0d0))
(%standin '%float-set-bits (lambda (f hi lo) (declare (ignore f)) (bits-to-double hi lo)))

;; the forms, read from the file
(with-open-file (in "net/actors.lisp")
  (loop for form = (read in nil :eof)
        until (eq form :eof)
        do (when (and (consp form)
                      (member (car form) '(defun defparameter defvar))
                      (member (symbol-name (second form)) *wanted* :test #'string=))
             (handler-case (eval form)
               (serious-condition (e) (format t "~&EVAL of ~A failed: ~A~%" (second form) e))))))

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
