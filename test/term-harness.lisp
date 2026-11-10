;;;; term-harness.lisp -- the wire functions of net/actors.lisp, loaded and usable from a script (the
;;;; cross-language tests load it).  The functions are READ FROM net/actors.lisp and evaluated, so what runs
;;;; is what ships.  This image lacks two float helpers and the decode cursor; they are supplied here (see
;;;; test/term-format.lisp for the reasoning).

(defparameter *wanted*
  '("SOFT-SUBTAG" "TERM-FLOAT-P" "%UTF8-LENGTH" "%UTF8-PUT" "%UTF8-GET"
    "TERM-SIZE" "TERM-SIZE-VALUE" "TERM-ENCODE" "TERM-ENCODE-VALUE" "TERM-DECODE-STEP"))

(defparameter *cell* (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
(defparameter *buf* (make-array 65536 :element-type '(unsigned-byte 8) :initial-element 0))

(defun %addr (array)
  (+ (modus-int::%gc-word-of array (modus-int::%conv-addr #x100050A0)) 7))

(defun bits-to-double (hi lo)
  "The double whose IEEE bits are HI (the high 32) and LO (the low 32): arithmetic, exact for every
   finite double and zero."
  (let* ((sign (ash hi -31))
         (expo (logand (ash hi -20) #x7FF))
         (mant (+ (* (logand hi #xFFFFF) 4294967296) lo))
         (value (cond ((and (= expo 0) (= mant 0)) 0.0d0)
                      ((= expo 0) (* (float mant 1d0) (expt 2d0 -1074)))
                      (t (* (+ 1d0 (/ (float mant 1d0) 4503599627370496d0))
                            (expt 2d0 (- expo 1023)))))))
    (if (= sign 1) (- 0.0d0 value) value)))

(setf (fdefinition '%float-lo32) (fdefinition 'modus-int::%float-lo32))
(setf (fdefinition '%float-hi32) (fdefinition 'modus-int::%float-hi32))

(defun %standin (sym fn)
  (unless (fboundp sym) (setf (fdefinition sym) fn)))

(%standin 'decode-ptr-addr (lambda () (%addr *cell*)))
(%standin '%make-float2 (lambda () 0.0d0))
(%standin '%float-set-bits (lambda (f hi lo) (declare (ignore f)) (bits-to-double hi lo)))

(with-open-file (in "net/actors.lisp")
  (loop for form = (read in nil :eof)
        until (eq form :eof)
        do (when (and (consp form)
                      (member (car form) '(defun defparameter defvar))
                      (member (symbol-name (second form)) *wanted* :test #'string=))
             (handler-case (eval form)
               (serious-condition (e) (format t "~&EVAL of ~A failed: ~A~%" (second form) e))))))

(defun term-hex (value)
  "VALUE as the wire bytes, in hex."
  (let* ((n (term-encode value (%addr *buf*)))
         (s (make-string (* 2 n)))
         (digits "0123456789abcdef"))
    (dotimes (i n s)
      (let ((b (aref *buf* i)))
        (setf (char s (* 2 i)) (char digits (ash b -4)))
        (setf (char s (+ 1 (* 2 i))) (char digits (logand b 15)))))))

(defun hex-term (hex)
  "The term the wire bytes in HEX (hex digits) decode to."
  (let ((n (floor (length hex) 2)))
    (dotimes (i n)
      (setf (aref *buf* i) (+ (* 16 (digit-char-p (char hex (* 2 i)) 16))
                              (digit-char-p (char hex (+ 1 (* 2 i))) 16))))
    (setf (mem-ref (%addr *cell*) :u64) (%addr *buf*))
    (term-decode-step)))

(defun term= (a b)
  "Structural equality for terms: floats by value, strings by characters, conses by parts."
  (cond ((and (floatp a) (floatp b)) (= a b))
        ((and (stringp a) (stringp b)) (equal a b))
        ((and (consp a) (consp b)) (and (term= (car a) (car b)) (term= (cdr a) (cdr b))))
        ((or (consp a) (consp b) (stringp a) (stringp b)) nil)
        (t (eql a b))))

(defun read-lines (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil) while line collect line)))
