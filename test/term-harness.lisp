;;;; term-harness.lisp -- the wire format (net/term.lisp), loaded and usable from a script: the cross-language
;;;; tests load it.  The caller provides DECODE-PTR-ADDR (the decoder's read cursor); here it is a cell of
;;;; this script's own.  Everything else is the shipped code.

(defparameter *cell* (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
(defparameter *buf* (make-array 65536 :element-type '(unsigned-byte 8) :initial-element 0))

(defun %addr (array)
  (+ (modus-int::%gc-word-of array (modus-int::%conv-addr #x100050A0)) 7))

(defun decode-ptr-addr () (%addr *cell*))

(load "net/term.lisp")

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
