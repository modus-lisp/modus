;;;; test/typep-element-type.lisp -- TYPEP on an array type checks the ELEMENT
;;;; TYPE: a general (element-type T) vector is not an (UNSIGNED-BYTE 8) one.
;;;;
;;;;   ./modus --script test/typep-element-type.lisp     (identical under sbcl --script)
;;;;
;;;; Until 2026-10-14 a general vector matched every non-character element type,
;;;; so (typep (vector 1 "two") '(simple-array (unsigned-byte 8) (*))) was T, and
;;;; MODUS:CHANNEL-SEND, asking exactly that, sent its elements as bytes.  Each
;;;; row's expected answer is SBCL's; the file prints the same lines on both.

(defvar *fails* 0)
(defun row (what got want)
  (format t "~&~a ~a: ~a~%" (if (eq (and got t) want) "ok  " "FAIL") what (and got t))
  (unless (eq (and got t) want) (setq *fails* (+ *fails* 1))))

(let ((gen (vector 1 "two" (list 3)))
      (zeros (make-array 3 :initial-element 0))
      (u8 (make-array 3 :element-type '(unsigned-byte 8)))
      (str (make-string 2)))
  (row "general vector, (simple-array (unsigned-byte 8) (*))" (typep gen '(simple-array (unsigned-byte 8) (*))) nil)
  (row "general vector of 0s, (vector (unsigned-byte 8))" (typep zeros '(vector (unsigned-byte 8))) nil)
  (row "general vector, (vector (integer 0 255))" (typep gen '(vector (integer 0 255))) nil)
  (row "general vector, (vector t)" (typep gen '(vector t)) t)
  (row "general vector, simple-vector" (typep gen 'simple-vector) t)
  (row "general vector, (simple-array t (*))" (typep gen '(simple-array t (*))) t)
  (row "general vector, (vector *)" (typep gen '(vector *)) t)
  (row "u8 vector, (simple-array (unsigned-byte 8) (*))" (typep u8 '(simple-array (unsigned-byte 8) (*))) t)
  (row "u8 vector, (vector t)" (typep u8 '(vector t)) nil)
  (row "string, (vector (unsigned-byte 8))" (typep str '(vector (unsigned-byte 8))) nil)
  (row "string, (vector character)" (typep str '(vector character)) t))
(let ((bits (make-array 4 :element-type 'bit :initial-element 0)))
  (row "bit vector, (vector (unsigned-byte 1))" (typep bits '(vector (unsigned-byte 1))) t)
  (row "bit vector, (vector bit)" (typep bits '(vector bit)) t))
(let ((f (make-sequence '(vector single-float) 3 :initial-element 1.0)))
  (row "(make-sequence '(vector single-float) ...) is one" (typep f '(vector single-float)) t))
(row "(make-sequence '(vector (unsigned-byte 8)) ...) is one"
     (typep (make-sequence '(vector (unsigned-byte 8)) 3 :initial-element 0) '(vector (unsigned-byte 8))) t)
(format t "~&TYPEP-ELEMENT-TYPE: ~a failed~%" *fails*)
#+modus (sys-exit (if (zerop *fails*) 0 1))
#-modus (sb-ext:exit :code (if (zerop *fails*) 0 1))
