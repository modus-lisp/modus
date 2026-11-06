;;;; test/vector-push-extend-octets.lisp — VECTOR-PUSH-EXTEND keeps the vector's kind.
;;;;
;;;;   ./modus --script test/vector-push-extend-octets.lisp
;;;;   (prints VECTOR-PUSH-EXTEND-OCTETS PASS n / FAIL n)
;;;;
;;;; Growing an adjustable (UNSIGNED-BYTE 8) vector used to allocate its new store with a bare
;;;; MAKE-ARRAY: a general vector, a word a byte.  Every CL answer stayed right -- the header
;;;; still said (UNSIGNED-BYTE 8) -- so only the STORE shows it: seal.http builds every body
;;;; this way, and a 2.4 MB git pack became 19 MB, past a Pi Zero desk's free heap.  The CL
;;;; questions are SBCL's answers; the store check is the one the old code fails.

(defvar *pass* 0)
(defvar *fail* 0)
(defmacro check (name form expected)
  `(let ((got (handler-case ,form (error (e) (list :error (type-of e))))))
     (if (equalp got ,expected)
         (incf *pass*)
         (progn (incf *fail*) (format t "FAIL ~a~%  expected ~s~%  got      ~s~%" ,name ,expected got)))))

(defun octet-store-p (v)
  "V's backing store is an octet vector, not a general one."
  (and (modus-int::%prim-u8-vector-p (modus-int::%mda-data v)) t))

(let ((v (make-array 4 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)) (sum 0))
  (dotimes (i 200000) (vector-push-extend (logand i 255) v))
  (dotimes (i (length v)) (incf sum (aref v i)))
  (check "element type" (array-element-type v) '(unsigned-byte 8))
  (check "length" (length v) 200000)
  (check "contents" sum 25493856)
  (check "last element" (aref v 199999) (logand 199999 255))
  (check "typep" (typep v '(vector (unsigned-byte 8))) t)
  (check "the store is octets after 16 doublings" (octet-store-p v) t))

(let ((v (make-array 0 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
  (dotimes (i 5) (vector-push-extend i v))
  (check "grown from empty" (coerce v 'list) '(0 1 2 3 4))
  (check "grown from empty: octets" (octet-store-p v) t))

(let ((v (make-array 3 :element-type '(unsigned-byte 8) :fill-pointer 3 :adjustable t
                       :initial-contents '(1 2 3))))
  (vector-push-extend 9 v 100)
  (check "extension hint" (coerce v 'list) '(1 2 3 9))
  (check "extension hint: octets" (octet-store-p v) t))

(let* ((base (make-array 10 :element-type '(unsigned-byte 8) :initial-contents '(0 1 2 3 4 5 6 7 8 9)))
       (d (make-array 4 :element-type '(unsigned-byte 8) :displaced-to base
                        :displaced-index-offset 3 :fill-pointer 4 :adjustable t)))
  (vector-push-extend 77 d)
  (check "displaced, grown: its own copy" (coerce d 'list) '(3 4 5 6 77))
  (check "displaced, grown: the base untouched" (aref base 7) 7))

(let ((v (make-array 2 :adjustable t :fill-pointer 0)))
  (dotimes (i 1000) (vector-push-extend (list i) v))
  (check "general vector stays general" (array-element-type v) t)
  (check "general vector contents" (aref v 999) '(999)))

(let ((s (make-array 1 :element-type 'character :adjustable t :fill-pointer 0)))
  (dotimes (i 300) (vector-push-extend (code-char (+ 97 (mod i 26))) s))
  (check "string contents" (subseq s 0 5) "abcde")
  (check "string length" (length s) 300))

;; a float store must not take the octet path: its values, not their raw words, come back
;; (upstream VECTOR-PUSH-EXTEND.30 caught a version that grew it into a packed store)
(let* ((a1 (make-array 8 :initial-element 0.0f0 :element-type 'single-float))
       (a2 (make-array 6 :element-type 'single-float :displaced-to a1
                         :displaced-index-offset 2 :adjustable t :fill-pointer 6)))
  (vector-push-extend 1.0f0 a2)
  (check "single-float, displaced and grown" (coerce a2 'list) '(0.0 0.0 0.0 0.0 0.0 0.0 1.0)))
(let ((v (make-array 2 :element-type 'double-float :adjustable t :fill-pointer 0)))
  (dotimes (i 100) (vector-push-extend (float i 1d0) v))
  (check "double-float grown" (list (aref v 0) (aref v 99)) '(0.0d0 99.0d0)))

(format t "~&VECTOR-PUSH-EXTEND-OCTETS ~:[FAIL~;PASS~] ~d passed, ~d failed~%"
        (zerop *fail*) *pass* *fail*)
