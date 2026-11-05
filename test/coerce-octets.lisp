;;;; test/coerce-octets.lisp — COERCE returns an octet vector that is already of the type.
;;;;
;;;;   ./modus --script test/coerce-octets.lisp
;;;;   (prints COERCE-OCTETS PASS n / FAIL n)
;;;;
;;;; CLHS COERCE: "If object is already of type result-type, the object itself is returned."
;;;; Modus spotted that only on an exact TYPE-OF match, which a type with an open length --
;;;; (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) -- never is, so every such COERCE copied.  cram's
;;;; inflate coerces its input that way on each call, and cairn inflates each object of a git
;;;; pack from the whole pack: kiln's 1210 objects copied its 2.4 MB pack 1210 times.  The
;;;; identity answers are SBCL's; the copies must still be copies.

(defvar *pass* 0)
(defvar *fail* 0)
(defmacro check (name form expected)
  `(let ((got (handler-case ,form (error (e) (list :error (type-of e))))))
     (if (equalp got ,expected)
         (incf *pass*)
         (progn (incf *fail*) (format t "FAIL ~a~%  expected ~s~%  got      ~s~%" ,name ,expected got)))))

(let ((v (make-array 5 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4 5))))
  ;; already of the type: the object itself
  (check "simple-array (*)" (eq v (coerce v '(simple-array (unsigned-byte 8) (*)))) t)
  (check "simple-array (5)" (eq v (coerce v '(simple-array (unsigned-byte 8) (5)))) t)
  (check "simple-array *" (eq v (coerce v '(simple-array (unsigned-byte 8) *))) t)
  (check "simple-array no dims" (eq v (coerce v '(simple-array (unsigned-byte 8)))) t)
  (check "array (*)" (eq v (coerce v '(array (unsigned-byte 8) (*)))) t)
  (check "array rank 1" (eq v (coerce v '(array (unsigned-byte 8) 1))) t)
  (check "vector" (eq v (coerce v '(vector (unsigned-byte 8)))) t)
  (check "vector 5" (eq v (coerce v '(vector (unsigned-byte 8) 5))) t)
  (check "vector *" (eq v (coerce v '(vector (unsigned-byte 8) *))) t)
  ;; a wrong length is an error, not this object
  (check "vector 4 is not v" (handler-case (eq v (coerce v '(vector (unsigned-byte 8) 4))) (error () :error)) :error)
  ;; not already of the type: a fresh octet vector
  (let ((c (coerce (list 1 2 3) '(simple-array (unsigned-byte 8) (*)))))
    (check "list -> octets" (list (length c) (aref c 2) (array-element-type c)) '(3 3 (unsigned-byte 8))))
  (let* ((g (vector 7 8 9)) (c (coerce g '(simple-array (unsigned-byte 8) (*)))))
    (check "general vector -> octets is a copy" (list (eq c g) (aref c 0) (array-element-type c))
           '(nil 7 (unsigned-byte 8))))
  (let* ((a (make-array 3 :element-type '(unsigned-byte 8) :adjustable t :initial-contents '(4 5 6)))
         (c (coerce a '(simple-array (unsigned-byte 8) (*)))))
    (check "adjustable -> simple is a copy" (list (eq c a) (aref c 1)) '(nil 5)))
  ;; other element types are untouched by this path
  (check "octets -> list" (coerce v 'list) '(1 2 3 4 5))
  (check "octets -> general vector" (array-element-type (coerce v 'simple-vector)) t)
  (check "octets -> simple-vector contents" (coerce v 'simple-vector) #(1 2 3 4 5)))

;; SIMPLE-VECTOR means element type T: anything specialized is copied, a general one is itself
(check "string -> simple-vector" (coerce "abc" 'simple-vector) #(#\a #\b #\c))
(check "string -> simple-vector is general" (array-element-type (coerce "abc" 'simple-vector)) t)
(check "single-floats -> simple-vector"
       (coerce (make-array 2 :element-type 'single-float :initial-element 1.5) 'simple-vector) #(1.5 1.5))
(let ((g (vector 1 2 3)))
  (check "simple-vector -> simple-vector is itself" (eq g (coerce g 'simple-vector)) t))

;; the case that mattered: big input, many coerces, no copies
(let ((big (make-array 2000000 :element-type '(unsigned-byte 8))))
  (check "2 MB x 1000 coerces, all EQ"
         (loop repeat 1000 always (eq big (coerce big '(simple-array (unsigned-byte 8) (*)))))
         t))

(format t "COERCE-OCTETS ~a ~d / ~d~%" (if (zerop *fail*) "PASS" "FAIL") *pass* (+ *pass* *fail*))
