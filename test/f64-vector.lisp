;;;; test/f64-vector.lisp -- packed double-float vectors (subtag #x18).
;;;;
;;;;   ./modus --script test/f64-vector.lisp        (prints F64-VECTOR PASS n / FAIL n)
;;;;   test/run-f64-vector.sh [MODUS]               (also runs it under SBCL and diffs)
;;;;
;;;; (simple-array double-float (*)) is a vector of 8-byte IEEE754 lanes with no pointers in
;;;; it: element type and upgrading say DOUBLE-FLOAT, AREF boxes, (SETF AREF) unboxes, and
;;;; every collector copies the payload as leaf data.  Every "=> " line is a question CL
;;;; answers the same way in SBCL; the runner diffs them.  The lines starting "#M" are
;;;; modus's own: the representation, the collector and the threads.  Doubles are shown as
;;;; their IEEE bits where printing could legitimately differ (a denormal's digits).

(setq *print-pretty* nil)
(defvar *pass* 0)
(defvar *fail* 0)

(defun bits (x)
  "(HI . LO), X's IEEE double bits, unsigned."
  #+sbcl (cons (logand (sb-kernel:double-float-high-bits x) #xFFFFFFFF)
               (sb-kernel:double-float-low-bits x))
  #+modus (cons (modus-int::%float-hi32 x) (modus-int::%float-lo32 x)))

(defun from-bits (hi lo)
  #+sbcl (sb-kernel:make-double-float (if (>= hi #x80000000) (- hi #x100000000) hi) lo)
  #+modus (modus-int::%make-typed-float hi lo 'double-float))

(defun err-kind (e)
  (cond ((typep e 'type-error) :type-error) (t :error)))

(defmacro show (name form)
  `(format t "~a => ~s~%" ,name
           (handler-case ,form (error (e) (list :error (err-kind e))))))

(defmacro check (name form expected)
  "A modus-only check: the line carries #M so the SBCL diff ignores it."
  `(let ((got (handler-case ,form (error (e) (list :error (err-kind e))))))
     (if (equalp got ,expected)
         (incf *pass*)
         (progn (incf *fail*)
                (format t "#M FAIL ~a~%#M   expected ~s~%#M   got      ~s~%" ,name ,expected got)))))

(defun lanes-bits (v) (map 'list #'bits v))

;;; ---- types and upgrading -------------------------------------------------
(show "upgraded double-float" (upgraded-array-element-type 'double-float))
(show "upgraded long-float" (upgraded-array-element-type 'long-float))
(show "upgraded (double-float 0d0 1d0)" (upgraded-array-element-type '(double-float 0d0 1d0)))
(show "upgraded (eql 1d0)" (upgraded-array-element-type '(eql 1d0)))
(show "upgraded single-float" (upgraded-array-element-type 'single-float))
(show "upgraded float" (upgraded-array-element-type 'float))
(show "upgraded real" (upgraded-array-element-type 'real))
(show "upgraded t" (upgraded-array-element-type t))

(let ((v (make-array 4 :element-type 'double-float)))
  (show "array-element-type" (array-element-type v))
  (show "type-of" (type-of v))
  (show "zero-filled" (coerce v 'list))
  (show "length" (length v))
  (show "typep (simple-array double-float (*))" (typep v '(simple-array double-float (*))))
  (show "typep (vector double-float 4)" (typep v '(vector double-float 4)))
  (show "typep (vector double-float 5)" (typep v '(vector double-float 5)))
  (show "typep (array long-float)" (typep v '(array long-float)))
  (show "typep (simple-array t (*))" (typep v '(simple-array t (*))))
  (show "typep (vector single-float)" (typep v '(vector single-float)))
  (show "typep simple-vector" (typep v 'simple-vector))
  (show "simple-vector-p" (simple-vector-p v))
  (show "vectorp arrayp stringp" (list (vectorp v) (arrayp v) (stringp v)))
  (show "array-rank / dims" (list (array-rank v) (array-dimensions v) (array-total-size v)))
  (show "adjustable / fill-pointer" (list (adjustable-array-p v) (array-has-fill-pointer-p v))))

(show "subtypep sa-df sa-t" (multiple-value-list (subtypep '(simple-array double-float (*)) '(simple-array t (*)))))
(show "subtypep sa-df vector" (multiple-value-list (subtypep '(simple-array double-float (*)) 'vector)))
(show "subtypep sa-df (vector long-float)" (multiple-value-list (subtypep '(simple-array double-float (*)) '(vector long-float))))

;;; ---- values: extremes, signed zero, infinities, NaN ------------------------
(let* ((inf (from-bits #x7FF00000 0))
       (ninf (from-bits #xFFF00000 0))
       (nan (from-bits #x7FF80000 0))
       (xs (list most-positive-double-float most-negative-double-float
                 least-positive-double-float least-negative-double-float
                 least-positive-normalized-double-float double-float-epsilon
                 -0d0 0d0 1d0 -1.5d0 pi inf ninf nan
                 (from-bits #x000FFFFF #xFFFFFFFF)))   ; the largest denormal
       (v (make-array (length xs) :element-type 'double-float)))
  (let ((i 0)) (dolist (x xs) (setf (aref v i) x) (incf i)))
  (show "extremes round-trip bits" (equal (lanes-bits v) (mapcar #'bits xs)))
  (show "extremes bits" (lanes-bits v))
  (show "-0d0 keeps its sign" (list (eql (aref v 6) -0d0) (eql (aref v 6) 0d0) (= (aref v 6) 0d0)))
  (show "eql of a read and the stored" (list (eql (aref v 8) 1d0) (eql (aref v 10) pi))))

;;; ---- element type is enforced ---------------------------------------------
(let ((v (make-array 3 :element-type 'double-float)))
  (show "store an integer" (setf (aref v 0) 1))
  (show "store a single-float" (setf (aref v 0) 1.0f0))
  (show "store a ratio" (setf (aref v 0) 1/2))
  (show "store a character" (setf (aref v 0) #\a))
  (show "store nil" (setf (aref v 0) nil))
  (show "store returns the value" (setf (aref v 1) 2.5d0))
  (show "still intact" (coerce v 'list))
  (show "aref past the end" (aref v 3))
  (show "aref -1" (aref v -1))
  (show "setf aref past the end" (setf (aref v 3) 1d0))
  (show "svref signals" (svref v 0))
  (show ":initial-element 1 signals" (make-array 2 :element-type 'double-float :initial-element 1))
  (show ":initial-element 7d0" (coerce (make-array 3 :element-type 'double-float :initial-element 7d0) 'list))
  (show ":initial-contents" (coerce (make-array 3 :element-type 'double-float :initial-contents '(1d0 -2d0 3.25d0)) 'list))
  (show ":initial-contents bad" (make-array 2 :element-type 'double-float :initial-contents '(1d0 2))))

;;; ---- the declared fast paths ------------------------------------------------
(defun d-get (v i)
  (declare (type (simple-array double-float (*)) v))
  (aref v i))
(defun d-set (v i x)
  (declare (type (simple-array double-float (*)) v))
  (setf (aref v i) x))
(defun d-get0 (v i)
  (declare (type (simple-array double-float (*)) v) (optimize (safety 0)))
  (aref v i))
(defun d-set0 (v i x)
  (declare (type (simple-array double-float (*)) v) (type double-float x) (optimize (safety 0)))
  (setf (aref v i) x))
(defun d-dot (a b)
  (declare (type (simple-array double-float (*)) a b))
  (let ((s 0d0)) (declare (type double-float s))
    (dotimes (i (length a) s) (setq s (+ s (* (aref a i) (aref b i)))))))
(defun d-axpy (k x y)
  (declare (type (simple-array double-float (*)) x y) (type double-float k))
  (dotimes (i (length x) y) (setf (aref y i) (+ (* k (aref x i)) (aref y i)))))
(defun d-axpy0 (k x y)
  (declare (type (simple-array double-float (*)) x y) (type double-float k) (optimize (safety 0)))
  (dotimes (i (length x) y) (setf (aref y i) (+ (* k (aref x i)) (aref y i)))))
(defun d-tree-value (v i)
  "A tree stored AND used: the store's value is the double."
  (declare (type (simple-array double-float (*)) v))
  (list (setf (aref v i) (* 2d0 (aref v i))) (aref v i)))

(let ((a (make-array 5 :element-type 'double-float :initial-contents '(1d0 2d0 3d0 4d0 5d0)))
      (b (make-array 5 :element-type 'double-float :initial-element 0.5d0)))
  ;; enough calls that a hot-JIT runtime has compiled them natively too
  (dotimes (k 100) (d-get a 1) (d-set b 0 0.5d0) (d-dot a b) (d-get0 a 0) (d-set0 b 4 0.5d0))
  (show "declared get" (list (d-get a 0) (d-get a 4)))
  (show "declared set" (list (d-set a 1 -7.25d0) (coerce a 'list)))
  (show "declared set int signals" (d-set a 1 3))
  (show "declared set single signals" (d-set a 1 3.0f0))
  (show "declared get past end" (d-get a 5))
  (show "declared get -1" (d-get a -1))
  (show "declared set past end" (d-set a 5 1d0))
  (show "safety 0 get/set" (list (d-set0 b 2 9.5d0) (d-get0 b 2)))
  (show "dot" (d-dot a b))
  (show "axpy" (coerce (d-axpy 2d0 a (copy-seq b)) 'list))
  (show "axpy safety 0" (coerce (d-axpy0 2d0 a (copy-seq b)) 'list))
  (show "tree store value" (d-tree-value (copy-seq a) 2))
  (show "axpy past a short y" (d-axpy 1d0 a (make-array 3 :element-type 'double-float))))

;;; ---- sequence functions ------------------------------------------------------
(let ((v (make-array 6 :element-type 'double-float :initial-contents '(10d0 20d0 30d0 40d0 50d0 60d0))))
  (show "subseq" (let ((s (subseq v 1 4))) (list (type-of s) (coerce s 'list))))
  (show "copy-seq" (let ((s (copy-seq v))) (list (type-of s) (equalp s v) (eq s v))))
  (show "reverse" (let ((s (reverse v))) (list (type-of s) (coerce s 'list))))
  (show "nreverse" (coerce (nreverse (copy-seq v)) 'list))
  (show "fill" (coerce (fill (copy-seq v) 9d0 :start 2 :end 4) 'list))
  (show "fill whole" (coerce (fill (copy-seq v) -1d0) 'list))
  (show "fill with an integer signals" (fill (copy-seq v) 9))
  (show "replace from a list" (coerce (replace (copy-seq v) '(1d0 2d0) :start1 4) 'list))
  (show "replace overlap up" (coerce (let ((w (copy-seq v))) (replace w w :start1 1)) 'list))
  (show "replace overlap down" (coerce (let ((w (copy-seq v))) (replace w w :start2 2)) 'list))
  (show "replace into general" (coerce (replace (make-array 3) v :start2 3) 'list))
  (show "replace from general" (coerce (replace (copy-seq v) (vector 1d0 2d0)) 'list))
  (show "map list" (map 'list #'1+ v))
  (show "map (vector double-float)" (let ((s (map '(vector double-float) #'- v))) (list (type-of s) (coerce s 'list))))
  (show "map-into" (coerce (map-into (copy-seq v) #'* v v) 'list))
  (show "reduce" (reduce #'+ v))
  (show "position / find / count" (list (position 40d0 v) (find 50d0 v) (count 30d0 v) (position 40 v)))
  (show "find-if / remove-if" (list (find-if (lambda (x) (> x 25d0)) v)
                                    (coerce (remove-if (lambda (x) (> x 25d0)) v) 'list)))
  (show "remove keeps the type" (type-of (remove 30d0 v)))
  (show "sort" (coerce (sort (copy-seq v) #'>) 'list))
  (show "stable-sort" (coerce (stable-sort (copy-seq v) #'> ) 'list))
  (show "concatenate" (let ((s (concatenate '(vector double-float) v '(1d0)))) (list (type-of s) (coerce s 'list))))
  (show "concatenate into list" (concatenate 'list v '(1d0)))
  (show "coerce list -> (vector double-float)" (let ((s (coerce '(1d0 2d0) '(vector double-float)))) (list (type-of s) (coerce s 'list))))
  (show "coerce -> simple-vector" (let ((s (coerce v 'simple-vector))) (list (type-of s) (aref s 0))))
  (show "make-sequence" (let ((s (make-sequence '(vector double-float) 3 :initial-element 5d0)))
                          (list (type-of s) (coerce s 'list))))
  (show "make-sequence (simple-array double-float (*))" (type-of (make-sequence '(simple-array double-float (*)) 2)))
  (show "make-sequence wrong length" (make-sequence '(vector double-float 4) 3))
  (show "equalp general vector" (equalp v (vector 10d0 20d0 30d0 40d0 50d0 60d0)))
  (show "equalp integers" (equalp v (vector 10 20 30 40 50 60)))
  (show "equal (not eq)" (equal v (copy-seq v)))
  (show "mismatch / search" (list (mismatch v (vector 10d0 20d0 31d0)) (search (vector 30d0 40d0) v)))
  (show "every / some" (list (every #'plusp v) (some #'minusp v)))
  (show "substitute" (coerce (substitute 0d0 30d0 v) 'list))
  (show "remove-duplicates" (coerce (remove-duplicates (concatenate '(vector double-float) v v)) 'list))
  (show "elt / setf elt" (let ((w (copy-seq v))) (setf (elt w 2) 3d0) (list (elt w 2) (elt w 0)))))

;;; ---- printing and reading ------------------------------------------------------
(let ((v (make-array 3 :element-type 'double-float :initial-contents '(1d0 2.5d0 -0d0))))
  (show "prin1-to-string" (prin1-to-string v))
  (show "format ~a" (format nil "~a" v))
  (show "print / read back equalp" (equalp (read-from-string (prin1-to-string v)) v))
  (show "empty" (prin1-to-string (make-array 0 :element-type 'double-float))))

;;; ---- displaced, adjustable, fill pointer --------------------------------------
(let ((v (make-array 6 :element-type 'double-float :initial-contents '(1d0 2d0 3d0 4d0 5d0 6d0))))
  (let ((d (make-array 3 :element-type 'double-float :displaced-to v :displaced-index-offset 2)))
    (show "displaced reads" (coerce d 'list))
    (setf (aref d 1) 99d0)
    (show "displaced write lands in the target" (list (aref v 3) (aref d 1)))
    (show "displaced element type" (array-element-type d))
    (show "displaced store signals" (setf (aref d 0) 1))
    (show "array-displacement" (multiple-value-bind (to off) (array-displacement d) (list (eq to v) off))))
  (show "adjust-array a simple one" (let ((a (adjust-array v 8)))
                                      (list (array-element-type a) (length a) (subseq (coerce a 'list) 0 6)))))
(let ((a (make-array 2 :element-type 'double-float :adjustable t :fill-pointer 0)))
  (vector-push-extend 1d0 a) (vector-push-extend -2d0 a) (vector-push-extend 3d0 a)
  (show "vector-push-extend" (list (coerce a 'list) (array-element-type a) (fill-pointer a)))
  (show "vector-pop" (list (vector-pop a) (fill-pointer a)))
  (vector-push-extend 3d0 a)
  (show "vector-push-extend int signals" (vector-push-extend 4 a))
  (show "adjust-array grows" (let ((b (adjust-array a 10 :fill-pointer 5)))
                               (list (length b) (aref b 0) (aref b 1) (array-element-type b))))
  (show "fill-pointer vector typep" (list (typep a '(vector double-float)) (typep a '(simple-array double-float (*))))))

;;; ---- multi-dimensional --------------------------------------------------------
(defun m-get (m i j)
  (declare (type (simple-array double-float (3 3)) m))
  (aref m i j))
(let ((m (make-array '(3 3) :element-type 'double-float)))
  (dotimes (i 3) (dotimes (j 3) (setf (aref m i j) (+ (* 10d0 i) j))))
  (show "2-D element type / type-of" (list (array-element-type m) (type-of m)))
  (show "2-D aref" (list (aref m 0 0) (aref m 1 2) (aref m 2 1)))
  (show "2-D declared aref" (list (m-get m 2 2) (m-get m 1 0)))
  (show "2-D row-major-aref" (list (row-major-aref m 5) (array-row-major-index m 1 2)))
  (show "2-D store an integer signals" (setf (aref m 0 0) 1))
  (show "2-D typep" (list (typep m '(simple-array double-float (3 3))) (typep m '(array t (3 3)))))
  (show "2-D print" (prin1-to-string m))
  (show "2-D :initial-contents" (aref (make-array '(2 2) :element-type 'double-float
                                                  :initial-contents '((1d0 2d0) (3d0 4d0)))
                                      1 0))
  (show "2-D displaced to 1-D" (coerce (make-array 9 :element-type 'double-float :displaced-to m) 'list))
  (show "2-D adjust" (let ((n (adjust-array m '(2 4) :initial-element 0d0)))
                       (list (array-dimensions n) (aref n 1 1) (aref n 1 3)))))

;;; ---- modus: the representation -------------------------------------------------
#+modus
(progn
  (defun packed-p (v) (eql (modus-int::obj-subtag v) #x18))
  (check "packed" (packed-p (make-array 3 :element-type 'double-float)) t)
  (check "long-float packs too" (packed-p (make-array 3 :element-type 'long-float)) t)
  (check "single does not" (packed-p (make-array 3 :element-type 'single-float)) nil)
  (check "subseq is packed" (packed-p (subseq (make-array 5 :element-type 'double-float) 1 3)) t)
  (check "copy-seq is packed" (packed-p (copy-seq (make-array 5 :element-type 'double-float))) t)
  (check "make-sequence is packed" (packed-p (make-sequence '(vector double-float) 3)) t)
  (check "coerce is packed" (packed-p (coerce '(1d0 2d0) '(vector double-float))) t)
  (check "map is packed" (packed-p (map '(vector double-float) #'identity '(1d0))) t)
  (check "a 2-D store is packed"
         (packed-p (modus-int::%mda-data (make-array '(2 2) :element-type 'double-float))) t)
  (check "an adjustable store is packed"
         (packed-p (modus-int::%mda-data (make-array 4 :element-type 'double-float :adjustable t))) t)
  (check "8 bytes a lane, no header word per element"
         (let ((v (make-array 1000 :element-type 'double-float)))
           (modus-int::%prim-array-length v))
         1000)
  (check "make-static-vector double-float"
         (let ((v (modus:make-static-vector 4 :element-type 'double-float)))
           (setf (aref v 3) 2.5d0)
           (prog1 (list (packed-p v) (aref v 3) (aref v 0)) (modus:free-static-vector v)))
         '(t 2.5d0 0d0)))

;;; ---- modus: the collector --------------------------------------------------------
;;; Many packed vectors among garbage, collected several times, their lanes intact bit for
;;; bit.  The lanes are chosen to look like pointers: low nibbles 1 and 9 (the cons and
;;; object tags) and high bits that put them inside the heap's address range, so a
;;; collector that scanned the payload would forward them and change the bits.
#+modus
(progn
  (defun gc-count () (modus-int::%gc-epoch))
  (defun pointerish-pattern (n k)
    (let ((v (make-array n :element-type 'double-float))
          (base (logand (modus-int::%val->word (cons k k)) -16)))
      (dotimes (i n v)
        (let ((w (+ base (* 16 (mod (* i 7) 4096)) (if (evenp i) 1 9))))
          (setf (aref v i) (from-bits (ldb (byte 32 32) w) (ldb (byte 32 0) w)))))))
  (defun pattern-bits (v) (map 'list #'bits v))
  (let* ((vs (loop for k below 40 collect (pointerish-pattern (+ 50 k) k)))
         (want (mapcar #'pattern-bits vs))
         (c0 (gc-count)))
    (dotimes (round 6)
      ;; garbage, then a collection
      (dotimes (j 20000) (make-list 8))
      (modus-int::%gc-collect-here)
      (dotimes (j 1000) (make-array 16 :element-type 'double-float)))
    (check "collections happened" (> (gc-count) c0) t)
    (check "40 packed vectors intact across collections" (equal (mapcar #'pattern-bits vs) want) t)
    (check "still packed after moving" (every #'packed-p vs) t)))

;;; ---- modus: a worker stores doubles it computed into a main-made vector ----------
;;; The element is raw bits, not a pointer to the worker's own box, so the shared-store
;;; guard has nothing to refuse.
#+modus
(let* ((n 1000)
       (v (make-array n :element-type 'double-float))
       (m (make-array '(10 10) :element-type 'double-float))
       (th (sb-thread:make-thread
            (lambda ()
              (dotimes (i n) (setf (aref v i) (/ (float (* i i) 1d0) 7d0)))
              (dotimes (i 10) (dotimes (j 10) (setf (aref m i j) (sqrt (float (+ i j) 1d0)))))
              :done))))
  (check "worker finished" (sb-thread:join-thread th) :done)
  (check "worker's doubles read back exact"
         (let ((ok t)) (dotimes (i n ok) (unless (= (aref v i) (/ (float (* i i) 1d0) 7d0)) (setq ok nil))))
         t)
  (check "worker's 2-D doubles read back exact"
         (let ((ok t)) (dotimes (i 10 ok) (dotimes (j 10) (unless (= (aref m i j) (sqrt (float (+ i j) 1d0))) (setq ok nil)))))
         t))

#+modus (format t "#M F64-VECTOR ~a ~d / FAIL ~d~%" (if (zerop *fail*) "PASS" "FAIL") *pass* *fail*)
#+modus (modus-int::sys-exit (if (zerop *fail*) 0 1))
