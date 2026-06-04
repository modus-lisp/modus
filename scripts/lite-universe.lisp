;;;; lite-universe.lisp — slim replacement for the ANSI suite's universe.lsp.
;;;;
;;;; The official universe.lsp at .../auxiliary/ansi_aux/universe.lsp
;;;; includes very long bignum-ratio literals that crash the Modus
;;;; reader.  Most ANSI test files use `(loop for x in *universe* …)'
;;;; or `(loop for x in *numbers* …)' patterns — they don't care how
;;;; BIG the universe is, just that it contains a representative slice
;;;; of CL value shapes.
;;;;
;;;; Load this BEFORE the test files instead of the upstream
;;;; universe.lsp.  The tests run; the per-element coverage is just
;;;; narrower than what the upstream test suite would exercise.

(defparameter *standard-package-names*
  '("COMMON-LISP" "COMMON-LISP-USER" "KEYWORD"))

(defparameter *package-objects*
  (loop for n in *standard-package-names*
        when (find-package n) collect (find-package n)))

(defparameter *integers*
  (list 0 1 -1 2 -2 3 -3 7 -7 17 100 -100 1000 -1000
        12387131 -12387131 999999 -999999
        most-positive-fixnum most-negative-fixnum))

(defparameter *floats*
  (list 0.0 1.0 -1.0 0.5 -0.5 3.14 -3.14 1000.0 -1000.0
        (if (boundp 'pi) pi 3.141592653589793d0)))

(defparameter *ratios*
  (list 1/2 -1/2 1/3 -1/3 2/3 -2/3 1/10 22/7 -22/7))

(defparameter *reals*
  (append *integers* *floats* *ratios*))

(defparameter *rationals*
  (append *integers* *ratios*))

(defparameter *numbers*
  (append *integers* *floats* *ratios*))

(defparameter *characters*
  (list #\a #\A #\0 #\9 #\+ #\- #\* #\/ #\.))

(defparameter *strings*
  (list "" "a" "abc" "ABC" "Hello, World!" " " "   "
        "the quick brown fox"))

(defparameter *conses*
  (list (cons 1 2) (cons 'a 'b) (cons nil nil) (cons t t)
        (list 1 2 3) (list 'a 'b 'c) (cons 'x (cons 'y 'z))
        (cons (cons 1 2) (cons 3 4))))

(defparameter *booleans*
  '(nil t))

(defparameter *keywords*
  '(:a :b :foo :bar :test :name :end))

(defparameter *symbols*
  '(nil t a b c d e f x y z foo bar baz quux car cdr cons))

(defparameter *uninterned-symbols*
  (list (make-symbol "FOO") (make-symbol "BAR")))

(defparameter *arrays*
  (list (make-array 0)
        (make-array 3 :initial-contents '(1 2 3))
        (make-array 5 :initial-element 0)
        #(1 2 3) #(a b c) #()))

(defparameter *hash-tables* nil)

(defparameter *streams* nil)

(defparameter *readtables* nil)

(defparameter *structures* nil)

(defparameter *functions*
  (list #'car #'cdr #'cons #'list #'identity))

(defparameter *universe*
  (append *symbols*
          *numbers*
          *characters*
          *strings*
          *conses*
          *arrays*
          *hash-tables*
          *functions*))
