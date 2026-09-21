;;;; print-specials-probe.lisp — the probe that gates hoisting a printer or
;;;; reader special variable out of a loop.
;;;;
;;;; Run it as `modus --load test/print-specials-probe.lisp --quit' and diff
;;;; against `sbcl --script' on the same file.  For the JIT-off arm prepend
;;;; `(setq *use-jit* nil)' — MODUS_NO_JIT=1 does not work on the aarch64 CLI.
;;;;
;;;; ITS ORACLE IS SBCL.  What can go wrong when a special is hoisted is not
;;;; slowness, it is a value that is one dynamic scope out of date, and no
;;;; Modus-internal comparison can see that: both the JIT and the interpreter
;;;; run the same bytecode, so they would agree on the same wrong answer.
;;;;
;;;; WHAT IT COVERS, and why these shapes rather than arbitrary printer tests:
;;;; each one puts a rebinding at a point a hoist would step over.  Section A
;;;; is the round trip (printer and reader composed, so a case or escape or
;;;; radix decision that is silently dropped shows up as a value that does not
;;;; come back).  Section B rebinds a printer variable BETWEEN two prints, and
;;;; between the elements of one list print, for every variable %write-obj
;;;; reads: *print-base*, *print-radix*, *print-case*, *print-escape*,
;;;; *print-readably*, *print-length*, *print-level*, *print-array*,
;;;; *print-gensym*, plus *readtable* (readtable-case, which the symbol
;;;; printer folds through) and *package* (the qualifier decision).  Section C
;;;; does the same to the reader: *read-base* changed between reads, a reader
;;;; macro that rebinds *read-base* for the rest of ONE form while the forms
;;;; around it stay decimal, and a reader macro installed in a non-standard
;;;; readtable that must not leak into the next read.
;;;;
;;;; DELIBERATELY NOT HERE, because Modus and SBCL differ for reasons that
;;;; have nothing to do with specials and a diff must stay clean:
;;;;   - (prin1-to-string ''x): SBCL abbreviates to 'X, Modus prints (QUOTE X).
;;;;   - an array under *print-array* NIL: SBCL prints the type and address,
;;;;     Modus prints #<Array>.
;;;;   - a package name that needs escaping: SBCL prints |PSC|::|ZED|, Modus
;;;;     prints PSC::|ZED| (the package name is emitted unescaped).
;;;; All three are open, all three are independent of this probe's subject.

(setq *print-pretty* nil)   ; SBCL's script mode defaults it T and line-wraps

(defun ps-setcase (rt v)
  #+sbcl (setf (readtable-case rt) v)
  #-sbcl (%set-readtable-case rt v))

;;; ---- A. round trip: (equal x (read-from-string (prin1-to-string x))) ----

(defvar *ps-corpus*
  (list 1 -1 0 42 -99999 1234567890123456789
        (* 123456789123456789 987654321987654321)
        1/3 -22/7 355/113
        1.0 -2.5 3.14159 1.0e10 -1.0e-10 0.0
        #\a #\Z #\Space #\Newline #\( #\\ #\| #\0
        "" "abc" "a\"b" "a\\b" "  spaces  " "|bar|"
        'foo 'cl:car :key :|weird key| :a
        '|lower case sym| '|| '|123|
        nil t '(nil) '(t . nil)
        '(1 2 3) '(1 . 2) '((1 2) (3 4)) '(a (b (c (d))))
        '(1 "two" #\3 :four 5/6)
        '(a . (b . (c . nil)))
        #(1 2 3) #(a "b" #\c) #()
        '(#(1 2) #(3 4))
        (list (list 1 2) 3 (list 4 (list 5)))))

(defun ps-roundtrip ()
  (let ((bad nil) (n 0))
    (dolist (x *ps-corpus*)
      (setq n (+ n 1))
      (let* ((s (prin1-to-string x))
             (r (handler-case (read-from-string s)
                  (error (e) (list :err (princ-to-string e))))))
        ;; EQUAL does not descend vectors (CLHS 5.3), so a vector that
        ;; round-trips perfectly is still not EQUAL to itself.  Accept the
        ;; printed form matching as well, which is the property being tested.
        (unless (or (equal x r)
                    (and (not (consp r))
                         (string= s (handler-case (prin1-to-string r)
                                      (error () "")))))
          (push s bad))))
    (list n (nreverse bad))))

;;; ---- B. a printer variable rebound where a hoist would step over it ----

;; *print-base* / *print-radix*: outer decimal, one print in the middle at 16.
(defun ps-b1 ()
  (with-output-to-string (s)
    (let ((*print-base* 10))
      (write 255 :stream s) (write-char #\Space s)
      (let ((*print-base* 16)) (write 255 :stream s))
      (write-char #\Space s) (write 255 :stream s)
      (write-char #\Space s)
      (let ((*print-radix* t) (*print-base* 2)) (write 10 :stream s))
      (write-char #\Space s)
      (let ((*print-radix* t)) (write 10 :stream s))
      (write-char #\Space s)
      (let ((*print-base* 8)) (write 8/9 :stream s)))))

;; *print-case*: the symbol printer's own hoist.
(defun ps-b2 ()
  (with-output-to-string (s)
    (let ((*print-case* :upcase))
      (princ (prin1-to-string 'foo) s) (write-char #\/ s)
      (let ((*print-case* :downcase)) (princ (prin1-to-string 'foo) s))
      (write-char #\/ s)
      (let ((*print-case* :capitalize)) (princ (prin1-to-string 'foo) s))
      (write-char #\/ s)
      (princ (prin1-to-string 'foo) s)
      (write-char #\/ s)
      (let ((*print-case* :downcase)) (princ (prin1-to-string nil) s))
      (write-char #\/ s)
      (let ((*print-case* :capitalize)) (princ (prin1-to-string nil) s))
      (write-char #\/ s)
      (let ((*print-case* :downcase)) (princ (prin1-to-string t) s)))))

;; *print-escape* toggled between the parts of one output.
(defun ps-b3 ()
  (with-output-to-string (s)
    (let ((*print-escape* t))
      (write "ab" :stream s)
      (let ((*print-escape* nil)) (write "ab" :stream s))
      (write "ab" :stream s)
      (write #\a :stream s)
      (let ((*print-escape* nil)) (write #\a :stream s))
      (write #\a :stream s))))

;; *print-length* / *print-level*: the budget belongs to the enclosing object.
(defun ps-b4 ()
  (with-output-to-string (s)
    (let ((*print-length* 2))
      (write '(1 2 3 4 5) :stream s) (write-char #\Space s)
      (let ((*print-length* nil)) (write '(1 2 3 4 5) :stream s))
      (write-char #\Space s)
      (write '((1 2 3) (4 5 6)) :stream s)
      (write-char #\Space s)
      (let ((*print-length* 0)) (write '(1 2 3) :stream s))
      (write-char #\Space s)
      (write #(1 2 3 4 5) :stream s))
    (let ((*print-level* 2))
      (write-char #\Space s)
      (write '(1 (2 (3 (4)))) :stream s)
      (write-char #\Space s)
      (let ((*print-level* nil)) (write '(1 (2 (3 (4)))) :stream s)))))

;; *print-array* / *print-readably* / *print-gensym*.
(defun ps-b5 ()
  (with-output-to-string (s)
    (let ((*print-array* t)) (write #(1 2 3) :stream s))
    (write-char #\Space s)
    (let ((*print-escape* nil) (*print-readably* t)) (write #\Space :stream s))
    (write-char #\Space s)
    (let ((*print-escape* t) (*print-readably* nil)) (write #\Space :stream s))
    (write-char #\Space s)
    (let ((*print-escape* t) (*print-gensym* t)) (write (make-symbol "GEE") :stream s))
    (write-char #\Space s)
    (let ((*print-escape* t) (*print-gensym* nil)) (write (make-symbol "GEE") :stream s))))

;; *readtable* (readtable-case) changed between prints AND between the
;; elements of one list print.
(defun ps-b6 ()
  (let ((rt (copy-readtable nil)))
    (with-output-to-string (s)
      (let ((*readtable* rt))
        (write 'foo :stream s) (write-char #\Space s)
        (ps-setcase rt :downcase) (write 'foo :stream s) (write-char #\Space s)
        (ps-setcase rt :preserve) (write 'foo :stream s) (write-char #\Space s)
        (ps-setcase rt :invert)   (write 'foo :stream s) (write-char #\Space s)
        (ps-setcase rt :upcase)
        (let ((*print-case* :downcase)) (write 'foo :stream s))))))

(defun ps-b7 ()
  (let ((up (copy-readtable nil)) (dn (copy-readtable nil)))
    (ps-setcase dn :downcase)
    (with-output-to-string (s)
      (let ((*readtable* up)) (write '(alpha) :stream s))
      (write-char #\Space s)
      (let ((*readtable* dn)) (write '(alpha) :stream s))
      (write-char #\Space s)
      (let ((*readtable* up)) (write '(alpha) :stream s)))))

;; *package*: the qualifier decision, with the current package changed between
;; three prints of the SAME symbol.
(defun ps-b8 ()
  (let ((p1 (or (find-package "PSA") (make-package "PSA" :use '())))
        (p2 (or (find-package "PSB") (make-package "PSB" :use '()))))
    (let ((sa (intern "ZED" p1)))
      (with-output-to-string (s)
        (let ((*package* p1)) (write sa :stream s))
        (write-char #\Space s)
        (let ((*package* p2)) (write sa :stream s))
        (write-char #\Space s)
        (let ((*package* p1)) (write sa :stream s))
        (write-char #\Space s)
        (let ((*package* p2) (*print-escape* nil)) (write sa :stream s))))))

;;; ---- C. a reader variable rebound where a hoist would step over it ----

(defun ps-c1 ()
  (list (let ((*read-base* 16)) (read-from-string "ff"))
        (let ((*read-base* 10)) (read-from-string "ff"))
        (let ((*read-base* 16)) (read-from-string "(ff 10)"))
        (let ((*read-base* 10)) (read-from-string "(10 10)"))))

;; A reader macro is user code: it may rebind *read-base* for the form it
;; reads while the forms on either side of it stay decimal.  A readtable
;; hoisted across the dispatch would read the wrong table for the rest.
(defun ps-c2 ()
  (let ((rt (copy-readtable nil)))
    (set-macro-character #\! (lambda (st ch) (declare (ignore ch))
                               (let ((*read-base* 16)) (read st t nil t)))
                         nil rt)
    (let ((*readtable* rt) (*read-base* 10))
      (read-from-string "(10 !10 10 !ff)"))))

;; A macro character in a non-standard readtable must not survive the exit
;; of the binding that installed it.
(defun ps-c3 ()
  (let ((rt (copy-readtable nil)))
    (set-macro-character #\% (lambda (st ch) (declare (ignore st ch)) :pct) nil rt)
    (list (let ((*readtable* rt)) (read-from-string "(a %)"))
          (read-from-string "(a b)"))))

;;; ---- report ----

(dolist (f '(ps-roundtrip ps-b1 ps-b2 ps-b3 ps-b4 ps-b5 ps-b6 ps-b7 ps-b8
             ps-c1 ps-c2 ps-c3))
  (format t "~a => ~s~%"
          f (handler-case (funcall f) (error (e) (list :err (princ-to-string e))))))
(format t "DONE~%")
