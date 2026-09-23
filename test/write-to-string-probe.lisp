;;; wts-probe.lisp -- WRITE-TO-STRING and the WRITE family, with and without
;;; keyword arguments, and under specials bound by the caller.  The no-args
;;; call is a separate code path; every line here must read the same on the
;;; pre-change binary, on this one, and (but for the noted lines) on SBCL.
(defun p (l v) (format t "~a ~s~%" l v))
(defun safe (thunk) (handler-case (funcall thunk) (error (c) (declare (ignore c)) :ERROR)))

;; no keyword args -- the fast path
(p "int"     (write-to-string 123456))
(p "neg"     (write-to-string -7))
(p "str"     (write-to-string "hello"))
(p "sym"     (write-to-string 'foo))
(p "kw"      (write-to-string :foo))
(p "char"    (write-to-string #\a))
(p "list"    (write-to-string (list 1 "two" #\3 'four)))
(p "nil"     (write-to-string nil))
(p "t"       (write-to-string t))
(p "cons"    (write-to-string (cons 1 2)))
(p "vec"     (write-to-string (vector 1 2 3)))
(p "ratio"   (write-to-string 1/3))

;; the same objects with escaping off, via the specials the caller binds
(p "esc-nil-str" (let ((*print-escape* nil)) (write-to-string "hello")))
(p "esc-nil-chr" (let ((*print-escape* nil)) (write-to-string #\a)))
(p "princ-str"   (princ-to-string "hello"))
(p "princ-chr"   (princ-to-string #\a))
(p "princ-sym"   (princ-to-string 'foo))
(p "prin1-str"   (prin1-to-string "hello"))
(p "readably"    (let ((*print-readably* t)) (write-to-string "x")))
(p "readably-e"  (let ((*print-readably* t) (*print-escape* nil)) (write-to-string "x")))

;; caller-bound specials must still reach the printer through the fast path
(p "base2"   (let ((*print-base* 2))  (write-to-string 10)))
(p "base16"  (let ((*print-base* 16)) (write-to-string 255)))
(p "radix"   (let ((*print-base* 2) (*print-radix* t)) (write-to-string 5)))
(p "case-dn" (let ((*print-case* :downcase)) (write-to-string 'foo)))
(p "len"     (let ((*print-length* 2)) (write-to-string (list 1 2 3 4))))
(p "lvl"     (let ((*print-level* 1)) (write-to-string (list 1 (list 2 (list 3))))))

;; keyword args -- the general path
(p "k-base"   (write-to-string 10 :base 2))
(p "k-radix"  (write-to-string 5 :base 2 :radix t))
(p "k-esc"    (write-to-string "x" :escape nil))
(p "k-case"   (write-to-string 'foo :case :downcase))
(p "k-len"    (write-to-string (list 1 2 3 4) :length 2))
(p "k-lvl"    (write-to-string (list 1 (list 2 (list 3))) :level 1))
(p "k-dup"    (write-to-string 4 :base 10 :base 2))
(p "k-dup2"   (write-to-string 4 :base 2 :base 10))
(p "k-ignore" (write-to-string 7 :pretty t :right-margin 20))
(p "k-odd"    (safe (lambda () (write-to-string 1 :base))))
(p "k-bad"    (safe (lambda () (write-to-string 1 :no-such-key 3))))

;; the binding must not leak
(progn (write-to-string 10 :base 2)
       (p "no-leak-base" *print-base*)
       (write-to-string "x" :escape nil)
       (p "no-leak-esc" *print-escape*))

;; WRITE / PRIN1 / PRINC to a stream, no args and with args
(p "w-stream"   (with-output-to-string (s) (write "x" :stream s)))
(p "w-stream-e" (with-output-to-string (s) (write "x" :stream s :escape nil)))
(p "w-plain"    (with-output-to-string (s) (write 'foo :stream s)))
(p "prin1-strm" (with-output-to-string (s) (prin1 "x" s)))
(p "princ-strm" (with-output-to-string (s) (princ "x" s)))
(p "print-strm" (with-output-to-string (s) (print 'a s)))
(p "format-s"   (format nil "~s|~a" "x" "x"))
(p "done" t)
