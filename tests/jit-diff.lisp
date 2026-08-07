;;;; jit-diff.lisp — the JIT-vs-interpret differential oracle.
;;;;
;;;; Modus has THREE execution paths and, until this file, only two had
;;;; coverage:
;;;;
;;;;   1. build-time native   translate-*.lisp output baked into the image.
;;;;                          The ANSI corpus test BODIES run this way.
;;;;   2. runtime interpret   mvm-eval -> mvm-interpret.  The default for
;;;;                          runtime EVAL/LOAD; what REPL probes exercise.
;;;;   3. runtime JIT         mvm-eval -> translate -> exec page -> native call.
;;;;                          Covered by NOTHING.
;;;;
;;;; This file covers path 3 against path 2 with the SAME image, the SAME
;;;; process and the SAME forms: each form is EVAL'd twice, once with the JIT
;;;; seam dynamically inhibited (*jit-inhibit* = T -> path 2) and once with it
;;;; live (path 3).  Any difference in the returned VALUES, in the number of
;;;; values, or in whether the form signalled, is a bug in one of the two
;;;; paths.  Two identical answers are not proof the JIT ran, so the harness
;;;; ALSO reads the seam's own counters (*jit-native-count*) around each form
;;;; and reports how many forms actually reached native code -- an "all clear"
;;;; from a run where nothing was JIT'd is not a measurement.
;;;;
;;;; Run:  ./modus --load tests/jit-diff.lisp --quit
;;;; (Requires a JIT-capable image: the shipping x64/aarch64 CLI, which has
;;;;  had the runtime JIT on by default since WS5 #206/#207.  On a JIT-off
;;;;  image every form reports NATIVE=0 and the harness says so.)
;;;;
;;;; Forms must be SIDE-EFFECT-TOLERANT: they are evaluated twice.  Forms that
;;;; deliberately mutate are in the DOUBLE-EXECUTION section, which measures
;;;; the doubling instead of assuming it away.

(defparameter *jd-total* 0)
(defparameter *jd-diverge* 0)
(defparameter *jd-native* 0)
(defparameter *jd-both-err* 0)

(defun jd-count () (if (boundp '*jit-native-count*)
                       (or *jit-native-count* 0)
                       0))

(defun jd-run (form jitp)
  "EVAL FORM with the JIT seam either live (JITP) or inhibited.  Returns
   (VALUES result-list native-delta), where result-list is the full multiple
   value list, or (:SIGNALLED <type-name>) if the form signalled."
  (let ((before (jd-count)))
    (setq *jit-inhibit* (not jitp))
    (let ((r (handler-case (multiple-value-list (eval form))
               (t (c) (list :signalled
                            (handler-case (type-of c) (t (c2) :unknown)))))))
      (setq *jit-inhibit* nil)
      (values r (- (jd-count) before)))))

(defparameter *jd-never-native* nil)

(defun jd-str (x)
  "Printed structure of X.  The oracle compares PRINTED forms, not EQUAL:
   two evaluations of `(make-array 3)` return two distinct vectors and CL's
   EQUAL is identity on arrays, so an EQUAL oracle reports every
   fresh-aggregate-returning form as a divergence.  Printing keeps the
   distinctions that matter (1 vs 1.0d0, #\\a vs #\\A, \"ab\" vs \"AB\") while
   ignoring the identity of a freshly consed result."
  (handler-case (prin1-to-string x) (t (c) "<unprintable>")))

(defun jd-check (label form)
  (setq *jd-total* (+ 1 *jd-total*))
  (multiple-value-bind (iv idelta) (jd-run form nil)
    (declare (ignore idelta))
    (multiple-value-bind (jv jdelta) (jd-run form t)
      (if (> jdelta 0)
          (setq *jd-native* (+ 1 *jd-native*))
          (setq *jd-never-native* (cons label *jd-never-native*)))
      (when (and (consp iv) (eq (car iv) :signalled)
                 (consp jv) (eq (car jv) :signalled))
        (setq *jd-both-err* (+ 1 *jd-both-err*)))
      (unless (or (equal iv jv) (string= (jd-str iv) (jd-str jv)))
        (setq *jd-diverge* (+ 1 *jd-diverge*))
        (format t "JIT-DIVERGE ~A~%  interp = ~S~%  jit    = ~S~%" label iv jv)))))

;;; ------------------------------------------------------------------
;;; The corpus.  Breadth over depth: every construct class the runtime
;;; compiler can emit, because turning the JIT on widens the blast radius of
;;; EVERY translator bug (task #221: six aarch64 ensure-src scratch clobbers,
;;; several with zero live sites in a shipping image -- latent exactly until
;;; the JIT starts pushing arbitrary runtime forms through those opcodes).
;;; ------------------------------------------------------------------

(defparameter *jd-forms*
  (list
   ;; --- integer arithmetic: every opcode the translator emits -------------
   (cons "add"          '(+ 1 2))
   (cons "add-neg"      '(+ -7 3))
   (cons "sub"          '(- 10 4))
   (cons "sub-neg"      '(- 4 10))
   (cons "mul"          '(* 6 7))
   (cons "mul-neg"      '(* -6 7))
   (cons "mul-big"      '(* 1000000 1000000))
   (cons "truncate"     '(truncate 17 5))
   (cons "truncate-neg" '(truncate -17 5))
   (cons "floor"        '(floor 17 5))
   (cons "floor-neg"    '(floor -17 5))
   (cons "ceiling"      '(ceiling 17 5))
   (cons "round"        '(round 17 5))
   (cons "mod"          '(mod 1 64))          ; <- the #220 aarch64 shape
   (cons "mod-neg"      '(mod -1 64))
   (cons "mod-var"      '(let ((a 17) (b 5)) (mod a b)))
   (cons "rem"          '(rem 17 5))
   (cons "rem-neg"      '(rem -17 5))
   (cons "abs"          '(abs -42))
   (cons "min"          '(min 3 1 2))
   (cons "max"          '(max 3 1 2))
   (cons "gcd"          '(gcd 84 36))
   (cons "lcm"          '(lcm 4 6))
   (cons "isqrt"        '(isqrt 145))
   (cons "expt"         '(expt 2 10))
   (cons "expt-big"     '(expt 2 100))
   (cons "1+"           '(1+ 41))
   (cons "1-"           '(1- 43))
   (cons "incf-local"   '(let ((x 1)) (incf x 5) x))
   (cons "decf-local"   '(let ((x 10)) (decf x 3) x))
   (cons "fixnum-ovf+"  '(+ most-positive-fixnum 1))
   (cons "fixnum-ovf*"  '(* most-positive-fixnum 2))
   (cons "bignum-add"   '(+ (expt 2 100) (expt 2 100)))
   (cons "bignum-sub"   '(- (expt 2 100) 1))
   (cons "bignum-mul"   '(* (expt 2 60) (expt 2 60)))
   (cons "bignum-cmp"   '(< (expt 2 100) (expt 2 101)))
   (cons "ratio"        '(/ 1 3))
   (cons "ratio-add"    '(+ 1/3 1/6))
   (cons "ratio-norm"   '(/ 4 8))

   ;; --- bit operations ---------------------------------------------------
   (cons "logand"       '(logand 12 10))
   (cons "logior"       '(logior 12 10))
   (cons "logxor"       '(logxor 12 10))
   (cons "lognot"       '(lognot 12))
   (cons "logand-neg"   '(logand -1 255))
   (cons "ash-left"     '(ash 1 10))
   (cons "ash-right"    '(ash 1024 -10))
   (cons "ash-neg"      '(ash -1024 -3))
   (cons "ash-var"      '(let ((n 5)) (ash 1 n)))
   (cons "logbitp"      '(logbitp 3 8))
   (cons "logcount"     '(logcount 255))
   (cons "int-length"   '(integer-length 255))
   (cons "ldb"          '(ldb (byte 4 4) 255))

   ;; --- floats -----------------------------------------------------------
   (cons "float-add"    '(+ 1.5d0 2.25d0))
   (cons "float-sub"    '(- 1.5d0 2.25d0))
   (cons "float-mul"    '(* 1.5d0 2.0d0))
   (cons "float-div"    '(/ 1.0d0 4.0d0))
   (cons "float-cmp"    '(< 1.5d0 2.0d0))
   (cons "float-neg"    '(- 0.0d0))
   (cons "float-coerce" '(coerce 3 'double-float))
   (cons "float-trunc"  '(truncate 7.5d0))
   (cons "float-single" '(+ 1.5f0 2.5f0))
   (cons "float-mixed"  '(+ 1 2.5d0))
   (cons "sqrt"         '(sqrt 16.0d0))
   (cons "float-print"  '(prin1-to-string 0.1d0))

   ;; --- comparisons / predicates ----------------------------------------
   (cons "eq-sym"       '(eq 'foo 'foo))
   (cons "eql-char"     '(eql #\a #\a))
   (cons "equal-list"   '(equal (list 1 2) (list 1 2)))
   (cons "equalp-str"   '(equalp "ABC" "abc"))
   (cons "numeq"        '(= 3 3.0d0))
   (cons "zerop"        '(zerop 0))
   (cons "evenp"        '(evenp 4))
   (cons "null-nil"     '(null nil))
   (cons "consp"        '(consp (cons 1 2)))
   (cons "consp-nil"    '(consp nil))         ; <- the #221 ensure-src shape
   (cons "atom-nil"     '(atom nil))          ; <- the #221 ensure-src shape
   (cons "atom-cons"    '(atom (cons 1 2)))
   (cons "symbolp"      '(symbolp 'x))
   (cons "keywordp"     '(keywordp :x))
   (cons "numberp"      '(numberp 1/2))
   (cons "stringp"      '(stringp "s"))
   (cons "characterp"   '(characterp #\a))
   (cons "functionp"    '(functionp #'car))
   (cons "arrayp"       '(arrayp (make-array 3)))

   ;; --- characters -------------------------------------------------------
   (cons "char-code"    '(char-code #\A))
   (cons "code-char"    '(code-char 65))
   (cons "char-upcase"  '(char-upcase #\a))
   (cons "char<"        '(char< #\a #\b))
   (cons "alpha-char-p" '(alpha-char-p #\7))
   (cons "digit-char-p" '(digit-char-p #\7))

   ;; --- strings (the header comment claims these "always interpret") ------
   (cons "str-length"   '(length "hello"))
   (cons "str-aref"     '(aref "hello" 1))
   (cons "str-char"     '(char "hello" 0))
   (cons "str-subseq"   '(subseq "hello" 1 3))
   (cons "str-concat"   '(concatenate 'string "ab" "cd"))
   (cons "str-eq"       '(string= "abc" "abc"))
   (cons "str-lt"       '(string< "abc" "abd"))
   (cons "str-upcase"   '(string-upcase "abc"))
   (cons "str-reverse"  '(reverse "abc"))
   (cons "str-search"   '(search "ll" "hello"))
   (cons "str-position" '(position #\l "hello"))
   (cons "str-make"     '(make-string 3 :initial-element #\x))
   (cons "str-setf"     '(let ((s (make-string 3 :initial-element #\a)))
                          (setf (aref s 1) #\b) s))
   (cons "str-format"   '(format nil "~A-~D" 'x 5))
   (cons "str-symbol"   '(symbol-name 'foo))
   (cons "str-intern"   '(string (intern "JD-TEST-SYM")))

   ;; --- LENGTH across types (the other "always interpret" claim) ---------
   (cons "len-list"     '(length (list 1 2 3)))
   (cons "len-nil"      '(length nil))
   (cons "len-vector"   '(length (vector 1 2 3)))
   (cons "len-string"   '(length "abcd"))
   (cons "len-bitvec"   '(length (make-array 5 :element-type 'bit)))

   ;; --- lists ------------------------------------------------------------
   (cons "cons"         '(cons 1 2))
   (cons "car"          '(car (list 1 2 3)))
   (cons "cdr"          '(cdr (list 1 2 3)))
   (cons "cadr"         '(cadr (list 1 2 3)))
   (cons "cddr"         '(cddr (list 1 2 3)))
   (cons "nth"          '(nth 2 (list 1 2 3 4)))
   (cons "nthcdr"       '(nthcdr 2 (list 1 2 3 4)))
   (cons "last"         '(last (list 1 2 3)))
   (cons "append"       '(append (list 1 2) (list 3)))
   (cons "revlist"      '(reverse (list 1 2 3)))
   (cons "nreverse"     '(nreverse (list 1 2 3)))
   (cons "member"       '(member 2 (list 1 2 3)))
   (cons "assoc"        '(assoc 'b (list (cons 'a 1) (cons 'b 2))))
   (cons "list*"        '(list* 1 2 (list 3)))
   (cons "mapcar"       '(mapcar #'1+ (list 1 2 3)))
   (cons "mapcar-lam"   '(mapcar (lambda (x) (* x x)) (list 1 2 3)))
   (cons "mapcan"       '(mapcan (lambda (x) (list x x)) (list 1 2)))
   (cons "remove"       '(remove 2 (list 1 2 3 2)))
   (cons "remove-if"    '(remove-if #'evenp (list 1 2 3 4)))
   (cons "sort"         '(sort (list 3 1 2) #'<))
   (cons "reduce"       '(reduce #'+ (list 1 2 3 4)))
   (cons "reduce-init"  '(reduce #'+ (list 1 2 3) :initial-value 10))
   (cons "find"         '(find 3 (list 1 2 3)))
   (cons "count"        '(count 2 (list 1 2 2 3)))
   (cons "every"        '(every #'numberp (list 1 2)))
   (cons "some"         '(some #'stringp (list 1 "a")))
   (cons "push-pop"     '(let ((l nil)) (push 1 l) (push 2 l) (list (pop l) l)))
   (cons "setf-car"     '(let ((c (cons 1 2))) (setf (car c) 9) c))
   (cons "setf-nth"     '(let ((l (list 1 2 3))) (setf (nth 1 l) 9) l))
   (cons "dotted"       '(cons 1 (cons 2 3)))
   (cons "circular-safe" '(let ((l (list 1 2 3))) (nthcdr 5 l)))

   ;; --- arrays / vectors -------------------------------------------------
   (cons "make-array"   '(make-array 3 :initial-element 7))
   (cons "aref"         '(aref (vector 1 2 3) 1))
   (cons "aset"         '(let ((a (make-array 3 :initial-element 0)))
                          (setf (aref a 1) 5) a))
   (cons "aset-varidx"  '(let ((a (make-array 3 :initial-element 0)) (i 2))
                          (setf (aref a i) 5) a))
   (cons "vector-lit"   '(vector 'a 'b 'a))
   (cons "array-2d"     '(let ((a (make-array (list 2 2) :initial-element 0)))
                          (setf (aref a 1 1) 3) (aref a 1 1)))
   (cons "adjustable"   '(let ((a (make-array 0 :adjustable t :fill-pointer 0)))
                          (vector-push-extend 1 a) (vector-push-extend 2 a)
                          (list (length a) (aref a 1))))
   (cons "bitvec"       '(let ((b (make-array 4 :element-type 'bit
                                              :initial-element 0)))
                          (setf (aref b 2) 1) b))
   (cons "ub8-vec"      '(let ((a (make-array 4 :element-type '(unsigned-byte 8)
                                              :initial-element 0)))
                          (setf (aref a 1) 255) (aref a 1)))
   (cons "row-major"    '(row-major-aref (vector 1 2 3) 2))
   (cons "coerce-vec"   '(coerce (list 1 2 3) 'vector))
   (cons "coerce-list"  '(coerce (vector 1 2 3) 'list))

   ;; --- hash tables ------------------------------------------------------
   (cons "hash-eql"     '(let ((h (make-hash-table)))
                          (setf (gethash 1 h) 'one) (gethash 1 h)))
   (cons "hash-equal"   '(let ((h (make-hash-table :test 'equal)))
                          (setf (gethash "k" h) 5) (gethash "k" h)))
   (cons "hash-count"   '(let ((h (make-hash-table)))
                          (setf (gethash 1 h) 1) (setf (gethash 2 h) 2)
                          (hash-table-count h)))
   (cons "hash-rem"     '(let ((h (make-hash-table)))
                          (setf (gethash 1 h) 1) (remhash 1 h)
                          (hash-table-count h)))
   (cons "hash-miss"    '(let ((h (make-hash-table))) (gethash 9 h)))

   ;; --- symbols / packages ----------------------------------------------
   (cons "quote-sym"    ''foo)
   (cons "keyword"      ':bar)
   (cons "gensym-p"     '(symbolp (gensym)))
   (cons "sym-pkg"      '(package-name (symbol-package :k)))
   (cons "read-sym-eq"  '(eq (read-from-string "JD-RT-SYM")
                          (intern "JD-RT-SYM")))

   ;; --- control flow -----------------------------------------------------
   (cons "if-t"         '(if t 1 2))
   (cons "if-nil"       '(if nil 1 2))
   (cons "cond"         '(cond ((eql 1 2) :a) ((eql 1 1) :b) (t :c)))
   (cons "cond-fall"    '(cond ((eql 1 2) :a) (t :z)))
   (cons "case"         '(case 3 (1 :a) (3 :c) (t :z)))
   (cons "case-else"    '(case 9 (1 :a) (t :z)))
   (cons "ecase-err"    '(ecase 9 (1 :a)))
   (cons "when"         '(when t 5))
   (cons "unless"       '(unless t 5))
   (cons "and"          '(and 1 2 3))
   (cons "and-nil"      '(and 1 nil 3))
   (cons "or"           '(or nil nil 3))
   (cons "not"          '(not nil))
   (cons "progn"        '(progn 1 2 3))
   (cons "prog1"        '(prog1 1 2 3))
   (cons "block-ret"    '(block b (return-from b 7) 9))
   (cons "block-nest"   '(block outer
                          (block inner (return-from outer 1)) 2))
   (cons "tagbody-go"   '(let ((n 0))
                          (tagbody
                           top (setq n (+ n 1))
                               (if (< n 3) (go top)))
                          n))
   (cons "catch-throw"  '(catch 'tag (throw 'tag 42) 9))
   (cons "catch-nothrow" '(catch 'tag 5))
   (cons "catch-deep"   '(catch 'a (catch 'b (throw 'a 1)) 2))
   (cons "unwind-prot"  '(let ((l nil))
                          (unwind-protect (push 1 l) (push 2 l))
                          l))
   (cons "unwind-throw" '(let ((l nil))
                          (catch 'x (unwind-protect (throw 'x 1) (push 2 l)))
                          l))

   ;; --- binding / closures ----------------------------------------------
   (cons "let"          '(let ((x 5)) (* x x)))
   (cons "let*"         '(let* ((x 5) (y (+ x 1))) (* x y)))
   (cons "let-shadow"   '(let ((x 1)) (let ((x 2)) x)))
   (cons "lambda-call"  '((lambda (x) (+ x 1)) 41))
   (cons "funcall-lam"  '(funcall (lambda (x y) (- x y)) 10 4))
   (cons "apply"        '(apply #'+ (list 1 2 3)))
   (cons "apply-spread" '(apply #'+ 1 2 (list 3 4)))
   (cons "closure"      '(let ((n 10)) (funcall (lambda (x) (+ n x)) 5)))
   (cons "closure-mut"  '(let ((n 0))
                          (let ((f (lambda () (setq n (+ n 1)))))
                            (funcall f) (funcall f) n)))
   (cons "two-counters" '(let ((mk (lambda ()
                                     (let ((c 0))
                                       (lambda () (setq c (+ c 1)) c)))))
                          (let ((a (funcall mk)) (b (funcall mk)))
                            (funcall a) (funcall a) (list (funcall a)
                                                          (funcall b)))))
   (cons "flet"         '(flet ((f (x) (* x 2))) (f 21)))
   (cons "flet-2"       '(flet ((f (x) (* x 2)) (g (x) (+ x 1))) (f (g 20))))
   (cons "labels-rec"   '(labels ((fact (n) (if (< n 2) 1 (* n (fact (- n 1))))))
                          (fact 10)))
   (cons "labels-mut"   '(labels ((ev (n) (if (eql n 0) t (od (- n 1))))
                                  (od (n) (if (eql n 0) nil (ev (- n 1)))))
                          (ev 10)))
   (cons "optional"     '(funcall (lambda (a &optional (b 9)) (list a b)) 1))
   (cons "rest"         '(funcall (lambda (&rest r) r) 1 2 3))
   (cons "key"          '(funcall (lambda (&key (a 1) b) (list a b)) :b 2))
   (cons "declare-ign"  '(funcall (lambda (x) (declare (ignore x)) 7) 1))

   ;; --- multiple values --------------------------------------------------
   (cons "values-2"     '(values 1 2))
   (cons "values-0"     '(values))
   (cons "values-3"     '(values 1 2 3))
   (cons "mvb"          '(multiple-value-bind (a b) (values 1 2) (list a b)))
   (cons "mvl"          '(multiple-value-list (values 1 2 3)))
   (cons "mvc"          '(multiple-value-call #'list (values 1 2) (values 3)))
   (cons "mvp1"         '(multiple-value-prog1 (values 1 2) 9))
   (cons "nth-value"    '(nth-value 1 (values 1 2)))
   (cons "floor-mv"     '(multiple-value-list (floor 17 5)))
   (cons "gethash-mv"   '(multiple-value-list
                          (let ((h (make-hash-table))) (gethash 1 h))))
   (cons "truncate-mv"  '(multiple-value-list (truncate -17 5)))

   ;; --- iteration --------------------------------------------------------
   (cons "dotimes"      '(let ((s 0)) (dotimes (i 100 s) (setq s (+ s i)))))
   (cons "dolist"       '(let ((s 0)) (dolist (x (list 1 2 3) s) (setq s (+ s x)))))
   (cons "do"           '(do ((i 0 (+ i 1)) (s 0 (+ s i))) ((eql i 5) s)))
   (cons "loop-count"   '(loop for i from 1 to 10 sum i))
   (cons "loop-collect" '(loop for i from 1 to 5 collect (* i i)))
   (cons "loop-chain"   '(loop for i from 2 to 4
                          append (loop for j from 2 to i collect (list i j))))
   (cons "loop-while"   '(let ((n 0)) (loop while (< n 5) do (setq n (+ n 1))) n))
   (cons "loop-across"  '(loop for c across "abc" collect c))
   (cons "nested-loop"  '(let ((s 0))
                          (dotimes (i 10) (dotimes (j 10) (setq s (+ s 1)))) s))

   ;; --- conditions -------------------------------------------------------
   (cons "hc-error"     '(handler-case (error "boom") (error (c) :caught)))
   (cons "hc-noerror"   '(handler-case 5 (error (c) :caught)))
   (cons "hc-noerror-cl" '(handler-case 5 (error (c) :caught) (:no-error (v) v)))
   (cons "hc-type"      '(handler-case (car 5) (t (c) :caught)))
   (cons "hb-signal"    '(let ((hit nil))
                          (handler-bind ((warning (lambda (c) (setq hit t))))
                            (handler-case (warn "w") (warning (c) nil)))
                          hit))
   (cons "signal-uncaught" '(handler-case (signal 'error) (t (c) :sig)))
   (cons "restart"      '(handler-case
                          (restart-case (error "x") (r () 7))
                          (error (c) :outer)))
   (cons "invoke-restart" '(restart-case
                            (handler-bind ((error (lambda (c)
                                                    (invoke-restart 'r))))
                              (error "x"))
                            (r () 7)))
   (cons "ignore-errors" '(ignore-errors (error "x")))
   (cons "err-div0"     '(handler-case (/ 1 0) (t (c) :div0)))
   (cons "err-undef"    '(handler-case (jd-no-such-function-xyz)
                          (t (c) :undef)))
   (cons "err-unbound"  '(handler-case (symbol-value 'jd-no-such-var-xyz)
                          (t (c) :unbound)))
   (cons "err-aref-oob" '(handler-case (aref (vector 1) 9) (t (c) :oob)))

   ;; --- types / coercion -------------------------------------------------
   (cons "typep-int"    '(typep 1 'integer))
   (cons "typep-list"   '(typep (list 1) 'list))
   (cons "typep-null"   '(typep nil 'null))
   (cons "type-of-fix"  '(type-of 1))
   (cons "subtypep"     '(multiple-value-list (subtypep 'fixnum 'integer)))
   (cons "coerce-char"  '(coerce 65 'character))

   ;; --- printing / reading ----------------------------------------------
   (cons "princ-str"    '(princ-to-string (list 1 2)))
   (cons "prin1-str"    '(prin1-to-string "a\"b"))
   (cons "write-str"    '(write-to-string 255 :base 16))
   (cons "format-d"     '(format nil "~D" 42))
   (cons "format-x"     '(format nil "~X" 255))
   (cons "format-a-lst" '(format nil "~{~A~^,~}" (list 1 2 3)))
   (cons "format-tilde" '(format nil "~5,'0D" 42))
   (cons "read-str"     '(read-from-string "(1 2 3)"))
   (cons "with-out-str" '(with-output-to-string (s) (princ 42 s)))

   ;; --- setf / places ----------------------------------------------------
   (cons "setf-gethash" '(let ((h (make-hash-table)))
                          (incf (gethash 'k h 0) 5) (gethash 'k h)))
   (cons "setf-values"  '(let (a b) (setf (values a b) (values 1 2)) (list a b)))
   (cons "rotatef"      '(let ((a 1) (b 2)) (rotatef a b) (list a b)))
   (cons "psetq"        '(let ((a 1) (b 2)) (psetq a b b a) (list a b)))
   (cons "setf-subseq"  '(let ((s (copy-seq "abcd")))
                          (setf (subseq s 1 3) "XY") s))

   ;; --- CLOS (runtime defclass/defmethod: the heaviest translator load) ---
   (cons "clos-struct"  '(progn (defstruct jd-pt x y)
                          (let ((p (make-jd-pt :x 1 :y 2)))
                            (list (jd-pt-x p) (jd-pt-y p)))))
   (cons "clos-class"   '(progn
                          (defclass jd-c () ((s :initform 5 :accessor jd-c-s)))
                          (jd-c-s (make-instance 'jd-c))))
   (cons "clos-method"  '(progn
                          (defclass jd-d () ())
                          (defmethod jd-m ((x jd-d)) 11)
                          (jd-m (make-instance 'jd-d))))
   (cons "clos-around"  '(progn
                          (defclass jd-e () ())
                          (defmethod jd-n ((x jd-e)) 1)
                          (defmethod jd-n :around ((x jd-e)) (+ 10 (call-next-method)))
                          (jd-n (make-instance 'jd-e))))

   ;; --- runtime-defined functions: the JIT-only blocker, measured ---------
   ;; A function defined at runtime is installed as an INTERPRETER trampoline
   ;; (%mvm-make-trampoline, interp.lisp), i.e. a HEAP closure with tag 9.  The
   ;; #206 native-callee guard therefore refuses to relocate a call to it, the
   ;; page build fails and the whole calling form interprets.  These forms
   ;; measure that: they must still return the RIGHT answer, and they are
   ;; expected to show up in the fallback census, not in NATIVE.
   (cons "rt-defun"     '(progn (defun jd-f1 (x) (* x 3)) (jd-f1 7)))
   (cons "rt-defun-2"   '(progn (defun jd-g1 (x) (+ x 1))
                          (defun jd-g2 (x) (* (jd-g1 x) 2))
                          (jd-g2 4)))
   (cons "rt-defmacro"  '(progn (defmacro jd-m1 (x) (list '+ x 1)) (jd-m1 41)))
   (cons "rt-fn-value"  '(progn (defun jd-h1 (x) (- x)) (mapcar #'jd-h1 (list 1 2))))
   (cons "rt-recursive" '(progn (defun jd-fib (n)
                                 (if (< n 2) n (+ (jd-fib (- n 1))
                                                  (jd-fib (- n 2)))))
                          (jd-fib 12)))
   (cons "rt-defparam"  '(progn (defparameter *jd-p1* 5) (* *jd-p1* 2)))
   (cons "rt-defvar"    '(progn (defvar *jd-v1* 7) (symbol-value '*jd-v1*)))

   ;; --- deep / stressful shapes -----------------------------------------
   (cons "deep-nest"    '(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 (+ 7 8)))))))) ; ANF #205
   (cons "nest-calls"   '(+ (length (list 1 2)) (* 2 (length "abc"))))
   (cons "big-let"      '(let ((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8))
                          (+ a b c d e f g h)))
   (cons "many-args"    '(list 1 2 3 4 5 6 7 8 9 10))
   (cons "cons-alloc"   '(let ((l nil))
                          (dotimes (i 200) (push i l)) (length l)))
   (cons "gc-stress"    '(let ((n 0))
                          (dotimes (i 2000) (setq n (length (list i i i))))
                          n))
   (cons "string-alloc" '(let ((s ""))
                          (dotimes (i 20) (setq s (concatenate 'string s "x")))
                          (length s)))
   (cons "quote-const"  ''(1 "two" #\3 :four 5.0d0))
   (cons "backquote"    '(let ((x 2)) `(1 ,x ,@(list 3 4))))
   (cons "eval-nested"  '(eval '(+ 1 2)))
   (cons "eval-2deep"   '(eval '(eval '(* 3 4))))))

;;; ------------------------------------------------------------------
;;; Double-execution probe.  The ONE remaining path that can re-run a form
;;; whose side effects already happened is the MV fallback (mvm-eval.lisp,
;;; WS5 #203) and the native-escape fallback.  A counter incremented by the
;;; form itself, evaluated ONCE, catches it; a value-only oracle cannot.
;;; ------------------------------------------------------------------

(defparameter *jd-side* 0)

(defun jd-once (label form expect-counter)
  (setq *jd-side* 0)
  (setq *jit-inhibit* nil)
  (let ((v (handler-case (multiple-value-list (eval form))
             (t (c) (list :signalled)))))
    (setq *jit-inhibit* nil)
    (setq *jd-total* (+ 1 *jd-total*))
    (unless (eql *jd-side* expect-counter)
      (setq *jd-diverge* (+ 1 *jd-diverge*))
      (format t "JIT-DOUBLE-EXEC ~A ran ~D time(s), expected ~D (value ~S)~%"
              label *jd-side* expect-counter v))))

;;; ------------------------------------------------------------------
;;; WS5 #218 — THE MULTIPLE-VALUES SIDE-EFFECT ORACLE.
;;;
;;; The MV double-execution bug is INVISIBLE to a value-only oracle: the
;;; returned values were always RIGHT (the interpreter re-ran the form and
;;; produced them correctly) — only the side effects were doubled.  That is
;;; why 288 forms of value comparison found nothing and one purpose-built
;;; counter found it.
;;;
;;; Its reach was never "the multiple-values case": it fired for ANY form
;;; whose last operation leaves MV-count > 1.  So this section probes the
;;; ordinary MV producers — floor / truncate / round / gethash / subtypep /
;;; read-from-string / multiple-value-bind / -list / -call — not just
;;; (values …).
;;;
;;; Each probe asserts THREE things, because a "fix" that returns one value
;;; where CL requires two would be worse than the doubling it replaced:
;;;   1. under JIT-on the counter increments EXACTLY ONCE;
;;;   2. under JIT-off (interpret) it also increments exactly once — so the
;;;      probe itself is honest;
;;;   3. the full multiple-value LIST equals the literal CL answer, and the
;;;      JIT and interpret paths agree with each other.
;;; It also records whether the form actually reached native code: an
;;; all-clear from a run where nothing was JIT'd is not a measurement.
;;; ------------------------------------------------------------------

(defparameter *jd-mv-native* 0)
(defparameter *jd-mv-total* 0)
(defparameter *jd-mv-clgap* 0)
(defparameter *jd-ht* nil)

(defun jd-mv-once (label form expect-values)
  "Evaluate FORM ONCE with the JIT live and ONCE with it inhibited, resetting
   *jd-side* before each.  FORM must increment *jd-side* exactly once per
   evaluation and return EXPECT-VALUES."
  (setq *jd-mv-total* (+ 1 *jd-mv-total*))
  (setq *jd-total* (+ 1 *jd-total*))
  ;; --- JIT live ---
  (setq *jd-side* 0)
  (setq *jit-inhibit* nil)
  (let* ((before (jd-count))
         (jv (handler-case (multiple-value-list (eval form))
               (t (c) (list :signalled (handler-case (type-of c)
                                         (t (c2) :unknown))))))
         (jnat (- (jd-count) before))
         (jside *jd-side*))
    ;; --- interpret ---
    (setq *jd-side* 0)
    (setq *jit-inhibit* t)
    (let ((iv (handler-case (multiple-value-list (eval form))
                (t (c) (list :signalled (handler-case (type-of c)
                                          (t (c2) :unknown)))))))
      (setq *jit-inhibit* nil)
      (let ((iside *jd-side*))
        (when (> jnat 0) (setq *jd-mv-native* (+ 1 *jd-mv-native*)))
        (unless (eql jside 1)
          (setq *jd-diverge* (+ 1 *jd-diverge*))
          (format t "JIT-DOUBLE-EXEC mv:~A ran ~D time(s) under JIT, expected 1 (value ~S)~%"
                  label jside jv))
        (unless (eql iside 1)
          (setq *jd-diverge* (+ 1 *jd-diverge*))
          (format t "INTERP-DOUBLE-EXEC mv:~A ran ~D time(s), expected 1 (value ~S)~%"
                  label iside iv))
        (unless (string= (jd-str jv) (jd-str iv))
          (setq *jd-diverge* (+ 1 *jd-diverge*))
          (format t "JIT-MV-DIVERGE mv:~A jit=~S interp=~S~%" label jv iv))
        (unless (string= (jd-str jv) (jd-str expect-values))
          ;; A literal mismatch that is IDENTICAL on both paths is not a JIT
          ;; defect — it is a pre-existing CL conformance gap in the shared
          ;; compiler, and counting it as a divergence would make this
          ;; differential oracle un-passable for reasons it does not measure.
          ;; It is still printed and still counted, separately.
          (if (string= (jd-str jv) (jd-str iv))
              (progn
                (setq *jd-mv-clgap* (+ 1 *jd-mv-clgap*))
                (format t "MV-CL-GAP (both paths) mv:~A got=~S expected=~S~%"
                        label jv expect-values))
              (progn
                (setq *jd-diverge* (+ 1 *jd-diverge*))
                (format t "JIT-MV-VALUES mv:~A jit=~S expected=~S~%"
                        label jv expect-values))))
        (when (eql jnat 0)
          (setq *jd-never-native* (cons label *jd-never-native*)))))))

(defun jd-mv-probes ()
  (setq *jd-ht* (make-hash-table))
  (setf (gethash 'a *jd-ht*) 11)
  ;; (values …) — the shape the docstring named …
  (jd-mv-once "values-2"
              '(progn (setq *jd-side* (+ *jd-side* 1)) (values 1 2))
              (list 1 2))
  (jd-mv-once "values-3"
              '(progn (setq *jd-side* (+ *jd-side* 1)) (values :a :b :c))
              (list :a :b :c))
  (jd-mv-once "values-0"
              '(progn (setq *jd-side* (+ *jd-side* 1)) (values))
              nil)
  (jd-mv-once "values-1"
              '(progn (setq *jd-side* (+ *jd-side* 1)) (values 9))
              (list 9))
  ;; … and the shapes it did NOT name, which are the common ones.
  (jd-mv-once "floor"
              '(progn (setq *jd-side* (+ *jd-side* 1)) (floor 7 2))
              (list 3 1))
  (jd-mv-once "floor-neg"
              '(progn (setq *jd-side* (+ *jd-side* 1)) (floor -7 2))
              (list -4 1))
  (jd-mv-once "truncate"
              '(progn (setq *jd-side* (+ *jd-side* 1)) (truncate 17 5))
              (list 3 2))
  (jd-mv-once "ceiling"
              '(progn (setq *jd-side* (+ *jd-side* 1)) (ceiling 17 5))
              (list 4 -3))
  (jd-mv-once "round"
              '(progn (setq *jd-side* (+ *jd-side* 1)) (round 7 2))
              (list 4 -1))
  (jd-mv-once "gethash-hit"
              '(progn (setq *jd-side* (+ *jd-side* 1)) (gethash 'a *jd-ht*))
              (list 11 t))
  (jd-mv-once "gethash-miss"
              '(progn (setq *jd-side* (+ *jd-side* 1)) (gethash 'zz *jd-ht*))
              (list nil nil))
  (jd-mv-once "subtypep"
              '(progn (setq *jd-side* (+ *jd-side* 1)) (subtypep 'integer 'number))
              (list t t))
  (jd-mv-once "read-from-string"
              '(progn (setq *jd-side* (+ *jd-side* 1)) (read-from-string "42"))
              (list 42 2))
  (jd-mv-once "mv-bind"
              '(progn (setq *jd-side* (+ *jd-side* 1))
                (multiple-value-bind (q r) (floor 17 5) (values q r)))
              (list 3 2))
  (jd-mv-once "mv-list"
              '(progn (setq *jd-side* (+ *jd-side* 1)) (multiple-value-list (floor 7 2)))
              (list (list 3 1)))
  (jd-mv-once "mv-call"
              '(progn (setq *jd-side* (+ *jd-side* 1))
                (multiple-value-call #'values (floor 17 5)))
              (list 3 2))
  (jd-mv-once "mv-prog1"
              '(progn (setq *jd-side* (+ *jd-side* 1))
                (multiple-value-prog1 (values 1 2) :ignored))
              (list 1 2))
  ;; MV as the tail of a real control-flow shape, not just a bare call.
  (jd-mv-once "mv-in-let"
              '(progn (setq *jd-side* (+ *jd-side* 1)) (let ((n 7)) (floor n 2)))
              (list 3 1))
  (jd-mv-once "mv-in-if"
              '(progn (setq *jd-side* (+ *jd-side* 1))
                (if (> 3 1) (values :y 2) (values :n 0)))
              (list :y 2))
  (jd-mv-once "mv-many"
              '(progn (setq *jd-side* (+ *jd-side* 1))
                (values 1 2 3 4 5 6 7 8))
              (list 1 2 3 4 5 6 7 8))
  ;; HEAP-VALUED extras: the words read back out of the MV block are real
  ;; pointers, not fixnums, so a stale (pre-GC) read shows up as garbage.
  (jd-mv-once "mv-heap"
              '(progn (setq *jd-side* (+ *jd-side* 1))
                (values (list 1 2) "ab" (list :x)))
              (list (list 1 2) "ab" (list :x))))

;;; GC-pressure stress.  Reading the MV block back is only correct while the
;;; BSS count stays authoritative: the collector scans exactly (count-1) words
;;; from #x10000098, so if anything reset the count before the extras were read,
;;; a collection triggered by the read-back's own cons loop would strand the
;;; not-yet-read extras at their from-space addresses.  Fixnum extras would
;;; survive that bug silently; freshly consed HEAP extras, evaluated in a loop
;;; that allocates, will not.
(defun jd-mv-stress (n)
  (let ((bad 0))
    (dotimes (i n)
      (let ((filler (make-string 128)))
        (setq filler filler)
        (multiple-value-bind (a b c)
            (eval '(values (list 1 2 3) (list 4 5) "zz"))
          (unless (and (equal a (list 1 2 3))
                       (equal b (list 4 5))
                       (equal c "zz"))
            (setq bad (+ bad 1))
            (when (< bad 4)
              (format t "JD-MV-STRESS-BAD iter=~D a=~S b=~S c=~S~%" i a b c))))))
    (setq *jd-total* (+ 1 *jd-total*))
    (unless (eql bad 0) (setq *jd-diverge* (+ 1 *jd-diverge*)))
    (format t "JD-MV-STRESS n=~D bad=~D~%" n bad)))

;;; ------------------------------------------------------------------
;;; Run
;;; ------------------------------------------------------------------

(format t "JD-START jit-enabled=~A~%"
        (handler-case (if (%jit-enabled-p) 1 0) (t (c) :err)))

(dolist (pr *jd-forms*)
  (jd-check (car pr) (cdr pr)))

;;; CROSS-FORM runtime definitions.  The forms above define and call inside ONE
;;; module, so their calls are IN-module and need no relocation.  The shape that
;;; matters for a real library -- and for `(actor-spawn #'my-entry)` -- is a
;;; function defined by one top-level form and called from a LATER one.  Those
;;; calls are OUT-of-module, the callee is an interpreter trampoline (a heap
;;; closure, tag 9, no PROT_EXEC), and the #206 guard refuses the relocation.
(defun jd-x1 (x) (* x 3))
(defun jd-x2 (x) (+ (jd-x1 x) 1))
(defmacro jd-xm (x) (list '+ x 1))
(defparameter *jd-xv* 5)

(jd-check "xform-call"    '(jd-x1 7))
(jd-check "xform-nested"  '(jd-x2 7))
(jd-check "xform-2calls"  '(+ (jd-x1 1) (jd-x1 2)))
(jd-check "xform-macro"   '(jd-xm 41))
(jd-check "xform-fnval"   '(mapcar #'jd-x1 (list 1 2)))
(jd-check "xform-funcall" '(funcall #'jd-x1 3))
(jd-check "xform-apply"   '(apply #'jd-x1 (list 3)))
(jd-check "xform-var"     '(* *jd-xv* 2))
(jd-check "xform-in-loop" '(let ((s 0)) (dotimes (i 5 s) (setq s (+ s (jd-x1 i))))))

;;; ------------------------------------------------------------------
;;; WS5 #222 -- NATIVE INSTALLATION OF RUNTIME-DEFINED FUNCTIONS
;;; ------------------------------------------------------------------
;;;
;;; A runtime DEFUN now installs REAL NATIVE CODE (a tag-3 pointer into the
;;; module's exec page) instead of a %mvm-make-trampoline heap closure, so the
;;; #206 native-callee relocation in %jit-reloc-calls resolves it and a later
;;; top-level form that calls it reaches native.  These probes cover the three
;;; things that installation must not break -- VALUES (interp vs JIT, via
;;; jd-check), function-object IDENTITY, and REDEFINITION -- plus the two shapes
;;; the task called out explicitly (redefinition, mutual recursion) and a GC
;;; probe, since an exec page lives OUTSIDE the collected heap and a
;;; persistently-installed function is never re-visited by the seam.

(defparameter *jd222-total* 0)
(defparameter *jd222-fail* 0)

(defun jd-assert (label got expected)
  "Structural assertion (printed-form comparison, like jd-check's oracle).
   Failures are rolled into *jd-diverge* so JD-OK/JD-FAIL remains the gate."
  (setq *jd222-total* (+ 1 *jd222-total*))
  (unless (string= (jd-str got) (jd-str expected))
    (setq *jd222-fail* (+ 1 *jd222-fail*))
    (setq *jd-diverge* (+ 1 *jd-diverge*))
    (format t "JD222-FAIL ~A~%  got      = ~S~%  expected = ~S~%" label got expected))
  nil)

(defparameter *jd222-earlybind* 0)

(defun jd-note-earlybind (label got expected)
  "KNOWN DIVERGENCE, reported rather than failed.  A runtime function that is
   ALREADY installed as native code baked its callees' addresses at page-build
   time, so redefining one of those callees is not seen by it — early binding,
   the same rule build-time native code has always had (CLAUDE.md limitation 1).
   The interpreter resolves by name on every call and therefore late-binds.
   Counted separately so the oracle stays a clean gate while the divergence
   stays VISIBLE and measured."
  (setq *jd222-earlybind* (+ 1 *jd222-earlybind*))
  (unless (string= (jd-str got) (jd-str expected))
    (format t "JD222-EARLYBIND ~A  got=~S  late-binding-would-give=~S~%"
            label got expected))
  nil)

(defparameter *jd-gcstale* 0)
(defparameter *jd-gcstale-hits* 0)

(defun jd-note-gcstale (label got expected)
  "KNOWN DIVERGENCE, reported rather than failed: CONST-POOL STALENESS ACROSS A
   COLLECTION.  A JIT'd module's `:li-const` sites are patched with the const
   object's CURRENT tagged heap address; the collector moves those objects and
   only re-bakes a page that is re-entered THROUGH THE SEAM.  A runtime macro's
   expander is not, so after the first real GC its quoted-symbol constants are
   dangling from-space pointers: `macro-function` still returns non-NIL but the
   expansion comes back with a garbage head (`(#<?> 2 3)` instead of `(+ 2 3)`),
   and compiling that signals UNDEFINED-FUNCTION.

   This was INVISIBLE until the collections in this file became real — every
   `survives GC` probe here allocated 2.5 MB against a 939 MB threshold and
   never collected once.  It is the same hazard #222's const-pool restriction
   describes; that restriction covers installed FUNCTIONS, and nothing has ever
   covered macro expanders.

   Counted separately so JD-OK stays a usable gate while the bug stays visible."
  (setq *jd-gcstale* (+ 1 *jd-gcstale*))
  (unless (string= (jd-str got) (jd-str expected))
    (setq *jd-gcstale-hits* (+ 1 *jd-gcstale-hits*))
    (format t "JD-GCSTALE ~A  got=~S  correct-would-be=~S~%" label got expected))
  nil)

;;; TAG PROBE.  %val->word is a compiler primop (SHL 1) and is only reliable
;;; where the surrounding code is NATIVE.  Under mvm-interpret the interpreter
;;; itself works in the word domain (reg-get = %val->word, reg-set = %word->val,
;;; interp.lisp:53), so an INTERPRETED %val->word double-converts and the low
;;; nibble reads back wrong: on the pre-#222 binary this expression returns 2
;;; for #'CAR, a build-time native function that is unambiguously tag 3.
;;;
;;; So the tag is computed in a top-level form of its OWN -- a bare SETQ whose
;;; only callees (symbol-function, floor) are build-time native, so it always
;;; relocates and always runs native -- and stashed in a global that jd-assert
;;; then reads.  Putting the expression inline as a jd-assert ARGUMENT does not
;;; work: jd-assert is itself a runtime-defined, const-bearing (format string)
;;; function that keeps its trampoline, so the whole calling form fails
;;; relocation and interprets, and the probe would measure the interpreter's
;;; %val->word rather than the installed function's tag.  LOGAND is avoided in
;;; favour of floor arithmetic for the same robustness reason.
(defparameter *jd222-tag* 0)
(defmacro jd-tag-into (form)
  (list 'setq '*jd222-tag*
        (list 'let (list (list 'w (list '%val->word form)))
              (list '- 'w (list '* 16 (list 'floor 'w 16))))))

;; --- the basic shape: define at runtime, call from a LATER top-level form ---
(defun jd-n1 (x) (* x 5))
(defun jd-n2 (x) (+ (jd-n1 x) 2))
(defun jd-nfact (n) (if (< n 2) 1 (* n (jd-nfact (- n 1)))))   ; self-recursion

(jd-check "d222-call"      '(jd-n1 7))
(jd-check "d222-nested"    '(jd-n2 7))
(jd-check "d222-selfrec"   '(jd-nfact 8))
(jd-check "d222-2calls"    '(+ (jd-n1 1) (jd-n1 2)))
(jd-check "d222-loop"      '(let ((s 0)) (dotimes (i 6 s) (setq s (+ s (jd-n1 i))))))
(jd-check "d222-mapcar"    '(mapcar #'jd-n1 (list 1 2 3)))
(jd-check "d222-funcall"   '(funcall #'jd-n1 4))
(jd-check "d222-apply"     '(apply (function jd-n2) (list 4)))
(jd-check "d222-reduce"    '(reduce #'+ (mapcar #'jd-n1 (list 1 2 3))))
(jd-check "d222-hof-sort"  '(sort (mapcar #'jd-n1 (list 3 1 2)) #'<))
;; the defun and the call in ONE later form (module-internal + cross-module mix)
(jd-check "d222-mixed"     '(progn (defun jd-n3 (x) (jd-n1 (+ x 1))) (jd-n3 2)))

;; --- IDENTITY: symbol-function / #'NAME / funcall must all agree ------------
(jd-assert "d222-id-eq"       (eq (function jd-n1) (symbol-function 'jd-n1)) t)
(jd-assert "d222-id-fnp"      (functionp (symbol-function 'jd-n1)) t)
(jd-assert "d222-id-call"     (funcall (symbol-function 'jd-n1) 3) 15)
(jd-assert "d222-id-apply"    (apply (symbol-function 'jd-n1) (list 3)) 15)
;; the point of the whole exercise: the installed object is NATIVE (tag 3), not
;; a heap closure (tag 9).  This is exactly the predicate %jit-reloc-calls uses,
;; and exactly what actor-spawn's `(untag fn)` + jump requires.
;; control: a build-time native function must read 3 (proves the probe works)
(jd-tag-into (symbol-function 'car))
(jd-assert "d222-tag-builtin" *jd222-tag* 3)
;; the claim: a RUNTIME-defined function now reads 3 too, not 9 (heap closure)
(jd-tag-into (symbol-function 'jd-n1))
(jd-assert "d222-tag-native"  *jd222-tag* 3)
(jd-tag-into (symbol-function 'jd-nfact))
(jd-assert "d222-tag-selfrec" *jd222-tag* 3)
;; a CONST-BEARING runtime defun is deliberately NOT installed: it keeps its
;; interpreter trampoline, which is a heap object (tag 9).  This is the measured
;; boundary of the feature, asserted rather than assumed.
(eval '(defun jd-tramp-fn (x) (format nil "~D" x)))
(jd-tag-into (symbol-function 'jd-tramp-fn))
(jd-assert "d222-tag-const-tramp" *jd222-tag* 9)

;; --- REDEFINITION (last-defun-wins), including the CACHED-FORM path ---------
;; The same call form text is evaluated before AND after the redefinition, so
;; the compiled-module tuple cache (and, under JIT, *jit-page-cache*) is hit the
;; second time.  Without cache invalidation the cached page would keep calling
;; the superseded address and silently return the OLD answer.
(eval '(defun jd-redef (x) (* x 2)))
(jd-assert "d222-redef-1st"   (eval '(jd-redef 5)) 10)
(eval '(defun jd-redef (x) (* x 100)))
(jd-assert "d222-redef-2nd"   (eval '(jd-redef 5)) 500)
(eval '(defun jd-redef (x) (- x)))
(jd-assert "d222-redef-3rd"   (eval '(jd-redef 5)) -5)
;; redefinition must also be visible through symbol-function and #'NAME
(jd-assert "d222-redef-symfn" (funcall (symbol-function 'jd-redef) 5) -5)
;; An indirect caller compiled BEFORE the change sees the definition current at
;; ITS compile time (correct), ...
(eval '(defun jd-redef-caller (x) (jd-redef x)))
(jd-assert "d222-redef-via-1" (eval '(jd-redef-caller 5)) -5)
;; ... but once jd-redef-caller is itself installed NATIVE it has BAKED
;; jd-redef's address, so a later redefinition of jd-redef is invisible to it.
;; This is the one semantic divergence #222 introduces and it is deliberately
;; reported, not fixed -- see jd-note-earlybind and GATE-RESULT-jit-defun.md.
(eval '(defun jd-redef (x) (* x 7)))
(jd-note-earlybind "d222-redef-via-2" (eval '(jd-redef-caller 5)) 35)
;; The DIRECT call, however, must always see the newest definition.
(jd-assert "d222-redef-direct" (eval '(jd-redef 5)) 35)
(jd-assert "d222-redef-symfn2" (funcall (symbol-function 'jd-redef) 5) 35)

;; --- MUTUAL RECURSION across separate top-level forms ----------------------
;; PASS 1: jd-even is defined while jd-odd does not yet exist, so jd-even's
;; page fails relocation (R-RELOC-CALL-UNRESOLVED) and it keeps its trampoline;
;; jd-odd then fails too (its callee jd-even is a heap closure).  Answers must
;; still be right -- this is the interpret fallback doing its job.
(eval '(defun jd-even (n) (if (= n 0) t (jd-odd (- n 1)))))
(eval '(defun jd-odd  (n) (if (= n 0) nil (jd-even (- n 1)))))
(jd-assert "d222-mutrec-p1-e"  (eval '(jd-even 10)) t)
(jd-assert "d222-mutrec-p1-o"  (eval '(jd-odd 10)) nil)
(jd-assert "d222-mutrec-p1-e7" (eval '(jd-even 7)) nil)
;; PASS 2: re-evaluating both does NOT help, and this was MEASURED, not assumed
;; -- the first draft of this file claimed "the limitation is one pass deep" and
;; the probe disproved it.  Mutual recursion split across separate top-level
;; forms can NEVER go native under this scheme: jd-even can only relocate if
;; jd-odd is already native, and jd-odd can only relocate if jd-even is, so
;; neither can go first.  Both keep their trampolines at pass 1, 2 and 3, and
;; the answers stay correct via the interpret fallback.
(eval '(defun jd-even (n) (if (= n 0) t (jd-odd (- n 1)))))
(eval '(defun jd-odd  (n) (if (= n 0) nil (jd-even (- n 1)))))
(jd-assert "d222-mutrec-p2-e"  (eval '(jd-even 10)) t)
(jd-assert "d222-mutrec-p2-o"  (eval '(jd-odd 11)) t)
(jd-assert "d222-mutrec-p2-e7" (eval '(jd-even 7)) nil)
;; ...so this one is EXPECTED in JD-NEVER-NATIVE.  It is listed there as the
;; measured boundary, not as an unexplained fallback.
(jd-check  "d222-mutrec-call"  '(list (jd-even 20) (jd-odd 20)))

;; The working shape: mutual recursion inside ONE top-level form.  Both bodies
;; are then in the SAME module, so the calls are IN-module and need no
;; relocation at all -- both install natively.  This is what a progn, a file
;; compiler, or LABELS produces, so the limitation above is narrower than it
;; looks.
(eval '(progn (defun jd-seven (n) (if (= n 0) t (jd-sodd (- n 1))))
              (defun jd-sodd  (n) (if (= n 0) nil (jd-seven (- n 1))))))
(jd-assert "d222-mutrec-1form-e" (eval '(jd-seven 10)) t)
(jd-assert "d222-mutrec-1form-o" (eval '(jd-sodd 11)) t)
(jd-tag-into (symbol-function 'jd-seven))
(jd-assert "d222-mutrec-1form-tag-e" *jd222-tag* 3)
(jd-tag-into (symbol-function 'jd-sodd))
(jd-assert "d222-mutrec-1form-tag-o" *jd222-tag* 3)
(jd-check  "d222-mutrec-1form-call" '(list (jd-seven 20) (jd-sodd 20)))

;; A plain FORWARD reference (not mutual) IS one pass deep: fwd-a defined while
;; fwd-b does not exist keeps its trampoline, but re-evaluating fwd-a once
;; fwd-b is native makes fwd-a native too -- the cascade, one definition at a
;; time, which is how a loaded library links against itself.
(eval '(defun jd-fwd-a (x) (jd-fwd-b x)))
(eval '(defun jd-fwd-b (x) (* x 2)))
(jd-tag-into (symbol-function 'jd-fwd-b))
(jd-assert "d222-fwd-b-native"  *jd222-tag* 3)
(jd-tag-into (symbol-function 'jd-fwd-a))
(jd-assert "d222-fwd-a-tramp"   *jd222-tag* 9)
(eval '(defun jd-fwd-a (x) (jd-fwd-b x)))
(jd-tag-into (symbol-function 'jd-fwd-a))
(jd-assert "d222-fwd-a-native"  *jd222-tag* 3)
(jd-assert "d222-fwd-value"     (eval '(jd-fwd-a 5)) 10)

;; --- CONST-POOL-BEARING defuns keep their trampoline: still CORRECT --------
;; A function whose native range contains a const-pool patch site is NOT
;; installed natively (the baked immediate would go stale across a GC and
;; nothing re-bakes a persistently-installed function).  It must still work.
(eval '(defun jd-conststr (x) (concatenate 'string "v=" (princ-to-string x))))
(eval '(defun jd-constlist () '(a b c)))
(jd-assert "d222-const-str"   (eval '(jd-conststr 7)) "v=7")
(jd-assert "d222-const-list"  (eval '(jd-constlist)) '(a b c))
(jd-check  "d222-const-call"  '(list (jd-conststr 1) (jd-constlist)))

;; --- GC: the exec page lives OUTSIDE the collected heap --------------------
;; A natively-installed function is entered directly by native code and is never
;; re-visited by the seam, so it must survive collection with no re-patching.
;; Allocate hard between calls and re-check the answers and the identity.
;;
;; MEASURE THE COLLECTION, DO NOT ASSUME IT.  These probes used to force GC with
;; `(eval '(dotimes (i 4000) (make-list 40)))` — roughly 2.5 MB of garbage
;; against a collection threshold at the 939 MB heap midpoint
;; (*linux-x64-r14-offset* #x38000000, build-generic-cli.lisp).  Reading the
;; collector's own counter afterwards gives gc_count = 0: not one collection
;; ever ran, so EVERY "survives GC" assertion in this file was vacuous.  #222's
;; evidence cited surviving a forced GC; that premise was untested.
;;
;; jd-force-gc allocates until the counter ACTUALLY advances and returns how
;; many collections it saw; jd-gc-fired asserts that number is non-zero, so a
;; future heap-size change cannot silently turn these back into no-ops.
;;
;; gc_count is a RAW word at BSS 0x10000060 bumped by `inc qword [abs]`
;; (translate-x64.lisp), so a :u64 read reinterprets it as a tagged VALUE — a
;; count of 1 reads back as a CONS and printing it aborts the load.  mem-ref
;; :u32 returns its low half properly tagged, which is what we want.
;;
;; NB the documented small-heap knob MODUS_GC_R14=262144 cannot substitute for
;; this: at that size the JIT falls back for essentially everything (native%~0
;; under heap pressure), so the very code path under test stops running.  These
;; probes therefore collect at the SHIPPING heap settings.
(defun jd-gcn () (mem-ref #x10000060 :u32))
(defun jd-force-gc ()
  (let ((b (jd-gcn)) (k 0))
    (loop
      (when (or (> (jd-gcn) b) (>= k 60)) (return (- (jd-gcn) b)))
      (dotimes (j 100000) (make-list 40))
      (setq k (+ k 1)))))
(defun jd-gc-fired (label)
  (let ((n (jd-force-gc)))
    ;; RE-ESTABLISH THIS FILE'S OWN MACRO.  A runtime DEFMACRO does not survive a
    ;; collection (the gc-macro-* assertions below MEASURE that; it is asserted,
    ;; not hidden).  jd-tag-into is a macro, so without this every jd-tag-into
    ;; after the first real GC compiles as a call to an undefined function and
    ;; ABORTS THE WHOLE LOAD — which is exactly what happens on main, and which
    ;; would leave the rest of the file unmeasured.  Re-defining it here costs
    ;; nothing and lets the file run to its summary on both branches.
    ;; (jd-gc-fired itself is safe: it is const-bearing, so it keeps an
    ;; interpreter trampoline, and mvm-interpret's op-li-const reads the pool
    ;; live rather than through a baked address.)
    (eval '(defmacro jd-tag-into (form)
             (list 'setq '*jd222-tag*
                   (list 'let (list (list 'w (list '%val->word form)))
                         (list '- 'w (list '* 16 (list 'floor 'w 16)))))))
    (jd-assert label (if (> n 0) :collected :no-collection) :collected)))

;;; --------------------------------------------------------------------------
;;; WHAT ACTUALLY SURVIVES A REAL COLLECTION
;;; --------------------------------------------------------------------------
;;; With the collection now real, these split the #222 premise into its parts.
;;; A runtime DEFUN installed as native code survives — value, identity and a
;;; direct funcall all hold.  A runtime DEFMACRO does NOT: `macro-function`
;;; still returns non-NIL, but the expander's own quoted-symbol constants have
;;; gone stale, so the expansion comes back with a garbage head and compiling it
;;; signals UNDEFINED-FUNCTION.  That is the const-pool staleness #222's
;;; restriction describes, observed directly — the restriction covers installed
;;; FUNCTIONS, and nothing ever covered macro expanders.
(eval '(defun jd-gcsurv (x) (+ (* x 3) 1)))
(eval '(defmacro jd-gcmac (a b) (list '+ a b)))
(jd-assert "gc-defun-before"  (eval '(jd-gcsurv 10)) 31)
(jd-assert "gc-macro-before"  (eval '(jd-gcmac 2 3)) 5)
(jd-gc-fired "gc-fired-survival")
(jd-assert "gc-defun-seam"    (handler-case (eval '(jd-gcsurv 10)) (t (c) :ERR)) 31)
(jd-assert "gc-defun-direct"  (handler-case (funcall (symbol-function 'jd-gcsurv) 10)
                                (t (c) :ERR)) 31)
(jd-assert "gc-defun-id"      (eq (function jd-gcsurv) (symbol-function 'jd-gcsurv)) t)
(jd-assert "gc-macro-fn-nonnil"
           (handler-case (if (macro-function 'jd-gcmac) t nil) (t (c) :ERR)) t)
;; Counted separately, like JD222-EARLYBIND: a KNOWN divergence that must stay
;; VISIBLE and measured without turning JD-OK red, so this file remains usable
;; as a gate while the bug is open.  On main both of these report; on a build
;; where the const pool is reached through a GC-updated indirection they do not.
(jd-note-gcstale "gc-macro-expand"
                 (handler-case (macroexpand-1 '(jd-gcmac 2 3)) (t (c) :ERR)) '(+ 2 3))
(jd-note-gcstale "gc-macro-use"
                 (handler-case (eval '(jd-gcmac 2 3)) (t (c) :ERR)) 5)

;;; ==========================================================================
;;; EXTENDED GC-SURVIVAL BATTERY
;;; ==========================================================================
;;; Two probes on one macro were thin evidence for "runtime macros work across
;;; GC", and thinner still for the CLASS the macro bug turned out to belong to.
;;; The defect is not about macros: %jit-patch-consts bakes each const-pool
;;; object's CURRENT heap address into a `movabs` immediate, and the only
;;; re-bake (%jit-entry-for) fires when a page is re-entered THROUGH THE SEAM.
;;; ANYTHING that re-enters the page some other way keeps reading the address
;;; the object had before the collection.  A macro expander is only the most
;;; visible instance; a lambda in a global, a lambda inside a data structure,
;;; a lambda returned by EVAL and a #222-installed DEFUN that CALLS a
;;; const-bearing sibling are all the same bug, and all of them are below.
;;;
;;; Everything here goes through jd-note-gcstale, not jd-assert, for the reason
;;; the two probes above do: the file must run to its summary on an image where
;;; the bug is OPEN (on such an image a broken macro would otherwise abort the
;;; load and leave the remaining ~250 assertions unmeasured).  JD-GCSTALE-HITS
;;; is the single headline: 0 means every quoted constant reachable from
;;; persisted code survived collection.
;;;
;;; Values are asserted, not liveness: each probe names the exact object it must
;;; get back, so a garbage head (`(#<?> 2 3)`) or a stale string counts as a hit
;;; rather than passing because nothing crashed.

;; (1) EXPANSION CARRYING A STRING AND A QUOTED LIST, not just a symbol head.
;; A symbol is the cheapest thing to notice going stale; strings and quoted
;; sublists live in the same pool and take the same baked address.
(eval '(defmacro jd-gcrich (x)
         (list 'list "s-lit" (list 'quote (list 'q1 'q2)) x)))
(jd-note-gcstale "gc-macro-rich-expand"
                 (handler-case (macroexpand-1 '(jd-gcrich 7)) (t (c) :ERR))
                 '(list "s-lit" '(q1 q2) 7))
(jd-note-gcstale "gc-macro-rich-value"
                 (handler-case (eval '(jd-gcrich 7)) (t (c) :ERR))
                 '("s-lit" (q1 q2) 7))

;; (2) A MACRO CALLING ANOTHER RUNTIME MACRO.  The outer expander's own consts
;; must survive AND the inner expander must still be found and still work, so
;; this fails if either page went stale.
(eval '(defmacro jd-gcinner (a) (list '* a 2)))
(eval '(defmacro jd-gcouter (a) (list 'jd-gcinner (list '+ a 1))))
(jd-note-gcstale "gc-macro-nested-before"
                 (handler-case (eval '(jd-gcouter 4)) (t (c) :ERR)) 10)

;; (3) CLOSURES THAT OUTLIVE THEIR MODULE — the same hazard without a macro.
(eval '(defparameter *jd-gcclo* (lambda () (list 'clo-sym "clo-str"))))
(eval '(defparameter *jd-gctbl* (list (cons :k (lambda () (list 'tbl-sym "tbl-str"))))))
(defparameter *jd-gcres* (eval '(function (lambda () (list 'res-sym "res-str")))))

;; (4) A #222-INSTALLED NATIVE DEFUN THAT CALLS A CONST-BEARING SIBLING.
;; jd-gcdirty holds the constants; jd-gcclean holds none, so the per-function
;; const check admits IT for native installation — but an in-module call is a
;; direct in-page branch straight into the dirty code.  The per-function check
;; cannot see that; only a page-level rule can.
(eval '(progn (defun jd-gcdirty () (list 'sib-sym "sib-str"))
              (defun jd-gcclean () (jd-gcdirty))))
(jd-note-gcstale "gc-sibling-before"
                 (handler-case (list (jd-gcdirty) (jd-gcclean)) (t (c) :ERR))
                 '((sib-sym "sib-str") (sib-sym "sib-str")))

;; (5) `#'RUNTIME-DEFUN` AS A VALUE.  jd-gctramp is const-bearing, so #222
;; leaves it an interpreter trampoline — a tag-9 HEAP closure.  The lambda
;; below is itself const-free, so its page builds, and %jit-reloc-fn-addrs
;; baked that heap address straight into a movabs (it had no tag-3 requirement,
;; unlike the CALL reloc).  On the pre-fix image this returns correctly, then
;; after one collection faults hard enough to escape its own handler-case —
;; the load reports `UNHANDLED-ESCAPE … NIL`, swallows the form and carries on.
;; Hence the two-step shape: the risky call is alone in its own top-level form,
;; writing a global that the assertion reads afterwards, so a swallowed form
;; still lands as a HIT and the probe COUNT is the same on both images.
(eval '(defun jd-gctramp () (list 'fa-sym "fa-str")))
(eval '(defparameter *jd-gcfa* (lambda () (funcall (function jd-gctramp)))))
(jd-note-gcstale "gc-fnaddr-value-before"
                 (handler-case (funcall *jd-gcfa*) (t (c) :ERR))
                 '(fa-sym "fa-str"))
(defparameter *jd-fa-res* :NOT-REACHED)

;; ---- one collection, then re-check every one of them ----
(jd-gc-fired "gc-ext-fired1")
(setq *jd-fa-res* (handler-case (funcall *jd-gcfa*) (t (c) :ERR)))
(jd-note-gcstale "gc-fnaddr-value-after" *jd-fa-res* '(fa-sym "fa-str"))
(jd-note-gcstale "gc-macro-rich-expand-after"
                 (handler-case (macroexpand-1 '(jd-gcrich 7)) (t (c) :ERR))
                 '(list "s-lit" '(q1 q2) 7))
(jd-note-gcstale "gc-macro-rich-value-after"
                 (handler-case (eval '(jd-gcrich 7)) (t (c) :ERR))
                 '("s-lit" (q1 q2) 7))
(jd-note-gcstale "gc-macro-nested-after"
                 (handler-case (eval '(jd-gcouter 4)) (t (c) :ERR)) 10)
;; the raw expander object, reached the way a user reaches it
(jd-note-gcstale "gc-macro-fn-funcall-after"
                 (handler-case (funcall (macro-function 'jd-gcmac) '(jd-gcmac 2 3) nil)
                   (t (c) :ERR))
                 '(+ 2 3))
(jd-note-gcstale "gc-closure-global-after"
                 (handler-case (funcall *jd-gcclo*) (t (c) :ERR))
                 '(clo-sym "clo-str"))
(jd-note-gcstale "gc-closure-in-data-after"
                 (handler-case (funcall (cdr (assoc :k *jd-gctbl*))) (t (c) :ERR))
                 '(tbl-sym "tbl-str"))
(jd-note-gcstale "gc-closure-eval-result-after"
                 (handler-case (funcall *jd-gcres*) (t (c) :ERR))
                 '(res-sym "res-str"))
(jd-note-gcstale "gc-sibling-dirty-after"
                 (handler-case (jd-gcdirty) (t (c) :ERR)) '(sib-sym "sib-str"))
(jd-note-gcstale "gc-sibling-clean-after"
                 (handler-case (jd-gcclean) (t (c) :ERR)) '(sib-sym "sib-str"))

;; (5) MANY COLLECTIONS LATER.  One collection only proves the FIRST move was
;; survived; a page whose address happens to be re-baked once, or a constant
;; that happened not to move, would pass that.  Three more collections move the
;; semispaces back and forth repeatedly.
(jd-gc-fired "gc-ext-fired2")
(jd-gc-fired "gc-ext-fired3")
(jd-gc-fired "gc-ext-fired4")
(jd-note-gcstale "gc-macro-many-gcs"
                 (handler-case (eval '(jd-gcrich 7)) (t (c) :ERR))
                 '("s-lit" (q1 q2) 7))
(jd-note-gcstale "gc-macro-nested-many-gcs"
                 (handler-case (eval '(jd-gcouter 4)) (t (c) :ERR)) 10)
(jd-note-gcstale "gc-closure-many-gcs"
                 (handler-case (funcall *jd-gcclo*) (t (c) :ERR))
                 '(clo-sym "clo-str"))

;; (6) REDEFINITION ACROSS A COLLECTION.  The NEW definition must win and must
;; itself survive the NEXT collection — this is what catches a fix that merely
;; re-bakes the page that happens to be cached, since a redefinition builds a
;; different page while the old one is still reachable.
;;
;; NOTE THE CALL-SITE TEXT.  Each use below is a DIFFERENT form — (jd-gcredef 5),
;; then 6, then 7 — deliberately, so this probe measures the GC question and
;; only the GC question.  Re-using the SAME text would instead hit
;; *mvm-eval-cache*, an unrelated and pre-existing defect measured separately
;; by jd-note-macro-redef below.
(eval '(defmacro jd-gcredef (a) (list 'list ''v1 a)))
(jd-note-gcstale "gc-redef-v1" (handler-case (eval '(jd-gcredef 5)) (t (c) :ERR))
                 '(v1 5))
(jd-gc-fired "gc-ext-fired5")
(eval '(defmacro jd-gcredef (a) (list 'list ''v2 a "r-str")))
(jd-note-gcstale "gc-redef-v2-fresh"
                 (handler-case (eval '(jd-gcredef 6)) (t (c) :ERR)) '(v2 6 "r-str"))
(jd-gc-fired "gc-ext-fired6")
(jd-note-gcstale "gc-redef-v2-after"
                 (handler-case (eval '(jd-gcredef 7)) (t (c) :ERR)) '(v2 7 "r-str"))

;;; ---- SECOND KNOWN-OPEN BUG, found by the probe above, counted separately ----
;;; A runtime macro REDEFINITION is invisible to a call site whose form TEXT was
;;; already evaluated once.  Nothing to do with GC — it reproduces with no
;;; collection anywhere, and identically on the pre-fix image:
;;;
;;;   (eval '(defmacro rm (a) (list 'list ''v1 a)))   (eval '(rm 5)) => (V1 5)
;;;   (eval '(defmacro rm (a) (list 'list ''v2 a)))   (eval '(rm 5)) => (V1 5)  ***
;;;                                                   (eval '(rm 6)) => (V2 6)
;;;                                                   (macroexpand-1 '(rm 5))
;;;                                                                  => (LIST 'V2 5)
;;;
;;; *mvm-eval-cache* is keyed by EQUAL on the FORMS, so the second `(rm 5)`
;;; replays the module compiled against the OLD expander — the macro was baked
;;; into that module at compile time.  MACRO-FUNCTION and MACROEXPAND-1 are both
;;; already correct, and a DEFUN redefinition is honoured (a compiled call
;;; resolves its callee by name at call time, so it late-binds); it is only the
;;; macro case that the cache freezes.  The parallel already exists for the
;;; native side — mvm-eval-forms drops *jit-page-cache* when a DEFUN is
;;; redefined — so the shape of the fix is known; it belongs in its own change
;;; with its own gate, not smuggled into a GC fix.
(defparameter *jd-macro-redef* 0)
(defparameter *jd-macro-redef-hits* 0)
(defun jd-note-macro-redef (label got expected)
  (setq *jd-macro-redef* (+ 1 *jd-macro-redef*))
  (unless (string= (jd-str got) (jd-str expected))
    (setq *jd-macro-redef-hits* (+ 1 *jd-macro-redef-hits*))
    (format t "JD-MACRO-REDEF ~A  got=~S  correct-would-be=~S~%" label got expected))
  nil)
(eval '(defmacro jd-cachemac (a) (list 'list ''c1 a)))
(jd-note-macro-redef "redef-cache-v1"
                     (handler-case (eval '(jd-cachemac 5)) (t (c) :ERR)) '(c1 5))
(eval '(defmacro jd-cachemac (a) (list 'list ''c2 a)))
(jd-note-macro-redef "redef-cache-same-form"
                     (handler-case (eval '(jd-cachemac 5)) (t (c) :ERR)) '(c2 5))
(jd-note-macro-redef "redef-cache-new-form"
                     (handler-case (eval '(jd-cachemac 9)) (t (c) :ERR)) '(c2 9))
(jd-note-macro-redef "redef-cache-expand"
                     (handler-case (macroexpand-1 '(jd-cachemac 5)) (t (c) :ERR))
                     '(list 'c2 5))

;;; ---- KNOWN-OPEN RESIDUAL, counted on its own line ----
;;; Not a macro and not a closure: ONE top-level form, JIT'd, that allocates
;;; past the collection threshold DURING its own execution.  The seam re-bakes
;;; const immediates at ENTRY (%jit-entry-for, keyed on %gc-count); a collection
;;; that fires mid-flight has no second chance, so the form's own quoted
;;; literals go stale for the remainder of that single execution and it returns
;;; a silently wrong answer with no macro, no closure and no persistence
;;; involved.  It is the same root cause as everything above and it is NOT
;;; fixed by the page-level const gate — reaching it needs the const pool read
;;; through a GC-updated indirection (the constant vector, task #226).
;;; Reported on its own counter so it cannot be mistaken for the class that IS
;;; closed, and so it stops reporting the day #226 lands.
(defparameter *jd-gcresid* 0)
(defparameter *jd-gcresid-hits* 0)
(defun jd-note-gcresid (label got expected)
  (setq *jd-gcresid* (+ 1 *jd-gcresid*))
  (unless (string= (jd-str got) (jd-str expected))
    (setq *jd-gcresid-hits* (+ 1 *jd-gcresid-hits*))
    (format t "JD-GCRESID ~A  got=~S  correct-would-be=~S~%" label got expected))
  nil)
(jd-note-gcresid
  "gc-midform-consts"
  (handler-case
      (eval '(let ((g0 (jd-gcn)))
               (dotimes (i 60)
                 (when (> (jd-gcn) g0) (return nil))
                 (dotimes (j 100000) (make-list 40)))
               (list 'mid-sym "mid-str")))
    (t (c) :ERR))
  '(mid-sym "mid-str"))

(eval '(defun jd-gcfn (x) (+ (* x 3) 1)))
(jd-assert "d222-gc-before"  (eval '(jd-gcfn 10)) 31)
(jd-gc-fired "d222-gc-fired1")
(jd-assert "d222-gc-after"   (eval '(jd-gcfn 10)) 31)
(jd-assert "d222-gc-id"      (eq (function jd-gcfn) (symbol-function 'jd-gcfn)) t)
(jd-tag-into (symbol-function 'jd-gcfn))
(jd-assert "d222-gc-tag"     *jd222-tag* 3)
(jd-gc-fired "d222-gc-fired2")
(jd-assert "d222-gc-after2"  (funcall (symbol-function 'jd-gcfn) 10) 31)
;; the trampoline-retaining (const-bearing) sibling must survive GC too
(jd-assert "d222-gc-const"   (eval '(jd-conststr 7)) "v=7")

;; --- arg-passing breadth over the native ABI -------------------------------
(eval '(defun jd-args6 (a b c d e f) (list a b c d e f)))
(eval '(defun jd-argopt (a &optional (b 10)) (+ a b)))
(eval '(defun jd-argrest (a &rest r) (cons a r)))
(eval '(defun jd-argkey (a &key (k 3)) (* a k)))
(jd-assert "d222-args6"   (eval '(jd-args6 1 2 3 4 5 6)) '(1 2 3 4 5 6))
(jd-assert "d222-argopt"  (eval '(list (jd-argopt 1) (jd-argopt 1 2))) '(11 3))
(jd-assert "d222-argrest" (eval '(jd-argrest 1 2 3)) '(1 2 3))
(jd-assert "d222-argkey"  (eval '(list (jd-argkey 2) (jd-argkey 2 :k 5))) '(6 10))
;; multiple values out of a natively installed runtime function
(eval '(defun jd-mvfn (x) (values x (* x 2) (* x 3))))
(jd-assert "d222-mv"      (eval '(multiple-value-list (jd-mvfn 4))) '(4 8 12))
(jd-check  "d222-mv-check" '(multiple-value-list (jd-mvfn 5)))
;; a runtime function that SIGNALS must still propagate (no double execution)
(eval '(defun jd-errfn (x) (if (< x 0) (error "neg") (* x 2))))
(jd-assert "d222-err-ok"  (eval '(jd-errfn 3)) 6)
(jd-assert "d222-err-sig"
           (handler-case (eval '(jd-errfn -1)) (error (c) :caught)) :caught)
;; closures returned by a natively installed runtime function
(eval '(defun jd-mkadder (n) (lambda (x) (+ x n))))
(jd-assert "d222-closure" (eval '(funcall (jd-mkadder 10) 5)) 15)
(jd-assert "d222-closure2"
           (eval '(let ((a (jd-mkadder 1)) (b (jd-mkadder 100)))
                    (list (funcall a 1) (funcall b 1))))
           '(2 101))

;; --- DOUBLE-EXECUTION across the native-install boundary -------------------
;; Defining a function is itself a side effect on the global tables, and the
;; install now happens BEFORE %jit-call.  Confirm the form still runs once.
(jd-once "d222-once-defun"
         '(progn (setq *jd-side* (+ *jd-side* 1))
                 (defun jd-once-fn (x) (* x 11))
                 (jd-once-fn 2)) 1)
(jd-once "d222-once-call"
         '(progn (setq *jd-side* (+ *jd-side* 1)) (jd-n1 3)) 1)
(jd-once "d222-once-redef"
         '(progn (setq *jd-side* (+ *jd-side* 1))
                 (defun jd-once-fn (x) (* x 12))
                 (jd-once-fn 2)) 1)

;; Side-effect / double-execution probes (evaluated ONCE each, JIT live).
(jd-once "side-single" '(setq *jd-side* (+ *jd-side* 1)) 1)
(jd-once "side-progn"  '(progn (setq *jd-side* (+ *jd-side* 1))
                         (setq *jd-side* (+ *jd-side* 1)) :done) 2)
(jd-once "side-mv"     '(progn (setq *jd-side* (+ *jd-side* 1)) (values 1 2)) 1)
(jd-once "side-rtfn"   '(progn (setq *jd-side* (+ *jd-side* 1))
                         (defun jd-side-fn (x) x)
                         (jd-side-fn 1)) 1)

;; WS5 #218: the multiple-values side-effect oracle.
(jd-mv-probes)
(jd-mv-stress 400)

(setq *jit-inhibit* nil)

;; The shapes that NEVER reached native code are the JIT-only blocker list.
(format t "~%JD-NEVER-NATIVE:~%")
(dolist (l (reverse *jd-never-native*)) (format t "  ~A~%" l))

(format t "~%JD-TOTAL=~D~%JD-DIVERGE=~D~%JD-NATIVE=~D~%JD-BOTH-SIGNALLED=~D~%"
        *jd-total* *jd-diverge* *jd-native* *jd-both-err*)
;; WS5 #218 proof: MV forms must reach native AND the MV fallback must be 0.
;; WS5 #222: native runtime-DEFUN installation.
(format t "JD222-TOTAL=~D~%JD222-FAIL=~D~%JD222-EARLYBIND=~D~%JD222-INSTALLED=~D~%"
        *jd222-total* *jd222-fail* *jd222-earlybind*
        (if (boundp '*jit-native-defun-count*)
            (or *jit-native-defun-count* 0) 0))
;; JD-GCSTALE-HITS is the headline of this file's GC work: how many probes saw a
;; const go stale across a REAL collection.  0 = the const pool survives GC.
(format t "JD-GCSTALE-PROBES=~D~%JD-GCSTALE-HITS=~D~%" *jd-gcstale* *jd-gcstale-hits*)
;; The mid-form-collection residual (see jd-note-gcresid): same root cause,
;; NOT covered by the page-level const gate, closed only by task #226.
(format t "JD-GCRESID-PROBES=~D~%JD-GCRESID-HITS=~D~%"
        *jd-gcresid* *jd-gcresid-hits*)
;; Runtime macro REDEFINITION vs *mvm-eval-cache* (see jd-note-macro-redef):
;; a separate, pre-existing, non-GC defect, reported so it is not confused
;; with either of the two above.
(format t "JD-MACRO-REDEF-PROBES=~D~%JD-MACRO-REDEF-HITS=~D~%"
        *jd-macro-redef* *jd-macro-redef-hits*)
(format t "JD-MV-TOTAL=~D~%JD-MV-NATIVE=~D~%JD-MV-CL-GAP=~D~%JD-MV-FALLBACK=~D~%"
        *jd-mv-total* *jd-mv-native* *jd-mv-clgap*
        (if (boundp '*jit-mv-fallback-count*) (or *jit-mv-fallback-count* 0) 0))
(format t "JD-~A~%" (if (eql *jd-diverge* 0) "OK" "FAIL"))
