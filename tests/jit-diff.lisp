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

;; Side-effect / double-execution probes (evaluated ONCE each, JIT live).
(jd-once "side-single" '(setq *jd-side* (+ *jd-side* 1)) 1)
(jd-once "side-progn"  '(progn (setq *jd-side* (+ *jd-side* 1))
                         (setq *jd-side* (+ *jd-side* 1)) :done) 2)
(jd-once "side-mv"     '(progn (setq *jd-side* (+ *jd-side* 1)) (values 1 2)) 1)
(jd-once "side-rtfn"   '(progn (setq *jd-side* (+ *jd-side* 1))
                         (defun jd-side-fn (x) x)
                         (jd-side-fn 1)) 1)

(setq *jit-inhibit* nil)

;; The shapes that NEVER reached native code are the JIT-only blocker list.
(format t "~%JD-NEVER-NATIVE:~%")
(dolist (l (reverse *jd-never-native*)) (format t "  ~A~%" l))

(format t "~%JD-TOTAL=~D~%JD-DIVERGE=~D~%JD-NATIVE=~D~%JD-BOTH-SIGNALLED=~D~%"
        *jd-total* *jd-diverge* *jd-native* *jd-both-err*)
(format t "JD-~A~%" (if (eql *jd-diverge* 0) "OK" "FAIL"))
