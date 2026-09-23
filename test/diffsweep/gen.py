#!/usr/bin/env python3
"""Generate test/diffsweep/sweep.lisp -- one file that runs identically under
SBCL and modus.  Each operation is a tiny DEFUN; the cases call it with
arguments supplied at RUNTIME, so the compiler cannot fold anything away and
the code under test is the op's own compiled body.  Output is "<index> <result>"
so printer differences in the ARGUMENTS cannot create noise; keys.txt maps an
index back to its case."""
import itertools, sys

MPF = 4611686018427387903       # most-positive-fixnum, identical on both
MNF = -4611686018427387904
INTS = [0, 1, -1, 2, -2, 3, 7, -7, 100, -100, 12345, -12345,
        MPF, MNF, MPF - 1, MNF + 1, 2**62, -(2**62), 2**63, 2**64, -(2**64),
        10**20, -(10**20)]
SMALL = [0, 1, -1, 2, -2, 7, -7, 100, -100]
COUNTS = [-70, -64, -63, -62, -33, -32, -31, -2, -1, 0, 1, 2, 31, 32, 33, 62, 63, 64, 70]
RATS = ["1/3", "-1/3", "7/2", "-7/2", "22/7", "-5/9"]
FLOATS = ["1.5", "-1.5", "2.25", "-2.25", "0.5", "100.75", "-0.25"]

ops = []   # (name, lambda-list, body)
cases = [] # (name, [arg-literals])

def op(name, params, body):
    ops.append((name, params, body))

def lit(x):
    return str(x)

# ---- integer binary ops, over the full interesting-integer cross product
for nm, body in [("add", "(+ a b)"), ("sub", "(- a b)"), ("mul", "(* a b)"),
                 ("div", "(/ a b)"),
                 ("trunc2", "(truncate a b)"), ("floor2", "(floor a b)"),
                 ("ceil2", "(ceiling a b)"), ("round2", "(round a b)"),
                 ("mod", "(mod a b)"), ("rem", "(rem a b)"),
                 ("logand", "(logand a b)"), ("logior", "(logior a b)"),
                 ("logxor", "(logxor a b)"), ("gcd", "(gcd a b)"),
                 ("lcm", "(lcm a b)"), ("min", "(min a b)"), ("max", "(max a b)"),
                 ("eq=", "(= a b)"), ("neq", "(/= a b)"), ("lt", "(< a b)"),
                 ("gt", "(> a b)"), ("le", "(<= a b)"), ("ge", "(>= a b)"),
                 ("eql", "(eql a b)")]:
    op("i-" + nm, "(a b)", body)
    for a, b in itertools.product(INTS, INTS):
        cases.append(("i-" + nm, [lit(a), lit(b)]))

# ---- small-operand variants: the fixnum fast paths are where inline codegen lives
for nm, body in [("add", "(+ a b)"), ("sub", "(- a b)"), ("mul", "(* a b)"),
                 ("trunc2", "(truncate a b)"), ("floor2", "(floor a b)"),
                 ("mod", "(mod a b)"), ("rem", "(rem a b)")]:
    op("k-" + nm, "(a b)", body)
    for a, b in itertools.product(SMALL, SMALL):
        cases.append(("k-" + nm, [lit(a), lit(b)]))

# ---- shifts
op("ash", "(a n)", "(ash a n)")
for a, n in itertools.product(INTS, COUNTS):
    cases.append(("ash", [lit(a), lit(n)]))
# constant-count ash inside the op: the documented compile-ash inline path
for k in [1, 2, 3, 30, -1, -2, -3, -30]:
    op(f"ashk{k}".replace("-", "m"), "(a)", f"(ash a {k})")
    for a in INTS:
        cases.append((f"ashk{k}".replace("-", "m"), [lit(a)]))

# ---- unary integer ops
for nm, body in [("neg", "(- a)"), ("abs", "(abs a)"), ("inc", "(1+ a)"),
                 ("dec", "(1- a)"), ("lognot", "(lognot a)"),
                 ("intlen", "(integer-length a)"), ("logcount", "(logcount a)"),
                 ("evenp", "(evenp a)"), ("oddp", "(oddp a)"), ("zerop", "(zerop a)"),
                 ("plusp", "(plusp a)"), ("minusp", "(minusp a)"),
                 ("signum", "(signum a)"), ("trunc1", "(truncate a)"),
                 ("floor1", "(floor a)"), ("sq", "(* a a)"),
                 ("fixnump", "(typep a 'fixnum)"), ("integerp", "(integerp a)"),
                 ("str", "(write-to-string a)"), ("strx", "(write-to-string a :base 16)")]:
    op("u-" + nm, "(a)", body)
    for a in INTS:
        cases.append(("u-" + nm, [lit(a)]))
op("isqrt", "(a)", "(isqrt a)")
for a in [x for x in INTS if x >= 0]:
    cases.append(("isqrt", [lit(a)]))
op("expt", "(a n)", "(expt a n)")
for a, n in itertools.product(SMALL + [12345, 2**62], [0, 1, 2, 3, 5, 10, 64]):
    cases.append(("expt", [lit(a), lit(n)]))

# ---- rationals
for nm, body in [("r-add", "(+ a b)"), ("r-mul", "(* a b)"), ("r-div", "(/ a b)"),
                 ("r-lt", "(< a b)"), ("r-trunc2", "(truncate a b)"),
                 ("r-floor2", "(floor a b)")]:
    op(nm, "(a b)", body)
    for a, b in itertools.product(RATS + ["2", "-3"], RATS + ["2", "-3"]):
        cases.append((nm, [a, b]))
for nm, body in [("r-num", "(numerator a)"), ("r-den", "(denominator a)"),
                 ("r-trunc1", "(truncate a)"), ("r-floor1", "(floor a)"),
                 ("r-ceil1", "(ceiling a)"), ("r-round1", "(round a)"),
                 ("r-rationalp", "(rationalp a)")]:
    op(nm, "(a)", body)
    for a in RATS:
        cases.append((nm, [a]))

# ---- floats (expect some printer noise; triage separately)
for nm, body in [("f-add", "(+ a b)"), ("f-mul", "(* a b)"), ("f-lt", "(< a b)"),
                 ("f-trunc2", "(truncate a b)"), ("f-floor2", "(floor a b)")]:
    op(nm, "(a b)", body)
    for a, b in itertools.product(FLOATS, FLOATS):
        cases.append((nm, [a, b]))
for nm, body in [("f-trunc1", "(truncate a)"), ("f-floor1", "(floor a)"),
                 ("f-round1", "(round a)"), ("f-floatp", "(floatp a)"),
                 ("f-rational", "(rational a)")]:
    op(nm, "(a)", body)
    for a in FLOATS:
        cases.append((nm, [a]))

# ---- characters
CHARS = ["#\\a", "#\\Z", "#\\0", "#\\9", "#\\Space", "#\\-", "#\\z"]
for nm, body in [("c-code", "(char-code a)"), ("c-up", "(char-upcase a)"),
                 ("c-down", "(char-downcase a)"), ("c-digit", "(digit-char-p a)"),
                 ("c-alpha", "(alpha-char-p a)"), ("c-upperp", "(upper-case-p a)")]:
    op(nm, "(a)", body)
    for a in CHARS:
        cases.append((nm, [a]))
op("c-lt", "(a b)", "(char< a b)")
for a, b in itertools.product(CHARS, CHARS):
    cases.append(("c-lt", [a, b]))

# ---- strings
STRS = ['""', '"a"', '"hello"', '"Hello World"', '"abcabc"', '"12345"', '"-42"']
for nm, body in [("s-len", "(length a)"), ("s-up", "(string-upcase a)"),
                 ("s-rev", "(reverse a)"), ("s-copy", "(copy-seq a)"),
                 ("s-parse", "(parse-integer a :junk-allowed t)"),
                 ("s-first", "(if (> (length a) 0) (char a 0) :empty)"),
                 ("s-sub", "(subseq a (min 1 (length a)))")]:
    op(nm, "(a)", body)
    for a in STRS:
        cases.append((nm, [a]))
for nm, body in [("s-eq", "(string= a b)"), ("s-lt", "(string< a b)"),
                 ("s-cat", "(concatenate 'string a b)"), ("s-search", "(search b a)"),
                 ("s-equal", "(string-equal a b)")]:
    op(nm, "(a b)", body)
    for a, b in itertools.product(STRS, STRS):
        cases.append((nm, [a, b]))

# ---- lists
LISTS = ["'()", "'(1)", "'(1 2 3)", "'(3 1 2)", "'(a b c)", "'(1 (2 3) 4)", "'(1 2 3 4 5 6)"]
for nm, body in [("l-len", "(length a)"), ("l-rev", "(reverse a)"),
                 ("l-last", "(last a)"), ("l-butlast", "(butlast a)"),
                 ("l-copy", "(copy-list a)"), ("l-second", "(second a)"),
                 ("l-nthcdr", "(nthcdr 2 a)"), ("l-rem1", "(remove 1 a)"),
                 ("l-mem2", "(member 2 a)"), ("l-mapinc", "(mapcar (lambda (x) (if (numberp x) (1+ x) x)) a)"),
                 ("l-sort", "(if (every #'numberp a) (sort (copy-list a) #'<) :skip)"),
                 ("l-reduce", "(if (every #'numberp a) (reduce #'+ a) :skip)"),
                 ("l-flat", "(let ((r nil)) (labels ((w (x) (if (consp x) (dolist (y x) (w y)) (push x r)))) (w a)) (nreverse r))")]:
    op(nm, "(a)", body)
    for a in LISTS:
        cases.append((nm, [a]))
for nm, body in [("l-append", "(append a b)"), ("l-union", "(length (union a b))"),
                 ("l-inter", "(length (intersection a b))"), ("l-eq", "(equal a b)")]:
    op(nm, "(a b)", body)
    for a, b in itertools.product(LISTS, LISTS):
        cases.append((nm, [a, b]))

# ---- multiple values, control, closures: the shapes TRUNCATE hid behind
op("mv-bind", "(a b)", "(multiple-value-bind (q r) (floor a b) (list q r (+ q r)))")
op("mv-call", "(a b)", "(multiple-value-call #'list (truncate a b) (floor a b))")
op("mv-nth", "(a b)", "(nth-value 1 (truncate a b))")
op("apply", "(a b)", "(apply #'+ a b '(1 2))")
op("funcall-cl", "(a b)", "(funcall (lambda (x y) (truncate x y)) a b)")
op("setf-v", "(a b)", "(let ((v (make-array 3 :initial-element 0))) (setf (aref v 1) (truncate a b)) (coerce v 'list))")
op("closure", "(a b)", "(let ((f (let ((k a)) (lambda (x) (+ k x))))) (funcall f b))")
op("loop-sum", "(a b)", "(loop for i from a below (+ a (min (abs b) 20)) sum i)")
op("dotimes", "(a b)", "(let ((s 0)) (dotimes (i (min (abs b) 50) s) (setq s (+ s (* i a)))))")
op("case", "(a b)", "(case (mod a 5) (0 :zero) (1 :one) ((2 3) :two-three) (t b))")
op("typecase", "(a b)", "(typecase a (fixnum :fix) (integer :int) (t b))")
for nm in ["mv-bind", "mv-call", "mv-nth", "apply", "funcall-cl", "setf-v",
           "closure", "loop-sum", "dotimes", "case", "typecase"]:
    for a, b in itertools.product(SMALL + [12345, MPF, 2**64], [1, -1, 3, -7, 100]):
        cases.append((nm, [lit(a), lit(b)]))

# ---- op names must be unique: a second DEFUN of one name silently replaces
# the first under last-defun-wins, and the first version of this file had one
# (s-sub, for strings AND for small integers) that read as a modus bug.
_names = [n for n, _, _ in ops]
_dups = sorted({n for n in _names if _names.count(n) > 1})
assert not _dups, "duplicate op names: %s" % _dups

# ---- write
with open(sys.argv[1], "w") as f:
    f.write(";;;; GENERATED by gen.py -- do not edit\n")
    f.write("(defun %sweep-eager-p () (let ((e (ignore-errors (sb-ext:posix-getenv \"EAGER\")))) (and e (string= e \"1\"))))\n")
    for name, params, body in ops:
        f.write(f"(defun sw-{name} {params} (multiple-value-list {body}))\n")
    f.write("(when (and (%sweep-eager-p) (fboundp 'jit-eager)) (funcall 'jit-eager))\n")
    f.write("(defparameter *sweep-cases* '(\n")
    for name, args in cases:
        # quoted list args are written without their quote inside the case list
        a2 = [x[1:] if x.startswith("'") else x for x in args]
        f.write(f" (sw-{name} {' '.join(a2)})\n")
    f.write("))\n")
    f.write("""(let ((i 0))
  (dolist (c *sweep-cases*)
    (let ((r (handler-case (apply (car c) (cdr c)) (error () :err))))
      (format t "~d ~s~%" i r))
    (setq i (+ i 1))))
(format t "SWEEP-DONE~%")
""")
with open(sys.argv[2], "w") as f:
    for i, (name, args) in enumerate(cases):
        f.write(f"{i} ({name} {' '.join(args)})\n")
print(f"{len(ops)} ops, {len(cases)} cases")
