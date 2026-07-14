;;;; build-x64-ql.lisp — Modus x64-linux REPL image with `ql:quickload'.
;;;;
;;;; A native ELF you run as an ordinary Linux process; it drops into
;;;; Modus's own self-hosted CL REPL on stdin (eval = eval2 = compile ->
;;;; MVM bytecode -> interpret).  Baked in on top of the generic runtime:
;;;;   - lib/tar.lisp             (ustar reader)
;;;;   - lib/install-tarball.lisp (untar -> parse .asd -> topo-sort -> eval)
;;;;   - lib/ql-shim.lisp         (the QL package + ql:quickload + the REPL)
;;;;
;;;; "Do something":
;;;;   sbcl --dynamic-space-size 4096 --script mvm/build-x64-ql.lisp   # → ./modus-ql
;;;;   ./modus-ql                                                      # → REPL on stdin
;;;; then, typed / piped into the REPL:
;;;;   (ql:quickload :sha1)          ; loads systems/sha1.tar (offline bundle)
;;;;   (sha1:sha1-hex "abc")         ; => "A9993E364706816ABA3E25717850C26C9CD0D89D"
;;;;
;;;; System location for v1 is OFFLINE: quickload maps  name -> systems/<name>.tar
;;;; under *ql-systems-dir* (default "systems/", relative to the process cwd; set
;;;; MODUS_QL_OUT for the binary path).  A bundled systems/sha1.tar ships in the
;;;; repo.  Network fetch (net/http-client) is a documented follow-up.
;;;;
;;;; This derives from build-generic.lisp (full CL runtime, no baked tests); the
;;;; only differences are the three baked lib/ files above and a driver that
;;;; enters %ql-repl when no argv script is given (argv1 still = load-and-exit).

;;; ============================================================
;;; 1. Load MVM infrastructure (SBCL-side)
;;; ============================================================

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(mvm-load "mvm/repl-source.lisp")

(format t "~%=== Building generic Modus image ===~%")

;;; ============================================================
;;; 2. Read source files as text (SBCL-side)
;;; ============================================================

(defun read-file-text (path)
  (with-open-file (s path :direction :input)
    (let ((text (make-string (file-length s))))
      (subseq text 0 (read-sequence text s)))))

(defun mvm-text (relative-path)
  (let ((path (merge-pathnames relative-path *modus-base*)))
    (modus.mvm::check-parses path)
    (read-file-text path)))

;; Strip `chipz::' / `chipz:' package qualifiers from a source string so the
;; flat-namespace image reader doesn't error `Package CHIPZ does not exist'
;; (which would silently drop the whole enclosing form).  Longer prefix first.
;; Mirrors build-aarch64.lisp's strip-package-prefixes.
(defun %ql-strip-one-prefix (text pfx)
  (let ((result text))
    (loop
      (let ((pos (search pfx result)))
        (unless pos (return result))
        (setf result (concatenate 'string
                                  (subseq result 0 pos)
                                  (subseq result (+ pos (length pfx)))))))))

(defun %ql-strip-chipz (text)
  (%ql-strip-one-prefix (%ql-strip-one-prefix text "chipz::") "chipz:"))

(format t "Reading source files...~%")

(defvar *prelude-source*  (mvm-text "mvm/prelude.lisp"))
(defvar *gc-source*       (mvm-text "mvm/gc.lisp"))
;; MCGC stage-4d pin API + pin-stress probe.  Included ONLY when
;; MODUS_MCGC_PINNING=1; flag-off omits it (byte-identical to canonical).
(defvar *mcgc-pin-source*
  (let ((v (sb-ext:posix-getenv "MODUS_MCGC_PINNING")))
    (if (and v (plusp (length v)) (not (string= v "0")))
        (concatenate 'string (string #\Newline)
                     (mvm-text "mvm/mcgc-pin.lisp") (string #\Newline))
        "")))
(defvar *rt-source*       (mvm-text "mvm/rt.lisp"))
;; STAGE 1 of retiring the tree-walker: ship the MVM ISA + bytecode
;; interpreter into the image so `eval` can eventually = compile→interpret
;; (one semantics, shared with the compiler) rather than the divergent
;; tree-walker.  mvm.lisp = opcode/vreg constants + structs; interp.lisp =
;; the bytecode executor (mvm-interpret).  interp.lisp uses #.+op-nop+
;; read-time eval, resolved by the readers binding *package* to :modus.mvm
;; (cross.lisp check-parses / read-all-forms-with-locations).
(defvar *isa-source*      (mvm-text "mvm/mvm.lisp"))
(defvar *interp-source*   (mvm-text "mvm/interp.lisp"))
;; STAGE 2: the MVM compiler itself, so (mvm-compile-all forms) runs in-image.
(defvar *compiler-source* (mvm-text "mvm/compiler.lisp"))
;; In-image override of ieee-float-bits: the build-time version uses
;; sb-kernel:double-float-* (host-only).  In-image a float is a 2-slot boxed
;; object (hi32/lo32); read them directly.  Appended AFTER the compiler source
;; so it wins (last-defun).  Only matters for compiling FLOAT literals.
(defvar *stage2-float-override* "
(defun ieee-float-bits (f)
  (logior (ash (logand (%prim-aref f 0) 4294967295) 32)
          (logand (%prim-aref f 1) 4294967295)))
;; Read hi/lo 32-bit halves directly from the boxed float's slots.  NEVER
;; combine into a 64-bit integer: for floats >= 2.0 the hi half >= #x40000000,
;; so (ash hi 32) >= 2^62 and Modus's bignum-range ASH is lossy, corrupting
;; the literal's bits at compile time (2.0/9.0/-1.5 all read back as garbage).
;; These two stay <= #xFFFFFFFF, safely in fixnum range.
(defun ieee-float-hi32 (f) (logand (%prim-aref f 0) 4294967295))
(defun ieee-float-lo32 (f) (logand (%prim-aref f 1) 4294967295))
;; Bignum-literal decomposition: read the already-built bignum object's slots
;; directly.  The host recompute path uses (logand value mask62), but the
;; compiled `logand` primop is a raw machine AND of tagged words — for a bignum
;; operand (a heap pointer) that yields garbage (e.g. (logand 2^62 (1- 2^62))
;; returned 66281634333124 instead of 0).  These read the decomposed limbs the
;; host already stored.
(defun %lit-bignum-big-p (value) (big-bignum-p value))
(defun %lit-bn-lo (value) (bignum-lo value))
(defun %lit-bn-hi (value) (bignum-hi value))
(defun %lit-bb-sign (value) (%bb-sign value))
(defun %lit-bb-nlimbs (value) (%bb-nlimbs value))
(defun %lit-bb-limb (value k) (%bb-limb value k))
")
;; STAGE 2 probe: eval2 = compile form to MVM bytecode + interpret it.
;; Closed-world (calls only resolve within the compiled module) — fine for
;; pure-arithmetic forms whose ops inline to MVM opcodes.
;; UNIFIED (post-flip): eval2-forms/eval2 now come from the CANONICAL
;; mvm/eval2.lisp — the same source the ANSI image compiles — instead of the
;; stale inline copy this string used to carry.  Unification was blocked by
;; eval2.lisp's `\"` extraction damage until the flip wave-3 fidelity fix
;; (77cea7c) repaired it; the file is now clean, directly mvm-text-readable.
(defvar *eval2-canonical-source* (mvm-text "mvm/eval2.lisp"))
(defvar *stage2-test-source* "
;; Multiply overflow promotion regression probes (compiled native mul-checked).
(defun %nat-mul-20 () (* 10000000000 10000000000))    ; bignum 10^20
(defun %nat-mul-small () (* 12345 678))               ; fixnum 8369910
;; Runtime EVAL of a defun-with-param then call it; guards the intern
;; composite-key %fixnum-* fix.  Expect 6.
(defun %s2-defun-add () (eval2-forms (list (list (quote defun) (quote g) (list (quote y)) (list (quote +) (quote y) 1)) (list (quote g) 5))))
;; Direct emit+fetch round-trip probe for a u64 immediate (isolates the
;; bytecode encoder/decoder from compile-integer).
(defun %ws1-u64rt (imm)
  (let ((buf (make-mvm-buffer :bytes (make-array 16))))
    (mvm-emit-u64 buf imm)
    (let ((bc (mvm-buffer-used-bytes buf)))
      (list (aref bc 0) (aref bc 1) (aref bc 2) (aref bc 3)
            (aref bc 4) (aref bc 5) (aref bc 6) (aref bc 7)
            (quote =>) (fetch-u64 bc 0)))))
(defun %ws1-rt-neg10 () (%ws1-u64rt -10))
(defun %ws1-rt-pos10 () (%ws1-u64rt 10))
(defun %ws1-ev (v) (eval2-forms (list v)))
(defun %ws1-halves-of (v)
  (list (quote v=) v
        (quote lo) (* (logand v 2147483647) 2)
        (quote hi) (logand (ash v -31) 4294967295)))
(defun %ws1-v2w (v) (%val->word v))
(defun %ws1-w2v (w) (%word->val w))
(defun %ws1-roundtrip (v) (%word->val (%val->word v)))
(defun %s2-if-false () (eval2 (list (quote if) (list (quote >) 1 2) 100 200)))
(defun %s2-eq-false () (eval2 (list (quote if) (list (quote =) 1 2) 100 200)))
(defun %s2-nested () (eval2 (list (quote +) (list (quote *) 2 3) (list (quote -) 10 4))))
(defun %s2-add () (eval2 (list (quote +) 1 2)))
(defun %s2-mul () (eval2 (list (quote *) 3 4)))
(defun %s2-sub () (eval2 (list (quote -) 10 3)))
(defun %s2-if  () (eval2 (list (quote if) (list (quote <) 1 2) 100 200)))
;; STAGE 3 (drop-native model): eval2 calling other BYTECODE functions.
;; A helper defun + an expression that calls it — bytecode->bytecode CALL,
;; no marshalling, one value representation.
(defun %s2-call-helper ()
  ;; (defun sq (x) (* x x)) (sq 7) -> 49
  (eval2-forms (list (list (quote defun) (quote sq) (list (quote x))
                           (list (quote *) (quote x) (quote x)))
                     (list (quote sq) 7))))
(defun %s2-call-two ()
  ;; (defun add3 (a b c) (+ a (+ b c))) (add3 10 20 30) -> 60
  (eval2-forms (list (list (quote defun) (quote add3) (list (quote a) (quote b) (quote c))
                           (list (quote +) (quote a) (list (quote +) (quote b) (quote c))))
                     (list (quote add3) 10 20 30))))
(defun %s2-recursion ()
  ;; (defun fact (n) (if (< n 2) 1 (* n (fact (- n 1))))) (fact 5) -> 120
  (eval2-forms (list (list (quote defun) (quote fact) (list (quote n))
                           (list (quote if) (list (quote <) (quote n) 2)
                                 1
                                 (list (quote *) (quote n)
                                       (list (quote fact) (list (quote -) (quote n) 1)))))
                     (list (quote fact) 5))))
;; WS1.0 spike: the value<->word reinterpret boundary (unified representation).
(defun %ws10-word-of-1 () (%val->word 1))            ; raw word of fixnum 1 = 2
(defun %ws10-fixnum-rt () (%word->val (%val->word 42))) ; round-trip fixnum -> 42
(defun %ws10-cons-rt ()
  ;; THE key test: reinterpret a real cons to its raw word and back; identity
  ;; (eq) must survive -> the boundary is non-copying = no marshalling.
  (let ((c (cons 11 22)))
    (eq c (%word->val (%val->word c)))))
(defun %ws10-cons-rt-car ()
  ;; after the round-trip, the reconstructed cons must still car/cdr correctly.
  (let* ((c (cons 11 22)) (c2 (%word->val (%val->word c))))
    (list (car c2) (cdr c2))))                       ; want (11 22)
(defun %ws10-mutate-through ()
  ;; mutate via the reconstructed pointer; the ORIGINAL must see it (shared,
  ;; not a copy) -> proves heap-sharing / identity end to end.
  (let* ((c (cons 11 22)) (c2 (%word->val (%val->word c))))
    (rplaca c2 99)
    (car c)))                                        ; want 99
;; A real NATIVE runtime function (compiled into the image, in the sft), NOT in
;; any eval2 module — the target for the WS1 runtime-call bridge.
(defun %rt-double (x) (* x 2))
(defun %rt-add3 (a b c) (+ a (+ b c)))
(defun %rt-carplus (c) (+ (car c) (cdr c)))   ; takes a CONS arg
(defun %rt-mkpair (a b) (cons a b))           ; returns a CONS
;; WS1 milestone: eval2 calling a real native runtime function (no marshalling).
(defun %ws1-call-native ()
  (eval2-forms (list (list (quote %rt-double) 21))))      ; want 42
(defun %ws1-call-native3 ()
  (eval2-forms (list (list (quote %rt-add3) 10 20 30))))  ; want 60
(defun %ws1-call-native-nested ()
  ;; native call whose arg is itself a native call -> proves value flow across
  ;; the bridge composes.
  (eval2-forms (list (list (quote %rt-double)
                           (list (quote %rt-double) 5)))))  ; want 20
;; WS1 structure: aligned op-cons/op-car on the real heap.
(defun %ws1-cons-car ()
  ;; pure in-interpreter: op-cons then op-car (no native call) -> 11
  (eval2-forms (list (list (quote car) (list (quote cons) 11 22)))))
(defun %ws1-cons-arg ()
  ;; eval2 builds a CONS and passes it to a NATIVE fn that cars/cdrs it ->
  ;; proves the interpreter's cons is a real native cons (no marshalling) -> 30
  (eval2-forms (list (list (quote %rt-carplus) (list (quote cons) 10 20)))))
(defun %ws1-cons-return ()
  ;; native fn RETURNS a cons; eval2 should hand back a real (7 . 8)
  (eval2-forms (list (list (quote %rt-mkpair) 7 8))))
;; Frontier batch: map what the aligned model handles vs what needs work.
;; Returns a list of results; labels/expected printed from the test script
;; (no string literals here — they would terminate the source-string).
(defun %ws1-try (form)
  (handler-case (eval2-forms (list form))
    (error (e) (list :err e))))
;; WS1 strings/vectors (interpreter-internal, make-mvm-object basis).
(defun %ws1-vec-const ()  ; constant index -> obj-ref
  (eval2-forms (list (list (quote aref) (vector 10 20 30) 1))))         ; 20
(defun %ws1-vec-var ()    ; variable index -> aref opcode
  (eval2-forms (list (list (quote let) (list (list (quote i) 2))
                           (list (quote aref) (vector 10 20 30) (quote i)))))) ; 30
(defun %ws1-str-aref ()   ; string elt via obj-ref + code-char wrap
  (eval2-forms (list (list (quote aref) \"abc\" 1))))                   ; #\b
(defun %ws1-vec-fn-var () ; variable index via function param (isolate let vs aref)
  (eval2-forms (list (list (quote defun) (quote g) (list (quote n))
                           (list (quote aref) (vector 10 20 30) (quote n)))
                     (list (quote g) 2))))                              ; 30
(defun %ws1-aref-raw ()   ; direct aref-opcode test: build vec, var idx from arith
  (eval2-forms (list (list (quote aref) (vector 10 20 30) (list (quote + ) 1 1))))) ; 30
;; WS1 #2: native strings cross the bridge to NATIVE string functions.
(defun %ws1-str-length () (eval2-forms (list (list (quote length) \"hello\"))))  ; 5
(defun %ws1-str-char ()   (eval2-forms (list (list (quote char) \"hello\" 1))))   ; #\e
(defun %ws1-str-upcase () (eval2-forms (list (list (quote string-upcase) \"abc\")))) ; -> ABC
;; NATIVE objects cross the bridge: vector length + FLOAT arithmetic.
(defun %ws1-vec-len ()   (eval2-forms (list (list (quote length) (vector 10 20 30))))) ; 3
(defun %ws1-float-add () (eval2-forms (list (list (quote +) 1.5 2.5))))   ; 4.0
(defun %ws1-float-mul () (eval2-forms (list (list (quote *) 2.0 3.0))))   ; 6.0
(defun %ws1-float-lit () (eval2-forms (list 1.5)))                        ; 1.5 (literal round-trip)
(defun %ws1-float-1arg () (eval2-forms (list (list (quote %rt-double) 1.5))))      ; 3.0 (1 float arg via bridge)
(defun %ws1-float-3arg () (eval2-forms (list (list (quote %rt-add3) 1.0 2.0 3.0)))) ; 6.0 (3 float args)
(defun %ws1-float-2lit () (eval2-forms (list (list (quote list) 1.5 2.5))))         ; (1.5 2.5) build 2 floats
(defun %ws1-2cons () (eval2-forms (list (list (quote cons) 1.0 2.0))))              ; (1.0 . 2.0) 2 floats via cons
(defun %ws1-2cons-fix () (eval2-forms (list (list (quote cons) 100 200))))          ; (100 . 200) 2 fixnums via cons (control)
;; Isolate build-vs-print: these don't PRINT the floats.
(defun %ws1-consp2f () (eval2-forms (list (list (quote consp) (list (quote cons) 1.0 2.0))))) ; T if build ok
(defun %ws1-floatp1 () (eval2-forms (list (list (quote floatp) (list (quote car) (list (quote cons) 1.0 2.0)))))) ; T
(defun %ws1-floatp2 () (eval2-forms (list (list (quote floatp) (list (quote cdr) (list (quote cons) 1.0 2.0)))))) ; T if 2nd float valid
(defun %ws1-eqfloat () (eval2-forms (list (list (quote eql) (list (quote car) (list (quote cons) 5.0 6.0)) 5.0)))) ; T if 1st = 5.0
(defun %ws1-eql-same () (eval2-forms (list (list (quote eql) 5.0 5.0))))   ; T? (eql semantics on 2 float literals)
(defun %ws1-car-gt () (eval2-forms (list (list (quote >) (list (quote car) (list (quote cons) 9.0 2.5)) 1.0)))) ; T (car 9.0 > 1.0)
(defun %ws1-cdr-gt () (eval2-forms (list (list (quote >) (list (quote cdr) (list (quote cons) 9.0 2.5)) 1.0)))) ; T (cdr 2.5 > 1.0); NIL if cdr=0.0
(defun %ws1-gt-ff () (eval2-forms (list (list (quote >) 9.0 1))))   ; T : ONE float (9.0) vs fixnum 1
(defun %ws1-lt-ff () (eval2-forms (list (list (quote <) 1 9.0))))   ; T : fixnum 1 vs ONE float
(defun %ws1-fp9 () (eval2-forms (list (list (quote floatp) 9.0))))  ; T : single float valid
(defun %ws1-int9 () (eval2-forms (list (list (quote truncate) 9.0)))) ; 9 : single-float value check via truncate
(defun %ws1-lit9 () (eval2-forms (list 9.0)))    ; 9.0 ? (hi bit31 set -> suspect 0.0)
(defun %ws1-lit2 () (eval2-forms (list 2.0)))    ; 2.0 ? (hi=0x40000000, hi<<1 bit31 set)
(defun %ws1-lit05 () (eval2-forms (list 0.5)))   ; 0.5 (hi<<1 bit31 NOT set -> expect ok)
(defun %ws1-lit-neg () (eval2-forms (list -1.5))) ; -1.5
(defun %ws1-2to31 () (eval2-forms (list 2147483648)))   ; 2^31 = 0x80000000 (bit31 set) -> round-trip?
(defun %ws1-2to31m () (eval2-forms (list 2147483647)))  ; 2^31-1 (bit31 NOT set) control
(defun %ws1-bigfix () (eval2-forms (list 2151677952)))  ; 0x80440000 (the 9.0 hi<<1)
(defun %ws1-hival () (eval2-forms (list 1075843072)))   ; 0x40220000 = 9.0 hi VALUE; LI imm = 0x80440000 (bit31)
(defun %ws1-hi2 () (eval2-forms (list 1073741824)))     ; 0x40000000 = 2.0 hi VALUE; LI imm = 0x80000000 (bit31)
;; Ratios (subtag #x33) and bignums (subtag #x30): same native-alloc + obj-set
;; path as floats — validate they round-trip and arithmetic builds them.
(defun %ws1-rat-lit () (eval2-forms (list 1/2)))                         ; 1/2 literal round-trip
(defun %ws1-rat-lit2 () (eval2-forms (list 3/4)))                        ; 3/4 literal
(defun %ws1-rat-div () (eval2-forms (list (list (quote /) 4 3))))        ; 4/3 via division
(defun %ws1-rat-num () (eval2-forms (list (list (quote numerator) 3/4)))) ; 3
(defun %ws1-rat-den () (eval2-forms (list (list (quote denominator) 3/4)))) ; 4
(defun %ws1-big-lit () (eval2-forms (list 4611686018427387904)))        ; 2^62 bignum literal
(defun %ws1-big-mul () (eval2-forms (list (list (quote *) 1000000000000 1000000000000)))) ; 10^24 bignum
(defun %ws1-big-add () (eval2-forms (list (list (quote +) 4611686018427387904 1)))) ; 2^62+1
(defun %ws1-big130 () (eval2-forms (list 1361129467683753853853498429727072845824))) ; 2^130 big-bignum literal
(defun %ws1-big-neg () (eval2-forms (list -4611686018427387904)))                     ; -2^62 negative small bignum
(defun %ws1-big-neg130 () (eval2-forms (list -1361129467683753853853498429727072845824))) ; -2^130 negative big-bignum
(defun %ws1-neg5 () (eval2-forms (list -5)))                      ; negative FIXNUM literal round-trip
(defun %ws1-neg-sub () (eval2-forms (list (list (quote -) 3 8)))) ; -5 via subtraction
(defun %ws1-neg-big () (eval2-forms (list -1000000)))             ; -10^6 negative fixnum
;; GC-stress: build an N-element list via interpreter recursion (op-cons each
;; step), holding the growing list `a` in a register across every allocation.
;; With early GC forced (MODUS_GC_R14 small), collections fire MID-eval; if the
;; regs are GC-safe (slots hold real values the collector traces+updates), the
;; held list survives and length is exact.  A stale-pointer bug would corrupt or
;; crash.
(defun %ws1-gc-stress (n)
  (eval2-forms (list (list (quote defun) (quote bld) (list (quote k) (quote a))
                           (list (quote if) (list (quote =) (quote k) 0)
                                 (quote a)
                                 (list (quote bld) (list (quote -) (quote k) 1)
                                       (list (quote cons) (quote k) (quote a)))))
                     (list (quote length) (list (quote bld) n (quote nil))))))
(defun %ws1-frontier ()
  (list
    (%ws1-try (list (quote quote) (list 1 2 3)))                                ; (1 2 3)
    (%ws1-try (list (quote car) (list (quote quote) (list 7 8 9))))             ; 7
    (%ws1-try (list (quote cons) 3 nil))                                        ; (3)
    (%ws1-try (list (quote let) (list (list (quote x) 5)) (list (quote *) (quote x) (quote x)))) ; 25
    (%ws1-try (list (quote let) (list (list (quote a) 3) (list (quote b) 4))
                    (list (quote +) (list (quote *) (quote a) (quote a)) (list (quote *) (quote b) (quote b))))) ; 25
    (%ws1-try (list (quote progn) 1 2 3))                                       ; 3
    (%ws1-try (list (quote list) 1 2 3))                                        ; (1 2 3)
    (%ws1-try (list (quote length) (list (quote quote) (list 1 2 3))))          ; 3
    (%ws1-try (list (quote reverse) (list (quote quote) (list 1 2 3))))         ; (3 2 1)
    (%ws1-try (list (quote append) (list (quote quote) (list 1 2)) (list (quote quote) (list 3 4)))))) ; (1 2 3 4)
;; Hand-built bytecode to isolate inline ADD/SUB/MUL opcodes in-image,
;; bypassing the compiler's fast-path branch structure.
;; (li v0 IMM)(li v1 IMM)(OP v16 v0 v1)(halt).  op-li=17, add=32 sub=33 mul=34,
;; halt=162, vr=16.  Values are TAGGED (n<<1).
(defun %s2-raw-add ()
  ;; (+ 10 3): tagged 20,6 -> add -> 26 -> untag 13
  (ash (mvm-interpret (make-array 25 :initial-contents
        (list 17 0 20 0 0 0 0 0 0 0  17 1 6 0 0 0 0 0 0 0  32 16 0 1  162))) -1))
(defun %s2-raw-sub ()
  ;; (- 10 3): tagged 20,6 -> sub -> 14 -> untag 7
  (ash (mvm-interpret (make-array 25 :initial-contents
        (list 17 0 20 0 0 0 0 0 0 0  17 1 6 0 0 0 0 0 0 0  33 16 0 1  162))) -1))
(defun %s2-raw-mul ()
  ;; (* 3 4): tagged 6,8 -> mul(untag*untag,retag) -> 24 -> untag 12
  (ash (mvm-interpret (make-array 25 :initial-contents
        (list 17 0 6 0 0 0 0 0 0 0  17 1 8 0 0 0 0 0 0 0  34 16 0 1  162))) -1))
")

;; STAGE 2: *opcode-table* is populated on the HOST by mvm.lisp's defopcode
;; toplevel setf forms, but in-image those bare toplevel setfs DON'T run at
;; boot (only def* init-thunks do).  encode-instruction reads the table for
;; each instruction's operand spec — an empty table means it emits opcodes
;; with NO operands (truncated bytecode).  Generate a populate-defun from the
;; host table and trigger it via a defparameter init-thunk (those DO run).
(defvar *opcode-table-init-source*
  (with-output-to-string (s)
    (format s "(defun %~A-opcode-table ()~%" "populate")
    (maphash (lambda (code info)
               (format s "  (setf (gethash ~D *opcode-table*) (make-opcode-info :code ~D :name ~S :operands (quote ~S) :description ~S))~%"
                       code code
                       (modus.mvm::opcode-info-name info)
                       (modus.mvm::opcode-info-operands info)
                       (modus.mvm::opcode-info-description info)))
             modus.mvm::*opcode-table*)
    (format s "  t)~%")
    ;; defparameter init-thunk runs %populate-opcode-table at boot, AFTER the
    ;; *opcode-table* defparameter (earlier in source order) creates the table.
    (format s "(defparameter *%opcode-table-ready* (progn (%populate-opcode-table) t))~%")))
;; STAGE 1 diagnostics: real (defun ...) so scan-defuns registers them in the
;; sft, letting runtime EVAL exercise the defstruct + interpreter in isolation.
(defvar *stage1-test-source* "
(defstruct (baz (:conc-name baz-)) (q 77))
(defun %s1-baz () (baz-q (make-baz)))
(defstruct (bar (:conc-name bar-)) (x (make-array 3 :initial-element 0) :type simple-vector) (y 5))
(defun %s1-barx () (bar-x (make-bar)))
(defun %s1-bary () (bar-y (make-bar)))
(defun %s1-mms () (make-mvm-state) 7)
(defun %s1-regs () (mvm-regs (make-mvm-state)))
(defun %s1-flags () (mvm-flags (make-mvm-state)))
(defun %s1-explicit-regs ()
  (mvm-regs (make-mvm-state :regs (make-array 23 :initial-element 0))))
(defun %s1-slot2 () (aref (make-mvm-state) 2))
(defun %s1-mklen () (length (make-array 23 :initial-element 0)))
(defun %s1-interp ()
  (mvm-interpret (make-array 11 :initial-contents (list 17 16 42 0 0 0 0 0 0 0 162))))
")
(defvar *rt-macros-source* (mvm-text "mvm/runtime-cl-macros.lisp"))
(defvar *bridge-source*
  (concatenate 'string
    (mvm-text "mvm/cl-sequences.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-streams.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-fileio.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-printer.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-reader.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-eval.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-clos.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-types.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-packages.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-conditions.lisp")
    (string #\Newline)
    (mvm-text "mvm/ansi-bridge.lisp")
    (string #\Newline)
    ;; POSIX ustar tar reader (pure CL, no FFI) — Quicklisp-client tar support.
    (mvm-text "lib/tar.lisp")
    (string #\Newline)
    ;; install-tarball.lisp (untar -> parse .asd -> topo-sort -> eval each form)
    ;; + the ql:quickload client shim, both BAKED IN (mirrors the aarch64 net
    ;; build's proven recipe).  install-tarball references `chipz:decompress'
    ;; for the .tar.gz path; the flat image has no CHIPZ package, so the
    ;; build-time reader would error `Package CHIPZ does not exist' and SILENTLY
    ;; DROP the whole form.  We strip the `chipz:'/`chipz::' prefixes so the
    ;; reference collapses to a bare `decompress' symbol.  v1 ships PLAIN .tar
    ;; archives, so decompress is never actually called (install-tarball only
    ;; calls it when the gzip magic 1f 8b is present); it resolves to the
    ;; unresolved-fn sentinel and stays dormant.  Network gzip fetch is a
    ;; documented follow-up.
    ;; NB: read via read-file-text (NOT mvm-text) — mvm-text's check-parses
    ;; uses the host SBCL reader, which errors on the `chipz:' qualifier before
    ;; we get to strip it.  The stripped text is well-formed and the in-build
    ;; MVM reader parses it fine.
    (%ql-strip-chipz (read-file-text (merge-pathnames "lib/install-tarball.lisp"
                                                      *modus-base*)))
    (string #\Newline)
    (mvm-text "lib/ql-shim.lisp")))
;; WS3 STEP 4b (2026-07-09): mvm/tree-walker.lisp is NO LONGER part of this
;; image — production eval is eval2 only.  The full-corpus + gauntlet census
;; measured ZERO %e2ic walker-fallback hits (the earlier "-142 fallback
;; inventory" was the :li-func offset-0 phantom, fixed in a07fe7d), and the
;; walker-free image gates clean (16335-16336 / CHUNK-CRASH=0 / FILE-WEDGE=30,
;; gauntlet 243/243 x2 at 11 FAILFORMs).  tree-walker.lisp remains the eval
;; engine of the four legacy fork builds ONLY.  If %e2ic-compile ever fails on
;; a new shape it signals honestly (UNDEFINED-FUNCTION via the NIL fn sentinel).

(format t "  prelude: ~D chars~%" (length *prelude-source*))
(format t "  gc:      ~D chars~%" (length *gc-source*))
(format t "  rt:      ~D chars~%" (length *rt-source*))
(format t "  bridge:  ~D chars~%" (length *bridge-source*))

;;; ============================================================
;;; 3. Build-time scanners (same as build-x64-linux) so runtime LOAD
;;; can find every defun's source — without these the symbol-function
;;; table only knows the ~229 hand-curated entries from
;;; cl-eval.lisp's %init-sft-list and runtime-EVAL of any other name
;;; resolves to %%unresolved-fn.
;;; ============================================================

(format t "Generating sft-auto / sym-name-auto / runtime-macros init...~%")

;; sft-auto: collect every (defun NAME ...) form's NAME across all
;; runtime source and emit (puthash "NAME" *symbol-function-table*
;; #'NAME) calls so the runtime can resolve any defun by name.

(defun scan-defuns (text)
  "Return list of defun names (strings) found in TEXT."
  (let ((names nil)
        (pos 0))
    (loop
      (let ((p (search "(defun " text :start2 pos)))
        (unless p (return (nreverse names)))
        (let* ((start (+ p 7))
               (end (or (position-if (lambda (c)
                                       (or (char= c #\Space)
                                           (char= c #\Newline)
                                           (char= c #\()
                                           (char= c #\)))) text
                                     :start start)
                        (length text))))
          (push (string-upcase (subseq text start end)) names)
          (setq pos end))))))

;; Driver source is concatenated into the image AND scanned for
;; defun / symbol names, so defuns added in the driver (sys-exit,
;; %argv1, runtime-bq-expand, etc.) land in *symbol-function-table*
;; just like the CL runtime ones.  Forward-declared here; the actual
;; content lives below (section 4).  See the trailing setf-via-symbol-
;; value-set trick to overwrite this without re-reading the file.
(defvar *driver-source* "
(defun sys-exit (code)
  (let ((c code))
    (syscall3 60 c 0 0)))
(defun halt ()
  (syscall3 60 1 0 0))
(defun %rbq-sym-name-eq (sym name)
  (and (symbolp sym) (string= (symbol-name sym) name)))
;; Level-tracking backquote expander.  LEVEL counts open backquotes
;; whose commas are still pending; the entry from the macro is LEVEL 1.
;; A COMMA at LEVEL 1 unquotes (its expr stays live); a COMMA at deeper
;; LEVEL is data that drops one level — this is what makes the classic
;; `,',x / `,,x tunnelling work (a `(... `(... ,',def ...)) form).  A
;; nested BACKQUOTE bumps the level by one for its template.
(defun runtime-bq-expand (template) (%rbq template 1))
(defun %rbq (template level)
  (cond
    ((null template) nil)
    ((atom template) (list 'quote template))
    ;; nested COMMA
    ((%rbq-sym-name-eq (car template) \"COMMA\")
     (if (= level 1)
         (cadr template)
         (list 'list (list 'quote 'comma) (%rbq (cadr template) (- level 1)))))
    ((%rbq-sym-name-eq (car template) \"COMMA-AT\")
     (if (= level 1)
         (cadr template)
         (list 'list (list 'quote 'comma-at) (%rbq (cadr template) (- level 1)))))
    ((%rbq-sym-name-eq (car template) \"COMMA-DOT\")
     (if (= level 1)
         (cadr template)
         (list 'list (list 'quote 'comma-dot) (%rbq (cadr template) (- level 1)))))
    ;; nested BACKQUOTE — descend one deeper level, rebuild the marker
    ((%rbq-sym-name-eq (car template) \"BACKQUOTE\")
     (list 'list (list 'quote 'backquote) (%rbq (cadr template) (+ level 1))))
    (t (%rbq-list template level))))
(defun %rbq-list (lst level)
  (cond
    ((null lst) (list 'quote nil))
    ((not (consp lst)) (%rbq lst level))
    ;; a dotted/atom whole-form COMMA tail like `(a . ,b)
    ((%rbq-sym-name-eq (car lst) \"COMMA\")
     (if (= level 1)
         (cadr lst)
         (%rbq lst level)))
    (t
     (let ((first (car lst)) (rest (cdr lst)))
       (cond
         ((and (consp first) (%rbq-sym-name-eq (car first) \"COMMA-AT\") (= level 1))
          (list 'append2 (cadr first) (%rbq-list rest level)))
         ((and (consp first) (%rbq-sym-name-eq (car first) \"COMMA-DOT\") (= level 1))
          (list 'append2 (cadr first) (%rbq-list rest level)))
         (t
          (list 'cons (%rbq first level)
                (%rbq-list rest level))))))))
;; COMPILE-TIME closure, NOT (eval '(lambda …)): this install runs at boot in
;; kernel-main, and under WS3 Phase-3 production EVAL is eval2 — a boot-time
;; eval2 of the lambda runs before init-all-globals (eval2's state defvars all
;; NIL) and silently produced a broken expander (callee resolution fell into
;; the :li-func offset-0 fallback), so every runtime defmacro with a backquote
;; body \"expanded\" to its raw (BACKQUOTE …) template — uiop define-package
;; became a silent no-op and the asdf gauntlet died at the first read-time #.
;; that depended on an earlier defparameter.  The historical eval-based install
;; only worked because boot-time eval used to be the tree-walker (flag NIL).
;; Convention: non-interp-closure *macro-function-table* entries are funcalled
;; with the WHOLE form — (cadr mform) is the template.
(defun %install-runtime-backquote ()
  (set-macro-function 'backquote
                      (function (lambda (mform) (runtime-bq-expand (cadr mform))))))
(defun %argv-string-at (addr)
  (let ((len 0))
    (let ((i 0))
      (loop
        (let ((b (mem-ref (+ addr i) :u8)))
          (when (= b 0) (return nil))
          (setq i (+ i 1)))
        (setq len i)))
    (if (zerop len) nil
        (let ((s (%make-string-array len)) (i 0))
          (loop
            (when (>= i len) (return s))
            (aset s i (mem-ref (+ addr i) :u8))
            (setq i (+ i 1)))))))
(defun %argv1 () (%argv-string-at #x10000208))
(defun %argv2 () (%argv-string-at #x10000248))
(defun %argc  () (mem-ref #x10000200 :u32))
;; Native diagnostic probes for the handler-frame chain (real RAM — an
;; interpreted mem-ref only sees the interp's per-state simulated memory
;; hash, so scripts must call these NATIVE fns to observe [#x10000400]
;; (handler-stack depth) and [#x10000180] (current armed frame RSP)).
(defun %hc-depth () (mem-ref #x10000400 :u32))
(defun %hc-armed-p () (if (eql (mem-ref #x10000180 :u32) 0) nil t))
;; Saved resume-IP (low 32 bits) of stacked frame N / the current frame —
;; code addrs are < 4GB so :u32 (tagged load) is exact.
(defun %hc-frame-ip (n) (mem-ref (+ #x10000408 (* 32 n) 16) :u32))
(defun %hc-cur-ip () (mem-ref #x10000190 :u32))
(defun kernel-main ()
  (init-symbol-table)
  (init-keyword-table)
  (%init-packages)
  (%init-streams)
  (%init-reader)
  (%init-condition-types)
  (%init-method-combinations)
  (%init-symbol-function-table)
  (%init-sft-auto)
  (setq *sym-name-table* (make-hash-table))
  (%init-sym-name-auto)
  (setq *macro-table* (make-hash-table))
  (%init-runtime-macros)
  (setq *cstr-scratch* #x0FE00000)  ; moved below heap base
  (setq *io-buf-addr*  #x0FF00000)  ; moved out of heap semispace 0; see memory note
  (%init-signal-handling)
  (%init-signal-symbols)
  (%init-make-load-form)
  (%install-runtime-backquote)
  ;; Init RT counters + registry (defvar init thunks don't run on bare metal)
  (setq *rt-test-count* 0)
  (setq *rt-pass-count* 0)
  (setq *rt-fail-count* 0)
  (setq *rt-registered-tests* nil)
  (%install-deftest-macro)
  ;; Run all built-in defvar init thunks.  Each is wrapped in
  ;; handler-case at compile time so a thunk that references a not-yet-
  ;; bound symbol can't kill the chain — see CLAUDE.md known limitation
  ;; #7 history.  Most thunks succeed and we get init values for free.
  (init-all-globals)
  ;; ANSI numeric/array constants whose DEFCONSTANT init thunks don't run
  ;; at boot (limitation #7).  The x64-linux gate image sets these in its
  ;; own kernel-main; build-generic relied only on init-all-globals, so
  ;; array-dimension-limit et al. were UNBOUND — third-party code that
  ;; reads them at read-time (chipz types-and-tables.lisp:
  ;; (deftype index () '(mod #.array-dimension-limit))) got UNBOUND-VARIABLE
  ;; during READ, silently dropping the whole file.
  (setq array-total-size-limit  (ash 1 24))
  (setq array-dimension-limit   (ash 1 24))
  (setq array-rank-limit        256)
  (setq call-arguments-limit    256)
  (setq lambda-parameters-limit 256)
  (setq pi 3.141592653589793d0)
  ;; AFTER init-all-globals — overrides defvar's init.  *write-object-
  ;; budget* defvars to 0 which immediately exhausts; we want a huge
  ;; budget so test names print fully.
  (setq *write-object-budget* 1000000)
  ;; Runtime CL macros (when/unless/setf/incf/case/dolist/etc.) — must
  ;; come AFTER init-all-globals so *modus-runtime-macros* has its
  ;; defvar value before we walk it.  Outer handler-case in case a
  ;; macro source string fails to parse (the install fn itself doesn't
  ;; wrap — see %install-runtime-cl-macros docstring).
  (handler-case (%install-runtime-cl-macros) (t (c) nil))
  ;; --- ql:quickload wiring -------------------------------------------------
  ;; defvar init-thunks don't run at boot (MVM Active Limitation 7), so set the
  ;; globals these paths depend on explicitly.
  (setq *tar-block-size* 512)
  (setq *ql-systems-dir* \"systems/\")
  ;; Create the QL package + bind QL:QUICKLOAD before the reader ever sees a
  ;; `ql:...' token (the REPL, or a script).
  (%ql-init)
  ;; --- entry: script mode (argv1) or interactive REPL ----------------------
  (let ((path (%argv1)))
    (cond
      ((null path)
       ;; No script argument: drop into the interactive stdin REPL.
       (handler-case (%ql-repl) (t (c) nil))
       (sys-exit 0))
      (t
       (handler-case
           (progn (load path) (sys-exit 0))
         (t (c)
            (write-string-serial \"unhandled condition while loading: \")
            (handler-case (write-string-serial path) (t (c) nil))
            (write-char-serial 10)
            (sys-exit 1)))))))
")

(defvar *all-runtime-source*
  (concatenate 'string *prelude-source*  (string #\Newline)
                       *gc-source*       (string #\Newline)
                       *mcgc-pin-source*
                       *rt-source*       (string #\Newline)
                       ;; STAGE 1: so the sft-auto scanner registers the
                       ;; interpreter's defuns (mvm-interpret, make-mvm-state,
                       ;; …) in *symbol-function-table* — without this they
                       ;; compile into the image but are unreachable from
                       ;; runtime EVAL.
                       *isa-source*      (string #\Newline)
                       *interp-source*   (string #\Newline)
                       *stage1-test-source* (string #\Newline)
                       *compiler-source* (string #\Newline)
                       *stage2-float-override* (string #\Newline)
                       *opcode-table-init-source* (string #\Newline)
                       *eval2-canonical-source* (string #\Newline)
                       *stage2-test-source* (string #\Newline)
                       *rt-macros-source* (string #\Newline)
                       *bridge-source*   (string #\Newline)
                       *driver-source*))

(defvar *all-defun-names*
  ;; Filter to names that look like valid CL identifiers.
  (remove-if-not (lambda (s)
                   (and (stringp s)
                        (> (length s) 0)
                        (every (lambda (c)
                                 (or (alphanumericp c)
                                     (find c "+-*/<=>?!@$%^&_:|.~")))
                               s)))
                 (scan-defuns *all-runtime-source*)))

(format t "  defuns found: ~D~%" (length *all-defun-names*))

;; Emit %init-sft-auto in chunks of ~200 to avoid the compiler's
;; function-size limits.

(defun emit-sft-auto (names chunk-size)
  (with-output-to-string (out)
    (let ((n-chunks (ceiling (length names) chunk-size)))
      (dotimes (c n-chunks)
        (format out "(defun %init-sft-auto-~D ()~%" c)
        (let ((start (* c chunk-size))
              (end (min (* (1+ c) chunk-size) (length names))))
          (loop for i from start below end
                do (format out "  (puthash ~S *symbol-function-table* #'~A)~%"
                          (nth i names) (nth i names))))
        (format out ")~%"))
      (format out "(defun %init-sft-auto ()~%")
      (dotimes (c n-chunks)
        (format out "  (%init-sft-auto-~D)~%" c))
      (format out ")~%"))))

(defvar *sft-auto-source* (emit-sft-auto *all-defun-names* 200))
(format t "  sft-auto: ~D chars~%" (length *sft-auto-source*))

;; sym-name-auto: collect every symbol NAME mentioned in source so
;; runtime symbol-name can recover the name for a native MVM symbol.

(defun scan-symbol-names (text)
  "Return list of distinct symbol-shaped tokens in TEXT."
  (let ((seen (make-hash-table :test 'equal))
        (result nil)
        (pos 0)
        (len (length text)))
    (loop
      (when (>= pos len) (return (nreverse result)))
      (let ((c (char text pos)))
        (cond
          ((or (char= c #\Space) (char= c #\Newline) (char= c #\Tab)
               (char= c #\Return) (char= c #\Page))
           (incf pos))
          ((char= c #\;)
           ;; comment to end of line
           (loop while (and (< pos len)
                            (not (char= (char text pos) #\Newline)))
                 do (incf pos)))
          ((char= c #\")
           ;; string literal — skip to closing quote
           (incf pos)
           (loop while (and (< pos len) (not (char= (char text pos) #\")))
                 do (when (char= (char text pos) #\\) (incf pos))
                    (incf pos))
           (incf pos))
          ;; Sharp dispatch: #\X character literal, #| ... |# block
          ;; comment, #(...) vector literal, #+/#- feature, etc.  Without
          ;; this special-case, #\" / #\; / #\( etc. fool the string- and
          ;; comment-skippers in the branches above and the scanner ends
          ;; up consuming dozens of legitimate tokens as if they were
          ;; inside a string.  In particular COMMA-AT on line 992 of
          ;; cl-reader.lisp was being eaten by the bogus string-skip
          ;; triggered by #\" on line 957.
          ((char= c #\#)
           (incf pos)
           (when (< pos len)
             (let ((next (char text pos)))
               (cond
                 ;; #\X — skip the backslash + next char (which may be
                 ;; any single character) + any trailing word (e.g.
                 ;; #\Newline / #\Space).
                 ((char= next #\\)
                  (incf pos)  ; past the backslash
                  (when (< pos len) (incf pos))  ; the literal char
                  ;; Multi-char names like Newline / Space / Tab — eat
                  ;; the rest of the word.
                  (loop while (and (< pos len)
                                   (alphanumericp (char text pos)))
                        do (incf pos)))
                 ;; #| ... |# block comment
                 ((char= next #\|)
                  (incf pos)
                  (loop while (and (< (1+ pos) len)
                                   (not (and (char= (char text pos) #\|)
                                             (char= (char text (1+ pos)) #\#))))
                        do (incf pos))
                  (when (< (1+ pos) len) (incf pos) (incf pos))))))
           ;; Other # forms (#( vector, #+ feature, etc.) just fall
           ;; through — the next iteration reads them as ordinary
           ;; tokens.
           )
          ((or (char= c #\() (char= c #\)) (char= c #\') (char= c #\`)
               (char= c #\,))
           (incf pos))
          (t
           ;; symbol token
           (let ((start pos))
             (loop while (and (< pos len)
                              (let ((ch (char text pos)))
                                (not (or (char= ch #\Space) (char= ch #\Newline)
                                         (char= ch #\() (char= ch #\))
                                         (char= ch #\Tab) (char= ch #\")
                                         (char= ch #\;)))))
                   do (incf pos))
             (let ((token (string-upcase (subseq text start pos))))
               (unless (or (gethash token seen)
                           (zerop (length token))
                           (every #'digit-char-p token))
                 (setf (gethash token seen) t)
                 (push token result))
               ;; Also register a keyword token's bare name (without the
               ;; leading colon).  compile-keyword uses (normalize-name
               ;; KW) → hash of "FOO" (not ":FOO"); symbol-name at
               ;; runtime looks up that hash in *sym-name-table*.
               ;; Without this, keywords interned at build-time print
               ;; as :|| because the colon-prefixed entry doesn't match.
               (when (and (> (length token) 1)
                          (char= (char token 0) #\:))
                 (let ((bare (subseq token 1)))
                   (unless (or (gethash bare seen)
                               (zerop (length bare)))
                     (setf (gethash bare seen) t)
                     (push bare result))))))))))))

(defvar *all-symbol-names* (scan-symbol-names *all-runtime-source*))

(format t "  symbol names found: ~D~%" (length *all-symbol-names*))

(defun emit-sym-name-auto (names chunk-size)
  (with-output-to-string (out)
    (let ((n-chunks (ceiling (length names) chunk-size)))
      (dotimes (c n-chunks)
        (format out "(defun %init-sym-name-auto-~D ()~%" c)
        (let ((start (* c chunk-size))
              (end (min (* (1+ c) chunk-size) (length names))))
          (loop for i from start below end
                do (format out "  (puthash (compute-name-hash ~S) *sym-name-table* ~S)~%"
                          (nth i names) (nth i names))))
        (format out ")~%"))
      (format out "(defun %init-sym-name-auto ()~%")
      (dotimes (c n-chunks)
        (format out "  (%init-sym-name-auto-~D)~%" c))
      (format out ")~%"))))

(defvar *sym-name-auto-source* (emit-sym-name-auto *all-symbol-names* 200))

;; Macro table — extract every (mvm-define-macro NAME ...) from
;; compiler.lisp at build time so runtime macroexpand-1 can see them.

(defun scan-mvm-define-macro-forms (text)
  "Find (mvm-define-macro \"NAME\" ...) forms and return their names."
  (let ((names nil)
        (pos 0))
    (loop
      (let ((p (search "(mvm-define-macro \"" text :start2 pos)))
        (unless p (return (nreverse names)))
        (let* ((start (+ p (length "(mvm-define-macro \"")))
               (end (position #\" text :start start)))
          (push (subseq text start end) names)
          (setq pos (1+ end)))))))

(defvar *compiler-source* (mvm-text "mvm/compiler.lisp"))
(defvar *macro-names* (scan-mvm-define-macro-forms *compiler-source*))
(format t "  macro names found: ~D~%" (length *macro-names*))

;; Emit %init-runtime-macros — register each macro NAME → a marker
;; (T) in *macro-table*.  This makes macroexpand-1 report "yes I know
;; about that macro" without actually expanding it.  Real expansion
;; (for runtime EVAL of macro forms) requires the actual lambdas which
;; we can't ship as easily; for now mark them as known.
(defvar *runtime-macros-source*
  (with-output-to-string (out)
    (format out "(defun %init-runtime-macros ()~%")
    (dolist (name *macro-names*)
      (format out "  (puthash (compute-name-hash ~S) *macro-table* t)~%" name))
    (format out ")~%")))

;;; ============================================================
;;; 4. Driver — moved to forward-declaration above so the scanner
;;; picks up its defuns (sys-exit / runtime-bq-expand / etc.).
;;; ============================================================

;;; ============================================================
;;; 5. Assemble *full-source*
;;; ============================================================

(format t "Assembling full source...~%")

(defvar *full-source*
  (concatenate 'string
    *prelude-source*
    (string #\Newline)
    *gc-source*
    (string #\Newline)
    *mcgc-pin-source*
    *rt-source*
    (string #\Newline)
    *rt-macros-source*
    (string #\Newline)
    *bridge-source*
    (string #\Newline)
    ;; STAGE 1: MVM ISA constants/structs + bytecode interpreter.
    *isa-source*
    (string #\Newline)
    *interp-source*
    (string #\Newline)
    *stage1-test-source*
    (string #\Newline)
    ;; STAGE 2: the MVM compiler + in-image float-bits override + eval2 probe.
    *compiler-source*
    (string #\Newline)
    *stage2-float-override*
    (string #\Newline)
    *opcode-table-init-source*
    (string #\Newline)
    *eval2-canonical-source*
    (string #\Newline)
    *stage2-test-source*
    (string #\Newline)
    ;; Defvar for *sym-name-table* (compiler.lisp now supplies *macro-table*'s
    ;; defvar; runtime macroexpand-1 references it).
    "(defvar *sym-name-table* nil)
"
    (string #\Newline)
    *sft-auto-source*
    (string #\Newline)
    *sym-name-auto-source*
    (string #\Newline)
    *runtime-macros-source*
    (string #\Newline)
    *driver-source*))

(format t "Full source: ~D characters~%" (length *full-source*))

;;; ============================================================
;;; 6. Build the image
;;; ============================================================

(mvm-load "boot/boot-linux-x64.lisp")

(in-package :modus.mvm)

(funcall (intern "INSTALL-X64-TRANSLATOR" "MODUS.MVM.X64"))
(setf modus.mvm.x64::*x64-linux-mode* t)
;; Boot preamble for linux-x64 ends 397 bytes into the file (ELF header
;; + entry stub).  Native code starts there, so the fn-entry alignment
;; loop must account for this offset — otherwise `:li-func` + OR-3 +
;; CALL-IND's sub-3 lands one byte before the prologue.  When the
;; preceding function's last byte happens to be RET (0xC3), the
;; misaligned call returns immediately, leaving the caller's RAX
;; intact (silently looks like the fn returned T or whatever else
;; was in RAX).  See reference_append_funcall_bug.md.
(setf modus.mvm.x64::*x64-native-code-offset* 397)

;; Enable the GC trampoline: without this, every :alloc-obj advances R12
;; unchecked and the heap walks past the mapped region in long-running
;; sessions.  build-x64-linux / build-x64 set this; we
;; need it too so the generic image survives ANSI sweeps.
(setf modus.mvm.x64::*x64-gc-enabled* t)
;; Linux-x64 layout: enable the CONS-KIND bitmap (GC correctness fix for the
;; cons-tagged-scratch symbol-truncation bug).  The kind-bitmap base delta is
;; a boot-linux-x64 layout constant, so the master flag is Linux-only for now.
(setf modus.mvm.x64::*mcgc-kind-bitmap-enabled* t)
;; Bring R14 to the heap midpoint so GC actually fires before the
;; from-space is exhausted.  Default leaves R14 at full heap end which
;; means the gc-check `cmp r12, r14; jl skip` only triggers after
;; allocation walked all the way to the end — too late for a Cheney
;; copy that needs the other half free.
(setf modus.mvm::*linux-x64-r14-offset* modus.mvm::+linux-x64-gc-midpoint+)
;; MCGC page-pinning test knob (stage 4).  OFF by default.  MODUS_MCGC_PINNING=1
;; enables the page-pool allocator + page collector for a pinning test build.
#+sbcl
(when (let ((v (sb-ext:posix-getenv "MODUS_MCGC_PINNING")))
        (and v (plusp (length v)) (not (string= v "0"))))
  (setf modus.mvm.x64::*mcgc-pinning-enabled* t)
  (format t "~&;; MCGC PAGE-PINNING ENABLED (test build)~%"))
;; Test knob: MODUS_MCGC_TORUN_CAP=<pages> caps each to-run segment so the
;; copy_object refill / to-run-chain path is exercised on ordinary workloads.
#+sbcl
(let ((cap (sb-ext:posix-getenv "MODUS_MCGC_TORUN_CAP")))
  (when (and cap (> (length cap) 0))
    (setf modus.mvm.x64::*mcgc-torun-cap-pages* (parse-integer cap))
    (format t "~&;; MCGC TO-RUN SEGMENT CAP = ~D pages (refill stress)~%"
            modus.mvm.x64::*mcgc-torun-cap-pages*)))
;; Debug knob: MODUS_GC_R14=<hex-or-dec bytes> forces R14 to a small offset
;; so GC fires early (fast repro of GC-from-runtime-EVAL faults).  Leaves
;; the from/to semispaces 448MB apart (unchanged), only moves the trigger.
#+sbcl
(let ((dbg (sb-ext:posix-getenv "MODUS_GC_R14")))
  (when (and dbg (> (length dbg) 0))
    (let ((v (parse-integer dbg :radix (if (and (> (length dbg) 1)
                                                (char= (char dbg 0) #\#))
                                           16 10)
                            :start (if (char= (char dbg 0) #\#) 1 0))))
      (setf modus.mvm::*linux-x64-r14-offset* v)
      (format t "~%[DEBUG] R14 offset forced to ~X (GC fires early)~%" v))))
#+sbcl
(when (let ((d (sb-ext:posix-getenv "MODUS_GC_DEBUG"))) (and d (> (length d) 0)))
  (setf modus.mvm.x64::*x64-gc-debug* t)
  (format t "~%[DEBUG] GC trampoline debug bytes enabled~%"))
;; A/B knob: MODUS_MCGC_KINDCHECK=0 keeps the cons-kind bitmap SET side
;; (image layout ~unchanged) but DISABLES the scan_word reject, to prove the
;; CHECK — not incidental layout shift — restores correctness.  Default on.
#+sbcl
(let ((kc (sb-ext:posix-getenv "MODUS_MCGC_KINDCHECK")))
  (when (and kc (string= kc "0"))
    (setf modus.mvm.x64::*mcgc-kind-check-enabled* nil)
    (format t "~%[DEBUG] MCGC cons-kind CHECK disabled (set side still on)~%")))

#+sbcl
(let ((sm (sb-ext:posix-getenv "MODUS_SYMMAP")))
  (when (and sm (> (length sm) 0))
    (setf modus.mvm::*write-symmap-path* sm)))

(format t "~%Compiling generic image (~D chars)...~%"
        (length cl-user::*full-source*))

(let ((image (build-image :target :linux-x64
                          :source-text cl-user::*full-source*)))
  (let ((path (or #+sbcl (sb-ext:posix-getenv "MODUS_QL_OUT") "modus-ql")))
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (format t "~%Wrote ~D bytes to ~A~%"
            (length (kernel-image-image-bytes image)) path)
    (format t "Usage: ~A <script.lisp>~%" path)))
