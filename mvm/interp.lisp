;;;; interp.lisp - MVM Bytecode Interpreter
;;;;
;;;; A decode-dispatch interpreter for the Modus Virtual Machine.
;;;; Portable Common Lisp -- runs on SBCL or embedded in Modus.
;;;;
;;;; Mapping to CL data structures:
;;;;   Virtual registers -> simple-vector of 23 slots
;;;;   Stack -> CL list       Heap -> CL cons cells / vectors
;;;;   Memory -> hash-table   Flags -> keyword (:eq :lt :gt)
;;;;
;;;; Tagging (mirrors Modus):
;;;;   Fixnum: value << 1, Cons: pointer|0x1, Object: pointer|0x9, NIL: 0

(in-package :modus.mvm)

;;; Tag constants and helpers

(defconstant +tag-fixnum+ 0)
(defconstant +tag-cons+   1)
(defconstant +tag-object+ 9)
;; NIL/T immediates must match the COMPILER's representation (compiler.lisp
;; +nil-value+ #xDEAD0001 / +t-value+ #xDEAD1009), not the obsolete 0/2 fixnum
;; model.  Truthiness is "not NIL": every value except #xDEAD0001 (and host
;; Lisp NIL) is true.  Comparison opcodes in the compiler build their T as
;; (nil | 8) = #xDEAD0009 — also non-NIL, so truthy.  mvm-nil-p must therefore
;; key on #xDEAD0001 exactly.
(defconstant +mvm-nil+ #xDEAD0001)
(defconstant +mvm-t+   #xDEAD1009)

(declaim (inline tag-fixnum untag-fixnum mvm-nil-p mvm-boolean))
(defun tag-fixnum (n) (ash n 1))
(defun untag-fixnum (tagged) (ash tagged -1))
(defun mvm-nil-p (val) (or (null val) (eql val +mvm-nil+)))
(defun mvm-boolean (b) (if b +mvm-t+ +mvm-nil+))

;; GC-safe register access (unified representation, WS1).  The interpreter
;; operates on RAW WORDS, but storing a raw pointer-word as a Lisp integer hides
;; it from the moving collector (it looks like a fixnum) -> stale after a
;; collection.  Fix: the regs vector slots hold REAL VALUES (so GC traces/updates
;; pointers normally, no collector changes), and these accessors convert with the
;; single-shift reinterprets: reg-get returns the slot value's raw word,
;; reg-set stores a raw word as the value it denotes.  Round-trips for fixnum,
;; cons, object, nil, t.  (Transient fast-path MASK words become bogus pointer
;; values briefly, but are consumed before the next allocation, so no GC scans
;; them; the conservative kind-bitmap gate also rejects such non-object-starts.)
(declaim (inline reg-get reg-set))
(defun reg-get (regs v) (%val->word (svref regs v)))
(defun reg-set (regs v w) (setf (svref regs v) (%word->val w)))

;;; Interpreter state

(defparameter *mvm-trace* nil)  ; when non-nil, mvm-interpret prints each opcode

;; Runtime-call bridge (unified-representation WS1): CALL targets at or above this
;; base are not in-module bytecode offsets but indices into the interpreter's
;; RUNTIME-TABLE (synthetic-offset -> native function name).  op-call funcalls the
;; real native function with %word->val'd args — the no-marshalling path proven in
;; WS1.0.  Base is far above any real eval2 module size and fits in a u32.
(defconstant +mvm-runtime-call-base+ #x40000000)

(defun %mvm-resolve-runtime-fn (name)
  "Resolve a native function by NAME (string) via the symbol-function table."
  (and (boundp '*symbol-function-table*)
       *symbol-function-table*
       (gethash name *symbol-function-table*)))

(defconstant +num-vregs+ 23)

(defstruct (mvm-state (:conc-name mvm-))
  (regs    (make-array +num-vregs+ :initial-element 0) :type simple-vector)
  (stack   nil :type list)
  (flags   :eq :type keyword)
  (memory  (make-hash-table :test 'eql))
  (heap    nil :type list)
  (halted  nil :type boolean)
  (call-stack nil :type list)
  (percpu  (make-hash-table :test 'eql))
  (interrupts-enabled t :type boolean)
  (io-ports (make-hash-table :test 'eql))
  ;; Calling-convention registers (native: RAX=nargs, a closure-env reg, and
  ;; the MV-COUNT slot).  Modeled as state fields so set/get-nargs, set/get-cenv,
  ;; and set-mv-count have somewhere to live.
  (nargs    0)
  (cenv     nil)
  (mv-count 1)
  ;; handler-case / catch / throw setjmp-longjmp stack.  Each entry is a
  ;; "jmp-buf": the bytecode resume-PC for the matching SETJMP (TRAP #x0510)
  ;; plus the snapshot of dynamic interp state to restore on LONGJMP (TRAP
  ;; #x0511 / a host condition signalled by a bridged `error`/`throw` call).
  ;; LONGJMP pops the top entry, restores the snapshot, sets pc to resume-PC
  ;; and VR to a non-NIL marker so the handler-case's `:bnnull` takes the
  ;; handler path (mirroring the native setjmp's non-zero return).
  (handlers nil :type list))

;; A jmp-buf saved by SETJMP (TRAP #x0510): everything the interpreter needs to
;; resume the handler-case body's setjmp point after a LONGJMP.  pc is the
;; bytecode offset of the instruction AFTER the setjmp trap (where the native
;; code does `mov dest,VR; bnnull dest,handler`); the rest is dynamic interp
;; state captured so the unwind restores the operand stack / call frames / the
;; calling-convention scratch registers to exactly the setjmp-time values.
(defstruct (mvm-jmpbuf (:conc-name mvm-jb-))
  (pc         0   :type fixnum)
  (stack      nil :type list)
  (call-stack nil :type list)
  (vfp        0)
  (nargs      0)
  (cenv       nil)
  (mv-count   1))

(declaim (inline vref vset))
(defun vref (state reg) (svref (mvm-regs state) reg))
(defun vset (state reg val) (setf (svref (mvm-regs state) reg) val))
(defsetf vref vset)

;;; Memory helpers

(defun mem-read-byte (state addr)
  (gethash addr (mvm-memory state) 0))

(defun mem-write-byte (state addr byte)
  (setf (gethash addr (mvm-memory state)) (logand byte #xFF)))

(defun mem-read (state addr width)
  (let ((val 0))
    (dotimes (i (ash 1 width) val)
      (setf val (logior val (ash (mem-read-byte state (+ addr i)) (* i 8)))))))

(defun mem-write (state addr val width)
  (dotimes (i (ash 1 width))
    (mem-write-byte state (+ addr i) (logand (ash val (* i -8)) #xFF))))

;;; Object representation — REAL native CL objects (no simulation).  The
;;; interpreter is itself native code, so it allocates native objects and
;;; reads/writes their slots with the native slot primitives %prim-aref /
;;; %prim-aset / %prim-array-length (these compile to obj-ref/aref/etc. that run
;;; NATIVELY, not back through this interpreter).  So strings, vectors, floats,
;;; ratios, bignums are all REAL CL objects that cross the native bridge.
(defun %alloc-native (size subtag)
  "Allocate a native object of SIZE slots with SUBTAG (the alloc-obj/-array/
   -string opcodes route here).  Fixed-size numeric boxes use their typed
   allocators; arrays/strings take the runtime size."
  (cond ((= subtag #x31) (make-string size :initial-element #\Space)) ; string
        ((= subtag #x32) (make-array size :initial-element nil))      ; simple-vector
        ((= subtag #x60) (%make-float2))   ; 2-slot boxed double-float
        ;; N1 typed floats: the float-literal compiler emits #x64 for a SINGLE
        ;; (which is what `1.5' is under *read-default-float-format*=single),
        ;; #x65 short, #x66 long.  Without these the #x64 alloc fell through to
        ;; the (t ...) make-array fallback → a #x32 simple-vector holding the
        ;; raw hi/lo words: floatp NIL, generic-add hit %fixnum-+ on two
        ;; pointers (garbage), every float diverged from native (WS4 oracle).
        ((= subtag #x64) (%make-single2))  ; 2-slot boxed single-float
        ((= subtag #x65) (%make-short2))   ; 2-slot boxed short-float
        ((= subtag #x66) (%make-long2))    ; 2-slot boxed long-float
        ((= subtag #x33) (%make-ratio))    ; 2-slot ratio (num . den)
        ((= subtag #x30) (%make-bignum))   ; 2-slot bignum header (limbs in a slot)
        ;; Closure (#x52): compile-make-closure emits `alloc-obj 2 #x52` then
        ;; obj-set slot 0 = fn-addr, slot 1 = env.  Allocate a REAL closure
        ;; object (not the make-array fallback) so op-obj-subtag reports #x52
        ;; — funcall's dispatch checks subtag #x52 to take the set-cenv +
        ;; call-indirect closure path.  A make-array (#x32) here made every
        ;; CAPTURING lambda fall through to the direct-call path (subtag !=
        ;; #x52) → empty/NIL result (WS4 oracle).  The placeholder slots are
        ;; overwritten by the following obj-sets; %prim-aset is uniform across
        ;; object types.
        ((= subtag #x52) (%make-closure 0 nil))
        (t (make-array size :initial-element nil))))
;; Slot ref/set/len via the NATIVE primitives.  %prim-aref returns the raw slot
;; (char CODE for a string), %prim-aset stores it; uniform across object types.
(defun %obj-elt-ref (obj idx) (%prim-aref obj idx))
(defun %obj-elt-set (obj idx val) (%prim-aset obj idx val))
(defun %obj-elt-len (obj) (%prim-array-length obj))

;;; Bytecode fetch helpers

(declaim (inline fetch-byte fetch-reg fetch-u16 fetch-s32 fetch-u32 fetch-u64 fetch-li-value))

(defun fetch-byte (bc pc)
  (values (aref bc pc) (1+ pc)))

(defun fetch-reg (bc pc)
  (values (logand (aref bc pc) #x1F) (1+ pc)))

(defun fetch-u16 (bc pc)
  (values (logior (aref bc pc) (ash (aref bc (+ pc 1)) 8))
          (+ pc 2)))

(defun fetch-s32 (bc pc)
  (let ((val (logior (aref bc pc) (ash (aref bc (+ pc 1)) 8)
                     (ash (aref bc (+ pc 2)) 16) (ash (aref bc (+ pc 3)) 24))))
    (values (if (>= val #x80000000) (- val #x100000000) val) (+ pc 4))))

(defun fetch-u32 (bc pc)
  (values (logior (aref bc pc) (ash (aref bc (+ pc 1)) 8)
                  (ash (aref bc (+ pc 2)) 16) (ash (aref bc (+ pc 3)) 24))
          (+ pc 4)))

(defun fetch-u64 (bc pc)
  ;; Reconstruct the 8-byte little-endian immediate as a SIGNED 64-bit value
  ;; held as a native (possibly negative) fixnum.  The old form
  ;; `(logior lo (ash hi 32))` formed an UNSIGNED value: for any immediate with
  ;; the high word's bit 31 set (every negative tagged literal — word's hi32 =
  ;; #xFFFFFFFF — and large positives), `(ash hi 32)` lands >= 2^62 where
  ;; Modus's bignum-range ASH is lossy, so the immediate read back as garbage
  ;; (negative fixnum literals like -5 became 2^62-ish).  Interpreting hi as
  ;; signed and combining with `+`/`*` keeps the common case (hi = 0 or
  ;; #xFFFFFFFF, i.e. |value| < 2^31) entirely in fixnum range and yields the
  ;; correct signed word, which `%word->val` (a single arithmetic SHR) then
  ;; turns back into the right value.
  (let* ((lo (logior (aref bc pc) (ash (aref bc (+ pc 1)) 8)
                     (ash (aref bc (+ pc 2)) 16) (ash (aref bc (+ pc 3)) 24)))
         (hi (logior (aref bc (+ pc 4)) (ash (aref bc (+ pc 5)) 8)
                     (ash (aref bc (+ pc 6)) 16) (ash (aref bc (+ pc 7)) 24)))
         (hi-signed (if (>= hi #x80000000) (- hi #x100000000) hi)))
    (values (+ lo (* hi-signed 4294967296)) (+ pc 8))))

(defun fetch-li-value (bc pc)
  "Read an 8-byte little-endian LI immediate (a tagged WORD W = value<<1) and
   return %word->val(W) = floor(W/2) DIRECTLY — without ever forming W itself.
   W can reach +/-2^63 for a boundary fixnum (|value| >= 2^61); forming it would
   overflow the in-image 62-bit fixnum range, and fetch-u64's `(* hi-signed
   2^32)` would then hit the broken in-image bignum MULTIPLY.  Computing the
   value as `(ash lo -1) + hi_signed*2^31` keeps every term in fixnum range:
   hi_signed in [-2^31, 2^31-1] so hi_signed*2^31 in [-2^62, 2^62-2^31], and
   floor(W/2) = floor(lo/2) + hi_signed*2^31.  Matches %word->val(W) (arith SHR)
   exactly for the full fixnum-value range; for small words (chars, pointers,
   normal fixnums) it is identical to the old fetch-u64 + %word->val path."
  (let* ((lo (logior (aref bc pc) (ash (aref bc (+ pc 1)) 8)
                     (ash (aref bc (+ pc 2)) 16) (ash (aref bc (+ pc 3)) 24)))
         (hi (logior (aref bc (+ pc 4)) (ash (aref bc (+ pc 5)) 8)
                     (ash (aref bc (+ pc 6)) 16) (ash (aref bc (+ pc 7)) 24)))
         (hi-signed (if (>= hi #x80000000) (- hi #x100000000) hi)))
    ;; hi-signed*2^31 stays in fixnum range EXCEPT when hi-signed = -2^31
    ;; (the only 32-bit value of magnitude 2^31): then the product is -2^62 and
    ;; computing it in-image overflows/SEGVs (2^62 isn't a fixnum).  That case is
    ;; exactly most-negative-fixnum territory; use the baked min-fixnum literal
    ;; (compiled correctly at host build time) plus the low half instead.
    (values (if (= hi-signed -2147483648)
                (+ (ash lo -1) -4611686018427387904)
                (+ (ash lo -1) (* hi-signed 2147483648)))
            (+ pc 8))))

;;; Conditions

(define-condition mvm-trap (error)
  ((code :initarg :code :reader mvm-trap-code))
  (:report (lambda (c s) (format s "MVM trap ~D" (mvm-trap-code c)))))

(define-condition mvm-type-error (error)
  ((expected :initarg :expected :reader mvm-type-error-expected)
   (got :initarg :got :reader mvm-type-error-got)
   (operation :initarg :operation :reader mvm-type-error-operation))
  (:report (lambda (c s) (format s "MVM type error in ~A: expected ~A, got ~S"
                                 (mvm-type-error-operation c)
                                 (mvm-type-error-expected c)
                                 (mvm-type-error-got c)))))

;;; ============================================================
;;; Native-HOF re-entrancy trampoline
;;; ============================================================
;;;
;;; The hard eval2 gap: a NATIVE higher-order function (mapcar/reduce/…) that
;;; funcalls an eval2 lambda VALUE.  funcall/apply/mapcar of #'NAME already
;;; work (the value is a real native fn the bridge funcalls), and an IN-module
;;; (funcall (lambda …) x) works (op-call-ind jumps within the same interpret
;;; loop).  But when an in-module bytecode lambda ESCAPES to native code — its
;;; value is handed to native mapcar via the op-CALL bridge's raw svref — native
;;; mapcar's `(funcall it elt)` gets an in-module bytecode OFFSET (a fixnum) or a
;;; #x52 closure whose slot-0 is such an offset.  Native funcall can't run
;;; bytecode → the call fails.
;;;
;;; Fix: when an eval2 lambda value crosses to native code (only at the op-CALL /
;;; op-CALL-IND runtime bridges), wrap it in a TRAMPOLINE — a real native Modus
;;; closure that, when funcalled with args, RE-ENTERS mvm-interpret on the SAME
;;; bytecode at the lambda's offset (with the args marshalled into a fresh
;;; register file and the closure env restored).  The interpreter is itself
;;; native code, so a nested mvm-interpret call is plain recursion; registers and
;;; the operand/call stacks are per-call (fresh state), while the heap and the
;;; real native objects the lambda manipulates are shared.
;;;
;;; Only the value ESCAPING to native is wrapped — the in-module op-CALL-IND
;;; jump path and funcall's closure (#x52 set-cenv) path are untouched, so the
;;; working (funcall (lambda …) x) and capturing-closure cases keep their fast
;;; in-loop dispatch.

(defun %mvm-make-trampoline (bc ftab rt offset env lam-offsets)
  "Return a native Modus closure that re-enters mvm-interpret at OFFSET (a
   bytecode entry) on BC with FTAB/RT, marshalling its call args into a fresh
   register file and restoring the closure ENV (NIL for a captureless lambda).
   Native HOFs (mapcar/reduce/…) funcall this exactly like any other function.
   LAM-OFFSETS is threaded through so a lambda that itself escapes a lambda to a
   native HOF (nested mapcar) keeps working on the re-entry."
  (lambda (&rest args)
    (mvm-interpret bc :entry-point offset
                      :function-table ftab :runtime-table rt
                      :return-raw nil
                      :initial-args args :initial-cenv env
                      :lambda-offsets lam-offsets)))

(defun %mvm-lambda-offset-p (n lam-offsets)
  "True if integer N is the bytecode entry offset of an eval2 LAMBDA / CLOSURE
   function (as recorded in the LAM-OFFSETS hash, keyed by offset).  Built in
   eval2-forms from the functions whose names carry the `$$LAMBDA` / `$$CLOSURE`
   marker — i.e. ONLY genuine lambda bodies, never the %eval2-thunk, helper
   defuns, or the function at offset 0.  This is what makes a BARE in-module
   offset (a captureless lambda's fn-addr value) safely distinguishable from an
   ordinary fixnum DATA argument at the native bridge: a data integer like 0 / 1
   / 2 (loop counters, indices) is never a lambda entry, so it is left alone.
   (The earlier ftab-membership test was unsafe: ftab[0] = 0, so the integer 0
   collided with the first function's offset and got wrapped, breaking loops.)"
  (and (integerp n) lam-offsets
       (gethash n lam-offsets)))

(defun %mvm-wrap-escaping (v bc ftab rt lam-offsets)
  "If V is an eval2 lambda value about to cross to NATIVE code, wrap it in a
   trampoline so native funcall can invoke it.  Two escaping shapes:
     - a #x52 CLOSURE object whose slot-0 is a LAMBDA bytecode offset: wrap
       (slot0 offset, slot1 env).  This is the CAPTURING escaped lambda —
       `(mapcar (lambda (x) (+ x k)) …)` where k is captured.
     - a BARE LAMBDA bytecode OFFSET (a fixnum in LAM-OFFSETS): a CAPTURELESS
       escaped lambda — `(mapcar (lambda (x) (* x 10)) …)`.  op-FN-ADDR stored
       its offset as a plain fixnum (so the in-module call-indirect path jumps
       to it); wrap it with NIL env.
     - everything else (data fixnums, conses, strings, real native fns, etc.)
       passes through unchanged."
  (cond
    ;; #x52 closure with a lambda offset in slot 0 → capturing lambda.
    ;; NB: native functionp is TRUE for a #x52 closure object, so this MUST be
    ;; checked WITHOUT a functionp guard — gate on cons/integer only.  A genuine
    ;; native function (#'NAME) is NOT a tagged object, so obj-subtag reads
    ;; garbage on it; requiring slot-0 to be a recorded LAMBDA offset rejects
    ;; such a false #x52 read.
    ((and v (not (consp v)) (not (integerp v))
          (= (obj-subtag v) #x52)
          (%mvm-lambda-offset-p (%prim-aref v 0) lam-offsets))
     (%mvm-make-trampoline bc ftab rt (%prim-aref v 0) (%prim-aref v 1) lam-offsets))
    ;; Bare lambda offset → captureless lambda.
    ((%mvm-lambda-offset-p v lam-offsets)
     (%mvm-make-trampoline bc ftab rt v nil lam-offsets))
    (t v)))

;;; ============================================================
;;; Native-call argument collection (register file + overflow stack)
;;; ============================================================

(defun %mvm-collect-call-args (state regs nargs bc ftab rt lam-offsets)
  "Collect the NARGS arguments for a native bridge call, in order
   (arg0 arg1 … arg{nargs-1}), wrapping any escaping eval2 lambda value.

   The MVM calling convention places the FIRST 4 args (compiler.lisp's
   +max-reg-args+ — hardcoded 4 here because that constant is defined in
   compiler.lisp, which loads AFTER interp.lisp) in registers V0..V3.  Args
   with index >= 4 are OVERFLOW: compile-call / compile-funcall :push them
   onto the mvm-stack (top of stack = arg4, next below = arg5, …) just
   before the :call / :call-indirect.  The original bridge read EVERY arg
   from regs[0..nargs-1], so for nargs>4 it read STALE register slots for
   args 4+ — every native fn bridge-called from eval2 with >=5 args got
   garbage for its 5th+ argument (the assoc/member/adjoin/remove/… clusters:
   the keyword validators saw garbage where :allow-other-keys / :test / :key
   should be → spurious PROGRAM-ERROR returned as a value).  Read the
   overflow args from the mvm-stack here (non-destructively — the caller's
   post-call POP cleanup still drains them)."
  (let ((args nil))
    ;; Prepend the overflow args FIRST in reverse (arg{nargs-1} … arg4) so the
    ;; final list begins arg0..arg3 (added below) then arg4..arg{nargs-1}.
    (when (> nargs 4)
      (let ((over (- nargs 4))
            (rev nil)
            (s (mvm-stack state)))
        ;; Walk the top OVER stack cells: car=arg4, cadr=arg5, …  Collect into
        ;; REV so rev = (arg{nargs-1} … arg4).
        (dotimes (i over)
          (setq rev (cons (car s) rev))
          (setq s (cdr s)))
        ;; rev = (arg{nargs-1} … arg4); prepend each (wrapped) → args now =
        ;; (arg4 … arg{nargs-1}).
        (dolist (v rev)
          (push (%mvm-wrap-escaping v bc ftab rt lam-offsets) args))))
    ;; Prepend the register args (indices min(nargs,4)-1 … 0) so they lead.
    (let ((i (- (if (> nargs 4) 4 nargs) 1)))
      (loop
        (when (< i 0) (return))
        (push (%mvm-wrap-escaping (svref regs i) bc ftab rt lam-offsets) args)
        (setq i (- i 1))))
    args))

;;; ============================================================
;;; The Main Interpreter
;;; ============================================================

(defun %mvm-longjmp-restore (state)
  "Perform an MVM LONGJMP: pop the nearest jmp-buf saved by SETJMP (TRAP #x0510),
   restore the dynamic interp state it captured, set VR to a non-NIL marker so
   the handler-case's BNNULL takes the handler path, and RETURN the bytecode
   resume-PC the loop should jump to.  Returns NIL when no handler frame is
   active (an unbalanced longjmp — the caller re-signals)."
  (let ((jb (pop (mvm-handlers state))))
    (when jb
      (setf (mvm-stack state) (mvm-jb-stack jb))
      (setf (mvm-call-stack state) (mvm-jb-call-stack jb))
      (setf (mvm-nargs state) (mvm-jb-nargs jb))
      (setf (mvm-cenv state) (mvm-jb-cenv jb))
      (setf (mvm-mv-count state) (mvm-jb-mv-count jb))
      (let ((regs (mvm-regs state)))
        (setf (svref regs +vreg-vfp+) (mvm-jb-vfp jb))
        (reg-set regs +vreg-vr+ +mvm-t+))
      (mvm-jb-pc jb))))

(defun mvm-interpret (bytecode &key (entry-point 0) function-table runtime-table
                                    (return-raw t) initial-args initial-cenv
                                    lambda-offsets)
  "Execute MVM bytecode starting at ENTRY-POINT.
   When RET or HALT is reached, return VR.  RETURN-RAW (default T, for callers
   that re-tag the result themselves via `(ash result -1)`) returns the RAW WORD
   %val->word(VR); pass RETURN-RAW NIL to get VR's VALUE directly — needed for
   boundary fixnums (|value| >= 2^61) whose word exceeds the in-image 62-bit
   fixnum range, so %val->word (and the caller's later %word->val) would overflow.
   RUNTIME-TABLE (optional, synthetic-offset -> native fn name string) routes
   CALLs to functions outside the bytecode module to a direct native funcall.

   INITIAL-ARGS / INITIAL-CENV (re-entrancy support, the native-HOF-over-an-
   eval2-lambda path): when a native higher-order function (mapcar/reduce/…)
   funcalls an eval2 lambda VALUE that escaped to native code, the escaping
   value is a trampoline closure (see %mvm-make-trampoline) that RE-ENTERS this
   interpreter at the lambda's bytecode offset.  INITIAL-ARGS is the list of
   call arguments — they are loaded into V0..Vn (the normal arg registers) and
   NARGS is set, exactly as an in-module CALL caller would set them, so the
   lambda's frame-enter prologue spills them into its frame.  INITIAL-CENV is
   the closure env (slot 1 of a #x52 closure) so a CAPTURING escaped lambda can
   resolve its captured vars via op-get-cenv.  The args are VALUES (the native
   funcall passes real values) so they are stored with raw svref, matching the
   in-module call convention (op-call's bridge reads args via svref too)."
  (let* ((state (make-mvm-state))
         (bc bytecode) (pc entry-point) (len (length bc))
         (ftab (or function-table (vector)))
         (regs (mvm-regs state)))
    (declare (type fixnum pc len) (type simple-vector regs) (ignorable ftab))
    (reg-set regs +vreg-vn+ +mvm-nil+)  ; VN holds the canonical NIL immediate
    (reg-set regs +vreg-vpc+ pc)
    ;; Re-entry arg marshalling: store each initial arg VALUE into V0..Vn and
    ;; set NARGS, then set the closure env.  Mirrors what a normal CALL caller
    ;; does (args in V0..V3 via push/pop, :set-nargs) so the callee's
    ;; frame-enter / &rest prologue see them.
    (when initial-args
      (let ((i 0))
        (dolist (a initial-args)
          (when (< (+ +vreg-v0+ i) +num-vregs+)
            (setf (svref regs (+ +vreg-v0+ i)) a))
          (setf i (+ i 1)))
        (setf (mvm-nargs state) i)))
    ;; The closure-env register is stored as a WORD (op-set-cenv does
    ;; `reg-get` = %val->word) and read back as a VALUE (op-get-cenv does
    ;; `reg-set` = %word->val).  INITIAL-CENV is a VALUE (slot 1 of the #x52
    ;; closure), so encode it to a word here to match the round-trip op-get-cenv
    ;; will perform — otherwise the captured-var extraction reads a corrupted
    ;; env (a TYPE-ERROR / wrong value).
    (when initial-cenv
      (setf (mvm-cenv state) (%val->word initial-cenv)))

    (loop
      ;; Re-load regs from the state each iteration: the compiled in-image
      ;; interpreter may cache `regs` in a register, but a GC during
      ;; interpretation copies the regs array and only updates the live root
      ;; (the state's slot) — a cached local would go stale and svref/setf
      ;; would read/write the dead from-space copy.  Reading mvm-regs fresh
      ;; each step always sees the current copy.  (Host SBCL is unaffected;
      ;; this is belt-and-suspenders there.)
      (setf regs (mvm-regs state))
      (when (or (mvm-halted state) (>= pc len))
        (return (if return-raw
                    (reg-get regs +vreg-vr+)        ; raw word (caller re-tags)
                    (svref regs +vreg-vr+))))       ; value directly (boundary-safe)
      (when *mvm-trace*
        (format t "  TRACE pc=~D op=~D flags=~S vr=~S~%"
                pc (aref bc pc) (mvm-flags state) (reg-get regs +vreg-vr+)))
      (let ((opcode (aref bc pc))
            ;; %lj — set non-NIL by the condition handler below when a host
            ;; condition signalled DURING this opcode (a bridged `error`/`throw`
            ;; native call) must be converted into an MVM LONGJMP to the nearest
            ;; active handler-case frame.  Performed AFTER the handler-case
            ;; unwinds the host stack back here, so the loop's pc/regs locals
            ;; (in the enclosing let*) survive and can be redirected.
            (%lj nil))
        (setf pc (1+ pc))
        (handler-case
        (case opcode

          ;; --- NOP / BREAK / TRAP ---
          (#.+op-nop+ nil)
          (#.+op-break+ (break "MVM BREAK at PC ~D" (1- pc)))
          (#.+op-trap+
           ;; TRAP code:u16 multiplexes several compiler pseudo-ops:
           ;;   code  < #x100  : FRAME-ENTER  (code = param count) — allocate frame
           ;;   #x100..#x1FF   : FRAME-ALLOC  (extend stack for locals)
           ;;   #x200..#x2FF   : FRAME-FREE   (pop locals)
           ;;   #x0310         : RDTSC
           ;; The interpreter's frame is a single over-allocated array set up at
           ;; FRAME-ENTER, so FRAME-ALLOC/FRAME-FREE are no-ops.  (The earlier bug:
           ;; treating FRAME-ALLOC as FRAME-ENTER re-allocated a fresh frame mid-
           ;; function, wiping VFP and any spilled locals -> stack-load read 0.)
           (multiple-value-bind (code npc) (fetch-u16 bc pc)
             (cond
               ((= code #x0310)
                ;; RDTSC: fake value in interpreter
                (reg-set regs +vreg-vr+ 0)
                (setf pc npc))
               ;; --- handler-case / catch / throw setjmp-longjmp ---
               ;; SETJMP (#x0510): push a jmp-buf recording the resume-PC (npc,
               ;; the instruction right after this trap, where the compiled
               ;; handler-case does `mov dest,VR; bnnull dest,handler`) plus a
               ;; snapshot of the dynamic interp state.  VR := 0 (NIL marker)
               ;; so the BNNULL falls through to the body (the native "setjmp
               ;; returned 0 = normal entry" path).
               ((= code #x0510)
                (push (make-mvm-jmpbuf
                       :pc npc
                       :stack (mvm-stack state)
                       :call-stack (mvm-call-stack state)
                       :vfp (svref regs +vreg-vfp+)
                       :nargs (mvm-nargs state)
                       :cenv (mvm-cenv state)
                       :mv-count (mvm-mv-count state))
                      (mvm-handlers state))
                (reg-set regs +vreg-vr+ +mvm-nil+)
                (setf pc npc))
               ;; LONGJMP (#x0511): pop the nearest jmp-buf, restore its dynamic
               ;; state, jump pc back to the setjmp's resume-PC, and set VR to a
               ;; non-NIL marker so the BNNULL there takes the HANDLER path
               ;; (mirroring the native "setjmp returned non-zero").  With no
               ;; handler active this is an unbalanced longjmp — signal so the
               ;; outer eval2-forms handler reports it (matches native: a longjmp
               ;; with a zeroed jmp-buf slot is undefined / a crash).
               ((= code #x0511)
                (let ((rpc (%mvm-longjmp-restore state)))
                  (if rpc
                      (progn (setf regs (mvm-regs state)) (setf pc rpc))
                      (error "MVM LONGJMP (TRAP #x0511) with no active handler-case"))))
               ;; CLEAR-HANDLER (#x0512): the body completed normally — pop the
               ;; jmp-buf so a later error doesn't unwind to this (now exited)
               ;; frame.  Pure pop; PC just advances.
               ((= code #x0512)
                (when (mvm-handlers state)
                  (pop (mvm-handlers state)))
                (setf pc npc))
               ((>= code #x100)
                ;; FRAME-ALLOC / FRAME-FREE: no-op (frame is over-allocated).
                (setf pc npc))
               (t
                ;; FRAME-ENTER: allocate a generously-sized frame so all locals
                ;; that later FRAME-ALLOCs would add still fit.
                (let* ((params (logand code #xFF))
                       (frame-size (+ params 64)))
                  (reg-set regs +vreg-vfp+
                        (%val->word (make-array frame-size :initial-element 0))))
                (setf pc npc)))))

          ;; --- Data Movement ---
          (#.+op-mov+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (reg-set regs vd (reg-get regs vs)) (setf pc npc2))))

          (#.+op-li+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             ;; Peek the high 32 bits to choose the load path WITHOUT forming the
             ;; full word.  |word| < 2^62 (the normal case: small fixnums, the
             ;; tag-check mask `1`, char/pointer immediates) goes through the
             ;; reg-set / %word->val path, which keeps the value<->word round-trip
             ;; exact so the WORD-domain ops (:or/:test/:and via reg-get) see the
             ;; right bits.  |word| >= 2^62 means a boundary fixnum literal whose
             ;; word overflows the in-image fixnum range — fetch-li-value returns
             ;; %word->val(word) DIRECTLY (overflow-free), stored as the value.
             (let* ((hi (logior (aref bc (+ npc 4)) (ash (aref bc (+ npc 5)) 8)
                                (ash (aref bc (+ npc 6)) 16) (ash (aref bc (+ npc 7)) 24)))
                    (hi-signed (if (>= hi #x80000000) (- hi #x100000000) hi)))
               (if (or (>= hi-signed #x40000000) (< hi-signed #x-40000000))
                   (multiple-value-bind (val npc2) (fetch-li-value bc npc)
                     (setf (svref regs vd) val) (setf pc npc2))
                   (multiple-value-bind (imm npc2) (fetch-u64 bc npc)
                     (reg-set regs vd imm) (setf pc npc2))))))

          ;; PUSH/POP store real VALUES on mvm-stack (not raw words) so the GC
          ;; traces/updates pushed pointers — same GC-safety reason as the regs
          ;; vector.  (A pushed float/cons raw-word integer would be invisible to
          ;; the collector and go stale across an allocation, e.g. when building
          ;; several boxed-float args.)
          (#.+op-push+
           (multiple-value-bind (vs npc) (fetch-reg bc pc)
             ;; regs already hold real VALUES — push the value directly (the
             ;; old %word->val∘reg-get round-trip overflowed for boundary fixnums).
             (push (svref regs vs) (mvm-stack state)) (setf pc npc)))

          (#.+op-pop+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (unless (mvm-stack state) (error "MVM: stack underflow at PC ~D" (1- pc)))
             (setf (svref regs vd) (pop (mvm-stack state))) (setf pc npc)))

          ;; --- Arithmetic (tagged fixnums: value << 1) ---
          ;; ADD/SUB: tag-preserving since (a<<1)+(b<<1) = (a+b)<<1
          (#.+op-add+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (reg-set regs vd (+ (reg-get regs va) (reg-get regs vb)))
                 (setf pc npc3)))))

          (#.+op-add-checked+
           ;; High-level `+`: promote on overflow via GENERIC-ADD.
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (setf (svref regs vd)
                       (generic-add (svref regs va) (svref regs vb)))
                 (setf pc npc3)))))

          (#.+op-sub+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (reg-set regs vd (- (reg-get regs va) (reg-get regs vb)))
                 (setf pc npc3)))))

          (#.+op-mul+
           ;; RAW tagged multiply (wraps) — building block for %fixnum-*.
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (reg-set regs vd
                       (tag-fixnum (* (untag-fixnum (reg-get regs va))
                                      (untag-fixnum (reg-get regs vb)))))
                 (setf pc npc3)))))

          (#.+op-mul-checked+
           ;; High-level `*`: promote on overflow via GENERIC-MULTIPLY.
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (setf (svref regs vd)
                       (generic-multiply (svref regs va) (svref regs vb)))
                 (setf pc npc3)))))

          (#.+op-mul26lo+
           ;; low 26 bits of untag(a)*untag(b), tagged
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (let ((product (* (untag-fixnum (reg-get regs va))
                                   (untag-fixnum (reg-get regs vb)))))
                   (reg-set regs vd (tag-fixnum (logand product #x3FFFFFF))))
                 (setf pc npc3)))))

          (#.+op-mul26hi+
           ;; bits 26+ of untag(a)*untag(b), tagged
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (let ((product (* (untag-fixnum (reg-get regs va))
                                   (untag-fixnum (reg-get regs vb)))))
                   (reg-set regs vd (tag-fixnum (ash product -26))))
                 (setf pc npc3)))))

          (#.+op-mul64lo+
           ;; low 64 bits of raw Va*Vb (no tag/untag)
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (reg-set regs vd
                       (logand (* (reg-get regs va) (reg-get regs vb))
                               #xFFFFFFFFFFFFFFFF))
                 (setf pc npc3)))))

          (#.+op-mul64hi+
           ;; high 64 bits of raw Va*Vb (no tag/untag)
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (reg-set regs vd
                       (ash (* (reg-get regs va) (reg-get regs vb)) -64))
                 (setf pc npc3)))))

          (#.+op-acc128+
           ;; mem128[Vaddr] += Vhi:Vlo (raw 128-bit accumulate)
           (multiple-value-bind (vaddr npc) (fetch-reg bc pc)
             (multiple-value-bind (vlo npc2) (fetch-reg bc npc)
               (multiple-value-bind (vhi npc3) (fetch-reg bc npc2)
                 (let* ((addr (reg-get regs vaddr))
                        (cur-lo (mem-read state addr 3))
                        (cur-hi (mem-read state (+ addr 8) 3))
                        (sum-lo (+ cur-lo (reg-get regs vlo)))
                        (carry (if (> sum-lo #xFFFFFFFFFFFFFFFF) 1 0))
                        (new-lo (logand sum-lo #xFFFFFFFFFFFFFFFF))
                        (new-hi (logand (+ cur-hi (reg-get regs vhi) carry)
                                        #xFFFFFFFFFFFFFFFF)))
                   (mem-write state addr new-lo 3)
                   (mem-write state (+ addr 8) new-hi 3))
                 (setf pc npc3)))))

          (#.+op-div+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (let ((b (untag-fixnum (reg-get regs vb))))
                   (when (zerop b) (error "MVM: division by zero at PC ~D" (1- pc)))
                   (reg-set regs vd
                         (tag-fixnum (truncate (untag-fixnum (reg-get regs va)) b))))
                 (setf pc npc3)))))

          (#.+op-mod+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (let ((b (untag-fixnum (reg-get regs vb))))
                   (when (zerop b) (error "MVM: modulus by zero at PC ~D" (1- pc)))
                   (reg-set regs vd
                         (tag-fixnum (mod (untag-fixnum (reg-get regs va)) b))))
                 (setf pc npc3)))))

          (#.+op-neg+ ; -(a<<1) = (-a)<<1
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (reg-set regs vd (- (reg-get regs vs))) (setf pc npc2))))

          (#.+op-inc+ ; tagged +1 = raw +2
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (incf (reg-get regs vd) 2) (setf pc npc)))

          (#.+op-dec+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (decf (reg-get regs vd) 2) (setf pc npc)))

          ;; --- Bitwise ---
          (#.+op-and+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (reg-set regs vd (logand (reg-get regs va) (reg-get regs vb)))
                 (setf pc npc3)))))

          (#.+op-or+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (reg-set regs vd (logior (reg-get regs va) (reg-get regs vb)))
                 (setf pc npc3)))))

          (#.+op-xor+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (reg-set regs vd (logxor (reg-get regs va) (reg-get regs vb)))
                 (setf pc npc3)))))

          (#.+op-shl+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (multiple-value-bind (amt npc3) (fetch-byte bc npc2)
                 (reg-set regs vd
                       (tag-fixnum (ash (untag-fixnum (reg-get regs vs)) amt)))
                 (setf pc npc3)))))

          (#.+op-shr+ ; logical shift right
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (multiple-value-bind (amt npc3) (fetch-byte bc npc2)
                 (let* ((u (untag-fixnum (reg-get regs vs)))
                        (shifted (if (>= u 0) (ash u (- amt))
                                     (ash (logand u #xFFFFFFFFFFFFFFFF) (- amt)))))
                   (reg-set regs vd (tag-fixnum shifted)))
                 (setf pc npc3)))))

          (#.+op-sar+ ; arithmetic shift right
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (multiple-value-bind (amt npc3) (fetch-byte bc npc2)
                 (reg-set regs vd
                       (tag-fixnum (ash (untag-fixnum (reg-get regs vs)) (- amt))))
                 (setf pc npc3)))))

          (#.+op-shlv+ ; shift left by register
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (multiple-value-bind (vc npc3) (fetch-reg bc npc2)
                 (reg-set regs vd
                       (tag-fixnum (ash (untag-fixnum (reg-get regs vs))
                                       (untag-fixnum (reg-get regs vc)))))
                 (setf pc npc3)))))

          (#.+op-sarv+ ; arithmetic shift right by register
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (multiple-value-bind (vc npc3) (fetch-reg bc npc2)
                 (reg-set regs vd
                       (tag-fixnum (ash (untag-fixnum (reg-get regs vs))
                                       (- (untag-fixnum (reg-get regs vc))))))
                 (setf pc npc3)))))

          (#.+op-ldb+ ; bit field extract
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (multiple-value-bind (pos npc3) (fetch-byte bc npc2)
                 (multiple-value-bind (size npc4) (fetch-byte bc npc3)
                   (reg-set regs vd
                         (tag-fixnum (ldb (byte size pos)
                                         (untag-fixnum (reg-get regs vs)))))
                   (setf pc npc4))))))

          ;; --- Comparison ---
          (#.+op-cmp+
           (multiple-value-bind (va npc) (fetch-reg bc pc)
             (multiple-value-bind (vb npc2) (fetch-reg bc npc)
               (let ((a (reg-get regs va)) (b (reg-get regs vb)))
                 (setf (mvm-flags state)
                       (cond ((and (integerp a) (integerp b))
                              (cond ((= a b) :eq) ((< a b) :lt) (t :gt)))
                             ((eql a b) :eq)
                             (t :gt))))
               (setf pc npc2))))

          (#.+op-test+
           (multiple-value-bind (va npc) (fetch-reg bc pc)
             (multiple-value-bind (vb npc2) (fetch-reg bc npc)
               (let ((r (if (and (integerp (reg-get regs va)) (integerp (reg-get regs vb)))
                            (logand (reg-get regs va) (reg-get regs vb)) 0)))
                 (setf (mvm-flags state)
                       (cond ((zerop r) :eq) ((< r 0) :lt) (t :gt))))
               (setf pc npc2))))

          ;; --- Branches (offsets relative to end of instruction) ---
          (#.+op-br+
           (multiple-value-bind (off npc) (fetch-s32 bc pc)
             (setf pc (+ npc off))))

          (#.+op-beq+
           (multiple-value-bind (off npc) (fetch-s32 bc pc)
             (setf pc (if (eq (mvm-flags state) :eq) (+ npc off) npc))))

          (#.+op-bne+
           (multiple-value-bind (off npc) (fetch-s32 bc pc)
             (setf pc (if (not (eq (mvm-flags state) :eq)) (+ npc off) npc))))

          (#.+op-blt+
           (multiple-value-bind (off npc) (fetch-s32 bc pc)
             (setf pc (if (eq (mvm-flags state) :lt) (+ npc off) npc))))

          (#.+op-bge+
           (multiple-value-bind (off npc) (fetch-s32 bc pc)
             (setf pc (if (member (mvm-flags state) '(:eq :gt)) (+ npc off) npc))))

          (#.+op-ble+
           (multiple-value-bind (off npc) (fetch-s32 bc pc)
             (setf pc (if (member (mvm-flags state) '(:eq :lt)) (+ npc off) npc))))

          (#.+op-bgt+
           (multiple-value-bind (off npc) (fetch-s32 bc pc)
             (setf pc (if (eq (mvm-flags state) :gt) (+ npc off) npc))))

          (#.+op-bnull+
           (multiple-value-bind (vs npc) (fetch-reg bc pc)
             (multiple-value-bind (off npc2) (fetch-s32 bc npc)
               (setf pc (if (mvm-nil-p (reg-get regs vs)) (+ npc2 off) npc2)))))

          (#.+op-bnnull+
           (multiple-value-bind (vs npc) (fetch-reg bc pc)
             (multiple-value-bind (off npc2) (fetch-s32 bc npc)
               (setf pc (if (not (mvm-nil-p (reg-get regs vs))) (+ npc2 off) npc2)))))

          ;; --- List Operations ---
          ;; --- List ops, ALIGNED MODEL (unified representation) ---
          ;; Registers hold RAW WORDS.  Reinterpret to a real value with
          ;; %word->val, do the NATIVE list op on the real (shared-heap) object,
          ;; and %val->word the result back to a raw word.  So a cons the
          ;; interpreter builds IS a real native cons holding real values —
          ;; directly usable by native runtime functions, no marshalling.
          ;; (mvm-boolean already returns the raw word of t/nil, so consp/atom
          ;; store it directly.)  GC-SAFE: reg-get/reg-set keep regs slots holding
          ;; real VALUES, so the moving collector traces+updates held pointers
          ;; (validated by the list-build-under-early-GC stress test).
          (#.+op-car+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (let ((v (svref regs vs)))
                 (setf (svref regs vd)
                       (cond ((consp v) (car v))
                             ((null v) nil)
                             (t (error 'mvm-type-error :operation "CAR"
                                       :expected "cons or nil" :got v)))))
               (setf pc npc2))))

          (#.+op-cdr+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (let ((v (svref regs vs)))
                 (setf (svref regs vd)
                       (cond ((consp v) (cdr v))
                             ((null v) nil)
                             (t (error 'mvm-type-error :operation "CDR"
                                       :expected "cons or nil" :got v)))))
               (setf pc npc2))))

          (#.+op-cons+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (let ((cell (cons (svref regs va) (svref regs vb))))
                   (push cell (mvm-heap state))      ; keep alive (anti-collection)
                   (setf (svref regs vd) cell))
                 (setf pc npc3)))))

          (#.+op-setcar+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (let ((cell (svref regs vd)))
                 (unless (consp cell)
                   (error 'mvm-type-error :operation "SETCAR" :expected "cons" :got cell))
                 (rplaca cell (svref regs vs)))
               (setf pc npc2))))

          (#.+op-setcdr+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (let ((cell (svref regs vd)))
                 (unless (consp cell)
                   (error 'mvm-type-error :operation "SETCDR" :expected "cons" :got cell))
                 (rplacd cell (svref regs vs)))
               (setf pc npc2))))

          (#.+op-consp+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (reg-set regs vd (mvm-boolean (consp (%word->val (reg-get regs vs)))))
               (setf pc npc2))))

          (#.+op-atom+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (reg-set regs vd (mvm-boolean (atom (%word->val (reg-get regs vs)))))
               (setf pc npc2))))

          ;; --- Object Operations ---
          (#.+op-alloc-obj+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (size npc2) (fetch-u16 bc npc)
               (multiple-value-bind (subtag npc3) (fetch-byte bc npc2)
                 (let ((obj (%alloc-native size subtag)))
                   (push obj (mvm-heap state))
                   (reg-set regs vd (%val->word obj)))
                 (setf pc npc3)))))

          ;; Arrays / strings / objects — REAL native CL objects.  Allocate via
          ;; %alloc-native; slot access via the native %prim-aref/%prim-aset/
          ;; %prim-array-length (which run natively, not back through here).  So
          ;; these objects cross the native bridge to real CL functions.
          (#.+op-alloc-array+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vcount npc2) (fetch-reg bc npc)
               (let ((obj (make-array (%word->val (reg-get regs vcount)) :initial-element nil)))
                 (push obj (mvm-heap state))
                 (reg-set regs vd (%val->word obj)))
               (setf pc npc2))))

          (#.+op-alloc-string+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (let ((obj (make-string (%word->val (reg-get regs vs)) :initial-element #\Space)))
                 (push obj (mvm-heap state))
                 (reg-set regs vd (%val->word obj)))
               (setf pc npc2))))

          (#.+op-aref+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vobj npc2) (fetch-reg bc npc)
               (multiple-value-bind (vidx npc3) (fetch-reg bc npc2)
                 ;; regs hold real VALUES — move them directly (svref).  The old
                 ;; `(%word->val (reg-get ...))` round-trip (value->word->value)
                 ;; overflowed the in-image fixnum range for boundary-magnitude
                 ;; element values (e.g. a small-bignum's lo limb 2^62-k).
                 (let ((obj (svref regs vobj))
                       (idx (svref regs vidx)))
                   (setf (svref regs vd) (%obj-elt-ref obj idx)))
                 (setf pc npc3)))))

          (#.+op-aset+
           (multiple-value-bind (vobj npc) (fetch-reg bc pc)
             (multiple-value-bind (vidx npc2) (fetch-reg bc npc)
               (multiple-value-bind (vs npc3) (fetch-reg bc npc2)
                 (let ((obj (svref regs vobj))
                       (idx (svref regs vidx)))
                   (%obj-elt-set obj idx (svref regs vs)))
                 (setf pc npc3)))))

          (#.+op-array-len+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vobj npc2) (fetch-reg bc npc)
               (let ((obj (svref regs vobj)))
                 (setf (svref regs vd) (%obj-elt-len obj)))
               (setf pc npc2))))

          (#.+op-obj-ref+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vobj npc2) (fetch-reg bc npc)
               (multiple-value-bind (idx npc3) (fetch-byte bc npc2)
                 (let ((obj (svref regs vobj)))
                   (setf (svref regs vd) (%obj-elt-ref obj idx)))
                 (setf pc npc3)))))

          (#.+op-obj-set+
           (multiple-value-bind (vobj npc) (fetch-reg bc pc)
             (multiple-value-bind (idx npc2) (fetch-byte bc npc)
               (multiple-value-bind (vs npc3) (fetch-reg bc npc2)
                 (let ((obj (svref regs vobj)))
                   (%obj-elt-set obj idx (svref regs vs)))
                 (setf pc npc3)))))

          (#.+op-obj-tag+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (let ((obj (%word->val (reg-get regs vs))))
                 (reg-set regs vd
                       (tag-fixnum (cond ((consp obj) +tag-cons+)
                                         ((integerp obj) +tag-fixnum+)
                                         (t +tag-object+)))))  ; native object
               (setf pc npc2))))

          (#.+op-obj-subtag+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               ;; native subtag extraction via the obj-subtag primop.
               ;;
               ;; Two callable representations cross this opcode and they need
               ;; DIFFERENT subtags so funcall's dispatch routes them correctly:
               ;;   - a native CLOSURE object (subtag #x52), built by
               ;;     compile-make-closure / %make-closure for a CAPTURING
               ;;     lambda — funcall must take the set-cenv + call-indirect
               ;;     closure path, which is gated on subtag #x52.
               ;;   - a bare native FUNCTION (a raw fn-addr resolved by
               ;;     op-FN-ADDR for #'NAME / a captureless lambda) — NOT a
               ;;     tagged object, so the raw obj-subtag primop reads garbage;
               ;;     report #x51 so funcall skips sym/closure/array and falls
               ;;     through to CALL-INDIRECT's bridge.
               ;; The earlier `(if (functionp obj) #x51 …)` collapsed BOTH to
               ;; #x51 because native functionp is true for a #x52 closure too,
               ;; so every CAPTURING lambda fell out of the closure path → the
               ;; funcall returned NIL (WS4 oracle).  Fix: prefer the real
               ;; obj-subtag for genuine OBJECTS (tag = object); only the
               ;; non-object callable (raw fn-addr) gets the #x51 fallback.
               (let* ((obj (%word->val (reg-get regs vs)))
                      (raw-st (obj-subtag obj))
                      (st (cond
                            ;; A native CLOSURE object reads a clean #x52 from
                            ;; the obj-subtag primop — keep it so funcall takes
                            ;; the closure (set-cenv) path.  Checked BEFORE the
                            ;; functionp arm: native functionp is true for a
                            ;; #x52 closure too, and the old `functionp→#x51`
                            ;; ordering swallowed every CAPTURING lambda.
                            ((= raw-st #x52) #x52)
                            ;; A bare native FUNCTION (raw fn-addr from #'NAME /
                            ;; captureless lambda) is NOT a tagged object, so
                            ;; obj-subtag's read above is garbage — report #x51
                            ;; so funcall falls through to CALL-INDIRECT's
                            ;; native bridge (funcall/apply/mapcar #'NAME).
                            ((functionp obj) #x51)
                            (t raw-st))))
                 (reg-set regs vd (tag-fixnum st)))
               (setf pc npc2))))

          ;; --- Raw Memory ---
          (#.+op-load+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vaddr npc2) (fetch-reg bc npc)
               (multiple-value-bind (width npc3) (fetch-byte bc npc2)
                 (let ((addr (reg-get regs vaddr)))
                   ;; compile-mem-ref already untags the address (:shr addr 1),
                   ;; so the reg holds the raw address VALUE.  reg-get's %val->word
                   ;; restores the full address word; do NOT untag again (that
                   ;; extra halving collapsed adjacent 8-byte slots to 2-byte
                   ;; physical spacing, corrupting the MV-VALUES region — every
                   ;; secondary's high 32 bits got the next slot's value).
                   (reg-set regs vd (mem-read state addr width)))
                 (setf pc npc3)))))

          (#.+op-store+
           (multiple-value-bind (vaddr npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (multiple-value-bind (width npc3) (fetch-byte bc npc2)
                 (let ((addr (reg-get regs vaddr)) (val (reg-get regs vs)))
                   ;; See op-load: the address is already untagged by compile-
                   ;; mem-ref's :shr; reg-get restores its word — do NOT untag
                   ;; again (the extra halving collapsed MV slot spacing 4x).
                   ;; mem-ref :u64 (width 3) is RAW — no tag shift (CLHS-aligned
                   ;; with the native mem-ref).  Storing the untagged value here
                   ;; while op-load %word->val's on read gave a one-SAR asymmetry
                   ;; that corrupted the multiple-value slots (#x10000090/98): the
                   ;; MV-count 2 read back as 1, so values/multiple-value-list/
                   ;; nth-value/MOD saw only the primary (WS4 oracle).  Keep the
                   ;; untag for :u8/:u32 (width<3), which want the raw low bytes;
                   ;; store the WORD (reg-get) for :u64 so op-load's %word->val
                   ;; round-trips it (works for fixnum AND pointer MV values).
                   (when (and (< width 3) (integerp val)) (setf val (untag-fixnum val)))
                   (mem-write state addr val width))
                 (setf pc npc3)))))

          (#.+op-fence+ nil) ; memory barrier: no-op

          ;; --- IEEE float arithmetic ---
          ;; Operands are REAL native float objects stored directly in the
          ;; register file (op-alloc-obj stores via reg-set∘%val->word, a
          ;; round-trip identity), so read them with raw svref and delegate to
          ;; the native %float-* primops (SSE2 in this compiled interp); store
          ;; the result float the same way op-alloc-obj does.  Without these,
          ;; (/ 1.0 4.0) — which compiles to %float-div (the FDIV opcode), not a
          ;; GENERIC-DIV bridge call like + / * — errored in eval2 (WS4 oracle).
          (#.+op-fadd+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (reg-set regs vd (%val->word (%float-add (svref regs va) (svref regs vb))))
                 (setf pc npc3)))))
          (#.+op-fsub+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (reg-set regs vd (%val->word (%float-sub (svref regs va) (svref regs vb))))
                 (setf pc npc3)))))
          (#.+op-fmul+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (reg-set regs vd (%val->word (%float-mul (svref regs va) (svref regs vb))))
                 (setf pc npc3)))))
          (#.+op-fdiv+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (reg-set regs vd (%val->word (%float-div (svref regs va) (svref regs vb))))
                 (setf pc npc3)))))
          (#.+op-itof+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (reg-set regs vd (%val->word (%float-from-int (svref regs vs))))
               (setf pc npc2))))
          (#.+op-ftoi+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               ;; %float-to-int returns an integer VALUE; %val->word so reg-set's
               ;; %word->val round-trips it (like the fixnum-arith opcodes).
               (reg-set regs vd (%val->word (%float-to-int (svref regs vs))))
               (setf pc npc2))))

          ;; --- Function Calling ---
          (#.+op-call+
           (multiple-value-bind (target npc) (fetch-u32 bc pc)
             (if (and runtime-table (>= target +mvm-runtime-call-base+))
                 ;; Runtime native call: funcall the real function inline with
                 ;; %word->val'd args (raw words -> real values, no marshalling),
                 ;; store %val->word of the result in VR, continue after the CALL.
                 (let* ((name (gethash target runtime-table))
                        (fn (%mvm-resolve-runtime-fn name))
                        (nargs (mvm-nargs state)))
                   (if fn
                       (let ((args
                              ;; regs hold real VALUES — pass them directly (the
                              ;; old %word->val∘reg-get round-trip overflowed for
                              ;; a boundary-fixnum arg/result).  %mvm-collect-call-
                              ;; args reads args 0..3 from V0..V3 and args 4+ from
                              ;; the mvm-stack (overflow), wrapping any escaping
                              ;; eval2 lambda value (a #x52 closure-over-offset or
                              ;; a bare in-module offset) in a re-entrant
                              ;; trampoline so a NATIVE higher-order callee
                              ;; (mapcar/reduce/…) can funcall it.  Pre-fix this
                              ;; read regs[0..nargs-1] only, so a >4-arg native
                              ;; call got garbage for its 5th+ arg.
                              (%mvm-collect-call-args state regs nargs
                                                      bc ftab runtime-table
                                                      lambda-offsets)))
                         ;; PROPAGATE SECONDARY VALUES across the bridge.  Native
                         ;; multi-valued fns (floor/truncate/round/rem returning a
                         ;; quotient AND remainder) write their secondaries to the
                         ;; REAL MV slots, which eval2 never reads — eval2 reads the
                         ;; SIMULATED MV slots (the mvm-memory hash, where compile-
                         ;; values / multiple-value-bind store via op-store and read
                         ;; via op-load).  So capture ALL return values here and
                         ;; mirror them into the simulated slots using the SAME word
                         ;; encoding op-store uses for :u64 (the WORD = %val->word of
                         ;; the value): mem-write count to +mv-count-addr+ and each
                         ;; secondary's word to +mv-values-addr+ + i*8.  The primary
                         ;; still goes in VR (single-value calls write count=1, so the
                         ;; non-MV path is unchanged).  Constants are interp-local
                         ;; (the compiler loads after interp, so its +mv-*-addr+
                         ;; defconstants aren't in scope here) — use the raw addrs.
                         (let* ((vals (multiple-value-list (apply fn args)))
                                (nvals (length vals)))
                           (setf (svref regs +vreg-vr+) (car vals))
                           ;; count slot (#x10000090) holds the WORD of the fixnum
                           ;; count — match a normal (setf (mem-ref ... :u64) n),
                           ;; which stores (reg-get) = %val->word of the fixnum.
                           (mem-write state #x10000090 (%val->word nvals) 3)
                           ;; secondaries (value 1+) -> #x10000098 + i*8, as words.
                           (let ((i 0))
                             (dolist (v (cdr vals))
                               (mem-write state (+ #x10000098 (* i 8))
                                          (%val->word v) 3)
                               (incf i)))))
                       (reg-set regs +vreg-vr+ +mvm-nil+))
                   (setf pc npc))
                 ;; In-module call: push return frame, jump to the bytecode.
                 ;; SAVE VFP (the caller's frame array) in the frame: the
                 ;; callee's frame-enter overwrites VFP with a fresh array and
                 ;; nothing restored it, so after the callee RET the caller
                 ;; read its OWN locals through the callee's frame → a local
                 ;; re-read after a call returned NIL (the accumulator
                 ;; `(funcall f) (funcall f)` lost f on the 2nd read).  op-ret
                 ;; restores VFP from the frame.
                 (progn
                   (push (list npc (mvm-stack state) (svref regs +vreg-vfp+))
                         (mvm-call-stack state))
                   (setf pc (if (< target (length ftab)) (aref ftab target) target))))))

          (#.+op-fn-addr+
           ;; (fn-addr Vd target:imm32) — load a callable for #'NAME / (function
           ;; NAME) / a captureless lambda.  The target is either:
           ;;   - a RUNTIME stub offset (>= runtime-call-base): #'NAME of a
           ;;     native function.  Resolve the name to the REAL native function
           ;;     OBJECT and store it directly in the slot (the alloc-obj store
           ;;     convention: reg-set∘%val->word = identity), so CALL-INDIRECT's
           ;;     functionp branch bridge-calls it.  This is the higher-order
           ;;     eval2 path (funcall/apply/mapcar #'NAME).
           ;;   - an in-module bytecode OFFSET: a captureless lambda or a defun
           ;;     compiled in THIS thunk.  Store the offset as a plain fixnum
           ;;     value so CALL-INDIRECT's integer branch jumps to the bytecode.
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (target npc2) (fetch-u32 bc npc)
               (if (and runtime-table (>= target +mvm-runtime-call-base+))
                   (let* ((name (gethash target runtime-table))
                          (fn (and name (%mvm-resolve-runtime-fn name))))
                     (if (functionp fn)
                         (reg-set regs vd (%val->word fn))
                         (reg-set regs vd +mvm-nil+)))
                   (setf (svref regs vd) target))
               (setf pc npc2))))

          (#.+op-call-ind+
           (multiple-value-bind (vs npc) (fetch-reg bc pc)
             ;; Read the raw stored VALUE (svref), not reg-get: a FN-ADDR to an
             ;; out-of-module name stores a real native function OBJECT directly
             ;; in the slot (the alloc-obj store convention), and reg-get's
             ;; %val->word would SHL-1 that object into garbage.  An in-module
             ;; FN-ADDR stores the bytecode OFFSET as a tagged fixnum value.
             (let ((target (svref regs vs)))
               (cond
                 ;; In-module bytecode offset (a plain integer): jump to it.
                 ;; MUST be checked BEFORE the functionp branch: a closure's
                 ;; fn-slot (and a captureless lambda's li-func) stores the
                 ;; closure/lambda's in-module bytecode OFFSET as a small
                 ;; integer, and functionp's code-range heuristic returns T
                 ;; for such a small integer — so the old functionp-first
                 ;; order routed every in-module closure call into the native
                 ;; bridge `(apply <offset> args)` → SIGSEGV (the offset is
                 ;; not a real function).  A genuine resolved native function
                 ;; OBJECT (the #'NAME bridge target) is NOT integerp, so it
                 ;; correctly falls through to the functionp branch below.
                 ;; Save VFP in the frame (see op-CALL) so a local re-read in
                 ;; the caller after this call survives the callee's frame.
                 ((integerp target)
                  (push (list npc (mvm-stack state) (svref regs +vreg-vfp+))
                        (mvm-call-stack state))
                  (setf pc target))
                 ;; Higher-order eval2 bridge: a resolved native function object
                 ;; (#'+ , #'1+ , #'< , a %*-FN wrapper, etc.).  funcall/apply/
                 ;; mapcar all route through here.  Pull nargs args (V0..) from
                 ;; the register file exactly as op-CALL's runtime bridge does and
                 ;; store the primary result in VR; do NOT push a return frame
                 ;; (the call completes natively, control returns inline).
                 ((functionp target)
                  (let* ((nargs (mvm-nargs state))
                         ;; Read args 0..3 from V0..V3 and args 4+ from the
                         ;; mvm-stack overflow (see op-CALL's bridge + %mvm-
                         ;; collect-call-args).  Wraps any escaping eval2 lambda
                         ;; arg in a trampoline so a native HOF reached via
                         ;; funcall/apply (e.g. (apply #'mapcar (list lambda
                         ;; list))) can funcall it.  Pre-fix this read
                         ;; regs[0..nargs-1] only → garbage for the 5th+ arg.
                         (args (%mvm-collect-call-args state regs nargs
                                                       bc ftab runtime-table
                                                       lambda-offsets)))
                    (setf (svref regs +vreg-vr+) (apply target args))
                    (setf pc npc)))
                 (t
                  (error "MVM: CALL-IND with non-callable target ~S" target))))))

          (#.+op-ret+
           (if (mvm-call-stack state)
               (let ((frame (pop (mvm-call-stack state))))
                 ;; frame = (return-pc saved-stack saved-vfp).  Restore the
                 ;; caller's PC, operand stack AND frame pointer — the last
                 ;; so a local re-read in the caller after this call sees the
                 ;; caller's frame, not the (now-dead) callee's.
                 (setf pc (first frame))
                 (setf (mvm-stack state) (second frame))
                 (setf (svref regs +vreg-vfp+) (third frame)))
               (setf (mvm-halted state) t)))

          (#.+op-tailcall+
           (multiple-value-bind (target npc) (fetch-u32 bc pc)
             (declare (ignore npc))
             (setf pc (if (< target (length ftab)) (aref ftab target) target))))

          ;; --- GC / Allocation ---
          (#.+op-alloc-cons+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (let ((cell (cons nil nil)))
               (push cell (mvm-heap state))
               (reg-set regs vd (%val->word cell)))   ; store the cons's raw word
             (setf pc npc)))

          (#.+op-gc-check+ nil) ; no-op in interpreter
          (#.+op-mcgc-collect+ nil) ; no-op in interpreter (no page pool)

          (#.+op-write-barrier+
           (multiple-value-bind (_vobj npc) (fetch-reg bc pc)
             (declare (ignore _vobj))
             (setf pc npc)))

          ;; --- Calling convention (nargs / closure-env / MV-count) ---
          (#.+op-set-nargs+
           (multiple-value-bind (n npc) (fetch-byte bc pc)
             (setf (mvm-nargs state) n) (setf pc npc)))
          (#.+op-get-nargs+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (reg-set regs vd (tag-fixnum (mvm-nargs state))) (setf pc npc)))
          (#.+op-set-cenv+
           (multiple-value-bind (vs npc) (fetch-reg bc pc)
             (setf (mvm-cenv state) (reg-get regs vs)) (setf pc npc)))
          (#.+op-get-cenv+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (reg-set regs vd (mvm-cenv state)) (setf pc npc)))
          (#.+op-set-mv-count+
           (multiple-value-bind (n npc) (fetch-byte bc pc)
             (setf (mvm-mv-count state) n) (setf pc npc)))

          ;; --- Actor / Concurrency ---
          (#.+op-save-ctx+
           (push (copy-seq regs) (mvm-stack state)))

          (#.+op-restore-ctx+
           (let ((saved (pop (mvm-stack state))))
             (when (and saved (typep saved 'simple-vector))
               (replace regs saved)
               (setf pc (reg-get regs +vreg-vpc+)))))

          (#.+op-yield+ nil) ; preemption: no-op

          (#.+op-atomic-xchg+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vaddr npc2) (fetch-reg bc npc)
               (multiple-value-bind (vs npc3) (fetch-reg bc npc2)
                 (let* ((addr (reg-get regs vaddr))
                        (old (gethash addr (mvm-memory state) 0)))
                   (reg-set regs vd old)
                   (setf (gethash addr (mvm-memory state)) (reg-get regs vs)))
                 (setf pc npc3)))))

          ;; --- I/O and System ---
          (#.+op-io-read+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (port npc2) (fetch-u16 bc npc)
               (multiple-value-bind (_w npc3) (fetch-byte bc npc2)
                 (declare (ignore _w))
                 (reg-set regs vd (gethash port (mvm-io-ports state) 0))
                 (setf pc npc3)))))

          (#.+op-io-write+
           (multiple-value-bind (port npc) (fetch-u16 bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (multiple-value-bind (_w npc3) (fetch-byte bc npc2)
                 (declare (ignore _w))
                 (let ((val (reg-get regs vs)))
                   (case port
                     (0 (if (integerp val)     ; debug print
                            (format *standard-output* "~D" (untag-fixnum val))
                            (format *standard-output* "~S" val)))
                     (1 (write-char (code-char  ; char output
                                     (if (integerp val) (untag-fixnum val) 0))
                                    *standard-output*))
                     (2 (if (integerp val)     ; print + newline
                            (format *standard-output* "~D~%" (untag-fixnum val))
                            (format *standard-output* "~S~%" val)))
                     (otherwise (setf (gethash port (mvm-io-ports state)) val))))
                 (setf pc npc3)))))

          (#.+op-halt+ (setf (mvm-halted state) t))
          (#.+op-cli+  (setf (mvm-interrupts-enabled state) nil))
          (#.+op-sti+  (setf (mvm-interrupts-enabled state) t))

          (#.+op-percpu-ref+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (offset npc2) (fetch-u16 bc npc)
               (reg-set regs vd (gethash offset (mvm-percpu state) 0))
               (setf pc npc2))))

          (#.+op-percpu-set+
           (multiple-value-bind (offset npc) (fetch-u16 bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (setf (gethash offset (mvm-percpu state)) (reg-get regs vs))
               (setf pc npc2))))

          (otherwise
           (error "MVM: unknown opcode #x~2,'0X at PC ~D" opcode (1- pc))))
          ;; A bridged native `error`/`throw` (op-call into the runtime `error`
          ;; / `%signal-condition` fn) signals a real host CL condition here.
          ;; If a handler-case frame is active, capture the condition and
          ;; convert it to a LONGJMP (done below, after the unwind); otherwise
          ;; re-signal so eval2-forms' outer handler reports :interp-err (an
          ;; uncaught error / unmatched throw, matching native semantics).
          (error (c)
            (if (mvm-handlers state)
                (setf %lj c)
                (error c))))
        (when %lj
          (let ((rpc (%mvm-longjmp-restore state)))
            (when rpc
              (setf regs (mvm-regs state))
              (setf pc rpc)))))

      (reg-set regs +vreg-vpc+ pc))))

;;; ============================================================
;;; Helper: Run a Function by Index
;;; ============================================================

(defun mvm-run-function (bytecode function-table function-index &rest args)
  "Set up V0-V3 from ARGS (untagged integers), call function, return untagged VR."
  (let* ((buf (make-mvm-buffer))
         (nargs (min (length args) 4)))
    (loop for i below nargs for arg in args
          do (mvm-li buf i (tag-fixnum arg)))
    (loop for i from nargs below 4 do (mvm-li buf i 0))
    (mvm-call buf function-index)
    (mvm-halt buf)
    (let* ((prefix (mvm-buffer-used-bytes buf))
           (plen (length prefix))
           (combined (make-array (+ plen (length bytecode))
                                 :element-type '(unsigned-byte 8)))
           (adj-ftab (when function-table
                       (let ((ft (make-array (length function-table))))
                         (dotimes (i (length function-table) ft)
                           (setf (aref ft i) (+ (aref function-table i) plen)))))))
      (replace combined prefix)
      (replace combined bytecode :start1 plen)
      (let ((result (mvm-interpret combined :entry-point 0
                                            :function-table adj-ftab)))
        (if (integerp result) (untag-fixnum result) result)))))

;;; ============================================================
;;; Self-Test
;;; ============================================================

(defun mvm-interp-test ()
  "Run basic MVM interpreter tests.  Returns T if all pass."
  (let ((pass 0) (fail 0))
    (labels
        ((check (name expected actual)
           (if (eql expected actual)
               (progn (incf pass) (format t "  PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "  FAIL: ~A  expected ~S, got ~S~%"
                              name expected actual))))
         (run2 (name expected emit-fn)
           (let ((buf (make-mvm-buffer)))
             (funcall emit-fn buf) (mvm-halt buf)
             (check name expected (mvm-interpret (mvm-buffer-used-bytes buf))))))

      (format t "~%MVM Interpreter Tests~%======================~%")

      ;; Arithmetic
      (run2 "ADD 10+20=30" (tag-fixnum 30)
            (lambda (b) (mvm-li b +vreg-v0+ (tag-fixnum 10))
              (mvm-li b +vreg-v1+ (tag-fixnum 20))
              (mvm-add b +vreg-vr+ +vreg-v0+ +vreg-v1+)))
      (run2 "SUB 50-17=33" (tag-fixnum 33)
            (lambda (b) (mvm-li b +vreg-v0+ (tag-fixnum 50))
              (mvm-li b +vreg-v1+ (tag-fixnum 17))
              (mvm-sub b +vreg-vr+ +vreg-v0+ +vreg-v1+)))
      (run2 "MUL 6*7=42" (tag-fixnum 42)
            (lambda (b) (mvm-li b +vreg-v0+ (tag-fixnum 6))
              (mvm-li b +vreg-v1+ (tag-fixnum 7))
              (mvm-mul b +vreg-vr+ +vreg-v0+ +vreg-v1+)))
      (run2 "DIV 100/7=14" (tag-fixnum 14)
            (lambda (b) (mvm-li b +vreg-v0+ (tag-fixnum 100))
              (mvm-li b +vreg-v1+ (tag-fixnum 7))
              (mvm-div b +vreg-vr+ +vreg-v0+ +vreg-v1+)))
      (run2 "MOD 17%5=2" (tag-fixnum 2)
            (lambda (b) (mvm-li b +vreg-v0+ (tag-fixnum 17))
              (mvm-li b +vreg-v1+ (tag-fixnum 5))
              (mvm-mod b +vreg-vr+ +vreg-v0+ +vreg-v1+)))
      (run2 "NEG 42=-42" (tag-fixnum -42)
            (lambda (b) (mvm-li b +vreg-v0+ (tag-fixnum 42))
              (mvm-neg b +vreg-vr+ +vreg-v0+)))
      (run2 "INC x3=3" (tag-fixnum 3)
            (lambda (b) (mvm-li b +vreg-v0+ (tag-fixnum 0))
              (mvm-inc b +vreg-v0+) (mvm-inc b +vreg-v0+) (mvm-inc b +vreg-v0+)
              (mvm-mov b +vreg-vr+ +vreg-v0+)))
      (run2 "DEC x3 from 10=7" (tag-fixnum 7)
            (lambda (b) (mvm-li b +vreg-v0+ (tag-fixnum 10))
              (mvm-dec b +vreg-v0+) (mvm-dec b +vreg-v0+) (mvm-dec b +vreg-v0+)
              (mvm-mov b +vreg-vr+ +vreg-v0+)))

      ;; Bitwise
      (run2 "AND #xFF&#x0F=#x0F" (tag-fixnum #x0F)
            (lambda (b) (mvm-li b +vreg-v0+ (tag-fixnum #xFF))
              (mvm-li b +vreg-v1+ (tag-fixnum #x0F))
              (mvm-and b +vreg-vr+ +vreg-v0+ +vreg-v1+)))
      (run2 "OR #xF0|#x0F=#xFF" (tag-fixnum #xFF)
            (lambda (b) (mvm-li b +vreg-v0+ (tag-fixnum #xF0))
              (mvm-li b +vreg-v1+ (tag-fixnum #x0F))
              (mvm-or b +vreg-vr+ +vreg-v0+ +vreg-v1+)))
      (run2 "XOR #xFF^#xFF=0" (tag-fixnum 0)
            (lambda (b) (mvm-li b +vreg-v0+ (tag-fixnum #xFF))
              (mvm-li b +vreg-v1+ (tag-fixnum #xFF))
              (mvm-xor b +vreg-vr+ +vreg-v0+ +vreg-v1+)))
      (run2 "SHL 1<<10=1024" (tag-fixnum 1024)
            (lambda (b) (mvm-li b +vreg-v0+ (tag-fixnum 1))
              (mvm-shl b +vreg-vr+ +vreg-v0+ 10)))
      (run2 "SAR 1024>>3=128" (tag-fixnum 128)
            (lambda (b) (mvm-li b +vreg-v0+ (tag-fixnum 1024))
              (mvm-sar b +vreg-vr+ +vreg-v0+ 3)))
      (run2 "LDB #xABCD pos=4 size=8=#xBC" (tag-fixnum #xBC)
            (lambda (b) (mvm-li b +vreg-v0+ (tag-fixnum #xABCD))
              (mvm-ldb b +vreg-vr+ +vreg-v0+ 4 8)))

      ;; Data movement
      (run2 "PUSH/POP preserves 42" (tag-fixnum 42)
            (lambda (b) (mvm-li b +vreg-v0+ (tag-fixnum 42))
              (mvm-push b +vreg-v0+) (mvm-li b +vreg-v0+ (tag-fixnum 0))
              (mvm-pop b +vreg-vr+)))

      ;; Branches
      (run2 "Loop to 5" (tag-fixnum 5)
            (lambda (b) (mvm-li b +vreg-v0+ (tag-fixnum 0))
              (mvm-li b +vreg-v1+ (tag-fixnum 5))
              (mvm-inc b +vreg-v0+) (mvm-cmp b +vreg-v0+ +vreg-v1+)
              (mvm-blt b -8) (mvm-mov b +vreg-vr+ +vreg-v0+)))
      (run2 "BR forward" (tag-fixnum 0)
            (lambda (b) (mvm-li b +vreg-vr+ (tag-fixnum 0))
              (mvm-br b 10) (mvm-li b +vreg-vr+ (tag-fixnum 999))))
      (run2 "BNULL branch on nil" (tag-fixnum 42)
            (lambda (b) (mvm-li b +vreg-v0+ 0) (mvm-li b +vreg-v1+ (tag-fixnum 42))
              (mvm-bnull b +vreg-v0+ 10) (mvm-li b +vreg-v1+ (tag-fixnum 99))
              (mvm-mov b +vreg-vr+ +vreg-v1+)))

      ;; List operations
      (run2 "CONS/CAR/CDR: car+cdr=30" (tag-fixnum 30)
            (lambda (b) (mvm-li b +vreg-v0+ (tag-fixnum 10))
              (mvm-li b +vreg-v1+ (tag-fixnum 20))
              (mvm-cons b +vreg-v2+ +vreg-v0+ +vreg-v1+)
              (mvm-car b +vreg-v3+ +vreg-v2+) (mvm-cdr b +vreg-v4+ +vreg-v2+)
              (mvm-add b +vreg-vr+ +vreg-v3+ +vreg-v4+)))
      (run2 "CONSP of cons=T" +mvm-t+
            (lambda (b) (mvm-li b +vreg-v0+ (tag-fixnum 10))
              (mvm-li b +vreg-v1+ (tag-fixnum 20))
              (mvm-cons b +vreg-v2+ +vreg-v0+ +vreg-v1+)
              (mvm-consp b +vreg-vr+ +vreg-v2+)))
      (run2 "SETCAR: car=99" (tag-fixnum 99)
            (lambda (b) (mvm-li b +vreg-v0+ (tag-fixnum 1))
              (mvm-li b +vreg-v1+ (tag-fixnum 2))
              (mvm-cons b +vreg-v2+ +vreg-v0+ +vreg-v1+)
              (mvm-li b +vreg-v3+ (tag-fixnum 99))
              (mvm-setcar b +vreg-v2+ +vreg-v3+)
              (mvm-car b +vreg-vr+ +vreg-v2+)))

      ;; Object operations
      (run2 "OBJ alloc/set/ref=99" (tag-fixnum 99)
            (lambda (b) (mvm-alloc-obj b +vreg-v0+ 3 #x42)
              (mvm-li b +vreg-v1+ (tag-fixnum 99))
              (mvm-obj-set b +vreg-v0+ 0 +vreg-v1+)
              (mvm-obj-ref b +vreg-vr+ +vreg-v0+ 0)))
      (run2 "OBJ-SUBTAG=#xAB" (tag-fixnum #xAB)
            (lambda (b) (mvm-alloc-obj b +vreg-v0+ 2 #xAB)
              (mvm-obj-subtag b +vreg-vr+ +vreg-v0+)))

      ;; Function CALL/RET
      (let ((fb (make-mvm-buffer)) (mb (make-mvm-buffer)))
        (mvm-add fb +vreg-vr+ +vreg-v0+ +vreg-v1+) (mvm-ret fb)
        (let* ((fv (mvm-buffer-used-bytes fb)) (fl (length fv)) (ft (vector 0)))
          (mvm-li mb +vreg-v0+ (tag-fixnum 3))
          (mvm-li mb +vreg-v1+ (tag-fixnum 4))
          (mvm-call mb 0) (mvm-halt mb)
          (let* ((mv (mvm-buffer-used-bytes mb))
                 (c (make-array (+ fl (length mv)) :element-type '(unsigned-byte 8))))
            (replace c fv) (replace c mv :start1 fl)
            (check "CALL/RET: add(3,4)=7" (tag-fixnum 7)
                   (mvm-interpret c :entry-point fl :function-table ft)))))

      ;; mvm-run-function helper
      (let ((buf (make-mvm-buffer)))
        (mvm-mul buf +vreg-vr+ +vreg-v0+ +vreg-v1+) (mvm-ret buf)
        (check "mvm-run-function: mul(6,7)=42" 42
               (mvm-run-function (mvm-buffer-used-bytes buf) (vector 0) 0 6 7)))

      ;; Tail call: iterative V1+V0
      (let ((buf (make-mvm-buffer)))
        (mvm-li buf +vreg-v4+ (tag-fixnum 0))
        (mvm-cmp buf +vreg-v0+ +vreg-v4+)
        (mvm-bne buf 4) (mvm-mov buf +vreg-vr+ +vreg-v1+) (mvm-ret buf)
        (mvm-dec buf +vreg-v0+) (mvm-inc buf +vreg-v1+) (mvm-tailcall buf 0)
        (let* ((fv (mvm-buffer-used-bytes buf)) (fl (length fv))
               (ft (vector 0)) (mb (make-mvm-buffer)))
          (mvm-li mb +vreg-v0+ (tag-fixnum 5))
          (mvm-li mb +vreg-v1+ (tag-fixnum 10))
          (mvm-call mb 0) (mvm-halt mb)
          (let* ((mv (mvm-buffer-used-bytes mb))
                 (c (make-array (+ fl (length mv)) :element-type '(unsigned-byte 8))))
            (replace c fv) (replace c mv :start1 fl)
            (check "TAILCALL: add(5,10)=15" (tag-fixnum 15)
                   (mvm-interpret c :entry-point fl :function-table ft)))))

      ;; I/O write (visual check)
      (let ((buf (make-mvm-buffer)))
        (mvm-li buf +vreg-v0+ (tag-fixnum 12345))
        (mvm-io-write buf 0 +vreg-v0+ 0)
        (mvm-li buf +vreg-vr+ (tag-fixnum 0)) (mvm-halt buf)
        (format t "  I/O port 0 output: ")
        (mvm-interpret (mvm-buffer-used-bytes buf))
        (format t "~%")
        (check "IO-WRITE port 0" t t))

      (format t "~%Results: ~D passed, ~D failed~%" pass fail)
      (zerop fail))))
