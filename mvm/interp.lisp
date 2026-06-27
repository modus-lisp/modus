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
  (mv-count 1))

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
;;; The Main Interpreter
;;; ============================================================

(defun mvm-interpret (bytecode &key (entry-point 0) function-table runtime-table
                                    (return-raw t))
  "Execute MVM bytecode starting at ENTRY-POINT.
   When RET or HALT is reached, return VR.  RETURN-RAW (default T, for callers
   that re-tag the result themselves via `(ash result -1)`) returns the RAW WORD
   %val->word(VR); pass RETURN-RAW NIL to get VR's VALUE directly — needed for
   boundary fixnums (|value| >= 2^61) whose word exceeds the in-image 62-bit
   fixnum range, so %val->word (and the caller's later %word->val) would overflow.
   RUNTIME-TABLE (optional, synthetic-offset -> native fn name string) routes
   CALLs to functions outside the bytecode module to a direct native funcall."
  (let* ((state (make-mvm-state))
         (bc bytecode) (pc entry-point) (len (length bc))
         (ftab (or function-table (vector)))
         (regs (mvm-regs state)))
    (declare (type fixnum pc len) (type simple-vector regs) (ignorable ftab))
    (reg-set regs +vreg-vn+ +mvm-nil+)  ; VN holds the canonical NIL immediate
    (reg-set regs +vreg-vpc+ pc)

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
      (let ((opcode (aref bc pc)))
        (setf pc (1+ pc))
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
               ;; native subtag extraction via the obj-subtag primop.  A native
               ;; FUNCTION object (resolved by op-FN-ADDR for #'NAME) is a
               ;; callable whose representation the raw obj-subtag primop does
               ;; not read cleanly here, so report its subtag (#x51 function)
               ;; directly.  This lets funcall's dispatch correctly skip the
               ;; symbol(#x50)/closure(#x52)/array(#x32) branches and fall
               ;; through to CALL-INDIRECT, which bridge-calls it (higher-order
               ;; eval2: funcall/apply/mapcar #'NAME).
               (let ((obj (%word->val (reg-get regs vs))))
                 (reg-set regs vd (tag-fixnum (if (functionp obj)
                                                  #x51
                                                  (obj-subtag obj)))))
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
                       (let ((args nil))
                         ;; regs hold real VALUES — pass them directly (the old
                         ;; %word->val∘reg-get round-trip overflowed for a
                         ;; boundary-fixnum arg/result).
                         (dotimes (i nargs)
                           (push (svref regs (- nargs 1 i)) args))
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
                 (progn
                   (push (cons npc (mvm-stack state)) (mvm-call-stack state))
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
                 ;; Higher-order eval2 bridge: a resolved native function object
                 ;; (#'+ , #'1+ , #'< , a %*-FN wrapper, etc.).  funcall/apply/
                 ;; mapcar all route through here.  Pull nargs args (V0..) from
                 ;; the register file exactly as op-CALL's runtime bridge does and
                 ;; store the primary result in VR; do NOT push a return frame
                 ;; (the call completes natively, control returns inline).
                 ((functionp target)
                  (let ((nargs (mvm-nargs state))
                        (args nil))
                    (dotimes (i nargs)
                      (push (svref regs (- nargs 1 i)) args))
                    (setf (svref regs +vreg-vr+) (apply target args))
                    (setf pc npc)))
                 ;; In-module bytecode offset (a fixnum value): jump to it.
                 ((integerp target)
                  (push (cons npc (mvm-stack state)) (mvm-call-stack state))
                  (setf pc target))
                 (t
                  (error "MVM: CALL-IND with non-callable target ~S" target))))))

          (#.+op-ret+
           (if (mvm-call-stack state)
               (let ((frame (pop (mvm-call-stack state))))
                 (setf pc (car frame))
                 (setf (mvm-stack state) (cdr frame)))
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
           (error "MVM: unknown opcode #x~2,'0X at PC ~D" opcode (1- pc)))))

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
