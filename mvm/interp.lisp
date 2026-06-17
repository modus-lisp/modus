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
(defconstant +mvm-nil+ 0)
(defconstant +mvm-t+   2)  ; tagged fixnum 1

(declaim (inline tag-fixnum untag-fixnum mvm-nil-p mvm-boolean))
(defun tag-fixnum (n) (ash n 1))
(defun untag-fixnum (tagged) (ash tagged -1))
(defun mvm-nil-p (val) (or (null val) (eql val +mvm-nil+)))
(defun mvm-boolean (b) (if b +mvm-t+ +mvm-nil+))

;;; Interpreter state

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
  (io-ports (make-hash-table :test 'eql)))

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

;;; Object representation (vector: slot 0 = header, slots 1..N = data)

(defun make-mvm-object (size subtag)
  (let ((obj (make-array (+ 1 size) :initial-element 0)))
    (setf (svref obj 0) (logior (ash subtag 8) +tag-object+))
    obj))

(defun mvm-obj-tag-val (obj) (logand (svref obj 0) #xF))
(defun mvm-obj-subtag-val (obj) (logand (ash (svref obj 0) -8) #xFF))

;;; Bytecode fetch helpers

(declaim (inline fetch-byte fetch-reg fetch-u16 fetch-s32 fetch-u32 fetch-u64))

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
  (let ((lo (logior (aref bc pc) (ash (aref bc (+ pc 1)) 8)
                    (ash (aref bc (+ pc 2)) 16) (ash (aref bc (+ pc 3)) 24)))
        (hi (logior (aref bc (+ pc 4)) (ash (aref bc (+ pc 5)) 8)
                    (ash (aref bc (+ pc 6)) 16) (ash (aref bc (+ pc 7)) 24))))
    (values (logior lo (ash hi 32)) (+ pc 8))))

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

(defun mvm-interpret (bytecode &key (entry-point 0) function-table)
  "Execute MVM bytecode starting at ENTRY-POINT.
   Returns the value in VR when RET or HALT is reached."
  (let* ((state (make-mvm-state))
         (bc bytecode) (pc entry-point) (len (length bc))
         (ftab (or function-table (vector)))
         (regs (mvm-regs state)))
    (declare (type fixnum pc len) (type simple-vector regs) (ignorable ftab))
    (setf (svref regs +vreg-vn+) nil)
    (setf (svref regs +vreg-vpc+) pc)

    (loop
      (when (or (mvm-halted state) (>= pc len))
        (return (svref regs +vreg-vr+)))
      (let ((opcode (aref bc pc)))
        (setf pc (1+ pc))
        (case opcode

          ;; --- NOP / BREAK / TRAP ---
          (#.+op-nop+ nil)
          (#.+op-break+ (break "MVM BREAK at PC ~D" (1- pc)))
          (#.+op-trap+
           ;; TRAP code:u16 — function prologue sentinel emitted by the compiler.
           ;; The low 8 bits encode param-count; the high 8 bits encode local-count.
           ;; Allocate a frame object in VFP with enough slots for params + locals.
           (multiple-value-bind (code npc) (fetch-u16 bc pc)
             (cond
               ((= code #x0310)
                ;; RDTSC: Read timestamp counter — return fake value in interpreter
                (setf (svref regs +vreg-vr+) 0)
                (setf pc npc))
               (t
                (let* ((params (logand code #xFF))
                       (locals (ash code -8))
                       (frame-size (+ params locals 4)))   ; extra slots for safety
                  (setf (svref regs +vreg-vfp+)
                        (make-array frame-size :initial-element 0)))
                (setf pc npc)))))

          ;; --- Data Movement ---
          (#.+op-mov+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (setf (svref regs vd) (svref regs vs)) (setf pc npc2))))

          (#.+op-li+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (imm npc2) (fetch-u64 bc npc)
               (setf (svref regs vd) imm) (setf pc npc2))))

          (#.+op-push+
           (multiple-value-bind (vs npc) (fetch-reg bc pc)
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
                 (setf (svref regs vd) (+ (svref regs va) (svref regs vb)))
                 (setf pc npc3)))))

          (#.+op-sub+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (setf (svref regs vd) (- (svref regs va) (svref regs vb)))
                 (setf pc npc3)))))

          (#.+op-mul+ ; untag, multiply, re-tag
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (setf (svref regs vd)
                       (tag-fixnum (* (untag-fixnum (svref regs va))
                                      (untag-fixnum (svref regs vb)))))
                 (setf pc npc3)))))

          (#.+op-mul26lo+
           ;; low 26 bits of untag(a)*untag(b), tagged
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (let ((product (* (untag-fixnum (svref regs va))
                                   (untag-fixnum (svref regs vb)))))
                   (setf (svref regs vd) (tag-fixnum (logand product #x3FFFFFF))))
                 (setf pc npc3)))))

          (#.+op-mul26hi+
           ;; bits 26+ of untag(a)*untag(b), tagged
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (let ((product (* (untag-fixnum (svref regs va))
                                   (untag-fixnum (svref regs vb)))))
                   (setf (svref regs vd) (tag-fixnum (ash product -26))))
                 (setf pc npc3)))))

          (#.+op-mul64lo+
           ;; low 64 bits of raw Va*Vb (no tag/untag)
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (setf (svref regs vd)
                       (logand (* (svref regs va) (svref regs vb))
                               #xFFFFFFFFFFFFFFFF))
                 (setf pc npc3)))))

          (#.+op-mul64hi+
           ;; high 64 bits of raw Va*Vb (no tag/untag)
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (setf (svref regs vd)
                       (ash (* (svref regs va) (svref regs vb)) -64))
                 (setf pc npc3)))))

          (#.+op-acc128+
           ;; mem128[Vaddr] += Vhi:Vlo (raw 128-bit accumulate)
           (multiple-value-bind (vaddr npc) (fetch-reg bc pc)
             (multiple-value-bind (vlo npc2) (fetch-reg bc npc)
               (multiple-value-bind (vhi npc3) (fetch-reg bc npc2)
                 (let* ((addr (svref regs vaddr))
                        (cur-lo (mem-read state addr 3))
                        (cur-hi (mem-read state (+ addr 8) 3))
                        (sum-lo (+ cur-lo (svref regs vlo)))
                        (carry (if (> sum-lo #xFFFFFFFFFFFFFFFF) 1 0))
                        (new-lo (logand sum-lo #xFFFFFFFFFFFFFFFF))
                        (new-hi (logand (+ cur-hi (svref regs vhi) carry)
                                        #xFFFFFFFFFFFFFFFF)))
                   (mem-write state addr new-lo 3)
                   (mem-write state (+ addr 8) new-hi 3))
                 (setf pc npc3)))))

          (#.+op-div+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (let ((b (untag-fixnum (svref regs vb))))
                   (when (zerop b) (error "MVM: division by zero at PC ~D" (1- pc)))
                   (setf (svref regs vd)
                         (tag-fixnum (truncate (untag-fixnum (svref regs va)) b))))
                 (setf pc npc3)))))

          (#.+op-mod+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (let ((b (untag-fixnum (svref regs vb))))
                   (when (zerop b) (error "MVM: modulus by zero at PC ~D" (1- pc)))
                   (setf (svref regs vd)
                         (tag-fixnum (mod (untag-fixnum (svref regs va)) b))))
                 (setf pc npc3)))))

          (#.+op-neg+ ; -(a<<1) = (-a)<<1
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (setf (svref regs vd) (- (svref regs vs))) (setf pc npc2))))

          (#.+op-inc+ ; tagged +1 = raw +2
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (incf (svref regs vd) 2) (setf pc npc)))

          (#.+op-dec+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (decf (svref regs vd) 2) (setf pc npc)))

          ;; --- Bitwise ---
          (#.+op-and+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (setf (svref regs vd) (logand (svref regs va) (svref regs vb)))
                 (setf pc npc3)))))

          (#.+op-or+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (setf (svref regs vd) (logior (svref regs va) (svref regs vb)))
                 (setf pc npc3)))))

          (#.+op-xor+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (setf (svref regs vd) (logxor (svref regs va) (svref regs vb)))
                 (setf pc npc3)))))

          (#.+op-shl+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (multiple-value-bind (amt npc3) (fetch-byte bc npc2)
                 (setf (svref regs vd)
                       (tag-fixnum (ash (untag-fixnum (svref regs vs)) amt)))
                 (setf pc npc3)))))

          (#.+op-shr+ ; logical shift right
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (multiple-value-bind (amt npc3) (fetch-byte bc npc2)
                 (let* ((u (untag-fixnum (svref regs vs)))
                        (shifted (if (>= u 0) (ash u (- amt))
                                     (ash (logand u #xFFFFFFFFFFFFFFFF) (- amt)))))
                   (setf (svref regs vd) (tag-fixnum shifted)))
                 (setf pc npc3)))))

          (#.+op-sar+ ; arithmetic shift right
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (multiple-value-bind (amt npc3) (fetch-byte bc npc2)
                 (setf (svref regs vd)
                       (tag-fixnum (ash (untag-fixnum (svref regs vs)) (- amt))))
                 (setf pc npc3)))))

          (#.+op-shlv+ ; shift left by register
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (multiple-value-bind (vc npc3) (fetch-reg bc npc2)
                 (setf (svref regs vd)
                       (tag-fixnum (ash (untag-fixnum (svref regs vs))
                                       (untag-fixnum (svref regs vc)))))
                 (setf pc npc3)))))

          (#.+op-sarv+ ; arithmetic shift right by register
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (multiple-value-bind (vc npc3) (fetch-reg bc npc2)
                 (setf (svref regs vd)
                       (tag-fixnum (ash (untag-fixnum (svref regs vs))
                                       (- (untag-fixnum (svref regs vc))))))
                 (setf pc npc3)))))

          (#.+op-ldb+ ; bit field extract
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (multiple-value-bind (pos npc3) (fetch-byte bc npc2)
                 (multiple-value-bind (size npc4) (fetch-byte bc npc3)
                   (setf (svref regs vd)
                         (tag-fixnum (ldb (byte size pos)
                                         (untag-fixnum (svref regs vs)))))
                   (setf pc npc4))))))

          ;; --- Comparison ---
          (#.+op-cmp+
           (multiple-value-bind (va npc) (fetch-reg bc pc)
             (multiple-value-bind (vb npc2) (fetch-reg bc npc)
               (let ((a (svref regs va)) (b (svref regs vb)))
                 (setf (mvm-flags state)
                       (cond ((and (integerp a) (integerp b))
                              (cond ((= a b) :eq) ((< a b) :lt) (t :gt)))
                             ((eql a b) :eq)
                             (t :gt))))
               (setf pc npc2))))

          (#.+op-test+
           (multiple-value-bind (va npc) (fetch-reg bc pc)
             (multiple-value-bind (vb npc2) (fetch-reg bc npc)
               (let ((r (if (and (integerp (svref regs va)) (integerp (svref regs vb)))
                            (logand (svref regs va) (svref regs vb)) 0)))
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
               (setf pc (if (mvm-nil-p (svref regs vs)) (+ npc2 off) npc2)))))

          (#.+op-bnnull+
           (multiple-value-bind (vs npc) (fetch-reg bc pc)
             (multiple-value-bind (off npc2) (fetch-s32 bc npc)
               (setf pc (if (not (mvm-nil-p (svref regs vs))) (+ npc2 off) npc2)))))

          ;; --- List Operations ---
          (#.+op-car+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (let ((v (svref regs vs)))
                 (setf (svref regs vd)
                       (cond ((consp v) (car v))
                             ((mvm-nil-p v) nil)
                             (t (error 'mvm-type-error :operation "CAR"
                                       :expected "cons or nil" :got v)))))
               (setf pc npc2))))

          (#.+op-cdr+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (let ((v (svref regs vs)))
                 (setf (svref regs vd)
                       (cond ((consp v) (cdr v))
                             ((mvm-nil-p v) nil)
                             (t (error 'mvm-type-error :operation "CDR"
                                       :expected "cons or nil" :got v)))))
               (setf pc npc2))))

          (#.+op-cons+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (va npc2) (fetch-reg bc npc)
               (multiple-value-bind (vb npc3) (fetch-reg bc npc2)
                 (let ((cell (cons (svref regs va) (svref regs vb))))
                   (push cell (mvm-heap state))
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
               (setf (svref regs vd) (mvm-boolean (consp (svref regs vs))))
               (setf pc npc2))))

          (#.+op-atom+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (setf (svref regs vd) (mvm-boolean (atom (svref regs vs))))
               (setf pc npc2))))

          ;; --- Object Operations ---
          (#.+op-alloc-obj+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (size npc2) (fetch-u16 bc npc)
               (multiple-value-bind (subtag npc3) (fetch-byte bc npc2)
                 (let ((obj (make-mvm-object size subtag)))
                   (push obj (mvm-heap state))
                   (setf (svref regs vd) obj))
                 (setf pc npc3)))))

          (#.+op-obj-ref+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vobj npc2) (fetch-reg bc npc)
               (multiple-value-bind (idx npc3) (fetch-byte bc npc2)
                 (let ((obj (svref regs vobj)))
                   (unless (vectorp obj)
                     (error 'mvm-type-error :operation "OBJ-REF" :expected "object" :got obj))
                   (setf (svref regs vd) (svref obj (1+ idx))))
                 (setf pc npc3)))))

          (#.+op-obj-set+
           (multiple-value-bind (vobj npc) (fetch-reg bc pc)
             (multiple-value-bind (idx npc2) (fetch-byte bc npc)
               (multiple-value-bind (vs npc3) (fetch-reg bc npc2)
                 (let ((obj (svref regs vobj)))
                   (unless (vectorp obj)
                     (error 'mvm-type-error :operation "OBJ-SET" :expected "object" :got obj))
                   (setf (svref obj (1+ idx)) (svref regs vs)))
                 (setf pc npc3)))))

          (#.+op-obj-tag+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (let ((obj (svref regs vs)))
                 (setf (svref regs vd)
                       (tag-fixnum (cond ((consp obj) +tag-cons+)
                                         ((vectorp obj) (mvm-obj-tag-val obj))
                                         ((integerp obj) +tag-fixnum+)
                                         (t 0)))))
               (setf pc npc2))))

          (#.+op-obj-subtag+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (let ((obj (svref regs vs)))
                 (setf (svref regs vd)
                       (tag-fixnum (if (vectorp obj) (mvm-obj-subtag-val obj) 0))))
               (setf pc npc2))))

          ;; --- Raw Memory ---
          (#.+op-load+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vaddr npc2) (fetch-reg bc npc)
               (multiple-value-bind (width npc3) (fetch-byte bc npc2)
                 (let ((addr (svref regs vaddr)))
                   (when (integerp addr) (setf addr (untag-fixnum addr)))
                   (setf (svref regs vd) (mem-read state addr width)))
                 (setf pc npc3)))))

          (#.+op-store+
           (multiple-value-bind (vaddr npc) (fetch-reg bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (multiple-value-bind (width npc3) (fetch-byte bc npc2)
                 (let ((addr (svref regs vaddr)) (val (svref regs vs)))
                   (when (integerp addr) (setf addr (untag-fixnum addr)))
                   (when (integerp val) (setf val (untag-fixnum val)))
                   (mem-write state addr val width))
                 (setf pc npc3)))))

          (#.+op-fence+ nil) ; memory barrier: no-op

          ;; --- Function Calling ---
          (#.+op-call+
           (multiple-value-bind (target npc) (fetch-u32 bc pc)
             (push (cons npc (mvm-stack state)) (mvm-call-stack state))
             (setf pc (if (< target (length ftab)) (aref ftab target) target))))

          (#.+op-call-ind+
           (multiple-value-bind (vs npc) (fetch-reg bc pc)
             (let ((target (svref regs vs)))
               (push (cons npc (mvm-stack state)) (mvm-call-stack state))
               (if (integerp target)
                   (setf pc (untag-fixnum target))
                   (error "MVM: CALL-IND with non-integer target ~S" target)))))

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
               (setf (svref regs vd) cell))
             (setf pc npc)))

          (#.+op-gc-check+ nil) ; no-op in interpreter
          (#.+op-mcgc-collect+ nil) ; no-op in interpreter (no page pool)

          (#.+op-write-barrier+
           (multiple-value-bind (_vobj npc) (fetch-reg bc pc)
             (declare (ignore _vobj))
             (setf pc npc)))

          ;; --- Actor / Concurrency ---
          (#.+op-save-ctx+
           (push (copy-seq regs) (mvm-stack state)))

          (#.+op-restore-ctx+
           (let ((saved (pop (mvm-stack state))))
             (when (and saved (typep saved 'simple-vector))
               (replace regs saved)
               (setf pc (svref regs +vreg-vpc+)))))

          (#.+op-yield+ nil) ; preemption: no-op

          (#.+op-atomic-xchg+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (vaddr npc2) (fetch-reg bc npc)
               (multiple-value-bind (vs npc3) (fetch-reg bc npc2)
                 (let* ((addr (svref regs vaddr))
                        (old (gethash addr (mvm-memory state) 0)))
                   (setf (svref regs vd) old)
                   (setf (gethash addr (mvm-memory state)) (svref regs vs)))
                 (setf pc npc3)))))

          ;; --- I/O and System ---
          (#.+op-io-read+
           (multiple-value-bind (vd npc) (fetch-reg bc pc)
             (multiple-value-bind (port npc2) (fetch-u16 bc npc)
               (multiple-value-bind (_w npc3) (fetch-byte bc npc2)
                 (declare (ignore _w))
                 (setf (svref regs vd) (gethash port (mvm-io-ports state) 0))
                 (setf pc npc3)))))

          (#.+op-io-write+
           (multiple-value-bind (port npc) (fetch-u16 bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (multiple-value-bind (_w npc3) (fetch-byte bc npc2)
                 (declare (ignore _w))
                 (let ((val (svref regs vs)))
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
               (setf (svref regs vd) (gethash offset (mvm-percpu state) 0))
               (setf pc npc2))))

          (#.+op-percpu-set+
           (multiple-value-bind (offset npc) (fetch-u16 bc pc)
             (multiple-value-bind (vs npc2) (fetch-reg bc npc)
               (setf (gethash offset (mvm-percpu state)) (svref regs vs))
               (setf pc npc2))))

          (otherwise
           (error "MVM: unknown opcode #x~2,'0X at PC ~D" opcode (1- pc)))))

      (setf (svref regs +vreg-vpc+) pc))))

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
