;;;; translate-x64.lisp - MVM Bytecode to x86-64 Native Code Translator
;;;;
;;;; Translates MVM (Modus Virtual Machine) bytecode into x86-64 machine
;;;; code using the x64-asm.lisp instruction encoder.  Each MVM virtual
;;;; instruction maps to 1-5 native x86-64 instructions.
;;;;
;;;; Virtual registers are resolved to physical x86-64 registers according
;;;; to the mapping in target.lisp.  Registers V9-V15 that have no physical
;;;; home are spilled to the stack frame at [RBP - offset].
;;;;
;;;; Branch targets in MVM bytecode are 16-bit signed offsets from the end
;;;; of the branch instruction.  During translation we build a map from MVM
;;;; bytecode positions to native code positions and use label-based fixups
;;;; provided by the assembler for forward references.

(in-package :cl-user)

(defpackage :modus.mvm.x64
  (:use :cl :modus.mvm :modus.asm)
  (:export
   #:translate-mvm-to-x64
   #:translate-function
   #:install-x64-translator))

(in-package :modus.mvm.x64)

(defvar *x64-linux-mode* nil
  "When non-nil, TRAP codes emit Linux syscalls instead of bare-metal I/O.
   Set by Linux x64 builds to use SYS_write/SYS_read/SYS_exit instead of
   port I/O, PIC/PIT setup, etc.")

(defvar *x64-li-const-patches* nil
  "List of (native-byte-offset . pool-index) pairs collected during
   translation.  Each entry says: at NATIVE-BYTE-OFFSET in the native
   code buffer, an 8-byte placeholder immediate needs to be patched
   with the tagged address of constant-pool[POOL-INDEX].
   Bound freshly to nil at the start of TRANSLATE-MVM-TO-X64; read
   by ASSEMBLE-KERNEL-IMAGE after translation completes.")

;;; ============================================================
;;; Physical Register Mapping
;;; ============================================================
;;;
;;; Maps each MVM virtual register number to an x86-64 physical register
;;; symbol recognised by x64-asm.lisp, or NIL for spilled registers.
;;;
;;;   V0  → RSI   V1  → RDI   V2  → R8    V3  → R9     (args)
;;;   V4  → RBX   V5  → RCX   V6  → RDX   V7  → R10    (general)
;;;   V8  → R11   V9..V15 → spill
;;;   VR  → RAX   VA  → R12   VL  → R14   VN  → R15
;;;   VSP → RSP   VFP → RBP

(defparameter *vreg-to-x64*
  (vector 'rsi  ; V0   0
          'rdi  ; V1   1
          'r8   ; V2   2
          'r9   ; V3   3
          'rbx  ; V4   4
          'rcx  ; V5   5
          'rdx  ; V6   6
          'r10  ; V7   7
          'r11  ; V8   8
          nil   ; V9   9   spill
          nil   ; V10  10  spill
          nil   ; V11  11  spill
          nil   ; V12  12  spill
          nil   ; V13  13  spill
          nil   ; V14  14  spill
          nil   ; V15  15  spill
          'rax  ; VR   16
          'r12  ; VA   17
          'r14  ; VL   18
          'r15  ; VN   19
          'rsp  ; VSP  20
          'rbp  ; VFP  21
          nil)) ; VPC  22  (not mapped)

(defconstant +max-inline-vreg+ 8
  "Virtual registers above this index spill to the stack frame.")

(defconstant +spill-slot-size+ 8
  "Each spill slot is 8 bytes (one 64-bit word).")

(defun vreg-phys (vreg)
  "Return the physical register symbol for VREG, or NIL if spilled."
  (when (< vreg (length *vreg-to-x64*))
    (aref *vreg-to-x64* vreg)))

(defun vreg-spills-p (vreg)
  "Does VREG spill to the stack on x86-64?"
  (and (>= vreg 9) (<= vreg 15)))

;;; Callee-saved register save area (top of frame, just below RBP):
;;;   RBP-8:  saved RBX
;;;   RBP-16: saved R12
;;;   RBP-24: saved R14
;;;   RBP-32: saved R15
(defconstant +callee-save-size+ 32
  "Bytes reserved for callee-saved registers (RBX, R12, R14, R15).")

(defun spill-offset (vreg)
  "Return the frame offset (negative from RBP) for a spilled register.
   V9 → [RBP-40], V10 → [RBP-48], ..., V15 → [RBP-88].
   Shifted down by 32 bytes to make room for callee-saved register saves."
  (- (+ (* (- vreg +max-inline-vreg+) +spill-slot-size+)
        +callee-save-size+)))

(defconstant +n-spill-slots+ 7
  "Number of spill slots needed (V9..V15).")

(defconstant +spill-frame-size+ (* +n-spill-slots+ +spill-slot-size+)
  "Total bytes reserved in the stack frame for spill slots.")

(defconstant +frame-slot-base+ -96
  "RBP-relative offset for frame slot 0 (local variables via obj-ref VFP).
   Frame slots grow downward: slot N is at RBP + frame-slot-base - N*8.
   Above this: callee-saved saves (32 bytes) + spill slots (56 bytes) = 88.")

(defconstant +frame-total-size+ 1120
  "Total frame reservation in bytes. Callee-saved saves (32) + spill slots (56)
   + 128 frame slots for local variables (1024) = 1112, rounded to 1120.
   Note: fe-mul (crypto.lisp) has ~74 nested let/let* bindings but works because
   many are within inner lambdas (each gets their own frame).")

;;; ============================================================
;;; Scratch Register for Spill Mediation
;;; ============================================================
;;;
;;; When a source or destination operand is a spilled register we load
;;; from / store to the spill slot via RAX as a scratch register.
;;; Care must be taken when RAX is also the VR mapping; in translate-
;;; instruction the ordering ensures correctness.

(defconstant +scratch-reg+ 'rax
  "Temporary register used when mediating spilled virtual registers.")

;;; ============================================================
;;; Register Materialisation Helpers
;;; ============================================================

(defun emit-load-vreg (buf vreg phys-dest)
  "Load virtual register VREG into physical register PHYS-DEST.
   If VREG maps to a physical register, emit MOV if different.
   If VREG is spilled, load from the frame."
  (let ((phys (vreg-phys vreg)))
    (cond
      (phys
       (unless (eq phys phys-dest)
         (emit-mov-reg-reg buf phys-dest phys)))
      ((vreg-spills-p vreg)
       (emit-mov-reg-mem buf phys-dest 'rbp (spill-offset vreg)))
      (t
       (error "MVM x64: cannot load vreg ~D" vreg)))))

(defun emit-store-vreg (buf vreg phys-src)
  "Store physical register PHYS-SRC into virtual register VREG.
   If VREG maps to a physical register, emit MOV if different.
   If VREG is spilled, store to the frame."
  (let ((phys (vreg-phys vreg)))
    (cond
      (phys
       (unless (eq phys phys-src)
         (emit-mov-reg-reg buf phys phys-src)))
      ((vreg-spills-p vreg)
       (emit-mov-mem-reg buf 'rbp phys-src (spill-offset vreg)))
      (t
       (error "MVM x64: cannot store vreg ~D" vreg)))))

(defun emit-vreg-to-vreg (buf dst src)
  "Move value from virtual register SRC to virtual register DST.
   Handles all combinations of physical and spilled registers."
  (let ((phys-dst (vreg-phys dst))
        (phys-src (vreg-phys src)))
    (cond
      ;; Both physical
      ((and phys-dst phys-src)
       (unless (eq phys-dst phys-src)
         (emit-mov-reg-reg buf phys-dst phys-src)))
      ;; Src physical, dst spilled
      ((and (null phys-dst) phys-src)
       (emit-store-vreg buf dst phys-src))
      ;; Src spilled, dst physical
      ((and phys-dst (null phys-src))
       (emit-load-vreg buf src phys-dst))
      ;; Both spilled — route through scratch
      (t
       (emit-load-vreg buf src +scratch-reg+)
       (emit-store-vreg buf dst +scratch-reg+)))))

;;; ============================================================
;;; Destination Register Resolution
;;; ============================================================
;;;
;;; Many MVM instructions write a result.  If the destination virtual
;;; register has a physical mapping we compute directly into it.
;;; Otherwise we compute into +scratch-reg+ and store afterwards.

(defun dest-phys-or-scratch (vreg)
  "Return the physical register to compute the result into for VREG.
   If VREG has a physical register, return it; else return +scratch-reg+."
  (or (vreg-phys vreg) +scratch-reg+))

(defun maybe-store-scratch (buf vreg)
  "If VREG is spilled, store +scratch-reg+ into its spill slot."
  (when (vreg-spills-p vreg)
    (emit-store-vreg buf vreg +scratch-reg+)))

(defun emit-zero-word-range (buf ptr-reg end-reg zero-reg)
  "Emit a runtime loop that stores ZERO-REG into every 8-byte word in
   [PTR-REG, END-REG).  PTR-REG is advanced by 8 each iteration and is
   left == END-REG on exit; ZERO-REG and END-REG are read-only.

   This zero-initialises the payload of dynamically-sized allocations
   (+op-alloc-array+ / +op-alloc-string+) so the Cheney GC's FLAT
   word-by-word to-space scan (emit-gc-trampoline) never sees an
   uninitialised payload word as a spurious root.  See the long comment
   in the +op-alloc-obj+ handler for the full mechanism — the same
   uninit-payload-scanned-as-pointer corruption applies to arrays and
   strings whose elements are written by the caller AFTER allocation
   (a window during which another alloc can trigger GC).

   Unlike alloc-obj's unrolled MOVs, this is a single fixed-size loop
   regardless of element count, so zeroing the full aligned region
   (including any 16-byte alignment tail) does NOT change code layout —
   the alloc-obj alignment-tail layout hazard does not apply here."
  (let ((loop-label (make-label))
        (done-label (make-label)))
    (emit-label buf loop-label)
    (emit-cmp-reg-reg buf ptr-reg end-reg)
    (emit-jcc buf :ae done-label)             ; ptr >= end (unsigned) → stop
    (emit-mov-mem-reg buf ptr-reg zero-reg 0) ; [ptr] = 0
    (emit-add-reg-imm buf ptr-reg 8)          ; ptr += 8
    (emit-jmp buf loop-label)
    (emit-label buf done-label)))

;;; ============================================================
;;; Mostly-Copying GC — object-start bitmap maintenance
;;; ============================================================
;;; Config-word BSS slots (must match boot-linux-x64.lisp +mcgc-cfg-*+):
;;;   0x10000E00 page_base   0x10000E18 bitmap_base
(defconstant +mcgc-cfg-page-base-addr+ #x10000E00)
(defconstant +mcgc-cfg-bitmap-addr+    #x10000E18)

(defconstant +mcgc-kindbitmap-delta+   #x804000
  "Byte offset from the object-start bitmap base to the CONS-KIND bitmap
   base.  Both bitmaps are +mcgc-bitmap-size+ bytes (1 bit / 16-byte
   granule); the kind bitmap lives in the metadata region just past the
   run-free-list.  Its base = [+mcgc-cfg-bitmap-addr+] + this delta, so no
   extra boot config word (and hence no boot-preamble growth / no
   *x64-native-code-offset* bump) is needed.  The exact value is ASSERTED
   against the real metadata layout in boot-linux-x64.lisp; if the heap
   sizing ever changes the build fails loudly there.

   KIND BIT semantics: SET = the start at this granule is a CONS; CLEAR =
   it is an OBJECT (or not a start — gated by the object-start bitmap).
   Only cons allocations / cons survivor-copies set it; object starts leave
   it 0.  scan_word cross-checks a candidate's TAG against this bit so a
   conservative scratch word that aliases a live object's BASE with the
   WRONG tag (e.g. symbol_base|1) can no longer be copied as the wrong type
   — the bug that truncated a 40-byte symbol into a 16-byte cons.")

(defun emit-mcgc-set-start-bit (buf addr-reg)
  "Set the MCGC object-start bitmap bit for the object whose RAW start
   address is in ADDR-REG (a tagged-pointer's untagged base — i.e. the
   value of R12 at the moment the header was written).  No-op unless
   *mcgc-bitmap-enabled*.

   Bitmap = 1 bit / 16-byte granule.  granule = (addr - page_base) >> 4.
   Set bit GRANULE counting from bitmap_base using BTS [base], idx — the
   x86 BTS with a register bit-index and memory base addresses bits
   beyond the first qword, so no manual byte/bit split is needed.

   Preserves ALL registers (saves/restores RAX, RCX, RDX); ADDR-REG is
   read-only and must NOT be one of RAX/RCX/RDX (callers pass R12 or a
   callee-saved reg, or copy first)."
  (when (mcgc-bitmap-on-p)
    (when (member addr-reg '(rax rcx rdx))
      (error "emit-mcgc-set-start-bit: ADDR-REG must not be RAX/RCX/RDX"))
    (emit-push buf 'rax)
    (emit-push buf 'rcx)
    (emit-push buf 'rdx)
    ;; RAX = addr - page_base
    (emit-mov-reg-reg buf 'rax addr-reg)
    (emit-bytes buf #x48 #x2B #x04 #x25)          ; sub rax, [abs32] (page_base)
    (emit-u32 buf +mcgc-cfg-page-base-addr+)
    (emit-shr-reg-imm buf 'rax 4)                 ; rax = granule index
    ;; RCX = bitmap_base
    (emit-bytes buf #x48 #x8B #x0C #x25)          ; mov rcx, [abs32] (bitmap_base)
    (emit-u32 buf +mcgc-cfg-bitmap-addr+)
    ;; BTS [rcx], rax  — set bit number RAX counting from [rcx]
    (emit-bytes buf #x48 #x0F #xAB #x01)          ; bts [rcx], rax
    (emit-pop buf 'rdx)
    (emit-pop buf 'rcx)
    (emit-pop buf 'rax)))

;;; ============================================================
;;; Translation State
;;; ============================================================

(defstruct translate-state
  (buf nil)                    ; code-buffer from x64-asm
  (mvm-bytes nil)              ; raw MVM bytecode vector
  (mvm-length 0)               ; length of bytecode region
  (mvm-offset 0)               ; start offset into mvm-bytes
  ;; Maps from MVM bytecode position → native code label.
  ;; Populated on the first pass (scan) or lazily on demand.
  (position-labels (make-hash-table :test 'eql))
  ;; Function table: function-index → native code label
  (function-table nil)
  ;; GC helper label (one per translated unit)
  (gc-label nil)
  ;; Handler-stack helper labels (one per translated unit). The push helper
  ;; saves the current [#x10000180/+8/+16] state to a memory stack so nested
  ;; handler-cases don't clobber their parent's setjmp frame; the pop helper
  ;; restores. Without these, every per-test handler-case in ANSI fork
  ;; wrappers would reset [180]=0 on completion, killing signal recovery
  ;; for subsequent tests in the same fork.
  (handler-push-label nil)
  (handler-pop-label nil)
  ;; BARE-METAL safepoint-deadline stub (nil on Linux): YIELD sites call it
  ;; when the PIT ISR has set the deadline-pending flag [0x10000D30].  The
  ;; stub clears the flag and performs the standard longjmp through the
  ;; innermost armed handler-case — at a SAFE POINT (loop back-edge), never
  ;; mid-intern/mid-alloc/mid-GC the way the old ISR-side longjmp did.
  (yield-longjmp-label nil))

(defun ensure-label-at (state mvm-pos)
  "Ensure a label exists for MVM bytecode position MVM-POS.
   Returns the label."
  (let ((ht (translate-state-position-labels state)))
    (or (gethash mvm-pos ht)
        (setf (gethash mvm-pos ht) (make-label)))))

;;; ============================================================
;;; Two-Operand ALU Pattern
;;; ============================================================
;;;
;;; Many MVM instructions are three-address: (op Vd, Va, Vb).
;;; On x86-64 most ALU ops are two-address: dst = dst OP src.
;;; The pattern is:
;;;   1. Load Va into dest register (or compute into scratch)
;;;   2. Apply ALU op with Vb as source
;;;   3. Store result if dest was scratch

(defun emit-alu-rrr (buf emitter vd va vb)
  "Emit a two-operand ALU pattern for (op Vd, Va, Vb).
   EMITTER is (lambda (buf dst-phys src-phys)) that emits the ALU op."
  (let* ((d (dest-phys-or-scratch vd))
         (pa (vreg-phys va))
         (pb (vreg-phys vb)))
    ;; Step 1: get Va into d
    (cond
      (pa (unless (eq pa d) (emit-mov-reg-reg buf d pa)))
      (t  (emit-load-vreg buf va d)))
    ;; Step 2: apply op.  Need Vb in a physical register.
    (cond
      (pb (funcall emitter buf d pb))
      (t
       ;; Vb is spilled.  Load into a temp.  We need a temp that is not
       ;; the same as d.  Use R13 (currently unused by MVM mapping) as
       ;; a second scratch when d is RAX, otherwise use RAX.
       (let ((tmp (if (eq d 'rax) 'r13 'rax)))
         (emit-push buf tmp)               ; save tmp
         (emit-load-vreg buf vb tmp)
         (funcall emitter buf d tmp)
         (emit-pop buf tmp))))             ; restore tmp
    ;; Step 3: store if spilled
    (maybe-store-scratch buf vd)))

;;; ============================================================
;;; Instruction Translation
;;; ============================================================

(defun translate-instruction (state opcode operands mvm-next-pos)
  "Translate a single MVM instruction into x86-64 code.
   OPCODE is the numeric MVM opcode.
   OPERANDS is the list of decoded operands.
   MVM-NEXT-POS is the bytecode position after this instruction.
   Returns no useful value; side-effects the code-buffer in STATE."
  (let ((buf (translate-state-buf state)))
    (macrolet ((op= (sym) `(= opcode ,sym)))
      (cond
        ;; ============================================
        ;; NOP / BREAK / TRAP
        ;; ============================================
        ((op= +op-nop+)
         (emit-nop buf))

        ((op= +op-break+)
         (emit-int buf 3))            ; INT 3 — debug breakpoint

        ((op= +op-trap+)
         (let ((code (first operands)))
           (cond
             ((< code #x100)
              ;; Frame-enter: code = param count.
              ;; Prologue is emitted at function boundaries.
              ;; If > 4 params, copy overflow args from caller's stack
              ;; to local frame slots so stack-load can find them.
              (when (> code 4)
                (loop for i from 4 below code
                      for src-offset = (+ 16 (* (- i 4) 8))  ; [RBP + 16 + k*8]
                      for dst-offset = (+ +frame-slot-base+ (* i -8))  ; frame slot i
                      do (emit-mov-reg-mem buf 'rax 'rbp src-offset)
                         (emit-mov-mem-reg buf 'rbp 'rax dst-offset))))
             ((< code #x0300)
              ;; Frame-alloc and frame-free: NOP
              ;; Frame slots are pre-allocated in the 352-byte frame.
              nil)
             ((= code #x0300)
              ;; Serial write: V0 (RSI) contains tagged fixnum char code
              (if *x64-linux-mode*
                  (progn
                    ;; Linux: SYS_write(1, &byte, 1)
                    ;; Save regs clobbered by syscall (RCX, R11, plus our args RDI, RSI, RDX)
                    (emit-bytes buf #x56)                  ; push rsi (save V0)
                    (emit-bytes buf #x57)                  ; push rdi
                    (emit-bytes buf #x52)                  ; push rdx
                    ;; Untag char and store on stack
                    (emit-bytes buf #x48 #x8B #x44 #x24 #x10) ; mov rax, [rsp+16] (saved RSI)
                    (emit-bytes buf #x48 #xD1 #xF8)       ; sar rax, 1 (untag)
                    (emit-bytes buf #x88 #x44 #x24 #x10)  ; mov [rsp+16], al (reuse saved RSI slot)
                    ;; SYS_write(fd=1, buf=&byte, len=1)
                    (emit-bytes buf #x48 #xC7 #xC7 #x01 #x00 #x00 #x00) ; mov rdi, 1 (stdout)
                    (emit-bytes buf #x48 #x8D #x74 #x24 #x10) ; lea rsi, [rsp+16]
                    (emit-bytes buf #x48 #xC7 #xC2 #x01 #x00 #x00 #x00) ; mov rdx, 1
                    (emit-bytes buf #x48 #xC7 #xC0 #x01 #x00 #x00 #x00) ; mov rax, 1 (SYS_write)
                    (emit-bytes buf #x0F #x05)             ; syscall
                    ;; Restore regs
                    (emit-bytes buf #x5A)                  ; pop rdx
                    (emit-bytes buf #x5F)                  ; pop rdi
                    (emit-bytes buf #x5E))                 ; pop rsi
                  (progn
                    ;; Bare metal: OUT to COM1
                    (emit-bytes buf #x89 #xF0)            ; mov eax, esi
                    (emit-bytes buf #xD1 #xF8)            ; sar eax, 1 (untag)
                    (emit-bytes buf #x66 #xBA #xF8 #x03)  ; mov dx, 0x3F8
                    (emit-bytes buf #xEE))))               ; out dx, al
             ((= code #x0301)
              (if *x64-linux-mode*
                  (progn
                    ;; Linux: SYS_read(0, &byte, 1)
                    (emit-bytes buf #x57)                  ; push rdi
                    (emit-bytes buf #x52)                  ; push rdx
                    (emit-bytes buf #x48 #xC7 #xC7 #x00 #x00 #x00 #x00) ; mov rdi, 0 (stdin)
                    (emit-bytes buf #x48 #x8D #x74 #x24 #x10) ; lea rsi, [rsp+16] (temp on stack)
                    (emit-bytes buf #x48 #xC7 #xC2 #x01 #x00 #x00 #x00) ; mov rdx, 1
                    (emit-bytes buf #x48 #xC7 #xC0 #x00 #x00 #x00 #x00) ; mov rax, 0 (SYS_read)
                    (emit-bytes buf #x0F #x05)             ; syscall
                    (emit-bytes buf #x5A)                  ; pop rdx
                    (emit-bytes buf #x5F)                  ; pop rdi
                    ;; Result byte at [rsp-16] (where we read into, adjusted for pops)
                    (emit-bytes buf #x0F #xB6 #x74 #x24 #xF0) ; movzx esi, byte [rsp-16]
                    (emit-bytes buf #xD1 #xE6))            ; shl esi, 1 (tag)
                  (progn
                    ;; Bare metal: poll COM1 LSR then IN from COM1
                    (emit-bytes buf #x66 #xBA #xFD #x03)  ; mov dx, 0x3FD
                    (emit-bytes buf #xEC)                   ; in al, dx
                    (emit-bytes buf #xA8 #x01)              ; test al, 1
                    (emit-bytes buf #x74 #xF7)              ; jz -9
                    (emit-bytes buf #x66 #xBA #xF8 #x03)  ; mov dx, 0x3F8
                    (emit-bytes buf #xEC)                   ; in al, dx
                    (emit-bytes buf #x0F #xB6 #xF0)       ; movzx esi, al
                    (emit-bytes buf #xD1 #xE6))))
             ((= code #x0302)
              ;; Memory barrier: mfence
              (emit-bytes buf #x0F #xAE #xF0))
             ((= code #x0303)
              ;; STI+HLT: sleep until interrupt
              (if *x64-linux-mode*
                  ;; Linux: NOP (can't HLT in userspace)
                  (emit-bytes buf #x90)
                  (progn
                    (emit-bytes buf #xFB)    ; sti
                    (emit-bytes buf #xF4)))) ; hlt
             ((= code #x0304)
              ;; WFI: PAUSE on x86 (hint to yield CPU in spin-wait loops)
              (emit-bytes buf #xF3 #x90))
             ((= code #x0500)
              ;; SYS_exit: V0 (RSI) = tagged exit code
              (emit-bytes buf #x48 #x89 #xF7)  ; mov rdi, rsi (exit code)
              (emit-bytes buf #x48 #xD1 #xFF)   ; sar rdi, 1 (untag)
              (emit-bytes buf #x48 #xC7 #xC0 #x3C #x00 #x00 #x00) ; mov rax, 60 (SYS_exit)
              (emit-bytes buf #x0F #x05))       ; syscall
             ((= code #x0502)
              ;; Generic 3-arg Linux syscall
              ;; V0(RSI)=syscall#, V1(RDI)=arg1, V2(R8)=arg2, V3(R9)=arg3
              ;; All tagged fixnums, untagged before syscall
              ;; Result in V0(RSI), tagged
              (emit-bytes buf #x56)              ; push rsi
              (emit-bytes buf #x57)              ; push rdi
              (emit-bytes buf #x52)              ; push rdx
              (emit-bytes buf #x41 #x50)         ; push r8
              (emit-bytes buf #x41 #x51)         ; push r9
              ;; rax = untag V0 (syscall number)
              (emit-bytes buf #x48 #x89 #xF0)   ; mov rax, rsi
              (emit-bytes buf #x48 #xD1 #xF8)   ; sar rax, 1
              ;; rdi = untag V1 (arg1)
              (emit-bytes buf #x48 #xD1 #xFF)   ; sar rdi, 1
              ;; rsi = untag V2 (arg2)
              (emit-bytes buf #x4C #x89 #xC6)   ; mov rsi, r8
              (emit-bytes buf #x48 #xD1 #xFE)   ; sar rsi, 1
              ;; rdx = untag V3 (arg3)
              (emit-bytes buf #x4C #x89 #xCA)   ; mov rdx, r9
              (emit-bytes buf #x48 #xD1 #xFA)   ; sar rdx, 1
              ;; syscall
              (emit-bytes buf #x0F #x05)         ; syscall
              ;; Tag result → V0 (RSI)
              (emit-bytes buf #x48 #x01 #xC0)   ; add rax, rax
              (emit-bytes buf #x48 #x89 #xC6)   ; mov rsi, rax
              ;; Restore other regs
              (emit-bytes buf #x41 #x59)         ; pop r9
              (emit-bytes buf #x41 #x58)         ; pop r8
              (emit-bytes buf #x5A)              ; pop rdx
              (emit-bytes buf #x5F)              ; pop rdi (restored)
              ;; Don't restore RSI — it has the result
              (emit-bytes buf #x48 #x83 #xC4 #x08)) ; add rsp, 8 (discard saved rsi)
             ((= code #x0503)
              ;; Raw syscall: V0(RSI)=syscall#(TAGGED), V1(RDI)=arg1, V2(R8)=arg2, V3(R9)=arg3
              ;; Syscall number is untagged (SHR 1). Args 1-3 passed as-is (raw).
              ;; Result in V0(RSI), raw (not tagged).
              (emit-bytes buf #x57)              ; push rdi
              (emit-bytes buf #x52)              ; push rdx
              (emit-bytes buf #x41 #x50)         ; push r8
              (emit-bytes buf #x41 #x51)         ; push r9
              ;; rax = untag V0 (syscall number)
              (emit-bytes buf #x48 #x89 #xF0)   ; mov rax, rsi
              (emit-bytes buf #x48 #xD1 #xF8)   ; sar rax, 1
              ;; rdi = V1 (arg1, raw)
              ;; rdi already has V1
              ;; rsi = V2 (arg2, raw)
              (emit-bytes buf #x4C #x89 #xC6)   ; mov rsi, r8
              ;; rdx = V3 (arg3, raw)
              (emit-bytes buf #x4C #x89 #xCA)   ; mov rdx, r9
              ;; syscall
              (emit-bytes buf #x0F #x05)         ; syscall
              ;; Result → V0 (RSI), raw
              (emit-bytes buf #x48 #x89 #xC6)   ; mov rsi, rax
              ;; Restore
              (emit-bytes buf #x41 #x59)         ; pop r9
              (emit-bytes buf #x41 #x58)         ; pop r8
              (emit-bytes buf #x5A)              ; pop rdx
              (emit-bytes buf #x5F))             ; pop rdi
             ((= code #x0504)
              ;; %MMAP-SHARED-PAGE: shared-memory anonymous mmap.
              ;; V0(RSI) = size (tagged fixnum, expected page-multiple).
              ;; Result = mmap address, stored in V0(RSI) tagged.
              ;;
              ;; Equivalent to:
              ;;   mmap(NULL, size, PROT_READ|PROT_WRITE,
              ;;        MAP_SHARED|MAP_ANONYMOUS, -1, 0)
              ;; → 6-arg syscall we'd otherwise need syscall6 support for.
              ;; Hard-coded here so parent-child shared memory works over
              ;; the existing syscall3 trap infrastructure.
              (emit-bytes buf #x57)              ; push rdi
              (emit-bytes buf #x52)              ; push rdx
              (emit-bytes buf #x41 #x50)         ; push r8
              (emit-bytes buf #x41 #x51)         ; push r9
              (emit-bytes buf #x41 #x52)         ; push r10
              (emit-bytes buf #x41 #x53)         ; push r11
              ;; rax = 9 (SYS_mmap)
              (emit-bytes buf #xB8 #x09 #x00 #x00 #x00)
              ;; rdi = 0 (addr = NULL)
              (emit-bytes buf #x48 #x31 #xFF)
              ;; rsi = untag(V0) (size)
              (emit-bytes buf #x48 #xD1 #xFE)   ; sar rsi, 1
              ;; rdx = 3 (prot = PROT_READ|PROT_WRITE)
              (emit-bytes buf #xBA #x03 #x00 #x00 #x00)
              ;; r10 = 33 (flags = MAP_SHARED|MAP_ANONYMOUS = 0x01|0x20)
              (emit-bytes buf #x41 #xBA #x21 #x00 #x00 #x00)
              ;; r8 = -1 (fd)
              (emit-bytes buf #x49 #xC7 #xC0 #xFF #xFF #xFF #xFF)
              ;; r9 = 0 (offset)
              (emit-bytes buf #x4D #x31 #xC9)
              ;; syscall
              (emit-bytes buf #x0F #x05)
              ;; Tag result → V0 (RSI)
              (emit-bytes buf #x48 #x01 #xC0)   ; add rax, rax
              (emit-bytes buf #x48 #x89 #xC6)   ; mov rsi, rax
              ;; Restore
              (emit-bytes buf #x41 #x5B)         ; pop r11
              (emit-bytes buf #x41 #x5A)         ; pop r10
              (emit-bytes buf #x41 #x59)         ; pop r9
              (emit-bytes buf #x41 #x58)         ; pop r8
              (emit-bytes buf #x5A)              ; pop rdx
              (emit-bytes buf #x5F))             ; pop rdi
             ((= code #x0320)
              ;; SETUP-IRQ: PIC remap + PIT timer + IDT + ISR for HLT-based io-delay
              (if *x64-linux-mode*
                  ;; Linux: NOP (no PIC/PIT access in userspace)
                  (emit-bytes buf #x90)
                  (progn
              ;; Clobbers RAX, RCX, RDX, RDI — save/restore around
              (emit-bytes buf #x51)  ; push rcx
              (emit-bytes buf #x52)  ; push rdx
              (emit-bytes buf #x57)  ; push rdi
              ;; --- PIC remap: master IRQ0→0x20, slave IRQ8→0x28 ---
              ;; ICW1: init + ICW4 needed
              (emit-bytes buf #xB0 #x11 #x66 #xBA #x20 #x00 #xEE)  ; mov al,0x11; mov dx,0x20; out dx,al
              (emit-bytes buf #xB0 #x11 #x66 #xBA #xA0 #x00 #xEE)  ; slave ICW1
              ;; ICW2: vector offsets
              (emit-bytes buf #xB0 #x20 #x66 #xBA #x21 #x00 #xEE)  ; master→0x20
              (emit-bytes buf #xB0 #x28 #x66 #xBA #xA1 #x00 #xEE)  ; slave→0x28
              ;; ICW3: wiring
              (emit-bytes buf #xB0 #x04 #x66 #xBA #x21 #x00 #xEE)  ; master: slave on IRQ2
              (emit-bytes buf #xB0 #x02 #x66 #xBA #xA1 #x00 #xEE)  ; slave: cascade
              ;; ICW4: 8086 mode
              (emit-bytes buf #xB0 #x01 #x66 #xBA #x21 #x00 #xEE)
              (emit-bytes buf #xB0 #x01 #x66 #xBA #xA1 #x00 #xEE)
              ;; Mask: unmask only IRQ0 (PIT) on master, mask all on slave
              ;; IRQ2 (cascade) masked too — prevents spurious slave IRQ 0x2F triple fault
              (emit-bytes buf #xB0 #xFE #x66 #xBA #x21 #x00 #xEE)  ; master: 0xFE = ~IRQ0 only
              (emit-bytes buf #xB0 #xFF #x66 #xBA #xA1 #x00 #xEE)  ; slave: 0xFF = all masked
              ;; --- PIT channel 0: ~1000Hz (divisor 1193 = 0x04A9) ---
              (emit-bytes buf #xB0 #x34 #x66 #xBA #x43 #x00 #xEE)  ; mode 2, lobyte/hibyte
              (emit-bytes buf #xB0 #xA9 #x66 #xBA #x40 #x00 #xEE)  ; divisor low
              (emit-bytes buf #xB0 #x04 #x66 #xBA #x40 #x00 #xEE)  ; divisor high
              ;; --- Zero IDT area (768 bytes = 96 qwords at 0x4F0000) ---
              (emit-bytes buf #x48 #xBF) (emit-u32 buf #x4F0000) (emit-u32 buf 0)  ; mov rdi, 0x4F0000
              (emit-bytes buf #x48 #x31 #xC0)  ; xor rax, rax
              (emit-bytes buf #x48 #xB9) (emit-u32 buf 96) (emit-u32 buf 0)  ; mov rcx, 96
              (emit-bytes buf #xF3 #x48 #xAB)  ; rep stosq
              ;; --- Write IDT entry 0x20 at 0x4F0200 ---
              ;; ISR at 0x4F0800: offset_lo=0x0800, selector=0x10, type=0x8E
              (emit-bytes buf #x48 #xBF) (emit-u32 buf #x4F0200) (emit-u32 buf 0)  ; mov rdi, 0x4F0200
              (emit-bytes buf #xC7 #x07) (emit-u32 buf #x00100800)  ; [rdi] = selector<<16|offset_lo
              (emit-bytes buf #xC7 #x47 #x04) (emit-u32 buf #x004F8E00)  ; [rdi+4] = offset_mid<<16|type
              (emit-bytes buf #xC7 #x47 #x08) (emit-u32 buf 0)  ; [rdi+8] = offset_hi
              (emit-bytes buf #xC7 #x47 #x0C) (emit-u32 buf 0)  ; [rdi+12] = reserved
              ;; --- Write IDT entry 0x2B at 0x4F0000 + 0x2B*16 = 0x4F02B0 ---
              ;; E1000 IRQ 11 → vector 0x2B, ISR at 0x4F0810
              (emit-bytes buf #x48 #xBF) (emit-u32 buf #x4F02B0) (emit-u32 buf 0)  ; mov rdi, 0x4F02B0
              (emit-bytes buf #xC7 #x07) (emit-u32 buf #x00100810)  ; [rdi] = selector<<16|offset_lo
              (emit-bytes buf #xC7 #x47 #x04) (emit-u32 buf #x004F8E00)  ; [rdi+4] = offset_mid<<16|type
              (emit-bytes buf #xC7 #x47 #x08) (emit-u32 buf 0)  ; [rdi+8] = offset_hi
              (emit-bytes buf #xC7 #x47 #x0C) (emit-u32 buf 0)  ; [rdi+12] = reserved
              ;; --- Write PIT ISR at 0x4F0800 (8 bytes) ---
              ;; push rax; mov al,0x20; out 0x20,al; pop rax; iretq
              (emit-bytes buf #x48 #xBF) (emit-u32 buf #x4F0800) (emit-u32 buf 0)  ; mov rdi, 0x4F0800
              (emit-bytes buf #xC7 #x07) (emit-u32 buf #xE620B050)  ; ISR bytes 0-3
              (emit-bytes buf #xC7 #x47 #x04) (emit-u32 buf #xCF485820)  ; ISR bytes 4-7
              ;; --- Write E1000 ISR at 0x4F0810 (10 bytes) ---
              ;; Slave IRQ EOI: send 0x20 to slave PIC (0xA0) then master PIC (0x20)
              ;; Byte sequence: 50 B0 20 E6 A0 E6 20 58 48 CF
              ;; push rax; mov al,0x20; out 0xA0,al; out 0x20,al; pop rax; iretq
              (emit-bytes buf #x48 #xBF) (emit-u32 buf #x4F0810) (emit-u32 buf 0)  ; mov rdi, 0x4F0810
              ;; LE dwords: [50 B0 20 E6]=0xE620B050, [A0 E6 20 58]=0x5820E6A0, [48 CF xx xx]
              (emit-bytes buf #xC7 #x07) (emit-u32 buf #xE620B050)  ; bytes 0-3
              (emit-bytes buf #xC7 #x47 #x04) (emit-u32 buf #x5820E6A0)  ; bytes 4-7
              ;; bytes 8-9: 48 CF (REX prefix + iretq)
              (emit-bytes buf #x66 #xC7 #x47 #x08) (emit-u16 buf #xCF48)  ; mov word [rdi+8], 0xCF48
              ;; --- LIDT ---
              (emit-bytes buf #x48 #x83 #xEC #x10)  ; sub rsp, 16
              (emit-bytes buf #x66 #xC7 #x04 #x24 #xFF #x02)  ; mov word [rsp], 767
              (emit-bytes buf #xC7 #x44 #x24 #x02) (emit-u32 buf #x4F0000)  ; [rsp+2] = base low
              (emit-bytes buf #xC7 #x44 #x24 #x06) (emit-u32 buf 0)  ; [rsp+6] = base high
              (emit-bytes buf #x0F #x01 #x1C #x24)  ; lidt [rsp]
              (emit-bytes buf #x48 #x83 #xC4 #x10)  ; add rsp, 16
              (emit-bytes buf #x5F)  ; pop rdi
              (emit-bytes buf #x5A)  ; pop rdx
              (emit-bytes buf #x59)))) ; pop rcx — end of progn for bare-metal #x0320
             ((= code #x0321)
              ;; TIMER-REARM: NOP on x64 (only meaningful on AArch64 virt)
              nil)
             ((= code #x0310)
              ;; RDTSC: Read timestamp counter, return 64-bit result in RAX
              ;; Combine EDX:EAX into full 64-bit value
              ;; rdtsc
              (emit-bytes buf #x0F #x01 #xF9)  ; RDTSCP (waits for instructions)
              ;; mov ecx, eax (save low 32)
              (emit-bytes buf #x89 #xC1)
              ;; mov eax, edx
              (emit-bytes buf #x89 #xD0)
              ;; shl rax, 32
              (emit-bytes buf #x48 #xC1 #xE0 #x20)
              ;; or rax, rcx
              (emit-bytes buf #x48 #x09 #xC8))
             ((= code #x0510)
              (let ((skiparm-label (make-label)))
              ;; SETJMP: Save RSP, RBP, and return address to fixed memory.
              ;; On first call, returns NIL (#xDEAD0001) in RAX.
              ;; On longjmp, execution resumes here with RAX = T (#xDEAD1009).
              ;;
              ;; Fixed addresses (in Linux heap reserved area):
              ;;   0x10000180: saved RSP
              ;;   0x10000188: saved RBP
              ;;   0x10000190: saved return address (IP after this sequence)
              ;;
              ;; CRITICAL: must NOT overlap +closure-env-addr+ (0x10000140) —
              ;; every closure call writes env there, so sharing the address
              ;; with saved-RSP silently nuked handler-case state whenever the
              ;; body made a closure call. The cascade of FAIL lines in
              ;; per-test fork output was the signal handler longjmping with
              ;; a stale env-pointer masquerading as a saved RSP.
              ;;
              ;; We use LEA + RIP-relative to get the return address.
              ;; Layout:
              ;;   call __handler_push ; push outer state to stack at 0x10000408
              ;;   lea rcx, [rip+N]    ; address of "return point" after the jmp
              ;;   mov [addr], rsp     ; save RSP
              ;;   mov [addr+8], rbp   ; save RBP
              ;;   mov [addr+16], rcx  ; save return IP
              ;;   mov rax, NIL        ; first-time return value
              ;;   jmp +5              ; skip longjmp-return block
              ;;   (longjmp return point — RAX already has T from longjmp)
              ;;
              ;; First, save the OUTER handler state to the per-fork stack.
              ;; CLEAR-HANDLER (and longjmp/sigsegv) pop it back, so nested
              ;; handler-cases don't tear down the parent's setjmp frame.
              ;;
              ;; BARE-METAL: bracket the whole transition (push + arm) with
              ;; the in-transition flag at [0x10000D28].  The PIT deadline
              ;; ISR DEFERS its longjmp while the flag is set — otherwise a
              ;; deadline expiring mid-arm longjmps through a HALF-WRITTEN
              ;; [0x10000180] frame (new RSP, stale IP).
              (unless *x64-linux-mode*
                (emit-bytes buf #x48 #xC7 #x04 #x25) ; mov qword [imm32], 1
                (emit-u32 buf #x10000D28)
                (emit-u32 buf 1))
              (emit-call buf (translate-state-handler-push-label state))
              ;; BARE-METAL BALANCED-CAP: r11=1 from __handler_push means the
              ;; push was CAPPED (frame dropped, overflow counted) — skip
              ;; arming [0x10000180] so this handler-case is a transparent
              ;; no-op instead of a stack corrupter.  See emit-handler-helpers.
              (unless *x64-linux-mode*
                (emit-bytes buf #x4D #x85 #xDB)      ; test r11, r11
                (emit-jcc buf :ne skiparm-label))
              ;; Save RSP to 0x10000180
              ;; Use movabs with RCX as temp (address > 0x7FFFFFFF, can't use disp32)
              ;; mov rcx, 0x10000180
              (emit-bytes buf #x48 #xB9)
              (emit-u32 buf #x10000180) (emit-u32 buf 0)
              ;; mov [rcx], rsp
              (emit-bytes buf #x48 #x89 #x21)
              ;; mov [rcx+8], rbp
              (emit-bytes buf #x48 #x89 #x69 #x08)
              ;; mov [rcx+24], rbx  — save callee-saved RBX (V4) at slot 0x198.
              ;; The compiler caches values (e.g. let-bound cons-list cursors)
              ;; in V4 across function calls, relying on callees to preserve
              ;; it.  But a longjmp INTERRUPTS a mid-flight callee whose
              ;; epilogue may not have completed the RBX restore — RBX then
              ;; holds the callee's local value, not the setjmp-time value
              ;; the compiler tracks.  Saving RBX here and restoring in all
              ;; three longjmp paths (TRAP #x0511, #PF handler at 0x4F0820,
              ;; deadline-IRQ ISR at 0x4F0900) keeps RBX consistent.
              (emit-bytes buf #x48 #x89 #x59 #x18)
              ;; Save the address of the FIRST instruction after this trap block
              ;; into [rcx+16].  After lea (7 bytes), we still have:
              ;;   mov [rcx+16], rax     (4 bytes)
              ;;   movabs rax, NIL       (10 bytes)
              ;; plus the new mov [rcx+24], rbx (4 bytes; emitted ABOVE so
              ;; not in the post-lea distance).
              ;; = 14 bytes between end-of-LEA and end-of-trap-block (Linux).
              ;; BARE-METAL adds the 12-byte in-transition flag clear between
              ;; mov [rcx+16],rax and the movabs: distance = 4+12+10 = 26.
              ;; lea rax, [rip+N] lands rax at the byte AFTER the trap.
              (emit-bytes buf #x48 #x8D #x05
                          (if *x64-linux-mode* #x0E #x1A)
                          #x00 #x00 #x00)            ; lea rax, [rip+14/26]
              ;; mov [rcx+16], rax  — save return IP
              (emit-bytes buf #x48 #x89 #x41 #x10)
              (unless *x64-linux-mode*
                ;; capped setjmp lands here (arm skipped); both paths clear
                ;; the in-transition flag.
                (emit-label buf skiparm-label)
                (emit-bytes buf #x48 #xC7 #x04 #x25) ; mov qword [imm32], 0
                (emit-u32 buf #x10000D28)
                (emit-u32 buf 0))
              ;; mov rax, NIL (#xDEAD0001) — first-time return.
              ;; On longjmp, execution jumps just past this and RAX holds T
              ;; (set by LONGJMP trap), so the value going into VR is T.
              (emit-bytes buf #x48 #xB8)
              (emit-u32 buf #xDEAD0001) (emit-u32 buf 0)))
             ((= code #x0511)
              ;; LONGJMP: Restore RSP/RBP/IP from [#x10000180], then pop
              ;; the per-fork handler stack so the OUTER handler frame
              ;; becomes active. Sets RAX = T (#xDEAD1009) so setjmp
              ;; "returns" non-nil.  Body shared with the bare-metal
              ;; safepoint-deadline stub — see emit-longjmp-body.
              (emit-longjmp-body buf (translate-state-handler-pop-label state)))
             ((= code #x0512)
              ;; CLEAR-HANDLER: Pop outer handler state back into
              ;; [#x10000180/+8/+16]. If the per-fork handler stack is
              ;; empty, the helper writes 0 to [#x10000180] (legacy
              ;; "no handler" sentinel that the SIGSEGV stub checks).
              ;; __handler_pop preserves RAX (see emit-handler-helpers)
              ;; so (handler-case body) bodies that return in RAX aren't
              ;; clobbered by the pop.
              ;; BARE-METAL: bracket with the in-transition flag so the PIT
              ;; deadline ISR can't longjmp between the pop's depth--
              ;; and its [0x10000180] frame copy (a double-consume that
              ;; skips/leaks one frame).
              (unless *x64-linux-mode*
                (emit-bytes buf #x48 #xC7 #x04 #x25) ; mov qword [imm32], 1
                (emit-u32 buf #x10000D28)
                (emit-u32 buf 1))
              (emit-call buf (translate-state-handler-pop-label state))
              (unless *x64-linux-mode*
                (emit-bytes buf #x48 #xC7 #x04 #x25) ; mov qword [imm32], 0
                (emit-u32 buf #x10000D28)
                (emit-u32 buf 0)))
             ((= code #x0530)
              ;; COPY-OVERFLOW-ARGS: at runtime, read nargs from the
              ;; nargs slot and copy any args beyond V0..V3 from the
              ;; caller's stack (above RBP) into local frame slots 4..N
              ;; so the &rest prologue can build a list of any size.
              ;; Without this trap, emit-rest-prologue can only assemble
              ;; rest lists when nargs ≤ +max-reg-args+ (4) — beyond
              ;; that, callers (especially apply) silently truncate.
              ;; Cap raised to 32 args (was 24).  Frame has 128 slots
              ;; (1120 bytes) so 32 fits, and the per-defun &rest ladder
              ;; that mirrors this cap stays close to original code
              ;; size.  Going much higher blows the binary up (the
              ;; ladder is unrolled per defun) — raising to 50 cost
              ;; +30MB and regressed -7 ANSI via layout shift (measured
              ;; 2026-06-10), so it stays at 32.
              ;;
              ;; Layout:
              ;;   src ptr = rbp + 16 + (i-4)*8  for arg i ≥ 4
              ;;   dst ptr = rbp + frame-slot-base + i*-8
              ;;           = rbp - 96 - i*8
              ;;
              ;; Implementation:
              ;;   mov eax, [0x10000150]     ; nargs untagged
              ;;   cmp eax, 5
              ;;   jl  done
              ;;   cmp eax, 24
              ;;   jle nocap
              ;;   mov eax, 24
              ;; nocap:
              ;;   sub eax, 4                 ; count = min(nargs,16) - 4
              ;;   mov rdx, rbp
              ;;   add rdx, 16                ; src = rbp + 16
              ;;   mov r8,  rbp
              ;;   sub r8, 128                ; dst = rbp + slot[4] offset
              ;; loop:
              ;;   test eax, eax
              ;;   jz done
              ;;   mov rcx, [rdx]
              ;;   mov [r8], rcx
              ;;   add rdx, 8
              ;;   sub r8, 8
              ;;   dec eax
              ;;   jmp loop
              ;; done:
              ;;
              ;; mov eax, dword [0x10000150]
              (emit-bytes buf #xA1 #x50 #x01 #x00 #x10 #x00 #x00 #x00 #x00)
              ;; cmp eax, 5
              (emit-bytes buf #x83 #xF8 #x05)
              ;; jl done (32-bit relative — patched below)
              (let ((done-label (make-label))
                    (loop-label (make-label))
                    (nocap-label (make-label)))
                (emit-jcc buf :l done-label)
                ;; cmp eax, 32
                (emit-bytes buf #x83 #xF8 #x20)
                ;; jle nocap
                (emit-jcc buf :le nocap-label)
                ;; mov eax, 32
                (emit-bytes buf #xB8 #x20 #x00 #x00 #x00)
                (emit-label buf nocap-label)
                ;; sub eax, 4
                (emit-bytes buf #x83 #xE8 #x04)
                ;; mov rdx, rbp
                (emit-bytes buf #x48 #x89 #xEA)
                ;; add rdx, 16
                (emit-bytes buf #x48 #x83 #xC2 #x10)
                ;; mov r8, rbp
                (emit-bytes buf #x49 #x89 #xE8)
                ;; sub r8, 128 (imm32 — imm8=0x80 sign-extends to -128 = +128
                ;; which is the wrong direction).
                (emit-bytes buf #x49 #x81 #xE8 #x80 #x00 #x00 #x00)
                (emit-label buf loop-label)
                ;; test eax, eax
                (emit-bytes buf #x85 #xC0)
                ;; jz done
                (emit-jcc buf :e done-label)
                ;; mov rcx, [rdx]
                (emit-bytes buf #x48 #x8B #x0A)
                ;; mov [r8], rcx
                (emit-bytes buf #x49 #x89 #x08)
                ;; add rdx, 8
                (emit-bytes buf #x48 #x83 #xC2 #x08)
                ;; sub r8, 8
                (emit-bytes buf #x49 #x83 #xE8 #x08)
                ;; dec eax
                (emit-bytes buf #xFF #xC8)
                ;; jmp loop
                (emit-jmp buf loop-label)
                (emit-label buf done-label)))
             ((= code #x0520)
              ;; INSTALL-SIGNAL-HANDLERS:
              ;; Installs SIGSEGV (11), SIGBUS (7), SIGFPE (8), SIGILL (4) handlers.
              ;; The handler is an embedded assembly stub — NOT a Lisp function,
              ;; because a Lisp function entry would allocate stack and possibly
              ;; trigger GC (unsafe in signal context). The stub does the same
              ;; work as TRAP #x0511 (%hc-longjmp): if a handler-case is active
              ;; (saved RSP at 0x10000140 != 0), restore RSP/RBP/IP and jump back;
              ;; otherwise sys_exit(139).
              (let ((stub-label (make-label))
                    (exit-label (make-label))
                    (restorer-label (make-label))
                    (past-stub-label (make-label))
                    (pop-label (translate-state-handler-pop-label state)))
                ;; Skip stub on this execution path.
                (emit-jmp buf past-stub-label)

                ;; --- Embedded handler stub (kernel jumps here on signal) ---
                (emit-label buf stub-label)
                ;; FRAGILITY DIAG: capture useful state from the kernel's
                ;; ucontext (RDX, requires SA_SIGINFO above).
                ;;
                ;; Offsets are ucontext-relative: ucontext starts with
                ;; uc_flags(8) + uc_link(8) + uc_stack(24) = 40 bytes header,
                ;; then uc_mcontext.gregs[NGREG=23 longs of 8 bytes each].
                ;; gregs index for each named register:
                ;;   R8=0 .. R15=7, RDI=8, RSI=9, RBP=10, RBX=11, RDX=12,
                ;;   RAX=13, RCX=14, RSP=15, RIP=16
                ;; ucontext-relative byte offsets = 40 + gregs[i] * 8:
                ;;   RAX = 40 + 13*8 = 144 = 0x90
                ;;   RSP = 40 + 15*8 = 160 = 0xA0
                ;;   RIP = 40 + 16*8 = 168 = 0xA8
                ;;
                ;; Why each is useful:
                ;;   RIP   = the faulting target (= 0xdead0001 for `call NIL')
                ;;   RSP   = top of caller's stack at the moment of fault
                ;;   [RSP] = byte AFTER the failing call instruction —
                ;;           the call site we actually want to disassemble
                ;;   RAX   = what got loaded as the call target (0xdead0001
                ;;           confirms "tagged NIL was used as a function")
                ;;
                ;; Slots 0x10000C30/C38/C40/C48 — overwritten on each fault,
                ;; so the FAIL-record path reads them after the longjmp settles.
                (emit-bytes buf #x48 #x8B #x82 #xA8 #x00 #x00 #x00) ; mov rax, [rdx+0xA8] (saved RIP)
                (emit-bytes buf #x48 #x89 #x04 #x25)
                (emit-u32 buf #x10000C30)
                (emit-bytes buf #x48 #x8B #x82 #xA0 #x00 #x00 #x00) ; mov rax, [rdx+0xA0] (saved RSP)
                (emit-bytes buf #x48 #x89 #x04 #x25)
                (emit-u32 buf #x10000C38)
                (emit-bytes buf #x48 #x8B #x00)                     ; mov rax, [rax]    (call site)
                (emit-bytes buf #x48 #x89 #x04 #x25)
                (emit-u32 buf #x10000C40)
                (emit-bytes buf #x48 #x8B #x82 #x90 #x00 #x00 #x00) ; mov rax, [rdx+0x90] (saved RAX)
                (emit-bytes buf #x48 #x89 #x04 #x25)
                (emit-u32 buf #x10000C48)
                ;; Also capture si_addr (siginfo+16) and the kernel-passed
                ;; RDX itself, to verify our ucontext-relative reads.
                (emit-bytes buf #x48 #x8B #x46 #x10)                ; mov rax, [rsi+16] (si_addr)
                (emit-bytes buf #x48 #x89 #x04 #x25)
                (emit-u32 buf #x10000C50)
                (emit-bytes buf #x48 #x89 #xD0)                     ; mov rax, rdx (ucontext ptr)
                (emit-bytes buf #x48 #x89 #x04 #x25)
                (emit-u32 buf #x10000C58)
                ;; mov rcx, 0x10000180  (saved-handler-state address)
                (emit-bytes buf #x48 #xB9)
                (emit-u32 buf #x10000180) (emit-u32 buf 0)
                ;; rdx = [rcx]  (saved RSP — zero means no handler-case active)
                (emit-bytes buf #x48 #x8B #x11)
                ;; test rdx, rdx
                (emit-bytes buf #x48 #x85 #xD2)
                ;; jz exit_path
                (emit-jcc buf :e exit-label)
                ;; --- Active handler-case: do the longjmp ---
                ;; Save OUR state to scratch (#x10000C10..) before the pop
                ;; helper overwrites [180].
                ;; rdx already = [rcx] = our RSP
                (emit-bytes buf #x48 #x89 #x14 #x25)             ; mov [imm32], rdx
                (emit-u32 buf #x10000C10)
                ;; rdx = [rcx+8] (our RBP)
                (emit-bytes buf #x48 #x8B #x51 #x08)
                (emit-bytes buf #x48 #x89 #x14 #x25)
                (emit-u32 buf #x10000C18)
                ;; rdx = [rcx+16] (our IP)
                (emit-bytes buf #x48 #x8B #x51 #x10)
                (emit-bytes buf #x48 #x89 #x14 #x25)
                (emit-u32 buf #x10000C20)
                ;; rdx = [rcx+24] (our saved RBX).  The stub previously did
                ;; NOT restore RBX — unlike TRAP #x0511 — so a fault-driven
                ;; longjmp resumed the handler-case with the interrupted
                ;; callee's RBX (V4) still live: the setjmp-time V4 value the
                ;; compiler tracks was silently replaced (silent-unwind).
                (emit-bytes buf #x48 #x8B #x51 #x18)
                (emit-bytes buf #x48 #x89 #x14 #x25)
                (emit-u32 buf #x10000C28)
                ;; Pop handler stack into [180]/+8/+16/+24 so the outer
                ;; handler-case becomes active when we land in the body.
                (emit-call buf pop-label)
                ;; Restore from scratch and jump
                ;; mov rdx, [0x10000C20]   ; IP
                (emit-bytes buf #x48 #x8B #x14 #x25)
                (emit-u32 buf #x10000C20)
                ;; mov rbp, [0x10000C18]
                (emit-bytes buf #x48 #x8B #x2C #x25)
                (emit-u32 buf #x10000C18)
                ;; mov rbx, [0x10000C28]   ; restore caller's V4=RBX
                (emit-bytes buf #x48 #x8B #x1C #x25)
                (emit-u32 buf #x10000C28)
                ;; mov rsp, [0x10000C10]
                (emit-bytes buf #x48 #x8B #x24 #x25)
                (emit-u32 buf #x10000C10)
                ;; mov eax, 0xDEAD1009 (T sentinel — 32-bit imm zero-extends to rax)
                (emit-bytes buf #xB8 #x09 #x10 #xAD #xDE)
                ;; jmp rdx
                (emit-bytes buf #xFF #xE2)

                ;; --- No active handler: sys_exit(139) ---
                (emit-label buf exit-label)
                ;; mov edi, 139
                (emit-bytes buf #xBF #x8B #x00 #x00 #x00)
                ;; mov eax, 60 (sys_exit)
                (emit-bytes buf #xB8 #x3C #x00 #x00 #x00)
                ;; syscall
                (emit-bytes buf #x0F #x05)

                ;; --- Restorer trampoline (sa_restorer) ---
                ;; Linux x86-64 requires SA_RESTORER + sa_restorer on raw
                ;; sigaction; without them, signal delivery to user handlers
                ;; silently fails.  Our handler longjmps and never returns,
                ;; so the restorer is reached only if the handler returns
                ;; normally — defensive: invoke rt_sigreturn.
                (emit-label buf restorer-label)
                ;; mov eax, 15 (SYS_rt_sigreturn)
                (emit-bytes buf #xB8 #x0F #x00 #x00 #x00)
                (emit-bytes buf #x0F #x05)              ; syscall

                ;; --- Past stub: install handlers via rt_sigaction ---
                (emit-label buf past-stub-label)

                ;; Save callee-saved/clobbered regs.
                (emit-bytes buf #x57)              ; push rdi
                (emit-bytes buf #x52)              ; push rdx
                (emit-bytes buf #x41 #x50)         ; push r8
                (emit-bytes buf #x41 #x52)         ; push r10
                (emit-bytes buf #x41 #x53)         ; push r11

                ;; Allocate 32 bytes on stack for sigaction struct.
                (emit-bytes buf #x48 #x83 #xEC #x20)  ; sub rsp, 32

                ;; [rsp+0]  = sa_handler = stub-label.
                ;; lea rax, [rip+disp] then mov [rsp], rax.
                (emit-lea-label buf 'rax stub-label)
                (emit-bytes buf #x48 #x89 #x04 #x24) ; mov [rsp], rax
                ;; [rsp+8]  = sa_flags = SA_NODEFER | SA_RESTORER | SA_SIGINFO.
                ;; SA_RESTORER is required on x86-64 (kernel silently drops
                ;; signals without it). SA_NODEFER lets us re-enter the
                ;; handler for a SIGSEGV that happens during longjmp setup,
                ;; otherwise the kernel queues and eventually kills via
                ;; the default handler anyway.  SA_SIGINFO (0x4) is required
                ;; for the kernel to populate RDX with the ucontext pointer
                ;; on handler entry — without it, RDX is unspecified and our
                ;; ucontext-relative reads (uc_mcontext.gregs[…]) read
                ;; garbage from wherever RDX happened to point.
                (emit-bytes buf #x48 #xC7 #x44 #x24 #x08 #x04 #x00 #x00 #x44)
                ;; [rsp+16] = sa_restorer = restorer-label.
                (emit-lea-label buf 'rax restorer-label)
                (emit-bytes buf #x48 #x89 #x44 #x24 #x10)  ; mov [rsp+16], rax
                ;; [rsp+24] = sa_mask = 0
                (emit-bytes buf #x48 #xC7 #x44 #x24 #x18 #x00 #x00 #x00 #x00)

                ;; Install for each signum.
                (dolist (signum '(11 7 8 4))
                  (emit-bytes buf #x48 #xC7 #xC7) (emit-u32 buf signum) ; mov rdi, signum
                  (emit-bytes buf #x48 #x89 #xE6)        ; mov rsi, rsp
                  (emit-bytes buf #x48 #x31 #xD2)        ; xor rdx, rdx
                  (emit-bytes buf #x49 #xC7 #xC2 #x08 #x00 #x00 #x00) ; mov r10, 8
                  (emit-bytes buf #x48 #xC7 #xC0 #x0D #x00 #x00 #x00) ; mov rax, 13 (rt_sigaction)
                  (emit-bytes buf #x0F #x05))            ; syscall

                ;; Free struct
                (emit-bytes buf #x48 #x83 #xC4 #x20)  ; add rsp, 32
                ;; Restore regs
                (emit-bytes buf #x41 #x5B)         ; pop r11
                (emit-bytes buf #x41 #x5A)         ; pop r10
                (emit-bytes buf #x41 #x58)         ; pop r8
                (emit-bytes buf #x5A)              ; pop rdx
                (emit-bytes buf #x5F)              ; pop rdi
                ;; Result in V0 = NIL.
                ;; Encoding for `mov rsi, r15`: REX.W|R = 0x4C, opcode 0x89,
                ;; ModRM 0xFE (mod=11, reg=R15 via REX.R, rm=RSI).  Previous
                ;; bytes 0x49 0x89 0xFE encoded `mov r14, rdi` (REX.W|B with
                ;; reg=RDI, rm=R14 via REX.B), silently clobbering the
                ;; alloc-limit r14 every time signal handlers were installed
                ;; — every subsequent GC check then took the wrong branch
                ;; and the allocator walked past the mmap region, SEGV'ing
                ;; mid-test (e.g. test 3091 on Linux x64 after %INTERN-SYMBOL).
                (emit-bytes buf #x4C #x89 #xFE)))  ; mov rsi, r15
             (t
              ;; Real CPU trap
              (emit-mov-reg-imm buf 'rax code)
              (emit-int buf #x30)))))

        ;; ============================================
        ;; Data Movement
        ;; ============================================
        ((op= +op-mov+)
         ;; (mov Vd Vs)
         (let ((vd (first operands))
               (vs (second operands)))
           (emit-vreg-to-vreg buf vd vs)))

        ((op= +op-li+)
         ;; (li Vd imm64)
         (let ((vd (first operands))
               (imm (second operands)))
           (let ((d (dest-phys-or-scratch vd)))
             (emit-mov-reg-imm buf d imm)
             (maybe-store-scratch buf vd))))

        ((op= +op-li-const+)
         ;; (li-const Vd idx)
         ;; Emit a MOVABS reg, 0 placeholder.  The 8-byte immediate
         ;; will be patched with the tagged pool address at image-
         ;; assembly time.  emit-mov-reg-imm in 64-bit mode emits:
         ;;   REX.W (1B) | B8+r (1B) | imm64 (8B) = 10 bytes.
         ;; The immediate sits at start+2.
         (let* ((vd (first operands))
                (idx (second operands))
                (d (dest-phys-or-scratch vd))
                (start-pos (code-buffer-position buf)))
           (emit-mov-reg-imm buf d 0)
           ;; Sanity: 10-byte movabs.  If this changes (e.g. due to
           ;; small-immediate optimisation in emit-mov-reg-imm), the
           ;; patch offset below would be wrong.
           (let ((emitted (- (code-buffer-position buf) start-pos)))
             (unless (= emitted 10)
               (error "li-const: expected 10-byte movabs, got ~D" emitted)))
           (push (cons (+ start-pos 2) idx) *x64-li-const-patches*)
           (maybe-store-scratch buf vd)))

        ((op= +op-push+)
         ;; (push Vs)
         (let* ((vs (first operands))
                (phys (vreg-phys vs)))
           (if phys
               (emit-push buf phys)
               (progn
                 (emit-load-vreg buf vs +scratch-reg+)
                 (emit-push buf +scratch-reg+)))))

        ((op= +op-pop+)
         ;; (pop Vd)
         (let* ((vd (first operands))
                (phys (vreg-phys vd)))
           (if phys
               (emit-pop buf phys)
               (progn
                 (emit-pop buf +scratch-reg+)
                 (emit-store-vreg buf vd +scratch-reg+)))))

        ;; ============================================
        ;; Arithmetic (tagged fixnum)
        ;; ============================================
        ((op= +op-add+)
         ;; (add Vd Va Vb) — tagged fixnums add directly
         (let ((vd (first operands))
               (va (second operands))
               (vb (third operands)))
           (emit-alu-rrr buf #'emit-add-reg-reg vd va vb)))

        ((op= +op-sub+)
         ;; (sub Vd Va Vb) — tagged fixnums subtract directly
         (let ((vd (first operands))
               (va (second operands))
               (vb (third operands)))
           (emit-alu-rrr buf #'emit-sub-reg-reg vd va vb)))

        ;; Overflow-detecting add/sub.  Same encoding as plain ADD/SUB
        ;; on x86 — they already set OF — so we reuse the regular emit
        ;; helper and rely on the very next MVM insn being :bvs.
        ;; MOV/STR for spill writeback do not touch OF.
        ((op= +op-adds+)
         (let ((vd (first operands))
               (va (second operands))
               (vb (third operands)))
           (emit-alu-rrr buf #'emit-add-reg-reg vd va vb)))

        ((op= +op-subs+)
         (let ((vd (first operands))
               (va (second operands))
               (vb (third operands)))
           (emit-alu-rrr buf #'emit-sub-reg-reg vd va vb)))

        ((op= +op-bvs+)
         (let* ((off (first operands))
                (target-pos (+ mvm-next-pos off))
                (label (ensure-label-at state target-pos)))
           (emit-jcc buf :o label)))

        ((op= +op-mul+)
         ;; (mul Vd Va Vb) — tagged fixnum multiplication.
         ;;
         ;; Naïve "IMUL tagged_a tagged_b, then SAR 1" overflows when
         ;; the raw product approaches 2^62 (fixnum max).  Reason:
         ;;   tagged_a = a_raw << 1
         ;;   tagged_b = b_raw << 1
         ;;   IMUL → (a_raw * b_raw) << 2
         ;; Fits in 64-bit signed only when (a_raw * b_raw) < 2^62 / 4
         ;; = 2^60.  Past that the intermediate spills into bit 63 and
         ;; the SAR 1 propagates a stale sign — silently corrupting any
         ;; product whose raw value sits in [2^60, 2^62).  This was
         ;; the bug behind (* 293429342220215299 10) returning negative
         ;; junk (see %print-decimal-to-stream + %bignum-divmod-fixnum
         ;; chains on bignum-lo values in [2^60, 2^62)).
         ;;
         ;; Fix: untag ONE operand FIRST (SAR 1 on a) so the intermediate
         ;; is (a_raw * b_raw) << 1 = tagged result directly — no SAR 1
         ;; needed afterward, and overflow happens only when the result
         ;; itself can't fit in a tagged fixnum (= what we want).
         ;;
         ;; x86-64 IMUL r64, r/m64 (two-operand form):  REX.W 0F AF /r
         (let* ((vd (first operands))
                (va (second operands))
                (vb (third operands))
                (d (dest-phys-or-scratch vd)))
           ;; Load Va into d and untag.
           (emit-load-vreg buf va d)
           (emit-sar-reg-imm buf d 1)
           ;; IMUL d, Vb (tagged) → tagged result directly.
           (let ((pb (vreg-phys vb)))
             (if pb
                 (emit-imul-reg-reg buf d pb)
                 (progn
                   (let ((tmp (if (eq d 'rax) 'r13 'rax)))
                     (emit-push buf tmp)
                     (emit-load-vreg buf vb tmp)
                     (emit-imul-reg-reg buf d tmp)
                     (emit-pop buf tmp)))))
           (maybe-store-scratch buf vd)))

        ((op= +op-mul-checked+)
         ;; Tagged * with bignum overflow promotion.  Multiply in R13 (saved;
         ;; not free) so va/vb vregs stay intact for the slow path.  On signed
         ;; overflow, call GENERIC-MULTIPLY(va,vb) -> bignum.
         ;;
         ;; CALLER-SAVE: GENERIC-MULTIPLY is a full Lisp call that clobbers ALL
         ;; caller-saved registers (V0=rsi V1=rdi V2=r8 V3=r9 V5=rcx V6=rdx
         ;; V7=r10 V8=r11) and may trigger GC.  The register allocator does NOT
         ;; know this inline slow path makes a call, so it never spills live
         ;; vregs around it.  We therefore save/restore every caller-saved GP
         ;; reg + rbx(V4) here — the correct ABI for an inline call that may
         ;; fire on a genuine user-level overflow mid-expression with live
         ;; vregs.  (r12/r14/r15 are global alloc/heap regs the callee
         ;; preserves; GC scans our pushed copies as roots and forwards them,
         ;; so stack save is correct.)  NB: the historic "implicit global
         ;; <param>" compiler corruption was NOT a caller-save gap — it was the
         ;; intern composite-key `(* pkg-hash 2^61-1)` promoting to a bignum
         ;; (lossy in-image logand); fixed in cl-packages.lisp / prelude.lisp
         ;; by switching that site to %fixnum-* (raw wrapping :mul).
         (let* ((vd (first operands)) (va (second operands)) (vb (third operands))
                (d (dest-phys-or-scratch vd))
                (gm-label *x64-genmul-label*))
           (cond
             (gm-label
              (let ((done (make-label)))
                (emit-push buf 'r13)
                (emit-load-vreg buf va 'r13)
                (emit-sar-reg-imm buf 'r13 1)
                (let ((pb (vreg-phys vb)))
                  (if pb (emit-imul-reg-reg buf 'r13 pb)
                      (progn (emit-push buf 'rax) (emit-load-vreg buf vb 'rax)
                             (emit-imul-reg-reg buf 'r13 'rax) (emit-pop buf 'rax))))
                (emit-jcc buf :no done)
                ;; --- overflow slow path: GENERIC-MULTIPLY(va, vb) ---
                ;; Save ALL caller-saved GP regs + rbx (V4) before loading args.
                ;; 1 (r13) + 9 here = 10 pushes = 80 bytes -> 16-aligned at call.
                (emit-push buf 'rsi) (emit-push buf 'rdi)
                (emit-push buf 'r8)  (emit-push buf 'r9)
                (emit-push buf 'rcx) (emit-push buf 'rdx)
                (emit-push buf 'r10) (emit-push buf 'r11)
                (emit-push buf 'rbx)
                (emit-load-vreg buf va 'rsi)           ; arg0 = va (still intact)
                (emit-load-vreg buf vb 'rdi)           ; arg1 = vb
                (emit-bytes buf #xC7 #x04 #x25 #x50 #x01 #x00 #x10 #x02 #x00 #x00 #x00) ; [nargs]=2
                (emit-call buf gm-label)
                (emit-mov-reg-reg buf 'r13 'rax)       ; result -> r13
                (emit-pop buf 'rbx)
                (emit-pop buf 'r11) (emit-pop buf 'r10)
                (emit-pop buf 'rdx) (emit-pop buf 'rcx)
                (emit-pop buf 'r9)  (emit-pop buf 'r8)
                (emit-pop buf 'rdi) (emit-pop buf 'rsi)
                (emit-label buf done)
                (emit-mov-reg-reg buf d 'r13)
                (emit-pop buf 'r13)
                (maybe-store-scratch buf vd)))
             (t
              (emit-load-vreg buf va d) (emit-sar-reg-imm buf d 1)
              (let ((pb (vreg-phys vb)))
                (if pb (emit-imul-reg-reg buf d pb)
                    (let ((tmp (if (eq d 'rax) 'r13 'rax)))
                      (emit-push buf tmp) (emit-load-vreg buf vb tmp)
                      (emit-imul-reg-reg buf d tmp) (emit-pop buf tmp))))
              (maybe-store-scratch buf vd)))))

        ((op= +op-add-checked+)
         ;; Tagged + with bignum overflow promotion.  tag(a)+tag(b)=2(a+b)=tag(a+b)
         ;; directly (no untag needed); ADD sets OF iff a+b leaves fixnum range.
         ;; Sum into R13 (saved) so va/vb vregs stay intact for the slow call.
         ;; Same full caller-save ABI as op-mul-checked (GENERIC-ADD is a full
         ;; Lisp call that clobbers all caller-saved regs and may GC).
         (let* ((vd (first operands)) (va (second operands)) (vb (third operands))
                (d (dest-phys-or-scratch vd))
                (ga-label *x64-genadd-label*))
           (cond
             (ga-label
              (let ((done (make-label)))
                (emit-push buf 'r13)
                (emit-load-vreg buf va 'r13)
                (let ((pb (vreg-phys vb)))
                  (if pb (emit-add-reg-reg buf 'r13 pb)
                      (progn (emit-push buf 'rax) (emit-load-vreg buf vb 'rax)
                             (emit-add-reg-reg buf 'r13 'rax) (emit-pop buf 'rax))))
                (emit-jcc buf :no done)
                ;; --- overflow slow path: GENERIC-ADD(va, vb) ---
                (emit-push buf 'rsi) (emit-push buf 'rdi)
                (emit-push buf 'r8)  (emit-push buf 'r9)
                (emit-push buf 'rcx) (emit-push buf 'rdx)
                (emit-push buf 'r10) (emit-push buf 'r11)
                (emit-push buf 'rbx)
                (emit-load-vreg buf va 'rsi)
                (emit-load-vreg buf vb 'rdi)
                (emit-bytes buf #xC7 #x04 #x25 #x50 #x01 #x00 #x10 #x02 #x00 #x00 #x00) ; [nargs]=2
                (emit-call buf ga-label)
                (emit-mov-reg-reg buf 'r13 'rax)
                (emit-pop buf 'rbx)
                (emit-pop buf 'r11) (emit-pop buf 'r10)
                (emit-pop buf 'rdx) (emit-pop buf 'rcx)
                (emit-pop buf 'r9)  (emit-pop buf 'r8)
                (emit-pop buf 'rdi) (emit-pop buf 'rsi)
                (emit-label buf done)
                (emit-mov-reg-reg buf d 'r13)
                (emit-pop buf 'r13)
                (maybe-store-scratch buf vd)))
             (t
              (emit-load-vreg buf va d)
              (let ((pb (vreg-phys vb)))
                (if pb (emit-add-reg-reg buf d pb)
                    (let ((tmp (if (eq d 'rax) 'r13 'rax)))
                      (emit-push buf tmp) (emit-load-vreg buf vb tmp)
                      (emit-add-reg-reg buf d tmp) (emit-pop buf tmp))))
              (maybe-store-scratch buf vd)))))

        ((op= +op-mul26lo+)
         ;; (mul26lo Vd Va Vb) — low 26 bits of untag(Va)*untag(Vb), tagged
         ;; On x64: untag both, IMUL (64-bit result is enough), AND 0x3FFFFFF, retag
         (let* ((vd (first operands))
                (va (second operands))
                (vb (third operands))
                (d (dest-phys-or-scratch vd)))
           (emit-load-vreg buf va d)
           (emit-sar-reg-imm buf d 1)        ; untag va
           (let ((pb (vreg-phys vb)))
             (if pb
                 (progn
                   (emit-push buf 'r13)
                   (emit-mov-reg-reg buf 'r13 pb)
                   (emit-sar-reg-imm buf 'r13 1)  ; untag vb
                   (emit-imul-reg-reg buf d 'r13)
                   (emit-pop buf 'r13))
                 (progn
                   (let ((tmp (if (eq d 'rax) 'r13 'rax)))
                     (emit-push buf tmp)
                     (emit-load-vreg buf vb tmp)
                     (emit-sar-reg-imm buf tmp 1)
                     (emit-imul-reg-reg buf d tmp)
                     (emit-pop buf tmp)))))
           (emit-and-reg-imm buf d #x3FFFFFF)
           (emit-shl-reg-imm buf d 1)         ; retag
           (maybe-store-scratch buf vd)))

        ((op= +op-mul26hi+)
         ;; (mul26hi Vd Va Vb) — bits 26+ of untag(Va)*untag(Vb), tagged
         ;; On x64: untag both, IMUL, SHR 26, retag
         (let* ((vd (first operands))
                (va (second operands))
                (vb (third operands))
                (d (dest-phys-or-scratch vd)))
           (emit-load-vreg buf va d)
           (emit-sar-reg-imm buf d 1)        ; untag va
           (let ((pb (vreg-phys vb)))
             (if pb
                 (progn
                   (emit-push buf 'r13)
                   (emit-mov-reg-reg buf 'r13 pb)
                   (emit-sar-reg-imm buf 'r13 1)  ; untag vb
                   (emit-imul-reg-reg buf d 'r13)
                   (emit-pop buf 'r13))
                 (progn
                   (let ((tmp (if (eq d 'rax) 'r13 'rax)))
                     (emit-push buf tmp)
                     (emit-load-vreg buf vb tmp)
                     (emit-sar-reg-imm buf tmp 1)
                     (emit-imul-reg-reg buf d tmp)
                     (emit-pop buf tmp)))))
           (emit-shr-reg-imm buf d 26)
           (emit-shl-reg-imm buf d 1)         ; retag
           (maybe-store-scratch buf vd)))

        ((op= +op-mul64lo+)
         ;; (mul64lo Vd Va Vb) — low 64 bits of raw Va*Vb (no tag/untag)
         ;; On x64: MOV RAX, Va; MUL Vb → RDX:RAX; result in RAX
         (let* ((vd (first operands))
                (va (second operands))
                (vb (third operands)))
           ;; Save RDX (V6) — MUL clobbers it
           (emit-push buf 'rdx)
           ;; Load Va into RAX
           (emit-load-vreg buf va 'rax)
           ;; Get Vb into a register for MUL
           (let ((pb (vreg-phys vb)))
             (if pb
                 ;; MUL r/m64: REX.W F7 /4  (unsigned multiply RDX:RAX = RAX * r/m)
                 (emit-mul-reg buf pb)
                 (progn
                   (emit-push buf 'r13)
                   (emit-load-vreg buf vb 'r13)
                   (emit-mul-reg buf 'r13)
                   (emit-pop buf 'r13))))
           ;; Result (low 64) is in RAX
           (emit-store-vreg buf vd 'rax)
           (emit-pop buf 'rdx)))

        ((op= +op-mul64hi+)
         ;; (mul64hi Vd Va Vb) — high 64 bits of raw Va*Vb (no tag/untag)
         ;; On x64: MOV RAX, Va; MUL Vb → RDX:RAX; result in RDX
         (let* ((vd (first operands))
                (va (second operands))
                (vb (third operands)))
           ;; Save RDX (V6) unless Vd is V6 (RDX)
           (unless (= vd 6) (emit-push buf 'rdx))
           ;; Load Va into RAX
           (emit-load-vreg buf va 'rax)
           ;; Get Vb into a register for MUL
           (let ((pb (vreg-phys vb)))
             (if pb
                 (emit-mul-reg buf pb)
                 (progn
                   (emit-push buf 'r13)
                   (emit-load-vreg buf vb 'r13)
                   (emit-mul-reg buf 'r13)
                   (emit-pop buf 'r13))))
           ;; Result (high 64) is in RDX
           (emit-store-vreg buf vd 'rdx)
           (unless (= vd 6) (emit-pop buf 'rdx))))

        ((op= +op-acc128+)
         ;; (acc128 Vaddr Vlo Vhi) — mem128[Vaddr] += Vhi:Vlo (raw)
         ;; On x64: ADD [addr], lo; ADC [addr+8], hi
         (let ((vaddr (first operands))
               (vlo (second operands))
               (vhi (third operands)))
           ;; Save scratch regs
           (emit-push buf 'r13)
           ;; Load addr into r13
           (emit-load-vreg buf vaddr 'r13)
           ;; Load lo into RAX (save/restore around it)
           (emit-push buf 'rax)
           (emit-load-vreg buf vlo 'rax)
           ;; ADD [r13], RAX — sets carry flag
           (emit-add-mem-reg buf 'r13 'rax 0)
           ;; Load hi
           (emit-load-vreg buf vhi 'rax)
           ;; ADC [r13+8], RAX — add with carry
           (emit-adc-mem-reg buf 'r13 'rax 8)
           ;; Restore
           (emit-pop buf 'rax)
           (emit-pop buf 'r13)))

        ((op= +op-div+)
         ;; (div Vd Va Vb) — tagged fixnum division
         ;; IDIV divides RDX:RAX by operand; quotient in RAX.
         ;; Must save both operands to stack first since IDIV clobbers
         ;; RAX and RDX, and CQO clobbers RDX — any of which may hold
         ;; Va or Vb (V5=RCX, V6=RDX, VR=RAX).
         (let ((vd (first operands))
               (va (second operands))
               (vb (third operands)))
           ;; Save both operands to stack (safe regardless of physical mapping)
           (emit-load-vreg buf va 'rax)
           (emit-push buf 'rax)
           (emit-load-vreg buf vb 'rax)
           (emit-push buf 'rax)
           ;; Pop divisor → RCX, untag
           (emit-pop buf 'rcx)
           (emit-sar-reg-imm buf 'rcx 1)
           ;; Pop dividend → RAX, untag
           (emit-pop buf 'rax)
           (emit-sar-reg-imm buf 'rax 1)
           ;; CQO: sign-extend RAX → RDX:RAX (safe: Vb is in RCX)
           (emit-bytes buf #x48 #x99)
           ;; IDIV RCX: RAX = quotient, RDX = remainder
           (emit-bytes buf #x48 #xF7 #xF9)
           ;; Re-tag quotient: SHL RAX, 1
           (emit-shl-reg-imm buf 'rax 1)
           (emit-store-vreg buf vd 'rax)))

        ((op= +op-mod+)
         ;; (mod Vd Va Vb) — tagged fixnum modulus
         ;; Same stack-save approach as div to avoid register clobbering.
         (let ((vd (first operands))
               (va (second operands))
               (vb (third operands)))
           (emit-load-vreg buf va 'rax)
           (emit-push buf 'rax)
           (emit-load-vreg buf vb 'rax)
           (emit-push buf 'rax)
           (emit-pop buf 'rcx)
           (emit-sar-reg-imm buf 'rcx 1)
           (emit-pop buf 'rax)
           (emit-sar-reg-imm buf 'rax 1)
           (emit-bytes buf #x48 #x99)         ; CQO
           (emit-bytes buf #x48 #xF7 #xF9)    ; IDIV RCX
           ;; Remainder in RDX → re-tag
           (emit-shl-reg-imm buf 'rdx 1)
           (emit-store-vreg buf vd 'rdx)))

        ((op= +op-neg+)
         ;; (neg Vd Vs) — negate tagged fixnum
         ;; NEG preserves the tag for fixnums: -(n<<1) = (-n)<<1
         (let* ((vd (first operands))
                (vs (second operands))
                (d (dest-phys-or-scratch vd)))
           (emit-load-vreg buf vs d)
           ;; NEG r64: REX.W F7 /3
           (emit-neg-reg buf d)
           (maybe-store-scratch buf vd)))

        ((op= +op-inc+)
         ;; (inc Vd) — add tagged fixnum 1 (= raw 2)
         (let* ((vd (first operands))
                (phys (vreg-phys vd)))
           (if phys
               (emit-add-reg-imm buf phys 2)
               (progn
                 (emit-load-vreg buf vd +scratch-reg+)
                 (emit-add-reg-imm buf +scratch-reg+ 2)
                 (emit-store-vreg buf vd +scratch-reg+)))))

        ((op= +op-dec+)
         ;; (dec Vd) — subtract tagged fixnum 1 (= raw 2)
         (let* ((vd (first operands))
                (phys (vreg-phys vd)))
           (if phys
               (emit-sub-reg-imm buf phys 2)
               (progn
                 (emit-load-vreg buf vd +scratch-reg+)
                 (emit-sub-reg-imm buf +scratch-reg+ 2)
                 (emit-store-vreg buf vd +scratch-reg+)))))

        ;; ============================================
        ;; Bitwise Operations
        ;; ============================================
        ((op= +op-and+)
         ;; (and Vd Va Vb) — bitwise AND (tag-preserving for fixnums)
         (let ((vd (first operands))
               (va (second operands))
               (vb (third operands)))
           (emit-alu-rrr buf #'emit-and-reg-reg vd va vb)))

        ((op= +op-or+)
         ;; (or Vd Va Vb)
         (let ((vd (first operands))
               (va (second operands))
               (vb (third operands)))
           (emit-alu-rrr buf #'emit-or-reg-reg vd va vb)))

        ((op= +op-xor+)
         ;; (xor Vd Va Vb)
         (let ((vd (first operands))
               (va (second operands))
               (vb (third operands)))
           (emit-alu-rrr buf #'emit-xor-reg-reg vd va vb)))

        ((op= +op-shl+)
         ;; (shl Vd Vs imm8) — shift left by immediate
         (let* ((vd (first operands))
                (vs (second operands))
                (count (third operands))
                (d (dest-phys-or-scratch vd)))
           (emit-load-vreg buf vs d)
           (emit-shl-reg-imm buf d count)
           (maybe-store-scratch buf vd)))

        ((op= +op-shr+)
         ;; (shr Vd Vs imm8) — logical shift right
         (let* ((vd (first operands))
                (vs (second operands))
                (count (third operands))
                (d (dest-phys-or-scratch vd)))
           (emit-load-vreg buf vs d)
           (emit-shr-reg-imm buf d count)
           (maybe-store-scratch buf vd)))

        ((op= +op-sar+)
         ;; (sar Vd Vs imm8) — arithmetic shift right
         (let* ((vd (first operands))
                (vs (second operands))
                (count (third operands))
                (d (dest-phys-or-scratch vd)))
           (emit-load-vreg buf vs d)
           (emit-sar-reg-imm buf d count)
           (maybe-store-scratch buf vd)))

        ((op= +op-shlv+)
         ;; (shlv Vd Vs Vc) — shift left by register count
         ;; x86-64 variable shifts require count in CL (RCX)
         (let* ((vd (first operands))
                (vs (second operands))
                (vc (third operands))
                (d (dest-phys-or-scratch vd))
                (pc (vreg-phys vc))
                (need-save (and (not (eq pc 'rcx))
                                (not (eq d 'rcx)))))
           ;; Load source into dest reg
           (emit-load-vreg buf vs d)
           ;; Get shift count into RCX
           (cond
             ((eq pc 'rcx))  ; already in RCX
             (t
              (when need-save (emit-push buf 'rcx))
              (emit-load-vreg buf vc 'rcx)))
           ;; Shift
           (emit-shl-reg-cl buf d)
           ;; Restore RCX if saved
           (when (and need-save (not (eq pc 'rcx)))
             (emit-pop buf 'rcx))
           (maybe-store-scratch buf vd)))

        ((op= +op-sarv+)
         ;; (sarv Vd Vs Vc) — arithmetic shift right by register count
         (let* ((vd (first operands))
                (vs (second operands))
                (vc (third operands))
                (d (dest-phys-or-scratch vd))
                (pc (vreg-phys vc))
                (need-save (and (not (eq pc 'rcx))
                                (not (eq d 'rcx)))))
           (emit-load-vreg buf vs d)
           (cond
             ((eq pc 'rcx))
             (t
              (when need-save (emit-push buf 'rcx))
              (emit-load-vreg buf vc 'rcx)))
           (emit-sar-reg-cl buf d)
           (when (and need-save (not (eq pc 'rcx)))
             (emit-pop buf 'rcx))
           (maybe-store-scratch buf vd)))

        ((op= +op-ldb+)
         ;; (ldb Vd Vs pos:imm8 size:imm8) — bit field extract
         ;; Shift right by pos, mask to size bits
         (let* ((vd (first operands))
                (vs (second operands))
                (pos (third operands))
                (size (fourth operands))
                (d (dest-phys-or-scratch vd))
                (mask (1- (ash 1 size))))
           (emit-load-vreg buf vs d)
           (when (> pos 0)
             (emit-shr-reg-imm buf d pos))
           (emit-and-reg-imm buf d mask)
           (maybe-store-scratch buf vd)))

        ;; ============================================
        ;; Comparison
        ;; ============================================
        ((op= +op-cmp+)
         ;; (cmp Va Vb) — sets CPU flags
         (let ((va (first operands))
               (vb (second operands)))
           (let ((pa (vreg-phys va))
                 (pb (vreg-phys vb)))
             (cond
               ;; Both physical
               ((and pa pb)
                (emit-cmp-reg-reg buf pa pb))
               ;; Va physical, Vb spilled
               ((and pa (null pb))
                (emit-push buf 'rax)
                (emit-load-vreg buf vb 'rax)
                (emit-cmp-reg-reg buf pa 'rax)
                (emit-pop buf 'rax))
               ;; Va spilled, Vb physical
               ((and (null pa) pb)
                (emit-push buf 'rax)
                (emit-load-vreg buf va 'rax)
                (emit-cmp-reg-reg buf 'rax pb)
                (emit-pop buf 'rax))
               ;; Both spilled
               (t
                (emit-push buf 'rax)
                (emit-push buf 'r13)
                (emit-load-vreg buf va 'rax)
                (emit-load-vreg buf vb 'r13)
                (emit-cmp-reg-reg buf 'rax 'r13)
                (emit-pop buf 'r13)
                (emit-pop buf 'rax))))))

        ((op= +op-test+)
         ;; (test Va Vb) — AND, sets flags, discards result
         (let ((va (first operands))
               (vb (second operands)))
           (let ((pa (vreg-phys va))
                 (pb (vreg-phys vb)))
             (cond
               ((and pa pb)
                (emit-test-reg-reg buf pa pb))
               ((and pa (null pb))
                (emit-push buf 'rax)
                (emit-load-vreg buf vb 'rax)
                (emit-test-reg-reg buf pa 'rax)
                (emit-pop buf 'rax))
               ((and (null pa) pb)
                (emit-push buf 'rax)
                (emit-load-vreg buf va 'rax)
                (emit-test-reg-reg buf 'rax pb)
                (emit-pop buf 'rax))
               (t
                (emit-push buf 'rax)
                (emit-push buf 'r13)
                (emit-load-vreg buf va 'rax)
                (emit-load-vreg buf vb 'r13)
                (emit-test-reg-reg buf 'rax 'r13)
                (emit-pop buf 'r13)
                (emit-pop buf 'rax))))))

        ;; ============================================
        ;; Branches
        ;; ============================================
        ;;
        ;; MVM branch offsets are 16-bit signed, relative to the end
        ;; of the branch instruction in the MVM bytecode stream.
        ;; We compute the absolute MVM target position and emit a
        ;; Jcc/JMP to the corresponding native label.

        ((op= +op-br+)
         ;; (br off16) — unconditional branch
         (let* ((off (first operands))
                (target-pos (+ mvm-next-pos off))
                (label (ensure-label-at state target-pos)))
           (emit-jmp buf label)))

        ((op= +op-beq+)
         (let* ((off (first operands))
                (target-pos (+ mvm-next-pos off))
                (label (ensure-label-at state target-pos)))
           (emit-jcc buf :e label)))

        ((op= +op-bne+)
         (let* ((off (first operands))
                (target-pos (+ mvm-next-pos off))
                (label (ensure-label-at state target-pos)))
           (emit-jcc buf :ne label)))

        ((op= +op-blt+)
         (let* ((off (first operands))
                (target-pos (+ mvm-next-pos off))
                (label (ensure-label-at state target-pos)))
           (emit-jcc buf :l label)))

        ((op= +op-bge+)
         (let* ((off (first operands))
                (target-pos (+ mvm-next-pos off))
                (label (ensure-label-at state target-pos)))
           (emit-jcc buf :ge label)))

        ((op= +op-ble+)
         (let* ((off (first operands))
                (target-pos (+ mvm-next-pos off))
                (label (ensure-label-at state target-pos)))
           (emit-jcc buf :le label)))

        ((op= +op-bgt+)
         (let* ((off (first operands))
                (target-pos (+ mvm-next-pos off))
                (label (ensure-label-at state target-pos)))
           (emit-jcc buf :g label)))

        ((op= +op-bnull+)
         ;; (bnull Vs off16) — compare Vs against R15 (NIL), branch if equal
         (let* ((vs (first operands))
                (off (second operands))
                (target-pos (+ mvm-next-pos off))
                (label (ensure-label-at state target-pos))
                (ps (vreg-phys vs)))
           (if ps
               (emit-cmp-reg-reg buf ps 'r15)
               (progn
                 (emit-push buf 'rax)
                 (emit-load-vreg buf vs 'rax)
                 (emit-cmp-reg-reg buf 'rax 'r15)
                 (emit-pop buf 'rax)))
           (emit-jcc buf :e label)))

        ((op= +op-bnnull+)
         ;; (bnnull Vs off16) — branch if Vs is not NIL
         (let* ((vs (first operands))
                (off (second operands))
                (target-pos (+ mvm-next-pos off))
                (label (ensure-label-at state target-pos))
                (ps (vreg-phys vs)))
           (if ps
               (emit-cmp-reg-reg buf ps 'r15)
               (progn
                 (emit-push buf 'rax)
                 (emit-load-vreg buf vs 'rax)
                 (emit-cmp-reg-reg buf 'rax 'r15)
                 (emit-pop buf 'rax)))
           (emit-jcc buf :ne label)))

        ;; ============================================
        ;; List Operations
        ;; ============================================
        ((op= +op-car+)
         ;; (car Vd Vs) — load car: [Vs - 1] (untag cons ptr)
         ;;
         ;; The bare deref is intentional.  CL semantics say (car X) on a
         ;; non-cons (other than NIL) signals a TYPE-ERROR.  Modus
         ;; achieves that by faulting: the in-process SIGSEGV handler at
         ;; #x0520 converts the SEGV into a condition, handler-case
         ;; catches it, and *.ERROR.* tests get the T they expect.  An
         ;; earlier bug-6 fix attempt added a tag-check + return-NIL
         ;; fast-path here; that closed our 4 markers and -90 ANSI
         ;; *.ERROR.* tests because silently returning NIL is NOT what
         ;; ANSI says car-of-fixnum should do.  See
         ;; fragility-open-problem.md "bug 6 misdiagnosis (2026-04-28)"
         ;; for the full debrief.
         (let* ((vd (first operands))
                (vs (second operands))
                (d (dest-phys-or-scratch vd))
                (ps (vreg-phys vs)))
           (if ps
               (emit-mov-reg-mem buf d ps -1)
               (progn
                 ;; Load Vs into temp, then deref
                 (let ((tmp (if (eq d 'rax) 'r13 'rax)))
                   (emit-push buf tmp)
                   (emit-load-vreg buf vs tmp)
                   (emit-mov-reg-mem buf d tmp -1)
                   (emit-pop buf tmp))))
           (maybe-store-scratch buf vd)))

        ((op= +op-cdr+)
         ;; (cdr Vd Vs) — load cdr: [Vs + 7] (-1 + 8)
         ;; See +op-car+ for why no tag-check here.
         (let* ((vd (first operands))
                (vs (second operands))
                (d (dest-phys-or-scratch vd))
                (ps (vreg-phys vs)))
           (if ps
               (emit-mov-reg-mem buf d ps 7)
               (progn
                 (let ((tmp (if (eq d 'rax) 'r13 'rax)))
                   (emit-push buf tmp)
                   (emit-load-vreg buf vs tmp)
                   (emit-mov-reg-mem buf d tmp 7)
                   (emit-pop buf tmp))))
           (maybe-store-scratch buf vd)))

        ((op= +op-cons+)
         ;; (cons Vd Va Vb) — allocate cons cell via bump allocator
         ;; [R12+0] = car (Va), [R12+8] = cdr (Vb)
         ;; result = R12 | 1 (cons tag), R12 += 16
         (let* ((vd (first operands))
                (va (second operands))
                (vb (third operands))
                (d (dest-phys-or-scratch vd)))
           ;; Store car
           (let ((pa (vreg-phys va)))
             (if pa
                 (emit-mov-mem-reg buf 'r12 pa 0)
                 (progn
                   (emit-load-vreg buf va +scratch-reg+)
                   (emit-mov-mem-reg buf 'r12 +scratch-reg+ 0))))
           ;; Store cdr
           (let ((pb (vreg-phys vb)))
             (if pb
                 (emit-mov-mem-reg buf 'r12 pb 8)
                 (progn
                   (emit-load-vreg buf vb +scratch-reg+)
                   (emit-mov-mem-reg buf 'r12 +scratch-reg+ 8))))
           ;; MCGC object-start bit (R12 still = cons base).
           (emit-mcgc-set-start-bit buf 'r12)
           ;; ...and the CONS-KIND bit (this start IS a cons) — out-of-line
           ;; CALL (5B) to the shared setter, which reads R12.
           (emit-mcgc-call-cons-bit buf)
           ;; Result = R12 + 1 (cons tag)
           (emit-lea buf d 'r12 1)
           ;; Advance alloc pointer
           (emit-add-reg-imm buf 'r12 16)
           (maybe-store-scratch buf vd)))

        ((op= +op-setcar+)
         ;; (setcar Vd Vs) — [Vd - 1] = Vs (write through cons tag)
         (let* ((vd (first operands))
                (vs (second operands))
                (pd (vreg-phys vd))
                (ps (vreg-phys vs)))
           (cond
             ((and pd ps)
              (emit-mov-mem-reg buf pd ps -1))
             ((and pd (null ps))
              (emit-push buf 'rax)
              (emit-load-vreg buf vs 'rax)
              (emit-mov-mem-reg buf pd 'rax -1)
              (emit-pop buf 'rax))
             ((and (null pd) ps)
              (emit-push buf 'rax)
              (emit-load-vreg buf vd 'rax)
              (emit-mov-mem-reg buf 'rax ps -1)
              (emit-pop buf 'rax))
             (t
              (emit-push buf 'rax)
              (emit-push buf 'r13)
              (emit-load-vreg buf vd 'rax)
              (emit-load-vreg buf vs 'r13)
              (emit-mov-mem-reg buf 'rax 'r13 -1)
              (emit-pop buf 'r13)
              (emit-pop buf 'rax)))))

        ((op= +op-setcdr+)
         ;; (setcdr Vd Vs) — [Vd + 7] = Vs
         (let* ((vd (first operands))
                (vs (second operands))
                (pd (vreg-phys vd))
                (ps (vreg-phys vs)))
           (cond
             ((and pd ps)
              (emit-mov-mem-reg buf pd ps 7))
             ((and pd (null ps))
              (emit-push buf 'rax)
              (emit-load-vreg buf vs 'rax)
              (emit-mov-mem-reg buf pd 'rax 7)
              (emit-pop buf 'rax))
             ((and (null pd) ps)
              (emit-push buf 'rax)
              (emit-load-vreg buf vd 'rax)
              (emit-mov-mem-reg buf 'rax ps 7)
              (emit-pop buf 'rax))
             (t
              (emit-push buf 'rax)
              (emit-push buf 'r13)
              (emit-load-vreg buf vd 'rax)
              (emit-load-vreg buf vs 'r13)
              (emit-mov-mem-reg buf 'rax 'r13 7)
              (emit-pop buf 'r13)
              (emit-pop buf 'rax)))))

        ((op= +op-consp+)
         ;; (consp Vd Vs) — test low bit for cons tag (0x01)
         ;; Must exclude NIL (0xDEAD0001 has cons tag but is not a cons)
         ;; Result: T or NIL in Vd
         (let* ((vd (first operands))
                (vs (second operands))
                (d (dest-phys-or-scratch vd))
                (false-label (make-label))
                (true-label (make-label))
                (end-label (make-label)))
           (emit-load-vreg buf vs d)
           ;; Check for nil first: nil is NOT a cons
           (emit-cmp-reg-reg buf d 'r15)
           (emit-jcc buf :e false-label)
           ;; Test low 4 bits: AND with 0x0F, compare to 0x01
           (let ((scratch d))
             (emit-and-reg-imm buf scratch #x0F)
             (emit-cmp-reg-imm buf scratch 1)
             (emit-jcc buf :e true-label))
           ;; Not a cons: load NIL
           (emit-label buf false-label)
           (emit-mov-reg-reg buf d 'r15)
           (emit-jmp buf end-label)
           ;; Is a cons: load T
           (emit-label buf true-label)
           (emit-mov-reg-imm buf d #xDEAD1009)
           (emit-label buf end-label)
           (maybe-store-scratch buf vd)))

        ((op= +op-atom+)
         ;; (atom Vd Vs) — opposite of consp, but nil IS an atom
         (let* ((vd (first operands))
                (vs (second operands))
                (d (dest-phys-or-scratch vd))
                (true-label (make-label))
                (end-label (make-label)))
           (emit-load-vreg buf vs d)
           ;; Check for nil first: nil IS an atom
           (emit-cmp-reg-reg buf d 'r15)
           (emit-jcc buf :e true-label)
           ;; Test low 4 bits
           (emit-and-reg-imm buf d #x0F)
           (emit-cmp-reg-imm buf d 1)
           (emit-jcc buf :ne true-label)
           ;; Is a cons → atom returns NIL
           (emit-mov-reg-reg buf d 'r15)
           (emit-jmp buf end-label)
           ;; Not a cons (or nil) → atom returns T
           (emit-label buf true-label)
           (emit-mov-reg-imm buf d #xDEAD1009)
           (emit-label buf end-label)
           (maybe-store-scratch buf vd)))

        ;; ============================================
        ;; Object Operations
        ;; ============================================
        ((op= +op-alloc-obj+)
         ;; (alloc-obj Vd count:imm16 subtag:imm8)
         ;; Allocate an object with COUNT elements from bump allocator.
         ;; Write header word at [R12]: (count << 8) | subtag
         ;; Result = R12 | 0x09 (object tag), advance R12 by (count+2)*8.
         ;; Elements start at offset 16 (8 byte header + 8 byte padding).
         (let* ((vd (first operands))
                (count (second operands))
                (subtag (third operands))
                (d (dest-phys-or-scratch vd))
                (header (logior (ash count 8) subtag))
                (alloc-bytes (logand (+ (* (+ count 2) 8) 15) (lognot 15))))
           ;; Write header
           (emit-mov-reg-imm buf +scratch-reg+ header)
           (emit-mov-mem-reg buf 'r12 +scratch-reg+ 0)
           ;; ---- Zero-initialise the payload (CRITICAL for GC correctness) ----
           ;; The Cheney scan (emit-gc-trampoline) is a FLAT word-by-word walk
           ;; from to_start..free_ptr that calls scan_word on EVERY word — it
           ;; does NOT respect object headers.  So any uninitialised payload
           ;; word (the +8 padding slot and the data slots) that a callsite has
           ;; not yet written is scanned as a potential root.  If such a garbage
           ;; word has low nibble 1/9 and lands in [from_start, from_end),
           ;; copy_object treats it as a live pointer, reads its target's first
           ;; word as a "header", and (if the size guard passes) writes a
           ;; forwarding pointer into that RANDOM from-space location —
           ;; clobbering whatever real object lives there.  This is the
           ;; allocation-pressure-dependent "#<?N>" heap corruption (a live
           ;; condition / keyword object's header overwritten mid-load).
           ;; Allocators that fill every slot before the next gc-check are safe,
           ;; but %intern-keyword / %make-symbol / %make-float / closures alloc
           ;; then run MORE allocating code (aset, puthash, cons) before slot-0
           ;; is written, opening the window.  Zeroing the payload makes every
           ;; not-yet-written word a fixnum 0 (nibble 0), which scan_word
           ;; ignores.  Header is at +0; the payload words are the +8 padding
           ;; word and the COUNT data words (offsets +16.. ) — COUNT+1 words.
           ;; (The 16-byte alignment-tail word that exists for odd COUNT is
           ;; deliberately left alone: zeroing it as well shifted code layout
           ;; enough to re-expose a SEPARATE residual corruption site near the
           ;; ASDF/BUNDLE forms — a contiguous define-package cascade returned
           ;; — whereas zeroing exactly the logical payload broke the cascade
           ;; cleanly.  The tail word is beyond the object's data and is a
           ;; lower-risk follow-up; see the handoff.)  RAX (= +scratch-reg+)
           ;; held the header and is free now; the LEA for d happens after.
           (emit-bytes buf #x31 #xC0)               ; xor eax, eax  (rax = 0)
           (let ((off 8))
             (dotimes (i (+ count 1))
               (emit-mov-mem-reg buf 'r12 'rax off)  ; mov [r12+off], rax
               (incf off 8)))
           ;; MCGC object-start bit (R12 still = object base).
           (emit-mcgc-set-start-bit buf 'r12)
           ;; Result = R12 | object-tag
           (emit-lea buf d 'r12 #x09)
           ;; Advance alloc pointer: (count+2)*8, aligned to 16
           (emit-add-reg-imm buf 'r12 alloc-bytes)
           (maybe-store-scratch buf vd)))

        ((op= +op-alloc-array+)
         ;; (alloc-array Vd Vcount) — dynamic array allocation
         ;; Vcount: UNTAGGED element count (compiler SAR'd it)
         ;; Allocates (count+1)*8 bytes, aligned to 16 (header + elements)
         ;; Header = (count << 8) | array-subtag
         ;; Result = R12 | 0x09 (object tag)
         ;;
         ;; Ordering note: do all scratch-RAX work FIRST (header build,
         ;; size calc, R12 advance) and compute d = R12-old + 9 LAST via
         ;; the saved R12-old on the stack.  Earlier code emitted
         ;; `LEA d, [R12+9]` BEFORE a `POP RAX`; when `d == RAX`
         ;; (because vd's phys reg was RAX, or vd was spilled and d
         ;; defaulted to scratch), the pop clobbered the array pointer
         ;; with the restored count.  Resulting array had length 0 and
         ;; the slot stored the aligned size instead of the pointer.
         (let* ((vd (first operands))
                (vcount (second operands))
                (d (dest-phys-or-scratch vd))
                (pc (vreg-phys vcount)))
           ;; Save R12-old on stack for final LEA.
           (emit-push buf 'r12)
           ;; Load count into scratch register.
           (if pc
               (emit-mov-reg-reg buf +scratch-reg+ pc)
               (emit-load-vreg buf vcount +scratch-reg+))
           ;; Save count on stack (will be clobbered by header build).
           (emit-push buf +scratch-reg+)
           ;; Build header: (count << 8) | subtag-array.
           (emit-shl-reg-imm buf +scratch-reg+ 8)
           (emit-or-reg-imm buf +scratch-reg+ #x32)  ; array subtag
           ;; Write header at [R12] (R12 still points to array base).
           (emit-mov-mem-reg buf 'r12 +scratch-reg+ 0)
           ;; MCGC object-start bit (R12 still = array base; the helper
           ;; saves/restores RAX so the header value it holds is irrelevant
           ;; — it is overwritten by the count pop on the next line).
           (emit-mcgc-set-start-bit buf 'r12)
           ;; Restore count, compute allocation size.
           (emit-pop buf +scratch-reg+)
           ;; size = (count + 2) << 3, aligned to 16
           ;; +2 because elements start at offset +16 (header + padding)
           (emit-add-reg-imm buf +scratch-reg+ 2)  ; count + 2
           (emit-shl-reg-imm buf +scratch-reg+ 3)  ; * 8 bytes per word
           (emit-add-reg-imm buf +scratch-reg+ 15)  ; for alignment
           (emit-and-reg-imm buf +scratch-reg+ -16) ; align to 16
           ;; Advance alloc pointer.
           (emit-add-reg-reg buf 'r12 +scratch-reg+)
           ;; Recover R12-old (the array base) and compute d = base | 9.
           (emit-pop buf +scratch-reg+)
           ;; ---- Zero-initialise the payload (CRITICAL for GC correctness) ----
           ;; The caller writes array elements AFTER this opcode returns; any
           ;; alloc it does in between can trigger the Cheney GC, whose flat
           ;; to-space scan would read this object's still-uninitialised
           ;; payload words as candidate roots (see +op-alloc-obj+).  Zero
           ;; them so every not-yet-written word is fixnum 0 (nibble 0),
           ;; ignored by scan_word.  RAX (+scratch-reg+) = base; R12 = new
           ;; top.  Walk RCX from base+8 (skip header) to R12, storing RDX=0.
           ;; RCX/RDX are saved/restored so the surrounding allocator state
           ;; is untouched; RAX survives the loop, so the final LEA is valid.
           (emit-push buf 'rcx)
           (emit-push buf 'rdx)
           (emit-lea buf 'rcx +scratch-reg+ 8)        ; rcx = base + 8
           (emit-bytes buf #x48 #x31 #xD2)            ; xor rdx, rdx
           (emit-zero-word-range buf 'rcx 'r12 'rdx)
           (emit-pop buf 'rdx)
           (emit-pop buf 'rcx)
           (emit-lea buf d +scratch-reg+ #x09)
           (maybe-store-scratch buf vd)))

        ((op= +op-set-cenv+)
         ;; Set R13 (closure-env reg) from Vs.  R13 is reserved for
         ;; passing the closure env-list across funcall — caller writes
         ;; it before the indirect call, callee reads it at entry.
         (let* ((vs (first operands))
                (ps (vreg-phys vs)))
           (if ps
               (emit-mov-reg-reg buf 'r13 ps)
               (progn
                 (emit-load-vreg buf vs +scratch-reg+)
                 (emit-mov-reg-reg buf 'r13 +scratch-reg+)))))

        ((op= +op-get-cenv+)
         ;; Read R13 into Vd.  Closure body prologue uses this exactly
         ;; once at entry to copy the env-list into a local before any
         ;; nested funcall could overwrite R13.
         (let* ((vd (first operands))
                (d (dest-phys-or-scratch vd)))
           (emit-mov-reg-reg buf d 'r13)
           (maybe-store-scratch buf vd)))

        ((op= +op-set-nargs+)
         ;; Store nargs (imm8) at fixed slot #x10000150 — the nargs
         ;; convention slot used by callees with &rest to know how
         ;; many args the caller passed. Encoded as:
         ;;   mov dword [0x10000150], imm32
         (let ((n (first operands)))
           (emit-bytes buf #xC7 #x04 #x25)         ; mov [disp32], imm32
           (emit-bytes buf #x50 #x01 #x00 #x10)    ; disp32 = #x10000150
           (emit-bytes buf (logand n #xFF) #x00 #x00 #x00)))

        ((op= +op-get-nargs+)
         ;; Load the nargs slot into RAX (scratch), tag as fixnum
         ;; (shl 1), then move to Vd.  Tagging here keeps :get-nargs
         ;; compatible with the rest of the IR's tagged-value world
         ;; — comparisons against :li (which takes already-tagged
         ;; immediates) and :cons just work.
         (let* ((vd (first operands))
                (d (dest-phys-or-scratch vd)))
           ;; mov eax, dword [0x10000150]
           (emit-bytes buf #xA1)                   ; mov eax, m32 (special form)
           (emit-bytes buf #x50 #x01 #x00 #x10
                            #x00 #x00 #x00 #x00)   ; abs64 (mov eax variant)
           ;; shl rax, 1  — tag as fixnum
           (emit-bytes buf #x48 #xD1 #xE0)
           (unless (eq d 'rax)
             (emit-mov-reg-reg buf d 'rax))
           (maybe-store-scratch buf vd)))

        ;; ================================================================
        ;; IEEE 64-bit float arithmetic — SSE2 lowering.
        ;;
        ;; Float OBJECTS in modus are subtag #x60, 2 slots:
        ;;   slot 0 = hi32 tagged fixnum (sign-extended into bits 32..63)
        ;;   slot 1 = lo32 tagged fixnum (positive when masked to low 32)
        ;; Stored value = tag-shift left by 1.
        ;;
        ;; Per-op pattern:
        ;;   1. Stack-save both operand pointers (avoids vreg/scratch aliasing).
        ;;   2. Pop each, load slot 0 + slot 1, untag (sar 1), combine into a
        ;;      single u64 of IEEE bits, MOVQ into XMM.
        ;;   3. ADDSD / SUBSD / MULSD / DIVSD xmm0, xmm1.
        ;;   4. Allocate a fresh 2-slot float object via the R12 bump
        ;;      (header at [R12], padding at [R12+8], slots at [R12+16/24]).
        ;;   5. MOVQ rcx, xmm0; split into hi32/lo32; tag-shift left by 1;
        ;;      store back to slots; LEA dest, [R12+9]; ADD R12, 32.
        ;;
        ;; XMM0/XMM1 are caller-saved on System V; modus's allocator doesn't
        ;; otherwise use XMM regs so we can clobber freely.
        ;; ================================================================
        ((or (op= +op-fadd+) (op= +op-fsub+) (op= +op-fmul+) (op= +op-fdiv+))
         (let* ((vd (first operands))
                (va (second operands))
                (vb (third operands))
                (sse-opcode (cond ((op= +op-fadd+) #x58)   ; ADDSD
                                  ((op= +op-fsub+) #x5C)   ; SUBSD
                                  ((op= +op-fmul+) #x59)   ; MULSD
                                  (t                #x5E)))) ; DIVSD
           ;; Stack-save operands.
           (emit-load-vreg buf va 'rax)
           (emit-push buf 'rax)
           (emit-load-vreg buf vb 'rax)
           (emit-push buf 'rax)

           ;; Pop Vb pointer → load its IEEE bits into xmm1.
           (emit-pop buf 'rax)
           (emit-mov-reg-mem buf 'rcx 'rax 7)    ; slot 0 (hi32 tagged)
           (emit-sar-reg-imm buf 'rcx 1)         ; untag
           (emit-shl-reg-imm buf 'rcx 32)        ; into upper half
           (emit-mov-reg-mem buf 'rdx 'rax 15)   ; slot 1 (lo32 tagged)
           (emit-sar-reg-imm buf 'rdx 1)         ; untag (sign-ext)
           (emit-shl-reg-imm buf 'rdx 32)        ; mask off sign
           (emit-shr-reg-imm buf 'rdx 32)        ; via shl/shr 32
           (emit-or-reg-reg buf 'rcx 'rdx)       ; combine
           (emit-bytes buf #x66 #x48 #x0F #x6E #xC9)  ; MOVQ xmm1, rcx

           ;; Pop Va pointer → load its IEEE bits into xmm0.
           (emit-pop buf 'rax)
           (emit-mov-reg-mem buf 'rcx 'rax 7)
           (emit-sar-reg-imm buf 'rcx 1)
           (emit-shl-reg-imm buf 'rcx 32)
           (emit-mov-reg-mem buf 'rdx 'rax 15)
           (emit-sar-reg-imm buf 'rdx 1)
           (emit-shl-reg-imm buf 'rdx 32)
           (emit-shr-reg-imm buf 'rdx 32)
           (emit-or-reg-reg buf 'rcx 'rdx)
           (emit-bytes buf #x66 #x48 #x0F #x6E #xC1)  ; MOVQ xmm0, rcx

           ;; Float op: ADDSD/SUBSD/MULSD/DIVSD xmm0, xmm1
           ;;   F2 0F <op> C1  (ModR/M: 11 000 001 — xmm0 dest, xmm1 src)
           (emit-bytes buf #xF2 #x0F sse-opcode #xC1)

           ;; Allocate fresh 2-slot float object at R12.
           (emit-mov-reg-imm buf 'rcx #x260)            ; (count=2)<<8 | subtag #x60
           (emit-mov-mem-reg buf 'r12 'rcx 0)           ; header at [R12]

           ;; Extract result bits: MOVQ rcx, xmm0
           (emit-bytes buf #x66 #x48 #x0F #x7E #xC1)

           ;; Slot 0 = (hi32 sign-extended) << 1
           (emit-mov-reg-reg buf 'rdx 'rcx)
           (emit-sar-reg-imm buf 'rdx 32)
           (emit-shl-reg-imm buf 'rdx 1)
           (emit-mov-mem-reg buf 'r12 'rdx 16)

           ;; Slot 1 = (lo32 zero-extended) << 1
           (emit-mov-reg-reg buf 'rdx 'rcx)
           (emit-shl-reg-imm buf 'rdx 32)
           (emit-shr-reg-imm buf 'rdx 32)
           (emit-shl-reg-imm buf 'rdx 1)
           (emit-mov-mem-reg buf 'r12 'rdx 24)

           ;; MCGC object-start bit (R12 still = float base).
           (emit-mcgc-set-start-bit buf 'r12)
           ;; Result tagged pointer = R12 + 9; advance R12 by 32 bytes.
           (let ((d (dest-phys-or-scratch vd)))
             (emit-lea buf d 'r12 9)
             (emit-add-reg-imm buf 'r12 32)
             (maybe-store-scratch buf vd))))

        ((op= +op-itof+)
         ;; (itof Vd Vs) — tagged integer → freshly-allocated float object.
         (let* ((vd (first operands))
                (vs (second operands)))
           ;; Load tagged int into RAX, untag (SAR 1), CVTSI2SD xmm0, rax
           (emit-load-vreg buf vs 'rax)
           (emit-sar-reg-imm buf 'rax 1)
           ;; CVTSI2SD xmm0, rax: F2 REX.W 0F 2A C0
           (emit-bytes buf #xF2 #x48 #x0F #x2A #xC0)
           ;; Allocate + store-back (same tail as fadd)
           (emit-mov-reg-imm buf 'rcx #x260)
           (emit-mov-mem-reg buf 'r12 'rcx 0)
           (emit-bytes buf #x66 #x48 #x0F #x7E #xC1)   ; MOVQ rcx, xmm0
           (emit-mov-reg-reg buf 'rdx 'rcx)
           (emit-sar-reg-imm buf 'rdx 32)
           (emit-shl-reg-imm buf 'rdx 1)
           (emit-mov-mem-reg buf 'r12 'rdx 16)
           (emit-mov-reg-reg buf 'rdx 'rcx)
           (emit-shl-reg-imm buf 'rdx 32)
           (emit-shr-reg-imm buf 'rdx 32)
           (emit-shl-reg-imm buf 'rdx 1)
           (emit-mov-mem-reg buf 'r12 'rdx 24)
           ;; MCGC object-start bit (R12 still = float base).
           (emit-mcgc-set-start-bit buf 'r12)
           (let ((d (dest-phys-or-scratch vd)))
             (emit-lea buf d 'r12 9)
             (emit-add-reg-imm buf 'r12 32)
             (maybe-store-scratch buf vd))))

        ((op= +op-ftoi+)
         ;; (ftoi Vd Vs) — float → tagged integer (truncate toward zero).
         (let* ((vd (first operands))
                (vs (second operands)))
           ;; Load Vs float bits → xmm0
           (emit-load-vreg buf vs 'rax)
           (emit-mov-reg-mem buf 'rcx 'rax 7)
           (emit-sar-reg-imm buf 'rcx 1)
           (emit-shl-reg-imm buf 'rcx 32)
           (emit-mov-reg-mem buf 'rdx 'rax 15)
           (emit-sar-reg-imm buf 'rdx 1)
           (emit-shl-reg-imm buf 'rdx 32)
           (emit-shr-reg-imm buf 'rdx 32)
           (emit-or-reg-reg buf 'rcx 'rdx)
           (emit-bytes buf #x66 #x48 #x0F #x6E #xC1)   ; MOVQ xmm0, rcx
           ;; CVTTSD2SI rax, xmm0: F2 REX.W 0F 2C C0
           (emit-bytes buf #xF2 #x48 #x0F #x2C #xC0)
           ;; Tag as fixnum: shl rax, 1
           (emit-shl-reg-imm buf 'rax 1)
           (let ((d (dest-phys-or-scratch vd)))
             (unless (eq d 'rax) (emit-mov-reg-reg buf d 'rax))
             (maybe-store-scratch buf vd))))

        ((op= +op-fcmp+)
         ;; (fcmp Va Vb) — UCOMISD-style float compare, sets x86 flags.
         ;; After this, :beq/:blt/:bgt/etc. work (UCOMISD sets ZF/PF/CF).
         (let* ((va (first operands))
                (vb (second operands)))
           (emit-load-vreg buf va 'rax)
           (emit-push buf 'rax)
           (emit-load-vreg buf vb 'rax)
           (emit-push buf 'rax)
           ;; Vb → xmm1
           (emit-pop buf 'rax)
           (emit-mov-reg-mem buf 'rcx 'rax 7)
           (emit-sar-reg-imm buf 'rcx 1)
           (emit-shl-reg-imm buf 'rcx 32)
           (emit-mov-reg-mem buf 'rdx 'rax 15)
           (emit-sar-reg-imm buf 'rdx 1)
           (emit-shl-reg-imm buf 'rdx 32)
           (emit-shr-reg-imm buf 'rdx 32)
           (emit-or-reg-reg buf 'rcx 'rdx)
           (emit-bytes buf #x66 #x48 #x0F #x6E #xC9)   ; MOVQ xmm1, rcx
           ;; Va → xmm0
           (emit-pop buf 'rax)
           (emit-mov-reg-mem buf 'rcx 'rax 7)
           (emit-sar-reg-imm buf 'rcx 1)
           (emit-shl-reg-imm buf 'rcx 32)
           (emit-mov-reg-mem buf 'rdx 'rax 15)
           (emit-sar-reg-imm buf 'rdx 1)
           (emit-shl-reg-imm buf 'rdx 32)
           (emit-shr-reg-imm buf 'rdx 32)
           (emit-or-reg-reg buf 'rcx 'rdx)
           (emit-bytes buf #x66 #x48 #x0F #x6E #xC1)   ; MOVQ xmm0, rcx
           ;; UCOMISD xmm0, xmm1: 66 0F 2E C1
           (emit-bytes buf #x66 #x0F #x2E #xC1)))

        ((op= +op-alloc-string+)
         ;; Like alloc-array but with string subtag #x31.
         ;; Same d/scratch-clobber bug fix as +op-alloc-array+: compute
         ;; d = R12-old + 9 via the saved R12-old at the end, after all
         ;; RAX-clobbering work is done.
         (let* ((vd (first operands))
                (vcount (second operands))
                (d (dest-phys-or-scratch vd))
                (pc (vreg-phys vcount)))
           (emit-push buf 'r12)              ; save R12-old
           (if pc (emit-mov-reg-reg buf +scratch-reg+ pc)
               (emit-load-vreg buf vcount +scratch-reg+))
           (emit-push buf +scratch-reg+)
           (emit-shl-reg-imm buf +scratch-reg+ 8)
           (emit-or-reg-imm buf +scratch-reg+ #x31)  ; STRING subtag
           (emit-mov-mem-reg buf 'r12 +scratch-reg+ 0)
           ;; MCGC object-start bit (R12 still = string base).
           (emit-mcgc-set-start-bit buf 'r12)
           (emit-pop buf +scratch-reg+)      ; restore count
           (emit-add-reg-imm buf +scratch-reg+ 2)
           (emit-shl-reg-imm buf +scratch-reg+ 3)
           (emit-add-reg-imm buf +scratch-reg+ 15)
           (emit-and-reg-imm buf +scratch-reg+ -16)
           (emit-add-reg-reg buf 'r12 +scratch-reg+)
           (emit-pop buf +scratch-reg+)      ; recover R12-old
           ;; ---- Payload zero-init DELIBERATELY NOT done here ----
           ;; ROOT-CAUSE UPDATE (Fable5 string/GC seat): the prior "string
           ;; headers count BYTES" hypothesis is FALSE.  Strings store one
           ;; char-code per 8-byte WORD (compile-quote / %codes-to-string fill
           ;; via :obj-set / aset), so the element count IS a word count and
           ;; alloc-string's `(count+2)*8` advance AGREES exactly with
           ;; copy_object's `(count+2)*8` copy size (emit-gc-trampoline).  There
           ;; is NO byte/word size disagreement — copy is correct for #x31.
           ;;
           ;; A clean layout-isolation probe (emit the identical push/pop/loop
           ;; instructions but zero an EMPTY range — `lea rcx,[r12]`) keeps the
           ;; gauntlet at 243/44, while the REAL zeroing collapses it to 87.
           ;; So the regression is the FUNCTIONAL zeroing of string CONTENT,
           ;; NOT layout shift and NOT a GC-scan/size bug (disproving the prior
           ;; seat's "fix the size accounting first" plan — there's nothing to
           ;; fix there).  Runtime size-gating localised TWO independent
           ;; functional regressions:
           ;;   - zeroing the 4096-word file read buffer (%make-string-array
           ;;     4096 in %make-file-stream-full, cl-fileio.lisp) corrupts the
           ;;     package-name symbol read at gauntlet form 80; and
           ;;   - zeroing only SHORT strings still desyncs the reader by form
           ;;     ~125.
           ;; Both are reader/heap-content interactions exposed by turning the
           ;; payload garbage into NUL (char-code 0), not GC-root false-
           ;; positives, and neither reproduces under plain read/alloc stress.
           ;; Root-causing the desync needs a MODUS_GC_DEBUG-traced build; until
           ;; then the safe state is byte-identical-to-baseline (no zeroing, no
           ;; added instructions → no layout shift).  The residual #<?90> at
           ;; gauntlet 233/236 (corrupted-subtag objects) survives and is a
           ;; SEPARATE site — it is present at baseline with string-zero OFF, so
           ;; it is not caused by un-zeroed string bodies.  See handoff.
           (emit-lea buf d +scratch-reg+ #x09)
           (maybe-store-scratch buf vd)))

        ((op= +op-obj-ref+)
         ;; (obj-ref Vd Vobj idx:imm8) — load slot at offset
         (let* ((vd (first operands))
                (vobj (second operands))
                (idx (third operands))
                (d (dest-phys-or-scratch vd)))
           (if (= vobj +vreg-vfp+)
               ;; Frame slot access: use safe RBP-relative offset below spill area
               (emit-mov-reg-mem buf d 'rbp (+ +frame-slot-base+ (* idx -8)))
               ;; Normal object slot access
               ;; Slot address = (Vobj - 9) + 8 + idx*8 = Vobj + (idx*8 - 1)
               (let ((offset (+ (* idx 8) -1 8))
                     (po (vreg-phys vobj)))
                 (if po
                     (emit-mov-reg-mem buf d po offset)
                     (progn
                       (let ((tmp (if (eq d 'rax) 'r13 'rax)))
                         (emit-push buf tmp)
                         (emit-load-vreg buf vobj tmp)
                         (emit-mov-reg-mem buf d tmp offset)
                         (emit-pop buf tmp))))))
           (maybe-store-scratch buf vd)))

        ((op= +op-obj-set+)
         ;; (obj-set Vobj idx:imm8 Vs) — store slot
         (let* ((vobj (first operands))
                (idx (second operands))
                (vs (third operands)))
           (if (= vobj +vreg-vfp+)
               ;; Frame slot store: use safe RBP-relative offset below spill area
               (let ((ps (vreg-phys vs)))
                 (if ps
                     (emit-mov-mem-reg buf 'rbp ps (+ +frame-slot-base+ (* idx -8)))
                     (progn
                       (emit-push buf 'rax)
                       (emit-load-vreg buf vs 'rax)
                       (emit-mov-mem-reg buf 'rbp 'rax (+ +frame-slot-base+ (* idx -8)))
                       (emit-pop buf 'rax))))
               ;; Normal object slot store
               ;; Slot address = (Vobj - 9) + 16 + idx*8 = Vobj + (idx*8 + 7)
               (let ((offset (+ (* idx 8) -1 8))
                     (po (vreg-phys vobj))
                     (ps (vreg-phys vs)))
                 (cond
                   ((and po ps)
                    (emit-mov-mem-reg buf po ps offset))
                   ((and po (null ps))
                    (emit-push buf 'rax)
                    (emit-load-vreg buf vs 'rax)
                    (emit-mov-mem-reg buf po 'rax offset)
                    (emit-pop buf 'rax))
                   ((and (null po) ps)
                    (emit-push buf 'rax)
                    (emit-load-vreg buf vobj 'rax)
                    (emit-mov-mem-reg buf 'rax ps offset)
                    (emit-pop buf 'rax))
                   (t
                    (emit-push buf 'rax)
                    (emit-push buf 'r13)
                    (emit-load-vreg buf vobj 'rax)
                    (emit-load-vreg buf vs 'r13)
                    (emit-mov-mem-reg buf 'rax 'r13 offset)
                    (emit-pop buf 'r13)
                    (emit-pop buf 'rax)))))))

        ((op= +op-obj-tag+)
         ;; (obj-tag Vd Vs) — extract low 4 bits
         (let* ((vd (first operands))
                (vs (second operands))
                (d (dest-phys-or-scratch vd)))
           (emit-load-vreg buf vs d)
           (emit-and-reg-imm buf d #x0F)
           ;; Tag result as fixnum: SHL 1
           (emit-shl-reg-imm buf d 1)
           (maybe-store-scratch buf vd)))

        ((op= +op-obj-subtag+)
         ;; (obj-subtag Vd Vs) — extract subtag from header word
         ;; Header is at [Vs - 9] (untag object pointer).
         ;;
         ;; Tag-safety: only deref if Vs is actually a heap pointer.  Two
         ;; ways the deref can crash:
         ;;
         ;;   1. Vs's low nibble is not 9 (it's a fixnum, cons, immediate,
         ;;      or forward).  [Vs - 9] is then off by some random offset
         ;;      and may land in unmapped memory.
         ;;
         ;;   2. Vs IS T (= +t-value+ = #xDEAD1009) — low nibble 9 looks
         ;;      like a heap pointer, but T is an immediate.  [T - 9] =
         ;;      #xDEAD1000, one byte past the 4KB NIL-page mmap, and
         ;;      SIGSEGVs.  The crash surfaces from process-of-elimination
         ;;      cond chains in cl-types (cos/sin/exp/cosh/...) whose
         ;;      guards (fixnump/consp/null) don't catch T, and from
         ;;      rt-floatp called via rt-equal whenever a test returns T
         ;;      and expected something else.
         ;;
         ;; Result on tag mismatch: dest = 0 (subtag 0, falsifies any
         ;; specific-subtag comparison the caller does — e.g. (= subtag
         ;; #x32) returns NIL, (= subtag #x50) returns NIL, etc.).
         ;;
         ;; NIL = #xDEAD0001 has low nibble 1 (cons) so the nibble check
         ;; catches it; we only need the explicit T-check for T.
         (let* ((vd (first operands))
                (vs (second operands))
                (d (dest-phys-or-scratch vd)))
           (let* ((ps (vreg-phys vs))
                  (fail-label (make-label))
                  (done-label (make-label))
                  ;; tmp must be distinct from d.  d ∈ {V-phys-regs, rax};
                  ;; r13 isn't a V-phys-reg, so r13 is always disjoint
                  ;; except when d is rax — in that case use r13 ourselves.
                  (tmp (if (eq d 'rax) 'r13 'rax)))
             (emit-push buf tmp)
             ;; Load vs into tmp — preserves the source through clobbers.
             (if ps
                 (emit-mov-reg-reg buf tmp ps)
                 (emit-load-vreg buf vs tmp))
             ;; Tag check: low nibble of tmp == 9 ?  d is scratch for AND.
             (emit-mov-reg-reg buf d tmp)
             (emit-and-reg-imm buf d #x0F)
             (emit-cmp-reg-imm buf d 9)
             (emit-jcc buf :ne fail-label)
             ;; T-immediate check — T (#xDEAD1009) shares tag-9 with real
             ;; heap pointers but [T-9] is unmapped.  d still scratch.
             (emit-mov-reg-imm buf d #xDEAD1009)
             (emit-cmp-reg-reg buf tmp d)
             (emit-jcc buf :e fail-label)
             ;; Real heap object — extract subtag from header at [tmp-9].
             (emit-mov-reg-mem buf d tmp -9)
             (emit-and-reg-imm buf d #xFF)
             (emit-shl-reg-imm buf d 1)
             (emit-jmp buf done-label)
             (emit-label buf fail-label)
             ;; subtag = 0 — falsifies any caller's specific-subtag cmp.
             (emit-mov-reg-imm buf d 0)
             (emit-label buf done-label)
             (emit-pop buf tmp))
           (maybe-store-scratch buf vd)))

        ;; ============================================
        ;; Variable-Index Array Operations
        ;; ============================================
        ((op= +op-aref+)
         ;; (aref Vd Vobj Vidx) — variable-index array load
         ;; Element at [Vobj + Vidx*4 + 7]
         ;; (Vidx is tagged fixnum: real_idx*2, *4 gives real_idx*8)
         ;; Offset: (Vobj - 9) + 16 + idx*8 = Vobj + idx*8 + 7
         (let* ((vd (first operands))
                (vobj (second operands))
                (vidx (third operands))
                (d (dest-phys-or-scratch vd)))
           ;; Compute address in scratch: Vidx*4
           (let ((pidx (vreg-phys vidx)))
             (if pidx
                 (emit-mov-reg-reg buf +scratch-reg+ pidx)
                 (emit-load-vreg buf vidx +scratch-reg+)))
           (emit-shl-reg-imm buf +scratch-reg+ 2)
           ;; Add Vobj
           (let ((pobj (vreg-phys vobj)))
             (if pobj
                 (emit-add-reg-reg buf +scratch-reg+ pobj)
                 (progn
                   (emit-push buf 'r13)
                   (emit-load-vreg buf vobj 'r13)
                   (emit-add-reg-reg buf +scratch-reg+ 'r13)
                   (emit-pop buf 'r13))))
           ;; Load from [scratch + 7]
           (emit-mov-reg-mem buf d +scratch-reg+ 7)
           (maybe-store-scratch buf vd)))

        ((op= +op-aset+)
         ;; (aset Vobj Vidx Vs) — variable-index array store
         ;; Store Vs at [Vobj + Vidx*4 + 7]
         (let* ((vobj (first operands))
                (vidx (second operands))
                (vs (third operands)))
           ;; Compute address in scratch: Vidx*4
           (let ((pidx (vreg-phys vidx)))
             (if pidx
                 (emit-mov-reg-reg buf +scratch-reg+ pidx)
                 (emit-load-vreg buf vidx +scratch-reg+)))
           (emit-shl-reg-imm buf +scratch-reg+ 2)
           ;; Add Vobj
           (let ((pobj (vreg-phys vobj)))
             (if pobj
                 (emit-add-reg-reg buf +scratch-reg+ pobj)
                 (progn
                   (emit-push buf 'r13)
                   (emit-load-vreg buf vobj 'r13)
                   (emit-add-reg-reg buf +scratch-reg+ 'r13)
                   (emit-pop buf 'r13))))
           ;; Store Vs at [scratch + 7]
           (let ((ps (vreg-phys vs)))
             (if ps
                 (emit-mov-mem-reg buf +scratch-reg+ ps 7)
                 (progn
                   (emit-push buf 'r13)
                   (emit-load-vreg buf vs 'r13)
                   (emit-mov-mem-reg buf +scratch-reg+ 'r13 7)
                   (emit-pop buf 'r13))))))

        ((op= +op-array-len+)
         ;; (array-len Vd Vobj) — extract element count from header
         ;; Header at [Vobj - 9], count = header >> 8, tagged = count << 1.
         ;;
         ;; Same tag-safety hazard as +op-obj-subtag+: T (= #xDEAD1009)
         ;; passes the implicit tag-9 check by sharing the low nibble,
         ;; but T is an immediate and [T - 9] = #xDEAD1000 is one byte
         ;; past the NIL-page mmap → SIGSEGV.  Predicates that gate on
         ;; obj-subtag now succeed-but-return-zero for T (post-fix), but
         ;; %clos-instance-p / %gf-p then proceed to (>= (array-length x) 1)
         ;; which crashes here.  This was the residual layout-fragility
         ;; for the CLOS family at N=1 even after the obj-subtag fix.
         ;;
         ;; Fix mirror: tag-check, then T-check; on either fail, return
         ;; tagged fixnum 0 (no real array has length 0 normally; callers
         ;; like `(>= (array-length x) 1)' yield NIL on zero, which is
         ;; the correct "not an array of N+ slots" answer).
         (let* ((vd (first operands))
                (vobj (second operands))
                (d (dest-phys-or-scratch vd)))
           (let* ((po (vreg-phys vobj))
                  (fail-label (make-label))
                  (done-label (make-label))
                  ;; tmp must differ from d; r13 isn't a V-phys-reg.
                  (tmp (if (eq d 'rax) 'r13 'rax)))
             (emit-push buf tmp)
             (if po
                 (emit-mov-reg-reg buf tmp po)
                 (emit-load-vreg buf vobj tmp))
             ;; Tag check.
             (emit-mov-reg-reg buf d tmp)
             (emit-and-reg-imm buf d #x0F)
             (emit-cmp-reg-imm buf d 9)
             (emit-jcc buf :ne fail-label)
             ;; T-immediate check.
             (emit-mov-reg-imm buf d #xDEAD1009)
             (emit-cmp-reg-reg buf tmp d)
             (emit-jcc buf :e fail-label)
             ;; Real heap object — read header at [tmp-9], count = h >> 8, tag.
             (emit-mov-reg-mem buf d tmp -9)
             (emit-shr-reg-imm buf d 8)
             (emit-shl-reg-imm buf d 1)
             (emit-jmp buf done-label)
             (emit-label buf fail-label)
             (emit-mov-reg-imm buf d 0)
             (emit-label buf done-label)
             (emit-pop buf tmp))
           (maybe-store-scratch buf vd)))

        ;; ============================================
        ;; Raw Memory Operations
        ;; ============================================
        ((op= +op-load+)
         ;; (load Vd Vaddr width:imm8) — raw memory read
         ;; Width: 0=u8, 1=u16, 2=u32, 3=u64
         (let* ((vd (first operands))
                (vaddr (second operands))
                (width (third operands))
                (d (dest-phys-or-scratch vd)))
           ;; Get address into a temp
           (let ((pa (vreg-phys vaddr)))
             (unless pa
               (emit-load-vreg buf vaddr 'rax)
               (setf pa 'rax))
             (ecase width
               (0 ;; u8: MOVZX r64, byte [addr]
                ;; REX.W 0F B6 /r (ModRM: [reg])
                (emit-movzx-byte buf d pa))
               (1 ;; u16: MOVZX r64, word [addr]
                (emit-movzx-word buf d pa))
               (2 ;; u32: MOV r32, [addr] (zero-extends to 64)
                (emit-mov-reg32-mem buf d pa))
               (3 ;; u64: MOV r64, [addr]
                (emit-mov-reg-mem buf d pa 0))))
           (maybe-store-scratch buf vd)))

        ((op= +op-store+)
         ;; (store Vaddr Vs width:imm8) — raw memory write
         (let* ((vaddr (first operands))
                (vs (second operands))
                (width (third operands))
                (pa (vreg-phys vaddr))
                (ps (vreg-phys vs)))
           ;; Need address in one register, value in another
           (unless pa
             (emit-push buf 'rax)
             (emit-load-vreg buf vaddr 'rax)
             (setf pa 'rax))
           (unless ps
             (emit-push buf 'r13)
             (emit-load-vreg buf vs 'r13)
             (setf ps 'r13))
           (ecase width
             (0 (emit-mov-mem-byte buf pa ps))
             (1 (emit-mov-mem-word buf pa ps))
             (2 (emit-mov-mem-dword buf pa ps))
             (3 (emit-mov-mem-reg buf pa ps 0)))
           ;; Restore temps if we pushed them
           (when (vreg-spills-p vs) (emit-pop buf 'r13))
           (when (vreg-spills-p vaddr) (emit-pop buf 'rax))))

        ((op= +op-fence+)
         ;; MFENCE: 0F AE F0
         (emit-bytes buf #x0F #xAE #xF0))

        ;; ============================================
        ;; Function Calling
        ;; ============================================
        ((op= +op-call+)
         ;; (call target:imm32)
         ;; Target operand is the bytecode offset of the called function.
         (let* ((target-offset (first operands))
                (fn-table (translate-state-function-table state))
                (label (when fn-table (gethash target-offset fn-table))))
           (if label
               (emit-call buf label)
               ;; Unknown target — emit CALL rel32 with placeholder
               (emit-call buf (make-label)))))

        ((op= +op-call-ind+)
         ;; (call-ind Vs) — indirect call through register.
         ;; Vs holds a tagged function pointer (tag = 3, see TAG-PLAN.md).
         ;; Strip the tag with SUB reg, 3 before the indirect call.
         ;; If Vs was untagged (e.g. came from an obj-ref slot that
         ;; stored a raw addr — closure slot-0 should now be tagged
         ;; too) the call would land 3 bytes before the function and
         ;; almost certainly fault, which is fine — it's a type error
         ;; the caller has to fix.
         (let* ((vs (first operands))
                (ps (vreg-phys vs))
                (call-reg (or ps +scratch-reg+)))
           (unless ps
             (emit-load-vreg buf vs +scratch-reg+))
           (emit-sub-reg-imm buf call-reg 3)
           (emit-call-reg buf call-reg)))

        ((op= +op-ret+)
         ;; Return: restore RBX, tear down frame and return
         (emit-mov-reg-mem buf 'rbx 'rbp -8)
         (emit-mov-reg-reg buf 'rsp 'rbp)
         (emit-pop buf 'rbp)
         (emit-ret buf))

        ((op= +op-tailcall+)
         ;; (tailcall target:imm32) — tear down frame and jump
         ;; Target operand is the bytecode offset of the called function.
         (let* ((target-offset (first operands))
                (fn-table (translate-state-function-table state))
                (label (when fn-table (gethash target-offset fn-table))))
           ;; Restore RBX, tear down frame
           (emit-mov-reg-mem buf 'rbx 'rbp -8)
           (emit-mov-reg-reg buf 'rsp 'rbp)
           (emit-pop buf 'rbp)
           ;; Jump instead of call
           (if label
               (emit-jmp buf label)
               (emit-jmp buf (make-label)))))

        ;; ============================================
        ;; GC and Allocation
        ;; ============================================
        ((op= +op-alloc-cons+)
         ;; (alloc-cons Vd) — bump-allocate cons cell, tag as cons
         ;; Result = R12 | 1, R12 += 16
         (let* ((vd (first operands))
                (d (dest-phys-or-scratch vd)))
           ;; MCGC object-start bit (R12 = cons base before advance).
           (emit-mcgc-set-start-bit buf 'r12)
           ;; ...and the CONS-KIND bit (this start IS a cons) — out-of-line
           ;; CALL (5B) to the shared setter, which reads R12.
           (emit-mcgc-call-cons-bit buf)
           (emit-mov-reg-reg buf d 'r12)
           (emit-or-reg-imm buf d 1)      ; tag as cons
           (emit-add-reg-imm buf 'r12 16)  ; advance alloc pointer
           (maybe-store-scratch buf vd)))

        ((op= +op-gc-check+)
         ;; Check R12 (alloc ptr) against R14 (alloc limit)
         ;; If R12 >= R14, call GC (or NOP if no GC configured).
         ;; Under pinning the call routes to the page-GC trampoline (which
         ;; refills R12/R14 from the rebuilt free-list); otherwise the legacy
         ;; Cheney trampoline.  Same post-write semantics: the <= GUARD
         ;; overshoot for small objects lands in free pages of the same run.
         (let* ((page-lbl (and (mcgc-pinning-on-p) (mcgc-page-gc-label)))
                (gc-lbl (or page-lbl (translate-state-gc-label state))))
           (when gc-lbl
             (let ((skip-label (make-label)))
               (emit-cmp-reg-reg buf 'r12 'r14)
               (emit-jcc buf :l skip-label)    ; if alloc < limit, skip
               (emit-call buf gc-lbl)
               (emit-label buf skip-label)))))

        ((op= +op-mcgc-collect+)
         ;; (%mcgc-collect) — force a full page collection UNCONDITIONALLY when
         ;; pinning is on (the gc-check is R12<R14-gated; an explicit collect
         ;; must run even with room left, so the pin-stress probe can force GCs
         ;; with pinned objects live).  When pinning is OFF this is a no-op
         ;; (flag-off stays byte-identical: page-lbl is nil).
         (let ((page-lbl (and (mcgc-pinning-on-p) (mcgc-page-gc-label))))
           (if page-lbl
               (emit-call buf page-lbl)
               (emit-nop buf))))

        ((op= +op-write-barrier+)
         ;; (write-barrier Vobj) — mark card table dirty
         ;; For now emit a stub: the card table is addressed by
         ;; shifting the object address right by page bits and
         ;; writing a dirty byte.  This is a placeholder that the
         ;; runtime GC will configure.
         (let* ((vobj (first operands))
                (po (vreg-phys vobj)))
           (unless po
             (emit-load-vreg buf vobj 'rax)
             (setf po 'rax))
           ;; SHR po, 12 (page bits); then write 1 to card table
           ;; This is a stub — actual implementation depends on the
           ;; GC card table base address.
           (emit-nop buf)))

        ;; ============================================
        ;; SAP (System Area Pointer)
        ;; ============================================
        ;; SAP object layout: [header:8][raw-address:8] = 16 bytes
        ;; Tagged SAP pointer: (raw_obj_addr | 0x09)
        ;; To get raw address: strip tag (AND -16, SHL to byte addr), load [+8]

        ((op= +op-sap-new+)
         ;; (sap-new Vd Vaddr) - allocate SAP, store raw address from Vaddr
         ;; Vaddr contains a raw u64 address (from :u64 load or syscall result)
         (let* ((vd (first operands))
                (vaddr (second operands))
                (d (dest-phys-or-scratch vd))
                (pa (vreg-phys vaddr)))
           ;; Write header at R12: (1 << 8) | 0x16 = 0x116
           (emit-mov-reg-imm buf +scratch-reg+ #x116)
           (emit-mov-mem-reg buf 'r12 +scratch-reg+ 0)
           ;; Write raw address at R12+8
           (unless pa
             (emit-load-vreg buf vaddr 'rax)
             (setf pa 'rax))
           (emit-mov-mem-reg buf 'r12 pa 8)
           ;; MCGC object-start bit (R12 still = SAP base).
           (emit-mcgc-set-start-bit buf 'r12)
           ;; Result = R12 | 0x09 (object tag)
           (emit-lea buf d 'r12 #x09)
           ;; Advance alloc pointer by 16 (header + 1 data word, already 16-aligned)
           (emit-add-reg-imm buf 'r12 16)
           (maybe-store-scratch buf vd)))

        ((op= +op-sap-ref8+)
         ;; (sap-ref8 Vd Vsap Voff) - load u8 at sap.addr + off → tagged fixnum
         (let* ((vd (first operands))
                (vsap (second operands))
                (voff (third operands))
                (d (dest-phys-or-scratch vd))
                (ps (vreg-phys vsap)))
           ;; Extract raw address from SAP: strip tag, load [obj+8]
           (unless ps (emit-load-vreg buf vsap +scratch-reg+) (setf ps +scratch-reg+))
           ;; AND with -16 strips object tag bits, then *2 gives byte addr (ASH 1)
           ;; But simpler: (sap & ~0xF) gives raw obj base (since tag is in low 4 bits)
           ;; Then raw_obj_byte_addr = (sap & ~0xF) * 2... no, tagged ptr is already
           ;; a byte address with tag in low bits on 64-bit.
           ;; Actually: object tag is 0x09. Raw byte addr = tagged_ptr - 9.
           ;; Then raw_address = mem[raw_byte_addr + 8]
           (emit-lea buf 'rax ps -9)         ; rax = raw object base
           (emit-mov-reg-mem buf 'rax 'rax 8) ; rax = raw address from SAP
           ;; Add offset (untag: SAR 1)
           (let ((po (vreg-phys voff)))
             (unless po (emit-load-vreg buf voff 'rcx) (setf po 'rcx))
             ;; offset is tagged fixnum, SAR 1 to get byte offset
             ;; rax + (po >> 1) → effective address
             (emit-mov-reg-reg buf 'rcx po)
             (emit-sar-reg-imm buf 'rcx 1)
             (emit-add-reg-reg buf 'rax 'rcx))
           ;; Load u8, zero-extend, tag as fixnum (SHL 1)
           (emit-bytes buf #x0F #xB6 #x00)   ; movzx eax, byte [rax]
           (emit-bytes buf #x48 #x01 #xC0)    ; add rax, rax (tag)
           (emit-mov-reg-reg buf d 'rax)
           (maybe-store-scratch buf vd)))

        ((op= +op-sap-ref32+)
         ;; (sap-ref32 Vd Vsap Voff) - load u32 → tagged fixnum
         (let* ((vd (first operands))
                (vsap (second operands))
                (voff (third operands))
                (d (dest-phys-or-scratch vd))
                (ps (vreg-phys vsap)))
           (unless ps (emit-load-vreg buf vsap +scratch-reg+) (setf ps +scratch-reg+))
           (emit-lea buf 'rax ps -9)
           (emit-mov-reg-mem buf 'rax 'rax 8)
           (let ((po (vreg-phys voff)))
             (unless po (emit-load-vreg buf voff 'rcx) (setf po 'rcx))
             (emit-mov-reg-reg buf 'rcx po)
             (emit-sar-reg-imm buf 'rcx 1)
             (emit-add-reg-reg buf 'rax 'rcx))
           ;; Load u32, zero-extend to 64-bit, tag as fixnum
           (emit-bytes buf #x8B #x00)         ; mov eax, [rax] (32-bit, zero-extends)
           (emit-bytes buf #x48 #x01 #xC0)    ; add rax, rax (tag)
           (emit-mov-reg-reg buf d 'rax)
           (maybe-store-scratch buf vd)))

        ((op= +op-sap-ref64+)
         ;; (sap-ref64 Vd Vsap Voff) - load raw u64 (NOT tagged)
         (let* ((vd (first operands))
                (vsap (second operands))
                (voff (third operands))
                (d (dest-phys-or-scratch vd))
                (ps (vreg-phys vsap)))
           (unless ps (emit-load-vreg buf vsap +scratch-reg+) (setf ps +scratch-reg+))
           (emit-lea buf 'rax ps -9)
           (emit-mov-reg-mem buf 'rax 'rax 8)
           (let ((po (vreg-phys voff)))
             (unless po (emit-load-vreg buf voff 'rcx) (setf po 'rcx))
             (emit-mov-reg-reg buf 'rcx po)
             (emit-sar-reg-imm buf 'rcx 1)
             (emit-add-reg-reg buf 'rax 'rcx))
           ;; Load raw u64
           (emit-bytes buf #x48 #x8B #x00)    ; mov rax, [rax]
           (emit-mov-reg-reg buf d 'rax)
           (maybe-store-scratch buf vd)))

        ((op= +op-sap-set8+)
         ;; (sap-set8 Vsap Voff Vval) - store byte at sap.addr + off
         (let* ((vsap (first operands))
                (voff (second operands))
                (vval (third operands))
                (ps (vreg-phys vsap)))
           (unless ps (emit-load-vreg buf vsap +scratch-reg+) (setf ps +scratch-reg+))
           (emit-lea buf 'rax ps -9)
           (emit-mov-reg-mem buf 'rax 'rax 8)
           (let ((po (vreg-phys voff)))
             (unless po (emit-load-vreg buf voff 'rcx) (setf po 'rcx))
             (emit-mov-reg-reg buf 'rcx po)
             (emit-sar-reg-imm buf 'rcx 1)
             (emit-add-reg-reg buf 'rax 'rcx))
           ;; Value: untag (SAR 1), store byte
           (let ((pv (vreg-phys vval)))
             (unless pv (emit-load-vreg buf vval 'rdx) (setf pv 'rdx))
             (emit-mov-reg-reg buf 'rdx pv)
             (emit-sar-reg-imm buf 'rdx 1)
             ;; mov [rax], dl
             (emit-bytes buf #x88 #x10))))

        ((op= +op-sap-set32+)
         ;; (sap-set32 Vsap Voff Vval) - store u32 at sap.addr + off
         (let* ((vsap (first operands))
                (voff (second operands))
                (vval (third operands))
                (ps (vreg-phys vsap)))
           (unless ps (emit-load-vreg buf vsap +scratch-reg+) (setf ps +scratch-reg+))
           (emit-lea buf 'rax ps -9)
           (emit-mov-reg-mem buf 'rax 'rax 8)
           (let ((po (vreg-phys voff)))
             (unless po (emit-load-vreg buf voff 'rcx) (setf po 'rcx))
             (emit-mov-reg-reg buf 'rcx po)
             (emit-sar-reg-imm buf 'rcx 1)
             (emit-add-reg-reg buf 'rax 'rcx))
           (let ((pv (vreg-phys vval)))
             (unless pv (emit-load-vreg buf vval 'rdx) (setf pv 'rdx))
             (emit-mov-reg-reg buf 'rdx pv)
             (emit-sar-reg-imm buf 'rdx 1)
             ;; mov [rax], edx
             (emit-bytes buf #x89 #x10))))

        ((op= +op-sap-set64+)
         ;; (sap-set64 Vsap Voff Vval) - store raw u64 at sap.addr + off
         (let* ((vsap (first operands))
                (voff (second operands))
                (vval (third operands))
                (ps (vreg-phys vsap)))
           (unless ps (emit-load-vreg buf vsap +scratch-reg+) (setf ps +scratch-reg+))
           (emit-lea buf 'rax ps -9)
           (emit-mov-reg-mem buf 'rax 'rax 8)
           (let ((po (vreg-phys voff)))
             (unless po (emit-load-vreg buf voff 'rcx) (setf po 'rcx))
             (emit-mov-reg-reg buf 'rcx po)
             (emit-sar-reg-imm buf 'rcx 1)
             (emit-add-reg-reg buf 'rax 'rcx))
           (let ((pv (vreg-phys vval)))
             (unless pv (emit-load-vreg buf vval 'rdx) (setf pv 'rdx))
             ;; Store raw u64 (no untag)
             ;; mov [rax], rdx
             (emit-bytes buf #x48 #x89 #x10))))

        ((op= +op-sap-addr+)
         ;; (sap-addr Vd Vsap) - extract address from SAP → tagged fixnum
         ;; Result is tagged (SHL 1) so it can be used as a normal Lisp value
         ;; and passed to syscall3 (which untags all args).
         (let* ((vd (first operands))
                (vsap (second operands))
                (d (dest-phys-or-scratch vd))
                (ps (vreg-phys vsap)))
           (unless ps (emit-load-vreg buf vsap +scratch-reg+) (setf ps +scratch-reg+))
           (emit-lea buf 'rax ps -9)          ; strip object tag
           (emit-mov-reg-mem buf 'rax 'rax 8) ; load raw address
           (emit-bytes buf #x48 #x01 #xC0)    ; add rax, rax (tag as fixnum)
           (emit-mov-reg-reg buf d 'rax)
           (maybe-store-scratch buf vd)))

        ;; ============================================
        ;; Actor / Concurrency
        ;; ============================================
        ((op= +op-save-ctx+)
         ;; Save all callee-saved registers to the stack
         ;; Used when suspending an actor
         (emit-push buf 'rbx)
         (emit-push buf 'r12)
         (emit-push buf 'r13)
         (emit-push buf 'r14)
         (emit-push buf 'r15))

        ((op= +op-restore-ctx+)
         ;; Restore callee-saved registers
         (emit-pop buf 'r15)
         (emit-pop buf 'r14)
         (emit-pop buf 'r13)
         (emit-pop buf 'r12)
         (emit-pop buf 'rbx))

        ((op= +op-yield+)
         ;; Preemption check.  LINUX: NOP (no scheduler, no deadline).
         ;; BARE-METAL: safepoint for the PIT deadline — when the ISR has
         ;; set the pending flag [0x10000D30], call the shared stub that
         ;; consumes it and longjmps through the innermost armed
         ;; handler-case.  YIELD sits at every compiled loop back-edge
         ;; (compiler.lisp emits it per iteration), so hung tests are
         ;; recovered at an instruction boundary where no intern/alloc/GC
         ;; critical section is mid-flight — the ISR-side longjmp used to
         ;; abandon half-written global state (see emit-yield-longjmp-stub).
         (if (or *x64-linux-mode*
                 (null (translate-state-yield-longjmp-label state)))
             (emit-nop buf)
             (let ((skip (make-label)))
               ;; cmp qword [0x10000D30], 0
               (emit-bytes buf #x48 #x83 #x3C #x25)
               (emit-u32 buf #x10000D30)
               (emit-bytes buf #x00)
               (emit-jcc buf :e skip)
               (emit-call buf (translate-state-yield-longjmp-label state))
               (emit-label buf skip))))

        ((op= +op-set-mv-count+)
         ;; Store tagged fixnum count to MV-COUNT address.
         ;; imm8 operand is the raw count (e.g., 1).
         ;; Tagged value = count << 1 (fixnum shift).
         ;; Emit: mov qword [MV_COUNT_ADDR], imm32
         ;; REX.W MOV r/m64, imm32 = 48 C7 05 disp32 imm32  (RIP-relative)
         ;; But we use absolute addressing: MOV [abs], imm isn't available
         ;; on x64. Use: MOV RAX, imm64; MOV [RAX], imm32 pattern.
         ;; Actually simpler: use MOV r/m64,imm32 with SIB=none, disp32
         ;; For absolute address we need: REX.W C7 04 25 addr32 imm32
         ;; Store count as tagged fixnum (count << 1) to match compile-values.
         ;; compile-values does (setf (mem-ref addr :u64) nvals) where nvals
         ;; is compiled as tagged, so the raw bits are nvals<<1.
         ;; set-mv-count must match: store count<<1.
         (let* ((count (first operands))
                (tagged (ash count 1))  ; fixnum shift to match compile-values
                (addr #x10000090))      ; MV-COUNT-ADDR
           ;; MOV qword [addr32], imm32 (sign-extended)
           ;; 48 C7 04 25 <addr32-le> <imm32-le>
           (emit-bytes buf #x48 #xC7 #x04 #x25)
           (emit-u32 buf addr)
           (emit-u32 buf tagged)))

        ((op= +op-atomic-xchg+)
         ;; (atomic-xchg Vd Vaddr Vs) — LOCK XCHG [Vaddr], Vs → Vd
         ;; x86 XCHG with memory is implicitly locked.
         (let* ((vd (first operands))
                (vaddr (second operands))
                (vs (third operands)))
           ;; Load Vs into RAX
           (emit-load-vreg buf vs 'rax)
           ;; Get address into a temp
           (let ((pa (vreg-phys vaddr)))
             (unless pa
               (emit-push buf 'r13)
               (emit-load-vreg buf vaddr 'r13)
               (setf pa 'r13))
             ;; XCHG [pa], RAX
             ;; REX.W 87 /r (ModRM mod=00 for [reg])
             (emit-xchg-mem-reg buf pa 'rax)
             (when (vreg-spills-p vaddr)
               (emit-pop buf 'r13)))
           ;; Result (old value) is in RAX
           (emit-store-vreg buf vd 'rax)))

        ;; ============================================
        ;; I/O Port Operations
        ;; ============================================
        ((op= +op-io-read+)
         ;; (io-read Vd port:imm16 width:imm8)
         ;; IN AL/AX/EAX, DX
         (let* ((vd (first operands))
                (port (second operands))
                (width (third operands))
                (d (dest-phys-or-scratch vd)))
           ;; Load port number into DX
           (emit-mov-reg-imm buf 'edx port)
           ;; IN instruction
           (ecase width
             (0 ;; byte: IN AL, DX
              (emit-bytes buf #xEC)
              ;; Zero-extend AL to RAX
              (emit-bytes buf #x48 #x0F #xB6 #xC0)) ; movzx rax, al
             (1 ;; word: IN AX, DX
              (emit-bytes buf #x66 #xED)
              ;; Zero-extend AX to RAX
              (emit-bytes buf #x48 #x0F #xB7 #xC0)) ; movzx rax, ax
             (2 ;; dword: IN EAX, DX (auto zero-extends to RAX)
              (emit-bytes buf #xED)))
           ;; Tag as fixnum: SHL RAX, 1
           (emit-shl-reg-imm buf 'rax 1)
           ;; Move result to destination
           (unless (eq d 'rax)
             (emit-mov-reg-reg buf d 'rax))
           (maybe-store-scratch buf vd)))

        ((op= +op-io-write+)
         ;; (io-write port:imm16 Vs width:imm8)
         ;; OUT DX, AL/AX/EAX
         (let* ((port (first operands))
                (vs (second operands))
                (width (third operands)))
           ;; Load port into DX
           (emit-mov-reg-imm buf 'edx port)
           ;; Load value into RAX, untag
           (emit-load-vreg buf vs 'rax)
           (emit-sar-reg-imm buf 'rax 1)
           ;; OUT instruction
           (ecase width
             (0 (emit-bytes buf #xEE))       ; OUT DX, AL
             (1 (emit-bytes buf #x66 #xEF))  ; OUT DX, AX
             (2 (emit-bytes buf #xEF)))))     ; OUT DX, EAX

        ((op= +op-halt+)
         ;; HLT: F4
         (emit-bytes buf #xF4))

        ((op= +op-cli+)
         ;; CLI: FA
         (emit-bytes buf #xFA))

        ((op= +op-sti+)
         ;; STI: FB
         (emit-bytes buf #xFB))

        ;; ============================================
        ;; Per-CPU Data
        ;; ============================================
        ((op= +op-percpu-ref+)
         ;; (percpu-ref Vd offset:imm16)
         ;; Read from GS segment: MOV reg, GS:[offset]
         ;; Prefix 65, REX.W 8B /05 disp32
         (let* ((vd (first operands))
                (offset (second operands))
                (d (dest-phys-or-scratch vd)))
           ;; GS prefix + MOV r64, [disp32]
           ;; 65 REX.W 8B /05 disp32  (RIP-relative, but we use absolute)
           ;; Actually for GS:[disp32] with no base: 65 REX.W 8B 04 25 disp32
           (emit-byte buf #x65)                   ; GS prefix
           (emit-byte buf (rex-prefix t (reg-extended-p d) nil nil))
           (emit-byte buf #x8B)                    ; MOV r64, r/m64
           ;; ModRM: mod=00 r/m=100 (SIB follows), reg=d
           (emit-byte buf (modrm #b00 (reg-code d) 4))
           ;; SIB: scale=00 index=100(none) base=101(disp32)
           (emit-byte buf #x25)
           (emit-u32 buf offset)
           (maybe-store-scratch buf vd)))

        ((op= +op-percpu-set+)
         ;; (percpu-set offset:imm16 Vs)
         ;; Write to GS segment: MOV GS:[offset], reg
         (let* ((offset (first operands))
                (vs (second operands))
                (ps (vreg-phys vs)))
           (unless ps
             (emit-load-vreg buf vs 'rax)
             (setf ps 'rax))
           ;; GS prefix + MOV [disp32], r64
           (emit-byte buf #x65)                    ; GS prefix
           (emit-byte buf (rex-prefix t (reg-extended-p ps) nil nil))
           (emit-byte buf #x89)                    ; MOV r/m64, r64
           (emit-byte buf (modrm #b00 (reg-code ps) 4))
           (emit-byte buf #x25)                    ; SIB for disp32
           (emit-u32 buf offset)))

        ;; ============================================
        ;; Function Address (for indirect calls)
        ;; ============================================
        ((op= +op-fn-addr+)
         ;; (fn-addr Vd target:imm32)
         ;; Load the native address of a function into Vd, tagged with
         ;; +tag-function+ (= 3) so funcall dispatch and FUNCTIONP can
         ;; identify it without ambiguity vs cons (tag 1) or object
         ;; (tag 9).  CALL-IND strips the tag before the indirect call.
         ;; Target is the bytecode offset, resolved via function table
         ;; to a native label. Uses LEA [RIP+disp32] for position-independent
         ;; address loading.
         (let* ((vd (first operands))
                (target-offset (second operands))
                (fn-table (translate-state-function-table state))
                (label (when fn-table (gethash target-offset fn-table)))
                (d (dest-phys-or-scratch vd)))
           (if label
               (progn
                 (emit-lea-label buf d label)
                 ;; OR with 3 to tag (cf. TAG-PLAN.md).  Function code
                 ;; is 16-byte (or better) aligned by NOP-padding so
                 ;; the low nibble is 0 — OR-3 gives a clean tag value.
                 (emit-or-reg-imm buf d 3))
               ;; Unknown target (the compiler's #xFFFFFFF0 unresolved-name
               ;; sentinel) — load NIL so funcall's NIL-guard signals
               ;; UNDEFINED-FUNCTION.  The old `0' was a live but
               ;; misaligned code-base pointer: CALL-IND's -3 tag strip
               ;; landed in boot-stub padding and executed whatever bytes
               ;; the boot immediates happened to be (the CHUNK-CRASH
               ;; regression class — see compiler.lisp :li-func).
               (emit-mov-reg-imm buf d #xDEAD0001))
           (maybe-store-scratch buf vd)))

        ;; ============================================
        ;; Unknown Opcode
        ;; ============================================
        (t
         ;; Emit a trap for unrecognised MVM instructions
         (emit-int buf #x30)
         (emit-byte buf opcode))))))

;;; ============================================================
;;; x86-64 Encoding Helpers
;;; ============================================================

(defun rex-prefix (w r x b)
  "Build a REX prefix byte.  W=64-bit operand, R=ModRM reg ext,
   X=SIB index ext, B=ModRM r/m or SIB base ext.
   Each argument is a generalized boolean."
  (logior #x40
          (if w 8 0)
          (if r 4 0)
          (if x 2 0)
          (if b 1 0)))

(defun modrm (mod reg rm)
  "Build a ModR/M byte.  MOD=2-bit, REG=3-bit, RM=3-bit.
   REG and RM should already be masked to low 3 bits."
  (logior (ash (logand mod #b11) 6)
          (ash (logand reg #b111) 3)
          (logand rm #b111)))

;;; ============================================================
;;; Additional x86-64 Instruction Emitters
;;; ============================================================
;;;
;;; These instructions are not in x64-asm.lisp but are needed by
;;; the translator.  They emit raw machine code bytes.

(defun emit-imul-reg-reg (buf dst src)
  "IMUL dst, src (two-operand signed multiply).
   REX.W + 0F AF /r"
  (let ((w t)
        (r (reg-extended-p dst))
        (b (reg-extended-p src)))
    (emit-byte buf (rex-prefix w r nil b))
    (emit-bytes buf #x0F #xAF)
    (emit-byte buf (modrm #b11 (reg-code dst) (reg-code src)))))

(defun emit-mul-reg (buf src)
  "MUL src — unsigned multiply RDX:RAX = RAX * src (one-operand form).
   REX.W + F7 /4"
  (emit-byte buf (rex-prefix t nil nil (reg-extended-p src)))
  (emit-byte buf #xF7)
  (emit-byte buf (modrm #b11 4 (reg-code src))))

(defun emit-add-mem-reg (buf base src offset)
  "ADD [base+offset], src — add register to memory (sets carry flag).
   REX.W + 01 /r [ModRM + disp]"
  (let ((r (reg-extended-p src))
        (b (reg-extended-p base)))
    (emit-byte buf (rex-prefix t r nil b))
    (emit-byte buf #x01)
    (let ((needs-sib (= (logand (reg-code base) 7) 4)))
      (cond
        ((zerop offset)
         (cond
           ((= (logand (reg-code base) 7) 5) ; RBP/R13 need disp8
            (emit-byte buf (modrm #b01 (reg-code src) (reg-code base)))
            (when needs-sib (emit-byte buf #x24))
            (emit-byte buf 0))
           (needs-sib
            (emit-byte buf (modrm #b00 (reg-code src) 4))
            (emit-byte buf #x24))
           (t
            (emit-byte buf (modrm #b00 (reg-code src) (reg-code base))))))
        ((<= -128 offset 127)
         (emit-byte buf (modrm #b01 (reg-code src) (if needs-sib 4 (reg-code base))))
         (when needs-sib (emit-byte buf #x24))
         (emit-byte buf (logand offset #xFF)))
        (t
         (emit-byte buf (modrm #b10 (reg-code src) (if needs-sib 4 (reg-code base))))
         (when needs-sib (emit-byte buf #x24))
         (emit-s32 buf offset))))))

(defun emit-adc-mem-reg (buf base src offset)
  "ADC [base+offset], src — add with carry register to memory.
   REX.W + 11 /r [ModRM + disp]"
  (let ((r (reg-extended-p src))
        (b (reg-extended-p base)))
    (emit-byte buf (rex-prefix t r nil b))
    (emit-byte buf #x11)
    (let ((needs-sib (= (logand (reg-code base) 7) 4)))
      (cond
        ((zerop offset)
         (cond
           ((= (logand (reg-code base) 7) 5)
            (emit-byte buf (modrm #b01 (reg-code src) (reg-code base)))
            (when needs-sib (emit-byte buf #x24))
            (emit-byte buf 0))
           (needs-sib
            (emit-byte buf (modrm #b00 (reg-code src) 4))
            (emit-byte buf #x24))
           (t
            (emit-byte buf (modrm #b00 (reg-code src) (reg-code base))))))
        ((<= -128 offset 127)
         (emit-byte buf (modrm #b01 (reg-code src) (if needs-sib 4 (reg-code base))))
         (when needs-sib (emit-byte buf #x24))
         (emit-byte buf (logand offset #xFF)))
        (t
         (emit-byte buf (modrm #b10 (reg-code src) (if needs-sib 4 (reg-code base))))
         (when needs-sib (emit-byte buf #x24))
         (emit-s32 buf offset))))))

(defun emit-neg-reg (buf reg)
  "NEG reg (two's complement negate).
   REX.W + F7 /3"
  (emit-byte buf (rex-prefix t nil nil (reg-extended-p reg)))
  (emit-byte buf #xF7)
  (emit-byte buf (modrm #b11 3 (reg-code reg))))

(defun emit-movzx-byte (buf dst base)
  "MOVZX dst, BYTE [base] — zero-extend byte to 64 bits.
   REX.W + 0F B6 /r (ModRM for [base])"
  (let ((r (reg-extended-p dst))
        (b (reg-extended-p base)))
    (emit-byte buf (rex-prefix t r nil b))
    (emit-bytes buf #x0F #xB6)
    ;; ModRM: mod=00, reg=dst, r/m=base
    (let ((needs-sib (= (logand (reg-code base) 7) 4)))
      (cond
        ((= (reg-code base) 5) ; RBP/R13 need disp8
         (emit-byte buf (modrm #b01 (reg-code dst) (reg-code base)))
         (emit-byte buf 0))
        (needs-sib
         (emit-byte buf (modrm #b00 (reg-code dst) 4))
         (emit-byte buf #x24))
        (t
         (emit-byte buf (modrm #b00 (reg-code dst) (reg-code base))))))))

(defun emit-movzx-word (buf dst base)
  "MOVZX dst, WORD [base] — zero-extend word to 64 bits.
   REX.W + 0F B7 /r"
  (let ((r (reg-extended-p dst))
        (b (reg-extended-p base)))
    (emit-byte buf (rex-prefix t r nil b))
    (emit-bytes buf #x0F #xB7)
    (let ((needs-sib (= (logand (reg-code base) 7) 4)))
      (cond
        ((= (reg-code base) 5)
         (emit-byte buf (modrm #b01 (reg-code dst) (reg-code base)))
         (emit-byte buf 0))
        (needs-sib
         (emit-byte buf (modrm #b00 (reg-code dst) 4))
         (emit-byte buf #x24))
        (t
         (emit-byte buf (modrm #b00 (reg-code dst) (reg-code base))))))))

(defun emit-mov-reg32-mem (buf dst base)
  "MOV dst32, [base] — 32-bit load (zero-extends to 64).
   No REX.W prefix (use 32-bit operand size).
   8B /r"
  (let ((r (reg-extended-p dst))
        (b (reg-extended-p base)))
    ;; REX needed only for extended registers, no W bit
    (when (or r b)
      (emit-byte buf (rex-prefix nil r nil b)))
    (emit-byte buf #x8B)
    (let ((needs-sib (= (logand (reg-code base) 7) 4)))
      (cond
        ((= (reg-code base) 5)
         (emit-byte buf (modrm #b01 (reg-code dst) (reg-code base)))
         (emit-byte buf 0))
        (needs-sib
         (emit-byte buf (modrm #b00 (reg-code dst) 4))
         (emit-byte buf #x24))
        (t
         (emit-byte buf (modrm #b00 (reg-code dst) (reg-code base))))))))

(defun emit-mov-mem-byte (buf base src)
  "MOV BYTE [base], src-low-byte.
   REX + 88 /r"
  (let ((r (reg-extended-p src))
        (b (reg-extended-p base)))
    ;; Need REX for SPL/BPL/SIL/DIL or extended regs
    (emit-byte buf (rex-prefix nil r nil b))
    (emit-byte buf #x88)
    (let ((needs-sib (= (logand (reg-code base) 7) 4)))
      (cond
        ((= (reg-code base) 5)
         (emit-byte buf (modrm #b01 (reg-code src) (reg-code base)))
         (emit-byte buf 0))
        (needs-sib
         (emit-byte buf (modrm #b00 (reg-code src) 4))
         (emit-byte buf #x24))
        (t
         (emit-byte buf (modrm #b00 (reg-code src) (reg-code base))))))))

(defun emit-mov-mem-word (buf base src)
  "MOV WORD [base], src — 16-bit store.
   66 prefix + 89 /r"
  (let ((r (reg-extended-p src))
        (b (reg-extended-p base)))
    (emit-byte buf #x66) ; operand size override
    (when (or r b)
      (emit-byte buf (rex-prefix nil r nil b)))
    (emit-byte buf #x89)
    (let ((needs-sib (= (logand (reg-code base) 7) 4)))
      (cond
        ((= (reg-code base) 5)
         (emit-byte buf (modrm #b01 (reg-code src) (reg-code base)))
         (emit-byte buf 0))
        (needs-sib
         (emit-byte buf (modrm #b00 (reg-code src) 4))
         (emit-byte buf #x24))
        (t
         (emit-byte buf (modrm #b00 (reg-code src) (reg-code base))))))))

(defun emit-mov-mem-dword (buf base src)
  "MOV DWORD [base], src — 32-bit store.
   89 /r (no REX.W)"
  (let ((r (reg-extended-p src))
        (b (reg-extended-p base)))
    (when (or r b)
      (emit-byte buf (rex-prefix nil r nil b)))
    (emit-byte buf #x89)
    (let ((needs-sib (= (logand (reg-code base) 7) 4)))
      (cond
        ((= (reg-code base) 5)
         (emit-byte buf (modrm #b01 (reg-code src) (reg-code base)))
         (emit-byte buf 0))
        (needs-sib
         (emit-byte buf (modrm #b00 (reg-code src) 4))
         (emit-byte buf #x24))
        (t
         (emit-byte buf (modrm #b00 (reg-code src) (reg-code base))))))))

(defun emit-xchg-mem-reg (buf base reg)
  "XCHG [base], reg — atomic exchange (implicit LOCK on x86).
   REX.W + 87 /r"
  (let ((r (reg-extended-p reg))
        (b (reg-extended-p base)))
    (emit-byte buf (rex-prefix t r nil b))
    (emit-byte buf #x87)
    (let ((needs-sib (= (logand (reg-code base) 7) 4)))
      (cond
        ((= (reg-code base) 5)
         (emit-byte buf (modrm #b01 (reg-code reg) (reg-code base)))
         (emit-byte buf 0))
        (needs-sib
         (emit-byte buf (modrm #b00 (reg-code reg) 4))
         (emit-byte buf #x24))
        (t
         (emit-byte buf (modrm #b00 (reg-code reg) (reg-code base))))))))

;;; ============================================================
;;; Function Prologue / Epilogue
;;; ============================================================

(defun emit-lea-label (buf phys-reg label)
  "Emit LEA reg, [RIP + disp32] to load a label's native address into a register.
   Uses the same fixup mechanism as emit-call (both are RIP-relative disp32)."
  ;; REX.W + LEA r64, [RIP+disp32]
  ;; Encoding: [REX] 8D [ModR/M: 00 reg 101]  disp32
  (let ((extended (reg-extended-p phys-reg)))
    (emit-byte buf (rex-prefix t extended nil nil))
    (emit-byte buf #x8D)    ; LEA
    ;; ModR/M: mod=00, r/m=101 (RIP-relative), reg=phys-reg
    (emit-byte buf (modrm #b00 (reg-code phys-reg) 5))
    ;; disp32: use same fixup as emit-call/emit-label-ref-rel32
    (if (label-position label)
        (emit-u32 buf (logand #xFFFFFFFF
                              (- (label-position label)
                                 (+ (code-buffer-position buf) 4))))
        (emit-label-ref-rel32 buf label))))

(defun emit-function-prologue (buf)
  "Emit the standard function prologue.
   push rbp / mov rbp,rsp / sub rsp,frame_size / save RBX
   In kernel mode, R12 (alloc ptr), R14 (alloc limit), R15 (nil) are global
   state that must NOT be saved/restored.  RBX (V4) is callee-saved."
  (emit-push buf 'rbp)
  (emit-mov-reg-reg buf 'rbp 'rsp)
  ;; Reserve space for callee-save + spill slots + frame slots
  (emit-sub-reg-imm buf 'rsp +frame-total-size+)
  ;; Save RBX (V4) as callee-saved register at [RBP-8]
  (emit-mov-mem-reg buf 'rbp 'rbx -8))

(defun emit-function-epilogue (buf)
  "Emit the standard function epilogue.
   Restore RBX / mov rsp,rbp / pop rbp / ret."
  (emit-mov-reg-mem buf 'rbx 'rbp -8)
  (emit-mov-reg-reg buf 'rsp 'rbp)
  (emit-pop buf 'rbp)
  (emit-ret buf))

;;; ============================================================
;;; Single Function Translation
;;; ============================================================

(defun translate-function (bytecode offset length target-buf)
  "Translate a single MVM function starting at OFFSET in BYTECODE
   for LENGTH bytes.  Native code is emitted into TARGET-BUF
   (a code-buffer).  Returns the code-buffer."
  (let* ((buf (or target-buf (make-code-buffer)))
         (state (make-translate-state
                 :buf buf
                 :mvm-bytes bytecode
                 :mvm-length length
                 :mvm-offset offset)))
    ;; Emit prologue
    (emit-function-prologue buf)
    ;; First pass: scan for branch targets and create labels
    (scan-branch-targets state)
    ;; Second pass: translate instructions
    (let ((pos offset)
          (limit (+ offset length)))
      (loop while (< pos limit)
            do (progn
                 ;; If there is a label at this MVM position, emit it
                 (let ((label (gethash pos (translate-state-position-labels state))))
                   (when label
                     (emit-label buf label)))
                 ;; Decode and translate
                 (let* ((decoded (decode-instruction bytecode pos))
                        (opcode (car decoded))
                        (operands (cadr decoded))
                        (new-pos (cddr decoded)))
                   (translate-instruction state opcode operands new-pos)
                   (setf pos new-pos)))))
    ;; Resolve label fixups
    (fixup-labels buf)
    buf))

(defun scan-branch-targets (state)
  "Pre-scan MVM bytecode to identify all branch targets.
   Creates labels for each target position so that forward branches
   can be resolved during the translation pass."
  (let* ((bytes (translate-state-mvm-bytes state))
         (offset (translate-state-mvm-offset state))
         (length (translate-state-mvm-length state))
         (pos offset)
         (limit (+ offset length)))
    (loop while (< pos limit)
          do (let* ((decoded (decode-instruction bytes pos))
                    (opcode (car decoded))
                    (operands (cadr decoded))
                    (new-pos (cddr decoded)))
               ;; Check if this is a branch instruction
               (let ((info (gethash opcode *opcode-table*)))
                 (when info
                   (let ((op-specs (opcode-info-operands info)))
                     ;; Branch instructions have :off32 in their operand spec
                     (when (member :off32 op-specs)
                       ;; Find the offset operand
                       (let ((off-idx (position :off32 op-specs)))
                         (when off-idx
                           (let* ((off (nth off-idx operands))
                                  (target-pos (+ new-pos off)))
                             (ensure-label-at state target-pos))))))))
               (setf pos new-pos)))))

;;; ============================================================
;;; Full Bytecode Translation
;;; ============================================================

(defvar *x64-gc-enabled* nil
  "When non-nil, emit GC trampoline and wire gc-check to call it.
   Set by Linux x64 builds that include gc.lisp.")

(defvar *x64-gc-debug* nil
  "When non-nil, the native GC trampoline writes diagnostic bytes to fd 1
   at entry/exit (see emit-gc-dbg-char).  Build-time knob only.")

(defvar *mcgc-bitmap-enabled* :follow-gc
  "Controls whether allocation sites set the mostly-copying GC's
   object-start bitmap bit for each freshly-allocated object (MCGC stage
   2+).  Through stage 2 the bit is WRITE-ONLY (the old Cheney collector
   ignores it); stage 3's collector consumes it to validate conservative
   roots.

   Value :FOLLOW-GC (the default) ties bitmap emission to
   *x64-gc-enabled* — every x64 GC-enabled build (all of which init the
   MCGC config words via boot-linux-x64 / boot-x64) gets the bitmap, and
   no GC-less build (fixpoint, plain build-mvm) does, so those stay
   byte-identical.  T / NIL force on / off.")

(defun mcgc-bitmap-on-p ()
  (if (eq *mcgc-bitmap-enabled* :follow-gc)
      *x64-gc-enabled*
      *mcgc-bitmap-enabled*))

(defvar *mcgc-collector-enabled* :follow-gc
  "Controls the MCGC stage-3 collector enhancements to the x64 GC
   trampoline:
     (a) scan_word validates a candidate pointer against the object-start
         bitmap before treating it as a heap reference — a word whose
         from-space target is NOT a recorded object start is rejected
         (kills the conservative false-positive forward-garbage class);
     (b) copy_object SETS the object-start bit for each survivor in
         to-space;
     (c) the trampoline CLEARS the bitmap bits covering the (now-free)
         old from-space at the end of each collection, so stale bits do
         not validate phantom roots on the next GC.
   :FOLLOW-GC ties it to *x64-gc-enabled* (and hence requires the bitmap
   writes of stage 2 and the config words of stage 1).  T / NIL force.")

(defun mcgc-collector-on-p ()
  (if (eq *mcgc-collector-enabled* :follow-gc)
      *x64-gc-enabled*
      *mcgc-collector-enabled*))

(defvar *mcgc-kind-bitmap-enabled* nil
  "MASTER gate for the CONS-KIND bitmap feature (the fix for the symbol-
   truncation-via-cons-tagged-scratch corruption).  DEFAULT NIL.

   The kind bitmap lives at [+mcgc-cfg-bitmap-addr+] + +mcgc-kindbitmap-delta+
   (#x804000) — a LINUX-x64 layout constant (boot-linux-x64.lisp asserts it).
   Bare-metal x64 (boot-x64.lisp, e.g. build-x64) lays the
   metadata out DIFFERENTLY, so that delta is wrong there.  Until the base is
   made layout-agnostic (a config word filled by each boot, or a lazy compute
   from page_count/freelist_base like emit-mcgc-pincount-init), the feature is
   gated ON only by the Linux builds (build-generic / build-x64-linux).  When
   NIL, NO kind-bitmap code is emitted (set / check / clear), so non-Linux
   x64 and GC-less builds are byte-identical to before this change.")

(defun mcgc-kind-bitmap-on-p ()
  "Kind-bitmap SET + CLEAR side: requires the object-start bitmap AND the
   master flag.  When this is on, cons alloc/copy sites mark cons starts and
   point-(c) clears the kind bits with the reclaimed range."
  (and (mcgc-bitmap-on-p) *mcgc-kind-bitmap-enabled*))

(defvar *mcgc-kind-check-enabled* t
  "Sub-gate for the scan_word CONS-KIND cross-check (the two
   emit-mcgc-cons-kind-or-jump calls), only meaningful when
   *mcgc-kind-bitmap-enabled*.  T (default) = reject a candidate whose
   pointer tag disagrees with the granule's cons/object kind bit.  NIL keeps
   the SET side + clear but skips ONLY the reject — an A/B proving the CHECK
   (not incidental layout shift) restores correctness.")

(defun mcgc-kind-check-on-p ()
  (and (mcgc-kind-bitmap-on-p) *mcgc-kind-check-enabled*))

(defvar *mcgc-pinning-enabled* nil
  "MCGC stage 3-4: the page-pinning mostly-copying collector (FFI/IO +
   Bartlett conservative-root pinning).  DEFAULT NIL — when off, the
   allocator and collector are EXACTLY the validation Cheney collector
   (mcgc-collector-on-p), so the binary is behavior-identical to canonical.
   When T, alloc sites emit the size-aware page pre-check (`emit-mcgc-
   ensure-room`) instead of the post-write gc-check model, refilling R12/R14
   from the run-free-list, and the collector runs the page-based phases.
   Requires *x64-gc-enabled* + the stage 1-2 bitmap/metadata.  Force T only
   in a pinning build/test; canonical stays NIL until stage 4 passes the gate.")

(defun mcgc-pinning-on-p ()
  (and *mcgc-pinning-enabled* *x64-gc-enabled*))

(defvar *mcgc-torun-cap-pages* 0
  "TEST KNOB (0 = off).  When > 0, every to-run SEGMENT the collector pops is
   capped at this many pages, with the run's remainder put back on the free-list.
   Tiny segments force frequent copy_object refills, so an ordinary workload
   exercises the to-run-chain / refill path deterministically (no need to
   engineer real pin fragmentation).  Set via MODUS_MCGC_TORUN_CAP=<pages> in a
   pinning build.  Does nothing unless mcgc-pinning is on.")

;;; Additional MCGC config-word slots for stage 3-4 (page pinning).  These
;;; extend the 0x10000E00.. block initialised by boot-linux-x64.lisp.  The
;;; run-free-list is an array of (start_page:u32, n_pages:u32) entries at
;;; +mcgc-cfg-freelist+ (base from stage 1); freelist_count counts ENTRIES.
(defconstant +mcgc-cfg-freelist-base-addr+  #x10000E20)  ; raw addr of run-free-list
(defconstant +mcgc-cfg-freelist-count-addr+ #x10000E28)  ; # of run entries
(defconstant +mcgc-cfg-alloc-page-addr+     #x10000E30)  ; current alloc page index
(defconstant +mcgc-cfg-data-end-addr+       #x10000E38)  ; raw addr one past data region
(defconstant +mcgc-cfg-descriptor-addr+     #x10000E10)  ; raw addr of page descriptor array
(defconstant +mcgc-cfg-page-count-addr+     #x10000E08)  ; total page count

;;; Stage-4 page-collector scratch words (ELF BSS, zero-init; NOT written by
;;; boot, so they read 0 until the collector's lazy-init seeds them).  Verified
;;; free above the 0x10000E00 config block (0x10000E40..0x10000EFF unused).
(defconstant +mcgc-cfg-run-start-addr+ #x10000E40)  ; raw addr: start of current alloc run
(defconstant +mcgc-cfg-run-end-addr+   #x10000E48)  ; raw addr: one past current alloc run
(defconstant +mcgc-cfg-to-start-addr+  #x10000E50)  ; raw addr: to-run start (this GC)
(defconstant +mcgc-cfg-to-end-addr+    #x10000E58)  ; raw addr: to-run end   (this GC)
(defconstant +mcgc-cfg-init-done-addr+ #x10000E60)  ; 0 until collector lazy-init ran
(defconstant +mcgc-cfg-from-start-addr+ #x10000E68) ; raw addr: from-run start (reclaim)
(defconstant +mcgc-cfg-from-end-addr+   #x10000E70) ; raw addr: from-run end   (reclaim)

;;; Stage-4c/4d page-PINNING config + scratch.  Slots 0x10000E78..0x10000EC0 are
;;; in the verified-free BSS gap (config block ends 0x10000E70; signal-handler
;;; scratch starts 0x10000C30 — non-overlapping).
(defconstant +mcgc-cfg-pincount-addr+  #x10000E78)  ; raw addr: per-page u32 PERSISTENT pin-count array
(defconstant +mcgc-cfg-scan-cursor-addr+ #x10000E80) ; raw addr: pinned-page scan cursor (transient)
(defconstant +mcgc-cfg-pin-init-addr+  #x10000E88)  ; 0 until pincount base computed (lazy)

;;; Stage-4e to-run REFILL: the collector evacuates survivors into a chain of
;;; to-run SEGMENTS, not one contiguous run.  When copy_object's bump ptr (R13)
;;; hits the current segment's end, it pops ANOTHER free run as the next segment
;;; (the "refill") so survivors that exceed the largest single free run (heavy
;;; pin fragmentation) no longer overflow + corrupt.  seg[i] is a 16-byte pair
;;; {start:u64, fill:u64}; the pair array lives just past the pin-count array in
;;; the MAP_ANON metadata slack (base computed at lazy-init).
(defconstant +mcgc-cfg-seg-arr-addr+   #x10000E90)  ; raw base of seg[] {start,fill} pair array
(defconstant +mcgc-cfg-seg-count-addr+ #x10000E98)  ; # active to-run segments this GC
(defconstant +mcgc-cfg-oom-addr+       #x10000EA0)  ; set to 1 if a refill found no free run (true OOM)
(defconstant +mcgc-cfg-uncap-addr+     #x10000EA8)  ; one-shot: next establish ignores the to-run cap
(defconstant +mcgc-max-segments+ 4096)              ; seg[] capacity (64 KiB of metadata slack)

(defconstant +mcgc-page-shift+ 12)                  ; 4 KiB pages
(defconstant +mcgc-page-bytes+ #x1000)              ; 4 KiB
(defconstant +mcgc-guard-bytes+ #x10000)            ; 64 KiB overshoot guard (16 pages)
(defconstant +mcgc-desc-pinned-bit+ 4)              ; descriptor bit2 = pinned-this-GC (transient)

(defun emit-mcgc-ensure-room (buf size-reg)
  "MCGC stage-4 size-aware allocation pre-check for objects that can EXCEED
   the GUARD (array/string with a runtime element count).  SIZE-REG holds the
   byte size of the object about to be written at [R12].  If R12+size would
   cross R14 (the guarded end of the current alloc run), CALL the page-GC
   trampoline (which copies survivors + refills R12/R14 from the rebuilt
   free-list to a fresh run) and retry once.  No-op unless mcgc-pinning-on-p.

   Emitted BEFORE the object is written: a page pool cannot absorb overshoot
   beyond the GUARD.  SIZE-REG must be a callee-saved reg the caller already
   holds (alloc-array/string keep the size in +scratch-reg+=RAX before the
   advance; we read it, but must not clobber it across the GC call, so we
   stash it).  Preserves ALL registers.

   GUARD note: small constant-size objects (cons/obj/float/sap, all <= GUARD)
   do NOT call this — they keep the legacy post-write gc-check, which under
   pinning routes to the same page-GC trampoline.  Their <= GUARD overshoot
   lands in the last 16 free pages of the SAME run, never the next region."
  (when (mcgc-pinning-on-p)
    (let ((ok (make-label))
          (retry (make-label)))
      ;; We need: if (R12 + size) > R14 -> GC ; then re-check (one retry).
      ;; size lives in SIZE-REG (RAX for array/string).  Preserve it across GC.
      (emit-label buf retry)
      (emit-push buf 'rax)                       ; save size (if SIZE-REG=RAX, saved here)
      (when (not (eq size-reg 'rax))
        (emit-mov-reg-reg buf 'rax size-reg))    ; rax = size
      (emit-add-reg-reg buf 'rax 'r12)           ; rax = R12 + size  (prospective end)
      (emit-cmp-reg-reg buf 'rax 'r14)           ; end vs guarded run end
      (emit-jcc buf :be ok)                       ; end <= R14 -> room, fall through
      ;; Not enough room: pop the saved size, run the page GC, retry.
      (emit-pop buf 'rax)                         ; restore size into RAX
      (when (not (eq size-reg 'rax))
        (emit-mov-reg-reg buf size-reg 'rax))    ; restore SIZE-REG
      (let ((gc (mcgc-page-gc-label)))
        (when gc (emit-call buf gc)))
      (emit-jmp buf retry)
      (emit-label buf ok)
      (emit-pop buf 'rax)                         ; restore size
      (when (not (eq size-reg 'rax))
        (emit-mov-reg-reg buf size-reg 'rax)))))

;;; The page-GC trampoline label is created per-translation-unit (like the
;;; Cheney gc-trampoline-label) and stashed here so alloc sites / gc-check
;;; can reach it without threading it through translate-state.
(defvar *mcgc-page-gc-label* nil)
(defun mcgc-page-gc-label () *mcgc-page-gc-label*)

(defvar *x64-genmul-label* nil)  ; GENERIC-MULTIPLY label for op-mul-checked
(defvar *x64-genadd-label* nil)  ; GENERIC-ADD label for op-add-checked

;;; Label of the shared out-of-line cons-kind-bit setter (emit-mcgc-cons-bit-
;;; subroutine).  Published here so the cons alloc sites (+op-cons+ /
;;; +op-alloc-cons+) can CALL it instead of inlining the ~40-byte BTS
;;; sequence at every cons (~190k sites).  Created + published only when
;;; (mcgc-bitmap-on-p); NIL otherwise (GC-less builds stay byte-identical).
(defvar *mcgc-cons-bit-label* nil)
(defun mcgc-cons-bit-label () *mcgc-cons-bit-label*)

(defvar *x64-native-code-offset* 0
  "Byte offset from load-address where native code begins in the final image.
   For linux-x64: ELF-header(120) + boot-code(192) + JMP(5) = 317 = 0x13D.
   Used to compute actual native addresses for funcall alignment checks.
   A function at code-buffer position P has native address:
     load_addr + *x64-native-code-offset* + P
   funcall checks (addr & 0xF == 1) to detect closures, so we must ensure
   ((*x64-native-code-offset* + P) & 0xF) != 1 for all function start P.")

;;; ============================================================
;;; Code-bounds patch records
;;; ============================================================
;;;
;;; The boot stub writes code_base and code_end (load_addr + native
;;; code section bounds) into fixed memory slots so user-level
;;; functionp / range-check predicates can identify raw fn-addrs by
;;; address rather than by a fragile bit-pattern heuristic.
;;;
;;; The values aren't known when the boot stub is emitted — they
;;; depend on total native-code size.  Strategy: emit `mov rax,
;;; imm64; mov [slot], rax` with placeholder zeros; the cross.lisp
;;; image-assembly path patches the imm64 bytes after the buffer
;;; layout is final.
;;;
;;; *x64-code-base-patch-offset* / *x64-code-end-patch-offset* are
;;; byte offsets within the BOOT-CODE BUFFER (not the final image)
;;; pointing at the 8-byte imm64 of the corresponding `mov rax,
;;; imm64`.  cross.lisp adds the boot-code base offset to convert
;;; them to image offsets at patch time.
;;;
;;; The slots are reserved at fixed BSS-equivalent addresses:
;;;   #x10000160 = code-base   (lowest fn-addr, inclusive)
;;;   #x10000168 = code-end    (one past highest fn-addr, exclusive)

(defconstant +code-base-slot+ #x10000160)
(defconstant +code-end-slot+  #x10000168)

(defvar *x64-code-base-patch-offset* nil)
(defvar *x64-code-end-patch-offset*  nil)

(defun emit-code-bounds-init (buf)
  "Emit the boot-stub init block that records code_base and code_end
   into fixed memory slots.  Call this from the per-build entry stub.

   Records the byte offsets of the two imm64 placeholders into
   *x64-code-base-patch-offset* and *x64-code-end-patch-offset* so
   cross.lisp can patch them with the resolved load_addr-relative
   addresses after the final image layout is known.

   Code emitted (34 bytes total):
     48 B8 ?? ?? ?? ?? ?? ?? ?? ??   ; mov rax, imm64 (code_base)
     48 89 04 25 60 01 00 10         ; mov [#x10000160], rax
     48 B8 ?? ?? ?? ?? ?? ?? ?? ??   ; mov rax, imm64 (code_end)
     48 89 04 25 68 01 00 10         ; mov [#x10000168], rax"
  (let ((start-pos (mvm-buffer-position buf)))
    ;; mov rax, imm64 (code_base placeholder).
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xB8)
    (setf *x64-code-base-patch-offset* (mvm-buffer-position buf))
    (dotimes (i 8) (mvm-emit-byte buf 0))
    ;; mov [imm32], rax — REX.W (48) + opcode (89) + ModR/M (04) + SIB (25) + disp32.
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x89)
    (mvm-emit-byte buf #x04) (mvm-emit-byte buf #x25)
    (mvm-emit-u32  buf +code-base-slot+)
    ;; mov rax, imm64 (code_end placeholder).
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xB8)
    (setf *x64-code-end-patch-offset* (mvm-buffer-position buf))
    (dotimes (i 8) (mvm-emit-byte buf 0))
    ;; mov [code-end-slot], rax.
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x89)
    (mvm-emit-byte buf #x04) (mvm-emit-byte buf #x25)
    (mvm-emit-u32  buf +code-end-slot+)
    (- (mvm-buffer-position buf) start-pos)))

(defun emit-longjmp-body (buf pop-label)
  "Emit the LONGJMP sequence (TRAP #x0511 body): restore RSP/RBP/IP/RBX
   from [#x10000180..198], pop the per-fork handler stack so the OUTER
   frame becomes active, set RAX = T (#xDEAD1009), and jump.  Used by the
   TRAP #x0511 emission and by the bare-metal safepoint-deadline stub.

   Order matters: we must read OUR state BEFORE the pop helper overwrites
   [180].  We stash it in scratch memory at #x10000C10..#x10000C28.

   BARE-METAL: (a) sets the in-transition flag [0x10000D28] so the PIT
   deadline ISR defers while the scratch/pop/restore sequence is
   mid-flight; (b) zeroes the live-overflow word [0x10000D20] — this
   longjmp unwinds past ALL capped (strictly inner) frames, whose
   CLEAR-HANDLERs will never textually run, so their pending absorbs
   must be discarded or they would wrongly swallow OUTER pops later."
  (unless *x64-linux-mode*
    (emit-bytes buf #x48 #xC7 #x04 #x25) ; mov qword [imm32], 1
    (emit-u32 buf #x10000D28)
    (emit-u32 buf 1)
    (emit-bytes buf #x48 #xC7 #x04 #x25) ; mov qword [imm32], 0
    (emit-u32 buf #x10000D20)
    (emit-u32 buf 0))
  ;; mov rcx, 0x10000180
  (emit-bytes buf #x48 #xB9)
  (emit-u32 buf #x10000180) (emit-u32 buf 0)
  ;; rdx = [rcx]      ; our RSP
  (emit-bytes buf #x48 #x8B #x11)
  ;; mov [0x10000C10], rdx
  (emit-bytes buf #x48 #x89 #x14 #x25)
  (emit-u32 buf #x10000C10)
  ;; rdx = [rcx+8]    ; our RBP
  (emit-bytes buf #x48 #x8B #x51 #x08)
  ;; mov [0x10000C18], rdx
  (emit-bytes buf #x48 #x89 #x14 #x25)
  (emit-u32 buf #x10000C18)
  ;; rdx = [rcx+16]   ; our IP
  (emit-bytes buf #x48 #x8B #x51 #x10)
  ;; mov [0x10000C20], rdx
  (emit-bytes buf #x48 #x89 #x14 #x25)
  (emit-u32 buf #x10000C20)
  ;; rdx = [rcx+24]   ; our saved RBX
  (emit-bytes buf #x48 #x8B #x51 #x18)
  ;; mov [0x10000C28], rdx
  (emit-bytes buf #x48 #x89 #x14 #x25)
  (emit-u32 buf #x10000C28)
  ;; Pop the handler stack back into [180]/+8/+16/+24
  (emit-call buf pop-label)
  ;; Restore from scratch and jump
  ;; mov rdx, [0x10000C20]   ; IP
  (emit-bytes buf #x48 #x8B #x14 #x25)
  (emit-u32 buf #x10000C20)
  ;; mov rbp, [0x10000C18]   ; RBP
  (emit-bytes buf #x48 #x8B #x2C #x25)
  (emit-u32 buf #x10000C18)
  ;; mov rbx, [0x10000C28]   ; restore caller's V4=RBX
  (emit-bytes buf #x48 #x8B #x1C #x25)
  (emit-u32 buf #x10000C28)
  ;; mov rsp, [0x10000C10]   ; RSP
  (emit-bytes buf #x48 #x8B #x24 #x25)
  (emit-u32 buf #x10000C10)
  (unless *x64-linux-mode*
    ;; clear the in-transition flag — state is now consistent
    ;; ([180] = parent frame, target regs restored).
    (emit-bytes buf #x48 #xC7 #x04 #x25) ; mov qword [imm32], 0
    (emit-u32 buf #x10000D28)
    (emit-u32 buf 0))
  ;; mov eax, 0xDEAD1009  (T sentinel; 32-bit zero-extends)
  (emit-bytes buf #xB8 #x09 #x10 #xAD #xDE)
  ;; jmp rdx
  (emit-bytes buf #xFF #xE2))

(defun emit-yield-longjmp-stub (buf stub-label pop-label)
  "BARE-METAL safepoint-deadline stub.  YIELD sites (every compiled loop
   back-edge) call here when the PIT deadline ISR has set the pending
   flag [0x10000D30].  The stub consumes the flag and longjmps through
   the innermost armed handler-case — identical to TRAP #x0511, but at a
   SAFE POINT.  The old design longjmped directly from the ISR at an
   arbitrary instruction boundary; when a slow (timing-out) test was
   mid-intern / mid-alloc / mid-GC, the abandoned half-written global
   state poisoned the whole image (observed: intern-table entries of
   0xCC.. garbage — every subsequent symbol lookup #PF'd at the same
   RIP, CR2=0x76CCCCCCCC, and the run wedged in the acosh/asin/gcd
   band).  Linux never had this class because its harness recovers hung
   forks by KILLING them (alarm), never by async longjmp.

   The CALL that reaches this stub pushes a return address; the longjmp
   switches RSP, so it is simply abandoned.  If NO handler is armed
   ([0x10000180]=0) the longjmp body reads a zero frame — but that state
   is unreachable here in practice: the flag is only consumed by running
   compiled code, which implies the runner's handler-cases are armed."
  (emit-label buf stub-label)
  ;; TWO-TIER CONSUMPTION: a longjmp from a loop INSIDE the runtime's own
  ;; machinery (intern bucket walks, global-alist updates, printer loops)
  ;; abandons a mutating critical section mid-flight — the observed
  ;; "zombie tail" (every chunk crashing after the first runtime-region
  ;; consumption at cos.1).  The runner writes the RUNTIME/CORPUS code
  ;; boundary address into [0x10000DA0] (the tagged fn pointer of a
  ;; marker defun that is the first function of the generated runner
  ;; text).  Yields whose CALLER (return address at [rsp]) lies ABOVE
  ;; the boundary are corpus/runner code — always safe to consume.
  ;; Yields BELOW it are runtime internals: only consume when the
  ;; pending count has reached 3 (three deadline periods stuck inside
  ;; the runtime = a genuine runtime hang; the corrupting longjmp is
  ;; then the last resort it always used to be).  [0x10000DA0]=0 (marker
  ;; not wired, e.g. non-ANSI builds) degrades to always-consume.
  (let ((consume (make-label))
        (no-consume (make-label)))
    (emit-bytes buf #x50)                  ; push rax
    (emit-bytes buf #x48 #x8B #x44 #x24 #x08) ; mov rax, [rsp+8] (caller)
    (emit-bytes buf #x48 #x3B #x04 #x25)   ; cmp rax, [imm32]
    (emit-u32 buf #x10000DA0)
    (emit-jcc buf :ae consume)
    ;; runtime region: pending >= 3 ?
    (emit-bytes buf #x48 #x8B #x04 #x25)   ; mov rax, [imm32]
    (emit-u32 buf #x10000D30)
    (emit-bytes buf #x48 #x83 #xF8 #x03)   ; cmp rax, 3
    (emit-jcc buf :ae consume)
    (emit-label buf no-consume)
    (emit-bytes buf #x58)                  ; pop rax
    (emit-bytes buf #xC3)                  ; ret (leave pending for a later yield)
    (emit-label buf consume)
    (emit-bytes buf #x58)                  ; pop rax
    ;; consume the pending flag
    (emit-bytes buf #x48 #xC7 #x04 #x25)   ; mov qword [imm32], 0
    (emit-u32 buf #x10000D30)
    (emit-u32 buf 0)
    (emit-longjmp-body buf pop-label)))

(defun emit-handler-helpers (buf push-label pop-label)
  "Emit two helper functions used by handler-case setjmp/clear traps:
     __handler_push: push current [#x10000180/+8/+16/+24] state onto a memory
                     stack at #x10000408 (depth at #x10000400, max 64).
     __handler_pop:  pop top of stack into [#x10000180/+8/+16/+24]; if stack
                     empty, write 0 to [#x10000180].
   Both preserve all callee-saved regs. They clobber rax/rcx/rdx/r10/r11
   only — caller-saved per SysV ABI.

   Memory map:
     #x10000180/+8/+16/+24 — current handler state (RSP/RBP/IP/RBX, 32 bytes)
     #x10000400        — handler-stack depth (qword, init 0 by fork)
     #x10000408+32*N   — handler-stack frame N (32 bytes, max 64 frames;
                         region ends at #x10000C08, just below the #x10000C10
                         longjmp scratch slots)

   The RBX slot [#x10000198] (saved by SETJMP, restored by every longjmp
   path) MUST be stacked with the rest of the frame: without it, nested
   handler-cases share ONE RBX slot, so after an inner frame is pushed and
   popped, an OUTER longjmp restores the INNER (dead) frame's RBX — the
   caller's V4-cached value is silently replaced and the handler continuation
   runs on corrupt state (the silent-unwind family)."
  ;; ---- __handler_push ----
  (let ((skip (make-label))
        (capped (make-label))
        (nomax (make-label)))
    (emit-label buf push-label)
    ;; r10 = depth = [0x10000400]
    (emit-bytes buf #x4C #x8B #x14 #x25)
    (emit-u32 buf #x10000400)
    ;; cmp r10, 64 ; jge skip/capped
    ;; BARE-METAL (non-Linux): route the capped case through a diagnostic
    ;; counter at [0x10000D00].  A capped push is a STRUCTURAL IMBALANCE:
    ;; the frame is silently dropped but the matching CLEAR-HANDLER /
    ;; longjmp still pops, so every capped setjmp+clear pair drains one
    ;; REAL frame from the stack (the bare-metal ANSI drain-to-depth-0
    ;; halt class).  The counter makes cap events observable from the
    ;; runner (mem-ref #x10000D00).
    (emit-bytes buf #x49 #x83 #xFA #x40)
    (emit-jcc buf :ge (if *x64-linux-mode* skip capped))
    ;; r11 = depth*32
    (emit-bytes buf #x4D #x6B #xDA #x20)            ; imul r11, r10, 32
    ;; r11 += 0x10000408
    (emit-bytes buf #x49 #x81 #xC3)                  ; add r11, imm32
    (emit-u32 buf #x10000408)
    ;; rax = [0x10000180]; [r11] = rax
    (emit-bytes buf #x48 #x8B #x04 #x25)
    (emit-u32 buf #x10000180)
    (emit-bytes buf #x49 #x89 #x03)                  ; mov [r11], rax
    ;; rax = [0x10000188]; [r11+8] = rax
    (emit-bytes buf #x48 #x8B #x04 #x25)
    (emit-u32 buf #x10000188)
    (emit-bytes buf #x49 #x89 #x43 #x08)             ; mov [r11+8], rax
    ;; rax = [0x10000190]; [r11+16] = rax
    (emit-bytes buf #x48 #x8B #x04 #x25)
    (emit-u32 buf #x10000190)
    (emit-bytes buf #x49 #x89 #x43 #x10)             ; mov [r11+16], rax
    ;; rax = [0x10000198]; [r11+24] = rax   (saved RBX — see docstring)
    (emit-bytes buf #x48 #x8B #x04 #x25)
    (emit-u32 buf #x10000198)
    (emit-bytes buf #x49 #x89 #x43 #x18)             ; mov [r11+24], rax
    ;; depth++ ; [0x10000400] = r10
    (emit-bytes buf #x49 #xFF #xC2)                  ; inc r10
    (emit-bytes buf #x4C #x89 #x14 #x25)             ; mov [imm32], r10
    (emit-u32 buf #x10000400)
    (unless *x64-linux-mode*
      ;; DIAG: max-depth watermark at [0x10000D08]
      (emit-bytes buf #x4C #x8B #x1C #x25)           ; mov r11, [imm32]
      (emit-u32 buf #x10000D08)
      (emit-bytes buf #x4D #x39 #xD3)                ; cmp r11, r10
      (emit-jcc buf :ge nomax)
      (emit-bytes buf #x4C #x89 #x14 #x25)           ; mov [imm32], r10
      (emit-u32 buf #x10000D08)
      (emit-label buf nomax)
      ;; r11 = 0: frame stored — SETJMP arms [0x10000180]
      (emit-bytes buf #x4D #x31 #xDB))               ; xor r11, r11
    (emit-label buf skip)
    (emit-bytes buf #xC3)                            ; ret
    (unless *x64-linux-mode*
      ;; capped-push path (BARE-METAL BALANCED-CAP): the frame is NOT
      ;; stored, but the event is COUNTED in the live-overflow word at
      ;; [0x10000D20] so the matching CLEAR-HANDLER pop absorbs it
      ;; instead of draining a real stored frame (the drain-to-depth-0
      ;; halt class).  r11=1 tells the SETJMP trap to skip arming
      ;; [0x10000180] entirely: the capped handler-case degrades to a
      ;; transparent no-op (its body's errors go to the enclosing
      ;; handler) rather than corrupting the stack.  [0x10000D00] is
      ;; the cumulative diagnostic count.
      (emit-label buf capped)
      (emit-bytes buf #x48 #xFF #x04 #x25)           ; inc qword [imm32]
      (emit-u32 buf #x10000D00)
      (emit-bytes buf #x48 #xFF #x04 #x25)           ; inc qword [imm32]
      (emit-u32 buf #x10000D20)
      (emit-bytes buf #x49 #xC7 #xC3 #x01 #x00 #x00 #x00) ; mov r11, 1
      (emit-bytes buf #xC3)))                        ; ret
  ;; ---- __handler_pop ----
  ;; Preserves RAX across the call. CLEAR-HANDLER is emitted after a
  ;; handler-case body succeeds; if dest == VR == RAX, the body's result
  ;; lives in RAX, and clobbering it would hand back a popped RSP
  ;; (masquerading as an unknown-subtag object like #<?184>).
  (let ((empty (make-label))
        (done (make-label))
        (no-ovf (make-label)))
    (emit-label buf pop-label)
    (emit-bytes buf #x50)                            ; push rax
    (unless *x64-linux-mode*
      ;; BARE-METAL BALANCED-CAP: if the live-overflow word [0x10000D20]
      ;; is non-zero, this pop textually matches a CAPPED push (whose
      ;; frame was never stored) — absorb it: decrement overflow, leave
      ;; [0x10000180..198] and the stored stack untouched.  Longjmp-side
      ;; callers (TRAP #x0511, the #PF/#GP recovery ISR, the PIT deadline
      ;; ISR) zero [0x10000D20] BEFORE popping — a longjmp unwinds past
      ;; ALL capped (strictly inner) frames, so their absorbs must not
      ;; fire against outer pops.
      (emit-bytes buf #x4C #x8B #x14 #x25)           ; mov r10, [imm32]
      (emit-u32 buf #x10000D20)
      (emit-bytes buf #x4D #x85 #xD2)                ; test r10, r10
      (emit-jcc buf :e no-ovf)
      (emit-bytes buf #x49 #xFF #xCA)                ; dec r10
      (emit-bytes buf #x4C #x89 #x14 #x25)           ; mov [imm32], r10
      (emit-u32 buf #x10000D20)
      (emit-jmp buf done)
      (emit-label buf no-ovf))
    ;; r10 = depth = [0x10000400]
    (emit-bytes buf #x4C #x8B #x14 #x25)
    (emit-u32 buf #x10000400)
    ;; test r10, r10 ; jz empty
    (emit-bytes buf #x4D #x85 #xD2)
    (emit-jcc buf :e empty)
    ;; depth-- ; [0x10000400] = r10
    (emit-bytes buf #x49 #xFF #xCA)                  ; dec r10
    (emit-bytes buf #x4C #x89 #x14 #x25)
    (emit-u32 buf #x10000400)
    ;; r11 = depth*32 + 0x10000408
    (emit-bytes buf #x4D #x6B #xDA #x20)             ; imul r11, r10, 32
    (emit-bytes buf #x49 #x81 #xC3)                  ; add r11, imm32
    (emit-u32 buf #x10000408)
    ;; Use r10 as memory-scratch (its value is now consumed) so RAX
    ;; stays pristine across mem-to-mem copies.
    ;; [0x10000180] = [r11]
    (emit-bytes buf #x4D #x8B #x13)                  ; mov r10, [r11]
    (emit-bytes buf #x4C #x89 #x14 #x25)             ; mov [imm32], r10
    (emit-u32 buf #x10000180)
    ;; [0x10000188] = [r11+8]
    (emit-bytes buf #x4D #x8B #x53 #x08)             ; mov r10, [r11+8]
    (emit-bytes buf #x4C #x89 #x14 #x25)
    (emit-u32 buf #x10000188)
    ;; [0x10000190] = [r11+16]
    (emit-bytes buf #x4D #x8B #x53 #x10)             ; mov r10, [r11+16]
    (emit-bytes buf #x4C #x89 #x14 #x25)
    (emit-u32 buf #x10000190)
    ;; [0x10000198] = [r11+24]   (saved RBX — see push above)
    (emit-bytes buf #x4D #x8B #x53 #x18)             ; mov r10, [r11+24]
    (emit-bytes buf #x4C #x89 #x14 #x25)
    (emit-u32 buf #x10000198)
    (emit-jmp buf done)
    (emit-label buf empty)
    (unless *x64-linux-mode*
      ;; DIAG: pop-at-empty counter at [0x10000D10] — each hit means the
      ;; stack drained below the structural base (imbalance evidence).
      (emit-bytes buf #x48 #xFF #x04 #x25)           ; inc qword [imm32]
      (emit-u32 buf #x10000D10))
    ;; [0x10000180] = 0  (legacy "no handler" sentinel)
    (emit-bytes buf #x48 #xC7 #x04 #x25)             ; mov qword [imm32], 0
    (emit-u32 buf #x10000180)
    (emit-u32 buf 0)
    (emit-label buf done)
    (emit-bytes buf #x58)                            ; pop rax
    (emit-bytes buf #xC3))                           ; ret
  (format t "  Handler-stack helpers emitted (push/pop)~%"))

(defun emit-gc-dbg-char (buf ch)
  "DEBUG (gated by *x64-gc-debug*): write byte CH to fd 1, preserving all
   registers used by the GC trampoline.  syscall clobbers RCX/R11 plus the
   arg regs; we push/pop everything we touch."
  (when *x64-gc-debug*
    ;; Save every reg the write syscall touches.
    (emit-push buf 'rax) (emit-push buf 'rdi) (emit-push buf 'rsi)
    (emit-push buf 'rdx) (emit-push buf 'rcx) (emit-push buf 'r11)
    ;; Put the byte in a scratch stack slot so RSI can point at it.
    (emit-bytes buf #x48 #x83 #xEC #x08)            ; sub rsp, 8
    (emit-bytes buf #x48 #xC7 #x04 #x24)            ; mov qword [rsp], imm32
    (emit-u32 buf ch)
    (emit-bytes buf #x48 #x89 #xE6)                 ; mov rsi, rsp (buf)
    (emit-bytes buf #x48 #xC7 #xC7 #x01 #x00 #x00 #x00) ; mov rdi, 1 (fd)
    (emit-bytes buf #x48 #xC7 #xC2 #x01 #x00 #x00 #x00) ; mov rdx, 1 (len)
    (emit-bytes buf #x48 #xC7 #xC0 #x01 #x00 #x00 #x00) ; mov rax, 1 (sys_write)
    (emit-bytes buf #x0F #x05)                      ; syscall
    (emit-bytes buf #x48 #x83 #xC4 #x08)            ; add rsp, 8
    (emit-pop buf 'r11) (emit-pop buf 'rcx) (emit-pop buf 'rdx)
    (emit-pop buf 'rsi) (emit-pop buf 'rdi) (emit-pop buf 'rax)))

(defun emit-mcgc-validate-or-jump (buf addr-reg reject-label)
  "MCGC stage-3 scan_word gate.  ADDR-REG holds a RAW (tag-stripped)
   from-space address that passed the from-space bounds check.  If the
   object-start bitmap bit for that address is CLEAR (i.e. the address is
   not a recorded object start — a conservative false positive or an
   interior pointer), jump to REJECT-LABEL.  Otherwise fall through.

   ADDR-REG must be RAX (the value scan_word already holds there).  Uses
   RDX and R8 as temps (saved/restored); preserves RAX/RBX/RCX/RSI/R13.

   granule = (addr - page_base) >> 4 ; CF = BT [bitmap_base], granule."
  (unless (eq addr-reg 'rax)
    (error "emit-mcgc-validate-or-jump: ADDR-REG must be RAX"))
  (emit-push buf 'rdx)
  (emit-push buf 'r8)
  (emit-mov-reg-reg buf 'rdx 'rax)
  (emit-bytes buf #x48 #x2B #x14 #x25)              ; sub rdx, [abs32] (page_base)
  (emit-u32 buf +mcgc-cfg-page-base-addr+)
  (emit-shr-reg-imm buf 'rdx 4)                     ; rdx = granule index
  (emit-bytes buf #x4C #x8B #x04 #x25)              ; mov r8, [abs32] (bitmap_base)
  (emit-u32 buf +mcgc-cfg-bitmap-addr+)
  (emit-bytes buf #x49 #x0F #xA3 #x10)              ; bt [r8], rdx  (CF=bit)
  (emit-pop buf 'r8)
  (emit-pop buf 'rdx)
  (emit-jcc buf :nc reject-label))                  ; bit clear → reject

(defun emit-mcgc-set-copy-bit (buf addr-reg)
  "MCGC stage-3: set the object-start bitmap bit for a survivor COPIED to
   to-space, whose RAW start address is in ADDR-REG (must not be one of
   the temps RAX/RDX/R8 — copy_object passes R13's old value via a
   caller-chosen reg).  Saves/restores RAX, RDX, R8."
  (when (member addr-reg '(rax rdx r8))
    (error "emit-mcgc-set-copy-bit: ADDR-REG must not be RAX/RDX/R8"))
  (emit-push buf 'rax)
  (emit-push buf 'rdx)
  (emit-push buf 'r8)
  (emit-mov-reg-reg buf 'rdx addr-reg)
  (emit-bytes buf #x48 #x2B #x14 #x25)              ; sub rdx, [abs32] (page_base)
  (emit-u32 buf +mcgc-cfg-page-base-addr+)
  (emit-shr-reg-imm buf 'rdx 4)                     ; granule
  (emit-bytes buf #x4C #x8B #x04 #x25)              ; mov r8, [abs32] (bitmap_base)
  (emit-u32 buf +mcgc-cfg-bitmap-addr+)
  (emit-bytes buf #x49 #x0F #xAB #x10)              ; bts [r8], rdx
  (emit-pop buf 'r8)
  (emit-pop buf 'rdx)
  (emit-pop buf 'rax))

(defun emit-mcgc-cons-bit-subroutine (buf label)
  "Emit (once) the shared out-of-line CONS-KIND-bit setter.
   Input: R12 = raw cons base (exactly what the cons alloc sites hold).
   Sets the cons-kind bit for that granule; PRESERVES ALL registers (uses
   RAX/RCX/RDX internally, saved/restored) and does NOT modify R12.  Cons
   alloc sites CALL this (5 bytes) instead of inlining ~40 bytes each."
  (emit-label buf label)
  (emit-push buf 'rax)
  (emit-push buf 'rcx)
  (emit-push buf 'rdx)
  (emit-mov-reg-reg buf 'rax 'r12)
  (emit-bytes buf #x48 #x2B #x04 #x25)          ; sub rax, [page_base]
  (emit-u32 buf +mcgc-cfg-page-base-addr+)
  (emit-shr-reg-imm buf 'rax 4)                 ; granule
  (emit-bytes buf #x48 #x8B #x0C #x25)          ; mov rcx, [bitmap_base]
  (emit-u32 buf +mcgc-cfg-bitmap-addr+)
  (emit-bytes buf #x48 #x81 #xC1)               ; add rcx, imm32 (kind delta)
  (emit-u32 buf +mcgc-kindbitmap-delta+)
  (emit-bytes buf #x48 #x0F #xAB #x01)          ; bts [rcx], rax
  (emit-pop buf 'rdx)
  (emit-pop buf 'rcx)
  (emit-pop buf 'rax)
  (emit-ret buf))

(defun emit-mcgc-call-cons-bit (buf)
  "At a CONS alloc site (R12 = cons base), CALL the shared cons-kind-bit
   subroutine (emit-mcgc-cons-bit-subroutine) to mark the granule as a cons
   start.  Out-of-line keeps the per-cons cost to one 5-byte CALL.  No-op
   unless (mcgc-kind-bitmap-on-p) (so non-Linux-x64 / GC-less builds stay
   byte-identical)."
  (when (mcgc-kind-bitmap-on-p)
    (emit-call buf (mcgc-cons-bit-label))))

(defun emit-mcgc-set-cons-bit-copy (buf addr-reg)
  "Set the CONS-KIND bitmap bit for a CONS survivor COPIED to to-space
   (RAW dest start in ADDR-REG, e.g. R13).  Mirrors emit-mcgc-set-copy-bit
   (saves RAX/RDX/R8, preserves RCX=from_end and the other GC live regs);
   ADDR-REG must not be RAX/RDX/R8.  No-op unless (mcgc-kind-bitmap-on-p)."
  (when (mcgc-kind-bitmap-on-p)
    (when (member addr-reg '(rax rdx r8))
      (error "emit-mcgc-set-cons-bit-copy: ADDR-REG must not be RAX/RDX/R8"))
    (emit-push buf 'rax)
    (emit-push buf 'rdx)
    (emit-push buf 'r8)
    (emit-mov-reg-reg buf 'rdx addr-reg)
    (emit-bytes buf #x48 #x2B #x14 #x25)          ; sub rdx, [page_base]
    (emit-u32 buf +mcgc-cfg-page-base-addr+)
    (emit-shr-reg-imm buf 'rdx 4)                 ; granule
    (emit-bytes buf #x4C #x8B #x04 #x25)          ; mov r8, [bitmap_base]
    (emit-u32 buf +mcgc-cfg-bitmap-addr+)
    (emit-bytes buf #x49 #x81 #xC0)               ; add r8, imm32 (kind delta)
    (emit-u32 buf +mcgc-kindbitmap-delta+)
    (emit-bytes buf #x49 #x0F #xAB #x10)          ; bts [r8], rdx
    (emit-pop buf 'r8)
    (emit-pop buf 'rdx)
    (emit-pop buf 'rax)))

(defun emit-mcgc-cons-kind-or-jump (buf addr-reg want-cons reject-label)
  "scan_word kind cross-check.  ADDR-REG (must be RAX) = a RAW from-space
   address already validated as a recorded object start.  Reads the CONS-
   KIND bit for that granule and rejects a TAG/KIND mismatch:
     WANT-CONS = T   (caller saw a cons-tagged candidate): reject (jump
                      REJECT-LABEL) if the bit is CLEAR — the start is an
                      OBJECT, so the cons tag is a conservative false
                      positive (the symbol_base|1 truncation bug).
     WANT-CONS = NIL (caller saw an object-tagged candidate): reject if the
                      bit is SET — the start is a CONS, so the object tag is
                      a false positive (the symmetric case).
   Uses RDX and R8 as temps (saved/restored); preserves RAX/RBX/RCX/RSI/R13.
   Kind bitmap base = [bitmap_base] + +mcgc-kindbitmap-delta+."
  (unless (eq addr-reg 'rax)
    (error "emit-mcgc-cons-kind-or-jump: ADDR-REG must be RAX"))
  (emit-push buf 'rdx)
  (emit-push buf 'r8)
  (emit-mov-reg-reg buf 'rdx 'rax)
  (emit-bytes buf #x48 #x2B #x14 #x25)              ; sub rdx, [page_base]
  (emit-u32 buf +mcgc-cfg-page-base-addr+)
  (emit-shr-reg-imm buf 'rdx 4)                     ; granule
  (emit-bytes buf #x4C #x8B #x04 #x25)              ; mov r8, [bitmap_base]
  (emit-u32 buf +mcgc-cfg-bitmap-addr+)
  (emit-bytes buf #x49 #x81 #xC0)                   ; add r8, imm32 (kind delta)
  (emit-u32 buf +mcgc-kindbitmap-delta+)
  (emit-bytes buf #x49 #x0F #xA3 #x10)              ; bt [r8], rdx  (CF = cons-kind bit)
  (emit-pop buf 'r8)
  (emit-pop buf 'rdx)
  (if want-cons
      (emit-jcc buf :nc reject-label)               ; cons tag but bit clear (object) → reject
      (emit-jcc buf :c reject-label)))              ; object tag but bit set (cons) → reject

(defun emit-gc-trampoline (buf gc-trampoline-label gc-collect-label)
  "Emit a complete Cheney copying GC in native x64 assembly.

   GC metadata layout (raw byte addresses at heap base 0x10000000):
     +0x40: from_start   +0x48: to_start   +0x50: space_size
     +0x58: stack_base   +0x60: gc_count
   All metadata values are stored as raw byte addresses (NOT tagged).

   Register convention during GC:
     R13 = free pointer (next write position in to-space)
     RBX = from_start (old from-space base)
     RCX = from_end   (old from-space end = from_start + space_size)
     R12/R14 = scratch during GC, restored to new alloc/limit at end"
  (declare (ignore gc-collect-label))

  ;; Labels for GC subroutines
  (let ((copy-label (make-label))      ; copy_object(RAX) -> RAX=new ptr, R13 advanced
        (scan-word-label (make-label))  ; scan_word(RAX=addr of word) -> update word, R13
        (restore-label (make-label)))

    (emit-label buf gc-trampoline-label)
    (unless *x64-linux-mode*
      ;; BARE-METAL: set the in-transition flag [0x10000D28] for the WHOLE
      ;; collection.  The PIT deadline ISR longjmps through the innermost
      ;; handler-case on expiry; a longjmp OUT OF MID-GC abandons a
      ;; half-copied heap (semispaces mid-flip, forwarding pointers
      ;; stamped, R12/R14 not yet swapped) — every subsequent alloc/deref
      ;; runs on corrupted state.  Observed as the fast-regime asin/gcd
      ;; band wedges: silent handler-stack drain + wild writes (deadline
      ;; counter = heap pointer) + global-alist walk spins.  The ISR
      ;; DEFERS (counter=1, retry next tick) while this flag is set, so
      ;; the deadline lands only at a consistent point after the GC.
      (emit-bytes buf #x48 #xC7 #x04 #x25)         ; mov qword [imm32], 1
      (emit-u32 buf #x10000D28)
      (emit-u32 buf 1))
    (emit-gc-dbg-char buf #x5B)          ; '[' — trampoline entry

    ;; ---- Save all caller registers ----
    (emit-push buf 'rax)
    (emit-push buf 'rsi)
    (emit-push buf 'rdi)
    (emit-push buf 'r8)
    (emit-push buf 'r9)
    (emit-push buf 'rbx)
    (emit-push buf 'rcx)
    (emit-push buf 'rdx)
    (emit-push buf 'r10)
    (emit-push buf 'r11)
    (emit-push buf 'r13)
    (emit-push buf 'rbp)
    ;; Save RSP for stack root scanning (after all pushes)
    (emit-bytes buf #x48 #x89 #xE5)              ; mov rbp, rsp  (save scan start)

    ;; ---- Load GC metadata ----
    ;; RBX = from_start (raw byte addr)
    (emit-bytes buf #x48 #x8B #x1C #x25)         ; mov rbx, [abs32]
    (emit-u32 buf #x10000040)
    ;; R13 = to_start -> becomes free pointer
    (emit-bytes buf #x4C #x8B #x2C #x25)         ; mov r13, [abs32]
    (emit-u32 buf #x10000048)
    ;; RCX = from_start + space_size = from_end
    (emit-bytes buf #x48 #x8B #x0C #x25)         ; mov rcx, [abs32]
    (emit-u32 buf #x10000050)
    (emit-add-reg-reg buf 'rcx 'rbx)             ; rcx = from_start + space_size

    (emit-gc-dbg-char buf #x70)          ; 'p' — pushed regs + metadata loaded, about to scan stack
    ;; ---- Scan stack roots ----
    ;; Walk from RBP (saved RSP) to stack_base
    ;; RDI = current scan address
    (emit-bytes buf #x48 #x89 #xEF)              ; mov rdi, rbp  (start of stack)
    (let ((stack-loop (make-label))
          (stack-done (make-label)))
      ;; RDX = stack_base
      (emit-bytes buf #x48 #x8B #x14 #x25)       ; mov rdx, [abs32]
      (emit-u32 buf #x10000058)

      (emit-label buf stack-loop)
      (emit-cmp-reg-reg buf 'rdi 'rdx)           ; rdi >= stack_base?
      (emit-jcc buf :ae stack-done)
      ;; Load the stack word
      (emit-mov-reg-mem buf 'rax 'rdi 0)          ; rax = [rdi]
      ;; Call scan_word subroutine (rax = addr of word to scan)
      (emit-bytes buf #x48 #x89 #xF8)            ; mov rax, rdi  (addr of the word)
      (emit-call buf scan-word-label)
      ;; Advance to next word
      (emit-add-reg-imm buf 'rdi 8)
      (emit-jmp buf stack-loop)
      (emit-label buf stack-done))
    (emit-gc-dbg-char buf #x73)          ; 's' — stack scan done

    ;; ---- Scan globals roots ----
    ;; Globals alist at 0x10000080
    (emit-mov-reg-imm buf 'rax #x10000080)
    (emit-call buf scan-word-label)
    ;; Symbol table at 0x10000088
    (emit-mov-reg-imm buf 'rax #x10000088)
    (emit-call buf scan-word-label)
    ;; Keyword intern table at 0x10000148 (init-keyword-table / %intern-keyword).
    ;; A heap hash-table root: without scanning it, any keyword interned
    ;; before a collection becomes a stale from-space pointer and the next
    ;; KEYWORDP / keyword deref faults.  %intern-keyword re-reads this slot
    ;; after each alloc expecting the GC to have forwarded it.
    (emit-mov-reg-imm buf 'rax #x10000148)
    (emit-call buf scan-word-label)
    ;; Package-by-hash table at 0x10000170 (%init-pkg-by-hash / %intern-symbol-pkg).
    ;; Same convention as the symbol intern table — a heap hash-table whose
    ;; root slot the GC must forward.
    (emit-mov-reg-imm buf 'rax #x10000170)
    (emit-call buf scan-word-label)
    ;; NOTE: the pre-interned signal-condition symbols ('TYPE-ERROR /
    ;; 'PROGRAM-ERROR / 'UNDEFINED-FUNCTION) no longer live in raw slots
    ;; 0xCA0/0xCA8/0xCB0 — those slots were NOT scanned here, so after the
    ;; first collection they DANGLED into recycled from-space (gdb HW
    ;; watchpoint showed a file-stream bpos cell reusing the old TYPE-ERROR
    ;; symbol's memory; every later %signal-type-error then produced a
    ;; condition %condition-p rejected, and the longjmp silently fell
    ;; through every handler frame — the asdf-gauntlet define-package
    ;; silent aborts).  %init-signal-symbols now stores them in SPECIALS
    ;; (*%sig-type-error-sym* etc., cl-conditions.lisp), which the globals-
    ;; alist scan above forwards correctly.  The old rationale for not
    ;; scanning the raw slots ("already forwarded via the symbol intern
    ;; table") was wrong precisely because forwarding the TABLE never
    ;; updates raw slot COPIES of the pointers.
    ;; Multiple-value return buffer: MV-COUNT (tagged fixnum) at 0x10000090,
    ;; the (count-1) "extra" values at 0x10000098, 0x100000A0... .  These
    ;; words can hold heap pointers (a cons/string/symbol returned as a
    ;; secondary value) that %values-list / multiple-value-* read AFTER an
    ;; allocating step — e.g. %values-list conses each element, and the
    ;; cons alloc can trigger GC mid-read, stranding the not-yet-read
    ;; extras.  Scan exactly the live extras: count-1 words from 0x10000098,
    ;; only when count>=2.  RDI = slot addr, R10 = remaining (both survive
    ;; scan_word/copy_object, which preserve RDI/R10).
    (let ((mv-loop (make-label))
          (mv-done (make-label)))
      ;; R10 = (mem[0x10000090] >> 1) - 1  =  number of extra values
      (emit-mov-reg-imm buf 'rax #x10000090)
      (emit-mov-reg-mem buf 'r10 'rax 0)         ; r10 = tagged count
      (emit-shr-reg-imm buf 'r10 1)              ; untag -> raw count
      (emit-sub-reg-imm buf 'r10 1)              ; r10 = count - 1 (extras)
      ;; if extras <= 0, nothing to scan (SUB sets SF/ZF: jle when <=0)
      (emit-cmp-reg-imm buf 'r10 0)
      (emit-jcc buf :le mv-done)
      ;; RDI = 0x10000098 (first extra value slot)
      (emit-mov-reg-imm buf 'rdi #x10000098)
      (emit-label buf mv-loop)
      (emit-mov-reg-reg buf 'rax 'rdi)           ; rax = slot addr
      (emit-call buf scan-word-label)
      (emit-add-reg-imm buf 'rdi 8)              ; next slot
      (emit-sub-reg-imm buf 'r10 1)              ; remaining--
      (emit-cmp-reg-imm buf 'r10 0)
      (emit-jcc buf :g mv-loop)
      (emit-label buf mv-done))
    (emit-gc-dbg-char buf #x72)          ; 'r' — roots scan done (globals+kw+pkg+mv)

    ;; ---- Cheney scan loop ----
    ;; R10 = scan pointer (starts at to_start)
    (emit-bytes buf #x4C #x8B #x14 #x25)         ; mov r10, [abs32]
    (emit-u32 buf #x10000048)                     ; r10 = to_start

    (let ((cheney-loop (make-label))
          (cheney-done (make-label)))
      (emit-label buf cheney-loop)
      ;; scan >= free_ptr? done
      (emit-cmp-reg-reg buf 'r10 'r13)
      (emit-jcc buf :ae cheney-done)
      ;; Scan the word at [r10]
      (emit-bytes buf #x4C #x89 #xD0)            ; mov rax, r10
      (emit-call buf scan-word-label)
      (emit-add-reg-imm buf 'r10 8)
      (emit-jmp buf cheney-loop)
      (emit-label buf cheney-done))
    (emit-gc-dbg-char buf #x63)          ; 'c' — cheney scan done

    ;; ---- Swap semispaces ----
    ;; new from_start = old to_start
    (emit-bytes buf #x48 #x8B #x04 #x25)         ; mov rax, [0x10000048]
    (emit-u32 buf #x10000048)
    (emit-bytes buf #x48 #x89 #x04 #x25)         ; mov [0x10000040], rax
    (emit-u32 buf #x10000040)
    ;; new to_start = old from_start (in RBX)
    (emit-bytes buf #x48 #x89 #x1C #x25)         ; mov [0x10000048], rbx
    (emit-u32 buf #x10000048)

    ;; ---- Update R12 and R14 ----
    ;; R12 = free_ptr (R13)
    (emit-bytes buf #x4D #x89 #xEC)              ; mov r12, r13
    ;; R14 = new from_start + space_size
    ;; new from_start was old to_start, now at [0x10000040]
    (emit-bytes buf #x48 #x8B #x04 #x25)         ; mov rax, [0x10000040]
    (emit-u32 buf #x10000040)
    (emit-bytes buf #x48 #x03 #x04 #x25)         ; add rax, [0x10000050]
    (emit-u32 buf #x10000050)
    (emit-bytes buf #x49 #x89 #xC6)              ; mov r14, rax

    ;; ---- MCGC point (c): clear the reclaimed from-space's object-start bitmap ----
    ;; RBX = old from_start: the semispace just fully evacuated by this
    ;; collection.  It is now dead (everything live was copied out) and about to
    ;; sit idle as to_start until the NEXT collection copies survivors into it.
    ;; Its object-start bits are stale.  Clear them now so that — two collections
    ;; hence, when this region is copied into and then scanned as from-space —
    ;; only CURRENT-generation object starts validate in scan_word.  Without this
    ;; the bitmap monotonically saturates (every granule that EVER held a start
    ;; stays set), and the conservative-root gate decays toward a no-op, slowly
    ;; re-admitting the false-positive forward-stamp corruption it exists to stop.
    ;; With it, each region's bitmap at scan time is exactly {survivors copied in
    ;; by the preceding GC} ∪ {allocations since} — sharp forever.
    ;;
    ;; Byte-exact REP STOSB (not STOSQ): the two semispaces' bitmap sub-ranges
    ;; are byte-adjacent at space_size/128, which is NOT 8-aligned, so a qword
    ;; clear would bleed into the sibling semispace's still-live bits.
    ;;   dest  = bitmap_base + (rbx - page_base) >> 7
    ;;   count = space_size >> 7  bytes   (1 bit / 16-byte granule, /8 = /128)
    ;; RAX/RCX/RDI were all pushed at trampoline entry and are restored below.
    (when (mcgc-collector-on-p)
      (emit-bytes buf #xFC)                          ; cld (forward STOS)
      (emit-mov-reg-reg buf 'rax 'rbx)               ; rax = old from_start
      (emit-bytes buf #x48 #x2B #x04 #x25)           ; sub rax, [page_base]
      (emit-u32 buf +mcgc-cfg-page-base-addr+)
      (emit-shr-reg-imm buf 'rax 7)                  ; rax = byte offset into bitmap
      (emit-bytes buf #x48 #x8B #x3C #x25)           ; mov rdi, [bitmap_base]
      (emit-u32 buf +mcgc-cfg-bitmap-addr+)
      (emit-bytes buf #x48 #x01 #xC7)                ; add rdi, rax  (rdi = dest)
      (emit-bytes buf #x48 #x8B #x0C #x25)           ; mov rcx, [space_size]
      (emit-u32 buf #x10000050)
      (emit-shr-reg-imm buf 'rcx 7)                  ; rcx = byte count = space_size/128
      (emit-bytes buf #x31 #xC0)                     ; xor eax, eax  (AL = fill 0)
      (emit-bytes buf #xF3 #xAA))                    ; rep stosb
    ;; Clear the CONS-KIND bitmap for the SAME reclaimed range.  Without this
    ;; a granule reused cons->object across cycles keeps a stale cons-kind
    ;; bit, and scan_word would then REJECT the object's real obj-tagged root
    ;; (kind=cons mismatch) → dangling.  Separately gated (Linux-x64 only)
    ;; from the start-bitmap clear above.  dest = bitmap_base + kind_delta +
    ;; (rbx-page_base)>>7.
    (when (mcgc-kind-bitmap-on-p)
      (emit-bytes buf #xFC)                          ; cld
      (emit-mov-reg-reg buf 'rax 'rbx)               ; rax = old from_start
      (emit-bytes buf #x48 #x2B #x04 #x25)           ; sub rax, [page_base]
      (emit-u32 buf +mcgc-cfg-page-base-addr+)
      (emit-shr-reg-imm buf 'rax 7)                  ; byte offset into bitmap
      (emit-bytes buf #x48 #x8B #x3C #x25)           ; mov rdi, [bitmap_base]
      (emit-u32 buf +mcgc-cfg-bitmap-addr+)
      (emit-bytes buf #x48 #x81 #xC7)                ; add rdi, imm32 (kind delta)
      (emit-u32 buf +mcgc-kindbitmap-delta+)
      (emit-bytes buf #x48 #x01 #xC7)                ; add rdi, rax  (rdi = dest)
      (emit-bytes buf #x48 #x8B #x0C #x25)           ; mov rcx, [space_size]
      (emit-u32 buf #x10000050)
      (emit-shr-reg-imm buf 'rcx 7)                  ; byte count = space_size/128
      (emit-bytes buf #x31 #xC0)                     ; xor eax, eax
      (emit-bytes buf #xF3 #xAA))                    ; rep stosb

    ;; ---- Increment GC count ----
    (emit-bytes buf #x48 #xFF #x04 #x25)          ; inc qword [0x10000060]
    (emit-u32 buf #x10000060)

    ;; ---- Restore registers ----
    (emit-jmp buf restore-label)

    ;; ===========================================================
    ;; SUBROUTINE: scan_word
    ;; Input: RAX = address of the 8-byte word to scan
    ;; Uses: RAX, RSI (temp), preserves RBX, RCX, R13
    ;; If the word is a heap pointer into from-space, copy the object
    ;; and update the word with the new pointer.
    ;; ===========================================================
    (emit-label buf scan-word-label)
    (emit-push buf 'rdx)                          ; save RDX (stack scan uses it for stack_base)
    (emit-push buf 'rax)                          ; save word address
    (let ((sw-not-ptr (make-label))
          (sw-done (make-label))
          (sw-is-cons (make-label))
          (sw-is-obj (make-label)))
      ;; Load the actual value at [rax]
      (emit-mov-reg-mem buf 'rsi 'rax 0)          ; rsi = [rax] = the value
      ;; Check tag: low 4 bits
      (emit-mov-reg-reg buf 'rax 'rsi)
      (emit-and-reg-imm buf 'rax #x0F)           ; rax = tag
      ;; Is it a cons (tag = 0x01)?
      (emit-cmp-reg-imm buf 'rax 1)
      (emit-jcc buf :e sw-is-cons)
      ;; Is it an object (tag = 0x09)?
      (emit-cmp-reg-imm buf 'rax 9)
      (emit-jcc buf :e sw-is-obj)
      ;; Not a pointer — skip
      (emit-jmp buf sw-not-ptr)

      ;; ---- Cons pointer ----
      (emit-label buf sw-is-cons)
      ;; Check if in from-space: from_start <= (rsi & ~0xF) < from_end
      (emit-mov-reg-reg buf 'rax 'rsi)
      (emit-and-reg-imm buf 'rax -16)            ; strip tag bits
      (emit-cmp-reg-reg buf 'rax 'rbx)           ; < from_start?
      (emit-jcc buf :b sw-not-ptr)
      (emit-cmp-reg-reg buf 'rax 'rcx)           ; >= from_end?
      (emit-jcc buf :ae sw-not-ptr)
      ;; MCGC: reject unless RAX is a recorded object start.
      (when (mcgc-collector-on-p)
        (emit-mcgc-validate-or-jump buf 'rax sw-not-ptr)
        ;; ...AND unless that start is actually a CONS.  A conservative
        ;; scratch word holding object_base|1 (cons tag on an object's base)
        ;; passes the start gate but must NOT be copied as a 16-byte cons —
        ;; that truncates the object and strands its real obj-tagged ref.
        (when (mcgc-kind-check-on-p)
          (emit-mcgc-cons-kind-or-jump buf 'rax t sw-not-ptr)))
      ;; In from-space. RSI = tagged cons ptr. Call copy_object.
      (emit-mov-reg-reg buf 'rax 'rsi)
      (emit-call buf copy-label)
      ;; RAX = new tagged pointer. Update the stack/heap word.
      (emit-pop buf 'rsi)                         ; rsi = original word address
      (emit-mov-mem-reg buf 'rsi 'rax 0)          ; [word_addr] = new ptr
      (emit-push buf 'rsi)                        ; keep stack balanced for sw-done
      (emit-jmp buf sw-done)

      ;; ---- Object pointer ----
      (emit-label buf sw-is-obj)
      ;; Check if in from-space
      (emit-mov-reg-reg buf 'rax 'rsi)
      (emit-and-reg-imm buf 'rax -16)            ; strip tag bits
      (emit-cmp-reg-reg buf 'rax 'rbx)           ; < from_start?
      (emit-jcc buf :b sw-not-ptr)
      (emit-cmp-reg-reg buf 'rax 'rcx)           ; >= from_end?
      (emit-jcc buf :ae sw-not-ptr)
      ;; MCGC: reject unless RAX is a recorded object start (same gate as the
      ;; cons path).  The Cheney scan walks copied objects' payload FLATLY and
      ;; calls scan_word on every word; a payload word that coincidentally
      ;; carries object tag 9 and lands in from-space would otherwise be
      ;; "copied" as a phantom object.  Every real object start has its bit set
      ;; (alloc sites in stage 2 + set-copy-bit on survivors), so this only
      ;; rejects false positives.
      (when (mcgc-collector-on-p)
        (emit-mcgc-validate-or-jump buf 'rax sw-not-ptr)
        ;; ...AND unless that start is actually an OBJECT (cons-kind bit
        ;; clear).  Symmetric to the cons path: a scratch word holding
        ;; cons_base|9 (object tag on a cons's base) must not be copied as a
        ;; variable-size object — it would read a cons car as a header.
        (when (mcgc-kind-check-on-p)
          (emit-mcgc-cons-kind-or-jump buf 'rax nil sw-not-ptr)))
      ;; In from-space. Call copy_object.
      (emit-mov-reg-reg buf 'rax 'rsi)
      (emit-call buf copy-label)
      ;; Update the word
      (emit-pop buf 'rsi)
      (emit-mov-mem-reg buf 'rsi 'rax 0)
      (emit-push buf 'rsi)
      (emit-jmp buf sw-done)

      ;; Not a pointer or not in from-space
      (emit-label buf sw-not-ptr)
      (emit-label buf sw-done)
      (emit-pop buf 'rax)                         ; discard saved word address
      (emit-pop buf 'rdx)                         ; restore RDX (stack_base for caller)
      (emit-ret buf))

    ;; ===========================================================
    ;; SUBROUTINE: copy_object
    ;; Input: RAX = tagged pointer (cons or object) in from-space
    ;; Output: RAX = new tagged pointer in to-space
    ;; Side effect: R13 (free ptr) advanced, forwarding ptr left in from-space
    ;; Preserves: RBX, RCX, RDI, R10, RBP
    ;; Clobbers: RAX, RSI, RDX, R8
    ;; ===========================================================
    (emit-label buf copy-label)
    (let ((copy-cons (make-label))
          (copy-obj (make-label))
          (copy-fwd (make-label))
          (copy-bogus (make-label))
          (copy-done (make-label)))
      ;; Determine type from tag
      (emit-mov-reg-reg buf 'rdx 'rax)           ; rdx = tagged ptr
      (emit-and-reg-imm buf 'rax #x0F)
      (emit-cmp-reg-imm buf 'rax 1)
      (emit-jcc buf :e copy-cons)
      ;; Must be object (tag 9)
      (emit-jmp buf copy-obj)

      ;; ---- Copy cons ----
      (emit-label buf copy-cons)
      ;; RDX = tagged cons ptr. Raw addr = rdx & ~0xF (same as rdx-1 for aligned ptrs)
      (emit-mov-reg-reg buf 'rsi 'rdx)
      (emit-and-reg-imm buf 'rsi -16)            ; rsi = raw addr of cons
      ;; Check if already forwarded: [rsi] has tag 0xF?
      (emit-mov-reg-mem buf 'rax 'rsi 0)          ; rax = car word
      (emit-mov-reg-reg buf 'r8 'rax)
      (emit-and-reg-imm buf 'r8 #x0F)
      (emit-cmp-reg-imm buf 'r8 #x0F)
      (emit-jcc buf :e copy-fwd)
      ;; Not forwarded. Copy 16 bytes to free_ptr (R13)
      ;; Copy car
      (emit-mov-reg-mem buf 'rax 'rsi 0)          ; rax = [rsi+0] = car
      (emit-bytes buf #x49 #x89 #x45 #x00)       ; mov [r13+0], rax
      ;; Copy cdr
      (emit-mov-reg-mem buf 'rax 'rsi 8)          ; rax = [rsi+8] = cdr
      (emit-bytes buf #x49 #x89 #x45 #x08)       ; mov [r13+8], rax
      ;; New tagged pointer = r13 | 1 (cons tag)
      (emit-bytes buf #x4C #x89 #xE8)            ; mov rax, r13
      (emit-or-reg-imm buf 'rax 1)               ; rax = r13 | 1
      ;; Leave forwarding pointer in from-space: [rsi] = r13 | 0xF
      (emit-bytes buf #x4C #x89 #xEA)            ; mov rdx, r13
      (emit-or-reg-imm buf 'rdx #x0F)            ; rdx = r13 | 0xF
      (emit-mov-mem-reg buf 'rsi 'rdx 0)          ; [rsi] = forward ptr
      ;; MCGC: set the object-start bit for the survivor at its NEW
      ;; (to-space) location (R13 = raw dest start, not yet advanced) so
      ;; the next collection — when to-space becomes from-space — validates
      ;; this object as a real start.  R13 is not RAX/RDX/R8, and the helper
      ;; saves/restores RAX/RDX/R8 (RAX=new tagged ptr, RDX=forward word).
      (when (mcgc-collector-on-p)
        (emit-mcgc-set-copy-bit buf 'r13)
        ;; This survivor is a CONS — mark its kind bit so the next GC's
        ;; scan_word accepts cons-tagged refs to it and rejects object-tagged
        ;; ones.  (Object survivors leave the kind bit 0; the bitmap is
        ;; cleared for the reclaimed region in point-(c) each cycle.)
        (emit-mcgc-set-cons-bit-copy buf 'r13))
      ;; Advance free pointer
      (emit-add-reg-imm buf 'r13 16)
      (emit-jmp buf copy-done)

      ;; ---- Already forwarded ----
      (emit-label buf copy-fwd)
      ;; RAX = forwarding word = new_addr | 0xF
      ;; Extract new addr and apply original tag
      (emit-and-reg-imm buf 'rax -16)            ; strip forward tag
      ;; RDX still has original tagged ptr. Extract tag from it.
      (emit-mov-reg-reg buf 'r8 'rdx)
      (emit-and-reg-imm buf 'r8 #x0F)            ; r8 = original tag
      (emit-or-reg-reg buf 'rax 'r8)             ; rax = new_addr | orig_tag
      (emit-jmp buf copy-done)

      ;; ---- Copy object ----
      (emit-label buf copy-obj)
      ;; RDX = tagged object ptr. Raw addr = rdx & ~0xF
      (emit-mov-reg-reg buf 'rsi 'rdx)
      (emit-and-reg-imm buf 'rsi -16)            ; rsi = raw addr of object
      ;; Check if already forwarded
      (emit-mov-reg-mem buf 'rax 'rsi 0)          ; rax = header word
      (emit-mov-reg-reg buf 'r8 'rax)
      (emit-and-reg-imm buf 'r8 #x0F)
      (emit-cmp-reg-imm buf 'r8 #x0F)
      (emit-jcc buf :e copy-fwd)
      ;; Not forwarded. Read element count from header.
      ;; Header = [subtag:8][unused:7][element-count:49]
      ;; Element count = header >> 8
      (emit-mov-reg-reg buf 'r8 'rax)            ; r8 = header
      (emit-shr-reg-imm buf 'r8 8)               ; r8 = element count
      ;; Total size = (count + 2) * 8, aligned to 16
      (emit-add-reg-imm buf 'r8 2)               ; count + 2
      (emit-shl-reg-imm buf 'r8 3)               ; * 8
      (emit-add-reg-imm buf 'r8 15)              ; + 15
      (emit-and-reg-imm buf 'r8 -16)             ; & ~15 (align to 16)
      ;; ---- Conservative-scan sanity guard (CRITICAL) ----
      ;; The stack-root scan is conservative: it treats ANY stack word with
      ;; low nibble 1/9 that lands in [from_start, from_end) as a heap
      ;; pointer.  Dead stack slots (garbage left by prior deeper calls), and
      ;; the uninitialised payload words of fresh make-string/make-array
      ;; objects scanned during the Cheney phase, can coincidentally satisfy
      ;; this and point at a NON-object.  Its first word, read as a "header",
      ;; yields a bogus element count; the old code then `rep movsq`'d that
      ;; bogus size, either running off to-space (re-entering GC / corrupting
      ;; the trampoline's return chain — gdb-free repro `[ p X [ ! p s r c ]`)
      ;; or silently double-copying and corrupting a live root on the 2nd GC
      ;; (held let-vars vanished, the gauntlet's define-package reexport died
      ;; mid-loop).
      ;;
      ;; INVARIANT: a real object lives ENTIRELY within from-space, i.e.
      ;;   from_start <= raw_addr  (already checked by scan_word) AND
      ;;   raw_addr + size <= from_end (RCX).
      ;; This single bound subsumes the "size <= space_size" check (any
      ;; in-from-space object with a sane end has a sane size) and also
      ;; catches the absurd-size case (raw_addr + huge_size overruns RCX, no
      ;; 64-bit wrap since raw_addr ~2^47 and size <= 2^52).  If the bound
      ;; fails this is NOT a real object: bail via copy-bogus, returning the
      ;; original tagged ptr unchanged (scan_word rewrites the dead slot to
      ;; itself — a no-op; the from-space bytes are untouched).
      ;; RAX is free here (header already consumed into R8), so no spill:
      (emit-mov-reg-reg buf 'rax 'rsi)           ; rax = raw addr
      (emit-add-reg-reg buf 'rax 'r8)            ; rax = raw addr + size
      (emit-cmp-reg-reg buf 'rax 'rcx)           ; raw_addr+size vs from_end
      (emit-jcc buf :a copy-bogus)               ; > from_end? not a real object — bail
      ;; Copy R8 bytes from RSI to R13 using REP MOVSQ
      ;; Save RDI and RCX (used by caller for stack scan / from_end)
      (emit-push buf 'rdi)
      (emit-push buf 'rcx)
      ;; Save old R13 (start of dest) for new tagged pointer
      (emit-push buf 'r13)
      ;; MCGC: set the object-start bit for the survivor at its NEW dest
      ;; (R13 = raw dest start, still pristine before REP MOVSQ advances
      ;; RDI).  R13 is not RAX/RDX/R8; the helper saves/restores RAX/RDX/R8
      ;; (R8 = size, RDX = tagged source) and leaves RDI/RCX untouched.
      (when (mcgc-collector-on-p)
        (emit-mcgc-set-copy-bit buf 'r13))
      ;; Set up REP MOVSQ: RSI=source, RDI=dest, RCX=count
      (emit-bytes buf #x4C #x89 #xEF)            ; mov rdi, r13 (dest)
      (emit-mov-reg-reg buf 'rcx 'r8)
      (emit-shr-reg-imm buf 'rcx 3)              ; count in qwords
      ;; RSI = source (already set)
      (emit-bytes buf #xF3 #x48 #xA5)            ; rep movsq
      ;; R13 = RDI (new free pointer, after copied data)
      (emit-bytes buf #x49 #x89 #xFD)            ; mov r13, rdi
      ;; Restore old R13 into RAX for new tagged pointer
      (emit-pop buf 'rax)                          ; rax = old r13 = dest start
      (emit-or-reg-imm buf 'rax 9)               ; tag as object
      ;; Restore RSI to point back to source for forwarding ptr
      ;; RSI was advanced by REP MOVSQ, restore from RDX
      (emit-mov-reg-reg buf 'rsi 'rdx)
      (emit-and-reg-imm buf 'rsi -16)            ; rsi = raw source addr again
      ;; Leave forwarding pointer: [rsi] = dest_start | 0xF
      (emit-mov-reg-reg buf 'rdx 'rax)           ; rdx = new tagged ptr (addr | 9)
      (emit-and-reg-imm buf 'rdx -16)            ; strip object tag
      (emit-or-reg-imm buf 'rdx #x0F)            ; add forward tag
      (emit-mov-mem-reg buf 'rsi 'rdx 0)          ; [old_addr] = forward ptr
      ;; Restore caller's RCX and RDI
      (emit-pop buf 'rcx)
      (emit-pop buf 'rdi)
      (emit-jmp buf copy-done)

      ;; ---- Bogus object: not a real heap object (conservative-scan false
      ;; positive).  Return the original tagged ptr unchanged in RAX so the
      ;; caller (scan_word) rewrites the dead stack slot to itself.  No copy,
      ;; no forwarding-pointer write — the from-space bytes are untouched. ----
      (emit-label buf copy-bogus)
      (emit-mov-reg-reg buf 'rax 'rdx)            ; rax = original tagged ptr
      (emit-label buf copy-done)
      ;; RAX = new tagged pointer
      (emit-ret buf))

    ;; ---- Restore label ----
    (emit-label buf restore-label)
    ;; RSP should equal RBP (saved after all pushes). Force it for safety.
    (emit-mov-reg-reg buf 'rsp 'rbp)
    (emit-pop buf 'rbp)
    (emit-pop buf 'r13)
    (emit-pop buf 'r11)
    (emit-pop buf 'r10)
    (emit-pop buf 'rdx)
    (emit-pop buf 'rcx)
    (emit-pop buf 'rbx)
    (emit-pop buf 'r9)
    (emit-pop buf 'r8)
    (emit-pop buf 'rdi)
    (emit-pop buf 'rsi)
    (emit-pop buf 'rax)
    (unless *x64-linux-mode*
      ;; BARE-METAL: clear the in-transition flag — heap consistent again.
      (emit-bytes buf #x48 #xC7 #x04 #x25)         ; mov qword [imm32], 0
      (emit-u32 buf #x10000D28)
      (emit-u32 buf 0))
    (emit-gc-dbg-char buf #x5D)          ; ']' — trampoline exit (after restore)
    (emit-ret buf)))

;;; ============================================================
;;; MCGC stage-4 page-collector helper emitters
;;; ============================================================

(defun emit-mov-reg-abs (buf reg slot)
  "REG = qword [SLOT] (absolute 32-bit address)."
  ;; REX.W (+ REX.R if reg is r8-r15) ; opcode 8B ; modrm reg=reg, rm=100 (SIB) ; SIB 25 ; disp32
  (emit-byte buf (logior #x48 (if (reg-extended-p reg) #x04 0)))
  (emit-byte buf #x8B)
  (emit-byte buf (logior #x04 (ash (logand (reg-code reg) 7) 3)))
  (emit-byte buf #x25)
  (emit-u32 buf slot))

(defun emit-mov-abs-reg (buf slot reg)
  "qword [SLOT] = REG (absolute 32-bit address)."
  (emit-byte buf (logior #x48 (if (reg-extended-p reg) #x04 0)))
  (emit-byte buf #x89)
  (emit-byte buf (logior #x04 (ash (logand (reg-code reg) 7) 3)))
  (emit-byte buf #x25)
  (emit-u32 buf slot))

(defun emit-mov-abs-imm32 (buf slot imm)
  "qword [SLOT] = sign-extended imm32."
  (emit-bytes buf #x48 #xC7 #x04 #x25)
  (emit-u32 buf slot)
  (emit-u32 buf imm))

(defun emit-mcgc-fill-descriptor-range (buf start-addr-reg end-addr-reg val zero-reg)
  "Set descriptor[page] = VAL (a byte 0/1/2) for every page whose data byte is
   in [START-ADDR-REG, END-ADDR-REG).  Clobbers RAX, RCX, RDI.  START/END
   read-only via copy.  The descriptor array is one byte per page, indexed by
   page = (addr - page_base) >> 12, so a contiguous data range maps to a
   CONTIGUOUS descriptor byte range [descriptor_base+start_page,
   descriptor_base+end_page) — filled with a single REP STOSB (O(pages) bytes,
   but ~100x faster than a per-page interpreted loop)."
  (declare (ignore zero-reg))
  ;; rax = start_page = (start - page_base) >> 12
  (emit-mov-reg-reg buf 'rax start-addr-reg)
  (emit-bytes buf #x48 #x2B #x04 #x25)             ; sub rax, [page_base]
  (emit-u32 buf +mcgc-cfg-page-base-addr+)
  (emit-shr-reg-imm buf 'rax +mcgc-page-shift+)
  ;; rcx = end_page = (end - page_base) >> 12  ; then rcx = count = end_page - start_page
  (emit-mov-reg-reg buf 'rcx end-addr-reg)
  (emit-bytes buf #x48 #x2B #x0C #x25)             ; sub rcx, [page_base]
  (emit-u32 buf +mcgc-cfg-page-base-addr+)
  (emit-shr-reg-imm buf 'rcx +mcgc-page-shift+)
  (emit-sub-reg-reg buf 'rcx 'rax)                 ; rcx = page count
  ;; rdi = descriptor_base + start_page
  (emit-mov-reg-abs buf 'rdi +mcgc-cfg-descriptor-addr+)
  (emit-add-reg-reg buf 'rdi 'rax)
  ;; al = VAL ; rep stosb
  (emit-bytes buf #xFC)                            ; cld
  (emit-bytes buf #xB0 (logand val #xFF))          ; mov al, imm8
  (emit-bytes buf #xF3 #xAA))                      ; rep stosb

(defun emit-mcgc-clear-bitmap-range (buf start-addr-reg end-addr-reg)
  "Byte-clear the object-start bitmap covering data range [START,END).
   bitmap byte offset of addr A = (A - page_base) >> 7  (1 bit / 16 bytes).
   Uses REP STOSB.  Clobbers RAX, RCX, RDI.  Range endpoints are page-aligned
   (multiples of 4096), so (A-page_base)>>7 is a whole byte and the two
   semispace-half bitmaps never share a byte at a page boundary."
  ;; rdi = bitmap_base + ((start - page_base) >> 7)
  (emit-mov-reg-reg buf 'rax start-addr-reg)
  (emit-bytes buf #x48 #x2B #x04 #x25)            ; sub rax, [page_base]
  (emit-u32 buf +mcgc-cfg-page-base-addr+)
  (emit-shr-reg-imm buf 'rax 7)                   ; byte offset into bitmap
  (emit-mov-reg-abs buf 'rdi +mcgc-cfg-bitmap-addr+)
  (emit-add-reg-reg buf 'rdi 'rax)                ; rdi = dest
  ;; rcx = (end - start) >> 7  bytes
  (emit-mov-reg-reg buf 'rcx end-addr-reg)
  (emit-sub-reg-reg buf 'rcx start-addr-reg)
  (emit-shr-reg-imm buf 'rcx 7)
  (emit-bytes buf #xFC)                            ; cld
  (emit-bytes buf #x31 #xC0)                       ; xor eax, eax
  (emit-bytes buf #xF3 #xAA))                      ; rep stosb

;;; ============================================================
;;; MCGC stage-4c/4d page-PINNING helper emitters
;;; ============================================================

(defun emit-mcgc-pincount-init (buf)
  "Lazily compute the per-page PERSISTENT pin-count array base and store it to
   +mcgc-cfg-pincount-addr+.  The array lives in the metadata region just past
   the run-free-list:  pincount_base = freelist_base + page_count*4.  (The boot
   stub MAP_ANON-zeroes the whole metadata region, so the array reads 0 until a
   %pin-object writes it.)  Idempotent via +mcgc-cfg-pin-init-addr+.  Clobbers
   RAX/RCX/RDX — call it where those are dead (collector lazy-init, %pin entry)."
  (let ((done (make-label)))
    (emit-mov-reg-abs buf 'rax +mcgc-cfg-pin-init-addr+)
    (emit-bytes buf #x48 #x85 #xC0)                 ; test rax, rax
    (emit-jcc buf :ne done)
    ;; rax = freelist_base ; rcx = page_count ; rax += rcx*4
    (emit-mov-reg-abs buf 'rax +mcgc-cfg-freelist-base-addr+)
    (emit-mov-reg-abs buf 'rcx +mcgc-cfg-page-count-addr+)
    (emit-shl-reg-imm buf 'rcx 2)                   ; page_count * 4 (u32 each)
    (emit-add-reg-reg buf 'rax 'rcx)
    (emit-mov-abs-reg buf +mcgc-cfg-pincount-addr+ 'rax)
    (emit-mov-abs-imm32 buf +mcgc-cfg-pin-init-addr+ 1)
    (emit-label buf done)))

(defun emit-mcgc-mark-page-pinned (buf raw-addr-reg)
  "Set the transient pinned bit (bit2) in descriptor[page(RAW-ADDR-REG)].
   RAW-ADDR-REG holds a tag-STRIPPED data address inside the region.  Reads
   descriptor byte, ORs in +mcgc-desc-pinned-bit+, writes back.  Uses RAX/RDX
   as temps (saved/restored); RAW-ADDR-REG must not be RAX/RDX."
  (when (member raw-addr-reg '(rax rdx))
    (error "emit-mcgc-mark-page-pinned: RAW-ADDR-REG must not be RAX/RDX"))
  (emit-gc-dbg-char buf #x4D)          ; 'M' mark-page-pinned [DEBUG]
  (emit-push buf 'rax)
  (emit-push buf 'rdx)
  ;; rdx = page index = (addr - page_base) >> 12
  (emit-mov-reg-reg buf 'rdx raw-addr-reg)
  (emit-bytes buf #x48 #x2B #x14 #x25)             ; sub rdx, [page_base]
  (emit-u32 buf +mcgc-cfg-page-base-addr+)
  (emit-shr-reg-imm buf 'rdx +mcgc-page-shift+)
  ;; rax = descriptor_base ; descriptor byte addr = rax + rdx
  (emit-mov-reg-abs buf 'rax +mcgc-cfg-descriptor-addr+)
  (emit-add-reg-reg buf 'rax 'rdx)                 ; rax = &descriptor[page]
  ;; [rax] |= 4   (or byte ptr [rax], 4)
  (emit-bytes buf #x80 #x08 +mcgc-desc-pinned-bit+) ; or byte [rax], imm8
  (emit-pop buf 'rdx)
  (emit-pop buf 'rax))

(defun emit-mcgc-page-pinned-or-jump (buf raw-addr-reg target-label)
  "If the PAGE containing RAW-ADDR-REG (tag-stripped data addr) is pinned —
   either the transient descriptor bit2 is set OR its persistent pin-count>0 —
   JUMP to TARGET-LABEL.  Otherwise fall through.  Used in scan_word: a field
   whose target page is pinned must KEEP its address (no forward).  Uses RDX/R8
   as temps (saved/restored), preserves everything else incl. RAW-ADDR-REG.
   RAW-ADDR-REG must not be RDX/R8."
  (when (member raw-addr-reg '(rdx r8))
    (error "emit-mcgc-page-pinned-or-jump: RAW-ADDR-REG must not be RDX/R8"))
  (emit-push buf 'rdx)
  (emit-push buf 'r8)
  ;; rdx = page index
  (emit-mov-reg-reg buf 'rdx raw-addr-reg)
  (emit-bytes buf #x48 #x2B #x14 #x25)             ; sub rdx, [page_base]
  (emit-u32 buf +mcgc-cfg-page-base-addr+)
  (emit-shr-reg-imm buf 'rdx +mcgc-page-shift+)
  ;; transient: r8 = descriptor_base ; test byte [r8+rdx], 4
  (emit-mov-reg-abs buf 'r8 +mcgc-cfg-descriptor-addr+)
  (emit-bytes buf #x41 #xF6 #x04 #x10 +mcgc-desc-pinned-bit+) ; test byte [r8+rdx], 4
  ;; restore temps BEFORE the jump so the target sees a clean stack
  (let ((pinned (make-label)) (after (make-label)))
    (emit-jcc buf :ne pinned)
    ;; persistent: r8 = pincount_base ; cmp dword [r8 + rdx*4], 0 ; ja pinned
    (emit-mov-reg-abs buf 'r8 +mcgc-cfg-pincount-addr+)
    (emit-bytes buf #x41 #x83 #x3C #x90 #x00)      ; cmp dword [r8+rdx*4], 0
    (emit-jcc buf :ne pinned)
    ;; not pinned
    (emit-pop buf 'r8)
    (emit-pop buf 'rdx)
    (emit-jmp buf after)
    (emit-label buf pinned)
    (emit-pop buf 'r8)
    (emit-pop buf 'rdx)
    (emit-jmp buf target-label)
    (emit-label buf after)))

(defun emit-mcgc-not-copyable-or-jump (buf raw-addr-reg target-label)
  "Whole-region from-space test for scan_word.  A heap data address is COPYABLE
   (a from-space object that should be forwarded) iff its page descriptor byte
   == 1 EXACTLY:
     0 = free        (not a live object)         -> keep addr (jump)
     1 = live        (from-space)                -> COPYABLE (fall through)
     3 = to-run      (copy destination)          -> keep addr (jump)
     5 = live+pinned (descriptor|=4 this GC)     -> keep addr (jump)
   So: if descriptor[page] != 1, JUMP to TARGET-LABEL (keep address, no copy).
   This single byte compare subsumes the from-range + pinned + to-run checks.
   Uses RDX/R8 as temps (saved/restored); RAW-ADDR-REG must not be RDX/R8."
  (when (member raw-addr-reg '(rdx r8))
    (error "emit-mcgc-not-copyable-or-jump: RAW-ADDR-REG must not be RDX/R8"))
  (emit-push buf 'rdx)
  (emit-push buf 'r8)
  ;; rdx = page index = (addr - page_base) >> 12
  (emit-mov-reg-reg buf 'rdx raw-addr-reg)
  (emit-bytes buf #x48 #x2B #x14 #x25)             ; sub rdx, [page_base]
  (emit-u32 buf +mcgc-cfg-page-base-addr+)
  (emit-shr-reg-imm buf 'rdx +mcgc-page-shift+)
  ;; r8 = descriptor_base ; cmp byte [r8+rdx], 1
  (emit-mov-reg-abs buf 'r8 +mcgc-cfg-descriptor-addr+)
  (emit-bytes buf #x41 #x80 #x3C #x10 #x01)        ; cmp byte [r8+rdx], 1
  (let ((copyable (make-label)) (after (make-label)))
    (emit-jcc buf :e copyable)                     ; ==1 -> copyable, fall through
    (emit-pop buf 'r8)
    (emit-pop buf 'rdx)
    (emit-jmp buf target-label)                    ; !=1 -> keep address
    (emit-label buf copyable)
    (emit-pop buf 'r8)
    (emit-pop buf 'rdx)
    (emit-label buf after)))

(defun emit-mcgc-count-state-dbg (buf val ch)
  "DEBUG: emit CH once per 8192 pages whose descriptor byte == VAL."
  (when *x64-gc-debug*
    (emit-push buf 'rax) (emit-push buf 'rdx) (emit-push buf 'rdi) (emit-push buf 'r8) (emit-push buf 'r9)
    (let ((kl (make-label)) (kd (make-label)) (ks (make-label)) (ke (make-label)))
      (emit-bytes buf #x48 #x31 #xFF)            ; xor rdi,rdi (page idx)
      (emit-bytes buf #x4D #x31 #xC9)            ; xor r9,r9 (match count)
      (emit-mov-reg-abs buf 'rdx +mcgc-cfg-page-count-addr+)
      (emit-mov-reg-abs buf 'r8 +mcgc-cfg-descriptor-addr+)
      (emit-label buf kl)
      (emit-cmp-reg-reg buf 'rdi 'rdx) (emit-jcc buf :ae kd)
      (emit-bytes buf #x41 #x8A #x04 #x38)       ; mov al, [r8+rdi]
      (emit-bytes buf #x3C (logand val #xFF))    ; cmp al, val
      (emit-jcc buf :ne ks)
      (emit-bytes buf #x49 #xFF #xC1)            ; inc r9
      (emit-bytes buf #x49 #x81 #xF9 #x00 #x20 #x00 #x00) ; cmp r9, 8192
      (emit-jcc buf :ne ks)
      (emit-bytes buf #x4D #x31 #xC9)            ; xor r9,r9
      (emit-gc-dbg-char buf ch)
      (emit-label buf ks)
      (emit-bytes buf #x48 #xFF #xC7)            ; inc rdi
      (emit-jmp buf kl)
      (emit-label buf kd))
    (emit-pop buf 'r9) (emit-pop buf 'r8) (emit-pop buf 'rdi) (emit-pop buf 'rdx) (emit-pop buf 'rax)))


(defun emit-mcgc-count-pinned-dbg (buf ch)
  "DEBUG (gated): emit byte CH once for each page whose descriptor bit2 (4) is
   set.  Clobbers RAX/RDX/RDI/R8 (saved/restored)."
  (when *x64-gc-debug*
    (emit-push buf 'rax) (emit-push buf 'rdx) (emit-push buf 'rdi) (emit-push buf 'r8)
    (let ((cnt-loop (make-label)) (cnt-done (make-label)) (cnt-skip (make-label)))
      (emit-bytes buf #x48 #x31 #xFF)            ; xor rdi, rdi (page idx)
      (emit-mov-reg-abs buf 'rdx +mcgc-cfg-page-count-addr+)
      (emit-mov-reg-abs buf 'r8 +mcgc-cfg-descriptor-addr+)
      (emit-label buf cnt-loop)
      (emit-cmp-reg-reg buf 'rdi 'rdx) (emit-jcc buf :ae cnt-done)
      (emit-bytes buf #x41 #xF6 #x04 #x38 #x04)  ; test byte [r8+rdi], 4
      (emit-jcc buf :e cnt-skip)
      (emit-gc-dbg-char buf ch)
      (emit-label buf cnt-skip)
      (emit-add-reg-imm buf 'rdi 1) (emit-jmp buf cnt-loop)
      (emit-label buf cnt-done))
    (emit-pop buf 'r8) (emit-pop buf 'rdi) (emit-pop buf 'rdx) (emit-pop buf 'rax)))


(defun emit-page-gc-trampoline (buf page-gc-label)
  "MCGC stage-4c/4d WHOLE-REGION page-based MOSTLY-COPYING collector WITH PINNING.

   Bartlett page pool over the WHOLE data region (no two-run split for the
   collector — that was 4b).  Descriptor byte per 4 KiB page:
     0 = free
     1 = live  (from-space candidate — copyable)
     3 = to-run (this GC's copy destination — kept, not a copy source)
     bit2 (|4) = pinned-this-GC (live+pinned = 5)
   Persistent FFI pins live in the separate per-page u32 pin-count array
   (+mcgc-cfg-pincount-addr+), which survives the per-GC descriptor reset.

   Phases:
     P0   clear transient pinned bits region-wide (descriptor &= ~4).
     P1a  pin pages with persistent pin-count>0 (descriptor |= 4).
     P1b  conservative stack scan: every word hitting a recorded object START
          in a live page marks that page pinned.  (No forward, no rewrite —
          pinned objects keep their address.)  Stack-only-reachable non-pinned
          objects are thus RETAINED in place via their pinned page (the Bartlett
          guarantee), so the stack is NOT a copy root.
     P2   pop a to-run from the free-list (mark its pages descriptor=3).  Forward
          PRECISE roots (globals/symtab/keyword/pkg/MV) into the to-run.  Scan
          every PINNED page's objects in place (gray roots).  Cheney-drain the
          to-run.  scan_word forwards a target ONLY if descriptor[page]==1
          (live, not pinned/to-run/free); else it keeps the address.
     P3   descriptor pass: live-copyable pages (state 1, evacuated) -> free +
          clear bitmap; pinned pages (5) -> live (1), clear pinned bit, keep
          bitmap; to-run pages (3) -> live (1) (the survivors' home).  Rebuild
          the run-free-list (coalesce free pages).  The to-run becomes the new
          alloc run.  No semispace flip.

   Register convention mirrors emit-gc-trampoline:
     RBX = page_base, RCX = data_end (whole-region from-bounds), R13 = to-run
     bump ptr, RBP = saved RSP.  R12/R14 reset on exit."
  (let ((copy-label (make-label))
        (scan-word-label (make-label))
        (restore-label (make-label))
        (init-done-label (make-label))
        (establish-label (make-label))   ; pop/establish a to-run segment
        (refill-label (make-label)))     ; finalize current seg + establish next
    (emit-label buf page-gc-label)
    (emit-gc-dbg-char buf #x7B)          ; '{' — page-GC entry

    ;; ---- Save all caller registers (same set/order as Cheney) ----
    (emit-push buf 'rax) (emit-push buf 'rsi) (emit-push buf 'rdi)
    (emit-push buf 'r8)  (emit-push buf 'r9)  (emit-push buf 'rbx)
    (emit-push buf 'rcx) (emit-push buf 'rdx) (emit-push buf 'r10)
    (emit-push buf 'r11) (emit-push buf 'r13) (emit-push buf 'rbp)
    (emit-bytes buf #x48 #x89 #xE5)              ; mov rbp, rsp

    ;; ================= Lazy init (first collection) =================
    ;; Anchor the page grid; mark the whole alloc-so-far region [page_base,
    ;; run_end) live (state 1); seed the free-list with the REST of the region
    ;; [run_end, data_end).  (The boot stub bumped R12 to ~midpoint; run_end is
    ;; recorded as that midpoint so already-allocated objects are all live.)
    (emit-mov-reg-abs buf 'rax +mcgc-cfg-init-done-addr+)
    (emit-bytes buf #x48 #x85 #xC0)             ; test rax, rax
    (emit-jcc buf :ne init-done-label)
    ;; RBX = page_base ; record run_start = page_base
    (emit-mov-reg-abs buf 'rbx +mcgc-cfg-page-base-addr+)
    (emit-mov-abs-reg buf +mcgc-cfg-run-start-addr+ 'rbx)
    ;; run0_end = page_base + half_bytes (same split point as 4b: the boot R12 is
    ;; below it, so all pre-init allocations are inside [page_base, run0_end)).
    (emit-mov-reg-abs buf 'r9 +mcgc-cfg-data-end-addr+)
    (emit-sub-reg-reg buf 'r9 'rbx)
    (emit-shr-reg-imm buf 'r9 +mcgc-page-shift+)
    (emit-shr-reg-imm buf 'r9 1)                ; r9 = half pages
    (emit-mov-reg-reg buf 'r8 'r9)
    (emit-shl-reg-imm buf 'r8 +mcgc-page-shift+) ; r8 = half_bytes
    (emit-mov-reg-reg buf 'rcx 'rbx)
    (emit-add-reg-reg buf 'rcx 'r8)             ; rcx = run0_end
    (emit-mov-abs-reg buf +mcgc-cfg-run-end-addr+ 'rcx)
    ;; mark [page_base, run0_end) live (state 1)
    (emit-mcgc-fill-descriptor-range buf 'rbx 'rcx 1 'r8)
    ;; mark [run0_end, data_end) free (state 0) — explicit (MAP_ANON is already
    ;; 0, but be robust).  Then the free-list rebuild below will pick it up; but
    ;; we also seed it now so the FIRST to-run pop has a run.
    (emit-mov-reg-abs buf 'rdx +mcgc-cfg-data-end-addr+)
    ;; RELOAD rcx = run0_end: the FIRST fill-descriptor-range above CLOBBERS RCX
    ;; (it is the REP STOSB count, left at 0).  Without this reload the second
    ;; fill's start address is 0 -> start_page = (0-page_base)>>12 = a huge
    ;; unsigned -> rep stosb to a wild descriptor address -> SEGFAULT.  (This was
    ;; the 4c collector's form-2 crash, found via small-R14 forced collection.)
    (emit-mov-reg-abs buf 'rcx +mcgc-cfg-run-end-addr+)
    (emit-mcgc-fill-descriptor-range buf 'rcx 'rdx 0 'r8)
    ;; seed free-list: one run [half .. total) pages.
    ;;   start_page = half (r9) ; n_pages = total - half
    (emit-mov-reg-abs buf 'rax +mcgc-cfg-data-end-addr+)
    (emit-mov-reg-abs buf 'r11 +mcgc-cfg-page-base-addr+)
    (emit-sub-reg-reg buf 'rax 'r11)
    (emit-shr-reg-imm buf 'rax +mcgc-page-shift+) ; rax = total pages
    (emit-sub-reg-reg buf 'rax 'r9)              ; rax = total - half = n_pages
    (emit-mov-reg-abs buf 'rdi +mcgc-cfg-freelist-base-addr+)
    (emit-bytes buf #x44 #x89 #x0F)             ; mov [rdi], r9d   (start_page=half)
    (emit-bytes buf #x89 #x47 #x04)             ; mov [rdi+4], eax (n_pages)
    (emit-mov-abs-imm32 buf +mcgc-cfg-freelist-count-addr+ 1)
    (emit-mcgc-pincount-init buf)
    (emit-mov-abs-imm32 buf +mcgc-cfg-init-done-addr+ 1)
    (emit-label buf init-done-label)
    (emit-mcgc-pincount-init buf)   ; idempotent; covers %pin before first GC
    ;; seg[] pair-array base = pincount_base + page_count*4 (just past the
    ;; per-page pin-count array in the MAP_ANON metadata slack).  Idempotent —
    ;; both operands are constant config words.  Stored once; persists in BSS.
    (emit-mov-reg-abs buf 'rax +mcgc-cfg-pincount-addr+)
    (emit-mov-reg-abs buf 'rcx +mcgc-cfg-page-count-addr+)
    (emit-shl-reg-imm buf 'rcx 2)                ; page_count * 4 (u32 pin-counts)
    (emit-add-reg-reg buf 'rax 'rcx)
    (emit-mov-abs-reg buf +mcgc-cfg-seg-arr-addr+ 'rax)
    (emit-gc-dbg-char buf #x4C)          ; 'L' — lazy-init done [DEBUG]

    ;; ================= Whole-region from-bounds =================
    ;; RBX = page_base, RCX = data_end.  These are the bounds for scan_word's
    ;; cheap "is this a heap data address" range gate; the descriptor byte then
    ;; decides copyable / pinned / to-run.
    (emit-mov-reg-abs buf 'rbx +mcgc-cfg-page-base-addr+)
    (emit-mov-reg-abs buf 'rcx +mcgc-cfg-data-end-addr+)
    (emit-mov-abs-reg buf +mcgc-cfg-from-start-addr+ 'rbx)
    (emit-mov-abs-reg buf +mcgc-cfg-from-end-addr+   'rcx)

    ;; ================= P0: clear transient pinned bits region-wide =========
    ;; descriptor[p] &= ~4 for p in [0, page_count).  Inline byte loop.
    (let ((p0-loop (make-label)) (p0-done (make-label)))
      (emit-mov-reg-abs buf 'rdi +mcgc-cfg-descriptor-addr+)
      (emit-mov-reg-abs buf 'rdx +mcgc-cfg-page-count-addr+) ; rdx = count
      (emit-label buf p0-loop)
      (emit-bytes buf #x48 #x85 #xD2)                       ; test rdx, rdx
      (emit-jcc buf :e p0-done)
      (emit-bytes buf #x80 #x27 #xFB)                       ; and byte [rdi], ~4
      (emit-add-reg-imm buf 'rdi 1)
      (emit-sub-reg-imm buf 'rdx 1)
      (emit-jmp buf p0-loop)
      (emit-label buf p0-done))
    (emit-gc-dbg-char buf #x30)          ; '0' — P0 clear-pin loop done [DEBUG]

    ;; ================= P1a: pin pages with persistent pin-count > 0 =========
    (let ((p1-loop (make-label)) (p1-done (make-label)) (p1-skip (make-label)))
      (emit-bytes buf #x48 #x31 #xC0)            ; xor rax, rax  (page idx)
      (emit-mov-reg-abs buf 'rdx +mcgc-cfg-page-count-addr+)
      (emit-label buf p1-loop)
      (emit-cmp-reg-reg buf 'rax 'rdx)
      (emit-jcc buf :ae p1-done)
      (emit-mov-reg-abs buf 'rsi +mcgc-cfg-pincount-addr+)
      (emit-bytes buf #x83 #x3C #x86 #x00)       ; cmp dword [rsi+rax*4], 0
      (emit-jcc buf :e p1-skip)
      (emit-mov-reg-abs buf 'rdi +mcgc-cfg-descriptor-addr+)
      (emit-bytes buf #x80 #x0C #x07 #x04)       ; or byte [rdi+rax], 4
      (emit-label buf p1-skip)
      (emit-add-reg-imm buf 'rax 1)
      (emit-jmp buf p1-loop)
      (emit-label buf p1-done))
    (emit-gc-dbg-char buf #x31)          ; '1' — P1a persistent-pin loop done [DEBUG]

    ;; ================= P1b: PIN via conservative stack scan =================
    (emit-bytes buf #x48 #x89 #xEF)              ; mov rdi, rbp
    (let ((sp-loop (make-label)) (sp-done (make-label)) (sp-next (make-label))
          (sp-cand (make-label)))
      (emit-mov-reg-abs buf 'rdx #x10000058)     ; rdx = stack_base
      (emit-label buf sp-loop)
      (emit-cmp-reg-reg buf 'rdi 'rdx)
      (emit-jcc buf :ae sp-done)
      (emit-mov-reg-mem buf 'rsi 'rdi 0)
      (emit-mov-reg-reg buf 'rax 'rsi)
      (emit-and-reg-imm buf 'rax #x0F)
      (emit-cmp-reg-imm buf 'rax 1)
      (emit-jcc buf :e sp-cand)
      (emit-cmp-reg-imm buf 'rax 9)
      (emit-jcc buf :ne sp-next)
      (emit-label buf sp-cand)
      (emit-mov-reg-reg buf 'rax 'rsi)
      (emit-and-reg-imm buf 'rax -16)
      (emit-cmp-reg-reg buf 'rax 'rbx)           ; < page_base?
      (emit-jcc buf :b sp-next)
      (emit-cmp-reg-reg buf 'rax 'rcx)           ; >= data_end?
      (emit-jcc buf :ae sp-next)
      ;; only pin a page that is currently a LIVE copyable page (state 1):
      ;; pinning a free/to-run page is meaningless; a stack word into a free
      ;; page is a stale false positive.  Reuse the object-start gate, then
      ;; require descriptor==1 before pinning.
      (emit-mcgc-validate-or-jump buf 'rax sp-next)
      (emit-mcgc-not-copyable-or-jump buf 'rax sp-next) ; skip if not state==1
      (emit-mov-reg-reg buf 'rsi 'rax)
      (emit-mcgc-mark-page-pinned buf 'rsi)
      (emit-label buf sp-next)
      (emit-add-reg-imm buf 'rdi 8)
      (emit-jmp buf sp-loop)
      (emit-label buf sp-done))
    (emit-gc-dbg-char buf #x50)          ; 'P' — pin phase done
    (emit-mcgc-count-pinned-dbg buf #x61)   ; 'a' count pinned after P1b [DEBUG]

    ;; ================= Establish the FIRST to-run segment =================
    ;; Reset the per-GC segment chain, then pop a to-run via establish-segment
    ;; (find-largest-free-run + mark descriptor=3 + append seg[0] + set
    ;; to_start/to_end/R13).  Survivors that later exceed this segment trigger
    ;; copy_object's refill (pop another free run as seg[1], seg[2], ...), so a
    ;; fragmented free region can no longer overflow the destination.
    (emit-mov-abs-imm32 buf +mcgc-cfg-seg-count-addr+ 0)   ; segments this GC
    (emit-mov-abs-imm32 buf +mcgc-cfg-oom-addr+ 0)         ; OOM flag clear
    (emit-call buf establish-label)                        ; sets to_start/to_end/R13, seg[0]
    (emit-mcgc-count-pinned-dbg buf #x62)   ; 'b' count pinned after to-run pop [DEBUG]

    (emit-gc-dbg-char buf #x70)          ; 'p' — about to scan roots
    ;; Reload from bounds (RBX/RCX) for scan_word.
    (emit-mov-reg-abs buf 'rbx +mcgc-cfg-from-start-addr+)
    (emit-mov-reg-abs buf 'rcx +mcgc-cfg-from-end-addr+)

    ;; ================= P2a: forward PRECISE roots into the to-run ===========
    (emit-mov-reg-imm buf 'rax #x10000080) (emit-call buf scan-word-label)
    (emit-mov-reg-imm buf 'rax #x10000088) (emit-call buf scan-word-label)
    (emit-mov-reg-imm buf 'rax #x10000148) (emit-call buf scan-word-label)
    (emit-mov-reg-imm buf 'rax #x10000170) (emit-call buf scan-word-label)
    (let ((mv-loop (make-label)) (mv-done (make-label)))
      (emit-mov-reg-imm buf 'rax #x10000090)
      (emit-mov-reg-mem buf 'r10 'rax 0)
      (emit-shr-reg-imm buf 'r10 1)
      (emit-sub-reg-imm buf 'r10 1)
      (emit-cmp-reg-imm buf 'r10 0)
      (emit-jcc buf :le mv-done)
      (emit-mov-reg-imm buf 'rdi #x10000098)
      (emit-label buf mv-loop)
      (emit-mov-reg-reg buf 'rax 'rdi)
      (emit-call buf scan-word-label)
      (emit-add-reg-imm buf 'rdi 8)
      (emit-sub-reg-imm buf 'r10 1)
      (emit-cmp-reg-imm buf 'r10 0)
      (emit-jcc buf :g mv-loop)
      (emit-label buf mv-done))
    (emit-gc-dbg-char buf #x72)          ; 'r' — precise roots done

    ;; ================= P2b: scan PINNED pages' objects (gray roots) =========
    ;; For each page whose descriptor pinned bit (4) is set, scan all granules
    ;; of that page via scan_word (forwarding any non-pinned references).  RDI/R9
    ;; are loop vars (preserved by scan_word); RSI = page cursor.
    ;; Gray scan walks only RECORDED OBJECT STARTS (object-start bitmap), not
    ;; every word.  A pinned page can carry a stale UNALLOCATED tail (no start
    ;; bits): the page got pinned while partially filled and was then KEPT in
    ;; place, so on a later GC its tail still holds leftover bytes that can alias
    ;; from-space objects.  Word-scanning that tail forwarded garbage as a gray
    ;; root — the latent LAYOUT-SENSITIVE corruption (which partial page is
    ;; pinned + what stale bytes it holds shifts with image layout; the heisenbug
    ;; that made "adding a defun break an unrelated test" for the pinning path).
    ;; For each start, scan [start, next-start) = the object's full extent; the
    ;; last object on a page (no next start before page end) is capped to one
    ;; granule (correct for the common cons; KNOWN LIMITATION — a LARGE object as
    ;; the very last live object on a pinned page would under-scan its tail
    ;; fields.  Harden with per-object sizing if a workload ever needs it; the
    ;; asdf gauntlet is byte-identical with the cap).
    (let ((pg-loop (make-label)) (pg-done (make-label)) (pg-skip (make-label))
          (gobj-loop (make-label)) (gobj-skip (make-label)) (page-done (make-label))
          (gend-loop (make-label)) (gend-next (make-label)) (gend-have (make-label))
          (gend-cap (make-label)) (gword-loop (make-label)) (gword-done (make-label)))
      (emit-mov-reg-reg buf 'rsi 'rbx)           ; rsi = page_base
      (emit-mov-reg-abs buf 'r11 +mcgc-cfg-from-end-addr+)
      (emit-label buf pg-loop)
      (emit-cmp-reg-reg buf 'rsi 'r11)
      (emit-jcc buf :ae pg-done)
      (emit-push buf 'rsi) (emit-push buf 'r11)
      (emit-mov-reg-reg buf 'rax 'rsi)
      (emit-bytes buf #x48 #x2B #x04 #x25) (emit-u32 buf +mcgc-cfg-page-base-addr+)
      (emit-shr-reg-imm buf 'rax +mcgc-page-shift+)        ; rax = page idx
      (emit-mov-reg-abs buf 'r8 +mcgc-cfg-descriptor-addr+)
      (emit-bytes buf #x41 #xF6 #x04 #x00 #x04)            ; test byte [r8+rax], 4
      (emit-pop buf 'r11) (emit-pop buf 'rsi)
      (emit-jcc buf :e pg-skip)
      (emit-gc-dbg-char buf #x67)          ; 'g' pinned page found in gray scan [DEBUG]
      (emit-mov-reg-reg buf 'rdi 'rsi)                     ; rdi = cursor = page start
      (emit-mov-reg-reg buf 'r9 'rsi)
      (emit-add-reg-imm buf 'r9 +mcgc-page-bytes+)         ; r9 = page end
      (emit-label buf gobj-loop)
      (emit-cmp-reg-reg buf 'rdi 'r9)
      (emit-jcc buf :ae page-done)
      (emit-mov-reg-reg buf 'rax 'rdi)                     ; rax = cursor (validate wants RAX)
      (emit-mcgc-validate-or-jump buf 'rax gobj-skip)      ; not a start -> skip granule
      ;; rdi is an object start.  Find object end r10 = next start (or page end).
      (emit-mov-reg-reg buf 'r10 'rdi)
      (emit-add-reg-imm buf 'r10 16)                       ; r10 = rdi + 1 granule
      (emit-label buf gend-loop)
      (emit-cmp-reg-reg buf 'r10 'r9)
      (emit-jcc buf :ae gend-cap)                          ; reached page end -> cap last object
      (emit-mov-reg-reg buf 'rax 'r10)
      (emit-mcgc-validate-or-jump buf 'rax gend-next)      ; r10 not a start -> keep looking
      (emit-jmp buf gend-have)                             ; r10 is a start -> object ends here
      (emit-label buf gend-next)
      (emit-add-reg-imm buf 'r10 16)
      (emit-jmp buf gend-loop)
      (emit-label buf gend-cap)
      (emit-mov-reg-reg buf 'r10 'rdi)                     ; last object: cap to one granule
      (emit-add-reg-imm buf 'r10 16)                       ;   (skip the stale unallocated tail)
      (emit-label buf gend-have)
      ;; scan words [rdi, r10), advancing rdi (preserved by scan_word)
      (emit-label buf gword-loop)
      (emit-cmp-reg-reg buf 'rdi 'r10)
      (emit-jcc buf :ae gword-done)
      (emit-mov-reg-reg buf 'rax 'rdi)
      (emit-call buf scan-word-label)
      (emit-add-reg-imm buf 'rdi 8)
      (emit-jmp buf gword-loop)
      (emit-label buf gword-done)
      (emit-jmp buf gobj-loop)                             ; rdi = r10 = next region
      (emit-label buf gobj-skip)
      (emit-add-reg-imm buf 'rdi 16)                       ; skip non-start granule
      (emit-jmp buf gobj-loop)
      (emit-label buf page-done)
      (emit-mov-reg-reg buf 'rsi 'rdi)                     ; rsi = page end (rdi reached r9)
      (emit-jmp buf pg-loop)
      (emit-label buf pg-skip)
      (emit-add-reg-imm buf 'rsi +mcgc-page-bytes+)
      (emit-jmp buf pg-loop)
      (emit-label buf pg-done))
    (emit-gc-dbg-char buf #x47)          ; 'G' — gray (pinned) scan done
    (emit-mcgc-count-state-dbg buf 3 #x54)   ; 'T' state-3 before reclaim [DEBUG]

    ;; ================= P2c: segmented Cheney-drain the to-run chain =========
    ;; R10 = scan_ptr, R11 = scan_seg (both preserved by scan_word).  Scan each
    ;; segment [seg[i].start, limit) where limit = seg[i].fill for a non-last
    ;; segment (finalized at the overflow that moved off it) or the live bump
    ;; ptr R13 for the last.  copy_object always appends to the LAST segment and
    ;; the chain grows (seg_count++) on refill, so the scan follows new segments
    ;; as they appear.  Done when scan_seg is the last segment and scan_ptr has
    ;; caught up to R13 with no further growth.
    (emit-bytes buf #x4D #x31 #xDB)             ; xor r11, r11   (scan_seg = 0)
    (emit-mov-reg-abs buf 'rax +mcgc-cfg-seg-arr-addr+)
    (emit-mov-reg-mem buf 'r10 'rax 0)          ; scan_ptr = seg[0].start
    (when *x64-gc-debug*                         ; [DEBUG] 'Z' if scan starts empty
      (emit-cmp-reg-reg buf 'r10 'r13)
      (let ((zok (make-label)))
        (emit-jcc buf :b zok) (emit-gc-dbg-char buf #x5A) (emit-label buf zok)))
    (let ((sc-loop (make-label)) (sc-done (make-label))
          (sc-last (make-label)) (sc-scan (make-label)))
      (emit-label buf sc-loop)
      ;; last_idx = seg_count - 1
      (emit-mov-reg-abs buf 'rax +mcgc-cfg-seg-count-addr+)
      (emit-sub-reg-imm buf 'rax 1)
      (emit-cmp-reg-reg buf 'r11 'rax)
      (emit-jcc buf :e sc-last)
      ;; NON-last segment: limit = seg[scan_seg].fill
      (emit-mov-reg-abs buf 'rdx +mcgc-cfg-seg-arr-addr+)
      (emit-mov-reg-reg buf 'rsi 'r11)
      (emit-shl-reg-imm buf 'rsi 4)             ; scan_seg * 16
      (emit-add-reg-reg buf 'rdx 'rsi)          ; rdx = &seg[scan_seg]
      (emit-mov-reg-mem buf 'rsi 'rdx 8)        ; rsi = seg[scan_seg].fill
      (emit-cmp-reg-reg buf 'r10 'rsi)
      (emit-jcc buf :b sc-scan)                 ; scan_ptr < fill -> scan a word
      ;; finished this segment -> advance scan to the next segment's start
      (emit-add-reg-imm buf 'r11 1)
      (emit-mov-reg-abs buf 'rdx +mcgc-cfg-seg-arr-addr+)
      (emit-mov-reg-reg buf 'rsi 'r11)
      (emit-shl-reg-imm buf 'rsi 4)
      (emit-add-reg-reg buf 'rdx 'rsi)
      (emit-mov-reg-mem buf 'r10 'rdx 0)        ; scan_ptr = seg[scan_seg].start
      (emit-jmp buf sc-loop)
      ;; LAST segment: limit = live bump ptr R13
      (emit-label buf sc-last)
      (emit-cmp-reg-reg buf 'r10 'r13)
      (emit-jcc buf :ae sc-done)                ; caught up -> drain complete
      (emit-label buf sc-scan)
      (emit-bytes buf #x4C #x89 #xD0)           ; mov rax, r10
      (emit-call buf scan-word-label)
      (emit-add-reg-imm buf 'r10 8)
      (emit-jmp buf sc-loop)
      (emit-label buf sc-done))
    (emit-gc-dbg-char buf #x63)          ; 'c' — cheney done

    ;; ============== Ensure the post-GC alloc run has guard headroom ==========
    ;; After the (possibly multi-segment) drain, R13 is the live bump ptr in the
    ;; LAST to-run segment, which becomes the new alloc run.  If a refill chain
    ;; left that segment nearly full (room <= 2*guard), allocation would cross
    ;; R14 = to_end-guard immediately and re-trigger GC forever.  Pop ONE more
    ;; (empty) segment via refill so the alloc run starts fresh with full room.
    ;; (No-op in the common single-segment case: a freshly popped to-run is far
    ;; larger than 2*guard, so have-room is taken.)  refill OOM (no free run)
    ;; leaves R13/to_end as-is — genuine heap exhaustion, nothing more to do.
    (let ((have-room (make-label)))
      (emit-mov-reg-abs buf 'rax +mcgc-cfg-to-end-addr+)
      (emit-sub-reg-reg buf 'rax 'r13)            ; room = to_end - R13
      (emit-cmp-reg-imm buf 'rax (* 2 +mcgc-guard-bytes+))
      (emit-jcc buf :a have-room)                 ; room > 2*guard -> fine
      (emit-mov-abs-imm32 buf +mcgc-cfg-uncap-addr+ 1) ; alloc run wants a LARGE (uncapped) run
      (emit-call buf refill-label)                ; finalize last seg + pop fresh one
      (emit-label buf have-room))

    ;; ================= P3: RECLAIM + rebuild free-list =================
    ;; Descriptor pass over [0, page_count): translate states for next GC.
    ;;   state 1 (live, evacuated) -> 0 (free) + clear page bitmap
    ;;   state 3 (to-run survivors) -> 1 (live)
    ;;   state 5 (live+pinned)      -> 1 (live), keep bitmap
    ;; Cursor RSI = page data addr, R11 = data_end; RAX = page idx.
    (let ((rc-loop (make-label)) (rc-done (make-label))
          (rc-torun (make-label)) (rc-free (make-label))
          (rc-advance (make-label)))
      (emit-mov-reg-abs buf 'rsi +mcgc-cfg-from-start-addr+) ; page_base
      (emit-mov-reg-abs buf 'r11 +mcgc-cfg-from-end-addr+)   ; data_end
      (emit-label buf rc-loop)
      (emit-cmp-reg-reg buf 'rsi 'r11)
      (emit-jcc buf :ae rc-done)
      (emit-mov-reg-reg buf 'rax 'rsi)
      (emit-bytes buf #x48 #x2B #x04 #x25) (emit-u32 buf +mcgc-cfg-page-base-addr+)
      (emit-shr-reg-imm buf 'rax +mcgc-page-shift+)        ; rax = page idx
      (emit-mov-reg-abs buf 'r8 +mcgc-cfg-descriptor-addr+)
      (emit-bytes buf #x41 #x8A #x14 #x00)                 ; mov dl, [r8+rax]
      ;; pinned? (bit2 set) -> live(1)
      (emit-bytes buf #xF6 #xC2 #x04)                      ; test dl, 4
      (emit-jcc buf :ne rc-torun)                          ; pinned -> set live 1
      ;; to-run (==3)? -> live(1)
      (emit-bytes buf #x80 #xFA #x03)                      ; cmp dl, 3
      (emit-jcc buf :e rc-torun)
      ;; live copyable (==1)? -> free(0) + clear bitmap
      (emit-bytes buf #x80 #xFA #x01)                      ; cmp dl, 1
      (emit-jcc buf :e rc-free)
      ;; free (0) or other -> leave as-is (free)
      (emit-jmp buf rc-advance)
      (emit-label buf rc-free)
      (emit-bytes buf #x41 #xC6 #x04 #x00 #x00)            ; mov byte [r8+rax],0
      (emit-push buf 'rsi) (emit-push buf 'r11)
      (emit-mov-reg-reg buf 'rdx 'rsi)
      (emit-add-reg-imm buf 'rdx +mcgc-page-bytes+)
      (emit-mcgc-clear-bitmap-range buf 'rsi 'rdx)
      (emit-pop buf 'r11) (emit-pop buf 'rsi)
      (emit-jmp buf rc-advance)
      (emit-label buf rc-torun)
      (emit-bytes buf #x41 #xC6 #x04 #x00 #x01)            ; mov byte [r8+rax],1
      (emit-label buf rc-advance)
      (emit-add-reg-imm buf 'rsi +mcgc-page-bytes+)
      (emit-jmp buf rc-loop)
      (emit-label buf rc-done))
    ;; Rebuild the run-free-list from a full descriptor scan, coalescing
    ;; consecutive FREE pages (descriptor==0) into (start_page,n_pages) runs.
    (let ((fl-loop (make-label)) (fl-done (make-label))
          (fl-free (make-label)) (fl-advance (make-label))
          (fl-after-flush (make-label)) (fl-no-run (make-label)))
      (emit-bytes buf #x48 #x31 #xF6)            ; xor rsi, rsi  (page idx 0)
      (emit-mov-reg-abs buf 'r9 +mcgc-cfg-page-count-addr+)
      (emit-bytes buf #x4D #x31 #xD2)            ; xor r10, r10  (entry count 0)
      (emit-mov-reg-imm buf 'r11 #xFFFFFFFFFFFFFFFF) ; r11 = -1 (no current run)
      (emit-mov-reg-abs buf 'rdi +mcgc-cfg-freelist-base-addr+)
      (emit-mov-reg-abs buf 'r8  +mcgc-cfg-descriptor-addr+)
      (emit-label buf fl-loop)
      (emit-cmp-reg-reg buf 'rsi 'r9)
      (emit-jcc buf :ae fl-done)
      (emit-bytes buf #x41 #x8A #x04 #x30)       ; mov al, [r8+rsi]  FIXED (was #x42=REX.X -> wrongly [rax+r14])
      (emit-bytes buf #x84 #xC0)                 ; test al, al
      (emit-jcc buf :e fl-free)
      ;; NON-free page: flush an open run if any
      (emit-bytes buf #x49 #x83 #xFB #xFF)       ; cmp r11, -1
      (emit-jcc buf :e fl-after-flush)
      (emit-bytes buf #x4A #x8D #x04 #xD7)       ; lea rax, [rdi + r10*8]
      (emit-bytes buf #x44 #x89 #x18)            ; mov [rax], r11d
      (emit-mov-reg-reg buf 'rdx 'rsi)
      (emit-sub-reg-reg buf 'rdx 'r11)
      (emit-bytes buf #x89 #x50 #x04)            ; mov [rax+4], edx
      (emit-add-reg-imm buf 'r10 1)
      (emit-mov-reg-imm buf 'r11 #xFFFFFFFFFFFFFFFF)
      (emit-label buf fl-after-flush)
      (emit-jmp buf fl-advance)
      (emit-label buf fl-free)
      (emit-bytes buf #x49 #x83 #xFB #xFF)       ; cmp r11, -1
      (emit-jcc buf :ne fl-advance)
      (emit-mov-reg-reg buf 'r11 'rsi)           ; open run
      (emit-label buf fl-advance)
      (emit-add-reg-imm buf 'rsi 1)
      (emit-jmp buf fl-loop)
      (emit-label buf fl-done)
      (emit-bytes buf #x49 #x83 #xFB #xFF)       ; cmp r11, -1
      (emit-jcc buf :e fl-no-run)
      (emit-bytes buf #x4A #x8D #x04 #xD7)       ; lea rax, [rdi + r10*8]
      (emit-bytes buf #x44 #x89 #x18)            ; mov [rax], r11d
      (emit-mov-reg-reg buf 'rdx 'r9)
      (emit-sub-reg-reg buf 'rdx 'r11)
      (emit-bytes buf #x89 #x50 #x04)            ; mov [rax+4], edx
      (emit-add-reg-imm buf 'r10 1)
      (emit-label buf fl-no-run)
      (emit-mov-abs-reg buf +mcgc-cfg-freelist-count-addr+ 'r10))
    (emit-gc-dbg-char buf #x52)          ; 'R' — reclaim+rebuild done
    (emit-mcgc-count-state-dbg buf 1 #x4B)   ; 'K' state-1 after reclaim [DEBUG]

    ;; ================= New alloc run = the to-run =================
    (emit-mov-reg-abs buf 'rax +mcgc-cfg-to-start-addr+)
    (emit-mov-abs-reg buf +mcgc-cfg-run-start-addr+ 'rax)
    (emit-mov-reg-abs buf 'rdx +mcgc-cfg-to-end-addr+)
    (emit-mov-abs-reg buf +mcgc-cfg-run-end-addr+ 'rdx)
    (emit-bytes buf #x4D #x89 #xEC)             ; mov r12, r13
    (emit-mov-reg-reg buf 'r14 'rdx)            ; r14 = to_end
    (emit-sub-reg-imm buf 'r14 +mcgc-guard-bytes+)
    (when *x64-gc-debug*                         ; [DEBUG] '<' if no room post-GC (r12 >= r14)
      (emit-cmp-reg-reg buf 'r12 'r14)
      (let ((rok (make-label)))
        (emit-jcc buf :b rok) (emit-gc-dbg-char buf #x3C) (emit-label buf rok)))

    (emit-bytes buf #x48 #xFF #x04 #x25) (emit-u32 buf #x10000060) ; gc_count++
    (emit-jmp buf restore-label)

    ;; ===========================================================
    ;; SUBROUTINE: scan_word (RAX = addr of word).  from-range RBX..RCX (whole
    ;; region); a target is FORWARDED iff its descriptor==1, else its address is
    ;; kept.  Preserves RBX/RCX/R13/RDI/R10/R11/RBP; clobbers RAX/RSI/RDX/R8.
    ;; ===========================================================
    (emit-label buf scan-word-label)
    (emit-push buf 'rdx)
    (emit-push buf 'rax)
    (let ((sw-not-ptr (make-label)) (sw-done (make-label))
          (sw-is-cons (make-label)) (sw-is-obj (make-label)))
      (emit-mov-reg-mem buf 'rsi 'rax 0)
      (emit-mov-reg-reg buf 'rax 'rsi)
      (emit-and-reg-imm buf 'rax #x0F)
      (emit-cmp-reg-imm buf 'rax 1)
      (emit-jcc buf :e sw-is-cons)
      (emit-cmp-reg-imm buf 'rax 9)
      (emit-jcc buf :e sw-is-obj)
      (emit-jmp buf sw-not-ptr)
      ;; cons
      (emit-label buf sw-is-cons)
      (emit-mov-reg-reg buf 'rax 'rsi)
      (emit-and-reg-imm buf 'rax -16)
      (emit-cmp-reg-reg buf 'rax 'rbx)
      (emit-jcc buf :b sw-not-ptr)
      (emit-cmp-reg-reg buf 'rax 'rcx)
      (emit-jcc buf :ae sw-not-ptr)
      (emit-gc-dbg-char buf #x44)          ; 'D' cons examined [DEBUG]
      (emit-mcgc-validate-or-jump buf 'rax sw-not-ptr)
      (emit-gc-dbg-char buf #x45)          ; 'E' cons validate passed [DEBUG]
      (emit-mcgc-not-copyable-or-jump buf 'rax sw-not-ptr) ; keep addr if !=1
      (emit-mov-reg-reg buf 'rax 'rsi)
      (emit-call buf copy-label)
      (emit-pop buf 'rsi)
      (emit-mov-mem-reg buf 'rsi 'rax 0)
      (emit-push buf 'rsi)
      (emit-jmp buf sw-done)
      ;; obj
      (emit-label buf sw-is-obj)
      (emit-mov-reg-reg buf 'rax 'rsi)
      (emit-and-reg-imm buf 'rax -16)
      (emit-cmp-reg-reg buf 'rax 'rbx)
      (emit-jcc buf :b sw-not-ptr)
      (emit-cmp-reg-reg buf 'rax 'rcx)
      (emit-jcc buf :ae sw-not-ptr)
      (emit-gc-dbg-char buf #x65)          ; 'e' obj examined [DEBUG]
      (emit-mcgc-validate-or-jump buf 'rax sw-not-ptr)
      (emit-gc-dbg-char buf #x46)          ; 'F' obj validate passed [DEBUG]
      (emit-mcgc-not-copyable-or-jump buf 'rax sw-not-ptr)
      (emit-mov-reg-reg buf 'rax 'rsi)
      (emit-call buf copy-label)
      (emit-pop buf 'rsi)
      (emit-mov-mem-reg buf 'rsi 'rax 0)
      (emit-push buf 'rsi)
      (emit-jmp buf sw-done)
      (emit-label buf sw-not-ptr)
      (emit-label buf sw-done)
      (emit-pop buf 'rax)
      (emit-pop buf 'rdx)
      (emit-ret buf))

    ;; ===========================================================
    ;; SUBROUTINE: copy_object (RAX = tagged from-ptr) -> RAX new tagged ptr.
    ;; The copy DEST is the to-run (R13).  copy_object only ever runs on a
    ;; descriptor==1 source (gated by scan_word).  The to_end bound check uses
    ;; +mcgc-cfg-to-end-addr+ (the to-run end), NOT RCX (which is data_end here),
    ;; so a runaway size is still caught.
    ;; ===========================================================
    (emit-label buf copy-label)
    (emit-gc-dbg-char buf #x43)          ; 'C' copy_object entry [DEBUG]
    (let ((copy-cons (make-label)) (copy-obj (make-label))
          (copy-fwd (make-label)) (copy-bogus (make-label))
          (copy-done (make-label))
          (cons-room (make-label)) (obj-room (make-label)))
      (emit-mov-reg-reg buf 'rdx 'rax)
      (emit-and-reg-imm buf 'rax #x0F)
      (emit-cmp-reg-imm buf 'rax 1)
      (emit-jcc buf :e copy-cons)
      (emit-jmp buf copy-obj)
      ;; cons
      (emit-label buf copy-cons)
      (emit-mov-reg-reg buf 'rsi 'rdx)
      (emit-and-reg-imm buf 'rsi -16)
      (emit-mov-reg-mem buf 'rax 'rsi 0)
      (emit-mov-reg-reg buf 'r8 'rax)
      (emit-and-reg-imm buf 'r8 #x0F)
      (emit-cmp-reg-imm buf 'r8 #x0F)
      (emit-jcc buf :e copy-fwd)
      ;; DEST overflow?  r13+16 > to_end -> refill (pop next to-run segment).
      ;; rsi(source)/rdx(orig tagged source) survive refill; OOM -> bogus.
      (emit-mov-reg-reg buf 'rax 'r13)
      (emit-add-reg-imm buf 'rax 16)
      (emit-mov-reg-abs buf 'r8 +mcgc-cfg-to-end-addr+)
      (emit-cmp-reg-reg buf 'rax 'r8)
      (emit-jcc buf :be cons-room)
      (emit-call buf refill-label)
      (emit-mov-reg-reg buf 'rax 'r13)
      (emit-add-reg-imm buf 'rax 16)
      (emit-mov-reg-abs buf 'r8 +mcgc-cfg-to-end-addr+)
      (emit-cmp-reg-reg buf 'rax 'r8)
      (emit-jcc buf :a copy-bogus)               ; still over after refill = OOM
      (emit-label buf cons-room)
      (emit-mov-reg-mem buf 'rax 'rsi 0)
      (emit-bytes buf #x49 #x89 #x45 #x00)        ; mov [r13], rax
      (emit-mov-reg-mem buf 'rax 'rsi 8)
      (emit-bytes buf #x49 #x89 #x45 #x08)        ; mov [r13+8], rax
      (emit-bytes buf #x4C #x89 #xE8)             ; mov rax, r13
      (emit-or-reg-imm buf 'rax 1)
      (emit-bytes buf #x4C #x89 #xEA)             ; mov rdx, r13
      (emit-or-reg-imm buf 'rdx #x0F)
      (emit-mov-mem-reg buf 'rsi 'rdx 0)
      (emit-mcgc-set-copy-bit buf 'r13)
      (emit-add-reg-imm buf 'r13 16)
      (emit-jmp buf copy-done)
      ;; forwarded
      (emit-label buf copy-fwd)
      (emit-and-reg-imm buf 'rax -16)
      (emit-mov-reg-reg buf 'r8 'rdx)
      (emit-and-reg-imm buf 'r8 #x0F)
      (emit-or-reg-reg buf 'rax 'r8)
      (emit-jmp buf copy-done)
      ;; object
      (emit-label buf copy-obj)
      (emit-mov-reg-reg buf 'rsi 'rdx)
      (emit-and-reg-imm buf 'rsi -16)
      (emit-mov-reg-mem buf 'rax 'rsi 0)
      (emit-mov-reg-reg buf 'r8 'rax)
      (emit-and-reg-imm buf 'r8 #x0F)
      (emit-cmp-reg-imm buf 'r8 #x0F)
      (emit-jcc buf :e copy-fwd)
      (emit-mov-reg-reg buf 'r8 'rax)
      (emit-shr-reg-imm buf 'r8 8)
      (emit-add-reg-imm buf 'r8 2)
      (emit-shl-reg-imm buf 'r8 3)
      (emit-add-reg-imm buf 'r8 15)
      (emit-and-reg-imm buf 'r8 -16)
      ;; sanity: SOURCE raw + size <= data_end else bogus (corrupt source guard)
      (emit-mov-reg-reg buf 'rax 'rsi)
      (emit-add-reg-reg buf 'rax 'r8)
      (emit-push buf 'rdx)
      (emit-mov-reg-abs buf 'rdx +mcgc-cfg-data-end-addr+)
      (emit-cmp-reg-reg buf 'rax 'rdx)
      (emit-pop buf 'rdx)
      (emit-jcc buf :a copy-bogus)
      ;; PAGE-PAD the destination so a COPIED object never STRADDLES a page
      ;; boundary.  This keeps per-page pinning EXACT: a pinned (kept-in-place)
      ;; object then lives wholly within its own page(s) and cannot "infect" the
      ;; next page.  Without it, bump-allocated survivors cross page boundaries
      ;; constantly, so pinning one object's page transitively keeps the whole
      ;; contiguous run (each boundary-crossing object pins the next page) ->
      ;; over-retention -> to-run exhaustion -> copy_object OOM returns stale
      ;; pointers -> corruption.  That straddle "infection" is the root of the
      ;; layout-sensitive pinning corruption (root-caused 2026-06-18: a 7-slot
      ;; array straddling pages 0x4b/0x4c, head pinned for a page-mate, tail
      ;; reclaimed+reused).  If page(R13) != page(R13+size-1), bump R13 to the next
      ;; page boundary; the gap left behind is harmless — P2c scans the to-run
      ;; word-by-word and scan_word filters non-pointers, and reclaim keeps the
      ;; to-run page.  An object > page_bytes still spans pages, but contiguously
      ;; from a page-aligned start (bounded to that one object, non-infectious).
      ;; Conses (16B / 16-aligned, page bounds 16-aligned) never straddle, so only
      ;; this obj path pads.  RAX scratch; RDI saved/restored.  Done BEFORE the
      ;; to_end check so refill (which resets R13 to a page-aligned segment start)
      ;; composes correctly.
      (emit-push buf 'rdi)
      (emit-mov-reg-reg buf 'rax 'r13)
      (emit-bytes buf #x48 #x2B #x04 #x25) (emit-u32 buf +mcgc-cfg-page-base-addr+) ; sub rax,[page_base]
      (emit-shr-reg-imm buf 'rax +mcgc-page-shift+)        ; rax = page(R13)
      (emit-mov-reg-reg buf 'rdi 'r13)
      (emit-add-reg-reg buf 'rdi 'r8)
      (emit-sub-reg-imm buf 'rdi 1)
      (emit-bytes buf #x48 #x2B #x3C #x25) (emit-u32 buf +mcgc-cfg-page-base-addr+) ; sub rdi,[page_base]
      (emit-shr-reg-imm buf 'rdi +mcgc-page-shift+)        ; rdi = page(R13+size-1)
      (emit-cmp-reg-reg buf 'rax 'rdi)
      (let ((no-pad (make-label)))
        (emit-jcc buf :e no-pad)                           ; same page -> no padding
        (emit-add-reg-imm buf 'rax 1)
        (emit-shl-reg-imm buf 'rax +mcgc-page-shift+)
        (emit-bytes buf #x48 #x03 #x04 #x25) (emit-u32 buf +mcgc-cfg-page-base-addr+) ; add rax,[page_base]
        (emit-mov-reg-reg buf 'r13 'rax)                   ; R13 = next page boundary
        (emit-label buf no-pad))
      (emit-pop buf 'rdi)
      ;; DEST overflow?  r13+size > to_end  <=>  r13 > to_end-size  -> refill.
      ;; ONLY RAX is free here: rsi(source)/r8(size)/rdx(orig tagged source) are
      ;; live, and RCX must stay = from_end (scan_word's range bound, which
      ;; copy_object is contracted to preserve — do NOT clobber it).
      (emit-mov-reg-abs buf 'rax +mcgc-cfg-to-end-addr+)
      (emit-sub-reg-reg buf 'rax 'r8)            ; rax = to_end - size
      (emit-cmp-reg-reg buf 'r13 'rax)
      (emit-jcc buf :be obj-room)                ; r13 <= to_end-size -> room
      (emit-call buf refill-label)
      (emit-mov-reg-abs buf 'rax +mcgc-cfg-to-end-addr+)
      (emit-sub-reg-reg buf 'rax 'r8)
      (emit-cmp-reg-reg buf 'r13 'rax)
      (emit-jcc buf :a copy-bogus)               ; still over after refill = OOM
      (emit-label buf obj-room)
      (emit-push buf 'rdi)
      (emit-push buf 'rcx)
      (emit-push buf 'r13)
      (emit-mcgc-set-copy-bit buf 'r13)
      (emit-bytes buf #x4C #x89 #xEF)             ; mov rdi, r13
      (emit-mov-reg-reg buf 'rcx 'r8)
      (emit-shr-reg-imm buf 'rcx 3)
      (emit-bytes buf #xF3 #x48 #xA5)             ; rep movsq
      (emit-bytes buf #x49 #x89 #xFD)             ; mov r13, rdi
      (emit-pop buf 'rax)                          ; rax = old r13 (dest start)
      (emit-or-reg-imm buf 'rax 9)
      (emit-mov-reg-reg buf 'rsi 'rdx)
      (emit-and-reg-imm buf 'rsi -16)
      (emit-mov-reg-reg buf 'rdx 'rax)
      (emit-and-reg-imm buf 'rdx -16)
      (emit-or-reg-imm buf 'rdx #x0F)
      (emit-mov-mem-reg buf 'rsi 'rdx 0)
      (emit-pop buf 'rcx)
      (emit-pop buf 'rdi)
      (emit-jmp buf copy-done)
      ;; bogus
      (emit-label buf copy-bogus)
      (emit-mov-reg-reg buf 'rax 'rdx)
      (emit-label buf copy-done)
      (emit-ret buf))

    ;; ===========================================================
    ;; SUBROUTINE: refill — the current (last) to-run segment is full.  Finalize
    ;; its fill point (= R13) and establish the NEXT segment (pop another free
    ;; run).  Called from copy_object; preserves all regs except R13 (which
    ;; establish-segment resets to the new segment's bump start).  On true OOM
    ;; (no free run) establish sets the OOM flag and leaves R13/to_end unchanged,
    ;; so copy_object's post-refill recheck routes the object to copy-bogus.
    ;; ===========================================================
    (emit-label buf refill-label)
    (emit-gc-dbg-char buf #x21)          ; '!' — refill entry [DEBUG]
    (emit-push buf 'rax) (emit-push buf 'rcx) (emit-push buf 'rdx)
    ;; seg[seg_count-1].fill = R13
    (emit-mov-reg-abs buf 'rax +mcgc-cfg-seg-arr-addr+)
    (emit-mov-reg-abs buf 'rcx +mcgc-cfg-seg-count-addr+)
    (emit-sub-reg-imm buf 'rcx 1)
    (emit-shl-reg-imm buf 'rcx 4)               ; (seg_count-1)*16
    (emit-add-reg-reg buf 'rax 'rcx)            ; rax = &seg[seg_count-1]
    (emit-bytes buf #x4C #x89 #x68 #x08)        ; mov [rax+8], r13   (.fill = R13)
    (emit-pop buf 'rdx) (emit-pop buf 'rcx) (emit-pop buf 'rax)
    (emit-call buf establish-label)
    (emit-ret buf)

    ;; ===========================================================
    ;; SUBROUTINE: establish-segment — pop the LARGEST free run, mark its pages
    ;; descriptor=3, append it as the next to-run segment (seg[seg_count].start),
    ;; and set to_start/to_end/R13 to it.  Preserves all regs except R13.  When
    ;; *mcgc-torun-cap-pages* > 0 (test knob) each segment is capped to that many
    ;; pages with the remainder put back on the free-list, so the refill path is
    ;; exercised on ordinary workloads.  No free run (or seg[] full) -> OOM flag.
    ;; ===========================================================
    (emit-label buf establish-label)
    (emit-push buf 'rax) (emit-push buf 'rcx) (emit-push buf 'rdx)
    (emit-push buf 'rsi) (emit-push buf 'rdi) (emit-push buf 'r8)
    (emit-push buf 'r9)  (emit-push buf 'r10) (emit-push buf 'r11)
    (let ((es-oom (make-label)) (es-done (make-label))
          (es-mxloop (make-label)) (es-mxnext (make-label)) (es-mxdone (make-label))
          (es-takeall (make-label)) (es-havetake (make-label))
          (cap *mcgc-torun-cap-pages*))
      ;; seg[] full?  seg_count >= MAX_SEG -> OOM
      (emit-mov-reg-abs buf 'rax +mcgc-cfg-seg-count-addr+)
      (emit-cmp-reg-imm buf 'rax +mcgc-max-segments+)
      (emit-jcc buf :ae es-oom)
      ;; find largest free run: r10=count, rdi=base, rsi=best_idx, r11=best_n, rcx=i
      (emit-mov-reg-abs buf 'r10 +mcgc-cfg-freelist-count-addr+)
      (emit-bytes buf #x4D #x85 #xD2)            ; test r10, r10
      (emit-jcc buf :e es-oom)
      (emit-mov-reg-abs buf 'rdi +mcgc-cfg-freelist-base-addr+)
      (emit-bytes buf #x48 #x31 #xF6)            ; xor rsi, rsi   (best_idx)
      (emit-bytes buf #x4D #x31 #xDB)            ; xor r11, r11   (best_n)
      (emit-bytes buf #x48 #x31 #xC9)            ; xor rcx, rcx   (i)
      (emit-label buf es-mxloop)
      (emit-cmp-reg-reg buf 'rcx 'r10) (emit-jcc buf :ae es-mxdone)
      (emit-bytes buf #x44 #x8B #x44 #xCF #x04)  ; mov r8d, [rdi+rcx*8+4]  (n_pages[i])
      (emit-bytes buf #x45 #x39 #xD8)            ; cmp r8d, r11d
      (emit-jcc buf :be es-mxnext)
      (emit-bytes buf #x45 #x89 #xC3)            ; mov r11d, r8d   (best_n)
      (emit-mov-reg-reg buf 'rsi 'rcx)           ; best_idx = i
      (emit-label buf es-mxnext)
      (emit-add-reg-imm buf 'rcx 1) (emit-jmp buf es-mxloop)
      (emit-label buf es-mxdone)
      ;; rdx = &entry = base + best_idx*8 ; eax = start_page ; r11 = best_n ; r9 = take
      (emit-bytes buf #x48 #x8D #x14 #xF7)       ; lea rdx, [rdi + rsi*8]
      (emit-bytes buf #x8B #x02)                 ; mov eax, [rdx]   (start_page)
      (when (> cap 0)
        ;; one-shot uncap (ensure-room alloc segment wants a LARGE run): if the
        ;; uncap flag is set, clear it and full-take regardless of cap.
        (emit-mov-reg-abs buf 'r9 +mcgc-cfg-uncap-addr+)
        (emit-bytes buf #x4D #x85 #xC9)                      ; test r9, r9
        (let ((do-cap (make-label)))
          (emit-jcc buf :e do-cap)
          (emit-mov-abs-imm32 buf +mcgc-cfg-uncap-addr+ 0)   ; clear one-shot
          (emit-jmp buf es-takeall)
          (emit-label buf do-cap))
        ;; best_n <= cap -> full take ; else take=cap and shrink entry in place
        (emit-bytes buf #x41 #x81 #xFB) (emit-u32 buf cap)   ; cmp r11d, cap
        (emit-jcc buf :be es-takeall)
        (emit-mov-reg-imm buf 'r9 cap)                       ; take = cap
        (emit-bytes buf #x81 #x02) (emit-u32 buf cap)        ; add dword [rdx], cap   (start_page += cap)
        (emit-bytes buf #x81 #x6A #x04) (emit-u32 buf cap)   ; sub dword [rdx+4], cap (n_pages -= cap)
        (emit-jmp buf es-havetake))
      ;; full-take: take = best_n ; swap-remove entry with the last
      (emit-label buf es-takeall)
      (emit-bytes buf #x45 #x89 #xD9)            ; mov r9d, r11d   (take = best_n)
      (emit-sub-reg-imm buf 'r10 1)              ; r10 = last_idx
      (emit-bytes buf #x4E #x8B #x04 #xD7)       ; mov r8, [rdi + r10*8]  (last entry)
      (emit-bytes buf #x4C #x89 #x02)            ; mov [rdx], r8          (overwrite best)
      (emit-mov-abs-reg buf +mcgc-cfg-freelist-count-addr+ 'r10)
      (emit-label buf es-havetake)
      ;; start_addr = page_base + start_page<<12   (eax = start_page)
      (emit-shl-reg-imm buf 'rax +mcgc-page-shift+)
      (emit-bytes buf #x48 #x03 #x04 #x25) (emit-u32 buf +mcgc-cfg-page-base-addr+) ; add rax,[page_base]
      (emit-mov-abs-reg buf +mcgc-cfg-to-start-addr+ 'rax)
      ;; end_addr = start_addr + take<<12   (r9 = take)
      (emit-mov-reg-reg buf 'rdx 'r9)
      (emit-shl-reg-imm buf 'rdx +mcgc-page-shift+)
      (emit-add-reg-reg buf 'rdx 'rax)           ; rdx = end_addr
      (emit-mov-abs-reg buf +mcgc-cfg-to-end-addr+ 'rdx)
      ;; mark descriptor [start,end)=3  (fill clobbers rax/rcx/rdi; args rsi=start, rdx=end)
      (emit-mov-reg-reg buf 'rsi 'rax)           ; rsi = start
      (emit-mcgc-fill-descriptor-range buf 'rsi 'rdx 3 'r8)
      ;; append segment: idx = seg_count ; seg[idx].start = to_start ; seg_count = idx+1
      (emit-mov-reg-abs buf 'rax +mcgc-cfg-seg-arr-addr+)
      (emit-mov-reg-abs buf 'rcx +mcgc-cfg-seg-count-addr+)
      (emit-mov-reg-reg buf 'rdx 'rcx)
      (emit-shl-reg-imm buf 'rdx 4)              ; idx*16
      (emit-add-reg-reg buf 'rax 'rdx)           ; rax = &seg[idx]
      (emit-mov-reg-abs buf 'rdx +mcgc-cfg-to-start-addr+)
      (emit-mov-mem-reg buf 'rax 'rdx 0)         ; seg[idx].start = to_start
      (emit-add-reg-imm buf 'rcx 1)
      (emit-mov-abs-reg buf +mcgc-cfg-seg-count-addr+ 'rcx)
      ;; R13 = to_start  (bump ptr for the new segment)
      (emit-mov-reg-abs buf 'r13 +mcgc-cfg-to-start-addr+)
      (emit-jmp buf es-done)
      (emit-label buf es-oom)
      (emit-mov-abs-imm32 buf +mcgc-cfg-oom-addr+ 1)
      (emit-gc-dbg-char buf #x58)          ; 'X' — refill OOM (no free run / seg[] full)
      (emit-label buf es-done))
    (emit-pop buf 'r11) (emit-pop buf 'r10) (emit-pop buf 'r9)
    (emit-pop buf 'r8)  (emit-pop buf 'rdi) (emit-pop buf 'rsi)
    (emit-pop buf 'rdx) (emit-pop buf 'rcx) (emit-pop buf 'rax)
    (emit-ret buf)

    ;; ---- Restore ----
    (emit-label buf restore-label)
    (emit-mov-reg-reg buf 'rsp 'rbp)
    (emit-pop buf 'rbp) (emit-pop buf 'r13) (emit-pop buf 'r11)
    (emit-pop buf 'r10) (emit-pop buf 'rdx) (emit-pop buf 'rcx)
    (emit-pop buf 'rbx) (emit-pop buf 'r9)  (emit-pop buf 'r8)
    (emit-pop buf 'rdi) (emit-pop buf 'rsi) (emit-pop buf 'rax)
    (emit-gc-dbg-char buf #x7D)          ; '}' — page-GC exit
    (emit-ret buf)))
(defun translate-mvm-to-x64 (bytecode function-table)
  "Translate MVM bytecode to x86-64 native code.
   BYTECODE is a vector of (unsigned-byte 8) containing MVM instructions.
   FUNCTION-TABLE is a list of (name offset length) entries describing
   the functions within the bytecode.
   Returns a code-buffer with the native code.

   Side effect: populates *x64-li-const-patches* with a list of
   (native-byte-offset . pool-index) pairs that the image-assembly
   stage uses to patch placeholder MOVABS immediates with real
   tagged constant-pool addresses."
  ;; Reset the patch list for this translation.
  (setf *x64-li-const-patches* nil)
  (let* ((buf (make-code-buffer))
         (n-functions (length function-table))
         ;; Create native labels for each function
         (fn-labels (make-array n-functions))
         (fn-map (make-hash-table :test 'equal))
         ;; Map bytecode-offset → native label for CALL resolution
         (fn-offset-to-label (make-hash-table :test 'eql))
         ;; GC trampoline label (pre-allocated so all translate-states can use it)
         (gc-trampoline-label (when *x64-gc-enabled* (make-label)))
         ;; MCGC stage-4 page-collector trampoline label (only when pinning on).
         ;; Stashed in *mcgc-page-gc-label* so gc-check / ensure-room sites can
         ;; reach it during instruction translation below.
         (page-gc-label (when (mcgc-pinning-on-p) (make-label)))
         ;; Shared cons-kind-bit setter label (only when the kind bitmap is
         ;; emitted).  Created here so cons alloc sites can forward-CALL it;
         ;; body emitted after the functions.
         (cons-bit-label (when (mcgc-kind-bitmap-on-p) (make-label)))
         ;; Handler-stack helpers (push/pop the per-fork stack at #x10000400)
         (handler-push-lbl (make-label))
         (handler-pop-lbl  (make-label))
         ;; BARE-METAL safepoint-deadline stub (YIELD sites call it; the
         ;; PIT ISR only sets the pending flag).  NIL on Linux → YIELD
         ;; stays a NOP and the Linux image is byte-identical.
         (yield-longjmp-lbl (unless *x64-linux-mode* (make-label)))
         ;; Find %GC-COLLECT function in the table (if present)
         (gc-collect-entry (when *x64-gc-enabled*
                             (find "%GC-COLLECT" function-table
                                   :key #'first :test #'string-equal))))
    ;; Publish the page-GC label so gc-check / ensure-room emit calls to it.
    (setf *mcgc-page-gc-label* page-gc-label)
    ;; Publish the cons-kind-bit setter label so cons alloc sites CALL it.
    (setf *mcgc-cons-bit-label* cons-bit-label)
    ;; Allocate a label for each function
    (loop for i from 0 below n-functions
          for entry in function-table
          for name = (first entry)
          for offset = (second entry)
          do (let ((label (make-label)))
               (setf (aref fn-labels i) label)
               (setf (gethash name fn-map) label)
               (setf (gethash offset fn-offset-to-label) label)))
    (setf *x64-genmul-label*
          (loop for entry in function-table
                when (string-equal (first entry) "GENERIC-MULTIPLY")
                  return (gethash (second entry) fn-offset-to-label)))
    (setf *x64-genadd-label*
          (loop for entry in function-table
                when (string-equal (first entry) "GENERIC-ADD")
                  return (gethash (second entry) fn-offset-to-label)))
    ;; Find the label for %gc-collect
    (let ((gc-collect-label (when gc-collect-entry
                              (gethash (second gc-collect-entry)
                                       fn-offset-to-label))))
      ;; Translate each function
      (loop for i from 0 below n-functions
            for entry in function-table
            for name = (first entry)
            for offset = (second entry)
            for length = (third entry)
            do
               (let* ((fn-label (aref fn-labels i))
                      (state (make-translate-state
                              :buf buf
                              :mvm-bytes bytecode
                              :mvm-length length
                              :mvm-offset offset
                              :function-table fn-offset-to-label
                              :gc-label gc-trampoline-label
                              :handler-push-label handler-push-lbl
                              :handler-pop-label handler-pop-lbl
                              :yield-longjmp-label yield-longjmp-lbl)))
                 ;; Emit function label
                 (emit-label buf fn-label)
                 ;; Emit prologue
                 (emit-function-prologue buf)
                 ;; If length=0 (orphaned stub with no bytecode), emit an immediate
                 ;; epilogue+ret so we don't fall through into the next function.
                 (when (zerop length)
                   (emit-function-epilogue buf))
                 ;; Pre-scan branch targets
                 (scan-branch-targets state)
                 ;; Translate instructions
                 (let ((pos offset)
                       (limit (+ offset length)))
                   (loop while (< pos limit)
                         do (progn
                              ;; Emit label if branch target
                              (let ((label (gethash pos
                                                    (translate-state-position-labels state))))
                                (when label
                                  (emit-label buf label)))
                              ;; Decode and translate
                              (let* ((decoded (decode-instruction bytecode pos))
                                     (opcode (car decoded))
                                     (operands (cadr decoded))
                                     (new-pos (cddr decoded)))
                                (handler-case
                                    (translate-instruction state opcode operands new-pos)
                                  (error (c)
                                    (error "~A (fn ~D '~A' mvm-pos ~D opcode ~D operands ~S)"
                                           c i name pos opcode operands)))
                                (setf pos new-pos)))))
                 ;; Function entry alignment.  Functions are tagged
                 ;; with +tag-function+ (= 3) at LI-FUNC time
                 ;; (mvm-fn-addr emits LEA + OR-3).  For the OR-3 to
                 ;; produce a clean tag we need the raw function
                 ;; address's low nibble to be 0 — otherwise the OR
                 ;; merges with stray low bits and the CALL-IND
                 ;; tag-strip (sub 3) lands inside the function body
                 ;; instead of at its entry.
                 (loop
                   (let* ((p (code-buffer-position buf))
                          (n (logand (+ *x64-native-code-offset* p) #xF)))
                     (if (zerop n)
                         (return)
                         (emit-nop buf))))))

      ;; Emit GC trampoline (after all functions, before fixup)
      (when (and gc-trampoline-label gc-collect-label)
        (emit-gc-trampoline buf gc-trampoline-label gc-collect-label)
        (format t "  GC trampoline emitted, %GC-COLLECT wired~%"))
      ;; Emit the shared cons-kind-bit setter (CALLed from cons alloc sites).
      (when cons-bit-label
        (emit-mcgc-cons-bit-subroutine buf cons-bit-label))
      ;; Emit the MCGC stage-4 page collector (only when pinning is enabled;
      ;; flag-off layout is byte-identical since this is skipped entirely).
      (when page-gc-label
        (emit-page-gc-trampoline buf page-gc-label)
        (format t "  MCGC page-GC trampoline emitted (pinning build)~%"))
      ;; Emit handler-stack helpers (push/pop) used by SETJMP/CLEAR-HANDLER
      ;; traps to support nested handler-cases without clobbering the
      ;; parent's setjmp frame.
      (emit-handler-helpers buf handler-push-lbl handler-pop-lbl)
      ;; BARE-METAL: safepoint-deadline stub (see emit-yield-longjmp-stub).
      (when yield-longjmp-lbl
        (emit-yield-longjmp-stub buf yield-longjmp-lbl handler-pop-lbl)))

    ;; Resolve all label fixups
    (fixup-labels buf)
    ;; Return result
    (values buf fn-map)))

;;; ============================================================
;;; Target Descriptor Installation
;;; ============================================================

(defun install-x64-translator ()
  "Install the x86-64 translator into the target descriptor.
   Sets translate-fn, emit-prologue, and emit-epilogue on *target-x86-64*."
  (setf (target-translate-fn modus.mvm:*target-x86-64*)
        #'translate-mvm-to-x64)
  (setf (target-emit-prologue modus.mvm:*target-x86-64*)
        #'emit-function-prologue)
  (setf (target-emit-epilogue modus.mvm:*target-x86-64*)
        #'emit-function-epilogue)
  modus.mvm:*target-x86-64*)

(defun translate-single-instruction (opcode operands target buf)
  "Translate one MVM instruction to native code.
   Conforms to the target translate-fn signature:
   (opcode operands target buf) → native code in buf."
  (declare (ignore target))
  (let ((state (make-translate-state :buf buf)))
    ;; mvm-next-pos is not meaningful for a single instruction
    ;; (branches will need fixup at a higher level)
    (translate-instruction state opcode operands 0)))

;;; ============================================================
;;; Utilities
;;; ============================================================

(defun translated-code-bytes (buf)
  "Return the native code bytes from a code-buffer as a simple vector."
  (let* ((bytes (code-buffer-bytes buf))
         (len (code-buffer-position buf))
         (result (make-array len)))
    (dotimes (i len result)
      (setf (aref result i) (aref bytes i)))))

(defun disassemble-native (buf &key (start 0) (end nil))
  "Print a hex dump of the native code in BUF for debugging."
  (let* ((bytes (code-buffer-bytes buf))
         (limit (or end (code-buffer-position buf))))
    (loop for pos from start below limit
          do (when (zerop (mod (- pos start) 16))
               (when (> pos start) (terpri))
               (format t "  ~4,'0X: " pos))
             (format t "~2,'0X " (aref bytes pos)))
    (terpri)))

(defun translation-statistics (bytecode-length native-buf)
  "Return statistics about the translation.
   Values: native-length, expansion-ratio."
  (let ((native-length (code-buffer-position native-buf)))
    (values native-length
            (if (zerop bytecode-length)
                0.0
                (float (/ native-length bytecode-length))))))
