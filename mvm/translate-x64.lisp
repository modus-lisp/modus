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
  (gc-label nil))

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
              ;; SETJMP: Save RSP, RBP, and return address to fixed memory.
              ;; On first call, returns NIL (#xDEAD0001) in RAX.
              ;; On longjmp, execution resumes here with RAX = T (#xDEAD1009).
              ;;
              ;; Fixed addresses (in Linux heap reserved area):
              ;;   0x10000140: saved RSP
              ;;   0x10000148: saved RBP
              ;;   0x10000150: saved return address (IP after this sequence)
              ;;
              ;; We use LEA + RIP-relative to get the return address.
              ;; Layout:
              ;;   lea rcx, [rip+N]    ; address of "return point" after the jmp
              ;;   mov [addr], rsp     ; save RSP
              ;;   mov [addr+8], rbp   ; save RBP
              ;;   mov [addr+16], rcx  ; save return IP
              ;;   mov rax, NIL        ; first-time return value
              ;;   jmp +5              ; skip longjmp-return block
              ;;   (longjmp return point — RAX already has T from longjmp)
              ;;
              ;; Save RSP to 0x10000140
              ;; Use movabs with RCX as temp (address > 0x7FFFFFFF, can't use disp32)
              ;; mov rcx, 0x10000140
              (emit-bytes buf #x48 #xB9)
              (emit-u32 buf #x10000140) (emit-u32 buf 0)
              ;; mov [rcx], rsp
              (emit-bytes buf #x48 #x89 #x21)
              ;; mov [rcx+8], rbp
              (emit-bytes buf #x48 #x89 #x69 #x08)
              ;; lea rax, [rip+2]  — address of instruction after the JMP below
              ;; The JMP short is 2 bytes (EB xx), so we want rip+2 to point past it
              (emit-bytes buf #x48 #x8D #x05 #x02 #x00 #x00 #x00)  ; lea rax, [rip+2]
              ;; mov [rcx+16], rax  — save return IP
              (emit-bytes buf #x48 #x89 #x41 #x10)
              ;; mov rax, NIL (#xDEAD0001) — first-time return
              (emit-bytes buf #x48 #xB8)
              (emit-u32 buf #xDEAD0001) (emit-u32 buf 0)
              ;; jmp +0  — skip 0 bytes (the longjmp path uses the saved IP directly)
              ;; No skip needed: longjmp jumps to the saved IP which is here
              ;; RAX has NIL for normal flow; longjmp sets RAX to T before jumping
              )
             ((= code #x0511)
              ;; LONGJMP: Restore RSP/RBP from fixed memory, jump to saved IP.
              ;; Sets RAX to T (#xDEAD1009) so setjmp "returns" non-nil.
              ;;
              ;; Clear the handler first (set saved RSP to 0)
              ;; mov rcx, 0x10000140
              (emit-bytes buf #x48 #xB9)
              (emit-u32 buf #x10000140) (emit-u32 buf 0)
              ;; mov rdx, [rcx+16]  — saved return IP
              (emit-bytes buf #x48 #x8B #x51 #x10)
              ;; mov rbp, [rcx+8]   — restore RBP
              (emit-bytes buf #x48 #x8B #x69 #x08)
              ;; mov rsp, [rcx]     — restore RSP
              (emit-bytes buf #x48 #x8B #x21)
              ;; Clear handler: mov qword [rcx], 0
              (emit-bytes buf #x48 #xC7 #x01 #x00 #x00 #x00 #x00)
              ;; mov rax, T (#xDEAD1009)  — longjmp return value
              (emit-bytes buf #x48 #xB8)
              (emit-u32 buf #xDEAD1009) (emit-u32 buf 0)
              ;; jmp rdx  — jump to saved return address
              (emit-bytes buf #xFF #xE2))
             ((= code #x0512)
              ;; CLEAR-HANDLER: Set saved RSP at 0x10000140 to 0
              ;; mov rcx, 0x10000140
              (emit-bytes buf #x48 #xB9)
              (emit-u32 buf #x10000140) (emit-u32 buf 0)
              ;; mov qword [rcx], 0
              (emit-bytes buf #x48 #xC7 #x01 #x00 #x00 #x00 #x00))
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

        ((op= +op-mul+)
         ;; (mul Vd Va Vb) — tagged: result = (Va * Vb) >> 1
         ;; Since both inputs carry the <<1 fixnum tag, the product
         ;; has a factor of 4 where we need 2, so SAR 1 corrects.
         ;;
         ;; x86-64 IMUL r64, r/m64 (two-operand form):
         ;;   REX.W 0F AF /r
         (let* ((vd (first operands))
                (va (second operands))
                (vb (third operands))
                (d (dest-phys-or-scratch vd)))
           ;; Load Va into d
           (emit-load-vreg buf va d)
           ;; IMUL d, Vb-phys
           (let ((pb (vreg-phys vb)))
             (if pb
                 (emit-imul-reg-reg buf d pb)
                 (progn
                   ;; Vb spilled — load into temp
                   (let ((tmp (if (eq d 'rax) 'r13 'rax)))
                     (emit-push buf tmp)
                     (emit-load-vreg buf vb tmp)
                     (emit-imul-reg-reg buf d tmp)
                     (emit-pop buf tmp)))))
           ;; Fix tagging: SAR d, 1
           (emit-sar-reg-imm buf d 1)
           (maybe-store-scratch buf vd)))

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
                (header (logior (ash count 8) subtag)))
           ;; Write header
           (emit-mov-reg-imm buf +scratch-reg+ header)
           (emit-mov-mem-reg buf 'r12 +scratch-reg+ 0)
           ;; Result = R12 | object-tag
           (emit-lea buf d 'r12 #x09)
           ;; Advance alloc pointer: (count+2)*8, aligned to 16
           (let ((alloc-bytes (logand (+ (* (+ count 2) 8) 15) (lognot 15))))
             (emit-add-reg-imm buf 'r12 alloc-bytes))
           (maybe-store-scratch buf vd)))

        ((op= +op-alloc-array+)
         ;; (alloc-array Vd Vcount) — dynamic array allocation
         ;; Vcount: UNTAGGED element count (compiler SAR'd it)
         ;; Allocates (count+1)*8 bytes, aligned to 16 (header + elements)
         ;; Header = (count << 8) | array-subtag
         ;; Result = R12 | 0x09 (object tag)
         (let* ((vd (first operands))
                (vcount (second operands))
                (d (dest-phys-or-scratch vd))
                (pc (vreg-phys vcount)))
           ;; Load count into scratch register
           (if pc
               (emit-mov-reg-reg buf +scratch-reg+ pc)
               (emit-load-vreg buf vcount +scratch-reg+))
           ;; Save count on stack (will be clobbered by header build)
           (emit-push buf +scratch-reg+)
           ;; Build header: (count << 8) | subtag-array
           (emit-shl-reg-imm buf +scratch-reg+ 8)
           (emit-or-reg-imm buf +scratch-reg+ #x32)  ; array subtag
           ;; Write header at [R12]
           (emit-mov-mem-reg buf 'r12 +scratch-reg+ 0)
           ;; Result = R12 | 0x09 (object tag)
           (emit-lea buf d 'r12 #x09)
           ;; Restore count, compute allocation size
           (emit-pop buf +scratch-reg+)
           ;; size = (count + 2) << 3, aligned to 16
           ;; +2 because elements start at offset +16 (header + padding)
           (emit-add-reg-imm buf +scratch-reg+ 2)  ; count + 2
           (emit-shl-reg-imm buf +scratch-reg+ 3)  ; * 8 bytes per word
           (emit-add-reg-imm buf +scratch-reg+ 15)  ; for alignment
           (emit-and-reg-imm buf +scratch-reg+ -16) ; align to 16
           ;; Advance alloc pointer
           (emit-add-reg-reg buf 'r12 +scratch-reg+)
           (maybe-store-scratch buf vd)))

        ((op= +op-alloc-string+)
         ;; Like alloc-array but with string subtag #x31
         (let* ((vd (first operands))
                (vcount (second operands))
                (d (dest-phys-or-scratch vd))
                (pc (vreg-phys vcount)))
           (if pc (emit-mov-reg-reg buf +scratch-reg+ pc)
               (emit-load-vreg buf vcount +scratch-reg+))
           (emit-push buf +scratch-reg+)
           (emit-shl-reg-imm buf +scratch-reg+ 8)
           (emit-or-reg-imm buf +scratch-reg+ #x31)  ; STRING subtag
           (emit-mov-mem-reg buf 'r12 +scratch-reg+ 0)
           (emit-lea buf d 'r12 #x09)
           (emit-pop buf +scratch-reg+)
           (emit-add-reg-imm buf +scratch-reg+ 2)
           (emit-shl-reg-imm buf +scratch-reg+ 3)
           (emit-add-reg-imm buf +scratch-reg+ 15)
           (emit-and-reg-imm buf +scratch-reg+ -16)
           (emit-add-reg-reg buf 'r12 +scratch-reg+)
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
         ;; Header is at [Vs - 9] (untag object pointer)
         (let* ((vd (first operands))
                (vs (second operands))
                (d (dest-phys-or-scratch vd)))
           (let ((ps (vreg-phys vs)))
             (if ps
                 (emit-mov-reg-mem buf d ps -9)
                 (progn
                   (let ((tmp (if (eq d 'rax) 'r13 'rax)))
                     (emit-push buf tmp)
                     (emit-load-vreg buf vs tmp)
                     (emit-mov-reg-mem buf d tmp -9)
                     (emit-pop buf tmp)))))
           ;; Extract low 8 bits of header as subtag
           (emit-and-reg-imm buf d #xFF)
           ;; Tag as fixnum
           (emit-shl-reg-imm buf d 1)
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
         ;; Header at [Vobj - 9], count = header >> 8, tagged = count << 1
         (let* ((vd (first operands))
                (vobj (second operands))
                (d (dest-phys-or-scratch vd)))
           (let ((po (vreg-phys vobj)))
             (if po
                 (emit-mov-reg-mem buf d po -9)
                 (progn
                   (let ((tmp (if (eq d 'rax) 'r13 'rax)))
                     (emit-push buf tmp)
                     (emit-load-vreg buf vobj tmp)
                     (emit-mov-reg-mem buf d tmp -9)
                     (emit-pop buf tmp)))))
           ;; header >> 8 gives element count, << 1 tags as fixnum
           (emit-shr-reg-imm buf d 8)
           (emit-shl-reg-imm buf d 1)
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
         ;; (call-ind Vs) — indirect call through register
         (let* ((vs (first operands))
                (ps (vreg-phys vs)))
           (if ps
               (emit-call-reg buf ps)
               (progn
                 (emit-load-vreg buf vs +scratch-reg+)
                 (emit-call-reg buf +scratch-reg+)))))

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
           (emit-mov-reg-reg buf d 'r12)
           (emit-or-reg-imm buf d 1)      ; tag as cons
           (emit-add-reg-imm buf 'r12 16)  ; advance alloc pointer
           (maybe-store-scratch buf vd)))

        ((op= +op-gc-check+)
         ;; Check R12 (alloc ptr) against R14 (alloc limit)
         ;; If R12 >= R14, call GC (or NOP if no GC configured)
         (let ((gc-lbl (translate-state-gc-label state)))
           (when gc-lbl
             (let ((skip-label (make-label)))
               (emit-cmp-reg-reg buf 'r12 'r14)
               (emit-jcc buf :l skip-label)    ; if alloc < limit, skip
               (emit-call buf gc-lbl)
               (emit-label buf skip-label)))))

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
         ;; Preemption check: decrement a yield counter and call the
         ;; scheduler if it reaches zero.  Stub: emit NOP for now.
         (emit-nop buf))

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
         ;; Load the native address of a function into Vd.
         ;; Target is the bytecode offset, resolved via function table
         ;; to a native label. Uses LEA [RIP+disp32] for position-independent
         ;; address loading.
         (let* ((vd (first operands))
                (target-offset (second operands))
                (fn-table (translate-state-function-table state))
                (label (when fn-table (gethash target-offset fn-table)))
                (d (dest-phys-or-scratch vd)))
           (if label
               (emit-lea-label buf d label)
               ;; Unknown target — load 0
               (emit-mov-reg-imm buf d 0))
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

    ;; ---- Scan globals roots ----
    ;; Globals alist at 0x10000080
    (emit-mov-reg-imm buf 'rax #x10000080)
    (emit-call buf scan-word-label)
    ;; Symbol table at 0x10000088
    (emit-mov-reg-imm buf 'rax #x10000088)
    (emit-call buf scan-word-label)

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
      ;; Copy R8 bytes from RSI to R13 using REP MOVSQ
      ;; Save RDI and RCX (used by caller for stack scan / from_end)
      (emit-push buf 'rdi)
      (emit-push buf 'rcx)
      ;; Save old R13 (start of dest) for new tagged pointer
      (emit-push buf 'r13)
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
    (emit-ret buf)))

(defun translate-mvm-to-x64 (bytecode function-table)
  "Translate MVM bytecode to x86-64 native code.
   BYTECODE is a vector of (unsigned-byte 8) containing MVM instructions.
   FUNCTION-TABLE is a list of (name offset length) entries describing
   the functions within the bytecode.
   Returns a code-buffer with the native code."
  (let* ((buf (make-code-buffer))
         (n-functions (length function-table))
         ;; Create native labels for each function
         (fn-labels (make-array n-functions))
         (fn-map (make-hash-table :test 'equal))
         ;; Map bytecode-offset → native label for CALL resolution
         (fn-offset-to-label (make-hash-table :test 'eql))
         ;; GC trampoline label (pre-allocated so all translate-states can use it)
         (gc-trampoline-label (when *x64-gc-enabled* (make-label)))
         ;; Find %GC-COLLECT function in the table (if present)
         (gc-collect-entry (when *x64-gc-enabled*
                             (find "%GC-COLLECT" function-table
                                   :key #'first :test #'string-equal))))
    ;; Allocate a label for each function
    (loop for i from 0 below n-functions
          for entry in function-table
          for name = (first entry)
          for offset = (second entry)
          do (let ((label (make-label)))
               (setf (aref fn-labels i) label)
               (setf (gethash name fn-map) label)
               (setf (gethash offset fn-offset-to-label) label)))
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
                              :gc-label gc-trampoline-label)))
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
                                (setf pos new-pos)))))))

      ;; Emit GC trampoline (after all functions, before fixup)
      (when (and gc-trampoline-label gc-collect-label)
        (emit-gc-trampoline buf gc-trampoline-label gc-collect-label)
        (format t "  GC trampoline emitted, %GC-COLLECT wired~%")))

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
