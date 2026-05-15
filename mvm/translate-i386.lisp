;;;; translate-i386.lisp - MVM Bytecode to i386 (32-bit x86) Native Code Translator
;;;;
;;;; Translates MVM (Modus Virtual Machine) bytecode into i386 machine code.
;;;; This is the HARDEST target because i386 has only 8 GPRs and most virtual
;;;; registers must be spilled to the stack frame.
;;;;
;;;; Register mapping (from target.lisp):
;;;;   V0  -> ESI   (arg0)
;;;;   V1  -> EDI   (arg1)
;;;;   V2  -> stack  (arg2, always spills -- only 2 register args!)
;;;;   V3  -> stack  (arg3, always spills)
;;;;   V4  -> EBX   (callee-saved general purpose)
;;;;   V5-V15 -> stack spill
;;;;   VR  -> EAX   (return value)
;;;;   VA  -> stack slot (no dedicated alloc register!)
;;;;   VL  -> stack slot (alloc limit)
;;;;   VN  -> stack slot (NIL constant address)
;;;;   VSP -> ESP   (stack pointer)
;;;;   VFP -> EBP   (frame pointer)
;;;;
;;;; Scratch registers: ECX, EDX (caller-saved, not mapped to any vreg)
;;;;   - ECX is primary scratch for spill mediation
;;;;   - EDX is secondary scratch, also used by CDQ/IDIV
;;;;   - EAX is VR but also used as scratch for arithmetic requiring it
;;;;
;;;; Key challenges:
;;;;   1. Only 3 virtual GPRs have physical mappings (V0, V1, V4) + VR(EAX)
;;;;   2. All arithmetic for spilled regs requires load/op/store sequences
;;;;   3. 32-bit word size: tagged fixnums use 31 bits (value << 1, LSB=0)
;;;;   4. No REX prefix (unlike x86-64); 32-bit operand size is default
;;;;   5. VA, VL, VN are stack slots -- alloc/GC checks need explicit loads
;;;;   6. Cons cell = 8 bytes (car at [ptr], cdr at [ptr+4])
;;;;   7. Cons tag = 0x01 (low 4 bits), Object tag = 0x09
;;;;
;;;; i386 encoding:
;;;;   [prefix] [opcode] [ModR/M] [SIB] [displacement] [immediate]
;;;;   ModR/M: [mod:2 | reg:3 | r/m:3]
;;;;   SIB:    [scale:2 | index:3 | base:3]
;;;;   No REX prefix; PUSH/POP move 4 bytes (not 8)
;;;;
;;;; Two-pass translation (like x64):
;;;;   Pass 1: Scan for branch targets, create labels
;;;;   Pass 2: Emit native code, using labels for forward references

(in-package :cl-user)

(defpackage :modus.mvm.i386
  (:use :cl :modus.mvm)
  (:export
   #:translate-mvm-to-i386
   #:translate-i386-function
   #:install-i386-translator
   #:i386-buffer
   #:i386-buffer-bytes
   #:i386-buffer-to-bytes
   #:i386-disassemble-native))

(in-package :modus.mvm.i386)

;;; ============================================================
;;; i386 Physical Register Encoding
;;; ============================================================
;;;
;;; The 8 GPRs of i386 and their 3-bit ModR/M encoding:
;;;   EAX=0  ECX=1  EDX=2  EBX=3  ESP=4  EBP=5  ESI=6  EDI=7

(defconstant +i386-eax+ 0)
(defconstant +i386-ecx+ 1)
(defconstant +i386-edx+ 2)
(defconstant +i386-ebx+ 3)
(defconstant +i386-esp+ 4)
(defconstant +i386-ebp+ 5)
(defconstant +i386-esi+ 6)
(defconstant +i386-edi+ 7)

;;; Scratch registers (not mapped to any virtual register)
(defconstant +scratch0+ +i386-ecx+  "ECX - primary scratch register")
(defconstant +scratch1+ +i386-edx+  "EDX - secondary scratch register")

;;; ============================================================
;;; Virtual -> Physical Register Mapping
;;; ============================================================
;;;
;;; NIL means the virtual register spills to the stack frame.

(defparameter *i386-vreg-map*
  (vector +i386-esi+     ; V0  -> ESI
          +i386-edi+     ; V1  -> EDI
          nil nil         ; V2, V3 (spill -- only 2 arg regs on i386)
          +i386-ebx+     ; V4  -> EBX
          nil nil nil     ; V5-V7 (spill)
          nil nil nil nil ; V8-V11 (spill)
          nil nil nil nil ; V12-V15 (spill)
          +i386-eax+     ; VR  -> EAX
          nil             ; VA  -> spill (alloc pointer)
          nil             ; VL  -> spill (alloc limit)
          nil             ; VN  -> spill (NIL constant)
          +i386-esp+     ; VSP -> ESP
          +i386-ebp+     ; VFP -> EBP
          nil))           ; VPC -> not mapped

;;; ============================================================
;;; Tagged Value Constants
;;; ============================================================

(defconstant +tag-cons+    #x01 "Cons tag: low 4 bits = 0001")
(defconstant +tag-object+  #x09 "Object tag: low 4 bits = 1001")
(defconstant +tag-mask+    #x0F "Mask for extracting tag from pointer")
(defconstant +i386-mvm-t+  #xDEAD1009 "Placeholder T value (object-tagged marker)")

;;; ============================================================
;;; Stack Frame Layout
;;; ============================================================
;;;
;;; All offsets are negative from EBP. The frame is structured as:
;;;
;;;   [EBP + 8]  = return address (pushed by CALL)
;;;   [EBP + 4]  = old EBP (pushed by PUSH EBP in prologue)
;;;   [EBP + 0]  = current frame pointer
;;;   [EBP +12] = V3 incoming arg (pushed by caller's CALL handler)
;;;   [EBP + 8] = V2 incoming arg (pushed by caller's CALL handler)
;;;   [EBP + 4] = return address (pushed by CALL)
;;;   [EBP + 0] = saved EBP (pushed by PUSH EBP in prologue)
;;;   [EBP -  4] = saved EBX (callee-saved)
;;;   [EBP -  8] = saved ESI (callee-saved)
;;;   [EBP - 12] = saved EDI (callee-saved)
;;;   [EBP - 16] = V2 local spill slot (copied from [EBP+8] in prologue)
;;;   [EBP - 20] = V3 local spill slot (copied from [EBP+12] in prologue)
;;;   [EBP - 24] = V5 spill slot    (V4 = EBX, not spilled)
;;;   [EBP - 28] = V6 spill slot
;;;   [EBP - 32] = V7 spill slot
;;;   [EBP - 36] = V8 spill slot
;;;   [EBP - 40] = V9 spill slot
;;;   [EBP - 44] = V10 spill slot
;;;   [EBP - 48] = V11 spill slot
;;;   [EBP - 52] = V12 spill slot
;;;   [EBP - 56] = V13 spill slot
;;;   [EBP - 60] = V14 spill slot
;;;   [EBP - 64] = V15 spill slot
;;;   Total frame reservation: 64 bytes + 32 frame slots = 96, rounded to 100
;;;
;;;   VA, VL, VN are at absolute addresses (0x600, 0x604, 0x608).
;;;   CALL handler pushes V2/V3 before CALL, callee copies to local slots.

(defconstant +save-ebx-off+  -4)
(defconstant +save-esi-off+  -8)
(defconstant +save-edi-off+ -12)
;;; VA, VL, VN are stored at FIXED memory addresses (not EBP-relative)
;;; because each function creates a new frame with a new EBP.
;;; Using absolute addresses ensures alloc state is globally accessible.
(defconstant +va-addr+      #x600)  ; Alloc pointer (absolute address)
(defconstant +vl-addr+      #x604)  ; Alloc limit (absolute address)
(defconstant +vn-addr+      #x608)  ; NIL constant (absolute address)
(defconstant +spill-base+   -28)   ; First general spill slot

(defun i386-spill-offset (vreg)
  "Calculate EBP-relative offset for a spilled virtual register.
   Returns a negative integer for valid spill vregs."
  (cond
    ;; VA, VL, VN use absolute addresses — handled specially, not EBP-relative
    ((= vreg +vreg-va+)  (error "i386: VA uses absolute address, not spill offset"))
    ((= vreg +vreg-vl+)  (error "i386: VL uses absolute address, not spill offset"))
    ((= vreg +vreg-vn+)  (error "i386: VN uses absolute address, not spill offset"))
    ;; V2, V3 — local spill slots (incoming args copied from [EBP+8/12] in prologue)
    ((= vreg 2)  -16)
    ((= vreg 3)  -20)
    ;; V5-V15 (V4 = EBX, not spilled) — local spill slots
    ((and (>= vreg 5) (<= vreg 15))
     (- -24 (* (- vreg 5) 4)))
    (t (error "i386: unexpected spill for vreg ~D" vreg))))

(defconstant +frame-slot-base+ -68
  "EBP-relative offset for frame slot 0 (local variables via obj-ref VFP).
   Frame layout: saved regs at -4/-8/-12, V2/V3 at -16/-20, V5-V15 at -24 to -64.
   Frame slots start at -68.
   Slot N is at EBP + frame-slot-base - N*4.")

(defconstant +frame-size+ 296
  "Total frame reservation in bytes. Includes saved regs, spill slots,
   and frame slots for local variables (obj-ref VFP N).
   84 bytes (regs+spills) + up to 200 bytes frame slots (50 slots) = 284, rounded to 296.")

;;; ============================================================
;;; Virtual Register Helpers
;;; ============================================================

(defun i386-vreg-phys (vreg)
  "Return the physical register code for VREG, or NIL if it spills."
  (and (< vreg (length *i386-vreg-map*))
       (aref *i386-vreg-map* vreg)))

(defun i386-vreg-spills-p (vreg)
  "Does VREG spill to the stack on i386?"
  (null (i386-vreg-phys vreg)))

;;; ============================================================
;;; i386 Code Buffer
;;; ============================================================

(defstruct i386-buffer
  (bytes (make-array 1572864))          ; 1.5MB, position tracks fill
  (labels (make-hash-table :test 'eql))
  (fixups nil)     ; list of (byte-position label-id fixup-type)
  (position 0))

(defun i386-emit-byte (buf byte)
  "Emit a single byte."
  (let ((pos (i386-buffer-position buf)))
    (setf (aref (i386-buffer-bytes buf) pos) (logand byte #xFF))
    (setf (i386-buffer-position buf) (+ pos 1))))

(defun i386-emit-u16 (buf val)
  "Emit a 16-bit little-endian value."
  (i386-emit-byte buf (logand val #xFF))
  (i386-emit-byte buf (logand (ash val -8) #xFF)))

(defun i386-emit-u32 (buf val)
  "Emit a 32-bit little-endian value."
  (i386-emit-byte buf (logand val #xFF))
  (i386-emit-byte buf (logand (ash val -8) #xFF))
  (i386-emit-byte buf (logand (ash val -16) #xFF))
  (i386-emit-byte buf (logand (ash val -24) #xFF)))

(defun i386-emit-s32 (buf val)
  "Emit a signed 32-bit little-endian value."
  (i386-emit-u32 buf (if (minusp val) (logand val #xFFFFFFFF) val)))

(defun i386-emit-s8 (buf val)
  "Emit a signed 8-bit value."
  (i386-emit-byte buf (if (minusp val) (logand val #xFF) val)))

(defun i386-current-pos (buf)
  "Current byte position in the code buffer."
  (i386-buffer-position buf))

(defun i386-emit-label (buf label-id)
  "Record current position as a branch target."
  (setf (gethash label-id (i386-buffer-labels buf))
        (i386-buffer-position buf)))

(defun i386-make-label ()
  "Create a new unique label."
  (mvm-make-label))

(defun i386-emit-fixup-rel32 (buf label-id)
  "Emit a 32-bit placeholder and record a rel32 fixup."
  (push (list (i386-buffer-position buf) label-id :rel32)
        (i386-buffer-fixups buf))
  (i386-emit-u32 buf 0))

(defun i386-fixup-labels (buf)
  "Resolve all branch label references.  Asserts rel32 fits in signed
   32-bit before encoding; out-of-range silently truncates and becomes
   a wild jump.  (Linux ELF max code size is 4GB so rel32 can in
   principle overflow once images grow past 2GB — currently impossible
   but the assert costs nothing.)"
  (let ((bytes (i386-buffer-bytes buf)))
    (dolist (fixup (i386-buffer-fixups buf))
      (destructuring-bind (pos label-id fixup-type) fixup
        (let ((target (gethash label-id (i386-buffer-labels buf))))
          (unless target
            (error "i386: undefined label ~A" label-id))
          (ecase fixup-type
            (:rel32
             ;; rel32 is relative to end of the 4-byte displacement field
             (let* ((rel (- target (+ pos 4))))
               (unless (<= -2147483648 rel 2147483647)
                 (error "i386 rel32 ~D out of range at pos ~D — ~
                         would silently truncate"
                        rel pos))
               (let ((urel (if (minusp rel) (logand rel #xFFFFFFFF) rel)))
                 (setf (aref bytes (+ pos 0)) (logand urel #xFF)
                       (aref bytes (+ pos 1)) (logand (ash urel -8) #xFF)
                       (aref bytes (+ pos 2)) (logand (ash urel -16) #xFF)
                       (aref bytes (+ pos 3)) (logand (ash urel -24) #xFF)))))))))))

(defun i386-buffer-to-bytes (buf)
  "Return the code buffer as a simple byte vector."
  (let* ((len (i386-buffer-position buf))
         (result (make-array len)))
    (dotimes (i len result)
      (setf (aref result i) (aref (i386-buffer-bytes buf) i)))))

;;; ============================================================
;;; i386 ModR/M and SIB Encoding
;;; ============================================================

(defun i386-modrm (mod reg rm)
  "Build a ModR/M byte: [mod:2 | reg:3 | r/m:3]"
  (logior (ash (logand mod 3) 6)
          (ash (logand reg 7) 3)
          (logand rm 7)))

(defun i386-sib (scale index base)
  "Build a SIB byte: [scale:2 | index:3 | base:3]"
  (logior (ash (logand scale 3) 6)
          (ash (logand index 7) 3)
          (logand base 7)))

(defun i386-emit-modrm-mem (buf reg-field base-reg offset)
  "Emit ModR/M (and SIB if needed) for [base-reg + offset] addressing.
   REG-FIELD is the /r field (register or opcode extension).
   Handles ESP (needs SIB) and EBP (needs explicit disp) edge cases."
  (let ((needs-sib (= base-reg +i386-esp+)))
    (cond
      ;; No displacement (but EBP always needs at least disp8)
      ((and (zerop offset) (/= base-reg +i386-ebp+))
       (if needs-sib
           (progn
             (i386-emit-byte buf (i386-modrm #b00 reg-field 4))
             (i386-emit-byte buf (i386-sib 0 4 +i386-esp+)))
           (i386-emit-byte buf (i386-modrm #b00 reg-field base-reg))))
      ;; 8-bit displacement fits
      ((<= -128 offset 127)
       (if needs-sib
           (progn
             (i386-emit-byte buf (i386-modrm #b01 reg-field 4))
             (i386-emit-byte buf (i386-sib 0 4 +i386-esp+)))
           (i386-emit-byte buf (i386-modrm #b01 reg-field base-reg)))
       (i386-emit-s8 buf offset))
      ;; Full 32-bit displacement
      (t
       (if needs-sib
           (progn
             (i386-emit-byte buf (i386-modrm #b10 reg-field 4))
             (i386-emit-byte buf (i386-sib 0 4 +i386-esp+)))
           (i386-emit-byte buf (i386-modrm #b10 reg-field base-reg)))
       (i386-emit-s32 buf offset)))))

;;; ============================================================
;;; i386 Instruction Emitters
;;; ============================================================

;;; --- MOV ---

(defun i386-emit-mov-reg-reg (buf dst src)
  "MOV dst, src (register-to-register, 32-bit)"
  (i386-emit-byte buf #x89)   ; MOV r/m32, r32
  (i386-emit-byte buf (i386-modrm #b11 src dst)))

(defun i386-emit-mov-reg-imm (buf reg imm)
  "MOV reg, imm32"
  (i386-emit-byte buf (+ #xB8 reg))
  (i386-emit-u32 buf (logand imm #xFFFFFFFF)))

(defun i386-emit-mov-reg-mem (buf reg base offset)
  "MOV reg, [base + offset]"
  (i386-emit-byte buf #x8B)
  (i386-emit-modrm-mem buf reg base offset))

(defun i386-emit-mov-mem-reg (buf base offset reg)
  "MOV [base + offset], reg"
  (i386-emit-byte buf #x89)
  (i386-emit-modrm-mem buf reg base offset))

(defun i386-emit-mov-mem-imm (buf base offset imm)
  "MOV DWORD [base + offset], imm32"
  (i386-emit-byte buf #xC7)
  (i386-emit-modrm-mem buf 0 base offset)
  (i386-emit-u32 buf (logand imm #xFFFFFFFF)))

;;; --- Absolute memory addressing (for VA/VL/VN globals) ---

(defun i386-emit-mov-reg-abs (buf reg addr)
  "MOV reg, [addr] (absolute 32-bit address, no base register)"
  ;; Encoding: 8B /r mod=00 r/m=5 disp32
  (i386-emit-byte buf #x8B)
  (i386-emit-byte buf (i386-modrm #b00 reg 5))
  (i386-emit-u32 buf (logand addr #xFFFFFFFF)))

(defun i386-emit-mov-abs-reg (buf addr reg)
  "MOV [addr], reg (absolute 32-bit address, no base register)"
  ;; Encoding: 89 /r mod=00 r/m=5 disp32
  (i386-emit-byte buf #x89)
  (i386-emit-byte buf (i386-modrm #b00 reg 5))
  (i386-emit-u32 buf (logand addr #xFFFFFFFF)))

(defun i386-emit-cmp-reg-abs (buf reg addr)
  "CMP reg, [addr] (compare register with absolute memory)"
  ;; Encoding: 3B /r mod=00 r/m=5 disp32
  (i386-emit-byte buf #x3B)
  (i386-emit-byte buf (i386-modrm #b00 reg 5))
  (i386-emit-u32 buf (logand addr #xFFFFFFFF)))

(defun i386-emit-push-abs (buf addr)
  "PUSH DWORD [addr] (push from absolute memory address)"
  ;; Encoding: FF /6 mod=00 r/m=5 disp32
  (i386-emit-byte buf #xFF)
  (i386-emit-byte buf (i386-modrm #b00 6 5))
  (i386-emit-u32 buf (logand addr #xFFFFFFFF)))

(defun i386-emit-pop-abs (buf addr)
  "POP DWORD [addr] (pop to absolute memory address)"
  ;; Encoding: 8F /0 mod=00 r/m=5 disp32
  (i386-emit-byte buf #x8F)
  (i386-emit-byte buf (i386-modrm #b00 0 5))
  (i386-emit-u32 buf (logand addr #xFFFFFFFF)))

;;; --- Load/Store for 8/16-bit widths ---

(defun i386-emit-movzx-byte (buf reg base offset)
  "MOVZX reg, BYTE [base+offset]"
  (i386-emit-byte buf #x0F)
  (i386-emit-byte buf #xB6)
  (i386-emit-modrm-mem buf reg base offset))

(defun i386-emit-movzx-word (buf reg base offset)
  "MOVZX reg, WORD [base+offset]"
  (i386-emit-byte buf #x0F)
  (i386-emit-byte buf #xB7)
  (i386-emit-modrm-mem buf reg base offset))

(defun i386-emit-mov-mem8-reg (buf base offset reg)
  "MOV BYTE [base+offset], reg (low 8 bits)"
  (i386-emit-byte buf #x88)
  (i386-emit-modrm-mem buf reg base offset))

(defun i386-emit-mov-mem16-reg (buf base offset reg)
  "MOV WORD [base+offset], reg (low 16 bits)"
  (i386-emit-byte buf #x66)   ; operand-size override prefix
  (i386-emit-byte buf #x89)
  (i386-emit-modrm-mem buf reg base offset))

;;; --- PUSH / POP ---

(defun i386-emit-push-reg (buf reg)
  "PUSH reg (32-bit)"
  (i386-emit-byte buf (+ #x50 reg)))

(defun i386-emit-pop-reg (buf reg)
  "POP reg (32-bit)"
  (i386-emit-byte buf (+ #x58 reg)))

(defun i386-emit-push-imm32 (buf imm)
  "PUSH imm32"
  (i386-emit-byte buf #x68)
  (i386-emit-u32 buf (logand imm #xFFFFFFFF)))

(defun i386-emit-push-mem (buf base offset)
  "PUSH DWORD [base+offset]"
  (i386-emit-byte buf #xFF)
  (i386-emit-modrm-mem buf 6 base offset))

;;; --- ALU Operations ---

(defmacro def-i386-alu (name opcode-rr opcode-ri-8 opcode-ri-32 modrm-ext
                         &optional (opcode-rm nil) (opcode-mr nil))
  "Define i386 ALU instruction forms: reg-reg, reg-imm, reg-mem, mem-reg."
  (let ((pkg (find-package :modus.mvm.i386)))
    `(progn
       (defun ,(intern (format nil "I386-EMIT-~A-REG-REG" name) pkg) (buf dst src)
         ,(format nil "~A dst, src (register-register)" name)
         (i386-emit-byte buf ,opcode-rr)
         (i386-emit-byte buf (i386-modrm #b11 src dst)))

       (defun ,(intern (format nil "I386-EMIT-~A-REG-IMM" name) pkg) (buf reg imm)
         ,(format nil "~A reg, imm" name)
         (cond
           ;; Sign-extended 8-bit immediate (most common)
           ((<= -128 imm 127)
            (i386-emit-byte buf ,opcode-ri-8)
            (i386-emit-byte buf (i386-modrm #b11 ,modrm-ext reg))
            (i386-emit-s8 buf imm))
           ;; Short form for EAX
           ((= reg +i386-eax+)
            (i386-emit-byte buf ,opcode-ri-32)
            (i386-emit-s32 buf imm))
           ;; General form
           (t
            (i386-emit-byte buf #x81)
            (i386-emit-byte buf (i386-modrm #b11 ,modrm-ext reg))
            (i386-emit-s32 buf imm))))

       ,@(when opcode-rm
           `((defun ,(intern (format nil "I386-EMIT-~A-REG-MEM" name) pkg) (buf reg base offset)
               ,(format nil "~A reg, [base+offset]" name)
               (i386-emit-byte buf ,opcode-rm)
               (i386-emit-modrm-mem buf reg base offset))))

       ,@(when opcode-mr
           `((defun ,(intern (format nil "I386-EMIT-~A-MEM-REG" name) pkg) (buf base offset reg)
               ,(format nil "~A [base+offset], reg" name)
               (i386-emit-byte buf ,opcode-mr)
               (i386-emit-modrm-mem buf reg base offset)))))))

;; Define all standard ALU operations: name, rr, ri8, ri32-eax, /ext, rm, mr
(def-i386-alu "ADD" #x01 #x83 #x05 0 #x03 #x01)
(def-i386-alu "SUB" #x29 #x83 #x2D 5 #x2B #x29)
(def-i386-alu "CMP" #x39 #x83 #x3D 7 #x3B #x39)
(def-i386-alu "AND" #x21 #x83 #x25 4 #x23 #x21)
(def-i386-alu "OR"  #x09 #x83 #x0D 1 #x0B #x09)
(def-i386-alu "XOR" #x31 #x83 #x35 6 #x33 #x31)

;;; --- TEST ---

(defun i386-emit-test-reg-reg (buf r1 r2)
  "TEST r1, r2 (AND, set flags, discard result)"
  (i386-emit-byte buf #x85)
  (i386-emit-byte buf (i386-modrm #b11 r2 r1)))

(defun i386-emit-test-reg-imm (buf reg imm)
  "TEST reg, imm32"
  (if (= reg +i386-eax+)
      (progn
        (i386-emit-byte buf #xA9)
        (i386-emit-u32 buf (logand imm #xFFFFFFFF)))
      (progn
        (i386-emit-byte buf #xF7)
        (i386-emit-byte buf (i386-modrm #b11 0 reg))
        (i386-emit-u32 buf (logand imm #xFFFFFFFF)))))

;;; --- Shifts ---

(defun i386-emit-shl-reg-imm (buf reg count)
  "SHL reg, imm8"
  (if (= count 1)
      (progn (i386-emit-byte buf #xD1)
             (i386-emit-byte buf (i386-modrm #b11 4 reg)))
      (progn (i386-emit-byte buf #xC1)
             (i386-emit-byte buf (i386-modrm #b11 4 reg))
             (i386-emit-byte buf count))))

(defun i386-emit-shr-reg-imm (buf reg count)
  "SHR reg, imm8 (logical shift right)"
  (if (= count 1)
      (progn (i386-emit-byte buf #xD1)
             (i386-emit-byte buf (i386-modrm #b11 5 reg)))
      (progn (i386-emit-byte buf #xC1)
             (i386-emit-byte buf (i386-modrm #b11 5 reg))
             (i386-emit-byte buf count))))

(defun i386-emit-sar-reg-imm (buf reg count)
  "SAR reg, imm8 (arithmetic shift right)"
  (if (= count 1)
      (progn (i386-emit-byte buf #xD1)
             (i386-emit-byte buf (i386-modrm #b11 7 reg)))
      (progn (i386-emit-byte buf #xC1)
             (i386-emit-byte buf (i386-modrm #b11 7 reg))
             (i386-emit-byte buf count))))

(defun i386-emit-shl-reg-cl (buf reg)
  "SHL reg, CL — shift left by count in CL"
  (i386-emit-byte buf #xD3)
  (i386-emit-byte buf (i386-modrm #b11 4 reg)))

(defun i386-emit-sar-reg-cl (buf reg)
  "SAR reg, CL — arithmetic shift right by count in CL"
  (i386-emit-byte buf #xD3)
  (i386-emit-byte buf (i386-modrm #b11 7 reg)))

;;; --- Multiply / Divide ---

(defun i386-emit-imul-reg-reg (buf dst src)
  "IMUL dst, src (two-operand signed multiply)"
  (i386-emit-byte buf #x0F)
  (i386-emit-byte buf #xAF)
  (i386-emit-byte buf (i386-modrm #b11 dst src)))

(defun i386-emit-mul-reg (buf reg)
  "MUL reg: unsigned multiply EDX:EAX = EAX * reg"
  (i386-emit-byte buf #xF7)
  (i386-emit-byte buf (i386-modrm #b11 4 reg)))

(defun i386-emit-idiv-reg (buf reg)
  "IDIV reg: signed divide EDX:EAX by reg, quotient->EAX, remainder->EDX"
  (i386-emit-byte buf #xF7)
  (i386-emit-byte buf (i386-modrm #b11 7 reg)))

(defun i386-emit-cdq (buf)
  "CDQ: sign-extend EAX into EDX:EAX"
  (i386-emit-byte buf #x99))

;;; --- NEG / NOT ---

(defun i386-emit-neg-reg (buf reg)
  "NEG reg (two's complement negate)"
  (i386-emit-byte buf #xF7)
  (i386-emit-byte buf (i386-modrm #b11 3 reg)))

(defun i386-emit-not-reg (buf reg)
  "NOT reg (bitwise complement)"
  (i386-emit-byte buf #xF7)
  (i386-emit-byte buf (i386-modrm #b11 2 reg)))

;;; --- LEA ---

(defun i386-emit-lea (buf dst base offset)
  "LEA dst, [base + offset]"
  (i386-emit-byte buf #x8D)
  (i386-emit-modrm-mem buf dst base offset))

;;; --- Jump / Call / Return ---

(defun i386-emit-jmp-rel32 (buf &optional label-id)
  "JMP rel32 (near unconditional jump)"
  (i386-emit-byte buf #xE9)
  (if label-id
      (i386-emit-fixup-rel32 buf label-id)
      (i386-emit-u32 buf 0)))

(defun i386-emit-jmp-reg (buf reg)
  "JMP reg (indirect near jump)"
  (i386-emit-byte buf #xFF)
  (i386-emit-byte buf (i386-modrm #b11 4 reg)))

(defun i386-emit-call-rel32 (buf &optional label-id)
  "CALL rel32 (near call)"
  (i386-emit-byte buf #xE8)
  (if label-id
      (i386-emit-fixup-rel32 buf label-id)
      (i386-emit-u32 buf 0)))

(defun i386-emit-call-reg (buf reg)
  "CALL reg (indirect near call)"
  (i386-emit-byte buf #xFF)
  (i386-emit-byte buf (i386-modrm #b11 2 reg)))

(defun i386-emit-ret (buf)
  "RET (near return)"
  (i386-emit-byte buf #xC3))

;;; --- Conditional Jumps ---

(defparameter *i386-cc-codes*
  '((:e  . #x4)  (:ne . #x5)
    (:l  . #xC)  (:ge . #xD)  (:le . #xE)  (:g  . #xF)
    (:b  . #x2)  (:ae . #x3)  (:be . #x6)  (:a  . #x7)
    (:z  . #x4)  (:nz . #x5)  (:s  . #x8)  (:ns . #x9))
  "Condition code keyword -> numeric encoding for Jcc.")

(defun i386-emit-jcc (buf cc &optional label-id)
  "Jcc rel32 (conditional jump, near form: 0F 80+cc)"
  (let ((code (cdr (assoc cc *i386-cc-codes*))))
    (unless code (error "i386: unknown condition code ~A" cc))
    (i386-emit-byte buf #x0F)
    (i386-emit-byte buf (+ #x80 code))
    (if label-id
        (i386-emit-fixup-rel32 buf label-id)
        (i386-emit-u32 buf 0))))

;;; --- Atomic Exchange ---

(defun i386-emit-xchg-mem-reg (buf base offset reg)
  "XCHG [base+offset], reg.  Implicitly locked when memory operand present."
  (i386-emit-byte buf #x87)
  (i386-emit-modrm-mem buf reg base offset))

;;; --- Special Instructions ---

(defun i386-emit-nop (buf) "NOP" (i386-emit-byte buf #x90))
(defun i386-emit-int3 (buf) "INT 3 (breakpoint)" (i386-emit-byte buf #xCC))
(defun i386-emit-int (buf n) "INT n" (i386-emit-byte buf #xCD) (i386-emit-byte buf n))
(defun i386-emit-cli (buf) "CLI (disable interrupts)" (i386-emit-byte buf #xFA))
(defun i386-emit-sti (buf) "STI (enable interrupts)" (i386-emit-byte buf #xFB))
(defun i386-emit-hlt (buf) "HLT (halt processor)" (i386-emit-byte buf #xF4))

(defun i386-emit-mfence (buf)
  "Memory fence.  Uses LOCK ADD [ESP], 0 for i386 compatibility
   (MFENCE is SSE2/i686+; LOCK ADD works on all i386+)."
  (i386-emit-byte buf #xF0)   ; LOCK prefix
  (i386-emit-byte buf #x83)   ; ADD r/m32, imm8
  (i386-emit-modrm-mem buf 0 +i386-esp+ 0)
  (i386-emit-s8 buf 0))

;;; --- I/O Port Instructions ---

(defun i386-emit-in-al-dx (buf)      (i386-emit-byte buf #xEC))
(defun i386-emit-in-ax-dx (buf)      (i386-emit-byte buf #x66) (i386-emit-byte buf #xED))
(defun i386-emit-in-eax-dx (buf)     (i386-emit-byte buf #xED))
(defun i386-emit-out-dx-al (buf)     (i386-emit-byte buf #xEE))
(defun i386-emit-out-dx-ax (buf)     (i386-emit-byte buf #x66) (i386-emit-byte buf #xEF))
(defun i386-emit-out-dx-eax (buf)    (i386-emit-byte buf #xEF))

(defun i386-emit-in-al-imm8 (buf port)
  "IN AL, imm8" (i386-emit-byte buf #xE4) (i386-emit-byte buf port))
(defun i386-emit-in-ax-imm8 (buf port)
  "IN AX, imm8" (i386-emit-byte buf #x66) (i386-emit-byte buf #xE5) (i386-emit-byte buf port))
(defun i386-emit-in-eax-imm8 (buf port)
  "IN EAX, imm8" (i386-emit-byte buf #xE5) (i386-emit-byte buf port))
(defun i386-emit-out-imm8-al (buf port)
  "OUT imm8, AL" (i386-emit-byte buf #xE6) (i386-emit-byte buf port))
(defun i386-emit-out-imm8-eax (buf port)
  "OUT imm8, EAX" (i386-emit-byte buf #xE7) (i386-emit-byte buf port))

;;; ============================================================
;;; Virtual Register Load/Store Helpers
;;; ============================================================
;;;
;;; On i386, most virtual registers spill to the stack. These helpers
;;; abstract the load/store pattern, generating MOV reg-reg when the
;;; vreg has a physical mapping, or MOV reg-mem / MOV mem-reg for spills.

(defun i386-vreg-abs-addr (vreg)
  "Return absolute memory address for VA/VL/VN, or nil for other vregs."
  (cond
    ((= vreg +vreg-va+) +va-addr+)
    ((= vreg +vreg-vl+) +vl-addr+)
    ((= vreg +vreg-vn+) +vn-addr+)
    (t nil)))

(defun i386-load-vreg (buf scratch vreg)
  "Load virtual register VREG into physical register SCRATCH.
   If VREG already lives in SCRATCH, no code is emitted."
  (let ((phys (i386-vreg-phys vreg))
        (abs (i386-vreg-abs-addr vreg)))
    (cond
      (phys (unless (= phys scratch)
              (i386-emit-mov-reg-reg buf scratch phys)))
      (abs  (i386-emit-mov-reg-abs buf scratch abs))
      (t    (i386-emit-mov-reg-mem buf scratch +i386-ebp+ (i386-spill-offset vreg))))))

(defun i386-store-vreg (buf vreg scratch)
  "Store physical register SCRATCH into virtual register VREG."
  (let ((phys (i386-vreg-phys vreg))
        (abs (i386-vreg-abs-addr vreg)))
    (cond
      (phys (unless (= phys scratch)
              (i386-emit-mov-reg-reg buf phys scratch)))
      (abs  (i386-emit-mov-abs-reg buf abs scratch))
      (t    (i386-emit-mov-mem-reg buf +i386-ebp+ (i386-spill-offset vreg) scratch)))))

(defun i386-vreg-or-scratch (buf vreg scratch)
  "If VREG has a physical register, return it.
   Otherwise load into SCRATCH and return SCRATCH."
  (let ((phys (i386-vreg-phys vreg)))
    (if phys phys
        (progn (i386-load-vreg buf scratch vreg) scratch))))

;;; ============================================================
;;; Prologue / Epilogue
;;; ============================================================

(defun i386-emit-prologue (buf)
  "Emit i386 function prologue.
   PUSH EBP; MOV EBP, ESP; SUB ESP, frame_size;
   save callee-saved EBX, ESI, EDI;
   copy incoming V2/V3 args from [EBP+8/12] to local spill slots."
  (i386-emit-push-reg buf +i386-ebp+)
  (i386-emit-mov-reg-reg buf +i386-ebp+ +i386-esp+)
  (i386-emit-sub-reg-imm buf +i386-esp+ +frame-size+)
  ;; Save callee-saved registers
  (i386-emit-mov-mem-reg buf +i386-ebp+ +save-ebx-off+ +i386-ebx+)
  (i386-emit-mov-mem-reg buf +i386-ebp+ +save-esi-off+ +i386-esi+)
  (i386-emit-mov-mem-reg buf +i386-ebp+ +save-edi-off+ +i386-edi+)
  ;; Copy incoming V2/V3 from caller-pushed stack args to local spill slots.
  ;; CALL handler always pushes V2/V3 before every CALL instruction.
  ;; [EBP+8] = V2 incoming, [EBP+12] = V3 incoming.
  (i386-emit-mov-reg-mem buf +scratch0+ +i386-ebp+ 8)   ; load V2 from [EBP+8]
  (i386-emit-mov-mem-reg buf +i386-ebp+ -16 +scratch0+)  ; store to local V2 slot
  (i386-emit-mov-reg-mem buf +scratch0+ +i386-ebp+ 12)  ; load V3 from [EBP+12]
  (i386-emit-mov-mem-reg buf +i386-ebp+ -20 +scratch0+)) ; store to local V3 slot

(defun i386-emit-epilogue (buf)
  "Emit i386 function epilogue. Return value should be in EAX (VR)."
  ;; Restore callee-saved registers
  (i386-emit-mov-reg-mem buf +i386-ebx+ +i386-ebp+ +save-ebx-off+)
  (i386-emit-mov-reg-mem buf +i386-esi+ +i386-ebp+ +save-esi-off+)
  (i386-emit-mov-reg-mem buf +i386-edi+ +i386-ebp+ +save-edi-off+)
  ;; Tear down frame
  (i386-emit-mov-reg-reg buf +i386-esp+ +i386-ebp+)
  (i386-emit-pop-reg buf +i386-ebp+)
  (i386-emit-ret buf))

;;; ============================================================
;;; Translation State
;;; ============================================================

(defstruct i386-translate-state
  (buf nil)                     ; i386-buffer
  (mvm-bytes nil)               ; raw MVM bytecode vector
  (mvm-length 0)                ; length of bytecode region
  (mvm-offset 0)                ; start offset into mvm-bytes
  ;; Maps MVM bytecode position -> native code label
  (label-map (make-hash-table :test 'eql))
  ;; Function table: function-index -> native code label
  (function-table nil)
  ;; GC helper label
  (gc-label nil))

(defun i386-ensure-label-at (state mvm-pos)
  "Ensure a label exists for MVM bytecode position MVM-POS."
  (let ((ht (i386-translate-state-label-map state)))
    (or (gethash mvm-pos ht)
        (setf (gethash mvm-pos ht) (i386-make-label)))))

;;; ============================================================
;;; MVM Opcode Translation
;;; ============================================================
;;;
;;; Single-instruction translator.  Called from the main translation
;;; loop for each decoded MVM instruction.

(defun i386-translate-insn (state opcode operands mvm-next-pos)
  "Translate one MVM instruction to i386 native code.
   OPCODE: numeric MVM opcode.
   OPERANDS: list of decoded operands.
   MVM-NEXT-POS: bytecode position after this instruction."
  (let ((buf (i386-translate-state-buf state)))
    (macrolet ((op= (sym) `(= opcode ,sym)))
      (cond
        ;; ============================================
        ;; NOP / BREAK / TRAP
        ;; ============================================
        ((op= +op-nop+)
         (i386-emit-nop buf))

        ((op= +op-break+)
         (i386-emit-int3 buf))

        ((op= +op-trap+)
         (let ((code (first operands)))
           (cond
             ((< code #x0100)
              ;; Frame-enter: code = nparams.
              ;; If nparams > 4, copy excess args from caller's stack
              ;; to frame slot locations where the compiler expects them.
              ;; Caller pushes V2, V3 at [EBP+8/12], then overflow args at [EBP+16+k*4].
              ;; Compiler binds param N (N>=4) to stack-slot N, accessed via
              ;; obj-ref VFP N → [EBP + frame-slot-base + N * -4].
              (when (> code 4)
                (loop for param-idx from 4 below code
                      for k from 0  ;; k-th overflow arg
                      do (let ((src-off (+ 16 (* k 4)))
                               (dst-off (+ +frame-slot-base+ (* param-idx -4))))
                           (i386-emit-mov-reg-mem buf +scratch0+ +i386-ebp+ src-off)
                           (i386-emit-mov-mem-reg buf +i386-ebp+ dst-off +scratch0+)))))
             ((< code #x0300)
              ;; Frame-alloc (#x100+N) and frame-free (#x200+N): NOP
              nil)
             ((= code #x0300)
              ;; Serial write: V0 (ESI) contains tagged fixnum char code
              ;; Poll TX ready: wait for LSR bit 5 (THR empty)
              (let ((poll-label (i386-make-label)))
                ;; mov dx, 0x3FD (COM1 LSR)
                (i386-emit-byte buf #x66)
                (i386-emit-byte buf #xBA)
                (i386-emit-byte buf #xFD)
                (i386-emit-byte buf #x03)
                (i386-emit-label buf poll-label)
                ;; in al, dx
                (i386-emit-byte buf #xEC)
                ;; test al, 0x20 (bit 5 = THRE)
                (i386-emit-byte buf #xA8)
                (i386-emit-byte buf #x20)
                ;; jz poll-label
                (i386-emit-jcc buf :z poll-label))
              ;; mov eax, esi (V0)
              (i386-emit-byte buf #x89)
              (i386-emit-byte buf #xF0)
              ;; sar eax, 1 (untag fixnum)
              (i386-emit-byte buf #xD1)
              (i386-emit-byte buf #xF8)
              ;; mov dx, 0x3F8 (COM1 data port)
              (i386-emit-byte buf #x66)
              (i386-emit-byte buf #xBA)
              (i386-emit-byte buf #xF8)
              (i386-emit-byte buf #x03)
              ;; out dx, al
              (i386-emit-byte buf #xEE))
             ((= code #x0301)
              ;; Serial read: poll RX ready, return tagged char in V0 (ESI)
              (let ((poll-label (i386-make-label)))
                ;; mov dx, 0x3FD (COM1 LSR)
                (i386-emit-byte buf #x66)
                (i386-emit-byte buf #xBA)
                (i386-emit-byte buf #xFD)
                (i386-emit-byte buf #x03)
                (i386-emit-label buf poll-label)
                ;; in al, dx
                (i386-emit-byte buf #xEC)
                ;; test al, 0x01 (bit 0 = data ready)
                (i386-emit-byte buf #xA8)
                (i386-emit-byte buf #x01)
                ;; jz poll-label
                (i386-emit-jcc buf :z poll-label))
              ;; mov dx, 0x3F8 (COM1 data port)
              (i386-emit-byte buf #x66)
              (i386-emit-byte buf #xBA)
              (i386-emit-byte buf #xF8)
              (i386-emit-byte buf #x03)
              ;; in al, dx (read byte)
              (i386-emit-byte buf #xEC)
              ;; movzx eax, al (zero-extend)
              (i386-emit-byte buf #x0F)
              (i386-emit-byte buf #xB6)
              (i386-emit-byte buf #xC0)
              ;; shl eax, 1 (tag as fixnum)
              (i386-emit-byte buf #xD1)
              (i386-emit-byte buf #xE0)
              ;; mov esi, eax (result in V0)
              (i386-emit-byte buf #x89)
              (i386-emit-byte buf #xC6))
             ((= code #x0302)
              ;; Memory barrier: NOP on i386 (strong ordering)
              nil)
             ((= code #x0320)
              ;; SETUP-IRQ: PIC remap + PIT timer + IDT + ISR for HLT-based io-delay
              ;; Save regs we clobber
              (i386-emit-byte buf #x51)  ; push ecx
              (i386-emit-byte buf #x52)  ; push edx
              (i386-emit-byte buf #x57)  ; push edi
              ;; --- PIC remap ---
              (dolist (pv '((#x20 #x11) (#xA0 #x11)   ; ICW1
                           (#x21 #x20) (#xA1 #x28)   ; ICW2
                           (#x21 #x04) (#xA1 #x02)   ; ICW3
                           (#x21 #x01) (#xA1 #x01)   ; ICW4
                           (#x21 #xFC) (#xA1 #xFF))) ; masks (IRQ0+IRQ1 unmasked)
                (i386-emit-byte buf #xB0) (i386-emit-byte buf (second pv))   ; mov al, val
                (i386-emit-byte buf #x66) (i386-emit-byte buf #xBA)
                (i386-emit-byte buf (logand (first pv) #xFF))
                (i386-emit-byte buf (ash (first pv) -8))  ; mov dx, port
                (i386-emit-byte buf #xEE))               ; out dx, al
              ;; --- PIT channel 0: ~1000Hz (divisor 1193 = 0x04A9) ---
              (dolist (pv '((#x43 #x34) (#x40 #xA9) (#x40 #x04)))
                (i386-emit-byte buf #xB0) (i386-emit-byte buf (second pv))
                (i386-emit-byte buf #x66) (i386-emit-byte buf #xBA)
                (i386-emit-byte buf (logand (first pv) #xFF))
                (i386-emit-byte buf (ash (first pv) -8))
                (i386-emit-byte buf #xEE))
              ;; --- Zero IDT area (384 bytes at 0x90000) ---
              (i386-emit-byte buf #xBF) (i386-emit-u32 buf #x90000)  ; mov edi
              (i386-emit-byte buf #x31) (i386-emit-byte buf #xC0)     ; xor eax, eax
              (i386-emit-byte buf #xB9) (i386-emit-u32 buf 96)        ; mov ecx, 96
              (i386-emit-byte buf #xF3) (i386-emit-byte buf #xAB)     ; rep stosd
              ;; --- Write IDT entry 0x20 at 0x90100 (8 bytes) ---
              ;; ISR at 0x90400
              (i386-emit-byte buf #xBF) (i386-emit-u32 buf #x90100)
              (i386-emit-byte buf #xC7) (i386-emit-byte buf #x07)
              (i386-emit-u32 buf #x00080400)  ; selector<<16|offset_lo
              (i386-emit-byte buf #xC7) (i386-emit-byte buf #x47) (i386-emit-byte buf #x04)
              (i386-emit-u32 buf #x00098E00)  ; offset_hi<<16|type
              ;; --- Write ISR at 0x90400 (7 bytes) ---
              ;; push eax; mov al,0x20; out 0x20,al; pop eax; iret
              (i386-emit-byte buf #xBF) (i386-emit-u32 buf #x90400)
              (i386-emit-byte buf #xC7) (i386-emit-byte buf #x07)
              (i386-emit-u32 buf #xE620B050)  ; 50 B0 20 E6
              (i386-emit-byte buf #x66) (i386-emit-byte buf #xC7)
              (i386-emit-byte buf #x47) (i386-emit-byte buf #x04)
              (i386-emit-byte buf #x20) (i386-emit-byte buf #x58)  ; 20 58
              (i386-emit-byte buf #xC6) (i386-emit-byte buf #x47)
              (i386-emit-byte buf #x06) (i386-emit-byte buf #xCF)  ; CF (iret)
              ;; --- Write keyboard ISR at 0x90410 (33 bytes) ---
              ;; Stores raw scancode into ring buffer at 0x600040.
              ;; Ring write index at 0x600030, 64-byte circular buffer.
              ;; push eax; push ebx; in al,0x60; mov ebx,[0x600030];
              ;; mov [ebx+0x600040],al; inc ebx; and ebx,0x3F;
              ;; mov [0x600030],ebx; mov al,0x20; out 0x20,al;
              ;; pop ebx; pop eax; iret
              (i386-emit-byte buf #xBF) (i386-emit-u32 buf #x90410)  ; mov edi, 0x90410
              ;; Write ISR bytes as dwords via stosd
              (i386-emit-byte buf #xFC)  ; cld
              ;; Bytes: 50 53 E4 60
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf #x60E45350)
              (i386-emit-byte buf #xAB)
              ;; Bytes: 8B 1D 30 00
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf #x00301D8B)
              (i386-emit-byte buf #xAB)
              ;; Bytes: 60 00 88 83
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf #x83880060)
              (i386-emit-byte buf #xAB)
              ;; Bytes: 40 00 60 00
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf #x00600040)
              (i386-emit-byte buf #xAB)
              ;; Bytes: 43 83 E3 3F
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf #x3FE38343)
              (i386-emit-byte buf #xAB)
              ;; Bytes: 89 1D 30 00
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf #x00301D89)
              (i386-emit-byte buf #xAB)
              ;; Bytes: 60 00 B0 20
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf #x20B00060)
              (i386-emit-byte buf #xAB)
              ;; Bytes: E6 20 5B 58
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf #x585B20E6)
              (i386-emit-byte buf #xAB)
              ;; Byte: CF (iret) — write as single byte
              (i386-emit-byte buf #xC6) (i386-emit-byte buf #x07) (i386-emit-byte buf #xCF)
              ;; --- Write IDT entry 0x21 at 0x90108 (keyboard IRQ1) ---
              ;; ISR at 0x90410, selector 0x0008, type 0x8E (32-bit interrupt gate)
              (i386-emit-byte buf #xBF) (i386-emit-u32 buf #x90108)
              (i386-emit-byte buf #xC7) (i386-emit-byte buf #x07)
              (i386-emit-u32 buf #x00080410)  ; selector<<16 | offset_lo
              (i386-emit-byte buf #xC7) (i386-emit-byte buf #x47) (i386-emit-byte buf #x04)
              (i386-emit-u32 buf #x00098E00)  ; offset_hi<<16 | type
              ;; --- Zero keyboard ring buffer state ---
              (i386-emit-byte buf #xC7) (i386-emit-byte buf #x05)
              (i386-emit-u32 buf #x600030) (i386-emit-u32 buf 0)  ; write idx = 0
              (i386-emit-byte buf #xC7) (i386-emit-byte buf #x05)
              (i386-emit-u32 buf #x600034) (i386-emit-u32 buf 0)  ; read idx = 0
              ;; --- Zero NIC interrupt state ---
              (i386-emit-byte buf #xC7) (i386-emit-byte buf #x05)
              (i386-emit-u32 buf #x600020) (i386-emit-u32 buf 0)  ; nic_pkt_pending = 0
              (i386-emit-byte buf #xC7) (i386-emit-byte buf #x05)
              (i386-emit-u32 buf #x600024) (i386-emit-u32 buf 0)  ; nic_irq = 0
              ;; --- Write NIC ISR (master PIC, IRQ 0-7) at 0x90450 (20 bytes) ---
              ;; push eax; mov byte [0x600020],1; in al,0x21; or al,XX;
              ;; out 0x21,al; mov al,0x20; out 0x20,al; pop eax; iret
              ;; The OR byte (offset 11) is patched at runtime by TRAP #x0322.
              (i386-emit-byte buf #xBF) (i386-emit-u32 buf #x90450)
              (i386-emit-byte buf #xFC)
              ;; 50 C6 05 20 : push eax; mov byte [0x600020],...
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf #x05C62050)
              (i386-emit-byte buf #xAB)
              ;; 00 60 00 01 : ...addr+value
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf #x01006000)
              (i386-emit-byte buf #xAB)
              ;; E4 21 0C 00 : in al,0x21; or al,0x00(patch)
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf #x000C21E4)
              (i386-emit-byte buf #xAB)
              ;; E6 21 B0 20 : out 0x21,al; mov al,0x20
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf #x20B021E6)
              (i386-emit-byte buf #xAB)
              ;; E6 20 58 CF : out 0x20,al; pop eax; iret
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf #xCF5820E6)
              (i386-emit-byte buf #xAB)
              ;; --- Write NIC ISR (slave PIC, IRQ 8-15) at 0x90470 (22 bytes) ---
              ;; push eax; mov byte [0x600020],1; in al,0xA1; or al,XX;
              ;; out 0xA1,al; mov al,0x20; out 0xA0,al; out 0x20,al; pop eax; iret
              (i386-emit-byte buf #xBF) (i386-emit-u32 buf #x90470)
              ;; 50 C6 05 20 : push eax; mov byte [0x600020],...
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf #x05C62050)
              (i386-emit-byte buf #xAB)
              ;; 00 60 00 01 : ...addr+value
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf #x01006000)
              (i386-emit-byte buf #xAB)
              ;; E4 A1 0C 00 : in al,0xA1; or al,0x00(patch)
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf #x000CA1E4)
              (i386-emit-byte buf #xAB)
              ;; E6 A1 B0 20 : out 0xA1,al; mov al,0x20
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf #x20B0A1E6)
              (i386-emit-byte buf #xAB)
              ;; E6 A0 E6 20 : out 0xA0,al; out 0x20,al
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf #x20E6A0E6)
              (i386-emit-byte buf #xAB)
              ;; 58 CF : pop eax; iret (+ 2 pad bytes)
              (i386-emit-byte buf #x66) (i386-emit-byte buf #xC7)
              (i386-emit-byte buf #x47) (i386-emit-byte buf #x14)
              (i386-emit-byte buf #x58) (i386-emit-byte buf #xCF)
              ;; --- LIDT ---
              (i386-emit-byte buf #x83) (i386-emit-byte buf #xEC) (i386-emit-byte buf #x08)  ; sub esp, 8
              (i386-emit-byte buf #x66) (i386-emit-byte buf #xC7)
              (i386-emit-byte buf #x04) (i386-emit-byte buf #x24)
              (i386-emit-byte buf #x7F) (i386-emit-byte buf #x01)  ; mov word [esp], 383
              (i386-emit-byte buf #xC7) (i386-emit-byte buf #x44)
              (i386-emit-byte buf #x24) (i386-emit-byte buf #x02)
              (i386-emit-u32 buf #x90000)  ; mov dword [esp+2], base
              (i386-emit-byte buf #x0F) (i386-emit-byte buf #x01)
              (i386-emit-byte buf #x1C) (i386-emit-byte buf #x24)  ; lidt [esp]
              (i386-emit-byte buf #x83) (i386-emit-byte buf #xC4) (i386-emit-byte buf #x08)  ; add esp, 8
              (i386-emit-byte buf #x5F)  ; pop edi
              (i386-emit-byte buf #x5A)  ; pop edx
              (i386-emit-byte buf #x59)) ; pop ecx
             ((= code #x0321)
              ;; TIMER-REARM: NOP on i386 (only meaningful on AArch64 virt)
              nil)
             ((= code #x0322)
              ;; SETUP-NIC-IDT: Install NIC IDT entry and unmask NIC IRQ.
              ;; IRQ number must be at [0x600024]. Handles both master/slave PIC.
              ;; Patches OR byte in ISR, writes IDT entry, unmasks PIC.
              (i386-emit-byte buf #x50)  ; push eax
              (i386-emit-byte buf #x53)  ; push ebx
              (i386-emit-byte buf #x51)  ; push ecx
              (i386-emit-byte buf #x52)  ; push edx
              ;; eax = IRQ number
              (i386-emit-byte buf #xA1) (i386-emit-u32 buf #x600024)
              ;; ebx = IDT entry addr = 0x90000 + (IRQ + 0x20) * 8
              (i386-emit-byte buf #x8D) (i386-emit-byte buf #x58) (i386-emit-byte buf #x20) ; lea ebx,[eax+0x20]
              (i386-emit-byte buf #xC1) (i386-emit-byte buf #xE3) (i386-emit-byte buf #x03) ; shl ebx,3
              (i386-emit-byte buf #x81) (i386-emit-byte buf #xC3) (i386-emit-u32 buf #x90000) ; add ebx,0x90000
              ;; edx = 1 << (IRQ & 7)
              (i386-emit-byte buf #x89) (i386-emit-byte buf #xC1) ; mov ecx, eax
              (i386-emit-byte buf #x83) (i386-emit-byte buf #xE1) (i386-emit-byte buf #x07) ; and ecx, 7
              (i386-emit-byte buf #xBA) (i386-emit-u32 buf 1)     ; mov edx, 1
              (i386-emit-byte buf #xD3) (i386-emit-byte buf #xE2) ; shl edx, cl
              ;; Branch: IRQ < 8 → master (0x90450), else slave (0x90470)
              (i386-emit-byte buf #x3C) (i386-emit-byte buf #x08) ; cmp al, 8
              (i386-emit-byte buf #x72) (i386-emit-byte buf 35)   ; jb master (skip 35 bytes)
              ;; --- Slave PIC path ---
              ;; Write IDT entry: ISR at 0x90470
              (i386-emit-byte buf #xC7) (i386-emit-byte buf #x03)
              (i386-emit-u32 buf #x00080470)  ; [ebx] = selector:16|offset_lo:16
              (i386-emit-byte buf #xC7) (i386-emit-byte buf #x43) (i386-emit-byte buf #x04)
              (i386-emit-u32 buf #x00098E00)  ; [ebx+4] = offset_hi:16|type:16
              ;; Patch OR byte in slave ISR (0x90470 + 11 = 0x9047B)
              (i386-emit-byte buf #x88) (i386-emit-byte buf #x15) (i386-emit-u32 buf #x9047B)
              ;; Unmask slave PIC: in al,0xA1; not dl; and al,dl; out 0xA1,al
              (i386-emit-byte buf #xE4) (i386-emit-byte buf #xA1)
              (i386-emit-byte buf #xF6) (i386-emit-byte buf #xD2) ; not dl
              (i386-emit-byte buf #x20) (i386-emit-byte buf #xD0) ; and al, dl
              (i386-emit-byte buf #xE6) (i386-emit-byte buf #xA1)
              ;; Unmask cascade IRQ2 on master: in al,0x21; and al,~4; out 0x21,al
              (i386-emit-byte buf #xE4) (i386-emit-byte buf #x21)
              (i386-emit-byte buf #x24) (i386-emit-byte buf #xFB) ; and al, 0xFB
              (i386-emit-byte buf #xE6) (i386-emit-byte buf #x21)
              (i386-emit-byte buf #xEB) (i386-emit-byte buf 27)   ; jmp done (skip master)
              ;; --- Master PIC path ---
              ;; Write IDT entry: ISR at 0x90450
              (i386-emit-byte buf #xC7) (i386-emit-byte buf #x03)
              (i386-emit-u32 buf #x00080450)
              (i386-emit-byte buf #xC7) (i386-emit-byte buf #x43) (i386-emit-byte buf #x04)
              (i386-emit-u32 buf #x00098E00)
              ;; Patch OR byte in master ISR (0x90450 + 11 = 0x9045B)
              (i386-emit-byte buf #x88) (i386-emit-byte buf #x15) (i386-emit-u32 buf #x9045B)
              ;; Unmask master PIC
              (i386-emit-byte buf #xE4) (i386-emit-byte buf #x21)
              (i386-emit-byte buf #xF6) (i386-emit-byte buf #xD2)
              (i386-emit-byte buf #x20) (i386-emit-byte buf #xD0)
              (i386-emit-byte buf #xE6) (i386-emit-byte buf #x21)
              ;; done:
              (i386-emit-byte buf #x5A)  ; pop edx
              (i386-emit-byte buf #x59)  ; pop ecx
              (i386-emit-byte buf #x5B)  ; pop ebx
              (i386-emit-byte buf #x58)) ; pop eax
             ((= code #x0323)
              ;; NIC-IRQ-UNMASK: Re-enable NIC IRQ in PIC after servicing.
              ;; Reads IRQ from [0x600024].
              (i386-emit-byte buf #x50)  ; push eax
              (i386-emit-byte buf #x51)  ; push ecx
              (i386-emit-byte buf #x52)  ; push edx
              ;; edx = ~(1 << (IRQ & 7))
              (i386-emit-byte buf #xA1) (i386-emit-u32 buf #x600024)
              (i386-emit-byte buf #x89) (i386-emit-byte buf #xC1) ; mov ecx, eax
              (i386-emit-byte buf #x83) (i386-emit-byte buf #xE1) (i386-emit-byte buf #x07)
              (i386-emit-byte buf #xBA) (i386-emit-u32 buf 1)
              (i386-emit-byte buf #xD3) (i386-emit-byte buf #xE2) ; shl edx, cl
              (i386-emit-byte buf #xF6) (i386-emit-byte buf #xD2) ; not dl
              ;; Branch: IRQ < 8 → master, else slave
              (i386-emit-byte buf #x3C) (i386-emit-byte buf #x08)
              (i386-emit-byte buf #x72) (i386-emit-byte buf 6)    ; jb master
              ;; Slave: in al,0xA1; and al,dl; out 0xA1,al
              (i386-emit-byte buf #xE4) (i386-emit-byte buf #xA1)
              (i386-emit-byte buf #x20) (i386-emit-byte buf #xD0)
              (i386-emit-byte buf #xE6) (i386-emit-byte buf #xA1)
              (i386-emit-byte buf #xEB) (i386-emit-byte buf 6)    ; jmp done
              ;; Master: in al,0x21; and al,dl; out 0x21,al
              (i386-emit-byte buf #xE4) (i386-emit-byte buf #x21)
              (i386-emit-byte buf #x20) (i386-emit-byte buf #xD0)
              (i386-emit-byte buf #xE6) (i386-emit-byte buf #x21)
              ;; done:
              (i386-emit-byte buf #x5A)
              (i386-emit-byte buf #x59)
              (i386-emit-byte buf #x58))
             ((= code #x0330)
              ;; MMIO-DO-READ32: read 32-bit value from raw address at [0x600140]
              ;; Result stored at [0x600148]. Bypasses fixnum tagging entirely.
              ;; push eax
              (i386-emit-byte buf #x50)
              ;; mov eax, [0x600140]  — load raw 32-bit address
              (i386-emit-byte buf #xA1)
              (i386-emit-u32 buf #x600140)
              ;; mov eax, [eax]  — read 32-bit value from that address
              (i386-emit-byte buf #x8B)
              (i386-emit-byte buf #x00)
              ;; mov [0x600148], eax  — store raw result
              (i386-emit-byte buf #xA3)
              (i386-emit-u32 buf #x600148)
              ;; pop eax
              (i386-emit-byte buf #x58))
             ((= code #x0331)
              ;; MMIO-DO-WRITE32: write 32-bit value from [0x600148] to address [0x600140]
              ;; push eax; push ecx
              (i386-emit-byte buf #x50)
              (i386-emit-byte buf #x51)
              ;; mov eax, [0x600140]  — load raw 32-bit address
              (i386-emit-byte buf #xA1)
              (i386-emit-u32 buf #x600140)
              ;; mov ecx, [0x600148]  — load raw 32-bit value
              (i386-emit-byte buf #x8B)
              (i386-emit-byte buf #x0D)
              (i386-emit-u32 buf #x600148)
              ;; mov [eax], ecx  — write value to address
              (i386-emit-byte buf #x89)
              (i386-emit-byte buf #x08)
              ;; pop ecx; pop eax
              (i386-emit-byte buf #x59)
              (i386-emit-byte buf #x58))
             ((= code #x0332)
              ;; IO-IN-DWORD-RAW: read 32-bit I/O port, store raw result at 0x600148
              ;; Port number from low 16 bits of [0x600140]
              ;; push eax; push edx
              (i386-emit-byte buf #x50)
              (i386-emit-byte buf #x52)
              ;; mov dx, [0x600140]  — load 16-bit port number
              (i386-emit-byte buf #x66)       ; operand size prefix
              (i386-emit-byte buf #x8B)       ; mov r16, [disp32]
              (i386-emit-byte buf #x15)       ; mod=00 reg=DX r/m=101 (disp32)
              (i386-emit-u32 buf #x600140)
              ;; in eax, dx  — read 32-bit value from port
              (i386-emit-byte buf #xED)
              ;; mov [0x600148], eax  — store raw result
              (i386-emit-byte buf #xA3)
              (i386-emit-u32 buf #x600148)
              ;; pop edx; pop eax
              (i386-emit-byte buf #x5A)
              (i386-emit-byte buf #x58))
             ((= code #x0333)
              ;; PCI-CONFIG-READ-RAW: full PCI config read cycle in native code
              ;; V0 (ESI) = tagged PCI address (without enable bit)
              ;; Result: raw 32-bit value stored at [0x600148], byte 0 tagged in V0/VR
              ;; push edx
              (i386-emit-byte buf #x52)
              ;; mov eax, esi (V0 = tagged addr)
              (i386-emit-byte buf #x89)
              (i386-emit-byte buf #xF0)
              ;; shr eax, 1 (untag fixnum)
              (i386-emit-byte buf #xD1)
              (i386-emit-byte buf #xE8)
              ;; or eax, 0x80000000 (set enable bit — can't do in Lisp!)
              (i386-emit-byte buf #x0D)       ; or eax, imm32
              (i386-emit-u32 buf #x80000000)
              ;; mov dx, 0x0CF8
              (i386-emit-byte buf #x66)
              (i386-emit-byte buf #xBA)
              (i386-emit-byte buf #xF8)
              (i386-emit-byte buf #x0C)
              ;; out dx, eax (write PCI config address)
              (i386-emit-byte buf #xEF)
              ;; mov dx, 0x0CFC
              (i386-emit-byte buf #x66)
              (i386-emit-byte buf #xBA)
              (i386-emit-byte buf #xFC)
              (i386-emit-byte buf #x0C)
              ;; in eax, dx (read PCI config data)
              (i386-emit-byte buf #xED)
              ;; mov [0x600148], eax (store raw 32-bit result)
              (i386-emit-byte buf #xA3)
              (i386-emit-u32 buf #x600148)
              ;; Return byte 0 tagged as fixnum in EAX
              (i386-emit-byte buf #x0F)       ; movzx eax, al
              (i386-emit-byte buf #xB6)
              (i386-emit-byte buf #xC0)
              (i386-emit-byte buf #xD1)       ; shl eax, 1 (tag)
              (i386-emit-byte buf #xE0)
              ;; pop edx
              (i386-emit-byte buf #x5A)
              ;; mov esi, eax (result in V0)
              (i386-emit-byte buf #x89)
              (i386-emit-byte buf #xC6))
             ((= code #x0334)
              ;; WBINVD: flush all CPU caches so DMA-visible memory is coherent.
              ;; Required on real hardware where NIC reads descriptors via DMA.
              ;; 0F 09 = WBINVD instruction
              (i386-emit-byte buf #x0F)
              (i386-emit-byte buf #x09))
             (t
              ;; Unknown trap: NOP (avoid crash via INT with no IDT)
              nil))))

        ;; ============================================
        ;; Data Movement
        ;; ============================================
        ((op= +op-mov+)
         ;; (mov Vd Vs) -- route through load/store helpers for VA/VL/VN safety
         (let ((vd (first operands))
               (vs (second operands)))
           (i386-load-vreg buf +scratch0+ vs)
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-li+)
         ;; (li Vd imm64) -- on i386, truncate to low 32 bits
         (let* ((vd (first operands))
                (imm (logand (second operands) #xFFFFFFFF))
                (pd (i386-vreg-phys vd))
                (abs (i386-vreg-abs-addr vd)))
           (cond
             (pd  (i386-emit-mov-reg-imm buf pd imm))
             (abs (i386-emit-mov-reg-imm buf +scratch0+ imm)
                  (i386-emit-mov-abs-reg buf abs +scratch0+))
             (t   (i386-emit-mov-mem-imm buf +i386-ebp+ (i386-spill-offset vd) imm)))))

        ((op= +op-push+)
         ;; (push Vs)
         (let* ((vs (first operands))
                (ps (i386-vreg-phys vs))
                (abs (i386-vreg-abs-addr vs)))
           (cond
             (ps  (i386-emit-push-reg buf ps))
             (abs (i386-emit-push-abs buf abs))
             (t   (i386-emit-push-mem buf +i386-ebp+ (i386-spill-offset vs))))))

        ((op= +op-pop+)
         ;; (pop Vd)
         (let* ((vd (first operands))
                (pd (i386-vreg-phys vd)))
           (if pd
               (i386-emit-pop-reg buf pd)
               (progn
                 (i386-emit-pop-reg buf +scratch0+)
                 (i386-store-vreg buf vd +scratch0+)))))

        ;; ============================================
        ;; Arithmetic (tagged fixnum: value << 1, LSB=0)
        ;; ============================================
        ((op= +op-add+)
         ;; (add Vd Va Vb) -- tagged fixnums add directly (tags cancel)
         ;; IMPORTANT: Load vb FIRST — loading va into EAX clobbers VR.
         (let ((vd (first operands))
               (va (second operands))
               (vb (third operands)))
           (i386-load-vreg buf +scratch0+ vb)
           (i386-load-vreg buf +i386-eax+ va)
           (i386-emit-add-reg-reg buf +i386-eax+ +scratch0+)
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-sub+)
         ;; (sub Vd Va Vb) -- tagged fixnums subtract directly
         ;; IMPORTANT: Load vb FIRST — loading va into EAX clobbers VR.
         (let ((vd (first operands))
               (va (second operands))
               (vb (third operands)))
           (i386-load-vreg buf +scratch0+ vb)
           (i386-load-vreg buf +i386-eax+ va)
           (i386-emit-sub-reg-reg buf +i386-eax+ +scratch0+)
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-mul+)
         ;; (mul Vd Va Vb) -- tagged multiply
         ;; Both inputs carry <<1 fixnum tag, so product has factor of 4
         ;; where we need 2.  SAR 1 corrects: (a<<1)*(b<<1) >> 1 = a*b<<1
         ;; IMPORTANT: Load vb into scratch0 FIRST — loading va into EAX
         ;; clobbers VR, so if vb=VR we'd get va's value.
         (let ((vd (first operands))
               (va (second operands))
               (vb (third operands)))
           (i386-load-vreg buf +scratch0+ vb)
           (i386-load-vreg buf +i386-eax+ va)
           (i386-emit-sar-reg-imm buf +i386-eax+ 1)   ; untag one operand
           (i386-emit-imul-reg-reg buf +i386-eax+ +scratch0+)
           ;; Result is already correctly tagged (a * (b<<1) = (a*b)<<1)
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-mul26lo+)
         ;; (mul26lo Vd Va Vb) — low 26 bits of untag(Va)*untag(Vb), tagged
         ;; Load vb into ECX, va into EAX, untag both, MUL → EDX:EAX
         ;; AND EAX, 0x3FFFFFF ; SHL EAX, 1 (retag)
         (let ((vd (first operands))
               (va (second operands))
               (vb (third operands)))
           (i386-load-vreg buf +scratch0+ vb)            ; ECX = vb
           (i386-load-vreg buf +i386-eax+ va)            ; EAX = va
           (i386-emit-sar-reg-imm buf +scratch0+ 1)      ; untag vb
           (i386-emit-sar-reg-imm buf +i386-eax+ 1)      ; untag va
           (i386-emit-mul-reg buf +scratch0+)             ; EDX:EAX = EAX * ECX
           (i386-emit-and-reg-imm buf +i386-eax+ #x3FFFFFF) ; mask to 26 bits
           (i386-emit-shl-reg-imm buf +i386-eax+ 1)      ; retag
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-mul26hi+)
         ;; (mul26hi Vd Va Vb) — bits 26+ of untag(Va)*untag(Vb), tagged
         ;; Load vb into ECX, va into EAX, untag both, MUL → EDX:EAX
         ;; SHRD EAX, EDX, 26 — or just: SHR EAX,26 + SHL EDX,6 + OR
         ;; Use: SHR EAX, 26 ; SHL EDX, 6 ; OR EAX, EDX ; SHL EAX, 1 (retag)
         (let ((vd (first operands))
               (va (second operands))
               (vb (third operands)))
           (i386-load-vreg buf +scratch0+ vb)            ; ECX = vb
           (i386-load-vreg buf +i386-eax+ va)            ; EAX = va
           (i386-emit-sar-reg-imm buf +scratch0+ 1)      ; untag vb
           (i386-emit-sar-reg-imm buf +i386-eax+ 1)      ; untag va
           (i386-emit-mul-reg buf +scratch0+)             ; EDX:EAX = EAX * ECX
           (i386-emit-shr-reg-imm buf +i386-eax+ 26)     ; EAX = low>>26 (top 6 bits)
           (i386-emit-shl-reg-imm buf +i386-edx+ 6)      ; EDX = hi<<6
           (i386-emit-or-reg-reg buf +i386-eax+ +i386-edx+) ; merge
           (i386-emit-shl-reg-imm buf +i386-eax+ 1)      ; retag
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-div+)
         ;; (div Vd Va Vb) -- tagged fixnum division
         ;; Untag both, IDIV, re-tag quotient
         ;; IMPORTANT: Load vb FIRST — loading va into EAX clobbers VR.
         (let ((vd (first operands))
               (va (second operands))
               (vb (third operands)))
           (i386-load-vreg buf +scratch0+ vb)
           (i386-emit-sar-reg-imm buf +scratch0+ 1)    ; untag divisor
           (i386-load-vreg buf +i386-eax+ va)
           (i386-emit-sar-reg-imm buf +i386-eax+ 1)   ; untag dividend
           (i386-emit-cdq buf)                          ; sign-extend EAX -> EDX:EAX
           (i386-emit-idiv-reg buf +scratch0+)          ; EAX = quotient, EDX = remainder
           (i386-emit-shl-reg-imm buf +i386-eax+ 1)    ; re-tag quotient
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-mod+)
         ;; (mod Vd Va Vb) -- tagged fixnum modulus
         ;; Same as div but result is remainder in EDX
         ;; IMPORTANT: Load vb FIRST — loading va into EAX clobbers VR.
         (let ((vd (first operands))
               (va (second operands))
               (vb (third operands)))
           (i386-load-vreg buf +scratch0+ vb)
           (i386-emit-sar-reg-imm buf +scratch0+ 1)
           (i386-load-vreg buf +i386-eax+ va)
           (i386-emit-sar-reg-imm buf +i386-eax+ 1)
           (i386-emit-cdq buf)
           (i386-emit-idiv-reg buf +scratch0+)
           ;; Remainder in EDX, re-tag
           (i386-emit-shl-reg-imm buf +i386-edx+ 1)
           (i386-store-vreg buf vd +i386-edx+)))

        ((op= +op-neg+)
         ;; (neg Vd Vs) -- negate tagged fixnum
         ;; NEG preserves the fixnum tag: -(n<<1) = (-n)<<1
         (let ((vd (first operands))
               (vs (second operands)))
           (i386-load-vreg buf +i386-eax+ vs)
           (i386-emit-neg-reg buf +i386-eax+)
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-inc+)
         ;; (inc Vd) -- add tagged fixnum 1 (raw value 2)
         (let* ((vd (first operands))
                (pd (i386-vreg-phys vd)))
           (if pd
               (i386-emit-add-reg-imm buf pd 2)
               (progn
                 (i386-load-vreg buf +i386-eax+ vd)
                 (i386-emit-add-reg-imm buf +i386-eax+ 2)
                 (i386-store-vreg buf vd +i386-eax+)))))

        ((op= +op-dec+)
         ;; (dec Vd) -- subtract tagged fixnum 1 (raw value 2)
         (let* ((vd (first operands))
                (pd (i386-vreg-phys vd)))
           (if pd
               (i386-emit-sub-reg-imm buf pd 2)
               (progn
                 (i386-load-vreg buf +i386-eax+ vd)
                 (i386-emit-sub-reg-imm buf +i386-eax+ 2)
                 (i386-store-vreg buf vd +i386-eax+)))))

        ;; ============================================
        ;; Bitwise Operations
        ;; ============================================
        ((op= +op-and+)
         ;; IMPORTANT: Load vb FIRST — loading va into EAX clobbers VR.
         (let ((vd (first operands)) (va (second operands)) (vb (third operands)))
           (i386-load-vreg buf +scratch0+ vb)
           (i386-load-vreg buf +i386-eax+ va)
           (i386-emit-and-reg-reg buf +i386-eax+ +scratch0+)
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-or+)
         (let ((vd (first operands)) (va (second operands)) (vb (third operands)))
           (i386-load-vreg buf +scratch0+ vb)
           (i386-load-vreg buf +i386-eax+ va)
           (i386-emit-or-reg-reg buf +i386-eax+ +scratch0+)
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-xor+)
         (let ((vd (first operands)) (va (second operands)) (vb (third operands)))
           (i386-load-vreg buf +scratch0+ vb)
           (i386-load-vreg buf +i386-eax+ va)
           (i386-emit-xor-reg-reg buf +i386-eax+ +scratch0+)
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-shl+)
         (let ((vd (first operands)) (vs (second operands)) (amt (third operands)))
           (i386-load-vreg buf +scratch0+ vs)
           (i386-emit-shl-reg-imm buf +scratch0+ amt)
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-shr+)
         (let ((vd (first operands)) (vs (second operands)) (amt (third operands)))
           (i386-load-vreg buf +scratch0+ vs)
           (i386-emit-shr-reg-imm buf +scratch0+ amt)
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-sar+)
         (let ((vd (first operands)) (vs (second operands)) (amt (third operands)))
           ;; Use scratch0 (ECX) instead of EAX to avoid clobbering VR.
           ;; This is critical when multiple SARs operate on different vregs
           ;; consecutively (e.g., mem-ref untag of address then value).
           (i386-load-vreg buf +scratch0+ vs)
           ;; SAR by amt: always arithmetic shift to preserve sign.
           ;; (SHR for unsigned untagging has its own opcode +op-shr+.)
           (i386-emit-sar-reg-imm buf +scratch0+ amt)
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-shlv+)
         ;; (shlv Vd Vs Vc) — shift left by register count
         ;; Load count into ECX (CL), source into EAX, SHL EAX,CL
         (let ((vd (first operands)) (vs (second operands)) (vc (third operands)))
           (i386-load-vreg buf +i386-ecx+ vc)
           (i386-load-vreg buf +i386-eax+ vs)
           (i386-emit-shl-reg-cl buf +i386-eax+)
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-sarv+)
         ;; (sarv Vd Vs Vc) — arithmetic shift right by register count
         (let ((vd (first operands)) (vs (second operands)) (vc (third operands)))
           (i386-load-vreg buf +i386-ecx+ vc)
           (i386-load-vreg buf +i386-eax+ vs)
           (i386-emit-sar-reg-cl buf +i386-eax+)
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-ldb+)
         ;; (ldb Vd Vs pos:imm8 size:imm8) -- bit field extract
         (let ((vd (first operands)) (vs (second operands))
               (pos (third operands)) (size (fourth operands)))
           (i386-load-vreg buf +i386-eax+ vs)
           (when (> pos 0)
             (i386-emit-shr-reg-imm buf +i386-eax+ pos))
           (i386-emit-and-reg-imm buf +i386-eax+ (logand (1- (ash 1 size)) #xFFFFFFFF))
           (i386-store-vreg buf vd +i386-eax+)))

        ;; ============================================
        ;; Comparison
        ;; ============================================
        ((op= +op-cmp+)
         ;; (cmp Va Vb) -- sets CPU flags
         ;; IMPORTANT: Load vb FIRST — loading va into EAX clobbers VR.
         (let ((va (first operands)) (vb (second operands)))
           (i386-load-vreg buf +scratch0+ vb)
           (i386-load-vreg buf +i386-eax+ va)
           (i386-emit-cmp-reg-reg buf +i386-eax+ +scratch0+)))

        ((op= +op-test+)
         ;; (test Va Vb) -- AND, sets flags, discards result
         (let ((va (first operands)) (vb (second operands)))
           (i386-load-vreg buf +scratch0+ vb)
           (i386-load-vreg buf +i386-eax+ va)
           (i386-emit-test-reg-reg buf +i386-eax+ +scratch0+)))

        ;; ============================================
        ;; Branches
        ;; ============================================
        ;; MVM branch offsets are 16-bit signed, relative to the end
        ;; of the branch instruction in MVM bytecode.

        ((op= +op-br+)
         (let* ((off (first operands))
                (target-pos (+ mvm-next-pos off))
                (label (i386-ensure-label-at state target-pos)))
           (i386-emit-jmp-rel32 buf label)))

        ((op= +op-beq+)
         (let* ((off (first operands))
                (target-pos (+ mvm-next-pos off))
                (label (i386-ensure-label-at state target-pos)))
           (i386-emit-jcc buf :e label)))

        ((op= +op-bne+)
         (let* ((off (first operands))
                (target-pos (+ mvm-next-pos off))
                (label (i386-ensure-label-at state target-pos)))
           (i386-emit-jcc buf :ne label)))

        ((op= +op-blt+)
         (let* ((off (first operands))
                (target-pos (+ mvm-next-pos off))
                (label (i386-ensure-label-at state target-pos)))
           (i386-emit-jcc buf :l label)))

        ((op= +op-bge+)
         (let* ((off (first operands))
                (target-pos (+ mvm-next-pos off))
                (label (i386-ensure-label-at state target-pos)))
           (i386-emit-jcc buf :ge label)))

        ((op= +op-ble+)
         (let* ((off (first operands))
                (target-pos (+ mvm-next-pos off))
                (label (i386-ensure-label-at state target-pos)))
           (i386-emit-jcc buf :le label)))

        ((op= +op-bgt+)
         (let* ((off (first operands))
                (target-pos (+ mvm-next-pos off))
                (label (i386-ensure-label-at state target-pos)))
           (i386-emit-jcc buf :g label)))

        ((op= +op-bnull+)
         ;; (bnull Vs off16) -- compare Vs against NIL (VN in frame slot)
         (let* ((vs (first operands))
                (off (second operands))
                (target-pos (+ mvm-next-pos off))
                (label (i386-ensure-label-at state target-pos)))
           (i386-load-vreg buf +i386-eax+ vs)
           (i386-emit-cmp-reg-abs buf +i386-eax+ +vn-addr+)
           (i386-emit-jcc buf :e label)))

        ((op= +op-bnnull+)
         ;; (bnnull Vs off16) -- branch if Vs is not NIL
         (let* ((vs (first operands))
                (off (second operands))
                (target-pos (+ mvm-next-pos off))
                (label (i386-ensure-label-at state target-pos)))
           (i386-load-vreg buf +i386-eax+ vs)
           (i386-emit-cmp-reg-abs buf +i386-eax+ +vn-addr+)
           (i386-emit-jcc buf :ne label)))

        ;; ============================================
        ;; List Operations (32-bit cons cells, 4-byte words)
        ;; ============================================
        ((op= +op-car+)
         ;; (car Vd Vs) -- load car from cons cell
         ;; Cons tag = 0x01, so untag: ptr - 1, car at [ptr-1+0] = [ptr-1]
         ;; With type check: verify low 4 bits == 0x01
         (let ((vd (first operands)) (vs (second operands)))
           (i386-load-vreg buf +i386-eax+ vs)
           ;; Type check
           (i386-emit-mov-reg-reg buf +scratch0+ +i386-eax+)
           (i386-emit-and-reg-imm buf +scratch0+ +tag-mask+)
           (i386-emit-cmp-reg-imm buf +scratch0+ +tag-cons+)
           (let ((ok-label (i386-make-label)))
             (i386-emit-jcc buf :e ok-label)
             (i386-emit-int3 buf)   ; trap on non-cons
             (i386-emit-label buf ok-label))
           ;; Strip tag, load car (word 0)
           (i386-emit-sub-reg-imm buf +i386-eax+ +tag-cons+)
           (i386-emit-mov-reg-mem buf +i386-eax+ +i386-eax+ 0)
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-cdr+)
         ;; (cdr Vd Vs) -- load cdr from cons cell
         ;; cdr at [ptr - tag + 4] = [ptr - 1 + 4] = [ptr + 3]
         (let ((vd (first operands)) (vs (second operands)))
           (i386-load-vreg buf +i386-eax+ vs)
           ;; Type check
           (i386-emit-mov-reg-reg buf +scratch0+ +i386-eax+)
           (i386-emit-and-reg-imm buf +scratch0+ +tag-mask+)
           (i386-emit-cmp-reg-imm buf +scratch0+ +tag-cons+)
           (let ((ok-label (i386-make-label)))
             (i386-emit-jcc buf :e ok-label)
             (i386-emit-int3 buf)
             (i386-emit-label buf ok-label))
           ;; Strip tag, load cdr (word 1 = offset 4 on 32-bit)
           (i386-emit-sub-reg-imm buf +i386-eax+ +tag-cons+)
           (i386-emit-mov-reg-mem buf +i386-eax+ +i386-eax+ 4)
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-cons+)
         ;; (cons Vd Va Vb) -- allocate cons cell via bump allocator
         ;; VA is at absolute addr. [VA+0]=car, [VA+4]=cdr
         ;; result = VA | cons_tag, advance VA by 16
         ;; 16-byte alignment required: 4-bit tag uses low nibble
         ;; IMPORTANT: Load cdr into scratch0 FIRST — loading car into EAX
         ;; clobbers VR, so if vb-arg=VR we'd get car's value for cdr.
         (let ((vd (first operands)) (va-arg (second operands)) (vb-arg (third operands)))
           ;; Load cdr value first (before EAX is touched)
           (i386-load-vreg buf +scratch0+ vb-arg)
           ;; Load alloc pointer into EDX
           (i386-emit-mov-reg-abs buf +i386-edx+ +va-addr+)
           ;; Store car
           (i386-load-vreg buf +i386-eax+ va-arg)
           (i386-emit-mov-mem-reg buf +i386-edx+ 0 +i386-eax+)
           ;; Store cdr (already in scratch0)
           (i386-emit-mov-mem-reg buf +i386-edx+ 4 +scratch0+)
           ;; Tag pointer as cons
           (i386-emit-mov-reg-reg buf +i386-eax+ +i386-edx+)
           (i386-emit-or-reg-imm buf +i386-eax+ +tag-cons+)
           ;; Bump alloc by 16 (cons = 8 bytes but need 16-byte alignment for tag)
           (i386-emit-add-reg-imm buf +i386-edx+ 16)
           (i386-emit-mov-abs-reg buf +va-addr+ +i386-edx+)
           ;; Store result
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-setcar+)
         ;; (setcar Vd Vs) -- [Vd - tag] = Vs
         ;; IMPORTANT: Load value FIRST — loading vd-reg into EAX clobbers VR,
         ;; so if vs=VR we'd get the cons pointer instead of the value.
         (let ((vd-reg (first operands)) (vs (second operands)))
           (i386-load-vreg buf +scratch0+ vs)
           (i386-load-vreg buf +i386-eax+ vd-reg)
           (i386-emit-sub-reg-imm buf +i386-eax+ +tag-cons+)
           (i386-emit-mov-mem-reg buf +i386-eax+ 0 +scratch0+)))

        ((op= +op-setcdr+)
         ;; (setcdr Vd Vs) -- [Vd - tag + 4] = Vs
         ;; IMPORTANT: Load value FIRST — same VR clobber issue as setcar.
         (let ((vd-reg (first operands)) (vs (second operands)))
           (i386-load-vreg buf +scratch0+ vs)
           (i386-load-vreg buf +i386-eax+ vd-reg)
           (i386-emit-sub-reg-imm buf +i386-eax+ +tag-cons+)
           (i386-emit-mov-mem-reg buf +i386-eax+ 4 +scratch0+)))

        ((op= +op-consp+)
         ;; (consp Vd Vs) -- test low 4 bits for cons tag
         (let ((vd (first operands)) (vs (second operands)))
           (i386-load-vreg buf +i386-eax+ vs)
           (i386-emit-and-reg-imm buf +i386-eax+ +tag-mask+)
           (i386-emit-cmp-reg-imm buf +i386-eax+ +tag-cons+)
           (let ((true-label (i386-make-label))
                 (done-label (i386-make-label)))
             (i386-emit-jcc buf :e true-label)
             ;; False: load NIL
             (i386-emit-mov-reg-abs buf +i386-eax+ +vn-addr+)
             (i386-emit-jmp-rel32 buf done-label)
             ;; True: load T
             (i386-emit-label buf true-label)
             (i386-emit-mov-reg-imm buf +i386-eax+ +i386-mvm-t+)
             (i386-emit-label buf done-label))
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-atom+)
         ;; (atom Vd Vs) -- opposite of consp
         (let ((vd (first operands)) (vs (second operands)))
           (i386-load-vreg buf +i386-eax+ vs)
           (i386-emit-and-reg-imm buf +i386-eax+ +tag-mask+)
           (i386-emit-cmp-reg-imm buf +i386-eax+ +tag-cons+)
           (let ((true-label (i386-make-label))
                 (done-label (i386-make-label)))
             (i386-emit-jcc buf :ne true-label)
             ;; Is cons -> return NIL
             (i386-emit-mov-reg-abs buf +i386-eax+ +vn-addr+)
             (i386-emit-jmp-rel32 buf done-label)
             ;; Not cons -> return T
             (i386-emit-label buf true-label)
             (i386-emit-mov-reg-imm buf +i386-eax+ +i386-mvm-t+)
             (i386-emit-label buf done-label))
           (i386-store-vreg buf vd +i386-eax+)))

        ;; ============================================
        ;; Object Operations (32-bit, 4-byte slots)
        ;; ============================================
        ((op= +op-alloc-obj+)
         ;; (alloc-obj Vd count:imm16 subtag:imm8)
         ;; Header word at [VA]: (count << 8) | subtag  (matches x64 format)
         ;; Result = VA | object_tag, advance VA by aligned size
         (let ((vd (first operands)) (count (second operands)) (subtag (third operands)))
           (i386-emit-mov-reg-abs buf +i386-edx+ +va-addr+)
           ;; Write header: (count << 8) | subtag
           (i386-emit-mov-mem-imm buf +i386-edx+ 0
                                  (logior (ash count 8) subtag))
           ;; Tag pointer
           (i386-emit-mov-reg-reg buf +i386-eax+ +i386-edx+)
           (i386-emit-or-reg-imm buf +i386-eax+ +tag-object+)
           ;; Bump alloc: header (4) + count*4, aligned to 16
           (let ((total (logand (+ (* (1+ count) 4) 15) (lognot 15))))
             (i386-emit-add-reg-imm buf +i386-edx+ total))
           (i386-emit-mov-abs-reg buf +va-addr+ +i386-edx+)
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-alloc-array+)
         ;; (alloc-array Vd Vcount) — dynamic array allocation
         ;; Vcount: UNTAGGED element count (compiler SAR'd it already)
         ;; Allocates (count+1)*4 bytes, aligned to 16 (header + elements)
         ;; Header = (count << 8) | 0x32 (array subtag)
         ;; Result = VA | 0x09 (object tag)
         (let ((vd (first operands)) (vcount (second operands)))
           ;; Load count into ECX
           (i386-load-vreg buf +i386-ecx+ vcount)
           ;; Save count on stack
           (i386-emit-push-reg buf +i386-ecx+)
           ;; Build header: (count << 8) | array-subtag
           (i386-emit-shl-reg-imm buf +i386-ecx+ 8)
           (i386-emit-or-reg-imm buf +i386-ecx+ #x32)
           ;; Load VA (alloc pointer) into EDX
           (i386-emit-mov-reg-abs buf +i386-edx+ +va-addr+)
           ;; Write header at [EDX]
           (i386-emit-mov-mem-reg buf +i386-edx+ 0 +i386-ecx+)
           ;; Result = EDX | object_tag -> EAX
           (i386-emit-mov-reg-reg buf +i386-eax+ +i386-edx+)
           (i386-emit-or-reg-imm buf +i386-eax+ +tag-object+)
           ;; Restore count into ECX
           (i386-emit-pop-reg buf +i386-ecx+)
           ;; Compute alloc size: (count + 1) * 4, aligned to 16
           ;; ECX = count; add 1 for header, mul by 4, align
           (i386-emit-add-reg-imm buf +i386-ecx+ 1)
           (i386-emit-shl-reg-imm buf +i386-ecx+ 2)  ; * 4
           (i386-emit-add-reg-imm buf +i386-ecx+ 15)  ; for alignment
           (i386-emit-and-reg-imm buf +i386-ecx+ -16)  ; align to 16
           ;; Advance alloc pointer: EDX += ECX
           (i386-emit-add-reg-reg buf +i386-edx+ +i386-ecx+)
           (i386-emit-mov-abs-reg buf +va-addr+ +i386-edx+)
           ;; Store result
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-obj-ref+)
         ;; (obj-ref Vd Vobj idx:imm8) -- load object slot
         (let ((vd (first operands)) (vobj (second operands)) (idx (third operands)))
           (if (= vobj +vreg-vfp+)
               ;; Frame slot access: use safe EBP-relative offset below spill area
               (progn
                 (i386-emit-mov-reg-mem buf +i386-eax+ +i386-ebp+
                                        (+ +frame-slot-base+ (* idx -4)))
                 (i386-store-vreg buf vd +i386-eax+))
               ;; Normal object slot access
               (progn
                 (i386-load-vreg buf +i386-eax+ vobj)
                 (i386-emit-sub-reg-imm buf +i386-eax+ +tag-object+)
                 (i386-emit-mov-reg-mem buf +i386-eax+ +i386-eax+ (* (1+ idx) 4))
                 (i386-store-vreg buf vd +i386-eax+)))))

        ((op= +op-obj-set+)
         ;; (obj-set Vobj idx:imm8 Vs) -- store object slot
         (let ((vobj (first operands)) (idx (second operands)) (vs (third operands)))
           (if (= vobj +vreg-vfp+)
               ;; Frame slot store: use safe EBP-relative offset below spill area
               (progn
                 (i386-load-vreg buf +scratch0+ vs)
                 (i386-emit-mov-mem-reg buf +i386-ebp+
                                        (+ +frame-slot-base+ (* idx -4)) +scratch0+))
               ;; Normal object slot store
               ;; IMPORTANT: Load value FIRST — loading vobj into EAX clobbers VR.
               (progn
                 (i386-load-vreg buf +scratch0+ vs)
                 (i386-load-vreg buf +i386-eax+ vobj)
                 (i386-emit-sub-reg-imm buf +i386-eax+ +tag-object+)
                 (i386-emit-mov-mem-reg buf +i386-eax+ (* (1+ idx) 4) +scratch0+)))))

        ((op= +op-obj-tag+)
         ;; (obj-tag Vd Vs) -- extract low 4-bit tag as tagged fixnum
         (let ((vd (first operands)) (vs (second operands)))
           (i386-load-vreg buf +i386-eax+ vs)
           (i386-emit-and-reg-imm buf +i386-eax+ +tag-mask+)
           (i386-emit-shl-reg-imm buf +i386-eax+ 1)  ; tag as fixnum
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-obj-subtag+)
         ;; (obj-subtag Vd Vs) -- extract subtag from object header
         ;; Header format: (count << 8) | subtag. Subtag is low 8 bits.
         (let ((vd (first operands)) (vs (second operands)))
           (i386-load-vreg buf +i386-eax+ vs)
           (i386-emit-sub-reg-imm buf +i386-eax+ +tag-object+)
           (i386-emit-mov-reg-mem buf +i386-eax+ +i386-eax+ 0)  ; load header
           (i386-emit-and-reg-imm buf +i386-eax+ #xFF)          ; mask to subtag
           (i386-emit-shl-reg-imm buf +i386-eax+ 1)             ; tag as fixnum
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-aref+)
         ;; (aref Vd Vobj Vidx) — variable-index array load
         ;; Element at [Vobj + Vidx*2 - 5]
         ;; (Vidx is tagged fixnum: real_idx*2, *2 gives real_idx*4 on 32-bit)
         ;; Vobj is object-tagged pointer: raw_addr + 9
         ;; raw_addr + 4 (header) + real_idx*4 = Vobj - 9 + 4 + (Vidx/2)*4
         ;; = Vobj + Vidx*2 - 5
         (let ((vd (first operands)) (vobj (second operands)) (vidx (third operands)))
           ;; Load Vidx into scratch0, shift left 1 (Vidx*2 = real_idx*4)
           (i386-load-vreg buf +scratch0+ vidx)
           (i386-emit-shl-reg-imm buf +scratch0+ 1)
           ;; Add Vobj
           (i386-load-vreg buf +i386-eax+ vobj)
           (i386-emit-add-reg-reg buf +i386-eax+ +scratch0+)
           ;; Load from [EAX - 5]
           (i386-emit-mov-reg-mem buf +i386-eax+ +i386-eax+ -5)
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-aset+)
         ;; (aset Vobj Vidx Vs) — variable-index array store
         ;; Store Vs at [Vobj + Vidx*2 - 5]
         (let ((vobj (first operands)) (vidx (second operands)) (vs (third operands)))
           ;; Load value into scratch0 FIRST (before EAX clobbers VR)
           (i386-load-vreg buf +scratch0+ vs)
           ;; Compute address: Vobj + Vidx*2 - 5
           (i386-load-vreg buf +i386-eax+ vidx)
           (i386-emit-shl-reg-imm buf +i386-eax+ 1)
           ;; Need Vobj in EDX (scratch1), add to EAX
           (i386-emit-push-reg buf +scratch0+)  ; save value
           (i386-load-vreg buf +scratch1+ vobj)
           (i386-emit-add-reg-reg buf +i386-eax+ +scratch1+)
           (i386-emit-pop-reg buf +scratch0+)   ; restore value
           ;; Store value at [EAX - 5]
           (i386-emit-mov-mem-reg buf +i386-eax+ -5 +scratch0+)))

        ((op= +op-array-len+)
         ;; (array-len Vd Vobj) — extract element count from header
         ;; Header at [Vobj - 9], count in upper 24 bits (on 32-bit: [31:8])
         ;; Tagged result = count << 1
         (let ((vd (first operands)) (vobj (second operands)))
           (i386-load-vreg buf +i386-eax+ vobj)
           (i386-emit-sub-reg-imm buf +i386-eax+ +tag-object+)
           (i386-emit-mov-reg-mem buf +i386-eax+ +i386-eax+ 0)  ; load header word
           (i386-emit-shr-reg-imm buf +i386-eax+ 8)             ; count = header >> 8
           (i386-emit-and-reg-imm buf +i386-eax+ #xFFFFFF)      ; mask to 24 bits
           (i386-emit-shl-reg-imm buf +i386-eax+ 1)             ; tag as fixnum
           (i386-store-vreg buf vd +i386-eax+)))

        ;; ============================================
        ;; Raw Memory Operations
        ;; ============================================
        ((op= +op-load+)
         ;; (load Vd Vaddr width:imm8)
         (let ((vd (first operands)) (vaddr (second operands)) (width (third operands)))
           (i386-load-vreg buf +i386-eax+ vaddr)
           (ecase width
             (0 (i386-emit-movzx-byte buf +i386-eax+ +i386-eax+ 0))
             (1 (i386-emit-movzx-word buf +i386-eax+ +i386-eax+ 0))
             (2 (i386-emit-mov-reg-mem buf +i386-eax+ +i386-eax+ 0))
             (3 (i386-emit-mov-reg-mem buf +i386-eax+ +i386-eax+ 0)))  ; 64-bit: low 32
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-store+)
         ;; (store Vaddr Vs width:imm8)
         ;; IMPORTANT: Load Vs (value) FIRST — loading Vaddr into EAX clobbers VR,
         ;; so if Vs=VR we'd get the address instead of the value.
         (let ((vaddr (first operands)) (vs (second operands)) (width (third operands)))
           (i386-load-vreg buf +scratch0+ vs)
           (i386-load-vreg buf +i386-eax+ vaddr)
           (ecase width
             (0 (i386-emit-mov-mem8-reg buf +i386-eax+ 0 +scratch0+))
             (1 (i386-emit-mov-mem16-reg buf +i386-eax+ 0 +scratch0+))
             (2 (i386-emit-mov-mem-reg buf +i386-eax+ 0 +scratch0+))
             (3 (i386-emit-mov-mem-reg buf +i386-eax+ 0 +scratch0+)))))

        ((op= +op-fence+)
         (i386-emit-mfence buf))

        ;; ============================================
        ;; Function Calling (cdecl convention)
        ;; ============================================
        ((op= +op-call+)
         ;; (call target:imm32)
         ;; Target operand is the bytecode offset of the called function.
         ;; ESI/EDI/EBX are callee-saved in cdecl, so they survive.
         ;; EAX/ECX/EDX are caller-saved and will be clobbered.
         ;; Push V2/V3 onto the stack so callee can access them at [EBP+8/12].
         (let* ((target-offset (first operands))
                (fn-table (i386-translate-state-function-table state))
                (label (when fn-table (gethash target-offset fn-table))))
           ;; Push V3 first (higher address), then V2 (lower address)
           (i386-emit-push-mem buf +i386-ebp+ -20)  ; V3
           (i386-emit-push-mem buf +i386-ebp+ -16)  ; V2
           (if label
               (i386-emit-call-rel32 buf label)
               (i386-emit-call-rel32 buf nil))
           ;; Clean up pushed V2/V3
           (i386-emit-add-reg-imm buf +i386-esp+ 8)))

        ((op= +op-call-ind+)
         ;; (call-ind Vs) -- indirect call through register
         ;; Push V2/V3 onto the stack so callee can access them at [EBP+8/12].
         (let ((vs (first operands)))
           (i386-emit-push-mem buf +i386-ebp+ -20)  ; V3
           (i386-emit-push-mem buf +i386-ebp+ -16)  ; V2
           (i386-load-vreg buf +i386-eax+ vs)
           (i386-emit-call-reg buf +i386-eax+)
           ;; Clean up pushed V2/V3
           (i386-emit-add-reg-imm buf +i386-esp+ 8)))

        ((op= +op-ret+)
         ;; Return: emit full epilogue (restores callee-saved, pops frame)
         (i386-emit-epilogue buf))

        ((op= +op-tailcall+)
         ;; (tailcall target:imm32) -- tear down frame and jump
         ;; Target operand is the bytecode offset of the called function.
         ;; For i386, we need V2/V3 on the stack for the target function.
         ;; Save V2/V3 to scratch regs, tear down frame, push them, then jump.
         (let* ((target-offset (first operands))
                (fn-table (i386-translate-state-function-table state))
                (label (when fn-table (gethash target-offset fn-table))))
           ;; Save V2/V3 to scratch registers before frame teardown
           (i386-emit-mov-reg-mem buf +scratch0+ +i386-ebp+ -16)  ; V2 → ECX
           (i386-emit-mov-reg-mem buf +scratch1+ +i386-ebp+ -20)  ; V3 → EDX
           ;; Restore callee-saved before frame teardown
           (i386-emit-mov-reg-mem buf +i386-ebx+ +i386-ebp+ +save-ebx-off+)
           (i386-emit-mov-reg-mem buf +i386-esi+ +i386-ebp+ +save-esi-off+)
           (i386-emit-mov-reg-mem buf +i386-edi+ +i386-ebp+ +save-edi-off+)
           ;; Tear down frame
           (i386-emit-mov-reg-reg buf +i386-esp+ +i386-ebp+)
           (i386-emit-pop-reg buf +i386-ebp+)
           ;; ESP now points to return address. Pop it, push V3, V2, ret addr.
           (i386-emit-pop-reg buf +i386-eax+)         ; EAX = return addr
           (i386-emit-push-reg buf +scratch1+)        ; push V3
           (i386-emit-push-reg buf +scratch0+)        ; push V2
           (i386-emit-push-reg buf +i386-eax+)        ; push return addr back
           ;; Now stack: [ESP]=ret_addr, [ESP+4]=V2, [ESP+8]=V3
           ;; Target's prologue does PUSH EBP; MOV EBP,ESP →
           ;; [EBP+4]=ret_addr, [EBP+8]=V2, [EBP+12]=V3 ✓
           (if label
               (i386-emit-jmp-rel32 buf label)
               (i386-emit-jmp-rel32 buf nil))))

        ;; ============================================
        ;; GC and Allocation
        ;; ============================================
        ((op= +op-alloc-cons+)
         ;; (alloc-cons Vd) -- bump-allocate cons cell, tag as cons
         (let ((vd (first operands)))
           (i386-emit-mov-reg-abs buf +i386-eax+ +va-addr+)
           ;; Tag as cons
           (i386-emit-mov-reg-reg buf +scratch0+ +i386-eax+)
           (i386-emit-or-reg-imm buf +scratch0+ +tag-cons+)
           ;; Bump alloc by 16 (16-byte alignment for 4-bit tag)
           (i386-emit-add-reg-imm buf +i386-eax+ 16)
           (i386-emit-mov-abs-reg buf +va-addr+ +i386-eax+)
           ;; Store tagged result
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-gc-check+)
         ;; Compare VA (alloc ptr) against VL (alloc limit)
         ;; Both at absolute addresses. If VA >= VL, trigger GC.
         ;; IMPORTANT: Use scratch0 (ECX), NOT EAX! EAX is VR and may hold
         ;; a live value (e.g., the car arg popped before GC-CHECK + CONS).
         (i386-emit-mov-reg-abs buf +scratch0+ +va-addr+)
         (i386-emit-cmp-reg-abs buf +scratch0+ +vl-addr+)
         (let ((ok-label (i386-make-label)))
           (i386-emit-jcc buf :b ok-label)   ; unsigned below -> room left
           ;; GC needed
           (let ((gc-lbl (i386-translate-state-gc-label state)))
             (if gc-lbl
                 (i386-emit-call-rel32 buf gc-lbl)
                 (i386-emit-int buf #x31)))  ; trap to GC handler
           (i386-emit-label buf ok-label)))

        ((op= +op-write-barrier+)
         ;; (write-barrier Vobj) -- mark card table dirty (stub)
         (let ((vobj (first operands)))
           (i386-load-vreg buf +i386-eax+ vobj)
           (i386-emit-shr-reg-imm buf +i386-eax+ 12)
           ;; Card table write would go here; NOP for now
           (i386-emit-nop buf)))

        ;; ============================================
        ;; Actor / Concurrency
        ;; ============================================
        ((op= +op-save-ctx+)
         ;; Save register-resident state for actor context switch
         (i386-emit-push-reg buf +i386-esi+)     ; V0
         (i386-emit-push-reg buf +i386-edi+)     ; V1
         (i386-emit-push-reg buf +i386-ebx+)     ; V4
         ;; Also save VA/VL/VN from absolute addresses
         (i386-emit-push-abs buf +va-addr+)
         (i386-emit-push-abs buf +vl-addr+)
         (i386-emit-push-abs buf +vn-addr+))

        ((op= +op-restore-ctx+)
         ;; Restore (reverse order of save)
         (i386-emit-pop-abs buf +vn-addr+)
         (i386-emit-pop-abs buf +vl-addr+)
         (i386-emit-pop-abs buf +va-addr+)
         (i386-emit-pop-reg buf +i386-ebx+)
         (i386-emit-pop-reg buf +i386-edi+)
         (i386-emit-pop-reg buf +i386-esi+))

        ((op= +op-yield+)
         ;; Preemption check: stub (NOP)
         (i386-emit-nop buf))

        ((op= +op-atomic-xchg+)
         ;; (atomic-xchg Vd Vaddr Vs) -- XCHG [Vaddr], Vs -> Vd
         (let ((vd (first operands)) (vaddr (second operands)) (vs (third operands)))
           (i386-load-vreg buf +i386-eax+ vs)         ; value to exchange
           (i386-load-vreg buf +scratch0+ vaddr)      ; address
           (i386-emit-xchg-mem-reg buf +scratch0+ 0 +i386-eax+)
           ;; Old value now in EAX
           (i386-store-vreg buf vd +i386-eax+)))

        ;; ============================================
        ;; I/O Port Operations
        ;; ============================================
        ((op= +op-io-read+)
         ;; (io-read Vd port:imm16 width:imm8)
         (let ((vd (first operands)) (port (second operands)) (width (third operands)))
           (cond
             ;; Short form for ports <= 255
             ((<= port 255)
              (ecase width
                (0 (i386-emit-in-al-imm8 buf port)
                   (i386-emit-and-reg-imm buf +i386-eax+ #xFF))
                (1 (i386-emit-in-ax-imm8 buf port)
                   (i386-emit-and-reg-imm buf +i386-eax+ #xFFFF))
                (2 (i386-emit-in-eax-imm8 buf port))
                (3 (i386-emit-in-eax-imm8 buf port))))
             ;; General form: port in DX
             (t
              (i386-emit-mov-reg-imm buf +i386-edx+ port)
              (ecase width
                (0 (i386-emit-in-al-dx buf)
                   (i386-emit-and-reg-imm buf +i386-eax+ #xFF))
                (1 (i386-emit-in-ax-dx buf)
                   (i386-emit-and-reg-imm buf +i386-eax+ #xFFFF))
                (2 (i386-emit-in-eax-dx buf))
                (3 (i386-emit-in-eax-dx buf)))))
           ;; Tag as fixnum: SHL 1
           (i386-emit-shl-reg-imm buf +i386-eax+ 1)
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-io-write+)
         ;; (io-write port:imm16 Vs width:imm8)
         (let ((port (first operands)) (vs (second operands)) (width (third operands)))
           ;; Load value, untag (SHR for unsigned values >= 2^30)
           (i386-load-vreg buf +i386-eax+ vs)
           (i386-emit-shr-reg-imm buf +i386-eax+ 1)
           (cond
             ((<= port 255)
              (ecase width
                (0 (i386-emit-out-imm8-al buf port))
                (1 (i386-emit-byte buf #x66) (i386-emit-out-imm8-eax buf port))
                (2 (i386-emit-out-imm8-eax buf port))
                (3 (i386-emit-out-imm8-eax buf port))))
             (t
              (i386-emit-mov-reg-imm buf +i386-edx+ port)
              (ecase width
                (0 (i386-emit-out-dx-al buf))
                (1 (i386-emit-out-dx-ax buf))
                (2 (i386-emit-out-dx-eax buf))
                (3 (i386-emit-out-dx-eax buf)))))))

        ((op= +op-halt+)
         ;; HLT: F4 — single instruction, CPU wakes on next interrupt
         (i386-emit-hlt buf))

        ((op= +op-cli+)
         (i386-emit-cli buf))

        ((op= +op-sti+)
         (i386-emit-sti buf))

        ;; ============================================
        ;; Per-CPU Data (FS segment on i386)
        ;; ============================================
        ((op= +op-percpu-ref+)
         ;; (percpu-ref Vd offset:imm16)
         ;; MOV EAX, FS:[disp32]
         (let ((vd (first operands)) (offset (second operands)))
           (i386-emit-byte buf #x64)   ; FS segment prefix
           (i386-emit-byte buf #x8B)   ; MOV r32, r/m32
           (i386-emit-byte buf (i386-modrm #b00 +i386-eax+ 5))  ; mod=00, rm=5 -> [disp32]
           (i386-emit-u32 buf offset)
           (i386-store-vreg buf vd +i386-eax+)))

        ((op= +op-percpu-set+)
         ;; (percpu-set offset:imm16 Vs)
         ;; MOV FS:[disp32], reg
         (let ((offset (first operands)) (vs (second operands)))
           (i386-load-vreg buf +i386-eax+ vs)
           (i386-emit-byte buf #x64)   ; FS segment prefix
           (i386-emit-byte buf #x89)   ; MOV r/m32, r32
           (i386-emit-byte buf (i386-modrm #b00 +i386-eax+ 5))
           (i386-emit-u32 buf offset)))

        ;; ============================================
        ;; Unknown Opcode
        ;; ============================================
        (t
         ;; Emit trap for unrecognised MVM instructions
         (i386-emit-int3 buf))))))

;;; ============================================================
;;; Branch Target Scanning (Pass 1)
;;; ============================================================

(defun i386-scan-branch-targets (state)
  "Pre-scan MVM bytecode to identify all branch targets and create labels."
  (let* ((bytes (i386-translate-state-mvm-bytes state))
         (offset (i386-translate-state-mvm-offset state))
         (length (i386-translate-state-mvm-length state))
         (pos offset)
         (limit (+ offset length)))
    (loop while (< pos limit)
          do (let* ((decoded (decode-instruction bytes pos))
                    (opcode (car decoded))
                    (operands (cadr decoded))
                    (new-pos (cddr decoded)))
               (let ((info (gethash opcode *opcode-table*)))
                 (when info
                   (let ((op-types (opcode-info-operands info)))
                     (when (member :off32 op-types)
                       ;; Find the offset operand value
                       (let ((off-idx (position :off32 op-types)))
                         (when off-idx
                           (let* ((off (nth off-idx operands))
                                  (target-pos (+ new-pos off)))
                             (i386-ensure-label-at state target-pos))))))))
               (setf pos new-pos)))))

;;; ============================================================
;;; Main Translation Entry Points
;;; ============================================================

(defun translate-i386-function (bytecode offset length &optional target-buf)
  "Translate a single MVM function to i386 native code.
   Returns an i386-buffer with the translated code."
  (let* ((buf (or target-buf (make-i386-buffer)))
         (state (make-i386-translate-state
                 :buf buf
                 :mvm-bytes bytecode
                 :mvm-length length
                 :mvm-offset offset)))
    ;; Emit prologue
    (i386-emit-prologue buf)
    ;; Pass 1: scan for branch targets
    (i386-scan-branch-targets state)
    ;; Pass 2: translate instructions
    (let ((pos offset)
          (limit (+ offset length)))
      (loop while (< pos limit)
            do (progn
                 ;; Emit label if this position is a branch target
                 (let ((label (gethash pos (i386-translate-state-label-map state))))
                   (when label
                     (i386-emit-label buf label)))
                 ;; Decode and translate
                 (let* ((decoded (decode-instruction bytecode pos))
                        (opcode (car decoded))
                        (operands (cadr decoded))
                        (new-pos (cddr decoded)))
                   (i386-translate-insn state opcode operands new-pos)
                   (setf pos new-pos)))))
    ;; Resolve label fixups
    (i386-fixup-labels buf)
    buf))

(defun translate-mvm-to-i386 (bytecode function-table)
  "Translate MVM bytecode to i386 native code.
   BYTECODE: vector of (unsigned-byte 8) containing MVM instructions.
   FUNCTION-TABLE: list of (name offset length) entries.
   Returns (VALUES i386-buffer function-name-to-label-map)."
  (let* ((buf (make-i386-buffer))
         (n-functions (length function-table))
         (fn-labels (make-array n-functions))
         (fn-map (make-hash-table :test 'equal))
         ;; Map bytecode-offset → native label for CALL resolution
         (fn-offset-to-label (make-hash-table :test 'eql)))
    ;; Allocate a label for each function
    (loop for i from 0 below n-functions
          for entry in function-table
          for name = (first entry)
          for offset = (second entry)
          do (let ((label (i386-make-label)))
               (setf (aref fn-labels i) label)
               (setf (gethash name fn-map) label)
               (setf (gethash offset fn-offset-to-label) label)))
    ;; Translate each function
    (loop for i from 0 below n-functions
          for entry in function-table
          for fn-offset = (second entry)
          for fn-length = (third entry)
          do (let* ((fn-label (aref fn-labels i))
                    (state (make-i386-translate-state
                            :buf buf
                            :mvm-bytes bytecode
                            :mvm-length fn-length
                            :mvm-offset fn-offset
                            :function-table fn-offset-to-label)))
               ;; Emit function label
               (i386-emit-label buf fn-label)
               ;; Emit prologue
               (i386-emit-prologue buf)
               ;; Pass 1: scan branch targets
               (i386-scan-branch-targets state)
               ;; Pass 2: translate
               (let ((pos fn-offset)
                     (limit (+ fn-offset fn-length)))
                 (loop while (< pos limit)
                       do (progn
                            (let ((label (gethash pos
                                                  (i386-translate-state-label-map state))))
                              (when label
                                (i386-emit-label buf label)))
                            (let* ((decoded (decode-instruction bytecode pos))
                                   (opcode (car decoded))
                                   (operands (cadr decoded))
                                   (new-pos (cddr decoded)))
                              (i386-translate-insn state opcode operands new-pos)
                              (setf pos new-pos)))))))
    ;; Resolve all label fixups
    (i386-fixup-labels buf)
    ;; Resolve fn-map: replace label IDs with actual native byte positions
    ;; so cross.lisp can read them directly as integers
    (let ((resolved-map (make-hash-table :test 'equal)))
      (maphash (lambda (name label-id)
                 (let ((pos (gethash label-id (i386-buffer-labels buf))))
                   (when pos
                     (setf (gethash name resolved-map) pos))))
               fn-map)
      (values buf resolved-map))))

;;; ============================================================
;;; Target Descriptor Installation
;;; ============================================================

(defun i386-translate-single-instruction (opcode operands target buf)
  "Translate one MVM instruction to i386 code.
   Conforms to the target translate-fn signature."
  (declare (ignore target))
  (let ((state (make-i386-translate-state :buf buf)))
    (i386-translate-insn state opcode operands 0)))

(defun install-i386-translator ()
  "Install the i386 translator into the *target-i386* descriptor."
  (setf (target-translate-fn modus.mvm:*target-i386*)
        #'translate-mvm-to-i386)
  (setf (target-emit-prologue modus.mvm:*target-i386*)
        (lambda (target buf) (declare (ignore target)) (i386-emit-prologue buf)))
  (setf (target-emit-epilogue modus.mvm:*target-i386*)
        (lambda (target buf) (declare (ignore target)) (i386-emit-epilogue buf)))
  modus.mvm:*target-i386*)

;;; ============================================================
;;; Debugging Utilities
;;; ============================================================

(defun i386-disassemble-native (buf &key (start 0) (end nil))
  "Print a hex dump of the native code in BUF."
  (let* ((bytes (i386-buffer-bytes buf))
         (limit (or end (i386-buffer-position buf))))
    (loop for pos from start below limit
          do (when (zerop (mod (- pos start) 16))
               (when (> pos start) (terpri))
               (format t "  ~4,'0X: " pos))
             (format t "~2,'0X " (aref bytes pos)))
    (terpri)))

(defun i386-translation-statistics (bytecode-length native-buf)
  "Return (VALUES native-length expansion-ratio)."
  (let ((native-length (i386-current-pos native-buf)))
    (values native-length
            (if (zerop bytecode-length) 0.0
                (float (/ native-length bytecode-length))))))

;;; ============================================================
;;; Register Name Table
;;; ============================================================

(defparameter *i386-reg-names*
  #("EAX" "ECX" "EDX" "EBX" "ESP" "EBP" "ESI" "EDI"))

(defun i386-reg-name (reg)
  "Return the printable name of an i386 register."
  (if (< reg (length *i386-reg-names*))
      (aref *i386-reg-names* reg)
      (format nil "?~D" reg)))

(defun i386-describe-vreg-mapping ()
  "Print the virtual-to-physical register mapping for debugging."
  (format t "~&i386 Virtual Register Mapping (~D-bit):~%" (* 4 8))
  (format t "  ~20A ~8A ~A~%" "VREG" "PHYS" "SPILL")
  (format t "  ~20A ~8A ~A~%" "----" "----" "-----")
  (dotimes (i (length *i386-vreg-map*))
    (let ((phys (aref *i386-vreg-map* i))
          (name (if (< i (length modus.mvm::*vreg-names*))
                    (aref modus.mvm::*vreg-names* i)
                    (format nil "?~D" i))))
      (if phys
          (format t "  ~20A ~8A~%" name (i386-reg-name phys))
          (format t "  ~20A ~8A [EBP~@D]~%" name "spill"
                  (handler-case (i386-spill-offset i)
                    (error () 0)))))))
