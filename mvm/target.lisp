;;;; target.lisp - Target Architecture Descriptors for MVM
;;;;
;;;; Each target architecture provides a descriptor that maps virtual
;;;; registers to physical registers and defines platform-specific
;;;; translation parameters.

(in-package :modus.mvm)

;;; ============================================================
;;; Target Descriptor Structure
;;; ============================================================

(defstruct target
  name            ; keyword: :x86-64, :riscv64, :aarch64, :ppc64, :i386, :68k
  word-size       ; bytes per word: 4 or 8
  endianness      ; :little or :big
  reg-map         ; vector mapping vreg → physical register keyword
  n-phys-regs     ; number of physical GPRs
  callee-saved    ; list of vregs that map to callee-saved phys regs
  arg-regs        ; list of vregs used for arguments (V0-V3)
  scratch-regs    ; list of vregs for scratch use
  max-inline-regs ; vregs above this index spill to stack
  page-size       ; memory page size (bytes)
  translate-fn    ; function: (mvm-opcode operands target buf) → native bytes
  emit-prologue   ; function: (target buf) → emit function prologue
  emit-epilogue   ; function: (target buf) → emit function epilogue
  emit-boot       ; function: (target image) → emit boot sequence
  (float-support :none) ; :native (HW FPU) | :software (Lisp soft-float) | :none
  features)       ; plist of target-specific features

;;; ============================================================
;;; x86-64 Target
;;; ============================================================
;;;
;;; Register mapping:
;;;   V0 → RSI,  V1 → RDI,  V2 → R8,   V3 → R9    (args)
;;;   V4 → RBX,  V5 → RCX,  V6 → RDX,  V7 → R10   (scratch/general)
;;;   V8 → R11,  V9-V15 → stack spill
;;;   VR → RAX   (return value)
;;;   VA → R12   (alloc pointer)
;;;   VL → R14   (alloc limit)
;;;   VN → R15   (NIL)
;;;   VSP → RSP  (stack)
;;;   VFP → RBP  (frame)

(defparameter *target-x86-64*
  (make-target
   :name :x86-64
   :word-size 8
   :endianness :little
   :reg-map #(:rsi :rdi :r8 :r9       ; V0-V3 (args)
              :rbx :rcx :rdx :r10     ; V4-V7 (general)
              :r11 nil nil nil         ; V8-V11 (V9+ spill)
              nil nil nil nil          ; V12-V15 (spill)
              :rax                     ; VR
              :r12                     ; VA
              :r14                     ; VL
              :r15                     ; VN
              :rsp                     ; VSP
              :rbp                     ; VFP
              nil)                     ; VPC (not mapped)
   :n-phys-regs 16
   :callee-saved '(4 21)   ; V4 (RBX), VFP (RBP) - R12/R14/R15 are system regs
   :arg-regs '(0 1 2 3)    ; V0-V3
   :scratch-regs '(5 6 7 8) ; V5-V8
   :max-inline-regs 8       ; V9+ spill to stack
   :page-size 4096
   :translate-fn nil         ; set by translate-x64.lisp
   :emit-prologue nil
   :emit-epilogue nil
   :emit-boot nil
   :float-support :native        ; SSE2: MOVQ/ADDSD/SUBSD/MULSD/DIVSD/UCOMISD
   :features '(:has-io-ports t :has-lapic t :has-sipi t)))

;;; ============================================================
;;; RISC-V 64-bit Target
;;; ============================================================
;;;
;;; Register mapping:
;;;   V0 → a0, V1 → a1, V2 → a2, V3 → a3   (args)
;;;   V4 → s0, V5 → s1, V6 → s2, V7 → s3    (callee-saved)
;;;   V8 → s4, V9 → s5, V10 → s6, V11 → s7   (callee-saved)
;;;   V12-V15 → stack spill
;;;   VR → a0   (return)
;;;   VA → s8   (alloc pointer)
;;;   VL → s9   (alloc limit)
;;;   VN → s10  (NIL)
;;;   VSP → sp  (stack)
;;;   VFP → fp  (frame = s0 alias)

(defparameter *target-riscv64*
  (make-target
   :name :riscv64
   :word-size 8
   :endianness :little
   :reg-map #(:a0 :a1 :a2 :a3        ; V0-V3 (args) = x10-x13
              :s0 :s1 :s2 :s3        ; V4-V7 (callee-saved) = x8,x9,x18,x19
              :s4 :s5 :s6 :s7        ; V8-V11 (callee-saved) = x20-x23
              nil nil nil nil         ; V12-V15 (spill)
              :a0                     ; VR (aliases V0)
              :s8                     ; VA = x24
              :s9                     ; VL = x25
              :s10                    ; VN = x26
              :sp                     ; VSP = x2
              :fp                     ; VFP = x8 (alias of s0)
              nil)                    ; VPC
   :n-phys-regs 32
   :callee-saved '(4 5 6 7 8 9 10 11)
   :arg-regs '(0 1 2 3)
   :scratch-regs '(5 6 7 8)  ; t0-t3 available as temporaries
   :max-inline-regs 11
   :page-size 4096
   :translate-fn nil
   :emit-prologue nil
   :emit-epilogue nil
   :emit-boot nil
   :features '(:has-sbi t :has-plic t)))

;;; ============================================================
;;; AArch64 Target
;;; ============================================================
;;;
;;; Register mapping:
;;;   V0 → x0, V1 → x1, V2 → x2, V3 → x3    (args)
;;;   V4 → x19, V5 → x20, V6 → x21, V7 → x22  (callee-saved)
;;;   V8 → x23, V9-V15 → stack spill
;;;   VR → x0   (return)
;;;   VA → x24  (alloc pointer)
;;;   VL → x25  (alloc limit)
;;;   VN → x26  (NIL)
;;;   VSP → sp  (stack)
;;;   VFP → x29 (frame pointer = fp)

(defparameter *target-aarch64*
  (make-target
   :name :aarch64
   :word-size 8
   :endianness :little
   :reg-map #(:x0 :x1 :x2 :x3       ; V0-V3 (args)
              :x19 :x20 :x21 :x22   ; V4-V7 (callee-saved)
              :x23 nil nil nil        ; V8-V11
              nil nil nil nil         ; V12-V15 (spill)
              :x0                     ; VR (aliases V0)
              :x24                    ; VA
              :x25                    ; VL
              :x26                    ; VN
              :sp                     ; VSP
              :x29                    ; VFP (fp)
              nil)                    ; VPC
   :n-phys-regs 31
   :callee-saved '(4 5 6 7 8)
   :arg-regs '(0 1 2 3)
   :scratch-regs '(5 6 7 8)
   :max-inline-regs 8
   :page-size 4096
   :translate-fn nil
   :emit-prologue nil
   :emit-epilogue nil
   :emit-boot nil
   :features '(:has-gic t :has-psci t)))

;;; ============================================================
;;; PowerPC 64-bit Target
;;; ============================================================
;;;
;;; Register mapping:
;;;   V0 → r3, V1 → r4, V2 → r5, V3 → r6    (args)
;;;   V4 → r14, V5 → r15, V6 → r16, V7 → r17  (callee-saved)
;;;   V8 → r18, V9-V15 → stack spill
;;;   VR → r3   (return)
;;;   VA → r19  (alloc pointer)
;;;   VL → r20  (alloc limit)
;;;   VN → r21  (NIL)
;;;   VSP → r1  (stack)
;;;   VFP → r31 (frame pointer)

(defparameter *target-ppc64*
  (make-target
   :name :ppc64
   :word-size 8
   :endianness :big
   :reg-map #(:r3 :r4 :r5 :r6       ; V0-V3 (args)
              :r14 :r15 :r16 :r17   ; V4-V7 (callee-saved)
              :r18 nil nil nil        ; V8-V11
              nil nil nil nil         ; V12-V15 (spill)
              :r3                     ; VR (aliases V0)
              :r19                    ; VA
              :r20                    ; VL
              :r21                    ; VN
              :r1                     ; VSP (stack)
              :r31                    ; VFP (frame)
              nil)                    ; VPC
   :n-phys-regs 32
   :callee-saved '(4 5 6 7 8)
   :arg-regs '(0 1 2 3)
   :scratch-regs '(5 6 7 8)
   :max-inline-regs 8
   :page-size 4096
   :translate-fn nil
   :emit-prologue nil
   :emit-epilogue nil
   :emit-boot nil
   :features '(:has-openfirmware t)))

;;; ============================================================
;;; PowerPC 32-bit Target
;;; ============================================================
;;;
;;; Same register mapping as PPC64 (r3-r6 args, r14-r18 callee-saved,
;;; r19/r20/r21 system, r1 stack, r31 frame) but with 4-byte words.

(defparameter *target-ppc32*
  (make-target
   :name :ppc32
   :word-size 4
   :endianness :big
   :reg-map #(:r3 :r4 :r5 :r6       ; V0-V3 (args)
              :r14 :r15 :r16 :r17   ; V4-V7 (callee-saved)
              :r18 nil nil nil        ; V8-V11
              nil nil nil nil         ; V12-V15 (spill)
              :r3                     ; VR (aliases V0)
              :r19                    ; VA
              :r20                    ; VL
              :r21                    ; VN
              :r1                     ; VSP (stack)
              :r31                    ; VFP (frame)
              nil)                    ; VPC
   :n-phys-regs 32
   :callee-saved '(4 5 6 7 8)
   :arg-regs '(0 1 2 3)
   :scratch-regs '(5 6 7 8)
   :max-inline-regs 8
   :page-size 4096
   :translate-fn nil
   :emit-prologue nil
   :emit-epilogue nil
   :emit-boot nil
   :features '(:32-bit t)))

;;; ============================================================
;;; i386 Target
;;; ============================================================
;;;
;;; Register mapping (only 8 GPRs!):
;;;   V0 → ESI, V1 → EDI, V2 → stack, V3 → stack  (args, V2-V3 always spill)
;;;   V4 → EBX, V5-V15 → stack spill
;;;   VR → EAX  (return)
;;;   VA → stack slot (no dedicated alloc register!)
;;;   VL → stack slot
;;;   VN → stack slot (NIL address in memory)
;;;   VSP → ESP (stack)
;;;   VFP → EBP (frame)
;;;
;;; The i386 target is the hardest: with only 8 GPRs, most virtual
;;; registers spill to stack. The translator must be clever about
;;; register allocation.

(defparameter *target-i386*
  (make-target
   :name :i386
   :word-size 4
   :endianness :little
   :reg-map #(:esi :edi nil nil       ; V0-V3 (V2-V3 spill)
              :ebx nil nil nil        ; V4 only
              nil nil nil nil         ; V8-V11 (spill)
              nil nil nil nil         ; V12-V15 (spill)
              :eax                    ; VR
              nil                     ; VA (spill)
              nil                     ; VL (spill)
              nil                     ; VN (spill)
              :esp                    ; VSP
              :ebp                    ; VFP
              nil)                    ; VPC
   :n-phys-regs 8
   :callee-saved '(4)    ; V4 (EBX)
   :arg-regs '(0 1)      ; Only V0-V1 in registers
   :scratch-regs nil
   :max-inline-regs 4     ; V5+ always spill
   :page-size 4096
   :translate-fn nil
   :emit-prologue nil
   :emit-epilogue nil
   :emit-boot nil
   :features '(:has-io-ports t :32-bit t)))

;;; ============================================================
;;; Motorola 68000 Target
;;; ============================================================
;;;
;;; Register mapping (split D0-D7 data, A0-A7 address):
;;;   V0 → D0, V1 → D1, V2 → D2, V3 → D3    (args, data regs)
;;;   V4 → D4, V5 → D5, V6 → D6, V7 → D7    (callee-saved)
;;;   V8-V15 → stack spill
;;;   VR → D0   (return, aliases V0)
;;;   VA → A2   (alloc pointer, address reg)
;;;   VL → A3   (alloc limit, address reg)
;;;   VN → A4   (NIL, address reg)
;;;   VSP → A7  (stack pointer = SP)
;;;   VFP → A6  (frame pointer = FP)
;;;
;;; Address registers (A0-A5) used for alloc/NIL/frame.
;;; Data registers (D0-D7) for computation.
;;; A0-A1 available as scratch for address operations.

(defparameter *target-68k*
  (make-target
   :name :68k
   :word-size 4
   :endianness :big
   :reg-map #(:d0 :d1 :d2 :d3       ; V0-V3 (args, data regs)
              :d4 :d5 :d6 :d7       ; V4-V7 (callee-saved)
              nil nil nil nil         ; V8-V11 (spill)
              nil nil nil nil         ; V12-V15 (spill)
              :d0                     ; VR (aliases V0)
              :a2                     ; VA (address reg)
              :a3                     ; VL (address reg)
              :a4                     ; VN (address reg)
              :a7                     ; VSP (SP)
              :a6                     ; VFP (FP)
              nil)                    ; VPC
   :n-phys-regs 16  ; 8 data + 8 address
   :callee-saved '(4 5 6 7)
   :arg-regs '(0 1 2 3)
   :scratch-regs nil
   :max-inline-regs 7
   :page-size 4096
   :translate-fn nil
   :emit-prologue nil
   :emit-epilogue nil
   :emit-boot nil
   :features '(:split-register-file t :has-68k-addressing t)))

;;; ============================================================
;;; ARM32 (ARMv5)
;;; ============================================================

(defparameter *target-arm32*
  (make-target
   :name :arm32
   :word-size 4
   :endianness :little
   :reg-map #(:r0 :r1 :r2 :r3       ; V0-V3 (args)
              :r4 :r5 :r6 :r7       ; V4-V7 (callee-saved)
              nil nil nil nil         ; V8-V11 (spill)
              nil nil nil nil         ; V12-V15 (spill)
              :r0                     ; VR (aliases V0)
              :r9                     ; VA (alloc pointer)
              :r10                    ; VL (alloc limit)
              :r8                     ; VN (NIL)
              :r13                    ; VSP (SP)
              :r11                    ; VFP (FP)
              nil)                    ; VPC
   :n-phys-regs 16
   :callee-saved '(4 5 6 7)
   :arg-regs '(0 1 2 3)
   :scratch-regs nil
   :max-inline-regs 8
   :page-size 4096
   :translate-fn nil
   :emit-prologue nil
   :emit-epilogue nil
   :emit-boot nil
   :features '(:no-hw-divide t :32-bit t)))

;;; ============================================================
;;; ARMv7-A (Cortex-A7/A8/A9/A15)
;;; ============================================================
;;;
;;; Same register mapping and A32 encoding as ARM32 (ARMv5), but with
;;; hardware divide (SDIV/UDIV), MOVW/MOVT, DMB, LDREX/STREX.

(defparameter *target-armv7*
  (make-target
   :name :armv7
   :word-size 4
   :endianness :little
   :reg-map #(:r0 :r1 :r2 :r3       ; V0-V3 (args)
              :r4 :r5 :r6 :r7       ; V4-V7 (callee-saved)
              nil nil nil nil         ; V8-V11 (spill)
              nil nil nil nil         ; V12-V15 (spill)
              :r0                     ; VR (aliases V0)
              :r9                     ; VA (alloc pointer)
              :r10                    ; VL (alloc limit)
              :r8                     ; VN (NIL)
              :r13                    ; VSP (SP)
              :r11                    ; VFP (FP)
              nil)                    ; VPC
   :n-phys-regs 16
   :callee-saved '(4 5 6 7)
   :arg-regs '(0 1 2 3)
   :scratch-regs nil
   :max-inline-regs 8
   :page-size 4096
   :translate-fn nil
   :emit-prologue nil
   :emit-epilogue nil
   :emit-boot nil
   :features '(:no-hw-divide nil :32-bit t)))

;;; ============================================================
;;; ARMv7-A RPi (raspi2b — Cortex-A7, DWC2 USB, PL011 UART)
;;; ============================================================
;;;
;;; Same ISA as ARMv7 (virt) but different UART base (0x3F201000).
;;; Uses same translator with RPi-specific UART address.

(defparameter *target-armv7-rpi*
  (make-target
   :name :armv7-rpi
   :word-size 4
   :endianness :little
   :reg-map #(:r0 :r1 :r2 :r3       ; V0-V3 (args)
              :r4 :r5 :r6 :r7       ; V4-V7 (callee-saved)
              nil nil nil nil         ; V8-V11 (spill)
              nil nil nil nil         ; V12-V15 (spill)
              :r0                     ; VR (aliases V0)
              :r9                     ; VA (alloc pointer)
              :r10                    ; VL (alloc limit)
              :r8                     ; VN (NIL)
              :r13                    ; VSP (SP)
              :r11                    ; VFP (FP)
              nil)                    ; VPC
   :n-phys-regs 16
   :callee-saved '(4 5 6 7)
   :arg-regs '(0 1 2 3)
   :scratch-regs nil
   :max-inline-regs 8
   :page-size 4096
   :translate-fn nil
   :emit-prologue nil
   :emit-epilogue nil
   :emit-boot nil
   :features '(:no-hw-divide nil :32-bit t)))

;;; ============================================================
;;; Target Registry
;;; ============================================================

(defparameter *targets* (make-hash-table :test 'eq))

(defun register-target (target)
  "Register a target descriptor"
  (setf (gethash (target-name target) *targets*) target))

(defun find-target (name)
  "Find a target descriptor by name"
  (gethash name *targets*))

;; Register all built-in targets
(register-target *target-x86-64*)
(register-target *target-riscv64*)
(register-target *target-aarch64*)
(register-target *target-ppc64*)
(register-target *target-ppc32*)
(register-target *target-i386*)
(register-target *target-68k*)
(register-target *target-arm32*)
(register-target *target-armv7*)
(register-target *target-armv7-rpi*)

(defun list-targets ()
  "List all registered target names"
  (let (names)
    (maphash (lambda (k v) (declare (ignore v)) (push k names)) *targets*)
    (sort names #'string< :key #'symbol-name)))

;;; ============================================================
;;; Target Query Functions
;;; ============================================================

(defun target-vreg-to-phys (target vreg)
  "Map a virtual register to its physical register on TARGET.
   Returns NIL if the vreg spills to stack."
  (let ((map (target-reg-map target)))
    (when (< vreg (length map))
      (aref map vreg))))

(defun target-vreg-spills-p (target vreg)
  "Does this virtual register spill to stack on TARGET?"
  (null (target-vreg-to-phys target vreg)))

(defun target-spill-offset (target vreg)
  "Return the stack frame offset for a spilled virtual register.
   Spill slots start after saved callee registers."
  (let ((max-inline (target-max-inline-regs target))
        (word (target-word-size target)))
    (when (> vreg max-inline)
      (* (- vreg max-inline) word))))

(defun target-64-bit-p (target)
  "Is this a 64-bit target?"
  (= (target-word-size target) 8))

(defun target-big-endian-p (target)
  "Is this a big-endian target?"
  (eq (target-endianness target) :big))

;;; ============================================================
;;; Target numeric width (WS5 Phase 2b)
;;; ============================================================
;;;
;;; A tagged fixnum is value<<1 in one machine word, so the magnitude is
;;; (word_bits - 2) bits: 62 on a 64-bit target, 30 on a 32-bit one.  Before
;;; this, the whole numeric tower hardcoded 62-bit magic numbers, which is why
;;; a 32-bit target could not have a working bignum path and crypto-32.lisp /
;;; crypto-w32.lisp / 32bit-overrides.lisp had to be hand-forked.
;;;
;;; DESIGN INVARIANT: a bignum LIMB is exactly a non-negative fixnum.  So the
;;; limb mask IS +fixnum-max+ and the limb width IS +fixnum-bits+ — ONE
;;; constant, not two that happen to coincide.  A small (2-slot) bignum is
;;; lo + hi*2^bits, giving 2*bits of capacity by construction: 124 bits at 62,
;;; 60 bits at 30.  Values past that already route to the LSB-first
;;; big-bignum limb list, which scales with limb width automatically — so no
;;; representation change is needed to move the width.
;;;
;;; TWO CONSUMERS, deliberately served differently:
;;;   HOST side  — compile-integer's literal decomposition needs the TARGET's
;;;                width, which varies per build.  These defparameters.
;;;   IN-IMAGE   — the bignum core needs a FOLDED literal.  Served by the
;;;                defconstant block WIDTH-CONSTANTS-SOURCE generates, which
;;;                compile-source-to-module prepends to every compilation.
;;; The in-image half must be literals: the in-image DEFCONSTANT handler
;;; EVALs the value form, so a host-only helper there would break
;;; `modus --compile` (the self-host path).

(defparameter *target-big-endian-p* nil
  "Endianness of the target being compiled FOR.  Set by BUILD-IMAGE from the
   target descriptor, alongside the numeric width.")

(defparameter *target-fixnum-bits* 62
  "Magnitude bits in a tagged fixnum on the target being compiled FOR.
   Set by BUILD-IMAGE from the target's word size; 62 for 64-bit, 30 for
   32-bit.  Everything else derives from this one knob.")

(defparameter +fixnum-bits+      62)
(defparameter +fixnum-max+       4611686018427387903)
(defparameter +fixnum-min+       -4611686018427387903)
(defparameter +fixnum-limit+     4611686018427387904)
(defparameter +fixnum-neg-limit+ -4611686018427387904)
(defparameter +fixnum-half+      2305843009213693952)
(defparameter +fixnum-neg-half+  -2305843009213693952)
(defparameter +fixnum-half-max+  2305843009213693951)
(defparameter +limb-bits+        62)
(defparameter +limb-bits-1+      61)
(defparameter +limb-split-bits+  30)
(defparameter +neg-limb-bits+    -62)
(defparameter +neg-limb-bits-1+  -61)
(defparameter +fixnum-read-guard+ 230584300921369395)
(defparameter +small-bignum-bits+ 124)

(defun set-target-fixnum-bits (bits)
  "Set the target fixnum width and recompute every derived constant.
   BITS is (word-size * 8) - 2."
  (setf *target-fixnum-bits*   bits
        +fixnum-bits+          bits
        +fixnum-max+           (- (ash 1 bits) 1)
        +fixnum-min+           (- (- (ash 1 bits) 1))
        +fixnum-limit+         (ash 1 bits)
        +fixnum-neg-limit+     (- (ash 1 bits))
        +fixnum-half+          (ash 1 (- bits 1))
        +fixnum-neg-half+      (- (ash 1 (- bits 1)))
        +fixnum-half-max+      (- (ash 1 (- bits 1)) 1)
        +limb-bits+            bits
        +limb-bits-1+          (- bits 1)
        +limb-split-bits+      (- (floor bits 2) 1)
        +neg-limb-bits+        (- bits)
        +neg-limb-bits-1+      (- (- bits 1))
        ;; reader's safe-accumulate bound for small radices
        +fixnum-read-guard+    (floor (ash 1 (- bits 1)) 10)
        ;; 2-slot bignum capacity: lo + hi*2^bits
        +small-bignum-bits+    (* 2 bits))
  bits)

(defun set-target-fixnum-bits-for (target)
  "Derive the numeric width (and endianness) from TARGET."
  (setf *target-big-endian-p* (eq (target-endianness target) :big))
  (set-target-fixnum-bits (- (* 8 (target-word-size target)) 2)))

(defun width-constants-source ()
  "The width DEFCONSTANT block, as source text, with the CURRENT target's
   values baked in as literals.

   compile-source-to-module prepends this to every compilation, so no build
   script can forget it — there is no path that compiles baked source with a
   missing or host-inherited width.  That matters: a missing constant would
   NOT fail loudly, because compile-variable-ref falls through to a global
   SYMBOL-VALUE read returning NIL, which would silently miscompile the whole
   numeric tower.  Making the injection unconditional and central removes that
   failure mode by construction rather than relying on every script."
  (format nil
    "(defconstant +fixnum-bits+ ~D)~%(defconstant +fixnum-max+ ~D)~%~
     (defconstant +fixnum-min+ ~D)~%(defconstant +fixnum-limit+ ~D)~%~
     (defconstant +fixnum-neg-limit+ ~D)~%(defconstant +fixnum-half+ ~D)~%~
     (defconstant +fixnum-neg-half+ ~D)~%(defconstant +fixnum-half-max+ ~D)~%~
     (defconstant +limb-bits+ ~D)~%(defconstant +limb-bits-1+ ~D)~%~
     (defconstant +limb-split-bits+ ~D)~%(defconstant +neg-limb-bits+ ~D)~%~
     (defconstant +neg-limb-bits-1+ ~D)~%(defconstant +fixnum-read-guard+ ~D)~%~
     (defconstant +small-bignum-bits+ ~D)~%"
    +fixnum-bits+ +fixnum-max+ +fixnum-min+ +fixnum-limit+ +fixnum-neg-limit+
    +fixnum-half+ +fixnum-neg-half+ +fixnum-half-max+ +limb-bits+ +limb-bits-1+
    +limb-split-bits+ +neg-limb-bits+ +neg-limb-bits-1+ +fixnum-read-guard+
    +small-bignum-bits+))
