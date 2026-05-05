;;;; mvm.lisp - Modus Virtual Machine Instruction Set
;;;;
;;;; The MVM is a portable virtual ISA that sits between the Lisp compiler
;;;; and native code. Inspired by Genera's Ivory processor but designed for
;;;; AOT register-machine translation rather than hardware interpretation.
;;;;
;;;; Design principles:
;;;;   - Register-based (matches RISC targets and Modus calling convention)
;;;;   - Typed operations (MVM-CAR traps on non-cons, MVM-ADD handles fixnum fast path)
;;;;   - GC cooperation is architectural (allocation, write barriers, preemption)
;;;;   - ~50 instructions (vs Ivory's 256)
;;;;
;;;; Instruction encoding:
;;;;   [opcode:8] [operands:variable]
;;;;   Registers: 4-bit encoding (V0-V15 + specials)
;;;;   Immediates: 8/16/32/64-bit
;;;;   Branch offsets: 16-bit signed

(in-package :cl-user)

(defpackage :modus.mvm
  (:use :cl)
  (:export
   ;; Virtual registers
   #:+vreg-v0+ #:+vreg-v1+ #:+vreg-v2+ #:+vreg-v3+
   #:+vreg-v4+ #:+vreg-v5+ #:+vreg-v6+ #:+vreg-v7+
   #:+vreg-v8+ #:+vreg-v9+ #:+vreg-v10+ #:+vreg-v11+
   #:+vreg-v12+ #:+vreg-v13+ #:+vreg-v14+ #:+vreg-v15+
   #:+vreg-vr+ #:+vreg-va+ #:+vreg-vl+ #:+vreg-vn+
   #:+vreg-vsp+ #:+vreg-vfp+ #:+vreg-vpc+
   ;; Opcodes
   #:+op-nop+ #:+op-break+
   #:+op-mov+ #:+op-li+ #:+op-li-const+ #:+op-push+ #:+op-pop+
   #:+op-add+ #:+op-sub+ #:+op-mul+ #:+op-div+ #:+op-mod+
   #:+op-neg+ #:+op-inc+ #:+op-dec+
   #:+op-and+ #:+op-or+ #:+op-xor+
   #:+op-shl+ #:+op-shr+ #:+op-sar+ #:+op-shlv+ #:+op-sarv+ #:+op-ldb+
   #:+op-cmp+ #:+op-test+
   #:+op-br+ #:+op-beq+ #:+op-bne+ #:+op-blt+ #:+op-bge+
   #:+op-ble+ #:+op-bgt+ #:+op-bnull+ #:+op-bnnull+
   #:+op-car+ #:+op-cdr+ #:+op-cons+ #:+op-setcar+ #:+op-setcdr+
   #:+op-consp+ #:+op-atom+
   #:+op-alloc-obj+ #:+op-obj-ref+ #:+op-obj-set+
   #:+op-obj-tag+ #:+op-obj-subtag+
   #:+op-aref+ #:+op-aset+ #:+op-array-len+
   #:+op-load+ #:+op-store+ #:+op-fence+
   #:+op-call+ #:+op-call-ind+ #:+op-ret+ #:+op-tailcall+
   #:+op-alloc-cons+ #:+op-gc-check+ #:+op-write-barrier+
   #:+op-save-ctx+ #:+op-restore-ctx+ #:+op-yield+ #:+op-atomic-xchg+
   #:+op-io-read+ #:+op-io-write+ #:+op-halt+
   #:+op-cli+ #:+op-sti+ #:+op-percpu-ref+ #:+op-percpu-set+
   #:+op-fn-addr+
   #:+op-mul26lo+ #:+op-mul26hi+
   #:+op-mul64lo+ #:+op-mul64hi+ #:+op-acc128+
   #:+op-sap-new+ #:+op-sap-ref8+ #:+op-sap-ref32+ #:+op-sap-ref64+
   #:+op-sap-set8+ #:+op-sap-set32+ #:+op-sap-set64+ #:+op-sap-addr+
   #:+op-set-mv-count+
   #:+op-alloc-string+
   #:+op-set-cenv+ #:+op-get-cenv+
   #:+op-set-nargs+ #:+op-get-nargs+
   #:+op-trap+
   ;; Instruction metadata
   #:*opcode-table* #:opcode-info #:make-opcode-info
   #:opcode-info-code #:opcode-info-name #:opcode-info-operands #:opcode-info-description
   ;; Encoding/decoding
   #:encode-instruction #:decode-instruction
   #:make-mvm-buffer #:mvm-buffer-bytes #:mvm-buffer-used-bytes #:mvm-buffer-position #:mvm-buffer-labels
   #:mvm-emit-byte #:mvm-emit-u16 #:mvm-emit-u32 #:mvm-emit-u64
   #:mvm-emit-s16 #:mvm-emit-s32
   ;; Labels
   #:mvm-make-label #:*mvm-label-counter*
   #:mvm-emit-label #:mvm-emit-branch-to-label #:mvm-fixup-labels
   ;; Instruction constructors
   #:mvm-nop #:mvm-break
   #:mvm-mov #:mvm-li #:mvm-li-const #:mvm-push #:mvm-pop
   #:mvm-add #:mvm-sub #:mvm-mul #:mvm-div #:mvm-mod
   #:mvm-neg #:mvm-inc #:mvm-dec
   #:mvm-and #:mvm-or #:mvm-xor
   #:mvm-shl #:mvm-shr #:mvm-sar #:mvm-shlv #:mvm-sarv #:mvm-ldb
   #:mvm-cmp #:mvm-test
   #:mvm-br #:mvm-beq #:mvm-bne #:mvm-blt #:mvm-bge
   #:mvm-ble #:mvm-bgt #:mvm-bnull #:mvm-bnnull
   #:mvm-car #:mvm-cdr #:mvm-cons #:mvm-setcar #:mvm-setcdr
   #:mvm-consp #:mvm-atom
   #:mvm-alloc-obj #:mvm-obj-ref #:mvm-obj-set
   #:mvm-obj-tag #:mvm-obj-subtag
   #:mvm-aref #:mvm-aset #:mvm-array-len
   #:+op-alloc-array+ #:mvm-alloc-array
   #:mvm-load #:mvm-store #:mvm-fence
   #:mvm-call #:mvm-call-ind #:mvm-ret #:mvm-tailcall
   #:mvm-alloc-cons #:mvm-gc-check #:mvm-write-barrier
   #:mvm-save-ctx #:mvm-restore-ctx #:mvm-yield #:mvm-atomic-xchg
   #:mvm-io-read #:mvm-io-write #:mvm-halt
   #:mvm-cli #:mvm-sti #:mvm-percpu-ref #:mvm-percpu-set
   #:mvm-fn-addr
   #:mvm-trap #:mvm-rdtsc
   #:mvm-mul26lo #:mvm-mul26hi
   #:mvm-mul64lo #:mvm-mul64hi #:mvm-acc128
   #:mvm-sap-new #:mvm-sap-ref8 #:mvm-sap-ref32 #:mvm-sap-ref64
   #:mvm-sap-set8 #:mvm-sap-set32 #:mvm-sap-set64 #:mvm-sap-addr
   ;; Tagging (from compiler.lisp, shared across modules)
   #:+tag-fixnum+ #:+tag-cons+ #:+tag-object+ #:+tag-immediate+ #:+tag-forward+
   #:+fixnum-shift+ #:tag-fixnum #:+mvm-nil+ #:+mvm-t+
   ;; Target descriptors (from target.lisp)
   #:target #:make-target #:target-name #:target-word-size #:target-endianness
   #:target-reg-map #:target-n-phys-regs #:target-callee-saved
   #:target-arg-regs #:target-scratch-regs #:target-max-inline-regs
   #:target-page-size #:target-translate-fn #:target-emit-prologue
   #:target-emit-epilogue #:target-emit-boot #:target-features
   #:*target-x86-64* #:*target-riscv64* #:*target-aarch64*
   #:*target-ppc64* #:*target-i386* #:*target-68k*
   #:register-target #:find-target #:list-targets
   #:target-vreg-to-phys #:target-vreg-spills-p #:target-spill-offset
   ;; Compiler (from compiler.lisp)
   #:compiled-module #:make-compiled-module
   #:compiled-module-bytecode #:compiled-module-function-table
   #:compiled-module-constant-table
   #:function-info #:make-function-info
   #:function-info-name #:function-info-param-count
   #:function-info-bytecode-offset #:function-info-bytecode-length
   #:mvm-compile-toplevel #:mvm-compile-function #:mvm-compile-all
   #:register-mvm-bootstrap-macros #:mvm-define-macro
   #:disassemble-module #:disassemble-mvm #:test-mvm-compiler
   ;; Interpreter (from interp.lisp)
   #:mvm-interpret #:mvm-run-function #:mvm-interp-test
   ;; Cross-compilation (from cross.lisp)
   #:mvm-module #:make-mvm-module
   #:mvm-module-name #:mvm-module-bytecode #:mvm-module-function-table
   #:mvm-module-constant-table #:mvm-module-source-text
   #:mvm-function-info #:make-mvm-function-info
   #:mvm-function-info-name #:mvm-function-info-name-hash
   #:mvm-function-info-native-offset #:mvm-function-info-native-length
   #:kernel-image #:kernel-image-image-bytes #:kernel-image-entry-point
   #:kernel-image-native-code #:kernel-image-boot-code
   #:compile-source-to-module #:translate-module-to-native
   #:assemble-kernel-image #:build-image #:write-kernel-image
   #:test-cross-compilation #:read-all-forms #:read-all-forms-with-locations #:compute-name-hash
   #:compiled-module-to-mvm-module
   ;; Translator installers
   #:install-riscv-translator #:install-aarch64-translator
   #:install-ppc-translator #:install-68k-translator))

(in-package :modus.mvm)

;;; ============================================================
;;; Virtual Register Definitions
;;; ============================================================
;;;
;;; 16 virtual GPRs + special registers.
;;; Each architecture maps these to physical registers; extras spill.
;;;
;;; V0-V3:  argument registers
;;; V4-V15: general purpose
;;; VR:     return value
;;; VA:     alloc pointer
;;; VL:     alloc limit
;;; VN:     NIL constant
;;; VSP:    stack pointer
;;; VFP:    frame pointer
;;; VPC:    virtual program counter

;; GPR registers (4-bit encoded, 0-15)
(defconstant +vreg-v0+  0)
(defconstant +vreg-v1+  1)
(defconstant +vreg-v2+  2)
(defconstant +vreg-v3+  3)
(defconstant +vreg-v4+  4)
(defconstant +vreg-v5+  5)
(defconstant +vreg-v6+  6)
(defconstant +vreg-v7+  7)
(defconstant +vreg-v8+  8)
(defconstant +vreg-v9+  9)
(defconstant +vreg-v10+ 10)
(defconstant +vreg-v11+ 11)
(defconstant +vreg-v12+ 12)
(defconstant +vreg-v13+ 13)
(defconstant +vreg-v14+ 14)
(defconstant +vreg-v15+ 15)

;; Special registers (encoded in 5-bit space, 16-22)
(defconstant +vreg-vr+  16)   ; Return value
(defconstant +vreg-va+  17)   ; Alloc pointer
(defconstant +vreg-vl+  18)   ; Alloc limit
(defconstant +vreg-vn+  19)   ; NIL constant
(defconstant +vreg-vsp+ 20)   ; Stack pointer
(defconstant +vreg-vfp+ 21)   ; Frame pointer
(defconstant +vreg-vpc+ 22)   ; Virtual program counter

(defparameter *vreg-names*
  #("V0" "V1" "V2" "V3" "V4" "V5" "V6" "V7"
    "V8" "V9" "V10" "V11" "V12" "V13" "V14" "V15"
    "VR" "VA" "VL" "VN" "VSP" "VFP" "VPC"))

(defun vreg-name (reg)
  "Return printable name for virtual register"
  (if (< reg (length *vreg-names*))
      (aref *vreg-names* reg)
      (format nil "V?~D" reg)))

(defun vreg-gpr-p (reg)
  "Is REG a general-purpose register (V0-V15)?"
  (<= 0 reg 15))

(defun vreg-special-p (reg)
  "Is REG a special register?"
  (>= reg 16))

(defun vreg-arg-p (reg)
  "Is REG an argument register (V0-V3)?"
  (<= 0 reg 3))

;;; ============================================================
;;; Opcode Definitions
;;; ============================================================
;;;
;;; Operand types:
;;;   :reg    - 5-bit register encoding (packed into bytes)
;;;   :imm8   - 8-bit immediate
;;;   :imm16  - 16-bit immediate
;;;   :imm32  - 32-bit immediate
;;;   :imm64  - 64-bit immediate
;;;   :off32  - 32-bit signed branch offset
;;;   :width  - 2-bit memory width (0=u8, 1=u16, 2=u32, 3=u64)

;; Special / NOP
(defconstant +op-nop+    #x00)
(defconstant +op-break+  #x01)
(defconstant +op-trap+   #x02)  ; (trap code:imm16)

;; Data movement
(defconstant +op-mov+    #x10)  ; (mov Vd Vs) - 2 reg operands
(defconstant +op-li+     #x11)  ; (li Vd imm64) - reg + 64-bit immediate
(defconstant +op-push+   #x12)  ; (push Vs) - 1 reg operand
(defconstant +op-pop+    #x13)  ; (pop Vd) - 1 reg operand
(defconstant +op-li-const+ #x14) ; (li-const Vd idx:imm64) - load tagged address
                                 ; of constant-pool entry [idx]; resolved at
                                 ; image-assembly time once pool address known.
                                 ; Wire-compatible with LI in encoded size (10 B),
                                 ; differs only in semantics: translator emits a
                                 ; placeholder absolute load and records a patch.

;; Arithmetic (tagged fixnum fast path)
(defconstant +op-add+    #x20)  ; (add Vd Va Vb) - 3 reg
(defconstant +op-sub+    #x21)  ; (sub Vd Va Vb) - 3 reg
(defconstant +op-mul+    #x22)  ; (mul Vd Va Vb) - 3 reg
(defconstant +op-div+    #x23)  ; (div Vd Va Vb) - 3 reg
(defconstant +op-mod+    #x24)  ; (mod Vd Va Vb) - 3 reg
(defconstant +op-neg+    #x25)  ; (neg Vd Vs) - 2 reg
(defconstant +op-inc+    #x26)  ; (inc Vd) - 1 reg
(defconstant +op-dec+    #x27)  ; (dec Vd) - 1 reg

;; Bitwise
(defconstant +op-and+    #x28)  ; (and Vd Va Vb) - 3 reg
(defconstant +op-or+     #x29)  ; (or Vd Va Vb) - 3 reg
(defconstant +op-xor+    #x2A)  ; (xor Vd Va Vb) - 3 reg
(defconstant +op-shl+    #x2B)  ; (shl Vd Vs imm8) - 2 reg + imm8
(defconstant +op-shr+    #x2C)  ; (shr Vd Vs imm8) - 2 reg + imm8
(defconstant +op-sar+    #x2D)  ; (sar Vd Vs imm8) - 2 reg + imm8
(defconstant +op-shlv+   #x2F)  ; (shlv Vd Vs Vc) - 3 reg, shift left by register
(defconstant +op-sarv+   #x32)  ; (sarv Vd Vs Vc) - 3 reg, arithmetic shift right by register
(defconstant +op-ldb+    #x2E)  ; (ldb Vd Vs pos:imm8 size:imm8) - 2 reg + 2 imm8

;; Comparison
(defconstant +op-cmp+    #x30)  ; (cmp Va Vb) - 2 reg (sets flags)
(defconstant +op-test+   #x31)  ; (test Va Vb) - 2 reg (AND, sets flags)

;; Branch
(defconstant +op-br+     #x40)  ; (br off32) - unconditional
(defconstant +op-beq+    #x41)  ; (beq off32) - branch if equal
(defconstant +op-bne+    #x42)  ; (bne off32) - branch if not equal
(defconstant +op-blt+    #x43)  ; (blt off32) - branch if less than
(defconstant +op-bge+    #x44)  ; (bge off32) - branch if greater or equal
(defconstant +op-ble+    #x45)  ; (ble off32) - branch if less or equal
(defconstant +op-bgt+    #x46)  ; (bgt off32) - branch if greater than
(defconstant +op-bnull+  #x47)  ; (bnull Vs off32) - branch if nil
(defconstant +op-bnnull+ #x48)  ; (bnnull Vs off32) - branch if non-nil

;; List operations (tagged, type-checking)
(defconstant +op-car+    #x50)  ; (car Vd Vs) - 2 reg
(defconstant +op-cdr+    #x51)  ; (cdr Vd Vs) - 2 reg
(defconstant +op-cons+   #x52)  ; (cons Vd Va Vb) - 3 reg (allocating)
(defconstant +op-setcar+  #x53) ; (setcar Vd Vs) - 2 reg (with write barrier)
(defconstant +op-setcdr+  #x54) ; (setcdr Vd Vs) - 2 reg (with write barrier)
(defconstant +op-consp+  #x55)  ; (consp Vd Vs) - 2 reg (type check → boolean)
(defconstant +op-atom+   #x56)  ; (atom Vd Vs) - 2 reg

;; Object operations
(defconstant +op-alloc-obj+  #x60)  ; (alloc-obj Vd size:imm16 subtag:imm8) - reg + imm16 + imm8
(defconstant +op-obj-ref+    #x61)  ; (obj-ref Vd Vobj idx:imm8) - 2 reg + imm8
(defconstant +op-obj-set+    #x62)  ; (obj-set Vobj idx:imm8 Vs) - 2 reg + imm8
(defconstant +op-obj-tag+    #x63)  ; (obj-tag Vd Vs) - 2 reg
(defconstant +op-obj-subtag+ #x64)  ; (obj-subtag Vd Vs) - 2 reg
(defconstant +op-aref+       #x65)  ; (aref Vd Vobj Vidx) - 3 reg, variable-index array load
(defconstant +op-aset+       #x66)  ; (aset Vobj Vidx Vs) - 3 reg, variable-index array store
(defconstant +op-array-len+  #x67)  ; (array-len Vd Vobj) - 2 reg, extract element count
(defconstant +op-alloc-array+ #x68) ; (alloc-array Vd Vcount) - 2 reg, dynamic array allocation

;; Memory (raw, for drivers/hardware)
(defconstant +op-load+   #x70)  ; (load Vd Vaddr width:imm8) - 2 reg + width
(defconstant +op-store+  #x71)  ; (store Vaddr Vs width:imm8) - 2 reg + width
(defconstant +op-fence+  #x72)  ; (fence) - no operands

;; Function calling
(defconstant +op-call+      #x80)  ; (call target:imm32) - function index/offset
(defconstant +op-call-ind+  #x81)  ; (call-ind Vs) - 1 reg
(defconstant +op-ret+       #x82)  ; (ret) - no operands
(defconstant +op-tailcall+  #x83)  ; (tailcall target:imm32) - function index/offset

;; GC and allocation
(defconstant +op-alloc-cons+    #x88)  ; (alloc-cons Vd) - 1 reg
(defconstant +op-gc-check+      #x89)  ; (gc-check) - no operands
(defconstant +op-write-barrier+ #x8A)  ; (write-barrier Vobj) - 1 reg

;; Actor/concurrency
(defconstant +op-save-ctx+    #x90)  ; (save-ctx) - no operands
(defconstant +op-restore-ctx+ #x91)  ; (restore-ctx) - no operands
(defconstant +op-yield+       #x92)  ; (yield) - no operands
(defconstant +op-atomic-xchg+ #x93)  ; (atomic-xchg Vd Vaddr Vs) - 3 reg

;; System/platform
(defconstant +op-io-read+    #xA0)  ; (io-read Vd port:imm16 width:imm8)
(defconstant +op-io-write+   #xA1)  ; (io-write port:imm16 Vs width:imm8)
(defconstant +op-halt+       #xA2)  ; (halt) - no operands
(defconstant +op-cli+        #xA3)  ; (cli) - disable interrupts
(defconstant +op-sti+        #xA4)  ; (sti) - enable interrupts
(defconstant +op-percpu-ref+ #xA5)  ; (percpu-ref Vd offset:imm16) - reg + imm16
(defconstant +op-percpu-set+ #xA6)  ; (percpu-set offset:imm16 Vs) - imm16 + reg
(defconstant +op-fn-addr+   #xA7)  ; (fn-addr Vd target:imm32) - load tagged function address

;; Wide multiply with 26-bit split (for crypto field arithmetic on 32-bit targets)
(defconstant +op-mul26lo+  #xA8)  ; (mul26lo Vd Va Vb) - low 26 bits of untag(Va)*untag(Vb), tagged
(defconstant +op-mul26hi+  #xA9)  ; (mul26hi Vd Va Vb) - bits 26+ of untag(Va)*untag(Vb), tagged

;; Raw 64-bit multiply (for donna64 crypto on 64-bit targets)
;; All values are raw u64 (no tag/untag) — use with mem-ref :u64
(defconstant +op-mul64lo+  #xAA)  ; (mul64lo Vd Va Vb) - low 64 bits of Va*Vb, raw
(defconstant +op-mul64hi+  #xAB)  ; (mul64hi Vd Va Vb) - high 64 bits of Va*Vb, raw
(defconstant +op-acc128+   #xAC)  ; (acc128 Vaddr Vlo Vhi) - mem128[Vaddr] += Vhi:Vlo

;; SAP (System Area Pointer) — raw address wrapper for FFI
(defconstant +op-sap-new+    #xB0)  ; (sap-new Vd Vaddr) - alloc SAP, store raw addr
(defconstant +op-sap-ref8+   #xB1)  ; (sap-ref8 Vd Vsap Voff) - load u8 → tagged
(defconstant +op-sap-ref32+  #xB2)  ; (sap-ref32 Vd Vsap Voff) - load u32 → tagged
(defconstant +op-sap-ref64+  #xB3)  ; (sap-ref64 Vd Vsap Voff) - load u64 → raw
(defconstant +op-sap-set8+   #xB4)  ; (sap-set8 Vsap Voff Vval) - store byte (val tagged)
(defconstant +op-sap-set32+  #xB5)  ; (sap-set32 Vsap Voff Vval) - store u32 (val tagged)
(defconstant +op-sap-set64+  #xB6)  ; (sap-set64 Vsap Voff Vval) - store u64 (val raw)
(defconstant +op-sap-addr+   #xB7)  ; (sap-addr Vd Vsap) - extract raw addr → Vd
(defconstant +op-set-mv-count+ #xB8) ; (set-mv-count imm8) - store count to MV-COUNT
(defconstant +op-alloc-string+ #xB9) ; (alloc-string Vd Vs) - allocate string with size in Vs
(defconstant +op-set-cenv+   #xBA)  ; (set-cenv Vs) - set closure-env register from Vs
(defconstant +op-get-cenv+   #xBB)  ; (get-cenv Vd) - read closure-env register into Vd
(defconstant +op-set-nargs+  #xBC)  ; (set-nargs imm8) - set nargs reg (RAX) before call
(defconstant +op-get-nargs+  #xBD)  ; (get-nargs Vd) - read nargs reg into Vd (callee entry)

;;; ============================================================
;;; Opcode Metadata Table
;;; ============================================================
;;;
;;; Format: (opcode name operand-types description)
;;; Operand types: :reg :imm8 :imm16 :imm32 :imm64 :off32 :width

(defstruct opcode-info
  code          ; numeric opcode
  name          ; symbolic name (keyword)
  operands      ; list of operand type keywords
  description)  ; human-readable description

(defparameter *opcode-table* (make-hash-table :test 'eql))

(defmacro defopcode (name code operands description)
  `(setf (gethash ,code *opcode-table*)
         (make-opcode-info :code ,code :name ,name
                           :operands ',operands
                           :description ,description)))

;; Special
(defopcode :nop    #x00 ()                    "No operation")
(defopcode :break  #x01 ()                    "Debugger breakpoint")
(defopcode :trap   #x02 (:imm16)              "Software trap")

;; Data movement
(defopcode :mov    #x10 (:reg :reg)           "Register to register move")
(defopcode :li     #x11 (:reg :imm64)         "Load 64-bit immediate (tagged)")
(defopcode :push   #x12 (:reg)                "Push register to stack")
(defopcode :pop    #x13 (:reg)                "Pop from stack to register")
(defopcode :li-const #x14 (:reg :imm64)       "Load tagged addr of constant-pool[idx]")

;; Arithmetic
(defopcode :add    #x20 (:reg :reg :reg)      "Add tagged fixnums")
(defopcode :sub    #x21 (:reg :reg :reg)      "Subtract tagged fixnums")
(defopcode :mul    #x22 (:reg :reg :reg)      "Multiply tagged fixnums")
(defopcode :div    #x23 (:reg :reg :reg)      "Truncating divide tagged fixnums")
(defopcode :mod    #x24 (:reg :reg :reg)      "Modulus tagged fixnums")
(defopcode :neg    #x25 (:reg :reg)           "Negate tagged fixnum")
(defopcode :inc    #x26 (:reg)                "Increment tagged fixnum")
(defopcode :dec    #x27 (:reg)                "Decrement tagged fixnum")

;; Bitwise
(defopcode :band   #x28 (:reg :reg :reg)      "Bitwise AND")
(defopcode :bor    #x29 (:reg :reg :reg)      "Bitwise OR")
(defopcode :bxor   #x2A (:reg :reg :reg)      "Bitwise XOR")
(defopcode :shl    #x2B (:reg :reg :imm8)     "Shift left by immediate")
(defopcode :shr    #x2C (:reg :reg :imm8)     "Logical shift right by immediate")
(defopcode :sar    #x2D (:reg :reg :imm8)     "Arithmetic shift right by immediate")
(defopcode :shlv   #x2F (:reg :reg :reg)      "Shift left by register")
(defopcode :sarv   #x32 (:reg :reg :reg)      "Arithmetic shift right by register")
(defopcode :ldb    #x2E (:reg :reg :imm8 :imm8) "Bit field extract (pos, size)")

;; Comparison
(defopcode :cmp    #x30 (:reg :reg)           "Compare two tagged values")
(defopcode :test   #x31 (:reg :reg)           "Test (AND without storing)")

;; Branch
(defopcode :br     #x40 (:off32)              "Unconditional branch")
(defopcode :beq    #x41 (:off32)              "Branch if equal")
(defopcode :bne    #x42 (:off32)              "Branch if not equal")
(defopcode :blt    #x43 (:off32)              "Branch if less than")
(defopcode :bge    #x44 (:off32)              "Branch if greater or equal")
(defopcode :ble    #x45 (:off32)              "Branch if less or equal")
(defopcode :bgt    #x46 (:off32)              "Branch if greater than")
(defopcode :bnull  #x47 (:reg :off32)         "Branch if nil")
(defopcode :bnnull #x48 (:reg :off32)         "Branch if non-nil")

;; List operations
(defopcode :car    #x50 (:reg :reg)           "Load car (trap on non-cons)")
(defopcode :cdr    #x51 (:reg :reg)           "Load cdr (trap on non-cons)")
(defopcode :cons   #x52 (:reg :reg :reg)      "Allocate cons cell")
(defopcode :setcar #x53 (:reg :reg)           "Set car (with write barrier)")
(defopcode :setcdr #x54 (:reg :reg)           "Set cdr (with write barrier)")
(defopcode :consp  #x55 (:reg :reg)           "Type check cons → boolean")
(defopcode :atom   #x56 (:reg :reg)           "Type check atom → boolean")

;; Object operations
(defopcode :alloc-obj  #x60 (:reg :imm16 :imm8)  "Allocate object with header")
(defopcode :obj-ref    #x61 (:reg :reg :imm8)     "Load object slot")
(defopcode :obj-set    #x62 (:reg :imm8 :reg)     "Store object slot")
(defopcode :obj-tag    #x63 (:reg :reg)            "Extract 4-bit tag")
(defopcode :obj-subtag #x64 (:reg :reg)            "Extract 8-bit subtag from header")
(defopcode :aref      #x65 (:reg :reg :reg)        "Variable-index array load")
(defopcode :aset      #x66 (:reg :reg :reg)        "Variable-index array store")
(defopcode :array-len #x67 (:reg :reg)             "Extract array element count")
(defopcode :alloc-array #x68 (:reg :reg)           "Dynamic array allocation from register count")

;; Memory
(defopcode :load   #x70 (:reg :reg :imm8)     "Raw memory read (width in imm8)")
(defopcode :store  #x71 (:reg :reg :imm8)     "Raw memory write (width in imm8)")
(defopcode :fence  #x72 ()                    "Full memory barrier")

;; Function calling
(defopcode :call     #x80 (:imm32)            "Call function by index/offset")
(defopcode :call-ind #x81 (:reg)              "Indirect call via register")
(defopcode :ret      #x82 ()                  "Return from function")
(defopcode :tailcall #x83 (:imm32)            "Tail call optimization")

;; GC
(defopcode :alloc-cons    #x88 (:reg)         "Bump-allocate cons cell")
(defopcode :gc-check      #x89 ()             "Check allocation limit, GC if needed")
(defopcode :write-barrier #x8A (:reg)         "Mark card table dirty")

;; Actor/concurrency
(defopcode :save-ctx    #x90 (:reg)           "Save actor context (addr in reg, result in reg)")
(defopcode :restore-ctx #x91 (:reg)           "Restore actor context (addr in reg, never returns)")
(defopcode :yield       #x92 ()               "Preemption check point")
(defopcode :atomic-xchg #x93 (:reg :reg :reg) "Atomic exchange for spinlocks")

;; System
(defopcode :io-read    #xA0 (:reg :imm16 :imm8)  "I/O port read")
(defopcode :io-write   #xA1 (:imm16 :reg :imm8)  "I/O port write")
(defopcode :halt       #xA2 ()                    "Halt CPU")
(defopcode :cli        #xA3 ()                    "Disable interrupts")
(defopcode :sti        #xA4 ()                    "Enable interrupts")
(defopcode :percpu-ref #xA5 (:reg :imm16)         "Per-CPU data read")
(defopcode :percpu-set #xA6 (:imm16 :reg)         "Per-CPU data write")
(defopcode :fn-addr   #xA7 (:reg :imm32)          "Load tagged function address")

;; Wide multiply with 26-bit split (crypto field arithmetic)
(defopcode :mul26lo  #xA8 (:reg :reg :reg)        "Low 26 bits of wide multiply, tagged")
(defopcode :mul26hi  #xA9 (:reg :reg :reg)        "High bits (>>26) of wide multiply, tagged")
(defopcode :mul64lo  #xAA (:reg :reg :reg)        "Low 64 bits of raw multiply")
(defopcode :mul64hi  #xAB (:reg :reg :reg)        "High 64 bits of raw multiply")
(defopcode :acc128   #xAC (:reg :reg :reg)        "128-bit accumulate: mem[Va] += Vc:Vb")

;; SAP (System Area Pointer) operations
(defopcode :sap-new    #xB0 (:reg :reg)              "Allocate SAP with raw address")
(defopcode :sap-ref8   #xB1 (:reg :reg :reg)         "Load u8 from SAP+off → tagged")
(defopcode :sap-ref32  #xB2 (:reg :reg :reg)         "Load u32 from SAP+off → tagged")
(defopcode :sap-ref64  #xB3 (:reg :reg :reg)         "Load raw u64 from SAP+off")
(defopcode :sap-set8   #xB4 (:reg :reg :reg)         "Store byte to SAP+off")
(defopcode :sap-set32  #xB5 (:reg :reg :reg)         "Store u32 to SAP+off")
(defopcode :sap-set64  #xB6 (:reg :reg :reg)         "Store raw u64 to SAP+off")
(defopcode :sap-addr   #xB7 (:reg :reg)              "Extract raw address from SAP")
(defopcode :set-mv-count #xB8 (:imm8)                "Set MV count to imm8 (tagged store to MV-COUNT-ADDR)")
(defopcode :alloc-string #xB9 (:reg :reg)            "Allocate string (subtag #x31) with size in reg")
(defopcode :set-cenv     #xBA (:reg)                 "Set closure-env register from Vs (R13 on x64)")
(defopcode :get-cenv     #xBB (:reg)                 "Read closure-env register into Vd (R13 on x64)")
(defopcode :set-nargs    #xBC (:imm8)                "Set nargs register (RAX) to imm8 — caller side, before :call-indirect")
(defopcode :get-nargs    #xBD (:reg)                 "Read nargs register (RAX) into Vd — callee side, first prologue op")

;;; ============================================================
;;; Memory Width Constants
;;; ============================================================

(defconstant +width-u8+  0)
(defconstant +width-u16+ 1)
(defconstant +width-u32+ 2)
(defconstant +width-u64+ 3)

;;; ============================================================
;;; Bytecode Buffer
;;; ============================================================

(defstruct mvm-buffer
  (bytes (make-array 67108864 :element-type '(unsigned-byte 8)))  ; 64MB fixed-size, position tracks fill
  (labels (make-hash-table :test 'eql))     ; label-id → position
  (fixups nil)                               ; list of (position label-id offset-from)
  (position 0))

(defun mvm-emit-byte (buf byte)
  "Emit a single byte to the MVM bytecode buffer"
  (let ((pos (mvm-buffer-position buf)))
    (setf (aref (mvm-buffer-bytes buf) pos) (logand byte #xFF))
    (setf (mvm-buffer-position buf) (+ pos 1))))

(defun mvm-buffer-used-bytes (buf)
  "Return a new array containing only the bytes emitted so far."
  (let* ((n (mvm-buffer-position buf))
         (src (mvm-buffer-bytes buf))
         (result (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n result)
      (setf (aref result i) (aref src i)))))

(defun mvm-emit-u16 (buf val)
  "Emit a 16-bit value (little-endian)"
  (mvm-emit-byte buf (logand val #xFF))
  (mvm-emit-byte buf (logand (ash val -8) #xFF)))

(defun mvm-emit-s16 (buf val)
  "Emit a 16-bit signed value (little-endian)"
  (mvm-emit-u16 buf (if (minusp val) (logand val #xFFFF) val)))

(defun mvm-emit-s32 (buf val)
  "Emit a 32-bit signed value (little-endian)"
  (mvm-emit-u32 buf (if (minusp val) (logand val #xFFFFFFFF) val)))

(defun mvm-emit-u32 (buf val)
  "Emit a 32-bit value (little-endian)"
  (mvm-emit-byte buf (logand val #xFF))
  (mvm-emit-byte buf (logand (ash val -8) #xFF))
  (mvm-emit-byte buf (logand (ash val -16) #xFF))
  (mvm-emit-byte buf (logand (ash val -24) #xFF)))

(defun mvm-emit-u64 (buf val)
  "Emit a 64-bit value (little-endian)"
  (mvm-emit-u32 buf (logand val #xFFFFFFFF))
  (mvm-emit-u32 buf (logand (ash val -32) #xFFFFFFFF)))

;;; ============================================================
;;; Register Encoding
;;; ============================================================
;;;
;;; Registers are packed into bytes as pairs where possible.
;;; A single register uses 1 byte (5 bits used, 3 reserved).
;;; Two registers pack into 2 bytes: [reg1:5 | reg2_hi:3] [reg2_lo:2 | pad:6]
;;; For simplicity, use 1 byte per register with 5 bits used.

(defun mvm-emit-reg (buf reg)
  "Emit a register operand (1 byte, 5 bits used)"
  (mvm-emit-byte buf (logand reg #x1F)))

(defun mvm-emit-reg-pair (buf reg1 reg2)
  "Emit two registers packed into 2 bytes"
  (mvm-emit-byte buf (logand reg1 #x1F))
  (mvm-emit-byte buf (logand reg2 #x1F)))

(defun mvm-emit-reg-triple (buf reg1 reg2 reg3)
  "Emit three registers in 3 bytes"
  (mvm-emit-byte buf (logand reg1 #x1F))
  (mvm-emit-byte buf (logand reg2 #x1F))
  (mvm-emit-byte buf (logand reg3 #x1F)))

;;; ============================================================
;;; Instruction Encoding Functions
;;; ============================================================

(defun encode-instruction (buf opcode &rest operands)
  "Encode an MVM instruction into the bytecode buffer.
   OPCODE is the numeric opcode. OPERANDS are the raw values
   matching the opcode's operand specification."
  (let* ((info (gethash opcode *opcode-table*))
         (spec (if info (opcode-info-operands info) nil)))
    (mvm-emit-byte buf opcode)
    (loop for op-type in spec
          for op-val in operands
          do (ecase op-type
               (:reg    (mvm-emit-reg buf op-val))
               (:imm8   (mvm-emit-byte buf op-val))
               (:imm16  (mvm-emit-u16 buf op-val))
               (:imm32  (mvm-emit-u32 buf op-val))
               (:imm64  (mvm-emit-u64 buf op-val))
               (:off32  (mvm-emit-s32 buf op-val))
               (:width  (mvm-emit-byte buf op-val))))))

;;; ============================================================
;;; Instruction Decoding
;;; ============================================================

(defun decode-u16 (bytes pos)
  "Decode a 16-bit little-endian value"
  (logior (aref bytes pos)
          (ash (aref bytes (1+ pos)) 8)))

(defun decode-s16 (bytes pos)
  "Decode a 16-bit signed little-endian value"
  (let ((val (decode-u16 bytes pos)))
    (if (>= val #x8000)
        (- val #x10000)
        val)))

(defun decode-u32 (bytes pos)
  "Decode a 32-bit little-endian value"
  (logior (aref bytes pos)
          (ash (aref bytes (+ pos 1)) 8)
          (ash (aref bytes (+ pos 2)) 16)
          (ash (aref bytes (+ pos 3)) 24)))

(defun decode-s32 (bytes pos)
  "Decode a 32-bit signed little-endian value"
  (let ((val (decode-u32 bytes pos)))
    (if (>= val #x80000000)
        (- val #x100000000)
        val)))

(defun decode-u64 (bytes pos)
  "Decode a 64-bit little-endian value"
  (logior (decode-u32 bytes pos)
          (ash (decode-u32 bytes (+ pos 4)) 32)))

(defun decode-instruction (bytes pos)
  "Decode an MVM instruction starting at POS in BYTES.
   Returns (VALUES opcode operands new-pos) where operands is a list."
  (when (>= pos (length bytes))
    (return-from decode-instruction (cons 0 (cons nil (1+ pos)))))  ; NOP, advance past OOB
  (let* ((opcode (aref bytes pos))
         (info (gethash opcode *opcode-table*))
         (spec (if info (opcode-info-operands info) nil))
         (cur (1+ pos))
         (operands nil))
    (dolist (op-type spec)
      (ecase op-type
        (:reg
         (push (logand (aref bytes cur) #x1F) operands)
         (incf cur 1))
        (:imm8
         (push (aref bytes cur) operands)
         (incf cur 1))
        (:imm16
         (push (decode-u16 bytes cur) operands)
         (incf cur 2))
        (:imm32
         (push (decode-u32 bytes cur) operands)
         (incf cur 4))
        (:imm64
         (push (decode-u64 bytes cur) operands)
         (incf cur 8))
        (:off32
         (push (decode-s32 bytes cur) operands)
         (incf cur 4))
        (:width
         (push (aref bytes cur) operands)
         (incf cur 1))))
    (cons opcode (cons (nreverse operands) cur))))

;;; ============================================================
;;; Convenience Instruction Constructors
;;; ============================================================
;;; These emit specific instructions to a bytecode buffer.

;; Special
(defun mvm-nop (buf)
  (encode-instruction buf +op-nop+))

(defun mvm-break (buf)
  (encode-instruction buf +op-break+))

(defun mvm-trap (buf code)
  (encode-instruction buf +op-trap+ code))

(defun mvm-rdtsc (buf)
  "Read timestamp counter, return 64-bit cycles in VR (RAX)."
  (encode-instruction buf +op-trap+ #x0310))

;; Data movement
(defun mvm-mov (buf vd vs)
  (encode-instruction buf +op-mov+ vd vs))

(defun mvm-li (buf vd imm64)
  (encode-instruction buf +op-li+ vd imm64))

(defun mvm-li-const (buf vd idx)
  "Emit a li-const instruction (load tagged addr of constant-pool[idx]).
   Wire-format mirrors LI: 1 opcode + 1 reg + 8-byte immediate.  The
   immediate is the constant-pool index; the translator emits a
   placeholder absolute-address load and records a fixup that the
   image-assembly stage patches with the real tagged pool address."
  (encode-instruction buf +op-li-const+ vd idx))

(defun mvm-push (buf vs)
  (encode-instruction buf +op-push+ vs))

(defun mvm-pop (buf vd)
  (encode-instruction buf +op-pop+ vd))

;; Arithmetic
(defun mvm-add (buf vd va vb)
  (encode-instruction buf +op-add+ vd va vb))

(defun mvm-sub (buf vd va vb)
  (encode-instruction buf +op-sub+ vd va vb))

(defun mvm-mul (buf vd va vb)
  (encode-instruction buf +op-mul+ vd va vb))

(defun mvm-div (buf vd va vb)
  (encode-instruction buf +op-div+ vd va vb))

(defun mvm-mod (buf vd va vb)
  (encode-instruction buf +op-mod+ vd va vb))

(defun mvm-neg (buf vd vs)
  (encode-instruction buf +op-neg+ vd vs))

(defun mvm-inc (buf vd)
  (encode-instruction buf +op-inc+ vd))

(defun mvm-dec (buf vd)
  (encode-instruction buf +op-dec+ vd))

;; Bitwise
(defun mvm-and (buf vd va vb)
  (encode-instruction buf +op-and+ vd va vb))

(defun mvm-or (buf vd va vb)
  (encode-instruction buf +op-or+ vd va vb))

(defun mvm-xor (buf vd va vb)
  (encode-instruction buf +op-xor+ vd va vb))

(defun mvm-shl (buf vd vs amt)
  (encode-instruction buf +op-shl+ vd vs amt))

(defun mvm-shr (buf vd vs amt)
  (encode-instruction buf +op-shr+ vd vs amt))

(defun mvm-sar (buf vd vs amt)
  (encode-instruction buf +op-sar+ vd vs amt))

(defun mvm-shlv (buf vd vs vc)
  (encode-instruction buf +op-shlv+ vd vs vc))

(defun mvm-sarv (buf vd vs vc)
  (encode-instruction buf +op-sarv+ vd vs vc))

(defun mvm-ldb (buf vd vs pos size)
  (encode-instruction buf +op-ldb+ vd vs pos size))

;; Comparison
(defun mvm-cmp (buf va vb)
  (encode-instruction buf +op-cmp+ va vb))

(defun mvm-test (buf va vb)
  (encode-instruction buf +op-test+ va vb))

;; Branch
(defun mvm-br (buf offset)
  (encode-instruction buf +op-br+ offset))

(defun mvm-beq (buf offset)
  (encode-instruction buf +op-beq+ offset))

(defun mvm-bne (buf offset)
  (encode-instruction buf +op-bne+ offset))

(defun mvm-blt (buf offset)
  (encode-instruction buf +op-blt+ offset))

(defun mvm-bge (buf offset)
  (encode-instruction buf +op-bge+ offset))

(defun mvm-ble (buf offset)
  (encode-instruction buf +op-ble+ offset))

(defun mvm-bgt (buf offset)
  (encode-instruction buf +op-bgt+ offset))

(defun mvm-bnull (buf vs offset)
  (encode-instruction buf +op-bnull+ vs offset))

(defun mvm-bnnull (buf vs offset)
  (encode-instruction buf +op-bnnull+ vs offset))

;; List operations
(defun mvm-car (buf vd vs)
  (encode-instruction buf +op-car+ vd vs))

(defun mvm-cdr (buf vd vs)
  (encode-instruction buf +op-cdr+ vd vs))

(defun mvm-cons (buf vd va vb)
  (encode-instruction buf +op-cons+ vd va vb))

(defun mvm-setcar (buf vd vs)
  (encode-instruction buf +op-setcar+ vd vs))

(defun mvm-setcdr (buf vd vs)
  (encode-instruction buf +op-setcdr+ vd vs))

(defun mvm-consp (buf vd vs)
  (encode-instruction buf +op-consp+ vd vs))

(defun mvm-atom (buf vd vs)
  (encode-instruction buf +op-atom+ vd vs))

;; Object operations
(defun mvm-alloc-obj (buf vd size subtag)
  (encode-instruction buf +op-alloc-obj+ vd size subtag))

(defun mvm-obj-ref (buf vd vobj idx)
  (encode-instruction buf +op-obj-ref+ vd vobj idx))

(defun mvm-obj-set (buf vobj idx vs)
  (encode-instruction buf +op-obj-set+ vobj idx vs))

(defun mvm-obj-tag (buf vd vs)
  (encode-instruction buf +op-obj-tag+ vd vs))

(defun mvm-obj-subtag (buf vd vs)
  (encode-instruction buf +op-obj-subtag+ vd vs))

(defun mvm-aref (buf vd vobj vidx)
  (encode-instruction buf +op-aref+ vd vobj vidx))

(defun mvm-aset (buf vobj vidx vs)
  (encode-instruction buf +op-aset+ vobj vidx vs))

(defun mvm-array-len (buf vd vobj)
  (encode-instruction buf +op-array-len+ vd vobj))

(defun mvm-alloc-array (buf vd vcount)
  (encode-instruction buf +op-alloc-array+ vd vcount))

;; Memory
(defun mvm-load (buf vd vaddr width)
  (encode-instruction buf +op-load+ vd vaddr width))

(defun mvm-store (buf vaddr vs width)
  (encode-instruction buf +op-store+ vaddr vs width))

(defun mvm-fence (buf)
  (encode-instruction buf +op-fence+))

;; Function calling
(defun mvm-call (buf target)
  (encode-instruction buf +op-call+ target))

(defun mvm-call-ind (buf vs)
  (encode-instruction buf +op-call-ind+ vs))

(defun mvm-ret (buf)
  (encode-instruction buf +op-ret+))

(defun mvm-tailcall (buf target)
  (encode-instruction buf +op-tailcall+ target))

;; GC
(defun mvm-alloc-cons (buf vd)
  (encode-instruction buf +op-alloc-cons+ vd))

(defun mvm-gc-check (buf)
  (encode-instruction buf +op-gc-check+))

(defun mvm-write-barrier (buf vobj)
  (encode-instruction buf +op-write-barrier+ vobj))

;; Actor/concurrency
(defun mvm-save-ctx (buf reg)
  (encode-instruction buf +op-save-ctx+ reg))

(defun mvm-restore-ctx (buf reg)
  (encode-instruction buf +op-restore-ctx+ reg))

(defun mvm-yield (buf)
  (encode-instruction buf +op-yield+))

(defun mvm-atomic-xchg (buf vd vaddr vs)
  (encode-instruction buf +op-atomic-xchg+ vd vaddr vs))

;; System
(defun mvm-io-read (buf vd port width)
  (encode-instruction buf +op-io-read+ vd port width))

(defun mvm-io-write (buf port vs width)
  (encode-instruction buf +op-io-write+ port vs width))

(defun mvm-halt (buf)
  (encode-instruction buf +op-halt+))

(defun mvm-cli (buf)
  (encode-instruction buf +op-cli+))

(defun mvm-sti (buf)
  (encode-instruction buf +op-sti+))

(defun mvm-percpu-ref (buf vd offset)
  (encode-instruction buf +op-percpu-ref+ vd offset))

(defun mvm-percpu-set (buf offset vs)
  (encode-instruction buf +op-percpu-set+ offset vs))

(defun mvm-fn-addr (buf vd target)
  (encode-instruction buf +op-fn-addr+ vd target))

(defun mvm-mul26lo (buf vd va vb)
  (encode-instruction buf +op-mul26lo+ vd va vb))

(defun mvm-mul26hi (buf vd va vb)
  (encode-instruction buf +op-mul26hi+ vd va vb))

(defun mvm-mul64lo (buf vd va vb)
  (encode-instruction buf +op-mul64lo+ vd va vb))

(defun mvm-mul64hi (buf vd va vb)
  (encode-instruction buf +op-mul64hi+ vd va vb))

(defun mvm-acc128 (buf vaddr vlo vhi)
  (encode-instruction buf +op-acc128+ vaddr vlo vhi))

;; SAP operations
(defun mvm-sap-new (buf vd vaddr)
  (encode-instruction buf +op-sap-new+ vd vaddr))
(defun mvm-sap-ref8 (buf vd vsap voff)
  (encode-instruction buf +op-sap-ref8+ vd vsap voff))
(defun mvm-sap-ref32 (buf vd vsap voff)
  (encode-instruction buf +op-sap-ref32+ vd vsap voff))
(defun mvm-sap-ref64 (buf vd vsap voff)
  (encode-instruction buf +op-sap-ref64+ vd vsap voff))
(defun mvm-sap-set8 (buf vsap voff vval)
  (encode-instruction buf +op-sap-set8+ vsap voff vval))
(defun mvm-sap-set32 (buf vsap voff vval)
  (encode-instruction buf +op-sap-set32+ vsap voff vval))
(defun mvm-sap-set64 (buf vsap voff vval)
  (encode-instruction buf +op-sap-set64+ vsap voff vval))
(defun mvm-sap-addr (buf vd vsap)
  (encode-instruction buf +op-sap-addr+ vd vsap))

(defun mvm-set-mv-count (buf count)
  "Emit SET-MV-COUNT with immediate count value."
  (encode-instruction buf +op-set-mv-count+ count))

;;; ============================================================
;;; Disassembler
;;; ============================================================

(defun disassemble-mvm (bytes &key (start 0) (end nil))
  "Disassemble MVM bytecode, printing human-readable instructions"
  (let ((limit (or end (length bytes)))
        (pos start))
    (loop while (< pos limit)
          do (let ((ipos pos))
               (let* ((decoded (decode-instruction bytes pos))
                      (opcode (car decoded))
                      (operands (cadr decoded))
                      (new-pos (cddr decoded)))
                 (let ((info (gethash opcode *opcode-table*)))
                   (if info
                       (format t "  ~4D: ~A~{ ~A~}~%"
                               ipos
                               (opcode-info-name info)
                               (loop for op in operands
                                     for spec in (opcode-info-operands info)
                                     collect (case spec
                                               (:reg (vreg-name op))
                                               (:off32 (format nil "@~D" (+ new-pos op)))
                                               (otherwise (format nil "~D" op)))))
                       (format t "  ~4D: UNKNOWN(#x~2,'0X)~{ ~D~}~%"
                               ipos opcode operands)))
                 (setf pos new-pos))))))

;;; ============================================================
;;; Label Support for Bytecode Emission
;;; ============================================================

(defvar *mvm-label-counter* 0)

(defun mvm-make-label ()
  "Create a new unique label ID for bytecode"
  (incf *mvm-label-counter*))

(defun mvm-emit-label (buf label-id)
  "Record the current position as the target of LABEL-ID"
  (setf (gethash label-id (mvm-buffer-labels buf))
        (mvm-buffer-position buf)))

(defun mvm-emit-branch-to-label (buf opcode label-id)
  "Emit a branch instruction with a placeholder offset, recording a fixup"
  (let ((insn-start (mvm-buffer-position buf)))
    (mvm-emit-byte buf opcode)
    ;; For bnull/bnnull, the register operand is already emitted by caller
    ;; Record fixup: (position-of-offset label-id offset-from-position)
    (push (list (mvm-buffer-position buf) label-id
                (+ (mvm-buffer-position buf) 4))  ; offset relative to end of s32
          (mvm-buffer-fixups buf))
    (mvm-emit-s32 buf 0)))  ; placeholder

(defun mvm-fixup-labels (buf)
  "Resolve all branch label references in the bytecode buffer"
  (let ((bytes (mvm-buffer-bytes buf)))
    (dolist (fixup (mvm-buffer-fixups buf))
      (destructuring-bind (offset-pos label-id rel-from) fixup
        (let* ((target (gethash label-id (mvm-buffer-labels buf)))
               (rel (- target rel-from))
               (urel (if (minusp rel) (logand rel #xFFFFFFFF) rel)))
          (unless target
            (error "MVM: Undefined label ~D" label-id))
          (setf (aref bytes offset-pos) (logand urel #xFF))
          (setf (aref bytes (+ offset-pos 1)) (logand (ash urel -8) #xFF))
          (setf (aref bytes (+ offset-pos 2)) (logand (ash urel -16) #xFF))
          (setf (aref bytes (+ offset-pos 3)) (logand (ash urel -24) #xFF)))))))

;;; ============================================================
;;; Instruction Size Calculation
;;; ============================================================

(defun instruction-size (opcode)
  "Return the encoded size in bytes for the given opcode"
  (let ((info (gethash opcode *opcode-table*)))
    (if info
        (+ 1 (loop for op-type in (opcode-info-operands info)
                    sum (ecase op-type
                          (:reg 1)
                          (:imm8 1)
                          (:imm16 2)
                          (:imm32 4)
                          (:imm64 8)
                          (:off32 4)
                          (:width 1))))
        1)))  ; unknown opcode = 1 byte
