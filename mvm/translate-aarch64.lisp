;;;; translate-aarch64.lisp - MVM Bytecode → AArch64 Native Code Translator
;;;;
;;;; Translates MVM virtual instructions into AArch64 (ARM64) machine code.
;;;; Includes a self-contained AArch64 instruction encoder and the main
;;;; translation loop that walks decoded MVM bytecode and emits native
;;;; 32-bit instruction words.
;;;;
;;;; AArch64 register mapping (from target.lisp):
;;;;   V0 → x0, V1 → x1, V2 → x2, V3 → x3    (args)
;;;;   V4 → x19, V5 → x20, V6 → x21, V7 → x22  (callee-saved)
;;;;   V8 → x23, V9-V15 → stack spill
;;;;   VR → x0   (return, aliases V0)
;;;;   VA → x24  (alloc pointer)
;;;;   VL → x25  (alloc limit)
;;;;   VN → x26  (NIL)
;;;;   VSP → sp  (stack pointer)
;;;;   VFP → x29 (frame pointer)
;;;;
;;;; Scratch registers:
;;;;   x16 (IP0) - intra-procedure scratch 0
;;;;   x17 (IP1) - intra-procedure scratch 1
;;;;   x30 (LR)  - link register
;;;;
;;;; ============================================================
;;;; RAW-ADDR-AUDIT — grep this token to find every site below where
;;;; a raw native byte address (untagged, no Lisp value tag) is
;;;; produced or consumed in a way that could surprise the runtime.
;;;; The hazards split into two categories:
;;;;
;;;;   (1) Untrusted callable.  Most prominently +op-call-ind+ — the
;;;;       BLR target is whatever Vd holds, which could be NIL (raw
;;;;       0), a fixnum, or arbitrary user data.  The fix here is a
;;;;       TBNZ+CBZ filter in front of BLR; future range-check (task
;;;;       #46) would complete it.
;;;;
;;;;   (2) Raw/Lisp boundary mismatch.  Memory slots written natively
;;;;       (raw bytes) but read via (mem-ref :u64) from Lisp — or
;;;;       vice versa — must agree on tagging.  (mem-ref :u64) treats
;;;;       the loaded 64-bit word as a Lisp value, and Lisp fixnums
;;;;       are stored SHL'd by 1 in registers, so any raw address the
;;;;       native side deposits at a slot the Lisp side later reads
;;;;       must be SHL'd at write time and ASR'd at the next native
;;;;       read.  The GC trampoline (emit-aarch64-handler-helpers)
;;;;       has the canonical example at slots 0x10000068/70/78; the
;;;;       handler-state slots at 0x10000180/188/190 are *not* read
;;;;       from Lisp and use the simpler raw-only convention.
;;;;
;;;; If you add a new boundary-crosser, follow these conventions and
;;;; leave a "RAW-ADDR-AUDIT:" comment so it can be re-found later.
;;;; ============================================================

(in-package :modus.mvm)

;;; ============================================================
;;; Configurable UART Base Address
;;; ============================================================

(defvar *aarch64-serial-base* nil
  "Base address of the UART for AArch64 serial-out.
   If nil (default), build-image uses the boot descriptor's :serial-base,
   falling back to 0x09000000 (QEMU virt PL011).
   Set explicitly to override: 0x3F215040 for BCM2837 mini UART,
   0x3F201000 for BCM2837 PL011, 0x1F00030000 for Raspberry Pi 5.")

(defvar *aarch64-serial-width* 0
  "Store width for serial write: 0=byte (strb, PL011), 2=word (str, mini UART).
   BCM2835 mini UART requires 32-bit stores to AUX_MU_IO register.")

(defvar *aarch64-serial-tx-poll* nil
  "TX-ready poll config: nil (no poll) or (offset bit polarity).
   offset = byte offset from serial base to status register.
   bit = bit number to test.
   polarity = :tbz (wait while clear) or :tbnz (wait while set).
   Mini UART: '(#x14 5 :tbz) — LSR at base+0x14, bit 5 = TX empty, wait while clear.
   PL011:     '(#x18 5 :tbnz) — UARTFR at base+0x18, bit 5 = TXFF, wait while set.")

(defvar *aarch64-sched-lock-addr* nil
  "Scheduler lock address for RESTORE-CONTEXT unlock. Nil = skip (no actors).
   Set to the sched-lock-addr value for actor builds (e.g. #x41200200 for QEMU virt,
   #x02000200 for RPi). Non-actor builds get zero overhead.")

(defvar *aarch64-setup-irq-enable* nil
  "When non-nil, TRAP #x0320 (setup-irq) emits GICv2 + virtual timer init.
   Set to t for QEMU virt standalone builds. Nil for fixpoint (no GIC access).")

(defvar *aarch64-yield-nop* nil
  "When non-nil, YIELD emits NOP instead of SEV+WFE. Required for QEMU raspi3b
   where WFE halts the CPU with no wake event (no GIC to generate events).")

(defvar *aarch64-translate-into-buf* nil
  "When non-nil, an a64-buffer that translate-mvm-to-aarch64 should APPEND
   into (instead of creating a fresh one).  Used by assemble-kernel-image
   to unify the boot preamble and translated code in a single buffer with
   shared labels and one fixup pass.  When set, the translator skips
   a64-resolve-fixups so the caller can resolve once after all emit.
   Bound by translate-module-to-native via its :into-buf keyword.")

(defvar *aarch64-translated-start-idx* nil
  "Buffer position (instruction units) where the current translation began
   within *aarch64-translate-into-buf*.  Subtract from a64-current-index
   to get offsets RELATIVE to translated-start (so the fn-addr patcher
   and any other downstream consumer of native-byte-offset arithmetic
   keeps working regardless of buffer prefix).")

(defvar *aarch64-handler-push-label* nil
  "Label-id (integer) for the per-fork handler-stack push helper.  Bound
   by assemble-kernel-image around the unified-buffer translate call so
   that trap emitters and boot's IRQ vectors can BL to one shared helper.
   Nil outside that dynamic extent — emit-aarch64-handler-helpers is a
   no-op when nil so non-unified callers don't pay the cost.")

(defvar *aarch64-handler-pop-label* nil
  "Label-id (integer) for the per-fork handler-stack pop helper.  See
   *aarch64-handler-push-label*.")

(defvar *aarch64-gc-trampoline-label* nil
  "Label-id (integer) for the GC trampoline.  Bound by
   assemble-kernel-image around the unified-buffer translate call so
   that +op-gc-check+ can BL into one shared trampoline instead of
   trapping with BRK #1 (which has no recovery path on AArch64
   bare metal).  Nil outside that dynamic extent — the gc-check
   translator falls back to legacy BRK behaviour when unbound.")

(defvar *aarch64-stack-align-16* nil
  "When T, emit :push/:pop as a 16-byte aligned pair
   (SUB SP, #16 ; STR Xs, [SP])/(LDR Xd, [SP] ; ADD SP, #16) instead of
   the bare-metal STR Xs, [SP, #-8]! / LDR Xd, [SP], #8 single-word form.
   Linux EL0 enforces SP-alignment via SCTLR_EL1.SA0; any SP-relative
   load/store with an unaligned SP base traps SIGBUS BUS_ADRALN.  The
   Linux/AArch64 build script enables this; bare-metal builds leave it
   NIL to preserve the existing 8-byte stack discipline (the handler
   frame layout in fork-file etc. assumes single-word push offsets).")

(defvar *aarch64-linux-mode* nil
  "When T, AArch64 traps that have an OS dependency (serial write 0x0300,
   sys-exit 0x0500) switch to Linux syscall implementations
   (write(2) to fd=1; exit(2) with status).  NIL is bare-metal (UART
   MMIO, infinite WFI loop).")

(defvar *aarch64-fn-align-offset* 0
  "Bytes of pre-native-code wrapper between load-addr and the first
   instruction of native code at runtime.  Bare-metal puts native code
   directly at load-addr+native_image_offset (offset=0 here).  Linux
   ELF wraps prepend 120 bytes of ehdr+phdr before the LOAD bytes —
   so set this to 120 in the Linux/AArch64 build script so the
   function-entry alignment loop lands runtime VAs on 16-byte
   boundaries (needed for OR-3 fn-pointer tagging).")

(defvar *aarch64-gc-collect-bytecode-offset* nil
  "Bytecode-offset of %gc-collect in the kernel image, computed in
   cross.lisp by scanning the function table.  Used by the GC
   trampoline emit (in emit-aarch64-handler-helpers) to plant a
   fn-addr-patched MOVZ+MOVK+BLR sequence that calls the compiled
   collector at runtime.")

(defvar *aarch64-genadd-bytecode-offset* nil
  "Bytecode-offset of GENERIC-ADD in the kernel image, computed in
   cross.lisp by scanning the function table (same wiring as
   *aarch64-gc-collect-bytecode-offset*).  Used by +op-add-checked+'s
   overflow slow path to call the Lisp bignum-promotion routine via a
   fn-addr-patched MOVZ+MOVK+BLR.  NIL when the image has no
   GENERIC-ADD — +op-add-checked+ then degrades to a plain wrapping
   ADD (the pre-overflow-promotion behavior).")

(defvar *aarch64-genmul-bytecode-offset* nil
  "Companion to *aarch64-genadd-bytecode-offset* for GENERIC-MULTIPLY /
   +op-mul-checked+.")

(defvar *aarch64-gensub-bytecode-offset* nil
  "Companion to *aarch64-genadd-bytecode-offset* for GENERIC-SUBTRACT /
   +op-sub-checked+.")

(defvar *aarch64-li-const-patches* nil
  "List of (native-byte-offset . pool-index) recorded by +op-li-const+
   translation.  Each entry says: at NATIVE-BYTE-OFFSET (relative to
   the start of translated code) there is a MOVZ + MOVK + MOVK + MOVK
   quad whose imm16 fields must be patched with the 64-bit tagged
   constant-pool address of pool slot POOL-INDEX.  Applied by
   cross.lisp's apply-li-const-patches once the image layout is final
   (mirrors x64's *x64-li-const-patches* MOVABS-immediate scheme).")

(defvar *aarch64-fn-addr-patches* nil
  "List of (native-byte-offset . target-bytecode-offset) recorded by
   +op-fn-addr+ translation.  Each entry says: at NATIVE-BYTE-OFFSET in
   the translator's output buffer, there is a MOVZ + MOVK pair whose
   imm16 fields need to be patched with the low and high 16 bits of
   the runtime address of the function whose bytecode entry is at
   TARGET-BYTECODE-OFFSET.  The patch is applied by cross.lisp's
   `apply-aarch64-fn-addr-patches` after image assembly, when the
   native-image-offset is known.

   This replaces the original ADR (±1 MB PC-relative) emit with an
   absolute 32-bit address load.  Image-wide function references
   become layout-shift-stable, eliminating the ADR-truncation class
   of fragility bugs on AArch64 ANSI builds.")

(defvar *aarch64-jit-mode* nil
  "WS4 aarch64 Stage 3.  Non-nil only inside the runtime-JIT driver around a
   translate-mvm-to-aarch64 call.  Under it, op-call to a SYNTHETIC runtime
   offset (>= #x40000000 — a function NOT in this module, resolved by NAME via
   the rt-table) emits a RELOCATABLE absolute call (a MOVZ/MOVK quad → x16;
   BLR x16) and records the patch site in *aarch64-call-relocs*, instead of the
   SVC #x0511 undefined-call trap.  Nil at image-build time → whole-image
   codegen unchanged (byte-identical to pre-Stage-3).")

(defvar *aarch64-call-relocs* nil
  "WS4 aarch64 Stage 3.  List of (movz-quad-native-byte-offset . synthetic-mvm-
   offset) collected during a JIT translation.  At JIT time the driver resolves
   synthetic-offset → name (rt-table) → raw native address (%mvm-resolve-
   runtime-fn, untagged), then rewrites the 4 MOVZ/MOVK imm16 fields at that
   offset.  Bound freshly to nil per JIT translation; empty for any hazard-free
   (rt-empty) module, so flag-off / hazard-free translation is byte-identical.")

(defvar *aarch64-fn-addr-relocs* nil
  "WS4 aarch64 Stage 4.  List of (movz-quad-native-byte-offset . synthetic-mvm-
   offset) for OUT-OF-MODULE #'NAME value-loads (op-fn-addr to a synthetic
   runtime offset) collected during a JIT translation.  Under *aarch64-jit-mode*
   op-fn-addr to a synthetic offset emits a full MOVZ/MOVK quad placeholder
   (instead of the sentinel-0 the whole-image path uses) and records the site
   here; the JIT driver patches it with the fn's TAGGED native word
   (%val->word fn — a value load keeps the +tag-function+ tag, NOT word-3).
   Nil at image build → whole-image codegen unchanged.")

(defvar *aarch64-gc-bitmap-enabled* nil
  "WS4-AA64 #160: when non-nil, every heap ALLOCATION opcode emits an inline
   object-start-bit SET for the object it creates (raw base in x24, pre-bump).
   The bit records that this 16-byte granule is a REAL object start; gc.lisp's
   %gc-forward-slot / %gc-scan-copied then REFUSE to %gc-copy-object any
   conservative root whose tag-stripped address is not a recorded start — so a
   false root (a bignum limb / scratch word that merely looks like a from-space
   cons/object pointer) can no longer stamp a forwarding pointer over
   mid-object data.  This is the aarch64 port of the x64 MCGC scan_word
   validation (ace1544/810a975), adapted to the Lisp-side collector.  Default
   nil = no SET emitted = byte-identical to pre-#160 (bare-metal, gate, x64).")

(defun emit-aarch64-gc-set-bit (buf cfg-bitmap-addr)
  "WS4-AA64 #160: set the bit for the object whose raw base is in x24 (the alloc
   pointer, BEFORE the opcode bumps it) in the bitmap whose base pointer lives at
   config CFG-BITMAP-ADDR (0x10000E18 = object-start, 0x10000E40 = cons-kind).
   1 bit / 16-byte granule; page_base at config 0x10000E00 — both bases stored
   value<<1 by %gc-bitmap-init, so LDR then ASR #1 recovers the raw address.
   Scratch x9..x13 (dead across MVM opcodes; alloc opcodes use x16/x17 + phys
   vregs, never x9..x13).  Guarded: base == 0 (pre-init / disabled) skips the
   write.  AND/LSLV/LDRB/STRB encodings assembler-verified."
  (a64-load-imm64 buf +a64-x10+ cfg-bitmap-addr)
  (a64-ldr-unsigned buf +a64-x11+ +a64-x10+ 0)
  (a64-asr-imm buf +a64-x11+ +a64-x11+ 1)            ; x11 = bitmap_base
  (a64-cmp-imm buf +a64-x11+ 0)
  (let ((skip (incf *mvm-label-counter*)))
    (let ((idx (a64-current-index buf)))
      (a64-bcond buf +cc-eq+ 0)                       ; base==0 → skip
      (a64-add-fixup buf idx skip :bcond))
    (a64-load-imm64 buf +a64-x10+ #x10000E00)
    (a64-ldr-unsigned buf +a64-x9+ +a64-x10+ 0)
    (a64-asr-imm buf +a64-x9+ +a64-x9+ 1)             ; x9 = page_base
    (a64-sub-reg buf +a64-x9+ +a64-x24+ +a64-x9+ 0 0) ; x9 = addr - page_base
    (a64-lsr-imm buf +a64-x10+ +a64-x9+ 7)            ; x10 = byte offset (gran>>3)
    (a64-add-reg buf +a64-x11+ +a64-x11+ +a64-x10+ 0 0) ; x11 = byte address
    (a64-lsr-imm buf +a64-x9+ +a64-x9+ 4)             ; x9 = granule
    (a64-emit buf #x92400929)                         ; AND x9, x9, #7  (bit index)
    (a64-movz buf +a64-x12+ 1 0)                      ; x12 = 1
    (a64-emit buf #x9AC9218C)                         ; LSLV x12, x12, x9  (1<<bit)
    (a64-emit buf #x3940016D)                         ; LDRB w13, [x11]
    (a64-orr-reg buf +a64-x13+ +a64-x13+ +a64-x12+)   ; w13 |= mask
    (a64-emit buf #x3900016D)                         ; STRB w13, [x11]
    (a64-set-label buf skip)))

(defun emit-aarch64-gc-mark-start (buf)
  "Set the OBJECT-START bit (bitmap base @0x10000E18) for the object at x24.
   Emitted at every alloc site under *aarch64-gc-bitmap-enabled*."
  (when *aarch64-gc-bitmap-enabled*
    (emit-aarch64-gc-set-bit buf #x10000E18)))

(defun emit-aarch64-gc-mark-cons (buf)
  "WS4-AA64 #160 bug#4: ALSO set the CONS-KIND bit (bitmap base @0x10000E40) for
   the 16-byte cons at x24, so %gc-scan-copied classifies it as a cons (scan
   car+cdr) rather than reading car as an object header.  Emitted only at the
   CONS alloc site, after emit-aarch64-gc-mark-start."
  (when *aarch64-gc-bitmap-enabled*
    (emit-aarch64-gc-set-bit buf #x10000E40)))

(defvar *aarch64-code-base-patch-offset* nil
  "Byte offset of the MOVZ that loads code_base in the boot stub's
   emit-aarch64-code-bounds-init block.  Patched at link time by
   cross.lisp::apply-aarch64-code-bounds-patches once the runtime
   code_base / code_end addresses are known.  Same encoding shape
   as *aarch64-fn-addr-patches* — MOVZ at offset, MOVK at offset+4 —
   together they materialise a 32-bit address into x16.  Nil between
   builds.  Without this slot wired up, functionp (cl-eval.lisp:1747)
   falls through to its characterp fallback for any fn-addr whose
   low byte is 0x05 and misclassifies it as a non-function.")

(defvar *aarch64-code-end-patch-offset* nil
  "Companion to *aarch64-code-base-patch-offset* for code_end.")

;;; ============================================================
;;; AArch64 Physical Register Numbers
;;; ============================================================

(defconstant +a64-x0+   0)
(defconstant +a64-x1+   1)
(defconstant +a64-x2+   2)
(defconstant +a64-x3+   3)
(defconstant +a64-x4+   4)   ; AAPCS caller-saved
(defconstant +a64-x5+   5)   ; AAPCS caller-saved
(defconstant +a64-x6+   6)   ; AAPCS caller-saved
(defconstant +a64-x7+   7)   ; AAPCS caller-saved
(defconstant +a64-x8+   8)   ; AAPCS indirect-result reg
(defconstant +a64-x9+   9)   ; AAPCS caller-saved temp
(defconstant +a64-x10+ 10)   ; AAPCS caller-saved temp
(defconstant +a64-x11+ 11)   ; AAPCS caller-saved temp
(defconstant +a64-x12+ 12)   ; AAPCS caller-saved temp
(defconstant +a64-x13+ 13)   ; AAPCS caller-saved temp
(defconstant +a64-x14+ 14)   ; AAPCS caller-saved temp
(defconstant +a64-x15+ 15)   ; AAPCS caller-saved temp
(defconstant +a64-x16+ 16)   ; IP0 scratch
(defconstant +a64-x17+ 17)   ; IP1 scratch
(defconstant +a64-x18+ 18)   ; platform reg (free on bare-metal — used by handler-case copy traps)
(defconstant +a64-x19+ 19)
(defconstant +a64-x20+ 20)
(defconstant +a64-x21+ 21)
(defconstant +a64-x22+ 22)
(defconstant +a64-x23+ 23)
(defconstant +a64-x24+ 24)   ; VA alloc pointer
(defconstant +a64-x25+ 25)   ; VL alloc limit
(defconstant +a64-x26+ 26)   ; VN nil
(defconstant +a64-x27+ 27)   ; CENV — closure-env (mirrors x64 R13).  Callee-
                             ; saved by AAPCS; not touched by handler-stack
                             ; PUSH/POP helpers (those clobber x9..x13 only).
(defconstant +a64-x29+ 29)   ; FP
(defconstant +a64-x30+ 30)   ; LR
(defconstant +a64-sp+  31)   ; SP (context-dependent encoding with XZR)
(defconstant +a64-xzr+ 31)   ; Zero register (same encoding as SP)

;;; ============================================================
;;; Virtual → Physical Register Mapping
;;; ============================================================

(defparameter *a64-vreg-to-phys*
  (let ((map (make-array 23 :initial-element nil)))
    ;; GPR arguments
    (setf (aref map +vreg-v0+) +a64-x0+)
    (setf (aref map +vreg-v1+) +a64-x1+)
    (setf (aref map +vreg-v2+) +a64-x2+)
    (setf (aref map +vreg-v3+) +a64-x3+)
    ;; Callee-saved
    (setf (aref map +vreg-v4+) +a64-x19+)
    (setf (aref map +vreg-v5+) +a64-x20+)
    (setf (aref map +vreg-v6+) +a64-x21+)
    (setf (aref map +vreg-v7+) +a64-x22+)
    (setf (aref map +vreg-v8+) +a64-x23+)
    ;; V9-V15 spill (nil)
    ;; Special registers
    (setf (aref map +vreg-vr+)  +a64-x0+)    ; return = x0
    (setf (aref map +vreg-va+)  +a64-x24+)   ; alloc pointer
    (setf (aref map +vreg-vl+)  +a64-x25+)   ; alloc limit
    (setf (aref map +vreg-vn+)  +a64-x26+)   ; NIL
    (setf (aref map +vreg-vsp+) +a64-sp+)    ; stack pointer
    (setf (aref map +vreg-vfp+) +a64-x29+)   ; frame pointer
    map))

(defun a64-phys-reg (vreg)
  "Map a virtual register to its AArch64 physical register number.
   Returns NIL for spilled registers (V9-V15)."
  (when (< vreg (length *a64-vreg-to-phys*))
    (aref *a64-vreg-to-phys* vreg)))

(defun a64-spill-offset (vreg)
  "Return the frame-relative offset for a spilled virtual register.
   Spill slots begin at [FP, #-64] and grow downward.
   Returns NIL for non-spilled registers."
  (when (and (>= vreg +vreg-v9+) (<= vreg +vreg-v15+))
    (* (- vreg +vreg-v9+) -8)))

(defun a64-resolve-reg (vreg scratch)
  "Resolve VREG to a physical register. If it spills, load it into
   SCRATCH from the spill slot and return SCRATCH."
  (or (a64-phys-reg vreg) scratch))

;;; ============================================================
;;; AArch64 Native Code Buffer
;;; ============================================================
;;;
;;; Native code is emitted as a vector of 32-bit instruction words.
;;; Branch fixups are resolved in a second pass.

(defstruct a64-buffer
  ;; 16M entries × 4 bytes = 64 MB native code initial capacity.
  ;; Bare-metal ANSI-test needs ~30 MB; Linux/AArch64 adds ~50%
  ;; overhead per :push (2 insns vs 1 for SP alignment) plus larger
  ;; syscall traps.  The WS3 production-mvm-eval ANSI image (in-image
  ;; mvm.lisp ISA + interp + compiler + mvm-eval) blows past 64 MB —
  ;; a64-emit doubles the array on demand (same fix class as the
  ;; 96 MB x64 code-buffer overflow found landing WS3 on x64).
  (code (make-array 16777216))
  (labels (make-hash-table :test 'eql))
  (fixups nil)
  (position 0))

(defun a64-emit (buf word)
  "Emit a single 32-bit instruction word, doubling the code array on
   overflow (the fixed 16M-entry array silently capped the WS3 mvm-eval
   ANSI image at exactly 2^24 instructions: the tail emits — including
   the GC trampoline that defines the gc-check BL target label — were
   lost, and a64-resolve-fixups failed with `undefined label 3`)."
  (let ((pos (a64-buffer-position buf))
        (code (a64-buffer-code buf)))
    (when (>= pos (length code))
      (let ((new (make-array (* 2 (length code)))))
        (replace new code)
        (setf (a64-buffer-code buf) new)
        (setf code new)))
    (setf (aref code pos) (logand word #xFFFFFFFF))
    (setf (a64-buffer-position buf) (+ pos 1))))

(defun a64-current-index (buf)
  "Return the current instruction index (next emission slot)."
  (a64-buffer-position buf))

(defun a64-set-label (buf label-id)
  "Record the current position as the target of LABEL-ID."
  (setf (gethash label-id (a64-buffer-labels buf))
        (a64-buffer-position buf)))

(defun a64-add-fixup (buf index label-id type)
  "Record a branch fixup: INDEX is the instruction to patch,
   LABEL-ID is the target, TYPE is :b/:bcond/:bl."
  (push (list index label-id type) (a64-buffer-fixups buf)))

(defun %a64-check-branch-range (type offset index)
  "Assert OFFSET (in 32-bit instruction units) fits the encoding range
   for the branch TYPE.  Without this each branch silently truncates
   via (logand offset MASK), turning an out-of-range branch into a
   wild jump.  Ranges are signed:
     :b / :bl         imm26 = ±2^25 instructions (±128 MB)
     :bcond / :cbz    imm19 = ±2^18 instructions (±1 MB)
     :adr             imm21 BYTES = ±2^20 (±1 MB) — offset arg is in
                      instruction units, so range is ±2^18 here.
     :tbz             imm14 = ±2^13 instructions (±32 KB)
   ADR's overflow class is the original (still-open) AArch64
   `reference_aarch64_fragility.md` cliff — the fn-addr SBCL-style
   fix moved most fn-addr loads to MOVZ+MOVK absolute, but pure
   `ADR` sites in the translator still exist."
  (let ((lo nil) (hi nil))
    (ecase type
      ((:b :bl)         (setq lo (- (ash 1 25)) hi (1- (ash 1 25))))
      ((:bcond :cbz)    (setq lo (- (ash 1 18)) hi (1- (ash 1 18))))
      (:adr             (setq lo (- (ash 1 18)) hi (1- (ash 1 18))))
      (:tbz             (setq lo (- (ash 1 13)) hi (1- (ash 1 13)))))
    (unless (<= lo offset hi)
      (error "AArch64 ~A branch offset ~D out of range [~D, ~D] at ~
              index ~D — would silently truncate"
             type offset lo hi index))))

(defun a64-resolve-fixups (buf)
  "Resolve all branch fixups by patching instruction words.
   Each branch type's offset is range-checked before encoding so an
   out-of-range branch produces a build-time error instead of a
   silently-truncated wild jump."
  (let ((code (a64-buffer-code buf)))
    (dolist (fixup (a64-buffer-fixups buf))
      (destructuring-bind (index label-id type) fixup
        (let* ((target (gethash label-id (a64-buffer-labels buf))))
          (unless target
            (error "AArch64: undefined label ~D (fixup at index ~D, type ~A)" label-id index type))
          (let ((offset (- target index)))
            (%a64-check-branch-range type offset index)
            (ecase type
              (:b
               ;; B imm26: patch bits [25:0]
               (let ((word (aref code index)))
                 (setf (aref code index)
                       (logior (logand word #xFC000000)
                               (logand offset #x3FFFFFF)))))
              (:bl
               ;; BL imm26: patch bits [25:0]
               (let ((word (aref code index)))
                 (setf (aref code index)
                       (logior (logand word #xFC000000)
                               (logand offset #x3FFFFFF)))))
              (:bcond
               ;; B.cond imm19: patch bits [23:5]
               (let ((word (aref code index)))
                 (setf (aref code index)
                       (logior (logand word #xFF00001F)
                               (ash (logand offset #x7FFFF) 5)))))
              (:adr
               ;; ADR Xd: immlo(2)|10000|immhi(19)|Rd(5)
               ;; offset is in instructions, convert to bytes for ADR encoding
               (let* ((byte-off (* offset 4))
                      (immlo (logand byte-off 3))
                      (immhi (logand (ash byte-off -2) #x7FFFF))
                      (word (aref code index))
                      (rd (logand word #x1F)))
                 ;; Use nested 2-arg logior (bare-metal multi-arg may clobber)
                 (setf (aref code index)
                       (logior (logior (ash immlo 29) (ash #b10000 24))
                               (logior (ash immhi 5) rd)))))
              (:tbz
               ;; TBZ Xt, #imm6, label  /  TBNZ same shape
               ;; Layout: b5|0110110|b40(5)|imm14(14 bits at [18:5])|Rt(5)
               ;; Patch bits [18:5] with the signed offset (offset = target - index).
               (let ((word (aref code index)))
                 (setf (aref code index)
                       (logior (logand word #xFFF8001F)
                               (ash (logand offset #x3FFF) 5)))))
              (:cbz
               ;; CBZ/CBNZ Xt, label
               ;; Layout: sf|011010|0/1|imm19(19 bits at [23:5])|Rt(5)
               ;; Patch bits [23:5] with the signed offset (±1MB, in instructions).
               (let ((word (aref code index)))
                 (setf (aref code index)
                       (logior (logand word #xFF00001F)
                               (ash (logand offset #x7FFFF) 5))))))))))))

(defun a64-buffer-to-bytes (buf)
  "Convert the instruction buffer to a byte vector (little-endian)."
  (let* ((code (a64-buffer-code buf))
         (n (a64-buffer-position buf))
         (bytes (make-array (* n 4))))
    (dotimes (i n bytes)
      (let ((w (aref code i))
            (base (* i 4)))
        (setf (aref bytes base)       (logand w #xFF))
        (setf (aref bytes (+ base 1)) (logand (ash w -8) #xFF))
        (setf (aref bytes (+ base 2)) (logand (ash w -16) #xFF))
        (setf (aref bytes (+ base 3)) (logand (ash w -24) #xFF))))))

;;; ============================================================
;;; AArch64 Instruction Encoders
;;; ============================================================
;;;
;;; All AArch64 instructions are 32-bit fixed width.
;;; We encode for the 64-bit (sf=1) variant unless noted.

;;; --- Condition codes ---

(defconstant +cc-eq+ #x0)    ; equal (Z=1)
(defconstant +cc-ne+ #x1)    ; not equal (Z=0)
(defconstant +cc-lt+ #xB)    ; signed less than (N!=V)
(defconstant +cc-ge+ #xA)    ; signed greater or equal (N==V)
(defconstant +cc-le+ #xD)    ; signed less or equal (Z=1 || N!=V)
(defconstant +cc-gt+ #xC)    ; signed greater than (Z=0 && N==V)
(defconstant +cc-cs+ #x2)    ; carry set / unsigned >=
(defconstant +cc-cc+ #x3)    ; carry clear / unsigned <
(defconstant +cc-vs+ #x6)    ; overflow set (V=1) — signed overflow occurred
(defconstant +cc-vc+ #x7)    ; overflow clear (V=0)
(defconstant +cc-al+ #xE)    ; always

;;; --- ADD (shifted register) ---
;;; sf|0|0|01011|shift(2)|0|Rm(5)|imm6(6)|Rn(5)|Rd(5)

(defun a64-add-reg (buf rd rn rm shift amount)
  "ADD Xd, Xn, Xm{, shift #amount}  (64-bit)  shift: 0=LSL 1=LSR 2=ASR"
  (a64-emit buf (logior (ash 1 31)        ; sf=1 (64-bit)
                        (ash 0 30)        ; op=0 (ADD)
                        (ash 0 29)        ; S=0
                        (ash #b01011 24)
                        (ash shift 22)
                        (ash 0 21)        ; must be 0
                        (ash rm 16)
                        (ash amount 10)
                        (ash rn 5)
                        rd)))

(defun a64-adds-reg (buf rd rn rm shift amount)
  "ADDS Xd, Xn, Xm{, shift #amount}  (64-bit, sets flags)  shift: 0=LSL 1=LSR 2=ASR"
  (a64-emit buf (logior (ash 1 31)        ; sf=1
                        (ash 0 30)        ; op=0
                        (ash 1 29)        ; S=1
                        (ash #b01011 24)
                        (ash shift 22)
                        (ash 0 21)
                        (ash rm 16)
                        (ash amount 10)
                        (ash rn 5)
                        rd)))

;;; --- SUB (shifted register) ---
;;; sf|1|0|01011|shift(2)|0|Rm(5)|imm6(6)|Rn(5)|Rd(5)

(defun a64-sub-reg (buf rd rn rm shift amount)
  "SUB Xd, Xn, Xm{, shift #amount}  (64-bit)  shift: 0=LSL 1=LSR 2=ASR"
  (a64-emit buf (logior (ash 1 31)        ; sf=1
                        (ash 1 30)        ; op=1 (SUB)
                        (ash 0 29)        ; S=0
                        (ash #b01011 24)
                        (ash shift 22)
                        (ash 0 21)
                        (ash rm 16)
                        (ash amount 10)
                        (ash rn 5)
                        rd)))

(defun a64-subs-reg (buf rd rn rm shift amount)
  "SUBS Xd, Xn, Xm{, shift #amount}  (64-bit, sets flags)  shift: 0=LSL 1=LSR 2=ASR"
  (a64-emit buf (logior (ash 1 31)
                        (ash 1 30)        ; SUB
                        (ash 1 29)        ; S=1
                        (ash #b01011 24)
                        (ash shift 22)
                        (ash 0 21)
                        (ash rm 16)
                        (ash amount 10)
                        (ash rn 5)
                        rd)))

(defun a64-cmp-reg (buf rn rm)
  "CMP Xn, Xm  →  SUBS XZR, Xn, Xm"
  (a64-subs-reg buf +a64-xzr+ rn rm 0 0))

;;; --- ADD/SUB (immediate) ---
;;; sf|op|S|100010|sh|imm12(12)|Rn(5)|Rd(5)
;;; sh=0: imm12 not shifted. sh=1: imm12 << 12.

(defun a64-add-imm (buf rd rn imm12)
  "ADD Xd, Xn, #imm12"
  (a64-emit buf (logior (ash 1 31)          ; sf=1
                        (ash 0 30)          ; op=0 (ADD)
                        (ash 0 29)          ; S=0
                        (ash #b100010 23)
                        (ash (logand imm12 #xFFF) 10)
                        (ash rn 5)
                        rd)))

(defun a64-adds-imm (buf rd rn imm12)
  "ADDS Xd, Xn, #imm12  (sets flags)"
  (a64-emit buf (logior (ash 1 31)
                        (ash 0 30)
                        (ash 1 29)          ; S=1
                        (ash #b100010 23)
                        (ash (logand imm12 #xFFF) 10)
                        (ash rn 5)
                        rd)))

(defun a64-sub-imm (buf rd rn imm12)
  "SUB Xd, Xn, #imm12"
  (a64-emit buf (logior (ash 1 31)
                        (ash 1 30)          ; SUB
                        (ash 0 29)
                        (ash #b100010 23)
                        (ash (logand imm12 #xFFF) 10)
                        (ash rn 5)
                        rd)))

(defun a64-subs-imm (buf rd rn imm12)
  "SUBS Xd, Xn, #imm12  (sets flags)"
  (a64-emit buf (logior (ash 1 31)
                        (ash 1 30)
                        (ash 1 29)          ; S=1
                        (ash #b100010 23)
                        (ash (logand imm12 #xFFF) 10)
                        (ash rn 5)
                        rd)))

(defun a64-cmp-imm (buf rn imm12)
  "CMP Xn, #imm12  →  SUBS XZR, Xn, #imm12"
  (a64-subs-imm buf +a64-xzr+ rn imm12))

;;; --- MOV (register) ---
;;; MOV Xd, Xm  →  ORR Xd, XZR, Xm

(defun a64-mov-reg (buf rd rm)
  "MOV Xd, Xm  →  ORR Xd, XZR, Xm"
  ;; ORR (shifted register): sf|01|01010|shift|0|Rm|imm6|Rn|Rd
  (a64-emit buf (logior (ash 1 31)          ; sf=1
                        (ash #b01 29)       ; opc=01 (ORR)
                        (ash #b01010 24)
                        (ash 0 22)          ; shift=LSL
                        (ash 0 21)
                        (ash rm 16)
                        (ash 0 10)          ; imm6=0
                        (ash +a64-xzr+ 5)  ; Rn=XZR
                        rd)))

;;; --- Logical (shifted register) ---
;;; sf|opc(2)|01010|shift(2)|N|Rm(5)|imm6(6)|Rn(5)|Rd(5)
;;; opc: 00=AND, 01=ORR, 10=EOR, 11=ANDS

(defun a64-and-reg (buf rd rn rm)
  "AND Xd, Xn, Xm"
  (a64-emit buf (logior (ash 1 31)          ; sf=1
                        (ash #b00 29)       ; AND
                        (ash #b01010 24)
                        (ash 0 22)          ; shift=LSL
                        (ash 0 21)          ; N=0
                        (ash rm 16)
                        (ash 0 10)          ; imm6=0
                        (ash rn 5)
                        rd)))

(defun a64-orr-reg (buf rd rn rm)
  "ORR Xd, Xn, Xm"
  (a64-emit buf (logior (ash 1 31)
                        (ash #b01 29)       ; ORR
                        (ash #b01010 24)
                        (ash 0 22)
                        (ash 0 21)
                        (ash rm 16)
                        (ash 0 10)
                        (ash rn 5)
                        rd)))

(defun a64-eor-reg (buf rd rn rm)
  "EOR Xd, Xn, Xm"
  (a64-emit buf (logior (ash 1 31)
                        (ash #b10 29)       ; EOR
                        (ash #b01010 24)
                        (ash 0 22)
                        (ash 0 21)
                        (ash rm 16)
                        (ash 0 10)
                        (ash rn 5)
                        rd)))

(defun a64-ands-reg (buf rd rn rm)
  "ANDS Xd, Xn, Xm  (sets flags → TST when Rd=XZR)"
  (a64-emit buf (logior (ash 1 31)
                        (ash #b11 29)       ; ANDS
                        (ash #b01010 24)
                        (ash 0 22)
                        (ash 0 21)
                        (ash rm 16)
                        (ash 0 10)
                        (ash rn 5)
                        rd)))

(defun a64-tst-reg (buf rn rm)
  "TST Xn, Xm  →  ANDS XZR, Xn, Xm"
  (a64-ands-reg buf +a64-xzr+ rn rm))

;;; --- IEEE float (FP) instructions ---
;;; Double-precision (FP64).  D0-D31 are FP regs; same number space as
;;; X regs for register fields.  The float "object" layout matches x64
;;; (mvm/translate-x64.lisp +op-fadd+): a 2-slot heap object with subtag
;;; #x60, tagged pointer = raw+9; slot 0 = hi32 sign-extended<<1, slot 1
;;; = lo32 zero-extended<<1.  Splitting into two tagged fixnums avoids
;;; the low-bit collision with fixnum tag.

(defun a64-fadd-d (buf dd dn dm)
  "FADD Dd, Dn, Dm  (FP64 add)"
  (a64-emit buf (logior #x1E602800 (ash dm 16) (ash dn 5) dd)))

(defun a64-fsub-d (buf dd dn dm)
  "FSUB Dd, Dn, Dm  (FP64 sub)"
  (a64-emit buf (logior #x1E603800 (ash dm 16) (ash dn 5) dd)))

(defun a64-fmul-d (buf dd dn dm)
  "FMUL Dd, Dn, Dm  (FP64 mul)"
  (a64-emit buf (logior #x1E600800 (ash dm 16) (ash dn 5) dd)))

(defun a64-fdiv-d (buf dd dn dm)
  "FDIV Dd, Dn, Dm  (FP64 div)"
  (a64-emit buf (logior #x1E601800 (ash dm 16) (ash dn 5) dd)))

(defun a64-fcmp-d (buf dn dm)
  "FCMP Dn, Dm  (FP64 compare, sets NZCV)"
  (a64-emit buf (logior #x1E602000 (ash dm 16) (ash dn 5))))

(defun a64-scvtf-d-x (buf dd xn)
  "SCVTF Dd, Xn  (signed int64 → double)"
  (a64-emit buf (logior #x9E620000 (ash xn 5) dd)))

(defun a64-fcvtzs-x-d (buf xd dn)
  "FCVTZS Xd, Dn  (double → signed int64, truncate toward zero)"
  (a64-emit buf (logior #x9E780000 (ash dn 5) xd)))

(defun a64-fmov-d-x (buf dd xn)
  "FMOV Dd, Xn  (move raw int64 bits → FP reg, no conversion)"
  (a64-emit buf (logior #x9E670000 (ash xn 5) dd)))

(defun a64-fmov-x-d (buf xd dn)
  "FMOV Xd, Dn  (move raw FP reg bits → int64, no conversion)"
  (a64-emit buf (logior #x9E660000 (ash dn 5) xd)))

;;; --- Shift (register) ---
;;; Variable shifts: LSLV/LSRV/ASRV encoded as data-processing (2 source)
;;; sf|0|0|11010110|Rm|001000|Rn|Rd  = LSLV
;;; sf|0|0|11010110|Rm|001001|Rn|Rd  = LSRV
;;; sf|0|0|11010110|Rm|001010|Rn|Rd  = ASRV

(defun a64-lslv (buf rd rn rm)
  "LSL Xd, Xn, Xm  (variable shift left)"
  (a64-emit buf (logior (ash 1 31)
                        (ash #b0011010110 21)
                        (ash rm 16)
                        (ash #b001000 10)
                        (ash rn 5)
                        rd)))

(defun a64-lsrv (buf rd rn rm)
  "LSR Xd, Xn, Xm  (variable logical shift right)"
  (a64-emit buf (logior (ash 1 31)
                        (ash #b0011010110 21)
                        (ash rm 16)
                        (ash #b001001 10)
                        (ash rn 5)
                        rd)))

(defun a64-asrv (buf rd rn rm)
  "ASR Xd, Xn, Xm  (variable arithmetic shift right)"
  (a64-emit buf (logior (ash 1 31)
                        (ash #b0011010110 21)
                        (ash rm 16)
                        (ash #b001010 10)
                        (ash rn 5)
                        rd)))

;;; --- Bitfield: UBFM / SBFM ---
;;; sf|opc(2)|100110|N|immr(6)|imms(6)|Rn(5)|Rd(5)
;;; UBFM: opc=10, SBFM: opc=00

(defun a64-ubfm (buf rd rn immr imms)
  "UBFM Xd, Xn, #immr, #imms  (unsigned bitfield move)"
  (a64-emit buf (logior (ash 1 31)            ; sf=1
                        (ash #b10 29)         ; opc=10 (UBFM)
                        (ash #b100110 23)
                        (ash 1 22)            ; N=1 for 64-bit
                        (ash (logand immr #x3F) 16)
                        (ash (logand imms #x3F) 10)
                        (ash rn 5)
                        rd)))

(defun a64-lsr-imm (buf rd rn amount)
  "LSR Xd, Xn, #amount  →  UBFM Xd, Xn, #amount, #63"
  (a64-ubfm buf rd rn amount 63))

(defun a64-lsl-imm (buf rd rn amount)
  "LSL Xd, Xn, #amount  →  UBFM Xd, Xn, #(64-amount), #(63-amount)"
  (a64-ubfm buf rd rn (logand (- 64 amount) #x3F) (- 63 amount)))

(defun a64-sbfm (buf rd rn immr imms)
  "SBFM Xd, Xn, #immr, #imms  (signed bitfield move)"
  (a64-emit buf (logior (ash 1 31)            ; sf=1
                        (ash #b00 29)         ; opc=00 (SBFM)
                        (ash #b100110 23)
                        (ash 1 22)            ; N=1
                        (ash (logand immr #x3F) 16)
                        (ash (logand imms #x3F) 10)
                        (ash rn 5)
                        rd)))

(defun a64-asr-imm (buf rd rn amount)
  "ASR Xd, Xn, #amount  →  SBFM Xd, Xn, #amount, #63"
  (a64-sbfm buf rd rn amount 63))

;;; --- MUL / SDIV ---
;;; MUL: sf|00|11011|000|Rm|0|Ra(=11111)|Rn|Rd   (MADD with Ra=XZR)
;;; SDIV: sf|00|11010110|Rm|00001|1|Rn|Rd

(defun a64-mul (buf rd rn rm)
  "MUL Xd, Xn, Xm  →  MADD Xd, Xn, Xm, XZR"
  (a64-emit buf (logior (ash 1 31)            ; sf=1
                        (ash #b0011011000 21)
                        (ash rm 16)
                        (ash 0 15)            ; o0=0 (MADD)
                        (ash +a64-xzr+ 10)   ; Ra=XZR → MUL alias
                        (ash rn 5)
                        rd)))

(defun a64-umulh (buf rd rn rm)
  "UMULH Xd, Xn, Xm — high 64 bits of unsigned Xn*Xm.
   Encoding: 1|00|11011|1|10|Rm|0|Ra(=11111)|Rn|Rd"
  (a64-emit buf (logior (ash 1 31)            ; sf=1
                        (ash #b0011011110 21) ; UMULH
                        (ash rm 16)
                        (ash 0 15)            ; o0=0
                        (ash +a64-xzr+ 10)   ; Ra=XZR
                        (ash rn 5)
                        rd)))

(defun a64-smulh (buf rd rn rm)
  "SMULH Xd, Xn, Xm — high 64 bits of signed Xn*Xm.
   Encoding: 1|00|11011|0|10|Rm|0|Ra(=11111)|Rn|Rd (U=0 vs UMULH's U=1).
   Used by +op-mul-checked+'s overflow test: a signed 64x64 multiply
   overflowed iff SMULH(a,b) /= (MUL(a,b) ASR #63)."
  (a64-emit buf (logior (ash 1 31)            ; sf=1
                        (ash #b0011011010 21) ; SMULH
                        (ash rm 16)
                        (ash 0 15)            ; o0=0
                        (ash +a64-xzr+ 10)   ; Ra=XZR
                        (ash rn 5)
                        rd)))

(defun a64-adc (buf rd rn rm)
  "ADC Xd, Xn, Xm — add with carry (uses C flag from previous ADDS).
   Encoding: 1|00|11010000|Rm|000000|Rn|Rd"
  (a64-emit buf (logior (ash 1 31)            ; sf=1
                        (ash #b0011010000 21)
                        (ash rm 16)
                        (ash 0 10)
                        (ash rn 5)
                        rd)))

(defun a64-sdiv (buf rd rn rm)
  "SDIV Xd, Xn, Xm"
  (a64-emit buf (logior (ash 1 31)
                        (ash #b0011010110 21)
                        (ash rm 16)
                        (ash #b000011 10)
                        (ash rn 5)
                        rd)))

;;; --- NEG ---

(defun a64-neg (buf rd rm)
  "NEG Xd, Xm  →  SUB Xd, XZR, Xm"
  (a64-sub-reg buf rd +a64-xzr+ rm 0 0))

;;; --- Move wide (MOVZ / MOVK) ---
;;; sf|opc(2)|100101|hw(2)|imm16(16)|Rd(5)
;;; MOVZ: opc=10, MOVK: opc=11

(defun a64-movz (buf rd imm16 hw)
  "MOVZ Xd, #imm16{, LSL #(hw*16)}"
  (a64-emit buf (logior (ash 1 31)            ; sf=1
                        (ash #b10 29)         ; MOVZ
                        (ash #b100101 23)
                        (ash (logand hw 3) 21)
                        (ash (logand imm16 #xFFFF) 5)
                        rd)))

(defun a64-movk (buf rd imm16 hw)
  "MOVK Xd, #imm16{, LSL #(hw*16)}"
  (a64-emit buf (logior (ash 1 31)
                        (ash #b11 29)         ; MOVK
                        (ash #b100101 23)
                        (ash (logand hw 3) 21)
                        (ash (logand imm16 #xFFFF) 5)
                        rd)))

(defun a64-movn (buf rd imm16 hw)
  "MOVN Xd, #imm16{, LSL #(hw*16)}  (move wide NOT)"
  (a64-emit buf (logior (ash 1 31)
                        (ash #b00 29)         ; MOVN
                        (ash #b100101 23)
                        (ash (logand hw 3) 21)
                        (ash (logand imm16 #xFFFF) 5)
                        rd)))

(defun a64-load-imm64 (buf rd imm64)
  "Load a 64-bit immediate into Xd using minimal MOVZ/MOVK sequence."
  (let* ((val (logand imm64 #xFFFFFFFFFFFFFFFF))
         (hw0 (logand val #xFFFF))
         (hw1 (logand (ash val -16) #xFFFF))
         (hw2 (logand (ash val -32) #xFFFF))
         (hw3 (logand (ash val -48) #xFFFF))
         (chunks (list (cons 0 hw0) (cons 1 hw1) (cons 2 hw2) (cons 3 hw3)))
         (nonzero (remove-if (lambda (c) (zerop (cdr c))) chunks)))
    (cond
      ;; Zero
      ((zerop val)
       (a64-movz buf rd 0 0))
      ;; Single 16-bit chunk
      ((= (length nonzero) 1)
       (let ((c (first nonzero)))
         (a64-movz buf rd (cdr c) (car c))))
      ;; Check if all-ones complement is cheaper (MOVN)
      ((let* ((inv (logxor val #xFFFFFFFFFFFFFFFF))
              (ihw0 (logand inv #xFFFF))
              (ihw1 (logand (ash inv -16) #xFFFF))
              (ihw2 (logand (ash inv -32) #xFFFF))
              (ihw3 (logand (ash inv -48) #xFFFF))
              (ichunks (list (cons 0 ihw0) (cons 1 ihw1)
                             (cons 2 ihw2) (cons 3 ihw3)))
              (inv-nonzero (remove-if (lambda (c) (zerop (cdr c))) ichunks)))
         (when (= (length inv-nonzero) 1)
           (let ((c (first inv-nonzero)))
             (a64-movn buf rd (cdr c) (car c))
             t))))
      ;; General case: MOVZ first non-zero, then MOVK the rest
      (t
       (let ((first-chunk (first nonzero))
             (rest-chunks (rest nonzero)))
         (a64-movz buf rd (cdr first-chunk) (car first-chunk))
         (dolist (c rest-chunks)
           (a64-movk buf rd (cdr c) (car c))))))))

;;; --- Load/Store (unsigned offset) ---
;;; size(2)|111|V|01|opc(2)|imm12(12)|Rn(5)|Rt(5)
;;; LDR (64-bit): size=11, V=0, opc=01 → 0xF9400000
;;; STR (64-bit): size=11, V=0, opc=00 → 0xF9000000

(defun a64-ldr-unsigned (buf rt rn imm12)
  "LDR Xt, [Xn, #imm12]  (unsigned offset, scaled by 8)"
  ;; imm12 is byte offset / 8 for 64-bit loads
  (let ((scaled (ash imm12 -3)))
    (a64-emit buf (logior #xF9400000
                          (ash (logand scaled #xFFF) 10)
                          (ash rn 5)
                          rt))))

(defun a64-str-unsigned (buf rt rn imm12)
  "STR Xt, [Xn, #imm12]  (unsigned offset, scaled by 8)"
  (let ((scaled (ash imm12 -3)))
    (a64-emit buf (logior #xF9000000
                          (ash (logand scaled #xFFF) 10)
                          (ash rn 5)
                          rt))))

;;; --- Load/Store (unscaled immediate, LDUR/STUR) ---
;;; size(2)|111|V|00|opc(2)|0|imm9(9)|00|Rn(5)|Rt(5)
;;; LDUR (64-bit): 0xF8400000 | imm9<<12
;;; STUR (64-bit): 0xF8000000 | imm9<<12

(defun a64-ldur (buf rt rn simm9)
  "LDUR Xt, [Xn, #simm9]  (unscaled signed offset)"
  (a64-emit buf (logior #xF8400000
                        (ash (logand simm9 #x1FF) 12)
                        (ash rn 5)
                        rt)))

(defun a64-stur (buf rt rn simm9)
  "STUR Xt, [Xn, #simm9]  (unscaled signed offset)"
  (a64-emit buf (logior #xF8000000
                        (ash (logand simm9 #x1FF) 12)
                        (ash rn 5)
                        rt)))

;;; --- Load/Store with variable width ---

(defun a64-ldr-width (buf rt rn offset width)
  "Load from [Xn, #offset] with WIDTH (0=u8, 1=u16, 2=u32, 3=u64).
   Uses LDUR for the unscaled offset form."
  ;; LDURB: 00|111000|01|0|imm9|00|Rn|Rt  = #x38400000
  ;; LDURH: 01|111000|01|0|imm9|00|Rn|Rt  = #x78400000
  ;; LDUR W: 10|111000|01|0|imm9|00|Rn|Rt = #xB8400000
  ;; LDUR X: 11|111000|01|0|imm9|00|Rn|Rt = #xF8400000
  (let ((base (ecase width
                (0 #x38400000)    ; LDURB
                (1 #x78400000)    ; LDURH
                (2 #xB8400000)    ; LDUR W
                (3 #xF8400000)))) ; LDUR X
    (a64-emit buf (logior base
                          (ash (logand offset #x1FF) 12)
                          (ash rn 5)
                          rt))))

(defun a64-str-width (buf rt rn offset width)
  "Store to [Xn, #offset] with WIDTH (0=u8, 1=u16, 2=u32, 3=u64).
   Uses STUR for the unscaled offset form."
  (let ((base (ecase width
                (0 #x38000000)    ; STURB
                (1 #x78000000)    ; STURH
                (2 #xB8000000)    ; STUR W
                (3 #xF8000000)))) ; STUR X
    (a64-emit buf (logior base
                          (ash (logand offset #x1FF) 12)
                          (ash rn 5)
                          rt))))

;;; --- Load/Store Pair (STP/LDP) ---
;;; opc(2)|101|V|0|type(2)|L|imm7(7)|Rt2(5)|Rn(5)|Rt1(5)
;;; STP (64-bit, signed offset): opc=10, V=0, type=10, L=0 → 0xA9000000
;;; LDP (64-bit, signed offset): opc=10, V=0, type=10, L=1 → 0xA9400000
;;; STP (pre-index):  type=11 → 0xA9800000
;;; LDP (post-index): type=01 (L=1) → 0xA8C00000

(defun a64-stp-offset (buf rt1 rt2 rn simm7)
  "STP Xt1, Xt2, [Xn, #simm7]  (signed offset, scaled by 8)"
  (let ((scaled (ash simm7 -3)))
    (a64-emit buf (logior #xA9000000
                          (ash (logand scaled #x7F) 15)
                          (ash rt2 10)
                          (ash rn 5)
                          rt1))))

(defun a64-ldp-offset (buf rt1 rt2 rn simm7)
  "LDP Xt1, Xt2, [Xn, #simm7]  (signed offset, scaled by 8)"
  (let ((scaled (ash simm7 -3)))
    (a64-emit buf (logior #xA9400000
                          (ash (logand scaled #x7F) 15)
                          (ash rt2 10)
                          (ash rn 5)
                          rt1))))

(defun a64-stp-pre (buf rt1 rt2 rn simm7)
  "STP Xt1, Xt2, [Xn, #simm7]!  (pre-index)"
  (let ((scaled (ash simm7 -3)))
    (a64-emit buf (logior #xA9800000
                          (ash (logand scaled #x7F) 15)
                          (ash rt2 10)
                          (ash rn 5)
                          rt1))))

(defun a64-ldp-post (buf rt1 rt2 rn simm7)
  "LDP Xt1, Xt2, [Xn], #simm7  (post-index)"
  (let ((scaled (ash simm7 -3)))
    (a64-emit buf (logior #xA8C00000
                          (ash (logand scaled #x7F) 15)
                          (ash rt2 10)
                          (ash rn 5)
                          rt1))))

;;; --- Load/Store (pre-index / post-index, single register) ---
;;; Pre-index:  size|111|V|00|opc|0|imm9|11|Rn|Rt
;;; Post-index: size|111|V|00|opc|0|imm9|01|Rn|Rt

(defun a64-str-pre (buf rt rn simm9)
  "STR Xt, [Xn, #simm9]!  (pre-index, 64-bit)"
  (a64-emit buf (logior #xF8000C00
                        (ash (logand simm9 #x1FF) 12)
                        (ash rn 5)
                        rt)))

(defun a64-ldr-post (buf rt rn simm9)
  "LDR Xt, [Xn], #simm9  (post-index, 64-bit)"
  (a64-emit buf (logior #xF8400400
                        (ash (logand simm9 #x1FF) 12)
                        (ash rn 5)
                        rt)))

;;; --- Branch (unconditional) ---
;;; B:  0|00101|imm26(26)
;;; BL: 1|00101|imm26(26)

(defun a64-b (buf imm26)
  "B imm26  (unconditional branch, PC-relative)"
  (a64-emit buf (logior (ash #b000101 26)
                        (logand imm26 #x3FFFFFF))))

(defun a64-bl (buf imm26)
  "BL imm26  (branch with link, PC-relative)"
  (a64-emit buf (logior (ash #b100101 26)
                        (logand imm26 #x3FFFFFF))))

;;; --- Conditional branch ---
;;; 0101010|0|imm19(19)|0|cond(4)

(defun a64-bcond (buf cond imm19)
  "B.cond imm19  (conditional branch, PC-relative)"
  (a64-emit buf (logior (ash #b01010100 24)
                        (ash (logand imm19 #x7FFFF) 5)
                        (logand cond #xF))))

;;; --- Branch register ---
;;; 1101011|opc(4)|11111|000000|Rn(5)|00000
;;; RET: opc=0010, BR: opc=0000, BLR: opc=0001

(defun a64-ret (buf &optional (rn +a64-x30+))
  "RET {Xn}  (return, default x30)"
  (a64-emit buf (logior #xD65F0000
                        (ash rn 5))))

(defun a64-br (buf rn)
  "BR Xn  (branch to register)"
  (a64-emit buf (logior #xD61F0000
                        (ash rn 5))))

(defun a64-blr (buf rn)
  "BLR Xn  (branch with link to register)"
  (a64-emit buf (logior #xD63F0000
                        (ash rn 5))))

;;; --- System instructions ---

(defun a64-nop (buf)
  "NOP"
  (a64-emit buf #xD503201F))

(defun a64-wfi (buf)
  "WFI  (wait for interrupt)"
  (a64-emit buf #xD503207F))

(defun a64-wfe (buf)
  "WFE  (wait for event)"
  (a64-emit buf #xD503205F))

(defun a64-brk (buf imm16)
  "BRK #imm16  (software breakpoint)"
  (a64-emit buf (logior #xD4200000
                        (ash (logand imm16 #xFFFF) 5))))

(defun a64-hlt (buf imm16)
  "HLT #imm16  (halt / semihosting trap)"
  (a64-emit buf (logior #xD4400000
                        (ash (logand imm16 #xFFFF) 5))))

(defun a64-svc (buf imm16)
  "SVC #imm16  (supervisor call)"
  (a64-emit buf (logior #xD4000001
                        (ash (logand imm16 #xFFFF) 5))))

;;; --- Data Memory Barrier ---
;;; DMB: 11010101000000110011|CRm(4)|1|01|11111
;;; CRm: #xB = ISH (inner shareable), #xF = SY (full system)

(defun a64-dmb (buf option)
  "DMB option  (data memory barrier)"
  (a64-emit buf (logior #xD503301F
                        (ash #b101 5)
                        (ash (logand option #xF) 8))))

(defun a64-dsb (buf option)
  "DSB option  (data synchronization barrier)"
  (a64-emit buf (logior #xD503301F
                        (ash #b100 5)
                        (ash (logand option #xF) 8))))

(defun a64-isb (buf)
  "ISB  (instruction synchronization barrier)"
  (a64-emit buf #xD5033FDF))

;;; --- MSR/MRS (interrupt control) ---
;;; MSR DAIFSet, #imm4:  1101010100|0|00|011|0100|imm4|110|11111
;;; MSR DAIFClr, #imm4:  1101010100|0|00|011|0100|imm4|111|11111

(defun a64-msr-daifset (buf imm4)
  "MSR DAIFSet, #imm4  (mask interrupts: bit 1=F, bit 2=I, bit 3=A)"
  ;; Encoding: D503419F | (imm4 << 8) for DAIFSet
  (a64-emit buf (logior #xD5034000
                        (ash #b110 5)
                        (ash (logand imm4 #xF) 8)
                        #x1F)))

(defun a64-msr-daifclr (buf imm4)
  "MSR DAIFClr, #imm4  (unmask interrupts)"
  (a64-emit buf (logior #xD5034000
                        (ash #b111 5)
                        (ash (logand imm4 #xF) 8)
                        #x1F)))

;;; --- MRS/MSR (system register access) ---
;;; System register encoding: op0[1:0]|op1[2:0]|CRn[3:0]|CRm[3:0]|op2[2:0] = 16 bits
;;; This 16-bit value must be shifted left by 5 to align with instruction bits [20:5].
;;; Common encodings:
;;;   TPIDR_EL1 (S3_0_C13_C0_4) = #xC684
;;;   VBAR_EL1  (S3_0_C12_C0_0) = #xC600

(defun a64-mrs (buf rt sysreg-encoding)
  "MRS Xt, <sysreg>  (read system register)"
  ;; MRS: 1101010100|1|op0[1:0]|op1[2:0]|CRn[3:0]|CRm[3:0]|op2[2:0]|Rt[4:0]
  ;; Base #xD5300000 has L=1 (read), op0 MSB set (op0 >= 2)
  (a64-emit buf (logior #xD5300000
                        (ash sysreg-encoding 5)
                        rt)))

(defun a64-msr-sysreg (buf sysreg-encoding rt)
  "MSR <sysreg>, Xt  (write system register)"
  ;; MSR: 1101010100|0|op0[1:0]|op1[2:0]|CRn[3:0]|CRm[3:0]|op2[2:0]|Rt[4:0]
  ;; Base #xD5100000 has L=0 (write), op0 MSB set (op0 >= 2)
  (a64-emit buf (logior #xD5100000
                        (ash sysreg-encoding 5)
                        rt)))

(defconstant +sysreg-tpidr-el1+ #xC684 "TPIDR_EL1: S3_0_C13_C0_4")
(defconstant +sysreg-vbar-el1+  #xC600 "VBAR_EL1: S3_0_C12_C0_0")
(defconstant +sysreg-cntpct-el0+ #xDF01 "CNTPCT_EL0: S3_3_C14_C0_1")

;;; --- Atomic exchange (LDXR/STXR pair) ---
;;; LDXR Xt, [Xn]: size|001000|0|1|0|Rs(11111)|0|Rt2(11111)|Rn|Rt
;;; STXR Ws, Xt, [Xn]: size|001000|0|0|0|Rs|0|Rt2(11111)|Rn|Rt
;;; 64-bit: size=11

(defun a64-ldxr (buf rt rn)
  "LDXR Xt, [Xn]  (load exclusive register)"
  (a64-emit buf (logior #xC85F7C00
                        (ash rn 5)
                        rt)))

(defun a64-stxr (buf rs rt rn)
  "STXR Ws, Xt, [Xn]  (store exclusive register, Rs=status)"
  (a64-emit buf (logior #xC8007C00
                        (ash rs 16)
                        (ash rn 5)
                        rt)))

;;; --- CSET (conditional set) ---
;;; CSET Xd, cond  →  CSINC Xd, XZR, XZR, invert(cond)
;;; CSINC encoding: sf|op|S|11010100|Rm(5)|cond(4)|0|o2|Rn(5)|Rd(5)
;;; For CSINC: sf=1, op=0, S=0, o2=1
;;; Rm=XZR(31), Rn=XZR(31), cond is inverted for CSET alias

(defun a64-cset (buf rd cond)
  "CSET Xd, cond  →  CSINC Xd, XZR, XZR, invert(cond)"
  (let ((inv-cond (logxor cond 1)))
    (a64-emit buf (logior (ash 1 31)              ; sf=1
                          (ash 0 30)              ; op=0
                          (ash 0 29)              ; S=0
                          (ash #b11010100 21)     ; CSINC fixed bits
                          (ash +a64-xzr+ 16)     ; Rm = XZR
                          (ash inv-cond 12)       ; cond (inverted)
                          (ash 0 11)              ; bit 11 = 0
                          (ash 1 10)              ; o2=1 (CSINC)
                          (ash +a64-xzr+ 5)      ; Rn = XZR
                          rd))))

;;; ============================================================
;;; Spill Slot Helpers
;;; ============================================================
;;;
;;; Spilled virtual registers (V9-V15) live in the stack frame.
;;; The frame layout (set up in prologue):
;;;   [FP+8]  = saved LR
;;;   [FP]    = saved FP
;;;   [FP-8]  = spill slot 0 (V9)
;;;   [FP-16] = spill slot 1 (V10)
;;;   ... etc
;;;
;;; For 7 spill slots (V9-V15) we need 56 bytes.

(defconstant +a64-spill-base-offset+ -8
  "Offset from FP to the first spill slot.")

(defconstant +a64-frame-slot-base+ -64
  "FP-relative offset for frame slot 0 (local variables via obj-ref VFP).
   Frame slots grow downward: slot N is at FP + frame-slot-base + N*(-8).
   This is below all spill slots (which end at FP-56) to avoid overlap.")

(defconstant +a64-locals-frame-size+ 1024
  "Extra stack allocation below FP for spill slots (56 bytes) and
   frame slots. 1024 bytes provides ~120 frame slots for local variables,
   sufficient for deeply nested crypto functions like fe-mul (~80 slots).")

(defun a64-spill-slot-offset (vreg)
  "Compute the FP-relative offset for a spilled vreg (V9-V15).
   Returns a negative offset suitable for LDUR/STUR."
  (+ +a64-spill-base-offset+ (* (- vreg +vreg-v9+) -8)))

(defun a64-load-spill (buf phys-dest vreg)
  "Load a spilled virtual register from its frame slot into PHYS-DEST."
  (a64-ldur buf phys-dest +a64-x29+ (a64-spill-slot-offset vreg)))

(defun a64-store-spill (buf phys-src vreg)
  "Store PHYS-SRC into the frame spill slot for VREG."
  (a64-stur buf phys-src +a64-x29+ (a64-spill-slot-offset vreg)))

(defun a64-emit-load-vreg (buf phys-dest vreg)
  "Ensure VREG is in PHYS-DEST. If VREG maps to a physical register,
   emit MOV if needed. If it spills, emit a load from the frame."
  (let ((phys (a64-phys-reg vreg)))
    (cond
      (phys
       (unless (= phys phys-dest)
         (a64-mov-reg buf phys-dest phys)))
      ((and (>= vreg +vreg-v9+) (<= vreg +vreg-v15+))
       (a64-load-spill buf phys-dest vreg))
      (t
       (error "AArch64: cannot load virtual register ~D" vreg)))))

(defun a64-emit-store-vreg (buf phys-src vreg)
  "Store PHYS-SRC into VREG's location. If VREG maps to a physical
   register, emit MOV if needed. If it spills, store to frame."
  (let ((phys (a64-phys-reg vreg)))
    (cond
      (phys
       (unless (= phys phys-src)
         (a64-mov-reg buf phys phys-src)))
      ((and (>= vreg +vreg-v9+) (<= vreg +vreg-v15+))
       (a64-store-spill buf phys-src vreg))
      (t
       (error "AArch64: cannot store to virtual register ~D" vreg)))))

;;; ============================================================
;;; Prologue / Epilogue
;;; ============================================================

(defun a64-emit-prologue (buf)
  "Emit the standard function prologue:
     STP x29, x30, [sp, #-80]!
     MOV x29, sp
     STP x19, x20, [sp, #16]
     STP x21, x22, [sp, #32]
     STP x23, x27, [sp, #48]
     ;; x24/x25/x26 are global state (alloc/limit/nil) — NOT saved
     SUB sp, sp, #1024
   Frame: 80 bytes save area + 1024 bytes for spill slots and frame locals.
   Note: x24 (alloc ptr), x25 (alloc limit), x26 (nil) are global state
   shared across all functions. They must NOT be saved/restored, or
   allocations made by callees would be lost on return.
   x27 (CENV) is callee-saved per AAPCS and IS preserved — set-cenv
   in the closure-dispatch path overwrites it on the path TO a callee,
   but a caller that itself was called by something expects its own
   x27 to survive the inner call."
  ;; Save FP and LR, allocate save area
  (a64-stp-pre buf +a64-x29+ +a64-x30+ +a64-sp+ -80)
  ;; Set up frame pointer: ADD x29, SP, #0
  ;; (Cannot use a64-mov-reg because ORR encodes reg 31 as XZR, not SP)
  (a64-add-imm buf +a64-x29+ +a64-sp+ 0)
  ;; Save callee-saved registers (x19-x23, x27)
  ;; x24/x25/x26 are global alloc/limit/nil — must persist across calls
  (a64-stp-offset buf +a64-x19+ +a64-x20+ +a64-sp+ 16)
  (a64-stp-offset buf +a64-x21+ +a64-x22+ +a64-sp+ 32)
  ;; Save x23 paired with x27 (CENV).  Replaces the previous xzr slot.
  (a64-stp-offset buf +a64-x23+ +a64-x27+ +a64-sp+ 48)
  ;; Allocate space for spill slots and frame locals below FP
  (a64-sub-imm buf +a64-sp+ +a64-sp+ +a64-locals-frame-size+))

(defun a64-emit-epilogue (buf)
  "Emit the standard function epilogue:
     ADD sp, sp, #1024
     LDP x23, x27, [sp, #48]
     LDP x21, x22, [sp, #32]
     LDP x19, x20, [sp, #16]
     LDP x29, x30, [sp], #80
     RET
   Note: x24/x25/x26 are NOT restored (global state).  x27 (CENV) IS
   restored — see prologue docstring."
  ;; Deallocate spill/frame-slot area
  (a64-add-imm buf +a64-sp+ +a64-sp+ +a64-locals-frame-size+)
  ;; Restore callee-saved registers (x19-x23, x27)
  ;; x24/x25/x26 are global alloc/limit/nil — do NOT restore
  (a64-ldp-offset buf +a64-x23+ +a64-x27+ +a64-sp+ 48)
  (a64-ldp-offset buf +a64-x21+ +a64-x22+ +a64-sp+ 32)
  (a64-ldp-offset buf +a64-x19+ +a64-x20+ +a64-sp+ 16)
  ;; Restore FP/LR and deallocate save area
  (a64-ldp-post buf +a64-x29+ +a64-x30+ +a64-sp+ 80)
  (a64-ret buf))

;;; ============================================================
;;; MVM Bytecode Decoder Helpers
;;; ============================================================
;;;
;;; These walk through MVM bytecode and build a list of decoded
;;; instructions with their source byte positions, used for
;;; computing branch target mappings.

(defstruct decoded-mvm-insn
  offset      ; byte position in MVM bytecode
  opcode      ; numeric opcode
  operands    ; list of operand values
  size)       ; size in bytes

(defun decode-mvm-stream (bytes &key (start 0) (end nil))
  "Decode all MVM instructions from BYTES into a list of decoded-mvm-insn."
  (let ((limit (or end (length bytes)))
        (pos start)
        (insns nil))
    (loop while (< pos limit)
          do (let ((ipos pos))
               (let* ((decoded (decode-instruction bytes pos))
                      (opcode (car decoded))
                      (operands (cadr decoded))
                      (new-pos (cddr decoded)))
                 (push (make-decoded-mvm-insn
                        :offset ipos
                        :opcode opcode
                        :operands operands
                        :size (- new-pos ipos))
                       insns)
                 (setf pos new-pos))))
    (nreverse insns)))

(defun build-offset-to-index-map (insns)
  "Build a hash table mapping MVM byte offset → instruction index."
  (let ((map (make-hash-table :test 'eql)))
    (loop for insn in insns
          for idx from 0
          do (setf (gethash (decoded-mvm-insn-offset insn) map) idx))
    map))

;;; ============================================================
;;; MVM → AArch64 Instruction Translation
;;; ============================================================
;;;
;;; Each MVM opcode translates to a short sequence of AArch64
;;; instructions. The translator does a two-pass approach:
;;;   Pass 1: Translate all instructions, emit placeholder branches
;;;   Pass 2: Resolve branch fixups (MVM offsets → native offsets)

(defun a64-emit-generic-arith-call (buf va vb bc-offset)
  "Emit the +op-add-checked+ / +op-mul-checked+ overflow slow path:
   an inline call GENERIC-ADD/GENERIC-MULTIPLY(va, vb), result left in
   x16.  Mirrors x64's op-mul-checked slow path (translate-x64.lisp
   ~line 1260) with the aarch64 twist that x19-x23/x27 (V4-V8, CENV)
   are callee-saved by the callee's own prologue and spilled vregs
   (V9+) live in the FP frame — so only the caller-scratch arg regs
   x0-x3 (V0-V3) need saving here.  x30 (LR) is clobbered by the BLR,
   which is fine: the enclosing function's prologue saved LR and its
   epilogue restores from the stack (same as every +op-call+ BL).
   The call target is materialised via a fn-addr-patched MOVZ+MOVK
   (+tag-function+ OR'd in by apply-aarch64-fn-addr-patches; SUB-3
   strips it before BLR — same convention as the GC trampoline)."
  ;; Save x0-x3 (two STP pairs = 32 bytes, keeps SP 16-aligned).
  (a64-stp-pre    buf +a64-x0+ +a64-x1+ +a64-sp+ -32)
  (a64-stp-offset buf +a64-x2+ +a64-x3+ +a64-sp+ 16)
  ;; Stage BOTH args before writing x0/x1 — va/vb may themselves live
  ;; in x0-x3 (their values are still intact; STP doesn't clobber), and
  ;; e.g. va=V1/vb=V0 would collide if we loaded x0 first.
  (a64-emit-load-vreg buf +a64-x9+  va)
  (a64-emit-load-vreg buf +a64-x10+ vb)
  (a64-mov-reg buf +a64-x0+ +a64-x9+)
  (a64-mov-reg buf +a64-x1+ +a64-x10+)
  ;; NARGS = 2 at the fixed convention slot (32-bit store).
  (a64-load-imm64 buf +a64-x17+ #x10000150)
  (a64-movz buf +a64-x16+ 2 0)
  (a64-str-width buf +a64-x16+ +a64-x17+ 0 2)
  ;; Call via fn-addr-patched MOVZ+MOVK+BLR.
  (let ((movz-byte-pos
         (* (- (a64-current-index buf)
               (or *aarch64-translated-start-idx* 0))
            4)))
    (push (cons movz-byte-pos bc-offset) *aarch64-fn-addr-patches*))
  (a64-movz buf +a64-x16+ 0 0)              ; placeholder low 16
  (a64-movk buf +a64-x16+ 0 1)              ; placeholder high 16
  (a64-sub-imm buf +a64-x16+ +a64-x16+ 3)   ; strip +tag-function+
  (a64-blr buf +a64-x16+)
  ;; Result -> x16, then restore the arg regs.
  (a64-mov-reg buf +a64-x16+ +a64-x0+)
  (a64-ldp-offset buf +a64-x2+ +a64-x3+ +a64-sp+ 16)
  (a64-ldp-post   buf +a64-x0+ +a64-x1+ +a64-sp+ 32))

(defun translate-mvm-insn (insn buf mvm-to-native-label)
  "Translate a single decoded MVM instruction, emitting AArch64
   native code into BUF. MVM-TO-NATIVE-LABEL maps MVM byte offsets
   to native label IDs for branch targets."
  (let ((op (decoded-mvm-insn-opcode insn))
        (args (decoded-mvm-insn-operands insn)))
    (macrolet ((vr (n) `(nth ,n args))
               (phys (n) `(a64-resolve-reg (nth ,n args) +a64-x16+))
               (phys2 (n) `(a64-resolve-reg (nth ,n args) +a64-x17+)))
      (flet ((ensure-src (vreg scratch)
               "Load vreg into scratch if it spills, return the physical reg."
               (let ((p (a64-phys-reg vreg)))
                 (if p p
                     (progn (a64-emit-load-vreg buf scratch vreg) scratch))))
             (store-dst (phys-src vreg)
               "Store phys-src into vreg's location."
               (a64-emit-store-vreg buf phys-src vreg)))

        (cond
          ;; ---- NOP ----
          ((= op +op-nop+)
           (a64-nop buf))

          ;; ---- BREAK ----
          ((= op +op-break+)
           (a64-brk buf 0))

          ;; ---- TRAP ----
          ((= op +op-trap+)
           (let ((code (vr 0)))
             (cond
               ((< code #x0100)
                ;; Frame-enter: emit function prologue
                (a64-emit-prologue buf)
                ;; If > 4 params, copy overflow args from caller's stack
                ;; to local frame slots so stack-load can find them.
                ;;
                ;; Source stride matches caller's :push stride.  When
                ;; *aarch64-stack-align-16* is enabled (Linux EL0
                ;; SCTLR.SA0 demands 16-byte SP alignment at every
                ;; SP-relative load/store), :push reserves 16 bytes per
                ;; arg — the value at [SP+0], 8 bytes of padding above.
                ;; So overflow arg k lives at [FP + 80 + k*16].  In
                ;; bare-metal 8-byte-stack mode it lives at [FP + 80 + k*8].
                ;;
                ;; Mismatching these silently corrupts every >4-param
                ;; call's args 5+ (slot N reads either junk padding or
                ;; the NEXT arg's value).  Found by probing %alloc-mda
                ;; (7 args): slot 6 (data) consistently read symbol T
                ;; (slot 5's etype value) instead of the underlying
                ;; array — triggering SIGSEGV at every subsequent aref
                ;; on fill-pointer arrays.  See feedback_aa64_stride.
                (when (> code 4)
                  (let ((arg-stride (if *aarch64-stack-align-16* 16 8)))
                    (loop for i from 4 below code
                          for src-offset = (+ 80 (* (- i 4) arg-stride))
                          for dst-offset = (+ +a64-frame-slot-base+ (* i -8))
                          do (a64-ldur buf +a64-x16+ +a64-x29+ src-offset)
                             (a64-stur buf +a64-x16+ +a64-x29+ dst-offset)))))
               ((< code #x0300)
                ;; Frame-alloc/frame-free: NOP for now
                nil)
               ((= code #x0300)
                (cond
                  (*aarch64-linux-mode*
                   ;; Linux userspace: write 1 byte to fd=1 via write(2).
                   ;; V0 holds the tagged char code.  Save the char to a
                   ;; scratch byte slot (0x100001F0), then syscall.
                   ;; Result discarded.  Caller-saved regs x0/x1/x2 are
                   ;; clobbered — translator already treats them as
                   ;; scratch around any function call boundary, and
                   ;; trap 0x0300 is only emitted in tail position via
                   ;; compile-write-char-serial which then emits
                   ;; (li dest 0), so no live value needs preserving.
                   ;;
                   ;;   asr x16, x0, #1      ; x16 = untagged char
                   ;;   load x17, 0x100001F0
                   ;;   strb w16, [x17]      ; store byte
                   ;;   mov x0, #1            ; fd=1
                   ;;   mov x1, x17          ; buf addr
                   ;;   mov x2, #1           ; count
                   ;;   mov x8, #64          ; write
                   ;;   svc #0
                   (a64-asr-imm buf +a64-x16+ +a64-x0+ 1)
                   (a64-load-imm64 buf +a64-x17+ #x100001F0)
                   ;; strb w16, [x17] — encoding 0x39000000 | (0 << 10) | (17 << 5) | 16
                   (a64-emit buf (logior #x39000000 (ash 17 5) 16))
                   (a64-load-imm64 buf +a64-x0+ 1)
                   (a64-mov-reg buf +a64-x1+ +a64-x17+)
                   (a64-load-imm64 buf +a64-x2+ 1)
                   (a64-load-imm64 buf +a64-x8+ 64)
                   (a64-emit buf #xD4000001))     ; SVC #0
                  (t
                   ;; Bare-metal: write to UART data register.
                   (a64-asr-imm buf +a64-x16+ +a64-x0+ 1)
                   (a64-load-imm64 buf +a64-x17+ *aarch64-serial-base*)
                   (when *aarch64-serial-tx-poll*
                     (destructuring-bind (offset bit polarity) *aarch64-serial-tx-poll*
                       (a64-ldr-width buf 18 +a64-x17+ offset 2)
                       (let ((opcode (ecase polarity
                                       (:tbz  #b00110110)
                                       (:tbnz #b00110111))))
                         (a64-emit buf (logior (ash opcode 24)
                                               (ash bit 19)
                                               (ash (logand -1 #x3FFF) 5)
                                               18)))))
                   (a64-str-width buf +a64-x16+ +a64-x17+ 0 *aarch64-serial-width*))))
               ((= code #x0301)
                ;; Serial read: poll UART until a byte is available,
                ;; return tagged fixnum char code in x0.
                ;; PL011 UARTFR offset = 0x18, RXFE bit = bit 4
                ;; load UART base into x17
                (a64-load-imm64 buf +a64-x17+ *aarch64-serial-base*)
                ;; poll loop (2 instructions):
                ;;   ldrb w16, [x17, #0x18]   ; read UARTFR
                (a64-ldr-width buf +a64-x16+ +a64-x17+ #x18 0)
                ;;   tbnz x16, #4, -4         ; if RXFE set, branch back
                ;; TBNZ encoding: b5|011011|1|b40|imm14|Rt
                ;; b5=0, b40=00100 (bit 4), imm14=-1 (back 1 insn), Rt=x16
                (a64-emit buf (logior (ash #b00110111 24)  ; TBNZ
                                      (ash 4 19)           ; bit number = 4
                                      (ash (logand -1 #x3FFF) 5)  ; imm14 = -1
                                      +a64-x16+))
                ;; read data byte: ldrb w0, [x17, #0]
                (a64-ldr-width buf +a64-x0+ +a64-x17+ 0 0)
                ;; tag as fixnum: lsl x0, x0, #1
                (a64-lsl-imm buf +a64-x0+ +a64-x0+ 1))
               ((= code #x0302)
                ;; DSB SY barrier: force all previous memory accesses to complete
                ;; Required on AArch64 with MMU off (Normal Non-cacheable) to
                ;; prevent write buffer reordering between peripheral pages
                (a64-dsb buf #xF))  ;; SY = full system
               ((= code #x0303)
                ;; Jump to address: untag V0 (ASR 1), BR x0
                (a64-asr-imm buf +a64-x0+ +a64-x0+ 1)
                (a64-br buf +a64-x0+))
               ((= code #x0304)
                ;; WFI: Wait For Interrupt (sleep until next IRQ)
                (a64-emit buf #xD503207F))
               ((= code #x0310)
                ;; RDTSC equivalent: read CNTPCT_EL0 (physical timer counter) into X0 (VR)
                (a64-mrs buf +a64-x0+ +sysreg-cntpct-el0+))
               ((= code #x0320)
                ;; SETUP-IRQ: virtual timer init (always) + GICv2 (conditional)
                ;; Save x0/x16 — TRAP is inline, compiler doesn't know we clobber them
                (a64-str-pre buf +a64-x0+ +a64-sp+ -16)
                (a64-str-pre buf +a64-x16+ +a64-sp+ -16)
                ;; GICv2 only on QEMU virt (not RPi, not fixpoint)
                (when *aarch64-setup-irq-enable*
                  ;; GIC distributor enable: GICD_CTLR = 1
                  (a64-load-imm64 buf +a64-x16+ #x08000000)
                  (a64-load-imm64 buf +a64-x0+ 1)
                  (a64-str-width buf +a64-x0+ +a64-x16+ 0 2)
                  ;; Enable virtual timer PPI (INTID 27): GICD_ISENABLER0[27] = 1
                  ;; GICD_ISENABLER0 at GICD_base+0x100 — load full address (STUR max offset=255)
                  (a64-load-imm64 buf +a64-x16+ #x08000100)
                  (a64-load-imm64 buf +a64-x0+ #x08000000)  ; 1<<27
                  (a64-str-width buf +a64-x0+ +a64-x16+ 0 2)
                  ;; Enable E1000 PCI INTA SPI (INTID 35): GICD_ISENABLER1[3] = 1
                  ;; GICD_ISENABLER1 at GICD_base+0x104 — covers INTIDs 32-63
                  (a64-load-imm64 buf +a64-x16+ #x08000104)
                  (a64-load-imm64 buf +a64-x0+ 8)  ; 1<<3 = INTID 35
                  (a64-str-width buf +a64-x0+ +a64-x16+ 0 2)
                  ;; GIC CPU interface enable: GICC_CTLR = 1
                  (a64-load-imm64 buf +a64-x16+ #x08010000)
                  (a64-load-imm64 buf +a64-x0+ 1)
                  (a64-str-width buf +a64-x0+ +a64-x16+ 0 2)
                  ;; GICC_PMR = 0xFF (accept all priorities)
                  (a64-load-imm64 buf +a64-x0+ #xFF)
                  (a64-str-width buf +a64-x0+ +a64-x16+ 4 2))
                ;; Virtual timer: always init (works on virt, RPi, fixpoint)
                ;; CNTV_TVAL_EL0 = 62500 (1ms at 62.5MHz)
                (a64-load-imm64 buf +a64-x0+ 62500)
                (a64-emit buf #xD51BE300)  ; MSR CNTV_TVAL_EL0, x0
                ;; Enable timer: CNTV_CTL_EL0 = 1
                (a64-load-imm64 buf +a64-x0+ 1)
                (a64-emit buf #xD51BE320)  ; MSR CNTV_CTL_EL0, x0
                ;; NOTE: IRQs stay masked. WFI wakes on pending timer PPI
                ;; even with PSTATE.I=1. io-delay re-arms timer before WFI.
                ;; Restore x16/x0 (reverse order)
                (a64-ldr-post buf +a64-x16+ +a64-sp+ 16)
                (a64-ldr-post buf +a64-x0+ +a64-sp+ 16))
               ((= code #x0321)
                ;; TIMER-REARM: re-arm virtual timer (always emit on AArch64)
                (a64-load-imm64 buf +a64-x0+ 62500)
                (a64-emit buf #xD51BE300))  ; MSR CNTV_TVAL_EL0, x0
               ((= code #x0323)
                ;; UNMASK-IRQS: clear DAIF.{I,F} so IRQ and FIQ both fire.
                ;; Used by per-test deadline mechanism.  FIQ matters because
                ;; on QEMU virt the vtimer fires as Group 0 (which the GICv2
                ;; non-secure CPU interface routes as FIQ, vector entry 6),
                ;; not as IRQ (vector entry 5).  Unmasking I alone leaves
                ;; FIQ masked and the vtimer goes silent.
                ;; MSR DAIFClr, #3 — encoding 0xD50343FF (imm=3 = I+F).
                (a64-emit buf #xD50343FF))
               ((= code #x0400)
                ;; switch-idle-stack: set SP to per-CPU idle-stack-top
                (a64-mrs buf +a64-x16+ +sysreg-tpidr-el1+)
                (a64-ldr-unsigned buf +a64-x16+ +a64-x16+ #x38)
                (a64-add-imm buf +a64-sp+ +a64-x16+ 0))
               ((and *aarch64-linux-mode* (= code #x0500))
                ;; Linux sys-exit: V0 (x0) = exit status.  exit_group(2) = 94.
                ;; mov x8, #94; svc #0.
                (a64-load-imm64 buf +a64-x8+ 94)
                (a64-emit buf #xD4000001))      ; SVC #0
               ((= code #x0502)
                ;; Generic 3-arg Linux syscall — AArch64 ABI.
                ;;   Inputs: V0=x0=syscall#(tagged), V1=x1=arg1(tagged),
                ;;           V2=x2=arg2(tagged), V3=x3=arg3(tagged).
                ;;   Output: V0=x0=result(tagged).
                ;; Linux AArch64 syscall ABI:
                ;;   x8 = syscall number (raw)
                ;;   x0..x5 = args (raw)
                ;;   SVC #0; result in x0 (raw).
                ;;
                ;; cl-fileio.lisp uses x86-64 syscall numbers — remap
                ;; to AArch64 generic ABI in Linux mode.  For numerical
                ;; remaps the dispatch is a cmp/csel chain.  For the
                ;; few syscalls whose AArch64 equivalent takes a
                ;; different arg shape (open → openat needs AT_FDCWD
                ;; prepended; stat → newfstatat similarly; unlink/mkdir/
                ;; rename → ...at), they're handled via a `%linux-*`
                ;; helper defun in the build script rather than here —
                ;; the trap just does straight numerical remap.
                ;;
                ;; Untag args into x9/x10 first to avoid clobbering
                ;; x0/x1 mid-shuffle.
                (a64-asr-imm buf +a64-x10+ +a64-x3+ 1)  ; x10 = arg3 untag
                (a64-asr-imm buf +a64-x9+  +a64-x2+ 1)  ; x9  = arg2 untag
                (a64-asr-imm buf +a64-x8+  +a64-x0+ 1)  ; x8  = syscall# untag
                (a64-asr-imm buf +a64-x0+  +a64-x1+ 1)  ; x0  = arg1 untag
                (a64-mov-reg buf +a64-x1+ +a64-x9+)     ; x1  = arg2
                (a64-mov-reg buf +a64-x2+ +a64-x10+)    ; x2  = arg3
                ;; AArch64 Linux: remap x86-64 syscall numbers to AArch64
                ;; generic ABI numbers via a sequence of cmp/csel.  Skip
                ;; the remap path on bare-metal (no Linux syscalls).
                (when *aarch64-linux-mode*
                  ;; Remap table for one-to-one cases (same arg shape).
                  ;; Format: ((x64-num . aarch64-num) ...).
                  (dolist (pair '(( 0 . 63)    ; read
                                  ( 1 . 64)    ; write
                                  ( 3 . 57)    ; close
                                  ( 5 . 80)    ; fstat
                                  ( 8 . 62)    ; lseek
                                  ( 9 . 222)   ; mmap
                                  (39 . 172)   ; getpid
                                  (60 . 93)    ; exit
                                  (93 . 93)    ; exit_group (idempotent)
                                  (217 . 61))) ; getdents64
                    ;; CMP x8, #x64-num; CSEL x8, #aarch64-num, x8, EQ
                    (let ((from (car pair))
                          (to   (cdr pair)))
                      (a64-cmp-imm buf +a64-x8+ from)
                      ;; Load target syscall # into x11.
                      (a64-movz buf +a64-x11+ (logand to #xFFFF) 0)
                      (when (> to #xFFFF)
                        (a64-movk buf +a64-x11+ (logand (ash to -16) #xFFFF) 1))
                      ;; CSEL x8, x11, x8, EQ — if x8 was `from`, x8 = `to`.
                      (a64-emit buf (logior #x9A800000
                                            (ash +a64-x8+ 16)
                                            (ash +cc-eq+ 12)
                                            (ash +a64-x11+ 5)
                                            +a64-x8+)))))
                (a64-svc buf 0)                       ; SVC #0
                (a64-lsl-imm buf +a64-x0+ +a64-x0+ 1)) ; tag result
               ((= code #x0503)
                ;; Raw 3-arg Linux syscall — args passed untagged as-is.
                ;;   V0=x0=syscall#(tagged), V1-V3 = raw args.
                ;;   Output: V0=x0=result(raw, untagged).
                (a64-asr-imm buf +a64-x8+ +a64-x0+ 1)  ; x8 = syscall# untag
                (a64-mov-reg buf +a64-x0+ +a64-x1+)    ; x0 = arg1 raw
                (a64-mov-reg buf +a64-x1+ +a64-x2+)    ; x1 = arg2 raw
                (a64-mov-reg buf +a64-x2+ +a64-x3+)    ; x2 = arg3 raw
                (a64-svc buf 0))
               ((and *aarch64-linux-mode* (= code #x0505))
                ;; LINUX-ALARM: AArch64 setitimer(ITIMER_REAL, &new, NULL).
                ;; V0 = duration in seconds (tagged; 0 = clear).
                ;; AArch64 generic ABI removed alarm(2); use setitimer (103).
                ;; struct itimerval is 32 bytes:
                ;;   it_interval.tv_sec  @ 0  = 0
                ;;   it_interval.tv_usec @ 8  = 0
                ;;   it_value.tv_sec     @ 16 = duration
                ;;   it_value.tv_usec    @ 24 = 0
                ;; We stage it at fixed BSS slot 0x10000280..0x100002A0.
                ;; Default SIGALRM action is process termination, which is
                ;; what we want for fork-file's per-file timeout — the
                ;; child gets killed, parent wait4 returns, we move on.
                (a64-asr-imm buf +a64-x10+ +a64-x0+ 1)  ; x10 = seconds untag
                (a64-load-imm64 buf +a64-x9+ #x10000280)
                (a64-str-unsigned buf +a64-xzr+ +a64-x9+ 0)   ; it_interval.tv_sec
                (a64-str-unsigned buf +a64-xzr+ +a64-x9+ 8)   ; it_interval.tv_usec
                (a64-str-unsigned buf +a64-x10+ +a64-x9+ 16)  ; it_value.tv_sec = N
                (a64-str-unsigned buf +a64-xzr+ +a64-x9+ 24)  ; it_value.tv_usec
                (a64-mov-reg buf +a64-x1+ +a64-x9+)     ; new = &itimerval
                (a64-movz buf +a64-x0+ 0 0)             ; which = ITIMER_REAL
                (a64-movz buf +a64-x2+ 0 0)             ; old = NULL
                (a64-movz buf +a64-x8+ 103 0)
                (a64-svc buf 0)
                (a64-lsl-imm buf +a64-x0+ +a64-x0+ 1))
               ((and *aarch64-linux-mode* (= code #x0504))
                ;; LINUX-MMAP-SHARED-PAGE: anonymous MAP_SHARED mmap.
                ;; V0(x0) = size (tagged); result = mmap address (tagged).
                ;; Equivalent to mmap(NULL, size, PROT_RW,
                ;;   MAP_SHARED|MAP_ANONYMOUS, -1, 0) — 6-arg syscall.
                ;; AArch64 mmap syscall # = 222.
                (a64-asr-imm buf +a64-x1+ +a64-x0+ 1)   ; x1 = size
                (a64-movz buf +a64-x0+ 0 0)             ; x0 = NULL
                (a64-movz buf +a64-x2+ 3 0)             ; x2 = PROT_RW
                (a64-movz buf +a64-x3+ #x21 0)          ; x3 = MAP_SHARED|MAP_ANON
                ;; x4 = -1 (fd) via MOVN
                (a64-emit buf (logior #x92800000 (ash 0 5) +a64-x4+))
                (a64-movz buf +a64-x5+ 0 0)             ; x5 = offset
                (a64-movz buf +a64-x8+ 222 0)
                (a64-svc buf 0)
                (a64-lsl-imm buf +a64-x0+ +a64-x0+ 1))  ; tag result
               ((and *aarch64-linux-mode* (= code #x0506))
                ;; LINUX-OPENAT: AArch64 openat(AT_FDCWD, path, flags, mode).
                ;;   Inputs: V1=path-addr(tagged), V2=flags(tagged), V3=mode(tagged).
                ;;   Output: V0 = fd or -errno (tagged).
                ;; AArch64 removed the legacy `open(2)` from the generic
                ;; ABI; the replacement is `openat(2)` (syscall 56) with
                ;; an extra `dirfd` arg (-100 = AT_FDCWD → relative to CWD).
                ;; All five existing cl-fileio.lisp open paths funnel
                ;; through `(syscall3 2 path flags mode)` — they instead
                ;; emit trap 0x0506 on Linux/AArch64 builds (see
                ;; cl-fileio override in build script).
                ;;   x0 = -100 (AT_FDCWD)
                ;;   x1 = untagged path-addr (V1 >> 1)
                ;;   x2 = untagged flags     (V2 >> 1)
                ;;   x3 = untagged mode      (V3 >> 1)
                ;;   x8 = 56; SVC #0; LSL x0 by 1 to re-tag.
                (a64-asr-imm buf +a64-x10+ +a64-x3+ 1)  ; x10 = mode
                (a64-asr-imm buf +a64-x9+  +a64-x2+ 1)  ; x9  = flags
                (a64-asr-imm buf +a64-x1+  +a64-x1+ 1)  ; x1  = path
                ;; x0 = -100 (AT_FDCWD) — MOVN to load negative 16-bit.
                ;; MOVN Xd, #imm16 = 0x92800000 | (imm16<<5) | Rd.
                ;; -100 = ~99 → MOVN x0, #99.
                (a64-emit buf (logior #x92800000 (ash 99 5) +a64-x0+))
                (a64-mov-reg buf +a64-x2+ +a64-x9+)
                (a64-mov-reg buf +a64-x3+ +a64-x10+)
                (a64-movz buf +a64-x8+ 56 0)
                (a64-svc buf 0)
                (a64-lsl-imm buf +a64-x0+ +a64-x0+ 1))
               ((and *aarch64-linux-mode* (= code #x0507))
                ;; LINUX-UNLINKAT: AArch64 unlinkat(AT_FDCWD, path, 0).
                ;; Caller: V1 = path-addr (tagged), V2/V3 ignored.
                (a64-asr-imm buf +a64-x1+ +a64-x1+ 1)   ; x1 = path
                (a64-emit buf (logior #x92800000 (ash 99 5) +a64-x0+))  ; x0 = -100
                (a64-movz buf +a64-x2+ 0 0)             ; x2 = 0 (flags)
                (a64-movz buf +a64-x8+ 35 0)            ; unlinkat
                (a64-svc buf 0)
                (a64-lsl-imm buf +a64-x0+ +a64-x0+ 1))
               ((and *aarch64-linux-mode* (= code #x0508))
                ;; LINUX-NEWFSTATAT: stat(path, buf) via newfstatat(AT_FDCWD,
                ;;   path, buf, AT_NO_AUTOMOUNT=0).  V1=path, V2=buf, V3 ignored.
                (a64-asr-imm buf +a64-x9+ +a64-x2+ 1)   ; x9 = buf
                (a64-asr-imm buf +a64-x1+ +a64-x1+ 1)   ; x1 = path
                (a64-emit buf (logior #x92800000 (ash 99 5) +a64-x0+))  ; x0 = -100
                (a64-mov-reg buf +a64-x2+ +a64-x9+)     ; x2 = buf
                (a64-movz buf +a64-x3+ 0 0)             ; x3 = 0 (flags)
                (a64-movz buf +a64-x8+ 79 0)            ; newfstatat
                (a64-svc buf 0)
                (a64-lsl-imm buf +a64-x0+ +a64-x0+ 1))
               ((and *aarch64-linux-mode* (= code #x0509))
                ;; LINUX-MKDIRAT: mkdirat(AT_FDCWD, path, mode).  V1=path, V2=mode.
                (a64-asr-imm buf +a64-x9+ +a64-x2+ 1)   ; x9 = mode
                (a64-asr-imm buf +a64-x1+ +a64-x1+ 1)   ; x1 = path
                (a64-emit buf (logior #x92800000 (ash 99 5) +a64-x0+))  ; x0 = -100
                (a64-mov-reg buf +a64-x2+ +a64-x9+)
                (a64-movz buf +a64-x8+ 34 0)            ; mkdirat
                (a64-svc buf 0)
                (a64-lsl-imm buf +a64-x0+ +a64-x0+ 1))
               ((and *aarch64-linux-mode* (= code #x050A))
                ;; LINUX-RENAMEAT: renameat(AT_FDCWD, old, AT_FDCWD, new).
                ;; V1=old, V2=new.
                (a64-asr-imm buf +a64-x9+ +a64-x2+ 1)   ; x9 = new
                (a64-asr-imm buf +a64-x1+ +a64-x1+ 1)   ; x1 = old
                (a64-emit buf (logior #x92800000 (ash 99 5) +a64-x0+))  ; x0 = AT_FDCWD
                (a64-emit buf (logior #x92800000 (ash 99 5) +a64-x2+))  ; x2 = AT_FDCWD
                (a64-mov-reg buf +a64-x3+ +a64-x9+)
                (a64-movz buf +a64-x8+ 38 0)            ; renameat
                (a64-svc buf 0)
                (a64-lsl-imm buf +a64-x0+ +a64-x0+ 1))
               ((and *aarch64-linux-mode* (= code #x0531))
                ;; %MMAP-EXEC-PAGE (WS4 JIT exec-memory primitive; arch-neutral
                ;; trap — x64 uses the same #x0531).  mmap(NULL, size,
                ;; PROT_RWX=7, MAP_PRIVATE|MAP_ANON=0x22, -1, 0).
                ;; V0(x0)=size(tagged); result = address (tagged).  mmap = 222.
                ;; Same shape as #x0504 (mmap-shared) but PROT_RWX + PRIVATE.
                (a64-asr-imm buf +a64-x1+ +a64-x0+ 1)   ; x1 = size
                (a64-movz buf +a64-x0+ 0 0)             ; x0 = NULL
                (a64-movz buf +a64-x2+ 7 0)             ; x2 = PROT_READ|WRITE|EXEC
                (a64-movz buf +a64-x3+ #x22 0)          ; x3 = MAP_PRIVATE|MAP_ANON
                (a64-emit buf (logior #x92800000 (ash 0 5) +a64-x4+)) ; x4 = -1 (fd)
                (a64-movz buf +a64-x5+ 0 0)             ; x5 = offset
                (a64-movz buf +a64-x8+ 222 0)
                (a64-svc buf 0)
                (a64-lsl-imm buf +a64-x0+ +a64-x0+ 1))  ; tag result
               ((= code #x0532)
                ;; %JIT-CALL (arch-neutral trap; x64 uses the same #x0532).
                ;; V0(x0) = entry byte-address as a TAGGED fixnum (word =
                ;; addr<<1).  Untag (ASR 1), then BLR it.  BLR clobbers x30
                ;; (our own return address), so save/restore the caller's LR
                ;; across the call via a pre/post-indexed stack slot.  The
                ;; JIT'd thunk sets up its own AAPCS frame and returns its
                ;; value in x0 — which IS VR on AArch64 (vreg-vr → x0), so no
                ;; extra move is needed.
                (a64-asr-imm buf +a64-x0+ +a64-x0+ 1)     ; x0 = raw entry addr
                (a64-str-pre buf +a64-x30+ +a64-sp+ -16)  ; STR x30,[sp,#-16]!
                (a64-blr buf +a64-x0+)                     ; call (result → x0)
                (a64-ldr-post buf +a64-x30+ +a64-sp+ 16)) ; LDR x30,[sp],#16
               ((= code #x0533)
                ;; %JIT-ICACHE-FLUSH base len (arch-neutral trap; NO-OP on x64).
                ;; After writing native bytes into a %mmap-exec-page region,
                ;; make them fetchable: clean the D-cache to PoU then invalidate
                ;; the I-cache over [base, base+len).  x86-64 needs none of this
                ;; (coherent I-fetch); AArch64 does, or the core executes stale
                ;; I-cache — a SILENT failure (works under qemu-user, crashes on
                ;; real Cortex-A hardware).  Stride 16 = the ARMv8 minimum cache
                ;; line; a stride no larger than the true line size is always
                ;; safe (redundant maintenance is harmless), so this is correct
                ;; on any implementation without reading CTR_EL0.
                ;; V0(x0)=base(tagged, page-aligned), V1(x1)=len(tagged).
                (a64-asr-imm buf +a64-x9+ +a64-x0+ 1)     ; x9  = base (raw)
                (a64-asr-imm buf +a64-x10+ +a64-x1+ 1)    ; x10 = len
                (a64-add-reg buf +a64-x10+ +a64-x9+ +a64-x10+ 0 0) ; x10 = end
                ;; D-cache clean to PoU
                (a64-mov-reg buf +a64-x14+ +a64-x9+)      ; cursor = base
                (let ((d-top (a64-current-index buf)))
                  (a64-emit buf (logior #xD50B7B20 +a64-x14+)) ; DC CVAU, x14
                  (a64-add-imm buf +a64-x14+ +a64-x14+ 16)
                  (a64-cmp-reg buf +a64-x14+ +a64-x10+)
                  (a64-bcond buf +cc-cc+ (- d-top (a64-current-index buf)))) ; B.LO
                (a64-dsb buf #xB)                          ; DSB ISH
                ;; I-cache invalidate to PoU
                (a64-mov-reg buf +a64-x14+ +a64-x9+)      ; cursor = base
                (let ((i-top (a64-current-index buf)))
                  (a64-emit buf (logior #xD50B7520 +a64-x14+)) ; IC IVAU, x14
                  (a64-add-imm buf +a64-x14+ +a64-x14+ 16)
                  (a64-cmp-reg buf +a64-x14+ +a64-x10+)
                  (a64-bcond buf +cc-cc+ (- i-top (a64-current-index buf)))) ; B.LO
                (a64-dsb buf #xB)                          ; DSB ISH
                (a64-isb buf))                             ; ISB
               ((= code #x0534)
                ;; %JIT-FREE-PAGE base len — munmap(addr, len).  aarch64=215.
                ;; Reclaims a transient JIT exec page (x64 = no-op); reclaim is
                ;; runtime-gated to *jit-reclaim-on* (currently off; flip round).
                (a64-asr-imm buf +a64-x0+ +a64-x0+ 1)     ; x0 = addr
                (a64-asr-imm buf +a64-x1+ +a64-x1+ 1)     ; x1 = len
                (a64-movz buf +a64-x8+ 215 0)             ; munmap
                (a64-svc buf 0)
                (a64-lsl-imm buf +a64-x0+ +a64-x0+ 1))    ; tag result
               ((= code #x0510)
                ;; SETJMP: Save SP, FP (X29), return-IP to 0x10000180/188/190.
                ;; First call: return NIL (=X26=0) in X0.  On longjmp:
                ;; execution resumes here with X0 = T (#xDEAD1009).
                ;;
                ;; RAW-ADDR-AUDIT: writes RAW SP, RAW FP, RAW IP into the
                ;; three handler-state slots (8 bytes each).  These slots
                ;; are read RAW by the sync-exception handler at entry
                ;; offset 0x200 (boot-aarch64.lisp::emit-aarch64-exception-vectors
                ;; entry 4) and by the IRQ deadline handler at entry
                ;; offset 0x280 — both restore SP from slot 180 and BR
                ;; to slot 190.  Nothing reads these via mem-ref :u64,
                ;; so NO SHL convention applies (unlike the GC metadata
                ;; at 0x10000068+).  Treat slots 180/188/190 + the
                ;; per-fork handler-stack frames as raw-only.
                ;; Mirrors x64 #x0510 (translate-x64.lisp:584).  Saved IP
                ;; is the byte AFTER the trap block — both first-call and
                ;; longjmp-call land there.  No skip-branch needed.
                ;;
                ;; Phase 3(b): if the per-fork handler-stack push helper
                ;; is registered, BL it FIRST so the OUTER 180/188/190
                ;; triple gets saved before this SETJMP overwrites it.
                ;; BL clobbers x30, so we save/restore the caller's LR
                ;; via scratch slot 0x10000FF0 (unused elsewhere).
                ;; CLEAR-HANDLER and LONGJMP (later steps) will pop the
                ;; outer triple back into 180/188/190.
                (when *aarch64-handler-push-label*
                  ;; save caller x30 to 0x10000FF0
                  (a64-movz buf +a64-x16+ #xFFF0 0)
                  (a64-movk buf +a64-x16+ #x1000 1)
                  (a64-str-unsigned buf +a64-x30+ +a64-x16+ 0)
                  ;; BL handler_push (fixup resolved at end of unified emit)
                  (let ((idx (a64-current-index buf)))
                    (a64-bl buf 0)
                    (a64-add-fixup buf idx *aarch64-handler-push-label* :bl))
                  ;; restore caller x30
                  (a64-movz buf +a64-x16+ #xFFF0 0)
                  (a64-movk buf +a64-x16+ #x1000 1)
                  (a64-ldr-unsigned buf +a64-x30+ +a64-x16+ 0))
                (a64-load-imm64 buf +a64-x16+ #x10000180)
                (a64-add-imm buf +a64-x17+ +a64-sp+ 0)        ; mov x17, sp
                (a64-str-unsigned buf +a64-x17+ +a64-x16+ 0)
                (a64-str-unsigned buf +a64-x29+ +a64-x16+ 8)
                ;; ADR x17, <after-mov-x0> — placeholder, patched below.
                (let ((adr-idx (a64-current-index buf)))
                  (a64-emit buf 0)  ; placeholder for ADR x17
                  (a64-str-unsigned buf +a64-x17+ +a64-x16+ 16)
                  (a64-mov-reg buf +a64-x0+ +a64-x26+)         ; first-time return = NIL
                  ;; AFTER this point execution falls through.  ADR target is here.
                  (let ((return-idx (a64-current-index buf)))
                    (let* ((byte-off (* (- return-idx adr-idx) 4))
                           (immlo (logand byte-off 3))
                           (immhi (logand (ash byte-off -2) #x7FFFF)))
                      (setf (aref (a64-buffer-code buf) adr-idx)
                            (logior (ash immlo 29)
                                    (ash #b10000 24)
                                    (ash immhi 5)
                                    17))))))
               ((= code #x0511)
                ;; LONGJMP: Restore SP, FP, IP from 0x10000180.
                ;; Set X0 = T (#xDEAD1009). BR to saved IP.
                ;;
                ;; RAW-ADDR-AUDIT: BR x16/x17 below jumps to a raw
                ;; native address read from slot 0x10000180+16.  That
                ;; slot is written by SETJMP (trap #x0510) with the
                ;; resume-PC of the active handler-case.  If LONGJMP
                ;; ever fires with no handler-case armed (slot=0), we
                ;; BR to 0 and wedge.  In normal use the sync-exception
                ;; handler at entry-4 takes that "no handler" path
                ;; *before* SVC #x0511 ever executes, so the trap body
                ;; assumes a handler is armed — but a stale call-ind to
                ;; this trap from outside a handler-case would skip the
                ;; pre-check.  TODO: prepend a CBZ x16, halt to be safe.
                ;;
                ;; Phase 3(d): if the per-fork pop helper is wired up,
                ;; we copy the inner 180/188/190 triple to scratch
                ;; slots (mirrors x64 #x10000C10/18/20), BL pop so
                ;; that 180/188/190 now hold the OUTER handler-case,
                ;; then restore SP/FP/IP from scratch.  The outer
                ;; becomes active for any future sync exception that
                ;; fires AFTER we BR back into the body.  When depth
                ;; is zero the pop helper writes zeros — same as the
                ;; pre-Phase-3 LONGJMP-then-CLEAR semantics.
                (cond
                  (*aarch64-handler-pop-label*
                   ;; Read current 180/188/190 → scratch 0xC10/C18/C20.
                   (a64-load-imm64 buf +a64-x16+ #x10000180)
                   (a64-load-imm64 buf +a64-x17+ #x10000C10)
                   (a64-ldr-unsigned buf +a64-x18+ +a64-x16+ 0)
                   (a64-str-unsigned buf +a64-x18+ +a64-x17+ 0)
                   (a64-ldr-unsigned buf +a64-x18+ +a64-x16+ 8)
                   (a64-str-unsigned buf +a64-x18+ +a64-x17+ 8)
                   (a64-ldr-unsigned buf +a64-x18+ +a64-x16+ 16)
                   (a64-str-unsigned buf +a64-x18+ +a64-x17+ 16)
                   ;; Pop the handler stack — overwrites 180/188/190
                   ;; with outer (or zeros if depth==0).  No need to
                   ;; preserve x30 here: we BR to scratch-IP at the end
                   ;; rather than returning.
                   (let ((idx (a64-current-index buf)))
                     (a64-bl buf 0)
                     (a64-add-fixup buf idx *aarch64-handler-pop-label* :bl))
                   ;; Restore inner SP/FP/IP from scratch.
                   (a64-load-imm64 buf +a64-x17+ #x10000C10)
                   (a64-ldr-unsigned buf +a64-x16+ +a64-x17+ 0)
                   (a64-add-imm buf +a64-sp+ +a64-x16+ 0)
                   (a64-ldr-unsigned buf +a64-x29+ +a64-x17+ 8)
                   (a64-ldr-unsigned buf +a64-x16+ +a64-x17+ 16)
                   (a64-load-imm64 buf +a64-x0+ #xDEAD1009)
                   (a64-br buf +a64-x16+))
                  (t
                   (a64-load-imm64 buf +a64-x16+ #x10000180)
                   (a64-ldr-unsigned buf +a64-x17+ +a64-x16+ 0)  ; saved SP
                   (a64-add-imm buf +a64-sp+ +a64-x17+ 0)
                   (a64-ldr-unsigned buf +a64-x29+ +a64-x16+ 8)  ; FP
                   (a64-ldr-unsigned buf +a64-x17+ +a64-x16+ 16) ; IP
                   (a64-load-imm64 buf +a64-x0+ #xDEAD1009)      ; T
                   (a64-br buf +a64-x17+))))
               ((= code #x0512)
                ;; CLEAR-HANDLER: Phase 3(c) — BL the per-fork handler
                ;; stack POP helper if it's wired up.  The helper either
                ;; reloads slot 0x10000180/188/190 from frame[depth-1]
                ;; (uncovering the outer handler-case) OR — when depth
                ;; is zero — writes zeros to slot 180/188/190 so the
                ;; sync-exception handler still sees "no handler"
                ;; (legacy semantics).  Preserves X0 (the handler-case
                ;; body's return value); see emit-aarch64-handler-helpers
                ;; — only x9..x13 are touched.
                ;;
                ;; Fall back to the simple STR XZR if helpers aren't
                ;; registered (non-unified caller or pre-Phase-3 build).
                (cond
                  (*aarch64-handler-pop-label*
                   ;; save caller x30 to 0x10000FF0 (slot reserved for
                   ;; trap-time LR scratch by Phase 3(b)).
                   (a64-movz buf +a64-x16+ #xFFF0 0)
                   (a64-movk buf +a64-x16+ #x1000 1)
                   (a64-str-unsigned buf +a64-x30+ +a64-x16+ 0)
                   (let ((idx (a64-current-index buf)))
                     (a64-bl buf 0)
                     (a64-add-fixup buf idx *aarch64-handler-pop-label* :bl))
                   (a64-movz buf +a64-x16+ #xFFF0 0)
                   (a64-movk buf +a64-x16+ #x1000 1)
                   (a64-ldr-unsigned buf +a64-x30+ +a64-x16+ 0))
                  (t
                   (a64-load-imm64 buf +a64-x16+ #x10000180)
                   (a64-str-unsigned buf +a64-xzr+ +a64-x16+ 0))))
               ((= code #x0513)
                ;; SAVE-OUTER: copy slot 0x10000180/188/190 → 0x100001A0/1A8/1B0.
                ;; Used by fork-file to establish a "fallback" handler that
                ;; the IRQ deadline can longjmp to even when slot 180 has
                ;; been zeroed by a per-test CLEAR-HANDLER.
                (a64-load-imm64 buf +a64-x16+ #x10000180)
                (a64-load-imm64 buf +a64-x17+ #x100001C0)
                (a64-ldr-unsigned buf +a64-x18+ +a64-x16+ 0)
                (a64-str-unsigned buf +a64-x18+ +a64-x17+ 0)
                (a64-ldr-unsigned buf +a64-x18+ +a64-x16+ 8)
                (a64-str-unsigned buf +a64-x18+ +a64-x17+ 8)
                (a64-ldr-unsigned buf +a64-x18+ +a64-x16+ 16)
                (a64-str-unsigned buf +a64-x18+ +a64-x17+ 16))
               ((= code #x0514)
                ;; CLEAR-OUTER: zero slot 0x100001C0 so the IRQ handler
                ;; falls through to "no handler".
                (a64-load-imm64 buf +a64-x16+ #x100001C0)
                (a64-str-unsigned buf +a64-xzr+ +a64-x16+ 0))
               ((= code #x0515)
                ;; RESTORE-OUTER: copy slot 0x100001C0/1C8/1D0 → 0x10000180/188/190.
                ;; Re-establishes the fork-file outer handler-case as
                ;; slot 180's value, so a subsequent SETJMP overwriting
                ;; slot 180 doesn't lose the outer.  Counterpart to
                ;; SAVE-OUTER (#x0513).  Use case: between per-test
                ;; handler-cases inside fork-file's thunk, where the
                ;; previous test's CLEAR-HANDLER zeroed slot 180.
                (a64-load-imm64 buf +a64-x16+ #x100001C0)
                (a64-load-imm64 buf +a64-x17+ #x10000180)
                (a64-ldr-unsigned buf +a64-x18+ +a64-x16+ 0)
                (a64-str-unsigned buf +a64-x18+ +a64-x17+ 0)
                (a64-ldr-unsigned buf +a64-x18+ +a64-x16+ 8)
                (a64-str-unsigned buf +a64-x18+ +a64-x17+ 8)
                (a64-ldr-unsigned buf +a64-x18+ +a64-x16+ 16)
                (a64-str-unsigned buf +a64-x18+ +a64-x17+ 16))
               ((= code #x0530)
                ;; COPY-OVERFLOW-ARGS — mirrors translate-x64.lisp:711.
                ;; Read dynamic nargs from slot 0x10000150, then copy stack
                ;; args 4..min(nargs,23) from caller's frame ([FP+80+(i-4)*8])
                ;; into the callee's local frame slots ([FP + frame-slot-base
                ;; + i*-8]) so emit-rest-prologue's cond ladder can stack-load
                ;; them uniformly.  Without this trap, AArch64 hit the SVC
                ;; fallback (sync exception → halt at boot 'B .') whenever a
                ;; &rest function was called.  Confirmed cause of the
                ;; AArch64-ANSI hang at make-package → %mapcar-…-designator →
                ;; nreverse → rplacd (2-arg call to a &rest defun).
                ;;
                ;; Scratch: x9 (count, then temp), x10 (src ptr), x11 (dst ptr).
                ;; x16/x17 used by load-imm64 sequences.  We DO NOT touch
                ;; x12-x15 either, since the IR allocator skips them but
                ;; the rest of the function still expects them stable.
                ;;
                ;; Layout (matches x64 cap of 24 total args):
                ;;   load nargs (32-bit zero-extend to x9 — strip any noise
                ;;             SET-NARGS writes only 16 bits).
                ;;   cmp w9, #5; b.lt done            (no overflow)
                ;;   cmp w9, #24; b.le nocap; mov w9, #24
                ;;   sub w9, w9, #4
                ;;   add x10, x29, #80   (src = FP + 80)
                ;;   sub x11, x29, #96   (dst = FP - 96, slot 4 raw offset)
                ;; loop:
                ;;   cbz w9, done
                ;;   ldr x16, [x10]; str x16, [x11]
                ;;   add x10, x10, #8; sub x11, x11, #8; sub w9, w9, #1
                ;;   b loop
                ;; done:
                (a64-load-imm64 buf +a64-x17+ #x10000150)
                ;; ldr w9, [x17] — read 32-bit nargs (zero-extends to x9)
                (a64-ldr-width buf 9 +a64-x17+ 0 2)
                ;; cmp w9, #5  (32-bit subs-imm form: SF=0)
                ;; Encoding: 31|30 28|24 22|21|imm12|Rn|Rd  for SUBS imm:
                ;;   sf=0, op=1 (SUB), S=1 (set flags), 100010, sh=0, imm12, Rn, Rd=31(wzr)
                ;; Easier: use existing helper.  a64-cmp-imm is sf=1; we need
                ;; a 32-bit cmp for the zero-extended w9, but a 64-bit cmp
                ;; gives the same result here (w9 only holds 32 bits).
                (a64-cmp-imm buf 9 5)
                ;; b.lt done — placeholder; back-patched after loop end.
                (let ((blt-idx (a64-current-index buf)))
                  (a64-emit buf 0)   ; B.LT placeholder
                  ;; cmp w9, #24
                  (a64-cmp-imm buf 9 24)
                  ;; b.le nocap (skip "mov w9, 24")
                  (let ((ble-idx (a64-current-index buf)))
                    (a64-emit buf 0)   ; B.LE placeholder
                    ;; movz w9, #24 — w9 = 24 (caps high nargs)
                    (a64-movz buf 9 24 0)
                    ;; nocap:
                    (let ((nocap-idx (a64-current-index buf)))
                      ;; patch ble → nocap
                      (let* ((bo (* (- nocap-idx ble-idx) 4))
                             (imm19 (logand (ash bo -2) #x7FFFF)))
                        (setf (aref (a64-buffer-code buf) ble-idx)
                              (logior #x54000000 (ash imm19 5) #b1101)))) ; LE
                    ;; sub w9, w9, #4
                    (a64-sub-imm buf 9 9 4)
                    ;; add x10, x29, #80 — src ptr
                    (a64-add-imm buf 10 +a64-x29+ 80)
                    ;; sub x11, x29, #96 — dst ptr
                    (a64-sub-imm buf 11 +a64-x29+ 96)
                    ;; loop:
                    (let ((loop-idx (a64-current-index buf)))
                      ;; cbz w9, done — 32-bit CBZ
                      (let ((cbz-idx (a64-current-index buf)))
                        (a64-emit buf 0)   ; CBZ placeholder
                        ;; ldr x16, [x10]
                        (a64-ldr-unsigned buf +a64-x16+ 10 0)
                        ;; str x16, [x11]
                        (a64-str-unsigned buf +a64-x16+ 11 0)
                        ;; add x10, x10, #(stride) — caller's :push stride
                        ;; (16 in Linux/aligned mode, 8 in bare-metal).
                        ;; Mismatching this corrupts the copied args the
                        ;; same way the static frame-enter copy did
                        ;; before the corresponding fix in <#x0100>.
                        (a64-add-imm buf 10 10
                                     (if *aarch64-stack-align-16* 16 8))
                        ;; sub x11, x11, #8 — dst stride is the frame
                        ;; slot pitch (always 8), independent of push
                        ;; mode — frame slots are densely packed in our
                        ;; spill area.
                        (a64-sub-imm buf 11 11 8)
                        ;; sub w9, w9, #1
                        (a64-sub-imm buf 9 9 1)
                        ;; b loop
                        (let* ((bo (* (- loop-idx (a64-current-index buf)) 4))
                               (imm26 (logand (ash bo -2) #x3FFFFFF)))
                          (a64-emit buf (logior #x14000000 imm26)))
                        ;; done:
                        (let ((done-idx (a64-current-index buf)))
                          ;; patch CBZ → done
                          (let* ((bo (* (- done-idx cbz-idx) 4))
                                 (imm19 (logand (ash bo -2) #x7FFFF)))
                            (setf (aref (a64-buffer-code buf) cbz-idx)
                                  (logior #x34000000 (ash imm19 5) 9)))
                          ;; patch initial B.LT → done
                          (let* ((bo (* (- done-idx blt-idx) 4))
                                 (imm19 (logand (ash bo -2) #x7FFFF)))
                            (setf (aref (a64-buffer-code buf) blt-idx)
                                  (logior #x54000000 (ash imm19 5) #b1011))))))))) ; LT
               ((= code #x0520)
                ;; INSTALL-SIGNAL-HANDLERS — AArch64 Linux.  Mirror of
                ;; translate-x64.lisp trap #x0520.  Installs SIGSEGV(11),
                ;; SIGBUS(7), SIGFPE(8), SIGILL(4) via rt_sigaction
                ;; (syscall 134).  The handler is an embedded asm stub —
                ;; NOT a Lisp function — because Lisp entry would allocate
                ;; stack/GC, both unsafe in signal context.
                ;;
                ;; Stub semantics: if a handler-case is active (saved SP
                ;; at #x10000180 != 0), pop the handler stack INLINE,
                ;; restore SP/FP/PC, jump back with x0 = T (#xDEAD1009).
                ;; Otherwise sys_exit(139).
                ;;
                ;; Branches inside the stub are manually patched (record
                ;; index, emit placeholder, compute offset, patch in
                ;; place) instead of using the fixup table — the fixup
                ;; resolver may not run for this emit context.
                ;;
                ;; Without this trap, CAR/CDR on non-NIL non-cons (and
                ;; other faulting fast-paths) propagate the SIGSEGV out
                ;; to qemu's "uncaught target signal 11" handler and kill
                ;; the fork — explaining the "uncatchable runtime EVAL
                ;; SEGV" pattern that pre-stamped probes 56491-56493 and
                ;; prestamped most of the documentation / defmethod /
                ;; gensym test clusters.

                ;; --- Branch past the embedded stub on this exec path ---
                (let ((past-stub-b-idx (a64-current-index buf)))
                  (a64-emit buf #x14000000)   ; B #0 — patched below.

                  ;; ============================================================
                  ;; Embedded signal handler stub.  Entry: x0=signum,
                  ;; x1=siginfo*, x2=ucontext*.  We don't touch ucontext.
                  ;; ============================================================
                  (let ((stub-start-idx (a64-current-index buf)))

                    ;; x9/x10/x11 = saved SP / FP / PC.
                    (a64-load-imm64 buf +a64-x16+ #x10000180)
                    (a64-ldur buf +a64-x9+  +a64-x16+ 0)
                    (a64-ldur buf +a64-x10+ +a64-x16+ 8)
                    (a64-ldur buf +a64-x11+ +a64-x16+ 16)

                    ;; If x9 == 0, no handler active → exit path.
                    ;; CBZ x9, #0 — patched to jump to exit-label.
                    (let ((cbz-exit-idx (a64-current-index buf)))
                      (a64-emit buf (logior #xB4000000 9))    ; CBZ x9

                      ;; ---- Inline handler-stack pop ----
                      ;; depth at #x10010000 ; frames at #x10010008 + depth*24.
                      ;; Scratch: x12 = depth-addr, x13 = depth, x14 = frame
                      ;; ptr, x15 = slot ptr (#x10000180), x16 = temp.
                      (a64-load-imm64 buf +a64-x12+ #x10010000)
                      (a64-ldur buf +a64-x13+ +a64-x12+ 0)
                      (a64-load-imm64 buf +a64-x15+ #x10000180)
                      ;; If depth == 0, write zeros to slot then skip.
                      (let ((cbz-zero-idx (a64-current-index buf)))
                        (a64-emit buf (logior #xB4000000 13))  ; CBZ x13

                        ;; --- depth > 0 branch: depth--, copy frame ---
                        (a64-sub-imm buf +a64-x13+ +a64-x13+ 1)
                        (a64-str-unsigned buf +a64-x13+ +a64-x12+ 0)
                        (a64-add-imm buf +a64-x14+ +a64-x12+ 8)        ; 0x10010008
                        (a64-lsl-imm buf +a64-x16+ +a64-x13+ 4)        ; depth*16
                        (a64-add-reg buf +a64-x14+ +a64-x14+ +a64-x16+ 0 0)
                        (a64-lsl-imm buf +a64-x16+ +a64-x13+ 3)        ; depth*8
                        (a64-add-reg buf +a64-x14+ +a64-x14+ +a64-x16+ 0 0)
                        (a64-ldur buf +a64-x16+ +a64-x14+ 0)
                        (a64-stur buf +a64-x16+ +a64-x15+ 0)
                        (a64-ldur buf +a64-x16+ +a64-x14+ 8)
                        (a64-stur buf +a64-x16+ +a64-x15+ 8)
                        (a64-ldur buf +a64-x16+ +a64-x14+ 16)
                        (a64-stur buf +a64-x16+ +a64-x15+ 16)
                        ;; Jump past zero branch.
                        (let ((b-after-pop-idx (a64-current-index buf)))
                          (a64-emit buf #x14000000)             ; B #0

                          ;; zero-depth target: write zeros to slot.
                          (let ((zero-target-idx (a64-current-index buf)))
                            (a64-stur buf +a64-xzr+ +a64-x15+ 0)
                            (a64-stur buf +a64-xzr+ +a64-x15+ 8)
                            (a64-stur buf +a64-xzr+ +a64-x15+ 16)

                            ;; after-pop target: restore SP/FP, x0=T, BR PC.
                            (let ((after-pop-idx (a64-current-index buf)))
                              ;; mov sp, x9  →  ADD sp, x9, #0.
                              (a64-add-imm buf +a64-sp+ +a64-x9+ 0)
                              (a64-mov-reg buf +a64-x29+ +a64-x10+)
                              ;; x0 = T = #xDEAD1009.
                              (a64-movz buf +a64-x0+ #x1009 0)
                              (a64-movk buf +a64-x0+ #xDEAD 1)
                              ;; BR x11.
                              (a64-emit buf (logior #xD61F0000 (ash +a64-x11+ 5)))

                              ;; --- exit label: sys_exit(139) ---
                              (let ((exit-label-idx (a64-current-index buf)))
                                (a64-movz buf +a64-x0+ 139 0)
                                (a64-movz buf +a64-x8+ 93 0)
                                (a64-svc buf 0)

                                ;; ============================================
                                ;; Patch in-stub branches (instruction-offset
                                ;; relative).  Note: B/CBZ imm fields are
                                ;; signed; logand with mask clips correctly
                                ;; for positive forward jumps used here.
                                ;; ============================================

                                ;; CBZ x9 → exit-label.
                                (let* ((off (- exit-label-idx cbz-exit-idx))
                                       (imm19 (logand off #x7FFFF))
                                       (code (a64-buffer-code buf)))
                                  (setf (aref code cbz-exit-idx)
                                        (logior (aref code cbz-exit-idx)
                                                (ash imm19 5))))
                                ;; CBZ x13 → zero-depth target.
                                (let* ((off (- zero-target-idx cbz-zero-idx))
                                       (imm19 (logand off #x7FFFF))
                                       (code (a64-buffer-code buf)))
                                  (setf (aref code cbz-zero-idx)
                                        (logior (aref code cbz-zero-idx)
                                                (ash imm19 5))))
                                ;; B (in depth-decrement branch) → after-pop.
                                (let* ((off (- after-pop-idx b-after-pop-idx))
                                       (imm26 (logand off #x3FFFFFF))
                                       (code (a64-buffer-code buf)))
                                  (setf (aref code b-after-pop-idx)
                                        (logior (aref code b-after-pop-idx)
                                                imm26)))

                                ;; ============================================
                                ;; Past stub — install handler.
                                ;; ============================================
                                (let ((past-stub-idx (a64-current-index buf)))

                                  ;; Patch the initial "B past_stub" branch.
                                  (let* ((off (- past-stub-idx past-stub-b-idx))
                                         (imm26 (logand off #x3FFFFFF))
                                         (code (a64-buffer-code buf)))
                                    (setf (aref code past-stub-b-idx)
                                          (logior (aref code past-stub-b-idx)
                                                  imm26)))

                                  ;; SUB sp, sp, #32 — reserve sigaction.
                                  (a64-emit buf #xD10083FF)

                                  ;; ADR x16, stub-start — sa_handler.
                                  (let ((adr-idx (a64-current-index buf)))
                                    (a64-emit buf (logior #x10000000 +a64-x16+))
                                    (let* ((byte-off (* (- stub-start-idx adr-idx) 4))
                                           (immlo (logand byte-off 3))
                                           (immhi (logand (ash byte-off -2) #x7FFFF))
                                           (code (a64-buffer-code buf)))
                                      (setf (aref code adr-idx)
                                            (logior (aref code adr-idx)
                                                    (ash immlo 29)
                                                    (ash #b10000 24)
                                                    (ash immhi 5)))))
                                  ;; STR x16, [sp, #0].
                                  (a64-str-unsigned buf +a64-x16+ +a64-sp+ 0)

                                  ;; sa_flags = SA_SIGINFO | SA_NODEFER = 0x40000004.
                                  (a64-movz buf +a64-x16+ 4 0)
                                  (a64-movk buf +a64-x16+ #x4000 1)
                                  (a64-str-unsigned buf +a64-x16+ +a64-sp+ 8)

                                  ;; sa_restorer = 0; sa_mask = 0.
                                  (a64-str-unsigned buf +a64-xzr+ +a64-sp+ 16)
                                  (a64-str-unsigned buf +a64-xzr+ +a64-sp+ 24)

                                  ;; Install each signum via rt_sigaction.
                                  (dolist (signum '(11 7 8 4))
                                    (a64-movz buf +a64-x0+ signum 0)
                                    (a64-add-imm buf +a64-x1+ +a64-sp+ 0)
                                    (a64-movz buf +a64-x2+ 0 0)
                                    (a64-movz buf +a64-x3+ 8 0)
                                    (a64-movz buf +a64-x8+ 134 0)   ; rt_sigaction
                                    (a64-svc buf 0))

                                  ;; ADD sp, sp, #32 — free sigaction.
                                  (a64-emit buf #x910083FF)

                                  ;; V0 = NIL (x26).
                                  (a64-mov-reg buf +a64-x0+ +a64-x26+)))))))))))

               (t
                ;; Real CPU trap
                (a64-svc buf code)))))

          ;; ---- MOV Vd, Vs ----
          ((= op +op-mov+)
           (let* ((vd (vr 0)) (vs (vr 1))
                  (ps (ensure-src vs +a64-x16+)))
             (store-dst ps vd)))

          ;; ---- LI Vd, imm64 ----
          ((= op +op-li+)
           (let ((vd (vr 0)) (imm (vr 1)))
             (let ((pd (a64-phys-reg vd)))
               (if pd
                   (a64-load-imm64 buf pd imm)
                   (progn
                     (a64-load-imm64 buf +a64-x16+ imm)
                     (store-dst +a64-x16+ vd))))))

          ;; ---- PUSH Vs ----
          ((= op +op-push+)
           (let ((ps (ensure-src (vr 0) +a64-x16+)))
             (cond
               (*aarch64-stack-align-16*
                ;; SUB SP, SP, #16 ; STR Xs, [SP].  Keeps SP 16-byte aligned
                ;; for Linux EL0 SCTLR.SA stack-alignment-check.
                (a64-sub-imm buf +a64-sp+ +a64-sp+ 16)
                (a64-str-unsigned buf ps +a64-sp+ 0))
               (t
                ;; STR Xs, [SP, #-8]! — original bare-metal path.
                (a64-str-pre buf ps +a64-sp+ -8)))))

          ;; ---- POP Vd ----
          ((= op +op-pop+)
           (let ((pd (a64-phys-reg (vr 0))))
             (cond
               (*aarch64-stack-align-16*
                ;; LDR Xd, [SP] ; ADD SP, SP, #16.
                (let ((dest (or pd +a64-x16+)))
                  (a64-ldr-unsigned buf dest +a64-sp+ 0)
                  (a64-add-imm buf +a64-sp+ +a64-sp+ 16)
                  (unless pd (store-dst +a64-x16+ (vr 0)))))
               (t
                (if pd
                    (a64-ldr-post buf pd +a64-sp+ 8)
                    (progn
                      (a64-ldr-post buf +a64-x16+ +a64-sp+ 8)
                      (store-dst +a64-x16+ (vr 0))))))))

          ;; ---- ADD Vd, Va, Vb ----
          ((= op +op-add+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-add-reg buf pd pa pb 0 0)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- SUB Vd, Va, Vb ----
          ((= op +op-sub+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-sub-reg buf pd pa pb 0 0)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- ADDS Vd, Va, Vb (sets V on signed overflow) ----
          ((= op +op-adds+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-adds-reg buf pd pa pb 0 0)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- SUBS Vd, Va, Vb (sets V on signed overflow) ----
          ((= op +op-subs+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-subs-reg buf pd pa pb 0 0)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- BVS off32 — branch if V flag set ----
          ((= op +op-bvs+)
           (let* ((mvm-offset (vr 0))
                  (target-byte (+ (decoded-mvm-insn-offset insn)
                                  (decoded-mvm-insn-size insn)
                                  mvm-offset))
                  (label (or (gethash target-byte mvm-to-native-label)
                             (setf (gethash target-byte mvm-to-native-label)
                                   (incf *mvm-label-counter*)))))
             (let ((idx (a64-current-index buf)))
               (a64-bcond buf +cc-vs+ 0)
               (a64-add-fixup buf idx label :bcond))))

          ;; ---- ADD-CHECKED Vd, Va, Vb ----
          ;; Tagged + with bignum overflow promotion (x64 sibling:
          ;; translate-x64.lisp +op-add-checked+).  tag(a)+tag(b) =
          ;; 2(a+b) = tag(a+b) directly; ADDS sets V iff the sum left
          ;; the 64-bit signed (= tagged fixnum) range.  On overflow,
          ;; call GENERIC-ADD(va,vb) -> bignum via the inline slow path.
          ;; Result flows through x16 on both paths so the fast path
          ;; can't clobber a phys-reg vd before the slow path reads
          ;; va/vb.
          ((= op +op-add-checked+)
           (let* ((vd (vr 0)) (va (vr 1)) (vb (vr 2))
                  (pa (ensure-src va +a64-x16+))
                  (pb (ensure-src vb +a64-x17+)))
             (cond
               (*aarch64-genadd-bytecode-offset*
                (let ((done (incf *mvm-label-counter*)))
                  (a64-adds-reg buf +a64-x16+ pa pb 0 0)
                  (let ((idx (a64-current-index buf)))
                    (a64-bcond buf +cc-vc+ 0)
                    (a64-add-fixup buf idx done :bcond))
                  (a64-emit-generic-arith-call
                   buf va vb *aarch64-genadd-bytecode-offset*)
                  (a64-set-label buf done)
                  (store-dst +a64-x16+ vd)))
               (t
                ;; No GENERIC-ADD in this image: plain wrapping add.
                (a64-add-reg buf +a64-x16+ pa pb 0 0)
                (store-dst +a64-x16+ vd)))))

          ;; ---- SUB-CHECKED Vd, Va, Vb ----
          ;; Tagged - with bignum overflow promotion (x64 sibling:
          ;; translate-x64.lisp +op-sub-checked+; add-checked mirror above).
          ;; tag(a)-tag(b) = 2(a-b) = tag(a-b) directly; SUBS sets V iff the
          ;; difference left the 64-bit signed (= tagged fixnum) range (e.g.
          ;; (- 0 most-negative-fixnum) = 2^62).  On overflow, call
          ;; GENERIC-SUBTRACT(va,vb) -> bignum via the inline slow path.
          ;; Result flows through x16 on both paths.
          ((= op +op-sub-checked+)
           (let* ((vd (vr 0)) (va (vr 1)) (vb (vr 2))
                  (pa (ensure-src va +a64-x16+))
                  (pb (ensure-src vb +a64-x17+)))
             (cond
               (*aarch64-gensub-bytecode-offset*
                (let ((done (incf *mvm-label-counter*)))
                  (a64-subs-reg buf +a64-x16+ pa pb 0 0)
                  (let ((idx (a64-current-index buf)))
                    (a64-bcond buf +cc-vc+ 0)
                    (a64-add-fixup buf idx done :bcond))
                  (a64-emit-generic-arith-call
                   buf va vb *aarch64-gensub-bytecode-offset*)
                  (a64-set-label buf done)
                  (store-dst +a64-x16+ vd)))
               (t
                ;; No GENERIC-SUBTRACT in this image: plain wrapping sub.
                (a64-sub-reg buf +a64-x16+ pa pb 0 0)
                (store-dst +a64-x16+ vd)))))

          ;; ---- MUL-CHECKED Vd, Va, Vb ----
          ;; Tagged * with bignum overflow promotion.  Fast path:
          ;; untag(va) * tagged(vb) = tag(a*b); a signed 64x64 multiply
          ;; overflowed iff SMULH(high 64) /= (low 64 ASR #63).  On
          ;; overflow, call GENERIC-MULTIPLY(va,vb) -> bignum.
          ((= op +op-mul-checked+)
           (let* ((vd (vr 0)) (va (vr 1)) (vb (vr 2))
                  (pa (ensure-src va +a64-x16+))
                  (pb (ensure-src vb +a64-x17+)))
             (cond
               (*aarch64-genmul-bytecode-offset*
                (let ((done (incf *mvm-label-counter*)))
                  (a64-asr-imm buf +a64-x9+ pa 1)          ; x9 = untag(va)
                  (a64-mov-reg buf +a64-x10+ pb)           ; x10 = vb (tagged)
                  (a64-mul   buf +a64-x16+ +a64-x9+ +a64-x10+) ; low 64
                  (a64-smulh buf +a64-x11+ +a64-x9+ +a64-x10+) ; high 64
                  ;; CMP x11, x16 ASR #63 — equal means no overflow.
                  (a64-subs-reg buf +a64-xzr+ +a64-x11+ +a64-x16+ 2 63)
                  (let ((idx (a64-current-index buf)))
                    (a64-bcond buf +cc-eq+ 0)
                    (a64-add-fixup buf idx done :bcond))
                  (a64-emit-generic-arith-call
                   buf va vb *aarch64-genmul-bytecode-offset*)
                  (a64-set-label buf done)
                  (store-dst +a64-x16+ vd)))
               (t
                ;; No GENERIC-MULTIPLY: plain (possibly wrapping) mul.
                (a64-asr-imm buf +a64-x9+ pa 1)
                (a64-mul buf +a64-x16+ +a64-x9+ pb)
                (store-dst +a64-x16+ vd)))))

          ;; ---- LI-CONST Vd, idx ----
          ;; Load the tagged address of constant-pool slot IDX.  Emit a
          ;; MOVZ + 3xMOVK placeholder quad; apply-li-const-patches
          ;; (cross.lisp) writes the four imm16 fields once the pool
          ;; vaddr is known.  Mirrors x64's MOVABS-placeholder scheme.
          ((= op +op-li-const+)
           (let* ((vd (vr 0))
                  (idx (vr 1))
                  (pd (or (a64-phys-reg vd) +a64-x16+))
                  (movz-byte-pos
                   (* (- (a64-current-index buf)
                         (or *aarch64-translated-start-idx* 0))
                      4)))
             (push (cons movz-byte-pos idx) *aarch64-li-const-patches*)
             (a64-movz buf pd 0 0)   ; bits 0-15
             (a64-movk buf pd 0 1)   ; bits 16-31
             (a64-movk buf pd 0 2)   ; bits 32-47
             (a64-movk buf pd 0 3)   ; bits 48-63
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- MUL Vd, Va, Vb ----
          ;; Tagged fixnum multiply: Vd = (Va >> 1) * Vb
          ;; (because both args carry a tag bit; shifting one removes double-tag)
          ((= op +op-mul+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             ;; ASR x16, Va, #1  (remove one tag bit)
             (a64-asr-imm buf +a64-x16+ pa 1)
             ;; MUL Vd, x16, Vb
             (a64-mul buf pd +a64-x16+ pb)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- MUL26LO Vd, Va, Vb ----
          ;; Low 26 bits of untag(Va)*untag(Vb), tagged
          ((= op +op-mul26lo+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-asr-imm buf +a64-x16+ pa 1)  ; untag va
             (a64-asr-imm buf +a64-x17+ pb 1)  ; untag vb
             (a64-mul buf pd +a64-x16+ +a64-x17+) ; 64-bit result
             ;; UBFX pd, pd, #0, #26 → UBFM pd, pd, #0, #25
             (a64-ubfm buf pd pd 0 25)          ; mask to 26 bits
             (a64-lsl-imm buf pd pd 1)          ; retag
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- MUL26HI Vd, Va, Vb ----
          ;; Bits 26+ of untag(Va)*untag(Vb), tagged
          ((= op +op-mul26hi+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-asr-imm buf +a64-x16+ pa 1)  ; untag va
             (a64-asr-imm buf +a64-x17+ pb 1)  ; untag vb
             (a64-mul buf pd +a64-x16+ +a64-x17+) ; 64-bit result
             (a64-lsr-imm buf pd pd 26)         ; shift right 26
             (a64-lsl-imm buf pd pd 1)          ; retag
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- MUL64LO Vd, Va, Vb ----
          ;; Low 64 bits of raw Va*Vb (no tag/untag)
          ((= op +op-mul64lo+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             ;; MUL Xd, Xa, Xb — raw 64-bit multiply, no untag/retag
             (a64-mul buf pd pa pb)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- MUL64HI Vd, Va, Vb ----
          ;; High 64 bits of raw Va*Vb (no tag/untag)
          ((= op +op-mul64hi+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             ;; UMULH Xd, Xa, Xb — upper 64 bits of unsigned multiply
             (a64-umulh buf pd pa pb)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- ACC128 Vaddr, Vlo, Vhi ----
          ;; mem128[Vaddr] += Vhi:Vlo  (128-bit accumulate, raw)
          ;; Strategy: push hi to stack, addr→x30, lo→x16, then:
          ;;   load mem[addr+0]→x17, ADDS x17,x17,x16, store back
          ;;   load mem[addr+8]→x16, pop hi→x17, ADC x16,x16,x17, store back
          ((= op +op-acc128+)
           (let* ((vaddr (vr 0))
                  (vlo (vr 1))
                  (vhi (vr 2)))
             ;; Step 1: Load hi into x17 and push to stack
             (let ((phi (ensure-src vhi +a64-x17+)))
               (unless (= phi +a64-x17+)
                 (a64-mov-reg buf +a64-x17+ phi)))
             (a64-stp-pre buf +a64-x17+ +a64-xzr+ +a64-sp+ -16)
             ;; Step 2: Load addr→x30, lo→x16
             (let ((paddr (ensure-src vaddr +a64-x16+)))
               (a64-mov-reg buf +a64-x30+ paddr))
             (let ((plo (ensure-src vlo +a64-x16+)))
               (unless (= plo +a64-x16+)
                 (a64-mov-reg buf +a64-x16+ plo)))
             ;; Now: x30=addr, x16=lo_add, stack[0]=hi_add
             ;; Step 3: Load mem_lo, ADDS, store
             (a64-ldur buf +a64-x17+ +a64-x30+ 0)       ; x17 = mem[addr+0]
             (a64-adds-reg buf +a64-x17+ +a64-x17+ +a64-x16+ 0 0) ; x17 += lo_add, sets C
             (a64-stur buf +a64-x17+ +a64-x30+ 0)       ; mem[addr+0] = x17
             ;; Step 4: Load mem_hi, pop hi_add, ADC, store
             (a64-ldur buf +a64-x16+ +a64-x30+ 8)       ; x16 = mem[addr+8]
             (a64-ldp-post buf +a64-x17+ +a64-xzr+ +a64-sp+ 16)  ; pop hi_add→x17
             (a64-adc buf +a64-x16+ +a64-x16+ +a64-x17+) ; x16 += hi_add + carry
             (a64-stur buf +a64-x16+ +a64-x30+ 8)))

          ;; ---- DIV Vd, Va, Vb ----
          ;; Tagged fixnum divide: result = (Va / Vb) then re-tag
          ;; SDIV gives untagged quotient, must shift left by 1 to re-tag
          ((= op +op-div+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             ;; SDIV x16, Va, Vb  (tagged / tagged = untagged)
             (a64-sdiv buf +a64-x16+ pa pb)
             ;; LSL Vd, x16, #1  (re-tag)
             (a64-lsl-imm buf pd +a64-x16+ 1)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- MOD Vd, Va, Vb ----
          ;; Vd = Va - (Va/Vb)*Vb  (tagged)
          ((= op +op-mod+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             ;; SDIV x16, Va, Vb
             (a64-sdiv buf +a64-x16+ pa pb)
             ;; MUL x16, x16, Vb  (quotient * divisor)
             (a64-mul buf +a64-x16+ +a64-x16+ pb)
             ;; SUB Vd, Va, x16  (remainder = dividend - q*divisor)
             (a64-sub-reg buf pd pa +a64-x16+ 0 0)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- NEG Vd, Vs ----
          ((= op +op-neg+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-neg buf pd ps)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- INC Vd ----
          ;; Tagged increment: add 2 (because tag bit is in bit 0)
          ((= op +op-inc+)
           (let* ((vd (vr 0))
                  (pd (ensure-src vd +a64-x16+)))
             (a64-add-imm buf pd pd 2)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- DEC Vd ----
          ((= op +op-dec+)
           (let* ((vd (vr 0))
                  (pd (ensure-src vd +a64-x16+)))
             (a64-sub-imm buf pd pd 2)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- AND Vd, Va, Vb ----
          ((= op +op-and+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-and-reg buf pd pa pb)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- OR Vd, Va, Vb ----
          ((= op +op-or+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-orr-reg buf pd pa pb)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- XOR Vd, Va, Vb ----
          ((= op +op-xor+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-eor-reg buf pd pa pb)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- SHL Vd, Vs, imm8 ----
          ((= op +op-shl+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (amt (vr 2))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-lsl-imm buf pd ps amt)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- SHR Vd, Vs, imm8 ----
          ((= op +op-shr+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (amt (vr 2))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-lsr-imm buf pd ps amt)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- SAR Vd, Vs, imm8 ----
          ((= op +op-sar+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (amt (vr 2))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-asr-imm buf pd ps amt)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- SHLV Vd, Vs, Vc ---- (shift left by register)
          ((= op +op-shlv+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pc (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-lslv buf pd ps pc)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- SARV Vd, Vs, Vc ---- (arithmetic shift right by register)
          ((= op +op-sarv+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pc (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-asrv buf pd ps pc)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- LDB Vd, Vs, pos, size ----
          ;; Bit field extract: UBFM Xd, Xn, #pos, #(pos+size-1)
          ((= op +op-ldb+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pos (vr 2))
                  (sz (vr 3))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-ubfm buf pd ps pos (+ pos sz -1))
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- CMP Va, Vb ----
          ((= op +op-cmp+)
           (let ((pa (ensure-src (vr 0) +a64-x16+))
                 (pb (ensure-src (vr 1) +a64-x17+)))
             (a64-cmp-reg buf pa pb)))

          ;; ---- TEST Va, Vb ----
          ((= op +op-test+)
           (let ((pa (ensure-src (vr 0) +a64-x16+))
                 (pb (ensure-src (vr 1) +a64-x17+)))
             (a64-tst-reg buf pa pb)))

          ;; ---- BR off16 ----
          ((= op +op-br+)
           (let* ((mvm-offset (vr 0))
                  (target-byte (+ (decoded-mvm-insn-offset insn)
                                  (decoded-mvm-insn-size insn)
                                  mvm-offset))
                  (label (gethash target-byte mvm-to-native-label)))
             (unless label
               (setf label (incf *mvm-label-counter*))
               (setf (gethash target-byte mvm-to-native-label) label))
             (let ((idx (a64-current-index buf)))
               (a64-b buf 0)  ; placeholder
               (a64-add-fixup buf idx label :b))))

          ;; ---- Conditional branches: BEQ/BNE/BLT/BGE/BLE/BGT ----
          ((= op +op-beq+)
           (let* ((mvm-offset (vr 0))
                  (target-byte (+ (decoded-mvm-insn-offset insn)
                                  (decoded-mvm-insn-size insn)
                                  mvm-offset))
                  (label (or (gethash target-byte mvm-to-native-label)
                             (setf (gethash target-byte mvm-to-native-label)
                                   (incf *mvm-label-counter*)))))
             (let ((idx (a64-current-index buf)))
               (a64-bcond buf +cc-eq+ 0)
               (a64-add-fixup buf idx label :bcond))))

          ((= op +op-bne+)
           (let* ((mvm-offset (vr 0))
                  (target-byte (+ (decoded-mvm-insn-offset insn)
                                  (decoded-mvm-insn-size insn)
                                  mvm-offset))
                  (label (or (gethash target-byte mvm-to-native-label)
                             (setf (gethash target-byte mvm-to-native-label)
                                   (incf *mvm-label-counter*)))))
             (let ((idx (a64-current-index buf)))
               (a64-bcond buf +cc-ne+ 0)
               (a64-add-fixup buf idx label :bcond))))

          ((= op +op-blt+)
           (let* ((mvm-offset (vr 0))
                  (target-byte (+ (decoded-mvm-insn-offset insn)
                                  (decoded-mvm-insn-size insn)
                                  mvm-offset))
                  (label (or (gethash target-byte mvm-to-native-label)
                             (setf (gethash target-byte mvm-to-native-label)
                                   (incf *mvm-label-counter*)))))
             (let ((idx (a64-current-index buf)))
               (a64-bcond buf +cc-lt+ 0)
               (a64-add-fixup buf idx label :bcond))))

          ((= op +op-bge+)
           (let* ((mvm-offset (vr 0))
                  (target-byte (+ (decoded-mvm-insn-offset insn)
                                  (decoded-mvm-insn-size insn)
                                  mvm-offset))
                  (label (or (gethash target-byte mvm-to-native-label)
                             (setf (gethash target-byte mvm-to-native-label)
                                   (incf *mvm-label-counter*)))))
             (let ((idx (a64-current-index buf)))
               (a64-bcond buf +cc-ge+ 0)
               (a64-add-fixup buf idx label :bcond))))

          ((= op +op-ble+)
           (let* ((mvm-offset (vr 0))
                  (target-byte (+ (decoded-mvm-insn-offset insn)
                                  (decoded-mvm-insn-size insn)
                                  mvm-offset))
                  (label (or (gethash target-byte mvm-to-native-label)
                             (setf (gethash target-byte mvm-to-native-label)
                                   (incf *mvm-label-counter*)))))
             (let ((idx (a64-current-index buf)))
               (a64-bcond buf +cc-le+ 0)
               (a64-add-fixup buf idx label :bcond))))

          ((= op +op-bgt+)
           (let* ((mvm-offset (vr 0))
                  (target-byte (+ (decoded-mvm-insn-offset insn)
                                  (decoded-mvm-insn-size insn)
                                  mvm-offset))
                  (label (or (gethash target-byte mvm-to-native-label)
                             (setf (gethash target-byte mvm-to-native-label)
                                   (incf *mvm-label-counter*)))))
             (let ((idx (a64-current-index buf)))
               (a64-bcond buf +cc-gt+ 0)
               (a64-add-fixup buf idx label :bcond))))

          ;; ---- BNULL Vs, off16 ----
          ;; CMP Vs, VN(x26); B.EQ target
          ((= op +op-bnull+)
           (let* ((ps (ensure-src (vr 0) +a64-x16+))
                  (mvm-offset (vr 1))
                  (target-byte (+ (decoded-mvm-insn-offset insn)
                                  (decoded-mvm-insn-size insn)
                                  mvm-offset))
                  (label (or (gethash target-byte mvm-to-native-label)
                             (setf (gethash target-byte mvm-to-native-label)
                                   (incf *mvm-label-counter*)))))
             (a64-cmp-reg buf ps +a64-x26+)
             (let ((idx (a64-current-index buf)))
               (a64-bcond buf +cc-eq+ 0)
               (a64-add-fixup buf idx label :bcond))))

          ;; ---- BNNULL Vs, off16 ----
          ((= op +op-bnnull+)
           (let* ((ps (ensure-src (vr 0) +a64-x16+))
                  (mvm-offset (vr 1))
                  (target-byte (+ (decoded-mvm-insn-offset insn)
                                  (decoded-mvm-insn-size insn)
                                  mvm-offset))
                  (label (or (gethash target-byte mvm-to-native-label)
                             (setf (gethash target-byte mvm-to-native-label)
                                   (incf *mvm-label-counter*)))))
             (a64-cmp-reg buf ps +a64-x26+)
             (let ((idx (a64-current-index buf)))
               (a64-bcond buf +cc-ne+ 0)
               (a64-add-fixup buf idx label :bcond))))

          ;; ---- CAR Vd, Vs ----
          ;; Cons cell layout: [car|cdr] with tag=1 (low bit)
          ;; CAR: LDUR Xd, [Xs, #-1]  (untag cons pointer)
          ((= op +op-car+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-ldur buf pd ps -1)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- CDR Vd, Vs ----
          ;; CDR: LDR Xd, [Xs, #7]  (untag + skip car = -1 + 8 = 7)
          ((= op +op-cdr+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-ldur buf pd ps 7)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- CONS Vd, Va, Vb ----
          ;; STP Va, Vb, [VA]  (store car, cdr at alloc pointer)
          ;; ADD Vd, VA, #1    (tag the pointer with cons tag = 1)
          ;; ADD VA, VA, #16   (bump alloc pointer by 2 words)
          ((= op +op-cons+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (emit-aarch64-gc-mark-start buf)   ; #160: x24 = new cons base
             (emit-aarch64-gc-mark-cons buf)    ; #160 bug#4: mark it CONS-KIND
             (a64-stp-offset buf pa pb +a64-x24+ 0)
             (a64-add-imm buf pd +a64-x24+ 1)
             (a64-add-imm buf +a64-x24+ +a64-x24+ 16)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- SETCAR Vd, Vs ----
          ;; STUR Vs, [Vd, #-1]  (untag and store to car)
          ((= op +op-setcar+)
           (let ((pd (ensure-src (vr 0) +a64-x16+))
                 (ps (ensure-src (vr 1) +a64-x17+)))
             (a64-stur buf ps pd -1)))

          ;; ---- SETCDR Vd, Vs ----
          ;; STUR Vs, [Vd, #7]  (untag + skip car)
          ((= op +op-setcdr+)
           (let ((pd (ensure-src (vr 0) +a64-x16+))
                 (ps (ensure-src (vr 1) +a64-x17+)))
             (a64-stur buf ps pd 7)))

          ;; ---- CONSP Vd, Vs ----
          ;; (consp NIL) MUST return NIL even though NIL=#xDEAD0001 has
          ;; cons-tag 1.  Pre-check Vs == x26 (NIL); if so force x16=0
          ;; so the cons-tag (1) test downstream fails.  Then test low
          ;; 4 bits for cons tag.  Mirrors x64's NIL pre-check.
          ((= op +op-consp+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pd (or (a64-phys-reg vd) +a64-x17+)))
             ;; x16 = ps & 0xF.
             (a64-movz buf +a64-x17+ #xF 0)
             (a64-and-reg buf +a64-x16+ ps +a64-x17+)
             ;; CMP ps, x26; CSEL x16, XZR, x16, EQ.
             ;; Encoding: Rd=Rn if cond else Rm.  Want: ps==x26 → x16=0,
             ;; ps!=x26 → x16 unchanged.  Rn=XZR (selected when EQ),
             ;; Rm=x16 (selected when NE).
             (a64-cmp-reg buf ps +a64-x26+)
             (a64-emit buf (logior #x9A800000
                                   (ash +a64-x16+ 16)        ; Rm = x16
                                   (ash +cc-eq+ 12)
                                   (ash 31 5)                ; Rn = XZR
                                   +a64-x16+))               ; Rd = x16
             (a64-cmp-imm buf +a64-x16+ 1)
             ;; x18 = T literal 0xDEAD1009
             (a64-movz buf +a64-x18+ #x1009 0)
             (a64-movk buf +a64-x18+ #xDEAD 1)
             ;; CSEL pd, x18, x26, EQ  →  pd = T if EQ else NIL.
             (a64-emit buf (logior #x9A800000
                                   (ash +a64-x26+ 16)
                                   (ash +cc-eq+ 12)
                                   (ash +a64-x18+ 5)
                                   pd))
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- ATOM Vd, Vs ----
          ;; NIL IS an atom — but NIL=#xDEAD0001 has cons-tag 1, so the
          ;; low-nibble test would mis-classify.  Same pre-check as
          ;; consp: ps==NIL → x16=0 (not 1) so the NE-against-1 path
          ;; below returns T (atom).
          ((= op +op-atom+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pd (or (a64-phys-reg vd) +a64-x17+)))
             (a64-movz buf +a64-x17+ #xF 0)
             (a64-and-reg buf +a64-x16+ ps +a64-x17+)
             ;; CMP ps, x26; CSEL x16, XZR, x16, EQ — Rn=XZR (EQ-path),
             ;; Rm=x16 (NE-path).
             (a64-cmp-reg buf ps +a64-x26+)
             (a64-emit buf (logior #x9A800000
                                   (ash +a64-x16+ 16)        ; Rm = x16
                                   (ash +cc-eq+ 12)
                                   (ash 31 5)                ; Rn = XZR
                                   +a64-x16+))               ; Rd = x16
             (a64-cmp-imm buf +a64-x16+ 1)
             (a64-movz buf +a64-x18+ #x1009 0)
             (a64-movk buf +a64-x18+ #xDEAD 1)
             ;; CSEL pd, x18, x26, NE  →  pd = T if not-EQ else NIL.
             (a64-emit buf (logior #x9A800000
                                   (ash +a64-x26+ 16)
                                   (ash +cc-ne+ 12)
                                   (ash +a64-x18+ 5)
                                   pd))
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- ALLOC-OBJ Vd, count:imm16, subtag:imm8 ----
          ;; LAYOUT matches x64 and cross.lisp's constant-pool emitter:
          ;;   [header(8) | padding(8) | slot0(8) | slot1(8) ... | align]
          ;; Header = (count << 8) | subtag.  Tagged pointer = raw + 9.
          ;; Total bytes = (count+2)*8 rounded up to 16.
          ;; OBJ-REF/OBJ-SET use offset = idx*8 + 7 (= raw - 9 + 16 + idx*8).
          ((= op +op-alloc-obj+)
           (let* ((vd (vr 0))
                  (count (vr 1))
                  (subtag (vr 2))
                  (total-size (logand (+ (* (+ count 2) 8) 15) (lognot 15)))
                  (header-imm (logior (ash count 8) subtag))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (emit-aarch64-gc-mark-start buf)   ; #160: x24 = new object base
             ;; Header value fits in MOVZ (if subtag-only) or MOVZ+MOVK
             ;; for larger counts — a64-load-imm64 picks the right form.
             (a64-load-imm64 buf +a64-x16+ header-imm)
             (a64-stur buf +a64-x16+ +a64-x24+ 0)
             ;; Tagged result = alloc_ptr + 9 (object tag matches x64).
             (a64-add-imm buf pd +a64-x24+ 9)
             (if (<= total-size #xFFF)
                 (a64-add-imm buf +a64-x24+ +a64-x24+ total-size)
                 (progn
                   (a64-load-imm64 buf +a64-x17+ total-size)
                   (a64-add-reg buf +a64-x24+ +a64-x24+ +a64-x17+ 0 0)))
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- OBJ-REF Vd, Vobj, idx:imm8 ----
          ;; Load slot: LDR Vd, [Vobj + idx*8 - tag_offset]
          ((= op +op-obj-ref+)
           (let* ((vd (vr 0))
                  (vobj (vr 1))
                  (idx (vr 2))
                  (pd (or (a64-phys-reg vd) +a64-x17+)))
             (if (= vobj +vreg-vfp+)
                 ;; Frame slot access: use FP-relative offset below spill area
                 (let ((offset (+ +a64-frame-slot-base+ (* idx -8))))
                   (if (and (>= offset -256) (<= offset 255))
                       (a64-ldur buf pd +a64-x29+ offset)
                       ;; Large offset: SUB x16, x29, #abs_offset; LDUR pd, [x16]
                       (progn
                         (a64-sub-imm buf +a64-x16+ +a64-x29+ (- offset))
                         (a64-ldur buf pd +a64-x16+ 0))))
                 ;; Normal object slot access — tag=9 layout, slot N at
                 ;; tagged + N*8 + 7 (= raw + 16 + N*8).
                 (let* ((pobj (ensure-src vobj +a64-x16+))
                        (offset (+ (* idx 8) 7)))
                   (if (and (>= offset -256) (<= offset 255))
                       (a64-ldur buf pd pobj offset)
                       (progn
                         (a64-load-imm64 buf +a64-x17+ offset)
                         (a64-add-reg buf +a64-x17+ pobj +a64-x17+ 0 0)
                         (a64-ldur buf pd +a64-x17+ 0)))))
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- OBJ-SET Vobj, idx:imm8, Vs ----
          ((= op +op-obj-set+)
           (let* ((vobj (vr 0))
                  (idx (vr 1))
                  (ps (ensure-src (vr 2) +a64-x17+)))
             (if (= vobj +vreg-vfp+)
                 ;; Frame slot store: use FP-relative offset below spill area
                 (let ((offset (+ +a64-frame-slot-base+ (* idx -8))))
                   (if (and (>= offset -256) (<= offset 255))
                       (a64-stur buf ps +a64-x29+ offset)
                       ;; Large offset: SUB x16, x29, #abs_offset; STUR ps, [x16]
                       (progn
                         (a64-sub-imm buf +a64-x16+ +a64-x29+ (- offset))
                         (a64-stur buf ps +a64-x16+ 0))))
                 ;; Normal object slot store — tag=9 layout, slot N at +N*8+7.
                 (let* ((pobj (ensure-src vobj +a64-x16+))
                        (offset (+ (* idx 8) 7)))
                   (if (and (>= offset -256) (<= offset 255))
                       (a64-stur buf ps pobj offset)
                       (progn
                         (a64-load-imm64 buf +a64-x16+ offset)
                         (a64-add-reg buf +a64-x16+ pobj +a64-x16+ 0 0)
                         (a64-stur buf ps +a64-x16+ 0)))))))

          ;; ---- OBJ-TAG Vd, Vs ----
          ;; Extract low 4 bits, then fixnum-tag (SHL 1) — matches x64.
          ;; compile-funcall compares against `(ash +tag-object+ +fixnum-shift+)`
          ;; = 18; result must be the shifted value or closure/symbol dispatch
          ;; is silently bypassed.  UBFIZ Xd, Xn, #1, #4 = UBFM Xd, Xn, #63, #3
          ;; — one instruction (replaces 2-insn AND+SHL).
          ((= op +op-obj-tag+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-ubfm buf pd ps 63 3)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- OBJ-SUBTAG Vd, Vs ----
          ;; Load header at raw = tagged-9, extract low 8 bits, fixnum-tag.
          ;; Returns (subtag << 1) so downstream `(= (obj-subtag x) #x32)`
          ;; patterns match the same fixnum-tagged constants as on x64.
          ;;
          ;; TAG-SAFETY (mirrors translate-x64.lisp +op-obj-subtag+):
          ;;   1. Low nibble != 9 → not a heap pointer (fixnum/cons/imm/forward).
          ;;      Loading at [Vs-9] would hit unmapped memory.  Return 0.
          ;;   2. Vs == T (#xDEAD1009) → low nibble 9 but T is an immediate.
          ;;      [T-9] = #xDEAD1000 is unmapped on AArch64 (beyond L2 range).
          ;;      Return 0.
          ;;
          ;; Result-on-fail = 0 (subtag 0) falsifies any caller's specific-
          ;; subtag comparison.  E.g. `(= (obj-subtag x) #x32)` on T returns
          ;; NIL — matches the x64 behaviour.
          ((= op +op-obj-subtag+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pd (or (a64-phys-reg vd) +a64-x16+))
                  (fail-label (incf *mvm-label-counter*))
                  (done-label (incf *mvm-label-counter*)))
             ;; Check 1: low nibble == 9 ?
             (a64-ubfm buf +a64-x17+ ps 63 3)  ; x17 = (Vs & 0xF) << 1
             (a64-cmp-imm buf +a64-x17+ 18)    ; compare to 18 (= 9 << 1)
             (let ((idx (a64-current-index buf)))
               (a64-bcond buf +cc-ne+ 0)
               (a64-add-fixup buf idx fail-label :bcond))
             ;; Check 2: Vs == T (#xDEAD1009) ?
             (a64-load-imm64 buf +a64-x17+ #xDEAD1009)
             (a64-cmp-reg buf ps +a64-x17+)
             (let ((idx (a64-current-index buf)))
               (a64-bcond buf +cc-eq+ 0)
               (a64-add-fixup buf idx fail-label :bcond))
             ;; Real object: load header, extract subtag, fixnum-tag.
             (a64-ldur buf +a64-x17+ ps -9)
             (a64-ubfm buf pd +a64-x17+ 63 7)
             (let ((idx (a64-current-index buf)))
               (a64-emit buf (logior (ash #b000101 26) (logand 0 #x3FFFFFF)))  ; B placeholder
               (a64-add-fixup buf idx done-label :b))
             (a64-set-label buf fail-label)
             ;; fail: pd = 0
             (a64-movz buf pd 0 0)
             (a64-set-label buf done-label)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- AREF Vd, Vobj, Vidx ----
          ;; Variable-index array load.  tag=9 layout: slot 0 at tagged+7
          ;; (= raw+16).  Vidx is fixnum-tagged (real_idx*2); shifting by 2
          ;; gives real_idx*8 in the address computation.
          ((= op +op-aref+)
           (let* ((vd (vr 0))
                  (pobj (ensure-src (vr 1) +a64-x16+))
                  (pidx (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-add-imm buf +a64-x16+ pobj 7)
             (a64-add-reg buf +a64-x16+ +a64-x16+ pidx 0 2)
             (a64-ldur buf pd +a64-x16+ 0)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- ASET Vobj, Vidx, Vs ----
          ;; Variable-index array store — tag=9 layout, slot 0 at tagged+7.
          ((= op +op-aset+)
           (let* ((pobj (ensure-src (vr 0) +a64-x16+))
                  (pidx (ensure-src (vr 1) +a64-x17+)))
             (a64-add-imm buf +a64-x16+ pobj 7)
             (a64-add-reg buf +a64-x16+ +a64-x16+ pidx 0 2)
             (let ((ps (ensure-src (vr 2) +a64-x17+)))
               (a64-stur buf ps +a64-x16+ 0))))

          ;; ---- ARRAY-LEN Vd, Vobj ----
          ;; Extract element count from header at raw = tagged-9.
          ;; count = header >> 8 (matches x64 count<<8 packing).
          ;; Tagged fixnum result = count << 1.
          ((= op +op-array-len+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-ldur buf +a64-x17+ ps -9)
             (a64-lsr-imm buf +a64-x17+ +a64-x17+ 8)
             (a64-lsl-imm buf pd +a64-x17+ 1)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- ALLOC-ARRAY Vd, Vcount ----
          ;; Dynamic array allocation (tag=9 layout, matches x64).
          ;; Header = (count << 8) | #x32 (array subtag).
          ;; Layout: header(8)+padding(8)+count*slot(8).
          ;; Total bytes = (count+2)*8 round-up-16 = floor((count+3)/2)*16
          ;; — correct for BOTH even and odd counts under the padding layout.
          ;; (The earlier (count+2)/2*16 formula under-allocates by 8 bytes
          ;;  for odd count when padding is present.)
          ((= op +op-alloc-array+)
           (let* ((vd (vr 0))
                  (pcount (ensure-src (vr 1) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (emit-aarch64-gc-mark-start buf)   ; #160: x24 = new array base
             ;; Build header: x16 = (count << 8) | #x32 via LSL + ORR-reg
             ;; (MOVK at shift=0 would clobber bits 0-15 and corrupt the
             ;;  low byte of count for count>=256, so use a separate temp).
             (a64-lsl-imm buf +a64-x16+ pcount 8)
             (a64-movz buf +a64-x9+ #x32 0)
             (a64-orr-reg buf +a64-x16+ +a64-x16+ +a64-x9+)
             (a64-stur buf +a64-x16+ +a64-x24+ 0)
             ;; Aligned size = floor((count+3)/2)*16.
             (a64-add-imm buf +a64-x17+ pcount 3)
             (a64-lsr-imm buf +a64-x17+ +a64-x17+ 1)
             (a64-lsl-imm buf +a64-x17+ +a64-x17+ 4)
             (a64-add-imm buf pd +a64-x24+ 9)
             (a64-add-reg buf +a64-x24+ +a64-x24+ +a64-x17+ 0 0)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- ALLOC-U8 Vd, Vcount ----  (byte-packed (unsigned-byte 8) vector)
          ;; Feature #183, ported from translate-x64.lisp +op-alloc-u8+.  Vcount
          ;; is a TAGGED fixnum (element/byte count N).  Object: header at [x24]
          ;; = (N << 8) | #x11 (u8-vector subtag), then 8-byte pad, then N packed
          ;; bytes at +16.  Total size = align16(16 + N).  (No MCGC start-bit /
          ;; zero-init — matches the aarch64 :alloc-array convention above.)
          ((= op +op-alloc-u8+)
           (let* ((vd (vr 0))
                  (pcount (ensure-src (vr 1) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (emit-aarch64-gc-mark-start buf)   ; #160: x24 = new u8-vector base
             (a64-asr-imm buf +a64-x9+ pcount 1)              ; x9 = N (untagged)
             (a64-lsl-imm buf +a64-x16+ +a64-x9+ 8)           ; x16 = N << 8
             (a64-movz buf +a64-x10+ #x11 0)                  ; x10 = 0x11 subtag
             (a64-orr-reg buf +a64-x16+ +a64-x16+ +a64-x10+)  ; x16 = header
             (a64-stur buf +a64-x16+ +a64-x24+ 0)             ; [x24] = header
             (a64-add-imm buf pd +a64-x24+ 9)                 ; result = base | tag9
             ;; size = align16(16 + N) = ((N + 31) >> 4) << 4
             (a64-add-imm buf +a64-x17+ +a64-x9+ 31)
             (a64-lsr-imm buf +a64-x17+ +a64-x17+ 4)
             (a64-lsl-imm buf +a64-x17+ +a64-x17+ 4)
             (a64-add-reg buf +a64-x24+ +a64-x24+ +a64-x17+ 0 0) ; bump alloc ptr
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- U8-REF Vd, Varr, Vidx ----  (load one byte from a u8 vector)
          ;; Byte address = (Varr - 9) + 16 + real_idx = Varr + 7 + real_idx.
          ;; Vidx is a TAGGED fixnum (real_idx*2); result is a TAGGED fixnum
          ;; (byte << 1), matching mem-ref :u8 and translate-x64 +op-u8-ref+.
          ((= op +op-u8-ref+)
           (let* ((vd (vr 0))
                  (parr (ensure-src (vr 1) +a64-x16+))
                  (pidx (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-asr-imm buf +a64-x9+ pidx 1)                ; x9 = real_idx
             (a64-add-reg buf +a64-x9+ +a64-x9+ parr 0 0)     ; x9 = Varr + real_idx
             ;; LDRB Wpd, [x9, #7]  = 0x39401C00 | (x9<<5) | pd
             (a64-emit buf (logior #x39401C00 (ash +a64-x9+ 5) pd))
             (a64-lsl-imm buf pd pd 1)                        ; tag: byte << 1
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- U8-SET Varr, Vidx, Vval ----  (store low byte of Vval)
          ;; Byte address = Varr + 7 + real_idx.  Vidx and Vval are TAGGED
          ;; fixnums; untag both (>>1).  Clobbers only scratch (x10/x11) — no
          ;; V-phys-reg is touched, matching translate-x64 +op-u8-set+.
          ((= op +op-u8-set+)
           (let* ((parr (ensure-src (vr 0) +a64-x16+))
                  (pidx (ensure-src (vr 1) +a64-x17+))
                  (pval (ensure-src (vr 2) +a64-x9+)))
             (a64-asr-imm buf +a64-x10+ pidx 1)               ; x10 = real_idx
             (a64-add-reg buf +a64-x10+ +a64-x10+ parr 0 0)   ; x10 = Varr + real_idx
             (a64-asr-imm buf +a64-x11+ pval 1)               ; x11 = untagged value
             ;; STRB W11, [x10, #7]  = 0x39001C00 | (x10<<5) | 11
             (a64-emit buf (logior #x39001C00 (ash +a64-x10+ 5) +a64-x11+))))

          ;; ---- LOAD Vd, Vaddr, width ----
          ((= op +op-load+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (width (vr 2))
                  (pd (or (a64-phys-reg vd) +a64-x17+)))
             (a64-ldr-width buf pd pa 0 width)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- STORE Vaddr, Vs, width ----
          ((= op +op-store+)
           (let ((pa (ensure-src (vr 0) +a64-x16+))
                 (ps (ensure-src (vr 1) +a64-x17+))
                 (width (vr 2)))
             (a64-str-width buf ps pa 0 width)))

          ;; ---- FENCE ----
          ((= op +op-fence+)
           (a64-dmb buf #xB))  ; DMB ISH

          ;; ---- CALL target:imm32 ----
          ;; Target operand is the bytecode offset of the called function.
          ;; Look up in mvm-to-native-label (bytecode-offset → label, set during init).
          ((= op +op-call+)
           ;; RAW-ADDR-AUDIT: direct call to a named function.  When the
           ;; target offset resolves to a label, emit BL.  When it
           ;; doesn't (undefined function — see e.g. `(%M arg)` in the
           ;; ANSI tests' macrolet-leaked source), we used to emit zero
           ;; bytes and silently fall through — control-flow continued
           ;; into whatever follow-up IR the compiler emitted, with
           ;; junk in V0.  That worked in baseline only by layout
           ;; coincidence; any address shift moved the fall-through
           ;; into something that wedged.  Now: emit SVC #x0511 so the
           ;; undefined-call traps into the active handler-case (which
           ;; the test wrappers always have armed) instead of running
           ;; off into the next instruction.  Same family as the NIL-
           ;; funcall trap in +op-call-ind+ — undefined direct call is
           ;; just the named-target variant.
           (let* ((target-offset (vr 0))
                  (label (gethash target-offset mvm-to-native-label)))
             (cond
               (label
                (let ((idx (a64-current-index buf)))
                  (a64-bl buf 0)  ; placeholder
                  (a64-add-fixup buf idx label :bl)))
               ;; WS4 aarch64 Stage 3: a JIT out-of-module call (synthetic
               ;; runtime offset).  Emit a RELOCATABLE absolute call — a
               ;; MOVZ/MOVK quad loads the callee's real native address into
               ;; x16, then BLR x16.  The 4 imm16 fields are patched at JIT
               ;; time (rt-table → %mvm-resolve-runtime-fn → address).  BLR
               ;; clobbers x30, restored by the enclosing fn's epilogue (same
               ;; as the BL label path).  Args/nargs are already staged by the
               ;; compiler's preceding IR, exactly as for the label path.
               ((and *aarch64-jit-mode* (>= target-offset #x40000000))
                (let ((movz-byte-off (* (- (a64-current-index buf)
                                           (or *aarch64-translated-start-idx* 0))
                                        4)))
                  (push (cons movz-byte-off target-offset) *aarch64-call-relocs*))
                (a64-movz buf +a64-x16+ 0 0)   ; placeholder addr[15:0]  LSL 0
                (a64-movk buf +a64-x16+ 0 1)   ; placeholder addr[31:16] LSL 16
                (a64-movk buf +a64-x16+ 0 2)   ; placeholder addr[47:32] LSL 32
                (a64-movk buf +a64-x16+ 0 3)   ; placeholder addr[63:48] LSL 48
                (a64-blr buf +a64-x16+))
               (t
                (format t "~&  AARCH64 CALL: NO LABEL for target-offset=~D — emitting SVC #x0511 trap~%"
                        target-offset)
                (a64-svc buf #x0511)))))

          ;; ---- CALL-IND Vs ----
          ;; Tag-aware indirect call.  If Vs has its low bit set, it's a
          ;; tagged Lisp value (cons, immediate, object, or forward) —
          ;; not a 4-byte-aligned native fn-addr.  BLR on such a value
          ;; branches to data → UDF → sync handler → longjmp/halt.  We
          ;; preempt that by trapping into the handler-case longjmp path
          ;; via TRAP #x0511, which signals "longjmp now" and the kernel
          ;; recovers cleanly through the active handler-case (test FAILs
          ;; rather than wedges).
          ((= op +op-call-ind+)
           ;; ps is a TAGGED function pointer (low nibble = +tag-function+ = 3),
           ;; per TAG-PLAN.md.  Validate the tag, then strip and BLR.
           ;; Any value with low nibble != 3 (cons=1, object=9, NIL, T,
           ;; fixnum, character, ...) takes the bad-callable path which
           ;; SVCs #x0511 — caught by the kernel's sync-exception
           ;; handler and longjmped through the active handler-case.
           ;;
           ;; The previous untagged-fn-ptr check was TBNZ #0 + CBZ,
           ;; which trapped any low-bit-set value (i.e., anything that
           ;; looked Lisp-tagged).  With function tagging that test
           ;; would fire on every legitimate fn-ptr (tag 3 has bit 0
           ;; set), so it's replaced with an explicit AND-nibble +
           ;; CMP-3 + B.NE-bad sequence.
           ;;
           ;; The compile-funcall NIL guard (compiler.lisp) already
           ;; handles NIL via %signal-undefined-function before this
           ;; IR runs, so this trap should only fire on genuinely
           ;; corrupt callables.
           (let* ((ps (ensure-src (vr 0) +a64-x16+))
                  (ok-label (incf *mvm-label-counter*))
                  (bad-label (incf *mvm-label-counter*)))
             ;; Extract low nibble into x17, compare with +tag-function+ (3).
             ;; Load mask via MOVZ (logical-immediate AND on AArch64 has
             ;; a fiddly imm encoding; register-based AND is clearer).
             (a64-movz buf +a64-x17+ #xF 0)
             (a64-and-reg buf +a64-x17+ ps +a64-x17+)
             (a64-cmp-imm buf +a64-x17+ 3)
             (let ((idx (a64-current-index buf)))
               (a64-emit buf (logior #x54000001     ; B.NE cond=0001
                                     (ash 0 5)))    ; placeholder offset
               (a64-add-fixup buf idx bad-label :bcond))
             ;; Valid tagged function pointer: strip the tag and BLR.
             (a64-sub-imm buf ps ps 3)
             (let ((idx (a64-current-index buf)))
               (a64-b buf 0)
               (a64-add-fixup buf idx ok-label :b))
             ;; Corrupt callable: SVC #x0511 — caught by the kernel's
             ;; sync-exception handler and longjmped through the
             ;; active handler-case.  compile-funcall already filters
             ;; NIL via %signal-undefined-function, so this trap
             ;; should fire only on genuinely garbage callables.
             (a64-set-label buf bad-label)
             (a64-svc buf #x0511)
             (a64-set-label buf ok-label)
             (a64-blr buf ps)))

          ;; ---- RET ----
          ((= op +op-ret+)
           (a64-emit-epilogue buf))

          ;; ---- TAILCALL target:imm32 ----
          ;; Target operand is the bytecode offset of the called function.
          ;; Restore frame, then B (not BL) to target.
          ((= op +op-tailcall+)
           (let* ((target-offset (vr 0))
                  (label (gethash target-offset mvm-to-native-label)))
             (when label
               ;; Deallocate spill/frame-slot area and restore callee-saved regs
               ;; x24/x25/x26 are global state — NOT restored
               (a64-add-imm buf +a64-sp+ +a64-sp+ +a64-locals-frame-size+)
               (a64-ldp-offset buf +a64-x23+ +a64-xzr+ +a64-sp+ 48)
               (a64-ldp-offset buf +a64-x21+ +a64-x22+ +a64-sp+ 32)
               (a64-ldp-offset buf +a64-x19+ +a64-x20+ +a64-sp+ 16)
               (a64-ldp-post buf +a64-x29+ +a64-x30+ +a64-sp+ 80)
               (let ((idx (a64-current-index buf)))
                 (a64-b buf 0)
                 (a64-add-fixup buf idx label :b)))))

          ;; ---- ALLOC-CONS Vd ----
          ;; Bump-allocate 16 bytes, return untagged pointer in Vd
          ((= op +op-alloc-cons+)
           (let* ((vd (vr 0))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (emit-aarch64-gc-mark-start buf)   ; #160: x24 = new cons base
             (emit-aarch64-gc-mark-cons buf)    ; #160 bug#4: mark it CONS-KIND
             (a64-mov-reg buf pd +a64-x24+)
             (a64-add-imm buf +a64-x24+ +a64-x24+ 16)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- GC-CHECK ----
          ;; CMP VA, VL; B.LT ok; BRK #1 (GC trap); ok:
          ((= op +op-gc-check+)
           ;; CMP alloc-ptr (x24) against limit (x25).  When x24 < x25
           ;; (still room) skip the slow path; otherwise call the GC
           ;; trampoline if it's wired up.  Legacy BRK #1 retained as
           ;; a fallback for builds where no trampoline is registered.
           ;;
           ;; Bare-metal puts the heap at low physical addresses
           ;; (0x09000000-0x0F800000) where signed and unsigned compares
           ;; agree, so the historical B.LT (signed) sufficed.  Linux
           ;; userspace puts the mmap'd heap at high addresses
           ;; (~0x7FFF_xxxxxxxx) where the sign bit can flip mid-heap;
           ;; B.LO (unsigned less-than) is the right form there.
           (a64-cmp-reg buf +a64-x24+ +a64-x25+)
           (let ((cc (if *aarch64-linux-mode* +cc-cc+ +cc-lt+)))
             (cond
               ;; Only emit the BL-to-trampoline when BOTH the label is
               ;; bound AND %GC-COLLECT's bytecode offset was found (i.e.
               ;; emit-aarch64-handler-helpers will actually emit the
               ;; trampoline and set its label).  Guarding on the label
               ;; alone produced `undefined label 3` for a module that has
               ;; :gc-check opcodes but no %GC-COLLECT in its function
               ;; table (e.g. the net/actors image, which omits gc.lisp).
               ;; #160 Stage 1: the NATIVE collector needs no %GC-COLLECT
               ;; bytecode offset, so accept it on the label alone.
               ((and *aarch64-gc-trampoline-label*
                     (or *aarch64-gc-native-mcgc*
                         *aarch64-gc-collect-bytecode-offset*))
                (let ((skip-label (incf *mvm-label-counter*)))
                  (let ((idx (a64-current-index buf)))
                    (a64-bcond buf cc 0)
                    (a64-add-fixup buf idx skip-label :bcond))
                  (let ((idx (a64-current-index buf)))
                    (a64-bl buf 0)
                    (a64-add-fixup buf idx *aarch64-gc-trampoline-label* :bl))
                  (a64-set-label buf skip-label)))
               (t
                (a64-bcond buf cc 2)
                (a64-brk buf 1)))))

          ;; ---- MCGC-COLLECT (no operands) ----
          ;; Page pinning is x64-only; on aarch64 this is a no-op (no page pool).
          ((= op +op-mcgc-collect+)
           (a64-nop buf))

          ;; ---- WRITE-BARRIER Vobj ----
          ;; Mark the card table entry dirty (simplified: just a DMB for now)
          ((= op +op-write-barrier+)
           ;; In a real implementation this would compute the card table
           ;; offset and store a dirty byte. For now, emit a memory barrier.
           (a64-dmb buf #xB))

          ;; ---- SAVE-CTX Vd ----
          ;; Real setjmp semantics for actor context switching.
          ;; Vd holds save area address (untagged by compiler).
          ;; Returns 0 (initial save) or 2 (resumed via restore-ctx).
          ;; Extra callee-saved (x20-x23, x29, x30) are pushed to the stack
          ;; so they're recovered when SP is restored. Only essential state
          ;; (SP, x24, x25, x19, continuation, obj-alloc/limit) goes in save area.
          ((= op +op-save-ctx+)
           (let* ((vd (vr 0))
                  (pa (ensure-src vd +a64-x0+)))
             ;; 1. Push extra callee-saved to stack (48 bytes, 16-byte aligned)
             (a64-stp-pre buf +a64-x20+ +a64-x21+ +a64-sp+ -48)
             (a64-stp-offset buf +a64-x22+ +a64-x23+ +a64-sp+ 16)
             (a64-stp-offset buf +a64-x29+ +a64-x30+ +a64-sp+ 32)
             ;; 2. Save SP (post-push) to save area [pa+0x00]
             (a64-add-imm buf +a64-x16+ +a64-sp+ 0)   ; MOV x16, SP
             (a64-str-unsigned buf +a64-x16+ pa 0)     ; [pa+0x00] = SP
             ;; 3. Save key registers to save area
             (a64-str-unsigned buf +a64-x24+ pa 8)     ; [pa+0x08] = x24 (alloc ptr)
             (a64-str-unsigned buf +a64-x25+ pa 16)    ; [pa+0x10] = x25 (alloc limit)
             (a64-str-unsigned buf +a64-x19+ pa 24)    ; [pa+0x18] = x19 (V4)
             ;; 4. ADR x17 → continuation; store to [pa+0x28]
             (let ((adr-idx (a64-current-index buf)))
               (a64-emit buf 0)                         ; placeholder for ADR x17
               (a64-str-unsigned buf +a64-x17+ pa #x28) ; [pa+0x28] = continuation
               ;; 5. Save per-CPU obj-alloc/obj-limit from TPIDR_EL1
               (a64-mrs buf +a64-x16+ +sysreg-tpidr-el1+)
               (a64-ldr-unsigned buf +a64-x17+ +a64-x16+ #x28)
               (a64-str-unsigned buf +a64-x17+ pa #x68)  ; [pa+0x68] = obj-alloc
               (a64-ldr-unsigned buf +a64-x17+ +a64-x16+ #x30)
               (a64-str-unsigned buf +a64-x17+ pa #x70)  ; [pa+0x70] = obj-limit
               ;; 6. Initial save: return 0
               (a64-movz buf +a64-x0+ 0 0)
               ;; 7. B to pop (skip resume entry)
               (let ((b-idx (a64-current-index buf)))
                 (a64-b buf 0)                           ; placeholder
                 ;; 8. Continuation label — restore-ctx BR's here
                 (let ((cont-idx (a64-current-index buf)))
                   ;; Patch ADR x17, <continuation>
                   ;; ADR: immlo(2)|10000|immhi(19)|Rd(5)
                   (let* ((byte-off (* (- cont-idx adr-idx) 4))
                          (immlo (logand byte-off 3))
                          (immhi (logand (ash byte-off -2) #x7FFFF)))
                     (setf (aref (a64-buffer-code buf) adr-idx)
                           (logior (ash immlo 29)
                                   (ash #b10000 24)
                                   (ash immhi 5)
                                   +a64-x17+)))
                   ;; Resume path: return 2 (tagged fixnum 1)
                   (a64-movz buf +a64-x0+ 2 0)
                   ;; 9. Pop callee-saved (both paths converge here)
                   (let ((pop-idx (a64-current-index buf)))
                     ;; Patch B forward to here
                     (setf (aref (a64-buffer-code buf) b-idx)
                           (logior (ash #b000101 26)
                                   (logand (- pop-idx b-idx) #x3FFFFFF)))
                     (a64-ldp-offset buf +a64-x29+ +a64-x30+ +a64-sp+ 32)
                     (a64-ldp-offset buf +a64-x22+ +a64-x23+ +a64-sp+ 16)
                     (a64-ldp-post buf +a64-x20+ +a64-x21+ +a64-sp+ 48)
                     ;; 10. Store result (x0) into Vd
                     (store-dst +a64-x0+ vd)))))))

          ;; ---- RESTORE-CTX Vd ----
          ;; Real longjmp semantics. Restores registers from save area,
          ;; switches SP, releases scheduler lock, enables interrupts,
          ;; then BR to saved continuation. Never returns.
          ((= op +op-restore-ctx+)
           (let* ((vd (vr 0))
                  (pa (ensure-src vd +a64-x0+)))
             ;; Move addr to x16 (pa may be x19/x24/x25 which get overwritten)
             (a64-mov-reg buf +a64-x16+ pa)
             ;; Load continuation address into x17
             (a64-ldr-unsigned buf +a64-x17+ +a64-x16+ #x28)
             ;; Restore per-CPU obj-alloc/obj-limit via TPIDR_EL1
             (a64-mrs buf +a64-x0+ +sysreg-tpidr-el1+)
             (a64-ldr-unsigned buf +a64-x1+ +a64-x16+ #x68)
             (a64-str-unsigned buf +a64-x1+ +a64-x0+ #x28)  ; TPIDR+0x28 = obj-alloc
             (a64-ldr-unsigned buf +a64-x1+ +a64-x16+ #x70)
             (a64-str-unsigned buf +a64-x1+ +a64-x0+ #x30)  ; TPIDR+0x30 = obj-limit
             ;; Restore callee-saved from save area
             (a64-ldr-unsigned buf +a64-x19+ +a64-x16+ #x18)
             (a64-ldr-unsigned buf +a64-x24+ +a64-x16+ #x08)
             (a64-ldr-unsigned buf +a64-x25+ +a64-x16+ #x10)
             ;; Restore SP from save area (AFTER register loads, SP change is the
             ;; "point of no return" — we're now on the resumed actor's stack)
             (a64-ldr-unsigned buf +a64-x0+ +a64-x16+ 0)
             (a64-add-imm buf +a64-sp+ +a64-x0+ 0)     ; MOV SP, x0
             ;; Release scheduler lock (MUST be after SP switch to prevent
             ;; another CPU from dequeuing this actor while on its stack)
             (when *aarch64-sched-lock-addr*
               (a64-load-imm64 buf +a64-x0+ *aarch64-sched-lock-addr*)
               (a64-str-unsigned buf +a64-xzr+ +a64-x0+ 0) ; store 0 → unlock
               ;; Memory barrier — ensure lock release visible to other CPUs
               (a64-dmb buf #xB)
               ;; Enable interrupts
               (a64-msr-daifclr buf #x3))
             ;; Jump to continuation (save-ctx's resume entry point)
             (a64-br buf +a64-x17+)))

          ;; ---- YIELD ----
          ;; Preemption check point (emitted at end of every loop iteration).
          ;; QEMU virt: SEV+WFE (SEV sets event register, WFE sees it and
          ;; returns immediately — lets QEMU process events).
          ;; QEMU raspi3b: NOP (WFE halts CPU with no wake source).
          ((= op +op-yield+)
           (if *aarch64-yield-nop*
               (a64-nop buf)
               (progn
                 (a64-emit buf #xD503209F)   ; SEV (send event)
                 (a64-wfe buf))))

          ;; ---- ATOMIC-XCHG Vd, Vaddr, Vs ----
          ;; LDXR/STXR loop for atomic exchange.
          ;; STXR Ws, Xt, [Xn] requires Ws ≠ Xt and Ws ≠ Xn (ARM spec).
          ;; Pick status register that doesn't conflict with pa or ps.
          ((= op +op-atomic-xchg+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (ps (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+))
                  ;; Status reg must differ from ps (Xt) and pa (Xn)
                  (status (cond ((/= ps +a64-x17+) +a64-x17+)
                                ((/= pa 15) 15)     ; x15
                                (t +a64-x0+)))
                  (loop-idx (a64-current-index buf)))
             ;; loop: LDXR Xd, [Vaddr]
             (a64-ldxr buf pd pa)
             ;; STXR Ws, Vs, [Vaddr]
             (a64-stxr buf status ps pa)
             ;; CBNZ Ws, loop (32-bit variant, sf=0)
             (let ((back-offset (- loop-idx (a64-current-index buf))))
               (a64-emit buf (logior #x35000000
                                     (ash (logand back-offset #x7FFFF) 5)
                                     status)))
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- IO-READ Vd, port:imm16, width:imm8 ----
          ;; On AArch64, MMIO: load the address (port as MMIO base + offset)
          ;; For bare-metal, port is treated as an absolute MMIO address
          ((= op +op-io-read+)
           (let* ((vd (vr 0))
                  (port (vr 1))
                  (width (vr 2))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-load-imm64 buf +a64-x17+ port)
             (a64-ldr-width buf pd +a64-x17+ 0 width)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- IO-WRITE port:imm16, Vs, width:imm8 ----
          ((= op +op-io-write+)
           (let ((port (vr 0))
                 (ps (ensure-src (vr 1) +a64-x17+))
                 (width (vr 2)))
             (a64-load-imm64 buf +a64-x16+ port)
             (a64-str-width buf ps +a64-x16+ 0 width)))

          ;; ---- HALT ----
          ;; WFI for idle scheduler loop. Wakes on interrupt (SGI/timer).
          ;; Semihosting exit is handled by a separate shutdown() function.
          ((= op +op-halt+)
           (a64-wfi buf))

          ;; ---- CLI (disable interrupts) ----
          ((= op +op-cli+)
           ;; MSR DAIFSet, #0x3  (mask IRQ + FIQ)
           (a64-msr-daifset buf #x3))

          ;; ---- STI (enable interrupts) ----
          ((= op +op-sti+)
           ;; MSR DAIFClr, #0x3  (unmask IRQ + FIQ)
           (a64-msr-daifclr buf #x3))

          ;; ---- PERCPU-REF Vd, offset:imm16 ----
          ;; Read per-CPU data via TPIDR_EL1
          ((= op +op-percpu-ref+)
           (let* ((vd (vr 0))
                  (offset (vr 1))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-mrs buf +a64-x17+ +sysreg-tpidr-el1+)  ; system reg encoding for TPIDR_EL1
             ;; LDR Xd, [x17, #offset]
             (if (and (zerop (mod offset 8)) (<= offset (* #xFFF 8)))
                 (a64-ldr-unsigned buf pd +a64-x17+ offset)
                 (progn
                   (a64-load-imm64 buf +a64-x16+ offset)
                   (a64-add-reg buf +a64-x17+ +a64-x17+ +a64-x16+ 0 0)
                   (a64-ldur buf pd +a64-x17+ 0)))
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- PERCPU-SET offset:imm16, Vs ----
          ((= op +op-percpu-set+)
           (let ((offset (vr 0))
                 (ps (ensure-src (vr 1) +a64-x17+)))
             (a64-mrs buf +a64-x16+ +sysreg-tpidr-el1+)
             (if (and (zerop (mod offset 8)) (<= offset (* #xFFF 8)))
                 (a64-str-unsigned buf ps +a64-x16+ offset)
                 (progn
                   (a64-load-imm64 buf +a64-x17+ offset)
                   (a64-add-reg buf +a64-x16+ +a64-x16+ +a64-x17+ 0 0)
                   (a64-stur buf ps +a64-x16+ 0)))))

          ;; ---- FN-ADDR Vd, target:imm32 ----
          ;; Load raw function address. Target is bytecode offset.
          ;;
          ;; Emits a MOVZ + MOVK pair (placeholder imm16=0 in both)
          ;; that loads a 32-bit absolute address.  The patch list
          ;; records the byte offset so cross.lisp can write the
          ;; actual address into the imm16 fields after image
          ;; assembly (when load_addr + native_image_offset +
          ;; target_native_offset is known).
          ;;
          ;; Replaces the original ADR (PC-relative ±1 MB), which
          ;; truncated for any function more than 1 MB from the
          ;; call site — the cause of layout-fragility bugs on
          ;; the 38 MB ANSI build.
          ((= op +op-fn-addr+)
           ;; RAW-ADDR-AUDIT: produces a RAW native function address
           ;; in Vd (low bit 0, no Lisp tag).  Callers must treat the
           ;; value as opaque-and-untagged.  In particular: storing it
           ;; into a closure slot is fine (subtag #x52 obj-set just
           ;; copies the bytes; obj-ref pulls them out for call-ind),
           ;; but storing it via mem-ref :u64 would silently SHL it by
           ;; 1 and any subsequent mem-ref :u64 read would interpret
           ;; the bits as a Lisp fixnum (= addr/2) — same trap the GC
           ;; trampoline metadata stash had.  Any future code that
           ;; flows fn-addr through :u64 storage must apply the
           ;; LSL/ASR-by-1 convention (see emit-aarch64-handler-helpers
           ;; for the canonical example).
           (let* ((vd (vr 0))
                  (target-offset (vr 1))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (cond
               ((gethash target-offset mvm-to-native-label)
                ;; Record the byte position of the MOVZ so the patcher
                ;; can find both MOVZ (movz-pos) and MOVK (movz-pos+4).
                ;; Position is RELATIVE to the start of translated code
                ;; (so the patcher's `native-image-offset + movz-byte-pos`
                ;; arithmetic works whether or not the buffer was shared
                ;; with a boot preamble — Phase 2 unification).
                (let ((movz-byte-pos
                       (* (- (a64-current-index buf)
                             (or *aarch64-translated-start-idx* 0))
                          4)))
                  (push (cons movz-byte-pos target-offset)
                        *aarch64-fn-addr-patches*))
                ;; MOVZ Xd, #0 (placeholder for low 16 bits)
                (a64-movz buf pd 0 0)
                ;; MOVK Xd, #0, lsl 16 (placeholder for high 16 bits)
                (a64-movk buf pd 0 1))
               ;; WS4 aarch64 Stage 4: a JIT out-of-module #'NAME value-load
               ;; (synthetic runtime offset).  Emit a FULL MOVZ/MOVK quad so the
               ;; complete 64-bit TAGGED fn word fits, and record the site in
               ;; *aarch64-fn-addr-relocs*; the JIT driver resolves it (rt-table
               ;; → %mvm-resolve-runtime-fn → %val->word fn, tagged) and patches
               ;; the 4 imm16 fields.  Without this the whole-image (t) branch
               ;; below would load 0 (a NIL sentinel) for an out-of-module #'fn.
               ((and *aarch64-jit-mode* (>= target-offset #x40000000))
                (let ((movz-byte-off (* (- (a64-current-index buf)
                                           (or *aarch64-translated-start-idx* 0))
                                        4)))
                  (push (cons movz-byte-off target-offset) *aarch64-fn-addr-relocs*))
                (a64-movz buf pd 0 0)   ; placeholder addr[15:0]  LSL 0
                (a64-movk buf pd 0 1)   ; placeholder addr[31:16] LSL 16
                (a64-movk buf pd 0 2)   ; placeholder addr[47:32] LSL 32
                (a64-movk buf pd 0 3))  ; placeholder addr[63:48] LSL 48
               (t
                ;; Unknown target: load 0 (NIL/sentinel)
                (a64-movz buf pd 0 0)
                ;; Pad with NOP so all fn-addr sites are 8 bytes.
                (a64-emit buf #xD503201F)))
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- SET-NARGS imm8 ----
          ;; Store the immediate nargs at the fixed convention slot
          ;; 0x10000150 (matches x64).  Callees with &rest read it via
          ;; GET-NARGS to know how many args the caller passed.
          ;; Sequence:
          ;;   mov w16, #nargs
          ;;   mov x17, #0x10000150
          ;;   str w16, [x17]
          ((= op +op-set-nargs+)
           (let ((n (vr 0)))
             (a64-movz buf +a64-x16+ (logand n #xFFFF) 0)
             (a64-load-imm64 buf +a64-x17+ #x10000150)
             (a64-str-width buf +a64-x16+ +a64-x17+ 0 2)))  ; size=2 = 32-bit STR

          ;; ---- GET-NARGS Vd ----
          ;; Load 32-bit from slot 0x10000150 into Vd, tagged as fixnum.
          ((= op +op-get-nargs+)
           (let* ((vd (vr 0))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-load-imm64 buf +a64-x17+ #x10000150)
             (a64-ldr-width buf pd +a64-x17+ 0 2)  ; size=2 = 32-bit LDR (zero-extends)
             (a64-lsl-imm buf pd pd 1)              ; tag as fixnum (shl 1)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- ALLOC-STRING Vd Vcount ----
          ;; Same shape as alloc-array but with subtag #x31 (string).
          ;; tag=9 layout: header(8)+padding(8)+char_slots.
          ;; Header = (count << 8) | #x31.  Tagged = alloc_ptr + 9.
          ;; Total bytes = (count+2)*8 round-up-16 = floor((count+3)/2)*16.
          ((= op +op-alloc-string+)
           (let* ((vd (vr 0))
                  (pcount (ensure-src (vr 1) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (emit-aarch64-gc-mark-start buf)   ; #160: x24 = new string base
             (a64-lsl-imm buf +a64-x16+ pcount 8)
             (a64-movz buf +a64-x9+ #x31 0)
             (a64-orr-reg buf +a64-x16+ +a64-x16+ +a64-x9+)
             (a64-stur buf +a64-x16+ +a64-x24+ 0)
             (a64-add-imm buf +a64-x17+ pcount 3)
             (a64-lsr-imm buf +a64-x17+ +a64-x17+ 1)
             (a64-lsl-imm buf +a64-x17+ +a64-x17+ 4)
             (a64-add-imm buf pd +a64-x24+ 9)
             (a64-add-reg buf +a64-x24+ +a64-x24+ +a64-x17+ 0 0)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- SET-MV-COUNT imm8 ----
          ;; Store tagged fixnum count to slot 0x10000090 (MV-COUNT).
          ;; Tagged value = count<<1 to match compile-values.
          ((= op +op-set-mv-count+)
           (let* ((count (vr 0))
                  (tagged (ash count 1)))
             (a64-load-imm64 buf +a64-x16+ tagged)
             (a64-load-imm64 buf +a64-x17+ #x10000090)
             (a64-str-unsigned buf +a64-x16+ +a64-x17+ 0)))

          ;; ---- SET-CENV Vs ----
          ;; Store the closure env-list into the dedicated closure-env
          ;; register x27.  Emitted only by compile-funcall's closure
          ;; path right before the BLR; callee reads x27 via GET-CENV at
          ;; entry.  x27 is callee-saved per AAPCS and is not touched by
          ;; the handler-stack PUSH/POP helpers — so it survives any
          ;; SETJMP/CLEAR-HANDLER BL that may sit between the funcall
          ;; site and the closure body's GET-CENV.
          ((= op +op-set-cenv+)
           (let* ((ps (ensure-src (vr 0) +a64-x16+)))
             (a64-mov-reg buf +a64-x27+ ps)))

          ;; ---- GET-CENV Vd ----
          ;; Snapshot the closure-env register x27 into a local at
          ;; closure body entry.  After this point the env is in a
          ;; frame slot / vreg and x27 is free to be re-set for any
          ;; further closure call.
          ((= op +op-get-cenv+)
           (let* ((vd (vr 0))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-mov-reg buf pd +a64-x27+)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ============================================================
          ;; IEEE float opcodes (FP64).  Mirror translate-x64.lisp.
          ;;
          ;; Float object layout:
          ;;   tagged ptr = raw + 9 (obj tag)
          ;;   [raw + 0]  header = #x260 (count=2 | subtag #x60)
          ;;   [raw + 8]  unused (alignment padding)
          ;;   [raw + 16] slot 0: hi32 sign-extended<<1 (tagged fixnum)
          ;;   [raw + 24] slot 1: lo32 zero-extended<<1 (tagged fixnum)
          ;; OBJ-REF idx*8+7 from tagged pointer reaches slot 0 at +7,
          ;; slot 1 at +15.
          ;;
          ;; Used scratch: x9..x11 (volatile), D0/D1 (FP scratch).
          ;; ============================================================

          ;; Helper: load float object bits (Vs is tagged ptr) into D-reg.
          ;; Generated inline for each opcode below.  Sequence:
          ;;   LDUR x9,  [ps, #7]    ; tagged hi32<<1
          ;;   LSR  x9,  x9, #1      ; untag (logical for hi32 sign bit handling — see split)
          ;;   LSL  x9,  x9, #32     ; into upper half
          ;;   LDUR x10, [ps, #15]   ; tagged lo32<<1
          ;;   LSR  x10, x10, #1     ; untag
          ;;   AND  x10, x10, #0xFFFFFFFF
          ;;   ORR  x9,  x9, x10     ; combine into 64-bit float bit pattern
          ;;   FMOV Dx,  x9          ; bits → FP reg
          ;;
          ;; NOTE on sign-extension: slot 0 was stored as (hi32 << 1)
          ;; where hi32 was the upper 32 bits of the float's bit pattern.
          ;; x64 uses SAR (arithmetic) then shifts to discard.  AArch64
          ;; uses LSR (logical) since we'll mask anyway.  Both work.

          ;; ---- FADD / FSUB / FMUL / FDIV Vd, Va, Vb ----
          ((or (= op +op-fadd+) (= op +op-fsub+)
               (= op +op-fmul+) (= op +op-fdiv+))
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             ;; Load Va float bits → D0
             (a64-ldur buf +a64-x9+  pa 7)
             (a64-lsr-imm buf +a64-x9+ +a64-x9+ 1)
             (a64-lsl-imm buf +a64-x9+ +a64-x9+ 32)
             (a64-ldur buf +a64-x10+ pa 15)
             (a64-lsr-imm buf +a64-x10+ +a64-x10+ 1)
             ;; AND x10, x10, #0xFFFFFFFF  via LSL 32 / LSR 32
             (a64-lsl-imm buf +a64-x10+ +a64-x10+ 32)
             (a64-lsr-imm buf +a64-x10+ +a64-x10+ 32)
             (a64-orr-reg buf +a64-x9+ +a64-x9+ +a64-x10+)
             (a64-fmov-d-x buf 0 +a64-x9+)             ; D0 ← Va bits
             ;; Load Vb float bits → D1
             (a64-ldur buf +a64-x9+  pb 7)
             (a64-lsr-imm buf +a64-x9+ +a64-x9+ 1)
             (a64-lsl-imm buf +a64-x9+ +a64-x9+ 32)
             (a64-ldur buf +a64-x10+ pb 15)
             (a64-lsr-imm buf +a64-x10+ +a64-x10+ 1)
             (a64-lsl-imm buf +a64-x10+ +a64-x10+ 32)
             (a64-lsr-imm buf +a64-x10+ +a64-x10+ 32)
             (a64-orr-reg buf +a64-x9+ +a64-x9+ +a64-x10+)
             (a64-fmov-d-x buf 1 +a64-x9+)             ; D1 ← Vb bits
             ;; Perform op: Dout ← D0 op D1
             (cond ((= op +op-fadd+) (a64-fadd-d buf 0 0 1))
                   ((= op +op-fsub+) (a64-fsub-d buf 0 0 1))
                   ((= op +op-fmul+) (a64-fmul-d buf 0 0 1))
                   (t                (a64-fdiv-d buf 0 0 1)))
             ;; D0 bits → x9
             (a64-fmov-x-d buf +a64-x9+ 0)
             ;; Allocate fresh 2-slot float at x24 (alloc ptr).
             ;; Header = (2<<8)|#x60 = #x260.
             (a64-movz buf +a64-x10+ #x260 0)
             (a64-stur buf +a64-x10+ +a64-x24+ 0)
             ;; Slot 0 = (hi32 sign-ext) << 1.  hi32 = x9 >>> 32 (arith).
             (a64-asr-imm buf +a64-x10+ +a64-x9+ 32)
             (a64-lsl-imm buf +a64-x10+ +a64-x10+ 1)
             (a64-stur buf +a64-x10+ +a64-x24+ 16)
             ;; Slot 1 = (lo32 zero-ext) << 1.
             (a64-lsl-imm buf +a64-x10+ +a64-x9+ 32)
             (a64-lsr-imm buf +a64-x10+ +a64-x10+ 32)
             (a64-lsl-imm buf +a64-x10+ +a64-x10+ 1)
             (a64-stur buf +a64-x10+ +a64-x24+ 24)
             ;; Tagged result = x24 + 9; advance x24 by 32.
             (a64-add-imm buf pd +a64-x24+ 9)
             (a64-add-imm buf +a64-x24+ +a64-x24+ 32)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- ITOF Vd, Vs ----
          ;; Tagged integer (Vs<<1) → freshly-allocated float object.
          ((= op +op-itof+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             ;; Untag: x9 = ps >> 1 (signed)
             (a64-asr-imm buf +a64-x9+ ps 1)
             ;; SCVTF D0, x9
             (a64-scvtf-d-x buf 0 +a64-x9+)
             ;; D0 bits → x9
             (a64-fmov-x-d buf +a64-x9+ 0)
             ;; Allocate 2-slot float (same tail as fadd)
             (a64-movz buf +a64-x10+ #x260 0)
             (a64-stur buf +a64-x10+ +a64-x24+ 0)
             (a64-asr-imm buf +a64-x10+ +a64-x9+ 32)
             (a64-lsl-imm buf +a64-x10+ +a64-x10+ 1)
             (a64-stur buf +a64-x10+ +a64-x24+ 16)
             (a64-lsl-imm buf +a64-x10+ +a64-x9+ 32)
             (a64-lsr-imm buf +a64-x10+ +a64-x10+ 32)
             (a64-lsl-imm buf +a64-x10+ +a64-x10+ 1)
             (a64-stur buf +a64-x10+ +a64-x24+ 24)
             (a64-add-imm buf pd +a64-x24+ 9)
             (a64-add-imm buf +a64-x24+ +a64-x24+ 32)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- FTOI Vd, Vs ----
          ;; Float object → tagged integer (truncate toward zero).
          ((= op +op-ftoi+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             ;; Reassemble float bits → D0
             (a64-ldur buf +a64-x9+  ps 7)
             (a64-lsr-imm buf +a64-x9+ +a64-x9+ 1)
             (a64-lsl-imm buf +a64-x9+ +a64-x9+ 32)
             (a64-ldur buf +a64-x10+ ps 15)
             (a64-lsr-imm buf +a64-x10+ +a64-x10+ 1)
             (a64-lsl-imm buf +a64-x10+ +a64-x10+ 32)
             (a64-lsr-imm buf +a64-x10+ +a64-x10+ 32)
             (a64-orr-reg buf +a64-x9+ +a64-x9+ +a64-x10+)
             (a64-fmov-d-x buf 0 +a64-x9+)
             ;; FCVTZS x9, D0
             (a64-fcvtzs-x-d buf +a64-x9+ 0)
             ;; Tag as fixnum: x9 << 1
             (a64-lsl-imm buf pd +a64-x9+ 1)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- FCMP Va, Vb ----
          ;; Sets NZCV; subsequent :beq/:blt/:bgt operate on FP flags.
          ((= op +op-fcmp+)
           (let* ((pa (ensure-src (vr 0) +a64-x16+))
                  (pb (ensure-src (vr 1) +a64-x17+)))
             ;; Reassemble Va float bits → D0
             (a64-ldur buf +a64-x9+  pa 7)
             (a64-lsr-imm buf +a64-x9+ +a64-x9+ 1)
             (a64-lsl-imm buf +a64-x9+ +a64-x9+ 32)
             (a64-ldur buf +a64-x10+ pa 15)
             (a64-lsr-imm buf +a64-x10+ +a64-x10+ 1)
             (a64-lsl-imm buf +a64-x10+ +a64-x10+ 32)
             (a64-lsr-imm buf +a64-x10+ +a64-x10+ 32)
             (a64-orr-reg buf +a64-x9+ +a64-x9+ +a64-x10+)
             (a64-fmov-d-x buf 0 +a64-x9+)
             ;; Reassemble Vb float bits → D1
             (a64-ldur buf +a64-x9+  pb 7)
             (a64-lsr-imm buf +a64-x9+ +a64-x9+ 1)
             (a64-lsl-imm buf +a64-x9+ +a64-x9+ 32)
             (a64-ldur buf +a64-x10+ pb 15)
             (a64-lsr-imm buf +a64-x10+ +a64-x10+ 1)
             (a64-lsl-imm buf +a64-x10+ +a64-x10+ 32)
             (a64-lsr-imm buf +a64-x10+ +a64-x10+ 32)
             (a64-orr-reg buf +a64-x9+ +a64-x9+ +a64-x10+)
             (a64-fmov-d-x buf 1 +a64-x9+)
             (a64-fcmp-d buf 0 1)))

          ;; ---- Unknown opcode ----
          (t
           ;; Emit a BRK with the opcode as immediate for debugging
           (a64-brk buf op)))))))

;;; ============================================================
;;; Per-fork handler-stack helpers
;;; ============================================================
;;;
;;; Mirrors SBCL's binding stack (BSP) and CCL's catch-frame linked
;;; list: per-fork handler-case state is saved/restored across a
;;; dedicated stack rather than being a single global slot.
;;;
;;; Memory layout (heap, low-1GB identity-mapped on bare metal):
;;;   0x10000180/188/190  = CURRENT handler state (SP / FP / IP)
;;;                          — what SETJMP writes, LONGJMP reads.
;;;   0x10010000          = handler-stack depth (pseudo-BSP).
;;;   0x10010008 + 24*N   = frame N = (saved-SP, saved-FP, saved-IP).
;;;
;;; PUSH (called by SETJMP just before it overwrites slot 180/188/190):
;;;   save current 180/188/190 triple to frame[depth], depth += 1.
;;;
;;; POP (called by CLEAR-HANDLER on normal completion AND by LONGJMP
;;; after it has copied 180/188/190 into scratch — pop discards the
;;; just-restored outer frame from the stack so a future SETJMP doesn't
;;; double-push.  Also called by the boot IRQ entries when an IRQ-caught
;;; wedge skips CLEAR-HANDLER):
;;;   if depth == 0: zero slot 180/188/190 and return.
;;;   else: depth -= 1; reload slot 180/188/190 from frame[depth].
;;;
;;; Both helpers are leaf functions: clobber x9..x13, preserve x30 (LR).

(defun emit-aarch64-code-bounds-init (buf)
  "Emit the boot-stub init block that records code_base and code_end
   into fixed slots 0x10000160 / 0x10000168.  Mirrors the x64 helper
   emit-code-bounds-init in translate-x64.lisp.

   Without this block, functionp's code-range check (cl-eval.lisp)
   sees zero in both slots, short-circuits past the in-code arm, and
   falls through to characterp/stringp/etc. — which can misclassify
   raw fn-addrs (low byte 0x05) as characters and return NIL.

   Records *aarch64-code-base-patch-offset* and -end-patch-offset as
   *byte* offsets so cross.lisp can fill in the actual addresses
   post-link.  Each value loads via MOVZ + MOVK (32-bit absolute,
   same convention as fn-addr patches) into x16, then stores via
   STR through x17 that holds the slot address."
  ;; a64-buffer-position is in 32-bit instruction units; convert to bytes
  ;; so the cross.lisp patcher (which works in bytes) finds the right MOVZ.
  ;; ---- code_base ----
  (setf *aarch64-code-base-patch-offset* (* (a64-buffer-position buf) 4))
  (a64-movz buf +a64-x16+ 0 0)              ; placeholder (lo16)
  (a64-movk buf +a64-x16+ 0 1)              ; placeholder (hi16 lsl 16)
  (a64-load-imm64 buf +a64-x17+ #x10000160)
  (a64-str-unsigned buf +a64-x16+ +a64-x17+ 0)
  ;; ---- code_end ----
  (setf *aarch64-code-end-patch-offset* (* (a64-buffer-position buf) 4))
  (a64-movz buf +a64-x16+ 0 0)              ; placeholder (lo16)
  (a64-movk buf +a64-x16+ 0 1)              ; placeholder (hi16 lsl 16)
  (a64-load-imm64 buf +a64-x17+ #x10000168)
  (a64-str-unsigned buf +a64-x16+ +a64-x17+ 0))

;;; ============================================================
;;; WS4-AA64 #160 STAGE 1: NATIVE Cheney GC trampoline
;;; ============================================================
;;; Mirrors x64 emit-gc-trampoline (translate-x64.lisp).  ZERO Lisp allocation
;;; during collection → structurally cannot re-enter the gc-check nor route
;;; through the bignum engine (the Lisp-collector failure classes).  Consumes
;;; the SAME bitmaps Stage-B/a6d12ae built: object-start @0x10000E18 (validated
;;; in scan_word, set on survivors in copy_object).  cons-kind kind-check +
;;; leaf-subtag type-aware skip are STAGE 2.
(defconstant +a64-x28+ 28)   ; free scratch (unused by Modus codegen); GC obj-bitmap base

(defvar *aarch64-gc-native-mcgc* nil
  "WS4-AA64 #160 Stage 1.  When non-nil, emit-aarch64-handler-helpers emits the
   NATIVE Cheney GC trampoline (below) at *aarch64-gc-trampoline-label* instead
   of the Lisp-%gc-collect-calling one.  Default nil → the Lisp path is emitted
   (byte-identical to pre-Stage-1 for gate/bare-metal).")

(defun a64-ldrb (buf wt xn)
  "LDRB Wt, [Xn, #0]  (assembler-verified 0x39400000|Xn<<5|Wt)."
  (a64-emit buf (logior #x39400000 (ash xn 5) wt)))
(defun a64-strb (buf wt xn)
  "STRB Wt, [Xn, #0]  (0x39000000|Xn<<5|Wt)."
  (a64-emit buf (logior #x39000000 (ash xn 5) wt)))
(defun a64-lslv (buf rd rn rm)
  "LSLV Xd, Xn, Xm  (0x9AC02000|Xm<<16|Xn<<5|Xd)."
  (a64-emit buf (logior #x9AC02000 (ash rm 16) (ash rn 5) rd)))

(defun emit-aarch64-native-gc-trampoline (buf)
  "Native Cheney copying collector.  Layout: label → B main → [scan_word] →
   [copy_object] → main.  GC registers (all mutator regs saved in a 240B frame):
     x19=from_start x20=from_end x21=free_ptr x22=to_start x23=stack_base
     x25=space_size x26=loop_var x27=page_base x28=obj_bitmap_base ; x9..x17=scratch
   All GC metadata (0x40..0x58) + bitmap config (0xE00/0xE18) are stored value<<1,
   so LOAD+ASR#1 recovers raw and STORE writes <<1.  x24/x25 (mutator VA/VL) are
   set to the new alloc-ptr / limit at exit; all other regs restored."
  (a64-set-label buf *aarch64-gc-trampoline-label*)
  (let ((scan-word (incf *mvm-label-counter*))
        (copy-obj  (incf *mvm-label-counter*))
        (main      (incf *mvm-label-counter*)))
    ;; entry: jump over the subroutines to main
    (let ((i (a64-current-index buf))) (a64-b buf 0) (a64-add-fixup buf i main :b))

    ;; ============ SUBROUTINE scan_word (x9 = ADDRESS of word) ============
    (a64-set-label buf scan-word)
    (a64-ldr-unsigned buf +a64-x10+ +a64-x9+ 0)       ; x10 = value
    (a64-lsr-imm buf +a64-x12+ +a64-x10+ 4)
    (a64-lsl-imm buf +a64-x12+ +a64-x12+ 4)            ; x12 = raw = val & ~15
    (a64-sub-reg buf +a64-x11+ +a64-x10+ +a64-x12+ 0 0) ; x11 = tag = val - raw
    (let ((maybe (incf *mvm-label-counter*))
          (sret  (incf *mvm-label-counter*)))
      (a64-cmp-imm buf +a64-x11+ 1)
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-eq+ 0) (a64-add-fixup buf i maybe :bcond))
      (a64-cmp-imm buf +a64-x11+ 9)
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-eq+ 0) (a64-add-fixup buf i maybe :bcond))
      (a64-ret buf)                                    ; not a pointer
      (a64-set-label buf maybe)
      ;; in from-space?  from_start(x19) <= raw(x12) < from_end(x20)
      (a64-cmp-reg buf +a64-x12+ +a64-x19+)
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-cc+ 0) (a64-add-fixup buf i sret :bcond)) ; raw<from_start
      (a64-cmp-reg buf +a64-x12+ +a64-x20+)
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-cs+ 0) (a64-add-fixup buf i sret :bcond)) ; raw>=from_end
      ;; object-start bitmap validate: gran=(raw-page)>>4 ; byte=objbmp+gran>>3 ; bit=gran&7
      (a64-sub-reg buf +a64-x13+ +a64-x12+ +a64-x27+ 0 0)
      (a64-lsr-imm buf +a64-x14+ +a64-x13+ 7)
      (a64-add-reg buf +a64-x14+ +a64-x28+ +a64-x14+ 0 0)  ; byte addr
      (a64-lsr-imm buf +a64-x13+ +a64-x13+ 4)              ; granule
      (a64-movz buf +a64-x17+ 7 0) (a64-and-reg buf +a64-x13+ +a64-x13+ +a64-x17+) ; bit idx
      (a64-ldrb buf +a64-x15+ +a64-x14+)
      (a64-movz buf +a64-x16+ 1 0) (a64-lslv buf +a64-x16+ +a64-x16+ +a64-x13+)    ; mask=1<<bit
      (a64-ands-reg buf +a64-xzr+ +a64-x15+ +a64-x16+)     ; TST byte & mask
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-eq+ 0) (a64-add-fixup buf i sret :bcond)) ; obj-start bit clear → skip
      ;; ---- CONS-KIND cross-check (x64-parity: emit-mcgc-cons-kind-or-jump) ----
      ;; A tag-9 (object) candidate whose target is a CONS (cons-kind bit SET), or a
      ;; tag-1 (cons) candidate whose target is NOT a cons (cons-kind CLEAR), is a
      ;; conservative FALSE POSITIVE (raw/unboxed data in a pointer-bearing object —
      ;; e.g. a stored hash in a hash-table backing array — that merely aliases a
      ;; live object start).  REJECT it (don't copy).  Without this, a tag-9 value
      ;; aliasing a cons is copied via copy_object's OBJECT path → the cons's CAR is
      ;; read as a header → astronomical count → the copy loop runs off the mapped
      ;; heap → SIGSEGV (WS4-AA64 #160 root cause).  conskind base @0x10000E40 (<<1);
      ;; scratch x13..x17; preserves x9(word-addr)/x10(value)/x11(tag)/x12(raw).
      (a64-load-imm64 buf +a64-x14+ #x10000E40) (a64-ldr-unsigned buf +a64-x14+ +a64-x14+ 0) (a64-asr-imm buf +a64-x14+ +a64-x14+ 1)
      (a64-sub-reg buf +a64-x13+ +a64-x12+ +a64-x27+ 0 0)          ; raw - page_base
      (a64-lsr-imm buf +a64-x15+ +a64-x13+ 7) (a64-add-reg buf +a64-x15+ +a64-x14+ +a64-x15+ 0 0) ; x15 = conskind byte addr
      (a64-lsr-imm buf +a64-x13+ +a64-x13+ 4)                      ; granule
      (a64-movz buf +a64-x16+ 7 0) (a64-and-reg buf +a64-x13+ +a64-x13+ +a64-x16+)  ; x13 = bit idx
      (a64-ldrb buf +a64-x16+ +a64-x15+)                           ; x16 = conskind byte
      (a64-movz buf +a64-x17+ 1 0) (a64-lslv buf +a64-x17+ +a64-x17+ +a64-x13+)     ; x17 = mask = 1<<bit
      (let ((ck-tag1 (incf *mvm-label-counter*)) (ck-done (incf *mvm-label-counter*)))
        (a64-cmp-imm buf +a64-x11+ 1)                              ; tag == 1 (cons)?
        (let ((i (a64-current-index buf))) (a64-bcond buf +cc-eq+ 0) (a64-add-fixup buf i ck-tag1 :bcond))
        ;; tag == 9 (object): REJECT if cons-kind bit SET (a tag-9 value → a cons)
        (a64-ands-reg buf +a64-xzr+ +a64-x16+ +a64-x17+)
        (let ((i (a64-current-index buf))) (a64-bcond buf +cc-ne+ 0) (a64-add-fixup buf i sret :bcond))
        (let ((i (a64-current-index buf))) (a64-b buf 0) (a64-add-fixup buf i ck-done :b))
        (a64-set-label buf ck-tag1)
        ;; tag == 1 (cons): REJECT if cons-kind bit CLEAR (a tag-1 value → a non-cons)
        (a64-ands-reg buf +a64-xzr+ +a64-x16+ +a64-x17+)
        (let ((i (a64-current-index buf))) (a64-bcond buf +cc-eq+ 0) (a64-add-fixup buf i sret :bcond))
        (a64-set-label buf ck-done))
      ;; copy + update the word (save x9 word-addr + x30 across the nested BL)
      (a64-stp-pre buf +a64-x9+ +a64-x30+ +a64-sp+ -16)
      (let ((i (a64-current-index buf))) (a64-bl buf 0) (a64-add-fixup buf i copy-obj :bl)) ; x10 tagged → x10 new
      (a64-ldp-post buf +a64-x9+ +a64-x30+ +a64-sp+ 16)
      (a64-str-unsigned buf +a64-x10+ +a64-x9+ 0)         ; [word] = new ptr
      (a64-set-label buf sret)
      (a64-ret buf))

    ;; ============ SUBROUTINE copy_object (x10 = tagged; → x10 = new; advances x21) ============
    (a64-set-label buf copy-obj)
    (a64-lsr-imm buf +a64-x11+ +a64-x10+ 4)
    (a64-lsl-imm buf +a64-x11+ +a64-x11+ 4)              ; x11 = raw src
    (a64-sub-reg buf +a64-x12+ +a64-x10+ +a64-x11+ 0 0)  ; x12 = tag (1 or 9)
    (a64-ldr-unsigned buf +a64-x13+ +a64-x11+ 0)         ; x13 = first word (car/header)
    (a64-lsr-imm buf +a64-x14+ +a64-x13+ 4)
    (a64-lsl-imm buf +a64-x14+ +a64-x14+ 4)
    (a64-sub-reg buf +a64-x14+ +a64-x13+ +a64-x14+ 0 0)  ; x14 = first-word tag
    (let ((co-fwd  (incf *mvm-label-counter*))
          (co-cons (incf *mvm-label-counter*))
          (co-done (incf *mvm-label-counter*))
          (oloop   (incf *mvm-label-counter*))
          (odone   (incf *mvm-label-counter*)))
      (a64-cmp-imm buf +a64-x14+ 15)
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-eq+ 0) (a64-add-fixup buf i co-fwd :bcond))
      (a64-cmp-imm buf +a64-x12+ 1)
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-eq+ 0) (a64-add-fixup buf i co-cons :bcond))
      ;; ---- object: size = align16((count+2)*8), EXCEPT u8-vector (#x11) whose
      ;;      header count is in BYTES → size = align16(16 + count). ----
      (a64-mov-reg buf +a64-x12+ +a64-x21+)              ; x12 = dest_start
      (a64-lsr-imm buf +a64-x15+ +a64-x13+ 8)            ; x15 = count
      (a64-lsl-imm buf +a64-x9+ +a64-x13+ 56) (a64-lsr-imm buf +a64-x9+ +a64-x9+ 56) ; x9 = subtag
      (a64-cmp-imm buf +a64-x9+ #x11)
      (let ((u8sz (incf *mvm-label-counter*)) (hadsz (incf *mvm-label-counter*)))
        (let ((i (a64-current-index buf))) (a64-bcond buf +cc-eq+ 0) (a64-add-fixup buf i u8sz :bcond))
        (a64-lsl-imm buf +a64-x15+ +a64-x15+ 3) (a64-add-imm buf +a64-x15+ +a64-x15+ 16) ; (count+2)*8 = count*8+16
        (let ((i (a64-current-index buf))) (a64-b buf 0) (a64-add-fixup buf i hadsz :b))
        (a64-set-label buf u8sz)
        (a64-add-imm buf +a64-x15+ +a64-x15+ 16)         ; u8: 16 + count(bytes)
        (a64-set-label buf hadsz))
      (a64-add-imm buf +a64-x15+ +a64-x15+ 15)
      (a64-lsr-imm buf +a64-x15+ +a64-x15+ 4)
      (a64-lsl-imm buf +a64-x15+ +a64-x15+ 4)            ; size aligned16
      (a64-add-reg buf +a64-x9+ +a64-x11+ +a64-x15+ 0 0) ; x9 = src end
      (a64-mov-reg buf +a64-x16+ +a64-x11+)              ; x16 = src ptr
      (a64-set-label buf oloop)
      (a64-cmp-reg buf +a64-x16+ +a64-x9+)
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-cs+ 0) (a64-add-fixup buf i odone :bcond))
      (a64-ldr-unsigned buf +a64-x17+ +a64-x16+ 0)
      (a64-str-unsigned buf +a64-x17+ +a64-x21+ 0)
      (a64-add-imm buf +a64-x16+ +a64-x16+ 8)
      (a64-add-imm buf +a64-x21+ +a64-x21+ 8)
      (let ((i (a64-current-index buf))) (a64-b buf 0) (a64-add-fixup buf i oloop :b))
      (a64-set-label buf odone)
      (a64-add-imm buf +a64-x17+ +a64-x12+ 15) (a64-str-unsigned buf +a64-x17+ +a64-x11+ 0) ; fwd ptr
      (a64-add-imm buf +a64-x10+ +a64-x12+ 9)            ; new tagged = dest|9
      ;; set object-start bit for dest_start (x12), scratch x13..x17
      (a64-sub-reg buf +a64-x13+ +a64-x12+ +a64-x27+ 0 0)
      (a64-lsr-imm buf +a64-x14+ +a64-x13+ 7) (a64-add-reg buf +a64-x14+ +a64-x28+ +a64-x14+ 0 0)
      (a64-lsr-imm buf +a64-x13+ +a64-x13+ 4)
      (a64-movz buf +a64-x17+ 7 0) (a64-and-reg buf +a64-x13+ +a64-x13+ +a64-x17+)
      (a64-ldrb buf +a64-x15+ +a64-x14+)
      (a64-movz buf +a64-x16+ 1 0) (a64-lslv buf +a64-x16+ +a64-x16+ +a64-x13+)
      (a64-orr-reg buf +a64-x15+ +a64-x15+ +a64-x16+) (a64-strb buf +a64-x15+ +a64-x14+)
      (let ((i (a64-current-index buf))) (a64-b buf 0) (a64-add-fixup buf i co-done :b))
      ;; ---- cons: 16 bytes ----
      (a64-set-label buf co-cons)
      (a64-mov-reg buf +a64-x12+ +a64-x21+)              ; dest_start
      (a64-str-unsigned buf +a64-x13+ +a64-x21+ 0)       ; car (x13 = first word)
      (a64-ldr-unsigned buf +a64-x14+ +a64-x11+ 8)       ; cdr = [src+8] (imm=BYTE offset, scaled /8)
      (a64-str-unsigned buf +a64-x14+ +a64-x21+ 8)       ; [dest+8] = cdr
      (a64-add-imm buf +a64-x17+ +a64-x12+ 15) (a64-str-unsigned buf +a64-x17+ +a64-x11+ 0) ; fwd
      (a64-add-imm buf +a64-x10+ +a64-x12+ 1)            ; new tagged = dest|1
      (a64-sub-reg buf +a64-x13+ +a64-x12+ +a64-x27+ 0 0)
      (a64-lsr-imm buf +a64-x14+ +a64-x13+ 7) (a64-add-reg buf +a64-x14+ +a64-x28+ +a64-x14+ 0 0)
      (a64-lsr-imm buf +a64-x13+ +a64-x13+ 4)
      (a64-movz buf +a64-x17+ 7 0) (a64-and-reg buf +a64-x13+ +a64-x13+ +a64-x17+)
      (a64-ldrb buf +a64-x15+ +a64-x14+)
      (a64-movz buf +a64-x16+ 1 0) (a64-lslv buf +a64-x16+ +a64-x16+ +a64-x13+)
      (a64-orr-reg buf +a64-x15+ +a64-x15+ +a64-x16+) (a64-strb buf +a64-x15+ +a64-x14+)
      ;; ALSO set the CONS-KIND bit for dest (x12) so the type-aware to-space
      ;; scan classifies this copy as a cons.  conskind base @0x10000E40 (<<1).
      ;; NB: x10 holds the RETURN value (dest|1) — must NOT clobber it; use x9
      ;; for the base and x13..x17 for the bit math.
      (a64-load-imm64 buf +a64-x9+ #x10000E40) (a64-ldr-unsigned buf +a64-x9+ +a64-x9+ 0) (a64-asr-imm buf +a64-x9+ +a64-x9+ 1)
      (a64-sub-reg buf +a64-x13+ +a64-x12+ +a64-x27+ 0 0)
      (a64-lsr-imm buf +a64-x14+ +a64-x13+ 7) (a64-add-reg buf +a64-x14+ +a64-x9+ +a64-x14+ 0 0)
      (a64-lsr-imm buf +a64-x13+ +a64-x13+ 4)
      (a64-movz buf +a64-x17+ 7 0) (a64-and-reg buf +a64-x13+ +a64-x13+ +a64-x17+)
      (a64-ldrb buf +a64-x15+ +a64-x14+)
      (a64-movz buf +a64-x16+ 1 0) (a64-lslv buf +a64-x16+ +a64-x16+ +a64-x13+)
      (a64-orr-reg buf +a64-x15+ +a64-x15+ +a64-x16+) (a64-strb buf +a64-x15+ +a64-x14+)
      (a64-add-imm buf +a64-x21+ +a64-x21+ 16)           ; advance free_ptr
      (let ((i (a64-current-index buf))) (a64-b buf 0) (a64-add-fixup buf i co-done :b))
      ;; ---- already forwarded: x13 = new|15 ----
      (a64-set-label buf co-fwd)
      (a64-lsr-imm buf +a64-x14+ +a64-x13+ 4) (a64-lsl-imm buf +a64-x14+ +a64-x14+ 4) ; new addr
      (a64-add-reg buf +a64-x10+ +a64-x14+ +a64-x12+ 0 0) ; new | orig tag
      (a64-set-label buf co-done)
      (a64-ret buf))

    ;; ============ MAIN BODY ============
    (a64-set-label buf main)
    ;; save mutator regs (same 240B frame as the Lisp trampoline)
    (a64-stp-pre    buf +a64-x0+  +a64-x1+  +a64-sp+ -240)
    (a64-stp-offset buf +a64-x2+  +a64-x3+  +a64-sp+ 16)
    (a64-stp-offset buf +a64-x4+  +a64-x5+  +a64-sp+ 32)
    (a64-stp-offset buf +a64-x6+  +a64-x7+  +a64-sp+ 48)
    (a64-stp-offset buf +a64-x8+  +a64-x9+  +a64-sp+ 64)
    (a64-stp-offset buf +a64-x10+ +a64-x11+ +a64-sp+ 80)
    (a64-stp-offset buf +a64-x12+ +a64-x13+ +a64-sp+ 96)
    (a64-stp-offset buf +a64-x14+ +a64-x15+ +a64-sp+ 112)
    (a64-stp-offset buf +a64-x16+ +a64-x17+ +a64-sp+ 128)
    (a64-str-unsigned buf +a64-x18+ +a64-sp+ 144)
    (a64-str-unsigned buf +a64-x19+ +a64-sp+ 152)
    (a64-str-unsigned buf +a64-x20+ +a64-sp+ 160)
    (a64-str-unsigned buf +a64-x21+ +a64-sp+ 168)
    (a64-str-unsigned buf +a64-x22+ +a64-sp+ 176)
    (a64-str-unsigned buf +a64-x23+ +a64-sp+ 184)
    (a64-str-unsigned buf +a64-x26+ +a64-sp+ 192)
    (a64-str-unsigned buf +a64-x27+ +a64-sp+ 200)
    (a64-str-unsigned buf +a64-x29+ +a64-sp+ 208)
    (a64-str-unsigned buf +a64-x30+ +a64-sp+ 216)
    ;; load GC metadata (all stored <<1 → ASR #1 to raw)
    (flet ((load-asr (rd addr) (a64-load-imm64 buf +a64-x16+ addr)
                     (a64-ldr-unsigned buf rd +a64-x16+ 0) (a64-asr-imm buf rd rd 1)))
      (load-asr +a64-x19+ #x10000040)                   ; from_start
      (load-asr +a64-x22+ #x10000048)                   ; to_start
      (a64-mov-reg buf +a64-x21+ +a64-x22+)             ; free_ptr = to_start
      (load-asr +a64-x25+ #x10000050)                   ; space_size
      (a64-add-reg buf +a64-x20+ +a64-x19+ +a64-x25+ 0 0) ; from_end = from_start+space_size
      (load-asr +a64-x23+ #x10000058)                   ; stack_base
      (load-asr +a64-x27+ #x10000E00)                   ; page_base
      (load-asr +a64-x28+ #x10000E18))                  ; obj-bitmap base
    ;; ---- scan stack: x26 from SP to stack_base ----
    (a64-add-imm buf +a64-x26+ +a64-sp+ 0)              ; x26 = SP (scan start)
    (let ((sloop (incf *mvm-label-counter*))
          (sdone (incf *mvm-label-counter*)))
      (a64-set-label buf sloop)
      (a64-cmp-reg buf +a64-x26+ +a64-x23+)
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-cs+ 0) (a64-add-fixup buf i sdone :bcond))
      (a64-mov-reg buf +a64-x9+ +a64-x26+)
      (let ((i (a64-current-index buf))) (a64-bl buf 0) (a64-add-fixup buf i scan-word :bl))
      (a64-add-imm buf +a64-x26+ +a64-x26+ 8)
      (let ((i (a64-current-index buf))) (a64-b buf 0) (a64-add-fixup buf i sloop :b))
      (a64-set-label buf sdone))
    ;; ---- fixed global roots ----
    (flet ((scan-fixed (addr) (a64-load-imm64 buf +a64-x9+ addr)
                       (let ((i (a64-current-index buf))) (a64-bl buf 0) (a64-add-fixup buf i scan-word :bl))))
      (scan-fixed #x10000080)      ; globals hash-table
      (scan-fixed #x10000088)      ; symbol intern table
      (scan-fixed #x10000148)      ; keyword intern table
      (scan-fixed #x10000170))     ; package-by-hash table
    ;; ---- MV region: count-1 extras from 0x98 (only when 2<=count<=16) ----
    ;; count = [0x90]>>1; extras = count-1.  Guards (both SIGNED, mirroring x64's
    ;; JLE — a b.eq-only guard let a zeroed/garbage count run extras=-1 as a
    ;; runaway scan): skip if extras<=0 (count 0/1) OR count>16 (garbage —
    ;; multiple-values-limit is 16), so a stale/uninit MV-count can't drive a
    ;; wild scan off into unmapped memory.
    (a64-load-imm64 buf +a64-x16+ #x10000090)
    (a64-ldr-unsigned buf +a64-x26+ +a64-x16+ 0)        ; tagged count
    (a64-asr-imm buf +a64-x26+ +a64-x26+ 1)             ; raw count
    (a64-sub-imm buf +a64-x26+ +a64-x26+ 1)             ; extras = count-1
    (let ((mvloop (incf *mvm-label-counter*))
          (mvdone (incf *mvm-label-counter*)))
      (a64-cmp-imm buf +a64-x26+ 0)
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-le+ 0) (a64-add-fixup buf i mvdone :bcond)) ; extras<=0
      (a64-cmp-imm buf +a64-x26+ 16)
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-gt+ 0) (a64-add-fixup buf i mvdone :bcond)) ; extras>16 garbage
      (a64-load-imm64 buf +a64-x9+ #x10000098)
      (a64-set-label buf mvloop)
      (a64-cmp-imm buf +a64-x26+ 0)
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-le+ 0) (a64-add-fixup buf i mvdone :bcond))
      (let ((i (a64-current-index buf))) (a64-bl buf 0) (a64-add-fixup buf i scan-word :bl))
      (a64-add-imm buf +a64-x9+ +a64-x9+ 8)
      (a64-sub-imm buf +a64-x26+ +a64-x26+ 1)
      (let ((i (a64-current-index buf))) (a64-b buf 0) (a64-add-fixup buf i mvloop :b))
      (a64-set-label buf mvdone))
    ;; ---- Cheney scan: OBJECT-BY-OBJECT, TYPE-AWARE ----
    ;; #160 PIECE 1: walk to-space object-by-object (cursor x26; next-obj x24;
    ;; slot cursor x18 — all preserved across scan_word/copy_object).  The
    ;; cons-kind bitmap (@0x10000E40) says whether the granule at the cursor is a
    ;; cons (scan car+cdr) or a headered object (read subtag).  LEAF subtags
    ;; (string #x10/#x31, u8 #x11, u64 #x14, sap #x16, bignum #x30, floats
    ;; #x60/#x64/#x65/#x66) carry RAW payload → copy but DO NOT scan their slots
    ;; as pointers (so a leaf data word aliasing a from-space start is never
    ;; rewritten — the residual gap the word-by-word scan had).  #x61 = mvm-module
    ;; is NOT a leaf (pointer slots) so it is scanned.  Object size mirrors
    ;; copy_object: u8 = align16(16+count[bytes]); else align16((count+2)*8).
    (a64-mov-reg buf +a64-x26+ +a64-x22+)               ; walk cursor = to_start
    (let ((cloop (incf *mvm-label-counter*))
          (cdone (incf *mvm-label-counter*))
          (is-cons (incf *mvm-label-counter*))
          (leafskip (incf *mvm-label-counter*))
          (u8sz2 (incf *mvm-label-counter*))
          (haveslots (incf *mvm-label-counter*))
          (sloop (incf *mvm-label-counter*))
          (sdone2 (incf *mvm-label-counter*)))
      (a64-set-label buf cloop)
      (a64-cmp-reg buf +a64-x26+ +a64-x21+)             ; cursor >= free_ptr → done
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-cs+ 0) (a64-add-fixup buf i cdone :bcond))
      ;; cons-kind check for cursor: conskind base @0x10000E40 (<<1)
      (a64-load-imm64 buf +a64-x9+ #x10000E40) (a64-ldr-unsigned buf +a64-x10+ +a64-x9+ 0) (a64-asr-imm buf +a64-x10+ +a64-x10+ 1)
      (a64-sub-reg buf +a64-x11+ +a64-x26+ +a64-x27+ 0 0)  ; cursor - page_base
      (a64-lsr-imm buf +a64-x12+ +a64-x11+ 7) (a64-add-reg buf +a64-x12+ +a64-x10+ +a64-x12+ 0 0) ; byte addr
      (a64-lsr-imm buf +a64-x11+ +a64-x11+ 4)              ; granule
      (a64-movz buf +a64-x13+ 7 0) (a64-and-reg buf +a64-x11+ +a64-x11+ +a64-x13+) ; bit idx
      (a64-ldrb buf +a64-x14+ +a64-x12+)
      (a64-movz buf +a64-x15+ 1 0) (a64-lslv buf +a64-x15+ +a64-x15+ +a64-x11+)  ; mask
      (a64-ands-reg buf +a64-xzr+ +a64-x14+ +a64-x15+)     ; TST
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-ne+ 0) (a64-add-fixup buf i is-cons :bcond)) ; set → cons
      ;; headered object
      (a64-ldr-unsigned buf +a64-x13+ +a64-x26+ 0)         ; x13 = header
      (a64-lsl-imm buf +a64-x14+ +a64-x13+ 56) (a64-lsr-imm buf +a64-x14+ +a64-x14+ 56) ; x14 = subtag
      (a64-lsr-imm buf +a64-x15+ +a64-x13+ 8)              ; x15 = count
      (a64-lsl-imm buf +a64-x16+ +a64-x15+ 3)              ; x16 = count*8
      (a64-add-imm buf +a64-x18+ +a64-x26+ 16)             ; x18 = slot cursor (obj+16)
      (a64-cmp-imm buf +a64-x14+ #x11)
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-eq+ 0) (a64-add-fixup buf i u8sz2 :bcond))
      (a64-add-imm buf +a64-x9+ +a64-x16+ 16)              ; general: count*8+16
      (let ((i (a64-current-index buf))) (a64-b buf 0) (a64-add-fixup buf i haveslots :b))
      (a64-set-label buf u8sz2)
      (a64-add-imm buf +a64-x9+ +a64-x15+ 16)              ; u8: 16+count(bytes)
      (a64-set-label buf haveslots)
      (a64-add-imm buf +a64-x9+ +a64-x9+ 15) (a64-lsr-imm buf +a64-x9+ +a64-x9+ 4) (a64-lsl-imm buf +a64-x9+ +a64-x9+ 4) ; align16
      (a64-add-reg buf +a64-x24+ +a64-x26+ +a64-x9+ 0 0)   ; x24 = next object pos (preserved)
      (a64-add-reg buf +a64-x26+ +a64-x18+ +a64-x16+ 0 0)  ; x26 = slot_end = obj+16+count*8
      (dolist (st (list #x10 #x11 #x14 #x16 #x30 #x31 #x60 #x64 #x65 #x66))
        (a64-cmp-imm buf +a64-x14+ st)
        (let ((i (a64-current-index buf))) (a64-bcond buf +cc-eq+ 0) (a64-add-fixup buf i leafskip :bcond)))
      ;; pointer-bearing: scan slots [x18 .. slot_end=x26)
      (a64-set-label buf sloop)
      (a64-cmp-reg buf +a64-x18+ +a64-x26+)
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-cs+ 0) (a64-add-fixup buf i sdone2 :bcond))
      (a64-mov-reg buf +a64-x9+ +a64-x18+)
      (let ((i (a64-current-index buf))) (a64-bl buf 0) (a64-add-fixup buf i scan-word :bl))
      (a64-add-imm buf +a64-x18+ +a64-x18+ 8)
      (let ((i (a64-current-index buf))) (a64-b buf 0) (a64-add-fixup buf i sloop :b))
      (a64-set-label buf sdone2)
      (a64-set-label buf leafskip)
      (a64-mov-reg buf +a64-x26+ +a64-x24+)                ; advance to next object
      (let ((i (a64-current-index buf))) (a64-b buf 0) (a64-add-fixup buf i cloop :b))
      ;; cons: scan car @cursor, cdr @cursor+8
      (a64-set-label buf is-cons)
      (a64-mov-reg buf +a64-x9+ +a64-x26+)
      (let ((i (a64-current-index buf))) (a64-bl buf 0) (a64-add-fixup buf i scan-word :bl))
      (a64-add-imm buf +a64-x9+ +a64-x26+ 8)
      (let ((i (a64-current-index buf))) (a64-bl buf 0) (a64-add-fixup buf i scan-word :bl))
      (a64-add-imm buf +a64-x26+ +a64-x26+ 16)
      (let ((i (a64-current-index buf))) (a64-b buf 0) (a64-add-fixup buf i cloop :b))
      (a64-set-label buf cdone))
    ;; ---- clear reclaimed (old from_start = x19) object-start bitmap range ----
    ;; dest = obj_bitmap(x28) + (x19-page_base)>>7 ; count = space_size(x25)>>7 bytes.
    ;; #160 FIX: the per-semispace byte count is NOT 8-aligned — space_size =
    ;; midpoint-alloc_start is NOT a power of 2 — so a pure u64 store loop
    ;; overshoots up to 7 bytes into the NEXT semispace's bitmap, clearing that
    ;; semispace's objects' object-start bits.  The next GC then REJECTS those
    ;; (now-live) objects as non-starts → root/chain references silently break
    ;; (the walked=1 bug).  Clear BYTE-EXACT: u64 only while a full 8 bytes fit
    ;; (x9 < end-7), then STRB the 0-7 byte tail.  (Mirrors x64's REP STOSB.)
    (a64-sub-reg buf +a64-x9+ +a64-x19+ +a64-x27+ 0 0)
    (a64-lsr-imm buf +a64-x9+ +a64-x9+ 7)
    (a64-add-reg buf +a64-x9+ +a64-x28+ +a64-x9+ 0 0)   ; x9 = dest
    (a64-lsr-imm buf +a64-x10+ +a64-x25+ 7)             ; x10 = byte count
    (a64-add-reg buf +a64-x10+ +a64-x9+ +a64-x10+ 0 0)  ; x10 = dest end (exclusive)
    (a64-sub-imm buf +a64-x11+ +a64-x10+ 7)             ; x11 = end-7 (last safe u64 start+1)
    (let ((zloop (incf *mvm-label-counter*))
          (zbyte (incf *mvm-label-counter*))
          (zdone (incf *mvm-label-counter*)))
      (a64-set-label buf zloop)
      (a64-cmp-reg buf +a64-x9+ +a64-x11+)              ; x9 >= end-7 → done with u64
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-cs+ 0) (a64-add-fixup buf i zbyte :bcond))
      (a64-str-unsigned buf +a64-xzr+ +a64-x9+ 0)       ; store 8 zero bytes (fits)
      (a64-add-imm buf +a64-x9+ +a64-x9+ 8)
      (let ((i (a64-current-index buf))) (a64-b buf 0) (a64-add-fixup buf i zloop :b))
      (a64-set-label buf zbyte)
      (a64-cmp-reg buf +a64-x9+ +a64-x10+)              ; byte tail: while x9 < end
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-cs+ 0) (a64-add-fixup buf i zdone :bcond))
      (a64-strb buf +a64-xzr+ +a64-x9+)                 ; store 1 zero byte
      (a64-add-imm buf +a64-x9+ +a64-x9+ 1)
      (let ((i (a64-current-index buf))) (a64-b buf 0) (a64-add-fixup buf i zbyte :b))
      (a64-set-label buf zdone))
    ;; ---- ALSO byte-exact clear the reclaimed CONS-KIND bitmap range ----
    ;; (else stale cons-kind bits in the reclaimed semispace would misclassify a
    ;; future object-start as a cons in the type-aware walk.)  Same range/method.
    (a64-load-imm64 buf +a64-x12+ #x10000E40) (a64-ldr-unsigned buf +a64-x12+ +a64-x12+ 0) (a64-asr-imm buf +a64-x12+ +a64-x12+ 1) ; x12 = conskind base
    (a64-sub-reg buf +a64-x9+ +a64-x19+ +a64-x27+ 0 0)
    (a64-lsr-imm buf +a64-x9+ +a64-x9+ 7)
    (a64-add-reg buf +a64-x9+ +a64-x12+ +a64-x9+ 0 0)   ; x9 = dest (conskind)
    (a64-lsr-imm buf +a64-x10+ +a64-x25+ 7)             ; x10 = byte count
    (a64-add-reg buf +a64-x10+ +a64-x9+ +a64-x10+ 0 0)  ; x10 = dest end (exclusive)
    (a64-sub-imm buf +a64-x11+ +a64-x10+ 7)             ; x11 = end-7
    (let ((zloop2 (incf *mvm-label-counter*))
          (zbyte2 (incf *mvm-label-counter*))
          (zdone2b (incf *mvm-label-counter*)))
      (a64-set-label buf zloop2)
      (a64-cmp-reg buf +a64-x9+ +a64-x11+)
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-cs+ 0) (a64-add-fixup buf i zbyte2 :bcond))
      (a64-str-unsigned buf +a64-xzr+ +a64-x9+ 0)
      (a64-add-imm buf +a64-x9+ +a64-x9+ 8)
      (let ((i (a64-current-index buf))) (a64-b buf 0) (a64-add-fixup buf i zloop2 :b))
      (a64-set-label buf zbyte2)
      (a64-cmp-reg buf +a64-x9+ +a64-x10+)
      (let ((i (a64-current-index buf))) (a64-bcond buf +cc-cs+ 0) (a64-add-fixup buf i zdone2b :bcond))
      (a64-strb buf +a64-xzr+ +a64-x9+)
      (a64-add-imm buf +a64-x9+ +a64-x9+ 1)
      (let ((i (a64-current-index buf))) (a64-b buf 0) (a64-add-fixup buf i zbyte2 :b))
      (a64-set-label buf zdone2b))
    ;; ---- swap metadata (store <<1) ----
    (a64-lsl-imm buf +a64-x9+ +a64-x22+ 1)              ; new from_start = to_start
    (a64-load-imm64 buf +a64-x16+ #x10000040) (a64-str-unsigned buf +a64-x9+ +a64-x16+ 0)
    (a64-lsl-imm buf +a64-x9+ +a64-x19+ 1)              ; new to_start = old from_start
    (a64-load-imm64 buf +a64-x16+ #x10000048) (a64-str-unsigned buf +a64-x9+ +a64-x16+ 0)
    ;; x24 = free_ptr ; x25 = new from_start(to_start x22) + space_size(x25)
    (a64-mov-reg buf +a64-x24+ +a64-x21+)
    (a64-add-reg buf +a64-x25+ +a64-x22+ +a64-x25+ 0 0)
    ;; gc_count += 1 (stored <<1 → += 2)
    (a64-load-imm64 buf +a64-x16+ #x10000060)
    (a64-ldr-unsigned buf +a64-x9+ +a64-x16+ 0) (a64-add-imm buf +a64-x9+ +a64-x9+ 2)
    (a64-str-unsigned buf +a64-x9+ +a64-x16+ 0)
    ;; ---- restore mutator regs + RET ----
    (a64-ldr-unsigned buf +a64-x30+ +a64-sp+ 216)
    (a64-ldr-unsigned buf +a64-x29+ +a64-sp+ 208)
    (a64-ldr-unsigned buf +a64-x27+ +a64-sp+ 200)
    (a64-ldr-unsigned buf +a64-x26+ +a64-sp+ 192)
    (a64-ldr-unsigned buf +a64-x23+ +a64-sp+ 184)
    (a64-ldr-unsigned buf +a64-x22+ +a64-sp+ 176)
    (a64-ldr-unsigned buf +a64-x21+ +a64-sp+ 168)
    (a64-ldr-unsigned buf +a64-x20+ +a64-sp+ 160)
    (a64-ldr-unsigned buf +a64-x19+ +a64-sp+ 152)
    (a64-ldr-unsigned buf +a64-x18+ +a64-sp+ 144)
    (a64-ldp-offset buf +a64-x16+ +a64-x17+ +a64-sp+ 128)
    (a64-ldp-offset buf +a64-x14+ +a64-x15+ +a64-sp+ 112)
    (a64-ldp-offset buf +a64-x12+ +a64-x13+ +a64-sp+ 96)
    (a64-ldp-offset buf +a64-x10+ +a64-x11+ +a64-sp+ 80)
    (a64-ldp-offset buf +a64-x8+  +a64-x9+  +a64-sp+ 64)
    (a64-ldp-offset buf +a64-x6+  +a64-x7+  +a64-sp+ 48)
    (a64-ldp-offset buf +a64-x4+  +a64-x5+  +a64-sp+ 32)
    (a64-ldp-offset buf +a64-x2+  +a64-x3+  +a64-sp+ 16)
    (a64-ldp-post buf +a64-x0+ +a64-x1+ +a64-sp+ 240)
    (a64-ret buf)))

(defun emit-aarch64-handler-helpers (buf)
  "If *aarch64-handler-push-label* / *aarch64-handler-pop-label* are
   bound (non-nil), emit the push and pop helpers into BUF and set
   those labels at the entry points.  No-op otherwise — non-unified
   callers don't pay for this."
  (when (and *aarch64-handler-push-label* *aarch64-handler-pop-label*)
    ;; ---- PUSH helper ----
    (a64-set-label buf *aarch64-handler-push-label*)
    ;; x9 = 0x10010000 (depth slot)
    (a64-movz buf +a64-x9+ #x0000 0)
    (a64-movk buf +a64-x9+ #x1001 1)
    ;; x10 = depth
    (a64-ldr-unsigned buf +a64-x10+ +a64-x9+ 0)
    ;; x11 = frame_base = 0x10010008 + depth*24
    (a64-add-imm buf +a64-x11+ +a64-x9+ 8)
    (a64-lsl-imm buf +a64-x12+ +a64-x10+ 4)        ; depth*16
    (a64-add-reg buf +a64-x11+ +a64-x11+ +a64-x12+ 0 0)
    (a64-lsl-imm buf +a64-x12+ +a64-x10+ 3)        ; depth*8
    (a64-add-reg buf +a64-x11+ +a64-x11+ +a64-x12+ 0 0)
    ;; x12 = 0x10000180 (current handler slot)
    (a64-movz buf +a64-x12+ #x0180 0)
    (a64-movk buf +a64-x12+ #x1000 1)
    ;; copy 3 doublewords: 180→frame+0, 188→frame+8, 190→frame+16
    (a64-ldr-unsigned buf +a64-x13+ +a64-x12+ 0)
    (a64-str-unsigned buf +a64-x13+ +a64-x11+ 0)
    (a64-ldr-unsigned buf +a64-x13+ +a64-x12+ 8)
    (a64-str-unsigned buf +a64-x13+ +a64-x11+ 8)
    (a64-ldr-unsigned buf +a64-x13+ +a64-x12+ 16)
    (a64-str-unsigned buf +a64-x13+ +a64-x11+ 16)
    ;; depth += 1
    (a64-add-imm buf +a64-x10+ +a64-x10+ 1)
    (a64-str-unsigned buf +a64-x10+ +a64-x9+ 0)
    (a64-ret buf)
    ;; ---- POP helper ----
    (a64-set-label buf *aarch64-handler-pop-label*)
    (a64-movz buf +a64-x9+ #x0000 0)
    (a64-movk buf +a64-x9+ #x1001 1)
    (a64-ldr-unsigned buf +a64-x10+ +a64-x9+ 0)
    ;; if depth == 0 → zero slot 180/188/190 and return
    (a64-cmp-imm buf +a64-x10+ 0)
    (let ((nz-label (incf *mvm-label-counter*)))
      (let ((idx (a64-current-index buf)))
        (a64-bcond buf +cc-ne+ 0)
        (a64-add-fixup buf idx nz-label :bcond))
      ;; depth == 0 path: zero 0x180/188/190 and return
      (a64-movz buf +a64-x12+ #x0180 0)
      (a64-movk buf +a64-x12+ #x1000 1)
      (a64-str-unsigned buf +a64-xzr+ +a64-x12+ 0)
      (a64-str-unsigned buf +a64-xzr+ +a64-x12+ 8)
      (a64-str-unsigned buf +a64-xzr+ +a64-x12+ 16)
      (a64-ret buf)
      ;; depth > 0 path: decrement, restore from frame[new-depth]
      (a64-set-label buf nz-label)
      (a64-sub-imm buf +a64-x10+ +a64-x10+ 1)
      (a64-str-unsigned buf +a64-x10+ +a64-x9+ 0)
      ;; x11 = 0x10010008 + depth*24
      (a64-add-imm buf +a64-x11+ +a64-x9+ 8)
      (a64-lsl-imm buf +a64-x12+ +a64-x10+ 4)
      (a64-add-reg buf +a64-x11+ +a64-x11+ +a64-x12+ 0 0)
      (a64-lsl-imm buf +a64-x12+ +a64-x10+ 3)
      (a64-add-reg buf +a64-x11+ +a64-x11+ +a64-x12+ 0 0)
      ;; restore 0x180/188/190 from frame
      (a64-movz buf +a64-x12+ #x0180 0)
      (a64-movk buf +a64-x12+ #x1000 1)
      (a64-ldr-unsigned buf +a64-x13+ +a64-x11+ 0)
      (a64-str-unsigned buf +a64-x13+ +a64-x12+ 0)
      (a64-ldr-unsigned buf +a64-x13+ +a64-x11+ 8)
      (a64-str-unsigned buf +a64-x13+ +a64-x12+ 8)
      (a64-ldr-unsigned buf +a64-x13+ +a64-x11+ 16)
      (a64-str-unsigned buf +a64-x13+ +a64-x12+ 16)
      (a64-ret buf)))
  ;; ---- GC trampoline ----
  ;; Called by +op-gc-check+ when x24 (alloc ptr) >= x25 (alloc limit).
  ;; Saves caller-saved regs, stashes x24/x25/pre-entry-SP into GC
  ;; metadata slots, invokes %gc-collect, reloads x24/x25 from the
  ;; (updated) metadata, restores caller regs, RETs.
  ;;
  ;; Frame layout (240 bytes, 16-byte aligned).  Saves ALL non-SP
  ;; integer regs except x24/x25 (which are the GC's input, not preserved)
  ;; and x28 (unused by Modus codegen) so any caller-saved or
  ;; Modus-global register that %gc-collect's compiled body touches
  ;; round-trips intact through the trampoline:
  ;;   [sp+  0..  7] x0   [sp+  8.. 15] x1   (STP pair)
  ;;   [sp+ 16.. 23] x2   [sp+ 24.. 31] x3
  ;;   [sp+ 32.. 39] x4   [sp+ 40.. 47] x5
  ;;   [sp+ 48.. 55] x6   [sp+ 56.. 63] x7
  ;;   [sp+ 64.. 71] x8   [sp+ 72.. 79] x9
  ;;   [sp+ 80.. 87] x10  [sp+ 88.. 95] x11
  ;;   [sp+ 96..103] x12  [sp+104..111] x13
  ;;   [sp+112..119] x14  [sp+120..127] x15
  ;;   [sp+128..135] x16  [sp+136..143] x17
  ;;   [sp+144..151] x18  [sp+152..159] x19
  ;;   [sp+160..167] x20  [sp+168..175] x21
  ;;   [sp+176..183] x22  [sp+184..191] x23
  ;;   [sp+192..199] x26  [sp+200..207] x27
  ;;   [sp+208..215] x29 (FP) [sp+216..223] x30 (LR)
  ;;   [sp+224..239] padding (2 slots)
  ;; WS4-AA64 #160 Stage 1: when *aarch64-gc-native-mcgc*, emit the NATIVE
  ;; Cheney collector at the trampoline label (no Lisp %gc-collect needed);
  ;; otherwise the Lisp-%gc-collect-calling trampoline below (unchanged).
  (when (and *aarch64-gc-native-mcgc* *aarch64-gc-trampoline-label*)
    (emit-aarch64-native-gc-trampoline buf))
  (when (and (not *aarch64-gc-native-mcgc*)
             *aarch64-gc-trampoline-label*
             *aarch64-gc-collect-bytecode-offset*)
    (a64-set-label buf *aarch64-gc-trampoline-label*)
    ;; Save x0..x17 as nine STP pairs (also allocates 240-byte frame
    ;; via the first pair's pre-index).
    (a64-stp-pre    buf +a64-x0+  +a64-x1+  +a64-sp+ -240)
    (a64-stp-offset buf +a64-x2+  +a64-x3+  +a64-sp+ 16)
    (a64-stp-offset buf +a64-x4+  +a64-x5+  +a64-sp+ 32)
    (a64-stp-offset buf +a64-x6+  +a64-x7+  +a64-sp+ 48)
    (a64-stp-offset buf +a64-x8+  +a64-x9+  +a64-sp+ 64)
    (a64-stp-offset buf +a64-x10+ +a64-x11+ +a64-sp+ 80)
    (a64-stp-offset buf +a64-x12+ +a64-x13+ +a64-sp+ 96)
    (a64-stp-offset buf +a64-x14+ +a64-x15+ +a64-sp+ 112)
    (a64-stp-offset buf +a64-x16+ +a64-x17+ +a64-sp+ 128)
    ;; Save x18 (platform reg used for some traps), x19..x23 (AAPCS
    ;; callee-saved — Modus's vreg spill slots), x26 (NIL), x27 (CENV),
    ;; x29 (FP), x30 (LR).  x28 is not defined as a constant and is
    ;; unused by Modus codegen, so we skip it.
    (a64-str-unsigned buf +a64-x18+ +a64-sp+ 144)
    (a64-str-unsigned buf +a64-x19+ +a64-sp+ 152)
    (a64-str-unsigned buf +a64-x20+ +a64-sp+ 160)
    (a64-str-unsigned buf +a64-x21+ +a64-sp+ 168)
    (a64-str-unsigned buf +a64-x22+ +a64-sp+ 176)
    (a64-str-unsigned buf +a64-x23+ +a64-sp+ 184)
    (a64-str-unsigned buf +a64-x26+ +a64-sp+ 192)
    (a64-str-unsigned buf +a64-x27+ +a64-sp+ 200)
    (a64-str-unsigned buf +a64-x29+ +a64-sp+ 208)
    (a64-str-unsigned buf +a64-x30+ +a64-sp+ 216)
    ;; RAW-ADDR-AUDIT: the trampoline writes RAW byte addresses (x24,
    ;; x25, SP+240) but %gc-collect reads them via (mem-ref ADDR :u64)
    ;; which treats the loaded bits as a Lisp value (low-bit-0 fixnum
    ;; = value/2).  To keep the round-trip lossless we LSL by 1 before
    ;; STR and ASR by 1 after LDR — that way the bytes deposited at
    ;; 0x10000068/70/78 match what mem-ref :u64 will reconstruct as the
    ;; raw byte address back in Lisp.  Without this the trampoline
    ;; "succeeds" (we even see %gc-collect's '12345' progress markers)
    ;; and *then* the runtime silently corrupts itself.  Any future
    ;; raw-address slot that's read on both the native and Lisp sides
    ;; needs the same SHL/ASR-by-1 dance — see also
    ;; reference_aarch64_gc_trampoline.md for the original debugging.
    ;;
    ;; Stash x24 (alloc ptr) → 0x10000070.  %gc-collect reads this
    ;; via (mem-ref ADDR :u64) and treats the result as a Lisp
    ;; integer.  Modus tags fixnums by SHL 1, so the stored 64-bit
    ;; word must already be SHL'd: we LSL x24 by 1 before storing,
    ;; and on the way back out (after %gc-collect) we ASR by 1.
    (a64-load-imm64 buf +a64-x16+ #x10000070)
    (a64-lsl-imm buf +a64-x17+ +a64-x24+ 1)       ; x17 = x24 << 1 (Lisp-tagged)
    (a64-str-unsigned buf +a64-x17+ +a64-x16+ 0)
    ;; Stash x25 (alloc limit) → 0x10000078 (same SHL convention).
    (a64-load-imm64 buf +a64-x16+ #x10000078)
    (a64-lsl-imm buf +a64-x17+ +a64-x25+ 1)
    (a64-str-unsigned buf +a64-x17+ +a64-x16+ 0)
    ;; Stash CURRENT SP → 0x10000068.  The trampoline's register-save
    ;; area at [sp, sp+232) IS a root region: any tagged pointer in
    ;; x0..x23/x26/x27 that's now sitting on the stack must be
    ;; forwarded by the GC scan or its restore at trampoline exit
    ;; uncovers a stale pre-GC pointer.  Pre-GC bug: we used SP+240
    ;; here, which skipped past the saved-reg frame and missed roots
    ;; — the kernel "worked" until a saved closure pointer landed in
    ;; the saved-reg frame and the post-GC restore handed it back as
    ;; a forwarding-tagged value, which then took funcall to a stale
    ;; object and wedged the runtime.  Stored SHL'd to match
    ;; (mem-ref :u64)'s tagging convention on the Lisp side.
    (a64-load-imm64 buf +a64-x16+ #x10000068)
    ;; LSL encodes reg 31 as XZR, so (lsl x17, sp, 1) produced (lsl x17, xzr, 1)
    ;; = 0, and every GC stored saved_rsp = 0.  Move SP to x17 via ADD-IMM
    ;; (which encodes reg 31 as SP) FIRST, then shift.  Diagnosed via gdb
    ;; watchpoint on 0x10000068 (2026-05-17): str x17, [x16] preceded by
    ;; lsl x17, xzr, #1 — caught the offending instruction.
    (a64-add-imm buf +a64-x17+ +a64-sp+ 0)        ; mov x17, sp
    (a64-lsl-imm buf +a64-x17+ +a64-x17+ 1)       ; x17 = SP << 1
    (a64-str-unsigned buf +a64-x17+ +a64-x16+ 0)
    ;; Set NARGS = 0 for the %gc-collect call (the ABI puts nargs at
    ;; raw u32 slot 0x10000150; only matters if the callee has an
    ;; arity check, but emit it for parity with the standard call
    ;; sequence).
    (a64-load-imm64 buf +a64-x17+ #x10000150)
    (a64-movz buf +a64-x16+ 0 0)
    ;; STUR w16, [x17] — 32-bit store; reuse a64-emit raw.
    (a64-emit buf (logior #xB8000010                ; STUR Wt, [Xn, #imm9]
                          (ash 0 12)                ; imm9 = 0
                          (ash 17 5)                ; Rn = x17
                          16))                      ; Rt = w16
    ;; Call %gc-collect via fn-addr-patched MOVZ+MOVK+BLR.  The
    ;; cross-link patcher will fill in the imm16 fields once
    ;; %gc-collect's native-offset is known.
    ;;
    ;; The patcher (apply-aarch64-fn-addr-patches in cross.lisp) ORs
    ;; the loaded vaddr with +tag-function+ (3) per TAG-PLAN.md.  We
    ;; SUB-3 here to strip the tag before BLR — same convention as
    ;; +op-call-ind+.  Without this strip, BLR targets a tagged
    ;; misaligned address and faults with sync exception when GC
    ;; first triggers.  Diagnosed via gdb at T:13637 wedge:
    ;; ELR_EL1=FAR_EL1=(gc-collect-entry|3), entry-4 handler reached
    ;; HALT loop because slot 180/1C0 were both unarmed at that
    ;; inter-test moment.
    (let ((movz-byte-pos
           (* (- (a64-current-index buf)
                 (or *aarch64-translated-start-idx* 0))
              4)))
      (push (cons movz-byte-pos *aarch64-gc-collect-bytecode-offset*)
            *aarch64-fn-addr-patches*))
    (a64-movz buf +a64-x16+ 0 0)              ; placeholder for low 16
    (a64-movk buf +a64-x16+ 0 1)              ; placeholder for high 16
    (a64-sub-imm buf +a64-x16+ +a64-x16+ 3)   ; strip +tag-function+
    (a64-blr buf +a64-x16+)
    ;; Reload x24, x25 from (now-updated) metadata.  %gc-collect
    ;; wrote these via (setf (mem-ref ADDR :u64) FREE-PTR) — and
    ;; since Lisp fixnums are stored SHL'd in registers, :u64 emits
    ;; the SHL'd 64-bit pattern.  ASR by 1 to recover the raw address.
    (a64-load-imm64 buf +a64-x16+ #x10000070)
    (a64-ldr-unsigned buf +a64-x24+ +a64-x16+ 0)
    (a64-asr-imm buf +a64-x24+ +a64-x24+ 1)
    (a64-load-imm64 buf +a64-x16+ #x10000078)
    (a64-ldr-unsigned buf +a64-x25+ +a64-x16+ 0)
    (a64-asr-imm buf +a64-x25+ +a64-x25+ 1)
    ;; Restore caller-saved regs and frame.
    (a64-ldr-unsigned buf +a64-x30+ +a64-sp+ 216)
    (a64-ldr-unsigned buf +a64-x29+ +a64-sp+ 208)
    (a64-ldr-unsigned buf +a64-x27+ +a64-sp+ 200)
    (a64-ldr-unsigned buf +a64-x26+ +a64-sp+ 192)
    (a64-ldr-unsigned buf +a64-x23+ +a64-sp+ 184)
    (a64-ldr-unsigned buf +a64-x22+ +a64-sp+ 176)
    (a64-ldr-unsigned buf +a64-x21+ +a64-sp+ 168)
    (a64-ldr-unsigned buf +a64-x20+ +a64-sp+ 160)
    (a64-ldr-unsigned buf +a64-x19+ +a64-sp+ 152)
    (a64-ldr-unsigned buf +a64-x18+ +a64-sp+ 144)
    (a64-ldp-offset buf +a64-x16+ +a64-x17+ +a64-sp+ 128)
    (a64-ldp-offset buf +a64-x14+ +a64-x15+ +a64-sp+ 112)
    (a64-ldp-offset buf +a64-x12+ +a64-x13+ +a64-sp+ 96)
    (a64-ldp-offset buf +a64-x10+ +a64-x11+ +a64-sp+ 80)
    (a64-ldp-offset buf +a64-x8+  +a64-x9+  +a64-sp+ 64)
    (a64-ldp-offset buf +a64-x6+  +a64-x7+  +a64-sp+ 48)
    (a64-ldp-offset buf +a64-x4+  +a64-x5+  +a64-sp+ 32)
    (a64-ldp-offset buf +a64-x2+  +a64-x3+  +a64-sp+ 16)
    (a64-ldp-post buf +a64-x0+ +a64-x1+ +a64-sp+ 240)
    (a64-ret buf)))

;;; ============================================================
;;; Main Translation Entry Point
;;; ============================================================

(defun translate-mvm-to-aarch64 (bytecode function-table)
  "Translate MVM bytecode to AArch64 native code.
   BYTECODE is a vector of (unsigned-byte 8) containing MVM instructions.
   FUNCTION-TABLE is a hash table mapping function index → MVM byte offset,
   or NIL if translation is for a single function body.

   Returns (values a64-buffer fn-offset-map) where fn-offset-map maps
   MVM bytecode offset → native byte offset for each function entry point.

   The translation proceeds in multiple passes:
     1. Decode all MVM instructions
     2. Build byte-offset → label mapping for branch targets
     3. Emit prologue
     4. Translate each instruction, recording native label positions
     5. Emit epilogue
     6. Resolve all branch fixups (skipped if appending into a shared
        buffer; caller resolves once after all emit)."
  ;; Reset the li-const patch list for a fresh module translation
  ;; (single-function translations — function-table nil — must not
  ;; drop a pending module's patches).
  (when function-table
    (setf *aarch64-li-const-patches* nil))
  ;; WS4 aarch64 Stage 3/4: fresh JIT call-reloc + fn-addr-reloc lists per module
  ;; (only populated under *aarch64-jit-mode*; nil otherwise so image-build
  ;; codegen is unchanged).
  (setf *aarch64-call-relocs* nil)
  (setf *aarch64-fn-addr-relocs* nil)
  (let* ((buf (or *aarch64-translate-into-buf* (make-a64-buffer)))
         ;; Index (instruction units) where translated code starts within
         ;; buf.  Zero when buf is a fresh one; non-zero when we're
         ;; appending into a pre-filled boot buffer.  fn-entry-offsets
         ;; are stored RELATIVE TO THIS START so existing downstream code
         ;; (kernel-image-entry-point arithmetic, JMP/B emission, etc.)
         ;; doesn't have to know whether the buffer was shared.
         (translated-start-idx (a64-buffer-position buf))
         ;; Expose to trap-time code (e.g. fn-addr patch site recorder)
         ;; via dynamic variable; see *aarch64-translated-start-idx*.
         (*aarch64-translated-start-idx* translated-start-idx)
         (insns (decode-mvm-stream bytecode))
         (offset-map (build-offset-to-index-map insns))
         (mvm-to-native-label (make-hash-table :test 'equal))
         ;; Pre-assign labels for all MVM byte offsets that might be
         ;; branch targets. We assign labels for every instruction
         ;; position conservatively.
         (mvm-offset-to-native-index (make-hash-table :test 'eql))
         ;; Track function entry positions: bytecode-offset → native-byte-offset
         (fn-entry-offsets (make-hash-table :test 'eql))
         (fn-bc-offsets (make-hash-table :test 'eql)))

    ;; Pre-register function entry points in the label table
    (when function-table
      (maphash (lambda (func-idx mvm-offset)
                 (let ((label (incf *mvm-label-counter*)))
                   (setf (gethash (list :func func-idx) mvm-to-native-label) label)
                   (setf (gethash mvm-offset mvm-to-native-label) label)
                   ;; Remember this bytecode offset is a function entry
                   (setf (gethash mvm-offset fn-bc-offsets) t)))
               function-table))

    ;; Pre-pass: register labels for ALL branch targets (including backward branches)
    (dolist (insn insns)
      (let ((op (decoded-mvm-insn-opcode insn))
            (operands (decoded-mvm-insn-operands insn)))
        (when (and (>= op #x40) (<= op #x48))  ; BR through BNNULL
          (let* ((off-idx (if (or (= op #x47) (= op #x48)) 1 0))  ; BNULL/BNNULL have Vs first
                 (mvm-offset (nth off-idx operands))
                 (target-byte (+ (decoded-mvm-insn-offset insn)
                                 (decoded-mvm-insn-size insn)
                                 mvm-offset)))
            (unless (gethash target-byte mvm-to-native-label)
              (setf (gethash target-byte mvm-to-native-label)
                    (incf *mvm-label-counter*)))))))

    ;; Pass 1: Translate all instructions
    (dolist (insn insns)
      (let ((mvm-off (decoded-mvm-insn-offset insn)))
        ;; Function-entry alignment.  Fn pointers are tagged with
        ;; +tag-function+ (3) at LI-FUNC patch time (see cross.lisp
        ;; apply-aarch64-fn-addr-patches), and CALL-IND strips with
        ;; SUB-3 before BLR.  For the OR-3 to produce a clean tag
        ;; the raw native address must have low nibble 0, i.e.,
        ;; 16-byte aligned.
        ;;
        ;; The final native VA of an entry is
        ;;   load-addr + native-image-offset + target-native-offset
        ;;     = load-addr + translated-start-idx*4
        ;;                 + (current-index - translated-start-idx)*4
        ;;     = load-addr + current-index*4
        ;; Both supported load-addrs (QEMU virt 0x40080000, RPi
        ;; 0x80000) are 16-byte aligned, so the constraint reduces
        ;; to (current-index mod 4) == 0.  Pad with NOPs.
        ;;
        ;; Exception: Linux/AArch64 (ELF wrap) prepends 120 bytes of
        ;; ELF header before the native bytes — so the runtime VA is
        ;; (load + 120 + native_offset).  120 mod 16 = 8, which means
        ;; we need (native_offset mod 16 = 8) to land VAs on 16.
        ;; *aarch64-fn-align-offset* (defaults to 0 = legacy bare-metal)
        ;; carries that offset into the alignment loop.
        (when (gethash mvm-off fn-bc-offsets)
          (loop while (/= 0 (mod (+ (* (a64-current-index buf) 4)
                                    *aarch64-fn-align-offset*)
                                 16))
                do (a64-emit buf #xD503201F)))  ; NOP
        ;; If this MVM offset has a label assigned (branch target),
        ;; record the native position for it now
        (let ((label (gethash mvm-off mvm-to-native-label)))
          (when label
            (a64-set-label buf label)))
        ;; Record native position for function entries.  Subtract
        ;; translated-start-idx so offsets stay relative to the start
        ;; of the translated region (whether or not buf was shared
        ;; with a boot preamble).
        (when (gethash mvm-off fn-bc-offsets)
          (setf (gethash mvm-off fn-entry-offsets)
                (* (- (a64-current-index buf) translated-start-idx) 4)))
        ;; Record MVM offset → native index mapping
        (setf (gethash mvm-off mvm-offset-to-native-index)
              (a64-current-index buf))
        ;; Translate the instruction
        (translate-mvm-insn insn buf mvm-to-native-label)))

    ;; Any labels pointing to the end of the bytecode stream
    (let ((end-offset (length bytecode)))
      (let ((label (gethash end-offset mvm-to-native-label)))
        (when label
          (a64-set-label buf label))))

    ;; If we're emitting into a shared buffer AND handler-stack labels
    ;; are bound, append the push/pop helpers after all translated code.
    ;; They sit at the tail of native-code (past kernel-main and every
    ;; defun) so adding them doesn't shift any fn-entry-offset.  At
    ;; Phase 3(a) these are dead code — no trap BLs to them yet.
    (when *aarch64-translate-into-buf*
      (emit-aarch64-handler-helpers buf))

    ;; Pass 2: Resolve all branch fixups.  Skip if we're appending into
    ;; a shared buffer — the caller will resolve once after appending
    ;; its own emit (e.g. handler-stack helpers, IRQ vector BLs to those
    ;; helpers from boot code).
    (unless *aarch64-translate-into-buf*
      (a64-resolve-fixups buf))

    (values buf fn-entry-offsets)))

(defun translate-mvm-function (bytecode)
  "Translate a single MVM function body to AArch64 native code.
   Wraps the translated code with prologue and epilogue.
   Returns the native bytes as a (vector (unsigned-byte 8))."
  (let* ((buf (make-a64-buffer)))
    ;; Emit prologue
    (a64-emit-prologue buf)
    ;; Translate the body
    (let* ((body-buf (translate-mvm-to-aarch64 bytecode nil))
           (body-code (a64-buffer-code body-buf))
           (body-len (a64-buffer-position body-buf)))
      ;; Copy body instructions into our buffer
      (dotimes (i body-len)
        (a64-emit buf (aref body-code i))))
    ;; Emit epilogue
    (a64-emit-epilogue buf)
    ;; Convert to bytes
    (a64-buffer-to-bytes buf)))

;;; ============================================================
;;; Multi-function Translation
;;; ============================================================

(defun translate-mvm-image (bytecode function-table)
  "Translate an entire MVM image (multiple functions) to AArch64.
   FUNCTION-TABLE maps function-index → (mvm-byte-offset . arity).

   Returns a byte vector of AArch64 machine code with each function
   preceded by its prologue and followed by its epilogue.

   Also returns as a second value a hash table mapping
   function-index → native-byte-offset."
  ;; Reset fn-addr + li-const patch lists at start of each image
  ;; translation.  Patches accumulated here are applied by cross.lisp
  ;; after image assembly; see *aarch64-fn-addr-patches* docstring.
  (setf *aarch64-fn-addr-patches* nil)
  (setf *aarch64-li-const-patches* nil)
  (let* ((buf (make-a64-buffer))
         (func-offsets (make-hash-table :test 'eql))
         (mvm-to-native-label (make-hash-table :test 'equal))
         ;; Collect function entries sorted by offset
         (func-entries nil))

    ;; Build sorted list of (func-idx mvm-offset arity)
    (maphash (lambda (idx info)
               (let ((off (if (consp info) (car info) info))
                     (arity (if (consp info) (cdr info) 0)))
                 (push (list idx off arity) func-entries)))
             function-table)
    (setf func-entries (sort func-entries #'< :key #'second))

    ;; Pre-assign labels for function entries
    (dolist (entry func-entries)
      (destructuring-bind (func-idx mvm-off arity) entry
        (declare (ignore arity))
        (let ((label (incf *mvm-label-counter*)))
          (setf (gethash (list :func func-idx) mvm-to-native-label) label)
          (setf (gethash mvm-off mvm-to-native-label) label))))

    ;; Decode all MVM instructions
    (let ((all-insns (decode-mvm-stream bytecode)))

      ;; Translate all instructions with prologues at function boundaries
      (let ((func-offsets-set (make-hash-table :test 'eql)))
        (dolist (entry func-entries)
          (setf (gethash (second entry) func-offsets-set) (first entry)))

        (dolist (insn all-insns)
          (let* ((mvm-off (decoded-mvm-insn-offset insn))
                 (func-idx (gethash mvm-off func-offsets-set)))
            ;; Emit prologue at function entry points
            (when func-idx
              (let ((label (gethash (list :func func-idx) mvm-to-native-label)))
                (when label
                  (a64-set-label buf label)))
              (setf (gethash func-idx func-offsets)
                    (* (a64-current-index buf) 4))
              (a64-emit-prologue buf))

            ;; Set label if this offset is a branch target
            (let ((label (gethash mvm-off mvm-to-native-label)))
              (when (and label (not func-idx))
                (a64-set-label buf label)))

            ;; Translate the instruction
            ;; Special handling: MVM-RET becomes epilogue
            (if (= (decoded-mvm-insn-opcode insn) +op-ret+)
                (a64-emit-epilogue buf)
                (translate-mvm-insn insn buf mvm-to-native-label))))))

    ;; Handle labels pointing past the last instruction
    (let ((end-offset (length bytecode)))
      (let ((label (gethash end-offset mvm-to-native-label)))
        (when label
          (a64-set-label buf label))))

    ;; Resolve fixups
    (a64-resolve-fixups buf)

    (values (a64-buffer-to-bytes buf) func-offsets)))

;;; ============================================================
;;; Target Descriptor Installation
;;; ============================================================

(defun install-aarch64-translator ()
  "Install the AArch64 translator into the target descriptor.
   Sets the translate-fn, emit-prologue, and emit-epilogue slots
   on *target-aarch64*."
  (setf (target-translate-fn *target-aarch64*)
        #'translate-mvm-to-aarch64)
  (setf (target-emit-prologue *target-aarch64*)
        (lambda (target buf)
          (declare (ignore target))
          (a64-emit-prologue buf)))
  (setf (target-emit-epilogue *target-aarch64*)
        (lambda (target buf)
          (declare (ignore target))
          (a64-emit-epilogue buf)))
  :aarch64)

;;; ============================================================
;;; Diagnostic: Disassemble Native Buffer
;;; ============================================================

(defun a64-disassemble-buffer (buf &key (start 0) (end nil))
  "Print a hex dump of the AArch64 instruction buffer for debugging."
  (let* ((code (a64-buffer-code buf))
         (limit (or end (a64-buffer-position buf))))
    (loop for i from start below limit
          do (format t "  ~4,'0X: ~8,'0X~%" (* i 4) (aref code i)))))

(defun a64-instruction-count (buf)
  "Return the number of instructions in the buffer."
  (a64-buffer-position buf))

(defun a64-code-size (buf)
  "Return the total code size in bytes."
  (* (a64-buffer-position buf) 4))
