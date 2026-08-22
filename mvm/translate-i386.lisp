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
;;;
;;; RELOCATABLE (WS5 i386-CL): these were `defconstant`s pinned at #x600.
;;; That works for bare-metal (low RAM is ours) but a HOSTED Linux/i386 ELF
;;; cannot map anything below mmap_min_addr (65536 by default), so the whole
;;; slot block has to move.  They are therefore `defparameter`s derived from
;;; *i386-globals-base*; call I386-SET-GLOBALS-BASE to relocate the block.
;;; Defaults are unchanged (#x600/#x604/#x608) so every existing bare-metal
;;; i386 build emits byte-identical code.
(defparameter *i386-globals-base* #x600
  "Base of the i386 absolute-address global slot block.")
(defparameter *va-addr*      #x600)  ; Alloc pointer (absolute address)
(defparameter *vl-addr*      #x604)  ; Alloc limit (absolute address)
(defparameter *vn-addr*      #x608)  ; NIL constant (absolute address)
;;; --- WS5 additions: slots that x64/aarch64 keep in spare PHYSICAL registers.
;;; i386 has no spare register (EAX=VR, ECX/EDX=scratch, EBX=V4, ESI=V0,
;;; EDI=V1, ESP/EBP), so the closure-env "register" and the nargs/MV-count
;;; convention slots all live in this block instead.  Single-threaded
;;; cooperative execution makes a global slot exactly as correct as x64's R13:
;;; the caller writes it immediately before the (indirect) call and the callee
;;; reads it as its first prologue action.
(defparameter *nargs-addr*   #x60C)  ; nargs convention slot (raw, untagged)
(defparameter *cenv-addr*    #x610)  ; closure-env "register" (x64: R13)
;;; MV-COUNT IS **NOT** A PRIVATE i386 SLOT — it is a SHARED contract address.
;;;
;;; Unlike nargs and cenv (whose only writer AND only reader are this
;;; translator's :set-nargs/:get-nargs and :set-cenv/:get-cenv), the
;;; multiple-value count is read and written by ARCHITECTURE-INDEPENDENT code:
;;; mvm/compiler.lisp bakes `+mv-count-addr+` (#x10000090) into the mem-ref
;;; forms it expands MULTIPLE-VALUE-BIND / MULTIPLE-VALUE-LIST / VALUES / NTH-
;;; VALUE into, and shared CL source (prelude.lisp %MV-TO-LIST, cl-eval.lisp,
;;; cl-clos.lisp, gc.lisp) reads the same literal.  translate-x64 and
;;; translate-aarch64 therefore emit :set-mv-count against #x10000090 too.
;;;
;;; This slot used to be relocated with the rest of the block, so on i386 the
;;; ONE writer (:set-mv-count) pointed at an address with ZERO readers: every
;;; function epilogue's "I returned exactly one value" reset was DISCARDED,
;;; while `(values a b c)` still stored 3 at #x10000090 through a compiler-
;;; emitted mem-ref.  The count was therefore monotonically stale-high, and any
;;; later (multiple-value-bind (x y) (f)) where F had internally produced
;;; multiple values read PHANTOM secondaries out of #x10000098.  Symptom:
;;; `(multiple-value-list (typep 1 'integer))` => ("T" T) on i386 vs (T) on x64.
;;; Keep this EQ to modus.mvm::+mv-count-addr+; it must not be relocated.
(defparameter *mvcount-addr* modus.mvm::+mv-count-addr+)  ; SHARED, see above
(defparameter *gc-page-base-addr* #x618)  ; raw from_start, for the bit-set
(defparameter *gc-startbmp-addr*  #x61C)  ; raw object-start bitmap base
(defparameter *gc-consbmp-addr*   #x620)  ; raw cons-kind bitmap base

(defun i386-set-globals-base (base)
  "Relocate the i386 absolute-address global slot block to BASE.
   Bare metal keeps the #x600 default; a hosted Linux ELF must pass an
   address inside a mapped LOAD segment (see boot/boot-linux-i386.lisp)."
  (setf *i386-globals-base* base
        *va-addr*      (+ base #x00)
        *vl-addr*      (+ base #x04)
        *vn-addr*      (+ base #x08)
        *nargs-addr*   (+ base #x0C)
        *cenv-addr*    (+ base #x10)
        ;; base+#x14 stays RESERVED (do not reuse): *mvcount-addr* is a shared
        ;; contract address, never relocated.  See its defparameter above.
        *gc-page-base-addr* (+ base #x18)
        *gc-startbmp-addr*  (+ base #x1C)
        *gc-consbmp-addr*   (+ base #x20))
  base)

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
;;; WS5 i386-CL translator knobs
;;; ============================================================

(defvar *i386-li-const-patches* nil
  "List of (native-byte-offset . pool-index) recorded by +op-li-const+.
   The offset points at the 4-byte immediate field of a `MOV r32, imm32`
   placeholder; cross.lisp's APPLY-LI-CONST-PATCHES writes the tagged
   constant-pool address there once the final image layout is known.
   Counterpart of *x64-li-const-patches* / *aarch64-li-const-patches*.")

(defparameter *i386-fn-tag-3* nil
  "When true, :fn-addr tags function addresses with the +3 function tag
   (TAG-PLAN.md; tags cons=1, fn=3, char=5, obj=9 are disjoint) and
   :call-ind strips it before the indirect call — the x64/aarch64 contract
   the shared CL runtime's funcall dispatch and FUNCTIONP assume.

   Default NIL keeps the LEGACY bare-metal i386 builds (mvm/repl-source.lisp
   and friends) byte-identical: they never reached a working :fn-addr (it was
   an unimplemented opcode that emitted INT3), so nothing there depends on
   either convention.  The CL/mvm-eval i386 build sets this to T.

   NB: with tagging on, function entry points MUST be 16-byte aligned in the
   FINAL virtual address so OR-3 yields a clean tag — see
   *i386-fn-align* / *i386-native-code-offset*.")

(defparameter *i386-fn-align* nil
  "When non-nil, an integer alignment (16) applied to every function entry
   point.  Padding is computed against (+ *i386-native-code-offset* pos) so
   the alignment holds at the RUNTIME virtual address, not merely at the
   buffer offset.  Required whenever *i386-fn-tag-3* is on.")

(defparameter *i386-record-unimpl* t
  "Record every MVM opcode that hits the translator's INT3 default, in
   *i386-unimpl-ops*.  A silent INT3 is the worst failure mode when bringing
   a target up: the image builds clean and then dies with no explanation.")

(defvar *i386-unimpl-ops* nil
  "Hash of opcode -> number of INT3 placeholders emitted for it.")

(defparameter *i386-safe-nop-traps* (list #x0520)
  "Trap codes that are GENUINELY safe to no-op on i386, with the reason.
   Everything NOT on this list fails loudly — see I386-EMIT-UNIMPL-TRAP.

   #x0520 INSTALL-SIGNAL-HANDLERS — pure side effect, no result, no control
     transfer.  Skipping it means a hardware fault is fatal instead of being
     longjmp-recovered into a handler-case; that DEGRADES diagnostics but
     cannot manufacture a wrong value.  Safe.

   Deliberately NOT on this list, because each either PRODUCES A VALUE or
   TRANSFERS CONTROL, so a no-op silently yields garbage that flows onward:
   #x0510 SETJMP (returns 0/nonzero), #x0511 LONGJMP (must transfer control),
   #x0512 CLEAR-HANDLER (pops handler state), #x0530 COPY-OVERFLOW-ARGS
   (materialises arguments 5+ — a no-op hands the callee stack garbage),
   #x0531 %MMAP-EXEC-PAGE (returns a page address).")

(defparameter *i386-eax-live-traps* (list #x0510 #x0511 #x0532)
  "Trap codes whose ARM OWNS EAX, so the :trap dispatch must NOT bracket them
   with push/pop EAX.  On i386 VR is EAX, and these are the only traps that
   traffic in VR:

   #x0510 SETJMP     — compiler.lisp emits (:trap #x0510) (:mov dest VR), so
     the result has to arrive in VR.  A trailing `pop eax` would restore the
     pre-trap EAX, setjmp would always read as NIL, and handler-case would
     silently never take its handler path.
   #x0511 LONGJMP    — hands the T sentinel to the setjmp resume point in EAX
     and never returns; the bracket's pop is unreachable anyway.
   #x0532 %JIT-CALL  — compiler.lisp emits (:trap #x0532) (:mov dest VR), so
     the JIT'd callee's return value arrives in EAX and the bracket's pop
     would restore the pre-call EAX and discard it.  Exactly SETJMP's case.

   #x0512 CLEAR-HANDLER is deliberately ABSENT: there EAX holds the
   handler-case body's result (dest==VR==EAX) and the bracket is exactly what
   preserves it across the pop helper.")

(defun i386-emit-unimpl-trap (buf code)
  "Emit code that NAMES ITSELF when an unimplemented trap is reached.

   Why this is not a NOP: a silent no-op on a load-bearing trap does not
   disable a feature, it produces GARBAGE THAT FLOWS ONWARD.  The 243
   no-op'd COPY-OVERFLOW-ARGS sites are why a missing >4-argument calling
   convention presented as a baffling SIGSEGV deep inside GENERIC-LOGAND
   instead of saying what was actually missing.  Same failure-mode family as
   a handler-case that masks a buffer overflow as \"giving up\", or a boot
   SIGSEGV handler that turns a real bug into a false pass.

   Hosted Linux: write(2, \"...trap #xNNNN\", n) then _exit(70).  The message
   is materialised position-independently with the call/pop idiom (i386 has
   no RIP-relative addressing): `call .do` pushes the address of the bytes
   that follow it, which ARE the string.
   Bare metal: INT3, the loudest thing available without a console."
  (i386-emit-abort-msg buf
                       (format nil "MODUS i386: unimplemented trap #x~4,'0X~%" code)
                       70))

(defun i386-emit-abort-msg (buf msg status)
  "Emit code that writes MSG to stderr and _exit(STATUS).  Hosted Linux only;
   bare metal gets INT3 (the loudest thing available without a console).

   The message is materialised position-independently with the call/pop idiom
   (i386 has no RIP-relative addressing): `call .do` pushes the address of the
   bytes that follow it, which ARE the string.

   Factored out of I386-EMIT-UNIMPL-TRAP so the handler-case traps can NAME a
   structurally impossible state (a longjmp with nothing armed) instead of
   jumping to a null saved IP — the same reasoning that made an unimplemented
   trap say its own number rather than silently no-op."
  (if *i386-linux-mode*
      (let ((do-lbl (i386-make-label))
            (str-lbl (i386-make-label)))
        (i386-emit-jmp-rel32 buf str-lbl)
        (i386-emit-label buf do-lbl)
        (i386-emit-pop-reg buf +i386-ecx+)                 ; ecx = &msg
        (i386-emit-mov-reg-imm buf +i386-ebx+ 2)           ; fd = stderr
        (i386-emit-mov-reg-imm buf +i386-edx+ (length msg)); count
        (i386-emit-mov-reg-imm buf +i386-eax+ 4)           ; SYS_write
        (i386-emit-byte buf #xCD) (i386-emit-byte buf #x80)
        (i386-emit-mov-reg-imm buf +i386-ebx+ status)
        (i386-emit-mov-reg-imm buf +i386-eax+ 1)           ; SYS_exit
        (i386-emit-byte buf #xCD) (i386-emit-byte buf #x80)
        (i386-emit-label buf str-lbl)
        (i386-emit-call-rel32 buf do-lbl)                  ; pushes &msg
        (loop for ch across msg do (i386-emit-byte buf (char-code ch))))
      (i386-emit-int3 buf)))

(defparameter *i386-linux-mode* nil
  "When true, the target is a HOSTED Linux/i386 ELF rather than bare metal:
   the serial-I/O traps become write(2) syscalls and the Linux syscall traps
   (#x0500 exit, #x0502 syscall3, #x0503 syscall3-raw) are emitted.
   Counterpart of *x64-linux-mode*.")

(defparameter *i386-native-code-offset* 0
  "Virtual-address offset of native-code byte 0 within the final image.
   Only used to make *i386-fn-align* padding correct; mirrors
   *x64-native-code-offset*.")

;;; GENERIC-ADD / -SUBTRACT / -MULTIPLY entry labels, resolved by name in
;;; TRANSLATE-MVM-TO-I386.  NIL (e.g. an image that does not define them, or
;;; single-instruction translation) makes the :add/:sub/:mul-checked opcodes
;;; degrade to plain wrapping arithmetic, exactly like translate-x64's
;;; fallback branch.
(defparameter *i386-checked-arith-slowpath* t
  "When NIL, :add/:sub/:mul-checked degrade to plain WRAPPING arithmetic even
   if GENERIC-ADD etc. are present.  An A/B knob: it isolates the new checked
   ops from the rest of i386 codegen when triaging a miscompile.")

(defparameter *i386-gc-bitmap-enabled* t
  "Emit the object-start / cons-kind bit-set at every allocation site.
   Must be ON whenever the collector is: without the bitmaps scan_word has no
   conservative-root validation and copy_object stamps forwarding pointers over
   mid-object data.  Like *i386-gc-enabled*, it takes effect only in
   *i386-linux-mode* — the bitmaps are mmap'd by boot/boot-linux-i386.lisp, and
   with a null base the bit-set would write through a garbage pointer.")

;;; ---- #259: allocation-payload initialisation -------------------------
;;; The Cheney scan (i386-emit-gc-trampoline) walks to-space as a FLAT
;;; word-by-word sweep and calls scan_word on EVERY 4-byte word, headers and
;;; padding included.  Any word an allocator leaves unwritten is therefore
;;; read as a conservative root candidate.  On a 32-bit word that is a much
;;; worse proposition than on x64: a random word has roughly 1/128 odds of
;;; carrying tag 1 or 9 AND landing inside a 256 MB semispace, whereas a
;;; random 64-bit word essentially never forms a plausible heap pointer.
;;;
;;; i386 leaves MORE unwritten than x64 does, because 16-byte alignment is a
;;; 4-word granule here and only a 2-word granule there:
;;;   - a CONS is 16 bytes but only car(+0) and cdr(+4) are ever written, so
;;;     HALF of every cons is uninitialised — and conses dominate the heap;
;;;   - copy_object's cons arm copies 8 bytes and advances the free pointer by
;;;     16, so the two stale tail words travel into to-space at every
;;;     collection and are swept again;
;;;   - :alloc-obj with count 1 or 2 rounds up to 16 bytes, leaving 1-2 words;
;;;   - :alloc-array / :alloc-string / :alloc-u8 leave the WHOLE payload
;;;     unwritten until the caller fills it, a window in which any intervening
;;;     allocation can collect.
(defparameter *i386-alloc-fill* :zero
  "How allocation sites initialise the words they do not otherwise write.
     NIL     — leave them (pre-#259 behaviour).
     :ZERO   — store 0, so every unwritten word is fixnum 0 (tag 0) and
               scan_word rejects it on the tag test alone.
     :POISON — store a distinctive even (fixnum-tagged, therefore inert)
               sentinel, for the diagnostic build that counts how many
               unwritten words the collector actually reads.")

(defparameter *i386-fill-strings* nil
  "Whether :alloc-string also fills its payload.  Default NIL, mirroring
   translate-x64.lisp's deliberate exclusion: zeroing string CONTENT there
   turned garbage into NUL (char-code 0) and desynchronised the reader, a
   functional regression proven distinct from layout shift.  The i386 arm
   keeps the same conservative default.")

(defconstant +i386-fill-poison-cons+ #x51510000
  "Diagnostic sentinel for the two unwritten tail words of a cons.  Low
   nibble 0 = fixnum, so it can never be mistaken for a root.")
(defconstant +i386-fill-poison-obj+  #x52520000
  "Diagnostic sentinel for unwritten object payload / alignment-tail words.")

(defparameter *i386-gc-instrument* nil
  "Emit conservative-root counters into scan_word and a 32-byte binary dump
   at the end of every collection.  Diagnostic only; never ship enabled.")

(defparameter *i386-gcdbg-base* #x1001E000
  "BSS counter block for *i386-gc-instrument*.  Sits in the free tail of the
   demand-zeroed BSS, between +linux-i386-argv-arena-end+ (#x1001E000) and
   +linux-i386-bss-end+ (#x10020000) — everything below is spoken for
   (cstr-scratch #x10004000, io-buf #x10008000, argv ptrs #x10009000,
   argv arena #x1000A000).
     +0  magic #xFEEDFACE   +4  scan_word calls    +8  tag 1/9 candidates
     +12 in from-space      +16 object-start ok    +20 copied
     +24 poison-cons words seen   +28 poison-obj words seen")

(defparameter *i386-gc-enabled* t
  "Whether :gc-check calls the collector.  DEFAULT T since the native i386
   Cheney collector landed (i386-emit-gc-trampoline).

   MILESTONE THAT FLIPPED IT ON: SHA-256 over 64 KiB completes with digest
   7daca2095d0438260fa849183dfc67faa459fdf4936e1bc91eec6b281b27e4c2 —
   byte-equal to Python hashlib, generated not transcribed — across 8
   collections, rc=0.  A 1000-cons chain survives 6 forced collections with
   every element intact, and 4 KiB of SHA-256 is exact under a collection
   every 1 MB (roughly 120 cycles), which on a 30-bit tower means constant
   bignum allocation across every one of them.

   OFF is still a supported configuration (MODUS_I386_GC=0 in
   build-i386-cli.lisp): with no collector every allocation is permanent and
   the arena is the lifetime budget, so bulk work dies honestly at the edge.")

(defvar *i386-gc-collect-label* nil
  "Label of the native collector, which :gc-check CALLs.  NIL when the
   collector is disabled, and :gc-check then falls back to its historical
   `int $0x31`.")

;;; Cheney GC metadata, at the same fixed addresses x64 and aarch64 use.
;;; Values are RAW byte addresses, exactly as on x64 — the native collector
;;; holds them in registers, so the address<<1 convention gc.lisp needed (it
;;; reads them with (mem-ref :u64), which is raw, so memory had to hold the
;;; TAGGED form) is gone.  boot/boot-linux-i386.lisp stores them raw to match.
;;; The saved-sp/va/vl slots at 0x68/0x70/0x78 are no longer used at all: the
;;; collector keeps ESP in a register and writes VA/VL itself.
(defparameter *i386-gc-stress-limit* nil
  "When set, the collector installs VL = new_from_start + this instead of
   + space_size, so collections keep recurring at that interval.  Corruption in
   a copying collector shows up at the SECOND collection (the first leaves the
   old semispace intact), so being able to force dozens of cycles out of a
   small workload is what makes the survival tests cheap.  NIL = normal.")

;;; REGION 0 ONLY.  The GC metadata block is per-region as of stage 1 (see
;;; mvm/compiler.lisp's +GC-OFF-*+ / +GC-REGION-ADDR+ block): the eight names
;;; below spell "region 0's block, field F", which is what a single-region image
;;; uses and therefore leaves this translator byte-for-byte as it was.  This
;;; collector does NOT read the active-region pointer, so an i386 image that
;;; created a second region would collect the wrong heap.  Porting it means the
;;; same edit translate-x64's emit-gc-trampoline took: load the region base
;;; once, address the five fields off it.  Nothing creates a second region on
;;; i386 today, and there is no way to boot one where this was written.
(defconstant +i386-gc-from-start+ modus.mvm::+gc-from-start-addr+)
(defconstant +i386-gc-to-start+   modus.mvm::+gc-to-start-addr+)
(defconstant +i386-gc-space-size+ modus.mvm::+gc-space-size-addr+)
(defconstant +i386-gc-stack-base+ modus.mvm::+gc-stack-base-addr+)
(defconstant +i386-gc-count+      modus.mvm::+gc-count-addr+)
(defvar *i386-genadd-label* nil)
(defvar *i386-gensub-label* nil)
(defvar *i386-genmul-label* nil)

;;; ============================================================
;;; handler-case / unwind-protect state (TRAP #x0510/#x0511/#x0512)
;;; ============================================================
;;;
;;; The jmp_buf is SIX words on i386, not x64's four.  ESI/EDI/EBX are V0/V1/V4
;;; — LIVE VREGS, not merely callee-saved — so a longjmp that fails to restore
;;; them resumes into garbage.  x64 only has to stack RBX (=V4) because its
;;; other vregs are either caller-saved or spilled.
;;;
;;;   +0  ESP   +4  EBP   +8  IP   +12 EBX(V4)   +16 ESI(V0)   +20 EDI(V1)
;;;
;;; The range 0x10000180..0x10000198 is free in the hosted BSS: gc.lisp's
;;; comment claims 0x180/0x188 for copy results but its code uses 0x100/0x108,
;;; and i386 does not run gc.lisp's collector at all.  It ends clear of argc
;;; (0x10000200).
;;;
;;; Handler-stack region 0x10000400..0x10000A00 (the boot map already reserves
;;; it), ending clear of the i386 global slot block at 0x10000A00.  That is
;;; 0x5F8 bytes = 63 frames of 24, so the depth cap is 63 rather than x64's 64.
;;;
;;; Scratch at 0x100003C0..0x100003F8 sits in the unused gap between the
;;; saved initial ESP (0x10000290) and the handler stack.

(defparameter *i386-jmpbuf-addr* #x10000180
  "Base of the six-word jmp_buf (ESP/EBP/IP/EBX/ESI/EDI).")
(defconstant +i386-jmpbuf-words+ 6)
(defconstant +i386-jmpbuf-size+ 24)

(defparameter *i386-hstack-depth-addr* #x10000400
  "Handler-stack depth (dword).  Demand-zeroed BSS, so it starts at 0.")
(defparameter *i386-hstack-base-addr* #x10000408
  "Handler-stack frame 0.  Frame N is at base + 24*N.")
(defparameter *i386-hstack-max-depth* 63
  "Frames that fit below the i386 global slot block at 0x10000A00.")

(defparameter *i386-longjmp-scratch-addr* #x100003C0
  "Six words.  LONGJMP copies the jmp_buf here BEFORE the pop overwrites it.")
(defparameter *i386-gcroot-bound-addr* #x100003E8
  "Scratch used by the GC trampoline's handler-stack root loop: scan_word
   clobbers EAX/ECX/EDX and every other register is collector state, so the
   loop bound has to live in memory.")
(defparameter *i386-hstack-capcount-addr* #x100003F0
  "Cumulative count of capped pushes (diagnostic only).")
(defparameter *i386-hstack-overflow-addr* #x100003F8
  "LIVE capped-push count — the BALANCED-CAP counter.  A capped push stores no
   frame, so the CLEAR-HANDLER that textually matches it must ABSORB its pop
   rather than drain a real stored frame (the drain-to-depth-0 class x64 hit on
   bare metal).  LONGJMP zeroes it: a longjmp unwinds past ALL capped (strictly
   inner) frames at once, so their pending absorbs must not fire against outer
   pops later.")

(defvar *i386-handler-push-label* nil)
(defvar *i386-handler-pop-label* nil)

;;; ============================================================
;;; i386 Code Buffer
;;; ============================================================

(defstruct i386-buffer
  (bytes (make-array 1572864))          ; 1.5MB, position tracks fill
  (labels (make-hash-table :test 'eql))
  (fixups nil)     ; list of (byte-position label-id fixup-type)
  (position 0))

(defun i386-grow-buffer (buf need)
  "Double the code buffer until it can hold NEED bytes.
   WS5: the buffer used to be a FIXED 1.5 MB simple-vector, which was fine
   for the 90 KB toy-REPL image but overflows immediately on a real CL image
   (the prelude + rt + cl-* bridge is ~2 MB of native code).  The failure was
   also badly disguised: cross.lisp's translate-module-to-native wraps the
   translator in a handler-case, so the array-index error surfaced only as
   `WARN: giving up on translator' followed by an image with 0 bytes of
   native code.  x64's code-buffer already grows on demand; now i386's does."
  (let* ((old (i386-buffer-bytes buf))
         (cap (length old)))
    (when (> need cap)
      (let ((new-cap cap))
        (loop while (< new-cap need) do (setf new-cap (* 2 new-cap)))
        (let ((new (make-array new-cap)))
          (replace new old)
          (setf (i386-buffer-bytes buf) new))))))

(defun i386-emit-byte (buf byte)
  "Emit a single byte."
  (let ((pos (i386-buffer-position buf)))
    (when (>= pos (length (i386-buffer-bytes buf)))
      (i386-grow-buffer buf (1+ pos)))
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

(defun i386-emit-fixup-diff32 (buf label-id anchor)
  "Emit a 32-bit placeholder holding (LABEL - ANCHOR), a purely
   BUFFER-RELATIVE difference.  Used by :fn-addr to materialise a function
   address position-independently (i386 has no RIP-relative LEA): the code
   does `call .next / pop d / add d, (fn - .next)`, and ANCHOR is the buffer
   position of `.next`.  Because the value is a difference between two
   positions in the same buffer it needs no knowledge of the final load
   address, unlike the x64 MOVABS form."
  (push (list (i386-buffer-position buf) label-id :diff32 anchor)
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
      (destructuring-bind (pos label-id fixup-type &optional anchor) fixup
        (let ((target (gethash label-id (i386-buffer-labels buf))))
          (unless target
            (error "i386: undefined label ~A" label-id))
          (ecase fixup-type
            (:diff32
             ;; Buffer-relative difference (label - anchor); see
             ;; i386-emit-fixup-diff32.
             (let* ((rel (- target anchor)))
               (unless (<= -2147483648 rel 2147483647)
                 (error "i386 diff32 ~D out of range at pos ~D" rel pos))
               (let ((urel (if (minusp rel) (logand rel #xFFFFFFFF) rel)))
                 (setf (aref bytes (+ pos 0)) (logand urel #xFF)
                       (aref bytes (+ pos 1)) (logand (ash urel -8) #xFF)
                       (aref bytes (+ pos 2)) (logand (ash urel -16) #xFF)
                       (aref bytes (+ pos 3)) (logand (ash urel -24) #xFF)))))
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
;;; Mechanized enforcement of the i386 register invariant
;;; ============================================================
;;;
;;; The invariant (stated in full at I386-LOAD-VREG) is: an opcode must not
;;; write EAX unless its destination vreg IS VR.  Documenting it is not
;;; enough — it has been violated twice, and both times the symptom appeared
;;; far from the cause with no size change to give it away.  So the emit
;;; helpers CHECK it at build time.  Every one of the opcodes that has never
;;; been hand-audited either passes silently (mechanically proven to respect
;;; the invariant) or fails the build naming itself.  Zero runtime cost.

(defparameter *i386-eax-allowlist*
  '(("DIV"         . "IDIV requires the dividend in EDX:EAX")
    ("MOD"         . "IDIV requires the dividend in EDX:EAX")
    ("MUL26LO"     . "MUL requires one operand in EAX")
    ("MUL26HI"     . "MUL requires one operand in EAX")
    ("IO-READ"     . "IN uses AL/AX/EAX by ISA")
    ("IO-WRITE"    . "OUT uses AL/AX/EAX by ISA")
    ("ATOMIC-XCHG" . "XCHG is issued against EAX here")
    ("CALL-IND"    . "EAX is caller-saved; VR cannot be live across a call")
    ("TAILCALL"    . "EAX is caller-saved; VR cannot be live across a call")
    ("CALL"        . "EAX is caller-saved; VR cannot be live across a call")
    ("TRAP"        . "syscall ABI/IN/OUT force EAX, so the :trap arm brackets the whole dispatch with push/pop EAX.  The handler-case traps #x0510/#x0511 DO deliver in EAX — deliberately, because EAX *is* VR and SETJMP's result goes to VR — and are excluded from the bracket by *i386-eax-live-traps*"))
  "Opcodes permitted to write EAX with a non-VR destination, each with the
   reason it is legitimate.  The justification lives NEXT TO the exemption on
   purpose: an allowlist without reasons becomes a place to hide bugs.")

(defvar *i386-check-eax-invariant* t)
(defvar *i386-eax-violations* nil
  "Hash of opcode-name -> violation count, filled by I386-CHECK-EAX-WRITE.
   COLLECTING rather than erroring is deliberate: cross.lisp wraps the
   translator in a handler-case, so an ERROR would surface as one masked
   \"WARN: translator error\" and hide every violation after the first —
   the same masking pattern this workstream keeps having to undo.  One build
   now reports the complete list.")
(defvar *i386-eax-invariant-fatal* nil
  "When true, a violation ERRORs instead of being recorded.")
(defvar *i386-cur-opname* nil "Opcode being translated, or NIL outside translation.")
(defvar *i386-cur-dest-is-vr* nil "Does the current opcode's destination vreg = VR?")

(defun i386-emit-gc-mark-start (buf base-reg &optional cons-p)
  "Set BASE-REG's object-start bit (and, when CONS-P, its cons-kind bit).

   This is what makes gc.lisp's conservative-root validation actually work on
   i386: %gc-is-start returns T unconditionally while bitmap_base is 0, so
   without these bits every false root is copied and copy_object stamps
   forwarding pointers over mid-object data.

   x86 BTS with a REGISTER bit offset does the whole bit-string address
   calculation in one instruction, so the sequence is just: granule index =
   (base - page_base) >> 4, then BTS [bitmap], index.

   VR-PRESERVING by construction (see the register invariant): computes in the
   ECX/EDX scratch pair, saved around the sequence so the caller's operands
   survive, and never touches EAX.  The build-time checker enforces this."
  (when (and *i386-gc-bitmap-enabled* *i386-linux-mode*)
    (i386-emit-push-reg buf +scratch0+)
    (i386-emit-push-reg buf +scratch1+)
    ;; ECX = granule index = (base - page_base) >> 4
    (i386-emit-mov-reg-reg buf +scratch0+ base-reg)
    (i386-emit-byte buf #x2B)                                  ; SUB r32, r/m32
    (i386-emit-byte buf (i386-modrm #b00 +scratch0+ 5))
    (i386-emit-u32 buf *gc-page-base-addr*)
    (i386-emit-shr-reg-imm buf +scratch0+ 4)
    ;; EDX = bitmap base ; BTS [EDX], ECX
    (i386-emit-mov-reg-abs buf +scratch1+ *gc-startbmp-addr*)
    (i386-emit-byte buf #x0F) (i386-emit-byte buf #xAB)        ; BTS r/m32, r32
    (i386-emit-byte buf (i386-modrm #b00 +scratch0+ +scratch1+))
    (when cons-p
      (i386-emit-mov-reg-abs buf +scratch1+ *gc-consbmp-addr*)
      (i386-emit-byte buf #x0F) (i386-emit-byte buf #xAB)
      (i386-emit-byte buf (i386-modrm #b00 +scratch0+ +scratch1+)))
    (i386-emit-pop-reg buf +scratch1+)
    (i386-emit-pop-reg buf +scratch0+)))

(defun i386-check-eax-write (reg)
  "Signal if REG is EAX and the current opcode may not write it."
  (when (and *i386-check-eax-invariant*
             (eql reg +i386-eax+)
             *i386-cur-opname*
             (not *i386-cur-dest-is-vr*)
             (not (assoc *i386-cur-opname* *i386-eax-allowlist* :test #'string=)))
    (let ((tbl (or *i386-eax-violations*
                   (setf *i386-eax-violations* (make-hash-table :test 'equal)))))
      (incf (gethash *i386-cur-opname* tbl 0)))
    (when *i386-eax-invariant-fatal*
      (error "i386 REGISTER INVARIANT VIOLATED in opcode ~A:~%~
            it writes EAX, but EAX *is* VR and this opcode's destination vreg~%~
            is not VR — so it destroys a live VR.  Compute in the ECX/EDX~%~
            scratch pair (+scratch0+/+scratch1+) and let I386-STORE-VREG~%~
            write EAX only when vd really is VR.  If EAX is genuinely~%~
            required (hardware operand, or after a CALL), add ~A to~%~
            *i386-eax-allowlist* WITH ITS REASON."
             *i386-cur-opname* *i386-cur-opname*)))
  reg)

(defun i386-eax-invariant-report ()
  "Alist of (opcode-name . violation-count), worst first."
  (let ((acc nil))
    (when *i386-eax-violations*
      (maphash (lambda (k v) (push (cons k v) acc)) *i386-eax-violations*))
    (sort acc #'> :key #'cdr)))

;;; ============================================================
;;; i386 Instruction Emitters
;;; ============================================================

;;; --- MOV ---

(defun i386-emit-mov-reg-reg (buf dst src)
  "MOV dst, src (register-to-register, 32-bit)"
  (i386-check-eax-write dst)
  (i386-emit-byte buf #x89)   ; MOV r/m32, r32
  (i386-emit-byte buf (i386-modrm #b11 src dst)))

(defun i386-emit-mov-reg-imm (buf reg imm)
  "MOV reg, imm32"
  (i386-check-eax-write reg)
  (i386-emit-byte buf (+ #xB8 reg))
  (i386-emit-u32 buf (logand imm #xFFFFFFFF)))

(defun i386-emit-mov-reg-mem (buf reg base offset)
  "MOV reg, [base + offset]"
  (i386-check-eax-write reg)
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
  (i386-check-eax-write reg)
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
  (i386-check-eax-write reg)
  (i386-emit-byte buf #x0F)
  (i386-emit-byte buf #xB6)
  (i386-emit-modrm-mem buf reg base offset))

(defun i386-emit-movzx-word (buf reg base offset)
  "MOVZX reg, WORD [base+offset]"
  (i386-check-eax-write reg)
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
  (i386-check-eax-write reg)
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
         (i386-check-eax-write dst)
         (i386-emit-byte buf ,opcode-rr)
         (i386-emit-byte buf (i386-modrm #b11 src dst)))

       (defun ,(intern (format nil "I386-EMIT-~A-REG-IMM" name) pkg) (buf reg imm)
         ,(format nil "~A reg, imm" name)
         (i386-check-eax-write reg)
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
  (i386-check-eax-write reg)
  (if (= count 1)
      (progn (i386-emit-byte buf #xD1)
             (i386-emit-byte buf (i386-modrm #b11 4 reg)))
      (progn (i386-emit-byte buf #xC1)
             (i386-emit-byte buf (i386-modrm #b11 4 reg))
             (i386-emit-byte buf count))))

(defun i386-emit-shr-reg-imm (buf reg count)
  "SHR reg, imm8 (logical shift right)"
  (i386-check-eax-write reg)
  (if (= count 1)
      (progn (i386-emit-byte buf #xD1)
             (i386-emit-byte buf (i386-modrm #b11 5 reg)))
      (progn (i386-emit-byte buf #xC1)
             (i386-emit-byte buf (i386-modrm #b11 5 reg))
             (i386-emit-byte buf count))))

(defun i386-emit-sar-reg-imm (buf reg count)
  "SAR reg, imm8 (arithmetic shift right)"
  (i386-check-eax-write reg)
  (if (= count 1)
      (progn (i386-emit-byte buf #xD1)
             (i386-emit-byte buf (i386-modrm #b11 7 reg)))
      (progn (i386-emit-byte buf #xC1)
             (i386-emit-byte buf (i386-modrm #b11 7 reg))
             (i386-emit-byte buf count))))

(defun i386-emit-shl-reg-cl (buf reg)
  "SHL reg, CL — shift left by count in CL"
  (i386-check-eax-write reg)
  (i386-emit-byte buf #xD3)
  (i386-emit-byte buf (i386-modrm #b11 4 reg)))

(defun i386-emit-sar-reg-cl (buf reg)
  "SAR reg, CL — arithmetic shift right by count in CL"
  (i386-check-eax-write reg)
  (i386-emit-byte buf #xD3)
  (i386-emit-byte buf (i386-modrm #b11 7 reg)))

;;; --- Multiply / Divide ---

(defun i386-emit-imul-reg-reg (buf dst src)
  "IMUL dst, src (two-operand signed multiply)"
  (i386-check-eax-write dst)
  (i386-emit-byte buf #x0F)
  (i386-emit-byte buf #xAF)
  (i386-emit-byte buf (i386-modrm #b11 dst src)))

(defun i386-emit-imul-reg-reg-imm8 (buf dst src imm)
  "IMUL dst, src, imm8 (6B /r ib) — three-operand signed multiply by a small
   constant.  Used for the handler-stack frame index (frame size 24 is not a
   power of two, so SHL will not do)."
  (i386-check-eax-write dst)
  (assert (<= -128 imm 127))
  (i386-emit-byte buf #x6B)
  (i386-emit-byte buf (i386-modrm #b11 dst src))
  (i386-emit-s8 buf imm))

(defun i386-emit-mov-abs-imm (buf addr imm)
  "MOV dword [addr], imm32 (C7 /0 with mod=00 r/m=101 disp32)"
  (i386-emit-byte buf #xC7)
  (i386-emit-byte buf (i386-modrm #b00 0 5))
  (i386-emit-u32 buf (logand addr #xFFFFFFFF))
  (i386-emit-u32 buf (logand imm #xFFFFFFFF)))

(defun i386-emit-add-abs-imm8 (buf addr imm)
  "ADD dword [addr], imm8 sign-extended (83 /0 with mod=00 r/m=101 disp32)"
  (assert (<= -128 imm 127))
  (i386-emit-byte buf #x83)
  (i386-emit-byte buf (i386-modrm #b00 0 5))
  (i386-emit-u32 buf (logand addr #xFFFFFFFF))
  (i386-emit-s8 buf imm))

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
  (i386-check-eax-write reg)
  (i386-emit-byte buf #xF7)
  (i386-emit-byte buf (i386-modrm #b11 3 reg)))

(defun i386-emit-not-reg (buf reg)
  "NOT reg (bitwise complement)"
  (i386-check-eax-write reg)
  (i386-emit-byte buf #xF7)
  (i386-emit-byte buf (i386-modrm #b11 2 reg)))

;;; --- LEA ---

(defun i386-emit-lea (buf dst base offset)
  "LEA dst, [base + offset]"
  (i386-check-eax-write dst)
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
    (:z  . #x4)  (:nz . #x5)  (:s  . #x8)  (:ns . #x9)
    (:o  . #x0)  (:no . #x1))
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

;;; ============================================================
;;; --- SSE2 double-precision + the boxed-float payload (#200/#201) ---
;;; ============================================================
;;;
;;; A float object is subtag #x60 (#x64..#x66 for single/short/long) with FOUR
;;; slots, each holding one 16-bit chunk of the IEEE-754 double stored TAGGED
;;; (chunk << 1):
;;;
;;;   slot 0 = bits 63..48   slot 1 = bits 47..32
;;;   slot 2 = bits 31..16   slot 3 = bits 15..0
;;;
;;; Every chunk is 0..65535, so the stored machine word is 0..131070: always
;;; positive, always low-bit-0 (the conservative collector reads the slot as a
;;; fixnum and never follows the float's raw bit pattern as a pointer) and it
;;; FITS A 32-BIT WORD.  The old 2 x 32-bit layout stored hi32 << 1, which
;;; overflows a 32-bit slot and loses the double's SIGN BIT — that, and only
;;; that, is why i386 had no floats.  See docs/i386-float-blocker.md;
;;; compiler.lisp's +float-slots+ / +subtag-float+ and
;;; mvm/float-slot-overrides.lisp must agree with this.
;;;
;;; i386 object shape (unlike x64 there is NO padding word): raw base R,
;;; tagged pointer P = R | 9, header at [R+0], slot i at [R + (i+1)*4].
;;; Size = align16((4+1)*4) = 32, which is exactly what the i386 collector's
;;; copy_object derives from the header count — allocator and collector agree.
;;;
;;; WHY THE STACK AND NOT REGISTER PAIRS.  x64 reassembles the four chunks
;;; into ONE 64-bit GPR and MOVQs it into an XMM.  i386 has no 64-bit GPR, and
;;; it has exactly TWO free registers (ECX/EDX — EAX *is* VR, ESI/EDI/EBX are
;;; V0/V1/V4, EBP is VFP), which is not enough to hold a pointer plus a
;;; 2-register 64-bit accumulator.  So the double is assembled directly in
;;; memory, 16 bits at a time, with `MOV WORD [ESP+k], dx`: the little-endian
;;; stack image of a double puts bits 15..0 at [ESP+0] and bits 63..48 at
;;; [ESP+6], so each chunk goes straight to its own halfword and there is no
;;; accumulator at all.  Peak live registers: one pointer + one chunk = 2.
;;; MOVSD then loads/stores the whole double from [ESP] in one instruction.
;;;
;;; SSE2 REQUIREMENT.  ADDSD/MOVSD/CVTSI2SD are SSE2 (Pentium 4+).  On hosted
;;; Linux (the only i386 target that reaches float code today — this is
;;; build-i386-cli) the kernel has already set CR4.OSFXSR/OSXMMEXCPT, so they
;;; just work.  A bare-metal i386 kernel would have to enable those CR4 bits
;;; before executing any of this; it currently never does, and it also never
;;; emits a float opcode, so nothing regresses.  Chose SSE2 over the x87 FPU
;;; deliberately: x87 computes in 80-bit extended precision and storing back
;;; to a double DOUBLE-ROUNDS, which can differ from x64 in the last bit.
;;; SSE2 is bit-exact against x64, and bit-exactness is the requirement.

(defconstant +i386-xmm0+ 0)
(defconstant +i386-xmm1+ 1)

(defconstant +i386-float-header+ #x460
  "Header word for a 4-slot double-float object: (count 4) << 8 | subtag #x60.
   Mirrors compiler.lisp's +float-slots+ / +subtag-float+ and translate-x64's
   literal; keep the three in step.")

(defconstant +i386-float-size+ 32
  "align16((+float-slots+ + 1) * 4) — the i386 object has a 4-byte header and
   NO padding word, so 5 words = 20 bytes rounds up to 32.")

(defun i386-emit-movsd-xmm-esp (buf xmm)
  "MOVSD xmm, [ESP] — F2 0F 10 /r with mod=00 r/m=100 (SIB) and SIB base=ESP."
  (i386-emit-byte buf #xF2) (i386-emit-byte buf #x0F) (i386-emit-byte buf #x10)
  (i386-emit-byte buf (i386-modrm #b00 xmm 4))
  (i386-emit-byte buf (i386-sib 0 4 +i386-esp+)))

(defun i386-emit-movsd-esp-xmm (buf xmm)
  "MOVSD [ESP], xmm — F2 0F 11 /r, same addressing form."
  (i386-emit-byte buf #xF2) (i386-emit-byte buf #x0F) (i386-emit-byte buf #x11)
  (i386-emit-byte buf (i386-modrm #b00 xmm 4))
  (i386-emit-byte buf (i386-sib 0 4 +i386-esp+)))

(defun i386-emit-sse-arith (buf sse-op xmm-dst xmm-src)
  "F2 0F <sse-op> /r — ADDSD(#x58) / SUBSD(#x5C) / MULSD(#x59) / DIVSD(#x5E)
   xmm-dst, xmm-src, register form."
  (i386-emit-byte buf #xF2) (i386-emit-byte buf #x0F) (i386-emit-byte buf sse-op)
  (i386-emit-byte buf (i386-modrm #b11 xmm-dst xmm-src)))

(defun i386-emit-ucomisd (buf xmm-dst xmm-src)
  "UCOMISD xmm-dst, xmm-src — 66 0F 2E /r.  Sets ZF/PF/CF like x64's, so the
   following :beq/:blt/:bgt read the same flags on both arches."
  (i386-emit-byte buf #x66) (i386-emit-byte buf #x0F) (i386-emit-byte buf #x2E)
  (i386-emit-byte buf (i386-modrm #b11 xmm-dst xmm-src)))

(defun i386-emit-cvtsi2sd (buf xmm gpr)
  "CVTSI2SD xmm, r32 — F2 0F 2A /r.  The source is a SIGNED 32-bit integer,
   which is exactly what an untagged i386 fixnum is (31 significant bits)."
  (i386-emit-byte buf #xF2) (i386-emit-byte buf #x0F) (i386-emit-byte buf #x2A)
  (i386-emit-byte buf (i386-modrm #b11 xmm gpr)))

(defun i386-emit-cvttsd2si (buf gpr xmm)
  "CVTTSD2SI r32, xmm — F2 0F 2C /r, truncating toward zero."
  (i386-check-eax-write gpr)
  (i386-emit-byte buf #xF2) (i386-emit-byte buf #x0F) (i386-emit-byte buf #x2C)
  (i386-emit-byte buf (i386-modrm #b11 gpr xmm)))

(defun i386-emit-float-bits-to-stack (buf ptr-reg tmp-reg)
  "PTR-REG holds the TAGGED pointer to a float object.  Push 8 bytes and fill
   them with the object's IEEE double, little-endian, so [ESP] is a valid
   `double' for MOVSD.  Clobbers PTR-REG (untagged in place) and TMP-REG;
   leaves ESP 8 lower — the caller must release it.

   Slot i carries bits 63-16i .. 48-16i, i.e. halfword (3-i) of the
   little-endian image, hence the (6 - 2i) displacement.  Storing only DX
   masks the chunk to 16 bits for free, so junk above bit 15 of a slot cannot
   corrupt a neighbour (the same guarantee x64 gets from its shl48/shr pair)."
  (i386-emit-sub-reg-imm buf ptr-reg +tag-object+)      ; tagged -> raw base
  (i386-emit-sub-reg-imm buf +i386-esp+ 8)
  (dotimes (i 4)
    (i386-emit-mov-reg-mem buf tmp-reg ptr-reg (* (1+ i) 4))
    (i386-emit-shr-reg-imm buf tmp-reg 1)               ; untag the chunk
    (i386-emit-mov-mem16-reg buf +i386-esp+ (- 6 (* 2 i)) tmp-reg)))

(defun i386-emit-float-bits-from-stack (buf base-reg tmp-reg)
  "The inverse: an 8-byte little-endian double sits at [ESP]; BASE-REG holds
   the RAW base of a freshly allocated 4-slot float object.  Split the double
   into four tagged 16-bit chunks and write slots 0..3.  Clobbers TMP-REG;
   BASE-REG and ESP are preserved.  MOVZX zero-extends, so `shl 1' can never
   produce a negative (pointer-looking) slot word."
  (dotimes (i 4)
    (i386-emit-movzx-word buf tmp-reg +i386-esp+ (- 6 (* 2 i)))
    (i386-emit-shl-reg-imm buf tmp-reg 1)               ; tag the chunk
    (i386-emit-mov-mem-reg buf base-reg (* (1+ i) 4) tmp-reg)))

(defun i386-emit-float-alloc (buf base-reg bump-reg)
  "Allocate a 4-slot double-float object at VA.  Leaves the RAW base in
   BASE-REG (still untagged — the caller stores the payload through it, then
   tags it), bumps VA by +i386-float-size+ using BUMP-REG, and sets the MCGC
   object-start bit.  Mirrors +op-alloc-obj+'s VR-preserving shape: the base
   stays in EDX and is tagged in place at the very end, so no third register
   (EAX = VR) is ever needed.

   The preceding :gc-check is emitted by compiler.lisp, not here — see the
   x64 note; 32 bytes is far inside the collector's overshoot guard."
  (i386-emit-mov-reg-abs buf base-reg *va-addr*)
  (i386-emit-mov-mem-imm buf base-reg 0 +i386-float-header+)
  ;; #259: a double-float is a 4-slot object, so header(+0) and slots
  ;; (+4..+16) account for 20 of the 32 bytes the granule reserves.  The
  ;; caller writes the four slots immediately (i386-emit-float-store-payload,
  ;; no allocation in between), but +20/+24/+28 are never written by anyone
  ;; and the flat Cheney sweep reads all three.
  (i386-emit-fill-slots buf base-reg '(20 24 28) :obj bump-reg)
  (i386-emit-mov-reg-reg buf bump-reg base-reg)
  (i386-emit-add-reg-imm buf bump-reg +i386-float-size+)
  (i386-emit-mov-abs-reg buf *va-addr* bump-reg)
  (i386-emit-gc-mark-start buf base-reg))               ; object-start only

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
    ((= vreg +vreg-va+) *va-addr*)
    ((= vreg +vreg-vl+) *vl-addr*)
    ((= vreg +vreg-vn+) *vn-addr*)
    (t nil)))

;;; ============================================================
;;; *** i386 REGISTER INVARIANT — read before adding an opcode ***
;;; ============================================================
;;;
;;;   AN OPCODE MUST NOT WRITE EAX UNLESS ITS DESTINATION VREG *IS* VR.
;;;   Compute in the ECX/EDX scratch pair (+scratch0+ / +scratch1+) and let
;;;   I386-STORE-VREG decide; it writes EAX only when vd really is VR.
;;;
;;; WHY THIS IS A STATED INVARIANT AND NOT CASE-BY-CASE VIGILANCE: EAX *is*
;;; VR on i386.  x64 and aarch64 have spare registers so their translators
;;; cannot express this hazard; i386 structurally can, and it has now bitten
;;; twice.  Round 1 (Stage C) was the ALU/flag ops: the compiler keeps a live
;;; operand in VR across the `:or tmp,a,b / :test tmp,1 / :add d,a,b`
;;; fixnum type-check, so an EAX-clobbering :or made `(+ a b)` compute
;;; (a|b)+b — observable as a loop counter advancing by 2.  Round 2 (Phase
;;; 3.2) was the object/memory ops: a clobbered VR reached +op-obj-subtag+ as
;;; tagged fixnum 9, which tag-stripped to address 9 and SIGSEGV'd deep
;;; inside GENERIC-LOGAND, nowhere near the actual cause.
;;;
;;; Both rounds share the signature that makes them expensive: the function
;;; SIZE is unchanged, so a size-based diff shows nothing; the wrong value
;;; surfaces far from the op that produced it.
;;;
;;; LEGITIMATE EXCEPTIONS, and the only ones:
;;;   * hardware-forced operands — :div/:mod (IDIV needs EDX:EAX),
;;;     :mul26lo/:mul26hi (MUL needs EAX), :io-read/:io-write (IN/OUT use
;;;     AL/AX/EAX), :atomic-xchg.
;;;   * anything at or after a CALL — :call/:call-ind/:tailcall.  EAX is
;;;     caller-saved, so VR cannot be live across a call by ABI anyway.
;;; If you add an opcode that needs EAX for any OTHER reason, it is a bug.

(defun i386-load-vreg (buf scratch vreg)
  "Load virtual register VREG into physical register SCRATCH.
   If VREG already lives in SCRATCH, no code is emitted.
   NB pass +scratch0+/+scratch1+, not +i386-eax+ — see the register
   invariant above."
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

;;; ============================================================
;;; Native i386 Cheney collector (the third arch arm)
;;; ============================================================
;;; The collector is arch-specific by nature — registers, word size, object
;;; layout — exactly as translate-*.lisp is N native back ends for one IR.
;;; x64 (emit-gc-trampoline) and aarch64 (emit-aarch64-native-gc-trampoline)
;;; each grew one; this is i386's, mirroring x64's structure (the most mature
;;; arm): scan_word / copy_object / flat Cheney to-space scan / semispace swap,
;;; with the object-start and cons-kind bitmaps as the conservative-root gate.
;;;
;;; WHY NATIVE AND NOT mvm/gc.lisp.  gc.lisp is a Lisp-side collector that has
;;; never actually run: %gc-read64 is (mem-ref a :u64), and :u64 is RAW
;;; (needs-tag NIL), so a loaded machine word is reinterpreted as a TAGGED Lisp
;;; value — i.e. as word/2.  A real cons/object pointer has low nibble 1 or 9
;;; and is therefore an ODD word, so FIXNUMP is false for it and the
;;; (if (fixnump val) ...) guard in %gc-is-pointer / %gc-is-forward returns NIL
;;; for exactly the words a collector exists to forward.  Measured with planted
;;; words: 0x20000001 and 0x20000009 both report is-pointer NIL.  Native code
;;; has no such problem: a machine word in a register is just a machine word.
;;;
;;; i386 SHAPES (all read off this file's own alloc opcodes, not assumed):
;;;   cons          16 bytes allocated, car at +0, cdr at +4
;;;   object        4-byte header at +0, slot i at +4+i*4,
;;;                 size = align16((count+1)*4)
;;;   u8 vector     subtag #x11, N packed bytes at +4, size = align16(4+N)
;;;   EVERY alloc site aligns to 16, so the 16-byte granule of the start/cons
;;;   bitmaps distinguishes every object — two objects can never share one.
;;;
;;; REGISTER CONTRACT inside the collector (PUSHAD has saved every mutator
;;; register, so all eight are free):
;;;   EBX = from_start        EDI = from_end        ESI = free pointer
;;;   EBP = loop cursor       EAX/ECX/EDX = temps
;;;   scan_word and copy_object PRESERVE EBX/EDI/EBP and advance only ESI.
;;; VA/VL are memory slots on i386, so unlike x64 there is no register pair to
;;; restore — the collector writes them directly.

(defun i386-emit-gcnative-markbit (buf addr-reg cons-p)
  "Set the object-start bit (and, when CONS-P, the cons-kind bit) for the raw
   to-space address in ADDR-REG.  Survivors must be marked at their NEW
   location: after the swap this region becomes from-space, and the next
   collection's scan_word validates roots against exactly these bits.
   Preserves every register (ECX/EDX are saved) and reads ADDR-REG only."
  (i386-emit-push-reg buf +scratch0+)
  (i386-emit-push-reg buf +scratch1+)
  (i386-emit-mov-reg-reg buf +scratch0+ addr-reg)
  (i386-emit-byte buf #x2B)                                ; SUB ECX, [page_base]
  (i386-emit-byte buf (i386-modrm #b00 +scratch0+ 5))
  (i386-emit-u32 buf *gc-page-base-addr*)
  (i386-emit-shr-reg-imm buf +scratch0+ 4)                 ; 16-byte granule
  (i386-emit-mov-reg-abs buf +scratch1+ *gc-startbmp-addr*)
  (i386-emit-byte buf #x0F) (i386-emit-byte buf #xAB)      ; BTS [EDX], ECX
  (i386-emit-byte buf (i386-modrm #b00 +scratch0+ +scratch1+))
  (when cons-p
    (i386-emit-mov-reg-abs buf +scratch1+ *gc-consbmp-addr*)
    (i386-emit-byte buf #x0F) (i386-emit-byte buf #xAB)
    (i386-emit-byte buf (i386-modrm #b00 +scratch0+ +scratch1+)))
  (i386-emit-pop-reg buf +scratch1+)
  (i386-emit-pop-reg buf +scratch0+))

(defun i386-fill-word (kind)
  "The word an allocation site stores into a slot it does not otherwise
   write, or NIL when *i386-alloc-fill* is off.  KIND is :CONS for a cons
   cell's two tail words and :OBJ for object payload / alignment tail."
  (case *i386-alloc-fill*
    (:zero 0)
    (:poison (if (eq kind :cons) +i386-fill-poison-cons+ +i386-fill-poison-obj+))
    (t nil)))

(defun i386-emit-fill-slots (buf base-reg offsets kind &optional via-reg)
  "Store the fill word into [BASE-REG + off] for each OFF in OFFSETS.

   With no VIA-REG this is immediate-form MOV — 7 bytes a slot, but it needs
   no register at all beyond BASE-REG.  Every call site below can spare one
   DEAD register, so they pass VIA-REG and get `XOR reg,reg' once plus 3
   bytes a slot; on an image with this many allocation sites that difference
   is megabytes of code.  VIA-REG must be dead at the call site AND must not
   be EAX (which is VR)."
  (let ((v (i386-fill-word kind)))
    (when v
      (cond ((null via-reg)
             (dolist (off offsets)
               (i386-emit-mov-mem-imm buf base-reg off v)))
            (t
             (if (zerop v)
                 (i386-emit-xor-reg-reg buf via-reg via-reg)
                 (i386-emit-mov-reg-imm buf via-reg v))
             (dolist (off offsets)
               (i386-emit-mov-mem-reg buf base-reg off via-reg)))))))

(defun i386-emit-fill-range (buf ptr-reg base-reg kind)
  "Fill [BASE-REG+4, PTR-REG) with the fill word, walking DOWN from PTR-REG.

   The downward walk is what makes this fit i386's register budget.  At every
   allocation site EAX is VR (the invariant is build-checked) and ECX/EDX are
   the only scratch, so an upward loop would need a third register for the
   bound — or a memory scratch word, which bare metal would have to reserve
   too.  Walking down lets the END double as the cursor: PTR-REG is consumed
   (it is dead at every call site) and BASE-REG, still needed for tagging,
   is only read.  Stops at BASE-REG itself, which holds the header.

   Unlike translate-x64's emit-zero-word-range this covers the 16-byte
   ALIGNMENT TAIL as well as the logical payload.  On x64 the tail is at most
   one word and zeroing it perturbed code layout enough to re-expose an
   unrelated residual; on i386 the granule is four words, so the tail is
   where most of the uninitialised memory actually is — a 1-slot object and
   a cons each leave two whole tail words for the flat scan to read."
  (let ((v (i386-fill-word kind)))
    (when v
      (let ((loop-label (i386-make-label))
            (done-label (i386-make-label)))
        (i386-emit-label buf loop-label)
        (i386-emit-sub-reg-imm buf ptr-reg 4)
        (i386-emit-cmp-reg-reg buf ptr-reg base-reg)
        (i386-emit-jcc buf :be done-label)      ; reached the header word
        (i386-emit-mov-mem-imm buf ptr-reg 0 v)
        (i386-emit-jmp-rel32 buf loop-label)
        (i386-emit-label buf done-label)))))

(defun i386-emit-gcdbg-inc (buf slot)
  "INC dword [*i386-gcdbg-base* + SLOT] when instrumenting.  Clobbers FLAGS
   only; every call site below recomputes them before the next branch."
  (when (and *i386-gc-instrument* *i386-linux-mode*)
    (i386-emit-byte buf #xFF)
    (i386-emit-byte buf (i386-modrm #b00 0 5))
    (i386-emit-u32 buf (+ *i386-gcdbg-base* slot))))

(defun i386-emit-gcdbg-count-poison (buf val-reg)
  "Count the two diagnostic sentinels as they are swept.  VAL-REG holds the
   word scan_word is examining."
  (when (and *i386-gc-instrument* *i386-linux-mode*)
    (dolist (pair (list (cons +i386-fill-poison-cons+ 24)
                        (cons +i386-fill-poison-obj+ 28)))
      (let ((skip (i386-make-label)))
        (i386-emit-cmp-reg-imm buf val-reg (car pair))
        (i386-emit-jcc buf :ne skip)
        (i386-emit-gcdbg-inc buf (cdr pair))
        (i386-emit-label buf skip)))))

(defun i386-emit-gcdbg-dump (buf)
  "write(2, counter-block, 32) so a run's totals survive in the log even if
   the process later dies.  Wrapped in its own PUSHAD/POPAD: the collector
   holds live state in EBX/ESI/EDI and the syscall clobbers EAX/ECX/EDX."
  (when (and *i386-gc-instrument* *i386-linux-mode*)
    (i386-emit-mov-abs-imm buf *i386-gcdbg-base* #xFEEDFACE)
    (i386-emit-byte buf #x60)                              ; PUSHAD
    (i386-emit-mov-reg-imm buf +i386-eax+ 4)               ; SYS_write
    (i386-emit-mov-reg-imm buf +i386-ebx+ 2)               ; fd 2 = stderr
    (i386-emit-mov-reg-imm buf +scratch0+ *i386-gcdbg-base*)
    (i386-emit-mov-reg-imm buf +scratch1+ 32)
    (i386-emit-byte buf #xCD) (i386-emit-byte buf #x80)    ; INT 0x80
    (i386-emit-byte buf #x61)))                            ; POPAD

(defun i386-emit-gcnative-bittest (buf addr-reg bitmap-addr)
  "BT the bit for raw address ADDR-REG in the bitmap whose base word lives at
   BITMAP-ADDR, leaving the answer in CF.  Preserves every register: the POPs
   that follow BT do not touch flags, so CF survives to the caller's Jcc."
  (i386-emit-push-reg buf +i386-eax+)
  (i386-emit-push-reg buf +scratch1+)
  (i386-emit-mov-reg-reg buf +i386-eax+ addr-reg)
  (i386-emit-byte buf #x2B)                                ; SUB EAX, [page_base]
  (i386-emit-byte buf (i386-modrm #b00 +i386-eax+ 5))
  (i386-emit-u32 buf *gc-page-base-addr*)
  (i386-emit-shr-reg-imm buf +i386-eax+ 4)
  (i386-emit-mov-reg-abs buf +scratch1+ bitmap-addr)
  (i386-emit-byte buf #x0F) (i386-emit-byte buf #xA3)      ; BT [EDX], EAX
  (i386-emit-byte buf (i386-modrm #b00 +i386-eax+ +scratch1+))
  (i386-emit-pop-reg buf +scratch1+)
  (i386-emit-pop-reg buf +i386-eax+))

(defun i386-emit-gcnative-copy (buf copy-label)
  "copy_object.  In: EAX = tagged from-space pointer.  Out: EAX = tagged
   to-space pointer, ESI advanced, forwarding pointer left behind.
   Preserves EBX/EDI/EBP; clobbers EAX/ECX/EDX."
  (let ((c-cons (i386-make-label)) (c-fwd (i386-make-label))
        (c-u8 (i386-make-label))   (c-align (i386-make-label))
        (c-bogus (i386-make-label))
        (c-loop (i386-make-label)) (c-done (i386-make-label)))
    (i386-emit-label buf copy-label)
    (i386-emit-push-reg buf +i386-eax+)          ; [ESP] = original tagged ptr
    (i386-emit-mov-reg-reg buf +scratch0+ +i386-eax+)
    (i386-emit-and-reg-imm buf +scratch0+ -16)   ; ECX = raw address
    (i386-emit-and-reg-imm buf +i386-eax+ 15)
    (i386-emit-cmp-reg-imm buf +i386-eax+ +tag-cons+)
    (i386-emit-jcc buf :e c-cons)

    ;; ---- object ----
    (i386-emit-mov-reg-mem buf +i386-eax+ +scratch0+ 0)   ; EAX = header
    (i386-emit-mov-reg-reg buf +scratch1+ +i386-eax+)
    (i386-emit-and-reg-imm buf +scratch1+ 15)
    (i386-emit-cmp-reg-imm buf +scratch1+ 15)
    (i386-emit-jcc buf :e c-fwd)
    ;; size -> EDX.  u8 vectors count BYTES, everything else counts WORDS.
    (i386-emit-mov-reg-reg buf +scratch1+ +i386-eax+)
    (i386-emit-and-reg-imm buf +scratch1+ 255)
    (i386-emit-cmp-reg-imm buf +scratch1+ #x11)
    (i386-emit-jcc buf :e c-u8)
    (i386-emit-mov-reg-reg buf +scratch1+ +i386-eax+)
    (i386-emit-shr-reg-imm buf +scratch1+ 8)     ; element count
    (i386-emit-add-reg-imm buf +scratch1+ 1)     ; + header word
    (i386-emit-shl-reg-imm buf +scratch1+ 2)     ; * 4
    (i386-emit-jmp-rel32 buf c-align)
    (i386-emit-label buf c-u8)
    (i386-emit-mov-reg-reg buf +scratch1+ +i386-eax+)
    (i386-emit-shr-reg-imm buf +scratch1+ 8)     ; byte count N
    (i386-emit-add-reg-imm buf +scratch1+ 4)     ; + header
    (i386-emit-label buf c-align)
    (i386-emit-add-reg-imm buf +scratch1+ 15)
    (i386-emit-and-reg-imm buf +scratch1+ -16)
    ;; ---- conservative-scan sanity guard (x64's, ported) ----
    ;; A false root that survives the bitmap gates would otherwise have its
    ;; first word read as a header, yielding an absurd size that runs the copy
    ;; loop off to-space.  A real object lies ENTIRELY inside from-space.
    (i386-emit-cmp-reg-imm buf +scratch1+ 16)
    (i386-emit-jcc buf :b c-bogus)
    (i386-emit-mov-reg-reg buf +i386-eax+ +scratch0+)
    (i386-emit-add-reg-reg buf +i386-eax+ +scratch1+)
    (i386-emit-cmp-reg-reg buf +i386-eax+ +i386-edi+)
    (i386-emit-jcc buf :a c-bogus)
    ;; ---- copy EDX bytes from ECX to ESI ----
    (i386-emit-push-reg buf +i386-esi+)          ; destination start
    (i386-emit-push-reg buf +scratch1+)          ; size
    (i386-emit-label buf c-loop)
    (i386-emit-test-reg-reg buf +scratch1+ +scratch1+)
    (i386-emit-jcc buf :e c-done)
    (i386-emit-mov-reg-mem buf +i386-eax+ +scratch0+ 0)
    (i386-emit-mov-mem-reg buf +i386-esi+ 0 +i386-eax+)
    (i386-emit-add-reg-imm buf +scratch0+ 4)
    (i386-emit-add-reg-imm buf +i386-esi+ 4)
    (i386-emit-sub-reg-imm buf +scratch1+ 4)
    (i386-emit-jmp-rel32 buf c-loop)
    (i386-emit-label buf c-done)
    (i386-emit-pop-reg buf +scratch1+)           ; size
    (i386-emit-pop-reg buf +i386-eax+)           ; destination start
    (i386-emit-sub-reg-reg buf +scratch0+ +scratch1+)  ; ECX = original raw
    (i386-emit-mov-reg-reg buf +scratch1+ +i386-eax+)
    (i386-emit-or-reg-imm buf +scratch1+ 15)
    (i386-emit-mov-mem-reg buf +scratch0+ 0 +scratch1+) ; forwarding pointer
    (i386-emit-gcnative-markbit buf +i386-eax+ nil)
    (i386-emit-or-reg-imm buf +i386-eax+ +tag-object+)
    (i386-emit-add-reg-imm buf +i386-esp+ 4)     ; drop saved tagged ptr
    (i386-emit-ret buf)

    ;; ---- bogus size: hand the pointer back untouched ----
    (i386-emit-label buf c-bogus)
    (i386-emit-pop-reg buf +i386-eax+)
    (i386-emit-ret buf)

    ;; ---- cons ----
    (i386-emit-label buf c-cons)
    (i386-emit-mov-reg-mem buf +i386-eax+ +scratch0+ 0)   ; car word
    (i386-emit-mov-reg-reg buf +scratch1+ +i386-eax+)
    (i386-emit-and-reg-imm buf +scratch1+ 15)
    (i386-emit-cmp-reg-imm buf +scratch1+ 15)
    (i386-emit-jcc buf :e c-fwd)
    (i386-emit-mov-mem-reg buf +i386-esi+ 0 +i386-eax+)   ; car
    (i386-emit-mov-reg-mem buf +i386-eax+ +scratch0+ 4)
    (i386-emit-mov-mem-reg buf +i386-esi+ 4 +i386-eax+)   ; cdr at +4, not +8
    (i386-emit-mov-reg-reg buf +i386-eax+ +i386-esi+)
    (i386-emit-or-reg-imm buf +i386-eax+ 15)
    (i386-emit-mov-mem-reg buf +scratch0+ 0 +i386-eax+)   ; forwarding pointer
    (i386-emit-gcnative-markbit buf +i386-esi+ t)
    (i386-emit-mov-reg-reg buf +i386-eax+ +i386-esi+)
    (i386-emit-add-reg-imm buf +i386-esi+ 16)
    (i386-emit-or-reg-imm buf +i386-eax+ +tag-cons+)
    (i386-emit-add-reg-imm buf +i386-esp+ 4)
    (i386-emit-ret buf)

    ;; ---- already forwarded: EAX = forwarding word ----
    (i386-emit-label buf c-fwd)
    (i386-emit-and-reg-imm buf +i386-eax+ -16)
    (i386-emit-pop-reg buf +scratch1+)           ; original tagged ptr
    (i386-emit-and-reg-imm buf +scratch1+ 15)    ; its tag
    (i386-emit-or-reg-reg buf +i386-eax+ +scratch1+)
    (i386-emit-ret buf)))

(defun i386-emit-gcnative-scan-word (buf scan-label copy-label)
  "scan_word.  In: EAX = ADDRESS of the word to examine.  If it holds a
   from-space cons/object pointer that the bitmaps confirm, copy the target and
   write the new pointer back.  Preserves EBX/EDI/EBP/ESI-as-free-ptr
   semantics; clobbers EAX/ECX/EDX."
  (let ((w-cons (i386-make-label)) (w-obj (i386-make-label))
        (w-copy (i386-make-label)) (w-done (i386-make-label)))
    (i386-emit-label buf scan-label)
    (i386-emit-push-reg buf +i386-eax+)                   ; address of the word
    (i386-emit-mov-reg-mem buf +scratch1+ +i386-eax+ 0)   ; EDX = the value
    (i386-emit-gcdbg-inc buf 4)                           ; words examined
    (i386-emit-gcdbg-count-poison buf +scratch1+)
    (i386-emit-mov-reg-reg buf +scratch0+ +scratch1+)
    (i386-emit-and-reg-imm buf +scratch0+ 15)
    (i386-emit-cmp-reg-imm buf +scratch0+ +tag-cons+)
    (i386-emit-jcc buf :e w-cons)
    (i386-emit-cmp-reg-imm buf +scratch0+ +tag-object+)
    (i386-emit-jcc buf :e w-obj)
    (i386-emit-jmp-rel32 buf w-done)

    ;; ---- cons-tagged candidate ----
    (i386-emit-label buf w-cons)
    (i386-emit-gcdbg-inc buf 8)                           ; tag 1/9 candidates
    (i386-emit-mov-reg-reg buf +scratch0+ +scratch1+)
    (i386-emit-and-reg-imm buf +scratch0+ -16)            ; ECX = raw target
    (i386-emit-cmp-reg-reg buf +scratch0+ +i386-ebx+)
    (i386-emit-jcc buf :b w-done)
    (i386-emit-cmp-reg-reg buf +scratch0+ +i386-edi+)
    (i386-emit-jcc buf :ae w-done)
    (i386-emit-gcdbg-inc buf 12)                          ; inside from-space
    ;; object-start bit must be SET (conservative-root validation)
    (i386-emit-gcnative-bittest buf +scratch0+ *gc-startbmp-addr*)
    (i386-emit-jcc buf :ae w-done)                        ; JNC
    (i386-emit-gcdbg-inc buf 16)                          ; object-start ok
    ;; ...and the start must really BE a cons.  aarch64 #160 (77c29e9): a
    ;; scratch word holding object_base|1 passes the start gate and would then
    ;; be copied as a 16-byte cons, truncating the object and stranding its
    ;; real obj-tagged reference.
    (i386-emit-gcnative-bittest buf +scratch0+ *gc-consbmp-addr*)
    (i386-emit-jcc buf :ae w-done)                        ; JNC
    (i386-emit-jmp-rel32 buf w-copy)

    ;; ---- object-tagged candidate ----
    (i386-emit-label buf w-obj)
    (i386-emit-gcdbg-inc buf 8)                           ; tag 1/9 candidates
    (i386-emit-mov-reg-reg buf +scratch0+ +scratch1+)
    (i386-emit-and-reg-imm buf +scratch0+ -16)
    (i386-emit-cmp-reg-reg buf +scratch0+ +i386-ebx+)
    (i386-emit-jcc buf :b w-done)
    (i386-emit-cmp-reg-reg buf +scratch0+ +i386-edi+)
    (i386-emit-jcc buf :ae w-done)
    (i386-emit-gcdbg-inc buf 12)                          ; inside from-space
    (i386-emit-gcnative-bittest buf +scratch0+ *gc-startbmp-addr*)
    (i386-emit-jcc buf :ae w-done)                        ; JNC
    (i386-emit-gcdbg-inc buf 16)                          ; object-start ok
    ;; ...and the start must NOT be a cons — the mirror-image cross-check:
    ;; cons_base|9 would otherwise be copied as a variable-size object, reading
    ;; a cons's car as a header.
    (i386-emit-gcnative-bittest buf +scratch0+ *gc-consbmp-addr*)
    (i386-emit-jcc buf :b w-done)                         ; JC

    (i386-emit-label buf w-copy)
    (i386-emit-gcdbg-inc buf 20)                          ; copied
    (i386-emit-mov-reg-reg buf +i386-eax+ +scratch1+)     ; tagged pointer
    (i386-emit-call-rel32 buf copy-label)
    (i386-emit-pop-reg buf +scratch1+)                    ; address of the word
    (i386-emit-mov-mem-reg buf +scratch1+ 0 +i386-eax+)
    (i386-emit-ret buf)

    (i386-emit-label buf w-done)
    (i386-emit-pop-reg buf +i386-eax+)
    (i386-emit-ret buf)))

(defun i386-emit-handler-helpers (buf push-label pop-label)
  "Emit __handler_push / __handler_pop: the NESTING half of the handler-case
   traps.  Both preserve EAX (CLEAR-HANDLER is emitted right after a
   handler-case body whose result is in dest — and dest==VR==EAX on i386, so
   clobbering EAX there would hand back a popped frame word as the value).
   Both clobber only ECX/EDX, the declared scratch pair.

   __handler_push: stack the CURRENT jmp_buf, then return EDX = 0 (stored) or
                   1 (capped).  SETJMP skips arming on 1, which degrades an
                   over-deep handler-case to a transparent no-op instead of a
                   stack corrupter.
   __handler_pop:  restore the top stacked frame into the jmp_buf, or zero the
                   jmp_buf when the stack is empty.

   Nesting is NOT deferrable.  SEQUENTIAL handler-cases — exactly what
   init-all-globals does, one per init thunk — work with a single-level
   implementation, so a version without this passes the early milestones and
   fails later, which is the worst available failure shape."
  (let ((jb *i386-jmpbuf-addr*)
        (hd *i386-hstack-depth-addr*)
        (hs *i386-hstack-base-addr*)
        (ovf *i386-hstack-overflow-addr*))
    ;; ---------------- __handler_push ----------------
    (let ((capped (i386-make-label)))
      (i386-emit-label buf push-label)
      (i386-emit-push-reg buf +i386-eax+)
      (i386-emit-mov-reg-abs buf +scratch0+ hd)              ; ECX = depth
      (i386-emit-cmp-reg-imm buf +scratch0+ *i386-hstack-max-depth*)
      (i386-emit-jcc buf :ge capped)
      ;; EDX = hs + depth*24
      (i386-emit-imul-reg-reg-imm8 buf +scratch1+ +scratch0+ +i386-jmpbuf-size+)
      (i386-emit-add-reg-imm buf +scratch1+ hs)
      (dotimes (i +i386-jmpbuf-words+)
        (i386-emit-mov-reg-abs buf +i386-eax+ (+ jb (* 4 i)))
        (i386-emit-mov-mem-reg buf +scratch1+ (* 4 i) +i386-eax+))
      (i386-emit-add-reg-imm buf +scratch0+ 1)
      (i386-emit-mov-abs-reg buf hd +scratch0+)
      (i386-emit-mov-reg-imm buf +scratch1+ 0)               ; EDX = 0: stored
      (i386-emit-pop-reg buf +i386-eax+)
      (i386-emit-ret buf)
      (i386-emit-label buf capped)
      (i386-emit-add-abs-imm8 buf *i386-hstack-capcount-addr* 1)
      (i386-emit-add-abs-imm8 buf ovf 1)
      (i386-emit-mov-reg-imm buf +scratch1+ 1)               ; EDX = 1: capped
      (i386-emit-pop-reg buf +i386-eax+)
      (i386-emit-ret buf))
    ;; ---------------- __handler_pop ----------------
    (let ((no-ovf (i386-make-label))
          (empty (i386-make-label))
          (done (i386-make-label)))
      (i386-emit-label buf pop-label)
      (i386-emit-push-reg buf +i386-eax+)
      ;; A pop that textually matches a CAPPED push absorbs it and touches
      ;; nothing else.
      (i386-emit-mov-reg-abs buf +scratch0+ ovf)
      (i386-emit-test-reg-reg buf +scratch0+ +scratch0+)
      (i386-emit-jcc buf :e no-ovf)
      (i386-emit-add-reg-imm buf +scratch0+ -1)
      (i386-emit-mov-abs-reg buf ovf +scratch0+)
      (i386-emit-jmp-rel32 buf done)
      (i386-emit-label buf no-ovf)
      (i386-emit-mov-reg-abs buf +scratch0+ hd)
      (i386-emit-test-reg-reg buf +scratch0+ +scratch0+)
      (i386-emit-jcc buf :e empty)
      (i386-emit-add-reg-imm buf +scratch0+ -1)
      (i386-emit-mov-abs-reg buf hd +scratch0+)
      (i386-emit-imul-reg-reg-imm8 buf +scratch1+ +scratch0+ +i386-jmpbuf-size+)
      (i386-emit-add-reg-imm buf +scratch1+ hs)
      (dotimes (i +i386-jmpbuf-words+)
        (i386-emit-mov-reg-mem buf +i386-eax+ +scratch1+ (* 4 i))
        (i386-emit-mov-abs-reg buf (+ jb (* 4 i)) +i386-eax+))
      (i386-emit-jmp-rel32 buf done)
      (i386-emit-label buf empty)
      ;; Zero the WHOLE jmp_buf, not just word 0.  Word 0 == 0 is the
      ;; "no handler armed" sentinel LONGJMP checks, but leaving the other
      ;; five words holding a dead frame's EBX/ESI/EDI leaves the collector
      ;; scanning stale roots (they are scanned — see i386-emit-gc-trampoline).
      (dotimes (i +i386-jmpbuf-words+)
        (i386-emit-mov-abs-imm buf (+ jb (* 4 i)) 0))
      (i386-emit-label buf done)
      (i386-emit-pop-reg buf +i386-eax+)
      (i386-emit-ret buf))))

(defun i386-emit-gc-trampoline (buf tramp-label collect-label)
  "A complete Cheney copying collector in native i386, called by :gc-check.

   WHAT THIS REPLACED, and why a bare `call %GC-COLLECT` could never work
   (WS5, 2026-07-31 — every claim below measured, none inferred):
     (1) %GC-COLLECT read the mutator stack pointer from 0x10000068, which
         nothing wrote.  It read 0, %gc-collect's own sanity check fired and
         set saved-rsp = stack_base, making the scan range EMPTY.  Measured:
         'S' on 12970 of 12970 collections, stack-word counter 0.
     (2) %GC-COLLECT published the post-collection alloc pointer and limit at
         0x10000070/78 and nothing read them back into VA/VL, so VA stayed at
         or above VL and the next allocation collected again.  Measured: a
         16 KiB SHA-256 needing at most two collections performed 12970.
     (3) Even with both fixed, gc.lisp cannot see a heap pointer at all — see
         the file-header note above.  That is why this arm is native.

   PUSHAD comes FIRST and the stack scan then starts at ESP, i.e. AT the
   register-save frame rather than past it: those saved registers are roots,
   and skipping them hands a stale pre-GC pointer back at POPAD (the aarch64
   fc25505 bug).  Nothing is written to 0x10000068/70/78 any more — the
   collector holds ESP in a register and writes VA/VL itself."
  (declare (ignore collect-label))
  (let ((scan-label (i386-make-label))
        (copy-label (i386-make-label))
        (body-label (i386-make-label)))
    (i386-emit-label buf tramp-label)
    (i386-emit-byte buf #x60)                             ; PUSHAD
    ;; --- metadata: EBX = from_start, ESI = free ptr (to_start), EDI = from_end
    (i386-emit-mov-reg-abs buf +i386-ebx+ +i386-gc-from-start+)
    (i386-emit-mov-reg-abs buf +i386-esi+ +i386-gc-to-start+)
    (i386-emit-mov-reg-reg buf +i386-edi+ +i386-ebx+)
    (i386-emit-byte buf #x03)                             ; ADD EDI, [space_size]
    (i386-emit-byte buf (i386-modrm #b00 +i386-edi+ 5))
    (i386-emit-u32 buf +i386-gc-space-size+)

    ;; --- stack roots: [ESP, stack_base) ---
    (let ((sl (i386-make-label)) (sd (i386-make-label)))
      (i386-emit-mov-reg-reg buf +i386-ebp+ +i386-esp+)
      (i386-emit-label buf sl)
      (i386-emit-cmp-reg-abs buf +i386-ebp+ +i386-gc-stack-base+)
      (i386-emit-jcc buf :ae sd)
      (i386-emit-mov-reg-reg buf +i386-eax+ +i386-ebp+)
      (i386-emit-call-rel32 buf scan-label)
      (i386-emit-add-reg-imm buf +i386-ebp+ 4)            ; 4-byte words
      (i386-emit-jmp-rel32 buf sl)
      (i386-emit-label buf sd))

    ;; --- fixed global roots (same set the x64 trampoline scans) ---
    (dolist (a (list #x10000080     ; global special-variable alist
                     #x10000088     ; symbol intern table
                     #x10000148     ; keyword intern table
                     #x10000170))   ; package-by-hash table
      (i386-emit-mov-reg-imm buf +i386-eax+ a)
      (i386-emit-call-rel32 buf scan-label))

    ;; --- the i386 GLOBAL SLOT BLOCK, scanned in full ---
    ;; x64 and aarch64 keep the closure-env register, nargs and MV-count in
    ;; PHYSICAL registers, which PUSHAD-equivalents already spill onto the
    ;; scanned stack.  i386 has no spare register, so they live in memory —
    ;; and CENV (globals+0x10) is a live HEAP POINTER that no root set covered.
    ;; That is the 2dd9e6f/c0dc6b8 invisible-root class, and it bites the
    ;; moment a collector actually forwards anything.  Scanning the whole block
    ;; rather than CENV alone keeps any slot added later covered by
    ;; construction; the non-pointer slots (VA/VL/page_base/bitmap bases are
    ;; 16-aligned, nargs/MV-count are small integers) fail the tag or range
    ;; test harmlessly.
    (let ((gl (i386-make-label)) (gd (i386-make-label)))
      (i386-emit-mov-reg-imm buf +i386-ebp+ *i386-globals-base*)
      (i386-emit-label buf gl)
      (i386-emit-cmp-reg-imm buf +i386-ebp+ (+ *i386-globals-base* #x24))
      (i386-emit-jcc buf :ae gd)
      (i386-emit-mov-reg-reg buf +i386-eax+ +i386-ebp+)
      (i386-emit-call-rel32 buf scan-label)
      (i386-emit-add-reg-imm buf +i386-ebp+ 4)
      (i386-emit-jmp-rel32 buf gl)
      (i386-emit-label buf gd))

    ;; --- the handler-case jmp_buf and every LIVE handler-stack frame ---
    ;; Three of the six words in a jmp_buf are VREGS (EBX=V4, ESI=V0, EDI=V1),
    ;; so they can be heap pointers held across an arbitrary amount of
    ;; allocation inside a handler-case body.  Missing them is the same
    ;; invisible-root class as CENV at globals+0x10, which sat unscanned until
    ;; this session.  ESP/EBP/IP are scanned too rather than skipped — the
    ;; stack (0x18000000) and the ELF image (0x08048000) are both BELOW the
    ;; arena (0x30000000), so scan_word's from-space bounds test rejects them,
    ;; and scanning the block whole keeps any slot added later covered.
    (let ((jl (i386-make-label)) (jd (i386-make-label)))
      (i386-emit-mov-reg-imm buf +i386-ebp+ *i386-jmpbuf-addr*)
      (i386-emit-label buf jl)
      (i386-emit-cmp-reg-imm buf +i386-ebp+ (+ *i386-jmpbuf-addr*
                                               +i386-jmpbuf-size+))
      (i386-emit-jcc buf :ae jd)
      (i386-emit-mov-reg-reg buf +i386-eax+ +i386-ebp+)
      (i386-emit-call-rel32 buf scan-label)
      (i386-emit-add-reg-imm buf +i386-ebp+ 4)
      (i386-emit-jmp-rel32 buf jl)
      (i386-emit-label buf jd))
    ;; Live frames only: [hstack_base, hstack_base + 24*depth).  scan_word
    ;; clobbers EAX/ECX/EDX and EBX/ESI/EDI are collector state, so the bound
    ;; goes to memory rather than a register.
    (let ((hl (i386-make-label)) (hd (i386-make-label)))
      (i386-emit-mov-reg-abs buf +scratch0+ *i386-hstack-depth-addr*)
      (i386-emit-imul-reg-reg-imm8 buf +scratch0+ +scratch0+ +i386-jmpbuf-size+)
      (i386-emit-add-reg-imm buf +scratch0+ *i386-hstack-base-addr*)
      (i386-emit-mov-abs-reg buf *i386-gcroot-bound-addr* +scratch0+)
      (i386-emit-mov-reg-imm buf +i386-ebp+ *i386-hstack-base-addr*)
      (i386-emit-label buf hl)
      (i386-emit-cmp-reg-abs buf +i386-ebp+ *i386-gcroot-bound-addr*)
      (i386-emit-jcc buf :ae hd)
      (i386-emit-mov-reg-reg buf +i386-eax+ +i386-ebp+)
      (i386-emit-call-rel32 buf scan-label)
      (i386-emit-add-reg-imm buf +i386-ebp+ 4)
      (i386-emit-jmp-rel32 buf hl)
      (i386-emit-label buf hd))

    ;; --- Cheney scan of to-space: [to_start, free_ptr), free_ptr growing ---
    (let ((cl (i386-make-label)) (cd (i386-make-label)))
      (i386-emit-mov-reg-abs buf +i386-ebp+ +i386-gc-to-start+)
      (i386-emit-label buf cl)
      (i386-emit-cmp-reg-reg buf +i386-ebp+ +i386-esi+)
      (i386-emit-jcc buf :ae cd)
      (i386-emit-mov-reg-reg buf +i386-eax+ +i386-ebp+)
      (i386-emit-call-rel32 buf scan-label)
      (i386-emit-add-reg-imm buf +i386-ebp+ 4)
      (i386-emit-jmp-rel32 buf cl)
      (i386-emit-label buf cd))

    ;; --- swap semispaces ---
    (i386-emit-mov-reg-abs buf +i386-eax+ +i386-gc-to-start+)
    (i386-emit-mov-abs-reg buf +i386-gc-from-start+ +i386-eax+)
    (i386-emit-mov-abs-reg buf +i386-gc-to-start+ +i386-ebx+)
    ;; --- install VA = free pointer, VL = new from_start + space_size ---
    (i386-emit-mov-abs-reg buf *va-addr* +i386-esi+)
    (if *i386-gc-stress-limit*
        (i386-emit-add-reg-imm buf +i386-eax+ *i386-gc-stress-limit*)
        (progn
          (i386-emit-byte buf #x03)                       ; ADD EAX, [space_size]
          (i386-emit-byte buf (i386-modrm #b00 +i386-eax+ 5))
          (i386-emit-u32 buf +i386-gc-space-size+)))
    (i386-emit-mov-abs-reg buf *vl-addr* +i386-eax+)

    ;; --- MCGC point (c): byte-exact clear of the reclaimed range's bitmaps ---
    ;; EBX is the semispace this collection just evacuated; its bits are stale.
    ;; Without this the bitmaps saturate monotonically and the conservative-root
    ;; gate decays to a no-op over many collections — the exact decay class that
    ;; cost x64 and aarch64 a round each.  REP STOSB, not STOSD: the two
    ;; semispaces' bitmap sub-ranges meet at space_size/128, which is not
    ;; 8-aligned, so a wider clear would erase the sibling's live bits.
    (dolist (bmp (list *gc-startbmp-addr* *gc-consbmp-addr*))
      (i386-emit-byte buf #xFC)                           ; CLD
      (i386-emit-mov-reg-reg buf +i386-eax+ +i386-ebx+)
      (i386-emit-byte buf #x2B)                           ; SUB EAX, [page_base]
      (i386-emit-byte buf (i386-modrm #b00 +i386-eax+ 5))
      (i386-emit-u32 buf *gc-page-base-addr*)
      (i386-emit-shr-reg-imm buf +i386-eax+ 7)            ; 1 bit / 16 bytes
      (i386-emit-mov-reg-abs buf +i386-edi+ bmp)
      (i386-emit-add-reg-reg buf +i386-edi+ +i386-eax+)
      (i386-emit-mov-reg-abs buf +scratch0+ +i386-gc-space-size+)
      (i386-emit-shr-reg-imm buf +scratch0+ 7)
      (i386-emit-byte buf #x31) (i386-emit-byte buf #xC0) ; XOR EAX, EAX
      (i386-emit-byte buf #xF3) (i386-emit-byte buf #xAA)); REP STOSB

    ;; --- gc_count++ ---
    (i386-emit-byte buf #xFF)                             ; INC dword [abs32]
    (i386-emit-byte buf (i386-modrm #b00 0 5))
    (i386-emit-u32 buf +i386-gc-count+)

    (i386-emit-gcdbg-dump buf)

    (i386-emit-byte buf #x61)                             ; POPAD
    (i386-emit-ret buf)

    ;; --- subroutines, jumped over by the body above ---
    (i386-emit-label buf body-label)
    (i386-emit-gcnative-scan-word buf scan-label copy-label)
    (i386-emit-gcnative-copy buf copy-label)))

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
  (let* ((buf (i386-translate-state-buf state))
         ;; Bind the invariant-checker's context for this opcode.  The
         ;; destination is operand 0 when the opcode's first operand type is
         ;; :reg; ops with no register destination get NIL, so ANY EAX write
         ;; from them is a violation.  See i386-check-eax-write.
         (%oi (gethash opcode *opcode-table*))
         (%tys (and %oi (opcode-info-operands %oi)))
         (*i386-cur-opname* (and %oi (string (opcode-info-name %oi))))
         (*i386-cur-dest-is-vr*
           (and %tys (eq (first %tys) :reg) operands
                (eql (first operands) +vreg-vr+))))
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
           ;; VR-PRESERVING: several trap arms MUST use EAX — the i386 syscall
           ;; ABI puts the call number and the result there, and IN/OUT use
           ;; AL/AX/EAX — so EAX cannot simply be avoided the way it was for
           ;; the ALU/object/memory opcodes.  Instead the whole dispatch is
           ;; bracketed with push/pop: for every trap that DELIVERS into
           ;; V0/ESI, restoring EAX afterwards is always correct.  For the
           ;; non-returning arms (SYS_exit, the unimplemented-trap reporter)
           ;; the unbalanced push is harmless — the process is leaving.
           ;;
           ;; THE EXCEPTIONS ARE THE HANDLER-CASE TRAPS.  SETJMP (#x0510) is
           ;; the first trap whose RESULT is a value in VR — and VR *is* EAX
           ;; here — and LONGJMP (#x0511) hands the T sentinel to the setjmp
           ;; resume point in EAX and never returns.  Both must therefore run
           ;; OUTSIDE the bracket; a trailing `pop eax` would make setjmp
           ;; always look like NIL and handler-case silently never take its
           ;; handler path.  CLEAR-HANDLER (#x0512) keeps the bracket — there
           ;; EAX holds the handler-case BODY's result and must be preserved,
           ;; which is exactly what the bracket does.
           (let ((eax-is-result (member code *i386-eax-live-traps*)))
           (unless eax-is-result
             (i386-emit-push-reg buf +i386-eax+))
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
             ((and (= code #x0300) *i386-linux-mode*)
              ;; HOSTED LINUX: serial write becomes write(1, &byte, 1).
              ;; V0 (ESI) holds the tagged char code.  i386 syscall ABI:
              ;; eax=nr, ebx=fd, ecx=buf, edx=len, int 0x80.  EBX carries V4
              ;; so it must be saved; ECX/EDX are translator scratch.
              (i386-emit-byte buf #x53)                       ; push ebx
              (i386-emit-byte buf #x89) (i386-emit-byte buf #xF0) ; mov eax, esi
              (i386-emit-byte buf #xD1) (i386-emit-byte buf #xF8) ; sar eax, 1
              (i386-emit-byte buf #x50)                       ; push eax (byte buffer)
              (i386-emit-byte buf #xBB) (i386-emit-u32 buf 1) ; mov ebx, 1 (stdout)
              (i386-emit-byte buf #x89) (i386-emit-byte buf #xE1) ; mov ecx, esp
              (i386-emit-byte buf #xBA) (i386-emit-u32 buf 1) ; mov edx, 1
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf 4) ; mov eax, 4 (SYS_write)
              (i386-emit-byte buf #xCD) (i386-emit-byte buf #x80) ; int 0x80
              (i386-emit-byte buf #x58)                       ; pop eax (discard buf)
              (i386-emit-byte buf #x5B))                      ; pop ebx

             ((and (= code #x0500) *i386-linux-mode*)
              ;; SYS_exit(V0): eax=1, ebx=untagged code, int 0x80.  Does not
              ;; return, so EBX need not be restored.
              (i386-emit-byte buf #x89) (i386-emit-byte buf #xF3) ; mov ebx, esi
              (i386-emit-byte buf #xD1) (i386-emit-byte buf #xFB) ; sar ebx, 1
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf 1)     ; mov eax, 1
              (i386-emit-byte buf #xCD) (i386-emit-byte buf #x80)) ; int 0x80

             ((and (= code #x0502) *i386-linux-mode*)
              ;; Generic 3-arg Linux syscall (compile-syscall3 emits :trap #x0502).
              ;;   sources: V0(ESI)=nr, V1(EDI)=a1, V2=[EBP-16], V3=[EBP-20]
              ;;   i386 ABI: eax=nr, ebx=a1, ecx=a2, edx=a3, int 0x80
              ;; All four are TAGGED fixnums and are untagged here; the result
              ;; is re-tagged into V0 (ESI), matching translate-x64's #x0502.
              ;; EBX holds V4 (callee-saved in this translator's own ABI) so it
              ;; is stacked around the call.
              (i386-emit-byte buf #x53)                          ; push ebx
              (i386-emit-byte buf #x89) (i386-emit-byte buf #xF0) ; mov eax, esi
              (i386-emit-byte buf #xD1) (i386-emit-byte buf #xF8) ; sar eax, 1
              (i386-emit-byte buf #x89) (i386-emit-byte buf #xFB) ; mov ebx, edi
              (i386-emit-byte buf #xD1) (i386-emit-byte buf #xFB) ; sar ebx, 1
              (i386-emit-mov-reg-mem buf +i386-ecx+ +i386-ebp+ -16) ; V2
              (i386-emit-byte buf #xD1) (i386-emit-byte buf #xF9) ; sar ecx, 1
              (i386-emit-mov-reg-mem buf +i386-edx+ +i386-ebp+ -20) ; V3
              (i386-emit-byte buf #xD1) (i386-emit-byte buf #xFA) ; sar edx, 1
              (i386-emit-byte buf #xCD) (i386-emit-byte buf #x80) ; int 0x80
              (i386-emit-byte buf #x01) (i386-emit-byte buf #xC0) ; add eax, eax (tag)
              (i386-emit-byte buf #x89) (i386-emit-byte buf #xC6) ; mov esi, eax
              (i386-emit-byte buf #x5B))                          ; pop ebx

             ((and (= code #x0503) *i386-linux-mode*)
              ;; Raw 3-arg syscall: number is TAGGED, args 1-3 are RAW, and the
              ;; result is RAW (not re-tagged).  Mirrors translate-x64 #x0503.
              (i386-emit-byte buf #x53)                          ; push ebx
              (i386-emit-byte buf #x89) (i386-emit-byte buf #xF0) ; mov eax, esi
              (i386-emit-byte buf #xD1) (i386-emit-byte buf #xF8) ; sar eax, 1
              (i386-emit-byte buf #x89) (i386-emit-byte buf #xFB) ; mov ebx, edi (raw)
              (i386-emit-mov-reg-mem buf +i386-ecx+ +i386-ebp+ -16)
              (i386-emit-mov-reg-mem buf +i386-edx+ +i386-ebp+ -20)
              (i386-emit-byte buf #xCD) (i386-emit-byte buf #x80) ; int 0x80
              (i386-emit-byte buf #x89) (i386-emit-byte buf #xC6) ; mov esi, eax
              (i386-emit-byte buf #x5B))                          ; pop ebx

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
             ;; ---- #x0510 / #x0511 / #x0512 — setjmp / longjmp / clear-handler
             ;;
             ;; DERIVED for i386, not transliterated from x64.  The three
             ;; points where the 32-bit form actually differs:
             ;;
             ;; (1) RESULT REGISTER.  compiler.lisp emits
             ;;         (emit-ir :trap #x0510) (emit-ir :mov dest +vreg-vr+)
             ;;     so SETJMP must deliver its result in VR — and on i386 VR
             ;;     *is* EAX.  But this :trap dispatch is bracketed with
             ;;     push/pop EAX, on the assumption that no trap returns a
             ;;     value there.  #x0510 is the first that does, so the
             ;;     bracket is made CONDITIONAL (see *i386-eax-live-traps*):
             ;;     if the trailing pop ran, setjmp would always appear to
             ;;     return NIL, handler-case would silently never take its
             ;;     handler path, and the failure would surface far from here.
             ;;     On x64 the question does not arise — RAX is not VR there.
             ;;
             ;; (2) SIX-WORD JMP_BUF.  ESI/EDI/EBX are V0/V1/V4 — live vregs,
             ;;     not merely callee-saved — so a longjmp that does not
             ;;     restore them resumes into garbage.  Layout, free range and
             ;;     GC-root treatment: see *i386-jmpbuf-addr*.
             ;;
             ;; (3) NESTING from the start: see i386-emit-handler-helpers.
             ;;
             ;; STACK RELOCATION is not a hazard: the saved ESP is on the
             ;; relocated stack (MAP_FIXED 0x18000000) and a longjmp only
             ;; unwinds to a SHALLOWER ESP, which shrinks the collector's scan
             ;; range.  The hazard would be a STALE saved ESP, which is why
             ;; __handler_pop zeroes the whole jmp_buf when the stack empties.
             ;;
             ;; Bare metal keeps the unimplemented-trap reporter: these
             ;; addresses are in the HOSTED Linux BSS (0x10000180 is past the
             ;; end of RAM on a `qemu-system-i386 -m 256` board), and the
             ;; bare-metal i386 images run mvm/repl-source.lisp, which has no
             ;; handler-case.  Gating on *i386-linux-mode* also keeps those
             ;; images byte-identical.
             ((and (= code #x0510) *i386-linux-mode* *i386-handler-push-label*)
              (let ((skiparm (i386-make-label))
                    (resume (i386-make-label)))
                ;; Stack the OUTER frame first; EDX comes back 1 if capped.
                (i386-emit-call-rel32 buf *i386-handler-push-label*)
                (i386-emit-test-reg-reg buf +scratch1+ +scratch1+)
                (i386-emit-jcc buf :ne skiparm)
                ;; Arm: ECX = &jmp_buf, then the five register words.
                (i386-emit-mov-reg-imm buf +scratch0+ *i386-jmpbuf-addr*)
                (i386-emit-mov-mem-reg buf +scratch0+  0 +i386-esp+)
                (i386-emit-mov-mem-reg buf +scratch0+  4 +i386-ebp+)
                (i386-emit-mov-mem-reg buf +scratch0+ 12 +i386-ebx+)
                (i386-emit-mov-mem-reg buf +scratch0+ 16 +i386-esi+)
                (i386-emit-mov-mem-reg buf +scratch0+ 20 +i386-edi+)
                ;; Resume IP.  i386 has no RIP-relative LEA, so use the
                ;; call/pop idiom the rest of this file uses: `call +0` pushes
                ;; the address of the next instruction, and a buffer-relative
                ;; diff32 (which needs no knowledge of the load address) turns
                ;; it into the address of RESUME.  The call/pop pair is stack
                ;; balanced and happens AFTER [ECX+0]=ESP, so the saved ESP is
                ;; exactly the ESP that RESUME will run with.
                (i386-emit-byte buf #xE8) (i386-emit-u32 buf 0) ; call next insn
                (let ((anchor (i386-current-pos buf)))
                  (i386-emit-pop-reg buf +scratch1+)
                  (i386-emit-byte buf #x81)                     ; add edx, imm32
                  (i386-emit-byte buf (i386-modrm #b11 0 +scratch1+))
                  (i386-emit-fixup-diff32 buf resume anchor))
                (i386-emit-mov-mem-reg buf +scratch0+ 8 +scratch1+)
                (i386-emit-label buf skiparm)
                ;; First return: NIL.  Read from *vn-addr* rather than baking
                ;; #xDEAD0001 in — bare-metal i386 uses 0 for NIL, and
                ;; :bnnull compares against this very slot.
                (i386-emit-mov-reg-abs buf +i386-eax+ *vn-addr*)
                ;; LONGJMP lands here with EAX already holding the T sentinel.
                (i386-emit-label buf resume)))
             ((and (= code #x0511) *i386-linux-mode* *i386-handler-pop-label*)
              ;; LONGJMP.  Read OUR frame out to scratch BEFORE the pop
              ;; overwrites the jmp_buf, then restore and transfer.
              (let ((armed (i386-make-label))
                    (ljs *i386-longjmp-scratch-addr*))
                ;; This longjmp unwinds past every capped (strictly inner)
                ;; frame at once, so their pending absorbs must be discarded.
                (i386-emit-mov-abs-imm buf *i386-hstack-overflow-addr* 0)
                (i386-emit-mov-reg-imm buf +scratch0+ *i386-jmpbuf-addr*)
                (dotimes (i +i386-jmpbuf-words+)
                  (i386-emit-mov-reg-mem buf +scratch1+ +scratch0+ (* 4 i))
                  (i386-emit-mov-abs-reg buf (+ ljs (* 4 i)) +scratch1+))
                ;; Nothing armed (saved ESP == 0) would mean jumping to a null
                ;; IP on a null stack.  Say so instead.
                (i386-emit-mov-reg-abs buf +scratch0+ ljs)
                (i386-emit-test-reg-reg buf +scratch0+ +scratch0+)
                (i386-emit-jcc buf :ne armed)
                (i386-emit-abort-msg
                 buf "MODUS i386: longjmp with no handler armed
" 71)
                (i386-emit-label buf armed)
                (i386-emit-call-rel32 buf *i386-handler-pop-label*)
                ;; Restore the live vregs first, then EBP, then the target IP
                ;; into EDX, then ESP.  Nothing after the ESP load touches
                ;; memory, so the abandoned frame cannot be read back.
                (i386-emit-mov-reg-abs buf +i386-ebx+ (+ ljs 12))
                (i386-emit-mov-reg-abs buf +i386-esi+ (+ ljs 16))
                (i386-emit-mov-reg-abs buf +i386-edi+ (+ ljs 20))
                (i386-emit-mov-reg-abs buf +i386-ebp+ (+ ljs 4))
                (i386-emit-mov-reg-abs buf +scratch1+ (+ ljs 8))
                (i386-emit-mov-reg-abs buf +i386-esp+ ljs)
                ;; EAX = T sentinel, so the :bnnull after SETJMP's
                ;; (:mov dest VR) takes the handler path.
                (i386-emit-mov-reg-imm buf +i386-eax+ +i386-mvm-t+)
                (i386-emit-jmp-reg buf +scratch1+)))
             ((and (= code #x0512) *i386-linux-mode* *i386-handler-pop-label*)
              ;; CLEAR-HANDLER: pop the outer frame back into the jmp_buf.
              ;; The dispatch's push/pop EAX bracket is what preserves the
              ;; handler-case body's result across this call (dest==VR==EAX);
              ;; __handler_pop preserves EAX itself as well.
              (i386-emit-call-rel32 buf *i386-handler-pop-label*))
             ((= code #x0530)
              ;; COPY-OVERFLOW-ARGS — the &rest prologue's runtime argument
              ;; copy, mirroring translate-x64.lisp's #x0530 arm.
              ;;
              ;; With more than +max-reg-args+ (4) arguments the caller PUSHES
              ;; args 4.. before :call pushes V2/V3, so in the callee the frame
              ;; reads:  [ebp+0] saved ebp, [ebp+4] return address, [ebp+8] V2,
              ;; [ebp+12] V3, [ebp+16] arg4, [ebp+20] arg5, ...
              ;; Hence  src(i) = ebp + 16 + (i-4)*4, which simplifies EXACTLY
              ;; to ebp + 4*i.  The destination is the local frame slot the
              ;; &rest cond-ladder loads from, the same address :obj-set uses
              ;; for a VFP destination:  dst(i) = ebp + frame-slot-base - 4*i.
              ;; Copying them there lets the ladder load args 0..3 and 4..N
              ;; through one addressing form.
              ;;
              ;; Cap at 32 total args, as x64 does: the ladder is unrolled per
              ;; defun, and raising x64's cap to 50 cost +30MB and regressed
              ;; ANSI via layout shift.  i386's frame has 128 slots, so 32 fits.
              ;;
              ;; Registers: the whole :trap dispatch is already bracketed with
              ;; push/pop EAX, and this arm additionally saves ECX/EDX/ESI, so
              ;; it clobbers nothing the prologue still needs.
              (let ((done (i386-make-label))
                    (nocap (i386-make-label))
                    (top (i386-make-label)))
                (i386-emit-push-reg buf +scratch0+)
                (i386-emit-push-reg buf +scratch1+)
                (i386-emit-push-reg buf +i386-esi+)
                ;; ECX = nargs (raw, untagged — the convention slot)
                (i386-emit-mov-reg-abs buf +scratch0+ *nargs-addr*)
                (i386-emit-cmp-reg-imm buf +scratch0+ 5)
                (i386-emit-jcc buf :l done)
                (i386-emit-cmp-reg-imm buf +scratch0+ 32)
                (i386-emit-jcc buf :le nocap)
                (i386-emit-mov-reg-imm buf +scratch0+ 32)
                (i386-emit-label buf nocap)
                ;; EDX = i, starting at +max-reg-args+
                (i386-emit-mov-reg-imm buf +scratch1+ 4)
                (i386-emit-label buf top)
                (i386-emit-cmp-reg-reg buf +scratch1+ +scratch0+)
                (i386-emit-jcc buf :ge done)
                ;; ESI = [ebp + 4*i]
                (i386-emit-mov-reg-reg buf +i386-esi+ +scratch1+)
                (i386-emit-shl-reg-imm buf +i386-esi+ 2)
                (i386-emit-add-reg-reg buf +i386-esi+ +i386-ebp+)
                (i386-emit-mov-reg-mem buf +i386-esi+ +i386-esi+ 0)
                ;; [ebp + frame-slot-base - 4*i] = ESI
                (i386-emit-push-reg buf +scratch1+)
                (i386-emit-shl-reg-imm buf +scratch1+ 2)
                (i386-emit-neg-reg buf +scratch1+)
                (i386-emit-add-reg-imm buf +scratch1+ +frame-slot-base+)
                (i386-emit-add-reg-reg buf +scratch1+ +i386-ebp+)
                (i386-emit-mov-mem-reg buf +scratch1+ 0 +i386-esi+)
                (i386-emit-pop-reg buf +scratch1+)
                (i386-emit-add-reg-imm buf +scratch1+ 1)
                (i386-emit-jmp-rel32 buf top)
                (i386-emit-label buf done)
                (i386-emit-pop-reg buf +i386-esi+)
                (i386-emit-pop-reg buf +scratch1+)
                (i386-emit-pop-reg buf +scratch0+)))
             ((and (= code #x0531) *i386-linux-mode*)
              ;; %MMAP-EXEC-PAGE — the ONE new primitive the WS4 JIT needs: a
              ;; page you can WRITE native bytes into AND then EXECUTE.
              ;; V0 (ESI) = size, tagged, a page multiple; result = the mmap
              ;; address, re-tagged into V0.  Mirrors translate-x64's #x0531
              ;; (PROT_RWX = 7, MAP_PRIVATE|MAP_ANONYMOUS = 0x22, fd = -1).
              ;;
              ;; USES old_mmap (syscall 90), NOT mmap2 (192), on purpose:
              ;; mmap2 passes its 6th argument in EBP, and EBP is VFP here —
              ;; the frame pointer every spilled vreg is addressed off.
              ;; old_mmap takes ONE argument, a pointer in EBX to a 6-dword
              ;; block {addr, len, prot, flags, fd, offset}, which we build on
              ;; the stack.  Nothing but EBX (V4, stacked) and EAX (already
              ;; bracketed by the :trap dispatch) is disturbed.  old_mmap's
              ;; offset field is in BYTES, and ours is 0, so the mmap2
              ;; page-units subtlety does not arise.
              (i386-emit-push-reg buf +i386-ebx+)                 ; save V4
              (i386-emit-byte buf #xD1) (i386-emit-byte buf #xFE) ; sar esi, 1
              ;; Build the arg block; pushes descend, so push in reverse.
              (i386-emit-push-imm32 buf 0)                        ; offset = 0
              (i386-emit-push-imm32 buf #xFFFFFFFF)               ; fd = -1
              (i386-emit-push-imm32 buf #x22)                     ; PRIVATE|ANON
              (i386-emit-push-imm32 buf 7)                        ; PROT_RWX
              (i386-emit-push-reg buf +i386-esi+)                 ; len
              (i386-emit-push-imm32 buf 0)                        ; addr = NULL
              (i386-emit-mov-reg-reg buf +i386-ebx+ +i386-esp+)   ; ebx = &args
              (i386-emit-byte buf #xB8) (i386-emit-u32 buf 90)    ; eax = old_mmap
              (i386-emit-byte buf #xCD) (i386-emit-byte buf #x80) ; int 0x80
              (i386-emit-add-reg-imm buf +i386-esp+ 24)           ; drop the block
              (i386-emit-byte buf #x01) (i386-emit-byte buf #xC0) ; add eax, eax
              (i386-emit-byte buf #x89) (i386-emit-byte buf #xC6) ; mov esi, eax
              (i386-emit-pop-reg buf +i386-ebx+))

             ((= code #x0532)
              ;; %JIT-CALL — V0 (ESI) holds the callee's byte address as a
              ;; TAGGED fixnum; untag and CALL it.  The callee builds its own
              ;; EBP frame and returns in EAX, which already IS the VR the
              ;; caller reads next.  Same shape as translate-x64's #x0532.
              ;;
              ;; #x0532 is in *i386-eax-live-traps* precisely because of that
              ;; last clause: compiler.lisp emits (:trap #x0532) (:mov dest VR)
              ;; and VR *is* EAX here, so the dispatch's usual push/pop EAX
              ;; bracket would restore the pre-call EAX and silently DISCARD
              ;; the callee's return value — the same failure mode SETJMP has.
              (i386-emit-byte buf #xD1) (i386-emit-byte buf #xFE) ; sar esi, 1
              (i386-emit-byte buf #xFF) (i386-emit-byte buf #xD6)) ; call esi

             ((= code #x0533)
              ;; %JIT-ICACHE-FLUSH — a NO-OP on x86: the instruction cache is
              ;; coherent with stores, so bytes written to an exec page are
              ;; visible to fetch with no maintenance.  AArch64 does the
              ;; DC CVAU / IC IVAU loop here; x64's arm is a NOP too.
              (i386-emit-nop buf))

             ((= code #x0534)
              ;; %JIT-FREE-PAGE — a NO-OP on x86, exactly as on x64: the
              ;; transient-page reclaim is runtime-gated to
              ;; *jit-target-arch* :aarch64, so the trap is emitted (mvm-eval
              ;; is shared source) but never executed here.
              (i386-emit-nop buf))

             (t
              ;; Unimplemented trap.  Recorded at BUILD time (keyed by
              ;; #x10000 + code so it cannot collide with an opcode number)
              ;; AND, unless explicitly classified safe, made to NAME ITSELF
              ;; at RUN time rather than silently no-op.  See
              ;; *i386-safe-nop-traps* / i386-emit-unimpl-trap.
              (when *i386-record-unimpl*
                (let ((tbl (or *i386-unimpl-ops*
                               (setf *i386-unimpl-ops* (make-hash-table :test 'eql)))))
                  (incf (gethash (+ #x10000 code) tbl 0))))
              (unless (member code *i386-safe-nop-traps*)
                (i386-emit-unimpl-trap buf code))))
           (unless eax-is-result
             (i386-emit-pop-reg buf +i386-eax+)))))

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
           ;; VR-PRESERVING (WS5): compute in the ECX/EDX scratch pair, not
           ;; EAX — EAX is VR and may hold a live value the compiler reads
           ;; again after this op (see the :or type-check note above).
           (i386-load-vreg buf +scratch1+ vb)
           (i386-load-vreg buf +scratch0+ va)
           (i386-emit-add-reg-reg buf +scratch0+ +scratch1+)
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-sub+)
         ;; (sub Vd Va Vb) -- tagged fixnums subtract directly
         ;; IMPORTANT: Load vb FIRST — loading va into EAX clobbers VR.
         (let ((vd (first operands))
               (va (second operands))
               (vb (third operands)))
           ;; VR-PRESERVING (WS5): compute in the ECX/EDX scratch pair, not
           ;; EAX — EAX is VR and may hold a live value the compiler reads
           ;; again after this op (see the :or type-check note above).
           (i386-load-vreg buf +scratch1+ vb)
           (i386-load-vreg buf +scratch0+ va)
           (i386-emit-sub-reg-reg buf +scratch0+ +scratch1+)
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-mul+)
         ;; (mul Vd Va Vb) -- tagged multiply
         ;; Both inputs carry <<1 fixnum tag, so product has factor of 4
         ;; where we need 2.  SAR 1 corrects: (a<<1)*(b<<1) >> 1 = a*b<<1
         ;; IMPORTANT: Load vb into scratch0 FIRST — loading va into EAX
         ;; clobbers VR, so if vb=VR we'd get va's value.
         (let ((vd (first operands))
               (va (second operands))
               (vb (third operands)))
           ;; VR-PRESERVING (WS5): compute in the ECX/EDX scratch pair.
           (i386-load-vreg buf +scratch1+ vb)
           (i386-load-vreg buf +scratch0+ va)
           (i386-emit-sar-reg-imm buf +scratch0+ 1)   ; untag one operand
           (i386-emit-imul-reg-reg buf +scratch0+ +scratch1+)
           ;; Result is already correctly tagged (a * (b<<1) = (a*b)<<1)
           (i386-store-vreg buf vd +scratch0+)))

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
           (i386-load-vreg buf +scratch0+ vs)
           (i386-emit-neg-reg buf +scratch0+)
           (i386-store-vreg buf vd +scratch0+)))

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
        ;; VR-PRESERVING FORM (WS5).  These used to compute in EAX, which is
        ;; VR: that destroyed a live VR even when vd was a DIFFERENT vreg.
        ;; With only 8 registers the compiler routinely keeps an operand in VR
        ;; across the type-check ALU op that precedes generic arithmetic, so
        ;; `(+ a b)` open-coded as `:or tmp,a,b / :test tmp,1 / :add d,a,b`
        ;; read back the OR'd value as `a` and computed (a|b)+b.  Symptom: a
        ;; loop counter advancing by 2.  Compute in the scratch pair (ECX/EDX)
        ;; and let i386-store-vreg touch EAX only when vd really is VR.
         (let ((vd (first operands)) (va (second operands)) (vb (third operands)))
           (i386-load-vreg buf +scratch1+ vb)
           (i386-load-vreg buf +scratch0+ va)
           (i386-emit-and-reg-reg buf +scratch0+ +scratch1+)
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-or+)
        ;; VR-PRESERVING FORM (WS5).  These used to compute in EAX, which is
        ;; VR: that destroyed a live VR even when vd was a DIFFERENT vreg.
        ;; With only 8 registers the compiler routinely keeps an operand in VR
        ;; across the type-check ALU op that precedes generic arithmetic, so
        ;; `(+ a b)` open-coded as `:or tmp,a,b / :test tmp,1 / :add d,a,b`
        ;; read back the OR'd value as `a` and computed (a|b)+b.  Symptom: a
        ;; loop counter advancing by 2.  Compute in the scratch pair (ECX/EDX)
        ;; and let i386-store-vreg touch EAX only when vd really is VR.
         (let ((vd (first operands)) (va (second operands)) (vb (third operands)))
           (i386-load-vreg buf +scratch1+ vb)
           (i386-load-vreg buf +scratch0+ va)
           (i386-emit-or-reg-reg buf +scratch0+ +scratch1+)
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-xor+)
        ;; VR-PRESERVING FORM (WS5).  These used to compute in EAX, which is
        ;; VR: that destroyed a live VR even when vd was a DIFFERENT vreg.
        ;; With only 8 registers the compiler routinely keeps an operand in VR
        ;; across the type-check ALU op that precedes generic arithmetic, so
        ;; `(+ a b)` open-coded as `:or tmp,a,b / :test tmp,1 / :add d,a,b`
        ;; read back the OR'd value as `a` and computed (a|b)+b.  Symptom: a
        ;; loop counter advancing by 2.  Compute in the scratch pair (ECX/EDX)
        ;; and let i386-store-vreg touch EAX only when vd really is VR.
         (let ((vd (first operands)) (va (second operands)) (vb (third operands)))
           (i386-load-vreg buf +scratch1+ vb)
           (i386-load-vreg buf +scratch0+ va)
           (i386-emit-xor-reg-reg buf +scratch0+ +scratch1+)
           (i386-store-vreg buf vd +scratch0+)))

        ;; ---- Constant-amount shifts, SATURATED at the word width ----------
        ;;
        ;; x86 MASKS a shift count to the low 5 bits in 32-bit operand size, so
        ;; `SAR EAX, 32' assembles to `SAR EAX, 0' — a NO-OP that silently
        ;; returns the operand unchanged.  The 64-bit ports never see this: a
        ;; count of 32 is an ordinary in-range 64-bit shift there.
        ;;
        ;; The MVM emits such counts.  compile-ash's inline fast path is gated
        ;; by `(and count (<= count 30))' — bounded ABOVE only — so EVERY right
        ;; shift, of any magnitude, is inlined as a single :sar.  `(ash 7 -32)'
        ;; therefore answered 7 on this target.
        ;;
        ;; That one wrong answer was the runtime-QUOTE failure.  mvm-emit-u64
        ;; emits its high word as `(logand (ash val -32) #xFFFFFFFF)'; with the
        ;; shift a no-op the high word repeated the low word, so every 8-byte
        ;; immediate read back wrong.  Under mvm-eval the only remaining user
        ;; of that path is :li-const, whose immediate is the *e2-const-pool*
        ;; INDEX for a quoted object — so `(eval '(quote X))' missed the pool
        ;; and returned NIL for every X.  Downstream: every compiled `',name'
        ;; was NIL, a runtime DEFMACRO's (set-macro-function ',name …)
        ;; registered nothing (silently — %macro-sym-key just answers NIL), and
        ;; (setf (symbol-function 'f) …) reported `not a symbol'.
        ;;
        ;; Saturating here rather than in compile-ash keeps the fix on the arch
        ;; whose instruction has the quirk, and leaves x64/aarch64 codegen
        ;; byte-identical.  A count >= 32 has exactly one right answer per op:
        ;; 0 for the logical shifts, and the replicated sign bit for SAR, which
        ;; `SAR reg, 31' produces.  (The variable-count :shlv/:sarv take their
        ;; count from CL and are masked by the same hardware rule; compile-ash
        ;; routes every VARIABLE count to runtime BIGNUM-ASH, so they are not
        ;; reachable with an out-of-range count from that path.)
        ((op= +op-shl+)
         (let ((vd (first operands)) (vs (second operands)) (amt (third operands)))
           (i386-load-vreg buf +scratch0+ vs)
           (if (>= amt 32)
               (i386-emit-mov-reg-imm buf +scratch0+ 0)
               (i386-emit-shl-reg-imm buf +scratch0+ amt))
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-shr+)
         (let ((vd (first operands)) (vs (second operands)) (amt (third operands)))
           (i386-load-vreg buf +scratch0+ vs)
           (if (>= amt 32)
               (i386-emit-mov-reg-imm buf +scratch0+ 0)
               (i386-emit-shr-reg-imm buf +scratch0+ amt))
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-sar+)
         (let ((vd (first operands)) (vs (second operands)) (amt (third operands)))
           ;; Use scratch0 (ECX) instead of EAX to avoid clobbering VR.
           ;; This is critical when multiple SARs operate on different vregs
           ;; consecutively (e.g., mem-ref untag of address then value).
           (i386-load-vreg buf +scratch0+ vs)
           ;; SAR by amt: always arithmetic shift to preserve sign.
           ;; (SHR for unsigned untagging has its own opcode +op-shr+.)
           (i386-emit-sar-reg-imm buf +scratch0+ (if (>= amt 32) 31 amt))
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
         ;; VR-PRESERVING (WS5): this opcode has NO destination, so it must
         ;; not touch EAX at all — EAX is VR, and the compiler keeps a live
         ;; operand there across the type-check `:or / :test` pair that
         ;; precedes open-coded generic arithmetic.  Clobbering it made
         ;; `(+ a b)` compute (a|b)+b.  Use the ECX/EDX scratch pair.
         (let ((va (first operands)) (vb (second operands)))
           (i386-load-vreg buf +scratch1+ vb)
           (i386-load-vreg buf +scratch0+ va)
           (i386-emit-cmp-reg-reg buf +scratch0+ +scratch1+)))

        ((op= +op-test+)
         ;; (test Va Vb) -- AND, sets flags, discards result
         ;; VR-PRESERVING (WS5): this opcode has NO destination, so it must
         ;; not touch EAX at all — EAX is VR, and the compiler keeps a live
         ;; operand there across the type-check `:or / :test` pair that
         ;; precedes open-coded generic arithmetic.  Clobbering it made
         ;; `(+ a b)` compute (a|b)+b.  Use the ECX/EDX scratch pair.
         (let ((va (first operands)) (vb (second operands)))
           (i386-load-vreg buf +scratch1+ vb)
           (i386-load-vreg buf +scratch0+ va)
           (i386-emit-test-reg-reg buf +scratch0+ +scratch1+)))

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
           ;; VR-PRESERVING (WS5): no destination — must not clobber EAX/VR.
           (i386-load-vreg buf +scratch0+ vs)
           (i386-emit-cmp-reg-abs buf +scratch0+ *vn-addr*)
           (i386-emit-jcc buf :e label)))

        ((op= +op-bnnull+)
         ;; (bnnull Vs off16) -- branch if Vs is not NIL
         (let* ((vs (first operands))
                (off (second operands))
                (target-pos (+ mvm-next-pos off))
                (label (i386-ensure-label-at state target-pos)))
           ;; VR-PRESERVING (WS5): no destination — must not clobber EAX/VR.
           (i386-load-vreg buf +scratch0+ vs)
           (i386-emit-cmp-reg-abs buf +scratch0+ *vn-addr*)
           (i386-emit-jcc buf :ne label)))

        ;; ============================================
        ;; List Operations (32-bit cons cells, 4-byte words)
        ;; ============================================
        ((op= +op-car+)
         ;; (car Vd Vs) -- load car from cons cell
         ;; Cons tag = 0x01, so untag: ptr - 1, car at [ptr-1+0] = [ptr-1]
         ;; With type check: verify low 4 bits == 0x01
         (let ((vd (first operands)) (vs (second operands)))
           (i386-load-vreg buf +scratch0+ vs)
           ;; Type check — VR-PRESERVING: value in ECX, tag scratch in EDX
           (i386-emit-mov-reg-reg buf +scratch1+ +scratch0+)
           (i386-emit-and-reg-imm buf +scratch1+ +tag-mask+)
           (i386-emit-cmp-reg-imm buf +scratch1+ +tag-cons+)
           (let ((ok-label (i386-make-label)))
             (i386-emit-jcc buf :e ok-label)
             (i386-emit-int3 buf)   ; trap on non-cons
             (i386-emit-label buf ok-label))
           ;; Strip tag, load car (word 0)
           (i386-emit-sub-reg-imm buf +scratch0+ +tag-cons+)
           (i386-emit-mov-reg-mem buf +scratch0+ +scratch0+ 0)
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-cdr+)
         ;; (cdr Vd Vs) -- load cdr from cons cell
         ;; cdr at [ptr - tag + 4] = [ptr - 1 + 4] = [ptr + 3]
         (let ((vd (first operands)) (vs (second operands)))
           (i386-load-vreg buf +scratch0+ vs)
           ;; Type check — VR-PRESERVING: value in ECX, tag scratch in EDX
           (i386-emit-mov-reg-reg buf +scratch1+ +scratch0+)
           (i386-emit-and-reg-imm buf +scratch1+ +tag-mask+)
           (i386-emit-cmp-reg-imm buf +scratch1+ +tag-cons+)
           (let ((ok-label (i386-make-label)))
             (i386-emit-jcc buf :e ok-label)
             (i386-emit-int3 buf)
             (i386-emit-label buf ok-label))
           ;; Strip tag, load cdr (word 1 = offset 4 on 32-bit)
           (i386-emit-sub-reg-imm buf +scratch0+ +tag-cons+)
           (i386-emit-mov-reg-mem buf +scratch0+ +scratch0+ 4)
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-cons+)
         ;; (cons Vd Va Vb) -- allocate cons cell via bump allocator
         ;; VA is at absolute addr. [VA+0]=car, [VA+4]=cdr
         ;; result = VA | cons_tag, advance VA by 16
         ;; 16-byte alignment required: 4-bit tag uses low nibble
         ;; IMPORTANT: Load cdr into scratch0 FIRST — loading car into EAX
         ;; clobbers VR, so if vb-arg=VR we'd get car's value for cdr.
         (let ((vd (first operands)) (va-arg (second operands)) (vb-arg (third operands)))
           ;; Load cdr value first (before EAX is touched)
           ;; VR-PRESERVING: car, cdr and the alloc pointer are three live
           ;; values but only ECX/EDX are available, so cdr goes via one
           ;; stack slot rather than through EAX (which is VR).
           (i386-load-vreg buf +scratch0+ vb-arg)          ; cdr
           (i386-emit-push-reg buf +scratch0+)
           (i386-emit-mov-reg-abs buf +scratch1+ *va-addr*); EDX = base
           (i386-load-vreg buf +scratch0+ va-arg)          ; car
           (i386-emit-mov-mem-reg buf +scratch1+ 0 +scratch0+)
           (i386-emit-pop-reg buf +scratch0+)              ; cdr back
           (i386-emit-mov-mem-reg buf +scratch1+ 4 +scratch0+)
           ;; #259: a cons occupies 16 bytes but only car(+0)/cdr(+4) are ever
           ;; written.  The other HALF is read by the flat Cheney sweep at
           ;; every collection — and copy_object's cons arm carries it along,
           ;; copying 8 bytes and advancing the free pointer by 16, so the
           ;; stale words are re-swept in to-space too.  Conses dominate the
           ;; heap, so these two words are the single largest source of
           ;; uninitialised memory the i386 collector reads.
           ;;
           ;; RESIDUAL, stated rather than assumed: this fills the tail of a
           ;; FRESHLY ALLOCATED cons only.  i386-emit-gcnative-copy's cons arm
           ;; writes [ESI+0]/[ESI+4] and then advances ESI by 16, so a cons
           ;; that SURVIVES a collection lands on two words of stale to-space
           ;; content again.  Closing that needs two stores in copy_object
           ;; (cheap — per surviving cons, not per allocation), but it is a
           ;; codegen change and therefore a fresh ladder gate, and the
           ;; measured value of the whole exercise is nil (see the #259 note
           ;; in CLAUDE.md), so it is deliberately left for whoever next has a
           ;; reason to re-gate this file.
           (i386-emit-fill-slots buf +scratch1+ '(8 12) :cons +scratch0+)
           (i386-emit-gc-mark-start buf +scratch1+ t)   ; object-start + cons-kind
           (i386-emit-mov-reg-reg buf +scratch0+ +scratch1+)
           (i386-emit-or-reg-imm buf +scratch0+ +tag-cons+)
           (i386-emit-add-reg-imm buf +scratch1+ 16)
           (i386-emit-mov-abs-reg buf *va-addr* +scratch1+)
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-setcar+)
         ;; (setcar Vd Vs) -- [Vd - tag] = Vs
         ;; IMPORTANT: Load value FIRST — loading vd-reg into EAX clobbers VR,
         ;; so if vs=VR we'd get the cons pointer instead of the value.
         (let ((vd-reg (first operands)) (vs (second operands)))
           (i386-load-vreg buf +scratch0+ vs)
           (i386-load-vreg buf +scratch1+ vd-reg)
           (i386-emit-sub-reg-imm buf +scratch1+ +tag-cons+)
           (i386-emit-mov-mem-reg buf +scratch1+ 0 +scratch0+)))

        ((op= +op-setcdr+)
         ;; (setcdr Vd Vs) -- [Vd - tag + 4] = Vs
         ;; IMPORTANT: Load value FIRST — same VR clobber issue as setcar.
         (let ((vd-reg (first operands)) (vs (second operands)))
           (i386-load-vreg buf +scratch0+ vs)
           (i386-load-vreg buf +scratch1+ vd-reg)
           (i386-emit-sub-reg-imm buf +scratch1+ +tag-cons+)
           (i386-emit-mov-mem-reg buf +scratch1+ 4 +scratch0+)))

        ((op= +op-consp+)
         ;; (consp Vd Vs) -- test low 4 bits for cons tag
         ;;
         ;; MUST EXCLUDE NIL FIRST.  On hosted Linux/i386 NIL is
         ;; #xDEAD0001 (+linux-i386-nil-value+), whose low nibble IS
         ;; +tag-cons+ — so a bare tag test answers T for NIL.  x64 has
         ;; always compared against R15 before the tag test for exactly
         ;; this reason; i386 never did.
         ;;
         ;; The consequence was not subtle.  `(car NIL)` correctly yields
         ;; NIL (compile-cxr-guard-and-deref short-circuits on :bnull), so
         ;; the improper-list-safe idiom used all over the compiler --
         ;;     (loop while (consp cur) do (walk (car cur) ...)
         ;;                                (setq cur (cdr cur)))
         ;; -- never terminated on i386: reaching the list's terminating
         ;; NIL, consp said T, (car NIL) gave NIL back, and the walk
         ;; recursed on NIL forever.  That is what blew the 8 MB stack with
         ;; 26,869 frames of %RETURN-FROM-ESCAPES-BLOCK-P's inner WALK on
         ;; (eval 42): the walker was not walking 42, it was walking NIL,
         ;; whose CAR is itself.
         ;;
         ;; Bare-metal i386 uses 0 for NIL, whose low nibble is 0 and so
         ;; was never affected; comparing against *vn-addr* (rather than
         ;; baking an immediate) is correct on both.
         (let ((vd (first operands)) (vs (second operands)))
           (i386-load-vreg buf +scratch0+ vs)
           (let ((true-label (i386-make-label))
                 (false-label (i386-make-label))
                 (done-label (i386-make-label)))
             (i386-emit-cmp-reg-abs buf +scratch0+ *vn-addr*)
             (i386-emit-jcc buf :e false-label)
             (i386-emit-and-reg-imm buf +scratch0+ +tag-mask+)
             (i386-emit-cmp-reg-imm buf +scratch0+ +tag-cons+)
             (i386-emit-jcc buf :e true-label)
             (i386-emit-label buf false-label)
             (i386-emit-mov-reg-abs buf +scratch0+ *vn-addr*)
             (i386-emit-jmp-rel32 buf done-label)
             (i386-emit-label buf true-label)
             (i386-emit-mov-reg-imm buf +scratch0+ +i386-mvm-t+)
             (i386-emit-label buf done-label))
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-atom+)
         ;; (atom Vd Vs) -- opposite of consp
         ;; VR-PRESERVING: computes in ECX, exactly like :consp above.  This
         ;; used to compute in EAX, which IS VR on i386, so whenever the
         ;; destination vreg was not VR it destroyed a live value.  Caught by
         ;; the mechanized invariant checker the moment layer 5 baked
         ;; compiler.lisp in — the first source to use ATOM with a non-VR
         ;; destination (5 sites).  Nothing before it had, which is exactly how
         ;; this class hides: the function size is unchanged and the wrong
         ;; value surfaces far from the opcode that produced it.
         ;; ...and NIL IS an atom, so it takes the T path here — the mirror
         ;; of the +op-consp+ NIL exclusion above.  Without it (atom NIL)
         ;; answered NIL on hosted Linux/i386.
         (let ((vd (first operands)) (vs (second operands)))
           (i386-load-vreg buf +scratch0+ vs)
           (let ((true-label (i386-make-label))
                 (done-label (i386-make-label)))
             (i386-emit-cmp-reg-abs buf +scratch0+ *vn-addr*)
             (i386-emit-jcc buf :e true-label)
             (i386-emit-and-reg-imm buf +scratch0+ +tag-mask+)
             (i386-emit-cmp-reg-imm buf +scratch0+ +tag-cons+)
             (i386-emit-jcc buf :ne true-label)
             ;; Is cons -> return NIL
             (i386-emit-mov-reg-abs buf +scratch0+ *vn-addr*)
             (i386-emit-jmp-rel32 buf done-label)
             ;; Not cons (or NIL) -> return T
             (i386-emit-label buf true-label)
             (i386-emit-mov-reg-imm buf +scratch0+ +i386-mvm-t+)
             (i386-emit-label buf done-label))
           (i386-store-vreg buf vd +scratch0+)))

        ;; ============================================
        ;; Object Operations (32-bit, 4-byte slots)
        ;; ============================================
        ((op= +op-alloc-obj+)
         ;; (alloc-obj Vd count:imm16 subtag:imm8)
         ;; Header word at [VA]: (count << 8) | subtag  (matches x64 format)
         ;; Result = VA | object_tag, advance VA by aligned size
         (let ((vd (first operands)) (count (second operands)) (subtag (third operands)))
           (i386-emit-mov-reg-abs buf +scratch1+ *va-addr*)
           (i386-emit-mov-mem-imm buf +scratch1+ 0
                                  (logior (ash count 8) subtag))
           ;; VR-PRESERVING: bump in ECX, then tag the BASE in EDX in place,
           ;; so no third register (EAX = VR) is needed.
           (let ((total (logand (+ (* (1+ count) 4) 15) (lognot 15))))
             ;; #259: the caller fills the slots AFTER this opcode returns, and
             ;; may allocate (and therefore collect) in between; the 16-byte
             ;; granule adds up to three more unwritten tail words on top.
             ;; Fill BEFORE ECX becomes the bump register: ECX is dead here, so
             ;; the unrolled form can hoist the fill word into it (3 bytes a
             ;; slot instead of 7).  Above the threshold a runtime loop keeps a
             ;; wide struct from blowing up code size; that form consumes ECX
             ;; as its cursor, so the bump is recomputed after it.
             (cond ((null (i386-fill-word :obj))
                    (i386-emit-mov-reg-reg buf +scratch0+ +scratch1+)
                    (i386-emit-add-reg-imm buf +scratch0+ total))
                   ((<= total 80)
                    (i386-emit-fill-slots
                     buf +scratch1+
                     (loop for off from 4 below total by 4 collect off) :obj
                     +scratch0+)
                    (i386-emit-mov-reg-reg buf +scratch0+ +scratch1+)
                    (i386-emit-add-reg-imm buf +scratch0+ total))
                   (t
                    (i386-emit-mov-reg-reg buf +scratch0+ +scratch1+)
                    (i386-emit-add-reg-imm buf +scratch0+ total)
                    (i386-emit-fill-range buf +scratch0+ +scratch1+ :obj)
                    (i386-emit-mov-reg-reg buf +scratch0+ +scratch1+)
                    (i386-emit-add-reg-imm buf +scratch0+ total))))
           (i386-emit-mov-abs-reg buf *va-addr* +scratch0+)
           (i386-emit-gc-mark-start buf +scratch1+)      ; object-start only
           (i386-emit-or-reg-imm buf +scratch1+ +tag-object+)
           (i386-store-vreg buf vd +scratch1+)))

        ((op= +op-alloc-array+)
         ;; (alloc-array Vd Vcount) — dynamic array allocation
         ;; Vcount: UNTAGGED element count (compiler SAR'd it already)
         ;; Allocates (count+1)*4 bytes, aligned to 16 (header + elements)
         ;; Header = (count << 8) | 0x32 (array subtag)
         ;; Result = VA | 0x09 (object tag)
         (let ((vd (first operands)) (vcount (second operands)))
           ;; Load count into ECX
           ;; VR-PRESERVING: keep the BASE in EDX throughout and tag it in
           ;; place at the end, so no third register (EAX = VR) is needed.
           (i386-load-vreg buf +scratch0+ vcount)
           (i386-emit-push-reg buf +scratch0+)              ; save count
           (i386-emit-shl-reg-imm buf +scratch0+ 8)
           (i386-emit-or-reg-imm buf +scratch0+ #x32)
           (i386-emit-mov-reg-abs buf +scratch1+ *va-addr*) ; EDX = base
           (i386-emit-mov-mem-reg buf +scratch1+ 0 +scratch0+)
           (i386-emit-pop-reg buf +scratch0+)               ; count
           (i386-emit-add-reg-imm buf +scratch0+ 1)
           (i386-emit-shl-reg-imm buf +scratch0+ 2)
           (i386-emit-add-reg-imm buf +scratch0+ 15)
           (i386-emit-and-reg-imm buf +scratch0+ -16)
           (i386-emit-add-reg-reg buf +scratch0+ +scratch1+); ECX = new VA
           (i386-emit-mov-abs-reg buf *va-addr* +scratch0+)
           ;; #259: the whole payload is unwritten until the caller fills it.
           (i386-emit-fill-range buf +scratch0+ +scratch1+ :obj)
           (i386-emit-gc-mark-start buf +scratch1+)      ; object-start only
           (i386-emit-or-reg-imm buf +scratch1+ +tag-object+)
           (i386-store-vreg buf vd +scratch1+)))

        ((op= +op-obj-ref+)
         ;; (obj-ref Vd Vobj idx:imm8) -- load object slot
         (let ((vd (first operands)) (vobj (second operands)) (idx (third operands)))
           (if (= vobj +vreg-vfp+)
               (progn
                 (i386-emit-mov-reg-mem buf +scratch0+ +i386-ebp+
                                        (+ +frame-slot-base+ (* idx -4)))
                 (i386-store-vreg buf vd +scratch0+))
               (progn
                 (i386-load-vreg buf +scratch0+ vobj)
                 (i386-emit-sub-reg-imm buf +scratch0+ +tag-object+)
                 (i386-emit-mov-reg-mem buf +scratch0+ +scratch0+ (* (1+ idx) 4))
                 (i386-store-vreg buf vd +scratch0+)))))

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
                 (i386-load-vreg buf +scratch1+ vobj)
                 (i386-emit-sub-reg-imm buf +scratch1+ +tag-object+)
                 (i386-emit-mov-mem-reg buf +scratch1+ (* (1+ idx) 4) +scratch0+)))))

        ;; ============================================
        ;; SAP (System Area Pointer) — a 1-slot object, subtag #x16, whose
        ;; single slot holds a RAW machine address rather than a tagged value.
        ;; Only SAP-NEW and SAP-ADDR are reached by the i386 CL image (one site
        ;; each); the ref/set opcodes are not emitted here and remain absent.
        ;;
        ;; i386 sizing: align16((1+1)*4) = 16 bytes, slot 0 at raw+4 — the
        ;; same 16 bytes x64 uses, but for a different reason (x64: 8-byte
        ;; header + 8-byte slot; i386: 4-byte header + 4-byte slot + padding).
        ;; ============================================
        ((op= +op-sap-new+)
         ;; (sap-new Vd Vaddr) — box the RAW address in Vaddr.
         (let ((vd (first operands)) (vaddr (second operands)))
           ;; Load the payload FIRST: it may live in a spill slot, and the
           ;; allocation sequence below owns both scratch registers.
           (i386-load-vreg buf +scratch0+ vaddr)
           (i386-emit-mov-reg-abs buf +scratch1+ *va-addr*)        ; EDX = base
           (i386-emit-mov-mem-reg buf +scratch1+ 4 +scratch0+)     ; slot 0 = addr
           (i386-emit-mov-mem-imm buf +scratch1+ 0 #x116)          ; (1<<8)|#x16
           ;; #259: header(+0) and slot 0(+4) are written; the 16-byte granule
           ;; leaves +8/+12 for the flat sweep to read.
           (i386-emit-fill-slots buf +scratch1+ '(8 12) :obj +scratch0+)
           (i386-emit-mov-reg-reg buf +scratch0+ +scratch1+)
           (i386-emit-add-reg-imm buf +scratch0+ 16)
           (i386-emit-mov-abs-reg buf *va-addr* +scratch0+)
           (i386-emit-gc-mark-start buf +scratch1+)                ; object-start
           (i386-emit-or-reg-imm buf +scratch1+ +tag-object+)
           (i386-store-vreg buf vd +scratch1+)))

        ((op= +op-sap-addr+)
         ;; (sap-addr Vd Vsap) — raw address out, TAGGED as a fixnum so it can
         ;; be handed to syscall3 (which untags every argument), exactly as
         ;; translate-x64's arm does.
         ;;
         ;; The `shl 1' is lossy for an address >= 2^31 on i386 (x64 loses
         ;; nothing until 2^63).  Every SAP in this image points into the
         ;; mmap'd heap at 0x1FFF_xxxx, so it does not bite today; a SAP over a
         ;; high-half mapping would need a different encoding.  Stated rather
         ;; than silently assumed.
         (let ((vd (first operands)) (vsap (second operands)))
           (i386-load-vreg buf +scratch0+ vsap)
           (i386-emit-sub-reg-imm buf +scratch0+ +tag-object+)
           (i386-emit-mov-reg-mem buf +scratch0+ +scratch0+ 4)
           (i386-emit-shl-reg-imm buf +scratch0+ 1)                ; tag
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-obj-tag+)
         ;; (obj-tag Vd Vs) -- extract low 4-bit tag as tagged fixnum
         (let ((vd (first operands)) (vs (second operands)))
           (i386-load-vreg buf +scratch0+ vs)
           (i386-emit-and-reg-imm buf +scratch0+ +tag-mask+)
           (i386-emit-shl-reg-imm buf +scratch0+ 1)  ; tag as fixnum
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-obj-subtag+)
         ;; (obj-subtag Vd Vs) -- extract subtag from object header
         ;; Header format: (count << 8) | subtag. Subtag is low 8 bits.
         ;;
         ;; TAG-SAFE, exactly as x64's +op-obj-subtag+ has been: only
         ;; dereference when Vs really is a heap pointer.  Two ways the raw
         ;; deref crashes:
         ;;   1. Vs's low nibble is not +tag-object+ (fixnum / cons /
         ;;      immediate / forward) — [Vs-9] is off by a random offset.
         ;;   2. Vs IS T (+i386-mvm-t+ = #xDEAD1009).  Its low nibble is 9,
         ;;      so it LOOKS like a heap pointer, but it is an immediate and
         ;;      [T-9] = #xDEAD1000 is unmapped.
         ;; On either, return subtag 0, which falsifies every caller's
         ;; specific-subtag comparison — the answer they want.
         ;;
         ;; The compiler RELIES on this: compile-integerp's docstring says
         ;; in as many words that it may reach the bignum-subtag check on T
         ;; "without crashing" because the opcode bails safely.  i386 did
         ;; not bail, so `(integerp T)` SIGSEGVed — which is where
         ;; %install-runtime-cl-macros died at boot, via %FORM-OP-IS's
         ;; `(and (integerp op) …)` while compiling a macro definition.
         ;;
         ;; NIL (#xDEAD0001 hosted, 0 bare-metal) has a non-9 nibble either
         ;; way, so the tag check already covers it; only T needs naming.
         (let ((vd (first operands)) (vs (second operands)))
           (i386-load-vreg buf +scratch0+ vs)
           (let ((fail-label (i386-make-label))
                 (done-label (i386-make-label)))
             (i386-emit-mov-reg-reg buf +scratch1+ +scratch0+)
             (i386-emit-and-reg-imm buf +scratch1+ +tag-mask+)
             (i386-emit-cmp-reg-imm buf +scratch1+ +tag-object+)
             (i386-emit-jcc buf :ne fail-label)
             ;; T-immediate check (register compare, not a >2^31 imm32).
             (i386-emit-mov-reg-imm buf +scratch1+ +i386-mvm-t+)
             (i386-emit-cmp-reg-reg buf +scratch0+ +scratch1+)
             (i386-emit-jcc buf :e fail-label)
             (i386-emit-sub-reg-imm buf +scratch0+ +tag-object+)
             (i386-emit-mov-reg-mem buf +scratch0+ +scratch0+ 0)  ; load header
             (i386-emit-and-reg-imm buf +scratch0+ #xFF)          ; mask to subtag
             (i386-emit-shl-reg-imm buf +scratch0+ 1)             ; tag as fixnum
             (i386-emit-jmp-rel32 buf done-label)
             (i386-emit-label buf fail-label)
             (i386-emit-mov-reg-imm buf +scratch0+ 0)
             (i386-emit-label buf done-label))
           (i386-store-vreg buf vd +scratch0+)))

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
           (i386-load-vreg buf +scratch1+ vobj)
           (i386-emit-add-reg-reg buf +scratch0+ +scratch1+)
           (i386-emit-mov-reg-mem buf +scratch0+ +scratch0+ -5)
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-aset+)
         ;; (aset Vobj Vidx Vs) — variable-index array store
         ;; Store Vs at [Vobj + Vidx*2 - 5]
         (let ((vobj (first operands)) (vidx (second operands)) (vs (third operands)))
           ;; Load value into scratch0 FIRST (before EAX clobbers VR)
           ;; Three operands but only two scratch registers now that EAX is
           ;; off-limits, so the VALUE goes through one stack slot.
           (i386-load-vreg buf +scratch0+ vs)
           (i386-emit-push-reg buf +scratch0+)          ; save value
           (i386-load-vreg buf +scratch0+ vidx)
           (i386-emit-shl-reg-imm buf +scratch0+ 1)
           (i386-load-vreg buf +scratch1+ vobj)
           (i386-emit-add-reg-reg buf +scratch0+ +scratch1+)  ; ECX = addr+5
           (i386-emit-pop-reg buf +scratch1+)           ; EDX = value
           (i386-emit-mov-mem-reg buf +scratch0+ -5 +scratch1+)))

        ((op= +op-array-len+)
         ;; (array-len Vd Vobj) — extract element count from header
         ;; Header at [Vobj - 9], count in upper 24 bits (on 32-bit: [31:8])
         ;; Tagged result = count << 1
         ;; Same tag-safety hazard as +op-obj-subtag+ above, and x64 carries
         ;; the same mirror guard: predicates that gate on obj-subtag now
         ;; return 0 for T, but %clos-instance-p / %gf-p go on to ask
         ;; (>= (array-length x) 1) — which lands here.  On a tag or
         ;; T-immediate miss return tagged fixnum 0, so `(>= … 1)' answers
         ;; NIL, i.e. "not an array of N+ slots".
         (let ((vd (first operands)) (vobj (second operands)))
           (i386-load-vreg buf +scratch0+ vobj)
           (let ((fail-label (i386-make-label))
                 (done-label (i386-make-label)))
             (i386-emit-mov-reg-reg buf +scratch1+ +scratch0+)
             (i386-emit-and-reg-imm buf +scratch1+ +tag-mask+)
             (i386-emit-cmp-reg-imm buf +scratch1+ +tag-object+)
             (i386-emit-jcc buf :ne fail-label)
             (i386-emit-mov-reg-imm buf +scratch1+ +i386-mvm-t+)
             (i386-emit-cmp-reg-reg buf +scratch0+ +scratch1+)
             (i386-emit-jcc buf :e fail-label)
             (i386-emit-sub-reg-imm buf +scratch0+ +tag-object+)
             (i386-emit-mov-reg-mem buf +scratch0+ +scratch0+ 0)  ; load header word
             (i386-emit-shr-reg-imm buf +scratch0+ 8)             ; count = header >> 8
             (i386-emit-and-reg-imm buf +scratch0+ #xFFFFFF)      ; mask to 24 bits
             (i386-emit-shl-reg-imm buf +scratch0+ 1)             ; tag as fixnum
             (i386-emit-jmp-rel32 buf done-label)
             (i386-emit-label buf fail-label)
             (i386-emit-mov-reg-imm buf +scratch0+ 0)
             (i386-emit-label buf done-label))
           (i386-store-vreg buf vd +scratch0+)))

        ;; ============================================
        ;; Raw Memory Operations
        ;; ============================================
        ((op= +op-load+)
         ;; (load Vd Vaddr width:imm8)
         (let ((vd (first operands)) (vaddr (second operands)) (width (third operands)))
           (i386-load-vreg buf +scratch0+ vaddr)
           (ecase width
             (0 (i386-emit-movzx-byte buf +scratch0+ +scratch0+ 0))
             (1 (i386-emit-movzx-word buf +scratch0+ +scratch0+ 0))
             (2 (i386-emit-mov-reg-mem buf +scratch0+ +scratch0+ 0))
             (3 (i386-emit-mov-reg-mem buf +scratch0+ +scratch0+ 0)))  ; 64-bit: low 32
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-store+)
         ;; (store Vaddr Vs width:imm8)
         ;; IMPORTANT: Load Vs (value) FIRST — loading Vaddr into EAX clobbers VR,
         ;; so if Vs=VR we'd get the address instead of the value.
         (let ((vaddr (first operands)) (vs (second operands)) (width (third operands)))
           (i386-load-vreg buf +scratch0+ vs)
           (i386-load-vreg buf +scratch1+ vaddr)
           (ecase width
             (0 (i386-emit-mov-mem8-reg buf +scratch1+ 0 +scratch0+))
             (1 (i386-emit-mov-mem16-reg buf +scratch1+ 0 +scratch0+))
             (2 (i386-emit-mov-mem-reg buf +scratch1+ 0 +scratch0+))
             (3 (i386-emit-mov-mem-reg buf +scratch1+ 0 +scratch0+)))))

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
           ;; Strip the +3 function tag that :fn-addr applied (TAG-PLAN.md).
           ;; Gated so the legacy bare-metal i386 images, which never had a
           ;; working :fn-addr, stay byte-identical.
           (when *i386-fn-tag-3*
             (i386-emit-sub-reg-imm buf +i386-eax+ 3))
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
           (i386-emit-mov-reg-abs buf +scratch1+ *va-addr*)
           ;; #259: :alloc-cons hands back a cell whose car/cdr the caller
           ;; writes later, so ALL FOUR words are uninitialised on exit.
           (i386-emit-fill-slots buf +scratch1+ '(0 4 8 12) :cons +scratch0+)
           (i386-emit-gc-mark-start buf +scratch1+ t)   ; object-start + cons-kind
           (i386-emit-mov-reg-reg buf +scratch0+ +scratch1+)
           (i386-emit-or-reg-imm buf +scratch0+ +tag-cons+)
           (i386-emit-add-reg-imm buf +scratch1+ 16)
           (i386-emit-mov-abs-reg buf *va-addr* +scratch1+)
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-gc-check+)
         ;; Compare VA (alloc ptr) against VL (alloc limit)
         ;; Both at absolute addresses. If VA >= VL, trigger GC.
         ;; IMPORTANT: Use scratch0 (ECX), NOT EAX! EAX is VR and may hold
         ;; a live value (e.g., the car arg popped before GC-CHECK + CONS).
         (i386-emit-mov-reg-abs buf +scratch0+ *va-addr*)
         (i386-emit-cmp-reg-abs buf +scratch0+ *vl-addr*)
         (let ((ok-label (i386-make-label)))
           (i386-emit-jcc buf :b ok-label)   ; unsigned below -> room left
           ;; GC needed
           (let ((gc-lbl (and *i386-gc-enabled*
                              (i386-translate-state-gc-label state))))
             (if gc-lbl
                 (i386-emit-call-rel32 buf gc-lbl)
                 (i386-emit-int buf #x31)))  ; trap to GC handler
           (i386-emit-label buf ok-label)))

        ((op= +op-write-barrier+)
         ;; (write-barrier Vobj) -- mark card table dirty (stub)
         (let ((vobj (first operands)))
           (i386-load-vreg buf +scratch0+ vobj)
           (i386-emit-shr-reg-imm buf +scratch0+ 12)
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
         (i386-emit-push-abs buf *va-addr*)
         (i386-emit-push-abs buf *vl-addr*)
         (i386-emit-push-abs buf *vn-addr*))

        ((op= +op-restore-ctx+)
         ;; Restore (reverse order of save)
         (i386-emit-pop-abs buf *vn-addr*)
         (i386-emit-pop-abs buf *vl-addr*)
         (i386-emit-pop-abs buf *va-addr*)
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

        ;; ================================================================
        ;; WS5 (i386 CL / mvm-eval): opcodes that existed on x64 + aarch64
        ;; but had never been ported here, so they hit the INT3 default.
        ;; Every one of these is load-bearing for the real CL runtime:
        ;; without them a Modus CL image on i386 traps as soon as it takes
        ;; a function address, builds a closure, allocates a string, or
        ;; compiles a form at runtime.
        ;; ================================================================

        ;; ---- LI-CONST Vd, idx ----  (constant-pool literal)
        ((op= +op-li-const+)
         ;; Emit `MOV r32, imm32` (B8+r id — always 5 bytes) as a
         ;; placeholder and record the imm field for the image-assembly
         ;; patch pass.  x64 uses a 10-byte MOVABS; on i386 a plain imm32
         ;; already covers the whole address space.
         (let* ((vd (first operands))
                (idx (second operands))
                (pd (i386-vreg-phys vd))
                (dst (or pd +i386-eax+))
                (start (i386-current-pos buf)))
           (i386-emit-mov-reg-imm buf dst 0)
           (let ((emitted (- (i386-current-pos buf) start)))
             (unless (= emitted 5)
               (error "i386 li-const: expected 5-byte MOV r32,imm32, got ~D"
                      emitted)))
           (push (cons (+ start 1) idx) *i386-li-const-patches*)
           (unless pd (i386-store-vreg buf vd dst))))

        ;; ---- FN-ADDR Vd, target ----  (tagged native function address)
        ((op= +op-fn-addr+)
         ;; i386 has no RIP-relative LEA, so the address is materialised
         ;; position-independently:
         ;;     call .next          ; E8 00000000  (pushes &.next)
         ;;   .next:
         ;;     pop  dst            ; 58+r
         ;;     add  dst, (fn-.next); 81 /0 id   <- :diff32 fixup
         ;;     or   dst, 3         ; function tag (when *i386-fn-tag-3*)
         ;; The ADD displacement is a buffer-relative difference, so it is
         ;; resolved entirely inside i386-fixup-labels.
         (let* ((vd (first operands))
                (target-offset (second operands))
                (fn-table (i386-translate-state-function-table state))
                (label (when fn-table (gethash target-offset fn-table)))
                (pd (i386-vreg-phys vd))
                ;; VR-PRESERVING: a spilled destination stages through ECX,
                ;; never EAX (which is VR).
                (dst (or pd +scratch0+)))
           (cond
             (label
              (i386-emit-byte buf #xE8)          ; call rel32
              (i386-emit-u32 buf 0)              ; rel32 = 0 -> next insn
              (let ((anchor (i386-current-pos buf)))
                (i386-emit-pop-reg buf dst)
                (i386-emit-byte buf #x81)                     ; ADD r/m32, imm32
                (i386-emit-byte buf (i386-modrm #b11 0 dst))  ; /0, mod=11
                (i386-emit-fixup-diff32 buf label anchor))
              (when *i386-fn-tag-3*
                (i386-emit-or-reg-imm buf dst 3)))
             (t
              ;; Unresolved name (the compiler's #xFFFFFFF0 sentinel): load
              ;; NIL so funcall's NIL guard raises UNDEFINED-FUNCTION rather
              ;; than calling a wild address.  Same choice as translate-x64.
              (i386-emit-mov-reg-abs buf dst *vn-addr*)))
           (unless pd (i386-store-vreg buf vd dst))))

        ;; ---- SET-CENV Vs / GET-CENV Vd ----  (closure environment)
        ;; x64 keeps this in R13.  i386 has no spare register, so it lives
        ;; in the global slot block; see *cenv-addr*.
        ((op= +op-set-cenv+)
         (let ((vs (first operands)))
           (i386-load-vreg buf +scratch0+ vs)
           (i386-emit-mov-abs-reg buf *cenv-addr* +scratch0+)))

        ((op= +op-get-cenv+)
         (let ((vd (first operands)))
           (i386-emit-mov-reg-abs buf +scratch0+ *cenv-addr*)
           (i386-store-vreg buf vd +scratch0+)))

        ;; ---- SET-NARGS imm8 / GET-NARGS Vd ----
        ;; Raw (untagged) count in the slot; :get-nargs tags it (<<1) so the
        ;; rest of the IR's tagged world can compare it against :li values.
        ((op= +op-set-nargs+)
         (let ((n (logand (first operands) #xFF)))
           (i386-emit-byte buf #xC7)                       ; MOV r/m32, imm32
           (i386-emit-byte buf (i386-modrm #b00 0 5))      ; mod=00 rm=101 -> disp32
           (i386-emit-u32 buf *nargs-addr*)
           (i386-emit-u32 buf n)))

        ((op= +op-get-nargs+)
         (let ((vd (first operands)))
           (i386-emit-mov-reg-abs buf +scratch0+ *nargs-addr*)
           (i386-emit-shl-reg-imm buf +scratch0+ 1)        ; tag as fixnum
           (i386-store-vreg buf vd +scratch0+)))

        ;; ---- SET-MV-COUNT imm8 ----  (multiple-values count, TAGGED)
        ((op= +op-set-mv-count+)
         (let ((tagged (ash (first operands) 1)))
           (i386-emit-byte buf #xC7)
           (i386-emit-byte buf (i386-modrm #b00 0 5))
           (i386-emit-u32 buf *mvcount-addr*)
           (i386-emit-u32 buf tagged)))

        ;; ---- ALLOC-STRING Vd, Vcount ----
        ;; Identical to :alloc-array except for the subtag (#x31).  Vcount is
        ;; UNTAGGED (the compiler already SAR'd it), and — as on every other
        ;; target — a string stores one char CODE per WORD, so the element
        ;; count is a word count: size = align16((count+1)*4).
        ((op= +op-alloc-string+)
         (let ((vd (first operands)) (vcount (second operands)))
           (i386-load-vreg buf +scratch0+ vcount)
           (i386-emit-push-reg buf +scratch0+)
           (i386-emit-shl-reg-imm buf +scratch0+ 8)
           (i386-emit-or-reg-imm buf +scratch0+ #x31)       ; STRING subtag
           (i386-emit-mov-reg-abs buf +scratch1+ *va-addr*)  ; EDX = base
           (i386-emit-mov-mem-reg buf +scratch1+ 0 +scratch0+)
           (i386-emit-pop-reg buf +scratch0+)               ; count
           (i386-emit-add-reg-imm buf +scratch0+ 1)         ; + header word
           (i386-emit-shl-reg-imm buf +scratch0+ 2)         ; * 4
           (i386-emit-add-reg-imm buf +scratch0+ 15)
           (i386-emit-and-reg-imm buf +scratch0+ -16)
           (i386-emit-add-reg-reg buf +scratch0+ +scratch1+); ECX = new VA
           (i386-emit-mov-abs-reg buf *va-addr* +scratch0+)
           ;; #259: string CONTENT is filled only when *i386-fill-strings* is
           ;; on.  Default OFF, mirroring translate-x64.lisp: zeroing string
           ;; bodies there turned uninitialised garbage into NUL (char-code 0)
           ;; and desynchronised the reader — a functional regression proven
           ;; (by an empty-range layout-isolation probe) to be about content,
           ;; not layout and not GC roots.
           (when *i386-fill-strings*
             (i386-emit-fill-range buf +scratch0+ +scratch1+ :obj))
           ;; VR-PRESERVING: tag the base in EDX in place (never EAX).
           (i386-emit-gc-mark-start buf +scratch1+)      ; object-start only
           (i386-emit-or-reg-imm buf +scratch1+ +tag-object+)
           (i386-store-vreg buf vd +scratch1+)))

        ;; ---- ALLOC-U8 Vd, Vcount ----  byte-packed (unsigned-byte 8) vector
        ;; Feature #183.  Vcount is a TAGGED fixnum (byte count N).  Object:
        ;; header at [VA] = (N << 8) | #x11, then N packed bytes at +4 (i386
        ;; objects have a 4-byte header and NO padding word — cf. :obj-ref's
        ;; (1+idx)*4).  Total size = align16(4 + N).
        ;; NOTE: the 24-bit i386 count field caps a u8 vector at 16 MB.
        ((op= +op-alloc-u8+)
         (let ((vd (first operands)) (vcount (second operands)))
           (i386-load-vreg buf +scratch0+ vcount)
           (i386-emit-sar-reg-imm buf +scratch0+ 1)         ; untag -> N
           (i386-emit-push-reg buf +scratch0+)
           (i386-emit-shl-reg-imm buf +scratch0+ 8)
           (i386-emit-or-reg-imm buf +scratch0+ #x11)       ; u8-vector subtag
           (i386-emit-mov-reg-abs buf +scratch1+ *va-addr*)  ; EDX = base
           (i386-emit-mov-mem-reg buf +scratch1+ 0 +scratch0+)
           (i386-emit-pop-reg buf +scratch0+)               ; N
           (i386-emit-add-reg-imm buf +scratch0+ 4)         ; header bytes
           (i386-emit-add-reg-imm buf +scratch0+ 15)
           (i386-emit-and-reg-imm buf +scratch0+ -16)
           (i386-emit-add-reg-reg buf +scratch0+ +scratch1+); ECX = new VA
           (i386-emit-mov-abs-reg buf *va-addr* +scratch0+)
           ;; #259: the byte payload is unwritten until the caller fills it.
           ;; Unlike :alloc-string this is safe to fill — a u8 vector is a
           ;; numeric buffer with no reader interaction, and translate-x64
           ;; zeroes its counterpart for exactly this reason.
           (i386-emit-fill-range buf +scratch0+ +scratch1+ :obj)
           ;; VR-PRESERVING: tag the base in EDX in place (never EAX).
           (i386-emit-gc-mark-start buf +scratch1+)      ; object-start only
           (i386-emit-or-reg-imm buf +scratch1+ +tag-object+)
           (i386-store-vreg buf vd +scratch1+)))

        ;; ---- U8-REF Vd, Varr, Vidx ----
        ;; Byte address = (Varr - 9) + 4 + real_idx = Varr + real_idx - 5.
        ;; Vidx is TAGGED (real_idx*2); result is TAGGED (byte << 1), matching
        ;; mem-ref :u8 and the x64/aarch64 ports.
        ((op= +op-u8-ref+)
         (let ((vd (first operands))
               (varr (second operands))
               (vidx (third operands)))
           (i386-load-vreg buf +scratch0+ vidx)
           (i386-load-vreg buf +scratch1+ varr)
           (i386-emit-sar-reg-imm buf +scratch0+ 1)         ; real_idx
           (i386-emit-add-reg-reg buf +scratch0+ +scratch1+)
           (i386-emit-movzx-byte buf +scratch0+ +scratch0+ -5)
           (i386-emit-shl-reg-imm buf +scratch0+ 1)         ; tag as fixnum
           (i386-store-vreg buf vd +scratch0+)))

        ;; ---- U8-SET Varr, Vidx, Vval ----
        ;; Byte address = Varr + real_idx - 5.  Vidx and Vval are TAGGED.
        ;; The value MUST end up in one of EAX/ECX/EDX/EBX — in 32-bit mode
        ;; only those four have an addressable low byte (reg fields 4..7 mean
        ;; AH/CH/DH/BH, NOT SIL/DIL), so ECX is used and `mov [eax-5], cl`
        ;; is emitted.  All three operands are loaded before EAX is clobbered.
        ((op= +op-u8-set+)
         (let ((varr (first operands))
               (vidx (second operands))
               (vval (third operands)))
           ;; VR-PRESERVING: value via one stack slot; stored from DL (EDX is
           ;; low-byte addressable, unlike ESI/EDI in 32-bit mode).
           (i386-load-vreg buf +scratch0+ vval)
           (i386-emit-sar-reg-imm buf +scratch0+ 1)         ; untag value
           (i386-emit-push-reg buf +scratch0+)
           (i386-load-vreg buf +scratch0+ vidx)
           (i386-emit-sar-reg-imm buf +scratch0+ 1)         ; real_idx
           (i386-load-vreg buf +scratch1+ varr)
           (i386-emit-add-reg-reg buf +scratch0+ +scratch1+); ECX = addr+5
           (i386-emit-pop-reg buf +scratch1+)               ; EDX = value
           (i386-emit-mov-mem8-reg buf +scratch0+ -5 +scratch1+)))

        ;; ---- ADDS / SUBS / BVS ----  (explicit overflow-flag arithmetic)
        ;; Same code as :add/:sub — the i386 MOV that stores the result does
        ;; not disturb EFLAGS, so a following :bvs still sees OF.
        ((op= +op-adds+)
         (let ((vd (first operands)) (va (second operands)) (vb (third operands)))
           ;; VR-PRESERVING (WS5): compute in the ECX/EDX scratch pair, not
           ;; EAX — EAX is VR and may hold a live value the compiler reads
           ;; again after this op (see the :or type-check note above).
           (i386-load-vreg buf +scratch1+ vb)
           (i386-load-vreg buf +scratch0+ va)
           (i386-emit-add-reg-reg buf +scratch0+ +scratch1+)
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-subs+)
         (let ((vd (first operands)) (va (second operands)) (vb (third operands)))
           ;; VR-PRESERVING (WS5): compute in the ECX/EDX scratch pair, not
           ;; EAX — EAX is VR and may hold a live value the compiler reads
           ;; again after this op (see the :or type-check note above).
           (i386-load-vreg buf +scratch1+ vb)
           (i386-load-vreg buf +scratch0+ va)
           (i386-emit-sub-reg-reg buf +scratch0+ +scratch1+)
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-bvs+)
         (let* ((off (first operands))
                (target-pos (+ mvm-next-pos off))
                (label (i386-ensure-label-at state target-pos)))
           (i386-emit-jcc buf :o label)))

        ;; ---- ADD-CHECKED / SUB-CHECKED / MUL-CHECKED ----
        ;; Tagged arithmetic with bignum overflow promotion.  This matters far
        ;; MORE on i386 than on the 64-bit targets: a fixnum here is ~30 bits,
        ;; so ordinary program values overflow routinely.
        ;;
        ;; The result is computed in EDX — never EAX — so that va/vb stay
        ;; readable for the slow path even when one of them is VR (EAX).  That
        ;; is the same reason translate-x64 sums into R13.
        ;;
        ;; Slow path calls GENERIC-ADD/-SUBTRACT/-MULTIPLY with the i386 call
        ;; convention: V0=ESI, V1=EDI, V2/V3 pushed by the caller.  ESI/EDI are
        ;; saved around the call because they hold live V0/V1; the callee
        ;; preserves EBX/ESI/EDI itself, and EAX/ECX/EDX are ours to clobber.
        ((or (op= +op-add-checked+) (op= +op-sub-checked+) (op= +op-mul-checked+))
         (let* ((vd (first operands)) (va (second operands)) (vb (third operands))
                (kind (cond ((op= +op-add-checked+) :add)
                            ((op= +op-sub-checked+) :sub)
                            (t :mul)))
                (gen-label (and *i386-checked-arith-slowpath*
                                (ecase kind
                                  (:add *i386-genadd-label*)
                                  (:sub *i386-gensub-label*)
                                  (:mul *i386-genmul-label*)))))
           ;; ---- fast path (result in EDX) ----
           (i386-load-vreg buf +scratch0+ vb)               ; ECX = vb
           (i386-load-vreg buf +scratch1+ va)               ; EDX = va
           (ecase kind
             (:add (i386-emit-add-reg-reg buf +scratch1+ +scratch0+))
             (:sub (i386-emit-sub-reg-reg buf +scratch1+ +scratch0+))
             ;; (a>>1) * (b<<1) = (a*b)<<1; IMUL sets OF exactly when the
             ;; tagged product leaves the 32-bit signed range.
             (:mul (i386-emit-sar-reg-imm buf +scratch1+ 1)
                   (i386-emit-imul-reg-reg buf +scratch1+ +scratch0+)))
           (if (null gen-label)
               ;; No generic-arith entry in this module (or single-instruction
               ;; translation): degrade to plain wrapping arithmetic, exactly
               ;; like translate-x64's fallback branch.
               (i386-store-vreg buf vd +scratch1+)
               (let ((done (i386-make-label)))
                 (i386-emit-jcc buf :no done)
                 ;; ---- overflow slow path ----
                 ;; va/vb are still intact: the fast path touched only ECX/EDX.
                 (i386-load-vreg buf +scratch0+ va)         ; ECX = va
                 (i386-load-vreg buf +scratch1+ vb)         ; EDX = vb
                 (i386-emit-push-reg buf +i386-esi+)        ; save V0
                 (i386-emit-push-reg buf +i386-edi+)        ; save V1
                 (i386-emit-mov-reg-reg buf +i386-esi+ +scratch0+)  ; arg0
                 (i386-emit-mov-reg-reg buf +i386-edi+ +scratch1+)  ; arg1
                 ;; nargs = 2 (raw), for a callee with &optional/&rest.
                 (i386-emit-byte buf #xC7)
                 (i386-emit-byte buf (i386-modrm #b00 0 5))
                 (i386-emit-u32 buf *nargs-addr*)
                 (i386-emit-u32 buf 2)
                 ;; V2/V3 are the caller's, still in this frame's slots.
                 (i386-emit-push-mem buf +i386-ebp+ -20)    ; V3
                 (i386-emit-push-mem buf +i386-ebp+ -16)    ; V2
                 (i386-emit-call-rel32 buf gen-label)
                 (i386-emit-add-reg-imm buf +i386-esp+ 8)
                 (i386-emit-mov-reg-reg buf +scratch1+ +i386-eax+)  ; result
                 (i386-emit-pop-reg buf +i386-edi+)
                 (i386-emit-pop-reg buf +i386-esi+)
                 (i386-emit-label buf done)
                 (i386-store-vreg buf vd +scratch1+)))))

        ;; ================================================================
        ;; IEEE 64-bit float arithmetic — SSE2 lowering (#200/#201).
        ;;
        ;; Layout, register-pressure argument and the SSE2-vs-x87 decision are
        ;; all documented at I386-EMIT-FLOAT-BITS-TO-STACK above; read that
        ;; before changing anything here.
        ;;
        ;; Per-op shape (compare translate-x64's, which is the same modulo the
        ;; stack detour that replaces its MOVQ through a 64-bit GPR):
        ;;   1. Load an operand pointer into ECX, expand its four chunks into
        ;;      an 8-byte little-endian double at [ESP], MOVSD it into an XMM,
        ;;      release the 8 bytes.  Done for Vb then Va, so only ONE operand
        ;;      pointer is ever live — no push/pop pairing needed.
        ;;   2. ADDSD / SUBSD / MULSD / DIVSD xmm0, xmm1.
        ;;   3. Allocate a fresh 4-slot float, MOVSD the result back out to
        ;;      [ESP], split it into four tagged 16-bit chunks, tag the base.
        ;;
        ;; XMM0/XMM1 are caller-saved and modus uses no XMM register for
        ;; anything else, so they can be clobbered freely.  ESP is restored
        ;; before every vreg access; spilled vregs are addressed off EBP
        ;; anyway, so the temporary ESP dips cannot disturb them.
        ;; ================================================================
        ((or (op= +op-fadd+) (op= +op-fsub+) (op= +op-fmul+) (op= +op-fdiv+))
         (let* ((vd (first operands))
                (va (second operands))
                (vb (third operands))
                (sse-op (cond ((op= +op-fadd+) #x58)     ; ADDSD
                              ((op= +op-fsub+) #x5C)     ; SUBSD
                              ((op= +op-fmul+) #x59)     ; MULSD
                              (t               #x5E))))  ; DIVSD
           ;; Vb -> xmm1
           (i386-load-vreg buf +scratch0+ vb)
           (i386-emit-float-bits-to-stack buf +scratch0+ +scratch1+)
           (i386-emit-movsd-xmm-esp buf +i386-xmm1+)
           (i386-emit-add-reg-imm buf +i386-esp+ 8)
           ;; Va -> xmm0
           (i386-load-vreg buf +scratch0+ va)
           (i386-emit-float-bits-to-stack buf +scratch0+ +scratch1+)
           (i386-emit-movsd-xmm-esp buf +i386-xmm0+)
           (i386-emit-add-reg-imm buf +i386-esp+ 8)
           ;; xmm0 = xmm0 <op> xmm1
           (i386-emit-sse-arith buf sse-op +i386-xmm0+ +i386-xmm1+)
           ;; Allocate, write the payload back, tag.
           (i386-emit-float-alloc buf +scratch1+ +scratch0+)
           (i386-emit-sub-reg-imm buf +i386-esp+ 8)
           (i386-emit-movsd-esp-xmm buf +i386-xmm0+)
           (i386-emit-float-bits-from-stack buf +scratch1+ +scratch0+)
           (i386-emit-add-reg-imm buf +i386-esp+ 8)
           (i386-emit-or-reg-imm buf +scratch1+ +tag-object+)
           (i386-store-vreg buf vd +scratch1+)))

        ((op= +op-itof+)
         ;; (itof Vd Vs) — tagged integer -> freshly allocated float object.
         (let ((vd (first operands)) (vs (second operands)))
           (i386-load-vreg buf +scratch0+ vs)
           (i386-emit-sar-reg-imm buf +scratch0+ 1)      ; untag (signed)
           (i386-emit-cvtsi2sd buf +i386-xmm0+ +scratch0+)
           (i386-emit-float-alloc buf +scratch1+ +scratch0+)
           (i386-emit-sub-reg-imm buf +i386-esp+ 8)
           (i386-emit-movsd-esp-xmm buf +i386-xmm0+)
           (i386-emit-float-bits-from-stack buf +scratch1+ +scratch0+)
           (i386-emit-add-reg-imm buf +i386-esp+ 8)
           (i386-emit-or-reg-imm buf +scratch1+ +tag-object+)
           (i386-store-vreg buf vd +scratch1+)))

        ((op= +op-ftoi+)
         ;; (ftoi Vd Vs) — float -> tagged integer, truncating toward zero.
         ;; Allocates nothing, so compiler.lisp correctly emits no :gc-check.
         ;;
         ;; KNOWN GAP, shared with x64 and inherited deliberately rather than
         ;; papered over: CVTTSD2SI returns the integer-indefinite value when
         ;; the double is out of range, and the following `shl 1' turns that
         ;; into 0 — a silent wrong answer instead of a bignum or an error.
         ;; i386 hits it earlier than x64 (fixnums are 31-bit here, 63-bit
         ;; there), so |x| >= 2^30 truncates wrong on i386 while x64 still
         ;; answers correctly.  Fixing it is one change in both translators.
         (let ((vd (first operands)) (vs (second operands)))
           (i386-load-vreg buf +scratch0+ vs)
           (i386-emit-float-bits-to-stack buf +scratch0+ +scratch1+)
           (i386-emit-movsd-xmm-esp buf +i386-xmm0+)
           (i386-emit-add-reg-imm buf +i386-esp+ 8)
           (i386-emit-cvttsd2si buf +scratch0+ +i386-xmm0+)
           (i386-emit-shl-reg-imm buf +scratch0+ 1)      ; tag as fixnum
           (i386-store-vreg buf vd +scratch0+)))

        ((op= +op-fcmp+)
         ;; (fcmp Va Vb) — UCOMISD, leaving x86 flags for the following
         ;; :beq/:blt/:bgt.  Identical flag semantics to x64's arm.
         (let ((va (first operands)) (vb (second operands)))
           (i386-load-vreg buf +scratch0+ vb)
           (i386-emit-float-bits-to-stack buf +scratch0+ +scratch1+)
           (i386-emit-movsd-xmm-esp buf +i386-xmm1+)
           (i386-emit-add-reg-imm buf +i386-esp+ 8)
           (i386-load-vreg buf +scratch0+ va)
           (i386-emit-float-bits-to-stack buf +scratch0+ +scratch1+)
           (i386-emit-movsd-xmm-esp buf +i386-xmm0+)
           (i386-emit-add-reg-imm buf +i386-esp+ 8)
           (i386-emit-ucomisd buf +i386-xmm0+ +i386-xmm1+)))

        ;; ============================================
        ;; Unknown Opcode
        ;; ============================================
        (t
         ;; Emit trap for unrecognised MVM instructions.
         ;; WS5: also RECORD it.  A silent INT3 is the single most expensive
         ;; failure mode when bringing a new target up — the image builds
         ;; clean and then dies at an address with no explanation.  The
         ;; counter table lets a build script print exactly which opcodes are
         ;; still missing before anything is ever run.
         (when *i386-record-unimpl*
           (let ((tbl (or *i386-unimpl-ops*
                          (setf *i386-unimpl-ops* (make-hash-table :test 'eql)))))
             (incf (gethash opcode tbl 0))))
         (i386-emit-int3 buf))))))

(defun i386-unimplemented-report ()
  "Return an alist of (opcode-number . emit-count) for every MVM opcode that
   fell through to the INT3 default during the last translation, sorted by
   count.  See *i386-record-unimpl*."
  (let ((acc nil))
    (when *i386-unimpl-ops*
      (maphash (lambda (k v) (push (cons k v) acc)) *i386-unimpl-ops*))
    (sort acc #'> :key #'cdr)))

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
  ;; Reset the per-translation LI-CONST patch list (see
  ;; *i386-li-const-patches* / cross.lisp's apply-li-const-patches).
  (setf *i386-li-const-patches* nil)
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
    ;; Resolve the generic-arithmetic entries used by the overflow slow paths
    ;; of :add/:sub/:mul-checked (mirrors translate-mvm-to-x64).  Absent from
    ;; the module ⇒ NIL ⇒ those opcodes degrade to wrapping arithmetic.
    (setf *i386-genadd-label*
          (loop for entry in function-table
                when (string-equal (first entry) "GENERIC-ADD")
                  return (gethash (second entry) fn-offset-to-label)))
    (setf *i386-gensub-label*
          (loop for entry in function-table
                when (string-equal (first entry) "GENERIC-SUBTRACT")
                  return (gethash (second entry) fn-offset-to-label)))
    (setf *i386-genmul-label*
          (loop for entry in function-table
                when (string-equal (first entry) "GENERIC-MULTIPLY")
                  return (gethash (second entry) fn-offset-to-label)))
    ;; WS5: the label :gc-check CALLs is the NATIVE collector, emitted after
    ;; the last function (see I386-EMIT-GC-TRAMPOLINE).  It needs nothing from
    ;; the module — no %GC-COLLECT bytecode offset, no Lisp-side entry — so
    ;; gate on the enable flag alone.  With the collector off, no label is
    ;; minted, :gc-check keeps its historical `int $0x31`, and the image is
    ;; byte-identical to one built before this collector existed.
    ;; *i386-linux-mode* is part of the gate on purpose: the collector depends
    ;; on boot-side initialisation — GC metadata at 0x10000040.., the two
    ;; mmap'd bitmaps, and a stack base below 2^30 — that ONLY
    ;; boot/boot-linux-i386.lisp performs.  A bare-metal i386 image has none of
    ;; it (its global slot block is at #x600 and those words are zero), so
    ;; enabling the collector there would call it with a null bitmap base and a
    ;; garbage from-space.  Bare-metal i386 therefore keeps `int $0x31` and is
    ;; byte-identical to a pre-collector build; wiring it up is a separate job
    ;; that starts with the boot-side init.
    (setf *i386-gc-collect-label*
          (and *i386-gc-enabled* *i386-linux-mode* (i386-make-label)))
    ;; handler-case helper entry points (TRAP #x0510/#x0511/#x0512).  Hosted
    ;; Linux only: the jmp_buf and handler stack live in the hosted BSS, which
    ;; a bare-metal i386 board does not have.  With these NIL the traps fall
    ;; through to the unimplemented reporter exactly as before, so every
    ;; bare-metal i386 image stays byte-identical.
    (setf *i386-handler-push-label*
          (and *i386-linux-mode* (i386-make-label)))
    (setf *i386-handler-pop-label*
          (and *i386-linux-mode* (i386-make-label)))
    ;; Translate each function
    (loop for i from 0 below n-functions
          for entry in function-table
          for fn-offset = (second entry)
          for fn-length = (third entry)
          do (progn
               ;; Function-entry alignment.  With *i386-fn-tag-3* on, :fn-addr
               ;; OR-3s the entry address, so the low nibble must be 0 at the
               ;; RUNTIME virtual address — hence padding is computed against
               ;; *i386-native-code-offset* + buffer position, not the buffer
               ;; position alone.
               (when *i386-fn-align*
                 (loop until (zerop (mod (+ *i386-native-code-offset*
                                            (i386-current-pos buf))
                                         *i386-fn-align*))
                       do (i386-emit-nop buf))))
          do (let* ((fn-label (aref fn-labels i))
                    (state (make-i386-translate-state
                            :buf buf
                            :mvm-bytes bytecode
                            :mvm-length fn-length
                            :mvm-offset fn-offset
                            :gc-label *i386-gc-collect-label*
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
    ;; The GC trampoline, emitted once after the last function.
    (when *i386-gc-collect-label*
      (when *i386-fn-align*
        (loop until (zerop (mod (+ *i386-native-code-offset* (i386-current-pos buf))
                                *i386-fn-align*))
              do (i386-emit-nop buf)))
      ;; The checker polices OPCODE codegen (EAX doubles as VR there).  Inside
      ;; the collector every mutator register is already saved by PUSHAD, so
      ;; EAX is an ordinary temp; silence the checker for this block.
      (let ((*i386-check-eax-invariant* nil))
        (i386-emit-gc-trampoline buf *i386-gc-collect-label* nil)))
    ;; The handler-case push/pop helpers, likewise emitted once after the last
    ;; function.  They use EAX as a copy temp but SAVE AND RESTORE it, which
    ;; the per-opcode checker cannot see; silence it for this block.
    (when *i386-handler-push-label*
      (when *i386-fn-align*
        (loop until (zerop (mod (+ *i386-native-code-offset* (i386-current-pos buf))
                                *i386-fn-align*))
              do (i386-emit-nop buf)))
      (let ((*i386-check-eax-invariant* nil))
        (i386-emit-handler-helpers buf *i386-handler-push-label*
                                   *i386-handler-pop-label*)))
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
