;;;; boot-rpi-cl.lisp — Raspberry Pi 3B (BCM2837, AArch64) boot sequence for
;;;; the REAL CL / MVM stack.
;;;;
;;;; Task #209 rung 1.  boot/boot-rpi.lisp is the OLD `repl-source' lineage
;;;; boot: it publishes a flat bump allocator (x24=0x04000000, x25=0x05000000),
;;;; NIL=0, and a 2 MB stack top — none of which the CL/mvm stack can use.
;;;; This file is its CL-lineage sibling.  It does NOT modify boot-rpi.lisp;
;;;; it defines a new entry emitter and REDEFINES `rpi-boot-descriptor' in the
;;;; building image only (each build script is its own SBCL process, so the
;;;; legacy `:target :rpi' builds are unaffected).
;;;;
;;;; WHY NOT REUSE THE QEMU-virt FIXPOINT BOOT (boot-aarch64.lisp)?
;;;; The fixpoint entry's whole purpose is an MMU that remaps the x64-shaped
;;;; runtime VAs onto QEMU virt's DRAM, which starts at PA 0x40000000:
;;;; VA 0x00000000-0x1FFFFFFF -> PA +0x40000000, with VA 0x10000000 -> PA
;;;; 0x50000000 via an L2[128] override.  On a Pi 3B there IS no PA
;;;; 0x40000000 — DRAM is 0x00000000-0x3EFFFFFF and the BCM2837 peripheral
;;;; window sits at 0x3F000000-0x3FFFFFFF.  The remap is not just unnecessary
;;;; here, it is unbackable.
;;;;
;;;; The happy accident: EVERY address the CL/mvm runtime wants is already
;;;; real, plain DRAM on a Pi 3B, so the correct Pi mapping is the IDENTITY
;;;; and no page tables are needed at all.
;;;;
;;;;   0x00080000  kernel8.img load address (GPU firmware / QEMU -kernel)
;;;;   0x00080800  exception vectors (VBAR_EL1)
;;;;   0x00081000  native code (kernel-main)
;;;;   ~0x0170xxxx image end (a ~23 MB CL image)
;;;;   0x08000000  stack top, grows DOWN (128 MB; ~105 MB clear of the image)
;;;;   0x09000000  Cheney heap base   = x24 (alloc pointer)
;;;;   0x0C800000  semispace midpoint = x25 (alloc limit)
;;;;   0x10000000  heap end / runtime metadata ("BSS") base
;;;;   0x10080000  per-CPU (TPIDR_EL1)
;;;;   0x3F000000  BCM2837 peripherals (PL011 UART0 at 0x3F201000)
;;;;   0x3F000000  <- everything above must stay BELOW this
;;;;
;;;; That is byte-for-byte the SAME VA layout the QEMU-virt fixpoint image
;;;; uses, which is what makes this a port rather than a redesign: the
;;;; compiler, the interpreter, gc.lisp's metadata block and every hard-coded
;;;; 0x1000xxxx runtime slot are unchanged.  Only the way those VAs come to
;;;; exist differs (identity + MMU-off here, page tables there).
;;;;
;;;; MMU IS OFF.  With SCTLR_EL1.M clear every access is Device-nGnRnE:
;;;; correct but uncached.  Under QEMU/TCG that costs nothing measurable (TCG
;;;; models no caches).  On real silicon it would be brutally slow and is the
;;;; first thing to fix in rung 2 — an identity 2-level table with
;;;; Normal-WB for 0x00000000-0x3EFFFFFF and Device for 0x3F000000+.  Two
;;;; further consequences worth knowing: unaligned accesses fault (Modus is
;;;; naturally aligned, and the legacy `:rpi' images have run MMU-off on this
;;;; board for a long time), and load/store-exclusive is UNPREDICTABLE on
;;;; Device memory (no actors here, so *aarch64-sched-lock-addr* is NIL and
;;;; the translator emits none).
;;;;
;;;; SERIAL IS PL011 (UART0, 0x3F201000), NOT the mini UART.  This is forced,
;;;; not preferred: `read-char-serial' (TRAP #x0301) is hardcoded to the PL011
;;;; register layout in translate-aarch64.lisp (~1843) — it polls UARTFR at
;;;; offset 0x18 for RXFE bit 4.  TX is parameterized (*aarch64-serial-width*
;;;; / *aarch64-serial-tx-poll*) so the mini UART can transmit, but there is
;;;; no *aarch64-serial-rx-poll*, so the mini UART cannot RECEIVE and a REPL
;;;; needs input.  Teaching the translator an RX-poll parameter is rung-2 work
;;;; and matters for the Pi Zero 2 W, whose console IS the mini UART.
;;;; Under QEMU raspi3b the PL011 is serial_hd(0): run with
;;;;   -serial stdio -serial null
;;;; (the legacy mini-UART images use the reverse, -serial null -serial stdio).

(in-package :modus.mvm)

;;; ============================================================
;;; RPi CL-lineage boot constants
;;; ============================================================

;; The GPU (and QEMU's raspi3b -kernel) place kernel8.img at PA 0x80000.
;; cross.lisp's aarch64 bare-metal path ADDS 0x80000 to the descriptor's
;; :load-addr when it computes fn-address constants, code bounds and
;; :li-const patches (apply-aarch64-fn-addr-patches, ~cross.lisp:560), so the
;; descriptor must declare 0 to land on 0x80000.
;;
;; boot-rpi.lisp declares :load-addr +rpi-kernel-base+ = 0x00080000 and so
;; resolves every such constant to 0x100000 — 512 KB past where the image
;; actually is.  That is latent there only because mvm/repl-source.lisp
;; contains no #' forms, so no :li-func patch site is ever emitted; the
;; hardcoded VBAR of 0x00080800 in emit-rpi-entry is the tell that the real
;; base is 0x80000.  A CL-stack image emits those patches constantly.
;; CHAIN-LOAD SUPPORT.  The UART chain loader (net/uart-bootloader.lisp,
;; `bootloader-load-addr' = #x300000) receives an image over the mini UART and
;; JUMPS TO 0x300000 — it cannot use 0x80000, because that is where the chain
;; loader itself is executing from.  So an image intended to be chain-loaded
;; must be built to RUN at 0x300000 instead of the GPU's 0x80000.
;;
;; Remember cross.lisp adds 0x80000 (see the note above), so:
;;     effective run address = declared :load-addr + 0x80000
;;     0x80000  (SD card, GPU-loaded)  <- declare 0
;;     0x300000 (chain-loaded)         <- declare 0x280000
;;
;; MODUS_RPI_CHAINLOAD=1 selects the chain-load layout.  Everything else —
;; stack top, heap, peripherals — is unchanged; only the image base and its
;; vector-table address move, and the 20 MB image at 0x300000 still ends far
;; below the 0x08000000 stack top.
(defvar *rpi-cl-chainload*
  (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_RPI_CHAINLOAD")))
    (and v (plusp (length v)) (not (string= v "0"))))
  "T when building an image to be delivered by the UART chain loader.")

(defconstant +rpi-cl-gpu-load-addr+   #x00000000) ; -> runs at 0x80000
(defconstant +rpi-cl-chain-load-addr+ #x00280000) ; -> runs at 0x300000

(defvar *rpi-cl-load-addr*
  (if *rpi-cl-chainload* +rpi-cl-chain-load-addr+ +rpi-cl-gpu-load-addr+))

;; Kept for source compatibility; the GPU-loaded value.
(defconstant +rpi-cl-load-addr+     #x00000000)

(defconstant +rpi-cl-pl011-base+    #x3F201000)  ; BCM2837 UART0 (PL011)
;; VBAR must track the image base: run address + 0x800.
(defvar *rpi-cl-vbar*
  (+ *rpi-cl-load-addr* #x80000 #x800)
  "Vector table VA = actual run address + 0x800.")
(defconstant +rpi-cl-vbar+          #x00080800)  ; GPU-loaded value
(defconstant +rpi-cl-native-off+    #x00001000)  ; native code at image + 0x1000

(defconstant +rpi-cl-stack-top+     #x08000000)  ; grows down
(defconstant +rpi-cl-heap-base+     #x09000000)  ; x24 — Cheney from-space
(defconstant +rpi-cl-heap-mid+      #x0C800000)  ; x25 — semispace midpoint
(defconstant +rpi-cl-heap-end+      #x10000000)  ; 112 MB total
(defconstant +rpi-cl-percpu+        #x10080000)  ; TPIDR_EL1

;; NIL register (x26).  The modern compiler bakes +nil-value+ = #xDEAD0001
;; into compiled literals and interp.lisp keys truthiness on that exact bit
;; pattern; boot-rpi.lisp's hardcoded x26 = 0 splits the NIL representation
;; and breaks mvm-eval.  Same knob and same value as the QEMU-virt bare image
;; (*aarch64-fixpoint-nil-value*, set to #xDEAD0001 by build-aarch64.lisp).
(defvar *rpi-cl-nil-value* #xDEAD0001)

;;; ============================================================
;;; Boot code
;;; ============================================================
;;; Image layout:  boot 0x000 | vectors 0x800 | native 0x1000

(defun emit-rpi-cl-exception-vectors (buf)
  "16 entries x 32 instructions = 2 KB at image offset 0x800.

   All entries are `B .' (spin).  The QEMU-virt bare image gives entry 4 a
   sync-exception -> handler-case longjmp (its SIGSEGV-recovery equivalent)
   and entry 5 a vtimer deadline IRQ; neither is ported yet.  Consequence,
   stated plainly: a HARDWARE fault on the Pi wedges the machine instead of
   surfacing as a recovered Lisp error.  Ordinary Lisp errors are signalled
   in software and unwind through handler-case normally, so E2SMOKE and the
   REPL's own error recovery are unaffected."
  (dotimes (entry 16)
    (declare (ignorable entry))
    (emit-aarch64-u32 buf #x14000000)                      ; B .
    (dotimes (i 31) (emit-aarch64-u32 buf #xD503201F))))   ; NOP x31

(defun emit-rpi-cl-entry (buf)
  "Emit the Pi 3B CL-lineage boot preamble (MMU off, identity addressing)."
  (let ((sp 31) (x0 0) (x16 16) (x17 17)
        (x24 24) (x25 25) (x26 26))

    ;; --- 1. Stack pointer -------------------------------------------------
    ;; 0x08000000, not boot-rpi.lisp's 0x00200000.  A CL image is tens of MB
    ;; and loads at 0x80000, so a 2 MB stack top sits INSIDE the image and
    ;; every push shreds native code.  This is exactly the failure the
    ;; QEMU-virt boot documents at boot-aarch64.lisp:764-786 (task #47).
    (emit-aarch64-load-imm64 buf x16 +rpi-cl-stack-top+)
    (emit-aarch64-mov-sp buf sp x16)

    ;; --- 2. PL011 UART0 init (same sequence as emit-rpi-entry) ------------
    ;;   CR = 0 | IBRD = 26 | FBRD = 3 | LCRH = 0x70 (8N1, FIFO) | CR = 0x301
    (emit-aarch64-load-imm64 buf x17 +rpi-cl-pl011-base+)
    (emit-aarch64-movz buf x0 0 0)
    (emit-aarch64-str-w buf x0 x17 12)      ; +0x30 UARTCR = 0
    (emit-aarch64-movz buf x0 26 0)
    (emit-aarch64-str-w buf x0 x17 9)       ; +0x24 UARTIBRD
    (emit-aarch64-movz buf x0 3 0)
    (emit-aarch64-str-w buf x0 x17 10)      ; +0x28 UARTFBRD
    (emit-aarch64-movz buf x0 #x70 0)
    (emit-aarch64-str-w buf x0 x17 11)      ; +0x2C UARTLCRH
    (emit-aarch64-movz buf x0 #x0301 0)
    (emit-aarch64-str-w buf x0 x17 12)      ; +0x30 UARTCR = enable|TX|RX

    ;; --- 3. GC allocation registers --------------------------------------
    ;; RAW byte addresses (see the RAW-ADDR-AUDIT note in boot-aarch64.lisp):
    ;; compiled code bumps x24 per allocation and compares against x25; when
    ;; they meet, +op-gc-check+ BLs the GC trampoline, which reloads both from
    ;; the metadata kernel-main's (%gc-init ...) publishes.
    (emit-aarch64-load-imm64 buf x24 +rpi-cl-heap-base+)
    (emit-aarch64-load-imm64 buf x25 +rpi-cl-heap-mid+)
    (emit-aarch64-load-imm64 buf x26 *rpi-cl-nil-value*)

    ;; --- 4. TPIDR_EL1 = per-CPU base -------------------------------------
    (emit-aarch64-load-imm64 buf x16 +rpi-cl-percpu+)
    (emit-aarch64-u32 buf #xD518D090)       ; MSR TPIDR_EL1, X16

    ;; --- 5. VBAR_EL1 ------------------------------------------------------
    (emit-aarch64-load-imm64 buf x16 *rpi-cl-vbar*)
    (emit-aarch64-u32 buf #xD518C010)       ; MSR VBAR_EL1, X16
    (emit-aarch64-u32 buf #xD5033FDF)       ; ISB SY

    ;; --- 6. code_base / code_end for FUNCTIONP ---------------------------
    ;; Leaves MOVZ/MOVK placeholders that cross.lisp fills in post-link
    ;; (apply-aarch64-code-bounds-patches).  Without it both slots read 0,
    ;; functionp short-circuits past its in-code-range arm and can
    ;; misclassify a raw fn-addr; boot-rpi.lisp emits no such block.
    (emit-aarch64-code-bounds-init buf)

    ;; --- 7. Branch to native code, then pad out to the vector table ------
    ;; a64-buffer-position counts 32-bit INSTRUCTIONS (unified buffer since
    ;; 8048454), so no /4 on the position — only on the byte target.
    (let* ((cur (a64-buffer-position buf))
           (skip (- (/ +rpi-cl-native-off+ 4) cur)))
      (when (minusp skip)
        (error "boot-rpi-cl: preamble overflowed its ~D-instruction budget ~
                (position ~D, native code starts at instruction ~D)"
               (/ #x800 4) cur (/ +rpi-cl-native-off+ 4)))
      (emit-aarch64-u32 buf (logior (ash #b000101 26) (logand skip #x3FFFFFF)))
      (let ((pad (- (/ #x800 4) (a64-buffer-position buf))))
        (when (minusp pad)
          (error "boot-rpi-cl: preamble ran into the exception vectors at 0x800"))
        (dotimes (i pad) (emit-aarch64-u32 buf #xD503201F))))

    ;; --- 8. Exception vectors at 0x800, native code follows at 0x1000 ----
    (emit-rpi-cl-exception-vectors buf)))

;;; ============================================================
;;; Descriptor
;;; ============================================================

(defun rpi-cl-boot-descriptor ()
  "Boot descriptor for a Pi 3B image running the real CL / MVM stack."
  (list :arch :aarch64
        :entry-fn #'emit-rpi-cl-entry
        :load-addr *rpi-cl-load-addr*
        :stack-top +rpi-cl-stack-top+
        :cons-base +rpi-cl-heap-base+
        :general-base (+ +rpi-cl-heap-mid+ #x01000000)
        :serial-base +rpi-cl-pl011-base+))

;; HOOK.  build-image resolves the descriptor through cross.lisp's
;; get-boot-descriptor, whose `:rpi' arm calls `rpi-boot-descriptor'.
;; Redefining that here — rather than adding a `:rpi-cl' target keyword to
;; cross.lisp — keeps this port to two NEW files and touches no shared source,
;; so the x64 and aarch64 CLI images stay byte-identical by construction.
;; Only a build script that explicitly loads THIS file is affected.
(defun rpi-boot-descriptor ()
  "REDEFINED by boot-rpi-cl.lisp: the CL-lineage Pi 3B descriptor."
  (rpi-cl-boot-descriptor))
