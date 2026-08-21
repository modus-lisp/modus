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

;; #271: where the boot preamble parks the firmware's device-tree pointer.
;;
;; The AArch64 Linux boot protocol hands the kernel the PHYSICAL ADDRESS of a
;; Flat Device Tree in X0, and the Pi firmware puts cmdline.txt into that tree
;; at /chosen/bootargs (QEMU's -append does the same).  X0 is scratch to
;; everything that follows — step 2 of the preamble writes it to the UART a few
;; instructions later — so it has to be saved FIRST or it is gone.
;;
;; WHY THIS ADDRESS IS FREE.  0x1000xxxx is the runtime metadata window (the
;; bare-metal stand-in for an ELF BSS).  Its assignments are dense but
;; enumerable: 0x…0040-0x…0060 GC metadata; 0x…0080/0088/0148/0170 the four
;; GC-scanned root tables; 0x…0090-0x…0138 the multiple-value block, which
;; mvm/interp.lisp routes u64 traffic into by ADDRESS RANGE, so nothing
;; unrelated may live there; 0x…0150-0x…01F0 nargs, code bounds and handler
;; slots; 0x…0280-0x…0408 the longjmp / handler-stack apparatus; 0x…0C08-
;; 0x…0DA0 fault-diagnostic and safepoint slots; 0x…0E00-0x…0EA8 the MCGC and
;; bitmap config block (this image uses E00/E18/E40 of it).  A repo-wide sweep
;; of every #x1000xxxx literal finds NOTHING between 0x10000EA8 and 0x10001000,
;; and the next occupied address anywhere above is 0x10010000, the AArch64
;; handler-stack depth.  0x10000F00 sits in that gap, 88 bytes clear of the
;; config block below it and 256 clear of anything above.
;;
;; DO NOT ADD IT TO THE BSS-ZEROING LIST.  build-rpi-cl-repl.lisp's kernel-main
;; prologue zeroes the metadata words that stand in for BSS.  This slot is
;; written BEFORE kernel-main runs, so zeroing it would erase the one thing it
;; exists to carry.  lib/fdt.lisp is the reader.
(defconstant +rpi-cl-dtb-ptr-slot+  #x10000F00)  ; firmware DTB pointer (X0)

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

;;; --- Sync-fault reporter -----------------------------------------------------
;;;
;;; WHY THIS EXISTS (task #263).  Every vector entry used to be `B .', so ANY
;;; hardware fault froze the board with ZERO output.  That is exactly what the
;;; alexandria wedge looked like for several sessions: 100% CPU, no bytes, and
;;; a very convincing story about "an infinite loop in the compiler".  It was
;;; an undefined-instruction fault (ESR EC=0) landing on this table.  Silence
;;; is the expensive part — a fault that PRINTS is a bug you fix in an hour.
;;;
;;; The QEMU-virt image's entry 4 recovers via a handler-case longjmp.  That is
;;; deliberately NOT what this does.  Recovery needs a trustworthy Lisp heap,
;;; and the faults seen here happen precisely when the heap or the code has
;;; already been damaged; resuming would launder a corrupt machine into a
;;; plausible-looking one.  This REPORTS and HALTS.
;;;
;;; Registers are read at EL2 (this image runs at EL2 — see the VBAR_EL1 *AND*
;;; VBAR_EL2 note in emit-rpi-cl-entry; the EL1 copies read 0).

(defun emit-rpi-cl-putc-poll (buf miniuart)
  "TX one byte from W0 through the console in X17.  3 instructions.
   Poll polarity differs per UART: PL011 FR bit5 = TX-FULL (wait WHILE set);
   mini-UART LSR bit5 = TX-EMPTY (wait UNTIL set)."
  (if miniuart
      (progn
        (emit-aarch64-u32 buf #xB9401629)   ; LDR  W9,[X17,#0x14]  AUX_MU_LSR
        (emit-aarch64-u32 buf #x362FFFE9))  ; TBZ  W9,#5,-1        wait til empty
      (progn
        (emit-aarch64-u32 buf #xB9401A29)   ; LDR  W9,[X17,#0x18]  UARTFR
        (emit-aarch64-u32 buf #x372FFFE9))) ; TBNZ W9,#5,-1        wait til !full
  (emit-aarch64-u32 buf #xB9000220))        ; STR  W0,[X17]

(defun emit-rpi-cl-putc-imm (buf ch miniuart)
  "Emit one literal character.  4 instructions."
  (emit-aarch64-u32 buf (logior #xD2800000 (ash (logand ch #xFFFF) 5) 0)) ; MOVZ X0,#ch
  (emit-rpi-cl-putc-poll buf miniuart))

(defun emit-rpi-cl-puthex64 (buf miniuart)
  "Print X10 as 16 hex digits, MSB first.  X11 = shift, X12 = 15 (preset by
   the caller).  12 instructions, no calls — a BL here would clobber the very
   X30 we may want to read."
  (emit-aarch64-u32 buf #xD280078B)         ; MOVZ X11,#60
  ;; L:
  (emit-aarch64-u32 buf #x9ACB2540)         ; LSRV X0,X10,X11
  (emit-aarch64-u32 buf #x8A0C0000)         ; AND  X0,X0,X12      (X12=15)
  (emit-aarch64-u32 buf #xF100241F)         ; CMP  X0,#9
  ;; B.HI must skip BOTH the digit ADD and the B that jumps over the letter
  ;; ADD, i.e. +3 — not +2.  At +2 it lands on the B itself, the letter branch
  ;; is unreachable, and every nibble above 9 prints as a raw control byte.
  ;; Caught by disassembling the built image; the encoding looked fine on paper.
  (emit-aarch64-u32 buf #x54000068)         ; B.HI +3             (>9 -> letter)
  (emit-aarch64-u32 buf #x9100C000)         ; ADD  X0,X0,#48      '0'
  (emit-aarch64-u32 buf #x14000002)         ; B    +2
  (emit-aarch64-u32 buf #x91015C00)         ; ADD  X0,X0,#87      'a'-10
  (emit-rpi-cl-putc-poll buf miniuart)      ; 3 instructions
  (emit-aarch64-u32 buf #xF100116B)         ; SUBS X11,X11,#4
  (emit-aarch64-u32 buf #x54FFFEAA))        ; B.GE -11            -> L

(defun emit-rpi-cl-sync-fault-handler (buf miniuart)
  "Body of the sync-exception reporter.  Lives in the entry-8..15 slots (lower-EL
   vectors, which this image can never take: it runs at EL2 with nothing below
   it).  Same trick the QEMU-virt image uses for its entry-7/8 continuations."
  ;; X17 = console data register.
  (emit-aarch64-load-imm64 buf 17 (if miniuart #x3F215040 +rpi-cl-pl011-base+))
  (emit-aarch64-u32 buf #xD28001EC)         ; MOVZ X12,#15   (nibble mask)
  (dolist (ch '(10 33 33 70 65 85 76 84 32)) ; "\n!!FAULT "
    (emit-rpi-cl-putc-imm buf ch miniuart))
  ;; ESR_EL2 — the fault class.  EC=0 means undefined instruction, which on
  ;; this platform means we executed data (see #263).
  (emit-rpi-cl-putc-imm buf 69 miniuart)    ; 'E'
  (emit-aarch64-u32 buf #xD53C520A)         ; MRS X10,ESR_EL2
  (emit-rpi-cl-puthex64 buf miniuart)
  (emit-rpi-cl-putc-imm buf 32 miniuart)
  ;; ELR_EL2 — the faulting PC.  Subtract the image base to index the symmap.
  (emit-rpi-cl-putc-imm buf 76 miniuart)    ; 'L'
  (emit-aarch64-u32 buf #xD53C402A)         ; MRS X10,ELR_EL2
  (emit-rpi-cl-puthex64 buf miniuart)
  (emit-rpi-cl-putc-imm buf 32 miniuart)
  ;; FAR_EL2 — the faulting data address (0 for an undefined instruction).
  (emit-rpi-cl-putc-imm buf 70 miniuart)    ; 'F'
  (emit-aarch64-u32 buf #xD53C600A)         ; MRS X10,FAR_EL2
  (emit-rpi-cl-puthex64 buf miniuart)
  (emit-rpi-cl-putc-imm buf 32 miniuart)
  ;; SP — the giveaway for the failure mode that started all this: if SP is
  ;; inside the image rather than below 0x08000000, the stack has run away and
  ;; is overwriting the kernel.
  (emit-rpi-cl-putc-imm buf 83 miniuart)    ; 'S'
  (emit-aarch64-u32 buf #x910003EA)         ; MOV X10,SP
  (emit-rpi-cl-puthex64 buf miniuart)
  (emit-rpi-cl-putc-imm buf 10 miniuart)    ; '\n'
  (emit-aarch64-u32 buf #x14000000))        ; B .  — halt, do NOT resume

(defun emit-rpi-cl-exception-vectors (buf &optional (miniuart nil))
  "16 entries x 32 instructions = 2 KB at image offset 0x800.

   Entry 4 (Current EL, SP_ELx, Sync) branches to a reporter that prints
   ESR/ELR/FAR/SP and halts; the reporter body occupies the entry-8..15 slots
   (lower-EL vectors, unreachable on this image).  Everything else is still
   `B .' — an IRQ or SError here has no meaning yet.

   BEFORE THIS, a hardware fault froze the board with no output at all, which
   is how #263 masqueraded as a compiler hang for several sessions."
  (dotimes (entry 8)
    (cond
      ((= entry 4)
       ;; B forward to the entry-8 slot: 4 entries x 32 instructions.
       (emit-aarch64-u32 buf (logior #x14000000 128))
       (dotimes (i 31) (emit-aarch64-u32 buf #xD503201F)))
      (t
       (emit-aarch64-u32 buf #x14000000)                    ; B .
       (dotimes (i 31) (emit-aarch64-u32 buf #xD503201F)))))
  ;; Entries 8..15 = 256 instruction slots: reporter + NOP padding.  The
  ;; assert is the point — silently overrunning into whatever follows the
  ;; vector table would be a far worse bug than the one being fixed.
  (let ((before (a64-buffer-position buf)))
    (emit-rpi-cl-sync-fault-handler buf miniuart)
    (let ((used (- (a64-buffer-position buf) before)))
      (when (> used 256)
        (error "rpi-cl sync-fault handler is ~D instructions; only 256 fit ~
                in the entry-8..15 slots" used))
      (dotimes (i (- 256 used)) (emit-aarch64-u32 buf #xD503201F)))))

(defun emit-rpi-cl-entry (buf)
  "Emit the Pi 3B CL-lineage boot preamble (MMU off, identity addressing)."
  (let ((sp 31) (x0 0) (x16 16) (x17 17)
        (x24 24) (x25 25) (x26 26))

    ;; --- 0. ZERO THE RUNTIME METADATA WINDOW (the BSS stand-in) -----------
    ;; On Linux the ELF BSS is zero-filled by the kernel.  On bare metal these
    ;; words hold whatever the firmware left in RAM, and the very first Lisp
    ;; function that reads a global dereferences the garbage at 0x10000080 as
    ;; the global-alist head.  QEMU HID this for the entire life of this image
    ;; because it zero-fills guest RAM; on a real Pi Zero 2 W the board printed
    ;; "BOOT" and then died before "MODUS-CL" with ESR 0x96000004 (data abort,
    ;; translation fault L0) on a garbage FAR — exactly the outcome section 5
    ;; predicts for a fault in Lisp init.
    ;;
    ;; This REPLACES a ~22-entry enumerated list of individual slots that used
    ;; to live in build-rpi-cl-repl.lisp's kernel-main prologue.  That list was
    ;; wrong twice over: it ran AFTER the MODUS-CL banner (so the banner itself
    ;; walked the garbage alist), and being an enumeration it could only ever
    ;; cover the slots someone remembered — a new metadata word would be
    ;; uninitialised on hardware and fine under emulation, i.e. invisible.
    ;;
    ;; Doing it HERE rather than in Lisp is what makes the bulk form legal:
    ;; the two words that must NOT be zeroed are written by this same preamble
    ;; AFTERWARDS — 0x10000F00 (the DTB pointer, step 0a below) and
    ;; 0x10000160/168 (code_base/code_end, step 6).  A Lisp-side bulk zero
    ;; would have to special-case both.
    ;;
    ;; Range 0x10000000..0x10001000 covers every documented slot (the highest
    ;; is the MCGC/bitmap config block ending at 0x10000EA8), plus the lone
    ;; AArch64 handler-stack depth word at 0x10010000.  X0 is untouched, so the
    ;; firmware DTB pointer survives into step 0a.
    (emit-aarch64-load-imm64 buf x16 #x10000000)
    (emit-aarch64-load-imm64 buf x17 #x10001000)
    (let ((loop-start (a64-current-index buf)))
      (a64-stur buf +a64-xzr+ x16 0)
      (a64-add-imm buf x16 x16 8)
      (a64-cmp-reg buf x16 x17)
      ;; B.LO (unsigned <) back to the store.
      (a64-bcond buf #b0011 (- loop-start (a64-current-index buf))))
    (emit-aarch64-load-imm64 buf x16 #x10010000)
    (a64-stur buf +a64-xzr+ x16 0)

    ;; --- 0a. FIRMWARE DEVICE-TREE POINTER (#271) --------------------------
    ;; MUST FOLLOW step 0, which zeroes the window this slot lives in, and
    ;; must precede step 2.  X0 holds the DTB physical address on entry per the
    ;; AArch64 Linux boot protocol, and step 2 below uses X0 as the UART data
    ;; scratch register.  Three instructions — MOVZ + MOVK to materialise the
    ;; slot address (0x10000F00 has exactly two non-zero halfwords, so
    ;; emit-aarch64-load-imm64 emits two), then one STR.  X16 is already the
    ;; preamble's scratch register and is reloaded by step 1 on the next
    ;; instruction, so nothing downstream can observe the borrow.
    ;;
    ;; The preamble is budgeted at 0x800 bytes (512 instructions) and step 7
    ;; pads whatever is left, so three more instructions shift NOTHING: native
    ;; code still begins at +rpi-cl-native-off+ = 0x1000 and every fn entry
    ;; keeps its 16-byte alignment.  (This is the AArch64 counterpart of the
    ;; x64 hazard where growing boot-linux-x64.lisp requires bumping
    ;; *x64-native-code-offset* — that image has no such padding step.)
    (emit-aarch64-load-imm64 buf x16 +rpi-cl-dtb-ptr-slot+)
    (emit-aarch64-str-x buf x0 x16 0)

    ;; --- 1. Stack pointer -------------------------------------------------
    ;; 0x08000000, not boot-rpi.lisp's 0x00200000.  A CL image is tens of MB
    ;; and loads at 0x80000, so a 2 MB stack top sits INSIDE the image and
    ;; every push shreds native code.  This is exactly the failure the
    ;; QEMU-virt boot documents at boot-aarch64.lisp:764-786 (task #47).
    (emit-aarch64-load-imm64 buf x16 +rpi-cl-stack-top+)
    (emit-aarch64-mov-sp buf sp x16)

    ;; --- 2. CONSOLE init --------------------------------------------------
    ;; WHICH UART depends on *AARCH64-SERIAL-BASE*, which the build script sets.
    ;; Getting this wrong is the worst failure on this board, for the reason
    ;; section 5 spells out: a wedged board and a dead serial link look
    ;; IDENTICAL.  Measured 2026-08-15 — pointing the translator's TRAP
    ;; #x0300/#x0301 emitters at the mini UART while this preamble still only
    ;; initialised the PL011 produced a totally silent board: the data path
    ;; wrote to AUX_MU_IO on a peripheral THIS CODE had never enabled.
    (if (= *aarch64-serial-base* #x3F215040)
        (progn
          ;; ---- BCM2835 mini UART (AUX), real Pi Zero 2 W ------------------
          ;; The Zero 2 W routes the PL011 to Bluetooth; the mini UART owns
          ;; GPIO14/15, so this is the ONLY console on the header pins.
          ;; AUX block base is 0x3F215000 = AUX_MU_IO (0x3F215040) - 0x40.
          ;; str-w offsets are SCALED BY 4, hence reg-offset/4 below.
          ;;   +0x04 AUX_ENABLES  1  = enable the mini UART
          ;;   +0x60 AUX_MU_CNTL  0  = TX/RX off while we configure
          ;;   +0x44 AUX_MU_IER   0  = no interrupts (we poll)
          ;;   +0x4C AUX_MU_LCR   3  = 8-bit  (bit1 is documented as reserved
          ;;                             but must be set for 8-bit operation)
          ;;   +0x50 AUX_MU_MCR   0  = RTS unused
          ;;   +0x68 AUX_MU_BAUD  270 = 115200 with core_freq=250 fixed in
          ;;                             config.txt: 250e6/(8*115200)-1 = 270.3
          ;;   +0x60 AUX_MU_CNTL  3  = TX+RX enable
          ;; GPIO14/15 muxing to ALT5 is left to the firmware, which does it
          ;; because config.txt carries enable_uart=1.
          ;; DO NOT REPROGRAM THE MINI UART.  The firmware has already enabled
          ;; it, muxed GPIO14/15 and set the divisor, because config.txt
          ;; carries enable_uart=1 — which is exactly why the UART chain
          ;; loader (boot-rpi.lisp) prints BOOT/RDY while doing NO AUX setup
          ;; whatsoever.  Inheriting that state is the proven-correct path.
          ;;
          ;; MEASURED THE HARD WAY: a previous version of this branch wrote the
          ;; full init sequence here, including AUX_MU_BAUD = 270, derived from
          ;; config.txt's core_freq=250 (250e6/(8*115200)-1).  The emitted code
          ;; was byte-perfect and the board was still SILENT.  The mini UART
          ;; divides the VPU CORE CLOCK, whose real value under start_cd.elf is
          ;; not reliably 250 MHz on a Zero 2 W (stock core_freq is 400), so a
          ;; hardcoded divisor reprograms a UART the firmware had ALREADY set
          ;; up correctly and silences it.  Touch nothing; just transmit.
          (emit-aarch64-load-imm64 buf x17 #x3F215040)
          ;; EARLY LIFE SIGN, before a single Lisp instruction runs.  The TX
          ;; FIFO is 8 deep and empty here, so 5 bytes need no LSR poll.
          ;; If "BOOT" appears but MODUS-CL never does, the console is fine
          ;; and the fault is in Lisp init - which is the single most useful
          ;; bit of information this board can give us.
          ;; x17 is AUX_MU_IO itself now, so the data register is offset 0.
          (dolist (ch '(66 79 79 84 10))       ; "BOOT\n"
            (emit-aarch64-movz buf x0 ch 0)
            (emit-aarch64-str-w buf x0 x17 0)))
        (progn
          ;; ---- PL011 UART0 (QEMU raspi3b) ---------------------------------
          ;;   CR = 0 | IBRD = 26 | FBRD = 3 | LCRH = 0x70 (8N1, FIFO) | CR = 0x301
          (emit-aarch64-load-imm64 buf x17 +rpi-cl-pl011-base+)
          (emit-aarch64-movz buf x0 0 0)
          (emit-aarch64-str-w buf x0 x17 12)   ; +0x30 UARTCR = 0
          (emit-aarch64-movz buf x0 26 0)
          (emit-aarch64-str-w buf x0 x17 9)    ; +0x24 UARTIBRD
          (emit-aarch64-movz buf x0 3 0)
          (emit-aarch64-str-w buf x0 x17 10)   ; +0x28 UARTFBRD
          (emit-aarch64-movz buf x0 #x70 0)
          (emit-aarch64-str-w buf x0 x17 11)   ; +0x2C UARTLCRH
          (emit-aarch64-movz buf x0 #x0301 0)
          (emit-aarch64-str-w buf x0 x17 12))) ; +0x30 UARTCR = enable|TX|RX

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

    ;; --- 5. VBAR_EL1 *AND* VBAR_EL2 ---------------------------------------
    ;; This image RUNS AT EL2 on a Pi 3B / Zero 2 W (measured: PSTATE 0x3c9,
    ;; M[3:0]=0b1001; every logged exception is "from EL2 to EL2").  Setting
    ;; only VBAR_EL1 left VBAR_EL2 = 0, so ANY fault vectored to
    ;; VBAR_EL2 + 0x200 = 0x200 — where there is no code — took an Undefined
    ;; Instruction, re-vectored to 0x200, and stormed FOREVER: 100% CPU, zero
    ;; output, zero allocation.  Measured 53 MILLION such exceptions in 128 s.
    ;; The vector table at *rpi-cl-vbar* was never even reachable, so the
    ;; `B .' vectors could not help.
    ;;
    ;; That is the single worst failure mode on real hardware, where there is
    ;; no `qemu -d int' to reveal it: a wedged board is indistinguishable from
    ;; a dead serial link — which cost an afternoon of chasing wiring that was
    ;; correct all along.
    (emit-aarch64-load-imm64 buf x16 *rpi-cl-vbar*)
    (emit-aarch64-u32 buf #xD518C010)       ; MSR VBAR_EL1, X16
    (emit-aarch64-u32 buf #xD51CC010)       ; MSR VBAR_EL2, X16
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
    ;; Pass the console selection so the fault reporter polls the right UART
    ;; (same test as step 2 — mini-UART on a Zero 2 W, PL011 under QEMU).
    (emit-rpi-cl-exception-vectors buf (= *aarch64-serial-base* #x3F215040))))

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
