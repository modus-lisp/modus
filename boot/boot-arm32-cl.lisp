;;;; boot-arm32-cl.lisp — bare-metal ARMv7 entry for the CL image (QEMU virt)
;;;;
;;;; The CL runtime on 32 bits expects the HOSTED arm32 picture
;;;; (boot-linux-arm32.lisp): the image low, the runtime's fixed block and the
;;;; heap at #x10000000.., the collector's kind map past the heap, NIL's page at
;;;; #xDEAD0000, and nothing the Lisp side addresses at or above 2^29 (a MEM-REF
;;;; address travels as a TAGGED 30-bit fixnum).  QEMU virt has RAM only from
;;;; #x40000000 and PCIe MMIO across #x10000000..#x3EFFFFFF, so this stub builds
;;;; that picture with the MMU, in 1 MB short-descriptor sections, and then
;;;; does what the hosted stub does after its mmaps:
;;;;
;;;;   VA #x00000000..#x0FFFFFFF -> PA #x40000000..  image (VA #x10000 = where
;;;;                                                 QEMU loads it, PA #x40010000),
;;;;                                                 stack, this page table
;;;;   VA #x10000000..#x2FFFFFFF -> PA #x50000000..  fixed block, heap, kind map
;;;;   VA #x30000000             -> PA #x09000000    PL011 (device memory)
;;;;   VA #x40000000..#x4FFFFFFF -> PA #x40000000..  identity, for the MMU switch
;;;;   VA #xDEA00000             -> PA #x70000000    NIL's page (#xDEAD0000)
;;;;
;;;; Needs -m 1024 (RAM to #x80000000).  Translated with ARM32-SET-CL-BARE-MODE
;;;; (bare traps, hosted layout) -- see translate-arm32.lisp *ARM32-CL-BARE*.

(in-package :modus.mvm)

(defconstant +arm32-cl-load-va+   #x10000    "VA of image byte 0 (PA #x40010000).")
(defconstant +arm32-cl-ram-pa+    #x40000000)
(defconstant +arm32-cl-data-pa+   #x50000000 "PA behind VA #x10000000.")
(defconstant +arm32-cl-uart-va+   #x30000000 "VA of the PL011 (PA #x09000000).")
(defconstant +arm32-cl-nil-pa+    #x70000000 "PA behind VA #xDEA00000 (NIL's page).")
(defconstant +arm32-cl-l1-va+     #x0F800000 "The L1 table (16 KB, 16 KB aligned).")
(defconstant +arm32-cl-stack-top+ #x0F000000 "Grows down; below the L1 table.")

(defun arm32-cl-section (va-mb pa normal-p)
  "Short-descriptor 1 MB section entry: AP=11 (domain is MANAGER anyway),
   Normal write-back (TEX=000 C=1 B=1) or Device (B=1, XN)."
  (declare (ignore va-mb))
  (logior (logand pa #xFFF00000) (if normal-p #xC0E #xC16)))

(defun emit-arm32-cl-entry (mbuf)
  "Entry stub of the bare ARMv7 CL image.  See the file header for the map."
  (let ((buf (make-arm32-buffer))
        (l1-pa (+ +arm32-cl-ram-pa+ +arm32-cl-l1-va+)))
    (flet ((w (insn) (arm32-emit buf insn))
           (fill-sections (va-mb count pa normal-p)
             ;; r0 = L1 + 4*va-mb, r1 = first descriptor, r2 = count; +1 MB each
             (let ((top (mvm-make-label)))
               (arm32-load-imm32 buf +arm-r0+ (+ l1-pa (* 4 va-mb)))
               (arm32-load-imm32 buf +arm-r1+ (arm32-cl-section va-mb pa normal-p))
               (arm32-load-imm32 buf +arm-r2+ count)
               (arm32-emit-label buf top)
               (arm32-str buf +arm-r1+ +arm-r0+ 0)
               (arm32-add-imm buf +arm-r0+ +arm-r0+ 0 4)
               (arm32-add-imm buf +arm-r1+ +arm-r1+ 6 1)      ; += #x100000 (1 ror 12)
               (arm32-sub-imm buf +arm-r2+ +arm-r2+ 0 1)
               (arm32-cmp-imm buf +arm-r2+ 0 0)
               (arm32-b-cond buf +arm-cc-ne+ top)))
           (zero-range (start bytes)
             (let ((top (mvm-make-label)))
               (arm32-load-imm32 buf +arm-r0+ start)
               (arm32-load-imm32 buf +arm-r1+ (floor bytes 4))
               (arm32-mov-imm buf +arm-r2+ 0 0)
               (arm32-emit-label buf top)
               (arm32-str buf +arm-r2+ +arm-r0+ 0)
               (arm32-add-imm buf +arm-r0+ +arm-r0+ 0 4)
               (arm32-sub-imm buf +arm-r1+ +arm-r1+ 0 1)
               (arm32-cmp-imm buf +arm-r1+ 0 0)
               (arm32-b-cond buf +arm-cc-ne+ top))))
      ;; ---- 1. the VFP (off out of reset; see boot-arm32.lisp's raspi2b entry)
      (w #xEE110F50)                     ; MRC p15,0,r0,c1,c0,2   CPACR
      (w #xE380060F)                     ; ORR r0,r0,#0x00F00000  cp10+cp11
      (w #xEE010F50)                     ; MCR p15,0,r0,c1,c0,2
      (w #xF57FF06F)                     ; ISB
      (w #xE3A00101)                     ; MOV r0,#0x40000000     FPEXC.EN
      (w #xEEE80A10)                     ; VMSR FPEXC,r0
      ;; ---- 2. the L1 table (MMU off: physical addresses) ------------------
      (zero-range l1-pa #x4000)
      (fill-sections #x000 #x100 +arm32-cl-ram-pa+  t)
      (fill-sections #x100 #x200 +arm32-cl-data-pa+ t)
      (fill-sections #x300 1     #x09000000          nil)
      (fill-sections #x400 #x100 +arm32-cl-ram-pa+  t)
      (fill-sections #xDEA 1     +arm32-cl-nil-pa+  t)
      ;; ---- 3. MMU on, then into VA space ----------------------------------
      (arm32-load-imm32 buf +arm-r0+ l1-pa)
      (w #xEE020F10)                     ; MCR p15,0,r0,c2,c0,0   TTBR0
      (arm32-mov-imm buf +arm-r1+ 0 0)
      (w #xEE021F50)                     ; MCR p15,0,r1,c2,c0,2   TTBCR = 0
      (w #xEE081F17)                     ; MCR p15,0,r1,c8,c7,0   TLBIALL
      (w #xE3E01000)                     ; MVN r1,#0
      (w #xEE031F10)                     ; MCR p15,0,r1,c3,c0,0   DACR: all MANAGER
      (w #xF57FF04F)                     ; DSB
      (w #xF57FF06F)                     ; ISB
      (w #xEE111F10)                     ; MRC p15,0,r1,c1,c0,0   SCTLR
      (w #xE3811001)                     ; ORR r1,r1,#1           M
      (w #xE3811004)                     ; ORR r1,r1,#4           C
      (w #xE3811A01)                     ; ORR r1,r1,#0x1000      I
      (w #xE3C11002)                     ; BIC r1,r1,#2           A (unaligned OK)
      (w #xEE011F10)                     ; MCR p15,0,r1,c1,c0,0
      (w #xF57FF06F)                     ; ISB
      ;; still at PA #x4001xxxx (identity-mapped); PC reads as here+8, so this
      ;; continues at the instruction after the NOP, #x40000000 lower = its VA.
      (w #xE24FF101)                     ; SUB pc,pc,#0x40000000
      (w #xE1A00000)                     ; NOP (skipped)
      ;; ---- 4. what the hosted stub does after its mmaps -------------------
      (arm32-load-imm32 buf +arm-sp+ +arm32-cl-stack-top+)
      (zero-range #x10000000 #x20000)    ; the fixed block (hosted: fresh mmap)
      (zero-range (+ +linux-arm32-heap-addr+ +linux-arm32-heap-size+
                     +linux-arm32-gc-guard+)
                  (floor +linux-arm32-heap-size+ 16))   ; the kind map
      ;; NIL's page: 1024 words of NIL, as the hosted stub writes it
      (arm32-load-imm32 buf +arm-r0+ #xDEAD0000)
      (arm32-load-imm32 buf +arm-r12+ +nil-value+)
      (arm32-load-imm32 buf +arm-r1+ 1024)
      (let ((fill (mvm-make-label)))
        (arm32-emit-label buf fill)
        (arm32-str buf +arm-r12+ +arm-r0+ 0)
        (arm32-add-imm buf +arm-r0+ +arm-r0+ 0 4)
        (arm32-sub-imm buf +arm-r1+ +arm-r1+ 0 1)
        (arm32-cmp-imm buf +arm-r1+ 0 0)
        (arm32-b-cond buf +arm-cc-ne+ fill))
      ;; allocation registers + region 0's control block (boot-linux-arm32)
      (arm32-load-imm32 buf +arm-r6+ +linux-arm32-heap-addr+)
      (arm32-load-imm32 buf +arm-r12+ +linux-arm32-heap-alloc-start+)
      (arm32-add buf +arm-r9+ +arm-r6+ +arm-r12+)
      (arm32-load-imm32 buf +arm-r12+ (- +linux-arm32-gc-midpoint+ +arm32-gc-overshoot-margin+))
      (arm32-add buf +arm-r10+ +arm-r6+ +arm-r12+)
      (arm32-load-imm32 buf +arm-r8+ +nil-value+)
      (arm32-load-imm32 buf +arm-lr+ #x10000040)
      (arm32-str buf +arm-r9+ +arm-lr+ 0)           ; [0x40] from_start
      (arm32-load-imm32 buf +arm-r12+ +linux-arm32-gc-midpoint+)
      (arm32-add buf +arm-r12+ +arm-r6+ +arm-r12+)
      (arm32-str buf +arm-r12+ +arm-lr+ 8)          ; [0x48] to_start
      (arm32-load-imm32 buf +arm-r12+ (- +linux-arm32-gc-midpoint+
                                         +linux-arm32-heap-alloc-start+))
      (arm32-str buf +arm-r12+ +arm-lr+ 16)         ; [0x50] space_size
      (arm32-mov buf +arm-r12+ +arm-sp+)
      (arm32-str buf +arm-r12+ +arm-lr+ 24)         ; [0x58] stack_base
      (arm32-mov-imm buf +arm-r12+ 0 0)
      (arm32-str buf +arm-r12+ +arm-lr+ 32)         ; [0x60] gc_count
      (arm32-mov-imm buf +arm-r11+ 0 0)             ; base frame
      (arm32-resolve-fixups buf)
      (loop for b across (arm32-buffer-to-bytes buf)
            do (mvm-emit-byte mbuf b)))))

(defun armv7-cl-boot-descriptor ()
  "Boot descriptor for the bare ARMv7 CL image (QEMU virt, MMU-remapped)."
  (list :arch :armv7
        :entry-fn #'emit-arm32-cl-entry
        :load-addr +arm32-cl-load-va+
        :stack-top +arm32-cl-stack-top+
        :cons-base +linux-arm32-heap-addr+
        :general-base (+ +linux-arm32-heap-addr+ +linux-arm32-gc-midpoint+)
        :endianness :little))
