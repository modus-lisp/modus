;;;; boot-linux-riscv32.lisp — Linux RISC-V 32 (RV32) ELF entry for Modus
;;;;
;;;; The 32-bit sibling of boot-linux-riscv.lisp.  Same syscall ABI (RISC-V uses
;;;; the asm-generic numbering at both widths, so read/write/exit are 63/64/93
;;;; here exactly as on RV64), same register conventions, same translator — the
;;;; differences are ELF CLASS, pointer width, and the stack layout argc/argv
;;;; arrive in.
;;;;
;;;; WHY RV32 AT ALL.  RV64 is what servers and QEMU virt run.  RV32 is what a
;;;; microcontroller runs: GD32VF103, ESP32-C3, CH32V, the SiFive E cores.  The
;;;; whole back end is shared, so the cost of reaching that class of hardware is
;;;; this file plus the width dispatch in translate-riscv.lisp — not a port.
;;;;
;;;; THREE THINGS THAT ARE NOT MERELY "NARROWER" ON RV32, each of which was a
;;;; real bug before it was a comment:
;;;;   1. SLLI's shift-amount field is FIVE bits, not six.  A shamt of 40 (the
;;;;      RV64 mask-to-24-bits distance) sets bit 25, which is funct7 — so it is
;;;;      not a wrong shift, it is a reserved instruction.
;;;;   2. AMOSWAP.D does not exist; funct3 encodes the ACCESS WIDTH.
;;;;   3. An address like #x80000000 is an ordinary RV32 value but is outside
;;;;      signed-32, so RV-EMIT-LI's 64-bit shift-and-add chain would run and
;;;;      build a value the register cannot hold.
;;;; All three are handled in translate-riscv.lisp, keyed off *RISCV-64-BIT*.

(in-package :modus.mvm)

;;; ============================================================
;;; Linux RV32 Constants
;;; ============================================================

(defconstant +linux-riscv32-load-addr+ #x10000
  "ELF load address.  Low, as the 32-bit ports do (boot-linux-arm32.lisp uses
   the same), and above mmap_min_addr (#x10000 by default on Linux).")

(defconstant +linux-riscv32-heap-addr+ #x10000000
  "Heap base — the SAME #x10000000 every other hosted port uses, and that is
   load-bearing rather than tidy: RISCV-SET-LINUX-MODE puts the convention slots
   at #x10000A00, the Cheney metadata block is at #x10000040, and the compiler
   bakes +MV-COUNT-ADDR+ in as a literal.  Moving the heap on this port means
   moving all three, so it does not move.

   It also has to stay below 2^30.  A MEM-REF address travels as a TAGGED
   fixnum, and a 4-byte word gives 30 fixnum bits — the same ceiling that makes
   i386 read its argv out of a staged BSS copy rather than off the live stack
   at #x40800390.  #x10000000 plus this heap tops out at #x18400000, well
   inside it; a heap at #x40000000 would be unaddressable by construction.")

(defconstant +linux-riscv32-heap-size+ #x8000000
  "128 MB: two 64 MB semispaces.  Generous for an embedded target and modest
   for QEMU.  A real microcontroller gets its own, much smaller, numbers; the
   point of naming them is that shrinking is a constant edit, not a port.")
(defconstant +linux-riscv32-heap-alloc-start+ #x2000
  "Where the bump allocator starts, as an offset into the heap — NOT the #x200
   the RV64 hosted port uses.  #x200 is exactly where that port's entry stub
   writes ARGC, so its first allocation lands on the argc slot; nothing has
   noticed because no payload on that port reads argc yet.  More generally the
   whole fixed absolute block the shared runtime owns lives in the first 4 KB of
   this address (metadata #x40, globals #x80, MV #x90..#x138, intern tables,
   argc #x200, handler frames #x400..#xC2F, the per-CPU mode word #xFF8), so an
   allocator starting below #x1000 is allocating on top of it.  Starting at
   #x2000 costs 8 KB and removes the whole class.")
(defconstant +linux-riscv32-gc-midpoint+ #x4000000)   ; 64 MB
(defconstant +linux-riscv32-gc-guard+ #x400000
  "4 MB past the second semispace.  :gc-check tests the alloc pointer against
   the limit WITHOUT knowing the size of the allocation that follows, so a large
   object can overshoot; every other hosted port carries the same guard for the
   same reason (see the i386 B3 defect in CLAUDE.md).")

;;; Syscall numbers are the asm-generic ones, IDENTICAL to RV64.  222 is
;;; __NR3264_mmap, which resolves to sys_mmap2 on a 32-bit kernel: its sixth
;;; argument is an offset in PAGES rather than bytes.  We pass 0, so the two
;;; spellings agree and nothing here depends on which one the kernel picked.
(defconstant +rv32-sys-read+   63)
(defconstant +rv32-sys-write+  64)
(defconstant +rv32-sys-exit+   93)
(defconstant +rv32-sys-mmap+   222)

;;; ============================================================
;;; ELF wrapper
;;; ============================================================

(defun wrap-in-elf32-le-riscv (raw-bytes load-addr &key bss-end)
  "ELF32-LE RV32 executable.  Delegates to WRAP-IN-ELF32-LE-I386 with
   e_machine 243 (EM_RISCV) — the only architecture-specific field in an
   ELF32-LE header — for the same reason WRAP-IN-ELF64-LE-RISCV delegates to the
   AArch64 wrapper: the program-header FIELD ORDER differs between ELF32 and
   ELF64 and one copy of it is enough.

   BSS-END is accepted and normally NIL: this port's entry stub mmaps its heap
   with MAP_FIXED before touching any fixed address, so unlike the 64-bit ports
   it does not lean on a demand-zeroed BSS to cover the metadata block."
  (wrap-in-elf32-le-i386 raw-bytes load-addr :bss-end bss-end :machine 243))

;;; ============================================================
;;; Userspace entry stub
;;; ============================================================

(defun emit-linux-riscv32-entry (mbuf)
  "Emit the RV32 Linux userspace entry into MBUF (cross.lisp's mvm-buffer).

   Builds an rv-buffer and appends its bytes, exactly as the RV64 stub does, so
   the instruction encoders are the ones the translator and the arch ladder
   exercise rather than a second hand-written copy.  Every load and store goes
   through RV-EMIT-LOAD-WORD / RV-EMIT-STORE-WORD, which are 4-byte here because
   INSTALL-RISCV32-TRANSLATOR has cleared *RISCV-64-BIT* — emitting an LD in a
   stub for a machine with no LD is the failure this indirection prevents.

   On entry SP points at argc, and argv is the array of WORD-sized pointers
   immediately above it — so argv[1] is at [sp+8] on RV32 where the RV64 stub
   reads [sp+16].  That is the one layout difference between the two stubs."
  (let ((buf (make-rv-buffer)))
    ;; --- argc / argv into callee-saved registers that survive an ecall
    (rv-emit-load-word buf +rv-s2+ +rv-sp+ 0)   ; s2 = argc
    (rv-emit-load-word buf +rv-s3+ +rv-sp+ 8)   ; s3 = argv[1]  (2 words up)
    ;; --- mmap FIRST.  The argc slot below is INSIDE this mapping, and this
    ;;     port asks for NO bss, so writing it before the mmap is not merely
    ;;     fragile here (as it was on RV64, which survived on its 896 MB BSS) —
    ;;     it is an immediate SIGSEGV, which is what the ARM32 port measured.
    (rv-emit-li buf +rv-a0+ +linux-riscv32-heap-addr+)
    (rv-emit-li buf +rv-a1+ (+ +linux-riscv32-heap-size+ +linux-riscv32-gc-guard+))
    (rv-emit-li buf +rv-a2+ 3)                  ; PROT_READ|PROT_WRITE
    (rv-emit-li buf +rv-a3+ #x32)               ; MAP_PRIVATE|ANONYMOUS|FIXED
    (rv-emit-li buf +rv-a4+ -1)                 ; fd
    (rv-emit-li buf +rv-a5+ 0)                  ; offset (pages for mmap2; 0)
    (rv-emit-li buf +rv-a7+ +rv32-sys-mmap+)
    (rv-emit-ecall buf)
    ;; s4 = heap base AS RETURNED, so a failed mapping shows up as a wild
    ;; pointer rather than as silent writes to an address nobody mapped.
    (rv-emit-mv buf +rv-s4+ +rv-a0+)
    ;; --- argc where the Lisp side looks for it, inside the fresh mapping
    (rv-emit-li buf +rv-t0+ (+ +linux-riscv32-heap-addr+ #x200))
    (rv-emit-sw buf +rv-s2+ +rv-t0+ 0)
    ;; --- MVM allocation registers: s8 = alloc pointer, s9 = limit, s10 = NIL
    (rv-emit-li buf +rv-t0+ +linux-riscv32-heap-alloc-start+)
    (rv-emit-add buf +rv-s8+ +rv-s4+ +rv-t0+)
    (rv-emit-li buf +rv-t0+ +linux-riscv32-gc-midpoint+)
    (rv-emit-add buf +rv-s9+ +rv-s4+ +rv-t0+)
    ;; VN = NIL = +NIL-VALUE+, not zero — see boot-linux-riscv.lisp.
    (rv-emit-li buf +rv-s10+ +nil-value+)
    ;; --- Cheney metadata, at the heap-relative block this port uses.  RAW
    ;;     addresses, matching what the collector expects.
    (rv-emit-li buf +rv-t1+ (+ +linux-riscv32-heap-addr+ #x40))
    (rv-emit-store-word buf +rv-s8+ +rv-t1+ 0)   ; [+0x00] from_start
    (rv-emit-store-word buf +rv-s9+ +rv-t1+ 4)   ; [+0x04] to_start
    (rv-emit-li buf +rv-t0+ (- +linux-riscv32-gc-midpoint+
                               +linux-riscv32-heap-alloc-start+))
    (rv-emit-store-word buf +rv-t0+ +rv-t1+ 8)   ; [+0x08] space_size
    (rv-emit-mv buf +rv-t0+ +rv-sp+)
    (rv-emit-store-word buf +rv-t0+ +rv-t1+ 12)  ; [+0x0C] stack_base
    (rv-emit-li buf +rv-t0+ 0)
    (rv-emit-store-word buf +rv-t0+ +rv-t1+ 16)  ; [+0x10] gc_count
    ;; --- fall through to translated native code
    (loop for b across (rv-buffer-to-bytes buf)
          do (mvm-emit-byte mbuf b))))

;;; ============================================================
;;; Boot descriptor
;;; ============================================================

(defun linux-riscv32-boot-descriptor ()
  "Boot descriptor for the hosted RV32 image."
  (list :arch :riscv32
        :entry-fn #'emit-linux-riscv32-entry
        :elf-format :linux-riscv32
        :elf-machine 243
        :elf-class 32
        :load-addr +linux-riscv32-load-addr+
        :heap-base +linux-riscv32-heap-addr+
        :cons-base +linux-riscv32-heap-addr+
        :endianness :little))
