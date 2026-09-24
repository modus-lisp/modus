;;;; boot-linux-68k.lisp — Linux/m68k ELF entry for Modus.
;;;;
;;;; The third big-endian hosted port, and the one whose syscall ABI is least
;;;; like anything else in the tree:
;;;;
;;;;   * `trap #0', with the syscall number in d0 and the result in d0.
;;;;   * Arguments are d1, d2, d3, d4, d5 and then **a0** — five data registers
;;;;     and an ADDRESS register.  Nothing else in this tree needs a mixed
;;;;     argument bank, and a six-argument syscall (mmap2) is where it shows.
;;;;   * mmap2 is 192 and takes its offset in PAGES.  90 (old_mmap) exists, but
;;;;     takes a POINTER to a block of six arguments rather than the arguments,
;;;;     so it needs a staged block in memory that mmap2 does not.
;;;;
;;;; Real m68k Linux runs on Amigas, Ataris, Macs and 68k VME boards.  The image
;;;; this produces is a static ELF32-BE executable with no libc.

(in-package :modus.mvm)

;;; ============================================================
;;; Constants
;;; ============================================================

(defconstant +linux-68k-load-addr+ #x10000
  "ELF load address, above Linux's default mmap_min_addr.")

(defconstant +linux-68k-heap-addr+ #x10000000
  "Heap base — the shared hosted address, and load-bearing rather than tidy:
   M68K-SET-LINUX-MODE puts the convention slots at #x10000A00, the Cheney
   metadata is at #x10000040, and +MV-COUNT-ADDR+ is baked in as a literal.  On
   this target the bare-metal image ALREADY uses #x10000090 for the MV slots
   (unlike RISC-V, where that address is UART MMIO, or ppc32, where it is outside
   the mapped TLBs), so only the convention-slot base actually moves.

   It also stays below 2^30, the ceiling a tagged MEM-REF address can express on
   a 4-byte word.")

(defconstant +linux-68k-heap-size+ #x8000000)      ; 128 MB: two 64 MB semispaces
(defconstant +linux-68k-gc-midpoint+ #x4000000)    ; 64 MB
(defconstant +linux-68k-gc-guard+ #x400000
  "4 MB past the second semispace: :gc-check tests the alloc pointer without
   knowing the size of the allocation that follows, so a big object can overshoot.")
(defconstant +linux-68k-heap-alloc-start+ #x2000
  "Clear of the whole fixed low block the shared runtime owns — metadata #x40,
   globals #x80, MV #x90..#x138, argc #x200, handler frames #x400..#xC2F.")

(defconstant +linux-68k-page-shift+ 12
  "mmap2 takes its file offset in pages.  We pass 0, so this is documentation of
   the units rather than arithmetic — but it is the difference between mmap2 and
   mmap and the reason the two cannot be swapped by changing only the number.")

;;; ============================================================
;;; Entry stub
;;; ============================================================

(defun emit-linux-68k-entry (mbuf)
  "Emit the Linux/m68k userspace entry into MBUF (cross.lisp's mvm-buffer).

   Builds an m68k-buffer and appends its bytes, as the other hosted stubs do, so
   the encoders are the ones the translator and the ladder exercise.

   On entry a7 points at argc with argv immediately above, so argv[1] is at
   8(a7).  d0/d1 are the translator's reserved scratch and free here; a2/a3/a4
   are VA/VL/VN and a6 is VFP, which the first function prologue expects to be
   pointing somewhere valid.

   No signal handlers and no JIT arena: a fault is a clean SIGSEGV rather than a
   longjmp into handler-case, and there is no m68k JIT.  Deliberate omissions."
  (let ((buf (make-m68k-buffer)))
    ;; --- argc, staged into the heap below.  d6/d7 are V4/V5 and nothing is
    ;;     live yet, so they hold argc/argv across the syscall.  (The syscall
    ;;     clobbers d0-d5 and a0 by the ABI above; d6/d7 survive.)
    (m68k-emit-move-disp-dn buf +68k-a7+ 0 +68k-d6+)      ; d6 = argc
    (m68k-emit-move-disp-dn buf +68k-a7+ 8 +68k-d7+)      ; d7 = argv[1]
    ;; --- mmap2 FIRST: the argc slot is INSIDE this mapping, and this port asks
    ;;     for no BSS, so writing it beforehand is an immediate SIGSEGV rather
    ;;     than merely fragile.  (RV64 survives that ordering only on the
    ;;     accident of a 896 MB BSS; ARM32, with no BSS, measured the fault.)
    ;;     mmap2(addr, len, PROT_READ|WRITE, MAP_PRIVATE|ANON|FIXED, -1, 0)
    (m68k-emit-move-imm-dn buf +linux-68k-heap-addr+ +68k-d1+)
    (m68k-emit-move-imm-dn buf (+ +linux-68k-heap-size+ +linux-68k-gc-guard+)
                           +68k-d2+)
    (m68k-emit-move-imm-dn buf 3 +68k-d3+)                ; PROT_READ|PROT_WRITE
    (m68k-emit-move-imm-dn buf #x32 +68k-d4+)             ; PRIVATE|ANON|FIXED
    (m68k-emit-move-imm-dn buf -1 +68k-d5+)               ; fd
    (m68k-emit-move-imm-an buf 0 +68k-a0+)                ; arg6: pgoff, in a0
    (m68k-emit-move-imm-dn buf +68k-linux-sys-mmap2+ +68k-d0+)
    (m68k-emit-trap buf 0)
    ;; d0 = heap base AS RETURNED.  MAP_FIXED means it equals the request, but
    ;; using the return value keeps a failed mapping visible as a wild pointer
    ;; rather than as silent writes to an address nobody mapped.
    (m68k-emit-move-dn-an buf +68k-d0+ +68k-a1+)          ; a1 = heap base
    ;; --- argc, now inside the mapping
    (m68k-emit-move-imm-an buf (+ +linux-68k-heap-addr+ #x200) +68k-a0+)
    (m68k-emit-move-dn-an-ind buf +68k-d6+ +68k-a0+)
    ;; --- MVM allocation registers: VA=a2, VL=a3, VN=a4
    (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d1+)
    (m68k-emit-addi buf +68k-d1+ +linux-68k-heap-alloc-start+)
    (m68k-emit-move-dn-an buf +68k-d1+ +68k-a2+)          ; VA
    (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d1+)
    (m68k-emit-addi buf +68k-d1+ +linux-68k-gc-midpoint+)
    (m68k-emit-move-dn-an buf +68k-d1+ +68k-a3+)          ; VL
    ;; VN = NIL = +NIL-VALUE+, not zero — see boot-linux-riscv.lisp.
    (m68k-emit-move-imm-an buf +nil-value+ +68k-a4+)
    ;; --- Cheney metadata, RAW addresses, at the heap-relative block
    (m68k-emit-move-imm-an buf (+ +linux-68k-heap-addr+ #x40) +68k-a0+)
    (m68k-emit-move-an-dn buf +68k-a2+ +68k-d1+)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ 0)      ; from_start
    (m68k-emit-move-an-dn buf +68k-a3+ +68k-d1+)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ 4)      ; to_start
    (m68k-emit-move-imm-dn buf (- +linux-68k-gc-midpoint+
                                  +linux-68k-heap-alloc-start+) +68k-d1+)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ 8)      ; space_size
    (m68k-emit-move-an-dn buf +68k-a7+ +68k-d1+)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ 12)     ; stack_base
    (m68k-emit-move-imm-dn buf 0 +68k-d1+)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ 16)     ; gc_count
    ;; --- VFP must point at a frame before the first prologue runs
    (m68k-emit-move-an-an buf +68k-a7+ +68k-a6+)
    ;; --- fall through to translated native code
    (loop for b across (m68k-buffer-to-bytes buf)
          do (mvm-emit-byte mbuf b))))

;;; ============================================================
;;; Boot descriptor
;;; ============================================================
;;;
;;; No :elf-format — cross.lisp's wrapper dispatch names the little-endian
;;; formats explicitly and falls through to a GENERIC arm that writes big-endian
;;; ELF from :elf-machine / :elf-class, because that arm was written for this
;;; architecture's bare-metal image.  The hosted port needs no new wrapper.

(defun linux-68k-boot-descriptor ()
  "Boot descriptor for the hosted big-endian m68k image."
  (list :arch :68k
        :entry-fn #'emit-linux-68k-entry
        :elf-machine 4                  ; EM_68K
        :elf-class 32
        :load-addr +linux-68k-load-addr+
        :heap-base +linux-68k-heap-addr+
        :cons-base +linux-68k-heap-addr+
        :endianness :big))
