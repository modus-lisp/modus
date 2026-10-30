;;;; boot-linux-ppc.lisp — Linux/PowerPC ELF entry for Modus, BOTH widths.
;;;;
;;;; The first BIG-ENDIAN hosted ports.  Everything above the syscall boundary is
;;;; shared with the four little-endian hosted ports; what is new here is byte
;;;; order in the ELF header (mvm/cross.lisp's WRAP-IN-ELF{32,64}-BE already
;;;; write it, for the bare-metal images) and the PowerPC syscall ABI.
;;;;
;;;; ONE FILE FOR BOTH WIDTHS because the Linux/PowerPC syscall table is the SAME
;;;; at 32 and 64 bits — write is 4 at both — unlike x86, where the 32- and
;;;; 64-bit numbering diverge completely.  The stubs differ only in the width of
;;;; a stack slot and in how far up the stack argv[1] sits.
;;;;
;;;; THE ABI, stated because it is not x86's and not RISC-V's:
;;;;   * `sc' with the syscall number in r0 and arguments in r3..r8, result in r3.
;;;;   * An error sets CR0.SO; the return value is NOT negated.  So the "negative
;;;;     return is the error" test every other port in this tree uses does not
;;;;     work here.  Nothing in these stubs checks either way, but a future
;;;;     caller that wants errno must read the condition register.
;;;;   * mmap is 90 and takes SIX REGISTER arguments.  It is not i386's old_mmap,
;;;;     which takes a pointer to an argument block, so no block is needed.

(in-package :modus.mvm)

;;; ============================================================
;;; Constants — shared by both widths
;;; ============================================================

(defconstant +linux-ppc-heap-addr+ #x10000000
  "Heap base, the same #x10000000 every hosted port uses.  That is load-bearing
   rather than tidy: PPC-SET-LINUX-MODE puts the convention slots at #x10000A00,
   the Cheney metadata block is at #x10000040, and the compiler bakes
   +MV-COUNT-ADDR+ in as a literal.  Moving the heap means moving all three.

   It must also stay below 2^30 on the 32-bit port, because a MEM-REF address
   travels as a TAGGED fixnum and a 4-byte word leaves 30 fixnum bits — the same
   ceiling that makes i386 read argv from a staged copy rather than off the live
   stack.  This heap tops out well inside it.")

(defconstant +linux-ppc-heap-size+ #x8000000
  "The DEFAULT CAP, not the size: 128 MB.  The size is chosen when the process
   starts (EMIT-LINUX-PPC-CHOOSE-HEAP-SIZE): --dynamic-space-size N[KB|MB|GB]
   (SBCL's flag, MB when unsuffixed), otherwise half of physical RAM from
   sysinfo(2), capped here; clamped to [+LINUX-PPC-HEAP-MIN+,
   +LINUX-PPC-HEAP-MAX+].  Nothing in the translator depends on it: the
   collector and every allocation site read the geometry the stub leaves in
   memory.  Same policy as boot-linux-68k.lisp.")
(defconstant +linux-ppc-heap-min+ #x100000 "1 MB: two 512 KB semispaces.")
(defconstant +linux-ppc-heap-max+ #x0F800000
  "248 MB: heap + guard end at #x1FC00000, where the kind map starts
   (+PPC-GC-KIND-MAP+), and everything stays below #x20000000 -- ARM32's and
   68k's geometry.")
(defconstant +linux-ppc-gc-guard+ #x400000
  "4 MB past the second semispace.  :gc-check tests the alloc pointer without
   knowing the size of the allocation that follows, so a large object can
   overshoot the limit; every hosted port carries this guard for that reason.
   Address space only.")
(defconstant +linux-ppc-gc-margin-max+ #x100000
  "Up to 1 MB kept free below VL, never more than a quarter of a semispace.")
(defconstant +linux-ppc-sys-sysinfo+ 116)

(defconstant +linux-ppc-heap-alloc-start+ #x2000
  "Where the bump allocator starts, as an offset into the heap.  NOT #x200: the
   whole fixed absolute block the shared runtime owns lives in the first 4 KB of
   the heap base (metadata #x40, globals #x80, MV #x90..#x138, intern tables,
   argc #x200, handler frames #x400..#xC2F, per-CPU mode #xFF8), so an allocator
   starting below #x1000 allocates on top of it.  The RV64 hosted port starts at
   #x200 — exactly where it writes argc — and has not noticed only because no
   payload there reads argc yet.")

(defconstant +linux-ppc32-load-addr+ #x10000
  "ELF load address for the 32-bit port; above Linux's default mmap_min_addr.")
(defconstant +linux-ppc64-load-addr+ #x400000
  "ELF load address for the 64-bit port, matching the other 64-bit hosted ports.")

;;; ============================================================
;;; Entry stubs
;;; ============================================================

(defun emit-linux-ppc-choose-heap-size (buf ws)
  "Leave the heap size in BYTES in r17 (callee-saved, so `sc' preserves it).
   argv's --dynamic-space-size first, else half of sysinfo(2)'s RAM, capped.
   Works in KB so no step overflows a 32-bit register, then clamps and rounds
   to 128 KB, which keeps the midpoint 64 KB-aligned."
  (let ((loop-top (mvm-make-label)) (next (mvm-make-label)) (no-flag (mvm-make-label))
        (digits (mvm-make-label)) (suffix (mvm-make-label)) (unit-g (mvm-make-label))
        (have-kb (mvm-make-label)) (cap-kb (mvm-make-label)) (ram-small (mvm-make-label))
        (ram-done (mvm-make-label)) (clamp (mvm-make-label)) (not-small (mvm-make-label))
        (not-big (mvm-make-label)) (skip (mvm-make-label))
        (cap (ash +linux-ppc-heap-size+ -10))
        (flag "--dynamic-space-size")
        ;; struct sysinfo: totalram is the 5th long, mem_unit the u32 after
        ;; 10 longs, two u16 and padding to a long
        (tr-off (* 4 ws))
        (mu-off (if (= ws 8) 104 52)))
    (flet ((li (r v) (ppc-emit-li buf r v))
           (cmpl-imm (r v) (ppc-emit-li buf +ppc-r12+ v) (ppc-emit-cmpl-word buf r +ppc-r12+)))
      ;; ---- argv: r14 = argc, r15 = &argv[0], r16 = i ----
      (ppc-emit-load-word buf +ppc-r14+ +ppc-r1+ 0)
      (ppc-emit-addi buf +ppc-r15+ +ppc-r1+ ws)
      (li +ppc-r16+ 1)
      (ppc-emit-label buf loop-top)
      (ppc-emit-cmp-word buf +ppc-r16+ +ppc-r14+)
      (ppc-emit-bge buf no-flag)
      (ppc-emit-slli buf +ppc-r4+ +ppc-r16+ (ppc-log2-ws))
      (ppc-emit-add buf +ppc-r6+ +ppc-r15+ +ppc-r4+)            ; &argv[i]
      (ppc-emit-load-word buf +ppc-r5+ +ppc-r6+ 0)
      (loop for c across flag for k from 0
            do (ppc-emit-lbz buf +ppc-r4+ +ppc-r5+ k)
               (ppc-emit-cmpi-word buf +ppc-r4+ (char-code c))
               (ppc-emit-bne buf next))
      (ppc-emit-lbz buf +ppc-r4+ +ppc-r5+ (length flag))
      (ppc-emit-cmpi-word buf +ppc-r4+ 0)
      (ppc-emit-bne buf next)
      ;; matched: the value is argv[i+1], if there is one
      (ppc-emit-addi buf +ppc-r4+ +ppc-r16+ 1)
      (ppc-emit-cmp-word buf +ppc-r4+ +ppc-r14+)
      (ppc-emit-bge buf no-flag)
      (ppc-emit-load-word buf +ppc-r5+ +ppc-r6+ ws)
      (li +ppc-r17+ 0)
      (ppc-emit-label buf digits)
      (ppc-emit-lbz buf +ppc-r4+ +ppc-r5+ 0)
      (ppc-emit-addi buf +ppc-r5+ +ppc-r5+ 1)
      (ppc-emit-cmpi-word buf +ppc-r4+ 48)
      (ppc-emit-blt buf suffix)
      (ppc-emit-cmpi-word buf +ppc-r4+ 57)
      (ppc-emit-bgt buf suffix)
      (ppc-emit-addi buf +ppc-r4+ +ppc-r4+ -48)
      ;; r17 = r17*10 + digit, saturating well above any cap
      (cmpl-imm +ppc-r17+ #x100000)
      (ppc-emit-bge buf digits)
      (ppc-emit-slli buf +ppc-r6+ +ppc-r17+ 3)
      (ppc-emit-add buf +ppc-r6+ +ppc-r6+ +ppc-r17+)
      (ppc-emit-add buf +ppc-r17+ +ppc-r6+ +ppc-r17+)
      (ppc-emit-add buf +ppc-r17+ +ppc-r17+ +ppc-r4+)
      (ppc-emit-b buf digits)
      (ppc-emit-label buf suffix)
      (ppc-emit-andi-dot buf +ppc-r4+ +ppc-r4+ #xDF)            ; upper-case it
      (ppc-emit-cmpi-word buf +ppc-r4+ 75)                       ; 'K'
      (ppc-emit-beq buf have-kb)
      (ppc-emit-cmpi-word buf +ppc-r4+ 71)                       ; 'G'
      (ppc-emit-beq buf unit-g)
      (ppc-emit-slli buf +ppc-r17+ +ppc-r17+ 10)                 ; MB -> KB
      (ppc-emit-b buf have-kb)
      (ppc-emit-label buf unit-g)
      (ppc-emit-cmpi-word buf +ppc-r17+ 0)
      (ppc-emit-beq buf have-kb)
      (li +ppc-r17+ (ash +linux-ppc-heap-max+ -10))
      (ppc-emit-b buf have-kb)
      (ppc-emit-label buf next)
      (ppc-emit-addi buf +ppc-r16+ +ppc-r16+ 1)
      (ppc-emit-b buf loop-top)
      ;; ---- no flag: half of physical RAM, capped ----
      (ppc-emit-label buf no-flag)
      (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ -128)
      ;; Preset totalram huge and mem_unit 1: if sysinfo fails, the cap wins.
      ;; (A PowerPC syscall error sets CR0.SO and does NOT negate r3.)
      (li +ppc-r4+ #x7FFFFFFF)
      (ppc-emit-store-word buf +ppc-r4+ +ppc-r1+ tr-off)
      (li +ppc-r4+ 1)
      (ppc-emit-stw buf +ppc-r4+ +ppc-r1+ mu-off)
      (ppc-emit-mr buf +ppc-r3+ +ppc-r1+)
      (li +ppc-r0+ +linux-ppc-sys-sysinfo+)
      (ppc-emit-sc buf)
      (ppc-emit-load-word buf +ppc-r17+ +ppc-r1+ tr-off)         ; totalram, units
      (ppc-emit-lwz buf +ppc-r4+ +ppc-r1+ mu-off)                ; mem_unit
      (cmpl-imm +ppc-r4+ 1024)
      (ppc-emit-blt buf ram-small)
      ;; mem_unit >= 1 KB: KB = totalram * (mem_unit >> 10), unless totalram is
      ;; already >= 1 G units, which is more than any cap
      (cmpl-imm +ppc-r17+ #x100000)
      (ppc-emit-bge buf cap-kb)
      (ppc-emit-srli buf +ppc-r4+ +ppc-r4+ 10)
      (ppc-emit-mul-word buf +ppc-r17+ +ppc-r17+ +ppc-r4+)
      (ppc-emit-b buf ram-done)
      (ppc-emit-label buf ram-small)
      (ppc-emit-srli buf +ppc-r17+ +ppc-r17+ 10)                 ; bytes -> KB
      (ppc-emit-mul-word buf +ppc-r17+ +ppc-r17+ +ppc-r4+)
      (ppc-emit-label buf ram-done)
      (ppc-emit-srli buf +ppc-r17+ +ppc-r17+ 1)                  ; half
      (cmpl-imm +ppc-r17+ cap)
      (ppc-emit-ble buf clamp)
      (ppc-emit-label buf cap-kb)
      (li +ppc-r17+ cap)
      (ppc-emit-label buf clamp)
      (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ 128)
      ;; ---- clamp to [min, max], round to 128 KB, KB -> bytes ----
      (ppc-emit-label buf have-kb)
      (cmpl-imm +ppc-r17+ (ash +linux-ppc-heap-min+ -10))
      (ppc-emit-bge buf not-small)
      (li +ppc-r17+ (ash +linux-ppc-heap-min+ -10))
      (ppc-emit-label buf not-small)
      (cmpl-imm +ppc-r17+ (ash +linux-ppc-heap-max+ -10))
      (ppc-emit-ble buf not-big)
      (li +ppc-r17+ (ash +linux-ppc-heap-max+ -10))
      (ppc-emit-label buf not-big)
      (ppc-emit-srli buf +ppc-r17+ +ppc-r17+ 7)
      (ppc-emit-slli buf +ppc-r17+ +ppc-r17+ 17)                 ; KB/128 * 128 KB
      (ppc-emit-label buf skip))))

(defun emit-linux-ppc-entry-common (mbuf 64-bit-p load-addr)
  "Emit the Linux/PowerPC userspace entry into MBUF (cross.lisp's mvm-buffer).

   Builds a ppc-buffer and appends its bytes, as the RISC-V stubs do, so the
   encoders are the ones the translator and the ladder exercise rather than a
   second hand-written copy.  *PPC-64-BIT* is bound around the body because the
   width-dispatching emitters (store-word, shift-right-arith-imm) read it, and a
   stub that emitted STD on a 32-bit machine would be an illegal instruction.

   On entry Linux has r1 pointing at argc, with argv immediately above it.

   THE HEAP IS SIZED HERE, AT RUN TIME, and everything that depends on the size
   goes into the Cheney block at #x10000040 (+GC-OFF-*+ offsets at both
   widths).  Layout, S = the chosen size:
     [base, base+S)              two S/2 semispaces (the first starts past the
                                 fixed low block, at base + alloc-start)
     [base+S, base+S+guard)      overshoot guard
     [+PPC-GC-KIND-MAP+, +S/16)  kind map, one byte per 16-byte granule, at a
                                 FIXED address past the largest heap

   It installs NO signal handlers (a fault is a clean SIGSEGV, not a longjmp
   into handler-case) and sets up no JIT arena -- there is no PPC JIT."
  (let* ((*ppc-64-bit* 64-bit-p)
         (ws (if 64-bit-p 8 4))
         (buf (make-ppc-buffer))
         (no-cap (mvm-make-label)))
    (declare (ignorable load-addr))
    (emit-linux-ppc-choose-heap-size buf ws)
    ;; --- mmap FIRST.  The argc slot below is INSIDE this mapping and this port
    ;;     asks for no BSS, so writing it before the mmap is an immediate SIGSEGV.
    ;;     mmap(addr, S + guard, PROT_READ|WRITE, MAP_PRIVATE|ANON|FIXED, -1, 0)
    ;;     then the kind map at its FIXED address, only S/16 long
    (ppc-emit-li buf +ppc-r3+ +ppc-gc-kind-map+)
    (ppc-emit-srli buf +ppc-r4+ +ppc-r17+ 4)
    (ppc-emit-li buf +ppc-r5+ 3)
    (ppc-emit-li buf +ppc-r6+ #x32)
    (ppc-emit-li buf +ppc-r7+ -1)
    (ppc-emit-li buf +ppc-r8+ 0)
    (ppc-emit-li buf +ppc-r0+ +ppc-linux-sys-mmap+)
    (ppc-emit-sc buf)
    (ppc-emit-li buf +ppc-r3+ +linux-ppc-heap-addr+)
    (ppc-emit-mr buf +ppc-r4+ +ppc-r17+)
    (ppc-emit-li buf +ppc-r12+ +linux-ppc-gc-guard+)
    (ppc-emit-add buf +ppc-r4+ +ppc-r4+ +ppc-r12+)
    (ppc-emit-li buf +ppc-r5+ 3)                 ; PROT_READ|PROT_WRITE
    (ppc-emit-li buf +ppc-r6+ #x32)              ; MAP_PRIVATE|ANONYMOUS|FIXED
    (ppc-emit-li buf +ppc-r7+ -1)                ; fd
    (ppc-emit-li buf +ppc-r8+ 0)                 ; offset
    (ppc-emit-li buf +ppc-r0+ +ppc-linux-sys-mmap+)
    (ppc-emit-sc buf)
    ;; r16 = heap base AS RETURNED.  MAP_FIXED means it equals the request, but
    ;; using the return value keeps a failed mapping visible as a wild pointer
    ;; instead of silent writes to an address nobody mapped.
    (ppc-emit-mr buf +ppc-r16+ +ppc-r3+)
    ;; --- argc, now inside the mapping
    (ppc-emit-load-word buf +ppc-r14+ +ppc-r1+ 0)
    (ppc-emit-li buf +ppc-r11+ (+ +linux-ppc-heap-addr+ #x200))
    (ppc-emit-stw buf +ppc-r14+ +ppc-r11+ 0)
    ;; --- r18 = midpoint = S/2;  r14 = space_size = mid - alloc-start
    (ppc-emit-srli buf +ppc-r18+ +ppc-r17+ 1)
    (ppc-emit-addi buf +ppc-r14+ +ppc-r18+ (- +linux-ppc-heap-alloc-start+))
    ;; --- r15 = margin = min(margin-max, space_size/4), 16-aligned
    (ppc-emit-srli buf +ppc-r15+ +ppc-r14+ 2)
    (ppc-emit-clear-low4 buf +ppc-r15+ +ppc-r15+)
    (ppc-emit-li buf +ppc-r12+ +linux-ppc-gc-margin-max+)
    (ppc-emit-cmpl-word buf +ppc-r15+ +ppc-r12+)
    (ppc-emit-ble buf no-cap)
    (ppc-emit-mr buf +ppc-r15+ +ppc-r12+)
    (ppc-emit-label buf no-cap)
    ;; --- MVM allocation registers.  VA=r19, VL=r20, VN=r21 (see target.lisp).
    (ppc-emit-addi buf +ppc-r19+ +ppc-r16+ +linux-ppc-heap-alloc-start+)
    (ppc-emit-add buf +ppc-r20+ +ppc-r16+ +ppc-r18+)
    (ppc-emit-subf buf +ppc-r20+ +ppc-r15+ +ppc-r20+)            ; VL = mid - margin
    ;; VN = NIL = +NIL-VALUE+, not zero — see boot-linux-riscv.lisp.
    (ppc-emit-li buf +ppc-r21+ +nil-value+)
    ;; --- Cheney metadata at the heap-relative block, RAW addresses.
    (ppc-emit-li buf +ppc-r11+ (+ +linux-ppc-heap-addr+ #x40))
    (ppc-emit-store-word buf +ppc-r19+ +ppc-r11+ #x00)          ; from_start
    (ppc-emit-add buf +ppc-r12+ +ppc-r16+ +ppc-r18+)
    (ppc-emit-store-word buf +ppc-r12+ +ppc-r11+ #x08)          ; to_start = base + mid
    (ppc-emit-store-word buf +ppc-r14+ +ppc-r11+ #x10)          ; space_size
    (ppc-emit-store-word buf +ppc-r1+  +ppc-r11+ #x18)          ; stack_base
    (ppc-emit-li buf +ppc-r12+ 0)
    (ppc-emit-store-word buf +ppc-r12+ +ppc-r11+ #x20)          ; gc_count
    ;; --- VFP (r31) needs to point at a frame before the first prologue runs.
    (ppc-emit-mr buf +ppc-r31+ +ppc-r1+)
    ;; --- fall through to translated native code
    (ppc-fixup-labels buf)
    (loop for b across (ppc-buffer-to-bytes buf)
          do (mvm-emit-byte mbuf b))))

(defun emit-linux-ppc32-entry (mbuf)
  (emit-linux-ppc-entry-common mbuf nil +linux-ppc32-load-addr+))

(defun emit-linux-ppc64-entry (mbuf)
  (emit-linux-ppc-entry-common mbuf t +linux-ppc64-load-addr+))

;;; ============================================================
;;; Boot descriptors
;;; ============================================================
;;;
;;; No :elf-format.  cross.lisp's wrapper dispatch names the little-endian
;;; formats explicitly and falls through to a GENERIC arm that writes ELF from
;;; :elf-machine / :elf-class / :elf-flags — and that arm is big-endian, because
;;; it was written for these architectures' bare-metal images.  So the hosted
;;; ports need no new wrapper at all; they need the right descriptor.

(defun linux-ppc32-boot-descriptor ()
  "Boot descriptor for the hosted 32-bit big-endian PowerPC image."
  (list :arch :ppc32
        :entry-fn #'emit-linux-ppc32-entry
        :elf-machine 20                 ; EM_PPC
        :elf-class 32
        :load-addr +linux-ppc32-load-addr+
        :heap-base +linux-ppc-heap-addr+
        :cons-base +linux-ppc-heap-addr+
        :endianness :big))

(defun linux-ppc64-boot-descriptor ()
  "Boot descriptor for the hosted 64-bit big-endian PowerPC image.

   :elf-flags 2 = EF_PPC64_ABI_V2, AND IT IS NOT OPTIONAL HERE.  I first set 0,
   reasoning that a static binary has no function descriptors for anyone to look
   for, and it SIGSEGV'd at si_addr=NULL before reaching its first syscall — with
   a verified-correct instruction stream at a verified-correct e_entry.

   Linux's ppc64 START_THREAD branches on exactly this flag: for an ELFv1 task it
   treats e_entry as the address of a FUNCTION DESCRIPTOR and dereferences it for
   the real entry point and the TOC, so our first instruction word was read as a
   code address and jumped to.  ELFv2 (\"look ma, no function descriptors\") takes
   e_entry as the code address, which is what this stub is.  The bare powernv
   image has carried flag 2 all along; I dismissed it as a skiboot concern."
  (list :arch :ppc64
        :entry-fn #'emit-linux-ppc64-entry
        :elf-machine 21                 ; EM_PPC64
        :elf-class 64
        :elf-flags 2                    ; EF_PPC64_ABI_V2 — see the docstring
        :load-addr +linux-ppc64-load-addr+
        :heap-base +linux-ppc-heap-addr+
        :cons-base +linux-ppc-heap-addr+
        :endianness :big))
