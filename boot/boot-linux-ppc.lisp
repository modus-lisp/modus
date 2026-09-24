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

(defconstant +linux-ppc-heap-size+ #x8000000)       ; 128 MB: two 64 MB semispaces
(defconstant +linux-ppc-gc-midpoint+ #x4000000)     ; 64 MB
(defconstant +linux-ppc-gc-guard+ #x400000
  "4 MB past the second semispace.  :gc-check tests the alloc pointer without
   knowing the size of the allocation that follows, so a large object can
   overshoot the limit; every hosted port carries this guard for that reason.")

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

(defun emit-linux-ppc-entry-common (mbuf 64-bit-p load-addr)
  "Emit the Linux/PowerPC userspace entry into MBUF (cross.lisp's mvm-buffer).

   Builds a ppc-buffer and appends its bytes, as the RISC-V stubs do, so the
   encoders are the ones the translator and the ladder exercise rather than a
   second hand-written copy.  *PPC-64-BIT* is bound around the body because the
   width-dispatching emitters (store-word, shift-right-arith-imm) read it, and a
   stub that emitted STD on a 32-bit machine would be an illegal instruction.

   On entry Linux has r1 pointing at argc, with argv immediately above it — so
   argv[1] is at 2 words up: 8 on the 32-bit port, 16 on the 64-bit one.

   This is the minimum a hosted image needs before kernel-main: argc where the
   Lisp side looks for it, a mapped heap, the MVM allocation registers, and the
   Cheney metadata.  It installs NO signal handlers (a fault is a clean SIGSEGV,
   not a longjmp into handler-case) and sets up no JIT arena — there is no PPC
   JIT.  Both are deliberate omissions with nothing depending on them."
  (let* ((*ppc-64-bit* 64-bit-p)
         (ws (if 64-bit-p 8 4))
         (buf (make-ppc-buffer)))
    (declare (ignorable load-addr))
    ;; --- argc / argv[1] into registers the syscall will not clobber.  r14/r15
    ;;     are V4/V5, callee-saved, and nothing has run yet to hold anything.
    (ppc-emit-load-word buf +ppc-r14+ +ppc-r1+ 0)          ; argc
    (ppc-emit-load-word buf +ppc-r15+ +ppc-r1+ (* 2 ws))   ; argv[1]
    ;; --- mmap FIRST.  The argc slot below is INSIDE this mapping and this port
    ;;     asks for no BSS, so writing it before the mmap is an immediate
    ;;     SIGSEGV rather than merely fragile.  (The RV64 port survived that
    ;;     ordering only on the accident of a 896 MB BSS; the ARM32 port, whose
    ;;     ELF does not ask for one, measured the fault.)
    ;;     mmap(addr, len, PROT_READ|WRITE, MAP_PRIVATE|ANON|FIXED, -1, 0)
    (ppc-emit-li buf +ppc-r3+ +linux-ppc-heap-addr+)
    (ppc-emit-li buf +ppc-r4+ (+ +linux-ppc-heap-size+ +linux-ppc-gc-guard+))
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
    (ppc-emit-li buf +ppc-r11+ (+ +linux-ppc-heap-addr+ #x200))
    (ppc-emit-stw buf +ppc-r14+ +ppc-r11+ 0)
    ;; --- MVM allocation registers.  VA=r19, VL=r20, VN=r21 (see target.lisp).
    (ppc-emit-li buf +ppc-r11+ +linux-ppc-heap-alloc-start+)
    (ppc-emit-add buf +ppc-r19+ +ppc-r16+ +ppc-r11+)
    (ppc-emit-li buf +ppc-r11+ +linux-ppc-gc-midpoint+)
    (ppc-emit-add buf +ppc-r20+ +ppc-r16+ +ppc-r11+)
    (ppc-emit-li buf +ppc-r21+ 0)                ; NIL = 0, as on every target
    ;; --- Cheney metadata at the heap-relative block, RAW addresses.
    (ppc-emit-li buf +ppc-r11+ (+ +linux-ppc-heap-addr+ #x40))
    (ppc-emit-store-word buf +ppc-r19+ +ppc-r11+ 0)             ; from_start
    (ppc-emit-store-word buf +ppc-r20+ +ppc-r11+ ws)            ; to_start
    (ppc-emit-li buf +ppc-r12+ (- +linux-ppc-gc-midpoint+
                                  +linux-ppc-heap-alloc-start+))
    (ppc-emit-store-word buf +ppc-r12+ +ppc-r11+ (* 2 ws))      ; space_size
    (ppc-emit-store-word buf +ppc-r1+  +ppc-r11+ (* 3 ws))      ; stack_base
    (ppc-emit-li buf +ppc-r12+ 0)
    (ppc-emit-store-word buf +ppc-r12+ +ppc-r11+ (* 4 ws))      ; gc_count
    ;; --- VFP (r31) needs to point at a frame before the first prologue runs.
    (ppc-emit-mr buf +ppc-r31+ +ppc-r1+)
    ;; --- fall through to translated native code
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
