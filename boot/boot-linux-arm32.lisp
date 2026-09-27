;;;; boot-linux-arm32.lisp — Linux ARM32 (EABI) ELF entry for Modus
;;;;
;;;; Fifth hosted port, after x64, AArch64, i386 and RV64.  Produces a userspace
;;;; ELF32-LE ARM executable: runs under qemu-arm-static and on any 32-bit ARM
;;;; Linux (Raspberry Pi in 32-bit mode, older ARM boards) unchanged.
;;;;
;;;; ARM EABI: `svc #0' with the syscall number in R7 and arguments in R0..R6,
;;;; returning in R0.  The numbers are ARM's own table — close to i386's, NOT
;;;; RV64's — and live in translate-arm32.lisp beside the traps that use them.
;;;;
;;;; MMAP2, not mmap: ARM32 has no plain mmap(2) for userspace here, and mmap2's
;;;; sixth argument is the offset in 4096-byte PAGES.  Zero for an anonymous
;;;; mapping, so the distinction costs nothing — but it is why the number is 192
;;;; rather than 90.

(in-package :modus.mvm)

;;; ============================================================
;;; Linux ARM32 constants
;;; ============================================================

(defconstant +linux-arm32-load-addr+ #x10000
  "ELF load address.  LOW, and deliberately: the heap below must sit at
   #x10000000 to match the shared metadata slots, so the image cannot start
   there.  64 KB clears mmap_min_addr on every configuration seen.")

(defconstant +linux-arm32-heap-addr+ #x10000000
  "Heap base, the same #x10000000 every hosted port uses — the shared CL runtime
   and gc.lisp hold the Cheney metadata at fixed absolute slots just below it.")

(defconstant +linux-arm32-heap-size+ #x18000000)   ; 384 MB
(defconstant +linux-arm32-heap-alloc-start+ #x200)
(defconstant +linux-arm32-gc-midpoint+ #x0C000000)
(defconstant +linux-arm32-gc-guard+ #x1000000
  "16 MB past the second semispace: :gc-check tests the alloc pointer AFTER the
   bump, so a large allocation can overshoot before anyone looks.")

;;; ============================================================
;;; ELF wrapper
;;; ============================================================

(defun wrap-in-elf32-le-arm (raw-bytes load-addr &key bss-end)
  "ELF32-LE ARM executable.  Delegates to WRAP-IN-ELF32-LE-I386 with
   e_machine = 40 (EM_ARM): the ELF32-LE header is identical otherwise, and the
   program-header field order that differs from ELF64 is documented there once
   rather than in each port."
  (wrap-in-elf32-le-i386 raw-bytes load-addr :bss-end bss-end :machine 40))

;;; ============================================================
;;; Userspace entry stub
;;; ============================================================

(defun emit-linux-arm32-entry (mbuf)
  "Emit the ARM32 Linux userspace entry into MBUF (an mvm-buffer).

   As in the RISC-V port, the instruction emitters write into the translator's
   own arm32-buffer, so this builds one and appends its bytes — the emitters are
   then the ones the arch ladder already exercises rather than a second copy.

   Minimum a hosted image needs before kernel-main: argc where the Lisp side
   looks for it, a mapped heap, the MVM allocation registers, and the Cheney
   metadata.  No JIT arena (there is no ARM32 JIT) and no signal handlers, so a
   fault is a clean SIGSEGV rather than a longjmp into handler-case."
  (let ((buf (make-arm32-buffer)))
    ;; On entry SP points at argc, then argv[0..].
    ;; LDR r4, [sp, #0]  — argc into a callee-saved register
    (arm32-ldr buf +arm-r4+ +arm-sp+ 0)
    ;; MMAP FIRST.  The argc slot at #x10000200 is INSIDE the heap mapping, so
    ;; writing it before the mapping exists is a SIGSEGV — measured exactly
    ;; that, si_addr=0x10000200.  (The RISC-V port had the same ordering and
    ;; survived only because its ELF asks for a 896 MB BSS that happens to
    ;; cover the address; both ports now map before they write, so neither
    ;; depends on that accident.)
    ;; mmap2(addr, len, PROT_READ|WRITE, MAP_PRIVATE|ANON|FIXED, -1, 0)
    (arm32-load-imm32 buf +arm-r0+ +linux-arm32-heap-addr+)
    (arm32-load-imm32 buf +arm-r1+ (+ +linux-arm32-heap-size+
                                      +linux-arm32-gc-guard+))
    (arm32-mov-imm buf +arm-r2+ 0 3)              ; PROT_READ|PROT_WRITE
    (arm32-load-imm32 buf +arm-r3+ #x32)          ; PRIVATE|ANONYMOUS|FIXED
    (arm32-load-imm32 buf +arm-r4+ #xFFFFFFFF)    ; fd = -1
    (arm32-mov-imm buf +arm-r5+ 0 0)              ; offset (in pages) = 0
    (arm32-load-imm32 buf +arm-r7+ +arm-linux-sys-mmap2+)
    (arm32-svc buf)
    ;; r6 = heap base AS RETURNED.  Using the return value rather than the
    ;; request keeps a failed mapping visible as a wild pointer instead of
    ;; silently writing to an address nobody mapped.
    (arm32-mov buf +arm-r6+ +arm-r0+)
    ;; NOW the argc slot, inside the mapping that exists as of the line above.
    (arm32-load-imm32 buf +arm-r5+ #x10000200)
    (arm32-str buf +arm-r4+ +arm-r5+ 0)
    ;; MVM registers: r9 = alloc pointer (VA), r10 = limit (VL), r8 = NIL
    (arm32-load-imm32 buf +arm-r12+ +linux-arm32-heap-alloc-start+)
    (arm32-add buf +arm-r9+ +arm-r6+ +arm-r12+)
    (arm32-load-imm32 buf +arm-r12+ +linux-arm32-gc-midpoint+)
    (arm32-add buf +arm-r10+ +arm-r6+ +arm-r12+)
    ;; VN = NIL = +NIL-VALUE+ (#xDEAD0001), not zero — see boot-linux-riscv.lisp.
    ;; ARM32 cannot load it as a rotated immediate, so it goes through the same
    ;; movw/movt pair every other 32-bit constant here uses.
    (arm32-load-imm32 buf +arm-r8+ +nil-value+)
    ;; Cheney metadata at the shared absolute slots, RAW addresses.
    (arm32-load-imm32 buf +arm-lr+ #x10000040)
    (arm32-str buf +arm-r9+ +arm-lr+ 0)           ; [0x40] from_start
    (arm32-str buf +arm-r10+ +arm-lr+ 8)          ; [0x48] to_start
    (arm32-load-imm32 buf +arm-r12+ (- +linux-arm32-gc-midpoint+
                                       +linux-arm32-heap-alloc-start+))
    (arm32-str buf +arm-r12+ +arm-lr+ 16)         ; [0x50] space_size
    (arm32-mov buf +arm-r12+ +arm-sp+)
    (arm32-str buf +arm-r12+ +arm-lr+ 24)         ; [0x58] stack_base
    (arm32-mov-imm buf +arm-r12+ 0 0)
    (arm32-str buf +arm-r12+ +arm-lr+ 32)         ; [0x60] gc_count
    ;; fall through to translated native code
    (loop for b across (arm32-buffer-to-bytes buf)
          do (mvm-emit-byte mbuf b))))

;;; ============================================================
;;; Boot descriptor
;;; ============================================================

(defun linux-arm32-boot-descriptor ()
  "Boot descriptor for the hosted ARM32 image."
  (list :arch :armv7
        :entry-fn #'emit-linux-arm32-entry
        :elf-format :linux-arm32
        :elf-machine 40
        :elf-class 32
        :load-addr +linux-arm32-load-addr+
        :heap-base +linux-arm32-heap-addr+
        :cons-base +linux-arm32-heap-addr+
        :endianness :little))
