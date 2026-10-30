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

(defconstant +linux-arm32-heap-size+ #xF800000
  "The LARGEST heap: 248 MB, ending (with the guard) at #x1FC00000 -- RV32's
   geometry, for RV32's reason: a MEM-REF address travels as a TAGGED 30-bit
   fixnum, so nothing the Lisp side addresses may sit at or above #x20000000.
   It was 384 MB, ending at #x29000000.

   The HOSTED image chooses its size at startup, as 68k and PowerPC do:
   --dynamic-space-size N[KB|MB|GB] (SBCL's flag, MB when unsuffixed), else
   half of physical RAM (sysinfo(2)), capped at +LINUX-ARM32-HEAP-DEFAULT-CAP+,
   clamped to [1 MB, this].  The kind map stays where this size puts it
   (#x1FC00000), so the collector's bias is still a constant; the stub maps
   only the S/16 bytes a heap of size S needs.  The bare CL image
   (boot-arm32-cl.lisp) still uses this size, fixed.")
(defconstant +linux-arm32-heap-default-cap+ #xF800000
  "With no --dynamic-space-size, half of RAM, but no more than this.")
(defconstant +linux-arm32-heap-min+ #x100000)
(defconstant +linux-arm32-sys-sysinfo+ 116)
(defconstant +linux-arm32-heap-alloc-start+ #x28000
  "Past the fixed runtime block (#x0000-#x0FFF), the CLI scratch pages
   (#x1000-#x8FFF), the staged argv (#x9000-#x1DFFF) and the handler-frame
   stack (#x20000-#x21FDF, translate-arm32.lisp ARM32-HFRAMES-ADDR), as on RV32.")
(defconstant +linux-arm32-gc-midpoint+ #x7C00000)
(defconstant +linux-arm32-gc-guard+ #x400000
  "4 MB past the heap: :gc-check tests the alloc pointer before an allocation
   whose size it does not know, so a large one can overshoot the limit.")

;;; Staged argv/envp, below 2^29 -- RV32's layout (see boot-linux-riscv32.lisp).
;;; qemu-arm puts the initial stack near #x40800000, which a tagged MEM-REF
;;; address cannot express, so the stub copies the vectors down.
(defconstant +linux-arm32-argv-ptrs+       #x10009000)
(defconstant +linux-arm32-argv-ptrs-end+   #x1000A000)
(defconstant +linux-arm32-argv-arena+      #x1000A000)
(defconstant +linux-arm32-argv-arena-end+  #x1001E000)

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

(defun emit-linux-arm32-choose-heap-size (buf)
  "Leave the heap size in BYTES in r8.  argc is in r11; argv[0] is at sp+4.
   --dynamic-space-size first, else half of sysinfo(2)'s RAM, capped; in KB so
   nothing overflows, clamped, rounded to 128 KB.  r0-r3, r7, r12 are scratch."
  (let ((top (mvm-make-label)) (next (mvm-make-label)) (no-flag (mvm-make-label))
        (digits (mvm-make-label)) (suffix (mvm-make-label)) (unit-g (mvm-make-label))
        (have-kb (mvm-make-label)) (cap-kb (mvm-make-label)) (ram-small (mvm-make-label))
        (ram-done (mvm-make-label)) (clamp (mvm-make-label))
        (cap (ash +linux-arm32-heap-default-cap+ -10))
        (kmax (ash +linux-arm32-heap-size+ -10))
        (kmin (ash +linux-arm32-heap-min+ -10))
        (flag "--dynamic-space-size"))
    (flet ((bc (cc l) (arm32-b-cond buf cc l))
           (imm (r v) (arm32-load-imm32 buf r v)))
      (arm32-mov-imm buf +arm-r0+ 0 1)                          ; i = 1
      (arm32-emit-label buf top)
      (arm32-cmp buf +arm-r0+ +arm-r11+)
      (bc +arm-cc-ge+ no-flag)
      (arm32-dp-reg buf +arm-dp-add+ +arm-r1+ +arm-sp+ +arm-r0+
                    :shift-type +arm-shift-lsl+ :shift-amt 2)   ; r1 = sp + 4i
      (arm32-ldr buf +arm-r2+ +arm-r1+ 4)                        ; argv[i]
      (loop for c across flag for k from 0
            do (arm32-ldrb buf +arm-r3+ +arm-r2+ k)
               (arm32-cmp-imm buf +arm-r3+ 0 (char-code c))
               (bc +arm-cc-ne+ next))
      (arm32-ldrb buf +arm-r3+ +arm-r2+ (length flag))
      (arm32-cmp-imm buf +arm-r3+ 0 0)
      (bc +arm-cc-ne+ next)
      (arm32-add-imm buf +arm-r3+ +arm-r0+ 0 1)
      (arm32-cmp buf +arm-r3+ +arm-r11+)
      (bc +arm-cc-ge+ no-flag)
      (arm32-ldr buf +arm-r2+ +arm-r1+ 8)                        ; argv[i+1]
      (arm32-mov-imm buf +arm-r8+ 0 0)
      (arm32-emit-label buf digits)
      (arm32-ldrb buf +arm-r3+ +arm-r2+ 0)
      (arm32-add-imm buf +arm-r2+ +arm-r2+ 0 1)
      (arm32-cmp-imm buf +arm-r3+ 0 48)
      (bc +arm-cc-cc+ suffix)
      (arm32-cmp-imm buf +arm-r3+ 0 57)
      (bc +arm-cc-hi+ suffix)
      (arm32-sub-imm buf +arm-r3+ +arm-r3+ 0 48)
      (imm +arm-r12+ #x100000)                                   ; saturate
      (arm32-cmp buf +arm-r8+ +arm-r12+)
      (bc +arm-cc-cs+ digits)
      (arm32-dp-reg buf +arm-dp-add+ +arm-r12+ +arm-r8+ +arm-r8+
                    :shift-type +arm-shift-lsl+ :shift-amt 2)   ; 5n
      (arm32-dp-reg buf +arm-dp-add+ +arm-r8+ +arm-r3+ +arm-r12+
                    :shift-type +arm-shift-lsl+ :shift-amt 1)   ; 10n + d
      (arm32-b buf digits)
      (arm32-emit-label buf suffix)
      (arm32-bic-imm buf +arm-r3+ +arm-r3+ 0 #x20)               ; upper-case it
      (arm32-cmp-imm buf +arm-r3+ 0 75)                          ; 'K'
      (bc +arm-cc-eq+ have-kb)
      (arm32-cmp-imm buf +arm-r3+ 0 71)                          ; 'G'
      (bc +arm-cc-eq+ unit-g)
      (arm32-lsl-imm buf +arm-r8+ +arm-r8+ 10)                   ; MB -> KB
      (arm32-b buf have-kb)
      (arm32-emit-label buf unit-g)
      (arm32-cmp-imm buf +arm-r8+ 0 0)
      (bc +arm-cc-eq+ have-kb)
      (imm +arm-r8+ kmax)
      (arm32-b buf have-kb)
      (arm32-emit-label buf next)
      (arm32-add-imm buf +arm-r0+ +arm-r0+ 0 1)
      (arm32-b buf top)
      ;; ---- no flag: half of RAM, capped.  Preset totalram huge and
      ;;      mem_unit 1, so a failed sysinfo lands on the cap.
      (arm32-emit-label buf no-flag)
      (arm32-sub-imm buf +arm-sp+ +arm-sp+ 0 128)
      (imm +arm-r12+ #x7FFFFFFF)
      (arm32-str buf +arm-r12+ +arm-sp+ 16)
      (arm32-mov-imm buf +arm-r12+ 0 1)
      (arm32-str buf +arm-r12+ +arm-sp+ 52)
      (arm32-mov buf +arm-r0+ +arm-sp+)
      (imm +arm-r7+ +linux-arm32-sys-sysinfo+)
      (arm32-svc buf)
      (arm32-ldr buf +arm-r8+ +arm-sp+ 16)                       ; totalram
      (arm32-ldr buf +arm-r1+ +arm-sp+ 52)                       ; mem_unit
      (imm +arm-r12+ 1024)
      (arm32-cmp buf +arm-r1+ +arm-r12+)
      (bc +arm-cc-cc+ ram-small)
      (imm +arm-r12+ #x100000)
      (arm32-cmp buf +arm-r8+ +arm-r12+)
      (bc +arm-cc-cs+ cap-kb)
      (arm32-lsr-imm buf +arm-r1+ +arm-r1+ 10)
      (arm32-mul buf +arm-r12+ +arm-r8+ +arm-r1+)
      (arm32-mov buf +arm-r8+ +arm-r12+)
      (arm32-b buf ram-done)
      (arm32-emit-label buf ram-small)
      (arm32-lsr-imm buf +arm-r8+ +arm-r8+ 10)
      (arm32-mul buf +arm-r12+ +arm-r8+ +arm-r1+)
      (arm32-mov buf +arm-r8+ +arm-r12+)
      (arm32-emit-label buf ram-done)
      (arm32-lsr-imm buf +arm-r8+ +arm-r8+ 1)                    ; half
      (imm +arm-r12+ cap)
      (arm32-cmp buf +arm-r8+ +arm-r12+)
      (bc +arm-cc-ls+ clamp)
      (arm32-emit-label buf cap-kb)
      (imm +arm-r8+ cap)
      (arm32-emit-label buf clamp)
      (arm32-add-imm buf +arm-sp+ +arm-sp+ 0 128)
      ;; ---- clamp, round to 128 KB, KB -> bytes ----
      (arm32-emit-label buf have-kb)
      (imm +arm-r12+ kmin)
      (arm32-cmp buf +arm-r8+ +arm-r12+)
      (arm32-mov-cond buf +arm-cc-cc+ +arm-r8+ +arm-r12+)
      (imm +arm-r12+ kmax)
      (arm32-cmp buf +arm-r8+ +arm-r12+)
      (arm32-mov-cond buf +arm-cc-hi+ +arm-r8+ +arm-r12+)
      (arm32-lsr-imm buf +arm-r8+ +arm-r8+ 7)
      (arm32-lsl-imm buf +arm-r8+ +arm-r8+ 17))))

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
    ;; LDR r11, [sp, #0] — argc, in a register NO syscall below uses.  It was
    ;; r4 -- which mmap2 takes as its fd (-1) -- so the "argc" stored after the
    ;; mapping was always -1.  Harmless while only ladder images ran (none reads
    ;; argc); fatal to a CLI.
    (arm32-ldr buf +arm-r11+ +arm-sp+ 0)
    ;; r8 = the heap size, chosen now (see +LINUX-ARM32-HEAP-SIZE+).  Every
    ;; syscall below preserves r8 (EABI returns in r0 only).
    (emit-linux-arm32-choose-heap-size buf)
    ;; MMAP FIRST.  The argc slot at #x10000200 is INSIDE the heap mapping, so
    ;; writing it before the mapping exists is a SIGSEGV — measured exactly
    ;; that, si_addr=0x10000200.  (The RISC-V port had the same ordering and
    ;; survived only because its ELF asks for a 896 MB BSS that happens to
    ;; cover the address; both ports now map before they write, so neither
    ;; depends on that accident.)
    ;; mmap2(addr, len, PROT_READ|WRITE, MAP_PRIVATE|ANON|FIXED, -1, 0)
    ;; First the collector's KIND map (a byte per 16-byte granule;
    ;; translate-arm32's hosted-collector header) at its FIXED address past the
    ;; largest heap, only S/16 long; then the heap itself, S + guard.
    (arm32-load-imm32 buf +arm-r0+ (+ +linux-arm32-heap-addr+ +linux-arm32-heap-size+
                                      +linux-arm32-gc-guard+))
    (arm32-lsr-imm buf +arm-r1+ +arm-r8+ 4)
    (arm32-mov-imm buf +arm-r2+ 0 3)
    (arm32-load-imm32 buf +arm-r3+ #x32)
    (arm32-load-imm32 buf +arm-r4+ #xFFFFFFFF)
    (arm32-mov-imm buf +arm-r5+ 0 0)
    (arm32-load-imm32 buf +arm-r7+ +arm-linux-sys-mmap2+)
    (arm32-svc buf)
    (arm32-load-imm32 buf +arm-r0+ +linux-arm32-heap-addr+)
    (arm32-load-imm32 buf +arm-r1+ +linux-arm32-gc-guard+)
    (arm32-add buf +arm-r1+ +arm-r1+ +arm-r8+)
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
    (arm32-str buf +arm-r11+ +arm-r5+ 0)
    ;; THE NIL PAGE: (car nil) / (cdr nil) are plain loads from #xDEAD0000, so it
    ;; must be mapped and NIL-filled, as on RV32/RV64.
    (arm32-load-imm32 buf +arm-r0+ #xDEAD0000)
    (arm32-load-imm32 buf +arm-r1+ 4096)
    (arm32-mov-imm buf +arm-r2+ 0 3)
    (arm32-load-imm32 buf +arm-r3+ #x32)
    (arm32-load-imm32 buf +arm-r4+ #xFFFFFFFF)
    (arm32-mov-imm buf +arm-r5+ 0 0)
    (arm32-load-imm32 buf +arm-r7+ +arm-linux-sys-mmap2+)
    (arm32-svc buf)
    (arm32-load-imm32 buf +arm-r12+ +nil-value+)
    (arm32-load-imm32 buf +arm-r1+ 1024)
    (let ((fill (incf *mvm-label-counter*)))
      (arm32-emit-label buf fill)
      (arm32-str buf +arm-r12+ +arm-r0+ 0)
      (arm32-add-imm buf +arm-r0+ +arm-r0+ 0 4)
      (arm32-sub-imm buf +arm-r1+ +arm-r1+ 0 1)
      (arm32-cmp-imm buf +arm-r1+ 0 0)
      (arm32-b-cond buf +arm-cc-ne+ fill))
    ;; STAGE argv/envp at +linux-arm32-argv-ptrs+ / -arena+.  r0 = source slot
    ;; (sp+4 = argv[0]), r1 = destination slot, r2 = arena cursor, r3 = NULLs
    ;; still to copy (argv's then envp's), r5/r12 = the two regions' limits,
    ;; r7 = the string being copied, lr = byte.  Stops early, leaving both
    ;; vectors terminated, rather than overrun either region.
    (let ((top (incf *mvm-label-counter*)) (copy (incf *mvm-label-counter*))
          (cloop (incf *mvm-label-counter*)) (full (incf *mvm-label-counter*))
          (done (incf *mvm-label-counter*)))
      (arm32-add-imm buf +arm-r0+ +arm-sp+ 0 4)
      (arm32-load-imm32 buf +arm-r1+ +linux-arm32-argv-ptrs+)
      (arm32-load-imm32 buf +arm-r2+ +linux-arm32-argv-arena+)
      (arm32-mov-imm buf +arm-r3+ 0 2)
      (arm32-load-imm32 buf +arm-r5+ (- +linux-arm32-argv-ptrs-end+ 8))
      (arm32-load-imm32 buf +arm-r12+ (- +linux-arm32-argv-arena-end+ 4096))
      (arm32-emit-label buf top)
      (arm32-ldr buf +arm-r7+ +arm-r0+ 0)
      (arm32-add-imm buf +arm-r0+ +arm-r0+ 0 4)
      (arm32-cmp-imm buf +arm-r7+ 0 0)
      (arm32-b-cond buf +arm-cc-ne+ copy)
      ;; a NULL: copy it; stop after the second
      (arm32-mov-imm buf +arm-lr+ 0 0)
      (arm32-str buf +arm-lr+ +arm-r1+ 0)
      (arm32-add-imm buf +arm-r1+ +arm-r1+ 0 4)
      (arm32-sub-imm buf +arm-r3+ +arm-r3+ 0 1)
      (arm32-cmp-imm buf +arm-r3+ 0 0)
      (arm32-b-cond buf +arm-cc-ne+ top)
      (arm32-b buf done)
      (arm32-emit-label buf copy)
      (arm32-cmp buf +arm-r1+ +arm-r5+)
      (arm32-b-cond buf +arm-cc-cs+ full)
      (arm32-cmp buf +arm-r2+ +arm-r12+)
      (arm32-b-cond buf +arm-cc-cs+ full)
      (arm32-str buf +arm-r2+ +arm-r1+ 0)            ; ptr[k] = arena
      (arm32-add-imm buf +arm-r1+ +arm-r1+ 0 4)
      (arm32-emit-label buf cloop)
      (arm32-ldrb buf +arm-lr+ +arm-r7+ 0)
      (arm32-strb buf +arm-lr+ +arm-r2+ 0)
      (arm32-add-imm buf +arm-r7+ +arm-r7+ 0 1)
      (arm32-add-imm buf +arm-r2+ +arm-r2+ 0 1)
      (arm32-cmp-imm buf +arm-lr+ 0 0)
      (arm32-b-cond buf +arm-cc-ne+ cloop)
      (arm32-b buf top)
      (arm32-emit-label buf full)
      (arm32-mov-imm buf +arm-lr+ 0 0)
      (arm32-str buf +arm-lr+ +arm-r1+ 0)
      (arm32-str buf +arm-lr+ +arm-r1+ 4)
      (arm32-emit-label buf done))
    ;; From the size in r8: lr = midpoint (S/2), r12 = space_size, r1 = margin
    ;; = min(1 MB, space_size/4) 16-aligned -- the collector's rule exactly.
    (arm32-lsr-imm buf +arm-lr+ +arm-r8+ 1)
    (arm32-load-imm32 buf +arm-r12+ +linux-arm32-heap-alloc-start+)
    (arm32-sub buf +arm-r12+ +arm-lr+ +arm-r12+)
    (arm32-lsr-imm buf +arm-r1+ +arm-r12+ 2)
    (arm32-bic-imm buf +arm-r1+ +arm-r1+ 0 15)
    (arm32-load-imm32 buf +arm-r2+ +arm32-gc-overshoot-margin+)
    (arm32-cmp buf +arm-r1+ +arm-r2+)
    (arm32-mov-cond buf +arm-cc-hi+ +arm-r1+ +arm-r2+)
    ;; MVM registers: r9 = alloc pointer (VA), r10 = limit (VL), r8 = NIL
    (arm32-load-imm32 buf +arm-r2+ +linux-arm32-heap-alloc-start+)
    (arm32-add buf +arm-r9+ +arm-r6+ +arm-r2+)
    ;; VL: the first semispace's end less the overshoot margin.
    (arm32-add buf +arm-r10+ +arm-r6+ +arm-lr+)
    (arm32-sub buf +arm-r10+ +arm-r10+ +arm-r1+)
    ;; Cheney metadata at the shared absolute slots, RAW addresses.
    (arm32-load-imm32 buf +arm-r2+ #x10000040)
    (arm32-str buf +arm-r9+ +arm-r2+ 0)           ; [0x40] from_start
    ;; to_start = heap + midpoint, computed here: NOT r10, which is VL and
    ;; stops the collector's margin short of it (storing r10 put to-space 1 MB
    ;; inside from-space, and the second collection corrupted the heap).
    (arm32-add buf +arm-r3+ +arm-r6+ +arm-lr+)
    (arm32-str buf +arm-r3+ +arm-r2+ 8)           ; [0x48] to_start
    (arm32-str buf +arm-r12+ +arm-r2+ 16)         ; [0x50] space_size
    (arm32-mov buf +arm-r3+ +arm-sp+)
    (arm32-str buf +arm-r3+ +arm-r2+ 24)          ; [0x58] stack_base
    (arm32-mov-imm buf +arm-r3+ 0 0)
    (arm32-str buf +arm-r3+ +arm-r2+ 32)          ; [0x60] gc_count
    ;; VN = NIL = +NIL-VALUE+ (#xDEAD0001), not zero -- see boot-linux-riscv.lisp.
    ;; LAST: r8 carried the heap size until here.
    (arm32-load-imm32 buf +arm-r8+ +nil-value+)
    ;; fall through to translated native code
    (arm32-resolve-fixups buf)
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
