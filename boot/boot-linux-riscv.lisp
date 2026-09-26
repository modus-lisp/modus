;;;; boot-linux-riscv.lisp — Linux RISC-V 64 ELF entry for Modus
;;;;
;;;; Counterpart to boot-linux-x64.lisp and boot-linux-aarch64.lisp: produces a
;;;; userspace ELF64-LE RV64 executable.  Runs under qemu-riscv64-static, and on
;;;; real hardware (VisionFive, StarFive, SiFive) unchanged.
;;;;
;;;; WHY HOSTED FIRST.  Bare metal needs a boot stub, an FPU enable, a
;;;; hand-written collector bring-up and a UART driver before a REPL can say
;;;; anything.  Hosted needs none of it: the kernel supplies mapped memory
;;;; through mmap, write(2) is the console, and a fault is a signal rather than a
;;;; triple fault.  Everything above the syscall boundary is then shared with the
;;;; two working hosted ports.
;;;;
;;;; THE RISC-V SYSCALL ABI IS NOT x86's.  RV64 uses the asm-generic numbering
;;;; (include/uapi/asm-generic/unistd.h), so the numbers below are NOT the x86-64
;;;; ones and are NOT the i386 ones — read = 63 where x86-64 says 0, write = 64
;;;; where x86-64 says 1.  `ecall' takes the number in a7 and arguments in
;;;; a0..a5, returning in a0; there is no separate errno register, a negative
;;;; return IS the error.

(in-package :modus.mvm)

;;; ============================================================
;;; Linux RISC-V 64 Constants
;;; ============================================================

(defconstant +linux-riscv-load-addr+ #x400000
  "Standard Linux ELF load address; well clear of mmap_min_addr.")

(defconstant +linux-riscv-heap-addr+ #x10000000
  "Heap base.  DELIBERATELY the same #x10000000 the x64 and AArch64 hosted
   ports use: the shared CL runtime and gc.lisp hold the GC metadata slots at
   fixed absolute addresses just below it, so a port that moves the heap has to
   move those too.  Note this is a HOSTED address and has nothing to do with
   bare-metal riscv, where #x10000000 is the NS16550 UART's MMIO window —
   under Linux that window is not mapped into our address space at all.")

(defconstant +linux-riscv-heap-size+ #x38000000)   ; 896 MB
(defconstant +linux-riscv-heap-alloc-start+ #x2000
  "Where the bump allocator starts, as an offset into the heap.

   #x200 WAS WRONG AND IT COST A DAY.  That is exactly where this stub writes
   ARGC, and it is inside the whole fixed block the shared runtime owns: metadata
   #x40, globals #x80, MV #x90..#x138, intern tables #x148/#x170, nargs #x150,
   the jmpbuf #x180..#x1FF, argc #x200, argv #x208/#x248, the handler stack
   #x400..#xC0F, the convention slots #xA00, the per-CPU mode word #xFF8.  An
   allocator starting at #x200 allocates ON TOP OF ALL OF IT.

   I FLAGGED THIS WHEN WRITING THE RV32 PORT and left it, recording it as \"latent
   only because no payload there reads argc yet\".  The real CL image reads all of
   it, and the failure was nothing like argc: the GLOBALS TABLE got allocated at
   #x10000280, the runtime then overwrote it, and an alist cursor came back holding
   T — neither NIL (so the probe loop never exited) nor a cons (so its
   consp-guarded advance was skipped), spinning forever without even faulting.
   Found with gdb-multiarch over qemu's gdbstub: the cursor register held
   #xDEAD1009 and the table pointer was #x10000281.

   #x2000 matches what boot-linux-riscv32.lisp already does, and leaves
   #x1000..#x2000 for the CLI's cstr/io scratch.")
(defconstant +linux-riscv-gc-midpoint+ #x1C000000)
(defconstant +linux-riscv-gc-guard+ #x1000000
  "16 MB past the second semispace, same as the other hosted ports: :gc-check
   tests the alloc pointer AFTER the bump, so a large allocation can overshoot
   the limit before anyone looks.")

;;; RV64 syscall numbers (asm-generic).  Named rather than inlined because the
;;; x86 numbers are muscle memory and wrong here.
(defconstant +rv-sys-read+   63)
(defconstant +rv-sys-write+  64)
(defconstant +rv-sys-exit+   93)
(defconstant +rv-sys-mmap+   222)

;;; ============================================================
;;; ELF wrapper
;;; ============================================================

(defun wrap-in-elf64-le-riscv (raw-bytes load-addr &key function-table
                                                        native-image-offset
                                                        native-code-length)
  "ELF64-LE RV64 executable.  Differs from the AArch64 wrapper in two fields —
   e_machine 243 (EM_RISCV) and a 4K p_align — so it delegates rather than
   duplicating 125 lines of header, section-table and symbol emission."
  (wrap-in-elf64-le-aa64 raw-bytes load-addr
                         :function-table function-table
                         :native-image-offset native-image-offset
                         :native-code-length native-code-length
                         :machine 243
                         :page-align #x1000
                         :bss-size +linux-riscv-heap-size+))

;;; ============================================================
;;; Userspace entry stub
;;; ============================================================

(defun emit-linux-riscv-entry (mbuf)
  "Emit the RV64 Linux userspace entry into BUF (an MVM-BUFFER).

   BUF is the generic mvm-buffer cross.lisp hands every :entry-fn, but the
   instruction emitters this stub wants (RV-EMIT-LI and friends) write into an
   rv-buffer.  Rather than add a parallel set of raw encoders — boot-riscv.lisp
   took that route and has only five — build an rv-buffer and append its bytes.
   The emitters are then exactly the ones the translator uses and the arch
   ladder exercises, instead of a second, untested copy.

   On entry Linux has SP pointing at argc, then argv[0..], so argc is [sp+0]
   and argv[1] is [sp+16].  This stub keeps to the minimum a hosted image needs
   before kernel-main: argc/argv where the Lisp side looks for them, a mapped
   heap, the MVM allocation registers, and the Cheney metadata.  It does NOT
   set up a JIT arena — there is no RISC-V JIT — and it does not install signal
   handlers, so a fault is a clean SIGSEGV rather than a longjmp into
   handler-case.  Both are deliberate omissions with nothing depending on them
   yet, not oversights."
  ;; BUF below is the rv-buffer; MBUF is the caller's mvm-buffer.
  (let ((buf (make-rv-buffer)))
  ;; --- argc / argv off the stack, into callee-saved regs that survive ecall
  (rv-emit-ld buf +rv-s2+ +rv-sp+ 0)     ; s2 = argc
  (rv-emit-ld buf +rv-s3+ +rv-sp+ 16)    ; s3 = argv[1]
  ;; --- mmap FIRST: the argc slot at #x10000200 is INSIDE the heap mapping.
  ;;     This port wrote it before mmap and worked, but only by accident — its
  ;;     ELF asks for a 896 MB BSS that happens to cover the address.  The ARM32
  ;;     port, whose ELF does not, SIGSEGV'd at si_addr=0x10000200 on the same
  ;;     ordering.  Map before writing and neither depends on the accident.
  ;; --- mmap the heap: MAP_FIXED so save-image can rely on the address
  ;;     mmap(addr, len, PROT_READ|WRITE, MAP_PRIVATE|ANON|FIXED, -1, 0)
  (rv-emit-li buf +rv-a0+ +linux-riscv-heap-addr+)
  (rv-emit-li buf +rv-a1+ (+ +linux-riscv-heap-size+ +linux-riscv-gc-guard+
                                ;; + the collector's START and CONS bitmaps
                                ;; (translate-riscv's allocation-bitmaps header)
                                (* 2 (rv-hosted-bitmap-bytes +linux-riscv-heap-size+))))
  (rv-emit-li buf +rv-a2+ 3)             ; PROT_READ|PROT_WRITE
  (rv-emit-li buf +rv-a3+ #x32)          ; MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED
  (rv-emit-li buf +rv-a4+ -1)            ; fd
  (rv-emit-li buf +rv-a5+ 0)             ; offset
  (rv-emit-li buf +rv-a7+ +rv-sys-mmap+)
  (rv-emit-ecall buf)
  ;; s4 = heap base as returned.  MAP_FIXED means it equals the request, but
  ;; using the RETURN VALUE keeps a failed mapping visible as a wild pointer
  ;; rather than silently writing to an address nobody mapped.
  (rv-emit-mv buf +rv-s4+ +rv-a0+)
  ;; --- NOW argc, inside the mapping the line above just made.
  (rv-emit-li buf +rv-t0+ #x10000200)
  (rv-emit-sw buf +rv-s2+ +rv-t0+ 0)
  ;; --- MVM allocation registers: s8 = alloc pointer, s9 = limit, s10 = NIL
  (rv-emit-li buf +rv-t0+ +linux-riscv-heap-alloc-start+)
  (rv-emit-add buf +rv-s8+ +rv-s4+ +rv-t0+)
  ;; VL stops +RV-GC-OVERSHOOT-MARGIN+ short of the semispace's end: :gc-check
  ;; cannot see the size of the allocation that follows it (translate-riscv's
  ;; collector header explains the margin).
  (rv-emit-li buf +rv-t0+ (- +linux-riscv-gc-midpoint+ +rv-gc-overshoot-margin+))
  (rv-emit-add buf +rv-s9+ +rv-s4+ +rv-t0+)
  ;; VN = NIL.  #xDEAD0001 (+NIL-VALUE+), NOT zero.  The three hosted ports that
  ;; run the real CL image — x64, AArch64, i386 — all load this immediate, and the
  ;; compiler knows the value: mvm/compiler.lisp defines +NIL-VALUE+ and reasons
  ;; about its low byte (0x01) when it classifies immediates.  A minimal ladder
  ;; payload cannot tell the difference, because every NIL test compares against
  ;; THIS REGISTER and so is self-consistent at any value; the real image can,
  ;; the moment anything meets a baked NIL literal.
  (rv-emit-li buf +rv-s10+ +nil-value+)
  ;; --- THE NIL PAGE.  (car nil) and (cdr nil) DEREFERENCE #xDEAD0000.
  ;;
  ;; +NIL-VALUE+ is #xDEAD0001 and its low nibble is 1 — the CONS TAG.  That is
  ;; deliberate: it lets car/cdr of NIL be a plain load rather than a guarded one,
  ;; at the price of needing a page at #xDEAD0000.  mvm/build-x64-linux.lisp maps
  ;; exactly this page with the comment "car nil must not segfault", and
  ;; mvm/mvm-eval.lisp names the failure shape: "unmapped #xDEAD0000 header ->
  ;; SIGSEGV".
  ;;
  ;; The hosted x64 CLI does NOT map it and works, so this is not universal — it
  ;; depends on which paths through the shared source an image actually takes.
  ;; This port takes one that does: measured, the real CL image faults at
  ;; si_addr=0xdead0000, which is precisely NIL-1, i.e. RISC-V's `ld rd,-1(rs)'.
  ;; Four kilobytes is cheaper than auditing every car in 3 million instructions.
  ;;
  ;; FILLED WITH NIL, not zeroed, so (car nil) and (cdr nil) answer NIL rather
  ;; than a fixnum 0 that would then be walked as a list.
  (rv-emit-li buf +rv-a0+ #xDEAD0000)
  (rv-emit-li buf +rv-a1+ 4096)
  (rv-emit-li buf +rv-a2+ 3)                   ; PROT_READ|PROT_WRITE
  (rv-emit-li buf +rv-a3+ #x32)                ; MAP_PRIVATE|ANONYMOUS|FIXED
  (rv-emit-li buf +rv-a4+ -1)
  (rv-emit-li buf +rv-a5+ 0)
  (rv-emit-li buf +rv-a7+ +rv-sys-mmap+)
  (rv-emit-ecall buf)
  ;; Fill it with NIL: 512 doublewords from the RETURNED address.
  (rv-emit-mv buf +rv-t1+ +rv-a0+)
  (rv-emit-li buf +rv-t2+ 512)
  ;; loop: sd s10,0(t1); addi t1,t1,8; addi t2,t2,-1; bne t2,x0,-12
  (rv-emit-sd buf +rv-s10+ +rv-t1+ 0)
  (rv-emit-addi buf +rv-t1+ +rv-t1+ 8)
  (rv-emit-addi buf +rv-t2+ +rv-t2+ -1)
  (rv-emit-bne buf +rv-t2+ +rv-x0+ -12)
  ;; --- Cheney metadata at the shared absolute slots 0x10000040..0x10000060.
  ;;     RAW addresses, matching what the native collector expects (the
  ;;     address<<1 convention gc.lisp once needed is gone).
  (rv-emit-li buf +rv-t1+ #x10000040)
  (rv-emit-sd buf +rv-s8+ +rv-t1+ 0)     ; [0x40] from_start
  (rv-emit-li buf +rv-t0+ +linux-riscv-gc-midpoint+)   ; NOT VL: VL stops a
  (rv-emit-add buf +rv-t0+ +rv-s4+ +rv-t0+)             ; margin short of it
  (rv-emit-sd buf +rv-t0+ +rv-t1+ 8)     ; [0x48] to_start
  (rv-emit-li buf +rv-t0+ (- +linux-riscv-gc-midpoint+
                             +linux-riscv-heap-alloc-start+))
  (rv-emit-sd buf +rv-t0+ +rv-t1+ 16)    ; [0x50] space_size
  (rv-emit-mv buf +rv-t0+ +rv-sp+)
  (rv-emit-sd buf +rv-t0+ +rv-t1+ 24)    ; [0x58] stack_base
  (rv-emit-li buf +rv-t0+ 0)
  (rv-emit-sd buf +rv-t0+ +rv-t1+ 32)    ; [0x60] gc_count
  ;; --- fall through to translated native code
  (loop for b across (rv-buffer-to-bytes buf)
          do (mvm-emit-byte mbuf b))))

;;; ============================================================
;;; Boot descriptor
;;; ============================================================

(defun linux-riscv-boot-descriptor ()
  "Boot descriptor for the hosted RV64 image."
  (list :arch :riscv64
        :entry-fn #'emit-linux-riscv-entry
        :elf-format :linux-riscv
        :elf-machine 243
        :elf-class 64
        :load-addr +linux-riscv-load-addr+
        :heap-base +linux-riscv-heap-addr+
        :cons-base +linux-riscv-heap-addr+
        :endianness :little))
