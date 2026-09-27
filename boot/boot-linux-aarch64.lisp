;;;; boot-linux-aarch64.lisp — Linux AArch64 ELF entry for Modus
;;;;
;;;; Counterpart to boot-linux-x64.lisp.  Produces a userspace ELF64-LE
;;;; AArch64 executable that runs on real Linux/ARM hardware (Raspberry
;;;; Pi 4/5 64-bit, ARM servers).  Much faster than QEMU emulation for
;;;; iterating on ANSI conformance.

(in-package :modus.mvm)

;;; ============================================================
;;; Linux AArch64 Constants
;;; ============================================================

(defconstant +linux-aarch64-load-addr+ #x400000)
(defconstant +linux-aarch64-fixed-heap-base+ #x2000000000
  "Where the boot stub asks (MAP_FIXED_NOREPLACE) for the heap: above the
   ELF's ~1 GB BSS tail (0x10000000 + heap size), far below the stack and the
   default mmap area.  Every process getting the same base is what lets a
   save-and-die snapshot restore in place (lib/save-image.lisp).")
(defconstant +linux-aarch64-jit-arena-base+ #x3000000000
  "Fixed RWX arena the %mmap-exec-page trap bump-allocates JIT pages (and the
   GC bitmaps) from; see the stub.  Its bump word is 0x10000F58.")
(defconstant +linux-aarch64-jit-arena-size+ #x20000000)   ; 512 MB of VA
(defconstant +linux-aarch64-heap-addr+ #x10000000)
(defconstant +linux-aarch64-heap-size+ #x38000000)   ; 896 MB
(defconstant +linux-aarch64-heap-alloc-start+ #x200)
(defconstant +linux-aarch64-gc-midpoint+ #x1C000000)

(defvar *linux-aarch64-r25-offset* +linux-aarch64-heap-size+
  "Offset from heap base for x25 (VL = alloc limit).  Midpoint with GC.")

(defconstant +linux-aarch64-gc-guard+ #x1000000
  "16 MB — the amount of MAPPED-BUT-UNCOUNTED memory that must lie past the
   TOP semispace's end.  x64 allocates this explicitly as
   +linux-x64-gc-guard+ (c9c6278); i386 omitted it and that was bug B3
   (18b223b), which killed 16 of the 22 ladder libraries.

   WHY: :gc-check tests `VA < VL' BEFORE an allocation whose size it does not
   know, so the alloc following a passing check overshoots VL by up to that
   object's whole size.  In the LOWER semispace the overshoot lands in the
   (mapped) upper one and is harmless.  In the UPPER semispace there is
   nothing above it but the end of the mmap, so without slack the object's own
   initialising stores fault.

   AArch64 SATISFIES THIS TODAY BY ACCIDENT, NOT BY DESIGN, which is why this
   is a checked invariant rather than a constant to add.  heap-size is
   #x38000000 (896 MB) while the shipping builds override the midpoint to
   #x08000000 (128 MB semispaces), leaving ~640 MB of slack — measured live:
   top semispace ends 0x7dac1ffffe00, mmap ends 0x7dac48000000.  But the
   midpoint is settable per-build and via MODUS_GC_MIDPOINT, and this file's
   own default (#x1C000000) would put the top semispace's end 512 bytes below
   the mapping end — i386's B3 exactly.  Hence: assert, don't assume.")


(defvar *linux-aarch64-gc-midpoint* +linux-aarch64-gc-midpoint+
  "WS4-AA64 #160: build-overridable semispace boundary.  to_start = heap+this,
   space_size = this - alloc_start.  Default = the 448MB constant; a GC-on repro
   build may shrink it (with a matching *linux-aarch64-r25-offset*) so collections
   fire on a modest allocation.")

(defun check-aarch64-gc-guard-invariant ()
  "Build-time only — emits no code.  Signals if the configured semispace
   geometry leaves less than +linux-aarch64-gc-guard+ of mapped slack above
   the top semispace.  GC-off builds (r25-offset = heap-size, so allocation is
   bounded by the mapping itself and never flips) are exempt."
  (when (/= *linux-aarch64-r25-offset* +linux-aarch64-heap-size+)
    (let* ((midpoint *linux-aarch64-gc-midpoint*)
           ;; to_start = base+midpoint, space_size = midpoint - alloc_start,
           ;; so the top semispace ends at base + 2*midpoint - alloc_start.
           (top-end (- (* 2 midpoint) +linux-aarch64-heap-alloc-start+))
           (slack (- +linux-aarch64-heap-size+ top-end)))
      (when (< slack +linux-aarch64-gc-guard+)
        (error "AArch64 GC arena has no overshoot guard: midpoint #x~X puts the ~
                top semispace's end at heap+#x~X, only ~D bytes below the ~D MB ~
                mapping — need at least #x~X (16 MB).  Either raise ~
                +linux-aarch64-heap-size+ to #x~X or lower the midpoint.  ~
                Shipping this is i386 bug B3 (18b223b): every allocation larger ~
                than the slack that trips :gc-check runs off the mmap and ~
                SIGSEGVs in its own initialising stores."
               midpoint top-end slack (ash +linux-aarch64-heap-size+ -20)
               +linux-aarch64-gc-guard+
               (+ top-end +linux-aarch64-gc-guard+)))))
  t)

(defvar *linux-aarch64-gc-metadata-shl* nil
  "WS4-AA64 #160: when non-nil, the entry stub stores the Cheney GC metadata
   (from_start/to_start/space_size/stack_base) SHL-1, matching gc.lisp's
   (mem-ref :u64) convention (memory holds value<<1; %gc-collect reads back the
   raw address).  The historical raw store was a LATENT bug — harmless while GC
   was disabled (metadata never read), but it would HALVE every address the
   moment a collection fired.  Default nil keeps the GC-off images (the ANSI
   gate) byte-identical; GC-on builds set it t.")

;; Shared ELF strtab sanitizer.  Same fn lives in boot-linux-x64.lisp;
;; we provide it here too so this file can be loaded without x64 boot.
(unless (fboundp '%sanitize-symbol-name)
  (defun %sanitize-symbol-name (name)
    "ELF strtab can't have NULs (used as terminator).  Replace any NULs in
     NAME with '_' and limit length to 200."
    (let ((s (string name)))
      (with-output-to-string (out)
        (loop for c across s
              for i below (min (length s) 200)
              do (write-char (if (char= c #\Null) #\_ c) out))))))

;;; ============================================================
;;; ELF64 Little-Endian Wrapper for AArch64
;;; ============================================================
;;;
;;; Shape: same 1 LOAD phdr + 5 shdrs as wrap-in-elf64-le (x64).
;;; Differences: e_machine = 183 (EM_AARCH64), p_align = 64K.

(defun wrap-in-elf64-le-aa64 (raw-bytes load-addr &key function-table
                                                       native-image-offset
                                                       native-code-length
                                                       (machine 183)
                                                       (page-align #x10000)
                                                       (bss-size nil))
  "Wrap raw image bytes in an ELF64-LE executable.

   Named for AArch64 because that is what it was written for, and the defaults
   still produce byte-identical AArch64 output; MACHINE, PAGE-ALIGN and
   BSS-SIZE make it serve any little-endian 64-bit target.  The only things
   that differ between ELF64-LE ports are e_machine, p_align and how much BSS
   the p_memsz reserves — everything else here, including the section headers
   and the per-function symbol table that objdump and addr2line need, is
   identical.  Copying 125 lines per architecture to change two numbers is how
   three ports drift apart.

   MACHINE   e_machine: 183 EM_AARCH64 (default), 243 EM_RISCV, 62 EM_X86_64.
   PAGE-ALIGN p_align: 64K on AArch64, 4K elsewhere.
   BSS-SIZE  extra p_memsz beyond p_filesz; defaults to the AArch64 heap size."
  (declare (ignorable bss-size))
  (let* ((ehdr-size 64)
         (phdr-size 56)
         (shdr-size 64)
         (sym-size  24)
         (header-total (+ ehdr-size phdr-size))
         (raw-len (length raw-bytes))
         (nio (or native-image-offset 0))
         (ncl (or native-code-length 0))
         (sorted-fns (stable-sort (copy-list function-table)
                                  #'< :key #'mvm-function-info-native-offset))
         (fn-sizes (let ((arr (make-array (length sorted-fns) :initial-element 0)))
                     (loop for i from 0 below (length sorted-fns)
                           for fi in sorted-fns
                           for next-off = (if (< (1+ i) (length sorted-fns))
                                              (mvm-function-info-native-offset
                                                (nth (1+ i) sorted-fns))
                                              ncl)
                           do (setf (aref arr i)
                                    (max 0 (- next-off
                                              (mvm-function-info-native-offset fi)))))
                     arr))
         (shstrtab (concatenate 'string
                                (string #\Null)
                                ".text" (string #\Null)
                                ".shstrtab" (string #\Null)
                                ".symtab" (string #\Null)
                                ".strtab" (string #\Null)))
         (shstrtab-bytes (map 'vector #'char-code shstrtab))
         (shstrtab-len (length shstrtab-bytes))
         (sym-names (cons "" (mapcar (lambda (fi)
                                       (%sanitize-symbol-name
                                         (mvm-function-info-name fi)))
                                     sorted-fns)))
         (sym-name-offsets (let ((acc 0) (offs nil))
                             (dolist (n sym-names (nreverse offs))
                               (push acc offs)
                               (incf acc (1+ (length n))))))
         (strtab-bytes (with-output-to-string (out)
                         (dolist (n sym-names)
                           (write-string n out)
                           (write-char #\Null out))))
         (strtab-byte-vec (map 'vector #'char-code strtab-bytes))
         (strtab-len (length strtab-byte-vec))
         (n-syms (1+ (length function-table)))
         (symtab-len (* n-syms sym-size))
         (shstrtab-offset (+ header-total raw-len))
         (symtab-offset (+ shstrtab-offset shstrtab-len))
         (strtab-offset (+ symtab-offset symtab-len))
         (shdrs-offset (+ strtab-offset strtab-len))
         (entry-point (+ load-addr header-total))
         (buf (make-mvm-buffer)))
    ;; Code linked high must END below where the moved region begins: the
    ;; ANSI gate's image is ~200 MB, and code at 0x7000000000 overlapping a
    ;; region at 0x700F000000 would be two MAP_FIXED mappings on one range.
    (when (and (>= load-addr +conv-region-end+)
               (> (+ load-addr header-total raw-len #x10000)
                  (conv-real +conv-region-low+))
               (< load-addr (conv-real +conv-region-low+)))
      (error "code #x~X..#x~X overlaps the runtime-data region at #x~X; raise MODUS_CONV_DELTA or lower MODUS_CODE_BASE"
             load-addr (+ load-addr header-total raw-len #x10000)
             (conv-real +conv-region-low+)))
    ;; Code linked LOW must end below the runtime-data region.  The unmoved
    ;; region starts at 0x10000000 (its scratch pages from 0x0F000000), which
    ;; leaves 252 MB from 0x400000; the ANSI gate outgrew that (273 MB on
    ;; origin/main, measured 2026-09-27): its own bytes land on the convention
    ;; block and it dies at boot reading ASCII as a gate word.  Moved region:
    ;; refuse.  Unmoved: say so loudly, since the historic gate build relies on
    ;; not refusing — link it high (docs/macos-hosting.md) to fix it.
    (when (< load-addr +conv-region-low+)
      (let ((end (+ load-addr header-total raw-len)))
        (when (> end +conv-region-low+)
          (if (eql (conv-real +conv-region-base+) +conv-region-base+)
              (format t "~&~%  *** WARNING: image #x~X..#x~X overlaps the runtime-data area at #x~X (scratch pages) / #x~X (convention block); it will corrupt itself at boot.  Link it high: MODUS_CODE_BASE (docs/macos-hosting.md). ***~%~%"
                      load-addr end +conv-region-low+ +conv-region-base+)
              (error "image #x~X..#x~X overlaps the moved region's old range at #x~X; link the code high (MODUS_CODE_BASE)"
                     load-addr end +conv-region-low+)))))
    (mvm-emit-byte buf #x7F)
    (mvm-emit-byte buf (char-code #\E))
    (mvm-emit-byte buf (char-code #\L))
    (mvm-emit-byte buf (char-code #\F))
    (mvm-emit-byte buf 2)              ; ELFCLASS64
    (mvm-emit-byte buf 1)              ; ELFDATA2LSB
    (mvm-emit-byte buf 1)              ; EV_CURRENT
    (mvm-emit-byte buf 0)              ; ELFOSABI_NONE
    (dotimes (i 8) (mvm-emit-byte buf 0))
    (mvm-emit-u16 buf 2)               ; ET_EXEC
    (mvm-emit-u16 buf machine)         ; EM_AARCH64 by default
    (mvm-emit-u32 buf 1)               ; e_version
    (mvm-emit-u64 buf entry-point)
    (mvm-emit-u64 buf ehdr-size)
    (mvm-emit-u64 buf shdrs-offset)
    (mvm-emit-u32 buf 0)
    (mvm-emit-u16 buf ehdr-size)
    (mvm-emit-u16 buf phdr-size)
    (mvm-emit-u16 buf 1)
    (mvm-emit-u16 buf shdr-size)
    (mvm-emit-u16 buf 5)
    (mvm-emit-u16 buf 2)
    ;; Program Header
    (mvm-emit-u32 buf 1)              ; PT_LOAD
    (mvm-emit-u32 buf 7)              ; PF_R|PF_W|PF_X
    (mvm-emit-u64 buf 0)
    (mvm-emit-u64 buf load-addr)
    (mvm-emit-u64 buf load-addr)
    (mvm-emit-u64 buf (+ header-total raw-len))
    ;; p_memsz: the image plus its BSS tail, which holds the runtime-data
    ;; region at 0x10000000.  When that region has moved (docs/macos-hosting.md
    ;; option B) the boot stub maps it at its real base, and the segment ends
    ;; at +CONV-REGION-LOW+ (0x0F000000) so any access still aimed at the OLD
    ;; address faults.
    ;; When the CODE is linked above the old region (the hosted layout's
    ;; :code-base) there is no BSS tail to keep at all: the segment is the file
    ;; plus a page of slack.
    (mvm-emit-u64 buf (cond ((>= load-addr +conv-region-end+)
                             (+ header-total raw-len #x10000))
                            ((eql (conv-real +conv-region-base+) +conv-region-base+)
                             (+ header-total raw-len
                                (or bss-size +linux-aarch64-heap-size+)))
                            (t (- +conv-region-low+ load-addr))))
    (mvm-emit-u64 buf page-align)     ; 64K on AArch64, 4K elsewhere
    (loop for b across raw-bytes do (mvm-emit-byte buf b))
    (loop for b across shstrtab-bytes do (mvm-emit-byte buf b))
    (dotimes (i sym-size) (mvm-emit-byte buf 0))
    (loop for fi in sorted-fns
          for name-offset in (cdr sym-name-offsets)
          for fn-size across fn-sizes
          do (let* ((nat-off  (or (mvm-function-info-native-offset fi) 0))
                    (sym-addr (+ load-addr header-total nio nat-off)))
               (mvm-emit-u32 buf name-offset)
               (mvm-emit-byte buf #x12)
               (mvm-emit-byte buf 0)
               (mvm-emit-u16 buf 1)
               (mvm-emit-u64 buf sym-addr)
               (mvm-emit-u64 buf fn-size)))
    (loop for b across strtab-byte-vec do (mvm-emit-byte buf b))
    (dotimes (i shdr-size) (mvm-emit-byte buf 0))
    (mvm-emit-u32 buf 1) (mvm-emit-u32 buf 1)
    (mvm-emit-u64 buf 6) (mvm-emit-u64 buf load-addr)
    (mvm-emit-u64 buf 0) (mvm-emit-u64 buf (+ header-total raw-len))
    (mvm-emit-u32 buf 0) (mvm-emit-u32 buf 0)
    (mvm-emit-u64 buf 16) (mvm-emit-u64 buf 0)
    (mvm-emit-u32 buf 7) (mvm-emit-u32 buf 3)
    (mvm-emit-u64 buf 0) (mvm-emit-u64 buf 0)
    (mvm-emit-u64 buf shstrtab-offset) (mvm-emit-u64 buf shstrtab-len)
    (mvm-emit-u32 buf 0) (mvm-emit-u32 buf 0)
    (mvm-emit-u64 buf 1) (mvm-emit-u64 buf 0)
    (mvm-emit-u32 buf 17) (mvm-emit-u32 buf 2)
    (mvm-emit-u64 buf 0) (mvm-emit-u64 buf 0)
    (mvm-emit-u64 buf symtab-offset) (mvm-emit-u64 buf symtab-len)
    (mvm-emit-u32 buf 4) (mvm-emit-u32 buf 1)
    (mvm-emit-u64 buf 8) (mvm-emit-u64 buf sym-size)
    (mvm-emit-u32 buf 25) (mvm-emit-u32 buf 3)
    (mvm-emit-u64 buf 0) (mvm-emit-u64 buf 0)
    (mvm-emit-u64 buf strtab-offset) (mvm-emit-u64 buf strtab-len)
    (mvm-emit-u32 buf 0) (mvm-emit-u32 buf 0)
    (mvm-emit-u64 buf 1) (mvm-emit-u64 buf 0)

    (mvm-buffer-used-bytes buf)))

;;; ============================================================
;;; Linux AArch64 Entry Stub
;;; ============================================================
;;;
;;; Linux puts argc/argv on the stack at SP:
;;;   [SP+0]  argc
;;;   [SP+8]  argv[0]
;;;   [SP+16] argv[1]
;;;   ...
;;;
;;; Syscall ABI: x8=number, x0..x5=args, SVC #0, result in x0.
;;; Generic AArch64 syscalls: write=64 exit=93 mmap=222 clone=220 wait4=260.
;;;
;;; MVM registers (translate-aarch64.lisp): x24=VA x25=VL x26=NIL x29=FP.

(defun emit-linux-aarch64-entry (buf)
  "Emit AArch64 Linux userspace entry stub into BUF (an a64-buffer).
   Sets up argc/argv globals, mmap heap, MVM regs.  Falls through to
   kernel-main."
  ;; LDR x19, [SP, #0]   — argc → x19 (callee-saved, survives syscalls)
  (emit-aarch64-u32 buf #xF94003F3)
  ;; LDR x20, [SP, #16]  — argv[1] → x20
  (emit-aarch64-u32 buf #xF9400BF4)
  ;; LDR x21, [SP, #24]  — argv[2] → x21
  (emit-aarch64-u32 buf #xF9400FF5)

  ;; THE RUNTIME-DATA REGION (docs/macos-hosting.md, option B).  Unmoved
  ;; (*conv-delta* 0) it is the ELF's own BSS tail and nothing is emitted
  ;; here.  Moved, the ELF stops at 0x10000000 (wrap-in-elf64-le-aa64) and the
  ;; region is mapped at its real base FIRST, before any store into it:
  ;; MAP_FIXED_NOREPLACE so a collision fails loudly instead of clobbering, and
  ;; exit 97 when the kernel will not give us that address.  Anonymous memory
  ;; is zeroed, which the ~900 MB BSS tail never reliably was.
  ;; Also when the CODE is linked high with the region unmoved (delta 0): then
  ;; there is no ELF BSS tail to hold the region either, so map it at its own
  ;; (virtual = real) address.  That layout is stock in every respect except
  ;; code placement, which is what makes it the gate's baseline for a
  ;; >252 MB image.
  (when (or (not (eql (conv-real +conv-region-base+) +conv-region-base+))
            (>= (linux-aarch64-code-base) +conv-region-end+))
    (emit-aarch64-load-imm64 buf 0 (conv-real +conv-region-low+))
    (emit-aarch64-load-imm64 buf 1 (- +conv-region-end+ +conv-region-low+))
    (emit-aarch64-load-imm64 buf 2 3)          ; PROT_READ|WRITE
    (emit-aarch64-load-imm64 buf 3 #x104022)   ; PRIV|ANON|NORESERVE|FIXED_NOREPLACE
    (emit-aarch64-load-imm64 buf 4 #xFFFFFFFFFFFFFFFF)
    (emit-aarch64-load-imm64 buf 5 0)
    (emit-aarch64-load-imm64 buf 8 222)        ; mmap
    (modus.mvm::a64-svc buf 0)          ; SVC #0
    (emit-aarch64-load-imm64 buf 16 (conv-real +conv-region-low+))
    (emit-aarch64-u32 buf #xEB10001F)          ; CMP x0, x16
    ;; B.EQ past the exit, patched from where it really ends: a Darwin image's
    ;; syscall is a five-instruction call, not one SVC.
    (let ((beq-at (a64-buffer-position buf)))
      (emit-aarch64-u32 buf 0)                 ; B.EQ <past exit>, patched
      (emit-aarch64-load-imm64 buf 0 97)       ; exit(97): region not mappable
      (emit-aarch64-load-imm64 buf 8 93)
      (modus.mvm::a64-svc buf 0)
      (setf (aref (a64-buffer-code buf) beq-at)
            (logior #x54000000 (ash (- (a64-buffer-position buf) beq-at) 5)))))

  ;; Store argc as 32-bit at [0x10000200].
  (emit-aarch64-load-imm64 buf 16 (conv-real #x10000200))
  ;; STR w19, [x16, #0]
  (emit-aarch64-u32 buf #xB9000213)

  ;; Zero-fill 128 bytes at 0x10000208.
  (emit-aarch64-load-imm64 buf 16 (conv-real #x10000208))
  (dotimes (i 16)
    ;; STR XZR, [x16, #(i*8)]
    (emit-aarch64-u32 buf (logior #xF9000000 (ash i 10) (ash 16 5) 31)))

  ;; Copy argv strings into fixed BSS so `%parse-decimal-at-fixed-*`
  ;; can read them.  Mirrors boot-linux-x64.lisp's rep-movsb path: the
  ;; Lisp side expects the *string contents* of argv[1] at 0x10000208
  ;; and argv[2] at 0x10000248, not the argv pointer.  Without this
  ;; copy, the fixed buffers stay zeroed and `(%parse-decimal-at-fixed-208)`
  ;; returns 0 — *skip-below* / *run-only-below* end up 0, every shard
  ;; runs the full suite, and the per-test range arguments are silently
  ;; ignored.  The destination is the region's REAL address, which may take
  ;; more than the two MOVZ/MOVK words these blocks once hand-encoded, so the
  ;; B.LE that skips a block is patched from where the block actually ends.
  (flet ((copy-argv (cmp-insn mov-src dst)
           (emit-aarch64-u32 buf cmp-insn)
           (let ((ble-at (a64-buffer-position buf)))
             (emit-aarch64-u32 buf 0)                ; B.LE <next block>, patched
             (emit-aarch64-u32 buf mov-src)          ; MOV x9, argv[n]
             (emit-aarch64-load-imm64 buf 10 (conv-real dst))
             (emit-aarch64-u32 buf #x528007EB)       ; MOVZ w11, #63    (max bytes)
             (emit-aarch64-u32 buf #x3940012C)       ; LDRB w12, [x9]
             (emit-aarch64-u32 buf #x340000CC)       ; CBZ w12, +6      (null term → exit loop)
             (emit-aarch64-u32 buf #x3900014C)       ; STRB w12, [x10]
             (emit-aarch64-u32 buf #x91000529)       ; ADD x9, x9, #1
             (emit-aarch64-u32 buf #x9100054A)       ; ADD x10, x10, #1
             (emit-aarch64-u32 buf #x5100016B)       ; SUB w11, w11, #1
             (emit-aarch64-u32 buf #x35FFFF4B)       ; CBNZ w11, -6     (back to LDRB)
             (setf (aref (a64-buffer-code buf) ble-at)
                   (logior #x5400000D                ; B.LE
                           (ash (- (a64-buffer-position buf) ble-at) 5))))))
    ;; argv[1] → 0x10000208 (max 63 bytes, source already null-terminated)
    (copy-argv #xF100067F #xAA1403E9 #x10000208)   ; CMP x19, #1 / MOV x9, x20
    ;; argv[2] → 0x10000248 (same shape, different src/dst)
    (copy-argv #xF1000A7F #xAA1503E9 #x10000248))  ; CMP x19, #2 / MOV x9, x21

  ;; mmap heap:
  ;;   x0=hint=0x10000000, x1=size, x2=PROT_RW(3), x3=MAP_PRIV|ANON(0x22),
  ;;   x4=-1, x5=0, x8=222(mmap).  SVC #0.
  (check-aarch64-gc-guard-invariant)
  ;; SAVE-AND-DIE (lib/save-image.lisp) needs the heap at the SAME address in
  ;; every process, so a snapshot restores with no relocation.  First try
  ;; MAP_FIXED_NOREPLACE (0x100000) at +linux-aarch64-fixed-heap-base+; if the
  ;; kernel returns anything else (address taken, or a pre-4.17 kernel that
  ;; ignores the flag and picks its own), fall through to the historical hint
  ;; mmap.  Restore then refuses with `heap base differs' instead of guessing.
  (emit-aarch64-load-imm64 buf 0 (hosted-layout :heap-base +linux-aarch64-fixed-heap-base+))
  (emit-aarch64-load-imm64 buf 1 +linux-aarch64-heap-size+)
  (emit-aarch64-load-imm64 buf 2 3)
  (emit-aarch64-load-imm64 buf 3 #x100022)   ; MAP_PRIV|ANON|FIXED_NOREPLACE
  (emit-aarch64-load-imm64 buf 4 #xFFFFFFFFFFFFFFFF)
  (emit-aarch64-load-imm64 buf 5 0)
  (emit-aarch64-load-imm64 buf 8 222)
  (modus.mvm::a64-svc buf 0)   ; SVC #0
  (emit-aarch64-load-imm64 buf 16 (hosted-layout :heap-base +linux-aarch64-fixed-heap-base+))
  (emit-aarch64-u32 buf #xEB10001F)   ; CMP x0, x16
  (let ((beq-at (a64-buffer-position buf)))
    (emit-aarch64-u32 buf 0)          ; B.EQ <past the fallback>, patched below
    (emit-aarch64-load-imm64 buf 0 #x10000000)
    (emit-aarch64-load-imm64 buf 1 +linux-aarch64-heap-size+)
    (emit-aarch64-load-imm64 buf 2 3)
    (emit-aarch64-load-imm64 buf 3 #x22)
    (emit-aarch64-load-imm64 buf 4 #xFFFFFFFFFFFFFFFF)
    (emit-aarch64-load-imm64 buf 5 0)
    (emit-aarch64-load-imm64 buf 8 222)
    (modus.mvm::a64-svc buf 0)   ; SVC #0
    (setf (aref (a64-buffer-code buf) beq-at)
          (logior #x54000000 (ash (- (a64-buffer-position buf) beq-at) 5))))
  ;; MOV x22, x0  (heap base → x22)
  (emit-aarch64-u32 buf #xAA0003F6)

  ;; The JIT exec arena (SAVE-AND-DIE), AFTER x22 has the heap base (this block clobbers x0): one fixed RWX mapping the
  ;; %mmap-exec-page trap bump-allocates from, so JIT pages and the GC bitmaps
  ;; sit at the same addresses in every process.  Bump word at 0x10000F58 =
  ;; arena base on success, 0 if the kernel refused (the trap then falls back
  ;; to mmap(NULL)).  MAP_NORESERVE: it is address space, not memory.
  (emit-aarch64-load-imm64 buf 0 (hosted-layout :jit-arena-base +linux-aarch64-jit-arena-base+))
  (emit-aarch64-load-imm64 buf 1 +linux-aarch64-jit-arena-size+)
  (emit-aarch64-load-imm64 buf 2 7)          ; PROT_READ|WRITE|EXEC
  (emit-aarch64-load-imm64 buf 3 #x104022)   ; PRIV|ANON|NORESERVE|FIXED_NOREPLACE
  (emit-aarch64-load-imm64 buf 4 #xFFFFFFFFFFFFFFFF)
  (emit-aarch64-load-imm64 buf 5 0)
  (emit-aarch64-load-imm64 buf 8 222)
  (modus.mvm::a64-svc buf 0)   ; SVC #0
  (emit-aarch64-load-imm64 buf 16 (hosted-layout :jit-arena-base +linux-aarch64-jit-arena-base+))
  (emit-aarch64-u32 buf #xEB10001F)   ; CMP x0, x16
  (emit-aarch64-u32 buf #x9A9F0000)   ; CSEL x0, x0, xzr, EQ
  (emit-aarch64-load-imm64 buf 17 (conv-real #x10000F58))
  (emit-aarch64-u32 buf #xF9000220)   ; STR x0, [x17]

  ;; Save argc/argv at heap base for Lisp reachability.
  ;; STR x19, [x22, #0]
  (emit-aarch64-u32 buf #xF90002D3)
  ;; STR x20, [x22, #24]
  (emit-aarch64-u32 buf #xF9000ED4)
  ;; STR x21, [x22, #32]
  (emit-aarch64-u32 buf #xF90012D5)

  ;; Set up MVM registers.
  ;; MOV x24, x22 ; ADD x24, x24, #alloc-start
  (emit-aarch64-u32 buf #xAA1603F8)
  (emit-aarch64-load-imm64 buf 16 +linux-aarch64-heap-alloc-start+)
  (emit-aarch64-u32 buf #x8B100318)   ; ADD x24, x24, x16
  ;; MOV x25, x22 ; ADD x25, x25, #r25-offset
  (emit-aarch64-u32 buf #xAA1603F9)
  (emit-aarch64-load-imm64 buf 16 *linux-aarch64-r25-offset*)
  (emit-aarch64-u32 buf #x8B100339)   ; ADD x25, x25, x16

  ;; GC metadata at absolute slots 0x10000040..0x10000060.
  ;; When *linux-aarch64-gc-metadata-shl*, double x10 (ADD x10,x10,x10 =
  ;; 0x8B0A014A) before each STR so memory holds value<<1 — the convention
  ;; gc.lisp's (mem-ref :u64) reads back (see the defvar docstring).
  (flet ((maybe-shl () (when *linux-aarch64-gc-metadata-shl*
                         (emit-aarch64-u32 buf #x8B0A014A))))  ; ADD x10,x10,x10
    (emit-aarch64-load-imm64 buf 17 (conv-real +gc-from-start-addr+))
    ;; from_start = mmap+alloc_start
    (emit-aarch64-u32 buf #xAA1603EA)   ; MOV x10, x22
    (emit-aarch64-load-imm64 buf 16 +linux-aarch64-heap-alloc-start+)
    (emit-aarch64-u32 buf #x8B10014A)   ; ADD x10, x10, x16
    (maybe-shl)
    (emit-aarch64-u32 buf #xF900022A)   ; STR x10, [x17, #0]
    ;; to_start   = mmap+midpoint
    (emit-aarch64-u32 buf #xAA1603EA)
    (emit-aarch64-load-imm64 buf 16 *linux-aarch64-gc-midpoint*)
    (emit-aarch64-u32 buf #x8B10014A)
    (maybe-shl)
    (emit-aarch64-u32 buf #xF900062A)   ; STR x10, [x17, #8]
    ;; space_size = midpoint - alloc_start
    (emit-aarch64-load-imm64 buf 10 (- *linux-aarch64-gc-midpoint*
                                       +linux-aarch64-heap-alloc-start+))
    (maybe-shl)
    (emit-aarch64-u32 buf #xF9000A2A)   ; STR x10, [x17, #16]
    ;; stack_base = current SP
    (emit-aarch64-u32 buf #x910003EA)   ; ADD x10, SP, #0
    (maybe-shl)
    (emit-aarch64-u32 buf #xF9000E2A)   ; STR x10, [x17, #24]
    ;; gc_count = 0
    (emit-aarch64-u32 buf #xF900123F))  ; STR XZR, [x17, #32]

  ;; x26 = NIL = #xDEAD0001 (matches x64's +nil-value+).  The original
  ;; aa64 boot used NIL=0; that lined up with an early `cset`-based
  ;; consp/atom which produced raw 0/1.  Those predicates have since
  ;; been rewritten to emit `CSEL pd, x18(T), x26(NIL), cond` — they
  ;; pull NIL from x26 directly, so changing x26's value just changes
  ;; what NIL is, and they keep working.
  ;;
  ;; NIL=0 caused a structural bug: fixnum 0 has bit pattern 0 too, so
  ;; `(null 0)` returned T and downstream tests like
  ;; `(when count (decf count))` mistreated `:count 0` as `:count nil`.
  ;; The same shape recurred at :end 0 (find/position), slot index 0
  ;; (CLOS), and at every place we used `(null x)` to mean "absent
  ;; sentinel" with x potentially-0.  Sentinel-substitution worked
  ;; case by case but kept needing fresh patches; this fixes the root.
  (emit-aarch64-load-imm64 buf 26 #xDEAD0001)
  ;; x18 = convention-block base #x10000000 (translate-aarch64 *a64-x18-base*)
  ;; ...unless x18 is off (Darwin, or MODUS_NO_X18 on Linux): then POISON it
  ;; with a non-canonical address, so any code still treating x18 as the base
  ;; faults on its first use instead of working by luck.
  (emit-aarch64-load-imm64 buf 18 (if modus.mvm::*a64-x18-base*
                                      (conv-real #x10000000)
                                      #x0018DEAD0018DEAD))

  ;; NATIVE MCGC: reserve x28 = the GC trampoline's absolute VA, loaded once
  ;; at boot, so every gc-check fire site is a single range-unlimited
  ;; `BLR x28` (BL's +/-128MB imm26 reach can't hit a tail trampoline across
  ;; the ~200MB ANSI-gate image).  MOVZ x28,#lo16 + MOVK x28,#hi16,LSL#16 —
  ;; patched post-link by cross.lisp::apply-aarch64-x28-trampoline-patch, which
  ;; reads *aarch64-x28-load-patch-offset* (the raw-bytes byte position we
  ;; record here) and writes the trampoline label's VA (< 2^32 for a ~200MB
  ;; image, so two MOV instructions suffice).  When native MCGC is off x28 is
  ;; free scratch (the Lisp collector uses BL) and we emit nothing here so the
  ;; boot stub stays byte-identical.
  (when modus.mvm::*aarch64-gc-native-mcgc*
    (setf modus.mvm::*aarch64-x28-load-patch-offset*
          (* (modus.mvm::a64-buffer-position buf) 4))
    ;; lo16 / hi16 [/ bits 32-47 when the code is linked above 4 GB]
    (modus.mvm::a64-emit-code-addr-placeholder buf modus.mvm::+a64-x28+))

  ;; #307: record the handler-stack PUSH/POP helpers' absolute VAs into
  ;; 0x10000F90/F98.  A runtime-JIT page has no labels and so cannot BL these
  ;; helpers; reading the VA out of a fixed slot lets it arm a REAL handler
  ;; frame instead of being rejected and interpreted.  Same MOVZ/MOVK
  ;; placeholder convention as the x28 load above; no-op when the helper
  ;; labels are unbound, so the boot stub stays byte-identical there.
  (modus.mvm::emit-aarch64-handler-va-init buf)

  ;; x29 (FP) = SP
  (emit-aarch64-u32 buf #x910003FD))

(defun linux-aarch64-code-base ()
  "Where the image's code is linked: +LINUX-AARCH64-LOAD-ADDR+ unless the
   hosted layout moves it (MODUS_CODE_BASE; docs/macos-hosting.md).  Linux
   maps an ET_EXEC at its p_vaddr, so on Linux moving it is only a link-time
   change; macOS will remap the signed pages there.  A code base above the
   old runtime-data region has no BSS tail to hold the region, so the boot
   stub maps it (moved or not), and code above 4 GB needs the translator's
   wide code-address placeholders; both are checked here, at build time."
  (let ((base (hosted-layout :code-base +linux-aarch64-load-addr+)))
    (when (and (>= base +conv-region-low+) (< base +conv-region-end+))
      (error "code base #x~X lies inside the runtime-data region [#x~X, #x~X)"
             base +conv-region-low+ +conv-region-end+))
    (when (and (>= base (ash 1 32)) (not *a64-code-addr-wide*))
      (error "code base #x~X is above 4 GB but *A64-CODE-ADDR-WIDE* is off" base))
    base))

(defun linux-aarch64-boot-descriptor ()
  (list :arch :aarch64
        :entry-fn #'emit-linux-aarch64-entry
        :load-addr (linux-aarch64-code-base)
        :elf-format :linux-aarch64))
