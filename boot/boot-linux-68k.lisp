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

(defconstant +linux-68k-heap-size+ #x8000000
  "The DEFAULT CAP, not the size: 128 MB, two 64 MB semispaces.  The size is
   chosen when the process starts (EMIT-LINUX-68K-ENTRY):
     --dynamic-space-size N[KB|MB|GB]   SBCL's own flag, MB when unsuffixed
     otherwise half of physical RAM (sysinfo(2)), capped here
   and clamped to [+LINUX-68K-HEAP-MIN+, +LINUX-68K-HEAP-MAX+].  A 68LC040 Mac
   with 36 MB gets an 18 MB heap from the same binary a 4 GB VM gets 128 MB
   from.  Nothing in the translator depends on it: the collector and every
   allocation site read the geometry the stub leaves in memory.")
(defconstant +linux-68k-heap-min+ #x100000 "1 MB: two 512 KB semispaces.")
(defconstant +linux-68k-heap-max+ #x0F800000
  "248 MB, ARM32's geometry: heap + guard end at #x1FC00000, where the kind map
   starts (+68K-GC-KIND-MAP+), and everything stays below #x20000000, which a
   tagged MEM-REF address can still express on a 4-byte word.")
(defconstant +linux-68k-gc-guard+ #x400000
  "4 MB past the second semispace: :gc-check tests the alloc pointer without
   knowing the size of the allocation that follows, so a big object can overshoot.
   Address space only: nothing touches it unless something overshoots.")
(defconstant +linux-68k-gc-margin-max+ #x100000
  "The collector keeps up to 1 MB free below VL (as RISC-V and ARM32 do), but
   never more than a quarter of a semispace, or a small heap would be all margin.")
(defconstant +linux-68k-sys-sysinfo+ 116)
(defconstant +linux-68k-heap-alloc-start+ #x20000
  "Past the fixed runtime block (#x0000-#x0FFF: metadata #x40, globals #x80,
   MV #x90..#x138, argc #x200, handler frames #x400..), the CLI scratch pages
   (#x1000-#x8FFF) and the staged argv (#x9000-#x1DFFF) -- ARM32's and RV32's
   layout, so the CL image's slots are where the shared runtime expects.  It
   was #x2000, enough for a single-file image and nothing more.")

;;; Staged argv/envp, below 2^29 -- ARM32's addresses.  qemu-m68k puts the
;;; initial stack near #x40800000, which a tagged MEM-REF address cannot
;;; express, so the stub copies the vectors down (EMIT-LINUX-68K-STAGE-ARGV).
(defconstant +linux-68k-argv-ptrs+       #x10009000)
(defconstant +linux-68k-argv-ptrs-end+   #x1000A000)
(defconstant +linux-68k-argv-arena+      #x1000A000)
(defconstant +linux-68k-argv-arena-end+  #x1001E000)

(defconstant +linux-68k-page-shift+ 12
  "mmap2 takes its file offset in pages.  We pass 0, so this is documentation of
   the units rather than arithmetic — but it is the difference between mmap2 and
   mmap and the reason the two cannot be swapped by changing only the number.")

;;; ============================================================
;;; Entry stub
;;; ============================================================

(defun emit-linux-68k-choose-heap-size (buf)
  "Leave the heap size in BYTES in A5 (a syscall clobbers d0-d5 and a0 only).
   Reads argv for --dynamic-space-size, else asks sysinfo(2) for physical RAM
   and takes half, capped at +LINUX-68K-HEAP-SIZE+.  Works in KB (D7) so no
   step can overflow 32 bits, then clamps and rounds to 128 KB, which keeps the
   midpoint 64 KB-aligned and the kind map's longword clear exact."
  (let ((loop-top (mvm-make-label)) (next (mvm-make-label)) (no-flag (mvm-make-label))
        (digits (mvm-make-label)) (suffix (mvm-make-label)) (unit-k (mvm-make-label))
        (unit-g (mvm-make-label)) (have-kb (mvm-make-label)) (cap-kb (mvm-make-label))
        (ram-small (mvm-make-label)) (ram-done (mvm-make-label)) (clamp (mvm-make-label))
        (not-small (mvm-make-label)) (not-big (mvm-make-label))
        (cap (ash +linux-68k-heap-size+ -10))
        (flag "--dynamic-space-size"))
    ;; ---- argv: D6 = argc, D5 = i, A5 = &argv[0] ----
    (m68k-emit-move-disp-dn buf +68k-a7+ 0 +68k-d6+)
    (m68k-emit-lea-disp buf +68k-a7+ 4 +68k-a5+)
    (m68k-emit-moveq buf +68k-d5+ 1)
    (m68k-emit-label buf loop-top)
    (m68k-emit-cmp-dn-dn buf +68k-d6+ +68k-d5+)              ; i - argc
    (m68k-emit-bge buf no-flag)
    (m68k-emit-move-dn-dn buf +68k-d5+ +68k-d0+)
    (m68k-emit-lsl-imm buf +68k-d0+ 2)
    (m68k-emit-move-an-an buf +68k-a5+ +68k-a1+)
    (m68k-emit-adda-dn buf +68k-d0+ +68k-a1+)                 ; A1 = &argv[i]
    (m68k-emit-move-disp-an buf +68k-a1+ 0 +68k-a0+)
    (loop for c across flag
          do (m68k-emit-move-byte-postinc-dn buf +68k-a0+ +68k-d0+)
             (m68k-emit-cmpi-byte buf +68k-d0+ (char-code c))
             (m68k-emit-bne buf next))
    (m68k-emit-move-byte-postinc-dn buf +68k-a0+ +68k-d0+)
    (m68k-emit-word buf #x4A00)                               ; TST.B D0
    (m68k-emit-bne buf next)
    ;; matched: the value is argv[i+1], if there is one
    (m68k-emit-move-dn-dn buf +68k-d5+ +68k-d0+)
    (m68k-emit-addq buf +68k-d0+ 1)
    (m68k-emit-cmp-dn-dn buf +68k-d6+ +68k-d0+)
    (m68k-emit-bge buf no-flag)
    (m68k-emit-move-disp-an buf +68k-a1+ 4 +68k-a0+)
    (m68k-emit-moveq buf +68k-d7+ 0)
    (m68k-emit-label buf digits)
    (m68k-emit-moveq buf +68k-d0+ 0)
    (m68k-emit-move-byte-postinc-dn buf +68k-a0+ +68k-d0+)
    (m68k-emit-cmpi buf +68k-d0+ 48)                          ; '0'
    (m68k-emit-bcc-word buf +68k-cc-cs+ suffix)
    (m68k-emit-cmpi buf +68k-d0+ 57)                          ; '9'
    (m68k-emit-bcc-word buf +68k-cc-hi+ suffix)
    (m68k-emit-subi buf +68k-d0+ 48)
    ;; D7 = D7*10 + D0, saturating well above any cap
    (m68k-emit-cmpi buf +68k-d7+ #x100000)
    (m68k-emit-bcc-word buf +68k-cc-cc+ digits)
    (m68k-emit-move-dn-dn buf +68k-d7+ +68k-d1+)
    (m68k-emit-lsl-imm buf +68k-d7+ 3)
    (m68k-emit-add-dn-dn buf +68k-d1+ +68k-d7+)
    (m68k-emit-add-dn-dn buf +68k-d1+ +68k-d7+)
    (m68k-emit-add-dn-dn buf +68k-d0+ +68k-d7+)
    (m68k-emit-bra buf digits)
    (m68k-emit-label buf suffix)
    (m68k-emit-andi buf +68k-d0+ #xDF)                        ; upper-case it
    (m68k-emit-cmpi buf +68k-d0+ 75)                          ; 'K'
    (m68k-emit-beq buf have-kb)
    (m68k-emit-cmpi buf +68k-d0+ 71)                          ; 'G'
    (m68k-emit-beq buf unit-g)
    ;; MB (unsuffixed, M or MB): KB = n << 10, which cannot overflow below 2^20
    (m68k-emit-moveq buf +68k-d1+ 10)
    (m68k-emit-lsl-dn buf +68k-d1+ +68k-d7+)
    (m68k-emit-bra buf have-kb)
    (m68k-emit-label buf unit-g)
    (m68k-emit-tst buf +68k-d7+)
    (m68k-emit-beq buf have-kb)
    (m68k-emit-move-imm-dn buf (ash +linux-68k-heap-max+ -10) +68k-d7+)
    (m68k-emit-bra buf have-kb)
    (m68k-emit-label buf next)
    (m68k-emit-addq buf +68k-d5+ 1)
    (m68k-emit-bra buf loop-top)
    ;; ---- no flag: half of physical RAM, capped ----
    (m68k-emit-label buf no-flag)
    (m68k-emit-lea-disp buf +68k-a7+ -64 +68k-a7+)
    (m68k-emit-move-an-dn buf +68k-a7+ +68k-d1+)
    (m68k-emit-move-imm-dn buf +linux-68k-sys-sysinfo+ +68k-d0+)
    (m68k-emit-trap buf 0)
    (m68k-emit-tst buf +68k-d0+)
    (m68k-emit-bne buf cap-kb)
    (m68k-emit-move-disp-dn buf +68k-a7+ 16 +68k-d7+)         ; totalram, in units
    (m68k-emit-move-disp-dn buf +68k-a7+ 52 +68k-d1+)         ; mem_unit
    (m68k-emit-cmpi buf +68k-d1+ 1024)
    (m68k-emit-bcc-word buf +68k-cc-cs+ ram-small)
    ;; mem_unit >= 1 KB: KB = totalram * (mem_unit >> 10), unless that is
    ;; already >= 1 GB of units, which is more than any cap
    (m68k-emit-cmpi buf +68k-d7+ #x100000)
    (m68k-emit-bcc-word buf +68k-cc-cc+ cap-kb)
    (m68k-emit-moveq buf +68k-d0+ 10)
    (m68k-emit-word buf (logior #xE0A8 +68k-d1+))             ; LSR.L D0,D1
    (m68k-emit-muls buf +68k-d1+ +68k-d7+)
    (m68k-emit-bra buf ram-done)
    (m68k-emit-label buf ram-small)
    (m68k-emit-moveq buf +68k-d0+ 10)
    (m68k-emit-word buf (logior #xE0A8 +68k-d7+))             ; LSR.L D0,D7: bytes->KB
    (m68k-emit-muls buf +68k-d1+ +68k-d7+)
    (m68k-emit-label buf ram-done)
    (m68k-emit-lsr-imm buf +68k-d7+ 1)                        ; half
    (m68k-emit-cmpi buf +68k-d7+ cap)
    (m68k-emit-bcc-word buf +68k-cc-ls+ clamp)
    (m68k-emit-label buf cap-kb)
    (m68k-emit-move-imm-dn buf cap +68k-d7+)
    (m68k-emit-label buf clamp)
    (m68k-emit-lea-disp buf +68k-a7+ 64 +68k-a7+)
    ;; ---- clamp to [min, max], round to 128 KB, bytes into A5 ----
    (m68k-emit-label buf have-kb)
    (m68k-emit-cmpi buf +68k-d7+ (ash +linux-68k-heap-min+ -10))
    (m68k-emit-bcc-word buf +68k-cc-cc+ not-small)
    (m68k-emit-move-imm-dn buf (ash +linux-68k-heap-min+ -10) +68k-d7+)
    (m68k-emit-label buf not-small)
    (m68k-emit-cmpi buf +68k-d7+ (ash +linux-68k-heap-max+ -10))
    (m68k-emit-bcc-word buf +68k-cc-ls+ not-big)
    (m68k-emit-move-imm-dn buf (ash +linux-68k-heap-max+ -10) +68k-d7+)
    (m68k-emit-label buf not-big)
    (m68k-emit-andi buf +68k-d7+ (logand (lognot 127) #xFFFFFFFF))
    (m68k-emit-moveq buf +68k-d1+ 10)
    (m68k-emit-lsl-dn buf +68k-d1+ +68k-d7+)
    (m68k-emit-move-dn-an buf +68k-d7+ +68k-a5+)))

(defun emit-linux-68k-stage-argv (buf)
  "Copy argv[] and envp[] (pointer vectors AND strings) from the initial stack
   to +LINUX-68K-ARGV-PTRS+ / -ARENA+, each vector NULL-terminated, as ARM32's
   stub does.  Stops early -- both vectors still terminated -- rather than
   overrun either region.  A7 still points at argc.  Clobbers D0, D3, A0, A1,
   A3, A4; must run after the heap is mapped (both regions are inside it)."
  (let ((top (mvm-make-label)) (copy (mvm-make-label)) (cloop (mvm-make-label))
        (full (mvm-make-label)) (done (mvm-make-label)))
    (m68k-emit-lea-disp buf +68k-a7+ 4 +68k-a0+)                 ; &argv[0]
    (m68k-emit-move-imm-an buf +linux-68k-argv-ptrs+ +68k-a1+)
    (m68k-emit-move-imm-an buf +linux-68k-argv-arena+ +68k-a3+)
    (m68k-emit-moveq buf +68k-d3+ 2)                             ; NULLs to copy
    (m68k-emit-label buf top)
    ;; MOVEA.L (A0)+, A4
    (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-an+ +68k-a4+
                                               +ea-mode-an-postinc+ +68k-a0+))
    (m68k-emit-move-an-dn buf +68k-a4+ +68k-d0+)
    (m68k-emit-tst buf +68k-d0+)
    (m68k-emit-bne buf copy)
    ;; a NULL: copy it; stop after the second
    (m68k-emit-moveq buf +68k-d0+ 0)
    (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-an-postinc+ +68k-a1+
                                               +ea-mode-dn+ +68k-d0+))   ; MOVE.L D0,(A1)+
    (m68k-emit-subq buf +68k-d3+ 1)
    (m68k-emit-bne buf top)
    (m68k-emit-bra buf done)
    (m68k-emit-label buf copy)
    ;; room for this pointer plus a terminator, and a string's worth of arena?
    (m68k-emit-word buf (logior #xB1FC (ash +68k-a1+ 9)))       ; CMPA.L #imm,A1
    (m68k-emit-long buf (- +linux-68k-argv-ptrs-end+ 8))
    (m68k-emit-bcc-word buf +68k-cc-cc+ full)
    (m68k-emit-word buf (logior #xB1FC (ash +68k-a3+ 9)))       ; CMPA.L #imm,A3
    (m68k-emit-long buf (- +linux-68k-argv-arena-end+ 4096))
    (m68k-emit-bcc-word buf +68k-cc-cc+ full)
    (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-an-postinc+ +68k-a1+
                                               +ea-mode-an+ +68k-a3+))   ; MOVE.L A3,(A1)+
    (m68k-emit-label buf cloop)
    (m68k-emit-move-byte-postinc-dn buf +68k-a4+ +68k-d0+)       ; MOVE.B (A4)+,D0
    (m68k-emit-word buf (m68k-encode-move-word 1 +ea-mode-an-postinc+ +68k-a3+
                                               +ea-mode-dn+ +68k-d0+))   ; MOVE.B D0,(A3)+
    (m68k-emit-word buf #x4A00)                                  ; TST.B D0
    (m68k-emit-bne buf cloop)
    (m68k-emit-bra buf top)
    (m68k-emit-label buf full)
    (m68k-emit-moveq buf +68k-d0+ 0)
    (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a1+ 0)
    (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a1+ 4)
    (m68k-emit-label buf done)))

(defun emit-linux-68k-nil-page (buf)
  "Map the page at #xDEAD0000 and fill it with NIL: (car nil) and (cdr nil) are
   plain loads from NIL's own address, as on ARM32 / RV32 / RV64.  Clobbers
   D0-D5 and A0 (the syscall), and D1/D2/A0 for the fill."
  (let ((fill (mvm-make-label)))
    (m68k-emit-move-imm-dn buf #xDEAD0000 +68k-d1+)
    (m68k-emit-move-imm-dn buf 4096 +68k-d2+)
    (m68k-emit-move-imm-dn buf 3 +68k-d3+)
    (m68k-emit-move-imm-dn buf #x32 +68k-d4+)
    (m68k-emit-move-imm-dn buf -1 +68k-d5+)
    (m68k-emit-move-imm-an buf 0 +68k-a0+)
    (m68k-emit-move-imm-dn buf +68k-linux-sys-mmap2+ +68k-d0+)
    (m68k-emit-trap buf 0)
    (m68k-emit-move-imm-an buf #xDEAD0000 +68k-a0+)
    (m68k-emit-move-imm-dn buf +nil-value+ +68k-d1+)
    (m68k-emit-move-imm-dn buf 1024 +68k-d2+)
    (m68k-emit-label buf fill)
    (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-an-postinc+ +68k-a0+
                                               +ea-mode-dn+ +68k-d1+))   ; MOVE.L D1,(A0)+
    (m68k-emit-subq buf +68k-d2+ 1)
    (m68k-emit-bne buf fill)))

(defun emit-linux-68k-entry (mbuf)
  "Emit the Linux/m68k userspace entry into MBUF (cross.lisp's mvm-buffer).

   Builds an m68k-buffer and appends its bytes, as the other hosted stubs do, so
   the encoders are the ones the translator and the ladder exercise.

   On entry a7 points at argc with argv immediately above.  d0/d1 are the
   translator's reserved scratch and free here; a2/a3/a4 are VA/VL/VN and a6 is
   VFP, which the first function prologue expects to be pointing somewhere
   valid.

   THE HEAP IS SIZED HERE, AT RUN TIME (EMIT-LINUX-68K-CHOOSE-HEAP-SIZE), and
   the geometry is written to the Cheney block at #x10000040 for the collector
   to read.  Layout, S = the chosen size:
     [base, base+S)              two S/2 semispaces (the first starts past the
                                 fixed low block, at base + alloc-start)
     [base+S, base+S+guard)      overshoot guard
     [+68K-GC-KIND-MAP+, +S/16)  kind map, one byte per 16-byte granule, at a
                                 FIXED address past the largest heap, so the
                                 allocation sites can bake its bias in

   No signal handlers and no JIT arena: a fault is a clean SIGSEGV rather than a
   longjmp into handler-case, and there is no m68k JIT.  Deliberate omissions."
  (let ((buf (make-m68k-buffer))
        (no-cap (mvm-make-label)))
    (emit-linux-68k-choose-heap-size buf)
    ;; --- mmap2 FIRST: the argc slot is INSIDE this mapping, and this port asks
    ;;     for no BSS, so writing it beforehand is an immediate SIGSEGV.
    ;;     mmap2(addr, S + guard, PROT_READ|WRITE, MAP_PRIVATE|ANON|FIXED, -1, 0)
    (m68k-emit-move-imm-dn buf +linux-68k-heap-addr+ +68k-d1+)
    (m68k-emit-move-an-dn buf +68k-a5+ +68k-d2+)
    (m68k-emit-addi buf +68k-d2+ +linux-68k-gc-guard+)
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
    ;; --- the kind map, at its FIXED address, only as long as THIS heap needs:
    ;;     mmap2(+68K-GC-KIND-MAP+, S/16, ...)
    (m68k-emit-move-imm-dn buf +68k-gc-kind-map+ +68k-d1+)
    (m68k-emit-move-an-dn buf +68k-a5+ +68k-d2+)
    (m68k-emit-lsr-imm buf +68k-d2+ 4)
    (m68k-emit-move-imm-dn buf 3 +68k-d3+)
    (m68k-emit-move-imm-dn buf #x32 +68k-d4+)
    (m68k-emit-move-imm-dn buf -1 +68k-d5+)
    (m68k-emit-move-imm-an buf 0 +68k-a0+)
    (m68k-emit-move-imm-dn buf +68k-linux-sys-mmap2+ +68k-d0+)
    (m68k-emit-trap buf 0)
    ;; --- the NIL page and the staged argv (both CL-image requirements)
    (emit-linux-68k-nil-page buf)
    (emit-linux-68k-stage-argv buf)
    (m68k-emit-move-imm-dn buf +linux-68k-heap-addr+ +68k-d0+)   ; d0 = heap base again (MAP_FIXED)
    (m68k-emit-move-dn-an buf +68k-d0+ +68k-a1+)
    ;; --- argc, now inside the mapping
    (m68k-emit-move-disp-dn buf +68k-a7+ 0 +68k-d6+)
    (m68k-emit-move-imm-an buf (+ +linux-68k-heap-addr+ #x200) +68k-a0+)
    (m68k-emit-move-dn-an-ind buf +68k-d6+ +68k-a0+)
    ;; --- D7 = midpoint = S/2;  D6 = space_size = mid - alloc-start
    (m68k-emit-move-an-dn buf +68k-a5+ +68k-d7+)
    (m68k-emit-lsr-imm buf +68k-d7+ 1)
    (m68k-emit-move-dn-dn buf +68k-d7+ +68k-d6+)
    (m68k-emit-subi buf +68k-d6+ +linux-68k-heap-alloc-start+)
    ;; --- D5 = margin = min(margin-max, space_size/4), 16-aligned
    (m68k-emit-move-dn-dn buf +68k-d6+ +68k-d5+)
    (m68k-emit-lsr-imm buf +68k-d5+ 2)
    (m68k-emit-andi buf +68k-d5+ (logand (lognot 15) #xFFFFFFFF))
    (m68k-emit-cmpi buf +68k-d5+ +linux-68k-gc-margin-max+)
    (m68k-emit-bcc-word buf +68k-cc-ls+ no-cap)
    (m68k-emit-move-imm-dn buf +linux-68k-gc-margin-max+ +68k-d5+)
    (m68k-emit-label buf no-cap)
    ;; --- MVM allocation registers: VA=a2, VL=a3, VN=a4
    (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d1+)
    (m68k-emit-addi buf +68k-d1+ +linux-68k-heap-alloc-start+)
    (m68k-emit-move-dn-an buf +68k-d1+ +68k-a2+)          ; VA
    (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d1+)
    (m68k-emit-add-dn-dn buf +68k-d7+ +68k-d1+)
    (m68k-emit-sub-dn-dn buf +68k-d5+ +68k-d1+)
    (m68k-emit-move-dn-an buf +68k-d1+ +68k-a3+)          ; VL = mid - margin
    ;; VN = NIL = +NIL-VALUE+, not zero — see boot-linux-riscv.lisp.
    (m68k-emit-move-imm-an buf +nil-value+ +68k-a4+)
    ;; --- Cheney metadata, RAW addresses, at the heap-relative block, with
    ;;     mvm/compiler.lisp's +GC-OFF-*+ offsets
    (m68k-emit-move-imm-an buf (+ +linux-68k-heap-addr+ #x40) +68k-a0+)
    (m68k-emit-move-an-dn buf +68k-a2+ +68k-d1+)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ #x00)   ; from_start
    (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d1+)
    (m68k-emit-add-dn-dn buf +68k-d7+ +68k-d1+)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ #x08)   ; to_start = base + mid
    (m68k-emit-move-dn-disp buf +68k-d6+ +68k-a0+ #x10)   ; space_size
    (m68k-emit-move-an-dn buf +68k-a7+ +68k-d1+)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ #x18)   ; stack_base
    (m68k-emit-moveq buf +68k-d1+ 0)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ #x20)   ; gc_count
    ;; --- VFP must point at a frame before the first prologue runs
    (m68k-emit-move-an-an buf +68k-a7+ +68k-a6+)
    ;; --- fall through to translated native code
    (m68k-fixup-labels buf)
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
