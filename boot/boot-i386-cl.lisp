;;;; boot-i386-cl.lisp — bare-metal i386 entry for the CL image (Multiboot: QEMU
;;;; -kernel, scripts/dos386-chain.s's MODUS.COM on DOS, our own boot later)
;;;;
;;;; boot-arm32-cl.lisp's design on i386.  The CL runtime on 32 bits expects the
;;;; HOSTED i386 picture (boot-linux-i386.lisp): the fixed block and globals at
;;;; #x10000000.., the heap at #x19000000 with its two collector bitmaps, the
;;;; stack below #x18800000, every address the Lisp side names below 2^30.  A
;;;; PC's RAM is wherever the BIOS says, from 1 MB up, so this stub builds that
;;;; picture with 386 PAGING (4 KB pages: a 386 has no 4 MB ones) over the RAM
;;;; the loader reports, then does what the hosted stub does after its mmaps:
;;;;
;;;;   VA 0 .. image end            -> PA the same   the image (loaded at 1 MB),
;;;;                                                 the BIOS area, the IDT at
;;;;                                                 #x90000, VGA text memory
;;;;   VA #x10000000, 128 KB        -> PA after it   fixed block, globals,
;;;;                                                 handler frames, FP slots
;;;;   VA #x18700000 .. #x18800000  -> PA            the stack (1 MB)
;;;;   VA #xDEAD0000                -> PA            NIL's page, all NIL
;;;;   VA #x19000000, S + M         -> PA            the heap: two semispaces
;;;;                                                 of S/2, the guard M
;;;;   VA after it, 2 x (S+M)/128   -> PA            the two collector bitmaps
;;;;
;;;; S is chosen from the RAM left after the rest (minus room for the page
;;;; tables), M = min(16 MB, S/8) -- boot-linux-i386's rule -- and both go to
;;;; the BSS words the collector reads.  Translated with I386-SET-CL-BARE-MODE
;;;; (bare traps, hosted layout): see translate-i386.lisp *I386-CL-BARE*.
;;;;
;;;; The image's end comes from its own Multiboot header's load_end_addr, which
;;;; the build patches once the image is written (build-cl-repl-common.lisp).
;;;;
;;;; A SAVED CORE may follow the image inside load_end (scripts/dos386-core.py):
;;;; the low block, the heap and the bitmap slices as boot init left them.  With
;;;; paging on, the stub copies them into place when the heap's live part fits
;;;; below this machine's allocation limit, sets #x8024, and keeps the core's
;;;; allocation pointer and collection count; kernel-main then skips boot init
;;;; (%CORE-REQUESTED-P).  The core's pages stay mapped and unused.

(in-package :modus.mvm)

(defconstant +i386-cl-load-addr+  #x100000)
(defconstant +i386-cl-heap-va+    #x19000000 "+linux-i386-heap-hint+.")
(defconstant +i386-cl-stack-top+  #x18800000 "Hosted: +linux-i386-stack-addr+ + its 8 MB.")
(defconstant +i386-cl-stack-size+ #x100000)
(defconstant +i386-cl-low-va+     #x10000000)
(defconstant +i386-cl-low-size+   #x20000 "+linux-i386-bss-end+ - #x10000000.")
(defconstant +i386-cl-pt-reserve+ #x40000 "RAM kept back for the heap's page tables.")
;; the stub's own variables, in the low megabyte (identity-mapped, unused)
(defconstant +i386-cl-v-panext+ #x8000 "next free physical byte")
(defconstant +i386-cl-v-pd+     #x8004 "the page directory")
(defconstant +i386-cl-v-ramend+ #x8008)
(defconstant +i386-cl-v-imgend+ #x800C)
(defconstant +i386-cl-v-s+      #x8010)
(defconstant +i386-cl-v-m+      #x8014)
(defconstant +i386-cl-v-b+      #x8018 "one bitmap's bytes")
(defconstant +i386-cl-v-bm1+    #x801C)
(defconstant +i386-cl-v-bm2+    #x8020)
(defconstant +i386-cl-v-core+   #x8024
  "1 when the stub restored a saved core (%CORE-REQUESTED-P reads it), else 0.")
(defconstant +i386-cl-v-hdr+    #x8030 "The core's header (physical = virtual), or 0.")
(defconstant +i386-cl-v-tailpa+ #x8034 "The boot-only tail's first page.")
(defconstant +i386-cl-v-tail+   #x8038 "The tail's bytes the heap takes (0: none).")
(defconstant +i386-cl-v-coren+  #x803C "The core's whole pages' bytes the heap takes.")
(defconstant +i386-cl-v-hcur+   #x8040 "The next heap VA to map.")
(defconstant +i386-cl-v-free+   #x8044 "The core's allocation pointer.")
(defconstant +i386-cl-v-count+  #x8048 "The core's collection count.")
(defconstant +i386-cl-v-heapb+  #x804C "The core's heap bytes.")
(defconstant +i386-cl-v-bmb+    #x8050 "One bitmap slice's bytes.")
(defconstant +i386-cl-cmdline+  #x8200
  "The Multiboot command line, copied here by the stub (255 bytes, NUL-ended).")
(defconstant +i386-cl-core-data+ 64 "The core's header bytes; its data follow.")
(defconstant +i386-cl-boot-stack+ #x8F000 "Below the IDT at #x90000.")
(defconstant +i386-cl-tick-addr+ #x8100
  "The PIT's tick count (100 Hz), kept by the timer interrupt: the clock
   (translate-i386 *I386-PIT-CLOCK*).  Low memory, identity-mapped.")

(defun i386cl-abs (op-bytes addr)
  "OP-BYTES (a list) then the 32-bit ADDR: an absolute-operand instruction."
  (append op-bytes (i386l-le32-list addr)))

(defun emit-i386-cl-entry (buf)
  "Entry stub of the bare i386 CL image.  See the file header for the map."
  (let* ((x87 (let ((s (find-symbol "*I386-X87*" "MODUS.MVM.I386"))) (and s (symbol-value s))))
         (soft (let ((s (find-symbol "*CLI-SOFT-FLOAT*" "COMMON-LISP-USER")))
                 (and s (boundp s) (symbol-value s))))
         (nil-value +linux-i386-nil-value+)
         (g +linux-i386-globals+))
    (i386l-assemble
     buf
     (append
      (list '(#xFA) '(#xFC)                                   ; cli, cld
            (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-core+) (i386l-le32-list 0)))
      ;; the FPU: SSE2's OS bits, or x87 at 53-bit precision (MODUS_I386_FP=x87,
      ;; which runs where there is no CR4 to write -- a 386 or 486), or for
      ;; the soft-float image (MODUS_I386_SOFT_FLOAT, a 486SX or a 386 with no
      ;; coprocessor) no FPU at all: CR0.EM=1, MP=0, so a stray FPU
      ;; instruction traps (#NM) instead of doing nothing, and no CR4, which a
      ;; 486SX does not have (writing it is #UD: the image died at its first
      ;; instruction there).
      (cond
        (x87
         (list '(#xDB #xE3)                                   ; fninit
               '(#x68 #x7F #x02 #x00 #x00)                    ; push #x027F
               '(#xD9 #x2C #x24)                              ; fldcw [esp]
               '(#x83 #xC4 #x04)))                            ; add esp, 4
        (soft
         (list '(#x0F #x20 #xC0 #x83 #xC8 #x04                ; cr0: EM=1
                 #x83 #xE0 #xFD #x0F #x22 #xC0)))             ;      MP=0
        (t
         (list '(#x0F #x20 #xC0 #x25 #xFB #xFF #xFF #xFF      ; cr0: EM=0
                 #x83 #xC8 #x02 #x0F #x22 #xC0)               ;      MP=1
               '(#x0F #x20 #xE0 #x0D #x00 #x06 #x00 #x00      ; cr4: OSFXSR|OSXMMEXCPT
                 #x0F #x22 #xE0))))
      (list
       (i386cl-abs '(#x89 #x1D) #x500)                        ; mov [0x500], ebx  (the info)
       ;; the command line, to #x8200 before anything is allocated: QEMU puts
       ;; it just past the image, where the page tables are about to go
       (append (i386cl-abs '(#xC6 #x05) +i386-cl-cmdline+) '(#x00))   ; mov byte [cmdline], 0
       '(#xF6 #x03 #x04) '(:j8 #x74 cmd-done)                ; test byte [ebx], 4
       '(#x8B #x73 #x10)                                      ; mov esi, [ebx+16]
       (cons #xBF (i386l-le32-list +i386-cl-cmdline+))
       (cons #xB9 (i386l-le32-list 255))
       '(:label cmd-next)
       '(#xAC) '(#xAA) '(#x84 #xC0) '(:j8 #x74 cmd-done)      ; lodsb, stosb, test al, al
       '(#x49) '(:j8 #x75 cmd-next)                           ; dec ecx, jnz
       '(#xC6 #x07 #x00)                                      ; mov byte [edi], 0
       '(:label cmd-done)
       (cons #xBC (i386l-le32-list +i386-cl-boot-stack+))     ; mov esp, boot stack
       ;; RAM end = 1 MB + mem_upper KB; the image's end from its header
       '(#x8B #x43 #x08)                                      ; mov eax, [ebx+8]
       '(#xC1 #xE0 #x0A)                                      ; shl eax, 10
       (cons #x05 (i386l-le32-list #x100000))                 ; add eax, 1 MB
       (i386cl-abs '(#xA3) +i386-cl-v-ramend+)
       (i386cl-abs '(#xA1) (+ +i386-cl-load-addr+ 20))        ; mov eax, [load_end]
       (cons #x05 (i386l-le32-list #xFFF))
       (cons #x25 (i386l-le32-list #xFFFFF000))               ; page-round
       (i386cl-abs '(#xA3) +i386-cl-v-imgend+)
       (i386cl-abs '(#xA3) +i386-cl-v-panext+)
       ;; the page directory
       (cons #xB9 (i386l-le32-list 4096)) '(:call 0 alloc)
       (i386cl-abs '(#xA3) +i386-cl-v-pd+)
       '(#x89 #xC7) (cons #xB9 (i386l-le32-list 4096)) '(:call 0 zero)
       ;; identity: [0, image end)
       '(#x31 #xFF) '(#x31 #xF6)                              ; edi = esi = 0
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-imgend+)
       '(#xC1 #xE9 #x0C) '(:call 0 map)                    ; shr ecx, 12
       ;; the fixed block, zeroed (hosted: a fresh mapping)
       (cons #xB9 (i386l-le32-list +i386-cl-low-size+)) '(:call 0 alloc)
       '(#x50) '(#x89 #xC7) (cons #xB9 (i386l-le32-list +i386-cl-low-size+)) '(:call 0 zero)
       '(#x5E)                                                ; pop esi
       (cons #xBF (i386l-le32-list +i386-cl-low-va+))
       (cons #xB9 (i386l-le32-list (floor +i386-cl-low-size+ 4096))) '(:call 0 map)
       ;; the stack
       (cons #xB9 (i386l-le32-list +i386-cl-stack-size+)) '(:call 0 alloc)
       '(#x89 #xC6)
       (cons #xBF (i386l-le32-list (- +i386-cl-stack-top+ +i386-cl-stack-size+)))
       (cons #xB9 (i386l-le32-list (floor +i386-cl-stack-size+ 4096))) '(:call 0 map)
       ;; NIL's page, all NIL
       (cons #xB9 (i386l-le32-list 4096)) '(:call 0 alloc)
       '(#x50) '(#x89 #xC7) (cons #xB8 (i386l-le32-list nil-value))
       (cons #xB9 (i386l-le32-list 1024)) '(#xF3 #xAB)       ; rep stosd
       '(#x5E) (cons #xBF (i386l-le32-list #xDEAD0000))
       (cons #xB9 (i386l-le32-list 1)) '(:call 0 map)
       ;; ---- a SAVED CORE (scripts/dos386-core.py), found before the heap is
       ;; sized: the file's last 16 bytes are 'MODUSCOR' and the header's
       ;; physical address.  Header: 'MCOR', version 2, the allocation pointer,
       ;; the heap bytes from #x19000000, one bitmap slice's bytes, the
       ;; boot-only tail's first and last page (0 0: none); at +64 the low
       ;; block, the heap, both slices.  Once restored, the tail's code and the
       ;; core's own pages are dead, so they count as RAM here and become the
       ;; heap's first pages (tail, then core).  Copying the core into them is
       ;; safe going forward: every destination lies below its source.
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-hdr+) (i386l-le32-list 0))
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-tail+) (i386l-le32-list 0))
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-coren+) (i386l-le32-list 0))
       (i386cl-abs '(#xA1) (+ +i386-cl-load-addr+ 20))        ; mov eax, [load_end]
       (cons #x3D (i386l-le32-list (+ +i386-cl-load-addr+ 64)))  ; long enough?
       '(:jcc32 #x72 probe-done)                              ; jb
       (cons #x81 (cons #x78 (cons #xF0 (i386l-le32-list #x55444F4D))))  ; cmp [eax-16], 'MODU'
       '(:jcc32 #x75 probe-done)
       (cons #x81 (cons #x78 (cons #xF4 (i386l-le32-list #x524F4353))))  ; cmp [eax-12], 'SCOR'
       '(:jcc32 #x75 probe-done)
       '(#x8B #x58 #xF8)                                      ; mov ebx, [eax-8]
       (cons #x81 (cons #x3B (i386l-le32-list #x524F434D)))   ; cmp [ebx], 'MCOR'
       '(:jcc32 #x75 probe-done)
       (cons #x83 '(#x7B #x04 #x02))                          ; cmp dword [ebx+4], 2
       '(:jcc32 #x75 probe-done)
       (i386cl-abs '(#x89 #x1D) +i386-cl-v-hdr+)
       '(#x8B #x4B #x14) (i386cl-abs '(#x89 #x0D) +i386-cl-v-tailpa+)   ; tail's first page
       '(#x8B #x53 #x18) '(#x29 #xCA)                         ; edx = last - first
       '(:j8 #x76 no-tail)                                    ; jbe
       (i386cl-abs '(#x89 #x15) +i386-cl-v-tail+)
       '(:label no-tail)
       ;; the core's whole pages: [header, (load_end - 16) & ~4095)
       '(#x89 #xC1) '(#x83 #xE9 #x10)                         ; ecx = load_end - 16
       '(#x81 #xE1) (i386l-le32-list #xFFFFF000)
       '(#x29 #xD9)                                           ; - header
       '(:j8 #x76 probe-done)                                 ; jbe
       (i386cl-abs '(#x89 #x0D) +i386-cl-v-coren+)
       '(:label probe-done)
       ;; ---- the heap's size from what is left: X = R - R/32 covers S + M and
       ;;      the bitmaps (2/128); M = min(16 MB, X/9), S = X - M ... -----------
       '(:label size-again)
       (i386cl-abs '(#xA1) +i386-cl-v-ramend+)
       (i386cl-abs '(#x2B #x05) +i386-cl-v-panext+)          ; sub eax, [panext]
       (cons #x2D (i386l-le32-list +i386-cl-pt-reserve+))     ; sub eax, reserve
       (i386cl-abs '(#x03 #x05) +i386-cl-v-tail+)            ; + the dead tail
       (i386cl-abs '(#x03 #x05) +i386-cl-v-coren+)           ; + the core's pages
       '(#x89 #xC2) '(#xC1 #xEA #x05) '(#x29 #xD0)            ; eax -= eax/32
       '(#x89 #xC7)                                           ; edi = X
       '(#x31 #xD2) (cons #xB9 (i386l-le32-list 9)) '(#xF7 #xF1) ; eax = X/9
       (cons #x3D (i386l-le32-list +linux-i386-gc-guard+))
       '(:j8 #x76 m-small)
       (cons #xBB (i386l-le32-list +linux-i386-gc-guard+))    ; M = 16 MB
       '(#x89 #xF8) '(#x29 #xD8)                              ; S = X - M
       '(:j8 #xEB m-done)
       '(:label m-small)
       '(#x89 #xC3) '(#xC1 #xE0 #x03)                         ; M = X/9, S = 8M
       '(:label m-done)
       (cons #x3D (i386l-le32-list #x20000000))               ; S <= 512 MB
       '(:j8 #x76 s-ok)
       (cons #xB8 (i386l-le32-list #x20000000))
       (cons #xBB (i386l-le32-list +linux-i386-gc-guard+))
       '(:label s-ok)
       (cons #x25 (i386l-le32-list #xFFFC0000))               ; S to 256 KB
       '(#x81 #xE3 #x00 #xF0 #xFF #xFF)                       ; M to a page
       (i386cl-abs '(#xA3) +i386-cl-v-s+)
       (i386cl-abs '(#x89 #x1D) +i386-cl-v-m+)
       ;; the core fits if its allocation pointer is below this machine's
       ;; limit, heap + S/2 - M; if not, size again without it (a cold boot)
       (i386cl-abs '(#x8B #x1D) +i386-cl-v-hdr+)
       '(#x85 #xDB) '(:jcc32 #x74 fits)                       ; test ebx, ebx; jz
       '(#xD1 #xE8)                                           ; eax = S/2
       (cons #x05 (i386l-le32-list +i386-cl-heap-va+))
       (i386cl-abs '(#x2B #x05) +i386-cl-v-m+)
       '(#x39 #x43 #x08)                                      ; cmp [ebx+8], eax
       '(:jcc32 #x72 fits)                                    ; jb
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-hdr+) (i386l-le32-list 0))
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-tail+) (i386l-le32-list 0))
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-coren+) (i386l-le32-list 0))
       '(:jmp32 0 size-again)
       '(:label fits)
       ;; the heap, S + M at VA #x19000000 (not zeroed: every allocation
       ;; initialises what it takes, as on the hosted mmap's fresh pages):
       ;; the tail's pages, the core's, then fresh RAM.  [hcur] = next VA.
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-hcur+) (i386l-le32-list +i386-cl-heap-va+))
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-tail+)
       '(#x85 #xC9) '(:j8 #x74 map-no-tail)
       (i386cl-abs '(#x8B #x35) +i386-cl-v-tailpa+)
       (i386cl-abs '(#x8B #x3D) +i386-cl-v-hcur+)
       '(#xC1 #xE9 #x0C) '(:call 0 map)
       (i386cl-abs '(#xA1) +i386-cl-v-tail+) (i386cl-abs '(#x01 #x05) +i386-cl-v-hcur+)
       '(:label map-no-tail)
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-coren+)
       '(#x85 #xC9) '(:j8 #x74 map-no-core)
       (i386cl-abs '(#x8B #x35) +i386-cl-v-hdr+)
       (i386cl-abs '(#x8B #x3D) +i386-cl-v-hcur+)
       '(#xC1 #xE9 #x0C) '(:call 0 map)
       (i386cl-abs '(#xA1) +i386-cl-v-coren+) (i386cl-abs '(#x01 #x05) +i386-cl-v-hcur+)
       '(:label map-no-core)
       ;; the rest: S + M - (hcur - heap)
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-s+) (i386cl-abs '(#x03 #x0D) +i386-cl-v-m+)
       (cons #x81 (cons #xC1 (i386l-le32-list +i386-cl-heap-va+)))   ; add ecx, heap
       (i386cl-abs '(#x2B #x0D) +i386-cl-v-hcur+)
       '(#x51) '(:call 0 alloc) '(#x89 #xC6) '(#x59)          ; esi = PA
       '(#xC1 #xE9 #x0C)
       (i386cl-abs '(#x8B #x3D) +i386-cl-v-hcur+) '(:call 0 map)
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-s+) (i386cl-abs '(#x03 #x0D) +i386-cl-v-m+)  ; ecx = S+M
       ;; one bitmap = (S+M)/128, page-rounded; both zeroed, after the heap
       '(#xC1 #xE9 #x07)
       '(#x81 #xC1) (i386l-le32-list #xFFF)
       '(#x81 #xE1) (i386l-le32-list #xFFFFF000)
       (i386cl-abs '(#x89 #x0D) +i386-cl-v-b+)
       (i386cl-abs '(#xA1) +i386-cl-v-s+)
       (i386cl-abs '(#x03 #x05) +i386-cl-v-m+)
       (cons #x05 (i386l-le32-list +i386-cl-heap-va+))
       (i386cl-abs '(#xA3) +i386-cl-v-bm1+)
       (i386cl-abs '(#x03 #x05) +i386-cl-v-b+)
       (i386cl-abs '(#xA3) +i386-cl-v-bm2+))
      (loop for v in (list +i386-cl-v-bm1+ +i386-cl-v-bm2+)
            append (list (i386cl-abs '(#x8B #x0D) +i386-cl-v-b+) '(:call 0 alloc)
                         '(#x50) '(#x89 #xC7) (i386cl-abs '(#x8B #x0D) +i386-cl-v-b+)
                         '(:call 0 zero) '(#x5E)
                         (i386cl-abs '(#x8B #x3D) v)            ; mov edi, [bmN va]
                         (i386cl-abs '(#x8B #x0D) +i386-cl-v-b+) '(#xC1 #xE9 #x0C)
                         '(:call 0 map)))
      (list
       ;; ---- paging on ----
       (i386cl-abs '(#xA1) +i386-cl-v-pd+)
       '(#x0F #x22 #xD8)                                      ; mov cr3, eax
       '(#x0F #x20 #xC0) (cons #x0D (i386l-le32-list #x80000000)) '(#x0F #x22 #xC0)
       '(:j8 #xEB paged)                                      ; serialise
       '(:label paged)
       (cons #xBC (i386l-le32-list (- +i386-cl-stack-top+ 16)))   ; mov esp, stack top
       ;; ---- the saved core, if the probe above kept it: every header field
       ;; it needs is read FIRST, since copying the heap overwrites the
       ;; header's page (the core's pages are now the heap's)
       (i386cl-abs '(#x8B #x1D) +i386-cl-v-hdr+)
       '(#x85 #xDB) '(:jcc32 #x74 no-core)
       '(#x8B #x43 #x08) (i386cl-abs '(#xA3) +i386-cl-v-free+)
       (cons #x8B (cons #x83 (i386l-le32-list (+ +i386-cl-core-data+ (- +gc-count-addr+ +i386-cl-low-va+)))))
       (i386cl-abs '(#xA3) +i386-cl-v-count+)
       '(#x8B #x43 #x0C) (i386cl-abs '(#xA3) +i386-cl-v-heapb+)
       '(#x8B #x43 #x10) (i386cl-abs '(#xA3) +i386-cl-v-bmb+)
       (list #x8D #x73 +i386-cl-core-data+)                   ; lea esi, [ebx+64]
       (cons #xBF (i386l-le32-list +i386-cl-low-va+))
       (cons #xB9 (i386l-le32-list (floor +i386-cl-low-size+ 4))) '(#xF3 #xA5)   ; rep movsd
       (cons #xBF (i386l-le32-list +i386-cl-heap-va+))
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-heapb+) '(#xC1 #xE9 #x02) '(#xF3 #xA5)
       (i386cl-abs '(#x8B #x3D) +i386-cl-v-bm1+)
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-bmb+) '(#xC1 #xE9 #x02) '(#xF3 #xA5)
       (i386cl-abs '(#x8B #x3D) +i386-cl-v-bm2+)
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-bmb+) '(#xC1 #xE9 #x02) '(#xF3 #xA5)
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-core+) (i386l-le32-list 1))
       '(:label no-core)
       ;; ---- what the hosted stub does after its mmaps (boot-linux-i386) ----
       (i386cl-abs '(#xA1) +i386-cl-v-s+)
       (i386cl-abs '(#xA3) +linux-i386-heap-total-addr+)
       (i386cl-abs '(#xA1) +i386-cl-v-m+)
       (i386cl-abs '(#xA3) +linux-i386-heap-margin-addr+)
       ;; VA = from_start = page_base = heap + alloc_start
       (cons #xB9 (i386l-le32-list (+ +i386-cl-heap-va+ +linux-i386-heap-alloc-start+)))
       (i386cl-abs '(#x89 #x0D) (+ g #x00))
       (i386cl-abs '(#x89 #x0D) +gc-from-start-addr+)
       (i386cl-abs '(#x89 #x0D) (+ g #x18))
       (i386cl-abs '(#x89 #x0D) #x10000E00)
       ;; to_start = heap + S/2; VL = to_start - M; space_size = S/2 - alloc_start
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-s+) '(#xD1 #xE9)  ; ecx = S/2
       '(#x89 #xC8) (cons #x2D (i386l-le32-list +linux-i386-heap-alloc-start+))
       (i386cl-abs '(#xA3) +gc-space-size-addr+)
       '(#x81 #xC1) (i386l-le32-list +i386-cl-heap-va+)
       (i386cl-abs '(#x89 #x0D) +gc-to-start-addr+)
       (i386cl-abs '(#x2B #x0D) +i386-cl-v-m+)
       (i386cl-abs '(#x89 #x0D) (+ g #x04))                  ; VL
       (i386cl-abs '(#xC7 #x05) (+ g #x08)) (i386l-le32-list nil-value)  ; VN = NIL
       (i386cl-abs '(#xC7 #x05) +gc-stack-base-addr+) (i386l-le32-list (- +i386-cl-stack-top+ 16))
       (i386cl-abs '(#xC7 #x05) +gc-count-addr+) (i386l-le32-list 0)
       ;; the bitmaps' bases, and gc.lisp's copies
       (i386cl-abs '(#xA1) +i386-cl-v-bm1+)
       (i386cl-abs '(#xA3) (+ g #x1C)) (i386cl-abs '(#xA3) #x10000E18)
       (i386cl-abs '(#xA1) +i386-cl-v-bm2+)
       (i386cl-abs '(#xA3) (+ g #x20)) (i386cl-abs '(#xA3) #x10000E40)
       ;; a restored core: its allocation pointer, and its collection count
       ;; (memos keyed on the GC epoch were made at the same addresses)
       (append (i386cl-abs '(#x83 #x3D) +i386-cl-v-core+) '(#x01))   ; cmp [core], 1
       '(:jcc32 #x75 core-done)
       (i386cl-abs '(#xA1) +i386-cl-v-free+) (i386cl-abs '(#xA3) (+ g #x00))
       (i386cl-abs '(#xA1) +i386-cl-v-count+) (i386cl-abs '(#xA3) +gc-count-addr+)
       '(:label core-done)
       '(:jmp32 0 past-subroutines))
      ;; ---- subroutines (physical addresses: called with paging off) ----
      ;; alloc: ECX bytes (page-rounded) -> EAX = PA.  Clobbers ECX, EDX.
      (list '(:label alloc)
            (i386cl-abs '(#xA1) +i386-cl-v-panext+)
            '(#x81 #xC1) (i386l-le32-list #xFFF)
            '(#x81 #xE1) (i386l-le32-list #xFFFFF000)
            '(#x8D #x14 #x08)                                   ; lea edx, [eax+ecx]
            (i386cl-abs '(#x89 #x15) +i386-cl-v-panext+)
            '(#xC3)
            ;; zero: EDI = PA, ECX = bytes.  Clobbers EAX, ECX, EDI.
            '(:label zero)
            '(#xC1 #xE9 #x02) '(#x31 #xC0) '(#xF3 #xAB) '(#xC3)
            ;; map: EDI = VA, ESI = PA, ECX = pages.  Clobbers EAX-EDX, ESI, EDI.
            '(:label map)
            '(#x85 #xC9) '(:j8 #x74 map-done)                   ; test ecx, ecx
            '(#x89 #xFB) '(#xC1 #xEB #x16) '(#xC1 #xE3 #x02)    ; ebx = (va >> 22) * 4
            (i386cl-abs '(#x03 #x1D) +i386-cl-v-pd+)            ; + PD: &PDE
            '(#x8B #x03) '(#xA8 #x01) '(:j8 #x75 map-pt)        ; present?
            '(#x51) '(#x57)                                     ; push ecx, edi
            (cons #xB9 (i386l-le32-list 4096)) '(:call 0 alloc)
            '(#x50) '(#x89 #xC7) (cons #xB9 (i386l-le32-list 4096)) '(:call 0 zero)
            '(#x58) '(#x5F) '(#x59)                             ; pop eax, edi, ecx
            '(#x83 #xC8 #x03) '(#x89 #x03)                      ; PDE = PT | present|rw
            '(:label map-pt)
            '(#x25 #x00 #xF0 #xFF #xFF)                         ; PT base
            '(#x89 #xFA) '(#xC1 #xEA #x0C) '(#x81 #xE2) (i386l-le32-list #x3FF)
            '(#x8D #x14 #x90)                                   ; lea edx, [eax+edx*4]
            '(#x89 #xF0) '(#x83 #xC8 #x03) '(#x89 #x02)         ; PTE = PA | present|rw
            '(#x81 #xC7) (i386l-le32-list 4096)
            '(#x81 #xC6) (i386l-le32-list 4096)
            '(#x49) '(:j8 #xEB map)                             ; dec ecx
            '(:label map-done) '(#xC3)
            '(:label past-subroutines))))
    ;; the console and the timer, as the old bare entry does
    (i386-init-serial buf)
    (i386l-bytes buf #xC7 #x05) (i386l-le32 buf +i386-cl-tick-addr+) (i386l-le32 buf 0)
    (emit-i386-interrupt-setup buf +i386-cl-tick-addr+)
    (i386l-bytes buf #xFB)                                    ; sti: the clock ticks
    (mvm-emit-byte buf #x89) (mvm-emit-byte buf #xE5)        ; mov ebp, esp
    ;; native byte 0 on a 16-byte boundary (OR-3 tagged entries): the JMP
    ;; rel32 to kernel-main that cross.lisp appends is 5 bytes
    (loop until (zerop (mod (+ (mvm-buffer-position buf) 5) 16))
          do (mvm-emit-byte buf #x90))))

(defun i386-cl-boot-descriptor ()
  "Boot descriptor for the bare i386 CL image (Multiboot, paging over the
   hosted picture)."
  (list :arch :i386
        :multiboot-header-fn #'emit-i386-multiboot-header
        :entry-fn #'emit-i386-cl-entry
        :serial-init-fn (lambda (buf) (declare (ignore buf)) nil)  ; the entry does it
        :percpu-layout-fn #'i386-percpu-layout
        :load-addr +i386-cl-load-addr+
        :stack-top (- +i386-cl-stack-top+ 16)
        :cons-base (+ +i386-cl-heap-va+ +linux-i386-heap-alloc-start+)
        :general-base +i386-cl-heap-va+
        :percpu-base +i386-percpu-base+
        :percpu-stride +i386-percpu-stride+))

(defun i386-cl-patch-load-end (path)
  "Write PATH's own end into its Multiboot header's load_end_addr (the entry
   stub reads it to place the page tables after the image).  The aout-kludge
   header is at file offset 0; load_end_addr is its sixth word."
  (with-open-file (s path :direction :io :element-type '(unsigned-byte 8)
                          :if-exists :overwrite)
    (let ((end (+ +i386-cl-load-addr+ (file-length s))))
      (file-position s 20)
      (dotimes (i 4) (write-byte (ldb (byte 8 (* 8 i)) end) s))
      end)))
