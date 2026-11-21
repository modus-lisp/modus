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
(defvar *i386-cl-max-heap*
  (let ((v (sb-ext:posix-getenv "MODUS_I386_MAX_HEAP_MB")))
    (if (and v (plusp (length v))) (* (parse-integer v) #x100000) #x20000000))
  "The heap's S at most: 512 MB, or MODUS_I386_MAX_HEAP_MB (a test knob: a
   small heap on a big machine, so a collection comes early).")
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
;; DEMAND-PAGED CODE (a version-3 core with a code store): the restored
;; process's native code is not resident; a page fault in [plo, phi) maps a
;; slot of the pool there and decodes the 16 KB chunk into it.
(defconstant +i386-cl-v-plo+    #x8054 "Paged code: first chunk's address (0: no paging).")
(defconstant +i386-cl-v-phi+    #x8058 "Paged code: end.")
(defconstant +i386-cl-v-store+  #x805C "The code store's header (physical = virtual).")
(defconstant +i386-cl-v-poolpa+ #x8060 "The pool's first page.")
(defconstant +i386-cl-v-nslots+ #x8064 "The pool's 16 KB slots.")
(defconstant +i386-cl-v-next+   #x8068 "The next slot to fill (round robin).")
(defconstant +i386-cl-v-pagedn+ #x806C "The paged code's bytes, given to the heap.")
(defconstant +i386-cl-v-pool+   #x8070 "The pool's bytes.")
(defconstant +i386-cl-v-pfaddr+ #x8074 "The page-fault handler's address.")
(defconstant +i386-cl-v-faults+ #x8078 "Chunks decoded so far (a count, for the curious).")
(defconstant +i386-cl-v-rem+    #x807C "Heap bytes still to map.")
(defconstant +i386-cl-v-hole+   #x8080
  "A version-4 core's GAP: the paged code's bytes, which the file leaves out
   (scripts/dos386-core.py), or 0.  Everything the file holds past [plo, phi)
   sits that much lower in physical memory than its address.")
(defconstant +i386-cl-v-holelo+ #x8084 "The gap's plo, read before the probe.")
(defconstant +i386-cl-v-holehi+ #x8088 "The gap's phi.")
(defconstant +i386-cl-v-static-lo+ #x8090
  "A core boot's STATIC heap (translate-i386.lisp +I386-GC-STATIC-LO+): the
   restored heap, walked by every collection and never copied.")
(defconstant +i386-cl-v-static-hi+ #x8094 "Its end; 0: none.")
(defconstant +i386-cl-v-dataoff+ #x8098
  "A version-5 core's code store data: its file offset (0: in memory).  A
   page fault reads the chunk's sectors through the no-DOS loader's PM_READ.")
(defconstant +i386-cl-v-imglba+ #x809C "The image file's first sector (the boot record's).")
(defconstant +i386-cl-v-diskread+ #x80A0 "PM_READ's address (the boot record's).")
(defconstant +i386-cl-bootrec+ #x0580
  "scripts/pcboot2.s's boot record: 'PCBT', PM_READ, the image's first sector,
   the drive.")
(defconstant +i386-cl-v-stacksz+ #x80A4
  "The stack's bytes: 1 MB, or 384 KB on a machine under 16 MB.")
(defvar *i386-cl-gc-static*
  (not (equal (sb-ext:posix-getenv "MODUS_I386_GC_STATIC") "0"))
  "A core boot's restored heap is static, the semispaces above it hold only
   what is allocated after (the 68k's MODUS_68K_GC_STATIC): the heap needs the
   boot data once, not twice.  MODUS_I386_GC_STATIC=0: the old layout.")
(defconstant +i386-cl-owners+   #x8400 "Per slot: the chunk address it holds, or -1.  768 slots at most.")
(defconstant +i386-cl-pt-window+ #xFFC00000
  "The page tables, through the page directory's last entry pointing at itself.")
(defconstant +i386-cl-cmdline+  #x8200
  "The Multiboot command line, copied here by the stub (255 bytes, NUL-ended).")
(defconstant +i386-cl-core-data+ 64 "The core's header bytes; its data follow.")
(defconstant +i386-cl-boot-stack+ #x8F000 "Below the IDT at #x90000.")
(defconstant +i386-cl-tick-addr+ #x8100
  "The PIT's tick count (100 Hz), kept by the timer interrupt: the clock
   (translate-i386 *I386-PIT-CLOCK*).  Low memory, identity-mapped.")

(defun i386cl-say-and-halt (text)
  "Items that write TEXT to the top-left of VGA text memory and COM1, then
   stop -- the no-ram exit's shape, for any message."
  (append
   (list (cons #xBF (i386l-le32-list #xB8000)))
   (loop for ch across text
         for c = (char-code ch)
         append (list (list #x66 #xC7 #x07 c #x4F) '(#x83 #xC7 #x02)
                      '(#x66 #xBA #xF8 #x03) (list #xB0 c) '(#xEE)))
   (list '(#xFA) '(#xF4) '(#xEB #xFC))))                  ; cli; hlt; jmp hlt

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
            (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-core+) (i386l-le32-list 0))
            (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-static-lo+) (i386l-le32-list 0))
            (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-static-hi+) (i386l-le32-list 0))
            (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-dataoff+) (i386l-le32-list 0)))
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
       ;; identity: [0, image end) -- or, with a version-4 core, whose file
       ;; leaves the paged code [plo, phi) out (scripts/dos386-core.py): the
       ;; identity [0, phi) and [phi, ...) onto the file's rest, which the
       ;; loader put at plo.  [plo, phi)'s entries go when the paged code is
       ;; unmapped below; mapping them first makes their page tables.
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-hole+) (i386l-le32-list 0))
       (i386cl-abs '(#xA1) (+ +i386-cl-load-addr+ 20))        ; mov eax, [load_end]
       (cons #x3D (i386l-le32-list (+ +i386-cl-load-addr+ 64)))
       '(:jcc32 #x72 map-plain)
       (cons #x81 (cons #x78 (cons #xF0 (i386l-le32-list #x55444F4D))))  ; 'MODU'
       '(:jcc32 #x75 map-plain)
       (cons #x81 (cons #x78 (cons #xF4 (i386l-le32-list #x524F4353))))  ; 'SCOR'
       '(:jcc32 #x75 map-plain)
       '(#x8B #x58 #xF8)                                      ; ebx = header
       (cons #x81 (cons #x3B (i386l-le32-list #x524F434D)))   ; 'MCOR'
       '(:jcc32 #x75 map-plain)
       (cons #x83 '(#x7B #x04 #x04))                          ; version 4?
       '(:jcc32 #x72 map-plain)
       '(#x8B #x4B #x24) '(#x85 #xC9) '(:jcc32 #x74 map-plain)    ; ecx = [hdr+36], the gap
       (i386cl-abs '(#x89 #x0D) +i386-cl-v-hole+)
       '(#x8B #x53 #x1C)                                      ; edx = the store (physical)
       '(#x8B #x02) (i386cl-abs '(#xA3) +i386-cl-v-holelo+)   ; plo
       '(#x8B #x42 #x04) (i386cl-abs '(#xA3) +i386-cl-v-holehi+)  ; phi
       '(#x31 #xFF) '(#x31 #xF6)                              ; [0, phi) identity
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-holehi+)
       '(#xC1 #xE9 #x0C) '(:call 0 map)
       (i386cl-abs '(#x8B #x3D) +i386-cl-v-holehi+)           ; [phi, ..) -> plo
       (i386cl-abs '(#x8B #x35) +i386-cl-v-holelo+)
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-imgend+)
       '(#x29 #xF1) '(#xC1 #xE9 #x0C) '(:call 0 map)          ; sub ecx, esi; shr 12
       '(:jmp32 0 map-image-done)
       '(:label map-plain)
       '(#x31 #xFF) '(#x31 #xF6)                              ; edi = esi = 0
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-imgend+)
       '(#xC1 #xE9 #x0C) '(:call 0 map)                    ; shr ecx, 12
       '(:label map-image-done)
       ;; the fixed block, zeroed (hosted: a fresh mapping)
       (cons #xB9 (i386l-le32-list +i386-cl-low-size+)) '(:call 0 alloc)
       '(#x50) '(#x89 #xC7) (cons #xB9 (i386l-le32-list +i386-cl-low-size+)) '(:call 0 zero)
       '(#x5E)                                                ; pop esi
       (cons #xBF (i386l-le32-list +i386-cl-low-va+))
       (cons #xB9 (i386l-le32-list (floor +i386-cl-low-size+ 4096))) '(:call 0 map)
       ;; the stack: 1 MB of extended memory, or on a machine under 16 MB
       ;; 256 KB of CONVENTIONAL memory, [4F000h, 8F000h), which nothing uses
       ;; once the image runs (the boot stack's top is in it, and done with
       ;; before this stack is used): an 8 MB machine has no extended memory
       ;; to spare, and the REPL's deepest work fits
       (cons #xB9 (i386l-le32-list +i386-cl-stack-size+))
       (cons #x81 (cons #x3D (append (i386l-le32-list +i386-cl-v-ramend+) (i386l-le32-list #x1000000))))
       '(:j8 #x73 stack-big)                                  ; jae
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-stacksz+) (i386l-le32-list #x40000))
       (cons #xBE (i386l-le32-list #x4F000))                  ; esi = PA
       '(:j8 #xEB stack-map)
       '(:label stack-big)
       (i386cl-abs '(#x89 #x0D) +i386-cl-v-stacksz+)
       '(:call 0 alloc)
       '(#x89 #xC6)
       '(:label stack-map)
       (cons #xBF (i386l-le32-list +i386-cl-stack-top+))
       (i386cl-abs '(#x2B #x3D) +i386-cl-v-stacksz+)          ; edi = top - size
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-stacksz+) '(#xC1 #xE9 #x0C) '(:call 0 map)
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
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-plo+) (i386l-le32-list 0))
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-phi+) (i386l-le32-list 0))
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-store+) (i386l-le32-list 0))
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-pagedn+) (i386l-le32-list 0))
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-pool+) (i386l-le32-list 0))
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-faults+) (i386l-le32-list 0))
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
       '(:jcc32 #x72 probe-done)                              ; jb: versions 2 and 3
       (i386cl-abs '(#x89 #x1D) +i386-cl-v-hdr+)
       '(#x8B #x4B #x14) (i386cl-abs '(#x89 #x0D) +i386-cl-v-tailpa+)   ; tail's first page
       '(#x8B #x53 #x18) '(#x29 #xCA)                         ; edx = last - first
       '(:j8 #x76 no-tail)                                    ; jbe
       (i386cl-abs '(#x89 #x15) +i386-cl-v-tail+)
       '(:label no-tail)
       ;; the core's whole pages: [header, (load_end - 16) & ~4095), or, with
       ;; a code store (version 3), [header, store): the store stays
       '(#x89 #xC1) '(#x83 #xE9 #x10)                         ; ecx = load_end - 16
       '(#x81 #xE1) (i386l-le32-list #xFFFFF000)
       (cons #x83 '(#x7B #x04 #x03))                          ; cmp dword [ebx+4], 3
       '(:jcc32 #x72 core-span)                               ; jb
       '(#x8B #x4B #x1C)                                      ; ecx = store
       (i386cl-abs '(#x89 #x0D) +i386-cl-v-store+)
       '(#x8B #x01) (i386cl-abs '(#xA3) +i386-cl-v-plo+)      ; store: plo, phi
       '(#x8B #x41 #x04) (i386cl-abs '(#xA3) +i386-cl-v-phi+)
       (i386cl-abs '(#x2B #x05) +i386-cl-v-plo+)
       (i386cl-abs '(#xA3) +i386-cl-v-pagedn+)
       ;; version 5: the store's data is on the disk -- the boot record says
       ;; where the image starts and how to read it, or there is no paging
       (cons #x83 '(#x7B #x04 #x05))                          ; cmp dword [ebx+4], 5
       '(:jcc32 #x72 store-in-ram)
       '(#x8B #x43 #x28) (i386cl-abs '(#xA3) +i386-cl-v-dataoff+)  ; [hdr+40]
       (cons #x81 (cons #x3D (append (i386l-le32-list +i386-cl-bootrec+)
                                     (i386l-le32-list #x54424350))))   ; cmp [rec], 'PCBT'
       '(:jcc32 #x75 no-disk)
       (i386cl-abs '(#xA1) (+ +i386-cl-bootrec+ 8))
       '(#x85 #xC0) '(:jcc32 #x74 no-disk)
       (i386cl-abs '(#xA3) +i386-cl-v-imglba+)
       (i386cl-abs '(#xA1) (+ +i386-cl-bootrec+ 4))
       (i386cl-abs '(#xA3) +i386-cl-v-diskread+)
       '(:label store-in-ram)
       ;; the pool: RAM/4, at least 2 MB, at most the paged code itself (then
       ;; nothing is ever evicted) and 12 MB.  Measured with
       ;; test/dos386/coverage-forms.lisp: its code working set is ~6 MB;
       ;; a 6 MB pool ran it in the time of an unpaged image, 4 MB 35%
       ;; slower, 3 MB 4x.
       (i386cl-abs '(#xA1) +i386-cl-v-ramend+) '(#xC1 #xE8 #x02)
       (cons #x3D (i386l-le32-list #x200000)) '(:j8 #x73 pool-lo)
       (cons #xB8 (i386l-le32-list #x200000))
       '(:label pool-lo)
       (i386cl-abs '(#x3B #x05) +i386-cl-v-pagedn+) '(:j8 #x76 pool-paged)
       (i386cl-abs '(#xA1) +i386-cl-v-pagedn+)
       '(:label pool-paged)
       (cons #x3D (i386l-le32-list #xC00000)) '(:j8 #x76 pool-hi)
       (cons #xB8 (i386l-le32-list #xC00000))
       '(:label pool-hi)
       ;; ... unless the header names a size (+32; 0: this rule), 12 MB at most
       '(#x8B #x53 #x20) '(#x85 #xD2) '(:j8 #x74 pool-rule)
       '(#x89 #xD0) (cons #x3D (i386l-le32-list #xC00000)) '(:j8 #x76 pool-rule)
       (cons #xB8 (i386l-le32-list #xC00000))
       '(:label pool-rule)
       (i386cl-abs '(#x3B #x05) +i386-cl-v-pagedn+) '(:j8 #x76 pool-fits)
       (i386cl-abs '(#xA1) +i386-cl-v-pagedn+)
       '(:label pool-fits)
       ;; with a gap the pool is RAM the heap could have had: of what is left,
       ;; L, the pool takes max(L/3, L - 8 MB), from 512 KB up to the rule
       ;; above (a header-named size still wins).  L/3 is the small machine's
       ;; share (6 MB: ~1 MB, 8 MB: ~1.7 MB -- at 512 KB the code thrashed,
       ;; 7651 faults over 103 x (make-list 10000)), L - 8 MB the large one's
       '(#x51)                                                ; push ecx (the store, for core-span)
       (append (i386cl-abs '(#x83 #x3D) +i386-cl-v-hole+) '(#x00))
       '(:jcc32 #x74 pool-capped)
       '(#x83 #x7B #x20 #x00) '(:jcc32 #x75 pool-capped)      ; cmp dword [ebx+32], 0
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-ramend+)
       (i386cl-abs '(#x2B #x0D) +i386-cl-v-panext+)
       '(:j8 #x72 pool-min)                                   ; jb
       '(#x81 #xE9) (i386l-le32-list +i386-cl-pt-reserve+)    ; sub ecx, PTs: ecx = L
       '(:j8 #x72 pool-min)
       '(#x50)                                                ; push eax (the cap)
       '(#x89 #xCA)                                           ; edx = L
       '(#x81 #xEA) (i386l-le32-list #x800000)                ; edx = L - 8 MB
       '(:j8 #x73 pool-big)                                   ; jae
       '(#x31 #xD2)                                           ; edx = 0
       '(:label pool-big)
       '(#x52)                                                ; push edx
       '(#x89 #xC8) '(#x31 #xD2)                              ; eax = L, edx = 0
       (cons #xB9 (i386l-le32-list 3)) '(#xF7 #xF1)           ; eax = L/3
       '(#x5A)                                                ; pop edx
       '(#x39 #xD0) '(:j8 #x73 pool-third)                    ; cmp eax, edx; jae
       '(#x89 #xD0)                                           ; eax = L - 8 MB
       '(:label pool-third)
       '(#x89 #xC1)                                           ; ecx = the share
       '(#x58)                                                ; pop eax (the cap)
       '(#x39 #xC8) '(:j8 #x76 pool-floor)                    ; cmp eax, ecx; jbe
       '(#x89 #xC8)                                           ; eax = ecx
       '(:label pool-floor)
       (cons #x3D (i386l-le32-list #x80000)) '(:j8 #x73 pool-capped)   ; >= 512 KB
       '(:label pool-min)
       (cons #xB8 (i386l-le32-list #x80000))
       '(:label pool-capped)
       '(#x59)                                                ; pop ecx
       (cons #x25 (i386l-le32-list #xFFFFC000))
       (i386cl-abs '(#xA3) +i386-cl-v-pool+)
       ;; a gap: the paged code has no pages to give, the pool comes from
       ;; fresh RAM (pagedn = pool makes both of their terms below vanish)
       (append (i386cl-abs '(#x83 #x3D) +i386-cl-v-hole+) '(#x00))
       '(:jcc32 #x74 core-span)
       (i386cl-abs '(#xA3) +i386-cl-v-pagedn+)
       '(:label core-span)
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
       (i386cl-abs '(#x03 #x05) +i386-cl-v-pagedn+)          ; + the paged code
       (i386cl-abs '(#x2B #x05) +i386-cl-v-pool+)            ; - the pool
       ;; ... and with a gap, whose pool is fresh RAM (pagedn = pool cancelled
       ;; the two terms above), the pool again
       (append (i386cl-abs '(#x83 #x3D) +i386-cl-v-hole+) '(#x00))
       '(:j8 #x74 size-nogap)
       (i386cl-abs '(#x2B #x05) +i386-cl-v-pool+)
       '(:label size-nogap)
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
       (cons #x3D (i386l-le32-list *i386-cl-max-heap*))       ; S <= 512 MB
       '(:j8 #x76 s-ok)
       (cons #xB8 (i386l-le32-list *i386-cl-max-heap*))
       (cons #xBB (i386l-le32-list (if (= *i386-cl-max-heap* #x20000000)
                                       +linux-i386-gc-guard+
                                       (floor *i386-cl-max-heap* 8))))
       '(:label s-ok)
       (cons #x25 (i386l-le32-list #xFFFC0000))               ; S to 256 KB
       '(#x81 #xE3 #x00 #xF0 #xFF #xFF)                       ; M to a page
       (i386cl-abs '(#xA3) +i386-cl-v-s+)
       (i386cl-abs '(#x89 #x1D) +i386-cl-v-m+)
       ;; the core fits if its allocation pointer is below this machine's
       ;; limit, heap + S/2 - M; if not, size again without it (a cold boot)
       (i386cl-abs '(#x8B #x1D) +i386-cl-v-hdr+)
       '(#x85 #xDB) '(:jcc32 #x74 fits)                       ; test ebx, ebx; jz
       (if *i386-cl-gc-static*
           ;; static: the restored heap's pages, then two semispaces of at
           ;; least M + 128 KB each, within S
           (list :splice '(#x8B #x43 #x08)                    ; eax = free
                 (cons #x05 (i386l-le32-list #xFFF))
                 (cons #x25 (i386l-le32-list #xFFFFF000))
                 (cons #x2D (i386l-le32-list +i386-cl-heap-va+))   ; the static pages
                 (i386cl-abs '(#x8B #x0D) +i386-cl-v-m+)
                 '(#x81 #xC1) (i386l-le32-list #x20000)
                 '(#xD1 #xE1)                                 ; ecx = 2 (M + 128 KB)
                 '(#x01 #xC8)                                 ; add eax, ecx
                 (i386cl-abs '(#x3B #x05) +i386-cl-v-s+)
                 '(:jcc32 #x76 fits))                         ; jbe
           (list :splice '(#xD1 #xE8)                         ; eax = S/2
                 (cons #x05 (i386l-le32-list +i386-cl-heap-va+))
                 (i386cl-abs '(#x2B #x05) +i386-cl-v-m+)
                 '(#x39 #x43 #x08)                            ; cmp [ebx+8], eax
                 '(:jcc32 #x72 fits)))                        ; jb
       ;; a gap: the file holds no paged code to boot cold with
       (append (i386cl-abs '(#x83 #x3D) +i386-cl-v-hole+) '(#x00))
       '(:jcc32 #x75 no-ram)
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-hdr+) (i386l-le32-list 0))
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-tail+) (i386l-le32-list 0))
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-coren+) (i386l-le32-list 0))
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-plo+) (i386l-le32-list 0))
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-phi+) (i386l-le32-list 0))
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-pagedn+) (i386l-le32-list 0))
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-pool+) (i386l-le32-list 0))
       '(:jmp32 0 size-again)
       '(:label fits)
       ;; the heap, S + M at VA #x19000000 (not zeroed: every allocation
       ;; initialises what it takes, as on the hosted mmap's fresh pages):
       ;; the tail's pages, the core's, the paged code's past the pool, then
       ;; fresh RAM: each run only as far as the heap still needs ([rem]); a
       ;; small machine's heap can be smaller than what a restore frees.
       ;; [hcur] = the next heap VA.  The POOL is the paged code's first
       ;; pages: they are free once the code is unmapped, and contiguous.
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-hcur+) (i386l-le32-list +i386-cl-heap-va+))
       (i386cl-abs '(#xA1) +i386-cl-v-s+) (i386cl-abs '(#x03 #x05) +i386-cl-v-m+)
       (i386cl-abs '(#xA3) +i386-cl-v-rem+)
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-tail+) (i386cl-abs '(#x8B #x35) +i386-cl-v-tailpa+)
       '(:call 0 maprun)
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-coren+) (i386cl-abs '(#x8B #x35) +i386-cl-v-hdr+)
       '(:call 0 maprun)
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-pagedn+) (i386cl-abs '(#x2B #x0D) +i386-cl-v-pool+)
       (i386cl-abs '(#x8B #x35) +i386-cl-v-plo+) (i386cl-abs '(#x03 #x35) +i386-cl-v-pool+)
       '(:call 0 maprun)
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-rem+)
       '(#x85 #xC9) '(:j8 #x74 map-fresh-done)
       '(#x51) '(:call 0 alloc) '(#x89 #xC6) '(#x59)          ; esi = PA
       '(:call 0 maprun)
       '(:label map-fresh-done)
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-pagedn+)
       '(#x85 #xC9) '(:jcc32 #x74 map-no-paged)
       ;; the pool, its slots all empty: the paged code's first pages, or
       ;; with a gap fresh RAM
       (i386cl-abs '(#xA1) +i386-cl-v-plo+) (i386cl-abs '(#xA3) +i386-cl-v-poolpa+)
       (append (i386cl-abs '(#x83 #x3D) +i386-cl-v-hole+) '(#x00))
       '(:jcc32 #x74 pool-placed)
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-pool+) '(:call 0 alloc)
       (i386cl-abs '(#xA3) +i386-cl-v-poolpa+)
       '(:label pool-placed)
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-pool+) '(#xC1 #xE9 #x0E)    ; ecx = slots
       (i386cl-abs '(#x89 #x0D) +i386-cl-v-nslots+)
       (append (i386cl-abs '(#xC7 #x05) +i386-cl-v-next+) (i386l-le32-list 0))
       (cons #xBF (i386l-le32-list +i386-cl-owners+))
       '(#x83 #xC8 #xFF) '(#xF3 #xAB)                         ; eax = -1; rep stosd
       ;; the code's own addresses not present: [plo, phi), a PTE at a time
       (i386cl-abs '(#x8B #x3D) +i386-cl-v-plo+)
       '(:label unmap-next)
       (i386cl-abs '(#x3B #x3D) +i386-cl-v-phi+) '(:jcc32 #x73 unmap-done)
       '(#x89 #xF8) '(#xC1 #xE8 #x16) '(#xC1 #xE0 #x02)       ; eax = (va >> 22) * 4
       (i386cl-abs '(#x03 #x05) +i386-cl-v-pd+)
       '(#x8B #x00) (cons #x25 (i386l-le32-list #xFFFFF000))  ; eax = PT
       '(#x89 #xFA) '(#xC1 #xEA #x0C) '(#x81 #xE2) (i386l-le32-list #x3FF)
       '(#xC7 #x04 #x90) (i386l-le32-list 0)                  ; mov [eax+edx*4], 0
       '(#x81 #xC7) (i386l-le32-list 4096)
       '(:jmp32 0 unmap-next)
       '(:label unmap-done)
       ;; the page directory's last entry is the directory: the handler reaches
       ;; every PTE at +I386-CL-PT-WINDOW+
       (i386cl-abs '(#xA1) +i386-cl-v-pd+) '(#x89 #xC2) '(#x83 #xCA #x03)
       '(#x89 #x90) (i386l-le32-list (* 1023 4))              ; mov [eax+4092], edx
       '(:label map-no-paged)
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
       ;; ---- the page-fault handler.  The CALL pushes its address and skips it.
       '(:call 0 pf-after)
       ;; A fault in [plo, phi): the next slot of the pool (its old chunk
       ;; unmapped), mapped at the 16 KB chunk holding CR2, the chunk decoded
       ;; into it from the store.  Clean code, so a chunk evicted while a
       ;; return address still points into it just faults back in.  Anything
       ;; else: "#PF cr2 eip" on COM1 and halt -- no IDT entry for it before.
       '(:label pf-handler)
       '(#x60)                                                ; pushad
       '(#x0F #x20 #xD0)                                      ; mov eax, cr2
       (i386cl-abs '(#x3B #x05) +i386-cl-v-plo+) '(:jcc32 #x72 pf-other)
       (i386cl-abs '(#x3B #x05) +i386-cl-v-phi+) '(:jcc32 #x73 pf-other)
       (cons #x25 (i386l-le32-list #xFFFFC000))               ; the chunk
       '(#x89 #xC3)                                           ; ebx = chunk va
       ;; the victim, by SECOND CHANCE: a slot whose chunk was touched since
       ;; the hand last passed (an Accessed bit in its PTEs) loses the bits
       ;; and is passed over; every fault reloads CR3, so the CPU sets them
       ;; again on the next touch.  Ends within one turn of the pool.
       '(:label pf-pick)
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-next+)
       '(#x8D #x51 #x01)                                      ; edx = next + 1
       (i386cl-abs '(#x3B #x15) +i386-cl-v-nslots+) '(:j8 #x72 pf-wrap)
       '(#x31 #xD2)
       '(:label pf-wrap)
       (i386cl-abs '(#x89 #x15) +i386-cl-v-next+)
       (append '(#x8B #x04 #x8D) (i386l-le32-list +i386-cl-owners+))   ; eax = owner[ecx]
       '(#x83 #xF8 #xFF) '(:jcc32 #x74 pf-noevict)
       '(#x89 #xC7) '(#xC1 #xEF #x0C) '(#xC1 #xE7 #x02)       ; edi = PTE of the old chunk
       (cons #x81 (cons #xC7 (i386l-le32-list +i386-cl-pt-window+)))
       '(#x8B #x17) '(#x0B #x57 #x04) '(#x0B #x57 #x08) '(#x0B #x57 #x0C)
       '(#xF6 #xC2 #x20) '(:jcc32 #x74 pf-evict)              ; none Accessed: evict
       '(#x83 #x27 #xDF) '(#x83 #x67 #x04 #xDF) '(#x83 #x67 #x08 #xDF) '(#x83 #x67 #x0C #xDF)
       '(:jmp32 0 pf-pick)
       '(:label pf-evict)
       '(#x31 #xD2) '(#x89 #x17) '(#x89 #x57 #x04) '(#x89 #x57 #x08) '(#x89 #x57 #x0C)
       '(:label pf-noevict)
       (append '(#x89 #x1C #x8D) (i386l-le32-list +i386-cl-owners+))   ; owner[ecx] = chunk
       '(#x89 #xC8) '(#xC1 #xE0 #x0E)                         ; eax = slot * 16 KB
       (i386cl-abs '(#x03 #x05) +i386-cl-v-poolpa+) '(#x83 #xC8 #x03)  ; | present, rw
       '(#x89 #xDF) '(#xC1 #xEF #x0C) '(#xC1 #xE7 #x02)
       (cons #x81 (cons #xC7 (i386l-le32-list +i386-cl-pt-window+)))
       '(#x89 #x07) (cons #x05 (i386l-le32-list 4096))
       '(#x89 #x47 #x04) (cons #x05 (i386l-le32-list 4096))
       '(#x89 #x47 #x08) (cons #x05 (i386l-le32-list 4096))
       '(#x89 #x47 #x0C)
       '(#x0F #x20 #xD8) '(#x0F #x22 #xD8)                    ; reload CR3: a 386 has no INVLPG
       (i386cl-abs '(#xFF #x05) +i386-cl-v-faults+)
       ;; the store: plo, phi, chunk bytes, n, offsets[n+1], data
       '(#x89 #xD8) (i386cl-abs '(#x2B #x05) +i386-cl-v-plo+) '(#xC1 #xE8 #x0E)   ; eax = k
       (i386cl-abs '(#x8B #x35) +i386-cl-v-store+)
       '(#x8B #x4E #x0C)                                      ; ecx = n
       '(#x8D #x54 #x8E #x14)                                 ; edx = data = store + 4n + 20
       '(#x8B #x4C #x86 #x14)                                 ; ecx = offsets[k+1]
       '(#x8B #x74 #x86 #x10)                                 ; esi = offsets[k]
       (append (i386cl-abs '(#x83 #x3D) +i386-cl-v-dataoff+) '(#x00))
       '(:jcc32 #x75 pf-disk)
       '(#x01 #xD1) '(#x01 #xD6)                              ; + data
       '(:jmp32 0 pf-decode)
       ;; the store on the disk: the chunk's sectors to 30000h through PM_READ
       '(:label pf-disk)
       (i386cl-abs '(#xA1) +i386-cl-v-dataoff+) '(#x01 #xF0)  ; eax = its file byte
       '(#x29 #xF1) '(#x51)                                   ; ecx = len; push
       '(#x89 #xC2) '(#x81 #xE2) (i386l-le32-list 511) '(#x52)   ; edx = skip; push
       '(#xC1 #xE8 #x09) (i386cl-abs '(#x03 #x05) +i386-cl-v-imglba+)   ; eax = sector
       '(#x8D #x8C #x0A) (i386l-le32-list 511) '(#xC1 #xE9 #x09)       ; ecx = sectors
       (i386cl-abs '(#xFF #x15) +i386-cl-v-diskread+)         ; call [PM_READ]
       '(:jcc32 #x72 pf-diskerr)
       '(#x5A) '(#x59)                                        ; edx = skip, ecx = len
       '(#x8D #xB2) (i386l-le32-list #x30000)                 ; esi = 30000h + skip
       '(#x01 #xF1)                                           ; ecx = its end
       '(:label pf-decode)
       '(#x89 #xDF) '(#xFC)                                   ; edi = chunk; cld
       ;; LZ4 block: token; literals (15: + bytes to a non-255); unless at the
       ;; end, a 16-bit offset and the match (low nibble + 4, 15: + bytes)
       '(:label lz-next)
       '(#x31 #xC0) '(#xAC) '(#x89 #xC3)                      ; eax = ebx = token
       '(#xC1 #xE8 #x04) '(#x83 #xF8 #x0F) '(:jcc32 #x75 lz-lit)
       '(:label lz-lit-more)
       '(#x0F #xB6 #x16) '(#x46) '(#x01 #xD0)                 ; edx = *esi++; eax += edx
       (cons #x81 (cons #xFA (i386l-le32-list 255))) '(:jcc32 #x74 lz-lit-more)
       '(:label lz-lit)
       '(#x51) '(#x89 #xC1) '(#xF3 #xA4) '(#x59)              ; the literals
       '(#x39 #xCE) '(:jcc32 #x73 lz-done)                    ; end of the block?
       '(#x31 #xC0) '(#x66 #xAD) '(#x89 #xC2)                 ; edx = offset
       '(#x89 #xD8) '(#x83 #xE0 #x0F) '(#x83 #xF8 #x0F) '(:jcc32 #x75 lz-match)
       '(#x52)
       '(:label lz-match-more)
       '(#x0F #xB6 #x16) '(#x46) '(#x01 #xD0)
       (cons #x81 (cons #xFA (i386l-le32-list 255))) '(:jcc32 #x74 lz-match-more)
       '(#x5A)
       '(:label lz-match)
       '(#x83 #xC0 #x04)
       '(#x56) '(#x89 #xFE) '(#x29 #xD6)                      ; esi = edi - offset
       '(#x51) '(#x89 #xC1) '(#xF3 #xA4) '(#x59) '(#x5E)      ; byte by byte: overlap is a run
       '(:jmp32 0 lz-next)
       '(:label lz-done)
       '(#x61) '(#x83 #xC4 #x04) '(#xCF)                      ; popad; drop the error code; iret
       ;; not ours
       '(:label pf-other)
       '(#xB0 #x23) '(:call 0 pf-putc) '(#xB0 #x50) '(:call 0 pf-putc)
       '(#xB0 #x46) '(:call 0 pf-putc) '(#xB0 #x20) '(:call 0 pf-putc)
       '(#x0F #x20 #xD0) '(:call 0 pf-hex)                    ; cr2
       '(#xB0 #x20) '(:call 0 pf-putc)
       '(#x8B #x44 #x24 #x24) '(:call 0 pf-hex)               ; the faulting EIP
       '(#xB0 #x0A) '(:call 0 pf-putc)
       '(:label pf-halt) '(#xFA) '(#xF4) '(:j8 #xEB pf-halt)
       '(:label pf-putc)                                      ; AL to COM1
       '(#x52) '(#x50) (cons #x66 (cons #xBA '(#xFD #x03)))   ; mov dx, 3FDh
       '(:label pf-putc-wait) '(#xEC) '(#xA8 #x20) '(:j8 #x74 pf-putc-wait)
       '(#x58) (cons #x66 (cons #xBA '(#xF8 #x03))) '(#xEE) '(#x5A) '(#xC3)
       '(:label pf-hex)                                       ; EAX as 8 hex digits
       '(#x51) (cons #xB9 (i386l-le32-list 8))
       '(:label pf-hex-next)
       '(#xC1 #xC0 #x04) '(#x50) '(#x24 #x0F) '(#x04 #x30) '(#x3C #x39) '(:j8 #x76 pf-hex-digit)
       '(#x04 #x07)
       '(:label pf-hex-digit)
       '(:call 0 pf-putc) '(#x58) '(#x49) '(:jcc32 #x75 pf-hex-next)
       '(#x59) '(#xC3)
       '(:label pf-after)
       '(#x58) (i386cl-abs '(#xA3) +i386-cl-v-pfaddr+)       ; pop eax
       ;; with a gap, the header and the store are read from here on at their
       ;; addresses, the gap above their physical places
       (i386cl-abs '(#x8B #x0D) +i386-cl-v-hole+)
       '(#x85 #xC9) '(:jcc32 #x74 gap-done)
       (append (i386cl-abs '(#x83 #x3D) +i386-cl-v-hdr+) '(#x00))
       '(:j8 #x74 gap-store)
       (i386cl-abs '(#x01 #x0D) +i386-cl-v-hdr+)             ; add [hdr], ecx
       '(:label gap-store)
       (append (i386cl-abs '(#x83 #x3D) +i386-cl-v-store+) '(#x00))
       '(:j8 #x74 gap-done)
       (i386cl-abs '(#x01 #x0D) +i386-cl-v-store+)
       '(:label gap-done)
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
       (if *i386-cl-gc-static*
           ;; STATIC: [page_base, free) is the static heap; the semispaces
           ;; split what is above its last page; the young space is empty.
           ;; page_base (the bitmaps' base) stays the heap's start, so the
           ;; restored bits keep describing the static objects.
           (list :splice (i386cl-abs '(#xA1) +i386-cl-v-free+)
                 (i386cl-abs '(#xA3) +i386-cl-v-static-hi+)
                 (cons #xB9 (i386l-le32-list (+ +i386-cl-heap-va+ +linux-i386-heap-alloc-start+)))
                 (i386cl-abs '(#x89 #x0D) +i386-cl-v-static-lo+)
                 (cons #x05 (i386l-le32-list #xFFF))
                 (cons #x25 (i386l-le32-list #xFFFFF000))     ; eax = A, the first semispace
                 (i386cl-abs '(#x8B #x15) +i386-cl-v-s+)
                 '(#x81 #xC2) (i386l-le32-list +i386-cl-heap-va+)
                 '(#x29 #xC2) '(#xD1 #xEA)                    ; edx = (heap + S - A) / 2
                 '(#x83 #xE2 #xF0)                            ; to 16 bytes
                 (i386cl-abs '(#xA3) (+ g #x00))              ; VA = A
                 (i386cl-abs '(#xA3) +gc-from-start-addr+)
                 (i386cl-abs '(#x89 #x15) +gc-space-size-addr+)
                 '(#x8D #x0C #x10)                            ; lea ecx, [eax+edx]
                 (i386cl-abs '(#x89 #x0D) +gc-to-start-addr+)
                 (i386cl-abs '(#x2B #x0D) +i386-cl-v-m+)
                 (i386cl-abs '(#x89 #x0D) (+ g #x04)))        ; VL = to - M
           nil)
       '(:label core-done)
       '(:jmp32 0 past-subroutines))
      ;; ---- subroutines (physical addresses: called with paging off) ----
      ;; alloc: ECX bytes (page-rounded) -> EAX = PA.  Clobbers ECX, EDX.
      (list '(:label alloc)
            (i386cl-abs '(#xA1) +i386-cl-v-panext+)
            '(#x81 #xC1) (i386l-le32-list #xFFF)
            '(#x81 #xE1) (i386l-le32-list #xFFFFF000)
            '(#x8D #x14 #x08)                                   ; lea edx, [eax+ecx]
            ;; past the end of RAM: say so and stop, rather than map pages
            ;; that are not there (an 18 MB machine faulted in its heap)
            (i386cl-abs '(#x3B #x15) +i386-cl-v-ramend+) '(:jcc32 #x77 no-ram)   ; ja
            (i386cl-abs '(#x89 #x15) +i386-cl-v-panext+)
            '(#xC3)
            ;; "MODUS: not enough memory" at the top of the text screen (paging
            ;; is off, B8000h is itself) and on COM1, then halt
            '(:label no-disk)
            (cons :splice (i386cl-say-and-halt "MODUS: this image pages its code from the disk it booted from (scripts/pcboot.sh)"))
            '(:label pf-diskerr)
            (cons :splice (i386cl-say-and-halt "MODUS: a disk error paging code in"))
            '(:label no-ram)
            (cons #xBF (i386l-le32-list #xB8000))
            '(#x66 #xC7 #x07 77 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 77) '(#xEE)
            '(#x66 #xC7 #x07 79 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 79) '(#xEE)
            '(#x66 #xC7 #x07 68 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 68) '(#xEE)
            '(#x66 #xC7 #x07 85 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 85) '(#xEE)
            '(#x66 #xC7 #x07 83 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 83) '(#xEE)
            '(#x66 #xC7 #x07 58 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 58) '(#xEE)
            '(#x66 #xC7 #x07 32 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 32) '(#xEE)
            '(#x66 #xC7 #x07 110 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 110) '(#xEE)
            '(#x66 #xC7 #x07 111 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 111) '(#xEE)
            '(#x66 #xC7 #x07 116 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 116) '(#xEE)
            '(#x66 #xC7 #x07 32 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 32) '(#xEE)
            '(#x66 #xC7 #x07 101 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 101) '(#xEE)
            '(#x66 #xC7 #x07 110 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 110) '(#xEE)
            '(#x66 #xC7 #x07 111 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 111) '(#xEE)
            '(#x66 #xC7 #x07 117 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 117) '(#xEE)
            '(#x66 #xC7 #x07 103 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 103) '(#xEE)
            '(#x66 #xC7 #x07 104 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 104) '(#xEE)
            '(#x66 #xC7 #x07 32 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 32) '(#xEE)
            '(#x66 #xC7 #x07 109 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 109) '(#xEE)
            '(#x66 #xC7 #x07 101 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 101) '(#xEE)
            '(#x66 #xC7 #x07 109 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 109) '(#xEE)
            '(#x66 #xC7 #x07 111 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 111) '(#xEE)
            '(#x66 #xC7 #x07 114 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 114) '(#xEE)
            '(#x66 #xC7 #x07 121 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 121) '(#xEE)
            '(#x66 #xC7 #x07 32 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 32) '(#xEE)
            '(#x66 #xC7 #x07 102 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 102) '(#xEE)
            '(#x66 #xC7 #x07 111 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 111) '(#xEE)
            '(#x66 #xC7 #x07 114 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 114) '(#xEE)
            '(#x66 #xC7 #x07 32 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 32) '(#xEE)
            '(#x66 #xC7 #x07 116 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 116) '(#xEE)
            '(#x66 #xC7 #x07 104 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 104) '(#xEE)
            '(#x66 #xC7 #x07 105 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 105) '(#xEE)
            '(#x66 #xC7 #x07 115 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 115) '(#xEE)
            '(#x66 #xC7 #x07 32 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 32) '(#xEE)
            '(#x66 #xC7 #x07 105 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 105) '(#xEE)
            '(#x66 #xC7 #x07 109 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 109) '(#xEE)
            '(#x66 #xC7 #x07 97 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 97) '(#xEE)
            '(#x66 #xC7 #x07 103 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 103) '(#xEE)
            '(#x66 #xC7 #x07 101 #x4F) '(#x83 #xC7 #x02)
            '(#x66 #xBA #xF8 #x03) '(#xB0 101) '(#xEE)
            '(#x66 #xBA #xF8 #x03) '(#xB0 #x0D) '(#xEE) '(#xB0 #x0A) '(#xEE)
            '(:label no-ram-halt) '(#xFA) '(#xF4) '(:j8 #xEB no-ram-halt)
            ;; maprun: ECX bytes at PA ESI as the next heap pages, at most
            ;; [rem] of them.  Clobbers EAX-EDX, ESI, EDI.
            '(:label maprun)
            (i386cl-abs '(#x3B #x0D) +i386-cl-v-rem+) '(:j8 #x76 maprun-n)
            (i386cl-abs '(#x8B #x0D) +i386-cl-v-rem+)
            '(:label maprun-n)
            '(#x85 #xC9) '(:j8 #x74 maprun-done)
            (i386cl-abs '(#x29 #x0D) +i386-cl-v-rem+)
            '(#x51) (i386cl-abs '(#x8B #x3D) +i386-cl-v-hcur+) '(#xC1 #xE9 #x0C)
            '(:call 0 map)
            '(#x59) (i386cl-abs '(#x01 #x0D) +i386-cl-v-hcur+)
            '(:label maprun-done)
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
    ;; vector 14, the page fault: the handler above (an interrupt gate, so the
    ;; timer waits while a chunk decodes)
    (let ((gate (+ #x90000 (* 14 8))))
      (i386l-bytes buf #xA1) (i386l-le32 buf +i386-cl-v-pfaddr+)   ; mov eax, [pfaddr]
      (i386l-bytes buf #x89 #xC2)                                   ; mov edx, eax
      (i386l-bytes buf #x25) (i386l-le32 buf #xFFFF)
      (i386l-bytes buf #x0D) (i386l-le32 buf #x00080000)           ; selector 8
      (i386l-bytes buf #xA3) (i386l-le32 buf gate)
      (i386l-bytes buf #x81 #xE2) (i386l-le32 buf #xFFFF0000)
      (i386l-bytes buf #x81 #xCA) (i386l-le32 buf #x8E00)          ; present, 32-bit interrupt gate
      (i386l-bytes buf #x89 #x15) (i386l-le32 buf (+ gate 4)))
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
