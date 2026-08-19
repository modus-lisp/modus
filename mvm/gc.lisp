;;;; gc.lisp - Cheney Copying Garbage Collector for MVM
;;;;
;;;; Implements a semi-space copying collector using the Cheney algorithm.
;;;; The heap is split into two equal semispaces. When one fills up, live
;;;; objects are copied to the other semispace and the roles swap.
;;;;
;;;; This file is compiled by MVM and included in builds that need GC.
;;;; The collector must NOT allocate any heap memory — it uses only
;;;; mem-ref / mem-set for raw memory access and fixed-address metadata.
;;;;
;;;; GC metadata layout (fixed addresses in globals area):
;;;;   0x10000040: from_start  (byte address of current from-space start)
;;;;   0x10000048: to_start    (byte address of current to-space start)
;;;;   0x10000050: space_size  (size of each semispace in bytes)
;;;;   0x10000058: stack_base  (top of stack, set at boot)
;;;;   0x10000060: gc_count    (number of collections performed)
;;;;   0x10000068: saved_rsp   (RSP at GC entry, set by trampoline)
;;;;   0x10000070: saved_r12   (alloc ptr at GC entry, set by trampoline)
;;;;   0x10000078: saved_r14   (alloc limit at GC entry, set by trampoline)
;;;;
;;;; Tag encoding (64-bit words):
;;;;   xxx0 = fixnum (shift by 1) — not a pointer, skip
;;;;   0001 = cons — pointer to 16-byte car/cdr pair
;;;;   1001 = object — pointer to header + slots
;;;;   0101 = immediate (char, nil, bool) — skip
;;;;   1111 = forward — GC forwarding pointer (new address | 15)

(in-package :modus.mvm)

;;; ============================================================
;;; GC Fixed Address Constants
;;; ============================================================
;;; These are raw byte addresses. To use with mem-ref, shift left by 1
;;; (tag as fixnum). mem-ref :u64 does SHR 1 to get the real address.

;; Source code literals are byte addresses directly.
;; (mem-ref #xADDR :u64) reads from byte address ADDR.
;; The compiler tags the literal (SHL 1), mem-ref untags (SHR 1).
(defun %gc-from-start ()   (mem-ref #x10000040 :u64))
(defun %gc-to-start ()     (mem-ref #x10000048 :u64))
(defun %gc-space-size ()   (mem-ref #x10000050 :u64))
(defun %gc-stack-base ()   (mem-ref #x10000058 :u64))
(defun %gc-count ()        (mem-ref #x10000060 :u64))
(defun %gc-saved-rsp ()    (mem-ref #x10000068 :u64))
(defun %gc-saved-r12 ()    (mem-ref #x10000070 :u64))
(defun %gc-saved-r14 ()    (mem-ref #x10000078 :u64))

(defun %gc-set-from-start (v)  (setf (mem-ref #x10000040 :u64) v))
(defun %gc-set-to-start (v)    (setf (mem-ref #x10000048 :u64) v))
(defun %gc-set-count (v)       (setf (mem-ref #x10000060 :u64) v))
(defun %gc-set-r12 (v)         (setf (mem-ref #x10000070 :u64) v))
(defun %gc-set-r14 (v)         (setf (mem-ref #x10000078 :u64) v))

;;; ============================================================
;;; Tag Checking Helpers
;;; ============================================================

(defun %gc-is-fixnum (val)
  "Check if VAL is a fixnum (tag bit 0 = 0)."
  (= (logand val 1) 0))

(defun %gc-is-cons (val)
  "Check if VAL is a cons pointer (low 4 bits = 0001)."
  (= (logand val 15) 1))

(defun %gc-is-object (val)
  "Check if VAL is an object pointer (low 4 bits = 1001)."
  (= (logand val 15) 9))

(defun %gc-is-forward (val)
  "Check if VAL is a forwarding pointer (low 4 bits = 1111).
   SUPERSEDED inside the collector by %gc-is-forward-lo, which tests the low
   32-bit half and so can never see a promoted word at all.
   #160/linux: guard on FIXNUMP first.  %gc-read64 returns the raw 64-bit word
   as a Lisp integer; a word >= most-positive-fixnum (2^62) — common on Linux
   where the stack/heap hold high-bit machine words — is a BIGNUM, and
   (logand bignum 15) routes through GENERIC-LOGAND (%BB-LIMBS-LIST) which
   ALLOCATES → re-trips the gc-check → recursive %gc-collect.  A forwarding
   pointer is always a to-space address (~2^47, fixnum), so a non-fixnum word is
   never a forwarding pointer — skip it allocation-free."
  (if (fixnump val) (= (logand val 15) 15) nil))

(defun %gc-is-pointer (val)
  "Check if VAL is a heap pointer (cons or object).
   SUPERSEDED inside the collector by %gc-is-pointer-lo / %gc-cand-addr.
   #160/linux: guard on FIXNUMP first (see %gc-is-forward).  A genuine cons/
   object pointer into the heap (address ~2^47) is fixnum-range; a raw word that
   promoted to a BIGNUM (>= 2^62) is never such a pointer, and running
   (logand bignum 15) here would ALLOCATE (GENERIC-LOGAND) during collection —
   the recursive-GC bug.  Skip non-fixnum words allocation-free."
  (if (fixnump val)
      (let ((tag (logand val 15)))
        (if (= tag 1) t
            (if (= tag 9) t nil)))
      nil))

(defun %gc-is-immediate (val)
  "Check if VAL is an immediate (fixnum or immediate constant)."
  (let ((tag (logand val 1)))
    (if (= tag 0) t           ; fixnum
        (= (logand val 15) 5))))  ; immediate

;;; ============================================================
;;; Address Helpers
;;; ============================================================
;;; Raw addresses are untagged. To read/write memory at a raw address
;;; using mem-ref, we need to convert: tagged_addr = raw_addr * 2.
;;; mem-ref does SHR 1 internally to get the real address.

;;; ------------------------------------------------------------
;;; EXACT machine-word access — the :u64 HALVING DEFECT and its fix
;;; ------------------------------------------------------------
;;; *** MEASURED (aarch64 bare metal, 2026-08; originally seen on i386):
;;; (mem-ref A :u64) does NOT return the machine word at A.  memory-width-code
;;; returns needs-tag NIL for :u64, so the loaded word is left in the vreg
;;; UNTAGGED and every later Lisp operation reads it as a TAGGED value — the
;;; integer you get back is word/2.  Proven against physical memory with gdb:
;;; 0x10000040 (from_start) physically holds 0x12000000 while the collector
;;; must and does use 0x09000000.
;;;
;;; This is NOT an i386-only footnote (the old docstring here claimed x64 and
;;; aarch64 "use native trampolines instead" — FALSE for aarch64: its GC
;;; trampoline is a register-saving shim that CALLS this Lisp %gc-collect, so
;;; every bare-metal Pi/virt image runs this code).  Two consequences:
;;;
;;;   - A cons/object pointer word has low nibble 1 or 9, i.e. is ODD.  Halved,
;;;     it lands as a POINTER, not a fixnum, so (fixnump val) is false and
;;;     %gc-is-pointer / %gc-is-forward returned NIL for exactly the words a
;;;     collector exists to forward.  The candidate was rejected BEFORE the
;;;     object-start gate was ever consulted — %gc-forward-slot logged 0
;;;     declines while roots went unforwarded (164 dead-space pointers left on
;;;     the live stack after GC#1, 55 after GC#2).
;;;   - Header decoding was off by the same factor: a header with true count 5
;;;     and subtag #x32 decoded as count 2, subtag #x99.  (There are no
;;;     compensating shifts anywhere in this file, so fixing the read also
;;;     fixes header decoding — a real behavioural change, not a no-op.)
;;;
;;; THE FIX.  :u32 IS tagged (SHL 1 after a zero-extending load) and 32 bits
;;; always fit a 64-bit target's fixnum, so a :u32 load is EXACT.  Read and
;;; write machine words as two exact 32-bit halves, little-endian (lo at +0,
;;; hi at +4 — every target running this collector is LE).
;;;
;;; ALLOCATION-FREE BY CONSTRUCTION.  %gc-collect must not allocate: a bignum
;;; built here re-trips :gc-check and re-enters the collector (observable as
;;; the phase trace printing "1234" forever with no "5").  So NO caller ever
;;; materialises a 64-bit word: predicates take the LO half, word-to-word moves
;;; go through %gc-move-word, header fields come from %gc-header-count, and the
;;; single place halves are combined is %gc-cand-addr — which first REJECTS any
;;; word with hi > 65535 (>= 2^48), a range no supported heap occupies, so the
;;; combine is always fixnum-range.  All shift counts are <= 30 and all
;;; literals < 2^24, so nothing routes through BIGNUM-ASH and nothing overflows
;;; a 30-bit fixnum target either.

(defun %gc-word-lo (raw-addr)
  "Bits 0..31 of the machine word at byte address RAW-ADDR.  EXACT."
  (mem-ref raw-addr :u32))

(defun %gc-word-hi (raw-addr)
  "Bits 32..63 of the machine word at byte address RAW-ADDR.  EXACT."
  (mem-ref (+ raw-addr 4) :u32))

(defun %gc-set-halves (raw-addr lo hi)
  "Store the machine word whose halves are LO/HI at byte address RAW-ADDR."
  (setf (mem-ref raw-addr :u32) lo)
  (setf (mem-ref (+ raw-addr 4) :u32) hi))

(defun %gc-move-word (src-addr dst-addr)
  "Copy one machine word SRC-ADDR → DST-ADDR bit-exactly, without ever
   materialising it as a Lisp integer (so it cannot promote to a bignum)."
  (let ((lo (mem-ref src-addr :u32))
        (hi (mem-ref (+ src-addr 4) :u32)))
    (setf (mem-ref dst-addr :u32) lo)
    (setf (mem-ref (+ dst-addr 4) :u32) hi)))

(defun %gc-store-tagged (raw-addr addr tag)
  "Store the machine word (ADDR | TAG) at byte address RAW-ADDR, as halves.
   ADDR is a 16-byte-aligned raw byte address (always a Lisp fixnum here — it
   is one of our own heap addresses); TAG is its low nibble (1 cons / 9 object
   / 15 forwarding).  The high half is derived with two 16-bit shifts rather
   than (ash addr -32) because compile-ash inlines :sar only for counts <= 30 —
   a count of 32 would route through BIGNUM-ASH, which allocates."
  (let* ((hi (ash (ash addr -16) -16))
         (lo (- addr (* (* hi 65536) 65536))))
    (setf (mem-ref raw-addr :u32) (logior lo tag))
    (setf (mem-ref (+ raw-addr 4) :u32) hi)))

(defun %gc-header-count (lo hi)
  "Element count from an object header whose halves are LO/HI.  The count is
   the header word >> 8, so it is (hi << 24) | (lo >> 8).  16777216 = 2^24 is
   below every supported target's fixnum ceiling."
  (if (= hi 0)
      (ash lo -8)
      (+ (* hi 16777216) (ash lo -8))))

(defun %gc-is-forward-lo (lo)
  "T if the machine word whose low half is LO is a forwarding pointer.
   Only the low nibble matters, so the high half is irrelevant."
  (= (logand lo 15) 15))

(defun %gc-is-pointer-lo (lo)
  "T if the machine word whose low half is LO carries a cons (1) or object (9)
   tag.  Only the low nibble matters."
  (let ((tag (logand lo 15)))
    (if (= tag 1) t (if (= tag 9) t nil))))

(defun %gc-cand-addr (lo hi from-start from-size)
  "Tag-stripped byte address of the candidate machine word (LO,HI) when it is a
   cons/object pointer INTO [FROM-START, FROM-START+FROM-SIZE); NIL otherwise.
   This is the ONLY place the two halves are combined, and it rejects
   hi > 65535 first: a word >= 2^48 is never a Modus heap pointer on any
   supported target (bare metal sits below 2^32; hosted mmap heaps around
   0x7dac_xxxx_xxxx have hi ~ 0x7dac), and combining one would build a BIGNUM —
   an allocation inside the collector, which re-enters it.  Supersedes
   %gc-in-space, which took an already-materialised word."
  (if (%gc-is-pointer-lo lo)
      (if (> hi 65535)
          nil
          (let ((addr (if (= hi 0)
                          (logand lo (lognot 15))
                          (+ (* (* hi 65536) 65536) (logand lo (lognot 15))))))
            (if (< addr from-start)
                nil
                (if (>= addr (+ from-start from-size)) nil addr))))
      nil))

(defun %gc-tmp-free ()     (mem-ref #x10000108 :u64))
(defun %gc-set-tmp-free (v) (setf (mem-ref #x10000108 :u64) v))

;; TO-SPACE END, published by %gc-collect before any copying.  %gc-copy-object
;; needs it to refuse a copy that would run off the top of to-space; it only
;; receives the FROM-space geometry, and deriving to_end from it is fragile.
(defun %gc-to-end ()      (mem-ref #x10000110 :u64))
(defun %gc-set-to-end (v) (setf (mem-ref #x10000110 :u64) v))

(defun %gc-read64 (raw-addr)
  "DIAGNOSTIC ONLY — DO NOT CALL FROM INSIDE %gc-collect.
   Returns the EXACT machine word at RAW-ADDR (composed from the two faithful
   :u32 halves).  It is kept out of the collector because a word >= 2^62
   promotes to a BIGNUM, i.e. allocates.  Historically this was
   (mem-ref raw-addr :u64), which silently returned word/2 — see the block
   comment above."
  (let ((lo (mem-ref raw-addr :u32))
        (hi (mem-ref (+ raw-addr 4) :u32)))
    (if (= hi 0) lo (+ (* (* hi 65536) 65536) lo))))

(defun %gc-write64 (raw-addr val)
  "DIAGNOSTIC ONLY — DO NOT CALL FROM INSIDE %gc-collect.
   Writes VAL as the EXACT machine word at RAW-ADDR.  Historically this was
   (setf (mem-ref raw-addr :u64) val), which deposited val*2; read and write
   scaled together, so a round trip through the pair proved nothing (see the
   probe-discipline note in the commit message)."
  (let* ((hi (ash (ash val -16) -16))
         (lo (- val (* (* hi 65536) 65536))))
    (setf (mem-ref raw-addr :u32) lo)
    (setf (mem-ref (+ raw-addr 4) :u32) hi)))

(defun %gc-in-space (ptr space-start space-size)
  "SUPERSEDED by %gc-cand-addr (which works on halves and never materialises a
   word).  Kept for out-of-tree diagnostics.  Check if tagged pointer PTR
   points into the space starting at SPACE-START with size SPACE-SIZE."
  (let ((raw-addr (logand ptr (lognot 15))))
    ;; raw-addr has tag bits cleared but is still in the 'divided by 2'
    ;; pointer space. For cons (tag 1): addr = ptr - 1 → ptr & ~15.
    ;; For object (tag 9): addr = ptr - 9 → ptr & ~15.
    ;; These addresses are byte addresses already (MVM pointers ARE
    ;; byte addresses with tag bits in the low 4 bits).
    (if (< raw-addr space-start) nil
        (if (>= raw-addr (+ space-start space-size)) nil t))))

;;; ============================================================
;;; WS4-AA64 #160: Object-start bitmap (conservative-root validation)
;;; ============================================================
;;; 1 bit / 16-byte granule.  page_base (fixed heap base) at 0x10000E00,
;;; bitmap_base at 0x10000E18 — both stored value<<1 by %gc-bitmap-init so
;;; (mem-ref :u64) reads back the raw address.  The bit records that a granule
;;; is a REAL object start; %gc-forward-slot / %gc-scan-copied gate
;;; %gc-copy-object on it, so a false conservative root (a bignum limb / scratch
;;; word that merely looks like a from-space cons/object pointer, but lands
;;; mid-object) is NOT copied — killing the forward-pointer-stamp corruption
;;; class.  Native alloc sites (translate-aarch64 emit-aarch64-gc-mark-start)
;;; set the bit for mutator allocations; %gc-copy-object sets it for GC
;;; survivors (so the post-swap from-space has valid bits); %gc-collect clears
;;; the reclaimed from-space range after each swap.  When bitmap_base = 0
;;; (uninitialised: bare-metal, x64-dead path) EVERY predicate degrades to the
;;; pre-bitmap behaviour, so this whole mechanism is inert unless
;;; %gc-bitmap-init has run.

(defun %gc-bitmap-page-base () (mem-ref #x10000E00 :u64))
(defun %gc-bitmap-base ()      (mem-ref #x10000E18 :u64))
;; #160 bug#4: the CONS-KIND bitmap (a second object-start-sized bitmap, 1 bit /
;; 16-byte granule) records which starts are 16-byte CONS cells (car/cdr, no
;; header) vs headered OBJECTS.  %gc-scan-copied uses it to walk to-space
;; object-by-object and scan each object's slots by TYPE — instead of the old
;; conservative every-word scan that forwarded false-positive data words
;; (bignum limbs, float IEEE bits) and corrupted runtime structures.  Base at
;; config 0x10000E40 (past the x64 MCGC config block, which ends at 0x10000E38).
(defun %gc-cons-bitmap-base () (mem-ref #x10000E40 :u64))

(defun %gc-bitmap-init ()
  "Reserve the object-start AND cons-kind bitmaps.  page_base = the fixed heap
   base (from_start BEFORE any collection = the lowest object address); each
   bitmap = an 8 MB zero-filled PROT_RW mmap (covers a 1 GB heap span at 1 bit /
   16 bytes).  MUST run in kernel-main BEFORE any instrumented allocation (else a
   native alloc-site bit-set would write through a garbage base).  Non-allocating
   (mmap result + config addresses are fixnums)."
  (setf (mem-ref #x10000E00 :u64) (%gc-from-start))
  (setf (mem-ref #x10000E18 :u64) (%mmap-exec-page #x800000))
  (setf (mem-ref #x10000E40 :u64) (%mmap-exec-page #x800000)))

(defun %gc-mark-start (raw-addr)
  "Set the object-start bit for the object whose raw byte address is RAW-ADDR."
  (let ((bmp (%gc-bitmap-base)))
    (if (= bmp 0)
        nil
        (let* ((gran (ash (- raw-addr (%gc-bitmap-page-base)) -4))
               (byte-addr (+ bmp (ash gran -3)))
               (bit (logand gran 7)))
          (setf (mem-ref byte-addr :u8)
                (logior (mem-ref byte-addr :u8) (ash 1 bit)))))))

(defun %gc-is-start (raw-addr)
  "T if RAW-ADDR is a recorded object start.  When the bitmap is off
   (bitmap_base = 0) returns T unconditionally — the pre-#160 behaviour."
  (let ((bmp (%gc-bitmap-base)))
    (if (= bmp 0)
        t
        (let* ((gran (ash (- raw-addr (%gc-bitmap-page-base)) -4))
               (byte-addr (+ bmp (ash gran -3)))
               (bit (logand gran 7)))
          (if (= (logand (mem-ref byte-addr :u8) (ash 1 bit)) 0) nil t)))))

(defun %gc-mark-cons (raw-addr)
  "Set the CONS-KIND bit for the 16-byte cons whose raw byte address is RAW-ADDR
   (in addition to its object-start bit).  Only conses set this bit; headered
   objects leave it clear, so %gc-scan-copied classifies cons vs object."
  (let ((bmp (%gc-cons-bitmap-base)))
    (if (= bmp 0)
        nil
        (let* ((gran (ash (- raw-addr (%gc-bitmap-page-base)) -4))
               (byte-addr (+ bmp (ash gran -3)))
               (bit (logand gran 7)))
          (setf (mem-ref byte-addr :u8)
                (logior (mem-ref byte-addr :u8) (ash 1 bit)))))))

(defun %gc-is-cons-granule (raw-addr)
  "T if RAW-ADDR is a recorded CONS start (cons-kind bit set)."
  (let ((bmp (%gc-cons-bitmap-base)))
    (if (= bmp 0)
        nil
        (let* ((gran (ash (- raw-addr (%gc-bitmap-page-base)) -4))
               (byte-addr (+ bmp (ash gran -3)))
               (bit (logand gran 7)))
          (if (= (logand (mem-ref byte-addr :u8) (ash 1 bit)) 0) nil t)))))

(defun %gc-leaf-subtag-p (subtag)
  "T if an object of SUBTAG has a RAW-DATA payload (no Lisp pointers) — the
   to-space scan must NOT forward its slots.  Enumerated leaves: string #x10,
   u8-vector #x11, u64-vector #x14, sap #x16, bignum #x30, and every float
   flavor (#x60 double / #x64 single / #x65 short / #x66 long; #x60 also =
   mvm-bytecode, likewise raw).  Everything else (simple-vector/array/ratio/
   struct/hash/symbol/function/closure/module/…) is pointer-bearing — scan it.
   See compiler.lisp's float-subtag note: floats live at #x60/#x64..#x66 and
   #x61..#x63 are NON-float pointer-bearing objects, so they are NOT leaves."
  (if (= subtag #x10) t
   (if (= subtag #x11) t
    (if (= subtag #x14) t
     (if (= subtag #x16) t
      (if (= subtag #x30) t
       (if (= subtag #x60) t
        (if (= subtag #x64) t
         (if (= subtag #x65) t
          (if (= subtag #x66) t
           nil))))))))))

(defun %gc-clear-bitmap-range (range-start size)
  "Zero the object-start AND cons-kind bitmap bits for [RANGE-START,
   RANGE-START+SIZE) — the reclaimed from-space after a swap — so stale bits
   can't falsely accept/classify garbage next cycle.  Clears u64-at-a-time over
   the interior then a byte tail; stays WITHIN the exact byte range (the
   per-semispace bitmap boundary at size/128 is not 8-aligned, so over-clearing
   would erase the sibling's live bits)."
  (let ((bmp (%gc-bitmap-base))
        (cbmp (%gc-cons-bitmap-base)))
    (if (= bmp 0)
        nil
        (let* ((off (ash (- range-start (%gc-bitmap-page-base)) -7))
               (base (+ bmp off))
               (cbase (+ cbmp off))
               (nbytes (ash size -7))
               (nq (ash nbytes -3))
               (i 0))
          (loop
            (when (>= i nq) (return nil))
            (setf (mem-ref (+ base (* i 8)) :u64) 0)
            (setf (mem-ref (+ cbase (* i 8)) :u64) 0)
            (setq i (+ i 1)))
          (let ((j (* nq 8)))
            (loop
              (when (>= j nbytes) (return nil))
              (setf (mem-ref (+ base j) :u8) 0)
              (setf (mem-ref (+ cbase j) :u8) 0)
              (setq j (+ j 1))))))))

;;; ============================================================
;;; Object Copying
;;; ============================================================

(defun %gc-copy-object (raw-addr tag from-start from-size free-ptr)
  "Copy the object at byte address RAW-ADDR from from-space to to-space.
   TAG is its tag nibble (1 = cons, 9 = headered object); RAW-ADDR is already
   tag-stripped and range-checked by %gc-cand-addr — the caller never has to
   materialise the 64-bit word, which is what keeps this allocation-free.
   FREE-PTR is the current free pointer in to-space (raw byte address).

   Result stored at:
     0x10000100: the new tagged value, as an EXACT machine word (two halves)
     0x10000108: new free pointer (raw byte address, mem-ref :u64 convention)"
  (cond
    ;; Cons cell (tag = 1)
    ((= tag 1)
     (let ((car-lo (%gc-word-lo raw-addr))
           (car-hi (%gc-word-hi raw-addr)))
       (if (%gc-is-forward-lo car-lo)
           ;; Already forwarded — the from-space word IS (new-addr | 15);
           ;; hand back (new-addr | 1), high half untouched.
           (progn
             (%gc-set-halves #x10000100
                             (logior (logand car-lo (lognot 15)) 1) car-hi)
             (%gc-set-tmp-free free-ptr))
           ;; Copy car and cdr to to-space — exact word moves, no Lisp value
           ;; ever formed (a stack/heap word can be any 64 bits).
           (if (> (+ free-ptr 16) (%gc-to-end))
               ;; TO-SPACE OVERRUN GATE (see the object path below).
               (progn
                 (%gc-store-tagged #x10000100 raw-addr 1)
                 (%gc-set-tmp-free free-ptr))
           (progn
             (%gc-move-word raw-addr free-ptr)
             (%gc-move-word (+ raw-addr 8) (+ free-ptr 8))
             ;; Leave forwarding pointer in from-space
             (%gc-store-tagged raw-addr free-ptr 15)
             ;; #160: record the survivor's start bit in to-space (valid
             ;; from-space bits for the NEXT cycle after the swap).
             (%gc-mark-start free-ptr)
             ;; #160 bug#4: it's a CONS — record the cons-kind bit too so
             ;; the to-space object-by-object scan classifies it (16 bytes,
             ;; scan car+cdr) rather than reading car as an object header.
             (%gc-mark-cons free-ptr)
             ;; Return results
             (%gc-store-tagged #x10000100 free-ptr 1)
             (%gc-set-tmp-free (+ free-ptr 16)))))))
    ;; Object (tag = 9)
    ((= tag 9)
     (let ((hdr-lo (%gc-word-lo raw-addr))
           (hdr-hi (%gc-word-hi raw-addr)))
       (if (%gc-is-forward-lo hdr-lo)
           ;; Already forwarded
           (progn
             (%gc-set-halves #x10000100
                             (logior (logand hdr-lo (lognot 15)) 9) hdr-hi)
             (%gc-set-tmp-free free-ptr))
           ;; Copy header + all element slots
           (let ((count (%gc-header-count hdr-lo hdr-hi)))
             ;; Total size: (count + 2) * 8, aligned to 16
             ;; +2 for header word + padding word
             (let ((total-bytes (logand (+ (* (+ count 2) 8) 15) (lognot 15)))
                   (i 0))
               ;; MEMORY-SAFETY GATES.  No live object can be larger than the
               ;; semispace it was allocated in, so a computed size above
               ;; FROM-SIZE means this candidate is not an object at all and its
               ;; "header" is data.  DECLINE it instead of running the copy loop.
               ;;
               ;; This became reachable the moment the :u64 halving defect above
               ;; was fixed: before, %gc-is-pointer rejected every real pointer
               ;; so nothing was ever copied.  On a target WITHOUT the #160
               ;; object-start bitmap (bare-metal aarch64 — %gc-bitmap-init is
               ;; mmap-only and *aarch64-gc-bitmap-enabled* is nil, so
               ;; %gc-is-start degrades to T) a false conservative stack root is
               ;; the only thing standing between the collector and a wild copy.
               ;; MEASURED on the RPi image: a root pointing at a word
               ;; #xDEAD1009 (low nibble 9 — object-shaped) decoded to
               ;; count = 14593296, and the copy loop walked off DRAM into a
               ;; synchronous external abort (ESR #x97000010, FAR #x6F6F7BA5).
               ;; Declining leaves the slot pointing into the reclaimed
               ;; semispace, which is the pre-bitmap conservative-collector risk
               ;; and strictly better than scribbling over memory.  Where the
               ;; bitmap IS on these gates never fire: a real object always fits,
               ;; lies wholly inside its own semispace, and its copy fits in
               ;; to-space.  Three invariants, all provable, all cheap:
               ;;   (1) total-bytes <= from-size
               ;;   (2) raw-addr + total-bytes <= from-start + from-size
               ;;   (3) free-ptr  + total-bytes <= to_end
               ;; WHY (3) IS NOT OPTIONAL, measured and DETERMINISTIC (identical
               ;; trace on two runs of the same image): with only (1), a false
               ;; root near the top of the upper semispace copied past
               ;; 0x10000000 — and the GC METADATA BLOCK begins at 0x10000040,
               ;; so the collector overwrote its own from_start/saved_rsp.  The
               ;; tell is the phase trace turning from "12345" into "1S2345":
               ;; saved_rsp stopped passing %gc-collect's own window check, the
               ;; STACK SCAN was skipped, and the image died two collections
               ;; later.  What makes these roots false is visible in
               ;; compiler.lisp: +t-value+ is #xDEAD1009 and +nil-value+ is
               ;; #xDEAD0001 — the IMMEDIATES T and NIL carry low nibbles 9 and
               ;; 1, i.e. object and cons tags.  They are out of heap range so
               ;; %gc-cand-addr rejects them directly, but a stack word pointing
               ;; at a slot that HOLDS one lands mid-object, and the "header"
               ;; read there is #xDEAD1009 -> count 14593296.  That is exactly
               ;; what the #160 object-start bitmap exists to reject; it is not
               ;; wired up on bare-metal aarch64 (%gc-bitmap-init is mmap-only,
               ;; *aarch64-gc-bitmap-enabled* defaults nil), so until it is,
               ;; these three invariants are the only thing standing between a
               ;; false root and memory corruption.
               ;; TO-SPACE OVERRUN GATE.  MEASURED consequence of NOT having
               ;; it on the RPi image: the top semispace ends at 0x10000000 and
               ;; the GC METADATA BLOCK starts at 0x10000040, so a copy that
               ;; overruns to-space lands directly on from_start/saved_rsp — the
               ;; collector overwrites its own root-scan geometry.  The tell is
               ;; the phase trace turning into "1S2345": saved_rsp no longer
               ;; passes %gc-collect's own window check, so the STACK SCAN IS
               ;; SKIPPED and every stack root is dropped.
               (if (if (> total-bytes from-size) t
                       (if (> (+ raw-addr total-bytes) (+ from-start from-size)) t
                           (> (+ free-ptr total-bytes) (%gc-to-end))))
                   (progn
                     (%gc-store-tagged #x10000100 raw-addr 9)
                     (%gc-set-tmp-free free-ptr))
                   (progn
                     ;; Copy all bytes (word by word)
                     (loop
                       (when (>= i total-bytes) (return nil))
                       (%gc-move-word (+ raw-addr i) (+ free-ptr i))
                       (setq i (+ i 8)))
                     ;; Leave forwarding pointer in old location
                     (%gc-store-tagged raw-addr free-ptr 15)
                     ;; #160: record the survivor's start bit in to-space.
                     (%gc-mark-start free-ptr)
                     ;; Return results
                     (%gc-store-tagged #x10000100 free-ptr 9)
                     (%gc-set-tmp-free (+ free-ptr total-bytes)))))))))
    ;; Not a pointer — unreachable (callers gate on %gc-cand-addr, which only
    ;; admits tags 1 and 9); return the value unchanged so the caller's
    ;; write-back is a no-op.
    (t
     (%gc-store-tagged #x10000100 raw-addr tag)
     (%gc-set-tmp-free free-ptr))))

;;; ============================================================
;;; Root Scanning
;;; ============================================================

(defun %gc-forward-slot (raw-slot-addr from-start from-size free-ptr)
  "Process one root slot at raw byte address RAW-SLOT-ADDR.
   If it contains a pointer into from-space, copy the object and
   update the slot. Returns the new free pointer."
  (let* ((lo (%gc-word-lo raw-slot-addr))
         (hi (%gc-word-hi raw-slot-addr))
         (addr (%gc-cand-addr lo hi from-start from-size)))
    (if addr
        ;; #160: only copy a candidate that is a RECORDED object start.
        ;; A false root (looks like a from-space pointer but lands
        ;; mid-object) is left untouched — no forward-pointer stamp.
        (if (%gc-is-start addr)
            (progn
              (%gc-copy-object addr (logand lo 15) from-start from-size free-ptr)
              ;; Update the slot with the new pointer (exact word move)
              (%gc-move-word #x10000100 raw-slot-addr)
              ;; Return new free pointer
              (%gc-tmp-free))
            free-ptr)
        free-ptr)))

(defun %gc-scan-stack (rsp-val stack-base from-start from-size free-ptr)
  "Scan the stack for root pointers. The stack grows downward,
   so RSP is the lowest address and STACK-BASE is the highest.
   Each 8-byte word on the stack is checked as a potential tagged value.

   WIDTH BUG (measured, WS5/i386): the +8 step is the 64-bit word size baked
   in.  On a 32-bit target the stack is 4-byte-word granular, so this examines
   only every OTHER word and half the roots are never even looked at.  The
   same 64-bit shape is baked into %gc-copy-object (cdr at +8, size
   (count+2)*8) and %gc-scan-copied (slots at +16+i*8); on i386 the cdr is at
   +4, the size is align16((count+1)*4) and slot i is at +4+i*4.  Both need to
   come from the target word size, not from a literal."
  (let ((addr rsp-val)
        (fp free-ptr))
    (loop
      (when (>= addr stack-base) (return fp))
      (setq fp (%gc-forward-slot addr from-start from-size fp))
      (setq addr (+ addr 8)))))

(defun %gc-scan-globals (from-start from-size free-ptr)
  "Scan the global variable alist for root pointers.
   The alist head is at 0x10000080. Each entry is (name-hash . value).
   We need to forward the alist spine (cons cells) and the values."
  ;; The globals alist head pointer itself
  (let ((fp (%gc-forward-slot #x10000080 from-start from-size free-ptr)))
    ;; The symbol intern table head pointer
    (setq fp (%gc-forward-slot #x10000088 from-start from-size fp))
    ;; The keyword intern table (0x10000148) and package-by-hash table
    ;; (0x10000170) are ALSO heap roots — both are hash-tables interned
    ;; into during runtime EVAL.  Missing them stranded keywords/symbols
    ;; in dead from-space after a collection, faulting the next deref.
    ;; (This mirrors the x64 inline trampoline fix in translate-x64.lisp;
    ;; keep the two root sets in sync.)
    (setq fp (%gc-forward-slot #x10000148 from-start from-size fp))
    (setq fp (%gc-forward-slot #x10000170 from-start from-size fp))
    ;; NOTE: the pre-interned signal-condition symbols at 0xCA0/0xCA8/0xCB0
    ;; (%init-signal-symbols) are deliberately NOT scanned: they are
    ;; interned native MVM symbols already forwarded via the symbol intern
    ;; table at 0x10000088, and double-forwarding them regressed the x64
    ;; ASDF gauntlet (form 44 -> 36, byte-size-matched padding reached 44).
    ;; Keep the x64 trampoline and this Lisp-side scan in sync — see the
    ;; matching note in emit-gc-trampoline (translate-x64.lisp).
    ;; Multiple-value return buffer extras at 0x10000098+.  MV-COUNT at
    ;; 0x10000090 is a tagged fixnum; the live extras are (count-1) words
    ;; from 0x10000098.  These can hold heap pointers (secondary values)
    ;; read after an allocating step (e.g. %values-list conses each
    ;; element), so a collection mid-read strands the unread extras.
    ;; Scan exactly count-1 words, only when count>=2.
    ;; MV-COUNT is a TAGGED FIXNUM machine word (count<<1); its low half alone
    ;; is exact for any plausible count.  Historically this read the halved
    ;; :u64 value and shifted again, yielding count/2 — the extras scan was
    ;; silently short by half.
    (let ((count (ash (%gc-word-lo #x10000090) -1)))
      (when (>= count 2)
        (let ((i 0))
          (loop
            (when (>= i (- count 1)) (return))
            (setq fp (%gc-forward-slot (+ #x10000098 (* i 8))
                                       from-start from-size fp))
            (setq i (+ i 1))))))
    fp))

;;; ============================================================
;;; Cheney Scan Loop
;;; ============================================================

(defun %gc-scan-copied (scan-start free-ptr from-start from-size)
  "Cheney scan loop, TYPE-AWARE (#160 bug#4).  Walk to-space OBJECT-BY-OBJECT
   from SCAN-START to the (growing) free pointer.  At each object start use the
   CONS-KIND bitmap to classify:
     - CONS (cons-kind bit set): a 16-byte car/cdr pair — forward BOTH words,
       advance 16.
     - OBJECT (tag 9, header at start): read header → count + subtag.  If the
       subtag is a LEAF (%gc-leaf-subtag-p: string/u8/u64/sap/bignum/float),
       DO NOT scan its raw-data payload; else forward each of its COUNT slots
       (which start at start+16 after the header+padding words).  Advance by the
       object's aligned size = align16((count+2)*8).
   This replaces the old conservative every-word scan, which forwarded
   false-positive DATA words (bignum limbs, float IEEE bits) that happened to
   look like from-space pointers at a real object start — corrupting runtime
   structures (the GETHASH/SYMBOL-VALUE type-error recursion).  If the bitmaps
   are off (bitmap_base=0), fall back to the old word-by-word conservative scan
   (bare-metal / uninstrumented builds keep working).
   SCAN-START / FREE-PTR: raw byte addresses.  Returns the final free pointer."
  (let ((scan scan-start)
        (fp free-ptr))
    (if (= (%gc-bitmap-base) 0)
        ;; -- Legacy conservative word-by-word scan (bitmaps disabled) --
        (progn
          (loop
            (when (>= scan fp) (return nil))
            (let* ((lo (%gc-word-lo scan))
                   (hi (%gc-word-hi scan))
                   (addr (%gc-cand-addr lo hi from-start from-size)))
              (when addr
                (%gc-copy-object addr (logand lo 15) from-start from-size fp)
                (%gc-move-word #x10000100 scan)
                (setq fp (%gc-tmp-free))))
            (setq scan (+ scan 8)))
          fp)
        ;; -- Type-aware object-by-object walk (bitmaps on) --
        (progn
          (loop
            (when (>= scan fp) (return nil))
            (if (%gc-is-cons-granule scan)
                ;; CONS: forward car (scan) and cdr (scan+8); 16 bytes.
                (progn
                  (setq fp (%gc-forward-slot scan from-start from-size fp))
                  (setq fp (%gc-forward-slot (+ scan 8) from-start from-size fp))
                  (setq scan (+ scan 16)))
                ;; OBJECT: header at scan.
                (let* ((hdr-lo (%gc-word-lo scan))
                       (hdr-hi (%gc-word-hi scan))
                       (count (%gc-header-count hdr-lo hdr-hi))
                       (subtag (logand hdr-lo #xFF))
                       (total-bytes (logand (+ (* (+ count 2) 8) 15)
                                            (lognot 15))))
                  ;; Pointer-bearing objects: forward each of the COUNT slots
                  ;; (slot i at scan + 16 + i*8 — header word + padding word
                  ;; precede slot0).  Leaf objects: skip the raw payload.
                  (unless (%gc-leaf-subtag-p subtag)
                    (let ((i 0))
                      (loop
                        (when (>= i count) (return nil))
                        (setq fp (%gc-forward-slot (+ scan (+ 16 (* i 8)))
                                                   from-start from-size fp))
                        (setq i (+ i 1)))))
                  ;; Guard against a zero/garbage size stalling the walk.
                  (setq scan (+ scan (if (< total-bytes 16) 16 total-bytes))))))
          fp))))

;;; ============================================================
;;; Main Collector Entry Point
;;; ============================================================

(defun %gc-collect ()
  "Cheney copying garbage collector.
   Called from the GC trampoline when R12 >= R14.

   The trampoline has saved all registers and stored:
     R12 → [0x10000070]  (alloc pointer)
     R14 → [0x10000078]  (alloc limit)
     RSP → [0x10000068]  (stack pointer for root scanning)

   This function must NOT allocate any heap memory."
  (let ((from-start (%gc-from-start))
        (to-start (%gc-to-start))
        (space-size (%gc-space-size))
        (stack-base (%gc-stack-base))
        (saved-rsp (%gc-saved-rsp)))
    (write-char-serial 49)  ; '1' — got metadata
    ;; Sanity-check: saved-rsp must be in the valid stack window
    ;; (between guard at 0x07200000 and stack-base at 0x08000000 for
    ;; the AArch64 ANSI build).  If it's 0 or otherwise bogus, a
    ;; longjmp earlier zeroed the slot — scanning from 0 would walk
    ;; 128 MB of unrelated memory, eventually hitting the guard page
    ;; (or worse, corrupting random objects).  Skip the stack scan
    ;; in that case; we'll get a less-precise GC but it won't
    ;; runaway.  Print 'S' to mark the skip.
    (when (or (< saved-rsp #x07200000) (>= saved-rsp stack-base))
      (write-char-serial 83)  ; 'S' — stack-scan skipped
      (setq saved-rsp stack-base))
    ;; The free pointer in to-space starts at to_start
    ;; Publish to-space's end so %gc-copy-object can refuse an overrunning
    ;; copy (the metadata block sits immediately above the heap — see the gate
    ;; in %gc-copy-object).
    (%gc-set-to-end (+ to-start space-size))
    (let ((free-ptr to-start))
      ;; Step 1: Copy roots from the stack
      (setq free-ptr (%gc-scan-stack saved-rsp stack-base
                                      from-start space-size free-ptr))
      (write-char-serial 50)  ; '2' — stack scanned
      ;; Step 2: Copy roots from globals
      (setq free-ptr (%gc-scan-globals from-start space-size free-ptr))
      (write-char-serial 51)  ; '3' — globals scanned
      ;; Step 3: Cheney scan loop — process all copied objects
      (setq free-ptr (%gc-scan-copied to-start free-ptr
                                       from-start space-size))
      (write-char-serial 52)  ; '4' — scan done
      ;; Step 4: Swap semispaces
      (%gc-set-from-start to-start)
      (%gc-set-to-start from-start)
      ;; #160: clear the reclaimed (old from-space) bitmap range so stale
      ;; start-bits can't falsely accept garbage on the next cycle.  Survivors'
      ;; bits were set in to-space by %gc-copy-object (now the new from-space).
      (%gc-clear-bitmap-range from-start space-size)
      ;; Step 5: Update alloc pointer and limit
      (%gc-set-r12 free-ptr)
      (%gc-set-r14 (+ to-start space-size))
      ;; Step 6: Increment GC count
      (%gc-set-count (+ (%gc-count) 1))
      (write-char-serial 53)  ; '5' — done
      )))

;;; ============================================================
;;; GC Initialization
;;; ============================================================

(defun %gc-init (heap-start heap-size stack-base)
  "Initialize the GC metadata.
   HEAP-START: raw byte address of heap start (after globals area).
   HEAP-SIZE: total heap size in bytes (both semispaces).
   STACK-BASE: raw byte address of stack base (highest address).

   Splits the heap into two equal semispaces."
  (let ((space-size (ash heap-size -1)))  ; half the total heap
    (%gc-set-from-start heap-start)
    (%gc-set-to-start (+ heap-start space-size))
    (setf (mem-ref #x10000050 :u64) space-size)      ; space_size
    (setf (mem-ref #x10000058 :u64) stack-base)       ; stack_base
    (%gc-set-count 0)))
