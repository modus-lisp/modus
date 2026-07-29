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
  "Check if VAL is a forwarding pointer (low 4 bits = 1111)."
  (= (logand val 15) 15))

(defun %gc-is-pointer (val)
  "Check if VAL is a heap pointer (cons or object)."
  (let ((tag (logand val 15)))
    (if (= tag 1) t
        (if (= tag 9) t nil))))

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

(defun %gc-read64 (raw-addr)
  "Read a 64-bit word from byte address RAW-ADDR.
   RAW-ADDR is a Lisp integer representing the byte address.
   mem-ref handles the fixnum tagging internally."
  (mem-ref raw-addr :u64))

(defun %gc-write64 (raw-addr val)
  "Write a 64-bit word VAL to byte address RAW-ADDR.
   RAW-ADDR is a Lisp integer representing the byte address.
   mem-ref handles the fixnum tagging internally."
  (setf (mem-ref raw-addr :u64) val))

(defun %gc-in-space (ptr space-start space-size)
  "Check if tagged pointer PTR points into the space starting at
   SPACE-START with size SPACE-SIZE. PTR is a raw tagged value;
   the actual byte address is derived by stripping the tag."
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

(defun %gc-bitmap-init ()
  "Reserve the object-start bitmap.  page_base = the fixed heap base (from_start
   BEFORE any collection = the lowest object address); bitmap = an 8 MB
   zero-filled PROT_RW mmap (covers a 1 GB heap span at 1 bit / 16 bytes).  MUST
   run in kernel-main BEFORE any instrumented allocation (else a native
   alloc-site bit-set would write through a garbage bitmap_base).  Non-allocating
   (mmap result + config addresses are fixnums)."
  (setf (mem-ref #x10000E00 :u64) (%gc-from-start))
  (setf (mem-ref #x10000E18 :u64) (%mmap-exec-page #x800000)))

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

(defun %gc-clear-bitmap-range (range-start size)
  "Zero the bitmap bits for [RANGE-START, RANGE-START+SIZE) — the reclaimed
   from-space after a swap — so stale start-bits can't falsely accept garbage
   next cycle.  Clears u64-at-a-time over the interior then a byte tail; stays
   WITHIN the exact byte range (the per-semispace bitmap boundary at size/128 is
   not 8-aligned, so over-clearing would erase the sibling's live bits)."
  (let ((bmp (%gc-bitmap-base)))
    (if (= bmp 0)
        nil
        (let* ((base (+ bmp (ash (- range-start (%gc-bitmap-page-base)) -7)))
               (nbytes (ash size -7))
               (nq (ash nbytes -3))
               (i 0))
          (loop
            (when (>= i nq) (return nil))
            (setf (mem-ref (+ base (* i 8)) :u64) 0)
            (setq i (+ i 1)))
          (let ((j (* nq 8)))
            (loop
              (when (>= j nbytes) (return nil))
              (setf (mem-ref (+ base j) :u8) 0)
              (setq j (+ j 1))))))))

;;; ============================================================
;;; Object Copying
;;; ============================================================

(defun %gc-copy-object (ptr from-start from-size free-ptr)
  "Copy the object at tagged PTR from from-space to to-space.
   FROM-START and FROM-SIZE define the from-space boundaries.
   FREE-PTR is the current free pointer in to-space (raw byte address).
   Returns (new-tagged-value . new-free-ptr) packed as two values
   stored at fixed temporaries since we can't allocate cons cells.

   Result stored at:
     0x10000180: new tagged value
     0x10000188: new free pointer (raw)"
  (let ((tag (logand ptr 15)))
    (cond
      ;; Cons cell (tag = 1)
      ((= tag 1)
       (let ((raw-addr (- ptr 1)))   ; strip cons tag to get byte address
         ;; Check if already forwarded
         (let ((car-val (%gc-read64 raw-addr)))
           (if (%gc-is-forward car-val)
               ;; Already forwarded — extract new address
               (progn
                 (%gc-write64 #x10000100 (logior (logand car-val (lognot 15)) 1))
                 (%gc-write64 #x10000108 free-ptr))
               ;; Copy car and cdr to to-space
               (let ((cdr-val (%gc-read64 (+ raw-addr 8))))
                 ;; Write car and cdr to free-ptr in to-space
                 (%gc-write64 free-ptr car-val)
                 (%gc-write64 (+ free-ptr 8) cdr-val)
                 ;; New tagged pointer
                 (let ((new-ptr (logior free-ptr 1)))
                   ;; Leave forwarding pointer in from-space
                   (%gc-write64 raw-addr (logior free-ptr 15))
                   ;; #160: record the survivor's start bit in to-space (valid
                   ;; from-space bits for the NEXT cycle after the swap).
                   (%gc-mark-start free-ptr)
                   ;; Return results
                   (%gc-write64 #x10000100 new-ptr)
                   (%gc-write64 #x10000108 (+ free-ptr 16))))))))
      ;; Object (tag = 9)
      ((= tag 9)
       (let ((raw-addr (- ptr 9)))   ; strip object tag to get byte address
         ;; Read header
         (let ((header (%gc-read64 raw-addr)))
           (if (%gc-is-forward header)
               ;; Already forwarded
               (progn
                 (%gc-write64 #x10000100 (logior (logand header (lognot 15)) 9))
                 (%gc-write64 #x10000108 free-ptr))
               ;; Copy header + all element slots
               (let ((count (ash header -8))  ; element count from header
                     (subtag (logand header #xFF)))
                 ;; Total size: (count + 2) * 8, aligned to 16
                 ;; +2 for header word + padding word
                 (let ((total-bytes (logand (+ (* (+ count 2) 8) 15) (lognot 15)))
                       (i 0))
                   ;; Copy all bytes (word by word)
                   (loop
                     (when (>= i total-bytes) (return nil))
                     (%gc-write64 (+ free-ptr i) (%gc-read64 (+ raw-addr i)))
                     (setq i (+ i 8)))
                   ;; New tagged pointer
                   (let ((new-ptr (logior free-ptr 9)))
                     ;; Leave forwarding pointer in old location
                     (%gc-write64 raw-addr (logior free-ptr 15))
                     ;; #160: record the survivor's start bit in to-space.
                     (%gc-mark-start free-ptr)
                     ;; Return results
                     (%gc-write64 #x10000100 new-ptr)
                     (%gc-write64 #x10000108 (+ free-ptr total-bytes)))))))))
      ;; Not a pointer — return unchanged
      (t
       (%gc-write64 #x10000100 ptr)
       (%gc-write64 #x10000108 free-ptr)))))

;;; ============================================================
;;; Root Scanning
;;; ============================================================

(defun %gc-forward-slot (raw-slot-addr from-start from-size free-ptr)
  "Process one root slot at raw byte address RAW-SLOT-ADDR.
   If it contains a pointer into from-space, copy the object and
   update the slot. Returns the new free pointer."
  (let ((val (%gc-read64 raw-slot-addr)))
    (if (%gc-is-pointer val)
        (if (%gc-in-space val from-start from-size)
            ;; #160: only copy a candidate that is a RECORDED object start.
            ;; A false root (looks like a from-space pointer but lands
            ;; mid-object) is left untouched — no forward-pointer stamp.
            (if (%gc-is-start (logand val (lognot 15)))
                (progn
                  (%gc-copy-object val from-start from-size free-ptr)
                  ;; Update the slot with the new pointer
                  (%gc-write64 raw-slot-addr (%gc-read64 #x10000100))
                  ;; Return new free pointer
                  (%gc-read64 #x10000108))
                free-ptr)
            free-ptr)
        free-ptr)))

(defun %gc-scan-stack (rsp-val stack-base from-start from-size free-ptr)
  "Scan the stack for root pointers. The stack grows downward,
   so RSP is the lowest address and STACK-BASE is the highest.
   Each 8-byte word on the stack is checked as a potential tagged value."
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
    (let ((count (ash (%gc-read64 #x10000090) -1)))
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
  "Cheney scan loop: scan all objects that have been copied to to-space.
   For each pointer field in each object, if it points into from-space,
   copy the referenced object and update the pointer.
   SCAN-START: start of to-space data (raw byte address).
   FREE-PTR: current end of copied data (raw byte address).
   Returns the final free pointer."
  (let ((scan scan-start)
        (fp free-ptr))
    (loop
      (when (>= scan fp) (return fp))
      ;; Look at the word at scan position.
      ;; We need to figure out what kind of object this is.
      ;; In to-space, objects are laid out contiguously:
      ;; - cons cells: 16 bytes (car, cdr)
      ;; - objects: header + padding + slots
      ;;
      ;; The trick: we don't know a priori whether we're at a cons or object.
      ;; But we DO know because we track what was copied. The copy function
      ;; copies cons cells as 16-byte chunks and objects as header+slots.
      ;;
      ;; Simpler approach: just treat every 8-byte word as a potential
      ;; pointer and forward it. This is conservative for the scan but
      ;; correct because we only forward values that are actually pointers
      ;; into from-space.
      (let ((val (%gc-read64 scan)))
        (if (%gc-is-pointer val)
            (if (%gc-in-space val from-start from-size)
                ;; #160: object-start gate (same as %gc-forward-slot) — a
                ;; copied object's raw data word that looks like a from-space
                ;; pointer but isn't an object start is skipped.
                (if (%gc-is-start (logand val (lognot 15)))
                    (progn
                      (%gc-copy-object val from-start from-size fp)
                      (%gc-write64 scan (%gc-read64 #x10000100))
                      (setq fp (%gc-read64 #x10000108)))
                    nil)
                nil)
            ;; Not a pointer — check if it looks like an object header
            ;; (it has a valid subtag and count). Actually, we don't need
            ;; to distinguish — just scan word by word. Object headers
            ;; are not pointers (low bit patterns don't match cons/object
            ;; tags in practice), so they'll be skipped.
            nil))
      (setq scan (+ scan 8)))))

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
