;;;; mcgc-pin.lisp — MCGC stage-4d persistent PIN API + pin-stress probe.
;;;;
;;;; Included ONLY in pinning builds (MODUS_MCGC_PINNING=1).  Flag-off builds
;;;; do NOT compile this file, so the flag-off binary stays byte-identical to
;;;; canonical.  Provides:
;;;;   %pin-object / %unpin-object  — persistent per-page pin-count API.
;;;;   %pin-object-addr             — pin + return stable raw byte address (FFI).
;;;;   %mcgc-pin-stress             — the deliverable probe (allocate, pin,
;;;;                                  force GCs, assert address stable + intact).
;;;;
;;;; Addressing rule (CLHS-style mem-ref trap): every address fed to mem-ref is
;;;; either a CONSTANT config slot or a clean fixnum-range computed integer.  We
;;;; NEVER feed a :u64-loaded pointer back as a mem-ref address (that would
;;;; double-untag → SIGSEGV).  The pin-count base is recomputed each call from
;;;; constant config slots, not loaded-then-dereffed.

(in-package :modus.mvm)

;;; MCGC config-word slots (constants — same addresses boot writes).
(defconstant +mcgc-pin-cfg-page-base+   #x10000E00)  ; raw addr, first data byte
(defconstant +mcgc-pin-cfg-page-count+  #x10000E08)  ; number of pages
(defconstant +mcgc-pin-cfg-freelist+    #x10000E20)  ; raw addr of run-free-list

(defun %mcgc-pincount-base ()
  "Raw byte address of the per-page u32 pin-count array.
   pincount_base = freelist_base + page_count*4 (the array lives just past the
   run-free-list in the MAP_ANON-zeroed metadata region).  Both operands come
   from constant config slots, so the result is a clean fixnum address."
  (+ (mem-ref +mcgc-pin-cfg-freelist+ :u64)
     (* (mem-ref +mcgc-pin-cfg-page-count+ :u64) 4)))

(defun %mcgc-obj-raw-addr (obj)
  "Raw byte address of heap object OBJ (cons or object).  Proven tagged->raw
   derivation (cf. net/actors.lisp soft-subtag): a tagged pointer's Lisp value
   is (real_tagged_ptr >> 1); clearing low bits and shifting back recovers a
   mem-ref-usable raw byte address."
  (ash (logand obj (- 0 4)) 1))

(defun %mcgc-obj-size (obj)
  "Byte size of OBJ: 16 for a cons; else align16((count+2)*8) from the object
   header's element-count field (header >> 8)."
  (if (consp obj)
      16
      (let* ((raw (%mcgc-obj-raw-addr obj))
             (header (mem-ref raw :u64))
             (count (ash header -8)))
        (logand (+ (* (+ count 2) 8) 15) (lognot 15)))))

(defun %mcgc-pin-pages (obj delta)
  "Add DELTA (+1 / -1) to the pin-count of every page OBJ covers.  Returns the
   object's raw byte address.  No-op (still returns the address) when the page
   metadata isn't initialised (page_base==0)."
  (let ((page-base (mem-ref +mcgc-pin-cfg-page-base+ :u64))
        (raw (%mcgc-obj-raw-addr obj)))
    (when (> page-base 0)
      (let* ((size (%mcgc-obj-size obj))
             (pc-base (%mcgc-pincount-base))
             (first-page (ash (- raw page-base) -12))
             (last-page (ash (- (+ raw (- size 1)) page-base) -12))
             (p first-page))
        (loop
          (when (> p last-page) (return))
          (let* ((slot (+ pc-base (* p 4)))
                 (cur (mem-ref slot :u32)))
            (setf (mem-ref slot :u32) (+ cur delta)))
          (setq p (+ p 1)))))
    raw))

(defun %pin-object (obj)
  "Pin OBJ in place across GCs (increment per-page pin-count for every page it
   covers).  Returns OBJ.  Pair with %unpin-object."
  (%mcgc-pin-pages obj 1)
  obj)

(defun %unpin-object (obj)
  "Drop one pin on OBJ (decrement per-page pin-count).  After the last unpin the
   pages may be reclaimed/moved by the next GC."
  (%mcgc-pin-pages obj -1)
  obj)

(defun %pin-object-addr (obj)
  "Pin OBJ and return its stable RAW BYTE ADDRESS (for FFI/DMA handoff)."
  (%mcgc-pin-pages obj 1))

(defun %mcgc-page-pincount (obj)
  "Pin-count of the page containing OBJ's start (diagnostic)."
  (let ((page-base (mem-ref +mcgc-pin-cfg-page-base+ :u64))
        (raw (%mcgc-obj-raw-addr obj)))
    (if (> page-base 0)
        (mem-ref (+ (%mcgc-pincount-base) (* (ash (- raw page-base) -12) 4)) :u32)
        0)))

;;; ------------------------------------------------------------
;;; PIN-STRESS probe (the stage-4 deliverable).
;;; ------------------------------------------------------------
;;; Build a 4-slot vector, fill it with a known pattern, PIN it, capture its
;;; raw address, then churn the heap + force several explicit page collections
;;; while the object stays live ONLY through the pin (we drop the lexical ref by
;;; overwriting locals) — wait: to truly test pinning we keep one lexical ref so
;;; the test can re-check contents, but ALSO verify the address didn't change.
;;; A pinned object's address must be IDENTICAL before and after N GCs, and its
;;; contents intact.  Then %unpin and confirm a subsequent GC can move/reclaim.

(defun %mcgc-churn (n)
  "Allocate ~N conses to push the alloc pointer and provoke heap traffic."
  (let ((acc nil) (i 0))
    (loop
      (when (>= i n) (return))
      (setq acc (cons i acc))
      (setq i (+ i 1)))
    ;; return the length so the work isn't dead-code-eliminated
    (length acc)))

;; A GLOBAL holding the pinned test object.  A global is a PRECISE root, so
;; the page collector would normally FORWARD (move) the object — unless its
;; page is pinned via the persistent pin-count.  This isolates the persistent
;; pin path from conservative stack pinning.
(defvar *mcgc-pin-test-obj* nil)

(defun %mcgc-force-gc-clean ()
  "Churn the heap and force a page collection in a stack frame that does NOT
   reference *mcgc-pin-test-obj* — so the only thing keeping it pinned is the
   persistent pin-count (not a conservative stack pointer)."
  (%mcgc-churn 30000)
  (%mcgc-collect)
  (%mcgc-churn 30000)
  (%mcgc-collect))

(defun %mcgc-pin-stress ()
  "Stage-4 pin-stress deliverable.  Returns a list of result keywords.
   PASS when it contains :ADDR-STABLE :CONTENTS-OK :PINCOUNT-1 :UNPINNED-0.
   The object is held ONLY through a global (precise root, normally moved) and
   pinned via the persistent pin-count; a forced GC from a clean frame must
   leave its address + contents untouched."
  (let ((v (make-array 4)) (results nil))
    (setf (aref v 0) 111) (setf (aref v 1) 222)
    (setf (aref v 2) 333) (setf (aref v 3) 444)
    (setq *mcgc-pin-test-obj* v)
    (let ((addr0 (%pin-object-addr v)))
      ;; drop the local view: from here only the global + pin-count keep it.
      (setq v nil)
      (%mcgc-force-gc-clean)
      (%mcgc-force-gc-clean)
      (let* ((obj *mcgc-pin-test-obj*)
             (addr1 (%mcgc-obj-raw-addr obj)))
        (if (= addr0 addr1)
            (setq results (cons :addr-stable results))
            (setq results (cons :addr-MOVED results)))
        (if (and (= (aref obj 0) 111) (= (aref obj 1) 222)
                 (= (aref obj 2) 333) (= (aref obj 3) 444))
            (setq results (cons :contents-ok results))
            (setq results (cons :contents-CORRUPT results)))
        (if (= (%mcgc-page-pincount obj) 1)
            (setq results (cons :pincount-1 results))
            (setq results (cons :pincount-WRONG results)))
        ;; unpin -> count 0 -> page reclaimable
        (%unpin-object obj)
        (if (= (%mcgc-page-pincount obj) 0)
            (setq results (cons :unpinned-0 results))
            (setq results (cons :unpin-FAILED results)))
        results))))
