;;;; mcgc-pin.lisp — MCGC stage-4d persistent PIN API.
;;;;
;;;; Included ONLY in pinning builds (MODUS_MCGC_PINNING=1).  Flag-off builds
;;;; do NOT compile this file, so the flag-off binary stays byte-identical to
;;;; canonical.  Provides:
;;;;   %pin-object / %unpin-object  — persistent per-page pin-count API.
;;;;   %pin-object-addr             — pin + return stable raw byte address (FFI).
;;;;
;;;; NOTE: the pin-stress probe lives in a STANDALONE script (test/pin-stress.lisp,
;;;; run via the generic binary), NOT here.  Compiling a heavy probe function into
;;;; the image perturbed the register allocator and broke unrelated functions'
;;;; translation ("cannot load vreg N" cascade) — a latent translator fragility.
;;;; The runtime-eval'd script avoids it entirely.
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

(defun %mcgc-cfg-uval (slot)
  "Read a u64 config word as a CLEAN fixnum value.  (mem-ref slot :u64) returns
   the RAW bits in the result register with NO fixnum shift, so its Lisp VALUE
   is (raw >> 1) — half the true value.  Shifting left by 1 recovers the true
   byte-address / count, matching %mcgc-obj-raw-addr's scale so address
   arithmetic + mem-ref work.  (Without this the persistent %pin path read the
   pin-count array at a half-address and SIGSEGV'd.)"
  (ash (mem-ref slot :u64) 1))

(defun %mcgc-pincount-base ()
  "Raw byte address of the per-page u32 pin-count array.
   pincount_base = freelist_base + page_count*4 (the array lives just past the
   run-free-list in the MAP_ANON-zeroed metadata region).  Both operands come
   from constant config slots, so the result is a clean fixnum address."
  (+ (%mcgc-cfg-uval +mcgc-pin-cfg-freelist+)
     (* (%mcgc-cfg-uval +mcgc-pin-cfg-page-count+) 4)))

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
  (let ((page-base (%mcgc-cfg-uval +mcgc-pin-cfg-page-base+))
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
  (let ((page-base (%mcgc-cfg-uval +mcgc-pin-cfg-page-base+))
        (raw (%mcgc-obj-raw-addr obj)))
    (if (> page-base 0)
        (mem-ref (+ (%mcgc-pincount-base) (* (ash (- raw page-base) -12) 4)) :u32)
        0)))

