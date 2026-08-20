;;;; fdt.lisp — read the firmware's Flat Device Tree, and back %CLI-GETENV
;;;; with /chosen/bootargs (task #271).
;;;;
;;;; WHY.  Every knob in a bare-metal image was baked by the HOST Lisp at BUILD
;;;; time, so changing one cost a ~20-minute rebuild.  A real operating system
;;;; is told about itself by its firmware at boot.  On AArch64 the Linux boot
;;;; protocol hands the kernel the physical address of a Flat Device Tree in
;;;; **x0**, and the firmware puts the kernel command line in that tree at
;;;; /chosen/bootargs — which on a Raspberry Pi is literally the contents of
;;;; cmdline.txt, and under QEMU is `-append'.  Modus received that pointer on
;;;; every boot and threw it away.
;;;;
;;;; boot/boot-rpi-cl.lisp now stores x0 to +RPI-CL-DTB-PTR-SLOT+ (0x10000F00)
;;;; as the very first instruction of the image.  This file is the reader.
;;;;
;;;; LAZY, NOT EAGER.  Nothing here runs at boot.  The tree is walked on the
;;;; FIRST %CLI-GETENV call and the result cached, by which time the heap, the
;;;; CL runtime and the condition system are all up.  That ordering is not a
;;;; nicety on this platform: on a Pi every exception vector that is not the
;;;; sync-fault reporter is `b .', so a parser bug during boot is an INVISIBLE
;;;; hang — 100% CPU and not one byte of console output.  See #263, which cost
;;;; several sessions precisely because a fault looked like a compiler loop.
;;;;
;;;; NEVER FAULT.  A device tree is data supplied by firmware we do not
;;;; control, and it is read with the MMU OFF, where a stray address is an
;;;; unrecoverable data abort rather than a signal.  Every step is therefore
;;;; range-checked BEFORE it is dereferenced, and every failure returns NIL so
;;;; the machine boots exactly as it did before this file existed.  The guards,
;;;; in the order they fire:
;;;;
;;;;   %FDT-PLAUSIBLE-P   pointer is 0 / below 0x1000 / at or above the BCM2837
;;;;                      peripheral window 0x3F000000 / not 4-byte aligned
;;;;   %FDT-HEADER-OK     magic is not 0xD00DFEED; totalsize < 64 or > 2 MB;
;;;;                      the blob would run past the peripherals; version
;;;;                      outside [16,64]
;;;;   %FDT-BOOTARGS-AT   off_dt_struct / off_dt_strings inside the header or
;;;;                      past totalsize; a token straddling the end of the
;;;;                      blob; an unterminated node or property name; a
;;;;                      property length > 64 KB or running past the end; a
;;;;                      name offset past the end; an unknown token; a
;;;;                      negative depth (END_NODE without BEGIN_NODE); more
;;;;                      than 100000 tokens (a cyclic or adversarial blob)
;;;;
;;;; Every one of those returns NIL, and %CLI-GETENV then answers NIL for every
;;;; variable — which is exactly the behaviour of the stub this replaces.
;;;;
;;;; BIG-ENDIAN.  An FDT is big-endian regardless of the CPU.  %FDT-U32 does the
;;;; byte assembly explicitly out of four :u8 loads rather than swapping a :u32
;;;; load: the four multiplies are by constants, so no value ever leaves the
;;;; fixnum range and no variable-count ASH (which would route through
;;;; BIGNUM-ASH and cons) is involved.  The :u8 loads also mean nothing here
;;;; depends on the blob being 4-byte aligned in the first place.
;;;;
;;;; Strings are built with %MAKE-STRING-ARRAY + %PRIM-ASET (raw character
;;;; CODES), the same idiom lib/serial-repl.lisp uses on this board, and read
;;;; back with %PRIM-AREF.  Public AREF on a string yields a CHARACTER; none of
;;;; the comparisons here want one, and going through characters would mean
;;;; CODE-CHAR, which is an unguarded shift-and-tag primop.
;;;;
;;;; The ASET results are bound to a LET variable before being discarded — MVM
;;;; active limitation #2: a variable-index ASET in a `dest=nil' position can
;;;; drop the value.

;;; ------------------------------------------------------------------
;;; Raw access
;;; ------------------------------------------------------------------

;; The slot boot/boot-rpi-cl.lisp's first instruction stores x0 into.  Read as
;; two :u32 halves rather than one :u64: a :u32 load is faithful (the raw bits
;; land in a fixnum whose VALUE is the word), while a :u64 load returns the raw
;; bits SHIFTED RIGHT BY ONE and would silently lose bit 0.
(defun %fdt-ptr-slot () #x10000F00)

(defun %fdt-base ()
  (let ((lo (mem-ref (%fdt-ptr-slot) :u32))
        (hi (mem-ref (+ (%fdt-ptr-slot) 4) :u32)))
    (if (eql hi 0) lo (+ lo (* hi 4294967296)))))

(defun %fdt-u32 (a)
  (+ (* (mem-ref a :u8) 16777216)
     (* (mem-ref (+ a 1) :u8) 65536)
     (* (mem-ref (+ a 2) :u8) 256)
     (mem-ref (+ a 3) :u8)))

(defun %fdt-align4 (n)
  (let ((m (logand n 3)))
    (if (eql m 0) n (+ n (- 4 m)))))

;;; ------------------------------------------------------------------
;;; Guards
;;; ------------------------------------------------------------------

(defun %fdt-plausible-p (base)
  (and (integerp base)
       (> base 4095)
       (< base #x3F000000)
       (eql (logand base 3) 0)))

(defun %fdt-header-ok (base)
  (if (not (%fdt-plausible-p base))
      nil
      (if (not (eql (%fdt-u32 base) #xD00DFEED))
          nil
          (let ((total (%fdt-u32 (+ base 4)))
                (ver (%fdt-u32 (+ base 20))))
            (and (>= total 64)
                 (<= total 2097152)
                 (< (+ base total) #x3F000000)
                 (>= ver 16)
                 (<= ver 64))))))

;;; ------------------------------------------------------------------
;;; Byte-level string helpers (no allocation except where noted)
;;; ------------------------------------------------------------------

(defun %fdt-cstr-len (addr limit)
  (let ((n 0) (res -1) (done nil))
    (loop
      (when done (return res))
      (if (or (>= (+ addr n) limit) (> n 255))
          (setq done t)
          (if (eql (mem-ref (+ addr n) :u8) 0)
              (progn (setq res n) (setq done t))
              (setq n (+ n 1)))))))

(defun %fdt-name-is (addr limit s)
  (let ((n (length s)) (i 0) (ok t) (done nil))
    (loop
      (when done
        (return (and ok
                     (< (+ addr n) limit)
                     (eql (mem-ref (+ addr n) :u8) 0))))
      (if (>= i n)
          (setq done t)
          (progn
            (when (or (>= (+ addr i) limit)
                      (not (eql (mem-ref (+ addr i) :u8) (%prim-aref s i))))
              (setq ok nil)
              (setq done t))
            (setq i (+ i 1)))))))

(defun %fdt-copy-bytes (base n)
  (let ((out (%make-string-array n)) (i 0))
    (loop
      (when (>= i n) (return out))
      (let ((stored (%prim-aset out i (mem-ref (+ base i) :u8))))
        (let ((d stored)) d))
      (setq i (+ i 1)))))

;;; ------------------------------------------------------------------
;;; The walker
;;; ------------------------------------------------------------------
;;; Tokens: FDT_BEGIN_NODE=1 FDT_END_NODE=2 FDT_PROP=3 FDT_NOP=4 FDT_END=9.
;;; /chosen is mandated to be a child of the root, so it is matched at depth 2
;;; (root becomes depth 1) rather than anywhere in the tree.

(defun %fdt-bootargs-at (base)
  (if (not (%fdt-header-ok base))
      nil
      (let ((total (%fdt-u32 (+ base 4)))
            (offs (%fdt-u32 (+ base 8)))
            (offstr (%fdt-u32 (+ base 12))))
        (if (or (< offs 40) (>= offs total) (< offstr 40) (>= offstr total))
            nil
            (let ((p (+ base offs))
                  (limit (+ base total))
                  (strb (+ base offstr))
                  (depth 0)
                  (chosen -1)
                  (res nil)
                  (done nil)
                  (guard 0))
              (loop
                (when done (return res))
                (setq guard (+ guard 1))
                (if (or (> guard 100000) (> (+ p 8) limit))
                    (setq done t)
                    (let ((tok (%fdt-u32 p)))
                      (cond
                        ((eql tok 1)
                         (let ((nlen (%fdt-cstr-len (+ p 4) limit)))
                           (if (< nlen 0)
                               (setq done t)
                               (progn
                                 (setq depth (+ depth 1))
                                 (when (and (< chosen 0)
                                            (eql depth 2)
                                            (%fdt-name-is (+ p 4) limit "chosen"))
                                   (setq chosen depth))
                                 (setq p (+ p 4 (%fdt-align4 (+ nlen 1))))))))
                        ((eql tok 2)
                         (when (eql depth chosen) (setq chosen -1))
                         (setq depth (- depth 1))
                         (setq p (+ p 4))
                         (when (< depth 0) (setq done t)))
                        ((eql tok 3)
                         (if (> (+ p 12) limit)
                             (setq done t)
                             (let ((plen (%fdt-u32 (+ p 4)))
                                   (noff (%fdt-u32 (+ p 8))))
                               (if (or (> plen 65536)
                                       (> (+ p 12 plen) limit)
                                       (>= (+ strb noff) limit))
                                   (setq done t)
                                   (progn
                                     (when (and (> chosen 0)
                                                (> plen 0)
                                                (%fdt-name-is (+ strb noff) limit
                                                              "bootargs"))
                                       (let ((n (%fdt-cstr-len (+ p 12)
                                                               (+ p 12 plen))))
                                         (setq res (%fdt-copy-bytes
                                                    (+ p 12)
                                                    (if (< n 0) plen n)))
                                         (setq done t)))
                                     (setq p (+ p 12 (%fdt-align4 plen))))))))
                        ((eql tok 4) (setq p (+ p 4)))
                        ((eql tok 9) (setq done t))
                        (t (setq done t)))))))))))

;;; ------------------------------------------------------------------
;;; Cached entry point
;;; ------------------------------------------------------------------
;;; *FDT-SCANNED* is the cache VALIDITY flag, kept separate from the value so
;;; that a tree with no /chosen/bootargs (a legitimate NIL) is not re-walked on
;;; every lookup.  Both default to NIL, which is also what MVM active
;;; limitation #7 leaves them as, so no init-form has to run.

(defvar *fdt-bootargs* nil)
(defvar *fdt-scanned* nil)

(defun %fdt-bootargs ()
  (progn
    (when (null *fdt-scanned*)
      (setq *fdt-scanned* t)
      (setq *fdt-bootargs*
            (handler-case (%fdt-bootargs-at (%fdt-base)) (t (c) nil))))
    *fdt-bootargs*))

;;; ------------------------------------------------------------------
;;; KEY=VALUE lookup over the command line
;;; ------------------------------------------------------------------
;;; Whitespace-separated tokens; a token matches when it is NAME followed by
;;; `=' (code 61).  Written as ONE loop with an explicit token-start variable
;;; rather than nested loops, so there is no question about which block a
;;; RETURN leaves.  Position ALEN is treated as a virtual separator, which is
;;; what flushes the final token.

(defun %fdt-space-code-p (c)
  (or (eql c 32) (eql c 9) (eql c 10) (eql c 13) (eql c 0)))

(defun %fdt-codes-eq (s soff name n)
  (let ((i 0) (ok t) (done nil))
    (loop
      (when done (return ok))
      (if (>= i n)
          (setq done t)
          (progn
            (when (not (eql (%prim-aref s (+ soff i)) (%prim-aref name i)))
              (setq ok nil)
              (setq done t))
            (setq i (+ i 1)))))))

(defun %fdt-substr (s start end)
  (let ((n (- end start)))
    (let ((out (%make-string-array n)) (i 0))
      (loop
        (when (>= i n) (return out))
        (let ((stored (%prim-aset out i (%prim-aref s (+ start i)))))
          (let ((d stored)) d))
        (setq i (+ i 1))))))

(defun %bootargs-lookup (args name)
  (if (or (null args) (null name) (eql (length name) 0))
      nil
      (let ((alen (length args))
            (nlen (length name))
            (i 0)
            (start -1)
            (res nil)
            (done nil))
        (loop
          (when done (return res))
          (let ((atend (>= i alen)))
            (let ((sp (if atend t (%fdt-space-code-p (%prim-aref args i)))))
              (if sp
                  (progn
                    (when (>= start 0)
                      (when (and (> (- i start) nlen)
                                 (eql (%prim-aref args (+ start nlen)) 61)
                                 (%fdt-codes-eq args start name nlen))
                        (setq res (%fdt-substr args (+ start nlen 1) i))
                        (setq done t)))
                    (setq start -1))
                  (when (< start 0) (setq start i)))
              (when atend (setq done t))
              (setq i (+ i 1))))))))
