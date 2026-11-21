;;;; float-slot-overrides.lisp — THE single definition of the in-image
;;;; float / bignum literal-decomposition accessors.
;;;;
;;;; compiler.lisp defines host versions of IEEE-FLOAT-BITS / IEEE-FLOAT-HI32 /
;;;; IEEE-FLOAT-LO32 in terms of sb-kernel:double-float-*, which does not exist
;;;; in-image.  Every build script that bakes compiler.lisp into a Modus image
;;;; therefore appends this file's source AFTER the compiler source so these
;;;; definitions win (last-defun-wins).  They only matter for compiling FLOAT
;;;; and BIGNUM literals in-image.
;;;;
;;;; WHY THIS FILE EXISTS (#201): these two float accessors used to be
;;;; duplicated verbatim inside a *stage2-float-override* string literal in SIX
;;;; separate build scripts (build-generic, build-generic-cli, build-ansi-common,
;;;; build-modus-selfhost, build-x64-cl-repl, build-i386-cli).  They encode the
;;;; float object's SLOT LAYOUT.  Changing the layout while updating only five
;;;; of six copies produces an image that reads floats correctly *sometimes* —
;;;; silent numeric corruption, not a build error.  One definition, one place.
;;;;
;;;; This file is TEXT-INCLUDED into the image blob (read-file-text), not loaded
;;;; on the host.  Keep it to plain defuns with no host-only dependencies.

;;; FLOAT OBJECT LAYOUT (#201, width-neutral): subtag #x60 (double; #x64/#x65/
;;; #x66 for single/short/long), FOUR slots, each holding one 16-bit chunk of
;;; the IEEE-754 double:
;;;
;;;   slot 0 = bits 63..48    slot 1 = bits 47..32
;;;   slot 2 = bits 31..16    slot 3 = bits 15..0
;;;
;;; Each chunk is 0..65535, so the stored MACHINE WORD (chunk << 1, the fixnum
;;; tag) is 0..131070: always positive, always low-bit-0 — the conservative
;;; collector reads it as a fixnum and never follows the float's bit pattern as
;;; a pointer (the documented RIP=0xDEAD1004 class).  Unlike the old 2 x 32-bit
;;; layout, `chunk << 1` fits a 32-bit machine word, so the SAME layout works on
;;; i386 as on x64/aarch64.  See docs/i386-float-blocker.md.
;;;
;;; %PRIM-AREF returns the slot's LOGICAL value (the fixnum), i.e. the chunk
;;; itself — the << 1 is the machine-level tag, not part of the value.
;;;
;;; NEVER combine hi and lo into one 64-bit integer in-image: for floats >= 2.0
;;; the hi half >= #x40000000, so (ash hi 32) >= 2^62 and Modus's bignum-range
;;; ASH is lossy, corrupting the literal's bits at compile time (2.0/9.0/-1.5
;;; all read back as garbage).  Each half stays <= #xFFFFFFFF, safely in fixnum
;;; range.  IEEE-FLOAT-BITS is kept only for host/debug callers.
(defun ieee-float-hi32 (f)
  (logior (ash (%prim-aref f 0) 16) (%prim-aref f 1)))
(defun ieee-float-lo32 (f)
  (logior (ash (%prim-aref f 2) 16) (%prim-aref f 3)))
(defun ieee-float-bits (f)
  (logior (ash (ieee-float-hi32 f) 32) (ieee-float-lo32 f)))
;; Bignum-literal decomposition: read the already-built bignum object's slots
;; directly.  The host recompute path uses (logand value mask62), but the
;; compiled `logand` primop is a raw machine AND of tagged words — for a bignum
;; operand (a heap pointer) that yields garbage (e.g. (logand 2^62 (1- 2^62))
;; returned 66281634333124 instead of 0).  These read the decomposed limbs the
;; host already stored.
;;
;; THAT IS RIGHT ONLY WHEN THE TARGET'S LIMBS ARE THIS IMAGE'S LIMBS (W32 M1).
;; A self-hosted x64 compiler building for a 30-bit target (modus-sh
;; --compile-riscv32) sees literals that are bignums THERE but fixnums HERE --
;; +FIXNUM-LIMIT+ = 2^30 is one; reading "slot 0" of the fixnum 2^30 was
;; `The value 0 is not of type (INTEGER 0 -1)' and the init thunk silently
;; vanished from the image -- and literals that are bignums here have 62-bit
;; limbs where the target wants 30-bit ones.  So when the widths differ, the
;; literal is decomposed into the TARGET's limbs from this image's own limbs
;; with fixnum-only arithmetic: every field read is at most 31 bits wide, taken
;; from 62-bit limbs, so nothing here can leave the fixnum range.  When the
;; widths agree, the slot readers below run exactly as before.
(defun %lit-same-width-p () (= (ct-limb-bits) +limb-bits+))

(defun %lit-mag-limbs (value)
  "VALUE's magnitude as a vector of THIS image's limbs (+LIMB-BITS+ wide,
   LSB first), and its sign (1 / -1) as a second value."
  (cond
    ((fixnump value)
     (let ((v (make-array 1)))
       (setf (aref v 0) (if (< value 0) (- 0 value) value))
       (values v (if (< value 0) -1 1))))
    ((big-bignum-p value)
     (let* ((n (%bb-nlimbs value)) (v (make-array n)))
       (dotimes (i n) (setf (aref v i) (%bb-limb value i)))
       (values v (%bb-sign value))))
    (t
     ;; a small (2-slot) bignum: value = lo + hi * 2^B, two's complement
     (let ((lo (bignum-lo value)) (hi (bignum-hi value)) (v (make-array 2)))
       (if (>= hi 0)
           (progn (setf (aref v 0) lo) (setf (aref v 1) hi) (values v 1))
           (progn
             ;; -(lo + hi*2^B) = (2^B - lo) + (-hi - 1)*2^B, or (-hi)*2^B if lo = 0
             (if (= lo 0)
                 (progn (setf (aref v 0) 0) (setf (aref v 1) (- 0 hi)))
                 (progn (setf (aref v 0) (+ (- +fixnum-max+ lo) 1))
                        (setf (aref v 1) (- (- 0 hi) 1))))
             (values v -1)))))))

(defun %lit-mag-length (limbs)
  "INTEGER-LENGTH of the magnitude held in LIMBS."
  (let ((top (- (length limbs) 1)))
    (loop (when (or (< top 0) (not (= (aref limbs top) 0))) (return nil))
          (setq top (- top 1)))
    (if (< top 0) 0 (+ (* top +limb-bits+) (integer-length (aref limbs top))))))

(defun %lit-mag-field (limbs start width)
  "Bits [START, START+WIDTH) of the magnitude in LIMBS; WIDTH <= 31."
  (let* ((i (floor start +limb-bits+))
         (o (- start (* i +limb-bits+)))
         (n (length limbs))
         (mask (- (ash 1 width) 1)))
    (if (>= i n)
        0
        (let ((low (ash (aref limbs i) (- 0 o)))
              (have (- +limb-bits+ o)))
          (if (or (<= width have) (>= (+ i 1) n))
              (logand low mask)
              (let ((need (- width have)))
                (logand (logior low (ash (logand (aref limbs (+ i 1)) (- (ash 1 need) 1)) have))
                        mask)))))))

(defun %lit-bignum-big-p (value)
  (if (%lit-same-width-p)
      (big-bignum-p value)
      (> (%lit-mag-length (%lit-mag-limbs value)) (ct-small-bignum-bits))))
;; A target SMALL bignum holds fewer than 2 x target-limb bits, so on a host
;; whose limbs are wider it is a FIXNUM here, and plain arithmetic is exact.
(defun %lit-bn-lo (value)
  (if (%lit-same-width-p)
      (bignum-lo value)
      (if (fixnump value)
          (logand value (ct-fixnum-max))
          (error "cross-width literal: ~A is no small bignum on the target" value))))
(defun %lit-bn-hi (value)
  (if (%lit-same-width-p)
      (bignum-hi value)
      (if (fixnump value)
          (ash value (ct-neg-limb-bits))
          (error "cross-width literal: ~A is no small bignum on the target" value))))
(defun %lit-bb-sign (value)
  (if (%lit-same-width-p)
      (%bb-sign value)
      (multiple-value-bind (limbs sign) (%lit-mag-limbs value) limbs sign)))
(defun %lit-bb-nlimbs (value)
  (if (%lit-same-width-p)
      (%bb-nlimbs value)
      (let ((len (%lit-mag-length (%lit-mag-limbs value))) (b (ct-limb-bits)))
        (floor (+ len (- b 1)) b))))
(defun %lit-bb-limb (value k)
  (if (%lit-same-width-p)
      (%bb-limb value k)
      (%lit-mag-field (%lit-mag-limbs value) (* k (ct-limb-bits)) (ct-limb-bits))))
