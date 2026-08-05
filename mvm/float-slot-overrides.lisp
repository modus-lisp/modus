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
(defun %lit-bignum-big-p (value) (big-bignum-p value))
(defun %lit-bn-lo (value) (bignum-lo value))
(defun %lit-bn-hi (value) (bignum-hi value))
(defun %lit-bb-sign (value) (%bb-sign value))
(defun %lit-bb-nlimbs (value) (%bb-nlimbs value))
(defun %lit-bb-limb (value k) (%bb-limb value k))
