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

(defun ieee-float-bits (f)
  (logior (ash (logand (%prim-aref f 0) 4294967295) 32)
          (logand (%prim-aref f 1) 4294967295)))
;; Read hi/lo 32-bit halves directly from the boxed float's slots.  NEVER
;; combine into a 64-bit integer: for floats >= 2.0 the hi half >= #x40000000,
;; so (ash hi 32) >= 2^62 and Modus's bignum-range ASH is lossy, corrupting
;; the literal's bits at compile time (2.0/9.0/-1.5 all read back as garbage).
;; These two stay <= #xFFFFFFFF, safely in fixnum range.
(defun ieee-float-hi32 (f) (logand (%prim-aref f 0) 4294967295))
(defun ieee-float-lo32 (f) (logand (%prim-aref f 1) 4294967295))
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
