;;;; tags.lisp - Object Tagging for Modus
;;;;
;;;; Tagging scheme (4-bit):
;;;;   xxx0 = Fixnum (63-bit signed, low bit is 0)
;;;;   0001 = Cons pointer
;;;;   1001 = Object pointer (general heap object)
;;;;   0101 = Immediate (char, single-float)
;;;;   1111 = GC forwarding pointer
;;;;
;;;; This file defines tag constants and predicates.

(in-package :modus.runtime)

;;; ============================================================
;;; Tag Constants
;;; ============================================================

(defconstant +tag-mask+ #xF)         ; Low 4 bits
(defconstant +tag-fixnum+ #x0)       ; xxx0 (even)
(defconstant +tag-cons+ #x1)         ; 0001
(defconstant +tag-object+ #x9)       ; 1001
(defconstant +tag-immediate+ #x5)    ; 0101
(defconstant +tag-forward+ #xF)      ; 1111

;;; For fixnums, only low bit matters (must be 0)
(defconstant +fixnum-mask+ #x1)
(defconstant +fixnum-shift+ 1)       ; Fixnums shifted left 1

;;; Immediate subtypes (in upper bits of immediate)
(defconstant +imm-char+ 0)           ; Character
(defconstant +imm-float+ 1)          ; Single-float

;;; Object subtags (in header byte)
;;; 0x00-0x3F: Vector-like objects
(defconstant +subtag-simple-vector+ #x01)
(defconstant +subtag-string+ #x10)
(defconstant +subtag-u8-vector+ #x11)
(defconstant +subtag-u64-vector+ #x14)
(defconstant +subtag-sap+ #x16)
(defconstant +subtag-bignum+ #x30)
(defconstant +subtag-array+ #x32)
;;; 2-slot object holding a ratio: slot 0 = numerator, slot 1 = denominator.
;;; Replaces the older cons-based representation (cons num den) which
;;; collided with plain conses of two integers.
(defconstant +subtag-ratio+ #x33)
;;; 0x40-0x4F: Structured objects
(defconstant +subtag-struct+ #x40)
(defconstant +subtag-hash-table+ #x41)
;;; 0x50-0x5F: Callable/symbol objects
(defconstant +subtag-symbol+ #x50)
(defconstant +subtag-function+ #x51)
(defconstant +subtag-closure+ #x52)
;;; 0x60-0x6F: floats, then MVM objects
;;;
;;; NOTE ON AUTHORITY: this file is a REFERENCE document — it is in package
;;; :MODUS.RUNTIME and is not loaded by any build script.  The subtags the
;;; translators actually emit are the ones defined in `mvm/compiler.lisp`.
;;; Keep the two in sync by hand; when they disagree, compiler.lisp wins.
;;;
;;; #x60 is the DOUBLE-FLOAT object subtag, and has been since the numeric
;;; tower (N1).  THIS table had drifted and still listed #x60 as
;;; +subtag-mvm-bytecode+ — a name nothing in the tree allocates or reads.
;;; Recorded here so the next reader does not conclude #x60 is free.
;;;
;;; The float family deliberately SKIPS #x61..#x63.  #x61 is
;;; +subtag-mvm-module+, an object WITH pointer slots that the GC follows.
;;; A single-float parked there was scanned as an mvm-module, its raw IEEE
;;; bit-slots were followed as pointers, and the resulting corrupted funcall
;;; crashed with RIP=0xDEAD1004.  See CLAUDE.md.
;;; LAYOUT (#201): every float object — #x60 and #x64..#x66 alike — has FOUR
;;; slots, each holding one 16-bit chunk of the IEEE-754 double, stored TAGGED
;;; (chunk << 1):
;;;   slot 0 = bits 63..48   slot 1 = bits 47..32
;;;   slot 2 = bits 31..16   slot 3 = bits 15..0
;;; Each chunk is 0..65535, so the stored word is 0..131070: positive,
;;; low-bit-0 (the conservative collector reads it as a fixnum and never
;;; follows the float's bit pattern as a pointer) and — unlike the previous
;;; 2 x 32-bit layout, where hi32 << 1 overflowed a 32-bit word and lost the
;;; SIGN BIT — it fits a 32-bit machine word.  That is what makes the layout
;;; width-neutral across x64 / aarch64 / i386.  See docs/i386-float-blocker.md.
(defconstant +float-slots+ 4)             ; see compiler.lisp (authoritative)
(defconstant +subtag-float+ #x60)         ; double-float (the default float)
(defconstant +subtag-mvm-module+ #x61)    ; DO NOT hold numeric payloads here
(defconstant +subtag-single-float+ #x64)
(defconstant +subtag-short-float+ #x65)
(defconstant +subtag-long-float+ #x66)

;;; ============================================================
;;; Tag Extraction
;;; ============================================================

(defun tag-of (x)
  "Return the tag of object X"
  (logand x +tag-mask+))

(defun fixnump (x)
  "Is X a fixnum?"
  (zerop (logand x +fixnum-mask+)))

(defun consp (x)
  "Is X a cons cell?"
  (= (tag-of x) +tag-cons+))

(defun objectp (x)
  "Is X a general heap object?"
  (= (tag-of x) +tag-object+))

(defun immediatep (x)
  "Is X an immediate value?"
  (= (tag-of x) +tag-immediate+))

;;; ============================================================
;;; Pointer Operations
;;; ============================================================

(defun untag-cons (x)
  "Remove cons tag, return raw pointer"
  (- x +tag-cons+))

(defun untag-object (x)
  "Remove object tag, return raw pointer"
  (- x +tag-object+))

(defun tag-cons (ptr)
  "Add cons tag to pointer"
  (+ ptr +tag-cons+))

(defun tag-object (ptr)
  "Add object tag to pointer"
  (+ ptr +tag-object+))

;;; ============================================================
;;; Fixnum Operations
;;; ============================================================

(defun make-fixnum (n)
  "Tag integer N as fixnum (shift left 1)"
  (ash n +fixnum-shift+))

(defun fixnum-value (x)
  "Extract integer value from fixnum (shift right 1)"
  (ash x (- +fixnum-shift+)))

;;; ============================================================
;;; Character Operations
;;; ============================================================

(defun characterp (x)
  "Is X a character?"
  (and (immediatep x)
       (= (ldb (byte 8 8) x) +imm-char+)))

(defun make-char (code)
  "Create character immediate from char code"
  (logior +tag-immediate+
          (ash +imm-char+ 8)
          (ash code 16)))

(defun char-code (c)
  "Extract char code from character immediate"
  (ldb (byte 21 16) c))

;;; ============================================================
;;; Object Header
;;; ============================================================
;;;
;;; Header format (8 bytes):
;;;   [subtag:8][unused:8][element-count:48]

(defun object-subtag (obj)
  "Get subtag from object header"
  (let ((header (mem-ref (untag-object obj) :u64)))
    (ldb (byte 8 0) header)))

(defun object-element-count (obj)
  "Get element count from object header"
  (let ((header (mem-ref (untag-object obj) :u64)))
    (ldb (byte 48 16) header)))

(defun symbolp (x)
  "Is X a symbol?"
  (and (objectp x)
       (= (object-subtag x) +subtag-symbol+)))

(defun functionp (x)
  "Is X a function?"
  (and (objectp x)
       (let ((st (object-subtag x)))
         (or (= st +subtag-function+)
             (= st +subtag-closure+)))))

(defun vectorp (x)
  "Is X a vector?"
  (and (objectp x)
       (< (object-subtag x) #x40)))

(defun stringp (x)
  "Is X a string?"
  (and (objectp x)
       (= (object-subtag x) +subtag-string+)))

(defun closurep (x)
  "Is X a closure?"
  (and (objectp x)
       (= (object-subtag x) +subtag-closure+)))

(defun hash-table-p (x)
  "Is X a hash table?"
  (and (objectp x)
       (= (object-subtag x) +subtag-hash-table+)))

(defun structp (x)
  "Is X a struct?"
  (and (objectp x)
       (= (object-subtag x) +subtag-struct+)))
