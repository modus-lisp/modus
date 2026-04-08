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
;;; 0x40-0x4F: Structured objects
(defconstant +subtag-struct+ #x40)
(defconstant +subtag-hash-table+ #x41)
;;; 0x50-0x5F: Callable/symbol objects
(defconstant +subtag-symbol+ #x50)
(defconstant +subtag-function+ #x51)
(defconstant +subtag-closure+ #x52)
;;; 0x60-0x6F: MVM objects
(defconstant +subtag-mvm-bytecode+ #x60)
(defconstant +subtag-mvm-module+ #x61)

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
