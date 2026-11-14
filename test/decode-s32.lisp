;;;; decode-s32.lisp -- mvm/mvm.lisp's 32-bit operand decoders against the old
;;;; definitions, with every intermediate checked against the 30-bit tower.
;;;;
;;;; sbcl --script test/decode-s32.lisp            exit 0 = no failure
;;;;
;;;; The decoders are READ out of mvm/mvm.lisp (not copied) and evaluated in a
;;;; package whose + - ASH LOGIOR >= count every intermediate outside the
;;;; fixnum range of a 30-bit tower (|x| >= 2^29: a bignum on the 68k and every
;;;; 32-bit port).  Checked, for all 256 top bytes x 13 low-24-bit patterns:
;;;;  1. DECODE-S32 and DECODE-U32 give the old definitions' answers;
;;;;  2. DECODE-S32 makes NO bignum for a top byte of #x00-#x1F or #xE1-#xFF,
;;;;     i.e. any offset within +-(2^29 - 2^24).  (#xE0 takes the general path:
;;;;     (ash -32 24) is exactly -2^29, the edge of the range.)  The control: the
;;;;     same counter run over the old definition must see a bignum in EVERY case.

(defpackage :ds32 (:use :cl) (:shadow #:+ #:- #:ash #:logior #:>= #:<))
(in-package :ds32)
(defconstant +lim+ (expt 2 29))
(defvar *wide* 0)
(defun chk (x) (unless (cl:< (abs x) +lim+) (incf *wide*)) x)
(defun + (&rest a) (mapc #'chk a) (chk (apply #'cl:+ a)))
(defun - (&rest a) (mapc #'chk a) (chk (apply #'cl:- a)))
(defun ash (a n) (chk a) (chk (cl:ash a n)))
(defun logior (&rest a) (mapc #'chk a) (chk (apply #'cl:logior a)))
(defun >= (a b) (chk a) (chk b) (cl:>= a b))
(defun < (a b) (chk a) (chk b) (cl:< a b))

;; The old definitions, verbatim, under other names.
(defun old-u32 (bytes pos)
  (logior (aref bytes pos) (ash (aref bytes (cl:+ pos 1)) 8)
          (ash (aref bytes (cl:+ pos 2)) 16) (ash (aref bytes (cl:+ pos 3)) 24)))
(defun old-s32 (bytes pos)
  (let ((val (old-u32 bytes pos)))
    (if (>= val #x80000000) (- val #x100000000) val)))

;; The new ones, read from the source.
(let ((want '("%DECODE-LO24" "DECODE-U32" "DECODE-S32")) (got 0)
      (path (merge-pathnames "../mvm/mvm.lisp" (or *load-truename* *default-pathname-defaults*))))
  (with-open-file (in path)
    (let ((*package* (find-package :ds32)))
      (loop for form = (read in nil :eof) until (eq form :eof)
            when (and (consp form) (eq (car form) 'defun)
                      (member (symbol-name (second form)) want :test #'string=))
              do (eval form) (incf got))))
  (unless (= got 3) (format t "FAIL: found ~d of 3 decoders in mvm.lisp~%" got) (sb-ext:exit :code 1)))

(defvar *fail* 0)
(defun fail (fmt &rest a) (incf *fail*) (when (cl:< *fail* 20) (apply #'format t fmt a)))

(let ((los '(0 1 2 #x7F #x80 #xFF #x100 #xFFFF #x10000 #x7FFFFF #x800000 #xFFFFFE #xFFFFFF))
      (bytes (make-array 6 :element-type '(unsigned-byte 8)))
      (old-wide 0) (cases 0))
  (dotimes (b3 256)
    (dolist (lo los)
      (setf (aref bytes 1) (ldb (byte 8 0) lo) (aref bytes 2) (ldb (byte 8 8) lo)
            (aref bytes 3) (ldb (byte 8 16) lo) (aref bytes 4) b3)
      (incf cases)
      (let ((os (old-s32 bytes 1)) (ou (old-u32 bytes 1)))
        (let ((*wide* 0))
          (old-s32 bytes 1) (incf old-wide (if (cl:> *wide* 0) 1 0)))
        (let* ((*wide* 0) (ns (decode-s32 bytes 1)) (wide *wide*) (nu (decode-u32 bytes 1)))
          (unless (eql ns os) (fail "FAIL s32 b3=~2,'0x lo=~6,'0x: ~d, old ~d~%" b3 lo ns os))
          (unless (eql nu ou) (fail "FAIL u32 b3=~2,'0x lo=~6,'0x: ~d, old ~d~%" b3 lo nu ou))
          (when (and (or (cl:< b3 #x20) (cl:>= b3 #xE1)) (cl:> wide 0))
            (fail "FAIL s32 b3=~2,'0x lo=~6,'0x: result ~d, but ~d bignum intermediates~%"
                  b3 lo os wide))))))
  ;; control: the counter sees the old definition's bignums (every case has one).
  (unless (= old-wide cases)
    (fail "FAIL control: the old definition made bignums in ~d of ~d cases, expected all~%"
          old-wide cases))
  (format t "decode-s32: ~d cases, old definition wide in ~d, ~d failures~%" cases old-wide *fail*)
  (sb-ext:exit :code (if (zerop *fail*) 0 1)))
