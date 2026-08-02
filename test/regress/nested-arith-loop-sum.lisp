;;;; Regression: MVM compiler refuses `(op ... (loop ... sum (op <call> <call>)))`
;;;; (task #205).  compiler.lisp CHECK-ARITH-NESTING signals rather than
;;;; miscompile once *arith-push-depth* + operand depth reaches 2 with a call
;;;; inside; the LOOP ... SUM expansion supplies the enclosing push.
;;;;
;;;; This is ordinary portable CL.  SBCL prints 31; Modus currently refuses to
;;;; compile HDR-BITS at all, so the call reports UNDEFINED-FUNCTION.
;;;;
;;;; Found by loading the modus-lisp `cram` DEFLATE library unmodified, where
;;;; the same shape appears in EMIT-BLOCK (deflate.lisp) and BR-NEED
;;;; (inflate.lisp).  Distilled here so the repro carries no dependency.
;;;;
;;;; PASS = "[B] hdr-bits = 31".

(defvar *cl-lengths* (make-array 20 :initial-element 3))

(defun extra (tk) (case (car tk) (16 2) (17 3) (18 7) (t 0)))

(defun hdr-bits (rle hclen)
  (+ 3 5 5 4 (* 3 hclen)
     (loop for tk in rle sum (+ (aref *cl-lengths* (car tk))
                                (extra tk)))))

(format t "~&[A] compiled~%")
(format t "[B] hdr-bits = ~a (expect 31)~%" (hdr-bits (list (list 16) (list 17)) 1))
(format t "[D] done~%")
