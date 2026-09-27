;;; Regression: the interpreter's array allocation must ZERO-fill like the
;;; native arm.  %ALLOC-NATIVE filled #x32 vectors with NIL, so an interpreted
;;; (make-array n) / (make-array n :element-type '(signed-byte 32)) read NIL
;;; where the same bytecode JIT'd read 0 -- reel's coefficient blocks, a
;;; wrong picture whenever one decoder defun ran interpreted.  Run with the
;;; JIT on (first call is interpreted under *jit-hot-only*) and with
;;; MODUS_NO_JIT=1; the result must be identical.
(defvar *fails* 0)
(defmacro chk (name form)
  `(let ((v (handler-case ,form (error (c) (list :error (type-of c))))))
     (unless (eq v t) (setq *fails* (+ *fails* 1)) (format t "FAIL ~A => ~S~%" ,name v))))
(defun alloc-probe ()
  (list (aref (make-array 4) 0)
        (aref (make-array 4 :element-type '(signed-byte 32)) 3)
        (aref (make-array 4 :element-type 'fixnum) 1)
        (aref (make-array 4 :element-type '(unsigned-byte 8)) 2)
        (aref (make-array 4 :element-type '(signed-byte 16)) 0)
        (aref (make-array 300) 299)
        (let ((n 7)) (aref (make-array n) 6))))
(chk "interpreted-first-call" (equal (alloc-probe) '(0 0 0 0 0 0 0)))
(chk "second-call" (equal (alloc-probe) '(0 0 0 0 0 0 0)))
(chk "toplevel" (equal (list (aref (make-array 3) 0) (aref (make-array 3 :element-type '(signed-byte 32)) 2)) '(0 0)))
(chk "explicit-nil-kept" (null (aref (make-array 3 :initial-element nil) 1)))
(format t "~&interp-alloc-zero: ~A~%" (if (zerop *fails*) "PASS" (format nil "~D FAIL" *fails*)))
(sys-exit (if (zerop *fails*) 0 1))
