;;;; test/retry-compile-state.lisp -- a top-level form the compiler retries
;;;; (register promotion ran out of temporaries) still defines NAMED functions.
;;;;
;;;;   ./modus --script test/retry-compile-state.lisp     -> RETRY PASS / FAIL
;;;;
;;;; MVM-COMPILE-TOPLEVEL compiles a form with register promotion on and, if
;;;; that runs out of temporaries, compiles it again with promotion off.  The
;;;; retry used to inherit what the failed attempt left behind: *%DEFUN-DEFERRED*
;;;; still T from the IF branch the error unwound out of, so every DEFUN in the
;;;; form became an anonymous (SET-SYMBOL-FUNCTION 'NAME (LAMBDA ...)), and the
;;;; attempt's functions still on *PENDING-FLET-IR*, so a PROGN's went into the
;;;; module twice.  The JIT then had no function under the DEFUN's name to
;;;; publish, and it stayed interpreted: natrium's SHA-512 on aarch64 (the FLOOR
;;;; below is %SHA512-OUT's), 183 ms per RANDOM-BYTES against 0.45 ms on x64.
;;;; The shapes run aarch64 out of temporaries; x64 has more, so there the test
;;;; is a plain check that they go native.  On aarch64 before the fix RCS-A and
;;;; RCS-B stayed interpreted.

(defvar *rcs-fail* 0)
(defun rcs-check (what got want)
  (format t "~&  ~a ~a: ~s (want ~s)~%" (if (equal got want) "ok  " "FAIL") what got want)
  (unless (equal got want) (incf *rcs-fail*)))

;; One DEFUN alone (it was compiled as two anonymous lambdas).
(eval '(defun rcs-out (hv out start nbytes)
         (dotimes (b nbytes)
           (let ((half (aref hv (floor b 4))) (sh (* -8 (- 3 (mod b 4)))))
             (setf (aref out (+ start b)) (logand #xff (ash half sh)))))))
;; In a PROGN with a macro and DEFUNs before it (they went in twice).
(eval '(progn
         (defmacro rcs-hi (h n) `(ash ,h ,(- n)))
         (defun rcs-a (x) (rcs-hi x 3))
         (defun rcs-b (x) (+ (rcs-a x) 1))
         (defun rcs-out2 (hv out start nbytes)
           (dotimes (b nbytes)
             (let ((half (aref hv (floor b 4))) (sh (* -8 (- 3 (mod b 4)))))
               (setf (aref out (+ start b)) (logand #xff (ash half sh))))))))

(jit-eager)
(dolist (n '("COMMON-LISP-USER::RCS-OUT" "COMMON-LISP-USER::RCS-A"
             "COMMON-LISP-USER::RCS-B" "COMMON-LISP-USER::RCS-OUT2"))
  (rcs-check (format nil "~a native after JIT-EAGER" n) (modus-int::%jit-fn-native-p n) t))

;; And they still compute the right thing.
(let ((hv (vector #x01020304 #x05060708)) (out (make-array 8 :initial-element 0)))
  (rcs-out hv out 0 8)
  (rcs-check "RCS-OUT bytes" (coerce out 'list) '(1 2 3 4 5 6 7 8))
  (fill out 0)
  (rcs-out2 hv out 0 8)
  (rcs-check "RCS-OUT2 bytes" (coerce out 'list) '(1 2 3 4 5 6 7 8))
  (rcs-check "RCS-B" (rcs-b 80) 11))

(format t "~&RETRY ~a~%" (if (zerop *rcs-fail*) "PASS" "FAIL"))
