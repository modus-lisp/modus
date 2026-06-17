;;;; pin-stress.lisp — MCGC stage-4d pin verification (runtime-eval'd).
;;;;
;;;; Run via the GENERIC binary built with MODUS_MCGC_PINNING=1:
;;;;   MODUS_MCGC_PINNING=1 sbcl --script mvm/build-generic.lisp   # -> /tmp/modus
;;;;   /tmp/modus test/pin-stress.lisp
;;;;
;;;; Proves the core FFI guarantee: a %pin'd heap object's raw byte address is
;;;; STABLE across forced page collections, and its contents survive intact.
;;;; Lives in a standalone script (NOT compiled into the image) because a heavy
;;;; probe function perturbs the register allocator — see mcgc-pin.lisp.

(let ((v (make-array 5)))
  (setf (aref v 0) 100)
  (setf (aref v 1) 200)
  (setf (aref v 2) 300)
  (setf (aref v 3) 400)
  (setf (aref v 4) 500)
  ;; Pin v and capture its raw byte address BEFORE any collection.
  (let ((addr0 (%pin-object-addr v))
        (junk nil))
    ;; Churn the heap and force several explicit page collections.  Each
    ;; (%mcgc-collect) evacuates/reclaims; a correctly-pinned v must NOT move.
    (dotimes (round 8)
      (dotimes (j 80000) (setq junk (cons j junk)))
      (setq junk nil)
      (%mcgc-collect))
    (let ((addr1 (%mcgc-obj-raw-addr v)))
      (write-string-serial "PINSTRESS addr0=")
      (print-dec addr0)
      (write-string-serial " addr1=")
      (print-dec addr1)
      (write-string-serial " v0=")
      (print-dec (aref v 0))
      (write-string-serial " v4=")
      (print-dec (aref v 4))
      (if (and (= addr0 addr1)
               (= (aref v 0) 100)
               (= (aref v 4) 500))
          (write-string-serial " RESULT=PASS-STABLE-INTACT")
          (write-string-serial " RESULT=FAIL-MOVED-OR-CORRUPT"))
      (terpri))))
