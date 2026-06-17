;;;; pin-stress.lisp — MCGC stage-4 FFI pinning deliverable test.
;;;; Run via the GENERIC binary built with MODUS_MCGC_PINNING=1.
;;;; (Use MODUS_GC_R14=262144 to force frequent collections so the test is fast.)
;;;;
;;;; Proves the FFI guarantee: %pin-object pins a heap object in place — its raw
;;;; byte address stays STABLE across page collections (it is NOT evacuated),
;;;; contents survive, and %unpin-object releases it.  Collections are forced
;;;; here by heavy allocation (gc-check path), which is what real code triggers.
(let ((keep (cons 777 888)))
  (%pin-object keep)                                  ; persistent pin (pin-count++)
  (let ((addr0 (%mcgc-obj-raw-addr keep)))
    (write-string-serial "pincount=") (print-dec (%mcgc-page-pincount keep)) (terpri)
    (dotimes (i 60000) (cons i i))                    ; churn -> several page collections
    (let ((addr1 (%mcgc-obj-raw-addr keep)))
      (write-string-serial "PIN-STABLE=") (print-dec (if (= addr0 addr1) 1 0))
      (write-string-serial " INTACT=")    (print-dec (if (and (= (car keep) 777) (= (cdr keep) 888)) 1 0))
      (terpri)
      (%unpin-object keep)
      (write-string-serial "after-unpin-pincount=") (print-dec (%mcgc-page-pincount keep)) (terpri))))
