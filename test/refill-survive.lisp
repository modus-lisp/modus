;;;; refill-survive.lisp — correctness test for the to-run REFILL / segment chain.
;;;;
;;;; Build flag-ON (MODUS_MCGC_PINNING=1) with MODUS_GC_R14=262144 so collections
;;;; fire often, and MODUS_MCGC_TORUN_CAP=<small> so each to-run segment is tiny
;;;; and the live set spans MANY segments — directly exercising copy_object's
;;;; refill (pop another free run when R13 hits to_end) and the segmented Cheney
;;;; scan that follows the chain.
;;;;
;;;; Holds a 2000-cons live list across heavy churn, then verifies EVERY element
;;;; survived intact (exact length + exact sum of cars).  Any mishandled survivor
;;;; across a segment boundary truncates the list or corrupts a value -> fails.
(let ((lst nil))
  (dotimes (i 2000) (setq lst (cons i lst)))   ; live root: 2000 conses (i . rest)
  (dotimes (i 120000) (cons i i))              ; churn -> many collections + refills
  (let ((sum 0) (cnt 0) (p lst))
    (loop
      (when (null p) (return))
      (setq sum (+ sum (car p)))
      (setq cnt (+ cnt 1))
      (setq p (cdr p)))
    (write-string-serial "COUNT=") (print-dec cnt) (terpri)
    (write-string-serial "SUM=")   (print-dec sum) (terpri)
    ;; 0+1+...+1999 = 1999000
    (write-string-serial "OK=")
    (print-dec (if (and (= cnt 2000) (= sum 1999000)) 1 0)) (terpri)))
