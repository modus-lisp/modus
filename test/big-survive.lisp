(let ((lst nil))
  (dotimes (i 50000) (setq lst (cons i lst)))   ; ~800KB live list >> 256KB segment
  (dotimes (i 50000) (cons i i))                ; churn
  (let ((sum 0) (cnt 0) (p lst))
    (loop (when (null p) (return)) (setq sum (+ sum (car p))) (setq cnt (+ cnt 1)) (setq p (cdr p)))
    (write-string-serial "COUNT=")(print-dec cnt)(terpri)
    (write-string-serial "OK=")(print-dec (if (and (= cnt 50000)(= sum 1249975000)) 1 0))(terpri)))
