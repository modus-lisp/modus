(let ((keep (cons 777 888)))
  (let ((addr0 (%mcgc-obj-raw-addr keep)))
    (write-string-serial "addr0=") (print-dec addr0) (terpri)
    (dotimes (i 60000) (cons i i))         ; force several page collections
    (let ((addr1 (%mcgc-obj-raw-addr keep)))
      (write-string-serial "addr1=") (print-dec addr1)
      (write-string-serial " STABLE=") (print-dec (if (= addr0 addr1) 1 0))
      (write-string-serial " INTACT=") (print-dec (if (and (= (car keep) 777) (= (cdr keep) 888)) 1 0))
      (terpri))))
