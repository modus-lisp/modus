(let ((keep (cons 777 888)))
  (let ((addr0 (%mcgc-obj-raw-addr keep)))
    (modus-int::write-string-serial "addr0=") (modus-int::print-dec addr0) (terpri)
    (dotimes (i 60000) (cons i i))         ; force several page collections
    (let ((addr1 (%mcgc-obj-raw-addr keep)))
      (modus-int::write-string-serial "addr1=") (modus-int::print-dec addr1)
      (modus-int::write-string-serial " STABLE=") (modus-int::print-dec (if (= addr0 addr1) 1 0))
      (modus-int::write-string-serial " INTACT=") (modus-int::print-dec (if (and (= (car keep) 777) (= (cdr keep) 888)) 1 0))
      (terpri))))
