;;;; verify-chain-test.lisp -- the Lisp chain verifier on the real VPSBG report, plus two
;;;; negatives.  Run from the repo root:
;;;;   ./modus --load test/snp/verify-report.lisp --load test/snp/snp-verify.lisp \
;;;;           --load test/snp/verify-chain-test.lisp --quit
(defparameter *vc-dir* "test/snp/records/vpsbg-snpguest/")
(defun vc-run ()
  (let* ((rep (%read-file-bytes "test/snp/records/2026-10-08-vpsbg-epyc7713p-report.bin"))
         (ark (%read-file-bytes (concatenate 'string *vc-dir* "ark.der")))
         (ask (%read-file-bytes (concatenate 'string *vc-dir* "ask.der")))
         (vcek (%read-file-bytes (concatenate 'string *vc-dir* "vcek.der")))
         (good (snp-verify-report rep ark ask vcek)))
    (format t "REAL REPORT: ~A~%" (if good "PASS" "FAIL"))
    (let ((bad (copy-seq rep)))
      (setf (aref bad #x90) (logxor (aref bad #x90) 1))
      (format t "--- negative: one bit flipped in the measurement~%")
      (format t "NEGATIVE (measurement bit): ~A~%" (if (snp-verify-report bad ark ask vcek) "ACCEPTED (BAD)" "rejected (good)")))
    (format t "--- negative: VCEK replaced by the ASK certificate~%")
    (format t "NEGATIVE (wrong signer): ~A~%" (if (snp-verify-report rep ark ask ask) "ACCEPTED (BAD)" "rejected (good)"))
    good))

;; The EC2 report: signed by a VLEK, whose chain is ARK -> ASVK -> VLEK (the same profile).
(defun vc-run-ec2 ()
  (let* ((d "test/snp/records/")
         (rep (%read-file-bytes (concatenate 'string d "2026-10-08-ec2-c6a-report.bin")))
         (ark (%read-file-bytes (concatenate 'string d "2026-10-08-ec2-c6a-ark.der")))
         (asvk (%read-file-bytes (concatenate 'string d "2026-10-08-ec2-c6a-asvk.der")))
         (vlek (%read-file-bytes (concatenate 'string d "2026-10-08-ec2-c6a-vlek.der")))
         (good (snp-verify-report rep ark asvk vlek)))
    (format t "EC2 VLEK REPORT: ~A~%" (if good "PASS" "FAIL"))
    good))
