;;;; verify-report.lisp -- the parts of SNP report verification that need no
;;;; asymmetric crypto, run in Modus.  Usage (hosted CLI):
;;;;   ./modus --script test/snp/verify-report.lisp REPORT.bin HOSTKEY.hex
;;;; Prints the report's policy, TCB, VMPL, signer, measurement and whether
;;;; REPORT_DATA equals SHA-512 of the host public key.  NOT verified here:
;;;; the VCEK/VLEK signature and the ARK/ASK chain (P-384 ECDSA and RSA-PSS;
;;;; neither is in the tree yet).

(defun %read-file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let* ((n (file-length s)) (v (make-array n :element-type '(unsigned-byte 8))))
      (read-sequence v s) v)))

(defun %u32 (v o) (+ (aref v o) (* 256 (aref v (+ o 1))) (* 65536 (aref v (+ o 2))) (* 16777216 (aref v (+ o 3)))))
(defun %hex (v from n)
  (with-output-to-string (s)
    (dotimes (i n) (format s "~(~2,'0x~)" (aref v (+ from i))))))
(defun %hex-to-bytes (h)
  (let ((v (make-array (floor (length h) 2) :element-type '(unsigned-byte 8))))
    (dotimes (i (length v) v)
      (setf (aref v i) (parse-integer h :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))
(defun %same-bytes (a b)
  (and (= (length a) (length b))
       (let ((ok t)) (dotimes (i (length a) ok) (unless (= (aref a i) (aref b i)) (setq ok nil))))))

(defun snp-report-check (rep hostkey)
  "REP: the 1184-byte report.  HOSTKEY: the 32 raw Ed25519 public key bytes.
   Returns T if REPORT_DATA == SHA-512(hostkey)."
  (format t "version ~D  policy ~X  vmpl ~D  signed_by ~A~%"
          (%u32 rep 0) (%u32 rep 8) (%u32 rep #x30)
          (case (logand (ash (%u32 rep #x48) -2) 7) (0 "VCEK") (1 "VLEK") (t "?")))
  (format t "reported_tcb ~A~%" (%hex rep #x180 8))
  (format t "measurement ~A~%" (%hex rep #x90 48))
  (let* ((rd (subseq rep #x50 (+ #x50 64)))
         (want (progn (sha512-init) (sha512 hostkey)))
         (ok (%same-bytes rd want)))
    (format t "report_data == SHA-512(hostkey): ~A~%" ok)
    ok))

;; STATUS 2026-10-08: the report parse is right (version, policy, VMPL, signer and
;; measurement agree with Python on a real VPSBG report).  SHA-256/SHA-512 in
;; net/crypto.lisp read their round constants K from (e1000-state-base)+#x100/#x200,
;; which only SHA256-INIT / SHA512-INIT write ("Call sha512-init first!").  Without
;; that call K is all zero and the digest is SHA-with-K=0 (the 'wrong digests' once
;; recorded here: sha512("abc") => cbc97649...).  Hence the SHA512-INIT above.
;; Driver (run with --load, then --eval):
;;   ./modus --load test/snp/verify-report.lisp \
;;     --eval '(snp-report-check (%read-file-bytes "REPORT.bin") (%hex-to-bytes "HOSTKEY_HEX"))'
