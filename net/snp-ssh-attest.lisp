;;;; SEV-SNP attestation bound to the SSH host key (x86-64 UEFI CL image,
;;;; SNP mode + SSH build).  The binding is
;;;;
;;;;     report_data = SHA-512( the 32 raw bytes of the Ed25519 host public key )
;;;;
;;;; so a client that has just completed an SSH handshake -- and therefore holds
;;;; the host public key the server proved possession of -- can ask the same
;;;; session for an ATTESTATION_REPORT and check three things off-box
;;;; (test/snp/verify-report.py): the report's signature chains to the PSP (or
;;;; to the stand-in key in tests), its MEASUREMENT is the DDC'd image, and its
;;;; report_data is the digest of THIS host key.  A man in the middle would have
;;;; to present a report whose report_data names its own key, which the PSP will
;;;; not sign for a guest that does not run this image.
;;;;
;;;; The host key lives at e1000-state-base +0x730 (public) / +0x710 (private),
;;;; which the x64 SSH address map keeps OUTSIDE the SNP shared region.

(defun snp-ssh-host-pubkey ()
  "The 32-byte Ed25519 host public key, or NIL before SSH-BOOT installed one."
  (when (= (mem-ref (+ (e1000-state-base) #x624) :u32) 1)
    (let ((k (make-array 32 :element-type '(unsigned-byte 8))))
      (dotimes (i 32 k) (setf (aref k i) (mem-ref (+ (e1000-state-base) #x730 i) :u8))))))

(defun snp-ssh-report-data ()
  "SHA-512 of the raw host public key as a 64-byte vector, or NIL."
  (let ((k (snp-ssh-host-pubkey)))
    (when k
      (let ((h (sha512 k)) (v (make-array 64 :element-type '(unsigned-byte 8))))
        (dotimes (i 64 v) (setf (aref v i) (aref h i)))))))

(defun %snp-print-hex-lines (tag v)
  "TAG then the bytes of V as hex, 64 bytes per line, so a client reading the
   SSH channel can reassemble the value however long it is."
  (let ((n (length v)) (i 0))
    (loop
      (when (>= i n) (return))
      (format t "~A " tag)
      (dotimes (j (min 64 (- n i))) (format t "~(~2,'0x~)" (aref v (+ i j))))
      (terpri)
      (setq i (+ i 64)))))

(defun snp-attest-ssh ()
  "Print the host key, its digest and -- when the platform answers -- the
   ATTESTATION_REPORT binding it, as hex lines: SNP-HOSTKEY, SNP-REPORT-DATA,
   SNP-REPORT (19 lines) or SNP-STATUS.  Returns :REPORT or the status."
  (let ((k (snp-ssh-host-pubkey)))
    (cond
      ((null k) (format t "SNP-STATUS (:NO-HOST-KEY)~%") :no-host-key)
      (t
       (let ((rd (snp-ssh-report-data)))
         (%snp-print-hex-lines "SNP-HOSTKEY" k)
         (%snp-print-hex-lines "SNP-REPORT-DATA" rd)
         (let ((r (snp-attestation-report rd)))
           (cond (r (%snp-print-hex-lines "SNP-REPORT" r) :report)
                 (t (format t "SNP-STATUS ~A~%" *snp-last-status*) *snp-last-status*))))))))
