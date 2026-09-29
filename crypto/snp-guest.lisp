;;;; SEV-SNP guest message protocol (GHCB spec ch. "SNP guest request", the
;;;; format Linux's drivers/virt/coco/sev-guest speaks): the 96-byte message
;;;; header, AES-256-GCM under a VMPCK with the header tail as AAD, and the
;;;; MSG_REPORT_REQ / MSG_REPORT_RSP payloads that carry an attestation report.
;;;;
;;;; Portable CL over (unsigned-byte 8) vectors -- no memory, no GHCB, no
;;;; syscalls -- so it runs and is tested on the host (SBCL) and in-image alike.
;;;; Needs crypto/aes.lisp + crypto/gcm.lisp.  The platform half (secrets page,
;;;; shared pages, VMGEXIT) is net/snp-attest.lisp.
;;;;
;;;; Header (96 bytes, all little-endian):
;;;;   0x00 authtag[32]   GCM tag in the first 16 bytes
;;;;   0x20 msg_seqno u64 0x28 rsvd[8]
;;;;   0x30 algo u8 (1 = AES-256-GCM)  0x31 hdr_version u8 (1)  0x32 hdr_sz u16 (96)
;;;;   0x34 msg_type u8   0x35 msg_version u8 (1)  0x36 msg_sz u16
;;;;   0x38 rsvd u32      0x3C msg_vmpck u8  0x3D rsvd[35]
;;;; AAD = bytes 0x30..0x5F (48).  IV = msg_seqno as u64 LE, then 4 zero bytes.
;;;; Payload follows the header at 0x60, encrypted in place.

(defconstant +snp-msg-hdr-size+ 96)
(defconstant +snp-msg-aad-off+ #x30)
(defconstant +snp-msg-aad-len+ 48)
(defconstant +snp-aead-aes-256-gcm+ 1)
(defconstant +snp-msg-hdr-version+ 1)
(defconstant +snp-msg-version+ 1)
(defconstant +snp-msg-report-req+ 5)
(defconstant +snp-msg-report-rsp+ 6)
(defconstant +snp-report-req-size+ 96)     ; user_data[64] vmpl u32 rsvd[28]
(defconstant +snp-report-size+ 1184)       ; ATTESTATION_REPORT
(defconstant +snp-report-rsp-hdr+ 32)      ; status u32, report_size u32, rsvd[24]
(defconstant +snp-secrets-vmpck0-off+ #x20)
(defconstant +snp-secrets-seqno0-off+ #xA0)
(defconstant +snp-page-size+ 4096)

(defun %snp-bytes (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))
(defun %snp-put-u16 (v off x) (setf (aref v off) (logand x 255) (aref v (+ off 1)) (logand (ash x -8) 255)) v)
(defun %snp-put-u32 (v off x) (dotimes (i 4 v) (setf (aref v (+ off i)) (logand (ash x (* -8 i)) 255))))
(defun %snp-put-u64 (v off x) (dotimes (i 8 v) (setf (aref v (+ off i)) (logand (ash x (* -8 i)) 255))))
(defun %snp-get-u16 (v off) (logior (aref v off) (ash (aref v (+ off 1)) 8)))
(defun %snp-get-u32 (v off) (let ((x 0)) (dotimes (i 4 x) (setq x (logior x (ash (aref v (+ off i)) (* 8 i)))))))
(defun %snp-get-u64 (v off) (let ((x 0)) (dotimes (i 8 x) (setq x (logior x (ash (aref v (+ off i)) (* 8 i)))))))
(defun %snp-subseq (v start len) (let ((r (%snp-bytes len))) (dotimes (i len r) (setf (aref r i) (aref v (+ start i))))))
(defun %snp-copy-into (dst off src) (dotimes (i (length src) dst) (setf (aref dst (+ off i)) (aref src i))))

(defun snp-msg-iv (seqno)
  "12-byte GCM nonce: the message sequence number as a little-endian u64, then zeros."
  (%snp-put-u64 (%snp-bytes 12) 0 seqno))

(defun snp-msg-build (seqno msg-type payload vmpck &optional (vmpck-id 0))
  "One 4096-byte guest-request page: header + payload encrypted under VMPCK.
   SEQNO must be non-zero (the PSP rejects 0)."
  (when (zerop seqno) (error "snp-msg-build: message sequence number must be non-zero"))
  (let ((page (%snp-bytes +snp-page-size+)))
    (%snp-put-u64 page #x20 seqno)
    (setf (aref page #x30) +snp-aead-aes-256-gcm+)
    (setf (aref page #x31) +snp-msg-hdr-version+)
    (%snp-put-u16 page #x32 +snp-msg-hdr-size+)
    (setf (aref page #x34) msg-type)
    (setf (aref page #x35) +snp-msg-version+)
    (%snp-put-u16 page #x36 (length payload))
    (setf (aref page #x3C) vmpck-id)
    (let* ((aad (%snp-subseq page +snp-msg-aad-off+ +snp-msg-aad-len+))
           (r (aes-gcm-encrypt vmpck (snp-msg-iv seqno) payload aad)))
      (%snp-copy-into page +snp-msg-hdr-size+ (car r))
      (%snp-copy-into page 0 (cdr r)))
    page))

(defun snp-msg-open (page expected-seqno expected-type vmpck)
  "Authenticate and decrypt a response page.  Returns the payload, or
   (values NIL reason-keyword) -- :seqno, :type, :size, :auth."
  (let ((seqno (%snp-get-u64 page #x20))
        (type (aref page #x34))
        (sz (%snp-get-u16 page #x36)))
    (cond
      ((/= seqno expected-seqno) (values nil :seqno))
      ((/= type expected-type) (values nil :type))
      ((> (+ +snp-msg-hdr-size+ sz) +snp-page-size+) (values nil :size))
      (t (let* ((aad (%snp-subseq page +snp-msg-aad-off+ +snp-msg-aad-len+))
                (ct (%snp-subseq page +snp-msg-hdr-size+ sz))
                (tag (%snp-subseq page 0 16))
                (pt (aes-gcm-decrypt vmpck (snp-msg-iv seqno) ct aad tag)))
           (if pt (values pt nil) (values nil :auth)))))))

(defun snp-report-request-payload (report-data &optional (vmpl 0))
  "MSG_REPORT_REQ payload: 64 bytes of caller data (bound into the report),
   the VMPL, and 28 zero bytes."
  (let ((p (%snp-bytes +snp-report-req-size+)))
    (dotimes (i (min 64 (length report-data))) (setf (aref p i) (aref report-data i)))
    (%snp-put-u32 p 64 vmpl)
    p))

(defun snp-report-request-page (seqno report-data vmpck &optional (vmpl 0))
  (snp-msg-build seqno +snp-msg-report-req+ (snp-report-request-payload report-data vmpl) vmpck))

(defun snp-report-response-parse (payload)
  "MSG_REPORT_RSP payload -> (values status report-bytes-or-nil)."
  (let ((status (%snp-get-u32 payload 0))
        (size (%snp-get-u32 payload 4)))
    (if (and (zerop status) (= size +snp-report-size+) (>= (length payload) (+ +snp-report-rsp-hdr+ size)))
        (values 0 (%snp-subseq payload +snp-report-rsp-hdr+ size))
        (values status nil))))

(defun snp-report-open (page req-seqno vmpck)
  "Open the response to the request sent with REQ-SEQNO (the PSP answers with
   seqno+1 and type REPORT_RSP).  -> (values report-bytes status reason)."
  (multiple-value-bind (payload why) (snp-msg-open page (+ req-seqno 1) +snp-msg-report-rsp+ vmpck)
    (if (null payload)
        (values nil nil why)
        (multiple-value-bind (status report) (snp-report-response-parse payload)
          (values report status nil)))))

;;; ATTESTATION_REPORT field offsets (SEV-SNP ABI spec, table "ATTESTATION_REPORT").
(defun snp-report-version (r) (%snp-get-u32 r #x00))
(defun snp-report-guest-svn (r) (%snp-get-u32 r #x04))
(defun snp-report-policy (r) (%snp-get-u64 r #x08))
(defun snp-report-vmpl (r) (%snp-get-u32 r #x30))
(defun snp-report-report-data (r) (%snp-subseq r #x50 64))
(defun snp-report-measurement (r) (%snp-subseq r #x90 48))
(defun snp-report-host-data (r) (%snp-subseq r #xC0 32))
(defun snp-report-chip-id (r) (%snp-subseq r #x1A0 64))
(defun snp-report-signature (r) (%snp-subseq r #x2A0 144))

(defun snp-hex (v)
  (with-output-to-string (s) (dotimes (i (length v)) (format s "~(~2,'0x~)" (aref v i)))))
