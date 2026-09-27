;;;; SEV-SNP attestation, platform half (x86-64 UEFI CL image, SNP mode).
;;;;
;;;; The boot stub (boot/boot-uefi-snp.lisp) keeps a status block at 0x1A000
;;;; (+snp-status-base+ there; one page, below the #VC handler's, outside every
;;;; image):
;;;;   +0x00 u32  1 if SNP is active (C-bit found, shared region + GHCB set up)
;;;;   +0x28 u64  GPA of the SNP secrets page (from the EFI CC-blob table), or 0
;;;;   +0x30 u64  a TAGGED function pointer to the VMGEXIT routine (addr | 3)
;;;;   +0x38 u64  GPA of the GHCB
;;;;   +0x40 u64  base of the 2 MB shared (C=0) region
;;;; The request and response pages are the two 4 KB pages just below the GHCB
;;;; in that region.  Everything else -- the message framing, AES-256-GCM, the
;;;; report layout -- is crypto/snp-guest.lisp, which has no memory in it.
;;;;
;;;; The sequence counter lives where Linux keeps it, os_area.msg_seqno_0 in
;;;; the secrets page (0xA0): request = counter+1, and counter += 2 after the
;;;; exchange, so a guest that later boots Linux from this state still agrees
;;;; with the PSP.

(defconstant +snp-w-base+    #x1A000)   ; = boot-uefi-snp.lisp's +snp-status-base+
(defconstant +snp-w-active+  #x1A000)
(defconstant +snp-w-secrets+ #x1A028)
(defconstant +snp-w-vmgexit+ #x1A030)
(defconstant +snp-w-ghcb+    #x1A038)
(defconstant +snp-w-shared+  #x1A040)
(defconstant +snp-req-page-off+  #x1FD000)
(defconstant +snp-resp-page-off+ #x1FE000)
(defconstant +svm-vmgexit-guest-request+ #x80000011)

;; A :u64 mem-ref hands back the RAW word as a Lisp value (a GPA with bit 0
;; clear reads as the fixnum GPA/2), so addresses are read as two :u32 halves.
(defun %snp-u64v (a) (+ (mem-ref a :u32) (ash (mem-ref (+ a 4) :u32) 32)))
(defun snp-active-p () (= (mem-ref +snp-w-active+ :u32) 1))
(defun snp-secrets-gpa () (%snp-u64v +snp-w-secrets+))
(defun snp-ghcb-gpa () (%snp-u64v +snp-w-ghcb+))
(defun snp-shared-base () (%snp-u64v +snp-w-shared+))
(defun snp-req-page () (+ (snp-shared-base) +snp-req-page-off+))
(defun snp-resp-page () (+ (snp-shared-base) +snp-resp-page-off+))
(defun snp-vmgexit-word-p () (= (logand (mem-ref +snp-w-vmgexit+ :u32) 15) 3))
(defun snp-vmgexit ()
  "REP VMMCALL through the stub's routine.  The word IS a tagged fn pointer, so
   reading it raw and FUNCALLing it is the ordinary indirect call (sub 3; call).
   In :TEST mode the routine is `mov eax,NIL; ret' -- callable on any machine."
  (funcall (mem-ref +snp-w-vmgexit+ :u64)))

(defun %snp-read-bytes (addr n)
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n v) (setf (aref v i) (mem-ref (+ addr i) :u8)))))
(defun %snp-write-bytes (addr v)
  (dotimes (i (length v)) (setf (mem-ref (+ addr i) :u8) (aref v i))))
(defun %snp-zero (addr n) (dotimes (i n) (setf (mem-ref (+ addr i) :u8) 0)))

(defun snp-vmpck0 () (%snp-read-bytes (+ (snp-secrets-gpa) +snp-secrets-vmpck0-off+) 32))
(defun snp-seqno-counter () (mem-ref (+ (snp-secrets-gpa) +snp-secrets-seqno0-off+) :u32))
(defun snp-set-seqno-counter (n) (setf (mem-ref (+ (snp-secrets-gpa) +snp-secrets-seqno0-off+) :u32) n))

(defun %ghcb-put-u64 (g off val)
  (setf (mem-ref (+ g off) :u32) (logand val #xFFFFFFFF))
  (setf (mem-ref (+ g off 4) :u32) (logand (ash val -32) #xFFFFFFFF)))
(defun %ghcb-set-valid (g field)
  ;; valid bitmap at 0x3F0: bit (field/8) -- byte (field/8)/8, bit (field/8)%8
  (let* ((idx (floor field 8)) (a (+ g #x3F0 (floor idx 8))))
    (setf (mem-ref a :u8) (logior (mem-ref a :u8) (ash 1 (mod idx 8))))))

(defun snp-guest-request (req-gpa resp-gpa)
  "SNP_GUEST_REQUEST NAE: exit code 0x80000011, sw_exit_info_1 = request page
   GPA, sw_exit_info_2 = response page GPA; returns sw_exit_info_2 after the
   VMGEXIT (0 = the PSP processed the request; high 32 bits = VMM error)."
  (let ((g (snp-ghcb-gpa)))
    (%snp-zero (+ g #x3F0) 16)
    (%ghcb-put-u64 g #x390 +svm-vmgexit-guest-request+) (%ghcb-set-valid g #x390)
    (%ghcb-put-u64 g #x398 req-gpa) (%ghcb-set-valid g #x398)
    (%ghcb-put-u64 g #x3A0 resp-gpa) (%ghcb-set-valid g #x3A0)
    (setf (mem-ref (+ g #xFFC) :u32) 0)                 ; usage 0 = GHCB
    (setf (mem-ref (+ g #xFFA) :u8) 2) (setf (mem-ref (+ g #xFFB) :u8) 0)  ; protocol version 2
    (snp-vmgexit)
    (%snp-u64v (+ g #x3A0))))

(defvar *snp-last-status* nil
  "Why the last SNP-ATTESTATION-REPORT returned NIL: (:no-snp) (:no-secrets)
   (:vmm exit-info-2) (:psp status) (:open reason), or (:ok).")

(defun snp-attestation-report (report-data &optional (vmpl 0))
  "Ask the PSP for an ATTESTATION_REPORT binding the 64 bytes of REPORT-DATA.
   -> the 1184-byte report, or NIL with *SNP-LAST-STATUS* saying why."
  (cond
    ((not (snp-active-p)) (setq *snp-last-status* (list :no-snp)) nil)
    ((zerop (snp-secrets-gpa)) (setq *snp-last-status* (list :no-secrets)) nil)
    (t
     (let* ((vmpck (snp-vmpck0))
            (seqno (+ (snp-seqno-counter) 1))
            (req (snp-report-request-page seqno report-data vmpck vmpl)))
       (%snp-write-bytes (snp-req-page) req)
       (%snp-zero (snp-resp-page) +snp-page-size+)
       (let ((st (snp-guest-request (snp-req-page) (snp-resp-page))))
         (if (/= st 0)
             (progn (setq *snp-last-status* (list :vmm st)) nil)
             (multiple-value-bind (report status why)
                 (snp-report-open (%snp-read-bytes (snp-resp-page) +snp-page-size+) seqno vmpck)
               (snp-set-seqno-counter (+ seqno 1))
               (cond (report (setq *snp-last-status* (list :ok)) report)
                     (t (setq *snp-last-status* (list (if why :open :psp) (or why status))) nil)))))))))

(defun snp-attest-selftest ()
  "Print the stub's words, exercise the VMGEXIT call path (a no-op in :TEST
   mode) and try for a report.  On a plain machine the expected outcome is
   active 0, a callable routine, and status (:NO-SNP)."
  (format t "SNP: active ~D secrets #x~X ghcb #x~X shared #x~X vmgexit-word-tagged ~A~%"
          (mem-ref +snp-w-active+ :u32) (snp-secrets-gpa) (snp-ghcb-gpa) (snp-shared-base) (snp-vmgexit-word-p))
  (when (and (snp-vmgexit-word-p) (not (snp-active-p)))
    (snp-vmgexit) (format t "SNP: vmgexit routine called (test mode), returned~%"))
  (let ((r (snp-attestation-report (make-array 64 :element-type '(unsigned-byte 8) :initial-element 7))))
    (format t "SNP: report ~A status ~A~%" (if r (length r) nil) *snp-last-status*)
    (when r (format t "SNP: measurement ~A~%" (snp-hex (snp-report-measurement r))))
    (if r :report *snp-last-status*)))
