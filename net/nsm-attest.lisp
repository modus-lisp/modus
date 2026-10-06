;;;; net/nsm-attest.lisp -- AWS Nitro Enclaves attestation, hosted x86-64.
;;;;
;;;; The Nitro Security Module is /dev/nsm, driven by ONE ioctl: NSM_IOCTL =
;;;; _IOWR(0x0A, 0, struct NsmMessage) = #xC0200A00, where NsmMessage is two
;;;; iovecs {request ptr, len; response ptr, len} (32 bytes).  Requests and
;;;; responses are CBOR (lib/cbor.lisp).  An attestation request is
;;;;   {"Attestation": {"user_data": bytes|null, "nonce": bytes|null, "public_key": bytes|null}}
;;;; (each at most 1024 bytes) and the answer is {"Attestation": {"document": bytes}}
;;;; or {"Error": "..."}.  The document is a COSE_Sign1 over a CBOR map:
;;;; module_id, digest ("SHA384"), timestamp, pcrs {index: 48 bytes}, certificate
;;;; (the NSM's leaf, DER), cabundle (DER chain to the AWS root), and the three
;;;; request fields echoed.  test/nitro/verify-attestation.py checks all of it
;;;; off-box; this side only asks and parses.
;;;;
;;;; Binding, same as the SNP side (net/snp-ssh-attest.lisp): user_data is the
;;;; SHA-512 of the thing being bound (a host key), nonce comes from the verifier.

(defconstant +nsm-ioctl+ #xC0200A00)
(defconstant +nsm-page-bytes+ 16384)       ; request + response: documents are ~4-5 KB

(defvar *nsm-page* 0 "One mapping: request CBOR at +0, NsmMessage at +4096, response at +8192.")
(defvar *nsm-last-status* nil)

(defun %nsm-page ()
  (when (zerop *nsm-page*)
    (setq *nsm-page* (%mmap-shared-page +nsm-page-bytes+)))
  *nsm-page*)
(defun %nsm-cstr (s)
  "Write S as a NUL-terminated C string at *cstr-scratch*; return its address."
  (let ((a *cstr-scratch*))
    (dotimes (i (length s)) (setf (mem-ref (+ a i) :u8) (char-code (char s i))))
    (setf (mem-ref (+ a (length s)) :u8) 0)
    a))
;; SYSCALLS TAKE THEIR ADDRESSES AS ARGUMENTS, never read a global in the same
;; function: a SYSCALL3 whose function also reads a global returns the syscall
;; NUMBER instead of the result from runtime-compiled code (see build-cli-common
;; and the sb-sys shim's %SBS-* floor).  (%nsm-open) came back 2 = open's number
;; here, and the ioctl then went to stdout.
(defun %nsm-open-at (path-addr) (syscall3 2 path-addr 2 0))   ; open(O_RDWR)
(defun %nsm-ioctl (fd msg) (syscall3 16 fd +nsm-ioctl+ msg))
(defun %nsm-close (fd) (syscall3 3 fd 0 0))
(defun %nsm-open () (%nsm-open-at (%nsm-cstr "/dev/nsm")))

(defun %nsm-bytes-in (addr v)  (dotimes (i (length v)) (setf (mem-ref (+ addr i) :u8) (aref v i))))
(defun %nsm-bytes-out (addr n) (let ((v (make-array n :element-type '(unsigned-byte 8)))) (dotimes (i n v) (setf (aref v i) (mem-ref (+ addr i) :u8)))))

(defun nsm-request (req)
  "Send the CBOR REQ (a Lisp value for cbor-encode) to the NSM; -> the decoded
   response, or NIL with *NSM-LAST-STATUS* = (:no-nsm errno) / (:ioctl errno)."
  (let ((fd (%nsm-open)))
    (if (< fd 0)
        (progn (setq *nsm-last-status* (list :no-nsm fd)) nil)
        (let* ((pg (%nsm-page)) (bytes (cbor-encode req)) (msg (+ pg 4096)) (resp (+ pg 8192)))
          (%nsm-bytes-in pg bytes)
          (%gc-write64 msg pg)              (%gc-write64 (+ msg 8) (length bytes))
          (%gc-write64 (+ msg 16) resp)     (%gc-write64 (+ msg 24) (- +nsm-page-bytes+ 8192))
          (let ((r (%nsm-ioctl fd msg)))
            (%nsm-close fd)
            (if (< r 0)
                (progn (setq *nsm-last-status* (list :ioctl r)) nil)
                ;; the driver writes the response length back into the second iovec
                (let ((n (%gc-read64 (+ msg 24))))
                  (setq *nsm-last-status* (list :ok n))
                  (cbor-decode (%nsm-bytes-out resp n)))))))))

(defun nsm-describe ()
  "{\"DescribeNSM\"} -> (:map (version_major . N) (version_minor . N) (module_id . \"...\") ...) or NIL."
  (let ((r (nsm-request "DescribeNSM")))
    (and r (cbor-map-get r "DescribeNSM"))))

(defun nsm-attestation-document (user-data nonce &optional public-key)
  "-> the COSE_Sign1 attestation document as a byte vector, or NIL (see *NSM-LAST-STATUS*).
   USER-DATA / NONCE / PUBLIC-KEY are byte vectors or NIL, each at most 1024 bytes."
  (let ((r (nsm-request (list :map (cons "Attestation"
                                         (list :map (cons "user_data" (or user-data :null))
                                                    (cons "nonce" (or nonce :null))
                                                    (cons "public_key" (or public-key :null))))))))
    (cond ((null r) nil)
          ((cbor-map-get r "Error") (setq *nsm-last-status* (list :nsm-error (cbor-map-get r "Error"))) nil)
          (t (cbor-map-get (cbor-map-get r "Attestation") "document")))))

;;; ---- reading a document (the verifier does the cryptography; this is for
;;; printing what we were handed, and for tests on a document signed by a
;;; stand-in NSM) ----
(defun nsm-document-payload (doc)
  "DOC = COSE_Sign1 bytes (with or without tag 18) -> the decoded payload map."
  (let ((c (cbor-decode doc)))
    (when (and (consp c) (eq (car c) :tag)) (setq c (cddr c)))   ; 18(COSE_Sign1)
    (cbor-decode (third c))))                                    ; [protected, unprotected, payload, signature]
(defun nsm-document-pcr (payload i) (cbor-map-get (cbor-map-get payload "pcrs") i))
(defun nsm-print-document (doc)
  (let ((p (nsm-document-payload doc)))
    (format t "NSM-MODULE ~A~%NSM-DIGEST ~A~%NSM-TIMESTAMP ~A~%" (cbor-map-get p "module_id") (cbor-map-get p "digest") (cbor-map-get p "timestamp"))
    (dolist (i '(0 1 2 3 4 8)) (let ((v (nsm-document-pcr p i))) (when v (format t "NSM-PCR~D ~A~%" i (cbor-hex v)))))
    (let ((u (cbor-map-get p "user_data")) (n (cbor-map-get p "nonce")))
      (format t "NSM-USER-DATA ~A~%NSM-NONCE ~A~%" (if (vectorp u) (cbor-hex u) u) (if (vectorp n) (cbor-hex n) n)))
    p))

(defun nsm-attest-selftest ()
  "On a machine without /dev/nsm the expected answer is (:NO-NSM -2)."
  (let ((d (nsm-attestation-document (make-array 4 :element-type '(unsigned-byte 8) :initial-element 7) nil)))
    (format t "NSM: document ~A status ~A~%" (if d (length d) nil) *nsm-last-status*)
    (when d (nsm-print-document d))
    (if d :document *nsm-last-status*)))

;;; ---- the enclave console: a REPL over vsock -------------------------------
;;; An enclave's only network is vsock to its parent, and its console is one
;;; line per form.  The parent reaches it as CID <enclave> port PORT (nitro-cli
;;; prints the CID; socat or vsock-proxy forwards a TCP port to it).  One client
;;; at a time; a closed connection goes back to accepting; every reply line is
;;; `= VALUE' or `! CONDITION', so a driver can match it.  Lines are LF-ended.
(defun %vsock-reply (fd s)
  (let* ((n (length s)) (buf (make-array (+ n 1) :element-type '(unsigned-byte 8))))
    (dotimes (i n) (setf (aref buf i) (logand (char-code (char s i)) 255)))
    (setf (aref buf n) 10)
    (socket-send fd buf (+ n 1))))
(defun %vsock-eval-line (line)
  (handler-case (let ((v (eval (read-from-string line))))
                  (concatenate 'string "= " (handler-case (prin1-to-string v) (error (c) "<unprintable>"))))
    (error (c) (concatenate 'string "! " (handler-case (prin1-to-string c) (error (c2) "<condition>"))))))
(defun %vsock-serve-client (fd)
  (let ((buf (make-array 4096 :element-type '(unsigned-byte 8))) (line (make-string 0)))
    (loop
      (let ((n (socket-recv fd buf 4096)))
        (when (<= n 0) (return))
        (dotimes (i n)
          (let ((b (aref buf i)))
            (cond ((= b 10)
                   (when (> (length line) 0) (%vsock-reply fd (%vsock-eval-line line)))
                   (setq line (make-string 0)))
                  ((= b 13) nil)
                  (t (setq line (concatenate 'string line (string (code-char b))))))))))))
(defun vsock-repl (port)
  "Listen on vsock PORT (any CID) and serve forms, one per line, forever.
   Returns only if the listen fails (-1)."
  (let ((lfd (vsock-listen port 1)))
    (if (< lfd 0)
        (progn (format t "VSOCK-REPL: listen failed~%") -1)
        (progn
          (format t "VSOCK-REPL: port ~D~%" port) (finish-output)
          (loop
            (let ((fd (socket-accept lfd)))
              (when (>= fd 0)
                (%vsock-reply fd "modus vsock console; one form per line")
                (handler-case (%vsock-serve-client fd) (error (c) nil))
                (socket-close fd))))))))
