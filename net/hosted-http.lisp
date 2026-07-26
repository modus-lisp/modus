;;;; hosted-http.lisp — a minimal HTTP/1.0 client on the hosted socket layer.
;;;;
;;;; The gateway from "modus has sockets" to "modus fetches from the network":
;;;; URL -> DNS resolve (or dotted-IP) -> TCP connect -> GET -> read to close.
;;;; Plain HTTP only (TLS is a later rung — the org's `seal`).  Built entirely
;;;; on net/hosted-sockets.lisp (socket-connect/send/recv/close + dns-lookup).

;;; --- host string -> host-order IPv4 int (dotted literal or DNS A lookup) ---
(defun %host-string-to-ip (host)
  (let ((n (length host)) (dots 0) (alldigit t) (i 0))
    (loop
      (when (>= i n) (return nil))
      (let ((c (char-code (char host i))))
        (cond ((= c 46) (setq dots (+ dots 1)))
              ((and (>= c 48) (<= c 57)) nil)
              (t (setq alldigit nil))))
      (setq i (+ i 1)))
    (if (and alldigit (= dots 3))
        ;; dotted-decimal literal a.b.c.d -> a<<24|b<<16|c<<8|d
        (let ((res 0) (octet 0) (j 0))
          (loop
            (when (>= j n) (return (logior (ash res 8) octet)))
            (let ((c (char-code (char host j))))
              (if (= c 46)
                  (progn (setq res (logior (ash res 8) octet)) (setq octet 0))
                  (setq octet (+ (* octet 10) (- c 48)))))
            (setq j (+ j 1))))
        ;; hostname -> A record via connected-UDP DNS at 8.8.8.8
        (let ((name (make-array n)) (k 0))
          (loop
            (when (>= k n) (return nil))
            (aset name k (char-code (char host k)))
            (setq k (+ k 1)))
          (dns-lookup name n (%default-resolver) nil)))))

;;; --- write a Lisp STRING's char codes into byte array BUF at OFF; return
;;;     the new offset ---
(defun %http-put-str (buf off str)
  (let ((n (length str)) (i 0) (p off))
    (loop
      (when (>= i n) (return p))
      (aset buf p (char-code (char str i)))
      (setq p (+ p 1)) (setq i (+ i 1)))))

;;; Build "GET <path> HTTP/1.0\r\nHost: <host>\r\nConnection: close\r\n\r\n"
;;; into BUF; return length.  The Host header is mandatory for name-based
;;; virtual hosts / CDNs (CloudFront, S3, quicklisp).
(defun %http-build-request (host path buf)
  (let ((p 0))
    (setq p (%http-put-str buf p "GET "))
    (setq p (%http-put-str buf p path))
    (setq p (%http-put-str buf p " HTTP/1.0"))
    (aset buf p 13) (aset buf (+ p 1) 10) (setq p (+ p 2))     ; CRLF
    (setq p (%http-put-str buf p "Host: "))
    (setq p (%http-put-str buf p host))
    (aset buf p 13) (aset buf (+ p 1) 10) (setq p (+ p 2))     ; CRLF
    (setq p (%http-put-str buf p "Connection: close"))
    (aset buf p 13) (aset buf (+ p 1) 10) (setq p (+ p 2))     ; CRLF
    (aset buf p 13) (aset buf (+ p 1) 10) (setq p (+ p 2))     ; blank line
    p))

;;; Core fetch: HTTP/1.0 GET http://HOST:PORT/PATH.  HOST is a string (name
;;; or dotted-IP).  Fills OUTBUF (an (unsigned-byte 8) array) up to OUTMAX
;;; bytes with the RAW response (status line + headers + body).  Returns the
;;; total byte count, or negative: -1 DNS/parse fail, -2 connect fail.
(defun http-fetch (host port path outbuf outmax)
  (let ((ip (%host-string-to-ip host)))
    (if (= ip 0)
        -1
        (let ((fd (socket-connect ip port t)))          ; TCP
          (if (< fd 0)
              -2
              (progn
                (let ((req (make-array 1024)))
                  (let ((rlen (%http-build-request host path req)))
                    (socket-send fd req rlen)))
                (let ((total 0) (chunk (make-array 4096)))
                  (loop
                    (let ((n (socket-recv fd chunk 4096)))
                      (when (< n 1) (return nil))        ; 0 = peer closed (Connection: close)
                      (let ((i 0))
                        (loop
                          (when (or (>= i n) (>= total outmax)) (return nil))
                          (aset outbuf total (aref chunk i))
                          (setq total (+ total 1))
                          (setq i (+ i 1))))))
                  (socket-close fd)
                  total)))))))

;;; Find the body offset (first byte after the CRLFCRLF header terminator),
;;; or 0 if not found.
(defun %http-body-offset (arr len)
  (let ((i 0) (found 0))
    (loop
      (when (>= (+ i 3) len) (return found))
      (when (and (= (aref arr i) 13) (= (aref arr (+ i 1)) 10)
                 (= (aref arr (+ i 2)) 13) (= (aref arr (+ i 3)) 10))
        (setq found (+ i 4))
        (return found))
      (setq i (+ i 1)))))

;;; Convert LEN bytes of ARR (from OFF) into a Lisp string.
(defun %http-bytes-to-string (arr off len)
  (let ((s (make-array (- len off))) (i off) (j 0))
    (loop
      (when (>= i len) (return (coerce-to-simple-string s j)))
      (aset s j (code-char (aref arr i)))
      (setq i (+ i 1)) (setq j (+ j 1)))))

;;; Small helper: modus strings are char arrays; return a length-J prefix.
(defun coerce-to-simple-string (arr j)
  (let ((s (make-array j)) (i 0))
    (loop (when (>= i j) (return s)) (aset s i (aref arr i)) (setq i (+ i 1)))))

;;; --- URL-parsing convenience -------------------------------------------
;;; Parse "http://host[:port]/path" -> fetch, return the RESPONSE BODY as a
;;; string (headers stripped).  DEFAULT port 80, default path "/".
(defun http-get (url)
  (let ((n (length url)) (start 0))
    ;; strip leading "http://"
    (when (and (>= n 7)
               (char= (char url 0) #\h) (char= (char url 4) #\:)
               (char= (char url 5) #\/) (char= (char url 6) #\/))
      (setq start 7))
    ;; find host end (first ':' or '/'), port, path
    (let ((i start) (host-end n) (path-start n) (port 80))
      (loop
        (when (>= i n) (return nil))
        (let ((c (char url i)))
          (cond ((char= c #\:) (setq host-end i)
                                (let ((pe (+ i 1)) (pv 0))
                                  (loop
                                    (when (or (>= pe n) (char= (char url pe) #\/)) (return nil))
                                    (setq pv (+ (* pv 10) (- (char-code (char url pe)) 48)))
                                    (setq pe (+ pe 1)))
                                  (setq port pv) (setq path-start pe))
                                (return nil))
                ((char= c #\/) (setq host-end i) (setq path-start i) (return nil))))
        (setq i (+ i 1)))
      (when (= host-end n) (setq host-end n))
      (let ((host (subseq url start host-end))
            (path (if (>= path-start n) "/" (subseq url path-start n)))
            (outbuf (make-array 262144)))                ; 256 KiB response cap
        (let ((total (http-fetch host port path outbuf 262144)))
          (if (< total 0)
              (if (= total -1) "ERR: DNS/host" "ERR: connect")
              (let ((boff (%http-body-offset outbuf total)))
                (%http-bytes-to-string outbuf boff total))))))))

;;; Return the raw status line of URL (for a quick liveness probe).
(defun http-status (url)
  (let ((outbuf (make-array 4096)))
    (let ((total (http-fetch-url-raw url outbuf 4096)))
      (if (< total 0)
          "ERR"
          (let ((i 0) (e 0))
            (loop
              (when (or (>= i total) (= (aref outbuf i) 13)) (setq e i) (return nil))
              (setq i (+ i 1)))
            (%http-bytes-to-string outbuf 0 e))))))

(defun http-fetch-url-raw (url outbuf outmax)
  ;; same URL parse as http-get but returns raw (headers+body) into outbuf
  (let ((n (length url)) (start 0))
    (when (and (>= n 7) (char= (char url 0) #\h) (char= (char url 4) #\:)
               (char= (char url 5) #\/) (char= (char url 6) #\/))
      (setq start 7))
    (let ((i start) (host-end n) (path-start n) (port 80))
      (loop
        (when (>= i n) (return nil))
        (let ((c (char url i)))
          (cond ((char= c #\:) (setq host-end i)
                                (let ((pe (+ i 1)) (pv 0))
                                  (loop (when (or (>= pe n) (char= (char url pe) #\/)) (return nil))
                                    (setq pv (+ (* pv 10) (- (char-code (char url pe)) 48)))
                                    (setq pe (+ pe 1)))
                                  (setq port pv) (setq path-start pe))
                                (return nil))
                ((char= c #\/) (setq host-end i) (setq path-start i) (return nil))))
        (setq i (+ i 1)))
      (let ((host (subseq url start host-end))
            (path (if (>= path-start n) "/" (subseq url path-start n))))
        (http-fetch host port path outbuf outmax)))))
