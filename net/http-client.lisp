;;;; http-client.lisp - HTTP/1.0 client for bare-metal Modus
;;;;
;;;; Outbound HTTP GET requests with URL parsing and DNS resolution.
;;;; Uses the single-connection TCP client (e1000-state-base+0x30..0x44).
;;;;
;;;; In actor builds, http-fetch is overridden in actors-net-overrides.lisp
;;;; to route through the net-domain actor (which owns e1000-receive).

;;; ============================================================
;;; URL parsing
;;; ============================================================

;; Check for "http://" prefix, return offset past it (7) or 0
(defun url-skip-http (arr len)
  (if (< len 7) 0
      ;; h=104 t=116 t=116 p=112 :=58 /=47 /=47
      (if (eq (aref arr 0) 104)
          (if (eq (aref arr 4) 58)
              (if (eq (aref arr 6) 47) 7 0)
              0)
          0)))

;; Find end of host portion: first ':', '/' or end after start
(defun url-host-end (arr start len)
  (let ((pos start) (found 0))
    (loop
      (when (>= pos len) (return pos))
      (when (not (zerop found)) (return found))
      (let ((b (aref arr pos)))
        (when (eq b 58) (setq found pos))   ;; ':'
        (when (eq b 47) (setq found pos)))  ;; '/'
      (setq pos (+ pos 1)))
    (if (zerop found) pos found)))

;; Find port in URL: scan past ':' for digits, default 80
(defun url-parse-port (arr host-end len)
  (if (>= host-end len) 80
      (if (eq (aref arr host-end) 58)  ;; ':'
          (let ((pos (+ host-end 1)) (port 0))
            (loop
              (when (>= pos len) (return port))
              (let ((b (aref arr pos)))
                (when (eq b 47) (return port))  ;; '/'
                (when (< b 48) (return port))
                (when (> b 57) (return port))
                (let ((d (- b 48)))
                  (setq port (+ (* port 10) d))))
              (setq pos (+ pos 1)))
            (if (zerop port) 80 port))
          80)))

;; Find start of path in URL (first '/' after host), or len
(defun url-path-off (arr start len)
  (let ((pos start) (found 0))
    (loop
      (when (>= pos len) (return len))
      (when (not (zerop found)) (return found))
      (when (eq (aref arr pos) 47) (setq found pos))  ;; '/'
      (setq pos (+ pos 1)))
    (if (zerop found) len found)))

;; Parse dotted-decimal IP address from arr[start..end), return IP or 0
(defun parse-ip-addr (arr start end)
  (let ((octet 0) (result 0) (pos start) (valid 1) (dots 0))
    (loop
      (when (>= pos end) (return 0))
      (when (zerop valid) (return 0))
      (let ((b (aref arr pos)))
        (if (eq b 46)  ;; dot
            (progn
              (setq result (logior (ash result 8) octet))
              (setq octet 0)
              (setq dots (+ dots 1)))
            (if (>= b 48)
                (if (<= b 57)
                    (let ((d (- b 48)))
                      (setq octet (+ (* octet 10) d)))
                    (setq valid 0))
                (setq valid 0))))
      (setq pos (+ pos 1)))
    (if (zerop valid) 0
        (if (eq dots 3)
            (logior (ash result 8) octet)
            0))))

;; Check if host portion is all digits/dots (IP literal)
(defun host-is-ip (arr start end)
  (let ((pos start) (ok 1))
    (loop
      (when (>= pos end) (return ok))
      (when (zerop ok) (return 0))
      (let ((b (aref arr pos)))
        (when (not (eq b 46))
          (when (< b 48) (setq ok 0))
          (when (> b 57) (setq ok 0))))
      (setq pos (+ pos 1)))
    ok))

;; Resolve hostname: IP literal → parse, otherwise DNS
(defun resolve-host (url host-start host-end)
  (let ((host-len (- host-end host-start)))
    (if (not (zerop (host-is-ip url host-start host-end)))
        (parse-ip-addr url host-start host-end)
        ;; Copy host to array for dns-resolve
        (let ((name (make-array host-len)))
          (let ((i 0))
            (loop
              (when (>= i host-len) (return 0))
              (aset name i (aref url (+ host-start i)))
              (setq i (+ i 1))))
          (dns-resolve name host-len)))))

;;; ============================================================
;;; HTTP GET request builder
;;; ============================================================

;; Build "GET <path> HTTP/1.0\r\nHost: <host>\r\nConnection: close\r\n\r\n"
;; into buf. Returns total length.
(defun http-build-get (url h-off h-len p-off p-len buf)
  (let ((p 0))
    ;; "GET "
    (aset buf 0 71) (aset buf 1 69) (aset buf 2 84) (aset buf 3 32)
    (setq p 4)
    ;; Path
    (if (zerop p-len)
        (progn (aset buf p 47) (setq p (+ p 1)))  ;; "/"
        (let ((i 0))
          (loop
            (when (>= i p-len) (return 0))
            (aset buf p (aref url (+ p-off i)))
            (setq p (+ p 1))
            (setq i (+ i 1)))))
    ;; " HTTP/1.0\r\n"  (32 72 84 84 80 47 49 46 48 13 10)
    (aset buf p 32) (aset buf (+ p 1) 72) (aset buf (+ p 2) 84)
    (aset buf (+ p 3) 84) (aset buf (+ p 4) 80) (aset buf (+ p 5) 47)
    (aset buf (+ p 6) 49) (aset buf (+ p 7) 46) (aset buf (+ p 8) 48)
    (aset buf (+ p 9) 13) (aset buf (+ p 10) 10)
    (setq p (+ p 11))
    ;; "Host: "  (72 111 115 116 58 32)
    (aset buf p 72) (aset buf (+ p 1) 111) (aset buf (+ p 2) 115)
    (aset buf (+ p 3) 116) (aset buf (+ p 4) 58) (aset buf (+ p 5) 32)
    (setq p (+ p 6))
    ;; Host name
    (let ((i 0))
      (loop
        (when (>= i h-len) (return 0))
        (aset buf p (aref url (+ h-off i)))
        (setq p (+ p 1))
        (setq i (+ i 1))))
    ;; "\r\nConnection: close\r\n\r\n"
    ;; \r\n = 13 10
    ;; Connection: close = 67 111 110 110 101 99 116 105 111 110 58 32 99 108 111 115 101
    ;; \r\n\r\n = 13 10 13 10
    (aset buf p 13) (aset buf (+ p 1) 10)
    (setq p (+ p 2))
    (aset buf p 67)  (aset buf (+ p 1) 111) (aset buf (+ p 2) 110)
    (aset buf (+ p 3) 110) (aset buf (+ p 4) 101) (aset buf (+ p 5) 99)
    (aset buf (+ p 6) 116) (aset buf (+ p 7) 105) (aset buf (+ p 8) 111)
    (aset buf (+ p 9) 110) (aset buf (+ p 10) 58) (aset buf (+ p 11) 32)
    (aset buf (+ p 12) 99) (aset buf (+ p 13) 108) (aset buf (+ p 14) 111)
    (aset buf (+ p 15) 115) (aset buf (+ p 16) 101)
    (setq p (+ p 17))
    (aset buf p 13) (aset buf (+ p 1) 10)
    (aset buf (+ p 2) 13) (aset buf (+ p 3) 10)
    (+ p 4)))

;;; ============================================================
;;; TCP client helpers
;;; ============================================================

;; Copy TCP payload from RX buffer into dest array at dest-off.
;; Call immediately after tcp-receive returns > 0.
;; Returns number of bytes copied.
(defun tcp-rx-copy (dest dest-off)
  (let ((buf (e1000-rx-buf)))
    (let ((ip-total (buf-read-u16-mem buf 16))
          (tcp-hdr-len (ash (logand (mem-ref (+ buf 46) :u8) #xF0) -2)))
      (let ((data-len (- ip-total (+ 20 tcp-hdr-len))))
        (let ((data-base (+ (+ buf 34) tcp-hdr-len)))
          (let ((i 0))
            (loop
              (when (>= i data-len) (return data-len))
              (let ((src (+ data-base i))
                    (dst-idx (+ dest-off i)))
                (when (< dst-idx 4096)
                  (aset dest dst-idx (mem-ref src :u8))))
              (setq i (+ i 1))))
          data-len)))))

;; Global outbound TCP connection state
(defun tcp-state ()
  (mem-ref (+ (e1000-state-base) #x30) :u32))

;;; ============================================================
;;; HTTP fetch implementation
;;; ============================================================

;; Find \r\n\r\n in response buffer, return offset past it (body start).
;; Returns 0 if not found.
(defun http-find-body (arr len)
  (let ((pos 0) (found 0))
    (loop
      (when (not (zerop found)) (return found))
      (let ((remain (- len pos)))
        (when (< remain 4) (return 0)))
      (let ((b (aref arr pos)))
        (when (eq b 13)
          (let ((b1 (aref arr (+ pos 1))))
            (when (eq b1 10)
              (let ((b2 (aref arr (+ pos 2))))
                (when (eq b2 13)
                  (let ((b3 (aref arr (+ pos 3))))
                    (when (eq b3 10)
                      (setq found (+ pos 4))))))))))
      (setq pos (+ pos 1)))
    found))

;; Core fetch: parse URL, resolve, connect, GET, receive, return response.
;; Returns (cons response-array response-length) or 0 on failure.
(defun http-fetch-impl (url url-len)
  ;; Parse URL
  (let ((scheme-end (url-skip-http url url-len)))
    (let ((host-end (url-host-end url scheme-end url-len)))
      (let ((port (url-parse-port url host-end url-len))
            (path-start (url-path-off url scheme-end url-len)))
        ;; host-len: distance from scheme-end to first : or /
        (let ((host-len (- host-end scheme-end))
              (path-len (- url-len path-start)))
          ;; Resolve hostname
          (let ((ip (resolve-host url scheme-end host-end)))
            (when (zerop ip)
              ;; "DNS:0\n"
              (%serial-byte 68) (%serial-byte 78) (%serial-byte 83)
              (%serial-byte 58) (%serial-byte 48) (%serial-byte 10)
              (return 0))
            ;; Connect
            (when (zerop (tcp-connect ip port))
              ;; "TCP:F\n"
              (%serial-byte 84) (%serial-byte 67) (%serial-byte 80)
              (%serial-byte 58) (%serial-byte 70) (%serial-byte 10)
              (return 0))
            ;; Build and send GET request
            (let ((req-buf (make-array 512)))
              (let ((req-len (http-build-get url scheme-end host-len
                                              path-start path-len req-buf)))
                (tcp-send req-buf req-len)))
            ;; Receive response
            (let ((resp (make-array 4096))
                  (resp-len 0)
                  (done 0)
                  (idle 0))
              (loop
                (when (not (zerop done)) (return 0))
                (let ((n (tcp-receive 300)))
                  (if (> n 0)
                      (progn
                        (let ((copied (tcp-rx-copy resp resp-len)))
                          (setq resp-len (+ resp-len copied)))
                        (setq idle 0))
                      (setq idle (+ idle 1))))
                (when (zerop (tcp-state)) (setq done 1))
                (when (> idle 5) (setq done 1)))
              ;; Close
              (tcp-close)
              (cons resp resp-len))))))))

;;; ============================================================
;;; Response printing and default http-fetch
;;; ============================================================

;; Print response body (after headers) via %serial-byte
(defun http-print-result (result)
  (if (zerop result)
      (progn
        ;; "ERR\n"
        (%serial-byte 69) (%serial-byte 82) (%serial-byte 82) (%serial-byte 10)
        0)
      (let ((resp (car result))
            (resp-len (cdr result)))
        (let ((body-off (http-find-body resp resp-len)))
          (when (zerop body-off) (setq body-off 0))
          (let ((i body-off))
            (loop
              (when (>= i resp-len) (return 0))
              (%serial-byte (aref resp i))
              (setq i (+ i 1)))))
        (%serial-byte 10)
        resp-len)))

;; Default http-fetch: fetch and print. Overridden in actor mode.
(defun http-fetch (url url-len)
  (http-print-result (http-fetch-impl url url-len)))

;;; ============================================================
;;; Convenience functions
;;; ============================================================

;; fetch-test: GET from gateway (no DNS needed).
;; Pi → 10.0.0.1, QEMU → 10.0.2.2
(defun fetch-test ()
  (let ((url (make-array 17)))
    ;; "http://"
    (aset url 0 104) (aset url 1 116) (aset url 2 116) (aset url 3 112)
    (aset url 4 58) (aset url 5 47) (aset url 6 47)
    ;; Detect platform from e1000-state-base address
    (if (< (e1000-state-base) #x02000000)
        ;; Pi: "10.0.0.1/"
        (progn
          (aset url 7 49) (aset url 8 48) (aset url 9 46) (aset url 10 48)
          (aset url 11 46) (aset url 12 48) (aset url 13 46) (aset url 14 49)
          (aset url 15 47)
          (http-fetch url 16))
        ;; QEMU: "10.0.2.2/"
        (progn
          (aset url 7 49) (aset url 8 48) (aset url 9 46) (aset url 10 48)
          (aset url 11 46) (aset url 12 50) (aset url 13 46) (aset url 14 50)
          (aset url 15 47)
          (http-fetch url 16)))))

;; fetch: GET http://example.com/ (tests DNS + real internet)
(defun fetch ()
  (let ((url (make-array 20)))
    ;; "http://example.com/"
    (aset url 0 104) (aset url 1 116) (aset url 2 116) (aset url 3 112)
    (aset url 4 58) (aset url 5 47) (aset url 6 47)
    ;; example.com/
    (aset url 7 101) (aset url 8 120) (aset url 9 97) (aset url 10 109)
    (aset url 11 112) (aset url 12 108) (aset url 13 101) (aset url 14 46)
    (aset url 15 99) (aset url 16 111) (aset url 17 109) (aset url 18 47)
    (http-fetch url 19)))
