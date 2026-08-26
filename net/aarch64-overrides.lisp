;;;; aarch64-overrides.lisp - Single-threaded overrides for AArch64 SSH
;;;;
;;;; Loaded AFTER ssh.lisp to override functions that need structural changes
;;;; for single-threaded operation (no actor model, no runtime compiler).

;;; ============================================================
;;; Line editor: handle-edit-byte
;;; ============================================================

;; Insert a byte into the line buffer at cursor position, shift tail right
(defun line-insert-byte (b)
  (let ((len (edit-line-len))
        (pos (edit-cursor-pos)))
    (when (< len 250)
      ;; Shift bytes from pos..len-1 right by one
      (let ((i len))
        (loop
          (when (<= i pos) (return 0))
          (setf (mem-ref (+ (+ (ssh-ipc-base) #x28) i) :u8)
                (mem-ref (+ (+ (ssh-ipc-base) #x28) (- i 1)) :u8))
          (setq i (- i 1))))
      ;; Insert byte at cursor
      (setf (mem-ref (+ (+ (ssh-ipc-base) #x28) pos) :u8) b)
      (edit-set-line-len (+ len 1))
      (edit-set-cursor-pos (+ pos 1))
      ;; Echo: print from cursor to end, then move cursor back
      (write-byte b)
      (let ((i (+ pos 1)))
        (loop
          (when (>= i (+ len 1)) (return 0))
          (write-byte (mem-ref (+ (+ (ssh-ipc-base) #x28) i) :u8))
          (setq i (+ i 1))))
      ;; Move cursor back to just after inserted char
      (let ((tail (- len pos)))
        (when (> tail 0)
          ;; ESC [ <n> D — cursor left
          (write-byte 27) (write-byte 91)
          (print-dec tail)
          (write-byte 68))))))

;; Delete byte before cursor (backspace)
(defun line-delete-back ()
  (let ((pos (edit-cursor-pos)))
    (when (> pos 0)
      (let ((len (edit-line-len))
            (new-pos (- pos 1)))
        ;; Shift bytes from pos..len-1 left by one
        (let ((i pos))
          (loop
            (when (>= i len) (return 0))
            (setf (mem-ref (+ (+ (ssh-ipc-base) #x28) (- i 1)) :u8)
                  (mem-ref (+ (+ (ssh-ipc-base) #x28) i) :u8))
            (setq i (+ i 1))))
        (edit-set-line-len (- len 1))
        (edit-set-cursor-pos new-pos)
        ;; Erase: move left, reprint tail, space over old last char, reposition
        (write-byte 8)  ; backspace
        (let ((i new-pos))
          (loop
            (when (>= i (- len 1)) (return 0))
            (write-byte (mem-ref (+ (+ (ssh-ipc-base) #x28) i) :u8))
            (setq i (+ i 1))))
        (write-byte 32)  ; space over old last char
        ;; Move cursor back
        (let ((back (- len new-pos)))
          (when (> back 0)
            (write-byte 27) (write-byte 91)
            (print-dec back)
            (write-byte 68)))))))

;; Process a single byte of input for line editing
;; Returns: 1 = enter pressed, 2 = ctrl-d, nil = continue
(defun handle-edit-byte (b)
  (let ((esc (mem-ref (+ (ssh-ipc-base) #x12810) :u64)))
    (setf (mem-ref (+ (ssh-ipc-base) #x12A08) :u64) 0)
    (if (eq esc 1)
        ;; Got ESC: '[' -> state 2, else reset
        (if (eq b 91)
            (setf (mem-ref (+ (ssh-ipc-base) #x12810) :u64) 2)
            (setf (mem-ref (+ (ssh-ipc-base) #x12810) :u64) 0))
        (if (eq esc 2)
            ;; Got ESC[: handle arrow keys or extended sequence
            (progn
              (setf (mem-ref (+ (ssh-ipc-base) #x12810) :u64) 0)
              (if (eq b 67)  ; right arrow
                  (when (< (edit-cursor-pos) (edit-line-len))
                    (edit-set-cursor-pos (+ (edit-cursor-pos) 1))
                    (write-byte 27) (write-byte 91) (write-byte 67))
                  (when (eq b 68)  ; left arrow
                    (when (> (edit-cursor-pos) 0)
                      (edit-set-cursor-pos (- (edit-cursor-pos) 1))
                      (write-byte 27) (write-byte 91) (write-byte 68)))))
            (if (> esc 2)
                ;; Absorb extended escape sequences
                (if (>= b 64)
                    (setf (mem-ref (+ (ssh-ipc-base) #x12810) :u64) 0)
                    (setf (mem-ref (+ (ssh-ipc-base) #x12810) :u64) (+ esc 1)))
                ;; Normal state (esc=0)
                (if (eq b 27)
                    (setf (mem-ref (+ (ssh-ipc-base) #x12810) :u64) 1)
                    (if (if (eq b 127) 1 (eq b 8))
                        (line-delete-back)
                        (if (if (eq b 10) 1 (eq b 13))
                            ;; Enter
                            (progn
                              (setf (mem-ref (+ (ssh-ipc-base) #x24) :u32) (edit-line-len))
                              (setf (mem-ref (+ (ssh-ipc-base) #x20) :u32) 0)
                              (setf (mem-ref (+ (ssh-ipc-base) #x12A08) :u64) 1))
                            (if (eq b 4)
                                ;; Ctrl-D on empty line
                                (when (zerop (edit-line-len))
                                  (setf (mem-ref (+ (ssh-ipc-base) #x12A08) :u64) 2))
                                (if (eq b 3)
                                    ;; Ctrl-C: cancel line
                                    (progn
                                      (write-byte 94) (write-byte 67) (write-byte 10)
                                      (edit-set-line-len 0)
                                      (edit-set-cursor-pos 0)
                                      (emit-prompt))
                                    (if (eq b 21)
                                        ;; Ctrl-U: clear line
                                        (progn
                                          (when (> (edit-cursor-pos) 0)
                                            (write-byte 27) (write-byte 91)
                                            (print-dec (edit-cursor-pos))
                                            (write-byte 68))
                                          (write-byte 27) (write-byte 91) (write-byte 75)
                                          (edit-set-line-len 0)
                                          (edit-set-cursor-pos 0))
                                        ;; Printable character
                                        (when (> b 31)
                                          (when (< b 127)
                                            (line-insert-byte b))))))))))))
    (mem-ref (+ (ssh-ipc-base) #x12A08) :u64)))

;;; ============================================================
;;; Buffer-based s-expression reader (reads from line buffer, not UART)
;;; ============================================================

;; Read position: ssh-ipc-base + 0x20
;; Line length: ssh-ipc-base + 0x24
;; Line buffer: ssh-ipc-base + 0x28

(defun buf-read-char ()
  (let ((pos (mem-ref (+ (ssh-ipc-base) #x20) :u32))
        (len (mem-ref (+ (ssh-ipc-base) #x24) :u32)))
    (if (< pos len)
        (let ((ch (mem-ref (+ (+ (ssh-ipc-base) #x28) pos) :u8)))
          (setf (mem-ref (+ (ssh-ipc-base) #x20) :u32) (+ pos 1))
          ch)
        32)))

(defun buf-peek-char ()
  (let ((pos (mem-ref (+ (ssh-ipc-base) #x20) :u32))
        (len (mem-ref (+ (ssh-ipc-base) #x24) :u32)))
    (if (< pos len)
        (mem-ref (+ (+ (ssh-ipc-base) #x28) pos) :u8)
        32)))

(defun buf-is-ws (c)
  (if (eq c 32) 1 (if (eq c 10) 1 (if (eq c 13) 1 (if (eq c 9) 1 nil)))))

(defun buf-is-delim (c)
  (if (buf-is-ws c) 1 (if (eq c 41) 1 (if (eq c 40) 1 nil))))

(defun buf-is-digit (c)
  (if (>= c 48) (if (<= c 57) 1 nil) nil))

(defun buf-skip-ws ()
  (let ((c (buf-peek-char)))
    (if (buf-is-ws c)
        (progn (buf-read-char) (buf-skip-ws))
        c)))

(defun buf-read-sym-chars ()
  (let ((c (buf-peek-char)))
    (if (buf-is-delim c)
        nil
        (progn
          (buf-read-char)
          (let ((uc (if (>= c 97) (if (<= c 122) (- c 32) c) c)))
            (cons uc (buf-read-sym-chars)))))))

(defun buf-read-number-rest (acc)
  (let ((c (buf-peek-char)))
    (if (buf-is-digit c)
        (progn
          (buf-read-char)
          (let ((d (- c 48)))
            (let ((newacc (+ (* acc 10) d)))
              (buf-read-number-rest newacc))))
        acc)))

(defun buf-read-sexp ()
  (let ((c (buf-skip-ws)))
    (if (eq c 40)
        (progn (buf-read-char) (buf-read-list))
        (if (eq c 39)
            (progn
              (buf-read-char)
              (let ((val (buf-read-sexp)))
                (cons (mksym (cons 81 (cons 85 (cons 79 (cons 84 (cons 69 nil))))))
                      (cons val nil))))
            (if (buf-is-digit c)
                (progn
                  (buf-read-char)
                  (buf-read-number-rest (- c 48)))
                (if (eq c 45)
                    (progn
                      (buf-read-char)
                      (let ((c2 (buf-peek-char)))
                        (if (buf-is-digit c2)
                            (progn
                              (buf-read-char)
                              (- 0 (buf-read-number-rest (- c2 48))))
                            (let ((rest (buf-read-sym-chars)))
                              (mksym (cons 45 rest))))))
                    (progn
                      (buf-read-char)
                      (let ((uc (if (>= c 97) (if (<= c 122) (- c 32) c) c)))
                        (let ((rest (buf-read-sym-chars)))
                          (let ((name (cons uc rest)))
                            (if (symbol-eq name (cons 78 (cons 73 (cons 76 nil))))
                                nil
                                (if (symbol-eq name (cons 84 nil))
                                    1
                                    (mksym name)))))))))))))

(defun buf-read-list ()
  (let ((c (buf-skip-ws)))
    (if (eq c 41)
        (progn (buf-read-char) nil)
        (if (eq c 46)
            ;; Dotted pair
            (progn
              (buf-read-char)
              (let ((val (buf-read-sexp)))
                (buf-skip-ws)
                (buf-read-char)  ; consume ')'
                val))
            (let ((elem (buf-read-sexp)))
              (cons elem (buf-read-list)))))))

;;; ============================================================
;;; SSH eval override (buffer-based reader + REPL evaluator)
;;; ============================================================

;; Persistent globals for SSH eval sessions
;; Stored at ssh-ipc-base + 0x60000
(defun ssh-get-globals ()
  (let ((g (mem-ref (+ (ssh-ipc-base) #x60000) :u64)))
    (if (zerop g)
        (let ((new-g (cons nil nil)))
          (setf (mem-ref (+ (ssh-ipc-base) #x60000) :u64) new-g)
          new-g)
        g)))

;; Print an s-expression via write-byte (capture-aware)
;; write-byte aware decimal print (not write-char-serial like prelude's print-dec)
(defun ssh-print-dec (n)
  (if (< n 10)
      (write-byte (+ 48 n))
      (let ((q (truncate n 10)))
        (let ((r (- n (* q 10))))
          (ssh-print-dec q)
          (write-byte (+ 48 r))))))

(defun ssh-print-sexp (x)
  (if (null x)
      (progn (write-byte 78) (write-byte 73) (write-byte 76))
      (if (fixnump x)
          (if (< x 0)
              (progn (write-byte 45) (ssh-print-dec (- 0 x)))
              (ssh-print-dec x))
          (if (consp x)
              (if (eq (car x) 9999)
                  ;; Symbol: print name chars
                  (ssh-print-chars (cdr x))
                  ;; List
                  (progn
                    (write-byte 40)
                    (ssh-print-sexp (car x))
                    (ssh-print-list-tail (cdr x))))
              (write-byte 63)))))

(defun ssh-print-list-tail (xs)
  (if (null xs)
      (write-byte 41)
      (if (consp xs)
          (if (eq (car xs) 9999)
              ;; Improper list ending in symbol
              (progn
                (write-byte 32) (write-byte 46) (write-byte 32)
                (ssh-print-chars (cdr xs))
                (write-byte 41))
              (progn
                (write-byte 32)
                (ssh-print-sexp (car xs))
                (ssh-print-list-tail (cdr xs))))
          (progn
            (write-byte 32) (write-byte 46) (write-byte 32)
            (ssh-print-sexp xs)
            (write-byte 41)))))

(defun ssh-print-chars (chars)
  (when (consp chars)
    (write-byte (car chars))
    (ssh-print-chars (cdr chars))))

;; Override ssh-do-eval-expr: use buffer reader instead of read-list
(defun ssh-do-eval-expr (ssh)
  (let ((len (edit-line-len)))
    ;; Set up FIFO read position past '(' and length
    (setf (mem-ref (+ (ssh-ipc-base) #x20) :u32) 1)
    (setf (mem-ref (+ (ssh-ipc-base) #x24) :u32) len)
    ;; Parse from line buffer
    (let ((lst (buf-read-list)))
      ;; Evaluate
      (let ((globals (ssh-get-globals)))
        (let ((result (eval-sexp lst nil globals)))
          ;; Enable capture for output
          (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 3)
          (setf (mem-ref (+ (ssh-ipc-base) #x18) :u32) 0)
          (write-byte 10)
          (write-byte 61) (write-byte 32)
          (ssh-print-sexp result)
          (write-byte 10)
          ;; Flush captured output
          (let ((out-len (mem-ref (+ (ssh-ipc-base) #x18) :u32)))
            (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 0)
            (when (> out-len 0)
              (let ((out (make-array out-len)))
                (dotimes (i out-len)
                  (aset out i (mem-ref (+ (+ (ssh-ipc-base) #x100) i) :u8)))
                (ssh-send-string ssh out out-len)))))))))

;; Override native-eval to use the REPL interpreter
(defun native-eval (form)
  (eval-sexp form nil (ssh-get-globals)))

;;; ============================================================
;;; Pre-computed crypto (avoid >5s USB polling gap)
;;; ============================================================

;; Pre-compute Ed25519 host key derivatives for fast signing.
;; Stores clamped scalar s at state+0x680, prefix at state+0x6A0.
;; Must be called AFTER ed25519-init and host key setup.
(defun pre-compute-host-sign ()
  (let ((state (e1000-state-base)))
    (sha512-init)
    ;; Load host private key from state+0x710
    (let ((privkey (make-array 32)))
      (dotimes (i 32)
        (aset privkey i (mem-ref (+ state (+ #x710 i)) :u8)))
      ;; SHA-512(privkey) → 64-byte hash
      (let ((hash (sha512 privkey)))
        ;; Clamp first 32 bytes → scalar s → store at state+0x680
        (dotimes (i 32)
          (setf (mem-ref (+ state (+ #x680 i)) :u8) (aref hash i)))
        (setf (mem-ref (+ state #x680) :u8)
              (logand (mem-ref (+ state #x680) :u8) #xF8))
        (let ((b31 (mem-ref (+ state #x69F) :u8)))
          (setf (mem-ref (+ state #x69F) :u8)
                (logior (logand b31 #x7F) #x40)))
        ;; Second 32 bytes → prefix → store at state+0x6A0
        (dotimes (i 32)
          (setf (mem-ref (+ state (+ #x6A0 i)) :u8) (aref hash (+ i 32))))
        ;; Mark as pre-computed
        (setf (mem-ref (+ state #x6C0) :u32) 1)))))

;; Pre-compute X25519 server ephemeral key pair.
;; Stores private key at state+0x6C4, public key at state+0x6E4.
(defun pre-compute-server-eph (ssh)
  (let ((state (e1000-state-base)))
    ;; Generate random private key
    (let ((priv (make-array 32)))
      (dotimes (i 32) (aset priv i (ssh-random ssh)))
      ;; Store private key at state+0x6C4
      (dotimes (i 32)
        (setf (mem-ref (+ state (+ #x6C4 i)) :u8) (aref priv i)))
      ;; Compute and store public key at state+0x6E4
      (let ((pub (x25519-public-key priv)))
        (dotimes (i 32)
          (setf (mem-ref (+ state (+ #x6E4 i)) :u8) (aref pub i)))))))

;; Fast Ed25519 sign using pre-computed s, prefix, and host public key.
;; Saves one ed-base-mult and one SHA-512 compared to ed25519-sign.
(defun ed25519-sign-fast (message msg-len)
  (let ((state (e1000-state-base)))
    ;; Load pre-computed s and prefix
    (let ((s (make-array 32)))
      (dotimes (i 32)
        (aset s i (mem-ref (+ state (+ #x680 i)) :u8)))
      (let ((prefix (make-array 32)))
        (dotimes (i 32)
          (aset prefix i (mem-ref (+ state (+ #x6A0 i)) :u8)))
        ;; Load pre-computed host public key (a-enc) from state+0x730
        (let ((a-enc (make-array 32)))
          (dotimes (i 32)
            (aset a-enc i (mem-ref (+ state (+ #x730 i)) :u8)))
          ;; r = SHA-512(prefix || message) mod L
          (let ((r-input (concat-bytes prefix 32 message msg-len)))
            (let ((r (ed-reduce-scalar (sha512 r-input))))
              ;; R = r*B (the one remaining expensive operation)
              (let ((r-enc (ed-encode-point (ed-base-mult r))))
                ;; k = SHA-512(R || A || message) mod L
                (let ((k-input (concat3-bytes r-enc 32 a-enc 32 message msg-len)))
                  (let ((k (ed-reduce-scalar (sha512 k-input))))
                    ;; S = (r + k*s) mod L
                    (let ((ks (ed-scalar-mult-mod-l k s)))
                      (let ((sig-s (ed-scalar-add r ks)))
                        (concat-bytes r-enc 32 sig-s 32)))))))))))))


;; USB keep-alive: poll USB to prevent host NETDEV WATCHDOG timeout
(defun usb-keepalive ()
  (let ((i 0))
    (loop
      (when (>= i 100) (return 0))
      (io-delay)
      (let ((pkt-len (e1000-receive)))
        (when (not (zerop pkt-len))
          (let ((buf (e1000-rx-buf)))
            (let ((et-hi (mem-ref (+ buf 12) :u8)))
              (when (eq et-hi #x08)
                (let ((et-lo (mem-ref (+ buf 13) :u8)))
                  (if (eq et-lo #x06)
                      (let ((arp-op (buf-read-u16-mem buf 20)))
                        (when (eq arp-op 1) (arp-reply buf)))
                      (when (eq et-lo 0)
                        (let ((proto (mem-ref (+ buf 23) :u8)))
                          (when (eq proto 1) (icmp-handle buf 14)))))))))))
      (setq i (+ i 1)))))

;;; ============================================================
;;; Network overrides (single-threaded)
;;; ============================================================

;; Override receive: process one round of network packets inline
;; In multi-threaded x86, receive() blocks until an actor message arrives.
;; In single-threaded AArch64, we poll the E1000 and process one packet.
(defun receive ()
  (io-delay)
  (let ((pkt-len (e1000-receive)))
    (if (zerop pkt-len)
        1
        (let ((buf (e1000-rx-buf)))
          (let ((et-hi (mem-ref (+ buf 12) :u8))
                (et-lo (mem-ref (+ buf 13) :u8)))
            (if (eq et-hi #x08)
                (if (eq et-lo #x06)
                    (let ((arp-op (buf-read-u16-mem buf 20)))
                      (when (eq arp-op 1) (arp-reply buf)))
                    (when (eq et-lo 0)
                      (let ((proto (mem-ref (+ buf 23) :u8)))
                        (if (eq proto 1)
                            (icmp-handle buf 14)
                            (when (eq proto 6)
                              (net-handle-tcp buf pkt-len))))))
                ()))
          1))))


;; Override ssh-server for single-threaded AArch64
;; No GC helper, no actor-spawn. Runs network loop directly.
(defun ssh-server (port)
  (ssh-seed-random)
  (ssh-init-strings)
  ;; No GC helper on AArch64 (no native eval at runtime)
  (when (zerop (mem-ref (+ (e1000-state-base) #x624) :u32))
    (ssh-use-default-key))
  (write-byte 83) (write-byte 83) (write-byte 72)
  (write-byte 58) (print-dec port) (write-byte 10)
  ;; Store listen port
  (setf (mem-ref (+ (ssh-ipc-base) #x60438) :u32) port)
  ;; Clear connection table
  (let ((i 0))
    (loop
      (when (>= i 4) (return 0))
      (setf (mem-ref (conn-base i) :u32) 0)
      (setq i (+ i 1))))
  ;; Single-threaded: run network loop directly (never returns)
  (net-actor-main))

;; Override ssh-connection-handler for single-threaded operation
(defun ssh-connection-handler (conn)
  (let ((ssh (conn-ssh conn))
        (cb (conn-base conn)))
    (ssh-handle-connection ssh)
    ;; Regenerate server ephemeral X25519 key pair for next connection
    (pre-compute-server-eph ssh)
    (tcp-close-conn cb)
    (conn-free conn)))

;; Override ssh-handle-connection for single-threaded AArch64
(defun ssh-handle-connection (ssh)
  (let ((cb (- ssh #x20)))
    (ssh-send-version ssh)
    (when (zerop (ssh-receive-version ssh))
      (return ()))
    (let ((kexinit (ssh-build-kexinit ssh)))
      (ssh-send-payload ssh kexinit (array-length kexinit)))
    ;; FIX: ssh-receive-packet returns NIL on timeout; (zerop NIL) does NOT
    ;; catch it, so (car NIL) faults.  And the stock 100-try timeout is too
    ;; short on real hardware.  Null-check + use 50000 like ssh-message-loop.
    (let ((cli-kex (ssh-receive-packet ssh 50000)))
      (when (or (null cli-kex) (zerop cli-kex)) (return ()))
      (let ((cli-kex-payload (car cli-kex)))
        (when (not (eq (aref cli-kex-payload 0) 20)) (return ()))
        (ssh-mem-store (+ cb #x1F00) cli-kex-payload (cdr cli-kex))
        (setf (mem-ref (+ ssh #x20) :u32) (cdr cli-kex))
        (let ((kex-init (ssh-receive-packet ssh 50000)))
          (when (or (null kex-init) (zerop kex-init)) (return ()))
          (let ((kex-payload (car kex-init)))
            (when (not (eq (aref kex-payload 0) 30)) (return ()))
            (ssh-handle-kex ssh kex-payload (cdr kex-init))
            (ssh-send-newkeys ssh)
            (let ((nk (ssh-receive-packet ssh 50000)))
              (when (or (null nk) (zerop nk)) (return ()))
              (when (not (eq (aref (car nk) 0) 21)) (return ()))
              (ssh-derive-keys ssh)
              (ssh-message-loop ssh))))))))

;; Message dispatch - flat when chain avoids deep nesting
(defun ssh-dispatch-msg (ssh payload plen flag-addr)
  (let ((msg-type (aref payload 0)))
    (when (eq msg-type 5)
      (let ((svc-len (ssh-get-u32 payload 1))
            (svc (make-array 32)))
        (dotimes (i svc-len)
          (aset svc i (aref payload (+ 5 i))))
        (ssh-send-service-accept ssh svc svc-len)))
    (when (eq msg-type 50)
      (ssh-handle-userauth ssh payload plen))
    (when (eq msg-type 90)
      (let ((ctype-len (ssh-get-u32 payload 1)))
        (let ((cli-chan (ssh-get-u32 payload (+ 5 ctype-len))))
          (setf (mem-ref (+ ssh #x18) :u32) cli-chan)
          (setf (mem-ref (+ ssh #x14) :u32) 0)
          (ssh-send-channel-confirm ssh cli-chan 0))))
    (when (eq msg-type 98)
      (let ((rtype-len (ssh-get-u32 payload 5)))
        (let ((want-reply (aref payload (+ 9 rtype-len))))
          (when (not (zerop want-reply))
            (ssh-send-channel-success ssh
             (mem-ref (+ ssh #x18) :u32)))
          ;; "shell" (rtype_len=5, 's'=115) → send prompt, enter interactive
          (when (eq rtype-len 5)
            (when (eq (aref payload 9) 115)
              (ssh-send-prompt ssh)))
          ;; "exec" (rtype_len=4, 'e'=101) → execute command, send result
          (when (eq rtype-len 4)
            (when (eq (aref payload 9) 101)
              (let ((cmd-off (+ 10 rtype-len)))
                (let ((cmd-len (ssh-get-u32 payload cmd-off)))
                  (let ((cmd (make-array cmd-len)))
                    (dotimes (i cmd-len)
                      (aset cmd i (aref payload (+ cmd-off 4 i))))
                    (ssh-eval-line ssh cmd cmd-len)
                    ;; Send exit-status 0 + EOF + CLOSE after exec
                    (let ((cli-chan (mem-ref (+ ssh #x18) :u32)))
                      ;; SSH_MSG_CHANNEL_REQUEST "exit-status" want_reply=0 status=0
                      (let ((xs (make-array 25)))
                        (aset xs 0 98)
                        (ssh-put-u32 xs 1 cli-chan)
                        (ssh-put-u32 xs 5 11)
                        (aset xs 9 101) (aset xs 10 120) (aset xs 11 105)
                        (aset xs 12 116) (aset xs 13 45) (aset xs 14 115)
                        (aset xs 15 116) (aset xs 16 97) (aset xs 17 116)
                        (aset xs 18 117) (aset xs 19 115)
                        (aset xs 20 0)
                        (ssh-put-u32 xs 21 0)
                        (ssh-send-payload ssh xs 25))
                      (let ((eof-msg (make-array 5)))
                        (aset eof-msg 0 96)
                        (ssh-put-u32 eof-msg 1 cli-chan)
                        (ssh-send-payload ssh eof-msg 5))
                      (let ((close-msg (make-array 5)))
                        (aset close-msg 0 97)
                        (ssh-put-u32 close-msg 1 cli-chan)
                        (ssh-send-payload ssh close-msg 5)))
                    (setf (mem-ref flag-addr :u32) 0)))))))))
    (when (eq msg-type 94)
      (ssh-handle-channel-data ssh payload plen))
    (when (eq msg-type 97)
      (setf (mem-ref flag-addr :u32) 0))
    (when (eq msg-type 1)
      (setf (mem-ref flag-addr :u32) 0))))

;; Message loop — runs until client disconnects.
;; Never times out: keeps polling for data indefinitely.
(defun ssh-message-loop (ssh)
  (let ((flag-addr (+ (conn-ssh 3) #x700)))
    (setf (mem-ref flag-addr :u32) 1)
    (loop
      (when (zerop (mem-ref flag-addr :u32)) (return ()))
      ;; Use a reasonable per-poll timeout, but retry on nil (no-data).
      ;; Only exit when flag is cleared (client disconnect/close).
      (let ((pkt (ssh-receive-packet ssh 50000)))
        (when pkt
          (ssh-dispatch-msg ssh (car pkt) (cdr pkt) flag-addr))))))

;; Override net-wait-ack: also deliver piggybacked data from ACK packets
;; The original discards TCP data, losing the client's SSH version string
;; when it arrives in the same packet as the ACK.
(defun net-wait-ack (conn)
  (let ((cb (conn-base conn))
        ;; Server ISN: net-accept-connection already sent SYN-ACK once, bumping
        ;; cb+0x010 to ISN+1.  Save ISN so retransmits reuse the SAME seq.
        (isn (- (mem-ref (+ (conn-base conn) #x010) :u32) 1))
        (acked 0)
        (tries 0))
    (loop
      (when (not (zerop acked)) (return 1))
      (when (> tries 800) (return 0))
      ;; Retransmit the SYN-ACK (identical seq) if it was lost — the single-
      ;; threaded server no longer depends on the client's reconnect (which the
      ;; re-entrancy guard now blocks) to retry the handshake.
      (when (and (> tries 0) (eq (mod tries 40) 0))
        (setf (mem-ref (+ cb #x010) :u32) isn)
        (tcp-send-segment-conn cb 18 (make-array 0) 0))
      (io-delay)
      (let ((pkt-len (e1000-receive)))
        (when (not (zerop pkt-len))
          (let ((b2 (e1000-rx-buf)))
            (when (eq (mem-ref (+ b2 12) :u8) #x08)
              (when (eq (mem-ref (+ b2 13) :u8) 0)
                (when (eq (mem-ref (+ b2 23) :u8) 6)
                  (let ((f2 (mem-ref (+ b2 47) :u8)))
                    ;; Accept ONLY a real ACK (ACK set, SYN clear) — a SYN
                    ;; retransmit must not be mistaken for the handshake ACK.
                    (when (and (eq (logand f2 #x10) #x10)
                               (zerop (logand f2 #x02)))
                      (setf (mem-ref cb :u32) 2)
                      (setq acked 1)
                      ;; Deliver any piggybacked data
                      (net-deliver-data conn b2 pkt-len f2)))))))))
      (setq tries (+ tries 1)))))

;; Override net-accept-connection for single-threaded operation
;; Calls ssh-connection-handler directly instead of actor-spawn
(defun net-accept-connection (src-ip src-port dst-port buf)
  (let ((conn (conn-alloc)))
    (when (not (= conn (- 0 1)))
      (conn-init conn dst-port src-port src-ip)
      ;; Clear protocol type (uninitialized RAM on real hardware)
      (setf (mem-ref (+ (conn-base conn) #x1C) :u32) 0)
      ;; Set protocol type: port 80 → HTTP (1), else SSH (0)
      (when (eq dst-port 80)
        (setf (mem-ref (+ (conn-base conn) #x1C) :u32) 1))
      (let ((their-seq (buf-read-u32-mem buf 38)))
        (setf (mem-ref (+ (conn-base conn) #x014) :u32) (+ their-seq 1)))
      ;; Init recv buffer BEFORE net-wait-ack so piggybacked data works
      (let ((ssh (conn-ssh conn)))
        (setf (mem-ref (+ ssh #x6D4) :u32) 0))
      (tcp-send-segment-conn (conn-base conn) 18 (make-array 0) 0)
      (net-wait-ack conn)
      (when (eq (mem-ref (conn-base conn) :u32) 2)
        (if (eq (mem-ref (+ (conn-base conn) #x1C) :u32) 1)
            ;; HTTP: handle directly (no SSH state needed)
            (http-connection-handler conn)
            ;; SSH: init state and handle
            (progn
              (ssh-copy-host-key conn)
              (let ((ssh (conn-ssh conn)))
                (setf (mem-ref ssh :u32) 0)
                (setf (mem-ref (+ ssh #x04) :u32) 0)
                (setf (mem-ref (+ ssh #x08) :u32) 0)
                (setf (mem-ref (+ ssh #x0C) :u32) 0)
                (setf (mem-ref (+ ssh #x10) :u32) 0)
                (setf (mem-ref (+ ssh #x28) :u32) 0)
                (setf (mem-ref (+ ssh #x150) :u32) 0)
                (setf (mem-ref (+ ssh #x154) :u32) 0)
                (setf (mem-ref (+ ssh #x2C) :u32)
                      (+ src-port (* src-ip 7))))
              (setf (mem-ref (+ (ssh-ipc-base) #x60448) :u32) conn)
              (ssh-connection-handler conn)))))))
