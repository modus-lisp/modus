;;;; build-pizero2w-actors.lisp - Build Pi Zero 2 W SSH kernel with actor system
;;;;
;;;; Usage: sbcl --script mvm/build-pizero2w-actors.lisp
;;;;
;;;; Produces /tmp/piboot/kernel8.img — deploy with:
;;;;   sudo /tmp/usbboot/rpiboot -d /tmp/piboot
;;;;
;;;; DWC2 operates in USB device (gadget) mode with CDC-ECM Ethernet.
;;;; Actor system provides cooperative scheduling so crypto doesn't starve USB.
;;;; Net-domain (actor 2) polls USB; SSH handlers run as separate actors.

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(mvm-load "mvm/repl-source.lisp")

(defun read-file-text (path)
  "Read entire file as a string."
  (with-open-file (s path :direction :input)
    (let ((text (make-string (file-length s))))
      (let ((n (read-sequence text s)))
        (subseq text 0 n)))))

(defvar *net-dir*
  (merge-pathnames "net/" *modus-base*))

;; Source load order (actor-aware):
;; 1. arch-raspi3b.lisp          - RPi adapter + actor address hooks
;; 2. actors.lisp                 - shared actor system
;; 3. dwc2-device.lisp           - USB gadget NIC (CDC-ECM)
;; 4. ip.lisp                     - ARP/IP/TCP (provides net-actor-main)
;; 5. crypto.lisp                 - SHA, ChaCha20, Poly1305, X25519, Ed25519
;; 6. ssh.lisp                    - SSH server
;; 7. http.lisp                   - HTTP/1.0 server (port 80)
;; 8. http-client.lisp            - HTTP client
;; 9. aarch64-overrides.lisp      - line editor, buffer reader, SSH overrides
;; 10. actors-net-overrides.lisp  - actor-aware receive/spawn/exit overrides
(defvar *net-source*
  (format nil "~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%"
          (read-file-text (merge-pathnames "arch-raspi3b.lisp" *net-dir*))
          (read-file-text (merge-pathnames "actors.lisp" *net-dir*))
          (read-file-text (merge-pathnames "dwc2-device.lisp" *net-dir*))
          (read-file-text (merge-pathnames "ip.lisp" *net-dir*))
          (read-file-text (merge-pathnames "crypto.lisp" *net-dir*))
          (read-file-text (merge-pathnames "crypto-fast.lisp" *net-dir*))
          (read-file-text (merge-pathnames "ssh.lisp" *net-dir*))
          (read-file-text (merge-pathnames "http.lisp" *net-dir*))
          (read-file-text (merge-pathnames "http-client.lisp" *net-dir*))
          (read-file-text (merge-pathnames "aarch64-overrides.lisp" *net-dir*))
          (read-file-text (merge-pathnames "actors-net-overrides.lisp" *net-dir*))))

;;; ============================================================
;;; Build Pi Zero 2 W Actors+SSH image (DWC2 USB gadget + CDC-ECM)
;;; ============================================================

(in-package :modus.mvm)

;; Install the AArch64 translator
(install-aarch64-translator)

;; Use mini UART (0x3F215040) instead of PL011 (0x3F201000) for write-byte
(setf *aarch64-serial-base* #x3F215040)
;; BCM2837 peripherals require 32-bit stores
(setf *aarch64-serial-width* 2)
;; Poll AUX_MU_LSR (base+0x14) bit 5 (TX empty) before each write
(setf *aarch64-serial-tx-poll* '(#x14 5 :tbz))
;; Mini UART RX: AUX_MU_LSR bit 0 = data ready, so wait while CLEAR.
;; Without this the image PRINTS but never READS — the RX poll used to be
;; hardcoded to PL011 (0x18 / bit 4 / :tbnz), which QEMU raspi3b satisfies
;; and real Pi Zero 2 W hardware does not.
(setf *aarch64-serial-rx-poll* '(#x14 0 :tbz))
;; Scheduler lock address — RESTORE-CONTEXT must unlock after context switch
;; Without this, restore-context doesn't release the lock → deadlock on first yield
(setf *aarch64-sched-lock-addr* #x02000200)

(let* ((ssh-main (format nil "~{~A~%~}"
                        (list
                         "(defun kernel-main ()"
                         ;; Clear SSH IPC flags — uninitialized memory may suppress serial output
                         "  (setf (mem-ref #x01100014 :u32) 0)"
                         ;; Clear TCP send lock — uninitialized RAM deadlocks spin-lock
                         "  (setf (mem-ref (+ (ssh-ipc-base) #x60430) :u64) 0)"
                         ;; Clear eval globals pointer — uninitialized RAM causes lookup to
                         ;; traverse garbage pointers (hangs on any non-special-form eval)
                         "  (setf (mem-ref (+ (ssh-ipc-base) #x60000) :u64) 0)"
                         ;; === Set GPIO14=ALT5 (mini UART TX), GPIO15=ALT5 (mini UART RX) ===
                         "  (setf (mem-ref #x3F200004 :u32) #x12000)"
                         ;; === Mini UART init ===
                         "  (setf (mem-ref #x3F215004 :u32) 1)"
                         "  (setf (mem-ref #x3F215060 :u32) 0)"
                         "  (setf (mem-ref #x3F21504C :u32) 3)"
                         "  (setf (mem-ref #x3F215050 :u32) 0)"
                         "  (setf (mem-ref #x3F215044 :u32) 0)"
                         "  (setf (mem-ref #x3F215048 :u32) #xC6)"
                         "  (setf (mem-ref #x3F215068 :u32) 270)"
                         "  (setf (mem-ref #x3F215060 :u32) 3)"
                         ;; UART ready marker
                         "  (write-byte 85) (write-byte 65) (write-byte 82)"
                         "  (write-byte 84) (write-byte 10)"
                         ;; Initialize SMP + actor system (yield is safe before USB)
                         "  (smp-init)"
                         "  (actor-init)"
                         ;; === Crypto before USB — avoids NETDEV WATCHDOG ===
                         ;; All slow crypto runs here with no USB device active.
                         ;; usb-keepalive (yield-based) is a no-op since no other actors exist.
                         "  (write-byte 91) (write-byte 49) (write-byte 93)"
                         "  (sha256-init)"
                         "  (write-byte 91) (write-byte 50) (write-byte 93)"
                         "  (sha512-init)"
                         "  (write-byte 91) (write-byte 51) (write-byte 93)"
                         ;; Clear ed25519 init flag — uninitialized RAM on real hardware
                         "  (setf (mem-ref (+ (e1000-state-base) #x5D0) :u32) 0)"
                         "  (ed25519-init)"
                         "  (write-byte 91) (write-byte 52) (write-byte 93)"
                         "  (ssh-seed-random)"
                         "  (write-byte 91) (write-byte 53) (write-byte 93)"
                         ;; Host key inline
                         "  (let ((state (e1000-state-base)))"
                         "    (setf (mem-ref (+ state #x710) :u64) 0)"
                         "    (setf (mem-ref (+ state #x718) :u64) 0)"
                         "    (setf (mem-ref (+ state #x720) :u64) 0)"
                         "    (setf (mem-ref (+ state #x728) :u64) 0)"
                         "    (setf (mem-ref (+ state #x730) :u32) #xBC276A3B)"
                         "    (setf (mem-ref (+ state #x734) :u32) #x2DA4B6CE)"
                         "    (setf (mem-ref (+ state #x738) :u32) #xD0A8A362)"
                         "    (setf (mem-ref (+ state #x73C) :u32) #x730D6F2A)"
                         "    (setf (mem-ref (+ state #x740) :u32) #x77153265)"
                         "    (setf (mem-ref (+ state #x744) :u32) #xA643E21D)"
                         "    (setf (mem-ref (+ state #x748) :u32) #xA148C03A)"
                         "    (setf (mem-ref (+ state #x74C) :u32) #x29DA598B)"
                         "    (setf (mem-ref (+ state #x624) :u32) 1))"
                         "  (write-byte 91) (write-byte 57) (write-byte 93)"
                         ;; Pre-compute ed25519 host key derivatives (SHA-512 only, fast)
                         "  (pre-compute-host-sign)"
                         "  (write-byte 91) (write-byte 65) (write-byte 93)"
                         ;; Clear pre-compute flag — uninitialized RAM on real hardware
                         "  (setf (mem-ref (+ (e1000-state-base) #x6C0) :u32) 0)"
                         ;; Pre-compute server ephemeral X25519 key pair (~10s scalar mult)
                         "  (pre-compute-server-eph (conn-ssh 0))"
                         "  (write-byte 91) (write-byte 66) (write-byte 93)"
                         ;; === USB init — crypto already done, no NETDEV WATCHDOG ===
                         "  (cdc-ether-init)"
                         "  (write-byte 91) (write-byte 67) (write-byte 93)"
                         ;; Set DNS server to 8.8.8.8 (works with host NAT)
                         "  (setf (mem-ref (+ (e1000-state-base) #x58) :u32) #x08080808)"
                         "  (write-byte 83) (write-byte 83) (write-byte 72)"
                         "  (write-byte 58) (print-dec 22) (write-byte 10)"
                         "  (setf (mem-ref (+ (ssh-ipc-base) #x60438) :u32) 22)"
                         ;; Clear connection slots — uninitialized RAM on real hardware
                         "  (let ((i 0))"
                         "    (loop"
                         "      (when (>= i 4) (return 0))"
                         "      (setf (mem-ref (conn-base i) :u32) 0)"
                         "      (setq i (+ i 1))))"
                         ;; Spawn net-actor-main — polls USB via e1000-receive (dwc2-device.lisp)
                         "  (actor-spawn (fn-addr net-actor-main))"
                         ;; Primordial actor: idle yield loop
                         "  (loop (yield)))")))
       ;; Pi-specific overrides for actors.lisp compatibility
       ;; spin-lock: LDXR/STXR doesn't work on Cortex-A53 without MMU
       ;; usb-keepalive: actors-net-overrides.lisp yield version is used (last defun)
       ;; e1000-receive: dwc2-device.lisp polling version is used (last defun)
       (pi-keepalive (format nil "~{~A~%~}"
                       (list
                        ;; Direct UART output (bypasses SSH capture flags)
                        "(defun uart-out (b)"
                        "  (loop (when (not (zerop (logand (mem-ref #x3F215054 :u32) #x20))) (return 0)))"
                        "  (setf (mem-ref #x3F215040 :u32) b))"
                        ;; Pi-specific spin-lock: non-atomic (safe for cooperative scheduling)
                        "(defun spin-lock (addr)"
                        "  (loop"
                        "    (when (zerop (mem-ref addr :u64))"
                        "      (setf (mem-ref addr :u64) 1)"
                        "      (return 0))))"
                        ;; Diagnostic net-actor-main: tight poll USB, yield every 200 iters
                        ;; Prevents NETDEV WATCHDOG (5s) while still giving handler actors CPU
                        "(defun net-actor-main ()"
                        ;; N0: net-actor-main started
                        "  (uart-out 78) (uart-out 48) (uart-out 10)"
                        "  (let ((idle 0) (loop-cnt 0))"
                        "    (loop"
                        "      (setq loop-cnt (+ loop-cnt 1))"
                        ;; Print 'L' every 10000 iterations to show liveness
                        "      (when (>= loop-cnt 10000)"
                        "        (setq loop-cnt 0)"
                        "        (uart-out 76))"
                        "      (let ((msg (try-receive)))"
                        "        (when (not (zerop msg))"
                        ;; M1: message received
                        "          (uart-out 77) (uart-out 49) (uart-out 10)"
                        "          (let ((inner (cdr msg)))"
                        "            (let ((sender (car inner))"
                        "                  (url-info (cdr inner)))"
                        "              (let ((url (car url-info))"
                        "                    (url-len (cdr url-info)))"
                        ;; M2: about to call http-fetch-impl with url-len
                        "                (uart-out 77) (uart-out 50)"
                        "                (uart-out 58) (uart-out (+ 48 url-len)) (uart-out 10)"
                        "                (let ((result (http-fetch-impl url url-len)))"
                        ;; M3: fetch returned
                        "                  (uart-out 77) (uart-out 51) (uart-out 10)"
                        "                  (send sender result)))))))"
                        "      (let ((pkt-len (e1000-receive)))"
                        "        (if (zerop pkt-len)"
                        "            (progn"
                        "              (setq idle (+ idle 1))"
                        "              (when (>= idle 200)"
                        "                (setq idle 0)"
                        "                (yield)))"
                        "            (progn"
                        "              (setq idle 0)"
                        "              (let ((buf (e1000-rx-buf)))"
                        "                (let ((et-hi (mem-ref (+ buf 12) :u8))"
                        "                      (et-lo (mem-ref (+ buf 13) :u8)))"
                        "                  (if (eq et-hi #x08)"
                        "                      (if (eq et-lo #x06)"
                        "                          (let ((arp-op (buf-read-u16-mem buf 20)))"
                        "                            (when (eq arp-op 1) (arp-reply buf)))"
                        "                          (when (eq et-lo 0)"
                        ;; Learn source MAC from incoming IP packet (fixes broadcast gateway MAC)
                        "                            (let ((st (e1000-state-base)))"
                        "                              (dotimes (m 6)"
                        "                                (setf (mem-ref (+ st (+ #x28 m)) :u8)"
                        "                                      (mem-ref (+ buf (+ 6 m)) :u8))))"
                        "                            (let ((proto (mem-ref (+ buf 23) :u8)))"
                        "                              (if (eq proto 1)"
                        "                                  (icmp-handle buf 14)"
                        "                                  (when (eq proto 6)"
                        "                                    (net-handle-tcp buf pkt-len))))))"
                        "                      ())))"
                        "              (yield)))))))"
                        ;; Override ssh-handle-kex: add usb-keepalive during crypto to prevent NETDEV WATCHDOG
                        "(defun ssh-handle-kex (ssh kex-init-payload kex-init-len)"
                        "  (let ((cli-eph (make-array 32)))"
                        "    (dotimes (i 32) (aset cli-eph i (aref kex-init-payload (+ 5 i))))"
                        "    (let ((state (e1000-state-base)))"
                        "      (let ((srv-priv (make-array 32)))"
                        "        (dotimes (i 32)"
                        "          (aset srv-priv i (mem-ref (+ state (+ #x6C4 i)) :u8)))"
                        "        (let ((srv-eph (make-array 32)))"
                        "          (dotimes (i 32)"
                        "            (aset srv-eph i (mem-ref (+ state (+ #x6E4 i)) :u8)))"
                        "          (let ((shared (x25519 srv-priv cli-eph)))"
                        "            (usb-keepalive)"
                        "            (ssh-mem-store (+ ssh #x070) shared 32)"
                        "            (let ((h (ssh-compute-exchange-hash ssh cli-eph srv-eph shared)))"
                        "              (ssh-mem-store (+ ssh #x050) h 32)"
                        "              (when (zerop (mem-ref ssh :u32))"
                        "                (ssh-mem-store (+ ssh #x030) h 32)"
                        "                (setf (mem-ref ssh :u32) 1))"
                        "              (usb-keepalive)"
                        "              (let ((sig (ed25519-sign-fast h 32)))"
                        "                (usb-keepalive)"
                        "                (ssh-send-kex-reply ssh sig srv-eph)"
                        "                ))))))))"
                        ;; Override ssh-connection-handler: skip pre-compute-server-eph
                        ;; (10s X25519 risks USB NETDEV WATCHDOG; reconnection fails
                        ;; with "incorrect signature" regardless — needs deeper investigation)
                        "(defun ssh-connection-handler (conn)"
                        "  (let ((ssh (conn-ssh conn))"
                        "        (cb (conn-base conn)))"
                        "    (ssh-handle-connection ssh)"
                        "    (tcp-close-conn cb)"
                        "    (conn-free conn)"
                        "    (actor-exit)))"
                        ;; Override http-fetch: call http-fetch-impl directly (bypass actor messaging)
                        ;; The actor-based version sends to net-domain actor 2, but the yield/schedule
                        ;; mechanism stalls. Direct call works because the handler can poll USB itself.
                        "(defun http-fetch (url url-len)"
                        "  (http-print-result (http-fetch-impl url url-len)))"
                        ;; Override ssh-do-eval-expr: handle compiled function calls
                        ;; Check raw command bytes for known names, call directly.
                        ;; Fall back to interpreter for everything else.
                        "(defun ssh-eval-interp (ssh len)"
                        "  (setf (mem-ref (+ (ssh-ipc-base) #x20) :u32) 1)"
                        "  (setf (mem-ref (+ (ssh-ipc-base) #x24) :u32) len)"
                        "  (let ((lst (buf-read-list)))"
                        "    (let ((globals (ssh-get-globals)))"
                        "      (let ((result (eval-sexp lst nil globals)))"
                        "        (write-byte 10) (write-byte 61) (write-byte 32)"
                        "        (ssh-print-sexp result)"
                        "        (write-byte 10)))))"
                        "(defun ssh-do-eval-expr (ssh)"
                        "  (let ((len (edit-line-len)))"
                        ;; Enable capture for output
                        "    (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 3)"
                        "    (setf (mem-ref (+ (ssh-ipc-base) #x18) :u32) 0)"
                        ;; Check raw command: "(fetch-test)" = 12 chars, byte[1]='f'=102
                        "    (if (eq len 12)"
                        "        (if (eq (mem-ref (+ (ssh-ipc-base) #x29) :u8) 102)"
                        "            (fetch-test)"
                        "            (ssh-eval-interp ssh len))"
                        ;; "(fetch)" = 7 chars
                        "        (if (eq len 7)"
                        "            (if (eq (mem-ref (+ (ssh-ipc-base) #x29) :u8) 102)"
                        "                (fetch)"
                        "                (ssh-eval-interp ssh len))"
                        "            (ssh-eval-interp ssh len)))"
                        ;; Flush captured output
                        "    (let ((out-len (mem-ref (+ (ssh-ipc-base) #x18) :u32)))"
                        "      (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 0)"
                        "      (when (> out-len 0)"
                        "        (let ((out (make-array out-len)))"
                        "          (dotimes (i out-len)"
                        "            (aset out i (mem-ref (+ (+ (ssh-ipc-base) #x100) i) :u8)))"
                        "          (ssh-send-string ssh out out-len))))))"
)))
       (combined-source (concatenate 'string
                                      ssh-main
                                      cl-user::*net-source*
                                      pi-keepalive
                                      *repl-source*)))
  (format t "Building Pi Zero 2 W Actors+SSH image (DWC2 USB gadget)...~%")
  (format t "Combined source: ~D chars~%" (length combined-source))
  (let ((image (build-image :target :rpi :source-text combined-source)))
    (format t "Entry point offset: ~A~%" (kernel-image-entry-point image))
    (format t "Native code size: ~D~%" (length (kernel-image-native-code image)))
    (format t "Boot code size: ~D~%" (length (kernel-image-boot-code image)))
    (write-kernel-image image "/tmp/piboot/kernel8.img")
    (format t "Done. Deploy with:~%")
    (format t "  sudo /tmp/usbboot/rpiboot -d /tmp/piboot~%")
    (format t "Then configure host networking:~%")
    (format t "  sudo ip addr add 10.0.0.1/24 dev usb0~%")
    (format t "  sudo ip link set usb0 up~%")
    (format t "  ssh test@10.0.0.2~%")))
