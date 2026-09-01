;;;; build-arm32-ssh.lisp - Build ARM32 SSH kernel for QEMU raspi2b
;;;;
;;;; Usage: sbcl --script mvm/build-arm32-ssh.lisp
;;;;
;;;; Produces /tmp/modus-arm32-ssh.bin — boot with:
;;;;   qemu-system-arm -M raspi2b -m 1G -nographic \
;;;;     -device usb-net,netdev=net0 \
;;;;     -netdev 'user,id=net0,hostfwd=tcp::2222-:22'
;;;;
;;;; Uses DWC2 USB host + CDC Ethernet (same as raspi3b/Pi Zero 2 W).
;;;; All 32-bit crypto uses (hi16 . lo16) pair arithmetic (crypto-w32.lisp).

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

;;; Source load order:
;;;   arch-arm32-rpi → dwc2 → usb → cdc-ether → ip → crypto →
;;;   crypto-32 → crypto-w32 → ssh → aarch64-overrides → 32bit-overrides
;;;
;;; arch-arm32-rpi.lisp: DWC2 base, 32-bit object headers, low memory layout
;;; cdc-ether.lisp: provides e1000-send/e1000-receive/e1000-init (same API)
;;; crypto-w32.lisp MUST come after crypto.lisp + crypto-32.lisp
;;; 32bit-overrides.lisp: 30-bit fixnum safety overrides for crypto, SSH
(defvar *net-source*
  (format nil "~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%"
          (read-file-text (merge-pathnames "arch-arm32-rpi.lisp" *net-dir*))
          (read-file-text (merge-pathnames "dwc2.lisp" *net-dir*))
          (read-file-text (merge-pathnames "dwc2-32.lisp" *net-dir*))
          (read-file-text (merge-pathnames "usb.lisp" *net-dir*))
          (read-file-text (merge-pathnames "cdc-ether.lisp" *net-dir*))
          (read-file-text (merge-pathnames "ip.lisp" *net-dir*))
          (read-file-text (merge-pathnames "crypto.lisp" *net-dir*))
          (read-file-text (merge-pathnames "crypto-32.lisp" *net-dir*))
          (read-file-text (merge-pathnames "crypto-w32.lisp" *net-dir*))
          (read-file-text (merge-pathnames "ssh.lisp" *net-dir*))
          (read-file-text (merge-pathnames "aarch64-overrides.lisp" *net-dir*))
          (read-file-text (merge-pathnames "32bit-overrides.lisp" *net-dir*))
          (read-file-text (merge-pathnames "crypto-32-fast.lisp" *net-dir*))))

;;; ============================================================
;;; Build ARM32 SSH image (raspi2b + DWC2 USB + CDC Ethernet)
;;; ============================================================

(in-package :modus.mvm)

;; Install the ARMv7 RPi translator (PL011 UART at 0x3F201000)
(install-armv7-rpi-translator)

;; Ed25519 public key for private key = all zeros (32 bytes, little-endian)
;; Must store byte-by-byte because values like 0xBC overflow ARM32 fixnums
(let* ((ssh-main (format nil "~{~A~%~}"
                        (list
                         ;; Write 32-bit word (little-endian) at raw byte address
                         ;; Uses :u8 writes because ARM instruction encodings > 30-bit fixnum
                         "(defun write-word-raw (raw-addr b0 b1)"
                         "  (let ((a (* raw-addr 2)))"
                         "    (let ((bb0 b0))"
                         "      (let ((bb1 b1))"
                         "        (setf (mem-ref a :u8) (* (logand bb0 #xFF) 2))"
                         "        (setf (mem-ref (+ a 2) :u8) (* (ash bb0 -8) 2))"
                         "        (setf (mem-ref (+ a 4) :u8) (* (logand bb1 #xFF) 2))"
                         "        (setf (mem-ref (+ a 6) :u8) (* (ash bb1 -8) 2))))))"

                         "(defun write-exception-vectors ()"
                         ;; Write B . (0xEAFFFFFE) at all 8 vectors (0x00-0x1C)
                         "  (let ((i 0))"
                         "    (loop"
                         "      (when (>= i 32) (return 0))"
                         "      (write-word-raw i #xFFFE #xEAFF)"
                         "      (setq i (+ i 4))))"
                         ;; Vector 0x18 (IRQ): B 0x100 = branch to ISR
                         ;; Offset = (0x100 - 0x18 - 8) / 4 = 0x38 → EA000038
                         "  (write-word-raw #x18 #x0038 #xEA00))"

                         ;; ISR at 0x100: minimal — just return from IRQ
                         ;; IRQs should be masked (CPSR I=1), so this is safety only.
                         ;; If somehow IRQs fire, clear all HCINT and return.
                         "(defun write-isr ()"
                         ;; 0x100: SUB LR, LR, #4
                         "  (write-word-raw #x100 #xE004 #xE24E)"
                         ;; 0x104: PUSH {r0-r1, LR}
                         "  (write-word-raw #x104 #x4003 #xE92D)"
                         ;; 0x108: LDR r0, [PC, #0x10] → 0x120 (HCINT[0])
                         "  (write-word-raw #x108 #x0010 #xE59F)"
                         ;; 0x10C: MVN r1, #0
                         "  (write-word-raw #x10C #x1000 #xE3E0)"
                         ;; 0x110: STR r1, [r0] = clear HCINT[0]
                         "  (write-word-raw #x110 #x1000 #xE580)"
                         ;; 0x114: STR r1, [r0, #0x20] = clear HCINT[1]
                         "  (write-word-raw #x114 #x1020 #xE580)"
                         ;; 0x118: STR r1, [r0, #0x40] = clear HCINT[2]
                         "  (write-word-raw #x118 #x1040 #xE580)"
                         ;; 0x11C: POP {r0-r1, PC}^
                         "  (write-word-raw #x11C #x8003 #xE8FD)"
                         ;; Literal pool:
                         ;; 0x120: HCINT[0] = DWC2_base + 0x508 = 0x3F980508
                         "  (write-word-raw #x120 #x0508 #x3F98))"

                         ;; IRQ stack is set up in boot code (emit-armv7-rpi-entry)

                         "(defun kernel-main ()"
                         ;; Detect restart: if magic at ssh-ipc-base+0x60500 == 0xABCD, halt
                         "  (when (eq (mem-ref (+ (ssh-ipc-base) #x60500) :u32) #xABCD)"
                         "    (%serial-byte 82) (%serial-byte 83) (%serial-byte 84) (%serial-byte 10)"
                         "    (loop (%serial-byte 46) (io-delay)))"
                         "  (setf (mem-ref (+ (ssh-ipc-base) #x60500) :u32) #xABCD)"
                         "  (write-exception-vectors)"
                         "  (pci-assign-bars)"
                         "  (cdc-ether-init)"
                         "  (enable-dwc2-wfi)"
                         "  (setf (mem-ref (+ (ssh-ipc-base) #x60448) :u32) 0)"
                         "  (%serial-byte 91) (%serial-byte 49) (%serial-byte 93)"
                         "  (sha256-init)"
                         "  (%serial-byte 91) (%serial-byte 50) (%serial-byte 93)"
                         "  (sha512-init)"
                         "  (%serial-byte 91) (%serial-byte 51) (%serial-byte 93)"
                         "  (ed25519-init)"
                         "  (%serial-byte 91) (%serial-byte 52) (%serial-byte 93)"
                         "  (ssh-seed-random)"
                         "  (%serial-byte 91) (%serial-byte 53) (%serial-byte 93)"
                         "  (dhcp-client)"
                         "  (%serial-byte 91) (%serial-byte 54) (%serial-byte 93)"
                         "  (ssh-seed-random)"
                         "  (ssh-init-strings)"
                         ;; Embed pre-computed Ed25519 host key byte-by-byte
                         ;; Private key = 32 zero bytes
                         "  (let ((state (e1000-state-base)))"
                         "    (dotimes (i 32)"
                         "      (setf (mem-ref (+ state (+ #x710 i)) :u8) 0))"
                         ;; Public key stored byte-by-byte (fixnum-safe)
                         "    (setf (mem-ref (+ state #x730) :u8) #x3B)"
                         "    (setf (mem-ref (+ state #x731) :u8) #x6A)"
                         "    (setf (mem-ref (+ state #x732) :u8) #x27)"
                         "    (setf (mem-ref (+ state #x733) :u8) #xBC)"
                         "    (setf (mem-ref (+ state #x734) :u8) #xCE)"
                         "    (setf (mem-ref (+ state #x735) :u8) #xB6)"
                         "    (setf (mem-ref (+ state #x736) :u8) #xA4)"
                         "    (setf (mem-ref (+ state #x737) :u8) #x2D)"
                         "    (setf (mem-ref (+ state #x738) :u8) #x62)"
                         "    (setf (mem-ref (+ state #x739) :u8) #xA3)"
                         "    (setf (mem-ref (+ state #x73A) :u8) #xA8)"
                         "    (setf (mem-ref (+ state #x73B) :u8) #xD0)"
                         "    (setf (mem-ref (+ state #x73C) :u8) #x2A)"
                         "    (setf (mem-ref (+ state #x73D) :u8) #x6F)"
                         "    (setf (mem-ref (+ state #x73E) :u8) #x0D)"
                         "    (setf (mem-ref (+ state #x73F) :u8) #x73)"
                         "    (setf (mem-ref (+ state #x740) :u8) #x65)"
                         "    (setf (mem-ref (+ state #x741) :u8) #x32)"
                         "    (setf (mem-ref (+ state #x742) :u8) #x15)"
                         "    (setf (mem-ref (+ state #x743) :u8) #x77)"
                         "    (setf (mem-ref (+ state #x744) :u8) #x1D)"
                         "    (setf (mem-ref (+ state #x745) :u8) #xE2)"
                         "    (setf (mem-ref (+ state #x746) :u8) #x43)"
                         "    (setf (mem-ref (+ state #x747) :u8) #xA6)"
                         "    (setf (mem-ref (+ state #x748) :u8) #x3A)"
                         "    (setf (mem-ref (+ state #x749) :u8) #xC0)"
                         "    (setf (mem-ref (+ state #x74A) :u8) #x48)"
                         "    (setf (mem-ref (+ state #x74B) :u8) #xA1)"
                         "    (setf (mem-ref (+ state #x74C) :u8) #x8B)"
                         "    (setf (mem-ref (+ state #x74D) :u8) #x59)"
                         "    (setf (mem-ref (+ state #x74E) :u8) #xDA)"
                         "    (setf (mem-ref (+ state #x74F) :u8) #x29)"
                         "    (setf (mem-ref (+ state #x624) :u32) 1))"
                         "  (%serial-byte 83) (%serial-byte 83) (%serial-byte 72)"
                         "  (%serial-byte 58) (print-dec 22) (%serial-byte 10)"
                         ;; Pre-computed Ed25519 host key derivatives
                         ;; s (clamped SHA-512 of zeros) at state+0x680
                         "  (let ((state (e1000-state-base)))"
                         "    (setf (mem-ref (+ state #x680) :u8) #x50)"
                         "    (setf (mem-ref (+ state #x681) :u8) #x46)"
                         "    (setf (mem-ref (+ state #x682) :u8) #xAD)"
                         "    (setf (mem-ref (+ state #x683) :u8) #xC1)"
                         "    (setf (mem-ref (+ state #x684) :u8) #xDB)"
                         "    (setf (mem-ref (+ state #x685) :u8) #xA8)"
                         "    (setf (mem-ref (+ state #x686) :u8) #x38)"
                         "    (setf (mem-ref (+ state #x687) :u8) #x86)"
                         "    (setf (mem-ref (+ state #x688) :u8) #x7B)"
                         "    (setf (mem-ref (+ state #x689) :u8) #x2B)"
                         "    (setf (mem-ref (+ state #x68A) :u8) #xBB)"
                         "    (setf (mem-ref (+ state #x68B) :u8) #xFD)"
                         "    (setf (mem-ref (+ state #x68C) :u8) #xD0)"
                         "    (setf (mem-ref (+ state #x68D) :u8) #xC3)"
                         "    (setf (mem-ref (+ state #x68E) :u8) #x42)"
                         "    (setf (mem-ref (+ state #x68F) :u8) #x3E)"
                         "    (setf (mem-ref (+ state #x690) :u8) #x58)"
                         "    (setf (mem-ref (+ state #x691) :u8) #xB5)"
                         "    (setf (mem-ref (+ state #x692) :u8) #x79)"
                         "    (setf (mem-ref (+ state #x693) :u8) #x70)"
                         "    (setf (mem-ref (+ state #x694) :u8) #xB5)"
                         "    (setf (mem-ref (+ state #x695) :u8) #x26)"
                         "    (setf (mem-ref (+ state #x696) :u8) #x7A)"
                         "    (setf (mem-ref (+ state #x697) :u8) #x90)"
                         "    (setf (mem-ref (+ state #x698) :u8) #xF5)"
                         "    (setf (mem-ref (+ state #x699) :u8) #x79)"
                         "    (setf (mem-ref (+ state #x69A) :u8) #x60)"
                         "    (setf (mem-ref (+ state #x69B) :u8) #x92)"
                         "    (setf (mem-ref (+ state #x69C) :u8) #x4A)"
                         "    (setf (mem-ref (+ state #x69D) :u8) #x87)"
                         "    (setf (mem-ref (+ state #x69E) :u8) #xF1)"
                         "    (setf (mem-ref (+ state #x69F) :u8) #x56)"
                         ;; prefix at state+0x6A0
                         "    (setf (mem-ref (+ state #x6A0) :u8) #x0A)"
                         "    (setf (mem-ref (+ state #x6A1) :u8) #x6A)"
                         "    (setf (mem-ref (+ state #x6A2) :u8) #x85)"
                         "    (setf (mem-ref (+ state #x6A3) :u8) #xEA)"
                         "    (setf (mem-ref (+ state #x6A4) :u8) #xA6)"
                         "    (setf (mem-ref (+ state #x6A5) :u8) #x42)"
                         "    (setf (mem-ref (+ state #x6A6) :u8) #xDA)"
                         "    (setf (mem-ref (+ state #x6A7) :u8) #xC8)"
                         "    (setf (mem-ref (+ state #x6A8) :u8) #x35)"
                         "    (setf (mem-ref (+ state #x6A9) :u8) #x42)"
                         "    (setf (mem-ref (+ state #x6AA) :u8) #x4B)"
                         "    (setf (mem-ref (+ state #x6AB) :u8) #x5D)"
                         "    (setf (mem-ref (+ state #x6AC) :u8) #x7C)"
                         "    (setf (mem-ref (+ state #x6AD) :u8) #x8D)"
                         "    (setf (mem-ref (+ state #x6AE) :u8) #x63)"
                         "    (setf (mem-ref (+ state #x6AF) :u8) #x7C)"
                         "    (setf (mem-ref (+ state #x6B0) :u8) #x00)"
                         "    (setf (mem-ref (+ state #x6B1) :u8) #x40)"
                         "    (setf (mem-ref (+ state #x6B2) :u8) #x8C)"
                         "    (setf (mem-ref (+ state #x6B3) :u8) #x7A)"
                         "    (setf (mem-ref (+ state #x6B4) :u8) #x73)"
                         "    (setf (mem-ref (+ state #x6B5) :u8) #xDA)"
                         "    (setf (mem-ref (+ state #x6B6) :u8) #x67)"
                         "    (setf (mem-ref (+ state #x6B7) :u8) #x2B)"
                         "    (setf (mem-ref (+ state #x6B8) :u8) #x7F)"
                         "    (setf (mem-ref (+ state #x6B9) :u8) #x49)"
                         "    (setf (mem-ref (+ state #x6BA) :u8) #x85)"
                         "    (setf (mem-ref (+ state #x6BB) :u8) #x21)"
                         "    (setf (mem-ref (+ state #x6BC) :u8) #x42)"
                         "    (setf (mem-ref (+ state #x6BD) :u8) #x0B)"
                         "    (setf (mem-ref (+ state #x6BE) :u8) #x6D)"
                         "    (setf (mem-ref (+ state #x6BF) :u8) #xD3)"
                         ;; Mark as pre-computed
                         "    (setf (mem-ref (+ state #x6C0) :u32) 1))"
                         ;; Set SSH port and clear connections
                         "  (setf (mem-ref (+ (ssh-ipc-base) #x60438) :u32) 22)"
                         "  (let ((i 0))"
                         "    (loop"
                         "      (when (>= i 4) (return 0))"
                         "      (setf (mem-ref (conn-base i) :u32) 0)"
                         "      (setq i (+ i 1))))"
                         ;; Pre-computed server ephemeral X25519 key pair
                         "  (let ((state (e1000-state-base)))"
                         "    (setf (mem-ref (+ state #x6C4) :u8) #x00)"
                         "    (dotimes (i 30)"
                         "      (setf (mem-ref (+ state (+ #x6C5 i)) :u8) #x01))"
                         "    (setf (mem-ref (+ state #x6E3) :u8) #x41)"
                         "    (setf (mem-ref (+ state #x6E4) :u8) #xA4)"
                         "    (setf (mem-ref (+ state #x6E5) :u8) #xE0)"
                         "    (setf (mem-ref (+ state #x6E6) :u8) #x92)"
                         "    (setf (mem-ref (+ state #x6E7) :u8) #x92)"
                         "    (setf (mem-ref (+ state #x6E8) :u8) #xB6)"
                         "    (setf (mem-ref (+ state #x6E9) :u8) #x51)"
                         "    (setf (mem-ref (+ state #x6EA) :u8) #xC2)"
                         "    (setf (mem-ref (+ state #x6EB) :u8) #x78)"
                         "    (setf (mem-ref (+ state #x6EC) :u8) #xB9)"
                         "    (setf (mem-ref (+ state #x6ED) :u8) #x77)"
                         "    (setf (mem-ref (+ state #x6EE) :u8) #x2C)"
                         "    (setf (mem-ref (+ state #x6EF) :u8) #x56)"
                         "    (setf (mem-ref (+ state #x6F0) :u8) #x9F)"
                         "    (setf (mem-ref (+ state #x6F1) :u8) #x5F)"
                         "    (setf (mem-ref (+ state #x6F2) :u8) #xA9)"
                         "    (setf (mem-ref (+ state #x6F3) :u8) #xBB)"
                         "    (setf (mem-ref (+ state #x6F4) :u8) #x13)"
                         "    (setf (mem-ref (+ state #x6F5) :u8) #xD9)"
                         "    (setf (mem-ref (+ state #x6F6) :u8) #x06)"
                         "    (setf (mem-ref (+ state #x6F7) :u8) #xB4)"
                         "    (setf (mem-ref (+ state #x6F8) :u8) #x6A)"
                         "    (setf (mem-ref (+ state #x6F9) :u8) #xB6)"
                         "    (setf (mem-ref (+ state #x6FA) :u8) #x8C)"
                         "    (setf (mem-ref (+ state #x6FB) :u8) #x9D)"
                         "    (setf (mem-ref (+ state #x6FC) :u8) #xF9)"
                         "    (setf (mem-ref (+ state #x6FD) :u8) #xDC)"
                         "    (setf (mem-ref (+ state #x6FE) :u8) #x2B)"
                         "    (setf (mem-ref (+ state #x6FF) :u8) #x44)"
                         "    (setf (mem-ref (+ state #x700) :u8) #x09)"
                         "    (setf (mem-ref (+ state #x701) :u8) #xF8)"
                         "    (setf (mem-ref (+ state #x702) :u8) #xA2)"
                         "    (setf (mem-ref (+ state #x703) :u8) #x09))"
                         ;; Enable ARM timer for WFI-based io-delay (after all crypto init)
                         "  (enable-arm32-timer)"
                         "  (net-actor-main))"
                         ;; Override net-actor-main for single-threaded SSH
                         "(defun net-actor-main ()"
                         "  (%serial-byte 76) (%serial-byte 10)"
                         "  (let ((cnt 0))"
                         "  (loop"
                         "    (io-delay)"
                         "    (setq cnt (+ cnt 1))"
                         "    (when (eq (logand cnt #x3FFF) 0)"
                         "      (%serial-byte 46))"
                         "    (let ((pkt-len (e1000-receive)))"
                         "      (when (not (zerop pkt-len))"
                         "        (let ((buf (e1000-rx-buf)))"
                         "          (let ((et-hi (mem-ref (+ buf 12) :u8)))"
                         "            (when (eq et-hi #x08)"
                         "              (let ((et-lo (mem-ref (+ buf 13) :u8)))"
                         "                (if (eq et-lo #x06)"
                         "                    (let ((arp-op (buf-read-u16-mem buf 20)))"
                         "                      (when (eq arp-op 1)"
                         "                        (arp-reply buf)))"
                         "                    (when (eq et-lo 0)"
                         "                      (let ((st (e1000-state-base)))"
                         "                        (dotimes (m 6)"
                         "                          (setf (mem-ref (+ st (+ #x28 m)) :u8)"
                         "                                (mem-ref (+ buf (+ 6 m)) :u8))))"
                         "                      (let ((proto (mem-ref (+ buf 23) :u8)))"
                         "                        (if (eq proto 17)"
                         "                            (udp-handle buf 14)"
                         "                            (when (eq proto 6)"
                         "                              (net-handle-tcp buf pkt-len)))))))))))))))")))
       ;; Override ssh-handle-kex with usb-keepalive calls for DWC2
       ;; Args let-bound to survive function calls (MVM arg-clobber)
       (kex-override "
(defun ssh-handle-kex (ssh kex-init-payload kex-init-len)
  (let ((s ssh))
    (let ((kip kex-init-payload))
      (let ((cli-eph (make-array 32)))
        (dotimes (i 32) (aset cli-eph i (aref kip (+ 5 i))))
        (let ((state (e1000-state-base)))
          (let ((srv-priv (make-array 32)))
            (dotimes (i 32)
              (aset srv-priv i (mem-ref (+ state (+ #x6C4 i)) :u8)))
            (let ((srv-eph (make-array 32)))
              (dotimes (i 32)
                (aset srv-eph i (mem-ref (+ state (+ #x6E4 i)) :u8)))
              (let ((shared (x25519 srv-priv cli-eph)))
                (usb-keepalive)
                (ssh-mem-store (+ s #x070) shared 32)
                (let ((h (ssh-compute-exchange-hash s cli-eph srv-eph shared)))
                  (ssh-mem-store (+ s #x050) h 32)
                  (when (zerop (mem-ref s :u32))
                    (ssh-mem-store (+ s #x030) h 32)
                    (setf (mem-ref s :u32) 1))
                  (usb-keepalive)
                  (let ((sig (ed25519-sign-fast h 32)))
                    (usb-keepalive)
                    (ssh-send-kex-reply s sig srv-eph)))))))))))
")
       ;; usb-keepalive: no-op for now — e1000-receive/dwc2-poll-bulk-in hangs
       ;; during SSH kex on ARM32. The hardware mul26 opcodes make crypto fast
       ;; enough that SLIRP shouldn't timeout.
       (keepalive-override "(defun usb-keepalive () 0)
(defun enable-dwc2-wfi ()
  (setf (mem-ref (+ #x3F98050C) :u32) 3)
  (setf (mem-ref (+ #x3F980018) :u32) #x02000000)
  (setf (mem-ref #x3F00B210 :u32) #x200)
  (setf (mem-ref (+ #x01090000 #x0C) :u32) 1))
(defun io-delay ()
  (if (zerop (mem-ref (+ #x01090000 #x0C) :u32))
      0
      (wfi)))
(defun mmio-delay ()
  (let ((i 0))
    (loop (when (>= i 50) (return 0)) (setq i (+ i 1)))))
(defun net-wait-ack (conn)
  (let ((cb (conn-base conn)))
    (let ((acked 0))
      (let ((tries 0))
        (loop
          (when (not (zerop acked)) (return 1))
          (when (> tries 500) (return 0))
          (io-delay)
          (let ((pkt-len (e1000-receive)))
            (when (not (zerop pkt-len))
              (let ((b2 (e1000-rx-buf)))
                (when (eq (mem-ref (+ b2 12) :u8) #x08)
                  (when (eq (mem-ref (+ b2 13) :u8) 0)
                    (when (eq (mem-ref (+ b2 23) :u8) 6)
                      (let ((f2 (mem-ref (+ b2 47) :u8)))
                        (when (eq (logand f2 #x10) #x10)
                          (setf (mem-ref cb :u32) 2)
                          (net-deliver-data conn b2 pkt-len f2)
                          (setq acked 1)))))))))
          (setq tries (+ tries 1)))))))
(defun dwc2-poll-channel (ch)
  (let ((result (- 0 3)))
    (let ((i 0))
      (loop
        (when (>= i 50000)
          (dwc2-halt-channel ch)
          (return nil))
        (mmio-delay)
        (let ((hcint (dwc2-read (dwc2-hcint ch))))
          (when (not (zerop (logand hcint (hcint-chhltd))))
            (if (not (zerop (logand hcint (hcint-xfercompl))))
                (setq result 1)
                (if (not (zerop (logand hcint (hcint-stall))))
                    (setq result (- 0 1))
                    (if (not (zerop (logand hcint (hcint-nak))))
                        (setq result 0)
                        (setq result (- 0 2)))))
            (dwc2-write-allones (dwc2-hcint ch))
            (return nil))
          (when (not (zerop (logand hcint (hcint-xfercompl))))
            (setq result 1)
            (dwc2-write-allones (dwc2-hcint ch))
            (return nil))
          (when (not (zerop (logand hcint (hcint-nak))))
            (dwc2-write (dwc2-hcint ch) (hcint-nak))))
        (setq i (+ i 1))))
    result))
(defun dwc2-halt-channel (ch)
  (let ((hcchar (dwc2-read (dwc2-hcchar ch))))
    (dwc2-write (dwc2-hcchar ch)
                (logior hcchar (logior (hcchar-chena) (hcchar-chdis)))))
  (let ((j 0))
    (loop
      (when (>= j 10000) (return nil))
      (mmio-delay)
      (when (not (zerop (logand (dwc2-read (dwc2-hcint ch)) (hcint-chhltd))))
        (return nil))
      (setq j (+ j 1))))
  (dwc2-write-allones (dwc2-hcint ch)))
(defun e1000-receive ()
  (let ((hctsiz-before (dwc2-read (dwc2-hctsiz 1))))
    (let ((result (usb-bulk-receive (cdc-rx-buf-addr) 2048)))
      (if (eq result 1)
          (let ((remaining (logand hctsiz-before #x7FFFF)))
            (let ((actual (- 2048 remaining)))
              (setf (mem-ref (+ (e1000-state-base) #x44) :u32) actual)
              actual))
          (if (zerop result)
              (progn
                (mmio-delay)
                (let ((hctsiz2 (dwc2-read (dwc2-hctsiz 1))))
                  (let ((r2 (usb-bulk-receive (cdc-rx-buf-addr) 2048)))
                    (if (eq r2 1)
                        (let ((rem2 (logand hctsiz2 #x7FFFF)))
                          (let ((act2 (- 2048 rem2)))
                            (setf (mem-ref (+ (e1000-state-base) #x44) :u32) act2)
                            act2))
                        0))))
              0)))))
")       ;; Override receive for ARM32
       (receive-override "
(defun receive ()
  (io-delay)
  (let ((pkt-len (e1000-receive)))
    (if (zerop pkt-len)
        1
        (let ((buf (e1000-rx-buf)))
          (let ((et-hi (mem-ref (+ buf 12) :u8)))
            (let ((et-lo (mem-ref (+ buf 13) :u8)))
              (if (eq et-hi #x08)
                  (if (eq et-lo #x06)
                      (let ((arp-op (buf-read-u16-mem buf 20)))
                        (when (eq arp-op 1) (arp-reply buf)))
                      (when (eq et-lo 0)
                        (let ((st (e1000-state-base)))
                          (dotimes (m 6)
                            (setf (mem-ref (+ st (+ #x28 m)) :u8)
                                  (mem-ref (+ buf (+ 6 m)) :u8))))
                        (let ((proto (mem-ref (+ buf 23) :u8)))
                          (if (eq proto 17)
                              (udp-handle buf 14)
                              (when (eq proto 6)
                                (net-handle-tcp buf pkt-len))))))
                  ())))
          1))))
")
       ;; SSH receive-packet override: clean version with no debug markers.
       ;; Args let-bound to survive function calls (MVM arg-clobber).
       (recv-override "
(defun ssh-receive-packet (ssh timeout)
  (let ((s ssh))
    (let ((tmo timeout))
      (let ((cb (- s #x20)))
        (if (zerop (mem-ref (+ s #x0C) :u32))
            (let ((tries 0) (result ()))
              (loop
                (when result (return result))
                (when (> tries tmo) (return 0))
                (let ((blen (mem-ref (+ s #x6D4) :u32)))
                  (if (> blen 4)
                      (let ((arr (ssh-buf-to-array s blen)))
                        (let ((pkt-len (ssh-get-u32 arr 0)))
                          (if (not (< blen (+ 4 pkt-len)))
                              (let ((parsed (ssh-parse-packet s arr blen)))
                                (when parsed
                                  (ssh-buf-consume s (+ 4 pkt-len))
                                  (setf (mem-ref (+ s #x04) :u32)
                                        (+ (mem-ref (+ s #x04) :u32) 1))
                                  (setq result parsed)))
                              (receive))))
                      (receive)))
                (setq tries (+ tries 1))))
            (let ((tries 0) (result ()))
              (loop
                (when result (return result))
                (when (> tries tmo) (return 0))
                (let ((blen (mem-ref (+ s #x6D4) :u32)))
                  (if (> blen 20)
                      (let ((arr (ssh-buf-to-array s blen)))
                        (let ((dec (ssh-decrypt-packet s arr blen)))
                          (if dec
                              (progn
                                (ssh-buf-consume s
                                  (- blen (mem-ref (+ cb #x16F8) :u32)))
                                (setq result dec))
                              (receive))))
                      (receive)))
                (setq tries (+ tries 1)))))))))
")
       ;; SSH connection handler + message loop (no diagnostic markers)
       (ssh-overrides "
(defun ssh-message-loop (ssh)
  (let ((s ssh))
    (let ((flag-addr (+ (conn-ssh 3) #x700)))
      (setf (mem-ref flag-addr :u32) 1)
      (loop
        (when (zerop (mem-ref flag-addr :u32)) (return ()))
        (let ((pkt (ssh-receive-packet s 50000)))
          (when pkt
            (let ((payload (car pkt)))
              (let ((plen (cdr pkt)))
                (ssh-dispatch-msg s payload plen flag-addr)))))))))
(defun ssh-handle-connection-kex (ssh cb)
  (let ((s ssh))
    (let ((kex-init (ssh-receive-packet s 5000000)))
      (when (zerop kex-init) (return ()))
      (let ((kex-payload (car kex-init)))
        (let ((kex-len (cdr kex-init)))
          (when (not (eq (aref kex-payload 0) 30)) (return ()))
          (ssh-handle-kex s kex-payload kex-len)
          (ssh-send-newkeys s)
          (let ((nk (ssh-receive-nk s)))
            (when (zerop nk) (return ()))
            (let ((nk-payload (car nk)))
              (when (not (eq (aref nk-payload 0) 21)) (return ()))
              (ssh-derive-keys s)
              (ssh-message-loop s))))))))
(defun ssh-receive-nk (ssh)
  (let ((s ssh))
    (let ((tries 0))
      (let ((result ()))
        (loop
          (when result (return result))
          (when (> tries 5000000) (return 0))
          (let ((blen (mem-ref (+ s #x6D4) :u32)))
            (if (> blen 4)
                (let ((arr (ssh-buf-to-array s blen)))
                  (let ((pkt-len (ssh-get-u32 arr 0)))
                    (if (not (< blen (+ 4 pkt-len)))
                        (let ((parsed (ssh-parse-packet s arr blen)))
                          (when parsed
                            (ssh-buf-consume s (+ 4 pkt-len))
                            (setf (mem-ref (+ s #x04) :u32)
                                  (+ (mem-ref (+ s #x04) :u32) 1))
                            (setq result parsed)))
                        (receive))))
                (receive)))
          (setq tries (+ tries 1)))))))
(defun ssh-handle-connection (ssh)
  (let ((s ssh))
    (let ((cb (- s #x20)))
      (ssh-send-version s)
      (let ((rv (ssh-receive-version s)))
        (when (zerop rv) (return ())))
      (let ((kexinit (ssh-build-kexinit s)))
        (let ((kexlen (array-length kexinit)))
          (ssh-send-payload s kexinit kexlen)))
      (let ((cli-kex (ssh-receive-packet s 5000000)))
        (when (zerop cli-kex) (return ()))
        (let ((cli-kex-payload (car cli-kex)))
          (let ((cli-kex-len (cdr cli-kex)))
            (when (not (eq (aref cli-kex-payload 0) 20)) (return ()))
            (ssh-mem-store (+ cb #x1F00) cli-kex-payload cli-kex-len)
            (setf (mem-ref (+ s #x20) :u32) cli-kex-len)
            (ssh-handle-connection-kex s cb)))))))
(defun ssh-connection-handler (conn)
  (let ((ssh (conn-ssh conn)))
    (let ((cb (conn-base conn)))
      (ssh-handle-connection ssh)
      (tcp-close-conn cb)
      (conn-free conn))))
")
       (x25519-override "
(defun fe-sq-iter (dst n)
  (dotimes (i n)
    (fe-sq dst dst)))

;;; fe-invert split into 3 parts to stay under 25 sequential forms
(defun fe-invert-p1 (z z2 z9 z11 t0 t1)
  (let ((zz z))
    (let ((zz2 z2))
      (let ((zz9 z9))
        (let ((zz11 z11))
          (let ((tt0 t0))
            (let ((tt1 t1))
              (fe-sq zz2 zz)
              (fe-sq tt0 zz2)
              (fe-sq tt1 tt0)
              (fe-mul zz9 zz tt1)
              (fe-mul zz11 zz2 zz9)
              (fe-sq tt0 zz11)
              (fe-mul tt0 zz9 tt0)
              (fe-sq tt1 tt0)
              (fe-sq-iter tt1 4)
              (fe-mul tt1 tt1 tt0)
              (fe-sq zz2 tt1)
              (fe-sq-iter zz2 9)
              (fe-mul zz2 zz2 tt1))))))))

(defun fe-invert-p2 (z2 z9 t0 t1)
  (let ((zz2 z2))
    (let ((zz9 z9))
      (let ((tt0 t0))
        (let ((tt1 t1))
          (fe-sq zz9 zz2)
          (fe-sq-iter zz9 19)
          (fe-mul zz9 zz9 zz2)
          (fe-sq tt0 zz9)
          (fe-sq-iter tt0 9)
          (fe-mul tt0 tt0 tt1)
          (fe-sq tt1 tt0)
          (fe-sq-iter tt1 49)
          (fe-mul tt1 tt1 tt0)
          (fe-sq zz2 tt1)
          (fe-sq-iter zz2 99)
          (fe-mul zz2 zz2 tt1))))))

(defun fe-invert-p3 (z2 z9 z11 t0)
  (let ((zz2 z2))
    (let ((zz9 z9))
      (let ((zz11 z11))
        (let ((tt0 t0))
          (fe-sq zz9 zz2)
          (fe-sq-iter zz9 49)
          (fe-mul zz9 zz9 tt0)
          (fe-sq zz9 zz9)
          (fe-sq-iter zz9 4)
          (fe-mul tt0 zz9 zz11)
          tt0)))))

(defun fe-invert (z)
  (let ((zz z))
    (let ((z2 (make-array 40)))
      (let ((z9 (make-array 40)))
        (let ((z11 (make-array 40)))
          (let ((t0 (make-array 40)))
            (let ((t1 (make-array 40)))
              (fe-invert-p1 zz z2 z9 z11 t0 t1)
              (fe-invert-p2 z2 z9 t0 t1)
              (fe-invert-p3 z2 z9 z11 t0))))))))

;;; x25519 context: array of 18 pointers to fe arrays
;;; ctx[0]=kc, [1]=x1, [2]=x2, [3]=z2, [4]=x3, [5]=z3,
;;; [6]=a, [7]=aa, [8]=b, [9]=bb, [10]=fe-e, [11]=c,
;;; [12]=d, [13]=da, [14]=cb, [15]=t1, [16]=t2, [17]=a24
(defun x25-alloc-ctx ()
  (let ((ctx (make-array 18)))
    (aset ctx 0 (make-array 32))
    (x25-alloc-ctx2 ctx)))

(defun x25-alloc-ctx2 (ctx)
  (let ((c ctx))
    (let ((i 1))
      (loop
        (when (> i 17) (return c))
        (aset c i (make-array 40))
        (setq i (+ i 1))))))

(defun x25-ladder-step (ctx)
  (let ((c ctx))
    (let ((x2 (aref c 2)))
      (let ((z2 (aref c 3)))
        (let ((x3 (aref c 4)))
          (let ((z3 (aref c 5)))
            (let ((a (aref c 6)))
              (let ((aa (aref c 7)))
                (let ((b (aref c 8)))
                  (let ((bb (aref c 9)))
                    (fe-add a x2 z2)
                    (fe-sq aa a)
                    (fe-sub b x2 z2)
                    (fe-sq bb b)
                    (x25-step-mid c)))))))))))

(defun x25-step-mid (ctx)
  (let ((c ctx))
    (let ((x3 (aref c 4)))
      (let ((z3 (aref c 5)))
        (let ((aa (aref c 7)))
          (let ((bb (aref c 9)))
            (let ((fe-e (aref c 10)))
              (let ((cc (aref c 11)))
                (let ((d (aref c 12)))
                  (fe-sub fe-e aa bb)
                  (fe-add cc x3 z3)
                  (fe-sub d x3 z3)
                  (fe-mul (aref c 13) d (aref c 6))
                  (fe-mul (aref c 14) cc (aref c 8))
                  (x25-step-fin c))))))))))

(defun x25-step-fin (ctx)
  (let ((c ctx))
    (let ((t1 (aref c 15)))
      (let ((da (aref c 13)))
        (let ((cb (aref c 14)))
          (let ((t2 (aref c 16)))
            (fe-add t1 da cb)
            (fe-sq (aref c 4) t1)
            (fe-sub t1 da cb)
            (fe-sq t2 t1)
            (fe-mul (aref c 5) (aref c 1) t2)
            (fe-mul (aref c 2) (aref c 7) (aref c 9))
            (fe-mul t1 (aref c 17) (aref c 10))
            (fe-add t2 (aref c 7) t1)
            (fe-mul (aref c 3) (aref c 10) t2)))))))

(defun x25-cswap (ctx)
  (let ((c ctx))
    (let ((t1 (aref c 15)))
      (fe-copy t1 (aref c 2))
      (fe-copy (aref c 2) (aref c 4))
      (fe-copy (aref c 4) t1)
      (fe-copy t1 (aref c 3))
      (fe-copy (aref c 3) (aref c 5))
      (fe-copy (aref c 5) t1))))

(defun x25-get-bit (kc pos)
  (let ((k kc))
    (let ((p pos))
      (let ((byte-idx (ash p -3)))
        (let ((bit-idx (logand p 7)))
          (let ((byte-val (aref k byte-idx)))
            (let ((mask (ash 1 bit-idx)))
              (if (zerop (logand byte-val mask)) 0 1))))))))

(defun x25-loop-iter (ctx kc swap pos)
  (let ((c ctx))
    (let ((k kc))
      (let ((s swap))
        (let ((p pos))
          (let ((kt (x25-get-bit k p)))
            (when (not (= kt s))
              (x25-cswap c))
            (setq s kt)
            (x25-ladder-step c)
            s))))))

(defun x25-loop (ctx kc)
  (let ((c ctx))
    (let ((k kc))
      (let ((swap 0))
        (let ((pos 254))
          (dotimes (iter 255)
            (setq swap (x25-loop-iter c k swap pos))
            (setq pos (- pos 1)))
          (x25-loop-fin c swap))))))

(defun x25-loop-fin (ctx swap)
  (let ((c ctx))
    (when (not (zerop swap))
      (x25-cswap c))
    (fe-mul (aref c 15) (aref c 2) (fe-invert (aref c 3)))
    (fe-to-bytes (aref c 15))))

(defun x25-init (ctx k u)
  (let ((c ctx))
    (let ((kk k))
      (let ((uu u))
        (let ((kc (aref c 0)))
          (dotimes (i 32) (aset kc i (aref kk i)))
          (aset kc 0 (logand (aref kc 0) 248))
          (aset kc 31 (logand (aref kc 31) 127))
          (aset kc 31 (logior (aref kc 31) 64))
          (fe-copy (aref c 1) (fe-from-bytes uu))
          (x25-init2 c kc))))))

(defun x25-init2 (ctx kc)
  (let ((c ctx))
    (let ((k kc))
      (let ((x2 (aref c 2)))
        (let ((z2 (aref c 3)))
          (let ((z3 (aref c 5)))
            (let ((a24 (aref c 17)))
              (dotimes (i 40) (aset x2 i 0))
              (let ((x2b (+ (ash (logand x2 (- 0 4)) 1) 4)))
                (setf (mem-ref x2b :u32) 1))
              (dotimes (i 40) (aset z2 i 0))
              (fe-copy (aref c 4) (aref c 1))
              (dotimes (i 40) (aset z3 i 0))
              (let ((z3b (+ (ash (logand z3 (- 0 4)) 1) 4)))
                (setf (mem-ref z3b :u32) 1))
              (dotimes (i 40) (aset a24 i 0))
              (let ((a24b (+ (ash (logand a24 (- 0 4)) 1) 4)))
                (setf (mem-ref a24b :u32) 121665))
              k)))))))

(defun x25519 (k u)
  (let ((ctx (x25-alloc-ctx)))
    (let ((kc (x25-init ctx k u)))
      (x25-loop ctx kc))))

")
       (x25519-override2 "
;;; 30-bit fixnum safe overrides for crypto-32-fast.lisp
;;; Uses 3-word accumulators (hi2, hi, lo) at 12 bytes per h[k]
;;; Result area shifts to +0xDC

;; Clear 3-word h[k]
(defun fh-clear (base k)
  (let ((addr (+ base (+ #x64 (* k 12)))))
    (setf (mem-ref addr :u32) 0)
    (setf (mem-ref (+ addr 4) :u32) 0)
    (setf (mem-ref (+ addr 8) :u32) 0)))

;; Accumulate (rhi2, rhi, rlo) into 3-word h[k]
(defun fh-acc-3 (base k rhi2 rhi rlo)
  (let ((b base))
    (let ((addr (+ b (+ #x64 (* k 12)))))
      (let ((lo (+ (mem-ref (+ addr 8) :u32) rlo)))
        (let ((carry-lo (ash lo -26)))
          (setf (mem-ref (+ addr 8) :u32) (logand lo 67108863))
          (let ((hi (+ (mem-ref (+ addr 4) :u32) (+ rhi carry-lo))))
            (let ((carry-hi (ash hi -26)))
              (setf (mem-ref (+ addr 4) :u32) (logand hi 67108863))
              (setf (mem-ref addr :u32)
                    (+ (mem-ref addr :u32) (+ rhi2 carry-hi))))))))))

;; Plain: f[fi] * g[gi] → accumulate to h[k]
(defun fm-plain (base k fi gi)
  (let ((b base))
    (let ((kk k))
      (let ((a (fs-read-f b fi)))
        (let ((bb (fs-read-g b gi)))
          (let ((mlo (mul26lo a bb)))
            (let ((mhi (mul26hi a bb)))
              (fh-acc-3 b kk 0 mhi mlo))))))))

;; Multiply (mhi, mlo) by 19 via hardware mul, accumulate to h[k]
(defun fmul19-acc-3 (base k mhi mlo)
  (let ((b base))
    (let ((kk k))
      (let ((lo19-lo (mul26lo mlo 19)))
        (let ((lo19-hi (mul26hi mlo 19)))
          (let ((hi19-lo (mul26lo mhi 19)))
            (let ((hi19-hi (mul26hi mhi 19)))
              (let ((rhi (+ hi19-lo lo19-hi)))
                (fh-acc-3 b kk hi19-hi rhi lo19-lo)))))))))

;; Reduced: f[fi] * g[gi] * 19 → accumulate to h[k]
(defun fm-red (base k fi gi)
  (let ((b base))
    (let ((kk k))
      (let ((a (fs-read-f b fi)))
        (let ((bb (fs-read-g b gi)))
          (let ((mlo (mul26lo a bb)))
            (let ((mhi (mul26hi a bb)))
              (fmul19-acc-3 b kk mhi mlo))))))))

;; Carry: 26-bit step with 3-word h[k]
(defun fc-step26 (base k)
  (let ((b base))
    (let ((addr (+ b (+ #x64 (* k 12)))))
      (let ((hi2 (mem-ref addr :u32)))
        (let ((hi (mem-ref (+ addr 4) :u32)))
          (let ((lo (mem-ref (+ addr 8) :u32)))
            (setf (mem-ref (+ b (+ #xDC (* k 4))) :u32) lo)
            (let ((naddr (+ addr 12)))
              (let ((nlo (+ (mem-ref (+ naddr 8) :u32) hi)))
                (let ((carry (ash nlo -26)))
                  (setf (mem-ref (+ naddr 8) :u32) (logand nlo 67108863))
                  (let ((nhi (+ (mem-ref (+ naddr 4) :u32) (+ hi2 carry))))
                    (let ((carry2 (ash nhi -26)))
                      (setf (mem-ref (+ naddr 4) :u32) (logand nhi 67108863))
                      (setf (mem-ref naddr :u32)
                            (+ (mem-ref naddr :u32) carry2)))))))))))))

;; Carry: 25-bit step with 3-word h[k]
(defun fc-step25 (base k)
  (let ((b base))
    (let ((addr (+ b (+ #x64 (* k 12)))))
      (let ((hi2 (mem-ref addr :u32)))
        (let ((hi (mem-ref (+ addr 4) :u32)))
          (let ((lo (mem-ref (+ addr 8) :u32)))
            (setf (mem-ref (+ b (+ #xDC (* k 4))) :u32) (logand lo 33554431))
            (let ((c-lo (ash lo -25)))
              (let ((carry-a (+ (* hi 2) c-lo)))
                (let ((naddr (+ addr 12)))
                  (let ((nlo (+ (mem-ref (+ naddr 8) :u32) carry-a)))
                    (let ((carry (ash nlo -26)))
                      (setf (mem-ref (+ naddr 8) :u32) (logand nlo 67108863))
                      (let ((nhi (+ (mem-ref (+ naddr 4) :u32) (+ (* hi2 2) carry))))
                        (let ((carry2 (ash nhi -26)))
                          (setf (mem-ref (+ naddr 4) :u32) (logand nhi 67108863))
                          (setf (mem-ref naddr :u32)
                                (+ (mem-ref naddr :u32) carry2)))))))))))))))

;; Wrap carry from h[9] back to r[0]*19
(defun fc-wrap (base)
  (let ((b base))
    (let ((addr (+ b (+ #x64 (* 9 12)))))
      (let ((hi2 (mem-ref addr :u32)))
        (let ((hi (mem-ref (+ addr 4) :u32)))
          (let ((lo (mem-ref (+ addr 8) :u32)))
            (let ((r-base (+ b #xDC)))
              (setf (mem-ref (+ r-base 36) :u32) (logand lo 33554431))
              (let ((c-lo (ash lo -25)))
                (fc-wrap-mul b hi2 hi c-lo)))))))))

(defun fc-wrap-mul (base hi2 hi c-lo)
  (let ((b base))
    (let ((carry-a (+ (* hi 2) c-lo)))
      (let ((c19-lo (mul26lo carry-a 19)))
        (let ((c19-hi (mul26hi carry-a 19)))
          (let ((carry-b38 (* hi2 38)))
            (let ((r-base (+ b #xDC)))
              (let ((r0 (+ (mem-ref r-base :u32) c19-lo)))
                (let ((c0 (ash r0 -26)))
                  (setf (mem-ref r-base :u32) (logand r0 67108863))
                  (setf (mem-ref (+ r-base 4) :u32)
                        (+ (mem-ref (+ r-base 4) :u32)
                           (+ c0 (+ c19-hi carry-b38)))))))))))))

;; Write results from new offset
(defun fc-write-dst (base dst)
  (let ((b base))
    (let ((db (+ (ash (logand dst (- 0 4)) 1) 4)))
      (let ((r-base (+ b #xDC)))
        (let ((i 0))
          (loop
            (when (>= i 40) (return 0))
            (setf (mem-ref (+ db i) :u32) (mem-ref (+ r-base i) :u32))
            (setq i (+ i 4))))))))
")
       (deliver-override "
(defun net-deliver-data (conn buf pkt-len tcp-flags)
  (let ((cb (conn-base conn)))
    (let ((ssh (conn-ssh conn)))
      (let ((ip-total (buf-read-u16-mem buf 16)))
        (let ((tcp-data-off (ash (logand (mem-ref (+ buf 46) :u8) #xF0) -2)))
          (let ((data-len (- ip-total (+ 20 tcp-data-off))))
            (when (> data-len 0)
              (let ((their-seq (buf-read-u32-mem buf 38)))
                (let ((expected (mem-ref (+ cb #x014) :u32)))
                  (if (eq their-seq expected)
                      (let ((buf-len (mem-ref (+ ssh #x6D4) :u32)))
                        (let ((data-start (+ (+ (+ buf 14) 20) tcp-data-off)))
                          (let ((i 0))
                            (loop
                              (when (>= i data-len) (return 0))
                              (setf (mem-ref (+ (+ (+ ssh #x6D8) buf-len) i) :u8)
                                    (mem-ref (+ data-start i) :u8))
                              (setq i (+ i 1))))
                          (setf (mem-ref (+ ssh #x6D4) :u32) (+ buf-len data-len))
                          (setf (mem-ref (+ cb #x014) :u32) (+ their-seq data-len))
                          (tcp-ack-conn cb)))
                      (tcp-ack-conn cb)))))))))))
")
       (parse-override "
(defun ssh-parse-packet (ssh data data-len)
  (let ((s ssh))
    (let ((d data))
      (let ((dl data-len))
        (when (< dl 5) (return ()))
        (let ((packet-len (ssh-get-u32 d 0)))
          (when (< dl (+ 4 packet-len)) (return ()))
          (let ((pad-len (aref d 4)))
            (let ((payload-len (- (- packet-len pad-len) 1)))
              (let ((payload (make-array payload-len)))
                (let ((cb (- s #x20)))
                  (let ((i 0))
                    (loop
                      (when (>= i payload-len) (return 0))
                      (aset payload i (aref d (+ 5 i)))
                      (setq i (+ i 1))))
                  (setf (mem-ref (+ cb #x16F8) :u32) (- dl (+ 4 packet-len)))
                  (setf (mem-ref (+ cb #x16FC) :u32) (+ 4 packet-len))
                  (cons payload payload-len))))))))))
")
       (combined-source (concatenate 'string
                                      cl-user::*net-source*
                                      *repl-source*
                                      ssh-main
                                      keepalive-override
                                      receive-override
                                      kex-override
                                      recv-override
                                      deliver-override
                                      parse-override
                                      ssh-overrides
                                      x25519-override
                                      x25519-override2)))
  (format t "Building ARM32 SSH image (raspi2b + DWC2 USB)...~%")
  (format t "Combined source: ~D chars~%" (length combined-source))
  (let ((image (build-image :target :armv7-rpi :source-text combined-source)))
    (format t "Entry point offset: ~A~%" (kernel-image-entry-point image))
    (format t "Native code size: ~D~%" (length (kernel-image-native-code image)))
    (format t "Boot code size: ~D~%" (length (kernel-image-boot-code image)))
    (write-kernel-image image "/tmp/modus-arm32-ssh.bin")
    (format t "Done. Boot with:~%")
    (format t "  qemu-system-arm -M raspi2b -m 1G -nographic \\~%")
    (format t "    -device usb-net,netdev=net0 \\~%")
    (format t "    -netdev 'user,id=net0,hostfwd=tcp::2222-:22'~%")))
