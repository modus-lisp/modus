;;;; uart-bootloader.lisp - UART serial bootloader for Pi Zero 2 W
;;;;
;;;; Receives kernel images over mini UART from Pi 5 host.
;;;; Loaded AFTER arch-raspi3b.lisp and bcm2835-periph.lisp
;;;; (uses timer-read-lo, delay-us, led-blink from bcm2835-periph).
;;;;
;;;; Protocol:
;;;;   1. Bootloader waits ~2s for magic byte 0x55
;;;;   2. If received: sends ACK 0xAA
;;;;   3. Host sends 4-byte little-endian kernel size
;;;;   4. Host sends kernel data bytes
;;;;   5. Host sends 1-byte checksum (sum of all data bytes mod 256)
;;;;   6. Bootloader verifies checksum, prints "OK\n" or "ER\n"
;;;;   7. On success: jumps to loaded kernel at 0x300000

;;; ============================================================
;;; Mini UART Read (can't use TRAP #x0301 — hardcoded PL011)
;;; ============================================================
;;; AUX_MU_IO   = 0x3F215040  data register
;;; AUX_MU_LSR  = 0x3F215054  bit 0 = data ready

(defun uart-read-byte ()
  ;; Blocking read with safety timeout (~5 seconds at ~1GHz)
  (let ((i 0))
    (loop
      (when (> i 50000000) (return -1))
      (when (not (zerop (logand (mem-ref #x3F215054 :u32) 1)))
        (return (logand (mem-ref #x3F215040 :u32) #xFF)))
      (setq i (+ i 1)))))

(defun uart-read-byte-timeout (timeout-us)
  ;; Read with microsecond timeout using system timer
  (let ((deadline (+ (timer-read-lo) timeout-us)))
    (loop
      (when (>= (timer-read-lo) deadline) (return -1))
      (when (not (zerop (logand (mem-ref #x3F215054 :u32) 1)))
        (return (logand (mem-ref #x3F215040 :u32) #xFF))))))

(defun uart-read-u32 ()
  ;; Read 4-byte little-endian value
  ;; NOTE: must use 2-arg logior (MVM limitation)
  (let ((b0 (uart-read-byte)))
    (let ((b1 (uart-read-byte)))
      (let ((b2 (uart-read-byte)))
        (let ((b3 (uart-read-byte)))
          (let ((lo (logior b0 (ash b1 8))))
            (let ((hi (logior (ash b2 16) (ash b3 24))))
              (logior lo hi))))))))

;;; ============================================================
;;; Kernel Receive + Jump
;;; ============================================================

;; Default only.  The SENDER now supplies the load address on the wire (see
;; bootloader-receive), so the loader adapts to the image instead of the image
;; having to be built for a constant compiled in here.
;;
;; WHY: an image's fn-address constants, code bounds and :li-const patches are
;; baked for ONE base at build time (cross.lisp adds 0x80000 to the declared
;; :load-addr).  When that base and this constant disagree, the loader jumps
;; into a mislaid image and the board wedges SILENTLY — the same failure shape
;; as boot-rpi.lisp's double-counted :load-addr.  Carrying the address in the
;; protocol makes the mismatch impossible.
(defun bootloader-load-addr () #x300000)

;; Refuse an address that would overwrite the loader itself (we execute from
;; 0x80000) or land at/above the stack top.  A bad address is otherwise a
;; guaranteed silent wedge, so say "AD" and give up instead of jumping.
(defun bootloader-addr-ok (a)
  (if (< a #x200000) nil (if (>= a #x08000000) nil t)))

(defun bootloader-receive ()
  ;; Send ACK
  (write-byte #xAA)
  ;; Read 4-byte LOAD ADDRESS, then 4-byte kernel size
  (let ((addr (uart-read-u32)))
    (write-byte 65) (write-byte 68) (write-byte 58)  ;; "AD:"
    (print-hex32 addr)
    (write-byte 10)
    (if (not (bootloader-addr-ok addr))
        (progn
          (write-byte 65) (write-byte 68) (write-byte 10)  ;; "AD\n" = bad addr
          nil)
  (let ((size (uart-read-u32)))
    ;; Print size
    (write-byte 83) (write-byte 90) (write-byte 58)  ;; "SZ:"
    (print-hex32 size)
    (write-byte 10)
    ;; Read kernel data.  DRAIN THE WHOLE FIFO PER POLL, don't poll per byte.
    ;;
    ;; MEASURED on real hardware: the old one-poll-per-byte loop could only
    ;; absorb 8 bytes at a time — exactly the BCM2835 mini UART's RX FIFO depth
    ;; — and a sender writing more than that silently lost bytes, so `i' never
    ;; reached SIZE and the loop waited forever.  Sweep at 115200:
    ;;     chunk=8 -> OK    chunk=16/32/64/256 -> no OK, ever
    ;; Paced to 8 bytes it works but yields only ~1.0 KB/s, i.e. 5.6 HOURS for
    ;; a 20 MB CL image.
    ;;
    ;; Note the "TO" never appeared either: uart-read-byte's guard is
    ;; `(> i 50000000)', commented "~5 seconds at ~1GHz" — but with the MMU OFF
    ;; every access is Device-nGnRnE (uncached), so that spin is MINUTES, not
    ;; seconds.  A stalled receive therefore looks exactly like a hang.
    ;;
    ;; Draining while LSR bit 0 stays set amortises the poll across a whole
    ;; FIFO-full.  It does NOT fix the deeper cost — the per-byte store into
    ;; DRAM is also uncached with the MMU off — so the real lever remains
    ;; enabling caching (already the first rung-2 item in boot-rpi-cl.lisp).
    (let ((load-addr addr))
      (let ((i 0))
        (let ((checksum 0))
          (loop
            (when (>= i size) (return nil))
            ;; wait for at least one byte (with the existing safety timeout)
            (let ((b (uart-read-byte)))
              (when (= b -1)
                ;; Timeout during receive
                (write-byte 84) (write-byte 79) (write-byte 10)  ;; "TO\n"
                (return nil))
              (setf (mem-ref (+ load-addr i) :u8) b)
              (setq checksum (logand (+ checksum b) #xFF)))
            (setq i (+ i 1))
            ;; then take everything else already sitting in the FIFO
            (loop
              (when (>= i size) (return nil))
              (when (zerop (logand (mem-ref #x3F215054 :u32) 1)) (return nil))
              (let ((b2 (logand (mem-ref #x3F215040 :u32) #xFF)))
                (setf (mem-ref (+ load-addr i) :u8) b2)
                (setq checksum (logand (+ checksum b2) #xFF)))
              (setq i (+ i 1))))
          ;; Read expected checksum
          (let ((expected (uart-read-byte)))
            (if (= checksum expected)
                (progn
                  (write-byte 79) (write-byte 75) (write-byte 10)  ;; "OK\n"
                  ;; Small delay for UART TX to drain
                  (delay-us 50000)
                  ;; Jump to loaded kernel
                  (jump-to-address load-addr))
                (progn
                  (write-byte 69) (write-byte 82) (write-byte 10)  ;; "ER\n"
                  nil))))))))))

(defun uart-drain ()
  ;; Drain any garbage from UART RX FIFO (reset transients on GPIO)
  (let ((n 0))
    (loop
      (when (>= n 64) (return nil))
      (when (zerop (logand (mem-ref #x3F215054 :u32) 1))
        (return nil))
      (mem-ref #x3F215040 :u32)  ;; read and discard
      (setq n (+ n 1)))))

(defun bootloader-wait ()
  ;; Drain FIFO, then wait ~5 seconds for magic byte 0x55
  (uart-drain)
  (let ((b (uart-read-byte-timeout 5000000)))
    (if (= b #x55)
        (bootloader-receive)
        nil)))
