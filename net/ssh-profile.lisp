;;;; ssh-profile.lisp — Profiling override for ssh-handle-kex
;;;;
;;;; Prints ASCII timing markers around each major crypto operation:
;;;;   [X> ... X<]  x25519 shared secret
;;;;   [H> ... H<]  ssh-compute-exchange-hash (SHA-256)
;;;;   [S> ... S<]  ed25519-sign-fast
;;;;
;;;; Load AFTER ssh.lisp (last-defun-wins). Measure wall-clock time
;;;; on the host by watching serial output timestamps.

;;; Helper: print 2-char marker to serial
(defun prof-mark-2 (c1 c2)
  (%serial-byte 91)   ; [
  (%serial-byte c1)
  (%serial-byte c2)
  (%serial-byte 93)   ; ]
  (%serial-byte 10))  ; newline

(defun ssh-handle-kex (ssh kex-init-payload kex-init-len)
  (let ((cli-eph (make-array 32)))
    (dotimes (i 32) (aset cli-eph i (aref kex-init-payload (+ 5 i))))
    (ssh-kex-do-crypto ssh cli-eph)))

(defun ssh-kex-do-crypto (ssh cli-eph)
  (let ((state (e1000-state-base)))
    (let ((srv-priv (ssh-kex-load-key state #x6C4)))
      (let ((srv-eph (ssh-kex-load-key state #x6E4)))
        (prof-mark-2 88 62)  ; [X>]
        (let ((shared (x25519 srv-priv cli-eph)))
          (prof-mark-2 88 60)  ; [X<]
          (usb-keepalive)
          (ssh-mem-store (+ ssh #x070) shared 32)
          (ssh-kex-hash-and-sign ssh cli-eph srv-eph shared))))))

(defun ssh-kex-load-key (state offset)
  (let ((key (make-array 32)))
    (dotimes (i 32)
      (aset key i (mem-ref (+ state (+ offset i)) :u8)))
    key))

(defun ssh-kex-hash-and-sign (ssh cli-eph srv-eph shared)
  (prof-mark-2 72 62)  ; [H>]
  (let ((h (ssh-compute-exchange-hash ssh cli-eph srv-eph shared)))
    (prof-mark-2 72 60)  ; [H<]
    (ssh-mem-store (+ ssh #x050) h 32)
    (ssh-kex-finish-sign ssh h srv-eph)))

(defun ssh-kex-finish-sign (ssh h srv-eph)
  (when (zerop (mem-ref ssh :u32))
    (ssh-mem-store (+ ssh #x030) h 32)
    (setf (mem-ref ssh :u32) 1))
  (usb-keepalive)
  (prof-mark-2 83 62)  ; [S>]
  (let ((sig (ed25519-sign-fast h 32)))
    (prof-mark-2 83 60)  ; [S<]
    (usb-keepalive)
    (ssh-send-kex-reply ssh sig srv-eph)))
