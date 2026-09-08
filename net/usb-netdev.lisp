;;;; usb-netdev.lisp — runtime USB NIC driver binding + live hot-plug.
;;;;
;;;; Replaces build-time "last-defun-wins" NIC selection with a PROBE that runs
;;;; at boot and again on every USB connect/disconnect edge.  Each concrete
;;;; driver keeps its own discovery:
;;;;   - r8152.lisp    scans the bus for the RTL8153 (VID 0x0BDA / PID 0x8153);
;;;;   - cdc-ether.lisp cold-enumerates a CDC-ECM device.
;;;; This file expresses the driver REGISTRY as a priority probe over those,
;;;; latches the winner in a state slot, and dispatches the NIC seam
;;;; (e1000-send / e1000-receive / e1000-rx-buf) to the bound driver.
;;;;
;;;; MUST be loaded LAST among the net files so its e1000-* dispatchers win
;;;; last-defun-wins over each driver's thin e1000-* forwarder, and so its real
;;;; usb-netdev-hotplug-poll overrides the no-op default in ip.lisp.
;;;;
;;;; BOARD-OWED (hardware only): cold host-mode enumeration of the RTL8153 is
;;;; the flaky path the r8152 "adopt" model was written to avoid, and the live
;;;; re-bind on plug/unplug can only be exercised on real silicon.  The
;;;; dispatch, latch, registry order and W1C edge handling are what compile
;;;; here; the physical bring-up is validated on the Pi.

;; --- driver-id latch --------------------------------------------------------
;; A state SLOT at e1000-state-base+0x780, NOT a defvar: defvars are not
;; initialised at boot (compiler Active Limitation #7) and bare-metal DRAM is
;; not zeroed, so net-usb-probe writes it explicitly before any poll reads it.
;; Layout: 0 = none, 1 = cdc-ether (CDC-ECM), 2 = r8152 (RTL8153).
;; 0x780 is free — the SSH host key occupies 0x710..0x74C, the turnkey auth key
;; 0x750..0x770, the X25519 ephemeral 0x6C4..0x700.
(defun usb-netdev-slot () (+ (e1000-state-base) #x780))
(defun usb-netdev-get ()   (mem-ref (usb-netdev-slot) :u32))
(defun usb-netdev-set (id) (setf (mem-ref (usb-netdev-slot) :u32) id))

;; write-string-serial lives in mvm/prelude.lisp, which the RPi net images do
;; NOT bake (only write-char-serial + %serial-byte reach here).  Define it once,
;; loaded LAST, so this image's r8152 diagnostics resolve too instead of calling
;; an offset-0 stub.  Identical to the prelude definition.
(defun write-string-serial (str)
  (let ((len (array-length str))
        (i 0))
    (loop
      (when (= i len) (return nil))
      (write-char-serial (%prim-aref str i))
      (setq i (+ i 1)))))

;; --- registry: priority probe; each arm reuses a driver's own discovery -----
;; RTL8153 present on the bus  -> bind r8152 (id 2).
;; otherwise                   -> bind cdc-ether (id 1), the incumbent NIC, so
;;                                images with no dongle behave exactly as today.
;; Returns 1 if a driver was bound, 0 if none.
(defun net-usb-probe ()
  (usb-netdev-set 0)
  ;; Bind markers use %serial-byte (proven in kernel-main), not
  ;; write-string-serial: "ND:R"=r8152, "ND:C"=cdc-ecm, "ND:0"=none.
  (if (not (zerop (r8152-find-addr)))
      (let ((r (r8152-probe)))
        (if (> r 0)
            (progn (usb-netdev-set 2)
                   (%serial-byte 78) (%serial-byte 68) (%serial-byte 58)
                   (%serial-byte 82) (%serial-byte 10)
                   1)
            (progn (%serial-byte 78) (%serial-byte 68) (%serial-byte 58)
                   (%serial-byte 48) (%serial-byte 10)
                   0)))
      (progn (cdcether-probe)
             (usb-netdev-set 1)
             (%serial-byte 78) (%serial-byte 68) (%serial-byte 58)
             (%serial-byte 67) (%serial-byte 10)
             1)))

;; --- NIC seam dispatchers (win last-defun-wins over the e1000-* forwarders) --
(defun e1000-send (buf len)
  (let ((d (usb-netdev-get)))
    (if (eq d 2)
        (r8152-send buf len)
        (if (eq d 1) (cdcether-send buf len) 0))))

(defun e1000-receive ()
  (let ((d (usb-netdev-get)))
    (if (eq d 2)
        (r8152-receive)
        (if (eq d 1) (cdcether-receive) 0))))

(defun e1000-rx-buf ()
  (let ((d (usb-netdev-get)))
    (if (eq d 2)
        (r8152-rx-buf)
        (if (eq d 1) (cdcether-rx-buf) (cdc-rx-buf-addr)))))

(defun e1000-probe () (net-usb-probe))

;; --- live hot-plug ----------------------------------------------------------
;; HPRT0.prtconndet (bit 1, write-1-clear) latches a connect/disconnect edge;
;; prtconnsts (bit 0) is the current attach state.  On an edge: connected ->
;; re-probe and re-bind; gone -> unbind.  Clear the edge by writing prtconndet
;; back with the OTHER W1C bits masked off (dwc2-hprt0-mask), so this poll does
;; not accidentally clear port-enable/enable-change.  Read RAW to test the bits.
(defun usb-netdev-hotplug-poll ()
  (let ((hprt (dwc2-read (dwc2-hprt0))))
    (when (not (zerop (logand hprt (hprt0-prtconndet))))
      (dwc2-write (dwc2-hprt0)
                  (logior (logand hprt (dwc2-hprt0-mask)) (hprt0-prtconndet)))
      (if (zerop (logand hprt (hprt0-prtconnsts)))
          (progn (usb-netdev-set 0)
                 (%serial-byte 78) (%serial-byte 68) (%serial-byte 58)
                 (%serial-byte 85) (%serial-byte 10))   ; "ND:U" unplug
          (net-usb-probe)))))
