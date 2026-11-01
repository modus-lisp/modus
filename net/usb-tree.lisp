;;;; usb-tree.lisp - the whole USB bus, hubs inside hubs, enumerated from the root port.
;;;;
;;;; net/usb-hid-split.lisp was written for one hub: its keyboard and mouse records, its
;;;; hotplug scan and every split transaction name that hub.  A keyboard plugged into a
;;;; monitor's hub, or behind a dock, was never found (measured in QEMU with nested hubs:
;;;; the fidelity build's usb-hub high-speed=on).  And the network bring-up took the bus as
;;;; U-Boot left it -- different every netboot (ND:R, ND:0, CDC:E2 then ND:C).
;;;;
;;;; USBT-ENUMERATE starts from a reset of the root port, so U-Boot's state does not
;;;; matter, and walks the tree depth first: every hub's ports are powered, disabled, then
;;;; reset ONE AT A TIME (a device answers at address 0 only on the port just reset), its
;;;; device read at address 0, addressed, identified, and -- a hub -- configured and walked
;;;; in turn.  Each device gets a record in a table:
;;;;
;;;;   +0x00 address          +0x04 parent (index + 1; 0 = the root port)
;;;;   +0x08 port on parent   +0x0C speed: 0 full, 1 low, 2 high
;;;;   +0x10 TT hub address   +0x14 TT port  (0 0: no split; the device is high speed, or
;;;;                                          on the root port)
;;;;   +0x18 vendor:product   +0x1C device class   +0x20 ports (a hub; else 0)
;;;;   +0x24 bMaxPacketSize0  +0x28 configurations
;;;;
;;;; THE TT.  A full/low-speed device is reached by SPLIT transactions through the first
;;;; HIGH-SPEED hub above it, at the port its branch leaves that hub by -- through any
;;;; full-speed hubs in between (USB 2.0 11.14).  A high-speed device behind high-speed hubs
;;;; needs none.  The QEMU fidelity build checks exactly this and answers a misrouted split
;;;; with a transaction error.
;;;;
;;;; Then the drivers bind from the table: USBT-BIND-HID the first boot keyboard and boot
;;;; mouse wherever they are (into usb-hid-split.lisp's records, with their TT hub at +0x1C),
;;;; USBT-BIND-NIC the first high-speed device with a CDC-ECM configuration -- the RTL8153's
;;;; configuration 2, QEMU's usb-net -- as the NIC (net/usb-netdev.lisp kind 1).
;;;;
;;;; Blocking, from the main thread (or the I/O core's recovery, outside its safe region):
;;;; it conses nothing it keeps, but it waits on the bus.

;;; ---- the table ---------------------------------------------------------------------

(defun usbt-base () (+ (split-dma-base) #x4000))      ; +0 count, +4 next address
(defun usbt-max () 24)
(defun usbt-e (i) (+ (usbt-base) #x40 (* i #x40)))
(defun usbt-get (i off) (mem-ref (+ (usbt-e i) off) :u32))
(defun usbt-put (i off v) (setf (mem-ref (+ (usbt-e i) off) :u32) v))
(defun usbt-count () (mem-ref (usbt-base) :u32))

(defun usbt-live-p (i) (> (usbt-get i 0) 0))          ; a removed device's slot holds address 0

(defun usbt-address-used-p (a)
  (let ((i 0) (used nil))
    (loop (when (or used (>= i (usbt-count))) (return used))
      (when (= (usbt-get i 0) a) (setq used t))
      (setq i (+ i 1)))))

(defun usbt-new-address ()
  "The next address no live device holds (1..127, wrapping)."
  (let ((a (mem-ref (+ (usbt-base) 4) :u32)) (k 0))
    (loop
      (when (> a 127) (setq a 2))
      (when (or (not (usbt-address-used-p a)) (> k 127)) (return nil))
      (setq a (+ a 1) k (+ k 1)))
    (setf (mem-ref (+ (usbt-base) 4) :u32) (+ a 1))
    a))

(defun usbt-free-slot ()
  "A removed device's slot, else the next one."
  (let ((i 0) (free -1))
    (loop (when (or (>= free 0) (>= i (usbt-count))) (return nil))
      (unless (usbt-live-p i) (setq free i))
      (setq i (+ i 1)))
    (if (>= free 0) free (usbt-count))))

;;; ---- one control transfer, routed --------------------------------------------------

(defun usbt-xfer (addr tthub ttport ls mps0 rtype req value index len)
  "A control transfer to ADDR, split through TTHUB/TTPORT when TTHUB is not 0.  > 0 on
   success (IN data in USBT-BUF of the same route), <= 0 on failure."
  (if (zerop tthub)
      (usb-control-transfer addr rtype req value index (usb-data-buf) len)
      (let ((r (usb-split-control-in addr tthub ttport ls mps0 rtype req value index len)))
        (if (< r 0) r (max r 1)))))

(defun usbt-buf (tthub) (if (zerop tthub) (usb-data-buf) (split-data-buf)))

(defun usbt-ctl (i rtype req value index len)
  (usbt-xfer (usbt-get i 0) (usbt-get i #x10) (usbt-get i #x14) (= (usbt-get i #x0C) 1)
             (max 8 (usbt-get i #x24)) rtype req value index len))

(defun usbt-ibuf (i) (usbt-buf (usbt-get i #x10)))

;;; ---- hub ports ---------------------------------------------------------------------

(defun usbt-port-status (h p)
  "wPortStatus | wPortChange << 16 of port P of hub entry H, or 0."
  (if (> (usbt-ctl h #xA3 0 0 p 4) 0)
      (let ((b (usbt-ibuf h)))
        (logior (usb-desc-u16 b 0) (ash (usb-desc-u16 b 2) 16)))
      0))

(defun usbt-port-feature (h set feature p)
  (usbt-ctl h #x23 (if set 3 1) feature p 0))

(defun usbt-clear-changes (h p)
  (usbt-port-feature h nil 16 p)                       ; C_PORT_CONNECTION
  (usbt-port-feature h nil 17 p)                       ; C_PORT_ENABLE
  (usbt-port-feature h nil 20 p))                      ; C_PORT_RESET

(defun usbt-wait-ports (h)
  "After powering hub H's ports: wait until the set of connected ports has not changed for
   ~600 ms, at most ~4 s.  The mask."
  (let ((n (usbt-get h #x20)) (last -1) (stable 0) (k 0))
    (loop
      (when (or (>= stable 6) (> k 40)) (return last))
      (let ((m 0) (p 1))
        (loop (when (> p n) (return nil))
          (when (logbitp 0 (usbt-port-status h p)) (setq m (logior m (ash 1 p))))
          (setq p (+ p 1)))
        (if (= m last) (setq stable (+ stable 1)) (setq stable 0 last m)))
      (dwc2-delay-ms 50)
      (setq k (+ k 1)))))

;;; ---- a device --------------------------------------------------------------------

(defun usbt-identify (parent port speed tthub ttport)
  "The device just reset onto PARENT's PORT (PARENT -1: the root port), at address 0:
   address it, read what it is, record it.  Its index, or -1."
  (let ((ls (= speed 1)) (mps0 0) (try 0) (b (usbt-buf tthub)))
    (loop
      (when (or (> mps0 0) (>= try 3)) (return nil))
      (setq try (+ try 1))
      (when (> (usbt-xfer 0 tthub ttport ls 8 #x80 6 #x100 0 8) 0)
        (setq mps0 (max 8 (usb-desc-byte b 7))))
      (when (zerop mps0) (dwc2-delay-ms 20)))
    (if (or (zerop mps0) (>= (usbt-free-slot) (usbt-max)))
        -1
        (let ((addr (usbt-new-address)))
          (if (<= (usbt-xfer 0 tthub ttport ls mps0 #x00 5 addr 0 0) 0)
              -1
              (progn
                (dwc2-delay-ms 10)
                (if (<= (usbt-xfer addr tthub ttport ls mps0 #x80 6 #x100 0 18) 0)
                    -1
                    (let ((i (usbt-free-slot)))
                      (when (= i (usbt-count)) (setf (mem-ref (usbt-base) :u32) (+ i 1)))
                      (usbt-put i #x2C 0)                ; not yet offered to the drivers
                      (usbt-put i 0 addr)
                      (usbt-put i 4 (+ parent 1))
                      (usbt-put i 8 port)
                      (usbt-put i #x0C speed)
                      (usbt-put i #x10 tthub)
                      (usbt-put i #x14 ttport)
                      (usbt-put i #x18 (logior (ash (usb-desc-u16 b 8) 16) (usb-desc-u16 b 10)))
                      (usbt-put i #x1C (usb-desc-byte b 4))
                      (usbt-put i #x20 0)
                      (usbt-put i #x24 mps0)
                      (usbt-put i #x28 (usb-desc-byte b 17))
                      (kmsg-event 1 "USBD" (logior (ash addr 8) (ash speed 4) (+ parent 1))
                                  (usbt-get i #x18) (logior (ash tthub 8) ttport))
                      i))))))))

(defun usbt-hub (h)
  "Entry H is a hub: configure it, power its ports, and walk them."
  (when (and (> (usbt-ctl h #x80 6 #x200 0 9) 0)
             (> (usbt-ctl h #x00 9 (usb-desc-byte (usbt-ibuf h) 5) 0 0) 0)
             (> (usbt-ctl h #xA0 6 #x2900 0 8) 0))
    (let ((n (min 15 (usb-desc-byte (usbt-ibuf h) 2))) (p 1))
      (usbt-put h #x20 n)
      (loop (when (> p n) (return nil))
        (usbt-port-feature h t 8 p)                    ; PORT_POWER
        (usbt-port-feature h nil 1 p)                  ; disabled until its turn
        (setq p (+ p 1)))
      (dwc2-delay-ms 100)
      (let ((mask (usbt-wait-ports h)))
        (setq p 1)
        (loop (when (> p n) (return nil))
          (when (logbitp p mask) (usbt-port h p))
          (setq p (+ p 1)))))))

(defun usbt-port (h p)
  "Reset port P of hub H and enumerate what is behind it."
  (usbt-port-feature h t 4 p)                          ; PORT_RESET
  (dwc2-delay-ms 100)
  (let ((st (usbt-port-status h p)))
    (usbt-clear-changes h p)
    (dwc2-delay-ms 20)
    (when (and (logbitp 0 st) (logbitp 1 st))          ; connected and enabled
      (let* ((speed (cond ((logbitp 9 st) 1) ((logbitp 10 st) 2) (t 0)))
             (hub-hs (= (usbt-get h #x0C) 2))
             (tthub (cond ((= speed 2) 0) (hub-hs (usbt-get h 0)) (t (usbt-get h #x10))))
             (ttport (cond ((= speed 2) 0) (hub-hs p) (t (usbt-get h #x14))))
             (i (usbt-identify h p speed tthub ttport)))
        (when (and (>= i 0) (= (usbt-get i #x1C) 9))
          (usbt-hub i))))))

(defun usbt-enumerate ()
  "The whole bus, from a reset of the root port.  The number of devices found."
  (setf (mem-ref (usbt-base) :u32) 0)
  (setf (mem-ref (+ (usbt-base) 4) :u32) 1)
  (hdmi-mbox-property-1 #x28001 3 3)                   ; the controller powered
  (hid-dwc2-init-hw)
  (when (>= (dwc2-port-reset) 0)
    (dwc2-delay-ms 20)
    ;; HPRT0 PrtSpd (bits 17-18): 0 high, 1 full, 2 low
    (let* ((ps (logand (ash (dwc2-read (dwc2-hprt0)) -17) 3))
           (speed (cond ((= ps 0) 2) ((= ps 2) 1) (t 0)))
           ;; the (micro)frame interval in PHY clocks (60 MHz UTMI+): 125 us on a high-speed
           ;; port.  Read back after a cold init it was 60000 -- full speed's 1 ms -- on the Zero.
           (hfir (dwc2-write (dwc2-hfir) (if (= speed 2) 7500 60000)))
           (i (usbt-identify -1 1 speed 0 0)))
      (when (and (>= i 0) (= (usbt-get i #x1C) 9))
        (usbt-hub i))))
  (kmsg-event 1 "USBT" (usbt-count) 0 0)
  (usbt-count))

;;; ---- drivers bind from the table ---------------------------------------------------

(defun usbt-config-desc (i index)
  "Configuration descriptor INDEX of entry I into its buffer, whole (<= 512 bytes).  Its
   length, or 0."
  (if (<= (usbt-ctl i #x80 6 (logior #x200 index) 0 9) 0)
      0
      (let ((total (min 512 (usb-desc-u16 (usbt-ibuf i) 2))))
        (if (<= (usbt-ctl i #x80 6 (logior #x200 index) 0 total) 0) 0 total))))

(defun usbt-hid-kind (i)
  "Configure entry I if it is a boot keyboard (1) or mouse (2); into usb-hid-split.lisp's
   record for it.  The kind, or 0."
  (let ((total (usbt-config-desc i 0)) (b (usbt-ibuf i)) (kind 0))
    (when (> total 0)
      (let ((cfg (usb-desc-byte b 5)) (pos 0) (iface -1) (proto 0) (ep 0) (mps 0))
        (loop
          (when (or (>= pos total) (> ep 0)) (return nil))
          (let ((len (usb-desc-byte b pos)) (typ (usb-desc-byte b (+ pos 1))))
            (when (zerop len) (return nil))
            (cond
              ((= typ 4)
               (if (and (= (usb-desc-byte b (+ pos 5)) 3) (= (usb-desc-byte b (+ pos 6)) 1)
                        (member (usb-desc-byte b (+ pos 7)) '(1 2)))
                   (setq iface (usb-desc-byte b (+ pos 2)) proto (usb-desc-byte b (+ pos 7)))
                   (setq iface -1)))
              ((and (= typ 5) (>= iface 0) (logbitp 7 (usb-desc-byte b (+ pos 2)))
                    (= (logand (usb-desc-byte b (+ pos 3)) 3) 3))
               (setq ep (logand (usb-desc-byte b (+ pos 2)) #xF)
                     mps (logand (usb-desc-u16 b (+ pos 4)) #x7FF))))
            (setq pos (+ pos len))))
        (let ((dev (if (= proto 1) (hid-kbd) (hid-mouse))))
          (when (and (> ep 0) (not (= (hid-dev-get dev 0) 1)))
            (usbt-ctl i #x00 9 cfg 0 0)                    ; SET_CONFIGURATION
            (usbt-ctl i #x21 #x0B 0 iface 0)               ; SET_PROTOCOL(boot)
            (usbt-ctl i #x21 #x0A 0 iface 0)               ; SET_IDLE
            (hid-dev-put dev #x04 (usbt-get i 0))
            (hid-dev-put dev #x08 (usbt-get i #x14))       ; the TT port
            (hid-dev-put dev #x0C (if (= (usbt-get i #x0C) 1) 1 0))
            (hid-dev-put dev #x10 ep)
            (hid-dev-put dev #x14 (min mps 64))
            (hid-dev-put dev #x18 (hctsiz-pid-data0))
            (hid-dev-put dev #x1C (usbt-get i #x10))       ; the TT hub
            (hid-dev-put dev #x00 1)
            (setq kind proto)))))
    kind))

(defun usbt-bind-hid ()
  "Usb-hid-split.lisp's state, from the table: the first boot keyboard and the first boot
   mouse behind a TT (a split device), wherever they are.  (KEYBOARD-P MOUSE-P)"
  (let ((k 0))
    (loop (when (>= k #x200) (return nil)) (hid-put k 0) (setq k (+ k 4))))
  ;; the hotplug scan's hub: the root's, when the root port carries one
  (when (and (> (usbt-count) 0) (= (usbt-get 0 #x1C) 9))
    (hid-put #x00 (usbt-get 0 0))
    (hid-put #x04 (usbt-get 0 #x20)))
  (let ((i 0))
    (loop
      (when (>= i (usbt-count)) (return nil))
      (when (and (usbt-live-p i) (/= (usbt-get i #x1C) 9) (> (usbt-get i #x10) 0))
        (usbt-hid-kind i))
      (usbt-put i #x2C 1)
      (setq i (+ i 1))))
  (hid-put #x1F8 0) (hid-put #x94 0) (hid-put #x98 0)
  (hid-put #x1F0 (hid-magic))
  (list (= (hid-dev-get (hid-kbd) 0) 1) (= (hid-dev-get (hid-mouse) 0) 1)))

(defun usbt-ecm (i index)
  "Configuration INDEX of entry I, if it has a CDC-ECM control interface (class 2, subclass
   6) and a data interface (class 10) alternate with two bulk endpoints: a list
   (config-value control-iface data-iface data-alt ep-in ep-out mps), else NIL."
  (let ((total (usbt-config-desc i index)) (b (usbt-ibuf i)))
    (when (> total 0)
      (let ((cfg (usb-desc-byte b 5)) (pos 0) (ctl -1) (cur-class 0) (cur-if 0) (cur-alt 0)
            (in 0) (out 0) (mps 0) (found nil))
        (loop
          (when (or (>= pos total) found) (return nil))
          (let ((len (usb-desc-byte b pos)) (typ (usb-desc-byte b (+ pos 1))))
            (when (zerop len) (return nil))
            (cond
              ((= typ 4)
               (setq cur-if (usb-desc-byte b (+ pos 2)) cur-alt (usb-desc-byte b (+ pos 3))
                     cur-class (usb-desc-byte b (+ pos 5)) in 0 out 0)
               (when (and (= cur-class 2) (= (usb-desc-byte b (+ pos 6)) 6)) (setq ctl cur-if)))
              ((and (= typ 5) (= cur-class 10) (>= ctl 0)
                    (= (logand (usb-desc-byte b (+ pos 3)) 3) 2))
               (let ((a (usb-desc-byte b (+ pos 2))))
                 (setq mps (logand (usb-desc-u16 b (+ pos 4)) #x7FF))
                 (if (logbitp 7 a) (setq in (logand a #xF)) (setq out (logand a #xF))))
               (when (and (> in 0) (> out 0))
                 (setq found (list cfg ctl cur-if cur-alt in out mps)))))
            (setq pos (+ pos len))))
        found))))

(defun usbt-bind-nic ()
  "The first HIGH-SPEED device with a CDC-ECM configuration, configured and bound as the
   NIC (kind 1).  Its address, or 0."
  (let ((i 0) (addr 0))
    (loop
      (when (or (>= i (usbt-count)) (> addr 0)) (return nil))
      (when (and (usbt-live-p i) (= (usbt-get i #x0C) 2) (/= (usbt-get i #x1C) 9))
        (let ((c 0) (e nil))
          (loop
            (when (or e (>= c (max 1 (usbt-get i #x28)))) (return nil))
            (setq e (usbt-ecm i c))
            (setq c (+ c 1)))
          (when e
            (let ((a (usbt-get i 0)))
              (when (and (> (usbt-ctl i #x00 9 (nth 0 e) 0 0) 0)                    ; config
                         (> (usbt-ctl i #x01 11 (nth 3 e) (nth 2 e) 0) 0))          ; data alt
                (usbt-ctl i #x21 #x43 #x0E (nth 1 e) 0)       ; packet filter: directed+bcast+mcast
                (usb-set-dev-addr a)
                (usb-set-bulk-in-ep (nth 4 e)) (usb-set-bulk-out-ep (nth 5 e))
                (usb-set-bulk-in-mps (nth 6 e)) (usb-set-bulk-out-mps (nth 6 e))
                (usb-set-bulk-in-toggle 0) (usb-set-bulk-out-toggle 0)
                (let ((s (e1000-state-base)))
                  (modus.net.cdc:cdc-read-mac s)
                  (dolist (off '(#x10 #x14 #x44)) (setf (mem-ref (+ s off) :u32) 0)))
                ;; USB-BULK-RECEIVE's re-arm flag and arm time: cold RAM holds neither
                (setf (mem-ref (+ (usb-state-addr) #xA0) :u32) 0
                      (mem-ref (+ (usb-state-addr) #xA4) :u32) (logand (rdtsc) #xFFFFFFFF))
                ;; the I/O core copies every received frame with the vendor driver's helper
                (modus.net.r8152:r8152-ensure-helpers)
                (usb-netdev-set 1)
                (setq addr a)
                (kmsg-event 1 "NICT" a (usbt-get i #x18) (nth 0 e)))))))
      (setq i (+ i 1)))
    addr))

(defun usbt-status ()
  "The table, one plist per device."
  (let ((out nil) (i (- (usbt-count) 1)))
    (loop
      (when (< i 0) (return out))
      (when (usbt-live-p i)
      (push (list :addr (usbt-get i 0) :parent (- (usbt-get i 4) 1) :port (usbt-get i 8)
                  :speed (case (usbt-get i #x0C) (0 :full) (1 :low) (t :high))
                  :tt (list (usbt-get i #x10) (usbt-get i #x14))
                  :id (format nil "~4,'0X:~4,'0X" (ash (usbt-get i #x18) -16)
                              (logand (usbt-get i #x18) #xFFFF))
                  :class (usbt-get i #x1C) :ports (usbt-get i #x20))
            out))
      (setq i (- i 1)))))

;;; ---- hot plug, anywhere in the tree -----------------------------------------------
;;;
;;; USBT-SCAN asks every hub in the table for its ports' connection changes (a GET_STATUS
;;; per port: through a TT for a full-speed hub).  A port whose connection changed loses
;;; whatever was below it -- the device and, a hub, its whole branch: the keyboard, mouse or
;;; NIC among them is unbound -- and, connected now, is enumerated again and what is new is
;;; offered to the drivers.  The I/O core's hot-plug service runs it every 500 ms with the
;;; receive halted.

(defun usbt-hid-forget ()
  "Unbind the keyboard / mouse whose device is no longer in the table."
  (dolist (dev (list (hid-kbd) (hid-mouse)))
    (when (and (= (hid-dev-get dev 0) 1) (not (usbt-address-used-p (hid-dev-get dev #x04))))
      (hid-dev-put dev 0 0)
      (if (= dev (hid-kbd)) (hid-put #x88 0) (hid-put #x60 0)))))

(defun usbt-kill-branch (i)
  "Device I and everything below it leave the table."
  (usbt-put i 0 0)
  (let ((more t))
    (loop
      (unless more (return nil))
      (setq more nil)
      (let ((j 0))
        (loop (when (>= j (usbt-count)) (return nil))
          (let ((par (- (usbt-get j 4) 1)))
            (when (and (usbt-live-p j) (>= par 0) (not (usbt-live-p par)))
              (usbt-put j 0 0)
              (setq more t)))
          (setq j (+ j 1))))))
  (usbt-hid-forget)
  (unless (or (zerop (usb-netdev-get)) (usbt-address-used-p (usb-dev-addr)))
    (usb-netdev-set 0)
    (kmsg-event 2 "NICX" (usb-dev-addr) 0 0)))

(defun usbt-child-at (h p)
  (let ((i 0) (found -1))
    (loop (when (or (>= found 0) (>= i (usbt-count))) (return found))
      (when (and (usbt-live-p i) (= (usbt-get i 4) (+ h 1)) (= (usbt-get i 8) p))
        (setq found i))
      (setq i (+ i 1)))))

(defun usbt-bind-new ()
  "Offer devices not yet offered to the drivers: a boot keyboard / mouse into a free record,
   a CDC-ECM adapter when there is no NIC."
  (let ((i 0))
    (loop (when (>= i (usbt-count)) (return nil))
      (when (and (usbt-live-p i) (zerop (usbt-get i #x2C)))
        (usbt-put i #x2C 1)
        (when (and (/= (usbt-get i #x1C) 9) (> (usbt-get i #x10) 0))
          (usbt-hid-kind i)))
      (setq i (+ i 1))))
  (when (zerop (usb-netdev-get)) (usbt-bind-nic)))

(defun usbt-scan ()
  "One hot-plug pass over every hub; the number of ports whose connection changed."
  (let ((changes 0) (h 0))
    (loop
      (when (>= h (usbt-count)) (return nil))
      (when (and (usbt-live-p h) (= (usbt-get h #x1C) 9))
        (let ((p 1) (n (usbt-get h #x20)))
          (loop
            (when (or (> p n) (not (usbt-live-p h))) (return nil))
            (let ((st (usbt-port-status h p)))
              (when (logbitp 16 st)                    ; C_PORT_CONNECTION
                (setq changes (+ changes 1))
                (kmsg-event 1 "HOTP" (usbt-get h 0) p st)
                (usbt-clear-changes h p)
                (let ((old (usbt-child-at h p)))
                  (when (>= old 0) (usbt-kill-branch old)))
                (when (logbitp 0 st)
                  (dwc2-delay-ms 100)                  ; debounce
                  (usbt-port h p))))
            (setq p (+ p 1)))))
      (setq h (+ h 1)))
    (when (> changes 0) (usbt-bind-new))
    changes))
