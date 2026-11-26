;;;; ppcmac-input.lisp -- the PowerPC Mac's keyboard and mouse: ADB through CUDA or the PMU.
;;;;
;;;; CUDA is the 6805 microcontroller on the Mac's VIA (mac-io + #x16000), the
;;;; G3's and first G4's ADB master (and QEMU mac99's).  The host talks to it a
;;;; byte at a time through the VIA's shift register, with three port-B lines:
;;;; TIP (ours: a transfer is in progress, active low), TACK (ours: toggled per
;;;; byte), TREQ (CUDA's: it has a packet for us, active low).  Linux's
;;;; drivers/macintosh/via-cuda.c is the reference.  Polled, no interrupts.
;;;;
;;;; We switch CUDA to AUTOPOLL: it then polls the ADB devices itself and sends
;;;; a packet whenever one has data -- [0 flags cmd data...], cmd = addr<<4 | #xC
;;;; (Talk register 0).  Address 2 is the keyboard (two key bytes, bit 7 =
;;;; release, #xFF = none), 3 the mouse (byte 0: bit 7 = button UP, low 7 bits
;;;; dy; byte 1: low 7 bits dx; both 7-bit two's complement).
;;;;
;;;; The VIA is at VA #x08016000 (boot-ppcmac's DBAT3 maps PA #x80000000 there,
;;;; uncached), registers #x200 apart.

(defconstant +via-base+ #x08016000)
(defconstant +via-b+ 0)    (defconstant +via-dirb+ 2) (defconstant +via-sr+ 10)
(defconstant +via-acr+ 11) (defconstant +via-ifr+ 13) (defconstant +via-ier+ 14)
(defconstant +cuda-treq+ #x08) (defconstant +cuda-tack+ #x10) (defconstant +cuda-tip+ #x20)

(defun via (r) (mem-ref (+ +via-base+ (* r #x200)) :u8))
(defun via-set (r v) (setf (mem-ref (+ +via-base+ (* r #x200)) :u8) v))

(defun cuda-wait-sr ()
  "Wait for the shift register's interrupt flag; T, or NIL after a while."
  (dotimes (i 200000 nil)
    (when (logtest (via +via-ifr+) 4) (return t))))

(defun cuda-init ()
  (via-set +via-dirb+ (logand (logior (via +via-dirb+) (logior +cuda-tack+ +cuda-tip+))
                              (lognot +cuda-treq+)))
  (via-set +via-acr+ (logior (logand (via +via-acr+) (lognot #x1C)) #x0C))   ; SR: shift in, ext clock
  (via-set +via-ier+ #x7F)                                                    ; no interrupts
  (via-set +via-b+ (logior (via +via-b+) +cuda-tack+ +cuda-tip+))           ; idle
  (via +via-sr+)
  t)

(defun cuda-treq-p () (not (logtest (via +via-b+) +cuda-treq+)))

(defun cuda-read-packet ()
  "CUDA has asserted TREQ: read its packet, a list of bytes."
  (via-set +via-acr+ (logand (via +via-acr+) (lognot #x10)))                 ; shift in
  (via-set +via-b+ (logand (via +via-b+) (lognot +cuda-tip+)))              ; TIP
  (let ((bytes nil))
    (loop
      (unless (cuda-wait-sr) (return))
      (push (via +via-sr+) bytes)
      (when (logtest (via +via-b+) +cuda-treq+) (return))                    ; that was the last
      (via-set +via-b+ (logxor (via +via-b+) +cuda-tack+)))
    (via-set +via-b+ (logior (via +via-b+) +cuda-tack+ +cuda-tip+))
    (nreverse bytes)))

(defun cuda-send (bytes)
  "Send a packet; then read and return CUDA's reply."
  (loop while (cuda-treq-p) do (cuda-read-packet))          ; it talks first
  (via-set +via-acr+ (logior (via +via-acr+) #x10))          ; shift out
  (via-set +via-sr+ (first bytes))
  (via-set +via-b+ (logand (via +via-b+) (lognot +cuda-tip+)))
  (cuda-wait-sr)
  (dolist (b (rest bytes))
    (via-set +via-sr+ b)
    (via-set +via-b+ (logxor (via +via-b+) +cuda-tack+))
    (cuda-wait-sr))
  (via-set +via-acr+ (logand (via +via-acr+) (lognot #x10)))
  (via +via-sr+)
  (via-set +via-b+ (logior (via +via-b+) +cuda-tack+ +cuda-tip+))
  (dotimes (i 200000 nil)
    (when (cuda-treq-p) (return (cuda-read-packet)))))

(defun cuda-autopoll (on) (cuda-send (list 1 1 (if on 1 0))))

(defun adb-reset ()
  "ADB SendReset: every device back at its default address (keyboard 2, mouse
   3).  Open Firmware's enumeration moves them (QEMU's to 8 and 9), as Mac OS's
   does."
  (cuda-send (list 0 0)))

;;; ---- PMU: a PowerBook's (and a G4 Cube's) ADB master -------------------------
;;;
;;; The PowerBooks' power manager, a microcontroller on the same VIA.  Linux's
;;; drivers/macintosh/via-pmu.c is the reference.  A byte goes through the shift
;;; register with REQ (ours, port B #x10, active low) held while it moves and
;;; ACK (the PMU's, #x08) released when the PMU is ready for the next.  A
;;; command is its byte, then (for a variable-length one) a length byte, then
;;; its data; a variable-length reply is a length byte, then that many.  ADB
;;; goes through command #x20; with autopoll on, the PMU raises the VIA's CB1
;;; when a device has data, and #x78 (interrupt acknowledge) returns the
;;; interrupt bits and the packet: bit #x10 ADB, then the ADB command byte and
;;; the device's register 0 -- what CUDA sends after its two header bytes.

(defconstant +pmu-req+ #x10)
(defconstant +pmu-ack+ #x08)

(defun pmu-wait-ack ()
  (dotimes (i 200000 nil) (when (logtest (via +via-b+) +pmu-ack+) (return t))))

(defun pmu-send-byte (x)
  (via-set +via-acr+ (logior (via +via-acr+) #x1C))           ; shift out, ext clock
  (via-set +via-sr+ x)
  (via-set +via-b+ (logand (via +via-b+) (lognot +pmu-req+)))
  (cuda-wait-sr)
  (via-set +via-b+ (logior (via +via-b+) +pmu-req+))
  (pmu-wait-ack))

(defun pmu-recv-byte ()
  (via-set +via-acr+ (logior (logand (via +via-acr+) (lognot #x10)) #x0C))
  (via +via-sr+)
  (via-set +via-b+ (logand (via +via-b+) (lognot +pmu-req+)))
  (cuda-wait-sr)
  (let ((b (via +via-sr+)))
    (via-set +via-b+ (logior (via +via-b+) +pmu-req+))
    (pmu-wait-ack)
    b))

(defun pmu-command (cmd data &key length-byte reply)
  "Send CMD with DATA (a length byte first with LENGTH-BYTE); REPLY :VAR reads
   a length byte and that many bytes, a number that many, NIL none."
  (pmu-wait-ack)
  (pmu-send-byte cmd)
  (when length-byte (pmu-send-byte (length data)))
  (dolist (b data) (pmu-send-byte b))
  (cond ((eq reply :var) (let ((n (pmu-recv-byte))) (loop repeat n collect (pmu-recv-byte))))
        ((integerp reply) (loop repeat reply collect (pmu-recv-byte)))
        (t nil)))

(defun pmu-init ()
  (via-set +via-dirb+ (logand (logior (via +via-dirb+) +pmu-req+) (lognot +pmu-ack+)))
  (via-set +via-acr+ (logior (logand (via +via-acr+) (lognot #x1C)) #x0C))
  (via-set +via-ier+ #x7F)
  (via-set +via-b+ (logior (via +via-b+) +pmu-req+))
  (via-set +via-ifr+ #x7F)
  (pmu-command #x70 (list #x10))                    ; PMU_SET_INTR_MASK: ADB
  t)

(defun pmu-adb-reset () (pmu-command #x20 (list 0 0 0 0) :length-byte t))
(defun pmu-autopoll () (pmu-command #x20 (list 0 #x86 #xFF #xFF) :length-byte t))

(defun pmu-poll ()
  "Each pending PMU interrupt: its ADB data as a CUDA-shaped packet.  The PMU
   signals an interrupt on a mac-io GPIO line on the KeyLargo machines (and in
   QEMU), not the VIA's CB1 -- so ask: interrupt-acknowledge answers 0 when
   nothing is pending."
  (let ((n 0))
    (dotimes (i 8 n)
      (via-set +via-ifr+ #x10)
      (let ((r (pmu-command #x78 nil :reply :var)))
        (unless (and r (logtest (first r) #x10) (rest r)) (return n))
        ;; [ints cmd data...] -> [0 flags cmd data...]
        (%mac-adb-packet (list* 0 #x40 (rest r)))
        (incf n)))))

;;; ---- which one this Mac has: the boot stub's answer -----------------------------

(defvar *mac-via-kind* nil "1 CUDA, 2 PMU (boot-ppcmac's boot info, +28).")

(defun mac-boot-info (off)
  ;; MEM-REF :U32, not (+ (* 65536 hi) lo): a word >= 2^29 (a PCI address) is a
  ;; bignum here, which MEM-REF builds and a fixnum multiply would wrap
  (mem-ref (+ #x07F80000 off) :u32))

(defvar *usb-models* '("PowerBook5,6" "PowerBook5,7" "PowerBook5,8" "PowerBook5,9"
                        "PowerBook6,7" "PowerBook6,8")
  "PMU machines whose keyboard and trackpad are on USB (2005): USB at boot.")
(defvar *usb-at-boot* :auto "T, NIL, or :AUTO (USB-AT-BOOT-P decides).")

(defun usb-at-boot-p ()
  (if (eq *usb-at-boot* :auto)
      (or (/= *mac-via-kind* 2)
          (let ((m (ignore-errors (funcall 'dt-string (funcall 'dt) "model"))))
            (and m (member m *usb-models* :test #'string=) t)))
      *usb-at-boot*))

(defun mac-input-init ()
  (setq *mac-via-kind* (if (and (= (mem-ref #x07F80000 :u16) #x5050) (= (mac-boot-info 28) 2)) 2 1))
  (if (= *mac-via-kind* 2)
      (progn (pmu-init) (pmu-adb-reset) (pmu-autopoll))
      (progn (cuda-init) (adb-reset) (cuda-autopoll t)))
  (mac-set-clock)
  ;; USB keyboards and mice (ppcmac-usb.lisp, when the payload has it): at boot
  ;; where the machine's own keyboard is USB, or there is no PMU; elsewhere
  ;; (USB-INIT) from the listener starts it
  (when (and (fboundp 'usb-init) (usb-at-boot-p))
    (handler-case (funcall 'usb-init)
      (error (c) (ignore-errors (modus-int::write-string-serial "usb: ") (princ c)))))
  *mac-via-kind*)

;;; ---- restart ---------------------------------------------------------------
;;; (REBOOT): the machine restarts as from its power key, into Open Firmware's
;;; auto-boot.  To come back into Modus over the network every time, set once at
;;; the ok prompt (Cmd-Opt-P-R at the chime undoes it):
;;;   setenv boot-device enet:10.0.0.1,modus-boot.elf,10.0.0.9
;;;   setenv boot-file enet:10.0.0.1,modus.elf,10.0.0.9
;;;   setenv auto-boot? true
;;; (BOOT with no arguments loads boot-device and hands boot-file over as the
;;; bootargs, which modus-boot.elf reads as the image's path.)  The PMU's
;;; PMU_RESET (#xD0, no data) or CUDA's pseudo command RESET_SYSTEM (#x11), as
;;; Linux's pmu_restart / cuda_restart.

(defun reboot ()
  (if (= (if (and (= (mem-ref #x07F80000 :u16) #x5050) (= (mac-boot-info 28) 2)) 2 1) 2)
      (pmu-command #xD0 nil)
      (cuda-send (list 1 #x11)))
  (loop))                                       ; until the machine goes

;;; ---- the clock -------------------------------------------------------------
;;;
;;; Both microcontrollers keep the real-time clock: seconds since 1904-01-01
;;; (the Mac's epoch; Mac OS keeps it in local time), four bytes, big-endian.
;;; CUDA's pseudo command 3 (GET_TIME) answers [1 flags 3 t3 t2 t1 t0]; the
;;; PMU's #x38 (READ_RTC) answers the four bytes.  CL's epoch is 1900: four
;;; years (1900 was not a leap year) earlier.

(defconstant +mac-epoch-offset+ (* 1460 86400) "1904-01-01 minus 1900-01-01: 4 x 365 days.")

(defun %be32 (bytes) (reduce (lambda (a b) (+ (* a 256) b)) bytes :initial-value 0))

(defun mac-rtc ()
  "The Mac's clock as a universal time, or NIL."
  (if (eql *mac-via-kind* 2)
      (let ((r (pmu-command #x38 nil :reply 4)))
        (and (= (length r) 4) (+ (%be32 r) +mac-epoch-offset+)))
      (dotimes (i 8 nil)
        (let ((p (if (zerop i) (cuda-send (list 1 3))
                     (dotimes (j 200000 nil) (when (cuda-treq-p) (return (cuda-read-packet)))))))
          (cond ((null p) (return nil))
                ((and (= (first p) 1) (>= (length p) 7) (= (third p) 3))
                 (return (+ (%be32 (subseq p 3 7)) +mac-epoch-offset+)))
                (t (%mac-adb-packet p)))))))      ; an ADB packet got there first

(defun mac-set-clock ()
  "Set the image's clock from the Mac's.  The universal time, or NIL."
  (let ((ut (ignore-errors (mac-rtc))))
    (when (and ut (> ut 3600000000))              ; after 2014: a clock that was set
      (modus-int::set-universal-time ut))
    ut))

;;; ---- events ---------------------------------------------------------------

(defvar *mac-mouse-x* 512)
(defvar *mac-mouse-y* 384)
(defvar *mac-mouse-down* nil)
(defvar *mac-screen-w* 1024)
(defvar *mac-screen-h* 768)
(defvar *mac-shift* nil)
(defvar *mac-control* nil)
(defvar *mac-events* nil "Pending events, oldest first: (:pointer mask x y) or (:key down keysym).")

(defun %adb7 (b) (let ((v (logand b #x7F))) (if (>= v 64) (- v 128) v)))

;; ADB key code -> (keysym shifted-keysym), US layout
(defvar *adb-keymap* (make-array 128 :initial-element nil))
(let ((pairs '((0 "a") (1 "s") (2 "d") (3 "f") (4 "h") (5 "g") (6 "z") (7 "x") (8 "c") (9 "v")
               (11 "b") (12 "q") (13 "w") (14 "e") (15 "r") (16 "y") (17 "t")
               (18 "1!") (19 "2@") (20 "3#") (21 "4$") (22 "6^") (23 "5%") (24 "=+") (25 "9(")
               (26 "7&") (27 "-_") (28 "8*") (29 "0)") (30 "]}") (31 "o") (32 "u") (33 "[{")
               (34 "i") (35 "p") (37 "l") (38 "j") (39 "'\"") (40 "k") (41 ";:") (42 "\\|")
               (43 ",<") (44 "/?") (45 "n") (46 "m") (47 ".>") (49 "  ") (50 "`~"))))
  (dolist (p pairs)
    (let ((s (second p)))
      (setf (svref *adb-keymap* (first p))
            (if (= (length s) 1)
                (list (char-code (char s 0)) (char-code (char-upcase (char s 0))))
                (list (char-code (char s 0)) (char-code (char s 1))))))))
(dolist (p '((36 #xFF0D) (48 #xFF09) (51 #xFF08) (53 #xFF1B) (117 #xFFFF) (76 #xFF0D)
             (123 #xFF51) (126 #xFF52) (124 #xFF53) (125 #xFF54)       ; arrows (extended codes)
             (59 #xFF51) (62 #xFF52) (60 #xFF53) (61 #xFF54)           ; arrows (ADB ext keyboard)
             (115 #xFF50) (119 #xFF57) (116 #xFF55) (121 #xFF56)
             (56 #xFFE1) (54 #xFFE3) (58 #xFFE9) (55 #xFFEB)))
  (setf (svref *adb-keymap* (first p)) (list (second p) (second p))))

(defvar *mac-keys-down* nil "ADB codes seen pressed and not yet released.")

(defun %mac-key (code down)
  "Queue the key event(s) for ADB CODE.  A release of a key never seen pressed
   is a whole keystroke: CUDA keeps one packet, and a press can be replaced by
   its release before the host reads it (QEMU's sendkey)."
  (let ((e (svref *adb-keymap* (logand code #x7F))))
    (when e
      (when (= (first e) #xFFE1) (setq *mac-shift* down))
      (when (= (first e) #xFFE3) (setq *mac-control* down))
      (let ((sym (if *mac-shift* (second e) (first e))))
        (cond (down (pushnew code *mac-keys-down*)
                    (setq *mac-events* (append *mac-events* (list (list :key t sym)))))
              ((member code *mac-keys-down*)
               (setq *mac-keys-down* (remove code *mac-keys-down*))
               (setq *mac-events* (append *mac-events* (list (list :key nil sym)))))
              (t (setq *mac-events* (append *mac-events* (list (list :key t sym) (list :key nil sym))))))))))

(defun %mac-adb-packet (p)
  ;; [0 flags cmd data...]
  (when (and (>= (length p) 4) (= (first p) 0))
    (let ((addr (ash (third p) -4)) (data (cdddr p)))
      (case addr
        ((2 8) (dolist (b data)
                 (unless (= b #xFF) (%mac-key (logand b #x7F) (not (logtest b #x80))))))
        ((3 9) (when (>= (length data) 2)
             ;; byte 0 bit 7 clear: button 1 down; byte 1 bit 7 clear: button 2
             ;; (a two-button ADB mouse, QEMU's).  glass.wm's mask: bit 0 left,
             ;; bit 2 right.  A one-button Mac's right click is Control-click.
             (let* ((dy (%adb7 (first data))) (dx (%adb7 (second data)))
                    (b1 (not (logtest (first data) #x80)))
                    (b2 (not (logtest (second data) #x80)))
                    (mask (cond ((and b1 *mac-control*) 4)
                                (t (logior (if b1 1 0) (if b2 4 0))))))
               (setq *mac-mouse-x* (max 0 (min (1- *mac-screen-w*) (+ *mac-mouse-x* dx)))
                     *mac-mouse-y* (max 0 (min (1- *mac-screen-h*) (+ *mac-mouse-y* dy)))
                     *mac-mouse-down* (/= mask 0))
               (setq *mac-events*
                     (append *mac-events*
                             (list (list :pointer mask *mac-mouse-x* *mac-mouse-y*)))))))))))

(defun mac-input-poll ()
  "Read every packet the ADB master has, and every USB report; queue their
   events.  How many ADB packets."
  (when (fboundp 'usb-poll) (funcall 'usb-poll))
  (if (eql *mac-via-kind* 2)
      (pmu-poll)
      (let ((n 0))
        (loop while (cuda-treq-p)
              do (%mac-adb-packet (cuda-read-packet)) (incf n))
        n)))

(defun mac-next-event ()
  (pop *mac-events*))
