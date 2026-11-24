;;;; ppcmac-usb.lisp -- USB keyboards and mice on a PowerPC Mac, through OHCI.
;;;;
;;;; The PowerBooks of 2005 (PowerBook5,6..5,9) have no ADB: their keyboard and
;;;; trackpad are one USB device on the machine's own bus, and every New World
;;;; Mac takes a USB keyboard and mouse.  This is a small polled USB host:
;;;;
;;;;   - the device tree (boot-ppcmac copies Open Firmware's to #x07B00000)
;;;;     names the controllers: PCI class #x0C0310 OHCI, #x0C0320 EHCI;
;;;;   - EHCI, where there is one, is stopped and its ports given to its OHCI
;;;;     companions (CONFIGFLAG 0): every keyboard and mouse is full or low
;;;;     speed, which OHCI speaks;
;;;;   - each OHCI is reset and run, its root ports powered and reset, and the
;;;;     device on each enumerated (no hubs yet): address, configuration, and
;;;;     each HID interface -- boot keyboard, boot mouse, or an Apple trackpad --
;;;;     gets an interrupt endpoint polled every frame.
;;;;
;;;; Register and descriptor words are little-endian (PCI's), read and written
;;;; whole by the image's PPCMAC-IN32LE / -OUT32LE.  DMA memory is #x07C00000..
;;;; #x07D00000, identity-mapped and cache-coherent (Uni-N snoops).  The OHCI
;;;; specification (1.0a) and Linux's ohci-hcd and appletouch are the references.

(in-package :cl-user)

;;; ---- the device tree ---------------------------------------------------------

(defstruct dtn props children parent)

(defun dt-read ()
  "The device tree the boot stub copied, as DTN nodes: the root, or NIL."
  (let ((a #x07B00000))
    (unless (eql (mem-ref a :u32) #x44545245) (return-from dt-read nil))
    (incf a 4)
    (let ((stack nil) (root nil))
      (loop
        (let ((tag (mem-ref a :u32)))
          (incf a 4)
          (case tag
            (1 (let ((n (make-dtn :parent (first stack))))
                 (if stack (push n (dtn-children (first stack))) (setq root n))
                 (push n stack)))
            (2 (let* ((name (let ((s (make-string-output-stream)))
                              (dotimes (i 32)
                                (let ((c (mem-ref (+ a i) :u8)))
                                  (when (zerop c) (return))
                                  (write-char (code-char c) s)))
                              (get-output-stream-string s)))
                      (kept (mem-ref (+ a 36) :u32))
                      (v (make-array kept :element-type '(unsigned-byte 8))))
                 (dotimes (i kept) (setf (aref v i) (mem-ref (+ a 40 i) :u8)))
                 (push (list name v (+ a 40)) (dtn-props (first stack)))
                 (setq a (+ a 40 (* 4 (ceiling kept 4))))))
            (3 (let ((n (pop stack)))
                 (setf (dtn-props n) (nreverse (dtn-props n))
                       (dtn-children n) (nreverse (dtn-children n)))
                 (when (null stack) (return root))))
            (t (return root))))))))

(defvar *dt* nil)
(defun dt () (or *dt* (setq *dt* (dt-read))))

(defun dt-prop (n name) (second (assoc name (dtn-props n) :test #'string=)))

(defun dt-string (n name)
  (let ((v (dt-prop n name)))
    (and v (map 'string #'code-char (subseq v 0 (or (position 0 v) (length v)))))))

(defun dt-words (n name)
  "Property NAME as big-endian words, read where the stub left them: MEM-REF
   :U32 makes a word >= 2^29 a bignum, where shifting bytes together would wrap."
  (let ((p (assoc name (dtn-props n) :test #'string=)))
    (and p (loop for i from 0 below (- (length (second p)) 3) by 4
                 collect (mem-ref (+ (third p) i) :u32)))))

(defun dt-all (&optional (n (dt)))
  (and n (cons n (loop for c in (dtn-children n) append (dt-all c)))))

(defun dt-path (n)
  (if (dtn-parent n)
      (concatenate 'string (let ((p (dt-path (dtn-parent n)))) (if (string= p "/") "" p))
                   "/" (or (dt-string n "name") "?"))
      "/"))

(defun dt-pci-mem (n)
  "The first 32-bit memory range of PCI node N's assigned-addresses: its PA."
  (let ((w (dt-words n "assigned-addresses")))
    (loop while (>= (length w) 5)
          do (when (= (mod (floor (first w) #x1000000) 4) 2) (return (third w)))
             (setq w (nthcdr 5 w)))))

(defun mac-pa->va (pa)
  "The VA of device PA in boot-ppcmac's device window, or NIL."
  (let ((base (mac-boot-info 52)) (size (mac-boot-info 56)))
    (and pa (plusp size) (<= base pa) (< pa (+ base size))
         (+ #x08000000 (- pa base)))))

;;; ---- registers and descriptors: little-endian words -----------------------------

;; A register word can be >= 2^29, a bignum: take its bits with FLOOR and MOD.
;; (LDB and a constant ASH compile to an inline shift of a fixnum, which a
;; bignum's pointer is not.)
(defun le (a) (modus-int::ppcmac-in32le a))
(defun %bits (v from n) (mod (floor v (expt 2 from)) (expt 2 n)))
(defun le! (a v) (modus-int::ppcmac-out32le a v))

(defun usb-ms (ms)
  (let ((end (+ (get-internal-real-time) (* ms (floor internal-time-units-per-second 1000)))))
    (loop until (>= (get-internal-real-time) end))))

;;; ---- the controllers -------------------------------------------------------

(defun ehci-release (va)
  "Stop the EHCI at VA and route its ports to the companion OHCIs."
  (let* ((caplen (%bits (le va) 0 8)) (op (+ va caplen)))
    (le! op (logand (le op) (lognot 1)))                       ; USBCMD.RS = 0
    (dotimes (i 100) (when (logtest (le (+ op 4)) #x1000) (return)) (usb-ms 1))
    (le! (+ op #x40) 0)))                                       ; CONFIGFLAG = 0

;;; DMA memory: per controller 64 KB at #x07C00000 + 64K*k --
;;;   +#x0000 HCCA (256)    +#x0100 the control ED    +#x0200 interrupt EDs (16 x 16)
;;;   +#x0400 TDs (32 bytes a slot: 0..63 control, a ring; 64.. nine per interrupt ED)
;;;   +#x2000 the control buffer (256)
;;;   +#x2100 interrupt buffers: 128 bytes per interrupt TD slot
(defconstant +usb-dma+ #x07C00000)

(defstruct ohci va dma ports (next-addr 1) (ctl-td 0) (ctl-tail 0) (int-eds 0) (low nil))
(defstruct hid ohci addr iface kind ed pending dummy (maxp 8) (last nil) info x-old y-old (base nil))

(defconstant +hid-ring+ 8
  "Transfers kept queued per interrupt endpoint.  A device answers only a
   waiting transfer, and a keyboard's press and release between two would
   be one report -- the key lost -- so the controller always has several to
   fill while Lisp is busy elsewhere.")

(defvar *ohcis* nil)
(defvar *usb-hids* nil)
(defvar *usb-log* nil "What USB found, newest first: strings.")

(defun usb-log (fmt &rest args)
  (let ((s (apply #'format nil fmt args)))
    (push s *usb-log*)
    (ignore-errors (modus-int::write-string-serial "usb: ")
                   (modus-int::write-string-serial s)
                   (modus-int::write-char-serial 10))))

(defun %oreg (o r) (+ (ohci-va o) r))
(defun %td-slot (o i) (+ (ohci-dma o) #x400 (* 32 i)))
(defun %ctl-slot (o i) (%td-slot o (mod i 64)))
(defun %buf (o i) (+ (ohci-dma o) #x2000 (* 256 i)))

(defun %zero (a n) (dotimes (i (floor n 4)) (setf (mem-ref (+ a (* 4 i)) :u32) 0)))

(defun ohci-start (va k)
  (let* ((dma (+ +usb-dma+ (* k #x10000)))
         (o (make-ohci :va va :dma dma)))
    (%zero dma #x2000)
    (let ((fi (%bits (le (%oreg o #x34)) 0 14)))
      (when (< fi 11000) (setq fi #x2EDF))
      (le! (%oreg o #x08) 1)                                     ; HCR
      (dotimes (i 100) (unless (logtest (le (%oreg o #x08)) 1) (return)) (usb-ms 1))
      (le! (%oreg o #x14) #xC000007F)                            ; every interrupt off
      (le! (%oreg o #x0C) #x7F)
      (le! (%oreg o #x18) dma)                                   ; HCCA
      ;; the control ED, empty: head = tail = a dummy TD
      (setf (ohci-ctl-td o) 0 (ohci-ctl-tail o) (%td-slot o 0))
      (let ((ed (+ dma #x100)))
        (le! ed (ash 8 16)) (le! (+ ed 4) (ohci-ctl-tail o)) (le! (+ ed 8) (ohci-ctl-tail o)) (le! (+ ed 12) 0))
      (le! (%oreg o #x20) (+ dma #x100))
      (le! (%oreg o #x28) 0)
      (le! (%oreg o #x34) (logior (ash (floor (* (- fi 210) 6) 7) 16) fi))
      (le! (%oreg o #x40) (floor (* fi 9) 10))
      (le! (%oreg o #x44) #x628)
      (le! (%oreg o #x04) #x97)                                  ; operational; periodic, control lists
      ;; power: global, and per port
      (le! (%oreg o #x50) #x10000)
      (setf (ohci-ports o) (%bits (le (%oreg o #x48)) 0 8))
      (dotimes (p (ohci-ports o)) (le! (%oreg o (+ #x54 (* 4 p))) #x100))
      (usb-ms 100)
      o)))

;;; ---- transfers ----------------------------------------------------------------

(defun %td (a info cbp next be)
  (le! a info) (le! (+ a 4) cbp) (le! (+ a 8) next) (le! (+ a 12) be))

(defun ohci-control (o addr maxp setup &optional (len 0) (in t))
  "A control transfer to ADDR's endpoint 0: the 8 SETUP bytes, then LEN data
   bytes (IN, or OUT from the buffer), then status.  The data (an octet vector)
   or T for an OUT, or NIL when it failed."
  (let* ((ed (+ (ohci-dma o) #x100))
         (buf (%buf o 0))
         (tds (loop for i from 1 to 4 collect (%ctl-slot o (+ (ohci-ctl-td o) i))))
         (first-td (ohci-ctl-tail o))
         (data-td (first tds)) (status-td (if (plusp len) (second tds) (first tds)))
         (new-tail (if (plusp len) (third tds) (second tds))))
    (dotimes (i 8) (setf (mem-ref (+ buf i) :u8) (nth i setup)))
    ;; ED: this address, its speed, this packet size (idle: head = tail)
    (le! ed (logior addr (if (ohci-low o) (ash 1 13) 0) (ash (logand maxp #x7FF) 16)))
    ;; SETUP, DATA0;  DATA, DATA1 first;  STATUS, DATA1, the other direction
    (%td first-td (logior (ash 2 24) (ash 7 21)) buf (if (plusp len) data-td status-td) (+ buf 7))
    (when (plusp len)
      (%td data-td (logior (ash 3 24) (ash 7 21) (ash 1 18) (ash (if in 2 1) 19))
           (+ buf 16) status-td (+ buf 16 len -1)))
    (%td status-td (logior (ash 3 24) (ash 7 21) (ash (if (and in (plusp len)) 1 2) 19)) 0 new-tail 0)
    (%td new-tail 0 0 0 0)
    (modus-int::ppcmac-sync)
    (le! (+ ed 4) new-tail)                                      ; TailP: go
    (le! (%oreg o #x08) 2)                                       ; CLF
    (setf (ohci-ctl-td o) (+ (ohci-ctl-td o) (if (plusp len) 3 2)) (ohci-ctl-tail o) new-tail)
    (let ((ok (dotimes (i 500 nil)
                (let ((head (le (+ ed 8))))
                  (when (logtest head 1) (return nil))          ; halted
                  (when (= (logand head (lognot 15)) new-tail) (return t)))
                (usb-ms 1))))
      (unless ok
        ;; a halt or a timeout: empty the ED (head = tail, no halt) for the next one
        (le! (+ ed 8) new-tail)
        (return-from ohci-control nil))
      (if (and in (plusp len))
          (let* ((cbp (le (+ data-td 4)))
                 (got (if (zerop cbp) len (- cbp (+ buf 16))))
                 (v (make-array got :element-type '(unsigned-byte 8))))
            (dotimes (i got v) (setf (aref v i) (mem-ref (+ buf 16 i) :u8))))
          t))))

(defun %setup (rt req value index len)
  (list rt req (logand value #xFF) (ash value -8) (logand index #xFF) (ash index -8)
        (logand len #xFF) (ash len -8)))

(defun usb-get-descriptor (o addr maxp type index len)
  (ohci-control o addr maxp (%setup #x80 6 (+ (* type 256) index) 0 len) len t))

;;; ---- the interrupt endpoints -------------------------------------------------

(defun %int-buf (o td)
  "The 128-byte buffer of interrupt TD (by its slot)."
  (+ (ohci-dma o) #x2100 (* 128 (- (floor (- td (ohci-dma o) #x400) 32) 64))))

(defun %hid-arm (h td new-dummy)
  (let ((buf (%int-buf (hid-ohci h) td)))
    (%td td (logior (ash 7 21) (ash 1 18) (ash 2 19)) buf new-dummy (+ buf 127))
    (%td new-dummy 0 0 0 0)
    (modus-int::ppcmac-sync)
    (le! (+ (hid-ed h) 4) new-dummy)
    (setf (hid-pending h) (append (hid-pending h) (list td)) (hid-dummy h) new-dummy)))

(defun hid-add (o addr iface kind ep maxp low-speed info)
  (let* ((k (ohci-int-eds o))
         (ed (+ (ohci-dma o) #x200 (* 16 k)))
         (slots (loop for j below (1+ +hid-ring+) collect (%td-slot o (+ 64 (* (1+ +hid-ring+) k) j))))
         (h (make-hid :ohci o :addr addr :iface iface :kind kind :ed ed :maxp maxp :info info
                      :dummy (first slots))))
    (when (>= k 16) (return-from hid-add nil))
    (%td (first slots) 0 0 0 0)
    (le! ed (logior addr (ash (logand ep 15) 7) (if low-speed (ash 1 13) 0) (ash (logand maxp #x7FF) 16)))
    (le! (+ ed 4) (first slots)) (le! (+ ed 8) (first slots)) (le! (+ ed 12) 0)
    ;; chain it: every HCCA slot points at the first; each ED at the next
    (if (zerop k)
        (dotimes (i 32) (le! (+ (ohci-dma o) (* 4 i)) ed))
        (le! (+ (ohci-dma o) #x200 (* 16 (1- k)) 12) ed))
    (setf (ohci-int-eds o) (1+ k))
    ;; the ring: each slot armed in turn, the next one its dummy
    (loop for (td next) on slots while next do (%hid-arm h td next))
    (push h *usb-hids*)
    h))

(defun hid-poll (h)
  "Every transfer of H's that the controller has finished, oldest first: its
   bytes to HID-REPORT, and the TD armed again at the tail.  The controller
   retires in order and its HeadP names the first one not done, so every
   pending TD before it is done."
  (let* ((o (hid-ohci h)) (ed (hid-ed h)) (headp (le (+ ed 8)))
         (head (logand headp (lognot 15))))
    (loop repeat +hid-ring+          ; at most the ring: a HeadP outside it is not a reason to spin
          while (and (hid-pending h) (/= (first (hid-pending h)) head))
          do (let* ((td (pop (hid-pending h)))
                    (cc (%bits (le td) 28 4)) (cbp (le (+ td 4)))
                    (buf (%int-buf o td))
                    (n (if (zerop cbp) 128 (- cbp buf))))
               (when (and (or (zerop cc) (= cc 9)) (<= 0 n 128))      ; done, or short
                 (let ((v (make-array n :element-type '(unsigned-byte 8))))
                   (dotimes (i n) (setf (aref v i) (mem-ref (+ buf i) :u8)))
                   (hid-report h v)))
               (%hid-arm h (hid-dummy h) td)))
    (when (logtest headp 1)                                       ; halted: go on from here
      (le! (+ ed 8) head))))

;;; ---- enumeration ---------------------------------------------------------------

(defconstant +apple-trackpads+
  '((#x020E . :fountain) (#x020F . :fountain) (#x0210 . :fountain) (#x030A . :fountain)
    (#x030B . :geyser1) (#x0214 . :geyser2) (#x0215 . :geyser2) (#x0216 . :geyser2))
  "Apple's USB trackpads that send their sensors, not motion (Linux appletouch):
   Fountain in the PowerBooks of February 2005, Geyser 2 in October's.")

(defun usb-enumerate (o port)
  (let* ((st (le (%oreg o (+ #x54 (* 4 port)))))
         (low (logtest st #x200)))
    (setf (ohci-low o) low)
    ;; reset the port; wait for it
    (le! (%oreg o (+ #x54 (* 4 port))) #x10)
    (dotimes (i 100) (when (logtest (le (%oreg o (+ #x54 (* 4 port)))) #x100000) (return)) (usb-ms 1))
    (le! (%oreg o (+ #x54 (* 4 port))) #x100000)
    (usb-ms 20)
    (let* ((d8 (usb-get-descriptor o 0 8 1 0 8))
           (maxp (if (and d8 (>= (length d8) 8)) (aref d8 7) 8))
           (addr (ohci-next-addr o)))
      (unless d8 (return-from usb-enumerate (usb-log "port ~D: no answer" port)))
      (setf (ohci-next-addr o) (1+ addr))
      (ohci-control o 0 maxp (%setup 0 5 addr 0 0))
      (usb-ms 10)
      (let* ((dd (usb-get-descriptor o addr maxp 1 0 18))
             (vendor (and dd (>= (length dd) 12) (+ (aref dd 8) (* 256 (aref dd 9)))))
             (product (and dd (>= (length dd) 12) (+ (aref dd 10) (* 256 (aref dd 11)))))
             (c9 (usb-get-descriptor o addr maxp 2 0 9))
             (total (if (and c9 (>= (length c9) 4)) (+ (aref c9 2) (* 256 (aref c9 3))) 0))
             (cd (and (> total 0) (usb-get-descriptor o addr maxp 2 0 (min total 240)))))
        (usb-log "port ~D: ~:[full~;low~] speed, ~4,'0X:~4,'0X, address ~D" port low vendor product addr)
        (unless cd (return-from usb-enumerate nil))
        (ohci-control o addr maxp (%setup 0 9 (aref cd 5) 0 0))            ; SET_CONFIGURATION
        ;; the interfaces: each HID one with an interrupt IN endpoint
        (let ((i 0) (iface nil))
          (loop while (< (+ i 1) (length cd))
                do (let ((len (aref cd i)) (type (aref cd (1+ i))))
                     (when (zerop len) (return))
                     (cond ((and (= type 4) (>= len 9))
                            (setq iface (list (aref cd (+ i 2)) (aref cd (+ i 5)) (aref cd (+ i 6)) (aref cd (+ i 7)))))
                           ((and (= type 5) (>= len 7) iface (= (first (cdr iface)) 3)
                                 (logtest (aref cd (+ i 2)) #x80) (= (logand (aref cd (+ i 3)) 3) 3))
                            (destructuring-bind (num class sub proto) iface
                              (declare (ignore class))
                              (let* ((ep (logand (aref cd (+ i 2)) 15))
                                     (emaxp (+ (aref cd (+ i 4)) (* 256 (logand (aref cd (+ i 5)) 7))))
                                     (pad (and (eql vendor #x05AC) (= proto 2)
                                               (cdr (assoc product +apple-trackpads+))))
                                     (kind (cond (pad :trackpad) ((= proto 1) :keyboard) ((= proto 2) :mouse))))
                                (when kind
                                  (cond (pad
                                         (unless (eq pad :fountain) (usb-geyser-raw o addr maxp)))
                                        ((= sub 1)
                                         (ohci-control o addr maxp (%setup #x21 #x0B 0 num 0))))  ; SET_PROTOCOL boot
                                  (when (eq kind :keyboard)
                                    (ohci-control o addr maxp (%setup #x21 #x0A 0 num 0)))        ; SET_IDLE 0
                                  (usb-log "  interface ~D: ~(~A~)~@[ (~(~A~))~], endpoint ~D" num kind pad ep)
                                  (hid-add o addr num kind ep emaxp low pad))
                                (setq iface nil)))))
                       (incf i len))))))))

(defun usb-geyser-raw (o addr maxp)
  "A Geyser trackpad starts as a mouse: ask it for its sensors (appletouch's
   atp_geyser_init: feature report #x300, byte 0 := 4)."
  (let ((r (ohci-control o addr maxp (%setup #xA1 1 #x300 0 8) 8 t)))
    (when r
      (let ((buf (+ (%buf o 0) 16)))
        (dotimes (i 8) (setf (mem-ref (+ buf i) :u8) (if (< i (length r)) (aref r i) 0)))
        (setf (mem-ref buf :u8) 4)
        (ohci-control o addr maxp (%setup #x21 9 #x300 0 8) 8 nil)))))

(defun usb-init ()
  "Find, start and enumerate every OHCI the firmware named.  The HIDs found."
  (setq *ohcis* nil *usb-hids* nil *dt* nil)
  (let ((nodes (dt-all)))
    (unless nodes (usb-log "no device tree") (return-from usb-init nil))
    (dolist (n nodes)
      (let ((cc (first (dt-words n "class-code"))))
        (when (eql cc #x0C0320)
          (let ((va (mac-pa->va (dt-pci-mem n))))
            (usb-log "EHCI ~A: ~:[outside the device window~;released~]" (dt-path n) va)
            (when va (ehci-release va))))))
    (let ((k 0))
      (dolist (n nodes)
        (when (and (eql (first (dt-words n "class-code")) #x0C0310) (< k 4))
          (let* ((pa (dt-pci-mem n)) (va (mac-pa->va pa)))
            (if (null va)
                (usb-log "OHCI ~A at ~@[#x~X~]: outside the device window" (dt-path n) pa)
                (let ((o (ohci-start va k)))
                  (incf k)
                  (push o *ohcis*)
                  (usb-log "OHCI ~A at #x~X: ~D ports" (dt-path n) pa (ohci-ports o))
                  (dotimes (p (ohci-ports o))
                    (when (logtest (le (%oreg o (+ #x54 (* 4 p)))) 1)
                      (ignore-errors (usb-enumerate o p))))))))))
    *usb-hids*))

(defun usb-poll ()
  (dolist (h *usb-hids*) (hid-poll h)))

;;; ---- reports to events --------------------------------------------------------

(defvar *hid-keymap* (make-array 256 :initial-element nil))
(let ((pairs '((30 "1!") (31 "2@") (32 "3#") (33 "4$") (34 "5%") (35 "6^") (36 "7&") (37 "8*")
               (38 "9(") (39 "0)") (44 "  ") (45 "-_") (46 "=+") (47 "[{") (48 "]}") (49 "\\|")
               (51 ";:") (52 "'\"") (53 "`~") (54 ",<") (55 ".>") (56 "/?"))))
  (dotimes (i 26)
    (setf (svref *hid-keymap* (+ 4 i)) (list (+ 97 i) (+ 65 i))))
  (dolist (p pairs)
    (setf (svref *hid-keymap* (first p)) (list (char-code (char (second p) 0)) (char-code (char (second p) 1))))))
(dolist (p '((40 #xFF0D) (41 #xFF1B) (42 #xFF08) (43 #xFF09) (76 #xFFFF) (74 #xFF50) (77 #xFF57)
             (75 #xFF55) (78 #xFF56) (79 #xFF53) (80 #xFF51) (81 #xFF54) (82 #xFF52) (88 #xFF0D)))
  (setf (svref *hid-keymap* (first p)) (list (second p) (second p))))

(defun %queue (e) (setq *mac-events* (append *mac-events* (list e))))

(defun hid-keyboard (h r)
  ;; [modifiers reserved k1..k6]: a key in the new report and not the last is
  ;; pressed, in the last and not the new released
  (when (>= (length r) 8)
    (let* ((mods (aref r 0)) (shift (logtest mods #x22)) (ctrl (logtest mods #x11))
           (old (or (hid-last h) (make-array 8 :initial-element 0)))
           (omods (aref old 0)))
      (unless (eq ctrl (logtest omods #x11)) (setq *mac-control* ctrl) (%queue (list :key ctrl #xFFE3)))
      (unless (eq shift (logtest omods #x22)) (%queue (list :key shift #xFFE1)))
      (loop for i from 2 below 8
            for k = (aref old i)
            when (and (> k 3) (not (find k r :start 2)))
              ;; released as it was pressed: the old report's shift
              do (let ((e (svref *hid-keymap* k)))
                   (when e (%queue (list :key nil (if (logtest omods #x22) (second e) (first e)))))))
      (loop for i from 2 below 8
            for k = (aref r i)
            when (and (> k 3) (not (find k old :start 2)))
              do (let ((e (svref *hid-keymap* k))) (when e (%queue (list :key t (if shift (second e) (first e)))))))
      (setf (hid-last h) (subseq r 0 8)))))

(defun %s8 (b) (if (>= b 128) (- b 256) b))

(defun %pointer (dx dy buttons)
  (setq *mac-mouse-x* (max 0 (min (1- *mac-screen-w*) (+ *mac-mouse-x* dx)))
        *mac-mouse-y* (max 0 (min (1- *mac-screen-h*) (+ *mac-mouse-y* dy))))
  (let ((mask (cond ((and (logtest buttons 1) *mac-control*) 4)
                    (t (logior (if (logtest buttons 1) 1 0) (if (logtest buttons 2) 4 0)
                               (if (logtest buttons 4) 2 0))))))
    (setq *mac-mouse-down* (/= mask 0))
    (%queue (list :pointer mask *mac-mouse-x* *mac-mouse-y*))))

(defun hid-mouse (h r)
  (declare (ignore h))
  (when (>= (length r) 3)
    (%pointer (%s8 (aref r 1)) (%s8 (aref r 2)) (aref r 0))))

;;; An Apple trackpad's report is its sensors: a row of X and a row of Y
;;; capacitances.  A finger is where they rise over the untouched baseline;
;;; its position the weighted mean of the risen sensors; the pointer moves by
;;; the change.  appletouch.c's arithmetic: 26 X and 16 Y slots, threshold 5.

(defun trackpad-sensors (kind r)
  "26 X then 16 Y values from report R, laid out as KIND's model lays them."
  (let ((xy (make-array 42 :initial-element 0)))
    (flet ((at (j) (if (< j (length r)) (aref r j) 0)))
      (if (eq kind :geyser2)
          (progn
            (loop for i from 0 below 20 by 2 for j from 19 by 3
                  do (setf (svref xy i) (at j) (svref xy (1+ i)) (at (1+ j))))
            (loop for i from 0 below 9 by 2 for j from 1 by 3
                  do (setf (svref xy (+ 26 i)) (at j))
                     (when (< (1+ i) 16) (setf (svref xy (+ 26 i 1)) (at (1+ j))))))
          (dotimes (i 8)
            (setf (svref xy i) (at (+ (* 5 i) 2))
                  (svref xy (+ i 8)) (at (+ (* 5 i) 4))
                  (svref xy (+ i 16)) (at (+ (* 5 i) 42)))
            (when (< i 2) (setf (svref xy (+ i 24)) (at (+ (* 5 i) 44))))
            (setf (svref xy (+ 26 i)) (at (+ (* 5 i) 1))
                  (svref xy (+ 26 i 8)) (at (+ (* 5 i) 3))))))
    xy))

(defun %pad-mean (acc start n fact)
  (let ((pcum 0) (psum 0))
    (dotimes (i n)
      (let ((v (svref acc (+ start i))))
        (when (>= v 5) (incf pcum (* v i)) (incf psum v))))
    (if (plusp psum) (floor (* pcum fact) psum) 0)))

(defun hid-trackpad (h r)
  (let* ((kind (hid-info h))
         (datalen (case kind (:fountain 81) (t 64)))
         (cur (trackpad-sensors kind r)))
    (if (null (hid-base h))
        (setf (hid-base h) cur)                                  ; untouched: the baseline
        (let ((acc (make-array 42)))
          (dotimes (i 42)
            (let ((d (- (svref cur i) (svref (hid-base h) i))))
              (when (> d 127) (decf d 256))
              (when (< d -127) (incf d 256))
              (setf (svref acc i) (max 0 d))))
          (let* ((x (%pad-mean acc 0 26 64))
                 (y (%pad-mean acc 26 16 (if (eq kind :geyser2) 64 43)))
                 (button (if (and (>= (length r) datalen) (logtest (aref r (1- datalen)) 1)) 1 0)))
            (if (and (plusp x) (plusp y))
                (progn
                  (when (hid-x-old h)
                    (setq x (floor (+ (* 3 (hid-x-old h)) x) 4) y (floor (+ (* 3 (hid-y-old h)) y) 4))
                    (%pointer (floor (- x (hid-x-old h)) 2) (floor (- y (hid-y-old h)) 2) button))
                  (setf (hid-x-old h) x (hid-y-old h) y))
                (progn
                  (setf (hid-x-old h) nil (hid-y-old h) nil)
                  (%pointer 0 0 button))))))))

(defun hid-report (h r)
  (case (hid-kind h)
    (:keyboard (hid-keyboard h r))
    (:mouse (hid-mouse h r))
    (:trackpad (hid-trackpad h r))))
