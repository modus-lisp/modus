;;;; usb-io-core.lisp - the board's I/O core.  STAGE 1: interrupt discipline.
;;;;
;;;; THE DESIGN (docs/usb-io-core.md).  One core owns the USB host controller and
;;;; everything behind it.  It never conses, so it never collects and never holds a
;;;; heap pointer; it is permanently GC-SAFE, so a stop-the-world collection never
;;;; waits for it; nothing else is pinned to it, so it is never preempted.  Idle, it
;;;; sleeps in WFI and is woken by the controller's interrupt or a timer tick.
;;;;
;;;; WFI, NOT AN ISR.  PSTATE.I stays MASKED on the I/O core: an interrupt is never
;;;; TAKEN (every IRQ vector in this image is `B .'), it only ends the WFI.  The core
;;;; then reads the controller's status and services it in ordinary compiled code,
;;;; with no reentrancy and no saved context.  This is what the mini-Lisp board images
;;;; did (mini-lisp-final: net/arch-raspi3b.lisp DWC2-ENABLE-HOST-IRQ, "WFI wakes on
;;;; pending interrupt even with IRQs masked"); the CL port stubbed it out and every
;;;; board image since has been purely polled.
;;;;
;;;; ROUTING, three hops, all of which must be open:
;;;;   DWC2      GAHBCFG.GlblIntrMsk (bit 0) and GINTMSK select what raises the line;
;;;;   BCM2835   Enable IRQs 1 (0x3F00B210) bit 9 lets USB into the GPU interrupt;
;;;;   BCM2836   the GPU interrupt goes to ONE core: GPU_ROUTE (0x4000000C) bits 1:0.
;;;; And on the core: this image runs at EL2, where an IRQ with HCR_EL2.IMO clear
;;;; targets EL1 -- a LOWER level, so it is never taken here and may not wake WFI
;;;; either.  IO-HCR-IMO-ON sets IMO on the calling core (the same move
;;;; net/bare-perf-fiq.s makes with FMO for its FIQ); PSTATE.I keeps it from being taken.
;;;;
;;;; The BCM2836 local controller (PA 0x40000000) is not in the boot's page tables
;;;; (they map the first gigabyte); IO-MAP-LOCAL maps it exactly as bare-perf does.
;;;;
;;;; CONTROL BLOCK: the old ISR ring at USB-RING-BASE (0x11090000, Device RAM -- every
;;;; core sees a store after a DMB, no cache maintenance), u32 words:
;;;;   +0x00 'IOC1'        +0x04 probe core    +0x08 running      +0x0C stop request
;;;;   +0x10 wakes         +0x14 GPU (USB)     +0x18 timer        +0x1C neither
;;;;   +0x20 last GINTSTS  +0x24 last source   +0x28 GINTSTS bits ORed over the run
;;;;   +0x2C HCR_EL2 (low) +0x30 ticks/ms      +0x34 USB wakes with GINTSTS&GINTMSK = 0

(defun io-base () (modus.board::usb-ring-base))
(defun io-get (off) (mem-ref (+ (io-base) off) :u32))
(defun io-put (off v) (setf (mem-ref (+ (io-base) off) :u32) v))
(defun io-inc (off) (io-put off (logand (+ (io-get off) 1) #xFFFFFFFF)))

;;; ---- native stubs ---------------------------------------------------------------
;;; Words for system registers Lisp has no primitive for, in an exec page made once on
;;; the main thread.  Each stub returns its answer in x0 as a fixnum (LSL 1), which is
;;; what %JIT-CALL hands back.

(defvar *io-stub-page* 0)

(defun %io-stub-words ()
  ;; 0x00 IMO on:  mrs x0,hcr_el2; orr x0,x0,#0x10; msr hcr_el2,x0; isb; lsl x0,x0,#1; ret
  ;; 0x20 HCR:     mrs x0,hcr_el2; lsl x0,x0,#1; ret
  ;; 0x40 TLB:     dsb sy; tlbi alle2; dsb sy; isb; mov x0,#0; ret
  ;; 0x60 IMO off: mrs x0,hcr_el2; and x0,x0,#~0x10; msr hcr_el2,x0; isb; lsl x0,x0,#1; ret
  #(#xD53C1100 #xB27C0000 #xD51C1100 #xD5033FDF #xD37FF800 #xD65F03C0 #xD503201F #xD503201F
    #xD53C1100 #xD37FF800 #xD65F03C0 #xD503201F #xD503201F #xD503201F #xD503201F #xD503201F
    #xD5033F9F #xD50C871F #xD5033F9F #xD5033FDF #xD2800000 #xD65F03C0 #xD503201F #xD503201F
    #xD53C1100 #x927BF800 #xD51C1100 #xD5033FDF #xD37FF800 #xD65F03C0 #xD503201F #xD503201F))

(defun io-stubs ()
  "The stub page, made on first use.  Call it from the MAIN thread before handing the
   I/O loop to another core: a thread on cores 1..3 may not set a global."
  (when (eql *io-stub-page* 0)
    (let* ((w (%io-stub-words)) (n (length w)) (p (%mmap-exec-page (* 4 n))))
      (dotimes (i n) (setf (mem-ref (+ p (* 4 i)) :u32) (aref w i)))
      (%jit-icache-flush p (* 4 n))
      (setq *io-stub-page* p)))
  *io-stub-page*)

(defun %io-stub (off) (%jit-call (+ *io-stub-page* off)))
(defun io-hcr () (%io-stub #x20))
(defun io-hcr-imo-on () (%io-stub #x00))
(defun io-hcr-imo-off () (%io-stub #x60))

(defun io-map-local ()
  "Map PA 0x40000000.. (the BCM2836 local controller) as one Device block in L1[1] if
   nothing has; net/bare-perf.lisp's %PERF-MAP-LOCAL, which this mirrors."
  (io-stubs)
  (cond ((zerop (logior (mem-ref #x70008 :u32) (mem-ref #x7000C :u32)))
         (setf (mem-ref #x7000C :u32) 0)
         (setf (mem-ref #x70008 :u32) #x40000405)
         (%io-stub #x40)
         :mapped)
        (t :present)))

;;; ---- routing --------------------------------------------------------------------

(defun io-gpu-route () (logand (mem-ref #x4000000C :u32) 3))
(defun io-route-gpu-irq (core)
  "Send the GPU interrupt -- USB among it -- to CORE's IRQ line (FIQ routing kept)."
  (setf (mem-ref #x4000000C :u32)
        (logior (logand (mem-ref #x4000000C :u32) #xC) (logand core 3))))

(defun io-core-irq-source (core) (mem-ref (+ #x40000060 (* 4 core)) :u32))
(defun io-core-vtimer-irq (core on)
  "Route CORE's virtual timer (CNTVIRQ) to its IRQ line, or stop."
  (let* ((a (+ #x40000040 (* 4 core))) (v (mem-ref a :u32)))
    (setf (mem-ref a :u32) (if on (logior v 8) (logand v (lognot 8))))))

(defun io-usb-irq-enable (gintmsk)
  "Let the DWC2 raise its line for GINTMSK, and let USB into the GPU interrupt."
  (dwc2-write (dwc2-gintmsk) gintmsk)
  (dwc2-write (dwc2-gahbcfg) (logior (dwc2-read (dwc2-gahbcfg)) 1))
  (setf (mem-ref #x3F00B210 :u32) #x200))

(defun io-usb-irq-disable ()
  (setf (mem-ref #x3F00B21C :u32) #x200)          ; Disable IRQs 1, bit 9
  (dwc2-write (dwc2-gahbcfg) (logand (dwc2-read (dwc2-gahbcfg)) (lognot 1))))

(defun io-setup (core gintmsk)
  "On the MAIN thread: stubs, the local controller, the GPU route to CORE, the DWC2
   line for GINTMSK, and CORE's timer.  Then run IO-WAKE-PROBE on CORE."
  (io-stubs)
  (io-map-local)
  (io-route-gpu-irq core)
  (io-core-vtimer-irq core t)
  (io-usb-irq-enable gintmsk)
  (list :route (io-gpu-route) :gahbcfg (dwc2-read (dwc2-gahbcfg))
        :gintmsk (dwc2-read (dwc2-gintmsk)) :enable1 (mem-ref #x3F00B210 :u32)))

;;; ---- the probe ------------------------------------------------------------------

(defun io-wake-probe (core ms imo)
  "Sleep in WFI on THIS core (CORE: its number, 0..3) for MS milliseconds, re-arming the
   virtual timer before each WFI, and count what woke it.  IMO non-nil: set
   HCR_EL2.IMO first.  Allocates nothing.  Returns the number of wakes; the split is in
   the control block (IO-REPORT)."
  ;; CORE must be where this runs: the probe arms THIS core's timer but routes CORE's
  ;; timer and the GPU interrupt, so on any other core nothing would ever end the WFI.
  (unless (= core (%klog-core))
    (klog-event 3 "IOWK" core (%klog-core) -1)
    (return-from io-wake-probe -1))
  (dotimes (i 16) (io-put (* 4 i) 0))
  (io-put 0 #x494F4331)
  (io-put 4 core)
  (when imo (io-hcr-imo-on))
  (io-put #x2C (logand (ash (io-hcr) 0) #xFFFFFFFF))
  (let* ((per-ms (max 1 (truncate (cntfrq) 1000)))
         (end (+ (rdtsc) (* ms per-ms))))
    (io-put #x30 per-ms)
    (klog-event 1 "IOWK" core ms (io-get #x2C))
    (io-put 8 1)
    (setup-irq)
    (loop
      (when (or (>= (rdtsc) end) (/= (io-get #x0C) 0)) (return nil))
      (timer-rearm)
      (wfi)
      (io-inc #x10)
      (let ((src (io-core-irq-source core))
            (gs (dwc2-read (dwc2-gintsts))))
        (io-put #x24 src)
        (io-put #x20 gs)
        (io-put #x28 (logior (io-get #x28) gs))
        (cond ((logtest src #x100)
               (io-inc #x14)
               (when (zerop (logand gs (dwc2-read (dwc2-gintmsk)))) (io-inc #x34))
               ;; This probe services nothing: mask the line until the next run, or a
               ;; level-triggered source would end every WFI at once.
               (dwc2-write (dwc2-gintmsk) 0))
              ((logtest src 8) (io-inc #x18))
              (t (io-inc #x1C)))))
    (io-put 8 0)
    (klog-event 1 "IOWK" (io-get #x10) (io-get #x14) (io-get #x18))
    (io-get #x10)))

(defun io-report ()
  (list :wakes (io-get #x10) :usb (io-get #x14) :timer (io-get #x18) :other (io-get #x1C)
        :usb-unmasked-zero (io-get #x34)
        :gintsts-or (io-get #x28) :last-src (io-get #x24) :hcr (io-get #x2C)
        :ticks-per-ms (io-get #x30) :core (io-get 4)))

(defun io-status ()
  "The I/O core's interrupt routing and counters, for (modus:image-configuration)."
  (append
   (list :gahbcfg (dwc2-read (dwc2-gahbcfg)) :gintmsk (dwc2-read (dwc2-gintmsk))
         :usb-irq-enabled (logtest (mem-ref #x3F00B210 :u32) #x200)
         ;; the local controller is readable only once something has mapped it
         :gpu-route (if (zerop (mem-ref #x70008 :u32)) :unmapped (io-gpu-route)))
   (if (= (io-get 0) #x494F4331) (io-report) (list :probe :never-run))))

;;; ==================================================================================
;;; STAGE 2: THE ENGINE.  One loop, alone on its core, owns the DWC2.
;;; ==================================================================================
;;;
;;; What it does each pass (USBE-MAIN):
;;;   RX       channel 1 (the RTL8153's bulk-IN): when a transfer completes, every frame
;;;            of the aggregate is copied into the RX RING and the channel is re-armed at
;;;            once -- the device is never left NAKing while the stack walks a buffer.
;;;   TX       the four TX RINGS (one per producing core, so each has one writer and no
;;;            push needs an atomic) are sent over channel 2.
;;;   HID      the keyboard and mouse, every 8 ms (split transactions, channel 4's job
;;;            in the end; today the split engine's own channel).
;;;   HOTPLUG  the hub's ports every 500 ms, with the receive HALTED around the hub
;;;            traffic (the root-port trap, docs/usb-io-core.md) -- a policy of the one
;;;            scheduler now, not a hook between drivers.
;;;   PORT     HPRT0's change bits are cleared and logged (recovery is stage 4).
;;; and then, if nothing was done, it sleeps in WFI until the controller's interrupt
;;; (channel 1 only: its NAKs are masked), its 3 ms timer tick, or its MAILBOX: a
;;; producer writes core-1 mailbox 0 after a push, so a TX never waits for the tick.
;;;
;;; WHAT IT MAY TOUCH.  It runs inside a GC-SAFE region it never leaves: a region-0
;;; stop-the-world collection scans its published stack and never waits for it.  So it
;;; must hold no heap pointer: no allocation, no global, no string or other heap literal.
;;; Constants, raw memory and the controller only.  The allocation pointer is checked
;;; every pass (+0x48 counts passes on which it moved; it must stay 0).
;;;
;;; BLOCK at USBE-BASE (0x11500000, Normal-WB RAM -- not cleared at boot, so "running"
;;; means running in THIS boot: +0x64 holds the klog boot number it started under):
;;;   +0x00 'USBE'  +0x04 state (1 running, 2 stopping, 3 stopped)  +0x08 stop request
;;;   +0x0C core    +0x10 passes  +0x14 sleeps  +0x18 RX transfers  +0x1C RX frames
;;;   +0x20 RX ring full (frames dropped)  +0x24 RX errors  +0x28 TX frames
;;;   +0x2C TX errors  +0x30 TX ring full (producer gave up)  +0x34 HID polls
;;;   +0x38 hotplug scans  +0x3C port events  +0x40 last HPRT0  +0x44 alloc start
;;;   +0x48 allocating passes  +0x58 next HID poll  +0x5C next scan (HFNUM ticks)
;;;   +0x60 consumer holds a frame  +0x64 boot number
;;;   +0x1000 RX ring: head (engine) +0, tail (consumer) +4; slots at +0x2000,
;;;           128 x 2 KB: +0 length, +8 frame
;;;   +0x50000 + core*0x11000: TX ring for that core: head (producer) +0, tail (engine)
;;;           +4; 32 slots of 2 KB from +0x40

(defun usbe-base () #x11500000)
(defun usbe-get (off) (mem-ref (+ (usbe-base) off) :u32))
(defun usbe-put (off v) (setf (mem-ref (+ (usbe-base) off) :u32) (logand v #xFFFFFFFF)))
(defun usbe-inc (off) (usbe-put off (+ (usbe-get off) 1)))
(defun usbe-rx-slot (i) (+ (usbe-base) #x2000 (* (logand i 127) #x800)))
(defun usbe-tx-ring (c) (+ (usbe-base) #x50000 (* (logand c 3) #x11000)))

;; klog tags as numbers (a string is a heap object): 'USBE' 'RXER' 'TXER' 'PORT' 'CONS'
(defun %t-usbe () #x45425355)
(defun %t-rxer () #x52455852)
(defun %t-txer () #x52455854)
(defun %t-port () #x54524F50)
(defun %t-cons () #x534E4F43)

(defun %usbe-boot () (let ((b (%klog-base))) (if (zerop b) 0 (mem-ref (+ b 8) :u32))))
(defun usbe-running-p ()
  (and (= (usbe-get 0) #x45425355) (= (usbe-get 4) 1) (= (usbe-get #x64) (%usbe-boot))))

;;; ---- the BCM2836 core mailbox: how a producer wakes the engine ------------------
(defun %usbe-mbox-set (core) (+ #x40000080 (* 16 core)))
(defun %usbe-mbox-clear (core) (+ #x400000C0 (* 16 core)))
(defun usbe-poke () (setf (mem-ref (%usbe-mbox-set (usbe-get #xC)) :u32) 1))

;;; ---- RX (engine side) ------------------------------------------------------------
(defun usbe-rx-arm ()
  (modus.net.r8152:r8152-rearm)
  ;; completion, halt and errors only: a NAK every microframe must not wake the core
  (dwc2-write (dwc2-hcintmsk 1) #x78F))

(defun usbe-rx-push (src len)
  (let ((h (usbe-get #x1000)) (tl (usbe-get #x1004)))
    (if (>= (logand (- h tl) #xFFFFFFFF) 128)
        (usbe-inc #x20)
        (let* ((slot (usbe-rx-slot h))
               (m (modus.net.r8152:r8152-ncopy (+ slot 8) src len)))
          (loop
            (when (>= m len) (return nil))
            (setf (mem-ref (+ slot 8 m) :u8) (mem-ref (+ src m) :u8))
            (setq m (+ m 1)))
          (setf (mem-ref slot :u32) len)
          (memory-barrier)
          (usbe-put #x1000 (+ h 1))
          (usbe-inc #x1C)))))

(defun usbe-rx-walk (n)
  "Copy every frame of the N delivered aggregate bytes into the RX ring."
  (let ((off 0) (agg (modus.net.r8152:r8152-agg-buf)))
    (loop
      (when (> (+ off 24) n) (return nil))
      (let ((plen (- (logand (mem-ref (+ agg off) :u32) #x7FFF) 4)))
        (when (or (<= plen 0) (>= plen 1600) (> (+ off 24 plen) n)) (return nil))
        (usbe-rx-push (+ agg off 24) plen)
        (setq off (logand (+ off 24 plen 4 7) (lognot 7)))))))

(defun usbe-rx-service ()
  "1 if channel 1 had finished (its frames are in the ring, it is re-armed), else 0."
  (let ((r (dwc2-poll-bulk-in 1)))
    (cond ((zerop r) 0)
          ((= r 1)
           (usbe-inc #x18)
           (let ((n (- (modus.net.r8152:r8152-agg-size)
                       (logand (dwc2-read (dwc2-hctsiz 1)) #x7FFFF))))
             (modus.net.r8152:r8152-dcache-civac (modus.net.r8152:r8152-agg-buf) n)
             (usbe-rx-walk n))
           (usbe-rx-arm)
           1)
          (t (usbe-inc #x24)
             (klog-event 2 (%t-rxer) r (usbe-get #x24) 0)
             (usbe-rx-arm)
             1))))

;;; ---- TX (engine side) ------------------------------------------------------------
(defun usbe-tx-one (src len)
  ;; the RTL8153's tx_desc {len | first | last, 0}, then the frame, in the DMA buffer
  (let ((tx (e1000-tx-buf-base)) (i 0))
    (setf (mem-ref tx :u32) (logior len #xC0000000))
    (setf (mem-ref (+ tx 4) :u32) 0)
    (loop
      (when (>= i len) (return nil))
      (setf (mem-ref (+ tx 8 i) :u32) (mem-ref (+ src i) :u32))
      (setq i (+ i 4)))
    (let ((r (dwc2-bulk-transfer 2 (usb-dev-addr) (usb-bulk-out-ep) 0 tx (+ len 8)
                                 (usb-bulk-out-mps))))
      (if (eql r 1)
          (usbe-inc #x28)
          (progn (usbe-inc #x2C) (klog-event 2 (%t-txer) r len 0))))))

(defun usbe-tx-service ()
  "Send everything queued; how many frames."
  (let ((sent 0) (c 0))
    (loop
      (when (> c 3) (return nil))
      (let ((ring (usbe-tx-ring c)))
        (loop
          (let ((h (mem-ref ring :u32)) (tl (mem-ref (+ ring 4) :u32)))
            (when (= h tl) (return nil))
            (memory-barrier)
            (let ((slot (+ ring #x40 (* (logand tl 31) #x800))))
              (usbe-tx-one (+ slot 8) (min (mem-ref slot :u32) 2032)))
            (setf (mem-ref (+ ring 4) :u32) (logand (+ tl 1) #xFFFFFFFF))
            (setq sent (+ sent 1)))))
      (setq c (+ c 1)))
    sent))

;;; ---- HID, hotplug, the port ------------------------------------------------------
;;; A split transaction waits for later microframes (dwc2-split.lisp's
;;; DWC2-WAIT-NEXT-UFRAME), and an HID poll is mostly such waits: measured on the Zero,
;;; ~19 ms of every 20.  While the engine polls the keyboard and mouse, +0x68 is 1 and
;;; each of those waits first serves the receive and the send once -- so a frame waits a
;;; microframe, not a whole poll.  Not during hub scans: there the receive is halted on
;;; purpose and control traffic is in flight.  +0x6C counts the background rounds.
(defun usbe-background ()
  ;; the block survives a reset: "running" must mean THIS boot, on THIS core
  (when (and (= (usbe-get #x68) 1) (usbe-running-p) (= (%klog-core) (usbe-get #xC)))
    (usbe-put #x68 0)                          ; no recursion
    (usbe-inc #x6C)
    (usbe-rx-service)
    (usbe-tx-service)
    (usbe-put #x68 1)))

(defun usbe-hid-service ()
  (when (and (hid-ready-p) (hid-time-reached-p (usbe-get #x58)))
    (usbe-put #x58 (logand (+ (hid-now) 64) #x3FFF))          ; 8 ms
    (usbe-inc #x34)
    (usbe-put #x68 1)
    (when (> (hid-poll-dev (hid-kbd) (hid-kbd-buf)) 0)
      (hid-kbd-report (hid-kbd-buf)))
    (hid-repeat-tick)
    (let ((n (hid-poll-dev (hid-mouse) (hid-mouse-buf))))
      (when (> n 2) (hid-mouse-report (hid-mouse-buf) n)))
    (usbe-put #x68 0)))

(defun usbe-hotplug-service ()
  (let ((hub (hid-get #x00)))
    (when (and (> hub 0) (hid-time-reached-p (usbe-get #x5C)))
      (usbe-put #x5C (logand (+ (hid-now) (hid-hotplug-period)) #x3FFF))
      (usbe-inc #x38)
      ;; hub traffic with the bulk-IN armed disabled the root port: take what has
      ;; arrived, halt the receive, scan, re-arm
      (usbe-rx-service)
      (dwc2-halt-channel 1)
      (%hid-hotplug-scan hub)
      (usbe-rx-arm))))

(defun usbe-port-service ()
  (let ((p (dwc2-read (dwc2-hprt0))))
    ;; connect-detected (1), enable-changed (3), overcurrent-changed (5) are
    ;; write-1-to-clear and hold GINTSTS.PrtInt up: clear them, without writing
    ;; PrtEna (writing 1 there DISABLES the port)
    (when (logtest p #x2A)
      (dwc2-write (dwc2-hprt0) (logior (logand p (dwc2-hprt0-mask)) (logand p #x2A))))
    (when (/= (logand p #x05) (logand (usbe-get #x40) #x05))   ; connected, enabled
      (usbe-inc #x3C)
      (klog-event 3 (%t-port) p (usbe-get #x40) (usbe-get #x3C)))
    (usbe-put #x40 p)))

;;; ---- per-service accounting: +0x80 + 16*K for K = rx tx port hid hotplug -----------
;;; +0 counter ticks / 16 spent in it, +4 calls, +8 calls that moved the allocation
;;; pointer, +12 the largest single call (ticks / 16)
(defun usbe-measure (k t0 a0)
  "Account the service K that ran since T0 (counter) with allocation pointer A0.  Now."
  (let* ((now (rdtsc)) (d (ash (- now t0) -4)) (b (+ #x80 (* 16 k))))
    (usbe-put b (+ (usbe-get b) d))
    (usbe-inc (+ b 4))
    (unless (= a0 (get-alloc-ptr)) (usbe-inc (+ b 8)))
    (when (> d (usbe-get (+ b 12))) (usbe-put (+ b 12) d))
    now))

(defun usbe-profile ()
  "Per service: (name ms-total calls allocating-calls max-ms)."
  (let ((acc nil) (k 0) (tick-ms (/ (max 1 (cntfrq)) 16000.0)))
    (dolist (name '(:rx :tx :port :hid :hotplug))
      (let ((b (+ #x80 (* 16 k))))
        (push (list name (/ (usbe-get b) tick-ms) (usbe-get (+ b 4)) (usbe-get (+ b 8))
                    (/ (usbe-get (+ b 12)) tick-ms))
              acc))
      (setq k (+ k 1)))
    (nreverse acc)))

;;; ---- the loop --------------------------------------------------------------------
(defun usbe-main ()
  "The engine.  Run as a thread alone on its core (USBE-START); returns when stopped."
  (let ((core (%klog-core)))
    (usbe-put #xC core)
    (io-hcr-imo-on)                           ; at EL2, without it an IRQ never ends WFI
    (io-route-gpu-irq core)
    (io-core-vtimer-irq core t)
    (setf (mem-ref (+ #x40000050 (* 4 core)) :u32) 1)          ; mailbox 0 -> IRQ
    (setf (mem-ref (%usbe-mbox-clear core) :u32) #xFFFFFFFF)
    (hid-put #x1F8 (+ (%thr-cpu) 1))          ; the HID driver's owner: this thread
    (dwc2-write (dwc2-haintmsk) 2)            ; channel 1 only
    (when (zerop (mem-ref (+ (e1000-state-base) #x48) :u32))
      (dwc2-write (dwc2-hcintmsk 1) #x78F))   ; armed already: just quiet its NAKs
    (unless (zerop (mem-ref (+ (e1000-state-base) #x48) :u32))
      (usbe-rx-arm))
    (io-usb-irq-enable #x03000000)            ; GINTMSK: HChInt | PrtInt
    (usbe-put #x40 (dwc2-read (dwc2-hprt0)))
    (%gc-safe-enter)
    (setup-irq)
    (usbe-put 4 1)
    (klog-event 1 (%t-usbe) core 1 0)
    (usbe-put #x44 (logand (get-alloc-ptr) #xFFFFFFFF))
    (loop
      (when (/= (usbe-get 8) 0) (return nil))
      (usbe-inc #x10)
      (setf (mem-ref (%usbe-mbox-clear core) :u32) #xFFFFFFFF)
      (let ((work 0) (t0 (rdtsc)) (a0 (get-alloc-ptr)))
        (setq work (usbe-rx-service))
        (setq t0 (usbe-measure 0 t0 a0)) (setq a0 (get-alloc-ptr))
        (setq work (+ work (usbe-tx-service)))
        (setq t0 (usbe-measure 1 t0 a0)) (setq a0 (get-alloc-ptr))
        (usbe-port-service)
        (setq t0 (usbe-measure 2 t0 a0)) (setq a0 (get-alloc-ptr))
        (usbe-hid-service)
        (setq t0 (usbe-measure 3 t0 a0)) (setq a0 (get-alloc-ptr))
        (usbe-hotplug-service)
        (usbe-measure 4 t0 a0)
        (unless (= (logand (get-alloc-ptr) #xFFFFFFFF) (usbe-get #x44))
          (when (zerop (usbe-get #x48)) (klog-event 3 (%t-cons) (usbe-get #x10) 0 0))
          (usbe-inc #x48)
          (usbe-put #x44 (logand (get-alloc-ptr) #xFFFFFFFF)))
        (when (zerop work)
          (usbe-inc #x14)
          (timer-rearm)
          (wfi))))
    (usbe-put 4 2)
    (dwc2-write (dwc2-gintmsk) 0)
    (dwc2-write (dwc2-haintmsk) 0)
    (setf (mem-ref (+ #x40000050 (* 4 core)) :u32) 0)
    (io-core-vtimer-irq core nil)
    (hid-put #x1F8 0)
    (%gc-safe-leave)
    (klog-event 1 (%t-usbe) core 3 (usbe-get #x48))
    (usbe-put 4 3)
    0))

;;; ---- the other side: producers and the consumer (any core but the engine's) -----
(defun usbe-send (buf len)
  "Queue the frame in BUF (LEN bytes) on this core's TX ring and wake the engine.  1,
   or 0 when the ring stayed full."
  (let* ((ring (usbe-tx-ring (%klog-core))) (h (mem-ref ring :u32)) (k 0) (n (min len 2032)))
    (loop
      (when (< (logand (- h (mem-ref (+ ring 4) :u32)) #xFFFFFFFF) 32) (return nil))
      (when (> k 4000000) (usbe-inc #x30) (return-from usbe-send 0))
      (setq k (+ k 1)))
    (let ((slot (+ ring #x40 (* (logand h 31) #x800))) (i 0))
      (loop
        (when (>= i n) (return nil))
        (setf (mem-ref (+ slot 8 i) :u8) (aref buf i))
        (setq i (+ i 1)))
      (setf (mem-ref slot :u32) n)
      (memory-barrier)
      (setf (mem-ref ring :u32) (logand (+ h 1) #xFFFFFFFF))
      (usbe-poke)
      1)))

(defun usbe-receive ()
  "The next received frame's length, or 0.  The previous one is released; the frame
   stays at USBE-RX-BUF until the next call (the NIC interface's contract)."
  (let ((tl (usbe-get #x1004)))
    (when (= (usbe-get #x60) 1)
      (setq tl (logand (+ tl 1) #xFFFFFFFF))
      (usbe-put #x1004 tl)
      (usbe-put #x60 0))
    (if (= (usbe-get #x1000) tl)
        0
        (progn (memory-barrier)
               (usbe-put #x60 1)
               (mem-ref (usbe-rx-slot tl) :u32)))))

(defun usbe-rx-buf () (+ (usbe-rx-slot (usbe-get #x1004)) 8))

(defun usbe-rx-wait ()
  "Wait a little for a frame (the net stack's idle poll): T when one is there."
  (let ((i 0))
    (loop
      (when (/= (usbe-get #x1000) (usbe-get #x1004)) (return t))
      (when (>= i 5000) (return nil))
      (setq i (+ i 1)))))

(defun usbe-status ()
  (if (not (= (usbe-get 0) #x45425355))
      (list :engine :never-started)
      (list :running (usbe-running-p) :state (usbe-get 4) :core (usbe-get #xC)
            :passes (usbe-get #x10) :sleeps (usbe-get #x14)
            :rx-transfers (usbe-get #x18) :rx-frames (usbe-get #x1C)
            :rx-dropped (usbe-get #x20) :rx-errors (usbe-get #x24)
            :tx-frames (usbe-get #x28) :tx-errors (usbe-get #x2C) :tx-full (usbe-get #x30)
            :hid-polls (usbe-get #x34) :scans (usbe-get #x38) :port-events (usbe-get #x3C)
            :hprt0 (usbe-get #x40) :allocating-passes (usbe-get #x48)
            :background-rounds (usbe-get #x6C))))

(defun usbe-init-block ()
  (dotimes (i 64) (usbe-put (* 4 i) 0))
  (usbe-put #x1000 0) (usbe-put #x1004 0)
  (dotimes (c 4) (setf (mem-ref (usbe-tx-ring c) :u32) 0) (setf (mem-ref (+ (usbe-tx-ring c) 4) :u32) 0))
  (usbe-put #x64 (%usbe-boot))
  (usbe-put 0 #x45425355))
