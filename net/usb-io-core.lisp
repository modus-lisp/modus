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
  (unless (= core (%kmsg-core))
    (kmsg-event 3 "IOWK" core (%kmsg-core) -1)
    (return-from io-wake-probe -1))
  (dotimes (i 16) (io-put (* 4 i) 0))
  (io-put 0 #x494F4331)
  (io-put 4 core)
  (when imo (io-hcr-imo-on))
  (io-put #x2C (logand (ash (io-hcr) 0) #xFFFFFFFF))
  (let* ((per-ms (max 1 (truncate (cntfrq) 1000)))
         (end (+ (rdtsc) (* ms per-ms))))
    (io-put #x30 per-ms)
    (kmsg-event 1 "IOWK" core ms (io-get #x2C))
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
    (kmsg-event 1 "IOWK" (io-get #x10) (io-get #x14) (io-get #x18))
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
;;; means running in THIS boot: +0x64 holds the kmsg boot number it started under):
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

;;; THE CONTROL WORDS (offsets below #x1000) live in the DEVICE-RAM window, the rings in
;;; cacheable RAM.  Device RAM is written through and survives a board reset, so after a
;;; failure the next boot reads the engine's last counters; cacheable RAM loses whatever
;;; was still in the cache when the board reset (measured: the counters read 0).
(defun usbe-base () #x11500000)
(defun usbe-ctl-base () #x11090400)
(defun %usbe-addr (off) (if (< off #x1000) (+ (usbe-ctl-base) off) (+ (usbe-base) off)))
(defun usbe-get (off) (mem-ref (%usbe-addr off) :u32))
(defun usbe-put (off v) (setf (mem-ref (%usbe-addr off) :u32) (logand v #xFFFFFFFF)))
(defun usbe-inc (off) (usbe-put off (+ (usbe-get off) 1)))
(defun usbe-rx-slot (i) (+ (usbe-base) #x2000 (* (logand i 127) #x800)))
(defun usbe-tx-ring (c) (+ (usbe-base) #x50000 (* (logand c 3) #x11000)))

;;; A failure that repeats must not wash its own beginning out of the log (measured: a
;;; dead bus logged a slow hub scan every 0.5 s and the ring lost the moment it died).
;;; Kind K (0..7) counts its occurrences at +0xE0+4K; the first 8 are logged, then every
;;; 256th, with the count as the third word.  (+0x80..+0xDF is the per-service profile.)
(defun usbe-log-limited (k level tag a b)
  (let* ((off (+ #xE0 (* 4 (logand k 7)))) (n (+ (usbe-get off) 1)))
    (usbe-put off n)
    (when (or (<= n 8) (zerop (logand n 255)))
      (kmsg-event level tag a b n))))

;; kmsg tags as numbers (a string is a heap object): 'USBE' 'RXER' 'TXER' 'PORT' 'CONS'
(defun %t-usbe () #x45425355)
(defun %t-rxer () #x52455852)
(defun %t-txer () #x52455854)
(defun %t-port () #x54524F50)
(defun %t-cons () #x534E4F43)

(defun %usbe-boot () (let ((b (%kmsg-base))) (if (zerop b) 0 (mem-ref (+ b 8) :u32))))
(defun usbe-running-p ()
  (and (= (usbe-get 0) #x45425355) (= (usbe-get 4) 1) (= (usbe-get #x64) (%usbe-boot))))

;;; ---- the BCM2836 core mailbox: how a producer wakes the engine ------------------
(defun %usbe-mbox-set (core) (+ #x40000080 (* 16 core)))
(defun %usbe-mbox-clear (core) (+ #x400000C0 (* 16 core)))
(defun usbe-poke () (setf (mem-ref (%usbe-mbox-set (usbe-get #xC)) :u32) 1))

;;; ---- RX (engine side) ------------------------------------------------------------
;;; Two adapters' framings on channel 1, by the NIC the image bound (usb-netdev.lisp):
;;;   2  RTL8153 vendor mode: a 16 KB aggregate of frames, each behind a 24-byte rx_desc,
;;;      in cacheable RAM (cleaned+invalidated after DMA)
;;;   1  CDC-ECM (what a cold bring-up selects, below): one plain frame per transfer, in
;;;      the 2 KB Device-RAM buffer (no cache maintenance)
;;;   0  no NIC: the engine serves the keyboard, mouse and hub only
(defun usbe-nic () (usb-netdev-get))

(defun usbe-rx-arm ()
  (let ((nic (usbe-nic)))
    (cond ((= nic 2) (modus.net.r8152:r8152-rearm))
          ((= nic 1) (dwc2-start-bulk-in 1 (usb-dev-addr) (usb-bulk-in-ep)
                                         (modus.net.cdc:cdc-rx-buf-addr) 2048 (usb-bulk-in-mps))))
    ;; completion, halt and errors only: a NAK every microframe must not wake the core
    (unless (zerop nic) (dwc2-write (dwc2-hcintmsk 1) #x78F))))

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
  (let ((nic (usbe-nic)))
    (if (or (zerop nic) (= (usbe-get #x12C) 1))         ; none, or paused for a HID round
        0
        (let ((r (dwc2-poll-bulk-in 1)))
          (cond ((zerop r) 0)
                ((= r 1)
                 (usbe-inc #x18)
                 (if (= nic 2)
                     (let ((n (- (modus.net.r8152:r8152-agg-size)
                                 (logand (dwc2-read (dwc2-hctsiz 1)) #x7FFFF))))
                       (modus.net.r8152:r8152-dcache-civac (modus.net.r8152:r8152-agg-buf) n)
                       (usbe-rx-walk n))
                     (let ((n (- 2048 (logand (dwc2-read (dwc2-hctsiz 1)) #x7FFFF))))
                       (when (and (> n 13) (< n 1600))
                         (usbe-rx-push (modus.net.cdc:cdc-rx-buf-addr) n))))
                 (usbe-rx-arm)
                 1)
                (t (usbe-inc #x24)
                   (usbe-log-limited 1 2 (%t-rxer) r (dwc2-read (dwc2-hprt0)))
                   (usbe-rx-arm)
                   1))))))

;;; ---- TX (engine side) ------------------------------------------------------------
(defun usbe-tx-one (src len)
  ;; RTL8153: its tx_desc {len | first | last, 0}, then the frame; CDC-ECM: the frame
  (let* ((tx (e1000-tx-buf-base)) (i 0) (nic (usbe-nic)) (hd (if (= nic 2) 8 0)))
    (when (= nic 2)
      (setf (mem-ref tx :u32) (logior len #xC0000000))
      (setf (mem-ref (+ tx 4) :u32) 0))
    (loop
      (when (>= i len) (return nil))
      (setf (mem-ref (+ tx hd i) :u32) (mem-ref (+ src i) :u32))
      (setq i (+ i 4)))
    (let ((r (if (zerop nic)
                 0
                 (dwc2-bulk-transfer 2 (usb-dev-addr) (usb-bulk-out-ep) 0 tx (+ len hd)
                                     (usb-bulk-out-mps)))))
      (if (eql r 1)
          (usbe-inc #x28)
          (progn (usbe-inc #x2C) (usbe-log-limited 2 2 (%t-txer) r (dwc2-read (dwc2-hprt0))))))))

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
              (if (zerop (usbe-nic))
                  (usbe-inc #x78)                   ; no NIC (it is being brought back)
                  (usbe-tx-one (+ slot 8) (min (mem-ref slot :u32) 2032))))
            (setf (mem-ref (+ ring 4) :u32) (logand (+ tl 1) #xFFFFFFFF))
            (setq sent (+ sent 1)))))
      (setq c (+ c 1)))
    sent))

;;; ---- HID, hotplug, the port ------------------------------------------------------
;;; ---- the keyboard and mouse WITHOUT WAITING: split transactions as a state machine ----
;;;
;;; An interrupt-IN poll of a full/low-speed device behind the hub is a SPLIT: a
;;; start-split (SSPLIT) the hub's transaction translator accepts, then complete-splits
;;; (CSPLIT) in LATER microframes until the TT has the device's answer.  DWC2-SPLIT-PACKET
;;; does that by spinning through the microframes; measured, that was ~19 ms of every HID
;;; poll.  Here the engine starts a step, goes back to its loop, and comes back when the
;;; hardware says so:
;;;   the split channel's HALT interrupt   -- a step's result is in (HAINTMSK bit 6,
;;;                                          HCINTMSK = halted only)
;;;   the START-OF-FRAME interrupt          -- a new microframe began (GINTMSK bit 3, on
;;;                                          only while a CSPLIT waits for one)
;;; State, in the engine's control block (Device RAM):
;;;   +0x100 state  0 idle, 1 SSPLIT in flight, 2 CSPLIT waits for a later microframe,
;;;                 3 CSPLIT in flight
;;;   +0x104 device (0 keyboard, 1 mouse)   +0x108 microframe the wait started in
;;;   +0x10C CSPLIT tries   +0x110 errors   +0x114 issue time (counter >> 10)
;;;   +0x118 reports  +0x11C NAKs (nothing new)  +0x120 failed transactions
;;;   +0x124 SOF wakes
;;; The keyboard and the mouse take turns on channel 6; a round of both every 8 ms.

(defun %usbe-sof-on (on)
  (dwc2-write (dwc2-gintmsk) (if on #x03000008 #x03000000))
  (unless on (dwc2-write (dwc2-gintsts) 8)))          ; Sof is write-1-to-clear

(defun usbe-hid-dev (k) (if (= k 0) (hid-kbd) (hid-mouse)))
(defun usbe-hid-buf (k) (if (= k 0) (hid-kbd-buf) (hid-mouse-buf)))

;;; THE RECEIVE PAUSES FOR A KEYBOARD/MOUSE ROUND.  Measured on the Zero: with the NIC's
;;; bulk-IN armed on channel 1, every PERIODIC split to the keyboard and mouse is answered
;;; NYET until it gives up -- halt channel 1 and the same polls are NAKed (nothing new) at
;;; once; re-arm it and they fail again, every time.  (Control splits are unaffected, which is
;;; why enumeration worked and the polls never did.)  So a round -- the keyboard's
;;; transaction, then the mouse's, a few microframes each every 8 ms -- runs with the receive
;;; halted: what has arrived is taken first, and the channel is re-armed when the round
;;; ends.  The adapter buffers what arrives meanwhile.  +0x12C: 1 while paused.
(defun usbe-rx-pause ()
  (when (and (zerop (usbe-get #x12C)) (not (zerop (usbe-nic))))
    (usbe-rx-service)
    (dwc2-halt-channel 1)
    (usbe-put #x12C 1)))

(defun usbe-rx-resume ()
  (unless (zerop (usbe-get #x12C))
    (usbe-put #x12C 0)
    (usbe-rx-arm)))

(defun usbe-split-program (k csplit)
  "Program and enable one SSPLIT (CSPLIT non-nil: one CSPLIT) for device K.  No wait."
  (let* ((dev (usbe-hid-dev k)) (ch (split-ch))
         (hcchar (split-hcchar (hid-dev-get dev #x14) (hid-dev-get dev #x10) 1 3
                               (hid-dev-get dev #x04) (= (hid-dev-get dev #x0C) 1)))
         (hs (split-hcsplt (hid-dev-hub dev) (hid-dev-get dev #x08))))
    (dwc2-write (dwc2-hcint ch) #xFFFFFFFF)
    (dwc2-write (dwc2-hcintmsk ch) 2)                     ; wake on HALT only
    (dwc2-write (dwc2-hcchar ch) hcchar)
    (dwc2-write (dwc2-hcsplt ch) (if csplit (logior hs (hcsplt-compsplt)) hs))
    (dwc2-write (dwc2-hctsiz ch) (dwc2-build-hctsiz (hid-dev-get dev #x14) 1 (hid-dev-get dev #x18)))
    (dwc2-write (dwc2-hcdma ch) (dwc2-bus-addr (usbe-hid-buf k)))
    (memory-barrier)
    ;; U-Boot: an interrupt channel goes out in the next ODD (micro)frame when the current
    ;; one is even
    (let ((odd (if (zerop (logand (dwc2-read (dwc2-hfnum)) 1)) (hcchar-oddfrm) 0)))
      (dwc2-write (dwc2-hcchar ch)
                  (logior (logand hcchar (logxor (hcchar-chdis) #xFFFFFFFF))
                          (logior (hcchar-chena) odd))))
    (usbe-put #x114 (ash (rdtsc) -10))))

(defun usbe-hid-done (k result)
  "Device K's transaction is over (RESULT: 1 report, 0 nothing new, < 0 failed): the next
   device, or the round is over and the next starts in 8 ms."
  (cond ((= result 1) (usbe-inc #x118))
        ((= result 0) (usbe-inc #x11C))
        (t (usbe-inc #x120)
           ;; 'HIDF': why -- the result, the state and device, the channel's last HCINT
           (usbe-log-limited 4 2 #x46444948
                             (logior (ash (- result) 8) (ash (usbe-get #x100) 4) k)
                             (usbe-get #x128))))
  (usbe-put #x100 0)
  (%usbe-sof-on nil)
  (if (= k 0)
      (usbe-put #x104 1)
      (progn (usbe-put #x104 0)
             (usbe-rx-resume)                              ; the round is over
             (usbe-put #x58 (logand (+ (hid-now) 64) #x3FFF)))))

(defun usbe-hid-report (k)
  "Device K's CSPLIT brought data: flip its toggle, hand the report to the HID driver."
  (let* ((dev (usbe-hid-dev k)) (mps (hid-dev-get dev #x14))
         (n (- mps (logand (dwc2-read (dwc2-hctsiz (split-ch))) #x7FFFF)))
         (pid (hid-dev-get dev #x18)))
    (hid-dev-put dev #x18 (if (= pid (hctsiz-pid-data1)) (hctsiz-pid-data0) (hctsiz-pid-data1)))
    (if (= k 0)
        (when (> n 0) (hid-kbd-report (hid-kbd-buf)))
        (when (> n 2) (hid-mouse-report (hid-mouse-buf) n)))))

(defun usbe-hid-step ()
  "Advance the split state machine by whatever the hardware allows now.  Never waits."
  (when (hid-ready-p)
    (hid-repeat-tick)
    (let ((st (usbe-get #x100)) (k (usbe-get #x104)) (ch (split-ch)))
      (cond
        ;; idle: the next device of this round, or the next round when it is due
        ((= st 0)
         (when (or (= k 1) (hid-time-reached-p (usbe-get #x58)))
           (if (= (hid-dev-get (usbe-hid-dev k) 0) 1)
               (progn (usbe-put #x10C 0) (usbe-put #x110 0)
                      (usbe-rx-pause)
                      (usbe-split-program k nil) (usbe-put #x100 1))
               (usbe-hid-done k 0))))
        ;; waiting for a later microframe: the SOF interrupt woke us, or soon will
        ((= st 2)
         (dwc2-write (dwc2-gintsts) 8)
         (when (/= (logand (dwc2-read (dwc2-hfnum)) #x3FFF) (usbe-get #x108))
           (usbe-inc #x124)
           (%usbe-sof-on nil)
           (usbe-split-program k t)
           (usbe-put #x100 3)))
        ;; a split in flight: has the channel halted?
        (t
         (let ((hcint (dwc2-read (dwc2-hcint ch))))
           (usbe-put #x128 hcint)
           (if (zerop (logand hcint (hcint-chhltd)))
               ;; 5 ms without halting: give the channel back and the device up for now
               (when (> (logand (- (ash (rdtsc) -10) (usbe-get #x114)) #xFFFFFFFF)
                        (ash (truncate (max 1 (cntfrq)) 200) -10))     ; 5 ms
                 (dwc2-halt-channel ch)
                 (usbe-hid-done k -3))
               (progn
                 (dwc2-write (dwc2-hcint ch) #xFFFFFFFF)
                 (cond
                   ((logtest hcint (hcint-stall)) (usbe-hid-done k -1))
                   ((= st 1)                                 ; the SSPLIT's answer
                    (cond ((logtest hcint (hcint-ack))       ; accepted: CSPLIT later
                           (usbe-put #x108 (logand (dwc2-read (dwc2-hfnum)) #x3FFF))
                           (usbe-put #x100 2)
                           (%usbe-sof-on t))
                          ((logtest hcint (hcint-nak)) (usbe-hid-done k 0))
                          (t (usbe-inc #x110)
                             (if (> (usbe-get #x110) 3)
                                 (usbe-hid-done k -2)
                                 (usbe-split-program k nil)))))
                   (t                                        ; the CSPLIT's answer
                    (cond ((logtest hcint (hcint-nyet))      ; not yet: next microframe
                           (usbe-inc #x10C)
                           (if (> (usbe-get #x10C) 40)
                               (usbe-hid-done k -3)
                               (progn (usbe-put #x108 (logand (dwc2-read (dwc2-hfnum)) #x3FFF))
                                      (usbe-put #x100 2)
                                      (%usbe-sof-on t))))
                          ((or (logtest hcint (hcint-xfercompl)) (logtest hcint (hcint-ack)))
                           (usbe-hid-report k)
                           (usbe-hid-done k 1))
                          ((logtest hcint (hcint-nak)) (usbe-hid-done k 0))
                          (t (usbe-inc #x110)
                             (if (> (usbe-get #x110) 3)
                                 (usbe-hid-done k -2)
                                 (progn (usbe-split-program k nil) (usbe-put #x100 1)))))))))))))))

(defun usbe-hid-abort ()
  "Before anything else uses channel 6 (a hub scan's device setup, a recovery): stop a
   transaction in flight; the round starts again."
  (unless (zerop (usbe-get #x100))
    (dwc2-halt-channel (split-ch))
    (%usbe-sof-on nil)
    (usbe-put #x100 0)
    (usbe-put #x104 0))
  (usbe-put #x12C 0))                                     ; the caller re-arms the receive

(defun usbe-hotplug-service ()
  (let ((hub (hid-get #x00)))
    (when (and (> hub 0) (hid-time-reached-p (usbe-get #x5C)))
      (usbe-put #x5C (logand (+ (hid-now) (hid-hotplug-period)) #x3FFF))
      (usbe-inc #x38)
      ;; hub traffic with the bulk-IN armed disabled the root port: take what has
      ;; arrived, halt the receive, scan, re-arm
      (usbe-rx-service)
      (usbe-hid-abort)
      (dwc2-halt-channel 1)
      ;; a scan that finds a new device sets it up, and that conses (measured: the
      ;; engine's one allocating pass was a scan that met a stale connect change): scan
      ;; as an ordinary thread, then come back to the safe region with a new baseline
      (%gc-safe-leave)
      (let ((t0 (rdtsc)))
        ;; the whole tree (net/usb-tree.lisp) once it has been enumerated; else the one hub
        (if (> (usbt-count) 0) (usbt-scan) (%hid-hotplug-scan hub))
        ;; a scan holds the receive halted: a long one is worth a line ('SCAN', ms)
        (let ((ms (truncate (- (rdtsc) t0) (max 1 (truncate (cntfrq) 1000)))))
          (when (> ms 20) (usbe-log-limited 0 2 #x4E414353 ms (dwc2-read (dwc2-hprt0))))))
      (%gc-safe-enter)
      (usbe-put #x44 (logand (get-alloc-ptr) #xFFFFFFFF))
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
      (usbe-log-limited 3 3 (%t-port) p (usbe-get #x40)))
    (usbe-put #x40 p)
    ;; connected but NOT enabled: the bus is gone until the port is reset -- recover, at
    ;; most every 5 s (+0x74: when, in counter ticks / 65536, the next attempt may start)
    (when (and (logbitp 0 p) (not (logbitp 2 p))
               (>= (logand (- (ash (rdtsc) -16) (usbe-get #x74)) #xFFFFFFFF) 0)
               (< (logand (- (ash (rdtsc) -16) (usbe-get #x74)) #xFFFFFFFF) #x80000000))
      (usbe-put #x74 (+ (ash (rdtsc) -16) (* 5 (ash (max 1 (cntfrq)) -16))))
      (usbe-recover))))

;;; ---- RECOVERY (stage 4) -------------------------------------------------------------
;;;
;;; The failure this exists for: the root port DISABLED (HPRT0 PrtEna clear with the hub
;;; still connected) -- every device behind it gone at once, and nothing that waits on a
;;; transfer ever woken.  A disabled port is enabled again only by a USB reset, and a reset
;;; of the hub resets everything behind it, so recovery is the whole bus, cold:
;;;   1. HID-USB-BRINGUP: controller init, root-port reset, the hub at address 1, its ports
;;;      powered;
;;;   2. HID-SPLIT-INIT: the keyboard and mouse found and set up again;
;;;   3. USB-NIC-COLD-BRINGUP: the RTL8153 reset, addressed and put in CDC-ECM (its vendor
;;;      mode needs the vendor init U-Boot does, which modus does not have; CDC-ECM is a
;;;      standard class -- kiln's net-up has driven the board's network this way);
;;;   4. the engine's interrupt routing again (step 1 reset the controller).
;;; The network's own state (addresses, ARP, TCP connections) is untouched: TCP sends again
;;; what was lost.  Recovery CONSES (HID-SPLIT-INIT returns a list), so the engine leaves its
;;; GC-safe region for it, as any thread may, and takes a new allocation baseline after.
;;; Each step is logged ('RCVR' step result).

(defun %t-rcvr () #x52564352)

(defun usb-nic-cold-bringup (base)
  "The RTL8153 behind the hub, from whatever state, into CDC-ECM; bound as the NIC.  Its
   address, or 0 if none answered.  kiln's NET-UP sequence (boot/zero-ssh.lisp):
     1. every connected high-speed port DISABLED first -- after a cold bring-up they come
        up enabled, so several devices sit at the default address 0 at once and every
        transfer to address 0 collides (measured: no candidate answered);
     2. one port at a time: reset it (that enables it alone), read the descriptor at
        address 0, give the device its OWN address (BASE + its port: a device that is not
        the RTL8153 keeps it), check 0BDA:8153 -- a few tries, since the RTL8153 leaves
        the bus when it is reset and comes back;
     3. configuration 2 (CDC-ECM), data interface alternate 1, packet filter directed +
        broadcast + multicast."
  (let ((hub (hid-get #x00)) (n (hid-get #x04)) (p 1) (found 0) (hs 0))
    (when (> hub 0)
      ;; 1
      (loop
        (when (> p n) (return nil))
        (let ((st (hid-port-status hub p)))
          (when (and (logbitp 0 st) (logbitp 10 st))
            (setq hs (logior hs (ash 1 p)))
            (hid-reserve-port p)                              ; the scan leaves it alone
            (usb-control-transfer hub #x23 1 1 p 0 0)))        ; CLEAR_FEATURE(PORT_ENABLE)
        (setq p (+ p 1)))
      ;; 2, 3
      (setq p 1)
      (loop
        (when (or (> p n) (> found 0)) (return nil))
        (when (logbitp p hs)
          (let ((addr (+ base p)) (ids 0) (try 0))
            (loop
              (when (or (>= try 3) (> found 0) (/= ids 0)) (return nil))
              (setq try (+ try 1))
              (usb-control-transfer hub #x23 3 4 p 0 0)        ; SET_FEATURE(PORT_RESET)
              (dwc2-delay-ms 100)
              (hid-clear-port-changes hub p)
              (dwc2-delay-ms 20)
              (if (and (> (usb-get-descriptor 0 1 0 (usb-data-buf) 8) 0)
                       (> (usb-control-transfer 0 0 5 addr 0 0 0) 0)        ; SET_ADDRESS
                       (progn (dwc2-delay-ms 10)
                              (> (usb-get-descriptor addr 1 0 (usb-data-buf) 18) 0)))
                  (progn
                    (setq ids (logior (ash (usb-desc-u16 (usb-data-buf) 8) 16)
                                      (usb-desc-u16 (usb-data-buf) 10)))
                    (when (and (= ids #x0BDA8153)
                               (> (usb-control-transfer addr #x00 9 2 0 0 0) 0)      ; config 2
                               (> (usb-control-transfer addr #x01 11 1 1 0 0) 0)     ; alt 1
                               (> (usb-control-transfer addr #x21 #x43 #x0E 0 0 0) 0)) ; filter
                      (setq found p)))
                  ;; nothing at address 0: it may have left the bus -- wait for it (4 s)
                  (let ((i 0))
                    (loop (when (or (> i 400) (logbitp 0 (hid-port-status hub p))) (return nil))
                          (dwc2-delay-ms 10) (setq i (+ i 1)))
                    (dwc2-delay-ms 150))))
            (kmsg-event 2 #x4243494E p ids (if (= found p) addr 0))  ; 'NICB' port vid:pid addr
            (unless (= found p) (hid-release-port p))))
        (setq p (+ p 1))))
    (if (zerop found)
        0
        (let ((addr (+ base found)))
          (usb-set-dev-addr addr)
          (usb-set-bulk-in-ep 1) (usb-set-bulk-out-ep 2)
          (usb-set-bulk-in-mps 512) (usb-set-bulk-out-mps 512)
          (usb-set-bulk-in-toggle 0) (usb-set-bulk-out-toggle 0)
          (modus.net.cdc:cdc-read-mac (e1000-state-base))
          (usb-netdev-set 1)
          addr))))

(defun usbe-wait-hub-ports (hub)
  "After the hub's ports are powered, devices take a while to connect (USB 2.0 gives a
   port 100 ms to power up, then a connect needs its debounce): wait until the set of
   connected ports has not changed for 600 ms, at most 4 s.  The mask."
  (let ((n (hid-get #x04)) (last -1) (stable 0) (k 0))
    (loop
      (when (or (>= stable 6) (> k 40)) (return last))
      (let ((m 0) (p 1))
        (loop (when (> p n) (return nil))
              (when (logbitp 0 (hid-port-status hub p)) (setq m (logior m (ash 1 p))))
              (setq p (+ p 1)))
        (if (= m last) (setq stable (+ stable 1)) (setq stable 0 last m)))
      (dwc2-delay-ms 100)
      (setq k (+ k 1)))))

(defun usbe-irq-setup (core)
  "The engine's interrupt routing on the controller (again after a controller reset)."
  (dwc2-write (dwc2-haintmsk) (logior 2 (ash 1 (split-ch)))) ; the receive and the splits
  (io-usb-irq-enable #x03000000)                            ; GINTMSK: HChInt | PrtInt
  (io-route-gpu-irq core))

(defun usbe-recover ()
  "Bring the whole bus back (see RECOVERY above).  Called by the engine on its own core."
  (let ((core (usbe-get #xC)))
    (usbe-inc #x70)
    (kmsg-event 3 (%t-rcvr) 0 (dwc2-read (dwc2-hprt0)) (usbe-get #x70))
    (usbe-hid-abort)
    (%gc-safe-leave)
    (usb-netdev-set 0)
    ;; the whole tree again, from a reset of the root port (net/usb-tree.lisp)
    (let ((n (usbt-enumerate)))
      (kmsg-event 3 (%t-rcvr) 1 n 0)
      (let ((r (usbt-bind-hid)))
        (kmsg-event 3 (%t-rcvr) 2 (if (car r) 1 0) (if (cadr r) 1 0)))
      (hid-put #x1F8 (+ (%thr-cpu) 1))                       ; the HID driver's owner again
      (let ((a (usbt-bind-nic)))
        (when (> a 0) (hid-request-high-speed-reserve))
        (kmsg-event 3 (%t-rcvr) 3 a (usb-netdev-get))))
    (usbe-irq-setup core)
    (unless (zerop (usbe-nic)) (dwc2-halt-channel 1) (usbe-rx-arm))
    (usbe-put #x40 (dwc2-read (dwc2-hprt0)))
    (kmsg-event 3 (%t-rcvr) 4 (dwc2-read (dwc2-hprt0)) (usbe-nic))
    (%gc-safe-enter)
    (usbe-put #x44 (logand (get-alloc-ptr) #xFFFFFFFF))))

(defun usbe-test-disable-port ()
  "FOR TESTS: disable the root port, the failure recovery exists for (HPRT0 PrtEna is
   write-1-to-DISABLE).  From any core; the engine notices on its next pass."
  (let ((p (dwc2-read (dwc2-hprt0))))
    (dwc2-write (dwc2-hprt0) (logior (logand p (dwc2-hprt0-mask)) 4))
    (kmsg-event 2 #x54534554 p (dwc2-read (dwc2-hprt0)) (dwc2-hprt0-mask))   ; 'TEST'
    p))

;;; ---- per-service accounting: +0x80 + 16*K for K = rx tx port hid hotplug net -------
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
    (dolist (name '(:rx :tx :port :hid :hotplug :net))
      (let ((b (+ #x80 (* 16 k))))
        (push (list name (/ (usbe-get b) tick-ms) (usbe-get (+ b 4)) (usbe-get (+ b 8))
                    (/ (usbe-get (+ b 12)) tick-ms))
              acc))
      (setq k (+ k 1)))
    (nreverse acc)))

;;; ---- the loop --------------------------------------------------------------------
(defun usbe-main ()
  "The engine.  Run as a thread alone on its core (USBE-START); returns when stopped."
  (let ((core (%kmsg-core)))
    (usbe-put #xC core)
    (io-hcr-imo-on)                           ; at EL2, without it an IRQ never ends WFI
    (io-route-gpu-irq core)
    (io-core-vtimer-irq core t)
    (setf (mem-ref (+ #x40000050 (* 4 core)) :u32) 1)          ; mailbox 0 -> IRQ
    (setf (mem-ref (%usbe-mbox-clear core) :u32) #xFFFFFFFF)
    (hid-put #x1F8 (+ (%thr-cpu) 1))          ; the HID driver's owner: this thread
    (dwc2-write (dwc2-haintmsk) (logior 2 (ash 1 (split-ch))))  ; the receive, the splits
    ;; the receive: an RTL8153 transfer that is armed stays armed (its NAKs quieted);
    ;; anything else is halted and armed afresh
    (if (and (= (usbe-nic) 2) (zerop (mem-ref (+ (e1000-state-base) #x48) :u32)))
        (dwc2-write (dwc2-hcintmsk 1) #x78F)
        (progn (dwc2-halt-channel 1) (usbe-rx-arm)))
    (io-usb-irq-enable #x03000000)            ; GINTMSK: HChInt | PrtInt
    (usbe-put #x40 (dwc2-read (dwc2-hprt0)))
    (%gc-safe-enter)
    (setup-irq)
    (usbe-put 4 1)
    (kmsg-event 1 (%t-usbe) core 1 0)
    (usbe-put #x44 (logand (get-alloc-ptr) #xFFFFFFFF))
    (loop
      (when (/= (usbe-get 8) 0) (return nil))
      (usbe-inc #x10)
      (setf (mem-ref (%usbe-mbox-clear core) :u32) #xFFFFFFFF)
      (let ((work 0) (t0 (rdtsc)) (a0 (get-alloc-ptr)))
        ;; the port first: nothing else may wait on transfers to a bus that is gone
        (usbe-port-service)
        (setq t0 (usbe-measure 2 t0 a0)) (setq a0 (get-alloc-ptr))
        (setq work (usbe-rx-service))
        (setq t0 (usbe-measure 0 t0 a0)) (setq a0 (get-alloc-ptr))
        (setq work (+ work (usbe-tx-service)))
        (setq t0 (usbe-measure 1 t0 a0)) (setq a0 (get-alloc-ptr))
        ;; the network stack, once it is on this core (net/io-net.lisp): the frames the
        ;; receive just brought, then each connection's sends and timers
        (setq work (+ work (ion-step)))
        (setq t0 (usbe-measure 5 t0 a0)) (setq a0 (get-alloc-ptr))
        (usbe-hid-step)
        (setq t0 (usbe-measure 3 t0 a0)) (setq a0 (get-alloc-ptr))
        (usbe-hotplug-service)
        (usbe-measure 4 t0 a0)
        (unless (= (logand (get-alloc-ptr) #xFFFFFFFF) (usbe-get #x44))
          (when (zerop (usbe-get #x48)) (kmsg-event 3 (%t-cons) (usbe-get #x10) 0 0))
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
    (kmsg-event 1 (%t-usbe) core 3 (usbe-get #x48))
    (usbe-put 4 3)
    0))

;;; ---- the other side: producers and the consumer (any core but the engine's) -----
(defun usbe-send (buf len)
  "Queue the frame in BUF (LEN bytes) on this core's TX ring and wake the engine.  1,
   or 0 when the ring stayed full."
  (let* ((ring (usbe-tx-ring (%kmsg-core))) (h (mem-ref ring :u32)) (k 0) (n (min len 2032)))
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

;; A wait must let core 0's other threads run: the network service fills the queue the old
;; interface reads (net/net-service.lisp), so a TCP-RECEIVE that only spun here waited for
;; frames nothing would ever hand it.  NIL in an image without threads;
;; net/usb-engine-threads.lisp binds it to BT-YIELD.
(declaim (overridable usbe-idle-yield))
(defun usbe-idle-yield () nil)

(defun usbe-rx-wait ()
  "Wait a little for a frame (the net stack's idle poll), letting core 0's other threads
   run meanwhile: T when one is there."
  (let ((i 0))
    (loop
      (when (/= (usbe-get #x1000) (usbe-get #x1004)) (return t))
      (when (>= i 5000) (return nil))
      (when (zerop (logand i 255)) (usbe-idle-yield))
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
            :background-rounds (usbe-get #x6C) :recoveries (usbe-get #x70)
            :tx-no-nic (usbe-get #x78)
            :hid-reports (usbe-get #x118) :hid-naks (usbe-get #x11C)
            :hid-failures (usbe-get #x120) :sof-wakes (usbe-get #x124)
            :nic (usbe-nic))))

(defun usbe-init-block ()
  (dotimes (i 80) (usbe-put (* 4 i) 0))          ; +0x00..+0x13F: counters, limits, HID
  (usbe-put #x1000 0) (usbe-put #x1004 0)
  (dotimes (c 4) (setf (mem-ref (usbe-tx-ring c) :u32) 0) (setf (mem-ref (+ (usbe-tx-ring c) 4) :u32) 0))
  (usbe-put #x64 (%usbe-boot))
  (usbe-put 0 #x45425355))
