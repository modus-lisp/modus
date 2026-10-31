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
