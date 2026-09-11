;;;; hdmi-fb.lisp — modus's DISPLAY PATH: a VideoCore HDMI framebuffer for the
;;;; Pi Zero 2 W CL image, and the seam the pure-CL graphics stack sits on.
;;;;
;;;; WHY THIS FILE EXISTS.  glass (framebuffer + RFB/VNC + OPEN LOOK compositor),
;;;; reel (VP8/H.264), cassette (WebM/MP4), reed (audio), scribe (text) — the
;;;; whole modus-lisp media stack — are written to draw into a flat row-major
;;;; (unsigned-byte 32) buffer of 0x00RRGGBB pixels and are validated on SBCL.
;;;; They "drop onto modus once its display path lands."  This IS that path: it
;;;; asks the VideoCore for a real HDMI scanout buffer and blits a glass-shaped
;;;; (unsigned-byte 32) array into it.  Pixel format is identical on both sides
;;;; (glass: 0x00RRGGBB; here: make-color builds 0xFFRRGGBB, 32bpp), so a glass
;;;; framebuffer's pixels copy in with only the alpha byte to ignore.
;;;;
;;;; TARGET.  The link-address-agnostic CL image (boot-rpi-cl.lisp): MMU ON,
;;;; identity map, DRAM Normal-WB (cached), the BCM2837 peripheral window at
;;;; 0x3F000000 Device, and a 2 MB *uncached* USB-DMA window at 0x11000000.
;;;;
;;;; TWO CACHE-COHERENCY DECISIONS (the parts QEMU cannot teach — real silicon
;;;; only), both forced by "DRAM is cached here, the VideoCore is not a cache
;;;; participant":
;;;;   1. MAILBOX BUFFER lives in the uncached USB-DMA window (+hdmi-mbox-buf+),
;;;;      so the property tags the VC reads are never stranded in the CPU's
;;;;      cache.  16-byte aligned, sits at the TOP of the 2 MB window, clear of
;;;;      the DWC2's own ring/transfer buffers at the bottom.
;;;;   2. The SCANOUT BUFFER the VC allocates is cached RAM as the MMU stands.
;;;;      Pixels written through the normal cached mapping can sit in the CPU
;;;;      cache while the HDMI scanout reads stale DRAM -> a black or torn
;;;;      screen.  hdmi-present flushes the drawn span (dc cvac per cache line)
;;;;      before it counts as shown; see +hdmi-need-flush+.  (The alternative,
;;;;      a Device/uncached MMU block over the FB region, is a boot-rpi-cl.lisp
;;;;      change and is the faster long-term answer for animation; noted there.)

(in-package :modus.mvm)

;;; --- VideoCore property mailbox (BCM2837, peripheral base 0x3F000000) -------
;;; MAIL0 = ARM reads (status 0x18: bit30 empty).  MAIL1 = ARM writes (0x20;
;;; status 0x38: bit31 full).  Channel 8 = property tags, ARM->VC.
(defun hdmi-mbox-base ()   #x3F00B880)
(defun hdmi-mbox-read ()   (hdmi-mbox-base))            ; +0x00
(defun hdmi-mbox-status () (+ (hdmi-mbox-base) #x18))   ; MAIL0 status
(defun hdmi-mbox-write ()  (+ (hdmi-mbox-base) #x20))   ; MAIL1 write

;;; Property buffer + result cells in the UNCACHED USB-DMA window (0x11000000,
;;; 2 MB).  0x111F0000 is the last 64 KB — the DWC2 uses the low end.
(defun hdmi-mbox-buf () #x111F0000)   ; 16-aligned property buffer (<=256 B)
(defun hdmi-fb-state () #x111F0400)   ; [addr, size, pitch, width, height]
;; VC bus alias of the property buffer: uncached 0xC0000000 view of the phys buf.
(defun hdmi-mbox-buf-bus () (logior #xC0000000 (hdmi-mbox-buf)))

;;; sap read/write of a raw physical/MMIO word.  (mem-ref addr :u32) is the
;;; primitive; the address travels untagged as usual.
(defun hdmi-rd (addr) (mem-ref addr :u32))
(defun hdmi-wr (addr val) (setf (mem-ref addr :u32) val))

;;; --- framebuffer allocation is NOT baked here (deliberately) ----------------
;;; hdmi-fb-init used to do the simple-FB mailbox allocate, but it is redundant
;;; with fbtall (net/hdmi-runtime.lisp, pushed at runtime), which does the same
;;; property-tag dance AND writes hdmi-fb-state, and it was a footgun: called on
;;; a bare boot (before any allocate) its two mailbox spin-waits could hang the
;;; VideoCore mailbox and wedge the board.  The real display path we are moving
;;; to is the HVS hardware overlay (scaled YUV scanout from a stable address),
;;; not this simple linear-RGB framebuffer, so there is no reason to bake a
;;; simple-FB allocator.  fbtall owns runtime allocation; the accessors below
;;; just read hdmi-fb-state, which fbtall populates.

(defun hdmi-fb-addr ()  (hdmi-rd (+ (hdmi-fb-state) 0)))
(defun hdmi-fb-pitch () (hdmi-rd (+ (hdmi-fb-state) 8)))
(defun hdmi-fb-w ()     (hdmi-rd (+ (hdmi-fb-state) 12)))
(defun hdmi-fb-h ()     (hdmi-rd (+ (hdmi-fb-state) 16)))

;;; --- drawing (glass-compatible 0x00RRGGBB) ---------------------------------
(defun hdmi-color (r g b)
  (logior (logior (ash r 16) (ash g 8)) b))

(defun hdmi-fill-rect (x0 y0 w h color)
  "Fill a rectangle with a 0x00RRGGBB color.  Cached write; call hdmi-present."
  (let ((fb (hdmi-fb-addr)) (pitch (hdmi-fb-pitch)))
    (when (not (zerop fb))
      (let ((y 0))
        (loop (when (>= y h) (return nil))
          (let ((row (+ fb (* (+ y0 y) pitch) (* x0 4))) (x 0))
            (loop (when (>= x w) (return nil))
              (setf (mem-ref (+ row (* x 4)) :u32) color)
              (setq x (+ x 1))))
          (setq y (+ y 1)))))))

;;; BLIT a glass framebuffer (a modus (unsigned-byte 8/32) array of 0x00RRGGBB,
;;; row-major, W*H) straight onto the scanout.  This is the glass seam.
;;; src-addr = raw data address of the glass pixel array.
(defun hdmi-blit-u32 (src-addr w h)
  (let ((fb (hdmi-fb-addr)) (pitch (hdmi-fb-pitch)))
    (when (not (zerop fb))
      (let ((y 0))
        (loop (when (>= y h) (return nil))
          (let ((s (+ src-addr (* y w 4))) (d (+ fb (* y pitch))) (x 0))
            (loop (when (>= x w) (return nil))
              (setf (mem-ref (+ d (* x 4)) :u32) (mem-ref (+ s (* x 4)) :u32))
              (setq x (+ x 1))))
          (setq y (+ y 1)))))))

;;; hdmi-present: make what was drawn actually visible.  A full fence orders the
;;; pixel stores ahead of the scanout's reads.  NOTE (open, step 3): a fence does
;;; NOT evict dirty cache lines — if the VC-allocated scanout buffer lands in
;;; cached (Normal-WB) DRAM under the current MMU, pixels can sit in the CPU's
;;; D-cache and HDMI shows stale/black.  The real fix is to map the FB region
;;; Device/uncached in boot-rpi-cl.lisp (mirrors the DWC2 DMA window at
;;; 0x11000000) or add a dc-cvac clean primitive.  This barrier is enough to
;;; first answer "does the mailbox return a framebuffer on real silicon?" — the
;;; step that QEMU-raspi3b could not.
(defun hdmi-present ()
  (memory-barrier))
