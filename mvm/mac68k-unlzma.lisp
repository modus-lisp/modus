;;;; mac68k-unlzma.lisp -- the bare Mac's boot-disk loader: unpack a compressed
;;;; modus image to its load address and start it.
;;;;
;;;; scripts/mac68k-floppy.py builds this with mvm/unlzma-core.lisp for the
;;;; :mac68k target, LINKED AT +LOADER+ (MODUS_MAC68K_LOAD_ADDR) rather than at
;;;; +mac68k-load-addr+, because that is where the image it unpacks goes.  The
;;;; boot blocks read loader and payload off the disk in one _Read to +LOADER+
;;;; and jump to the loader, whose boot stub (boot/boot-mac68k.lisp) does
;;;; exactly what the image's will do again.  The payload is at +PAY+:
;;;;   +PAY+ + 0  the unpacked size, two u16 halves (hi, lo); 0; the stream's
;;;;              offset from +PAY+
;;;;   +PAY+ + 8  the jumper (68k code from the packer): push the 68040's
;;;;              data cache and invalidate both caches -- the image was
;;;;              written as DATA -- then jump to the image's first byte
;;;;   +PAY+ + n  the LZMA1 stream (lc=2 lp=1 pb=2, no header)
;;;;
;;;; On a 32 MB Mac (boot/boot-mac68k.lisp's map) everything here is memory the
;;;; image's boot stub and heap will claim, and dead by then: the image goes to
;;;; #x00100000; loader + payload at #x01200000 and the coder's state at
;;;; #x01A00000 are inside the heap span, which this loader never allocates
;;;; from (fixnums only); its stub zeroes only the low block, the kind map and
;;;; NIL's page, all clear of both.  Below #x0F000000, so nothing here is
;;;; rebased with the runtime-data region.
;;;;
;;;; Build (scripts/mac68k-floppy.py does it):
;;;;   MODUS_MAC68K_LOAD_ADDR=#x01200000 sbcl --script mvm/build-hosted.lisp \
;;;;     mac68k mvm/mac68k-unlzma.lisp mvm/unlzma-core.lisp
;;;; (in that order: the core reads this file's constants)

(defconstant +loader+ #x01200000)       ; MODUS_MAC68K_LOAD_ADDR
(defconstant +pay+    #x01210000)       ; the loader stays under 64 KB
(defconstant +out+    #x00100000)       ; +mac68k-load-addr+
(defconstant +st+     #x01A00000)       ; RH RL CH CL IPH IPL, u16 each
(defconstant +pr+     #x01A00100)       ; 7991 u16 probabilities

;; Written in the primitive forms, not unlzma-core.lisp's F+ / U16 macros: this
;; head is compiled FIRST (the core needs its constants), and a macro used
;; before its definition compiles as a call to an undefined function.
(defun kernel-main ()
  (let ((size (%fixnum-+ (%fixnum-* (the fixnum (mem-ref +pay+ :u16)) 65536)
                         (the fixnum (mem-ref (%fixnum-+ +pay+ 2) :u16)))))
    (declare (fixnum size))
    (lz-init (%fixnum-+ +pay+ (the fixnum (mem-ref (%fixnum-+ +pay+ 6) :u16))))
    (lz-decode size)
    (%jit-call (%fixnum-+ +pay+ 8))))   ; the jumper: does not return
