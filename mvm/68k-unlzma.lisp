;;;; 68k-unlzma.lisp -- the Linux/m68k floppy image's loader: an LZMA1 decoder
;;;; in Lisp with no runtime (built by mvm/build-hosted.lisp 68k, as a ladder
;;;; rung is).
;;;;
;;;; scripts/68k-floppy-pack.py appends a payload to the ELF this file builds
;;;; and one more PT_LOAD mapping the whole file at +MAP+ (from offset 0, so no
;;;; page padding), with the payload's file offset in e_ident[12..15].  At
;;;; P = +MAP+ + that offset:
;;;;   P + 0  the unpacked size, two u16 halves (hi, lo); a test flag; the
;;;;          stream's offset from P
;;;;   P + 8  the copier (68k code, scripts/68k-floppy-copier.s), then its
;;;;          segment table
;;;;   P + n  the LZMA1 stream (lc=2 lp=1 pb=2, no header)
;;;; The zero-filled memory after the file holds the output (+OUT+) and the
;;;; decoder's state (+ST+, +PR+).  KERNEL-MAIN unpacks the segments into
;;;; +OUT+ and calls the copier, which maps them at their addresses and jumps
;;;; to the image's entry with the stack the process started with.
;;;;
;;;; The decoder itself is mvm/unlzma-core.lisp; build both:
;;;;   mvm/build-hosted.lisp 68k mvm/68k-unlzma.lisp mvm/unlzma-core.lisp
;;;; (in that order: the core reads this file's constants)

(defconstant +map+ #x19000000)
(defconstant +out+ #x19200000)
(defconstant +st+  #x19A00000)          ; RH RL CH CL IPH IPL, u16 each
(defconstant +pr+  #x19A00100)          ; 7991 u16 probabilities

;; Written in the primitive forms, not unlzma-core.lisp's F+ / U16 macros: this
;; head is compiled FIRST (the core needs its constants), and a macro used
;; before its definition compiles as a call to an undefined function.
(defun kernel-main ()
  (let* ((p (%fixnum-+ +map+ (%fixnum-+ (%fixnum-* (the fixnum (mem-ref (%fixnum-+ +map+ 12) :u16)) 65536) (the fixnum (mem-ref (%fixnum-+ +map+ 14) :u16)))))
         (size (%fixnum-+ (%fixnum-* (the fixnum (mem-ref p :u16)) 65536) (the fixnum (mem-ref (%fixnum-+ p 2) :u16)))))
    (declare (fixnum p size))
    (lz-init (%fixnum-+ p (the fixnum (mem-ref (%fixnum-+ p 6) :u16))))
    (lz-decode size)
    (if (= (the fixnum (mem-ref (%fixnum-+ p 4) :u16)) 0)
        (%jit-call (%fixnum-+ p 8))            ; the copier: does not return
        (syscall3 4 1 +out+ size))      ; test mode: the unpacked bytes on stdout
    (sys-exit 0)))
