;;;; build-i386-hello.lisp — WS5 Stage B smoke test.
;;;;
;;;; The smallest possible HOSTED Linux/i386 ELF the new :linux-i386 target can
;;;; produce: no CL runtime, no compiler — just enough to prove the ELF32
;;;; wrapper, the entry stub (argv staging + mmap2 heap + global slot block),
;;;; the write(2) serial trap and the exit syscall all work end to end.
;;;;
;;;;   sbcl --script mvm/build-i386-hello.lisp
;;;;   qemu-i386-static /home/claude/ws5-gate-out/modus-i386-hello ; echo $?
;;;; Expected: prints "MODUS-I386-OK" and exits 0.
;;;;
;;;; (binfmt_misc is not registered for i386 here, so running the binary
;;;;  directly silently fails to exec — always go through qemu-i386-static.)

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))

;; NB: load the boot file BEFORE switching packages — inside :modus.mvm the
;; name MVM-LOAD is the 4-argument MVM ISA emitter, not the file loader.
(mvm-load "boot/boot-linux-i386.lisp")

(in-package :modus.mvm)

(modus.mvm.i386:install-i386-translator)

;; Hosted-Linux codegen: serial traps become write(2), syscall traps appear.
(setf modus.mvm.i386::*i386-linux-mode* t)
;; Relocate the absolute global slot block out of the unmappable low #x600
;; and into the demand-zeroed BSS.
(modus.mvm.i386::i386-set-globals-base +linux-i386-globals+)
;; Function tagging + 16-byte entry alignment (the CL contract).
(setf modus.mvm.i386::*i386-fn-tag-3* t)
(setf modus.mvm.i386::*i386-fn-align* 16)
(setf modus.mvm.i386::*i386-native-code-offset* 0)

(defparameter *hello-source* "
(defun putc (c)
  (write-char-serial c))

(defun sys-exit (code)
  (syscall3 1 code 0 0))

(defun kernel-main ()
  (putc 77)   ; M
  (putc 79)   ; O
  (putc 68)   ; D
  (putc 85)   ; U
  (putc 83)   ; S
  (putc 45)   ; -
  (putc 73)   ; I
  (putc 51)   ; 3
  (putc 56)   ; 8
  (putc 54)   ; 6
  (putc 45)   ; -
  (putc 79)   ; O
  (putc 75)   ; K
  (putc 10)
  (sys-exit 0))
")

(format t "~%Building hosted Linux/i386 smoke image...~%")

(let ((image (build-image :target :linux-i386 :source-text *hello-source*)))
  (format t "  boot code:   ~D bytes~%" (length (kernel-image-boot-code image)))
  (format t "  native code: ~D bytes~%" (length (kernel-image-native-code image)))
  (format t "  entry offset: ~A~%" (kernel-image-entry-point image))
  (let ((path (or #+sbcl (sb-ext:posix-getenv "MODUS_I386_OUT")
                  "/home/claude/ws5-gate-out/modus-i386-hello")))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (format t "~%Wrote ~D bytes to ~A~%"
            (length (kernel-image-image-bytes image)) path)
    (format t "Run: qemu-i386-static ~A~%" path)))
