;;;; build-hosted.lisp — a hosted Linux image for one of the bare-port arches,
;;;; from one source file.  What the hosted ladder (scripts/hosted-ladder.py)
;;;; builds.
;;;;
;;;; Usage: sbcl --script mvm/build-hosted.lisp ARCH [source.lisp]
;;;;   ARCH    riscv64 | riscv32 | arm32 | ppc64 | ppc32 | 68k
;;;;   source  the image's source text; default: a self-check that writes
;;;;           "OK" through the serial trap (write(2) here) and exits
;;;;   Output: /tmp/modus-ARCH-linux, or the arch's MODUS_*_LINUX_OUT
;;;;   Run:    qemu-ARCH-static /tmp/modus-ARCH-linux
;;;;
;;;; A CLEAN image: no runtime, no corpus, just the named source compiled for
;;;; the arch.  The x64 / aarch64 / i386 hosted images come from the CLI
;;;; lineage instead (build-generic-cli, build-aarch64-cli, build-i386-cli).
;;;;
;;;; This replaces six per-arch scripts that differed only in the row below.

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))

;;; ARCH  boot files  translator installer  hosted-mode setter  :target
;;;       output knob  default output  qemu-user binary
;;;
;;; The boot files: the generic ELF64-LE wrapper lives in
;;; boot-linux-aarch64.lisp and the ELF32-LE one in boot-linux-i386.lisp (both
;;; are arch-neutral apart from e_machine), so RV64 loads the first and
;;; RV32 / arm32 the second before their own descriptor.
;;;
;;; ORDER MATTERS: the translator is installed BEFORE hosted mode is set,
;;; because the installer is what sets the bare-metal slot base that hosted
;;; mode moves.
(defparameter *hosted-ports*
  '(("riscv64" ("boot/boot-linux-aarch64.lisp" "boot/boot-linux-riscv.lisp")
     "INSTALL-RISCV-TRANSLATOR"   "RISCV-SET-LINUX-MODE" :linux-riscv
     "MODUS_RISCV_LINUX_OUT"   "/tmp/modus-riscv-linux"   "qemu-riscv64-static")
    ("riscv32" ("boot/boot-linux-i386.lisp" "boot/boot-linux-riscv32.lisp")
     "INSTALL-RISCV32-TRANSLATOR" "RISCV-SET-LINUX-MODE" :linux-riscv32
     "MODUS_RISCV32_LINUX_OUT" "/tmp/modus-riscv32-linux" "qemu-riscv32-static")
    ("arm32"   ("boot/boot-linux-i386.lisp" "boot/boot-linux-arm32.lisp")
     "INSTALL-ARMV7-TRANSLATOR"   "ARM32-SET-LINUX-MODE" :linux-arm32
     "MODUS_ARM32_LINUX_OUT"   "/tmp/modus-arm32-linux"   "qemu-arm-static")
    ("ppc64"   ("boot/boot-linux-ppc.lisp")
     "INSTALL-PPC-TRANSLATOR"     "PPC-SET-LINUX-MODE"   :linux-ppc64
     "MODUS_PPC64_LINUX_OUT"   "/tmp/modus-ppc64-linux"   "qemu-ppc64-static")
    ("ppc32"   ("boot/boot-linux-ppc.lisp")
     "INSTALL-PPC32-TRANSLATOR"   "PPC-SET-LINUX-MODE"   :linux-ppc32
     "MODUS_PPC32_LINUX_OUT"   "/tmp/modus-ppc32-linux"   "qemu-ppc-static")
    ("68k"     ("boot/boot-linux-68k.lisp")
     "INSTALL-68K-TRANSLATOR"     "M68K-SET-LINUX-MODE"  :linux-68k
     "MODUS_68K_LINUX_OUT"     "/tmp/modus-68k-linux"     "qemu-m68k-static")))

(defparameter *hosted-port*
  (let ((arch (second sb-ext:*posix-argv*)))
    (or (assoc arch *hosted-ports* :test #'equal)
        (progn (format *error-output* "usage: build-hosted.lisp ARCH [source.lisp]~%  ARCH: ~{~A~^ | ~}~%"
                       (mapcar #'first *hosted-ports*))
               (sb-ext:exit :code 2)))))

(destructuring-bind (arch boot-files installer mode-setter target out-env out-default qemu)
    *hosted-port*
  (declare (ignore arch))
  (dolist (f boot-files) (mvm-load f))
  (require :sb-posix)
  (funcall (find-symbol installer :modus.mvm))
  (funcall (find-symbol mode-setter :modus.mvm) t)
  (let* ((src (third sb-ext:*posix-argv*))
         (out (or (sb-ext:posix-getenv out-env) out-default))
         (text (if src
                   (read-file-text src)
                   ;; Bytes out through the serial trap, then SYS-EXIT: falling
                   ;; off the end of kernel-main runs into whatever follows it.
                   ;; Hosted images exit, they do not return.
                   "(defun kernel-main () (write-char-serial 79) (write-char-serial 75) (write-char-serial 10) (sys-exit 0))")))
    (format t "~&Building hosted Linux image for ~A...~%" target)
    (let ((image (modus.mvm::build-image :target target :source-text text)))
      (format t "entry=~A native=~D boot=~D~%"
              (modus.mvm::kernel-image-entry-point image)
              (length (modus.mvm::kernel-image-native-code image))
              (length (modus.mvm::kernel-image-boot-code image)))
      (with-open-file (o out :direction :output :element-type '(unsigned-byte 8)
                             :if-exists :supersede)
        (write-sequence (modus.mvm::kernel-image-image-bytes image) o))
      ;; MAKE IT EXECUTABLE.  qemu-user rejects a non-executable file SILENTLY
      ;; (exit 1, nothing on stderr), which reads exactly like a broken image.
      ;; FIND-SYMBOL, not SB-POSIX:CHMOD: check-source-parses READS every
      ;; first-party file before any build, and that symbol does not exist
      ;; until the contrib is required, so the qualified name would fail the
      ;; parse sweep for every target.
      (funcall (find-symbol "CHMOD" "SB-POSIX") out #o755)
      (format t "Wrote ~D bytes to ~A~%" (length (modus.mvm::kernel-image-image-bytes image)) out)
      (format t "Run: ~A ~A~%" qemu out))))
