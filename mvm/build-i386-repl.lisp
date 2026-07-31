;;;; build-i386-repl.lisp - Build i386 REPL kernel for QEMU
;;;; LEGACY MINI-LISP PATH.  This image runs mvm/repl-source.lisp — a separate
;;;; 708-line toy Lisp with its own reader, printer and evaluator, NOT the real
;;;; Common Lisp stack.  The real CL image on i386 is mvm/build-i386-cli.lisp
;;;; (prelude + gc + rt + the cl-*.lisp bridge + crypto), driven by
;;;; scripts/run-i386.sh.  Retiring this second Lisp is its own workstream.
;;;;
;;;; Usage: sbcl --script mvm/build-i386-repl.lisp
;;;;
;;;; Produces /tmp/modus-i386.bin — boot with:
;;;;   qemu-system-i386 -kernel /tmp/modus-i386.bin -m 256 -nographic

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(mvm-load "mvm/repl-source.lisp")

(in-package :modus.mvm)

;; Install the i386 translator
(modus.mvm.i386:install-i386-translator)

(format t "Building i386 REPL image...~%")
(let ((image (build-image :target :i386 :source-text *repl-source*)))
  (format t "Entry point offset: ~A~%" (kernel-image-entry-point image))
  (format t "Native code size: ~D~%" (length (kernel-image-native-code image)))
  (format t "Boot code size: ~D~%" (length (kernel-image-boot-code image)))
  (write-kernel-image image "/tmp/modus-i386.bin")
  (format t "Done. Boot with:~%")
  (format t "  qemu-system-i386 -kernel /tmp/modus-i386.bin -m 256 -nographic~%"))
