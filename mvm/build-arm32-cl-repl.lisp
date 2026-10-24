;;;; build-arm32-cl-repl.lisp — the bare-metal ARMv7 CL REPL image (QEMU virt).
;;;;
;;;;   sbcl --dynamic-space-size 12288 --script mvm/build-arm32-cl-repl.lisp
;;;;   qemu-system-arm -M virt,highmem=off -cpu cortex-a15 -m 1024 \
;;;;       -kernel /tmp/modus-arm32-cl-repl.bin -display none -serial stdio -no-reboot
;;;;
;;;; The real CL stack on bare 32-bit ARM, replacing the mini-Lisp
;;;; build-arm32-repl.lisp.  Everything is in mvm/build-cl-repl-common.lisp
;;;; (platform :ARM32), boot/boot-arm32-cl.lisp and translate-arm32.lisp's
;;;; *ARM32-CL-BARE* mode.  MODUS_CL_REPL_OUT overrides the output path.

(defvar *cl-repl-platform* :arm32)
(load (merge-pathnames "build-cl-repl-common.lisp"
                       (directory-namestring (truename *load-truename*))))
