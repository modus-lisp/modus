;;;; build-aarch64-cl-repl.lisp — the AArch64 QEMU-virt image that runs the REAL
;;;; CL, the QEMU-virt sibling of build-rpi-cl-repl.lisp / build-x64-cl-repl.lisp.
;;;;
;;;; THIN HEAD.  Everything is in mvm/build-cl-repl-common.lisp: bind the one
;;;; selector (:virt) and load the common file, which does the rest.  Used to
;;;; reproduce/debug the aarch64 runtime JIT under QEMU-virt (gdbstub), per the
;;;; bare-metal observability workflow, instead of the flaky Pi board.
;;;;
;;;; Usage: sbcl --dynamic-space-size 12288 --script mvm/build-aarch64-cl-repl.lisp
;;;;   MODUS_VIRT_JIT=1 (default) runtime JIT on; =0 forces interpret.
;;;;   Output: /tmp/modus-aarch64-cl-repl.bin (MODUS_CL_REPL_OUT overrides).
;;;;   Run:    qemu-system-aarch64 -machine virt -cpu cortex-a57 -m 512 \
;;;;             -kernel /tmp/modus-aarch64-cl-repl.bin -nographic -no-reboot

(defvar *cl-repl-platform* :virt)
(load (merge-pathnames "build-cl-repl-common.lisp"
                       (directory-namestring (truename *load-truename*))))
