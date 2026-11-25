;;;; build-ppcmac-cl-repl.lisp — the CL REPL on a bare PowerPC Macintosh.
;;;;
;;;;   sbcl --dynamic-space-size 12288 --script mvm/build-ppcmac-cl-repl.lisp
;;;;   sbcl --script scripts/ppcmac-disk.lisp /tmp/modus-ppcmac.elf /tmp/modus-ppcmac.dsk
;;;;   qemu-system-ppc -M mac99 -m 1024 -nographic \
;;;;       -drive file=/tmp/modus-ppcmac.dsk,format=raw,media=disk
;;;;
;;;; Open Firmware boots the disk's blessed System Folder (its bootinfo.txt),
;;;; which loads :modus.elf; serial port A is the console.  Everything else is
;;;; build-cl-repl-common.lisp's :PPCMAC arms, boot/boot-ppcmac.lisp and
;;;; translate-ppc.lisp's *PPC-MAC-MODE*.  MODUS_CL_REPL_OUT overrides the
;;;; output path.
;;;
;;;; THE SMALL IMAGE BY DEFAULT (MODUS_SMALL_IMAGE=1, build-cli-common.lisp):
;;;; the boot tables as packed text, not 16 MB of generated PUTHASH code run
;;;; once at boot.  Measured under QEMU mac99: the image 36.1 -> 22.7 MB, the
;;;; runtime's start-up 106 s -> 3.8 s.  Its global-access calls stay off
;;;; (MODUS_SMALL_ACCESS=0): they save size, not time.  Either variable set
;;;; by the caller wins.
;;;; (SETENV through FIND-SYMBOL: the source-parses gate reads this file
;;;; without SB-POSIX loaded.)
(require :sb-posix)
(flet ((default (var value)
         (unless (sb-ext:posix-getenv var)
           (funcall (find-symbol "SETENV" "SB-POSIX") var value 1))))
  (default "MODUS_SMALL_IMAGE" "1")
  (default "MODUS_SMALL_ACCESS" "0"))
(defvar *cl-repl-platform* :ppcmac)
(load (merge-pathnames "build-cl-repl-common.lisp"
                       (directory-namestring (truename *load-truename*))))
