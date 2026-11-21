;;;; build-mac68k-cl-repl.lisp — the CL REPL on a bare 68k Macintosh.
;;;;
;;;;   sbcl --dynamic-space-size 12288 --script mvm/build-mac68k-cl-repl.lisp
;;;;   scripts/mac68k-disk.py /tmp/modus-mac68k-cl-repl.bin /tmp/modus-mac68k.dsk
;;;;
;;;; then boot the disk on a Mac (Basilisk II with 384 MB) with serial port A as
;;;; the console.  Everything else is build-cl-repl-common.lisp's :MAC68K arms
;;;; and boot/boot-mac68k.lisp.
(defvar *cl-repl-platform* :mac68k)
(load (merge-pathnames "build-cl-repl-common.lisp"
                       (directory-namestring (truename *load-truename*))))
