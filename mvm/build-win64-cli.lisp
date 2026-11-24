;;;; build-win64-cli.lisp — the hosted CLI as a Windows x86-64 PE32+ console EXE.
;;;;
;;;;   sbcl --dynamic-space-size 4096 --script mvm/build-win64-cli.lisp   # -> modus.exe
;;;;   test/win64/run-wine.sh modus.exe                                    # under Wine
;;;;
;;;; The same image as ./modus (same source, same runtime, runtime JIT on), built
;;;; by build-generic-cli.lisp with *CLI-WIN64* set; every Windows difference there
;;;; is guarded by that flag, and the Windows facts are in boot/boot-win64.lisp.
;;;; docs/win64.md is the overview.
;;;;
;;;; Knobs: MODUS_WIN64_OUT (output, default modus.exe); MODUS_NO_JIT=1
;;;; (interpreter only); MODUS_WIN64_TRACE=1 (the shim writes every syscall number
;;;; to stderr); and every build-generic-cli knob (MODUS_X64_MIDPOINT, ...).

(defvar cl-user::*cli-win64* t)
(setq cl-user::*cli-win64* t)
(load (merge-pathnames "build-generic-cli.lisp"
                       (directory-namestring (truename *load-truename*))))
