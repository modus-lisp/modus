;;;; mvm/build-ape-stub.lisp -- the Windows launcher modus.com starts with.
;;;;
;;;;   MODUS_APE_STUB_OUT=modus-ape-stub.exe  sbcl --script mvm/build-ape-stub.lisp
;;;;   (or a modus builder CLI: the same bytes)
;;;;
;;;; mvm/ape-stub-win64.lisp, compiled for :WIN64 with no CL runtime: the PE that
;;;; scripts/make-ape.lisp puts at the front of modus.com when the Windows CLI goes in
;;;; compressed.  This script adds what that source cannot write for itself -- the
;;;; kernel32 import constants, and one %S-NAME function per byte string it needs --
;;;; and maps a small heap (it allocates nothing on it) and no JIT arena, since Windows
;;;; commits both at startup.

(load (merge-pathnames "../lib/load-mvm.lisp" (directory-namestring (truename *load-truename*))))
(unless (find-package "MODUS.MVM.X64")
  (cl-user::mvm-load "mvm/x64-asm.lisp")
  (cl-user::mvm-load "mvm/translate-x64.lisp"))
(cl-user::mvm-load "boot/boot-linux-x64.lisp")
(cl-user::mvm-load "boot/boot-win64.lisp")
(in-package :modus.mvm)

(defparameter *ape-stub-strings*
  '(("kernel32" "kernel32.dll")
    ("getmodulefilenamea" "GetModuleFileNameA") ("createfilea" "CreateFileA")
    ("getfilesizeex" "GetFileSizeEx") ("setfilepointerex" "SetFilePointerEx")
    ("readfile" "ReadFile") ("writefile" "WriteFile") ("closehandle" "CloseHandle")
    ("getenvironmentvariablea" "GetEnvironmentVariableA")
    ("setenvironmentvariablea" "SetEnvironmentVariableA")
    ("createdirectorya" "CreateDirectoryA") ("getfileattributesa" "GetFileAttributesA")
    ("movefileexa" "MoveFileExA") ("getcommandlinea" "GetCommandLineA")
    ("createprocessa" "CreateProcessA") ("waitforsingleobject" "WaitForSingleObject")
    ("getexitcodeprocess" "GetExitCodeProcess")
    ("localappdata" "LOCALAPPDATA") ("temp" "TEMP")
    ("modus-dir" "\\modus") ("modus-dash" "\\modus-") ("dot-exe" ".exe") ("dot-tmp" ".tmp")
    ("modus-ape" "MODUS_APE") ("segment-name" "windows-x64.exe.gz")
    ("no-function" "modus.com: kernel32 lacks a function this launcher needs
")
    ("short-read" "modus.com: could not read this file
")
    ("no-self" "modus.com: Windows will not say where this file is
")
    ("no-trailer" "modus.com: this file has no segment directory
")
    ("no-segment" "modus.com: this file carries no Windows image
")
    ("no-cache" "modus.com: cannot write the image to %LOCALAPPDATA%\\modus
")
    ("bad-image" "modus.com: the Windows image in this file is damaged
")
    ("no-run" "modus.com: cannot run the cached Windows image
"))
  "(NAME TEXT): %S-NAME writes TEXT and a NUL at its argument and returns it.")

(defun ape-stub-source ()
  (with-output-to-string (o)
    (write-string (win64-import-constants-source) o)
    (dolist (e *ape-stub-strings*)
      (destructuring-bind (name text) e
        (format o "(defun %s-~A (p)~%" name)
        (dotimes (i (length text))
          (format o "  (setf (mem-ref (+ p ~D) :u8) ~D)~%" i (char-code (char text i))))
        (format o "  (setf (mem-ref (+ p ~D) :u8) 0)~%  p)~%" (length text))))
    (write-string (cl-user::mvm-text "mvm/ape-stub-win64.lisp") o)
    (let ((extra (sb-ext:posix-getenv "MODUS_APE_STUB_EXTRA")))    ; probe code, appended (a DEFUN replaces the launcher's: MODUS_REDEF_CHECK=0)
      (when (and extra (plusp (length extra)))
        (with-open-file (in extra) (loop for l = (read-line in nil) while l do (write-line l o)))))))

;; a modus host registered its targets at boot (mvm/builder.lisp %BUILDER-BOOT-INIT)
#-modus (funcall (intern "INSTALL-X64-TRANSLATOR" :modus.mvm.x64))
(setf modus.mvm.x64::*x64-linux-mode* t)
(setf modus.mvm.x64::*x64-gc-enabled* nil)
;; Every other emission setting a build script may set, at SBCL's defaults: on a modus
;; builder these would otherwise be the builder's own (mvm/builder.lisp
;; *BUILDER-HOST-JIT-FLAG-NAMES*) -- the CLI's TLS window, STW safepoints, write barrier.
(setf modus.mvm.x64::*x64-win-tls* nil modus.mvm.x64::*x64-gen-gc* nil
      modus.mvm.x64::*x64-gc-region-percpu* nil modus.mvm.x64::*x64-sched-lock-addr* nil
      modus.mvm.x64::*x64-stw* nil modus.mvm.x64::*x64-tls-window* nil
      modus.mvm.x64::*x64-jit-constvec-p* nil modus.mvm.x64::*mcgc-kind-bitmap-enabled* nil
      modus.mvm.x64::*mcgc-kind-check-enabled* t modus.mvm.x64::*mcgc-pinning-enabled* nil
      modus.mvm.x64::*ws5-force-no-kindcheck* nil
      *conv-relative* nil *conv-delta* 0 *hosted-layout* nil *pcrel-layout* nil *tls-window* nil)
(setf *win64-midpoint* #x400000)               ; 4 MB semispaces: committed, never used
(setf *win64-jit-arena-size* #x10000)
;; no collector: the allocation limit is the end of the small heap (a builder's own value
;; would be its midpoint)
(setf *linux-x64-r14-offset* (getf (x64-heap-layout (win64-midpoint)) :map))
(setf modus.mvm.x64::*x64-native-code-offset* (win64-native-code-offset))
(setf modus.mvm.x64::*x64-win-shim-va* (win64-shim-va))
(let ((sm (sb-ext:posix-getenv "MODUS_SYMMAP")))     ; function addresses, for a fault
  (when (and sm (plusp (length sm))) (setf *write-symmap-path* sm)))

(let* ((out (or (let ((v (sb-ext:posix-getenv "MODUS_APE_STUB_OUT"))) (and v (plusp (length v)) v))
                "modus-ape-stub.exe"))
       (image (cl-user::host-build-image :target :win64 :source-text (ape-stub-source))))
  (with-open-file (o out :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (write-sequence (kernel-image-image-bytes image) o))
  (format t "~&build-ape-stub: ~A, native ~D bytes~%" out (length (kernel-image-native-code image))))
