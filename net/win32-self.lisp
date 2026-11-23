;;;; net/win32-self.lisp -- the file a Windows process was started from.
;;;;
;;;; Compiled into the Windows image only (boot/boot-win64.lisp, *WIN64-PLATFORM-FILES*).
;;;; A modus.com (scripts/make-ape.lisp) carries segments after the PE; on Unix its shell
;;;; header names the file in MODUS_APE, but Windows runs the file itself with no header
;;;; run first, so the image asks: GetModuleFileNameA(NULL), the path of this executable.
;;;; Looked up by name (LoadLibraryA + GetProcAddress) and called through syscall #x3000,
;;;; as net/win32-time.lisp does: nothing is imported, and nothing is kept between calls.

(defun %w32-module-file-name ()
  "This executable's path, with / for \\, or NIL when Windows will not say."
  (let* ((page #x2000)
         (block (syscall6 9 0 page 3 34 -1 0)))
    (if (or (not (integerp block)) (<= block 0))
        nil
        (unwind-protect
             (let* ((name (+ block #x100)) (args (+ block #x200)) (buf (+ block #x400))
                    (lib (syscall6 +w32-loadlibrarya+ (%ww-put-cstr name "kernel32.dll") 0 0 0 0 0))
                    (fn (if (= lib 0) 0
                            (syscall6 +w32-getprocaddress+ lib (%ww-put-cstr name "GetModuleFileNameA")
                                      0 0 0 0))))
               (unless (= fn 0)
                 (%ww-put64 args 0)                ; hModule NULL: this executable
                 (%ww-put64 (+ args 8) buf)
                 (%ww-put64 (+ args 16) 4096)
                 (let ((n (logand (syscall3 #x3000 fn args 3) #xFFFFFFFF)))
                   (when (< 0 n 4096)
                     (let ((s (make-string n)))
                       (dotimes (i n s)
                         (let ((c (code-char (mem-ref (+ buf i) :u8))))
                           (setf (char s i) (if (char= c #\\) #\/ c)))))))))
          (syscall6 11 block page 0 0 0 0)))))

(defun %ape-self-path ()
  "MODUS_APE when a caller set it, else this executable (lib/cli-toplevel.lisp has the
   default, which on Windows would never see a modus.com)."
  (let ((v (%cli-getenv "MODUS_APE")))
    (if (and v (plusp (length v))) v (%w32-module-file-name))))
