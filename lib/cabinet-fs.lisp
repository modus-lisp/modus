;;;; lib/cabinet-fs.lisp — mount a cabinet filesystem under CL's OPEN/LOAD.
;;;;
;;;; cl-fileio.lisp's %SYS-* primitives are Linux syscalls.  On bare metal
;;;; there is no OS behind them, so OPEN and LOAD exist as symbols and fault
;;;; when called — which is what stops a real Quicklisp client running on a
;;;; Pi.  cl-fileio therefore carries ONE seam, the function-valued global
;;;; *CAB-CALL*; when it is set, every primitive routes here instead.
;;;;
;;;; This file is LOADED (not baked), after pagetree + cabinet are loaded,
;;;; because cl-fileio is compiled into the image long before the CABINET
;;;; package exists.  That ordering is also why the protocol is one generic
;;;; dispatch function rather than a vtable of typed callbacks: nothing in
;;;; the baked image may name a CABINET symbol.

(defvar *cabfs* nil)              ; the mounted cabinet FS object

;;; Cabinet deals in (unsigned-byte 8) vectors; the stream layer deals in
;;; char codes.  Both are just fixnums here, so no encoding happens at this
;;; boundary — UTF-8 is cabinet's business, above and below us.

(defun %cabfs-sym (name) (intern name (find-package "CABINET")))

(defvar *cf-read* nil) (defvar *cf-write* nil) (defvar *cf-create* nil)
(defvar *cf-exists* nil) (defvar *cf-size* nil) (defvar *cf-mkdir* nil)
(defvar *cf-unlink* nil) (defvar *cf-rename* nil) (defvar *cf-readdir* nil)
(defvar *cf-stat* nil)

(defun %cabfs-resolve ()
  "Cache cabinet's entry points once, so the per-syscall path is a funcall
   and not an INTERN + FIND-PACKAGE."
  (setq *cf-read*    (symbol-function (%cabfs-sym "READ-FILE")))
  (setq *cf-write*   (symbol-function (%cabfs-sym "WRITE-FILE")))
  (setq *cf-create*  (symbol-function (%cabfs-sym "CREATE")))
  (setq *cf-exists*  (symbol-function (%cabfs-sym "EXISTS-P")))
  (setq *cf-size*    (symbol-function (%cabfs-sym "FILE-SIZE")))
  (setq *cf-mkdir*   (symbol-function (%cabfs-sym "MAKE-DIRECTORIES")))
  (setq *cf-unlink*  (symbol-function (%cabfs-sym "UNLINK")))
  (setq *cf-rename*  (symbol-function (%cabfs-sym "RENAME")))
  (setq *cf-readdir* (symbol-function (%cabfs-sym "READDIR")))
  t)

(defun %cabfs-exists (path)
  (handler-case (and (funcall *cf-exists* *cabfs* path) t) (error () nil)))

(defun %cabfs-bytes (path)
  "File contents as an (unsigned-byte 8) vector; empty vector if absent."
  (handler-case (funcall *cf-read* *cabfs* path)
    (error () (make-array 0 :element-type '(unsigned-byte 8)))))

(defun %cabfs-put (path bytes)
  (unless (%cabfs-exists path)
    (handler-case (funcall *cf-create* *cabfs* path) (error () nil)))
  (funcall *cf-write* *cabfs* path bytes))

(defun %cabfs-write-byte (path pos code)
  "Store CODE at byte offset POS, extending the file as needed.
   The stream layer writes one byte per call, so this is O(n) per byte on a
   naive copy.  Kept simple deliberately: correctness first, and a
   write-behind buffer is a localised change once something measures slow."
  (let* ((old (%cabfs-bytes path))
         (len (length old))
         (n   (max len (+ pos 1)))
         (new (make-array n :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (replace new old)
    (setf (aref new pos) code)
    (%cabfs-put path new)
    1))

(defun %cabfs-dispatch (op &rest args)
  "The whole protocol cl-fileio's %CAB speaks."
  (let ((a (car args)) (b (cadr args)) (c (caddr args)))
    (cond
      ((eq op :exists) (%cabfs-exists a))
      ((eq op :read)   (%cabfs-bytes a))
      ((eq op :size)   (if (%cabfs-exists a) (length (%cabfs-bytes a)) -1))
      ((eq op :create) (progn (%cabfs-ensure-parent a)
                              (handler-case (funcall *cf-create* *cabfs* a)
                                (error () nil))
                              0))
      ((eq op :write-byte) (%cabfs-write-byte a b c))
      ((eq op :mkdir)  (handler-case (progn (funcall *cf-mkdir* *cabfs* a) 0)
                         (error () 0)))
      ((eq op :unlink) (handler-case (progn (funcall *cf-unlink* *cabfs* a) 0)
                         (error () -1)))
      ((eq op :rename) (handler-case (progn (funcall *cf-rename* *cabfs* a b) 0)
                         (error () -1)))
      ((eq op :readdir) (handler-case (funcall *cf-readdir* *cabfs* a)
                          (error () nil)))
      ;; Cabinet keeps mtimes, but nothing on the quickload path compares
      ;; them across a reboot (the FS is in RAM), so a constant is honest
      ;; and avoids a stat round trip per PROBE-FILE.
      ((eq op :mtime)  0)
      (t (error "cabinet-fs: unknown op ~s" op)))))

(defun %cabfs-parent (path)
  "Directory part of PATH, or NIL at the root."
  (let ((i (position #\/ path :from-end t)))
    (if (and i (> i 0)) (subseq path 0 i) nil)))

(defun %cabfs-ensure-parent (path)
  (let ((p (%cabfs-parent path)))
    (when p (handler-case (funcall *cf-mkdir* *cabfs* p) (error () nil)))))

(defun cabinet-mount (&optional fs)
  "Format (or adopt) a cabinet FS and route CL file I/O to it.
   After this, OPEN / LOAD / PROBE-FILE / DIRECTORY act on cabinet."
  (%cabfs-resolve)
  (setq *cabfs* (or fs (funcall (symbol-function (%cabfs-sym "FORMAT-FS")) nil)))
  (setq *cab-call* (function %cabfs-dispatch))
  (setq *cab-fds* nil)
  *cabfs*)

(defun cabinet-unmount ()
  "Put CL file I/O back on real syscalls (hosted images only)."
  (setq *cab-call* nil)
  (setq *cab-fds* nil)
  nil)
