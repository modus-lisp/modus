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

(defvar *cabfs-debug* nil)

(defun %cabfs-put (path bytes)
  (%cabfs-rcache-drop path)
  (unless (%cabfs-exists path)
    (handler-case (funcall *cf-create* *cabfs* path)
      (error (c)
        (when *cabfs-debug*
          (format t "~&[cabfs] create ~a failed: ~s ~a~%" path (type-of c)
                  (handler-case (format nil "~a" c) (error () "?")))
          (finish-output)))))
  (funcall *cf-write* *cabfs* path bytes))

;;; Write-behind cache.  The stream layer writes ONE BYTE per %CAB call, and
;;; the naive implementation re-read + re-wrote the whole file each time —
;;; O(n^2) per file, unusable for quicklisp's 568KB releases.txt.  Writes now
;;; land in a per-path (vector . len) cell and flush to cabinet lazily: on any
;;; read/size/exists of that path, and wholesale on readdir/rename/unlink.
;;; There is no :close op in the seam protocol, so the read-side flush is the
;;; correctness anchor (write file -> close -> LOAD it reads through :read).

(defvar *cf-wcache* nil)          ; alist (path . (bytes-vector . len))

;;; Read cache — ONE entry.  The stream layer refills through %CAB :read,
;;; and %cabfs-bytes re-materializes the ENTIRE file from cabinet on every
;;; call: a 276KB tar read byte-wise churned ~20-50 COLLECTIONS per untar
;;; (measured, #282).  Serving repeat reads of the same path from a cached
;;; copy removes that churn.  Invalidation: any write/create/flush of the
;;; path, and wholesale on unlink/rename.

(defvar *cf-rcache-path* nil)
(defvar *cf-rcache-bytes* nil)

(defun %cabfs-rcache-drop (path)
  "Invalidate the read cache: for PATH, or entirely when PATH is NIL."
  (when (and *cf-rcache-path*
             (or (null path) (equal path *cf-rcache-path*)))
    (setq *cf-rcache-path* nil)
    (setq *cf-rcache-bytes* nil)))

(defun %cabfs-bytes-cached (path)
  (if (and *cf-rcache-path* (equal path *cf-rcache-path*))
      *cf-rcache-bytes*
      (let ((b (%cabfs-bytes path)))
        (setq *cf-rcache-path* path)
        (setq *cf-rcache-bytes* b)
        b)))

(defun %cabfs-wc-cell (path)
  "The write cache cell for PATH, creating it (seeded from the existing file
   contents) on first use."
  (let ((e (assoc path *cf-wcache* :test (function equal))))
    (if e (cdr e)
        (let* ((old (if (%cabfs-exists path) (%cabfs-bytes path) nil))
               (n   (if old (length old) 0))
               (cap (max 256 (* 2 n)))
               (v   (make-array cap :element-type '(unsigned-byte 8)
                                    :initial-element 0)))
          (when old (replace v old))
          (let ((cell (cons v n)))
            (setq *cf-wcache* (cons (cons path cell) *cf-wcache*))
            cell)))))

(defun %cabfs-flush-path (path)
  (let ((e (assoc path *cf-wcache* :test (function equal))))
    (when e
      (let* ((cell (cdr e))
             (out (subseq (car cell) 0 (cdr cell))))
        (%cabfs-put path out))
      (setq *cf-wcache* (remove e *cf-wcache*))
      t)))

(defun %cabfs-flush-all ()
  (let ((paths (mapcar (function car) *cf-wcache*)))
    (dolist (p paths) (%cabfs-flush-path p))))

(defun %cabfs-write-byte (path pos code)
  "Store CODE at byte offset POS in the write cache, growing as needed."
  (%cabfs-rcache-drop path)
  (let* ((cell (%cabfs-wc-cell path))
         (v (car cell)))
    (when (>= pos (length v))
      (let ((nv (make-array (max 256 (* 2 (+ pos 1)))
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
        (replace nv v)
        (setf (car cell) nv)
        (setq v nv)))
    (setf (aref v pos) code)
    (when (>= pos (cdr cell)) (setf (cdr cell) (+ pos 1)))
    1))

(defun %cabfs-dispatch (op &rest args)
  "The whole protocol cl-fileio's %CAB speaks."
  (let ((a (car args)) (b (cadr args)) (c (caddr args)))
    (cond
      ((eq op :exists) (progn (%cabfs-flush-path a) (%cabfs-exists a)))
      ((eq op :read)   (progn (%cabfs-flush-path a) (%cabfs-bytes-cached a)))
      ((eq op :size)   (progn (%cabfs-flush-path a)
                              (if (%cabfs-exists a)
                                  (length (%cabfs-bytes a)) -1)))
      ((eq op :create) (progn (%cabfs-ensure-parent a)
                              ;; a fresh :create truncates — drop any stale cache
                              (let ((e (assoc a *cf-wcache*
                                              :test (function equal))))
                                (when e (setq *cf-wcache* (remove e *cf-wcache*))))
                              (handler-case (funcall *cf-create* *cabfs* a)
                                (error (c)
                                  (when *cabfs-debug*
                                    (format t "~&[cabfs] :create ~a failed: ~s~%"
                                            a (type-of c))
                                    (finish-output))))
                              0))
      ((eq op :write-byte) (%cabfs-write-byte a b c))
      ((eq op :mkdir)  (handler-case (progn (funcall *cf-mkdir* *cabfs* a) 0)
                         (error () 0)))
      ((eq op :unlink) (progn (%cabfs-flush-all) (%cabfs-rcache-drop nil)
                              (handler-case
                                  (progn (funcall *cf-unlink* *cabfs* a) 0)
                                (error () -1))))
      ((eq op :rename) (progn (%cabfs-flush-all) (%cabfs-rcache-drop nil)
                              (handler-case
                                  (progn (funcall *cf-rename* *cabfs* a b) 0)
                                (error () -1))))
      ((eq op :readdir) (progn (%cabfs-flush-all)
                               (handler-case (funcall *cf-readdir* *cabfs* a)
                                 (error () nil))))
      ;; Cabinet keeps mtimes, but nothing on the quickload path compares
      ;; them across a reboot (the FS is in RAM), so a constant is honest
      ;; and avoids a stat round trip per PROBE-FILE.
      ((eq op :mtime)  0)
      (t (error "cabinet-fs: unknown op type=~s name=~s args0=~s"
                (type-of op)
                (handler-case (symbol-name op) (error () :NOT-SYM))
                (handler-case (if (stringp a) a (type-of a)) (error () :?)))))))

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
