;;;; ql-shim.lisp — a minimal `(ql:quickload ...)` for Modus on x64-linux.
;;;;
;;;; This provides just enough of the Quicklisp client surface to load a
;;;; small, dependency-free pure-CL library FROM A LOCAL TARBALL BUNDLE:
;;;;
;;;;   (ql:quickload :sha1)          ; loads systems/sha1.tar
;;;;   (sha1:sha1-hex "abc")         ; => "A9993E36...9CD0D89D"
;;;;
;;;; Pipeline (all offline, no network):
;;;;   name (keyword/string/symbol)
;;;;     -> resolve to  <systems-dir>/<name>.tar
;;;;     -> read the tarball bytes
;;;;     -> install-tarball  (untar -> parse .asd -> topo-sort -> eval each form)
;;;;     -> return the loaded system name (string)
;;;;
;;;; System location for v1 is OFFLINE: a bundled `systems/` directory of
;;;; plain .tar archives (one per system).  Network fetch (net/http-client)
;;;; is a documented follow-up, not implemented here.
;;;;
;;;; The QL package + the quickload fdefinition are established at boot in
;;;; kernel-main (see %ql-init) — the package MUST exist before the reader
;;;; sees a `ql:...` token, so it cannot be a plain defpackage form loaded
;;;; lazily.  install-tarball / install-tarball-from-bytes are compiled into
;;;; the image alongside lib/tar.lisp; this file only adds the client surface.
;;;;
;;;; Depends on: lib/tar.lisp, lib/install-tarball.lisp (both baked into the
;;;; image), and the file-stream + reader + eval runtime.

;;; --- systems directory ------------------------------------------------------
;;; Configurable via the *QL-SYSTEMS-DIR* global; the build seeds it and
;;; kernel-main can override from an env var / argv if desired.

(defvar *ql-systems-dir* "systems/")

(defun ql-set-systems-dir (dir)
  "Set the directory quickload looks in for <name>.tar archives.  DIR should
   end with a trailing slash."
  (setq *ql-systems-dir* dir))

;;; --- name resolution --------------------------------------------------------

(defun %ql-name-string (name)
  "Coerce a system designator (keyword / string / symbol) to a lowercase
   system-name string.  :sha1 -> \"sha1\", \"sha1\" -> \"sha1\"."
  (cond ((stringp name) name)
        ((symbolp name) (string-downcase (symbol-name name)))
        (t (princ-to-string name))))

(defun %ql-tar-path (nm)
  "The bundled tarball path for system NM: <systems-dir>/<nm>.tar."
  (concatenate 'string *ql-systems-dir* nm ".tar"))

;;; --- the client entry point -------------------------------------------------
;;; The public name is QL:QUICKLOAD; %ql-quickload is the real body (defun on a
;;; bare name so the scan-defuns sft registration + last-defun-wins work
;;; without needing the QL package to exist at build-time read).  kernel-main's
;;; %ql-init points the interned QL:QUICKLOAD symbol at this function.

(defun %ql-quickload (name)
  "Load the system designated by NAME from the bundled systems/ directory.
   NAME may be a keyword (:sha1), string (\"sha1\"), or symbol.  Returns the
   loaded system name (string).  Signals an error if the tarball is missing."
  (let* ((nm (%ql-name-string name))
         (path (%ql-tar-path nm)))
    (write-string-serial "; loading ") (write-string-serial nm)
    (write-string-serial " from ") (write-string-serial path) (write-char-serial 10)
    (if (%sys-stat-exists path)
        (progn (install-tarball path nm) nm)
        (progn
          (write-string-serial "; ql:quickload: no tarball at ")
          (write-string-serial path) (write-char-serial 10)
          (error "ql:quickload: system not found")))))

;;; --- boot-time package install ----------------------------------------------

(defun %ql-init ()
  "Create the QL package, export QUICKLOAD, and bind QL:QUICKLOAD's function
   cell to %ql-quickload.  Runs from kernel-main BEFORE the REPL starts so the
   reader can resolve `ql:quickload' the first time a user types it."
  (handler-case
      (progn
        (make-package "QL" :use (list "CL"))
        (let ((sym (intern "QUICKLOAD" "QL")))
          (export sym "QL")
          (setf (symbol-function sym) (function %ql-quickload))))
    (t (c)
      (write-string-serial "; %ql-init failed") (write-char-serial 10))))

;;; --- the stdin REPL ---------------------------------------------------------
;;; A read/eval/print loop over fd 0 via a CHARACTER file stream (the proven
;;; %make-file-stream-full + buffered %sys-read-raw path).  `read' handles full
;;; form parsing and EOF; each value is printed with write-object.  eval is the
;;; production evaluator (eval2 = compile->MVM bytecode->interpret).

(defun %ql-repl ()
  "Interactive read-eval-print loop reading forms from standard input."
  (write-string-serial "Modus REPL (x64-linux).  Ctrl-D to exit.") (write-char-serial 10)
  (let ((in (%make-file-stream-full 0 0))
        (eof (list 'eof)))
    (loop
      (write-string-serial "> ")
      (let ((form (handler-case (read in nil eof)
                    (t (c)
                      (write-string-serial "READ-ERROR") (write-char-serial 10)
                      eof))))
        (when (eq form eof) (write-char-serial 10) (return-from %ql-repl nil))
        (handler-case
            (let ((v (eval form)))
              (write-object v) (write-char-serial 10))
          (t (c)
            (write-string-serial "ERROR: ")
            (handler-case (write-object c) (t (c2) (write-string-serial "<condition>")))
            (write-char-serial 10)))))))
