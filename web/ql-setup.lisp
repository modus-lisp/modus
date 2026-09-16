;;;; modus-quicklisp/setup.lisp — the loadable "install quicklisp" step.
;;;;
;;;; This is Modus's analogue of the real Quicklisp's quicklisp/setup.lisp: the
;;;; file your ~/.modusrc (~/.sbclrc analogue) (load)s to bring `ql:quickload'
;;;; into a running image.  Stock `./modus' ships WITHOUT any `ql' symbol —
;;;; exactly like stock SBCL has no `ql' until Quicklisp is installed.  You opt
;;;; in:
;;;;
;;;;   ./modus
;;;;   > (load "modus-quicklisp/setup.lisp")
;;;;   > (ql:quickload :sha1)
;;;;   > (sha1:sha1-hex "abc")   ; => "A9993E364706816ABA3E25717850C26C9CD0D89D"
;;;;
;;;; or, automatically on every interactive start, put in ~/.modusrc:
;;;;
;;;;   (load "/abs/path/to/modus/modus-quicklisp/setup.lisp")
;;;;
;;;; What it does, at RUNTIME, using ONLY the base CL runtime + mvm-eval that
;;;; build-generic-cli ships:
;;;;
;;;;   1. Defines the QL package + ql:quickload over install-tarball.
;;;;   2. Points *ql-systems-dir* at the bundled systems/ directory.
;;;;
;;;; The untar->parse-.asd->topo-sort->eval PIPELINE (lib/tar.lisp +
;;;; lib/install-tarball.lisp) is baked into build-generic-cli as a GENERAL
;;;; library primitive (install-tarball / tar-extract / ...), NOT as ql.  It is
;;;; baked rather than runtime-(load)ed because %tar-slice's `(make-array LEN)`
;;;; with a variable size hits a pre-existing mvm-eval bug (a variable-size
;;;; make-array returns a half-length array under the interpreter, truncating
;;;; any >512-byte tar entry so its source won't READ); the native build
;;;; compiles it correctly.  See mvm/build-generic-cli.lisp's *bridge-source*
;;;; note.  Either way, the QL package + quickload live ONLY in THIS loaded
;;;; setup — never in the binary.
;;;;
;;;; Paths are resolved relative to *modus-quicklisp-root* (default "" = the
;;;; process cwd, i.e. run ./modus from the repo root).  If you load setup from
;;;; elsewhere, first (setq *modus-quicklisp-root* "/abs/path/to/modus/").

;;; --- where the repo lives ---------------------------------------------------
;;; No *load-pathname* in the runtime loader, so setup can't self-locate; a
;;; single root prefix (default cwd) makes it explicit and overridable.

(defvar *modus-quicklisp-root* "")

(defun %mql-path (rel)
  "REL resolved against *modus-quicklisp-root* (a dir ending in / or \"\")."
  (concatenate 'string *modus-quicklisp-root* rel))

;;; --- the QL package + ql:quickload (moved from lib/ql-shim.lisp) ------------

(defvar *ql-systems-dir* "systems/")

(defun ql-set-systems-dir (dir)
  "Set the directory quickload looks in for <name>.tar archives.  DIR should
   end with a trailing slash."
  (setq *ql-systems-dir* dir))

(defun %ql-name-string (name)
  "Coerce a system designator (keyword / string / symbol) to a lowercase
   system-name string.  :sha1 -> \"sha1\", \"sha1\" -> \"sha1\"."
  (cond ((stringp name) name)
        ((symbolp name) (string-downcase (symbol-name name)))
        (t (princ-to-string name))))

(defun %ql-tar-path (nm)
  "The bundled tarball path for system NM: <systems-dir>/<nm>.tar."
  (concatenate 'string *ql-systems-dir* nm ".tar"))

(defun %ql-quickload (name)
  "Load the system designated by NAME from the bundled systems/ directory.
   NAME may be a keyword (:sha1), string (\"sha1\"), or symbol.  Returns the
   loaded system name (string).  Signals an error if the tarball is missing.
   Runs on the baked install-tarball / tar-extract pipeline."
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

(defun %ql-init ()
  "Create the QL package, export QUICKLOAD, and bind QL:QUICKLOAD's function
   cell to %ql-quickload.  After setup runs, a REPL/script can name
   `ql:quickload'."
  (handler-case
      (progn
        (unless (find-package "QL")
          (make-package "QL" :use (list "CL")))
        (let ((sym (intern "QUICKLOAD" "QL")))
          (export sym "QL")
          (setf (symbol-function sym) (function %ql-quickload))))
    (t (c)
      (write-string-serial "; %ql-init failed") (write-char-serial 10))))

;;; --- driver: run the whole setup at (load) time -----------------------------
;;; *ql-systems-dir* points at the bundled systems/ under the repo root.

(setq *ql-systems-dir* (%mql-path "systems/"))
(%ql-init)
(write-string-serial "; modus-quicklisp: ql:quickload ready (systems dir ")
(write-string-serial *ql-systems-dir*)
(write-string-serial ")")
(write-char-serial 10)
