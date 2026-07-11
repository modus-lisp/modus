;;;; install-tarball.lisp — install a Common Lisp library from a .tar.gz.
;;;;
;;;; Pipeline:  read file bytes -> chipz gunzip -> tar-extract -> find the .asd
;;;;            -> parse its :components (recursing into :module) -> topologically
;;;;            order the .lisp files by :depends-on (defaulting to declared order
;;;;            / :serial) -> read+eval each file's forms in order.
;;;;
;;;; This is the OFFLINE install path (no network).  It does NOT implement full
;;;; ASDF; it implements the "read the .asd's :components, load files in order"
;;;; subset that suffices for a flat/modular pure-CL library like alexandria.
;;;;
;;;; Depends on: lib/tar.lisp (tar-extract, tar-bytes-to-string) and chipz
;;;; (chipz:decompress) — the caller must have loaded chipz first.
;;;;
;;;; Public API:
;;;;   (install-tarball path)           -> install the (single) system in PATH.
;;;;   (install-tarball path sysname)   -> install the named system.
;;;;   Returns the system name (string) on success.

;;; --- file slurp -------------------------------------------------------------

(defun %it-slurp-bytes (path)
  "Read all bytes of PATH into a fresh (unsigned-byte 8) vector."
  (let ((s (open path :direction :input :element-type '(unsigned-byte 8))))
    (unwind-protect
         (let* ((len (file-length s))
                (buf (make-array len :element-type '(unsigned-byte 8)))
                (n (read-sequence buf s)))
           (if (= n len) buf (%tar-slice buf 0 n)))
      (close s))))

;;; --- string helpers ---------------------------------------------------------

(defun %it-suffix-p (str suffix)
  "True if STR ends with SUFFIX."
  (let ((ls (length str)) (lf (length suffix)))
    (and (>= ls lf)
         (string= (subseq str (- ls lf) ls) suffix))))

(defun %it-basename (path)
  "Last /-separated component of PATH."
  (let ((slash (position #\/ path :from-end t)))
    (if slash (subseq path (+ slash 1) (length path)) path)))

(defun %it-string-designator (x)
  "Coerce a component-name designator (string or symbol) to a string."
  (cond ((stringp x) x)
        ((symbolp x) (symbol-name x))
        (t (princ-to-string x))))

;;; --- reading forms from an in-memory source string --------------------------

(defun %it-read-forms (source-string)
  "Read every top-level form from SOURCE-STRING; return them in a list."
  (let ((s (make-string-input-stream source-string))
        (eof (list 'eof))
        (acc nil))
    (loop
      (let ((form (read s nil eof)))
        (when (eq form eof) (return))
        (setq acc (cons form acc))))
    (nreverse acc)))

(defun %it-eval-source (source-string tag)
  "Read+eval every top-level form in SOURCE-STRING (a .lisp file's text).
   Each form is eval'd; an error in one form is reported but does not abort
   the whole file (mirrors the proven form-by-form load path)."
  (let ((forms (%it-read-forms source-string))
        (count 0))
    (dolist (form forms)
      (handler-case
          (progn (eval form) (setq count (+ count 1)))
        (t (c)
          (write-string-serial "  !! form eval error in ")
          (write-string-serial tag) (write-string-serial ": ")
          (handler-case (write-object c) (t (c2) (write-string-serial "<err>")))
          (terpri))))
    count))

;;; --- .asd parsing -----------------------------------------------------------
;;;
;;; A component is one of:
;;;   (:file NAME . plist)            -> a source file NAME.lisp
;;;   (:module NAME :components (...) . plist)
;;;   (:static-file NAME . plist)     -> ignored (data file)
;;; We flatten the tree into an ordered list of (RELATIVE-PATH . DEPENDS-LIST)
;;; where RELATIVE-PATH is the module-prefixed base name (no extension) and
;;; DEPENDS-LIST is the module-local :depends-on names.

(defstruct it-file
  path        ; module-prefixed base name, e.g. "alexandria-1/package"
  name        ; local base name, e.g. "package"
  deps)       ; list of local dependency base names

(defun %it-plist-get (plist key)
  "Fetch KEY from a component plist (the cddr of a component form)."
  (let ((cur plist))
    (loop
      (when (null cur) (return nil))
      (when (eq (car cur) key) (return (cadr cur)))
      (setq cur (cddr cur)))))

(defun %it-collect-components (components prefix acc)
  "Recurse over a :components list, appending it-file structs to ACC (a
   reversed accumulator list) in declared order.  PREFIX is the current
   module path prefix (\"\" at top level)."
  (dolist (comp components)
    (when (consp comp)
      (let* ((kind (car comp))
             (name (%it-string-designator (cadr comp)))
             (plist (cddr comp)))
        (cond
          ((eq kind :file)
           (let ((deps (mapcar #'%it-string-designator
                               (%it-plist-get plist :depends-on))))
             (setq acc
                   (cons (make-it-file
                          :path (if (> (length prefix) 0)
                                    (concatenate 'string prefix name)
                                    name)
                          :name name
                          :deps deps)
                         acc))))
          ((eq kind :module)
           (let ((sub (%it-plist-get plist :components))
                 (new-prefix (concatenate 'string prefix name "/")))
             (setq acc (%it-collect-components sub new-prefix acc))))
          ;; :static-file and anything else: ignore.
          (t nil)))))
  acc)

(defun %it-find-defsystem (asd-forms sysname)
  "From the forms of an .asd, return the (defsystem ...) form for SYSNAME,
   or the FIRST defsystem if SYSNAME is NIL."
  (let ((first-ds nil))
    (dolist (form asd-forms)
      (when (and (consp form)
                 (symbolp (car form))
                 (string= (symbol-name (car form)) "DEFSYSTEM"))
        (let ((this-name (%it-string-designator (cadr form))))
          (when (null first-ds) (setq first-ds form))
          (when (and sysname (string= this-name sysname))
            (return-from %it-find-defsystem form)))))
    first-ds))

;;; --- topological sort by module-local deps ----------------------------------
;;;
;;; A file's :depends-on names are LOCAL to the file's own module.  Two modules
;;; can each declare a file with the same local name (alexandria has
;;; alexandria-1/package AND alexandria-2/package), so dedup / dependency
;;; resolution MUST key on the full PATH, and a dep name must resolve to a file
;;; in the SAME module (same path prefix), not the first same-named file found.

(defun %it-module-prefix (path)
  "The module-directory prefix of PATH (everything up to and incl. the last
   \"/\"), or \"\" for a top-level file."
  (let ((slash (position #\/ path :from-end t)))
    (if slash (subseq path 0 (+ slash 1)) "")))

(defun %it-file-by-dep (files prefix name)
  "Resolve dependency NAME (module-local) to the it-file at PREFIX+NAME."
  (let ((want (concatenate 'string prefix name))
        (cur files) (found nil))
    (loop
      (when (or found (null cur)) (return found))
      (when (string= (it-file-path (car cur)) want) (setq found (car cur)))
      (setq cur (cdr cur)))))

(defvar *it-emitted* nil)
(defvar *it-emitted-paths* nil)
(defvar *it-visiting* nil)

(defun %it-toposort (files)
  "Return FILES ordered so every file follows its :depends-on files.  Declared
   order is preserved among independent files (stable).  Missing deps are
   ignored; a dependency cycle degrades gracefully to declared order.  Recursive
   post-order DFS keyed on PATH.  The accumulators are specials reset with SETQ
   (defvar init-thunks don't run at boot in Modus, and dynamic LET-rebinding of
   a special is avoided — this fn is not re-entrant, which is fine here)."
  (setq *it-emitted* nil)
  (setq *it-emitted-paths* nil)
  (setq *it-visiting* nil)
  (dolist (root files)
    (%it-visit root files))
  (nreverse *it-emitted*))

(defun %it-visit (f files)
  "Post-order DFS visit of file F, emitting its deps first."
  (let ((path (it-file-path f)))
    (unless (or (member path *it-emitted-paths* :test #'string=)
                (member path *it-visiting* :test #'string=))
      (setq *it-visiting* (cons path *it-visiting*))
      (let ((prefix (%it-module-prefix path)))
        (dolist (dname (it-file-deps f))
          (let ((df (%it-file-by-dep files prefix dname)))
            (when df (%it-visit df files)))))
      (setq *it-visiting* (%it-remove-str path *it-visiting*))
      (setq *it-emitted* (cons f *it-emitted*))
      (setq *it-emitted-paths* (cons path *it-emitted-paths*)))))

(defun %it-remove-str (s list)
  "Remove the first occurrence of string S from LIST (string= compare)."
  (let ((acc nil) (cur list) (removed nil))
    (loop
      (when (null cur) (return (nreverse acc)))
      (if (and (not removed) (string= (car cur) s))
          (setq removed t)
          (setq acc (cons (car cur) acc)))
      (setq cur (cdr cur)))))

;;; --- driver -----------------------------------------------------------------

(defun install-tarball (path &optional sysname)
  "Install a Common Lisp system from the .tar.gz at PATH.  If SYSNAME is given,
   install that system; otherwise install the first defsystem found in the
   first .asd.  Returns the installed system name (string)."
  (write-string-serial "install-tarball: ") (write-string-serial path) (terpri)
  ;; 1. read + gunzip
  (let* ((gz (%it-slurp-bytes path))
         (tarbytes (if (and (>= (length gz) 2)
                            (= (aref gz 0) 31) (= (aref gz 1) 139))
                       (chipz:decompress nil 'chipz:gzip gz)
                       gz)))              ; allow a plain .tar too
    (write-string-serial "  gunzipped to ") (print-dec (length tarbytes))
    (write-string-serial " bytes") (terpri)
    ;; 2. untar
    (let ((entries (tar-extract tarbytes)))
      (write-string-serial "  extracted ") (print-dec (length entries))
      (write-string-serial " files") (terpri)
      ;; 3. locate the .asd (prefer one matching SYSNAME)
      (let ((asd-entry nil))
        (dolist (e entries)
          (let ((base (%it-basename (car e))))
            (when (%it-suffix-p base ".asd")
              (when (or (null asd-entry)
                        (and sysname
                             (string= base (concatenate 'string sysname ".asd"))))
                (setq asd-entry e)))))
        (when (null asd-entry)
          (error "install-tarball: no .asd file found in archive"))
        (write-string-serial "  using asd: ") (write-string-serial (car asd-entry)) (terpri)
        ;; The .asd lives in a directory; source files are relative to that dir.
        (let* ((asd-path (car asd-entry))
               (asd-dir (let ((slash (position #\/ asd-path :from-end t)))
                          (if slash (subseq asd-path 0 (+ slash 1)) "")))
               (asd-src (tar-bytes-to-string (cdr asd-entry)))
               (asd-forms (%it-read-forms asd-src))
               (ds (%it-find-defsystem asd-forms sysname)))
          (when (null ds)
            (error "install-tarball: no defsystem found in asd"))
          (let* ((this-sysname (%it-string-designator (cadr ds)))
                 (comps (%it-plist-get (cddr ds) :components))
                 (files (nreverse (%it-collect-components comps "" nil)))
                 (ordered (%it-toposort files)))
            (write-string-serial "  system: ") (write-string-serial this-sysname) (terpri)
            (write-string-serial "  load order:") (terpri)
            ;; 4. load each file's source, in order
            (dolist (f ordered)
              (let* ((rel (concatenate 'string asd-dir (it-file-path f) ".lisp"))
                     (ent (assoc rel entries :test #'string=)))
                (cond
                  ((null ent)
                   (write-string-serial "    (missing: ")
                   (write-string-serial rel) (write-string-serial ")") (terpri))
                  (t
                   (write-string-serial "    ") (write-string-serial rel) (terpri)
                   (%it-eval-source (tar-bytes-to-string (cdr ent))
                                    (it-file-path f))))))
            (write-string-serial "install-tarball: done, system=")
            (write-string-serial this-sysname) (terpri)
            this-sysname))))))
