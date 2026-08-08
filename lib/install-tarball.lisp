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

(defun %it-last-slash (path)
  "Index of the last #\/ in PATH, or NIL.  Manual scan — the compiled
   POSITION with :from-end t wedged on the bare-metal image."
  (let ((i 0) (n (length path)) (last nil))
    (loop
      (when (>= i n) (return last))
      (when (char= (char path i) #\/) (setq last i))
      (setq i (+ i 1)))))

(defun %it-basename (path)
  "Last /-separated component of PATH."
  (let ((slash (%it-last-slash path)))
    (if slash (subseq path (+ slash 1) (length path)) path)))

(defun %it-string-designator (x)
  "Coerce a component-name designator (string or symbol) to a string."
  (cond ((stringp x) x)
        ((symbolp x) (symbol-name x))
        (t (princ-to-string x))))

;;; --- reading forms from an in-memory source string --------------------------

(defun %it-read-forms (source-string)
  "Read every top-level form from SOURCE-STRING; return them in a list.
   Persistent string-input stream + READ (the standard loader path).  The
   :start keyword variant of READ-FROM-STRING wedged the bare-metal image;
   a single stream read repeatedly reads cleanly."
  (let ((s (make-string-input-stream source-string))
        (eof (list 'eof))
        (acc nil))
    (loop
      (let ((form (read s nil eof)))
        (when (eq form eof) (return))
        (setq acc (cons form acc))))
    (nreverse acc)))

(defun %it-eval-source (source-string tag)
  "Read+eval every top-level form of SOURCE-STRING with *PACKAGE* and
   *READTABLE* BOUND, per CLHS 24.2 (LOAD): \"load binds *readtable* and
   *package* to the values they held before loading the file\".  A loaded
   file's `(in-package :foo)` must NOT escape to the caller.

   THIS IS A REAL, ESCAPE-SAFE BIND, deliberately hand-rolled:
   `(let ((*package* ...)) ...)` compiles via COMPILE-LET-WITH-SPECIALS,
   which emits save / set / body / restore with NO unwind-protect — a
   throw or an escaping error out of the body SKIPS the restore (probe
   /home/claude/ws-loader/probes/p1.lisp, P4: *package* stays KEYWORD after
   a throw).  Same class of defect as the %WITH-HANDLER-BIND leak fixed in
   279f2cc; same remedy: lexical save + UNWIND-PROTECT + setq restore,
   which is verified escape-safe for throw, ERROR and UNDEFINED-FUNCTION
   (probes p2.lisp P5-P10).

   Before this, install-tarball loaded alexandria, whose last file does
   `(in-package :alexandria-2)`, and returned with *PACKAGE* still
   ALEXANDRIA-2 — so the CALLER's next form read `LF-GCN` as
   ALEXANDRIA-2::LF-GCN and died UNDEFINED-FUNCTION."
  (let ((saved-package *package*)
        (saved-readtable *readtable*))
    (unwind-protect
         (%it-eval-source-1 source-string tag)
      (setq *package* saved-package)
      (setq *readtable* saved-readtable))))

(defun %it-eval-source-1 (source-string tag)
  "The read+eval loop itself (see %it-eval-source for the binding contract).
   INTERLEAVES read and eval one form at a time from a single stream — this
   is essential: a leading `(in-package :foo)` must take effect (it side-
   effects *package* at eval time) BEFORE the file's remaining forms are READ,
   or those symbols intern in the wrong package.  Reading the whole file up
   front (the old path) left symbols split across packages across files.  An
   error in one form is reported but does not abort the whole file."
  (let ((s (make-string-input-stream source-string))
        (eof (list 'eof))
        (count 0))
    (loop
      (let ((form (handler-case (read s nil eof)
                    (t (c)
                      (write-string-serial "  !! read error in ")
                      (write-string-serial tag) (write-string-serial ": ")
                      (handler-case (write-object c) (t (c2) (write-string-serial "<err>")))
                      (write-char-serial 10)
                      eof))))     ; a read error ends the file
        (when (eq form eof) (return count))
        (handler-case
            (progn (eval form) (setq count (+ count 1)))
          (t (c)
            (write-string-serial "  !! form eval error in ")
            (write-string-serial tag) (write-string-serial ": ")
            (handler-case (write-object c) (t (c2) (write-string-serial "<err>")))
            (write-char-serial 10)))))))

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

(defun %it-feature-name-p (x)
  "True when the feature-expression atom X is currently on *FEATURES*.
   Compared by NAME, not identity: an .asd read in CL-USER interns
   `:sbcl' as a keyword, and Modus's *FEATURES* holds keywords too, but
   a designator may also arrive as an ordinary symbol."
  (let ((want (%it-string-designator x)) (found nil))
    (dolist (f *features*)
      (when (and (not found)
                 (symbolp f)
                 (string= (%it-string-designator f) want))
        (setq found t)))
    found))

(defun %it-feature-true-p (expr)
  "Evaluate an ASDF :IF-FEATURE feature expression against *FEATURES*.
   Grammar (ASDF 3 / CLHS 24.1.2.1 shape): an atom is a feature name, and
   (:AND e*) / (:OR e*) / (:NOT e) combine.  NIL means \"no :if-feature\"
   -> always true (the caller passes the plist value, absent = NIL)."
  (cond
    ((null expr) t)
    ((consp expr)
     (let ((op (car expr)) (args (cdr expr)))
       (cond
         ((and (symbolp op) (string= (%it-string-designator op) "AND"))
          (let ((r t))
            (dolist (a args) (unless (%it-feature-true-p a) (setq r nil)))
            r))
         ((and (symbolp op) (string= (%it-string-designator op) "OR"))
          (let ((r nil))
            (dolist (a args) (when (%it-feature-true-p a) (setq r t)))
            r))
         ((and (symbolp op) (string= (%it-string-designator op) "NOT"))
          (not (%it-feature-true-p (car args))))
         ;; Unknown operator: be conservative and INCLUDE the component.
         (t t))))
    (t (%it-feature-name-p expr))))

(defun %it-dir-prefix (s)
  "Normalise a component :PATHNAME designator to a directory prefix ending
   in exactly one \"/\" (\"\" for an empty/NIL designator)."
  (cond
    ((null s) "")
    (t (let ((str (%it-string-designator s)))
         (cond ((= (length str) 0) "")
               ((char= (char str (- (length str) 1)) #\/) str)
               (t (concatenate 'string str "/")))))))

(defun %it-collect-components (components prefix acc)
  "Recurse over a :components list, appending it-file structs to ACC (a
   reversed accumulator list) in declared order.  PREFIX is the current
   module path prefix (\"\" at top level).

   Honours two ASDF component options that the ladder's real (install-
   tarball) path cannot be measured without:

     :IF-FEATURE  — ASDF omits the component entirely when the feature
                    expression is false.  Without this we unconditionally
                    loaded every per-implementation file: bordeaux-threads
                    apiv1 alone has 16 mutually exclusive impl-* files
                    (impl-clisp, impl-allegro, …) and trivial-features has
                    12 tf-* files.  Loading them all is not \"a stricter
                    test\", it is loading code for a different Lisp.

     :PATHNAME    — the directory a module (or the system) actually lives
                    in, which is NOT always the module NAME: bordeaux-
                    threads' module \"api-v1\" has :pathname \"apiv1/\",
                    and named-readtables' whole system has :pathname
                    \"src\".  Without it every component resolved to a
                    path that is not in the tarball and the install
                    reported \"(missing: …)\" for all of them."
  (dolist (comp components)
    (when (and (consp comp)
               (%it-feature-true-p (%it-plist-get (cddr comp) :if-feature)))
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
           (let* ((sub (%it-plist-get plist :components))
                  (pn (%it-plist-get plist :pathname))
                  (dir (if pn (%it-dir-prefix pn)
                           (concatenate 'string name "/")))
                  (new-prefix (concatenate 'string prefix dir)))
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
  (let ((slash (%it-last-slash path)))
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

(defun install-tarball-from-bytes (gz &optional sysname)
  "Install a Common Lisp system from an in-memory .tar.gz byte vector GZ
   (an (unsigned-byte 8) vector — e.g. the body returned by HTTP-FETCH).
   Same pipeline as INSTALL-TARBALL but with no file read, so it works on
   bare metal where the fetched archive lives only in RAM.  If SYSNAME is
   given, install that system; otherwise install the first defsystem found.
   Returns the installed system name (string)."
  (write-string-serial "install-tarball-from-bytes: ") (print-dec (length gz))
  (write-string-serial " bytes") (write-char-serial 10)
  (%it-install-from-gz-bytes gz sysname))

(defun install-tarball (path &optional sysname)
  "Install a Common Lisp system from the .tar.gz at PATH.  If SYSNAME is given,
   install that system; otherwise install the first defsystem found in the
   first .asd.  Returns the installed system name (string)."
  (write-string-serial "install-tarball: ") (write-string-serial path) (write-char-serial 10)
  (%it-install-from-gz-bytes (%it-slurp-bytes path) sysname))

(defun %it-install-from-gz-bytes (gz sysname)
  "Install a system, with *PACKAGE* / *READTABLE* bound around the WHOLE
   install (outer belt to %it-eval-source's per-file braces).  ASDF's
   LOAD-SYSTEM does not change its caller's *PACKAGE* either, and the .asd
   itself is READ here — so a .asd that switches packages, or an abort
   between two component files, must not leak out.  Escape-safe by
   construction (lexical save + unwind-protect + setq), NOT (let ((*package*
   ...))) — see %it-eval-source's docstring for why that is not enough."
  (let ((saved-package *package*)
        (saved-readtable *readtable*))
    (unwind-protect
         (%it-install-from-gz-bytes-1 gz sysname)
      (setq *package* saved-package)
      (setq *readtable* saved-readtable))))

(defun %it-install-from-gz-bytes-1 (gz sysname)
  "Core installer: GZ is a .tar.gz (or plain .tar) byte vector already in
   memory.  gunzip -> untar -> parse .asd -> load files in order."
  ;; 1. gunzip
  (let* ((gzp (and (>= (length gz) 2) (= (aref gz 0) 31) (= (aref gz 1) 139)))
         (tarbytes (if gzp
                       (chipz:decompress nil 'chipz:gzip gz)
                       gz)))              ; allow a plain .tar too
    (write-string-serial "  gunzipped to ") (print-dec (length tarbytes))
    (write-string-serial " bytes") (write-char-serial 10)
    ;; 2. untar
    (let ((entries (tar-extract tarbytes)))
      (write-string-serial "  extracted ") (print-dec (length entries))
      (write-string-serial " files") (write-char-serial 10)
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
        (write-string-serial "  using asd: ") (write-string-serial (car asd-entry)) (write-char-serial 10)
        ;; The .asd lives in a directory; source files are relative to that dir.
        (let* ((asd-path (car asd-entry))
               (asd-dir (let ((slash (%it-last-slash asd-path)))
                          (if slash (subseq asd-path 0 (+ slash 1)) "")))
               (asd-src (tar-bytes-to-string (cdr asd-entry)))
               (asd-forms (%it-read-forms asd-src))
               (ds (%it-find-defsystem asd-forms sysname)))
          (when (null ds)
            (error "install-tarball: no defsystem found in asd"))
          (let* ((this-sysname (%it-string-designator (cadr ds)))
                 (comps (%it-plist-get (cddr ds) :components))
                 ;; System-level :PATHNAME — the subdirectory the components
                 ;; live in relative to the .asd.  named-readtables declares
                 ;; :pathname "src"; without honouring it every component
                 ;; resolved to <asd-dir>/package.lisp, which is not in the
                 ;; tarball, and the whole system loaded as 6 "(missing: …)".
                 (sys-dir (concatenate 'string asd-dir
                                       (%it-dir-prefix
                                        (%it-plist-get (cddr ds) :pathname))))
                 (files (nreverse (%it-collect-components comps "" nil)))
                 (ordered (%it-toposort files)))
            (write-string-serial "  system: ") (write-string-serial this-sysname) (write-char-serial 10)
            (write-string-serial "  load order:") (write-char-serial 10)
            ;; 4. load each file's source, in order
            (dolist (f ordered)
              (let* ((rel (concatenate 'string sys-dir (it-file-path f) ".lisp"))
                     (ent (assoc rel entries :test #'string=)))
                (cond
                  ((null ent)
                   (write-string-serial "    (missing: ")
                   (write-string-serial rel) (write-string-serial ")") (write-char-serial 10))
                  (t
                   (write-string-serial "    ") (write-string-serial rel) (write-char-serial 10)
                   (%it-eval-source (tar-bytes-to-string (cdr ent))
                                    (it-file-path f))))))
            (write-string-serial "install-tarball: done, system=")
            (write-string-serial this-sysname) (write-char-serial 10)
            this-sysname))))))
