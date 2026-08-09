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

(defun %it-slurp-text (path)
  "Whole contents of PATH as a STRING.  The on-disk twin of the
   TAR-BYTES-TO-STRING call the archive path makes on a tar entry, so the
   directory-backed loader (ASDF:LOAD-SYSTEM) and the archive-backed loader
   (INSTALL-TARBALL) hand IDENTICAL text to %IT-EVAL-SOURCE."
  (tar-bytes-to-string (%it-slurp-bytes path)))

(defun %it-file-exists-p (path)
  "True when PATH can be opened for input.  OPEN-and-CLOSE rather than
   PROBE-FILE: this is the same syscall the loader is about to make anyway,
   so a file that probes but cannot be read never reaches %IT-EVAL-SOURCE."
  (handler-case
      (let ((s (open path :direction :input :element-type '(unsigned-byte 8))))
        (close s)
        t)
    (t (c) nil)))

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

(defun %it-dirname (path)
  "Directory part of PATH, INCLUDING the trailing \"/\" (\"\" if there is no
   slash).  ASDF's SYSTEM-SOURCE-DIRECTORY of a .asd is exactly this."
  (let ((slash (%it-last-slash path)))
    (if slash (subseq path 0 (+ slash 1)) "")))

;;; --- version arithmetic (UIOP's PARSE-VERSION / VERSION< / VERSION<=) -------
;;;
;;; Two ladder systems refuse to be READ without these: split-sequence and
;;; bordeaux-threads both open with
;;;   #.(unless (or #+asdf3.1 (version<= "3.1" (asdf-version)))
;;;        (error "You need ASDF >= 3.1 …"))
;;; — a READ-TIME call, so the functions must exist before their .asd can be
;;; parsed at all.  Implemented here (compiled) and exposed under their ASDF /
;;; UIOP names by net/asdf-interface.lisp.

(defun %it-parse-version (s)
  "UIOP:PARSE-VERSION — a dotted version STRING to a list of integers.
   A component that is not a run of digits yields NIL for the whole version
   (UIOP treats an unparsable version as \"no version\", and every caller
   here already guards on NIL)."
  (let ((n (length s)) (i 0) (acc nil) (cur 0) (digits 0) (bad nil))
    (loop
      (when (or bad (> i n)) (return nil))
      (if (or (= i n) (char= (char s i) #\.))
          (progn
            (if (= digits 0) (setq bad t) (setq acc (cons cur acc)))
            (setq cur 0) (setq digits 0))
          (let ((d (digit-char-p (char s i))))
            (if (null d)
                (setq bad t)
                (progn (setq cur (+ (* cur 10) d))
                       (setq digits (+ digits 1))))))
      (setq i (+ i 1)))
    (if bad nil (nreverse acc))))

(defun %it-lexicographic< (x y)
  "UIOP:LEXICOGRAPHIC< specialised to integer lists: a strict prefix is LESS."
  (cond ((null y) nil)
        ((null x) t)
        ((< (car x) (car y)) t)
        ((< (car y) (car x)) nil)
        (t (%it-lexicographic< (cdr x) (cdr y)))))

(defun %it-version< (x y)
  "UIOP:VERSION< on version STRINGS.  NIL (unparsable / absent) is never <."
  (let ((px (and x (%it-parse-version x)))
        (py (and y (%it-parse-version y))))
    (and px py (%it-lexicographic< px py))))

(defun %it-version<= (x y)
  "UIOP:VERSION<= on version STRINGS — (not (version< y x)), UIOP's own
   definition, so \"3.1\" <= \"3.3.7\" and \"3.1\" <= \"3.1\" both hold."
  (let ((px (and x (%it-parse-version x)))
        (py (and y (%it-parse-version y))))
    (and px py (not (%it-lexicographic< py px)))))

(defun %it-string-designator (x)
  "Coerce an ASDF name designator to a string, ASDF's way (COERCE-NAME):
   a STRING is taken verbatim, a SYMBOL is DOWNCASED.

   The downcase is load-bearing, not cosmetic.  trivial-features declares
   `(:module src ...)` with a bare symbol, which reads as SRC; without the
   downcase every component resolved to \"SRC/tf-genera.lisp\" and the
   tarball holds \"src/tf-genera.lisp\", so the whole module went
   \"(missing: …)\".  Same rule makes `(defsystem :ieee-floats …)` match the
   caller's \"ieee-floats\" in %IT-FIND-DEFSYSTEM."
  (cond ((stringp x) x)
        ((symbolp x) (string-downcase (symbol-name x)))
        (t (princ-to-string x))))

;;; --- reading forms from an in-memory source string --------------------------

(defun %it-read-asd-forms (source-string)
  "%IT-READ-FORMS for a SYSTEM DEFINITION, with the reader's lenient
   missing-package mode ON (*READER-MISSING-PACKAGE-LENIENT*, cl-reader.lisp:25).

   A .asd is not ordinary source: it is metadata about a system, and it
   routinely names packages that only a running ASDF provides.  Because
   %IT-READ-FORMS reads the WHOLE file before %IT-FIND-DEFSYSTEM picks a
   form, ONE unreadable qualifier anywhere in the file lost the entire
   system definition.  Measured on the ladder tarballs (probes/asd.lisp):
   md5 / parse-float / documentation-utils / salza2 all begin
   `(asdf:defsystem …)`, and ieee-floats / iterate read fine until their
   TRAILING test-system form says `uiop:symbol-call` — six of 22 systems
   lost to a qualifier in a form we never even use.

   Lenient mode interns the bare name in *PACKAGE*, so `asdf:defsystem'
   reads as DEFSYSTEM — which is exactly what %IT-FIND-DEFSYSTEM matches on
   (by SYMBOL-NAME, already package-blind).  Scoped to the .asd read only:
   library SOURCE keeps strict CLHS behaviour, and so does everything else
   in the image (the flag defaults NIL; the ANSI gate never sees it set).
   Restored escape-safely, like the *PACKAGE* bind above.

   *PACKAGE* IS BOUND TO ASDF-USER when that package exists.  This is what
   real ASDF does — LOAD-ASD reads a system definition with *PACKAGE* bound
   to ASDF-USER, which :USEs CL, ASDF and UIOP — and it is what makes the
   read-time `#.(version<= \"3.1\" (asdf-version))' guard at the head of
   split-sequence.asd and bordeaux-threads.asd resolve to real functions
   instead of unbound symbols in whatever package the CALLER happened to be
   in.  When net/asdf-interface.lisp has not been installed the package does
   not exist and the read happens in the caller's package exactly as before,
   so this is a no-op on images without the ASDF interface."
  (let ((saved *reader-missing-package-lenient*)
        (saved-package *package*)
        (asdf-user (find-package "ASDF-USER")))
    (unwind-protect
         (progn (setq *reader-missing-package-lenient* t)
                (when asdf-user (setq *package* asdf-user))
                (%it-read-forms source-string))
      (setq *reader-missing-package-lenient* saved)
      (setq *package* saved-package))))

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
         ((and (symbolp op) (string= (%it-string-designator op) "and"))
          (let ((r t))
            (dolist (a args) (unless (%it-feature-true-p a) (setq r nil)))
            r))
         ((and (symbolp op) (string= (%it-string-designator op) "or"))
          (let ((r nil))
            (dolist (a args) (when (%it-feature-true-p a) (setq r t)))
            r))
         ((and (symbolp op) (string= (%it-string-designator op) "not"))
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

(defun %it-ordered-paths (ds)
  "The component load order of DEFSYSTEM form DS: a list of module-prefixed
   base names, no \".lisp\" extension, dependency-ordered.

   Extracted so the ARCHIVE loader (INSTALL-TARBALL) and the DIRECTORY loader
   (ASDF:LOAD-SYSTEM, net/asdf-interface.lisp) walk components through exactly
   ONE implementation.  A second component walker is precisely the \"second
   system loader in the image\" this work exists to avoid."
  (let* ((comps (%it-plist-get (cddr ds) :components))
         (files (nreverse (%it-collect-components comps "" nil)))
         (acc nil))
    (dolist (f (%it-toposort files))
      (setq acc (cons (it-file-path f) acc)))
    (nreverse acc)))

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

;;; Called as (funcall hook NAME DEFSYSTEM-FORM SOURCE-FILE) after a system's
;;; components have been loaded.  NIL = nobody is keeping a registry, which is
;;; the state of every image that does not install net/asdf-interface.lisp.
;;; MVM Active Limitation 7: this defvar's init-thunk does NOT run at boot, so
;;; the quiescent value is NIL — which is the value we want — and the ASDF
;;; interface SETQs it explicitly when it installs.
(defvar *it-register-hook* nil)

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
               (asd-forms (%it-read-asd-forms asd-src))
               (ds (%it-find-defsystem asd-forms sysname)))
          (when (null ds)
            (error "install-tarball: no defsystem found in asd"))
          (let* ((this-sysname (%it-string-designator (cadr ds)))
                 ;; System-level :PATHNAME — the subdirectory the components
                 ;; live in relative to the .asd.  named-readtables declares
                 ;; :pathname "src"; without honouring it every component
                 ;; resolved to <asd-dir>/package.lisp, which is not in the
                 ;; tarball, and the whole system loaded as 6 "(missing: …)".
                 (sys-dir (concatenate 'string asd-dir
                                       (%it-dir-prefix
                                        (%it-plist-get (cddr ds) :pathname))))
                 (ordered (%it-ordered-paths ds)))
            (write-string-serial "  system: ") (write-string-serial this-sysname) (write-char-serial 10)
            (write-string-serial "  load order:") (write-char-serial 10)
            ;; 4. load each file's source, in order
            (dolist (p ordered)
              (let* ((rel (concatenate 'string sys-dir p ".lisp"))
                     (ent (assoc rel entries :test #'string=)))
                (cond
                  ((null ent)
                   (write-string-serial "    (missing: ")
                   (write-string-serial rel) (write-string-serial ")") (write-char-serial 10))
                  (t
                   (write-string-serial "    ") (write-string-serial rel) (write-char-serial 10)
                   (%it-eval-source (tar-bytes-to-string (cdr ent)) p)))))
            ;; 5. announce the system to whoever is keeping the registry.
            ;;    ASDF's LOAD-SYSTEM leaves the system REGISTERED and marked
            ;;    loaded, and mgl-pax's autoload stubs call ASDF:LOAD-SYSTEM
            ;;    expecting exactly that.  The hook keeps the registry itself
            ;;    in net/asdf-interface.lisp (whose symbols live in a package
            ;;    this file cannot name at build time) while the fact "this
            ;;    system is now loaded" originates HERE, where it is true.
            ;;    SOURCE-FILE is NIL: an archive install has no on-disk .asd.
            (when *it-register-hook*
              (handler-case (funcall *it-register-hook* this-sysname ds nil)
                (t (c) nil)))
            (write-string-serial "install-tarball: done, system=")
            (write-string-serial this-sysname) (write-char-serial 10)
            this-sysname))))))
