;;;; asdf-interface.lisp — ASDF's *interface*, backed by Modus's own loader.
;;;;
;;;; WHAT THIS IS, AND WHAT IT DELIBERATELY IS NOT
;;;;
;;;; Modus has a system loader.  lib/install-tarball.lisp parses .asd files,
;;;; walks :components (recursing into :module), honours :if-feature and
;;;; :pathname, topologically orders files by :depends-on, and loads them —
;;;; and it reproduces host ASDF's answer line-for-line on 20 of the 22
;;;; systems on the library ladder.
;;;;
;;;; What Modus did NOT have was the NAME ASDF.  Libraries do not ask a Lisp
;;;; "please load my components"; they say `asdf:load-system', they subclass
;;;; `asdf:cl-source-file' in their .asd, and — the case that actually broke
;;;; two ladder systems outright — they open their .asd with a READ-TIME
;;;; assertion about ASDF's own version:
;;;;
;;;;     #.(unless (or #+asdf3.1 (version<= "3.1" (asdf-version)))
;;;;          (error "You need ASDF >= 3.1 to load this system correctly."))
;;;;
;;;; With :asdf3.1 absent the #+ deletes the (version<= …) form, `(or)' is
;;;; NIL, and the error fires before a single component has been looked at.
;;;; split-sequence and bordeaux-threads both died there.
;;;;
;;;; So this file supplies the INTERFACE — the packages, the symbols, the
;;;; classes and the entry points — and wires every entry point that does
;;;; real work to the loader Modus already has.  It is NOT vendored ASDF.
;;;; vendor/asdf/asdf.lisp is 12,836 lines; loading it would put a SECOND
;;;; system loader in the image and relegate Modus's own, which is the exact
;;;; opposite of the point.  A legible system carries the interface its
;;;; ecosystem speaks, not the implementation behind it.
;;;;
;;;; THE HONESTY RULE THIS FILE IS WRITTEN UNDER: an entry point either does
;;;; the real thing or SIGNALS.  Nothing here returns NIL to mean "fine".
;;;; ASDF:LOAD-SYSTEM of a system that cannot be found signals; ASDF:PERFORM,
;;;; which Modus has no plan/compile phase to implement, signals.  A stub that
;;;; silently succeeds is worse than the clean failure it replaces, because it
;;;; moves the error to somewhere unrelated and much later.
;;;;
;;;; Loaded the same way net/genera-compat.lisp is: as a SOURCE STRING baked
;;;; into the image and evaluated at boot (mvm/build-generic-cli.lisp).  It
;;;; has to be.  Everything below defines symbols in a package that does not
;;;; exist outside a running Modus — `asdf::load-system', `uiop::version<=' —
;;;; and every first-party build source is read host-side by CHECK-PARSES
;;;; with SBCL's reader, which rejects those with "Package ASDF does not
;;;; exist".  The heavy lifting (version arithmetic, .asd reading, component
;;;; ordering, file slurping) therefore lives COMPILED in
;;;; lib/install-tarball.lisp under `%it-' names that SBCL can read, and what
;;;; is left here is the naming layer.
;;;;
;;;; See KNOWN DEGENERACIES at the bottom before trusting an ASDF entry point.

;;; =====================================================================
;;; 1.  Packages
;;;
;;; Three, exactly as a real ASDF creates them:
;;;
;;;   UIOP       ASDF's portability layer.  The version predicates live
;;;              here because that is their home in ASDF, and ASDF
;;;              re-exports them.
;;;   ASDF       the interface proper.
;;;   ASDF-USER  the package a .asd is READ in.  It :USEs CL, ASDF and
;;;              UIOP, which is why the unqualified `version<=' and
;;;              `asdf-version' in the read-time guard above resolve at
;;;              all.  lib/install-tarball.lisp's %IT-READ-ASD-FORMS binds
;;;              *PACKAGE* to this package when it exists.
;;;
;;; ASDF :USEs UIOP and re-exports the version predicates, so
;;; `asdf:version<=' and `uiop:version<=' are the SAME symbol — as in real
;;; ASDF, and as ASDF-USER (which uses both) requires in order not to have
;;; a name conflict.
;;; =====================================================================

(defpackage "UIOP"
  (:use "COMMON-LISP")
  (:export "PARSE-VERSION" "VERSION<" "VERSION<=" "VERSION="
           "SYMBOL-CALL" "FIND-SYMBOL*"
           ;; section 2b: the portability surface real programs call
           "GETENV" "GETENVP" "GETCWD" "CHDIR" "TEMPORARY-DIRECTORY"
           "TMPIZE-PATHNAME" "READ-FILE-STRING" "READ-FILE-LINES"
           "SPLIT-STRING" "STRING-PREFIX-P" "STRING-SUFFIX-P" "EMPTYP"
           "FIRST-CHAR" "LAST-CHAR" "NATIVE-NAMESTRING" "PARSE-NATIVE-NAMESTRING"
           "MERGE-PATHNAMES*" "ENSURE-DIRECTORY-PATHNAME"
           "RUN-PROGRAM" "LAUNCH-PROGRAM" "WAIT-PROCESS" "TERMINATE-PROCESS"
           "PROCESS-ALIVE-P" "DELETE-DIRECTORY-TREE"))

(defpackage "ASDF"
  (:use "COMMON-LISP" "UIOP")
  (:export
   ;; re-exported from UIOP (same symbols)
   "PARSE-VERSION" "VERSION<" "VERSION<=" "VERSION="
   ;; version / identity
   "ASDF-VERSION" "VERSION-SATISFIES" "*CENTRAL-REGISTRY*"
   "*SYSTEM-DEFINITION-SEARCH-FUNCTIONS*"
   ;; the component/operation class namespace
   "COMPONENT" "SOURCE-FILE" "CL-SOURCE-FILE" "STATIC-FILE" "MODULE" "SYSTEM"
   "OPERATION" "COMPILE-OP" "LOAD-OP" "TEST-OP" "PREPARE-OP"
   ;; conditions
   "MISSING-COMPONENT" "MISSING-DEPENDENCY"
   ;; system definition + lookup
   "DEFSYSTEM" "FIND-SYSTEM" "REGISTERED-SYSTEM" "REGISTERED-SYSTEMS"
   "CLEAR-SYSTEM" "COERCE-NAME" "PRIMARY-SYSTEM-NAME"
   ;; component accessors
   "COMPONENT-NAME" "COMPONENT-LOADED-P"
   "SYSTEM-SOURCE-FILE" "SYSTEM-SOURCE-DIRECTORY" "SYSTEM-RELATIVE-PATHNAME"
   "SYSTEM-DEPENDS-ON" "SYSTEM-DEFSYSTEM-DEPENDS-ON"
   ;; operations
   "LOAD-SYSTEM" "REQUIRE-SYSTEM" "OPERATE" "OOS" "PERFORM"))

(defpackage "ASDF-USER"
  (:use "COMMON-LISP" "ASDF" "UIOP"))

;;; =====================================================================
;;; 2.  UIOP version arithmetic
;;;
;;; The implementations are compiled, in lib/install-tarball.lisp
;;; (%IT-PARSE-VERSION / %IT-VERSION< / %IT-VERSION<=, which follow UIOP's
;;; own LEXICOGRAPHIC< definition: a strict prefix is LESS, so "3.1" <
;;; "3.1.2" and (version<= "3.1" "3.3.7") is true).  These are the names.
;;; =====================================================================

(defun uiop::parse-version (s &optional on-error)
  "UIOP:PARSE-VERSION — dotted version STRING to a list of integers.
   ON-ERROR is accepted for signature compatibility and ignored: the
   compiled parser returns NIL for an unparsable version, which is the
   value every caller here already guards on."
  (declare (ignore on-error))
  (%it-parse-version s))

(defun uiop::version< (x y) (%it-version< x y))
(defun uiop::version<= (x y) (%it-version<= x y))
(defun uiop::version= (x y) (and (%it-version<= x y) (%it-version<= y x)))

(defun asdf::version-satisfies (version required)
  "ASDF:VERSION-SATISFIES — true when VERSION is at least REQUIRED.
   Quicklisp's setup.lisp asks exactly this of (asdf-version) before it will
   use an ASDF that is already present; without it the check fails and setup
   tries to load an ASDF of its own."
  (and (stringp version) (stringp required) (uiop::version<= required version)))

;;; =====================================================================
;;; 2b.  UIOP's portability surface -- the part programs actually call
;;;
;;; Measured over operandi's 27-system closure: getenv, read-file-string,
;;; split-string, string-prefix-p, getcwd, chdir, temporary-directory,
;;; tmpize-pathname, native-namestring, emptyp, first-/last-char ... and
;;; run-program.  The same honesty rule as the rest of this file: each one
;;; does the real thing or SIGNALS.  getcwd/chdir are syscalls in the hosted
;;; floor (net/hosted-sockets-post.lisp), looked up when called.
;;;
;;; RUN-PROGRAM AND FRIENDS SIGNAL.  Modus has no fork and no exec, by design
;;; (see SB-EXT:RUN-PROGRAM in net/sb-sys-shim.lisp); whether a hosted image
;;; should grow a process spawner is a decision, not a gap to paper over.
;;; =====================================================================

(defun uiop::getenv (name)
  "The value of environment variable NAME, or NIL."
  (%cli-getenv (asdf::%string-of name)))

(defun uiop::getenvp (name)
  (let ((v (uiop::getenv name))) (and v (plusp (length v)))))

(defun uiop::ensure-directory-pathname (x)
  "X as a directory pathname (a namestring gains its trailing slash)."
  (let ((s (if (stringp x) x (namestring x))))
    (pathname (if (and (plusp (length s)) (char= (char s (- (length s) 1)) #\/))
                  s
                  (concatenate 'string s "/")))))

(defun uiop::getcwd ()
  "The process's current directory, as a directory pathname."
  ;; %SBS-GETCWD is AOT code in the hosted floor, reached by NAME (it is not a
  ;; symbol in any package, so FIND-SYMBOL cannot see it); an image without
  ;; the floor answers UNDEFINED-FUNCTION, turned into a plain statement here.
  (let ((s (handler-case (%sbs-getcwd) (undefined-function (c) c nil))))
    (if s
        (uiop::ensure-directory-pathname s)
        (error "UIOP:GETCWD: this image cannot ask the OS for its directory"))))

(defun uiop::chdir (x)
  "Change the process's current directory to X."
  (let ((r (handler-case (%sbs-chdir (if (stringp x) x (namestring x)))
             (undefined-function (c) c
               (error "UIOP:CHDIR: this image cannot change the OS directory")))))
    (if (< r 0)
        (error "UIOP:CHDIR: cannot change to ~A (errno ~D)" x (- 0 r))
        t)))

(defun uiop::temporary-directory ()
  "$TMPDIR as a directory pathname, else /tmp/."
  (let ((d (uiop::getenv "TMPDIR")))
    (uiop::ensure-directory-pathname (if (and d (plusp (length d))) d "/tmp/"))))

(defun uiop::tmpize-pathname (x)
  "A sibling of X whose name is unique enough for a temporary file."
  (let ((p (pathname x)))
    (make-pathname :name (format nil "~A-tmp~36R~36R" (or (pathname-name p) "tmp")
                                 (random 1000000000) (get-universal-time))
                   :defaults p)))

(defun uiop::read-file-string (file &rest keys)
  "The contents of FILE as a string.  KEYS (:external-format …) are accepted;
   Modus's file streams have one external format."
  (declare (ignore keys))
  (with-open-file (in file)
    (let ((out (make-string-output-stream)))
      (loop (let ((c (read-char in nil nil)))
              (if c (write-char c out) (return nil))))
      (get-output-stream-string out))))

(defun uiop::read-file-lines (file &rest keys)
  (declare (ignore keys))
  (with-open-file (in file)
    (let ((acc nil))
      (loop (let ((l (read-line in nil nil)))
              (if l (setq acc (cons l acc)) (return (nreverse acc))))))))

(defun uiop::split-string (string &key max (separator '(#\Space #\Tab)))
  "UIOP's: split STRING at any character in SEPARATOR (a sequence of
   characters, or one character).  Empty pieces are kept.  With MAX, at most
   MAX pieces, split from the END, the FIRST piece keeping the rest -- UIOP's
   documented behaviour, not the one people guess."
  (let ((seps (if (characterp separator) (list separator) (coerce separator 'list)))
        (end (length string))
        (acc nil)
        (count 1))
    (let ((i (- end 1)))
      (loop
        (when (< i 0) (return nil))
        (when (and max (>= count max)) (return nil))
        (when (member (char string i) seps)
          (setq acc (cons (subseq string (+ i 1) end) acc))
          (setq end i)
          (setq count (+ count 1)))
        (setq i (- i 1))))
    (cons (subseq string 0 end) acc)))

(defun uiop::string-prefix-p (prefix string)
  (let ((p (string prefix)) (s (string string)))
    (and (<= (length p) (length s)) (string= p s :end2 (length p)))))

(defun uiop::string-suffix-p (string suffix)
  (let ((s (string string)) (x (string suffix)))
    (and (<= (length x) (length s)) (string= x s :start2 (- (length s) (length x))))))

(defun uiop::emptyp (x)
  (or (null x) (and (typep x 'sequence) (zerop (length x)))))

(defun uiop::first-char (s) (and (stringp s) (plusp (length s)) (char s 0)))
(defun uiop::last-char (s) (and (stringp s) (plusp (length s)) (char s (- (length s) 1))))

(defun uiop::native-namestring (x) (and x (namestring x)))
(defun uiop::parse-native-namestring (s &rest keys)
  (declare (ignore keys))
  (and s (pathname s)))
(defun uiop::merge-pathnames* (specified &optional (defaults *default-pathname-defaults*))
  (merge-pathnames specified defaults))

(defun %uiop-no-processes (name)
  (error "UIOP:~A: Modus has no processes to run (no fork, no exec)." name))
(defun uiop::run-program (command &rest keys)
  (declare (ignore command keys)) (%uiop-no-processes "RUN-PROGRAM"))
(defun uiop::launch-program (command &rest keys)
  (declare (ignore command keys)) (%uiop-no-processes "LAUNCH-PROGRAM"))
(defun uiop::wait-process (p) (declare (ignore p)) (%uiop-no-processes "WAIT-PROCESS"))
(defun uiop::terminate-process (p &rest keys)
  (declare (ignore p keys)) (%uiop-no-processes "TERMINATE-PROCESS"))
(defun uiop::process-alive-p (p) (declare (ignore p)) nil)
(defun uiop::delete-directory-tree (dir &rest keys)
  (declare (ignore keys))
  (error "UIOP:DELETE-DIRECTORY-TREE is not implemented on Modus (~A)" dir))

(defun asdf::%string-of (x)
  "A string designator (string / symbol / character) as a STRING.  Written
   out rather than calling CL:STRING so nothing here depends on which
   designator shapes Modus's STRING happens to accept."
  (cond ((stringp x) x)
        ((symbolp x) (symbol-name x))
        (t (princ-to-string x))))

;;; NOTE the parameter is ERRORP, not UIOP's own ERROR.  CL keeps variable
;;; and function namespaces apart so `(error …)' inside a binding of ERROR
;;; is legal Lisp, but this file is EVALUATED by Modus and there is no
;;; reason to make a shadowed CL:ERROR the thing being tested.  Callers pass
;;; the argument positionally, so the name is not part of the interface.
(defun uiop::find-symbol* (name package &optional (errorp t))
  "UIOP:FIND-SYMBOL* — the symbol NAME in PACKAGE, signalling when absent
   unless ERRORP is NIL.  NAME and PACKAGE are string designators."
  (let* ((p (find-package (asdf::%string-of package)))
         (s (and p (find-symbol (asdf::%string-of name) p))))
    (cond (s s)
          (errorp (error "UIOP:FIND-SYMBOL*: no symbol ~A in package ~A" name package))
          (t nil))))

(defun uiop::symbol-call (package name &rest args)
  "UIOP:SYMBOL-CALL — call the function named NAME in PACKAGE.  Used by
   .asd :perform clauses (ieee-floats and iterate both end with one)."
  (apply (symbol-function (uiop::find-symbol* name package)) args))

;;; =====================================================================
;;; 3.  Conditions
;;; =====================================================================

(define-condition asdf::missing-component (error)
  ((requires :initarg :requires :reader asdf::missing-requires)))

(define-condition asdf::missing-dependency (asdf::missing-component)
  ((required-by :initarg :required-by :reader asdf::missing-required-by)))

;;; =====================================================================
;;; 4.  The component / operation class namespace
;;;
;;; A .asd routinely SUBCLASSES these — named-readtables.asd opens with
;;;   (defclass named-readtables-source-file (cl-source-file) ())
;;; — and mgl-pax uses the SYMBOL `asdf:system' as a documentation
;;; locative type in ~60 places.  So the classes have to exist and to be
;;; real classes, not bare symbols.
;;;
;;; Only SYSTEM carries slots: Modus's loader never instantiates a
;;; per-file component object (it works from the .asd form directly), so a
;;; slotted CL-SOURCE-FILE would be a shape Modus never fills in.  The
;;; empty classes are a NAMESPACE that subclassing works against, which is
;;; the whole of what the corpus asks of them.
;;;
;;; The system's mutable "has it been loaded" bit is a CONS in the STATE
;;; slot rather than a writable slot, so nothing here depends on SETF of a
;;; CLOS slot accessor.
;;; =====================================================================

(defclass asdf::component () ())
(defclass asdf::source-file (asdf::component) ())
(defclass asdf::cl-source-file (asdf::source-file) ())
(defclass asdf::static-file (asdf::source-file) ())
(defclass asdf::module (asdf::component) ())

(defclass asdf::system (asdf::module)
  ((name        :initarg :name        :reader asdf::component-name)
   (source-file :initarg :source-file :reader asdf::%system-source-file)
   (form        :initarg :form        :reader asdf::%system-form)
   (state       :initarg :state       :reader asdf::%system-state)))

(defclass asdf::operation () ())
(defclass asdf::prepare-op (asdf::operation) ())
(defclass asdf::compile-op (asdf::operation) ())
(defclass asdf::load-op (asdf::operation) ())
(defclass asdf::test-op (asdf::operation) ())

(defun asdf::component-loaded-p (c)
  "True once C's components have been loaded into this image."
  (car (asdf::%system-state c)))

(defun asdf::%set-loaded (c v)
  (rplaca (asdf::%system-state c) v)
  v)

;;; =====================================================================
;;; 5.  Version advertisement
;;;
;;; *ASDF-VERSION* is a claim about the INTERFACE LEVEL this file
;;; implements a subset of — the same kind of claim net/genera-compat.lisp
;;; makes with :GENERA, and bounded the same way: by the KNOWN DEGENERACIES
;;; list at the bottom of this file.  It is NOT a claim to be carrying
;;; ASDF 3.3.7's implementation.
;;;
;;; The features matter as much as the number: the guard at the head of
;;; split-sequence.asd is `(or #+asdf3.1 (version<= …))', so with the
;;; feature absent the version function is never even consulted.  Pushed at
;;; the END of installation (%INIT-ASDF-INTERFACE), after every package,
;;; class and function above exists — so no reader conditional can select
;;; an ASDF branch whose support has not been installed yet.
;;; =====================================================================

(defvar asdf::*asdf-version* nil)

(defun asdf::asdf-version ()
  "The ASDF interface level Modus implements a subset of, as a version
   string.  See KNOWN DEGENERACIES for what that subset is."
  asdf::*asdf-version*)

;;; =====================================================================
;;; 6.  The registry
;;;
;;; ASDF keeps every system it has seen, whether or not it is loaded, and
;;; FIND-SYSTEM returns the SAME object for the same name.  So does this:
;;; the registry is an alist of (NAME-STRING . SYSTEM), and a system enters
;;; it from exactly two places —
;;;
;;;   * lib/install-tarball.lisp, through *IT-REGISTER-HOOK*, when an
;;;     archive install finishes.  The system is registered LOADED,
;;;     because it is.
;;;   * ASDF:FIND-SYSTEM, when it locates a <name>.asd on
;;;     *CENTRAL-REGISTRY*.  Registered NOT loaded — that is what
;;;     LOAD-SYSTEM is for.
;;;
;;; MVM Active Limitation 7 does not bite here (this file is EVALUATED at
;;; boot, so defvar initforms do run), but the specials are SETQ'd
;;; explicitly in %INIT-ASDF-INTERFACE anyway, matching the convention used
;;; everywhere else in the tree.
;;; =====================================================================

(defvar asdf::*registered-systems* nil)   ; alist (NAME . SYSTEM)
(defvar asdf::*systems-loading* nil)      ; names currently being loaded
(defvar asdf::*central-registry* nil)     ; list of directories holding .asd
;; The ASDF-2-era search hook.  Quicklisp's setup PUSHes its dist searcher
;; here, which needs the variable to exist first.  Not reset by
;; %INIT-ASDF-INTERFACE: whatever a client installed must survive.
(defvar asdf::*system-definition-search-functions* nil)

(defun asdf::coerce-name (x)
  "ASDF:COERCE-NAME — a system designator to its canonical string name.
   A SYSTEM answers with its own name; a STRING is verbatim; a SYMBOL is
   DOWNCASED (%IT-STRING-DESIGNATOR, the rule the loader already uses —
   without the downcase `(:module src …)' resolves to \"SRC/\")."
  (if (typep x 'asdf::system)
      (asdf::component-name x)
      (%it-string-designator x)))

(defun asdf::primary-system-name (name)
  "ASDF:PRIMARY-SYSTEM-NAME — the part of NAME before the first \"/\".
   `(primary-system-name \"foo/test\")' => \"foo\"."
  (let* ((s (asdf::coerce-name name))
         (n (length s))
         (i 0)
         (cut nil))
    (loop
      (when (or cut (>= i n)) (return nil))
      (when (char= (char s i) #\/) (setq cut i))
      (setq i (+ i 1)))
    (if cut (subseq s 0 cut) s)))

(defun asdf::%registry-drop (alist name)
  (let ((acc nil))
    (dolist (e alist)
      (unless (string= (car e) name) (setq acc (cons e acc))))
    (nreverse acc)))

(defun asdf::registered-system (name)
  "ASDF:REGISTERED-SYSTEM — the SYSTEM object for NAME if it has already
   been seen, else NIL.  Never consults *CENTRAL-REGISTRY*; never signals."
  (let ((n (asdf::coerce-name name)) (found nil))
    (dolist (e asdf::*registered-systems*)
      (when (and (null found) (string= (car e) n)) (setq found (cdr e))))
    found))

(defun asdf::registered-systems ()
  "ASDF:REGISTERED-SYSTEMS — the names of all systems seen so far."
  (let ((acc nil))
    (dolist (e asdf::*registered-systems*) (setq acc (cons (car e) acc)))
    (nreverse acc)))

(defun asdf::clear-system (name)
  "ASDF:CLEAR-SYSTEM — forget NAME, so the next FIND-SYSTEM re-reads its
   .asd.  Returns T if something was forgotten.  NOTE the degeneracy: this
   forgets the DEFINITION, it cannot unload the code already evaluated
   into the image."
  (let* ((n (asdf::coerce-name name))
         (had (asdf::registered-system n)))
    (setq asdf::*registered-systems*
          (asdf::%registry-drop asdf::*registered-systems* n))
    (if had t nil)))

(defun asdf::%register (name ds source-file loaded)
  "Enter a system in the registry and return the SYSTEM object.  DS is the
   DEFSYSTEM form as READ; SOURCE-FILE is the .asd path, or NIL for an
   archive install (which has no on-disk .asd)."
  (let ((sys (make-instance 'asdf::system
                            :name (asdf::coerce-name name)
                            :source-file source-file
                            :form ds
                            :state (list loaded))))
    (setq asdf::*registered-systems*
          (cons (cons (asdf::coerce-name name) sys)
                (asdf::%registry-drop asdf::*registered-systems*
                                      (asdf::coerce-name name))))
    sys))

(defun asdf::%register-installed (name ds source-file)
  "*IT-REGISTER-HOOK* — INSTALL-TARBALL calls this when a system's
   components have been loaded out of an archive."
  (asdf::%register name ds source-file t))

;;; =====================================================================
;;; 7.  Finding a system on disk
;;;
;;; Classic ASDF central-registry lookup: for each directory D on
;;; *CENTRAL-REGISTRY*, is there a D/<name>.asd?  No directory scan is
;;; needed or done — the path is computed, exactly as ASDF computes it.
;;;
;;; *CENTRAL-REGISTRY* starts EMPTY.  Seeding it with a guess (a Quicklisp
;;; tree, say) would make FIND-SYSTEM's answer depend on the machine rather
;;; than on what the image was told, and would quietly resolve a system the
;;; caller never pointed us at.  Callers push their own directories.
;;; =====================================================================

(defun asdf::%registry-dir (d)
  "A *CENTRAL-REGISTRY* entry as a directory string ending in exactly one
   \"/\"."
  ;; NAMESTRING, not PRINC-TO-STRING: a pathname entry (what Quicklisp and
  ;; most callers push) prints as its raw instance under PRINC.
  (let ((s (if (stringp d) d (namestring d))))
    (cond ((= (length s) 0) "")
          ((char= (char s (- (length s) 1)) #\/) s)
          (t (concatenate 'string s "/")))))

(defun asdf::%locate-asd (name)
  "Path of the .asd defining NAME, searching *CENTRAL-REGISTRY* in order,
   or NIL.  A secondary system \"foo/test\" lives in foo.asd, so the
   PRIMARY name is what is looked for."
  (let ((base (asdf::primary-system-name name)) (found nil))
    (dolist (d asdf::*central-registry*)
      (when (null found)
        (let ((p (concatenate 'string (asdf::%registry-dir d) base ".asd")))
          (when (%it-file-exists-p p) (setq found p)))))
    found))

(defun asdf::%register-from-asd (name path)
  "Read the .asd at PATH, find NAME's DEFSYSTEM in it, register it as NOT
   loaded, and return the SYSTEM."
  (let* ((forms (%it-read-asd-forms (%it-slurp-text path) path))
         (ds (%it-find-defsystem forms (asdf::coerce-name name))))
    (when (null ds)
      (error 'asdf::missing-component :requires (asdf::coerce-name name)))
    (asdf::%register name ds path nil)))

(defun asdf::%search-fns-locate (n)
  "Consult ASDF::*SYSTEM-DEFINITION-SEARCH-FUNCTIONS* — the ASDF-2-era
   hook quicklisp pushes QL-DIST::SYSTEM-DEFINITION-SEARCHER onto.  That
   searcher ensure-installs the release (tarball fetch + untar) and
   answers the installed .asd's pathname.  The variable is defined at
   RUNTIME (by ql's setup / the genera shim), so read it defensively;
   a searcher error is a miss."
  (let ((fns (handler-case asdf::*system-definition-search-functions*
               (t (c) nil)))
        (hit nil))
    (dolist (f fns hit)
      (when (null hit)
        (let ((r (handler-case (funcall f n) (t (c) nil))))
          (when r (setq hit r)))))))

(defun asdf::find-system (name &optional (error-p t))
  "ASDF:FIND-SYSTEM — the SYSTEM object for NAME.  Already-seen systems
   answer from the registry (so the object is EQ across calls); otherwise
   the .asd is located on *CENTRAL-REGISTRY* and read.  Signals
   ASDF:MISSING-COMPONENT when the system cannot be found, unless ERROR-P
   is NIL, in which case NIL."
  (let* ((n (asdf::coerce-name name))
         (hit (asdf::registered-system n)))
    (cond
      (hit hit)
      (t (let ((path (asdf::%locate-asd n)))
           (cond
             (path (asdf::%register-from-asd n path))
             (t
              ;; Registry + central-registry missed: run the runtime
              ;; search functions (quicklisp integration — see
              ;; %search-fns-locate).  A hit is the .asd's pathname.
              ;; Search for the PRIMARY name, as for *CENTRAL-REGISTRY*:
              ;; Quicklisp's searcher only answers a name that matches the
              ;; .asd file's own, so "seal/http" is found as seal.asd.
              (let ((spath (asdf::%search-fns-locate
                            (asdf::primary-system-name n))))
                (cond
                  (spath (asdf::%register-from-asd
                          n (handler-case (namestring spath)
                              (t (c) spath))))
                  (error-p (error 'asdf::missing-component :requires n))
                  (t nil))))))))))

;;; =====================================================================
;;; 8.  Loading
;;;
;;; This is where the interface meets Modus's loader.  The component walk
;;; is %IT-ORDERED-PATHS — the SAME function INSTALL-TARBALL uses, so a
;;; system loaded from a directory and the same system loaded from a
;;; tarball see identical :if-feature filtering, identical :pathname
;;; handling and identical dependency order.  There is one component
;;; walker in this image.
;;; =====================================================================

(defun asdf::system-depends-on (sys)
  "ASDF:SYSTEM-DEPENDS-ON — the :DEPENDS-ON list of SYS's definition, as
   written.  Entries may be plain designators or dependency FORMS such as
   `(:feature :corman (:require \"threads\"))'; see %DEP-NAMES for the
   ones the loader can act on."
  (%it-plist-get (cddr (asdf::%system-form sys)) :depends-on))

(defun asdf::system-defsystem-depends-on (sys)
  "ASDF:SYSTEM-DEFSYSTEM-DEPENDS-ON — systems needed to READ this
   definition.  Modus never EVALUATES a .asd (it reads and interprets the
   form), so nothing acts on this; it is reported, not honoured."
  (%it-plist-get (cddr (asdf::%system-form sys)) :defsystem-depends-on))

(defun asdf::%featurep (x)
  "A feature expression against *FEATURES*: a symbol (compared by name, as
   the reader does for #+), or (:not E), (:and E...), (:or E...)."
  (cond
    ((and x (symbolp x))
     (let ((hit nil))
       (dolist (f *features* hit)
         (when (and (symbolp f) (string= (symbol-name f) (symbol-name x)))
           (setq hit t)))))
    ((and (consp x) (symbolp (car x)))
     (let ((op (symbol-name (car x))))
       (cond
         ((string= op "NOT") (not (asdf::%featurep (cadr x))))
         ((string= op "AND") (let ((r t))
                               (dolist (e (cdr x) r)
                                 (unless (asdf::%featurep e) (setq r nil)))))
         ((string= op "OR") (let ((r nil))
                              (dolist (e (cdr x) r)
                                (when (asdf::%featurep e) (setq r t)))))
         (t nil))))
    (t nil)))

(defun asdf::%dep-name (d)
  "The system a single :DEPENDS-ON entry D requires, or NIL.
     name                    -> name
     (:version name v)       -> name  (the version is not checked)
     (:feature expr dep)     -> dep's name when EXPR holds, else NIL
     (:require module)       -> NIL   (a host module; Modus has none)"
  (cond
    ((stringp d) (asdf::coerce-name d))
    ((and d (symbolp d)) (asdf::coerce-name d))
    ((and (consp d) (symbolp (car d)))
     (let ((op (symbol-name (car d))))
       (cond
         ((string= op "VERSION") (asdf::%dep-name (cadr d)))
         ((string= op "FEATURE") (when (asdf::%featurep (cadr d))
                                   (asdf::%dep-name (caddr d))))
         (t nil))))
    (t nil)))

(defun asdf::%dep-names (sys)
  "The systems SYS's :DEPENDS-ON requires, in order (see %DEP-NAME).
   Dependency FORMS used to be dropped wholesale, which silently skipped a
   PRESENT :feature -- com.inuoe.jzon's (:feature (:not :ecl) float-features)
   left float-features unloaded, and every jzon file then failed to READ its
   first float-features: symbol."
  (let ((acc nil))
    (dolist (d (asdf::system-depends-on sys))
      (let ((n (asdf::%dep-name d)))
        (when n (setq acc (cons n acc)))))
    (nreverse acc)))

(defun asdf::%as-system (x)
  "X if it is a SYSTEM, else the system it names (FIND-SYSTEM, which signals)."
  (if (typep x 'asdf::system) x (asdf::find-system x)))

(defun asdf::system-source-file (sys)
  "ASDF:SYSTEM-SOURCE-FILE -- the .asd a system was read from.  SYS may be a
   system or its NAME, as in ASDF: quri's etld.lisp asks by keyword at READ
   time (#.), and a reader-only accessor answered NO-APPLICABLE-METHOD and
   discarded the rest of the file."
  (asdf::%system-source-file (asdf::%as-system sys)))

(defun asdf::system-source-directory (sys)
  "ASDF:SYSTEM-SOURCE-DIRECTORY — the directory of SYS's .asd, with a
   trailing \"/\".  NIL for an archive install, which has no on-disk .asd."
  (let ((f (asdf::system-source-file sys)))
    (and f (%it-dirname f))))

(defun asdf::system-relative-pathname (sys name)
  "ASDF:SYSTEM-RELATIVE-PATHNAME — NAME resolved against SYS's source
   directory."
  (let ((d (asdf::system-source-directory sys)))
    (when (null d)
      (error "ASDF:SYSTEM-RELATIVE-PATHNAME: system ~A has no source directory (it was installed from an archive)."
             (asdf::component-name sys)))
    ;; A pathname NAME is common (#p"data/x.dat") and must go through NAMESTRING.
    (pathname (concatenate 'string d (cond ((stringp name) name)
                                           ((pathnamep name) (namestring name))
                                           (t (princ-to-string name)))))))

(defun asdf::%load-system-files (sys)
  "Load SYS's components, in dependency order, from its source directory."
  (let* ((form (asdf::%system-form sys))
         (dir (asdf::system-source-directory sys)))
    (when (null dir)
      (error "ASDF:LOAD-SYSTEM: system ~A was installed from an archive and has no source directory to reload from."
             (asdf::component-name sys)))
    (let ((sys-dir (concatenate 'string dir
                                (%it-dir-prefix
                                 (%it-plist-get (cddr form) :pathname)))))
      (dolist (p (%it-ordered-paths form))
        (let ((full (concatenate 'string sys-dir p ".lisp")))
          (cond
            ((%it-file-exists-p full)
             (princ "    ") (princ full) (terpri) (finish-output)
             (%it-eval-source (%it-slurp-text full) p))
            (t
             (princ "    (missing: ") (princ full) (princ ")")
             (terpri) (finish-output))))))))

(defun asdf::%remove-name (n list)
  (let ((acc nil))
    (dolist (x list) (unless (string= x n) (setq acc (cons x acc))))
    (nreverse acc)))

(defun asdf::%fset-in (pkg name fn)
  "Install FN as PKG::NAME's function, if that symbol exists."
  (let ((s (and (find-package pkg) (find-symbol name pkg))))
    (when s (setf (fdefinition s) fn))
    s))

(defun asdf::%quirk-float-features ()
  "float-features has a branch per implementation and none for modus, so its
   float<->bits functions signal \"Implementation not supported.\" -- and jzon
   prints and reads every JSON number through them.  modus stores a double as
   hi/lo 32-bit halves (%FLOAT-HI32 / %FLOAT-LO32, %MAKE-TYPED-FLOAT) and has
   IEEE32 conversions (%SINGLE->BITS / %BITS->SINGLE).  They are redefined by
   EVALUATING DEFUNs, not by setting function cells: float-features DECLAIMs
   them INLINE, so callers expand the stored source, which only a new DEFUN
   replaces."
  (let ((p (find-package "ORG.SHIRAKUMO.FLOAT-FEATURES")))
    (when p
      (flet ((def (name lambda-list body)
               (let ((s (find-symbol name p)))
                 (when s (eval (list 'defun s lambda-list body))))))
        (def "DOUBLE-FLOAT-BITS" '(f)
          '(let ((d (float f 1d0)))
             (logior (ash (logand (%float-hi32 d) 4294967295) 32)
                     (logand (%float-lo32 d) 4294967295))))
        (def "LONG-FLOAT-BITS" '(f)
          '(let ((d (float f 1d0)))
             (logior (ash (logand (%float-hi32 d) 4294967295) 32)
                     (logand (%float-lo32 d) 4294967295))))
        (def "BITS-DOUBLE-FLOAT" '(bits)
          '(%make-typed-float (ash bits -32) (logand bits 4294967295) 'double-float))
        (def "BITS-LONG-FLOAT" '(bits)
          '(%make-typed-float (ash bits -32) (logand bits 4294967295) 'double-float))
        (def "SINGLE-FLOAT-BITS" '(f) '(%single->bits f))
        (def "BITS-SINGLE-FLOAT" '(b) '(%bits->single b)))
      (let ((s (find-symbol "WITH-FLOAT-TRAPS-MASKED" p)))
        (when s
          (eval (list 'defmacro s '(traps &body body)
                      '(declare (ignore traps))
                      '(cons 'progn body))))))))

(defvar asdf::*system-quirks*
  '(("float-features" . asdf::%quirk-float-features))
  "System name -> a function run right after that system loads: the fix-ups
   for libraries that have a branch per implementation and none for modus.")

(defun asdf::%apply-system-quirks (name)
  (let ((q (assoc name asdf::*system-quirks* :test #'string-equal)))
    (when q (handler-case (funcall (cdr q)) (serious-condition (c) nil)))))

(defun asdf::%jit-loaded-system ()
  "Translate what the system just loaded to native code.  Loaded source is
   installed as INTERPRETER TRAMPOLINES and nothing made it native until a
   program happened to start a thread (%MAKE-NATIVE-THREAD runs JIT-EAGER):
   natrium's X25519 took 11 s interpreted and 0.4 s native, SHA-256 100x, and
   TLS could not finish a ClientHello before the server hung up.  After each
   system rather than once at the end, so a later system's load-time calls
   into an earlier one run native too.  JIT-EAGER translates only what is new.
   MODUS_NO_LOAD_JIT=1 turns it off; a JIT failure never fails the load."
  ;; RAISED, not run: the outermost LOAD runs it between two toplevel forms
  ;; (mvm/ansi-bridge.lisp, DEFERRED JIT) -- running it here replaced the
  ;; still-executing Quicklisp functions under their own frames.
  (let ((off (%cli-getenv "MODUS_NO_LOAD_JIT")))
    (unless (and off (> (length off) 0) (not (string= off "0")))
      (setq *%jit-pending* t)))
  nil)

(defvar asdf::*system-aliases*
  '(("sqlite" . "sqlite-pure/cl-sqlite"))
  "System name -> the pure-Lisp system that provides the same package and API
   on modus.  Modus has no FFI, so a library that binds a C library (cl-sqlite
   over CFFI/libsqlite3) cannot load; its drop-in replacement can.  An alias
   applies only when the replacement can actually be found, so an image
   without it still reports the original system.")

(defun asdf::%system-alias (name)
  (let* ((n (if (stringp name) name (string-downcase (string name))))
         (a (assoc n asdf::*system-aliases* :test #'string-equal)))
    (if (and a (handler-case (asdf::find-system (cdr a) nil) (error (c) nil)))
        (cdr a)
        name)))

(defun asdf::load-system (name &rest keys)
  "ASDF:LOAD-SYSTEM — load NAME and everything it depends on, and return T.

   Backed by Modus's loader end to end: FIND-SYSTEM to get the definition,
   %IT-ORDERED-PATHS to order the components, %IT-EVAL-SOURCE to load each
   one.  A system already loaded (including one installed by
   INSTALL-TARBALL) is a no-op returning T, as in ASDF.  A system that
   cannot be found SIGNALS — it does not quietly return NIL.

   KEYS (:force, :verbose, …) are accepted for signature compatibility and
   ignored; see KNOWN DEGENERACIES."
  (declare (ignore keys))
  ;; Gray streams are installed on first use (net/sb-gray-shim.lisp): a
  ;; library that subclasses FUNDAMENTAL-* arrives through here.
  (when (fboundp '%ensure-gray-streams) (%ensure-gray-streams))
  (setq name (asdf::%system-alias name))
  (let* ((sys (asdf::find-system name t))
         (n (asdf::component-name sys)))
    (cond
      ((asdf::component-loaded-p sys) t)
      ;; A dependency cycle: we are already inside this system's load.
      ;; ASDF signals; returning T here would be a lie, but so would
      ;; recursing forever, and the cycle is in the LIBRARY's .asd.
      ((member n asdf::*systems-loading* :test #'string=)
       (error "ASDF:LOAD-SYSTEM: circular dependency on system ~A." n))
      (t
       (setq asdf::*systems-loading* (cons n asdf::*systems-loading*))
       (unwind-protect
            (progn
              (dolist (d (asdf::%dep-names sys)) (asdf::load-system d))
              (asdf::%load-system-files sys)
              (asdf::%set-loaded sys t)
              (asdf::%apply-system-quirks n)
              (asdf::%jit-loaded-system))
         (setq asdf::*systems-loading*
               (asdf::%remove-name n asdf::*systems-loading*)))
       t))))

(defun asdf::require-system (name &rest keys)
  "ASDF:REQUIRE-SYSTEM — LOAD-SYSTEM unless already loaded."
  (declare (ignore keys))
  (let ((sys (asdf::find-system name nil)))
    (if (and sys (asdf::component-loaded-p sys)) t (asdf::load-system name))))

(defun asdf::operate (operation component &rest keys)
  "ASDF:OPERATE — Modus implements exactly one operation, LOAD-OP (and its
   LOAD-SYSTEM spelling), because loading source is the only thing its
   loader does.  Anything else SIGNALS rather than pretending to plan a
   compile."
  (declare (ignore keys))
  (let ((op (if (symbolp operation) (symbol-name operation)
                (princ-to-string operation))))
    (if (or (string-equal op "LOAD-OP") (string-equal op "LOAD-SYSTEM")
            (string-equal op "LOAD-SOURCE-OP") (string-equal op "PREPARE-OP"))
        (asdf::load-system component)
        (error "ASDF:OPERATE: Modus implements LOAD-OP only; ~A has no implementation (there is no compile/FASL phase)." op))))

(defun asdf::oos (operation component &rest keys)
  "ASDF:OOS — OPERATE's older spelling."
  (declare (ignore keys))
  (asdf::operate operation component))

(defun asdf::perform (operation component)
  "ASDF:PERFORM — NOT IMPLEMENTED, and signals so.  PERFORM is the hook
   into ASDF's plan/compile machinery; Modus loads source directly and has
   no plan to perform steps of.  A .asd's `:perform' clauses and
   `(defmethod perform :around …)' methods are READ and ignored."
  (error "ASDF:PERFORM is not implemented on Modus (~S on ~S).  Modus's loader evaluates source directly; there is no operation plan."
         operation component))

;;; =====================================================================
;;; 9.  DEFSYSTEM
;;;
;;; Modus's loader READS a .asd and interprets the DEFSYSTEM form as data;
;;; it never evaluates one.  But `asdf:defsystem' must still be defined,
;;; because a .asd loaded any OTHER way — a plain `(load "foo.asd")', or a
;;; defsystem typed at the REPL — evaluates it.  So: evaluating a
;;; DEFSYSTEM registers the definition (not loaded), which is precisely
;;; what real ASDF's DEFSYSTEM does.
;;; =====================================================================

(defmacro asdf::defsystem (name &rest options)
  `(asdf::%register ',name (list* 'asdf::defsystem ',name ',options) nil nil))

;;; =====================================================================
;;; 10.  Installation
;;; =====================================================================

(defun asdf::%install-features ()
  "Advertise the ASDF interface levels.  The read-time guard in
   split-sequence.asd / bordeaux-threads.asd is
   `(or #+asdf3.1 (version<= \"3.1\" (asdf-version)))' — WITHOUT the
   feature the version call is deleted by the reader and never runs, so
   the feature is load-bearing on its own."
  (when (boundp '*features*)
    (dolist (f (list :asdf :asdf2 :asdf3 :asdf3.1 :asdf3.2 :asdf3.3))
      (unless (member f *features*)
        (setq *features* (cons f *features*))))))

(defun asdf::%init-asdf-interface ()
  (setq asdf::*asdf-version* "3.3.7")
  (setq asdf::*registered-systems* nil)
  (setq asdf::*systems-loading* nil)
  (setq asdf::*central-registry* nil)
  ;; Make INSTALL-TARBALL announce what it installs, so ASDF:FIND-SYSTEM /
  ;; ASDF:COMPONENT-LOADED-P tell the truth about archive installs and
  ;; mgl-pax's autoload stubs stop re-loading a system that is already in.
  (setq *it-register-hook* (function asdf::%register-installed))
  ;; Real ASDF counts itself and UIOP as systems that are already loaded, and
  ;; libraries depend on them by name (a :depends-on ("uiop") is common).
  (dolist (n (list "asdf" "uiop"))
    (asdf::%register n (list 'asdf::defsystem n) nil t))
  ;; SBCL contrib modules whose packages this image provides natively.  On
  ;; SBCL they are ASDF systems (seal's :depends-on ("sb-bsd-sockets")); here
  ;; they are already in the image, so they are registered as loaded -- but
  ;; only when the package really exists, as it does not on every target.
  (dolist (pair (list (cons "sb-bsd-sockets" "SB-BSD-SOCKETS")
                      (cons "sb-posix" "SB-POSIX")))
    (when (find-package (cdr pair))
      (asdf::%register (car pair) (list 'asdf::defsystem (car pair)) nil t)))
  (asdf::%install-features)
  t)

(asdf::%init-asdf-interface)

;;; =====================================================================
;;; KNOWN DEGENERACIES  (what an ASDF entry point will and will not do)
;;;
;;;  * NO COMPILATION, NO FASLs, NO PLAN.  Modus loads source.  ASDF:PERFORM
;;;    signals; ASDF:OPERATE accepts only LOAD-OP-shaped operations and
;;;    signals on anything else; a .asd's :perform clauses and its
;;;    `(defmethod perform :around ((o compile-op) …))' methods are read and
;;;    ignored.  :in-order-to is likewise not honoured.
;;;
;;;  * LOAD-SYSTEM'S KEYWORD ARGUMENTS ARE IGNORED.  :force, :force-not,
;;;    :verbose and friends are accepted so calls compile, and do nothing.
;;;    In particular there is no way to force a reload; CLEAR-SYSTEM forgets
;;;    a DEFINITION but cannot unload code already evaluated into the image.
;;;
;;;  * *CENTRAL-REGISTRY* IS THE ONLY SEARCH MECHANISM, and it starts empty.
;;;    ASDF's source-registry (~/.config/common-lisp/source-registry.conf,
;;;    CL_SOURCE_REGISTRY, the tree/directory DSL) is not implemented, and
;;;    neither is the output-translations layer.  A caller must push the
;;;    directories it wants searched.
;;;
;;;  * SECONDARY SYSTEMS ARE LOOKED UP, NOT INFERRED.  "foo/test" is sought
;;;    in foo.asd (PRIMARY-SYSTEM-NAME), which is right; but
;;;    package-inferred-system — where "foo/bar" means the FILE foo/bar.lisp
;;;    with dependencies read out of its defpackage — is not implemented.
;;;
;;;  * VERSIONS ARE NOT CHECKED.  :version in a defsystem is not read, and a
;;;    dependency written (:version "foo" "1.2") loads "foo" without checking
;;;    the version.  UIOP:VERSION< / VERSION<= themselves
;;;    are real and correct; nothing calls them on your behalf.
;;;
;;;  * ASDF-VERSION IS AN INTERFACE-LEVEL CLAIM, bounded by this list — the
;;;    same kind of claim net/genera-compat.lisp makes with :GENERA.  Code
;;;    that reader-conditionalises on #+asdf3.1 to pick which ASDF ENTRY
;;;    POINT to call will be right; code that assumes the machinery behind
;;;    those entry points will not.
;;;
;;;  * COMPONENT OBJECTS EXIST ONLY FOR SYSTEMS.  ASDF:CL-SOURCE-FILE and
;;;    friends are real classes so a .asd can subclass them, but Modus never
;;;    instantiates one: the loader works from the DEFSYSTEM form directly.
;;;    ASDF:COMPONENT-NAME therefore only ever answers for a SYSTEM.
;;; =====================================================================
