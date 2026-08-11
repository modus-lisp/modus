;;;; build-checks.lisp — HOST-ONLY build-time sanity checks over the image
;;;; source blob a build script is about to compile.
;;;;
;;;; THIS FILE IS NEVER BAKED INTO AN IMAGE.  It is loaded by
;;;; lib/load-mvm.lisp after cross.lisp and installs an encapsulation around
;;;; MODUS.MVM::BUILD-IMAGE, so every one of the ~30 build scripts is covered
;;;; without editing any of them, and no image source byte changes.
;;;;
;;;; ------------------------------------------------------------------
;;;; WHY (task #243)
;;;; ------------------------------------------------------------------
;;;; CLAUDE.md Active Limitation #7: `defvar` init-thunks do NOT run unless the
;;;; image's KERNEL-MAIN calls (INIT-ALL-GLOBALS).  A defvar whose initform
;;;; never runs silently reads back NIL (or stays unbound).  That has produced
;;;; at least five separate production bugs, each found the hard way:
;;;;
;;;;   1. *gensym-counter* / *gentemp-counter*  (0081253) — every gensym
;;;;      shared the name "GNIL".
;;;;   2. *pkg-tag* / *sym-tag* — %pkg-p compared against an uninitialised tag
;;;;      and returned T for ANY cons-with-NIL-car.
;;;;   3. char-code-limit — absent from the shipping CLI image entirely.
;;;;   4. *%compiler-macro-hashes* (#242, b44701d) — its initialiser,
;;;;      INIT-COMPILER-MACRO-SET, existed and was correct but was simply not
;;;;      CALLED by 3 of the 10 relevant build scripts.
;;;;   5. The modus2 GC bitmap config defvars — :follow-gc / T initforms read
;;;;      back NIL, so modus3 booted unhardened.
;;;;
;;;; The pattern is mechanical, therefore checkable.  This is the same shape as
;;;; CHECK-PARSES: fail the build, name the variable.
;;;;
;;;; ------------------------------------------------------------------
;;;; WHY PER-BUILD-SCRIPT, NOT PER-SOURCE-FILE
;;;; ------------------------------------------------------------------
;;;; #242 is the instructive case: the init function was present in every
;;;; source file, and correct.  What differed was whether a particular
;;;; kernel-main CALLED it.  A per-file check would have passed.  So the unit
;;;; of analysis here is THE ASSEMBLED BLOB one build script hands to
;;;; BUILD-IMAGE — which also means kernel-main bodies that live inside Lisp
;;;; STRING LITERALS in the build script are covered for free, because by the
;;;; time the blob reaches BUILD-IMAGE they are ordinary source text.
;;;;
;;;; ------------------------------------------------------------------
;;;; THE CHECKS
;;;; ------------------------------------------------------------------
;;;; A. ORPHANED-INITFORM.  A (defvar|defparameter NAME <non-NIL initform>)
;;;;    in an image whose KERNEL-MAIN does not reach INIT-ALL-GLOBALS, and
;;;;    where no reachable function assigns NAME.  Catches bugs 1, 2, 5.
;;;;
;;;; B. ORPHANED-INITIALISER.  A defun named INIT-* / %INIT-* that ASSIGNS a
;;;;    global this image declares and is not reachable from KERNEL-MAIN.
;;;;    Catches bug 4 — and note that bug 4's variable has a NIL initform, so
;;;;    check A is structurally blind to it; the signal is the dead
;;;;    initialiser, not the variable.
;;;;
;;;; The call graph counts head position, #'NAME and 'NAME as calls, and does
;;;; not propagate through the generated name registries (see
;;;; *GLOBAL-CHECK-REGISTRY-PREFIXES*).  The assignment scan over-approximates
;;;; on purpose — a LET binding of NAME counts as an assignment — because a
;;;; missed assignment is a FALSE POSITIVE and those are what kill a check.
;;;;
;;;; Measured on the tree at a3e8767: 15 of the 17 buildable images report
;;;; ZERO.  What it does report is in *GLOBAL-CHECK-BASELINE* below.
;;;;
;;;; ------------------------------------------------------------------
;;;; KNOBS
;;;; ------------------------------------------------------------------
;;;;   MODUS_GLOBAL_CHECK=0      disable entirely
;;;;   MODUS_GLOBAL_CHECK=warn   report but do not fail the build
;;;;   MODUS_GLOBAL_CHECK=force  run even on a blob over the size cap
;;;;   MODUS_GLOBAL_CHECK=dump   write the blob to $MODUS_GLOBAL_CHECK_OUT and
;;;;                             exit before compiling (offline analysis aid)
;;;; Default: report and FAIL the build.

(in-package :modus.mvm)

;;; ============================================================
;;; Allowlist
;;; ============================================================
;;; Variables that legitimately want the NIL/unbound default at boot: their
;;; initform is documentation of intent, or a lazy-init cache, and the code
;;; reading them copes with NIL.  KEEP THIS SHORT.  A long allowlist means the
;;; check is mistuned, not that the tree is fine.

(defvar *global-check-var-allowlist*
  '(;; -- add entries as "NAME"  ; reason
    )
  "Upcased variable-name strings exempt from check A.")

(defvar *global-check-init-allowlist*
  '(;; INIT-ALL-GLOBALS is the generated aggregate; it is *called* by boot, not
    ;; defined in source, but a build script may also define a shim.
    "INIT-ALL-GLOBALS")
  "Upcased defun-name strings exempt from check B.")

;;; ------------------------------------------------------------------
;;; BASELINE (the ratchet)
;;; ------------------------------------------------------------------
;;; Findings that EXIST TODAY on main, are REAL, and are deliberately not
;;; fixed here — each is its own gate (task #243 says: file them, don't fix
;;; them in the same commit).  They are still PRINTED on every build, they
;;; just do not fail it.  Anything NOT in this table fails the build, so a
;;; re-introduced instance — or a new one — is caught.
;;;
;;; Keyed by build-script basename.  :SUPPRESS-CHECK-A means the whole image
;;; has the single root cause "kernel-main never calls (init-all-globals)",
;;; which manifests as one finding per defvar; listing 60+ names would be
;;; noise around one fact.
;;;
;;; SHRINK THIS TABLE.  It is a debt register, not a config knob.

(defvar *global-check-baseline*
  '(("build-aarch64-cli" :suppress-check-a :suppress-check-b
     "#243 FINDING 1 (unfixed, filed): the shipping AArch64 CLI's KERNEL-MAIN
      never calls (INIT-ALL-GLOBALS), while its x64 twin build-generic-cli
      does.  85 defvars read back NIL there — including *SETF-EXPANDERS*,
      *RANDOM-STATE*, INTERNAL-TIME-UNITS-PER-SECOND, the *%TRIG-* constants
      and the whole MOST-POSITIVE-*-FLOAT family — and the two belt-and-braces
      initialisers that exist for exactly this case, %INIT-BOOLE-CONSTANTS and
      %INIT-STANDARD-CHARS, are not called either, so BOOLE-AND and friends are
      NIL too (check B).  One root cause, one fix.  Exactly the #242 shape:
      correct source, one build script that does not call the initialiser.
      VERIFIED ON THE BINARY, not inferred: under qemu-aarch64-static the
      shipping /home/claude/modus-aa64-cli signals UNBOUND-VARIABLE for
      BOOLE-AND, INTERNAL-TIME-UNITS-PER-SECOND, DOUBLE-FLOAT-EPSILON,
      MOST-POSITIVE-DOUBLE-FLOAT and *RANDOM-STATE*, where the x64 CLI built
      from the same sources returns 6 / 1000 / 2.22d-16 / ... correctly.")
    ("build-compiler-test" :suppress-check-a
     "#243 FINDING 2 (unfixed, filed): the compiler smoke image bakes
      compiler.lisp but its KERNEL-MAIN never calls (INIT-ALL-GLOBALS), so
      *OPCODE-TABLE*, *UNRESOLVED-CALLS*, *SETF-EXPANDERS*, *LET-BINDING-LIMIT*
      etc. are NIL in-image.  Diagnostic build, not shipped.")
    ;; The four ANSI GATE RUNNERS.  Over the size cap, so normally skipped
    ;; outright; these entries make MODUS_GLOBAL_CHECK=force honest rather
    ;; than a 57-line wall.  All four share build-ansi-common.lisp's harness
    ;; and none of them calls INIT-ALL-GLOBALS -- deliberately ("init-all-
    ;; globals not safe -- some thunks...", build-x64-linux.lisp ~1070) -- so
    ;; they set what they need by hand instead.
    ("build-x64-linux" :suppress-check-a
     "#243 FINDING 3 (unfixed, filed): 57 check-A findings, 0 check-B.
      MEASURED on this one.  Most are ANSI-corpus scaffolding (*UNIVERSE*,
      *SEARCHED-LIST*, *MY-CLASSES*) which genuinely does not care, but
      *MCGC-BITMAP-ENABLED* / *MCGC-COLLECTOR-ENABLED* /
      *MCGC-KIND-CHECK-ENABLED* are bug 5 from the header list, still live in
      the gate image, and *%TRIG-PI* / *%UNBOUND-SLOT* / *SETF-EXPANDERS*
      deserve their own look.")
    ("build-x64"          :suppress-check-a "#243 FINDING 3 — same harness as build-x64-linux (not separately measured).")
    ("build-aarch64-linux" :suppress-check-a "#243 FINDING 3 — same harness as build-x64-linux (not separately measured).")
    ("build-aarch64"      :suppress-check-a "#243 FINDING 3 — same harness as build-x64-linux (not separately measured)."))
  "Alist (build-script-basename . entries) of KNOWN, FILED, UNFIXED findings.")

(defun %gck-baseline-entry (label)
  (assoc label *global-check-baseline* :test #'equal))

;;; ------------------------------------------------------------------
;;; NAME REGISTRIES ARE NOT CALL SITES
;;; ------------------------------------------------------------------
;;; The CL images contain generated functions that stuff EVERY defun into a
;;; name table:
;;;   (defun %init-sft-auto-7 ()
;;;     (puthash "FOO" *symbol-function-table* #'FOO) ...)
;;; Following #'FOO out of one of those makes literally every function in the
;;; image "reachable from kernel-main" — measured: 3109 of 3109 on the CLI
;;; blob, with check B unable to fire on #242's own bug even with the call
;;; deleted.  Being in the SFT means CALLABLE BY NAME at runtime; it does not
;;; mean boot calls you, which is the whole question here.  So reachability
;;; does not propagate THROUGH these (they stay reachable themselves).

(defvar *global-check-registry-prefixes*
  '("%INIT-SFT-AUTO" "%INIT-SYM-NAME-AUTO")
  "Name prefixes of generated name-registry functions.  Reachability stops at
   them.  If the generators in the build scripts are renamed, the saturation
   warning printed by CHECK-GLOBAL-INITS is what tells you to update this.")

(defun %gck-registry-p (name)
  (dolist (p *global-check-registry-prefixes*)
    (when (and (>= (length name) (length p))
               (string= p name :end2 (length p)))
      (return t))))

;;; ============================================================
;;; Tiny source walkers
;;; ============================================================

(defun %gck-name (x)
  "Upcased name string of symbol X, or NIL if X is not a (non-NIL) symbol.
   Comparison is BY NAME because that is how the MVM resolves everything —
   the function table is keyed by name hash, not by package."
  (and x (symbolp x) (symbol-name x)))

(defun %gck-head-p (form name)
  (and (consp form) (equal (%gck-name (car form)) name)))

(defun %gck-comma-expr (form)
  "If FORM is SBCL's internal backquote-comma structure, its wrapped
   expression; else NIL.  Source is full of macros, and a call that appears
   only inside `(... ,(foo x) ...) would otherwise be invisible to the call
   graph — which would manufacture bogus unreachability findings."
  (let ((p (find-symbol "COMMA-P" :sb-int))
        (e (find-symbol "COMMA-EXPR" :sb-int)))
    (when (and p e (fboundp p) (fboundp e) (funcall p form))
      (funcall e form))))

(defun %gck-collect-symbols (form acc)
  "Push the name string of every symbol occurring anywhere in FORM onto ACC
   (a hash-table used as a set).  Vectors are walked too — literal #(...)
   vectors appear in source."
  (cond ((and form (symbolp form))
         (setf (gethash (symbol-name form) acc) t))
        ((consp form)
         (%gck-collect-symbols (car form) acc)
         (%gck-collect-symbols (cdr form) acc))
        ((and (vectorp form) (not (stringp form)))
         (loop for i from 0 below (length form)
               do (%gck-collect-symbols (aref form i) acc)))
        (t (let ((inner (%gck-comma-expr form)))
             (when inner (%gck-collect-symbols inner acc)))))
  acc)

(defun %gck-scan-toplevel (forms fns vars)
  "Walk toplevel FORMS, filling FNS (name-string -> body list) and VARS
   (list of (name-string kind has-initform-p initform)).  Recurses through the
   toplevel-splicing forms a build blob actually uses."
  (dolist (form forms)
    (when (consp form)
      (let ((head (%gck-name (car form))))
        (cond
          ((equal head "DEFUN")
           (let ((n (%gck-name (cadr form))))
             (when n
               ;; last-defun-wins, exactly like the compiler
               (setf (gethash n fns) (cddr form)))))
          ((or (equal head "DEFVAR") (equal head "DEFPARAMETER")
               (equal head "DEFCONSTANT"))
           (let ((n (%gck-name (cadr form))))
             (when n
               (push (list n head (consp (cddr form)) (caddr form)) (cdr vars)))))
          ((equal head "PROGN")
           (%gck-scan-toplevel (cdr form) fns vars))
          ((equal head "EVAL-WHEN")
           (%gck-scan-toplevel (cddr form) fns vars))
          ((equal head "LOCALLY")
           (%gck-scan-toplevel (cdr form) fns vars))))))
  (values fns vars))

(defun %gck-collect-calls (form acc)
  "Collect into ACC the name of every symbol FORM uses in a CALLEE position:
   the head of a form, #'NAME, and a bare 'NAME (which is how a callee reaches
   FUNCALL/APPLY/MAPCAR here).

   The first cut of this counted ANY symbol anywhere in a body as a call.
   That was measured to be useless, not merely loose: on the shipping CLI blob
   it made 3109 of 3109 defuns \"reachable\", so check B could never fire on
   the exact image class it exists for.  Head position is the tightening.

   QUOTE is not descended into (quoted lists are data), but a quoted SYMBOL is
   kept.  Backquote IS descended into — a macro template's calls are real
   calls at every expansion site."
  (cond
    ((consp form)
     (let* ((h (car form))
            (n (and h (symbolp h) (symbol-name h))))
       (cond
         ((and n (equal n "QUOTE"))
          (let ((q (and (consp (cdr form)) (cadr form))))
            (when (and q (symbolp q) (not (keywordp q)))
              (setf (gethash (symbol-name q) acc) t)))
          (return-from %gck-collect-calls acc))
         ((and n (equal n "FUNCTION"))
          (let ((q (and (consp (cdr form)) (cadr form))))
            (if (and q (symbolp q))
                (setf (gethash (symbol-name q) acc) t)
                (%gck-collect-calls q acc)))
          (return-from %gck-collect-calls acc))
         (n (setf (gethash n acc) t))))
     ;; Descend into every element (heads of nested forms live there too).
     (let ((tail form))
       (loop while (consp tail)
             do (%gck-collect-calls (car tail) acc)
                (setf tail (cdr tail)))
       (when tail (%gck-collect-calls tail acc))))
    ((and (vectorp form) (not (stringp form)))
     (loop for i from 0 below (length form)
           do (%gck-collect-calls (aref form i) acc)))
    (t (let ((inner (%gck-comma-expr form)))
         (when inner (%gck-collect-calls inner acc)))))
  acc)

(defun %gck-reachable (fns roots)
  "Two values: (1) the set of defun names transitively reachable from ROOTS,
   (2) the set of ALL symbol names mentioned anywhere inside those bodies.

   Over-approximating on purpose: ANY symbol in a body that names a known
   defun is treated as a call.  That includes #'NAME, (funcall 'NAME ..) and a
   bare quoted symbol — all of which really can be call sites here.

   The second value exists because some callees are NOT defined in source:
   INIT-ALL-GLOBALS in particular is synthesised by MVM-COMPILE-ALL from the
   defvar init thunks, so `(gethash \"INIT-ALL-GLOBALS\" reachable)' is always
   NIL and asking the first value whether boot runs the init thunks gives the
   wrong answer for every image that does."
  (let ((seen (make-hash-table :test 'equal))
        (mentioned (make-hash-table :test 'equal))
        (queue (copy-list roots)))
    (loop while queue
          do (let ((n (pop queue)))
               (unless (gethash n seen)
                 (setf (gethash n seen) t)
                 (setf (gethash n mentioned) t)
                 (let ((body (unless (%gck-registry-p n) (gethash n fns))))
                   (when body
                     ;; BODY is (ARGLIST . FORMS).  Calls come from the forms
                     ;; (head position); the arglist is scanned loosely because
                     ;; an &optional default may call an initialiser and a
                     ;; missed call would be a FALSE POSITIVE.
                     (let ((syms (%gck-collect-calls
                                  (cdr body)
                                  (%gck-collect-symbols (car body)
                                                        (make-hash-table :test 'equal)))))
                       (maphash (lambda (s v)
                                  (declare (ignore v))
                                  (setf (gethash s mentioned) t)
                                  (when (and (gethash s fns) (not (gethash s seen)))
                                    (push s queue)))
                                syms)))))))
    (values seen mentioned)))

(defun %gck-collect-assignments (form acc)
  "Record into ACC every variable name FORM could plausibly WRITE or REBIND.
   Over-approximate on purpose (see file header): a LET binding counts, so a
   variable that is only ever let-bound will never be flagged."
  (when (consp form)
    (let ((head (and (listp (cdr form)) (%gck-name (car form)))))
      (cond
        ((member head '("SETQ" "PSETQ" "SETF" "PSETF"
                        "INCF" "DECF" "PUSH" "PUSHNEW" "POP")
                 :test #'equal)
         ;; Mark every symbol appearing directly in the argument list.  That
         ;; over-counts (the VALUE of a setq gets marked too) — deliberately:
         ;; the bias is toward never crying wolf.  Walked with an explicit
         ;; CONSP loop because a backquote template's tail is not a list.
         (let ((tail (cdr form)))
           (loop while (consp tail)
                 do (let ((n (%gck-name (car tail))))
                      (when n (setf (gethash n acc) t)))
                    (setf tail (cdr tail)))))
        ((member head '("LET" "LET*") :test #'equal)
         ;; NOTE the LISTP guard: inside a backquote template the binding list
         ;; is often a comma structure, not a list.
         (when (listp (cadr form))
           (dolist (b (cadr form))
             (let ((n (%gck-name (if (consp b) (car b) b))))
               (when n (setf (gethash n acc) t))))))
        ((equal head "SET-SYMBOL-VALUE")
         ;; (set-symbol-value <hash> v) — the hash is opaque here; the only
         ;; source-level users are the generated init thunks.  Nothing to do.
         nil)))
    (%gck-collect-assignments (car form) acc)
    (%gck-collect-assignments (cdr form) acc))
  (let ((inner (%gck-comma-expr form)))
    (when inner (%gck-collect-assignments inner acc)))
  acc)

(defun %gck-globals-written (form varset acc)
  "Collect into ACC (a set) every global this image declares (VARSET) that
   FORM assigns via (setq|setf|psetq|psetf SYMBOL …).

   This is what makes check B precise instead of a name-prefix guess.  An
   `init-…' function that only pokes memory — e.g. net/ssh.lisp's
   INIT-GC-HELPER, which does (setf (mem-ref …) …) — initialises no global,
   so its being dead is not the bug class this check is about, and it must not
   fire.  It also lets the report name the VARIABLE, not just the function."
  (cond
    ((not (consp form))
     (let ((inner (%gck-comma-expr form)))
       (when inner (%gck-globals-written inner varset acc))))
    (t
     (let ((head (and (listp (cdr form)) (%gck-name (car form)))))
       (when (member head '("SETQ" "PSETQ" "SETF" "PSETF") :test #'equal)
         (let ((tail (cdr form)))
           (loop while (consp tail)
                 do (let ((n (%gck-name (car tail))))
                      (when (and n (gethash n varset)) (setf (gethash n acc) t)))
                    (setf tail (if (consp (cdr tail)) (cddr tail) nil))))))
     (%gck-globals-written (car form) varset acc)
     (%gck-globals-written (cdr form) varset acc)))
  acc)

;;; ============================================================
;;; The check
;;; ============================================================

(defun %gck-mode ()
  (let ((v (sb-ext:posix-getenv "MODUS_GLOBAL_CHECK")))
    (cond ((null v) :error)
          ((string= v "0") :off)
          ((string-equal v "warn") :warn)
          ((string-equal v "dump") :dump)
          ((string-equal v "force") :force)
          (t :error))))

;;; The whole cost of this check is RE-READING the blob; the analysis itself is
;;; sub-second.  Measured on the 17.9 MB build-x64-linux blob: 1129 s to read,
;;; 0.7 s to analyse.  That blob is 17 MB of baked ANSI test corpus, and the
;;; four ANSI GATE RUNNERS are the only images anywhere near it (the largest
;;; shipping blob is build-modus-selfhost at 5.0 MB).  They are also not
;;; shipped — #242's lesson is precisely that the gate image was the one
;;; already fine.  So: skip above a threshold, loudly, with an override.
;;;
;;; DO NOT read the skip as "nothing to see there".  A forced run on
;;; build-x64-linux reports 57 check-A findings (0 check-B): that image
;;; deliberately does not call INIT-ALL-GLOBALS ("init-all-globals not safe —
;;; some thunks...", build-x64-linux.lisp ~1070) and sets what it needs by
;;; hand.  Most of the 57 are ANSI-corpus scaffolding (*UNIVERSE*,
;;; *SEARCHED-LIST*, *MY-CLASSES*), but *MCGC-BITMAP-ENABLED* /
;;; *MCGC-COLLECTOR-ENABLED* / *MCGC-KIND-CHECK-ENABLED* are bug 5 of the list
;;; at the top of this file, still live.  Filed as #243 FINDING 3 and
;;; baselined below, so =force is honest too.
(defvar *global-check-max-blob-chars* (* 8 1024 1024))

(defun %gck-build-label ()
  "Basename of the build script SBCL was handed, e.g. \"build-generic-cli\".
   That — not the target keyword — is the identity of an image here: #242 was
   two build scripts for the SAME target disagreeing about whether an
   initialiser gets called."
  (or (sb-ext:posix-getenv "MODUS_GLOBAL_CHECK_LABEL")
      ;; `sbcl --script foo.lisp' leaves *POSIX-ARGV* = (\"sbcl\") — the script
      ;; path is NOT in it (measured).  It IS in the SB-IMPL::PROCESS-SCRIPT
      ;; stack frame, and that is the OUTERMOST script, which is what we want:
      ;; *LOAD-TRUENAME* here would say \"build-ansi-common\" for all four ANSI
      ;; gate runners, collapsing four distinct images into one label.
      (let ((frames (ignore-errors (sb-debug:list-backtrace :count 400))))
        (dolist (f frames)
          (when (and (consp f) (eq (car f) 'sb-impl::process-script)
                     (stringp (cadr f)))
            (let ((n (pathname-name (pathname (cadr f)))))
              (when n (return-from %gck-build-label n))))))
      (and *load-truename* (pathname-name *load-truename*))
      "image"))

(defun check-global-inits (source-text &key (label (%gck-build-label)))
  "Check the assembled image blob SOURCE-TEXT for globals whose initialisation
   never runs in THIS image.  See the file header.  Signals an error listing
   every finding unless MODUS_GLOBAL_CHECK says otherwise."
  (let ((mode (%gck-mode)))
    (when (eq mode :off) (return-from check-global-inits nil))
    (when (eq mode :dump)
      (let ((out (or (sb-ext:posix-getenv "MODUS_GLOBAL_CHECK_OUT")
                     "/tmp/modus-blob.lisp")))
        (with-open-file (s out :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
          (write-string source-text s))
        (format t "~&;; MODUS_GLOBAL_CHECK=dump: wrote ~D chars to ~A~%"
                (length source-text) out)
        (finish-output)
        (sb-ext:exit :code 0)))
    (when (and (not (eq mode :force))
               (> (length source-text) *global-check-max-blob-chars*))
      (format t "~&;; check-global-inits (~A): blob is ~,1F MB (> ~,1F MB) — SKIPPED. ~
This is an ANSI gate runner with the test corpus baked in; re-reading it costs ~
~~20 min for a check that is structurally quiet there.  MODUS_GLOBAL_CHECK=force ~
runs it anyway.~%"
              label (/ (length source-text) 1048576.0)
              (/ *global-check-max-blob-chars* 1048576.0))
      (finish-output)
      (return-from check-global-inits nil))
    (let* ((forms (handler-case (read-all-forms-with-locations source-text)
                    (error (e)
                      (format t "~&;; check-global-inits: unreadable blob (~A) — skipped~%" e)
                      (return-from check-global-inits nil))))
           (forms (if (consp forms) (car forms) forms))
           (fns (make-hash-table :test 'equal))
           (vars (list :vars)))
      (%gck-scan-toplevel forms fns vars)
      (setf vars (nreverse (cdr vars)))
      ;; No kernel-main => not a bootable image; nothing to be reachable FROM.
      (unless (gethash "KERNEL-MAIN" fns)
        (format t "~&;; check-global-inits (~A): no KERNEL-MAIN in blob — skipped~%"
                label)
        (finish-output)
        (return-from check-global-inits nil))
      (multiple-value-bind (reach mentioned)
          (%gck-reachable fns (list "KERNEL-MAIN"))
       (let* ((inits-run-p (gethash "INIT-ALL-GLOBALS" mentioned))
             (baseline (%gck-baseline-entry label))
             (suppress-a (member :suppress-check-a baseline))
             (suppress-b (member :suppress-check-b baseline))
             (assigned (make-hash-table :test 'equal))
             (baselined nil)
             (findings nil))
        ;; Assignments visible from any REACHABLE function.
        (maphash (lambda (n v)
                   (declare (ignore v))
                   (let ((body (gethash n fns)))
                     (when body (%gck-collect-assignments body assigned))))
                 reach)
        ;; --- Check A: orphaned initform -------------------------------
        ;; Only meaningful when the image does NOT run the init thunks.  When
        ;; kernel-main reaches INIT-ALL-GLOBALS the initforms DO run and there
        ;; is nothing to say.
        (unless inits-run-p
          (let ((seen (make-hash-table :test 'equal)))
            (dolist (v vars)
              (destructuring-bind (name kind has-init initform) v
                ;; DEFCONSTANT is exempt by construction: the MVM compiler
                ;; FOLDS constants at build time into *constants* (compiler.lisp
                ;; ~17551), so a defconstant reference never becomes a runtime
                ;; global read at all.  Only the *static-build-p* self-host path
                ;; emits an init thunk for one.
                (when (and (not (equal kind "DEFCONSTANT"))
                           has-init initform          ; NIL initform => no thunk
                           (not (gethash name seen))
                           (not (gethash name assigned))
                           (not (member name *global-check-var-allowlist*
                                        :test #'equal)))
                  (setf (gethash name seen) t)
                  (let ((f (format nil "ORPHANED-INITFORM ~A — initform ~S never runs ~
(this image's KERNEL-MAIN does not call INIT-ALL-GLOBALS) and no reachable ~
function assigns it, so it reads back NIL at runtime."
                                   name initform)))
                    (if suppress-a
                        (push f baselined)
                        (push f findings))))))))
        ;; --- Check B: orphaned initialiser ----------------------------
        ;; Bug 4's variable has a NIL initform, so check A is structurally
        ;; blind to it; the visible signal is that INIT-COMPILER-MACRO-SET
        ;; sits in the image unreferenced.  Two conjuncts keep this quiet:
        ;; the name must look like an initialiser, AND the body must actually
        ;; assign a global this image declares.
        (let ((names nil)
              (varset (make-hash-table :test 'equal))
              (covered (make-hash-table :test 'equal)))
          (dolist (v vars)
            (setf (gethash (first v) varset) t)
            ;; A global is already COVERED if its own initform does the job in
            ;; this image, or if some reachable function assigns it.  Several
            ;; `%init-…' helpers exist purely as belt-and-braces for the images
            ;; that skip INIT-ALL-GLOBALS (%INIT-BOOLE-CONSTANTS,
            ;; %INIT-STANDARD-CHARS); where the initforms DO run they are
            ;; legitimately redundant and must not be reported.  Verified
            ;; against the real binary: BOOLE-AND reads 6 and (boole boole-and
            ;; 12 10) = 8 in the shipping CLI.
            (when (or (and inits-run-p (third v) (fourth v))
                      (gethash (first v) assigned))
              (setf (gethash (first v) covered) t)))
          (maphash (lambda (n v) (declare (ignore v)) (push n names)) fns)
          (dolist (n (sort names #'string<))
            (when (and (or (and (> (length n) 5) (string= "INIT-" n :end2 5))
                           (and (> (length n) 6) (string= "%INIT-" n :end2 6)))
                       (not (gethash n reach))
                       (not (member n *global-check-init-allowlist* :test #'equal)))
              (let ((written nil))
                (maphash (lambda (k v) (declare (ignore v))
                           (unless (gethash k covered) (push k written)))
                         (%gck-globals-written (gethash n fns) varset
                                               (make-hash-table :test 'equal)))
                (when written
                  (let ((f (format nil "ORPHANED-INITIALISER ~A sets ~{~A~^, ~} — it is ~
defined in this image but NOT reachable from its KERNEL-MAIN, so ~:[those ~
variables keep~;that variable keeps~] the default.  Call it from kernel-main."
                                   n (sort written #'string<) (null (cdr written)))))
                    (if suppress-b (push f baselined) (push f findings))))))))
        (setf findings (nreverse findings))
        (setf baselined (nreverse baselined))
        ;; Always leave a line in the build log, like check-parses does.  A
        ;; check whose only output is silence cannot be distinguished from a
        ;; check that never ran — which is how this class of bug hides.
        (format t "~&;; check-global-inits (~A): ~D defuns, ~D reachable from ~
KERNEL-MAIN, ~D globals, init-all-globals ~:[NOT called~;called~] at boot; ~
~D finding~:P~@[ (+~D baselined)~]~%"
                label (hash-table-count fns) (hash-table-count reach)
                (length vars) inits-run-p (length findings)
                (and baselined (length baselined)))
        ;; A saturated call graph means check B is inert — say so rather than
        ;; report a reassuring zero.  See *GLOBAL-CHECK-REGISTRY-PREFIXES*.
        (when (and (> (hash-table-count fns) 200)
                   (= (hash-table-count reach) (hash-table-count fns)))
          (format t ";;   WARNING: call graph SATURATED (every defun reachable) — ~
check B is inert here.~%;;   A name-registry function is probably being ~
followed; add its prefix to *GLOBAL-CHECK-REGISTRY-PREFIXES*.~%"))
        (finish-output)
        ;; Known-and-filed debt: always reported, never fatal.
        (when baselined
          (format t "~&~%;; check-global-inits (~A): ~D KNOWN finding~:P, ~
baselined in mvm/build-checks.lisp:~%;;   ~A~%~{;;   - ~A~%~}"
                  label (length baselined) (third baseline)
                  (mapcar (lambda (s) (subseq s 0 (min 110 (length s)))) baselined))
          (finish-output))
        (when findings
          (let ((msg (format nil "~&~%*** check-global-inits (~A): ~D finding~:P ***~%~
Globals whose initialisation never runs in THIS image (CLAUDE.md Active~%~
Limitation #7 — see mvm/build-checks.lisp).  Either call the initialiser from~%~
this image's KERNEL-MAIN, or, if the default really is intended, add the name~%~
to MODUS.MVM::*GLOBAL-CHECK-VAR-ALLOWLIST* / *GLOBAL-CHECK-INIT-ALLOWLIST*~%~
WITH A REASON.  Set MODUS_GLOBAL_CHECK=warn to downgrade, =0 to disable.~%~%~
~{  - ~A~%~}~%"
                             label (length findings) findings)))
            (ecase mode
              (:warn (format t "~A" msg) (finish-output))
              (:error (format t "~A" msg) (finish-output) (error "check-global-inits failed")))))
        findings)))))

;;; ============================================================
;;; Wiring: encapsulate BUILD-IMAGE
;;; ============================================================
;;; Every build script funnels through BUILD-IMAGE, so wrapping it here covers
;;; all of them at once — including the ANSI gate runners and the ones whose
;;; kernel-main lives in a string literal.  Encapsulating (rather than editing
;;; cross.lisp) keeps every BAKED source file byte-identical: cross.lisp is
;;; itself compiled into the selfhost and fixpoint images.

(defvar *build-image-unwrapped* nil)

(unless *build-image-unwrapped*
  (setf *build-image-unwrapped* #'build-image)
  (setf (fdefinition 'build-image)
        (lambda (&rest args)
          (let ((src (or (getf args :source-text)
                         (let ((forms (getf args :source)))
                           (when forms
                             (with-output-to-string (s)
                               (dolist (f forms) (prin1 f s) (terpri s))))))))
            (when src (check-global-inits src)))
          (apply *build-image-unwrapped* args))))
