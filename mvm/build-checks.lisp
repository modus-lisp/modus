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
;;;; C. LET-OF-UNREGISTERED-SPECIAL (task #248).  `(let ((*x* v)) …)' where
;;;;    *X* is not a special the MVM compiler knows about compiles to a plain
;;;;    LEXICAL let — the global is never assigned and every callee reads the
;;;;    old value, silently.  See the CHECK C section for the measurement.
;;;;
;;;; D. COMPILER WARN HISTOGRAM (task #249).  COMPILE-FORM substitutes NIL for
;;;;    any form it does not recognise and only WARNs, so a clean build emits
;;;;    a broken image.  D tees the build's output, buckets every compiler
;;;;    WARN by shape and ratchets the histogram.  See the CHECK D section.
;;;;
;;;; E. SOURCE-PARSES (task #252).  A first-party .lisp that does not READ.
;;;;    CHECK-PARSES already fails loudly on this — for the files the CURRENT
;;;;    build concatenates.  E sweeps the whole tree, including the build
;;;;    scripts themselves, so a file no routine build touches cannot carry a
;;;;    read error for weeks.  See the CHECK E section.
;;;;
;;;; A and B are about what the image never RUNS; C and D are about what the
;;;; compiler silently DROPPED; E is about a file no build ever READS.  All
;;;; five share one property: nothing fails, nothing is logged, and the damage
;;;; shows up months later somewhere else.
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
;;;; Default: report and FAIL the build.  The knob covers all five checks.
;;;;
;;;; BYTE-NEUTRALITY.  This file is host-only, but the host is not neutral:
;;;; CL:*GENSYM-COUNTER* leaks into every emitted image (see the REPRODUCIBILITY
;;;; PIN below).  Three pins keep the checks from perturbing it; a
;;;; build-generic-cli image built with every check ON is byte-identical to one
;;;; built from a tree without this file's changes at all.  Verify with `cmp',
;;;; do not assume: `cmp modus <pristine>' after building both.

(in-package :modus.mvm)

;;; ------------------------------------------------------------------
;;; REPRODUCIBILITY PIN — this file must not perturb CL:*GENSYM-COUNTER*
;;; ------------------------------------------------------------------
;;; MEASURED, and it is not obvious: the host's *GENSYM-COUNTER* is an INPUT
;;; to the emitted binary.  EXPAND-CL-LOOP does `(gensym "NAT")` /
;;; `(gensym "BV")` while the MVM compiler expands IMAGE source, the resulting
;;; symbol becomes an implicit global, and its NAME — "NAT19385" — is
;;; name-hashed into the image.  A build-generic-cli image contains 99 of
;;; them.  So ANY host-side edit that shifts the counter by even one changes
;;; 435 bytes of a 37 MB binary, and "prove byte-identity with cmp" — the
;;; standard for a host-only check — becomes impossible to meet.
;;;
;;; Demonstrated exactly, not inferred: appending three bare `(gensym)` forms
;;; to the 6de6fc3 build-checks.lisp reproduces, byte for byte
;;; (sha256 03ea4c40…), the binary built from THIS file with the checks
;;; disabled.  Three, because DEFCLASS + friends below cost three at load.
;;;
;;; Two pins keep this file byte-neutral by construction:
;;;   * this one, restoring the counter at end of file (LOAD-time cost), and
;;;   * one in the BUILD-IMAGE wrapper, restoring it immediately before the
;;;     real BUILD-IMAGE runs (the checks' RUN-time cost).
;;; Rolling the counter BACK is safe here: everything created in between is a
;;; host-side macroexpansion symbol that never reaches image source, and the
;;; counter is monotonic for the whole of BUILD-IMAGE afterwards, so no two
;;; image-source gensyms can collide.
;;;
;;; That the host gensym counter leaks into the binary at all is a real
;;; reproducibility defect, filed separately; this pin only stops THIS file
;;; from tripping over it.

(defvar *gck-gensym-counter-at-load* *gensym-counter*)

;;; A check FAILING (findings) and a check BREAKING (a bug in the walker) must
;;; not look the same to a build.  The first should stop the build; the second
;;; is my problem, not the build's, and gets reported and stepped over — 27
;;; build scripts must not all die because one blob has a shape the analysis
;;; mishandles.  #243 conflated the two; this separates them.
(define-condition gck-check-failed (simple-error) ())

(defun %gck-fail (fmt &rest args)
  (error 'gck-check-failed :format-control fmt :format-arguments args))

(defmacro %gck-guard (name &body body)
  "Run BODY; let a GCK-CHECK-FAILED through, report any other error and go on."
  `(handler-case (progn ,@body)
     (gck-check-failed (c) (error c))
     (error (c)
       (format t "~&;; ~A: check itself errored (~A) — SKIPPED, not a build ~
failure.  Please fix mvm/build-checks.lisp.~%" ,name c)
       (finish-output)
       nil)))

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
    ("build-mvm" :suppress-check-a
     "#252: same image family and same root cause as build-compiler-test — the
      `mvm' cross-compiler command bakes compiler.lisp + the translators into a
      bare-metal image whose KERNEL-MAIN never calls (INIT-ALL-GLOBALS), so 20
      compiler-config defvars read NIL: *ARITH-OPS*, *LET-BINDING-LIMIT*,
      *UNRESOLVED-CALLS*, *SETF-EXPANDERS*, *X64-NATIVE-CODE-OFFSET*,
      *REGISTERS*, *VREG-NAMES*, the *MCGC-* set, and friends.  NOT NEWLY
      BROKEN and not a #252 regression: build-mvm.lisp has never been readable
      by this check before (it died in the READER at line 340 until #252 fixed
      the string literal), and the script itself already compensates in source
      — `check-arith-nesting no-op — *arith-ops* defvar isn't initialized on
      bare metal' is a comment in its own *ADAPTER-SOURCE*.  Filed as its own
      gate, same as FINDING 2.")
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

(defun check-global-inits (source-text &key (label (%gck-build-label)) (pre-read nil))
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
    (let* ((forms (or pre-read
                      (handler-case (read-all-forms-with-locations source-text)
                        (error (e)
                          (format t "~&;; check-global-inits: unreadable blob (~A) — skipped~%" e)
                          (return-from check-global-inits nil)))))
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
              (:error (format t "~A" msg) (finish-output) (%gck-fail "check-global-inits failed")))))
        findings)))))

;;; ============================================================
;;; CHECK C (task #248) — LET of an UNREGISTERED special is a silent no-op
;;; ============================================================
;;;
;;; compile-form's LET / LET* dispatch (mvm/compiler.lisp ~5153 and ~5180)
;;; turns `(let ((*x* v)) …)` into a real dynamic binding — the
;;; save / set-symbol-value / restore triple of COMPILE-LET-WITH-SPECIALS —
;;; only when *X* is
;;;
;;;   (a) in the CLHS-standard specials table (%ENSURE-CLHS-SPECIALS-TABLE),
;;;   (b) in *CLHS-EXTRA-SPECIALS* (build-ansi-common's per-file `declaim
;;;       (special …)` allowlist), or
;;;   (c) in *RUNTIME-SPECIAL-NAMES* — which is populated ONLY under
;;;       *MVM-EVAL-RUNTIME-P*, i.e. never during a host build, or
;;;   (d) named in a `(declare (special *x*))` in the LET's own body.
;;;
;;; A name in none of those compiles as a PLAIN LEXICAL LET.  The special is
;;; never bound; a callee — or the same function's own free reference from a
;;; different lexical scope — reads the UNCHANGED global.  Nothing warns.
;;;
;;; MEASURED, not inferred (scratch/probe-a.lisp, real compiler, this tree):
;;;
;;;   (defvar *clos-applying-defaults* nil)
;;;   (defun rdr () *clos-applying-defaults*)
;;;   (defun kernel-main () (let ((*clos-applying-defaults* t)) (rdr)))
;;;        => 20 IR ops, zero symbol-value/set-symbol-value ops
;;;
;;;   (defun kernel-main () (let ((*print-base* 16)) (rdr2)))
;;;        => 90 IR ops (the save/set/restore triple)
;;;
;;;   ... (let ((*my-flag* t)) (declare (special *my-flag*)) (rdr3))
;;;        => 90 IR ops        <- the escape hatch, and the recommended fix
;;;
;;; Note the DEFVAR in probe 1: declaring the variable in the SAME blob does
;;; NOT help.  That is the whole trap — the source reads as obviously dynamic.
;;;
;;; Real damage already done: `(:default-initargs …)` had never actually been
;;; gated by *CLOS-APPLYING-DEFAULTS* because %MAKE-INSTANCE-LIST bound it with
;;; a LET.  It surfaced only during c2896ac, when the spread call moved out of
;;; the caller's frame, and was fixed at that one site with SETQ +
;;; UNWIND-PROTECT.  The general hazard is live, hence this check.  It is the
;;; THIRD distinct silent failure mode for LET-of-a-special, after a311671
;;; fixed (1) leaking the binding on a non-local exit and (2) save/restoring a
;;; CLHS 3.3.4 FREE declaration that establishes no binding at all.
;;;
;;; SEVERITY CLASSIFICATION.  Not every hit is a bug.  A LET of an earmuffed
;;; name that nothing outside the binding form ever reads is legal, if ugly:
;;; the lexical binding is exactly what the code gets and exactly what it uses.
;;; So each finding is graded:
;;;
;;;   DYNAMIC-INTENT  the name is DEFVAR/DEFPARAMETER'd in this blob AND is
;;;                   read or written free by at least one OTHER top-level
;;;                   form.  The binding is meant to be seen through a call.
;;;                   These are the true positives.
;;;   LEXICAL-ONLY    no other top-level form mentions the name.  Deliberate
;;;                   (or at least harmless) lexical use of an earmuffed name.
;;;
;;; Only DYNAMIC-INTENT findings are eligible to fail the build.
;;;
;;; KNOWN BLIND SPOTS, stated rather than discovered later:
;;;   * A LET inside a DEFMACRO template that expands at RUNTIME may be fine
;;;     even when reported: mvm-eval's compiler consults *RUNTIME-SPECIAL-NAMES*
;;;     (populated by runtime DEFVAR), which a host build cannot see.  None of
;;;     the findings measured at 6de6fc3 are of that shape — all are direct
;;;     DEFUN bodies — but a future one might be, and the fix
;;;     (`(declare (special *X*))') is correct either way.
;;;   * Only EARMUFFED names are scanned.  A special without earmuffs has the
;;;     same hazard and is invisible here; the convention is the signal.
;;;   * A blob over *GLOBAL-CHECK-MAX-BLOB-CHARS* is skipped along with checks
;;;     A and B — i.e. the four ANSI gate runners, which are not shipped.

(defvar *global-check-let-special-allowlist*
  '(;; -- add entries as "NAME"  ; reason
    )
  "Upcased names exempt from check C.  Prefer `(declare (special *X*))' at the
   LET — that FIXES the code instead of silencing the report.  This list means
   \"not a bug\"; use *GLOBAL-CHECK-LET-SPECIAL-BASELINE* for \"a bug we have
   not fixed yet\".")

;;; The ratchet, by VARIABLE NAME rather than by build script: every one of
;;; these lives in a source file that a dozen images bake (cl-reader.lisp,
;;; compiler.lisp, cl-eval.lisp), so a per-script table would be the same five
;;; names copied 27 times.  Measured at 6de6fc3 across all 25 buildable
;;; images; anything NOT here fails the build.
;;;
;;; SHRINK THIS TABLE.  It is a debt register, not a config knob.  Each entry
;;; is filed as task #248 and is fixed the same way: add
;;; `(declare (special *X*))' to the LET body.
(defvar *global-check-let-special-baseline*
  '(("*SHARP-LABELS*"
     "#248 FINDING 1 (unfixed, filed) — mvm/cl-reader.lisp READ /
      READ-PRESERVING-WHITESPACE / READ-FROM-STRING each `(let ((*sharp-labels*
      nil)) …)' to give the call a fresh #n= label table.  In-image that LET is
      lexical, so the per-READ reset never happens: %READ-SHARP-DISPATCH and
      friends read and PUSH onto the GLOBAL table, which therefore accumulates
      across every read for the life of the image and is never cleared.
      VERIFIED ON THE BINARY, not inferred — in the shipping x64 CLI:
        (read-from-string \"#1=(1 2 3)\")  =>  (1 2 3)
        *sharp-labels*                   =>  ((1 (1 2 3) . :SHARP-RESOLVED))
        (read-from-string \"#1#\")         =>  (1 2 3)
      That last one must signal a reader-error: #1# is undefined in a fresh
      READ.  It resolves through the leaked table instead.")
    ("*SUPPRESS-LOOP-BLOCK-NIL*"
     "#248 FINDING 2 (unfixed, filed) — mvm/compiler.lisp COMPILE-COMPOUND's
      %NAMED-LOOP arm binds it so the inner simple-LOOP does not establish its
      own (block nil …) (CLHS 6.1.2.2).  Lexical in-image ⇒ the self-hosted /
      eval2 compiler gives a NAMED loop a spurious extra NIL block.")
    ("*LET-SKIP-IMPLICIT-SPECIALS*"
     "#248 FINDING 3 (unfixed, filed) — mvm/compiler.lisp
      COMPILE-LET-WITH-SPECIALS' own recursion guard.  Its docstring says
      `otherwise the dispatch re-detects the same names and recurses forever'.
      Lexical in-image ⇒ the guard is INERT in every self-hosted compiler.
      Ironic and self-referential: the LET-of-a-special machinery is itself a
      victim of the LET-of-a-special hazard.")
    ("*UWP-SEQ-COUNTER*"
     "#248 FINDING 4 (unfixed, filed) — mvm/compiler.lisp MVM-COMPILE-ALL
      resets it per compilation unit.  Lexical in-image ⇒ never reset, so the
      counter runs monotonically across every eval2 compile in the image.
      VERIFIED ON THE BINARY: in the shipping x64 CLI, two successive
      `(defun f (x) (unwind-protect (+ x 1) nil))' forms leave
      *UWP-SEQ-COUNTER* reading 2 then 3, where MVM-COMPILE-ALL's
      `(let ((*uwp-seq-counter* 0)) …)' should have reset it to 0 each time.")
    ("*INIT-THUNK-NAMES*"
     "#248 FINDING 5 (unfixed, filed) — mvm/cl-eval.lisp
      %MVM-EVAL-COMPILE-TUPLE and MVM-EVAL-FORMS both bind it around a compile.
      Lexical in-image ⇒ the eval2 path accumulates init-thunk names globally
      instead of per-unit.")
    ("*AARCH64-TRANSLATED-START-IDX*"
     "#248 FINDING 6 (unfixed, filed) — mvm/translate-aarch64.lisp
      TRANSLATE-MVM-TO-AARCH64 let*-binds it, and its own docstring says
      `Expose to trap-time code (e.g. fn-addr patch site recorder) via dynamic
      variable'.  Lexical in-image ⇒ the seven `(or *aarch64-translated-start-
      idx* 0)' readers all see 0, so a self-hosted AArch64 build appending into
      a pre-filled boot buffer computes native offsets against the wrong base.
      Present in build-aarch64-cli / -linux / -ssh etc., absent from x64.")
    ;; ---- FINDING 7: one cluster, mvm/cross.lisp, build-modus-selfhost only.
    ;; ASSEMBLE-KERNEL-IMAGE binds seven translator hand-off variables in one
    ;; LET and BUILD-IMAGE binds *AARCH64-SERIAL-BASE*; every consumer is in
    ;; translate-aarch64.lisp, i.e. a DIFFERENT function reading the GLOBAL.
    ;; Lexical in-image ⇒ MODUS BUILDING AN IMAGE FROM INSIDE MODUS (the WS5
    ;; self-host path, and only that path — the host SBCL build is fine
    ;; because SBCL honours the DEFVARs) hands the AArch64 translator NIL for
    ;; the GC trampoline label, the handler push/pop labels, the generic
    ;; arithmetic bytecode offsets and the UART base.
    ("*AARCH64-GENSUB-BYTECODE-OFFSET*"   "#248 FINDING 7 (unfixed, filed) — cross.lisp ASSEMBLE-KERNEL-IMAGE cluster.")
    ("*AARCH64-GENMUL-BYTECODE-OFFSET*"   "#248 FINDING 7 (unfixed, filed) — cross.lisp ASSEMBLE-KERNEL-IMAGE cluster.")
    ("*AARCH64-GENADD-BYTECODE-OFFSET*"   "#248 FINDING 7 (unfixed, filed) — cross.lisp ASSEMBLE-KERNEL-IMAGE cluster.")
    ("*AARCH64-GC-COLLECT-BYTECODE-OFFSET*" "#248 FINDING 7 (unfixed, filed) — cross.lisp ASSEMBLE-KERNEL-IMAGE cluster.")
    ("*AARCH64-GC-TRAMPOLINE-LABEL*"      "#248 FINDING 7 (unfixed, filed) — cross.lisp ASSEMBLE-KERNEL-IMAGE cluster.")
    ("*AARCH64-HANDLER-POP-LABEL*"        "#248 FINDING 7 (unfixed, filed) — cross.lisp ASSEMBLE-KERNEL-IMAGE cluster.")
    ("*AARCH64-HANDLER-PUSH-LABEL*"       "#248 FINDING 7 (unfixed, filed) — cross.lisp ASSEMBLE-KERNEL-IMAGE cluster.")
    ("*AARCH64-SERIAL-BASE*"
     "#248 FINDING 7 (unfixed, filed) — cross.lisp BUILD-IMAGE's
      `(let ((*aarch64-serial-base* (or *aarch64-serial-base* serial-base
      #x09000000))) …)'.  The whole point of that LET is that the translator,
      a different function, reads the global; lexical in-image it reads NIL."))
  "Alist (NAME . reason) of KNOWN, FILED, UNFIXED check-C findings.  Still
   PRINTED every build; they just do not fail it.")

(defun %gck-earmuffed-p (name)
  (and (stringp name) (> (length name) 2)
       (char= (char name 0) #\*)
       (char= (char name (1- (length name))) #\*)))

(defun %gck-registered-special-p (name)
  "Exactly the test compile-form's LET dispatch applies at HOST-BUILD time.
   (c) *RUNTIME-SPECIAL-NAMES* is deliberately not consulted: it is empty
   unless *MVM-EVAL-RUNTIME-P*, which no host build sets."
  (or (gethash (compute-name-hash name) (%ensure-clhs-specials-table))
      (and (boundp '*clhs-extra-specials*)
           (member name *clhs-extra-specials* :test #'string=))))

(defun %gck-let-declared-specials (body)
  "Names in a leading (declare (special …)) of BODY — the compiler's own
   EXTRACT-SPECIAL-VARS, so the two agree by construction."
  (handler-case (mapcar #'%gck-name (extract-special-vars body))
    (error () nil)))

(defun %gck-walk-lets (form acc)
  "Push (NAME . KIND) for every LET/LET* binding of an earmuffed name that the
   compiler will NOT dynamically bind.  QUOTE is not descended into (data);
   backquote templates ARE (they become code at every expansion site)."
  (cond
    ((consp form)
     (let ((h (%gck-name (car form))))
       (cond
         ((equal h "QUOTE") (return-from %gck-walk-lets acc))
         ((member h '("LET" "LET*") :test #'equal)
          (let ((bindings (cadr form))
                (declared (%gck-let-declared-specials (cddr form))))
            (when (listp bindings)
              (dolist (b bindings)
                (let ((n (%gck-name (if (consp b) (car b) b))))
                  (when (and n (%gck-earmuffed-p n)
                             (not (%gck-registered-special-p n))
                             (not (member n declared :test #'equal))
                             (not (member n *global-check-let-special-allowlist*
                                          :test #'equal)))
                    (push (cons n h) acc)))))))))
     (let ((tail form))
       (loop while (consp tail)
             do (setf acc (%gck-walk-lets (car tail) acc))
                (setf tail (cdr tail)))
       (when tail (setf acc (%gck-walk-lets tail acc)))))
    ((and (vectorp form) (not (stringp form)))
     (loop for i from 0 below (length form)
           do (setf acc (%gck-walk-lets (aref form i) acc))))
    (t (let ((inner (%gck-comma-expr form)))
         (when inner (setf acc (%gck-walk-lets inner acc))))))
  acc)

(defun %gck-toplevel-label (form)
  (if (and (consp form) (%gck-name (car form)))
      (format nil "~A ~A" (%gck-name (car form))
              (or (%gck-name (cadr form))
                  (and (consp (cadr form)) (%gck-name (car (cadr form))))
                  ""))
      "<form>"))

(defun check-let-of-unregistered-special (forms &key (label (%gck-build-label))
                                                     (lines nil))
  "Check C.  FORMS is the already-read top-level form list of the image blob.
   Returns the list of DYNAMIC-INTENT finding strings."
  (let ((mode (%gck-mode)))
    (when (eq mode :off) (return-from check-let-of-unregistered-special nil))
    (let ((declared-vars (make-hash-table :test 'equal))   ; name -> t
          (mentions (make-hash-table :test 'equal))        ; name -> count of TL forms mentioning it
          (hits nil)                                        ; (name kind tl-label line)
          (baseline (%gck-baseline-entry label)))
      ;; Pass 1: which names does this blob DECLARE, and how many distinct
      ;; top-level forms mention each?  A name mentioned by exactly the one
      ;; form that LET-binds it cannot be observed dynamically by anyone.  The
      ;; DEFVAR itself is NOT counted — otherwise every declared-and-let-bound
      ;; name would clear the >1 bar on its own declaration.
      (dolist (form forms)
        (let ((declp nil))
          (when (consp form)
            (let ((h (%gck-name (car form))))
              (when (member h '("DEFVAR" "DEFPARAMETER" "DEFCONSTANT") :test #'equal)
                (setf declp t)
                (let ((n (%gck-name (cadr form))))
                  (when n (setf (gethash n declared-vars) t))))))
          (unless declp
            (let ((seen (%gck-collect-symbols form (make-hash-table :test 'equal))))
              (maphash (lambda (s v) (declare (ignore v))
                         (when (%gck-earmuffed-p s)
                           (incf (gethash s mentions 0))))
                       seen)))))
      ;; Pass 2: the LET scan, per top-level form so findings carry a location.
      (let ((lv (and lines (coerce lines 'vector)))
            (i -1))
        (dolist (form forms)
          (incf i)
          (let ((found (%gck-walk-lets form nil)))
            (dolist (f (remove-duplicates found :test #'equal))
              (push (list (car f) (cdr f) (%gck-toplevel-label form)
                          (and lv (< i (length lv)) (aref lv i)))
                    hits)))))
      (setf hits (nreverse hits))
      (let ((dynamic nil) (lexical nil) (baselined nil))
        (dolist (h hits)
          (destructuring-bind (name kind tl line) h
            (let* ((declaredp (gethash name declared-vars))
                   (mentioned (gethash name mentions 0))
                   (dynamicp (and declaredp (> mentioned 1)))
                   (s (format nil "LET-OF-UNREGISTERED-SPECIAL ~A in ~A~@[ (blob line ~D)~] — ~
the ~A binds it LEXICALLY; the global is never set, so callees read the old ~
value.~:[~; The name is DEFVAR'd here and mentioned by ~:*~D top-level forms.~]"
                               name tl line kind (and dynamicp mentioned))))
              (cond ((not dynamicp) (push s lexical))
                    ((assoc name *global-check-let-special-baseline* :test #'equal)
                     (push s baselined))
                    (t (push s dynamic))))))
        (setf dynamic (nreverse dynamic)
              lexical (nreverse lexical)
              baselined (nreverse baselined))
        (format t "~&;; check-let-of-unregistered-special (~A): ~D LET/LET* of an ~
unregistered earmuffed name — ~D DYNAMIC-INTENT (~D baselined), ~D LEXICAL-ONLY~%"
                label (length hits) (+ (length dynamic) (length baselined))
                (length baselined) (length lexical))
        (when lexical
          (format t "~{;;   [lexical-only] ~A~%~}"
                  (mapcar (lambda (s) (subseq s 0 (min 150 (length s)))) lexical)))
        (when baselined
          (format t "~{;;   [known #248] ~A~%~}"
                  (mapcar (lambda (s) (subseq s 0 (min 150 (length s)))) baselined)))
        (finish-output)
        (when dynamic
          (let ((suppress (member :suppress-check-c baseline))
                (msg (format nil "~&~%*** check-let-of-unregistered-special (~A): ~D ~
finding~:P ***~%~
`(let ((*X* v)) …)' where *X* is not a CLHS-standard special, not in~%~
*CLHS-EXTRA-SPECIALS*, and not `(declare (special *X*))'d in the LET body,~%~
compiles to a PLAIN LEXICAL LET (task #248, mvm/build-checks.lisp).  The~%~
global is never assigned, so every callee sees the OLD value and nothing~%~
warns.  FIX: add `(declare (special *X*))' to the LET body (measured to~%~
restore the save/set/restore triple), or use SETQ + UNWIND-PROTECT.~%~
Set MODUS_GLOBAL_CHECK=warn to downgrade, =0 to disable.~%~%~{  - ~A~%~}~%"
                             label (length dynamic) dynamic)))
            (cond
              (suppress
               (format t "~&;; check-let-of-unregistered-special (~A): ~D KNOWN ~
finding~:P, baselined:~%~{;;   - ~A~%~}"
                       label (length dynamic)
                       (mapcar (lambda (s) (subseq s 0 (min 150 (length s)))) dynamic))
               (finish-output))
              ((eq mode :warn) (format t "~A" msg) (finish-output))
              (t (format t "~A" msg) (finish-output)
                 (%gck-fail "check-let-of-unregistered-special failed")))))
        dynamic))))

;;; ============================================================
;;; CHECK D (task #249) — the MVM compiler SUBSTITUTES NIL and only WARNs
;;; ============================================================
;;;
;;; compile-form's terminal cond arm (mvm/compiler.lisp ~4663) is
;;;
;;;     ;; Unrecognized — warn and compile as nil
;;;     (t (format t "  WARN: cannot compile ~S, using nil~%" form)
;;;        (compile-nil dest))
;;;
;;; The host build SUCCEEDS and an image IS emitted.  A backquote nested
;;; DIRECTLY inside a comma — `,@(or writer-body \`(… ,min-inst-len …))` —
;;; reads fine under SBCL, so check-parses is happy and the host-side sources
;;; are correct; but when the MVM compiler compiles that source INTO the image
;;; the inner COMMA structure reaches this arm and becomes NIL.  In the
;;; instance that surfaced this (during 6de6fc3) the in-image DEFSTRUCT then
;;; built a setter template with NIL where the slot index belonged and every
;;; runtime `(defstruct s a b)' died during expansion.  It was found only by
;;; diffing the build's WARN histogram against a baseline — so: make that
;;; histogram a first-class build artifact, and ratchet it.
;;;
;;; The compiler has several such "warn and substitute something" paths.  This
;;; check tees *STANDARD-OUTPUT* and *ERROR-OUTPUT* across the whole of
;;; BUILD-IMAGE (which is where COMPILE-SOURCE-TO-MODULE runs), buckets every
;;; WARN line by SHAPE, and compares the shape histogram to the per-build-
;;; script baseline in *GLOBAL-CHECK-WARN-BASELINE*.
;;;
;;; MEASURED on build-generic-cli at 6de6fc3: CANNOT-COMPILE = 0.  Zero, so it
;;; is FATAL — see the baseline table for what is nonzero and why.

(defvar *global-check-warn-shapes*
  ;; (KEY  MARKER  FATAL-P  DESCRIPTION)
  ;; Order matters: the first marker found in the line wins, so more specific
  ;; markers must precede their prefixes.
  '((:cannot-compile "WARN: cannot compile " t
     "compile-form fell through to its terminal cond arm and SUBSTITUTED NIL for
      a form it does not understand.  The build still succeeds and an image is
      still written; the form is simply gone.  Task #249.")
    (:implicit-global-setq "WARN: implicit global setq " nil
     "compile-setq assigned a name with no binding and no declaration — a global
      write.  Usually a typo or a missing let; occasionally deliberate.")
    (:implicit-global "WARN: implicit global " nil
     "compile-variable-ref read a name with no lexical binding as a global.  A
      typo reads back NIL forever.")
    (:unresolved-function "WARN li-func: unresolved function " nil
     "#'NAME where NAME is defined nowhere in the blob — an explicit NIL
      sentinel is emitted, so `(funcall #'NAME …)' faults at runtime.")
    (:bad-callable "WARN compile-call: " nil
     "A list-headed non-lambda in function position (CLAUDE.md, Source-Quality
      Guardrails).  Deliberate in ANSI corpus code that constructs bad
      callables.")
    (:unknown-loop-clause "WARN: unknown FOR clause " nil
     "expand-cl-loop dropped a FOR clause it does not implement.")
    (:unknown-being-kind "WARN: unknown BEING kind " nil
     "expand-cl-loop dropped a BEING clause it does not implement.")
    (:unknown-go-tag "WARN: unknown GO tag " nil
     "compile-go could not resolve a tag."))
  "Known WARN shapes the MVM compiler emits while compiling a blob.  A line
   containing \"WARN\" that matches NONE of these is an UNKNOWN shape and fails
   the build: a new silent-substitution path has been added and nobody decided
   whether it is acceptable.")

(defvar *global-check-warn-baseline*
  '(("build-generic-cli"
     (:implicit-global . 41)
     (:implicit-global-setq . 123)
     (:unresolved-function . 40)))
  "Per-build-script (LABEL . ((SHAPE . COUNT) …)) for the NON-fatal shapes.
   A FATAL shape (today only :CANNOT-COMPILE) ratchets at 0 in EVERY build
   script whether or not it has a row here — that is the point of #249.  A
   non-fatal shape is only compared where a row exists, so the 20-odd images
   nobody has measured stay quiet instead of emitting a wall of noise about a
   baseline that was never taken.  Measured at 6de6fc3.")

(defun %gck-warn-shape (line)
  (dolist (spec *global-check-warn-shapes*)
    (when (search (second spec) line) (return spec))))

(defun %gck-warn-line-p (line)
  "T for lines the MVM COMPILER emits.  Deliberately strict — the compiler's
   warnings all start the line (after indentation and an optional `;; '
   comment prefix) with the token WARN.  A loose (SEARCH \"WARN\") would also
   catch SBCL's own host-compiler chatter (`; caught WARNING:'), a build
   script echoing a filename, and `NOTE: redefining WARN' — and since an
   unrecognised WARN shape FAILS the build, a loose test here would make the
   check a liability rather than a ratchet."
  (let ((i 0) (n (length line)))
    (loop while (and (< i n) (member (char line i) '(#\Space #\Tab))) do (incf i))
    (when (and (< (+ i 2) n) (char= (char line i) #\;) (char= (char line (1+ i)) #\;))
      (incf i 2)
      (loop while (and (< i n) (member (char line i) '(#\Space #\Tab))) do (incf i)))
    (and (<= (+ i 4) n) (string= "WARN" line :start2 i :end2 (+ i 4)))))

;;; A minimal Gray tee: everything still reaches the real stream, and matching
;;; lines are additionally bucketed.  A broadcast-stream into a string-output-
;;; stream would work too but buffers the ENTIRE build log (the ANSI gate
;;; runners print tens of MB); this is O(1) memory in the log size.
(defclass gck-tee-stream (sb-gray:fundamental-character-output-stream)
  ((under :initarg :under :reader gck-tee-under)
   (buf   :initform (make-array 128 :element-type 'character
                                    :adjustable t :fill-pointer 0)
          :reader gck-tee-buf)
   (sink  :initarg :sink :reader gck-tee-sink)))

(defun %gck-tee-flush-line (s)
  (let ((buf (gck-tee-buf s)))
    (when (plusp (fill-pointer buf))
      (let ((line (coerce buf 'simple-string)))
        (when (%gck-warn-line-p line) (funcall (gck-tee-sink s) line)))
      (setf (fill-pointer buf) 0))))

(defmethod sb-gray:stream-write-char ((s gck-tee-stream) ch)
  (write-char ch (gck-tee-under s))
  (if (char= ch #\Newline)
      (%gck-tee-flush-line s)
      (vector-push-extend ch (gck-tee-buf s)))
  ch)

(defmethod sb-gray:stream-write-string ((s gck-tee-stream) string
                                        &optional (start 0) end)
  (let ((end (or end (length string))))
    (write-string string (gck-tee-under s) :start start :end end)
    (loop for i from start below end
          for ch = (char string i)
          do (if (char= ch #\Newline)
                 (%gck-tee-flush-line s)
                 (vector-push-extend ch (gck-tee-buf s)))))
  string)

(defmethod sb-gray:stream-line-column ((s gck-tee-stream)) nil)
(defmethod sb-gray:stream-force-output ((s gck-tee-stream))
  (force-output (gck-tee-under s)))
(defmethod sb-gray:stream-finish-output ((s gck-tee-stream))
  (finish-output (gck-tee-under s)))

(defun check-compiler-warns (hist unknown &key (label (%gck-build-label)))
  "Compare the WARN-shape histogram HIST (hash SHAPE -> count) collected while
   BUILD-IMAGE ran against *GLOBAL-CHECK-WARN-BASELINE*.  UNKNOWN is a list of
   sample WARN lines that matched no known shape."
  (let* ((mode (%gck-mode))
         (row (cdr (assoc label *global-check-warn-baseline* :test #'equal)))
         (fatal nil)
         (noted nil)
         (shapes (sort (mapcar #'first *global-check-warn-shapes*) #'string<
                       :key #'symbol-name)))
    (when (eq mode :off) (return-from check-compiler-warns nil))
    (format t "~&;; check-compiler-warns (~A): ~{~A~^ ~}~%"
            label
            (or (loop for k in shapes
                      for n = (gethash k hist 0)
                      when (plusp n) collect (format nil "~(~A~)=~D" k n))
                '("no compiler WARNs")))
    (dolist (spec *global-check-warn-shapes*)
      (destructuring-bind (key marker fatalp desc) spec
        (declare (ignore marker desc))
        (let ((n (gethash key hist 0))
              (base (or (cdr (assoc key row)) 0)))
          ;; Non-fatal shapes are only ratcheted where a baseline row was
          ;; actually MEASURED; a missing row is "unknown", not "zero".
          (when (and (> n base) (or fatalp row))
            (let ((s (format nil "~(~A~) = ~D (baseline ~D) — ~D NEW instance~:P"
                             key n base (- n base))))
              (if fatalp (push s fatal) (push s noted)))))))
    (dolist (line unknown)
      (push (format nil "UNKNOWN WARN SHAPE: ~A" line) fatal))
    (setf fatal (nreverse fatal) noted (nreverse noted))
    (when noted
      (format t "~&;; check-compiler-warns (~A): ~D non-fatal shape~:P above ~
baseline:~%~{;;   - ~A~%~}" label (length noted) noted)
      (finish-output))
    (when fatal
      (let ((msg (format nil "~&~%*** check-compiler-warns (~A): ~D finding~:P ***~%~
The MVM compiler warned and SUBSTITUTED something while compiling this image.~%~
The build otherwise succeeds and the binary is written, so this is the~%~
silent-degradation class task #249 exists for — most often a backquote nested~%~
DIRECTLY inside a comma, which SBCL reads fine but the MVM compiler turns into~%~
NIL (see mvm/cl-eval.lisp's DEFSTRUCT accessor emitter for the fix pattern:~%~
bind the inner template to a variable at top level and splice the variable).~%~
Set MODUS_GLOBAL_CHECK=warn to downgrade, =0 to disable.~%~%~{  - ~A~%~}~%"
                         label (length fatal) fatal)))
        (ecase mode
          ((:warn) (format t "~A" msg) (finish-output))
          ((:error :force :dump)
           (format t "~A" msg) (finish-output)
           (%gck-fail "check-compiler-warns failed")))))
    fatal))

;;; ============================================================
;;; CHECK E: SOURCE-PARSES — every first-party .lisp reads (task #252)
;;; ============================================================
;;; CHECK-PARSES (cross.lisp) already fails a build loudly on a file that does
;;; not read.  Its blind spot is COVERAGE: a build script only calls it on the
;;; files THAT build concatenates, so a file no routine build touches can carry
;;; a read error indefinitely.  Two did, both found by hand at #252, both the
;;; same shape — a prose comment containing a "quoted phrase", written INSIDE a
;;; Lisp string literal, so the first inner quote terminated the string and the
;;; prose became source:
;;;
;;;   boot/boot-linux-i386.lisp:147  (in a docstring)  -> build-i386-cli
;;;       "Comma not inside a backquote."          broke at 7fcee4c
;;;   mvm/build-mvm.lisp:340  (in *adapter-source*)    -> build-mvm
;;;       "illegal terminating character after a colon"  broke at 1461e65
;;;
;;; Neither file is in any gate, so neither was read for weeks.  i386 is one of
;;; the three platforms in the release gate (#203) and that side was not merely
;;; unmeasured, it was UNBUILDABLE.
;;;
;;; So: sweep EVERY first-party .lisp in the tree once per build process,
;;; whichever build it is — including the build scripts themselves, which
;;; CHECK-PARSES structurally cannot cover (a build script is read by SBCL
;;; before any Modus code runs).  Cost is ~1.5 s for 162 files; the sweep runs
;;; once per process, not once per BUILD-IMAGE call.
;;;
;;; FALSE POSITIVES, measured on the whole tree.  Three files reference
;;; packages that exist only inside a RUNNING image (CHIPZ, UIOP/ASDF, and the
;;; Genera SCL/SYS/PROCESS/SI/CLI set).  Two of the three are cured by creating
;;; the missing package on demand and deleting it again afterwards.  The third,
;;; lib/install-tarball.lisp, uses `chipz:<external-symbol>' — a stub package
;;; has no such external, and there is no way to satisfy that without a symbol
;;; oracle — so it is excluded BY NAME, not by a pattern.  Keep that list at
;;; three or fewer; a growing exclusion list means this check is being worked
;;; around rather than used.
(defvar *gck-parse-sweep-root*
  (and *load-truename*
       (make-pathname :directory (butlast (pathname-directory *load-truename*))
                      :name nil :type nil :defaults *load-truename*))
  "Repository root, derived at LOAD time from <root>/mvm/build-checks.lisp.")

(defvar *gck-parse-sweep-dirs* '("boot" "lib" "mvm" "net" "runtime"))

(defvar *gck-parse-sweep-exclusions* '("install-tarball")
  "Pathname-names (no directory, no type) excluded from CHECK E.  See the
   FALSE POSITIVES note above; each entry needs a reason there.")

(defvar *gck-parse-sweep-done* nil
  "CHECK E is tree-wide, not blob-wide, so it is worth running once per SBCL
   process — not once per BUILD-IMAGE call (the fixpoint build makes several).")

(defun %gck-parse-one (path stub-sink)
  "Read PATH the way CHECK-PARSES does.  Returns NIL on success or a one-line
   description of the reader error.  A `package X does not exist' error is not
   a parse failure — create X, hand its name to STUB-SINK for later deletion,
   and retry the file."
  (loop for attempt from 0 below 16 do
    (let ((outcome
            (handler-case
                (with-open-file (f path)
                  (let ((*package* (or (find-package :modus.mvm) *package*)))
                    (loop for next = (read f nil :eof) until (eq next :eof)))
                  nil)
              (package-error (e)
                (let ((p (package-error-package e)))
                  ;; A STRING designator means "no such package" — recoverable.
                  ;; A PACKAGE object means "symbol not external in it" — real
                  ;; for our purposes, and not a paren bug, so report it.
                  (if (and p (or (stringp p) (symbolp p)) (not (find-package p)))
                      (progn (make-package (string p) :use '(:cl))
                             (funcall stub-sink (string p))
                             :retry)
                      (%gck-one-line e))))
              (error (e) (%gck-one-line e)))))
      (unless (eq outcome :retry) (return outcome))))
  )

(defun %gck-one-line (e)
  "One-line rendering of a reader error.  SBCL appends `Stream: #<...>' to
   every READER-ERROR report; it is pure noise once the file is named, and it
   would eat the whole 160-char budget."
  (let* ((s (substitute #\Space #\Newline (princ-to-string e)))
         (cut (search "Stream: " s))
         (s (string-trim " " (if cut (subseq s 0 cut) s))))
    (subseq s 0 (min 160 (length s)))))

(defun check-source-parses ()
  "CHECK E.  Every .lisp under *GCK-PARSE-SWEEP-DIRS* must read cleanly."
  (let ((mode (%gck-mode)))
    (when (or (eq mode :off) *gck-parse-sweep-done* (null *gck-parse-sweep-root*))
      (return-from check-source-parses nil))
    (setf *gck-parse-sweep-done* t)
    (let ((files (sort (loop for d in *gck-parse-sweep-dirs*
                             append (directory
                                     (merge-pathnames
                                      (make-pathname :directory (list :relative d)
                                                     :name :wild :type "lisp")
                                      *gck-parse-sweep-root*)))
                       #'string< :key #'namestring))
          (stubs nil)
          (findings nil))
      (unwind-protect
           (dolist (p files)
             (unless (member (pathname-name p) *gck-parse-sweep-exclusions*
                             :test #'string-equal)
               (let ((e (%gck-parse-one p (lambda (n) (push n stubs)))))
                 (when e
                   (push (format nil "~A — ~A"
                                 (enough-namestring p *gck-parse-sweep-root*) e)
                         findings)))))
        (dolist (s stubs) (ignore-errors (delete-package s))))
      (setf findings (nreverse findings))
      (format t "~&;; check-source-parses: ~D file~:P swept, ~D unreadable~%"
              (length files) (length findings))
      (finish-output)
      (when findings
        (let ((msg (format nil "~&~%*** check-source-parses: ~D file~:P do not read ***~%~
A first-party source file failed SBCL's READ.  This is the #252 class: the~%~
file builds nothing until it is fixed, and nothing else in the tree notices~%~
because no gate reads it.  Common cause: a \"quoted phrase\" inside a Lisp~%~
string literal or docstring — the inner quote ENDS the string.~%~
Set MODUS_GLOBAL_CHECK=warn to downgrade, =0 to disable.~%~%~{  - ~A~%~}~%"
                           (length findings) findings)))
          (format t "~A" msg) (finish-output)
          (unless (eq mode :warn)
            (%gck-fail "check-source-parses failed"))))
      findings)))

;;; ============================================================
;;; Wiring: encapsulate BUILD-IMAGE
;;; ============================================================
;;; Every build script funnels through BUILD-IMAGE, so wrapping it here covers
;;; all of them at once — including the ANSI gate runners and the ones whose
;;; kernel-main lives in a string literal.  Encapsulating (rather than editing
;;; cross.lisp) keeps every BAKED source file byte-identical: cross.lisp is
;;; itself compiled into the selfhost and fixpoint images.
;;;
;;; Checks A/B (globals) and C (LET-of-special) run BEFORE, on the blob text.
;;; Check D can only run AFTER, because the WARNs it counts are emitted by the
;;; compile that BUILD-IMAGE performs.  D therefore fails the build after the
;;; image is assembled but before the script writes it — which is what we want:
;;; a failed check must not leave a binary behind.

(defvar *build-image-unwrapped* nil)

(defun %gck-pre-checks (src)
  "Read the blob ONCE and run every source-level check on it."
  (let ((mode (%gck-mode)))
    (when (eq mode :off) (return-from %gck-pre-checks nil))
    ;; E is tree-wide and blob-independent, so it runs first and unconditionally
    ;; (the size cap below is about re-reading a 17 MB blob, not about E).
    (%gck-guard "check-source-parses" (check-source-parses))
    ;; check-global-inits owns the =dump / size-cap policy; let it run first
    ;; and only pre-read when it would not have skipped anyway.
    (if (or (eq mode :dump)
            (and (not (eq mode :force))
                 (> (length src) *global-check-max-blob-chars*)))
        (%gck-guard "check-global-inits" (check-global-inits src))
        (let ((fl (handler-case (read-all-forms-with-locations src)
                    (error (e)
                      (format t "~&;; build-checks: unreadable blob (~A) — skipped~%" e)
                      nil))))
          (%gck-guard "check-global-inits" (check-global-inits src :pre-read fl))
          (when fl
            (%gck-guard "check-let-of-unregistered-special"
              (check-let-of-unregistered-special
               (if (consp fl) (car fl) fl)
               :lines (and (consp fl) (cdr fl)))))))))

(unless *build-image-unwrapped*
  (setf *build-image-unwrapped* #'build-image)
  (setf (fdefinition 'build-image)
        (lambda (&rest args)
          ;; Captured FIRST — before a single check runs — so that the value
          ;; handed back to the real BUILD-IMAGE below is exactly the one it
          ;; would have seen with no checks installed at all.  See the
          ;; REPRODUCIBILITY PIN at the top of this file.
          (let* ((gs *gensym-counter*)
                 (src (or (getf args :source-text)
                          (let ((forms (getf args :source)))
                            (when forms
                              (with-output-to-string (s)
                                (dolist (f forms) (prin1 f s) (terpri s)))))))
                 (%pre (when src (%gck-pre-checks src)))
                 (hist (make-hash-table :test 'eq))
                 (unknown nil)
                 (nunknown 0)
                 (sink (lambda (line)
                         (let ((spec (%gck-warn-shape line)))
                           (if spec
                               (incf (gethash (first spec) hist 0))
                               (progn (incf nunknown)
                                      (when (< (length unknown) 10)
                                        (pushnew line unknown :test #'equal)))))))
                 (result
                   (if (eq (%gck-mode) :off)
                       (progn (setf *gensym-counter* gs)
                              (apply *build-image-unwrapped* args))
                       (let* ((out (make-instance 'gck-tee-stream
                                                  :under *standard-output* :sink sink))
                              (err (make-instance 'gck-tee-stream
                                                  :under *error-output* :sink sink))
                              (*standard-output* out)
                              (*error-output* err))
                         ;; Warm PCL's discriminating functions for the tee's
                         ;; methods against a throwaway stream.  Computing a
                         ;; dfun the first time a new class reaches
                         ;; STREAM-WRITE-STRING costs a GENSYM, and left
                         ;; unwarmed it is spent INSIDE the build, past the
                         ;; pin below — measured as a stubborn +1 that shifted
                         ;; every NAT<n> name in the image by one.
                         (let ((warm (make-instance 'gck-tee-stream
                                                    :under (make-broadcast-stream)
                                                    :sink (lambda (l) (declare (ignore l))))))
                           (write-char #\x warm)
                           (write-string "warm" warm)
                           (fresh-line warm)
                           (terpri warm)
                           (force-output warm)
                           (finish-output warm))
                         ;; See the REPRODUCIBILITY PIN at the top of this
                         ;; file: hand the real BUILD-IMAGE the exact counter
                         ;; it would have seen with no checks installed.
                         (setf *gensym-counter* gs)
                         (unwind-protect (apply *build-image-unwrapped* args)
                           (%gck-tee-flush-line out)
                           (%gck-tee-flush-line err))))))
            (declare (ignorable nunknown %pre))
            (%gck-guard "check-compiler-warns"
              (check-compiler-warns hist (nreverse unknown)))
            result))))

;;; ------------------------------------------------------------------
;;; REPRODUCIBILITY PIN (2 of 2) — see the header.  Must be the LAST form.
;;; Restores CL:*GENSYM-COUNTER* to its value on entry so that loading this
;;; host-only file cannot shift the "NAT<n>" gensym names the MVM compiler
;;; bakes into every image.  Without it, editing this file changes 435 bytes
;;; of the emitted binary for no semantic reason.
(setf *gensym-counter* *gck-gensym-counter-at-load*)
