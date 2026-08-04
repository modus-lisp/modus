;;;; read-package-scope.lisp — #211 regression test for the build reader's
;;;; package handling.
;;;;
;;;; Two properties, which must hold TOGETHER:
;;;;
;;;;   (A) READ-ALL-FORMS-WITH-LOCATIONS honours a file's own (in-package …),
;;;;       so SYMBOL-PACKAGE is authoritative for unqualified symbols and not
;;;;       just for explicitly-qualified ones.
;;;;
;;;;   (B) That switch does NOT leak past the file that made it.  The build
;;;;       hands the reader one giant CONCATENATED blob, so without MVM-TEXT's
;;;;       per-file reset a declaration in file A would silently change the
;;;;       read package for file B, the harness, and the whole ANSI corpus —
;;;;       which measured NET −23 (loop14 −16, structures-03 −5: LOOP's
;;;;       %loop-parse-cond-clauses bound MODUS.MVM::IT while the corpus read
;;;;       MODUS.ASM::IT, so `COLLECT IT` became a free variable).
;;;;
;;;; Run:  sbcl --script tests/read-package-scope.lisp

(load (merge-pathnames "../lib/load-mvm.lisp" *load-truename*))

(in-package :cl-user)

(defvar *fails* 0)

(defun chk (name got want)
  (if (equal got want)
      (format t "  PASS ~A~%" name)
      (progn (incf *fails*)
             (format t "  FAIL ~A: got ~S want ~S~%" name got want))))

(defun pkg-of (form-sym)
  (let ((p (symbol-package form-sym)))
    (if p (package-name p) "#:UNINTERNED")))

;;; A file's text, exactly as MVM-TEXT would hand it over.
(defun wrapped (text)
  (modus.mvm::%build-package-scoped-source text))

(defun forms-of (blob)
  (car (modus.mvm::read-all-forms-with-locations blob)))

(format t "~%#211 read-package scope~%")

;;; ---------------------------------------------------------------
;;; (A) the switch works at all
;;; ---------------------------------------------------------------
(let* ((blob (wrapped "(in-package :modus.asm)
(defun probe-a (it) it)
"))
       (forms (forms-of blob))
       ;; last real form is the defun (the trailing reset is an in-package)
       (defun-form (find-if (lambda (f)
                              (and (consp f)
                                   (symbolp (car f))
                                   (string= (symbol-name (car f)) "DEFUN")))
                            forms)))
  (chk "A: in-package is honoured (defun name interns in MODUS.ASM)"
       (pkg-of (cadr defun-form)) "MODUS.ASM")
  (chk "A: and so does its lambda list"
       (pkg-of (car (caddr defun-form))) "MODUS.ASM"))

;;; ---------------------------------------------------------------
;;; (B) the switch does not leak into the NEXT file
;;; ---------------------------------------------------------------
;;; File A declares a package; file B declares none.  Concatenated exactly the
;;; way a build script concatenates them.  B's symbols must land in MODUS.MVM.
(let* ((file-a (wrapped "(in-package :modus.asm)
(defun probe-a (it) it)
"))
       (file-b (wrapped "(defun probe-b (it) it)
"))
       (blob (concatenate 'string file-a (string #\Newline) file-b))
       (forms (forms-of blob))
       (defuns (remove-if-not (lambda (f)
                                (and (consp f)
                                     (symbolp (car f))
                                     (string= (symbol-name (car f)) "DEFUN")))
                              forms)))
  (chk "B: two defuns read" (length defuns) 2)
  (chk "B: file A's symbols stay in MODUS.ASM"
       (pkg-of (cadr (first defuns))) "MODUS.ASM")
  (chk "B: file B's symbols land in MODUS.MVM (NO LEAK)"
       (pkg-of (cadr (second defuns))) "MODUS.MVM")
  (chk "B: file B's IT is MODUS.MVM::IT — the loop14 discriminator"
       (pkg-of (car (caddr (second defuns)))) "MODUS.MVM"))

;;; ---------------------------------------------------------------
;;; (B') an UNWRAPPED trailing chunk (an inline source string spliced by a
;;; build script) is also protected, because file A's text ends with a reset.
;;; ---------------------------------------------------------------
(let* ((blob (concatenate 'string
                          (wrapped "(in-package :modus.asm)
(defun probe-a (it) it)
")
                          (string #\Newline)
                          "(defun probe-inline (it) it)"))
       (forms (forms-of blob))
       (inline-defun (find-if (lambda (f)
                                (and (consp f) (symbolp (car f))
                                     (string= (symbol-name (car f)) "DEFUN")
                                     (string= (symbol-name (cadr f)) "PROBE-INLINE")))
                              forms)))
  (chk "B': inline (unwrapped) chunk after a switching file reads in MODUS.MVM"
       (pkg-of (cadr inline-defun)) "MODUS.MVM"))

;;; ---------------------------------------------------------------
;;; (C) leniency preserved: an unknown package must not error, and must not
;;; switch.  The same reader ingests ANSI fixtures whose packages need not
;;; exist host-side.
;;; ---------------------------------------------------------------
(let* ((blob (wrapped "(in-package :no-such-package-211)
(defun probe-c (it) it)
"))
       (forms (forms-of blob))
       (defun-form (find-if (lambda (f)
                              (and (consp f) (symbolp (car f))
                                   (string= (symbol-name (car f)) "DEFUN")))
                            forms)))
  (chk "C: unknown package leaves *package* untouched (no error, no switch)"
       (pkg-of (cadr defun-form)) "MODUS.MVM"))

;;; ---------------------------------------------------------------
;;; (D) the wrap is line-neutral: it must not shift any reported source line.
;;; ---------------------------------------------------------------
(let* ((raw "(defun l1 ())
(defun l2 ())
(defun l3 ())
")
       (raw-lines (cdr (modus.mvm::read-all-forms-with-locations raw)))
       (wrapped-lines (cdr (modus.mvm::read-all-forms-with-locations (wrapped raw)))))
  ;; wrapped adds a leading in-package on line 1 and a trailing one on the
  ;; last line; the three defuns must keep lines 1/2/3.
  (chk "D: wrapping does not shift source lines"
       (coerce (subseq wrapped-lines 1 4) 'list)
       (coerce raw-lines 'list)))

;;; ---------------------------------------------------------------
;;; (E) THE ENABLING PROPERTY — what the switch actually buys.
;;;
;;; DEFINE-REGISTERS, the one directly observable win the earlier attempts
;;; cited, was deleted as vestigial on main (b26e5f4), so there is no longer a
;;; user-visible behaviour that flips.  What remains is the property #210 needs:
;;; two build-baked files that declare different packages now produce
;;; DISTINGUISHABLE (package, name) pairs for the same name.  Before this
;;; change both read as the single symbol MODUS.MVM::SAME-NAME and the pair was
;;; indistinguishable — which is exactly why the function table could only ever
;;; be keyed on the bare name.
;;; ---------------------------------------------------------------
(let* ((blob (concatenate 'string
                          (wrapped "(in-package :modus.asm)
(defun same-name () 1)
")
                          (string #\Newline)
                          (wrapped "(in-package :modus.mvm.x64)
(defun same-name () 2)
")))
       (defuns (remove-if-not (lambda (f)
                                (and (consp f) (symbolp (car f))
                                     (string= (symbol-name (car f)) "DEFUN")))
                              (forms-of blob)))
       (a (cadr (first defuns)))
       (b (cadr (second defuns))))
  (chk "E: same NAME in both" (list (symbol-name a) (symbol-name b))
       (list "SAME-NAME" "SAME-NAME"))
  (chk "E: different PACKAGE" (list (pkg-of a) (pkg-of b))
       (list "MODUS.ASM" "MODUS.MVM.X64"))
  (chk "E: therefore not the same symbol — a (package,name) key can tell them
        apart, which a bare-name key cannot"
       (eq a b) nil))

;;; ---------------------------------------------------------------
;;; (F) the same property on a REAL first-party file rather than a fixture.
;;; mvm/x64-asm.lisp declares :modus.asm; read as the build reads it, its
;;; unqualified symbols must now land there.
;;; ---------------------------------------------------------------
(let* ((path (merge-pathnames "../mvm/x64-asm.lisp" *load-truename*))
       (text (with-open-file (s path)
               (let ((buf (make-string (file-length s))))
                 (subseq buf 0 (read-sequence buf s)))))
       (forms (forms-of (wrapped text)))
       (reg-info (find-if (lambda (f)
                            (and (consp f) (symbolp (car f))
                                 (string= (symbol-name (car f)) "DEFUN")
                                 (string= (symbol-name (cadr f)) "REG-INFO")))
                          forms)))
  (chk "F: x64-asm.lisp's REG-INFO interns in MODUS.ASM"
       (pkg-of (cadr reg-info)) "MODUS.ASM")
  ;; …and the wrap still restores MODUS.MVM afterwards, on a 3000-line real file.
  (let* ((blob (concatenate 'string (wrapped text) (string #\Newline)
                            "(defun after-x64-asm () nil)"))
         (tail (find-if (lambda (f)
                          (and (consp f) (symbolp (car f))
                               (string= (symbol-name (car f)) "DEFUN")
                               (string= (symbol-name (cadr f)) "AFTER-X64-ASM")))
                        (forms-of blob))))
    (chk "F: and the next chunk is back in MODUS.MVM"
         (pkg-of (cadr tail)) "MODUS.MVM")))

;;; ---------------------------------------------------------------
;;; (G) THE CORPUS PATH.  The ANSI corpus never goes through MVM-TEXT — the
;;; harness prints each transformed test file into *real-ansi-sources* itself
;;; (build-ansi-common.lisp) — so it needs, and now has, its own section wrap.
;;;
;;; And the corpus DOES declare packages: 267 of the 845 .lsp files carry an
;;; (in-package …), 250 of them :cl-test.  They reach the blob through
;;; `format ~S`, i.e. UPPERCASE — `(IN-PACKAGE "CL-TEST")` — which is exactly
;;; the shape the retired text eraser could not see (test H).
;;;
;;; The discriminator is a file that declares NOTHING following a file that
;;; declares CL-TEST: iteration/loop14.lsp has no in-package of its own, yet
;;; its tests were the ones failing.  Its IT must be MODUS.MVM::IT, because
;;; that is the symbol LOOP's %loop-parse-cond-clauses binds.
;;; ---------------------------------------------------------------
;; The harness creates the corpus's packages host-side so SBCL's reader can
;; resolve qualified symbols in the fixtures (build-ansi-common.lisp does the
;; same `(defpackage "CL-TEST" (:use "CL"))`).  Without it FIND-PACKAGE fails,
;; the reader's guard correctly declines to switch, and this test would prove
;; nothing about the declaring file.
(eval-when (:load-toplevel :execute)
  (unless (find-package "CL-TEST") (make-package "CL-TEST" :use '("CL"))))

(defun find-sym-named (name form)
  "First symbol named NAME anywhere in FORM (the LOOP body's anaphoric IT)."
  (cond ((and (symbolp form) form (string= (symbol-name form) name)) form)
        ((consp form) (or (find-sym-named name (car form))
                          (find-sym-named name (cdr form))))
        (t nil)))

(defun corpus-section (file body)
  "A corpus file's emitted section, exactly as build-ansi-common.lisp writes it."
  (concatenate 'string
               (format nil "~%;; === ~A ===~%" file)
               modus.mvm::*build-package-reset-text* (string #\Newline)
               body (string #\Newline)
               modus.mvm::*build-package-reset-text* (string #\Newline)))

(let* ((blob (concatenate 'string
                          ;; a chapter load.lsp, emitted verbatim, uppercase
                          (corpus-section "load.lsp"
                                          "(IN-PACKAGE \"CL-TEST\")
(DEFUN RUN-ANSI-LOAD () (LOOP FOR X IN '(1) WHEN X COLLECT IT))")
                          ;; loop14.lsp: declares nothing at all
                          (corpus-section "loop14.lsp"
                                          "(DEFUN RUN-ANSI-LOOP14 () (LOOP FOR X IN '(1) WHEN X COLLECT IT))")))
       (defuns (remove-if-not (lambda (f)
                                (and (consp f) (symbolp (car f))
                                     (string= (symbol-name (car f)) "DEFUN")))
                              (forms-of blob)))
       (it-of (lambda (f) (find-sym-named "IT" f))))
  (chk "G: two corpus defuns read" (length defuns) 2)
  (chk "G: the DECLARING corpus file really does read in CL-TEST"
       (pkg-of (cadr (first defuns))) "CL-TEST")
  (chk "G: …including its LOOP's IT"
       (pkg-of (funcall it-of (first defuns))) "CL-TEST")
  (chk "G: the NEXT corpus file, which declares nothing, is back in MODUS.MVM"
       (pkg-of (cadr (second defuns))) "MODUS.MVM")
  (chk "G: …and so is its IT — the loop14 discriminator, on the corpus path"
       (pkg-of (funcall it-of (second defuns))) "MODUS.MVM"))

;;; ---------------------------------------------------------------
;;; (H) NO TEXT ERASER MAY BE APPLIED TO A CONTAINED BLOB.
;;;
;;; This is the hole that survived the first fix.  The ANSI build scripts ran
;;; every blob through
;;;
;;;   (defun strip-in-package (text) … (search "(in-package " text) …)
;;;
;;; after the containment was inserted.  SEARCH on strings is case-sensitive,
;;; and the needle is lowercase, so it deleted every "(in-package :modus.mvm)"
;;; reset and left every uppercase `(IN-PACKAGE "CL-TEST")` — removing the
;;; containment and keeping the leak.  Measured on the 7a66219 build's own
;;; real-ansi-gen.lisp: 1508 resets stripped, 672 corpus declarations kept.
;;;
;;; H1 demonstrates the mechanism on a fixture (so the reason for H2 is
;;; legible); H2 is the guard that keeps it from coming back.
;;; ---------------------------------------------------------------
(defun retired-eraser (text)
  "The eraser that used to run after containment.  Verbatim, for the record."
  (let ((result text))
    (loop
      (let ((pos (search "(in-package " result)))
        (unless pos (return result))
        (let ((end (position #\) result :start pos)))
          (when end
            (setf result (concatenate 'string
                                      (subseq result 0 pos)
                                      (subseq result (1+ end))))))))))

(let* ((blob (concatenate 'string
                          (corpus-section "load.lsp" "(IN-PACKAGE \"CL-TEST\")")
                          (corpus-section "loop14.lsp" "(DEFUN RUN-ANSI-LOOP14 (IT) IT)")))
       (erased (retired-eraser blob))
       (defun-form (find-if (lambda (f)
                              (and (consp f) (symbolp (car f))
                                   (string= (symbol-name (car f)) "DEFUN")))
                            (forms-of erased))))
  (chk "H1: the eraser removes every lowercase containment reset"
       (search "(in-package :modus.mvm)" erased) nil)
  (chk "H1: …and keeps every uppercase corpus declaration"
       (integerp (search "(IN-PACKAGE \"CL-TEST\")" erased)) t)
  (chk "H1: …so the erased blob LEAKS — this is the whole bug"
       (pkg-of (cadr defun-form)) "CL-TEST"))

;;; H2: no build script may hand a CONTAINED blob to such an eraser.  Vendored
;;; raw text (net/chipz/install-tarball, spliced without containment and
;;; name-flattened anyway) is the one legitimate use, so it is allowed.
(let* ((build-dir (merge-pathnames "../mvm/" *load-truename*))
       (contained '("*prelude-source*" "*gc-source*" "*rt-source*"
                    "*bridge-source*" "*test-source*" "*ansi-aux-sources*"
                    "*real-ansi-sources*" "*e2diff-sources*"
                    "*compiler-in-image-source*" "*driver-source*"
                    "*full-source*" "(mvm-text "))
       (offenders nil))
  (dolist (path (directory (merge-pathnames "build-*.lisp" build-dir)))
    (with-open-file (s path)
      (loop for line = (read-line s nil nil)
            while line
            do (when (search "strip-in-package" line)
                 (dolist (blob contained)
                   (when (search blob line)
                     (push (format nil "~A: ~A"
                                   (file-namestring path)
                                   (string-trim " " line))
                           offenders)))))))
  (chk "H2: no build script strips in-package from a contained blob"
       (nreverse offenders) nil))

;;; H3: EVERY build script's MVM-TEXT must scope the file it reads.  H2 catches
;;; an eraser that removes containment; this catches containment that was never
;;; applied.  mvm/build-x64-cl-repl.lisp landed on main with an eighth,
;;; unwrapped copy of MVM-TEXT while this change was being gated — and it bakes
;;; x64-asm.lisp (:modus.asm), translate-x64.lisp (:modus.mvm.x64), mvm.lisp,
;;; compiler.lisp, interp.lisp, prelude.lisp and gc.lisp, so every one of those
;;; declarations was leaking into the rest of its blob.  A copy of MVM-TEXT is
;;; easy to add and easy to forget; this makes forgetting fail loudly.
(let* ((build-dir (merge-pathnames "../mvm/" *load-truename*))
       (unscoped nil))
  (dolist (path (directory (merge-pathnames "build-*.lisp" build-dir)))
    (let ((text (with-open-file (s path)
                  (let ((buf (make-string (file-length s))))
                    (subseq buf 0 (read-sequence buf s))))))
      (let ((p (search "(defun mvm-text " text)))
        (when p
          ;; the body up to the next top-level form
          (let* ((end (or (search (format nil "~%(") text :start2 (1+ p))
                          (length text)))
                 (body (subseq text p end)))
            (unless (search "%build-package-scoped-source" body)
              (push (file-namestring path) unscoped)))))))
  (chk "H3: every build script's MVM-TEXT applies the per-file package scope"
       (sort unscoped #'string<) nil))

;;; ---------------------------------------------------------------
;;; (I) #213 — A STAND-IN MUST READ IN THE PACKAGE OF THE THING IT REPLACES.
;;;
;;; mvm/ansi-bridge.lisp carries a hand-written scaffold (SBT-01..SBT-16)
;;; standing in for structures-03.lsp's `defstruct*` forms, which are commented
;;; out in the corpus.  Several of its constructors supply SYMBOL slot defaults:
;;;
;;;   (defun sbt-02-con-2 (a b) (vector 'sbt-02 a b 'z))
;;;   (defun sbt-06-con (&optional (a 'p) (b 'q) (c 'r)) (vector 'sbt-06 c b a))
;;;
;;; structures-03.lsp declares `(in-package :cl-test)`, so the values it compares
;;; those against are CL-TEST::Z / CL-TEST::P.  ansi-bridge.lisp declares no
;;; package, so before #213 the scaffold produced MODUS.MVM::Z — correctly NOT
;;; EQL, and (once #211 made per-file package identity real) correctly failing:
;;; gate IDs 15733/15734/15743/15744/15745, i.e. structures-03 02/2, 02/3 and
;;; 06/1-3.  The discriminator that identifies the mechanism: 02/1 (expects
;;; 1 2 3) and 06/4 (supplies all three values itself) contain no
;;; scaffold-supplied symbol and passed throughout.
;;;
;;; I2 is the property the fix installs; I1 records the guard-driven fallback
;;; that keeps the clean images (which bake ansi-bridge.lisp but have no
;;; CL-TEST and no structures-03) byte-identical; I3 guards the precondition.
;;; ---------------------------------------------------------------
(defun ansi-bridge-text ()
  (let ((path (merge-pathnames "../mvm/ansi-bridge.lisp" *load-truename*)))
    (with-open-file (s path)
      (let ((buf (make-string (file-length s))))
        (subseq buf 0 (read-sequence buf s))))))

(defun bridge-defun (forms name)
  (find-if (lambda (f)
             (and (consp f) (symbolp (car f))
                  (string= (symbol-name (car f)) "DEFUN")
                  (symbolp (cadr f))
                  (string= (symbol-name (cadr f)) name)))
           forms))

;;; I1: with CL-TEST absent, READ-ALL-FORMS-WITH-LOCATIONS' FIND-PACKAGE guard
;;; declines to switch and the block reads in MODUS.MVM, exactly as before #213.
;;; This is what the five clean-image builds that bake ansi-bridge.lisp see.
(let ((existing (find-package "CL-TEST")))
  (when existing (delete-package existing)))
(let* ((forms (forms-of (wrapped (ansi-bridge-text))))
       (con2 (bridge-defun forms "SBT-02-CON-2")))
  (chk "I1: CL-TEST absent -> the guard declines, scaffold reads in MODUS.MVM"
       (pkg-of (find-sym-named "Z" con2)) "MODUS.MVM"))

;;; I2: with CL-TEST present — the state every ANSI gate build is in by the time
;;; build-image reads the blob (build-ansi-common.lisp creates it at the
;;; `(defpackage "CL-TEST" (:use "CL"))` line, the wrappers' build-image call is
;;; the LAST form in the script) — the scaffold's symbol defaults must land in
;;; CL-TEST, and the switch must not leak into the rest of the file.
(make-package "CL-TEST" :use '("CL"))
(let* ((forms (forms-of (wrapped (ansi-bridge-text))))
       (con2 (bridge-defun forms "SBT-02-CON-2"))
       (con6 (bridge-defun forms "SBT-06-CON"))
       (con3 (bridge-defun forms "SBT-02-CON-3"))
       ;; The first defun AFTER the scaffold block, and one further out.
       ;; Probed via a LAMBDA-LIST variable / a non-CL name: a defun named
       ;; REVERSE reads as COMMON-LISP:REVERSE in both MODUS.MVM and CL-TEST
       ;; (both :use "CL"), so the NAME of an inherited symbol discriminates
       ;; nothing — only home-package-less symbols do.
       (after-adjacent (bridge-defun forms "REVERSE"))
       (after (bridge-defun forms "%CHECK-KW-ALLOWED"))
       ;; …and one from before it.
       (before (bridge-defun forms "EXPAND-IN-CURRENT-ENV")))
  (chk "I2: sbt-02-con-2's 'Z default interns in CL-TEST"
       (pkg-of (find-sym-named "Z" con2)) "CL-TEST")
  (chk "I2: sbt-02-con-3's 'X and 'Y defaults too"
       (list (pkg-of (find-sym-named "X" con3))
             (pkg-of (find-sym-named "Y" con3)))
       (list "CL-TEST" "CL-TEST"))
  (chk "I2: sbt-06-con's 'P 'Q 'R lambda-list defaults too"
       (list (pkg-of (find-sym-named "P" con6))
             (pkg-of (find-sym-named "Q" con6))
             (pkg-of (find-sym-named "R" con6)))
       (list "CL-TEST" "CL-TEST" "CL-TEST"))
  (chk "I2: the scaffold's own names are in CL-TEST"
       (pkg-of (cadr con2)) "CL-TEST")
  (chk "I2: code BEFORE the block is untouched (MODUS.MVM)"
       (pkg-of (cadr before)) "MODUS.MVM")
  (chk "I2: the very next defun after the block is back in MODUS.MVM"
       (pkg-of (car (caddr after-adjacent))) "MODUS.MVM")
  (chk "I2: code AFTER the block is back in MODUS.MVM — the switch is SCOPED"
       (pkg-of (cadr after)) "MODUS.MVM"))

;;; I3: the precondition.  The scaffold only lands in CL-TEST because the ANSI
;;; harness has created that package host-side by the time build-image reads the
;;; concatenated blob; the reader's guard silently declines otherwise, which
;;; would make the fix a no-op that still looks landed.  Guard the ordering:
;;; build-ansi-common.lisp must create CL-TEST, and must not delete it after.
(let* ((path (merge-pathnames "../mvm/build-ansi-common.lisp" *load-truename*))
       (text (with-open-file (s path)
               (let ((buf (make-string (file-length s))))
                 (subseq buf 0 (read-sequence buf s)))))
       (create (search "(defpackage \"CL-TEST\"" text))
       (last-delete (let ((p nil) (start 0))
                      (loop (let ((hit (search "(delete-package \"CL-TEST\")"
                                               text :start2 start)))
                              (if hit (progn (setq p hit) (setq start (1+ hit)))
                                  (return p)))))))
  (chk "I3: build-ansi-common.lisp creates CL-TEST host-side" (integerp create) t)
  (chk "I3: …and does not delete it afterwards"
       (or (null last-delete) (and create (< last-delete create))) t))

;;; ---------------------------------------------------------------
;;; (J) #214 — LOOP'S `IT` ANAPHOR IS RECOGNISED BY NAME, NOT BY IDENTITY.
;;;
;;; CLHS 6.1.1.5: LOOP keywords are not true keywords.  They are symbols
;;; recognised BY NAME (compared with STRING=) and are explicitly package-
;;; independent — IT among them.  So matching IT by symbol identity is wrong
;;; regardless of packages; matching by name is the conformant reading.
;;;
;;; compiler.lisp's %loop-parse-cond-clauses callers injected the binding with
;;; a backquoted `it`, which interns in MODUS.MVM (the package compiler.lisp
;;; itself is read in).  ENV-LOOKUP resolves bindings with :TEST #'EQUAL, and
;;; EQUAL on symbols is EQ, so a file declaring its own package reads
;;; CL-TEST::IT and never matched the MODUS.MVM::IT binding: the anaphor
;;; silently became a free variable.  Before #211 the build erased packages so
;;; both were one symbol and this could not surface; #211 made it live.
;;;
;;; The observable defect, on main, is BOTH symbols appearing in one expansion:
;;;   (LET ((MODUS.MVM::IT X)) (WHEN MODUS.MVM::IT (... CL-TEST::IT ...)))
;;; J1/J2 assert exactly one, in the package the source was read in.  J3 is the
;;; converse guard: ordinary variables must STILL be matched by identity.
;;; ---------------------------------------------------------------
(defun it-syms (form)
  "Every DISTINCT symbol named IT anywhere in FORM."
  (let ((acc nil))
    (labels ((walk (f)
               (cond ((and f (symbolp f) (string= (symbol-name f) "IT"))
                      (pushnew f acc))
                     ((consp f) (walk (car f)) (walk (cdr f))))))
      (walk form))
    acc))

(defun ct (name) (intern name "CL-TEST"))

;;; J1: WHEN … COLLECT IT, written entirely in CL-TEST.
(let* ((body (list (ct "FOR") (ct "X") (ct "IN") ''(1 2)
                   (ct "WHEN") (ct "X") (ct "COLLECT") (ct "IT")))
       (syms (it-syms (modus.mvm::expand-cl-loop body))))
  (chk "J1: WHEN…COLLECT IT expands to ONE it symbol (not binding+reference)"
       (length syms) 1)
  (chk "J1: …and it is the CL-TEST::IT the source actually wrote"
       (pkg-of (car syms)) "CL-TEST"))

;;; J2: the other three injection sites — WHEN/ELSE, UNLESS, UNLESS/ELSE.
(dolist (spec (list (list "WHEN…ELSE"
                          (list (ct "WHEN") (ct "X") (ct "COLLECT") (ct "IT")
                                (ct "ELSE") (ct "COLLECT") (ct "IT")))
                    (list "UNLESS"
                          (list (ct "UNLESS") (ct "X") (ct "COLLECT") (ct "IT")))
                    (list "UNLESS…ELSE"
                          (list (ct "UNLESS") (ct "X") (ct "COLLECT") (ct "IT")
                                (ct "ELSE") (ct "COLLECT") (ct "IT")))))
  (let* ((body (append (list (ct "FOR") (ct "X") (ct "IN") ''(1 2)) (cadr spec)))
         (syms (it-syms (modus.mvm::expand-cl-loop body))))
    (chk (format nil "J2: ~A binds the source's own IT" (car spec))
         (list (length syms) (and syms (pkg-of (car syms))))
         (list 1 "CL-TEST"))))

;;; J2b: a file that declares NOTHING still gets MODUS.MVM::IT — the fallback
;;; path, and the shape loop14.lsp (no in-package of its own) reads as.
(let* ((body (list 'modus.mvm::for 'modus.mvm::x 'modus.mvm::in ''(1 2)
                   'modus.mvm::when 'modus.mvm::x
                   'modus.mvm::collect 'modus.mvm::it))
       (syms (it-syms (modus.mvm::expand-cl-loop body))))
  (chk "J2b: an undeclared file's IT stays MODUS.MVM::IT, still single"
       (list (length syms) (and syms (pkg-of (car syms))))
       (list 1 "MODUS.MVM")))

;;; J2c: no IT mentioned at all — the binding still matches its own test, and
;;; nothing from another package leaks in.
(let* ((body (list (ct "FOR") (ct "X") (ct "IN") ''(1 2)
                   (ct "WHEN") (ct "X") (ct "COLLECT") (ct "X")))
       (syms (it-syms (modus.mvm::expand-cl-loop body))))
  (chk "J2c: clause with no IT reference still expands to one IT binding"
       (length syms) 1))

;;; J3: THE CONVERSE.  ENV-LOOKUP must NOT have become a name match: a user's
;;; CL-TEST::X and MODUS.MVM::X are legitimately different variables, which is
;;; exactly the distinction #211 established.  Only the injected anaphor is
;;; name-recognised.
(let* ((empty (modus.mvm::make-empty-env))
       (env   (car (modus.mvm::env-extend-stack empty (ct "X")))))
  (chk "J3: same symbol resolves"
       (not (null (modus.mvm::env-lookup env (ct "X")))) t)
  (chk "J3: same NAME in another package does NOT resolve (still identity)"
       (modus.mvm::env-lookup env 'modus.mvm::x) nil)
  ;; …and the two really are same-named distinct symbols, so the check above
  ;; is testing what it claims to.
  (chk "J3: (the two probes are same-named, different symbols)"
       (list (string= (symbol-name (ct "X")) (symbol-name 'modus.mvm::x))
             (eq (ct "X") 'modus.mvm::x))
       (list t nil)))

;;; J4: the same property on the CORPUS path, end to end — the blob a gate
;;; build actually reads, not a hand-built form list.  Mirrors (G).
(let* ((blob (corpus-section "loop14ish.lsp"
                             "(IN-PACKAGE \"CL-TEST\")
(DEFUN RUN-ANSI-LOOP14ISH () (LOOP FOR X IN '(1) WHEN X COLLECT IT))"))
       (defun-form (find-if (lambda (f)
                              (and (consp f) (symbolp (car f))
                                   (string= (symbol-name (car f)) "DEFUN")))
                            (forms-of blob)))
       ;; the LOOP form is the defun body
       (loop-form (car (cdddr defun-form)))
       (syms (it-syms (modus.mvm::expand-cl-loop (cdr loop-form)))))
  (chk "J4: corpus-read LOOP expands to one IT, in the file's own package"
       (list (length syms) (and syms (pkg-of (car syms))))
       (list 1 "CL-TEST")))

(format t "~%#211 read-package scope: ~:[~D FAILURE(S)~;ALL PASS~]~%"
        (zerop *fails*) *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
