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

(format t "~%#211 read-package scope: ~:[~D FAILURE(S)~;ALL PASS~]~%"
        (zerop *fails*) *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
