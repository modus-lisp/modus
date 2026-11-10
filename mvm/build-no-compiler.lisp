;;;; build-no-compiler.lisp -- MODUS_NO_COMPILER=1: the compiler's sources,
;;;; filtered down to what the rest of the image reaches.
;;;;
;;;; Loaded by build-cli-common.lisp only when the flag is set; never part of an
;;;; image.  The compiler's files (mvm.lisp, interp.lisp, compiler.lisp,
;;;; mvm-eval.lisp, fasl.lisp, ...) also hold runtime the rest of the image calls
;;;; -- COMPUTE-NAME-HASH, deftype expansion, PROCLAIM and package bookkeeping --
;;;; so they cannot simply be left out.  Instead every top-level definition in
;;;; them is kept only if the rest of the image reaches it, transitively, with
;;;; the compiler's ENTRY POINTS cut (*NC-CUT-P*).  An entry point the rest of
;;;; the image still names becomes a stub: a boot or redefinition hook that
;;;; answers NIL (*NC-NIL-HOOKS*), anything else an error saying the image has no
;;;; compiler -- never the build's silent NIL for an unresolved call.
;;;;
;;;; References are found TEXTUALLY (every symbol token outside strings and
;;;; comments), which over-approximates: a name that only appears as data still
;;;; keeps its definition.  That errs toward a bigger image, never a missing
;;;; function.  The source files themselves are unchanged; only the text this
;;;; build bakes is filtered.

(in-package :cl-user)

(defparameter *nc-always-kept-heads* '("IN-PACKAGE" "DEFPACKAGE" "DECLAIM" "%SOURCE-FILE")
  "Top-level forms kept whatever they name: they set context and cost nothing.
   %SOURCE-FILE is the marker naming the file the forms after it came from.")

(defparameter *nc-nil-hooks*
  '("%JIT-ENABLED-P" "%JIT-DRAIN-WANTED" "%JIT-FNADDR-THUNK-INVALIDATE"
    "%JIT-AFTER-RELOCATE" "%JIT-BEFORE-SAVE" "%JIT-X64-STUB-RETARGET"
    "%FASL-TAINT" "%FASL-EFFECT" "%POPULATE-OPCODE-TABLE")
  "Cut names the runtime calls as hooks (at boot, on redefinition, around a
   save): with no compiler there is nothing for them to do, so they answer NIL.")

(defun %nc-cut-p (name)
  "The compiler's entry points: never kept, stubbed if the rest names them."
  (flet ((pre (p) (and (>= (length name) (length p)) (string= p name :end2 (length p)))))
    (or (pre "MVM-EVAL") (pre "%MVM-EVAL") (pre "MVM-COMPILE") (pre "%MVM-COMPILE")
        (pre "MVM-INTERPRET") (pre "%MVM-INTERPRET") (pre "COMPILE-") (pre "%COMPILE-")
        (pre "%FASL-") (pre "FASL-") (pre "%JIT-") (pre "JIT-") (pre "%E2IC-")
        (pre "EMIT-") (pre "TRANSLATE-")
        (string= name "%CALL-INTERP-CLOSURE") (string= name "MAKE-MVM-STATE")
        (string= name "%POPULATE-OPCODE-TABLE"))))

;;; ---- reading top-level forms as TEXT spans --------------------------------

(defun %nc-skip-atom-char (s i n)
  "Past the character literal whose #\\ starts at I: one character, or a name
   such as #\\Newline / #\\Space."
  (let ((j (+ i 3)))
    (when (and (< (+ i 2) n) (alpha-char-p (char s (+ i 2))))
      (loop while (and (< j n) (or (alphanumericp (char s j)) (char= (char s j) #\-)))
            do (incf j)))
    j))

(defun %nc-form-spans (s)
  "(START . END) of every top-level parenthesized form in S, skipping
   comments, strings, |symbols| and character literals."
  (let ((n (length s)) (i 0) (depth 0) (start nil) (out nil))
    (loop
      (when (>= i n) (return (nreverse out)))
      (let ((c (char s i)))
        (cond
          ((char= c #\;)
           (setf i (or (position #\Newline s :start i) n)))
          ((and (char= c #\#) (< (1+ i) n) (char= (char s (1+ i)) #\|))
           (let ((e (search "|#" s :start2 (+ i 2)))) (setf i (if e (+ e 2) n))))
          ((and (char= c #\#) (< (1+ i) n) (char= (char s (1+ i)) #\\))
           (setf i (%nc-skip-atom-char s i n)))
          ((char= c #\")
           (incf i)
           (loop while (and (< i n) (char/= (char s i) #\"))
                 do (incf i (if (char= (char s i) #\\) 2 1)))
           (incf i))
          ((char= c #\|)
           (let ((e (position #\| s :start (1+ i)))) (setf i (if e (1+ e) n))))
          ((char= c #\()
           (when (zerop depth) (setf start i))
           (incf depth) (incf i))
          ((char= c #\))
           (decf depth) (incf i)
           (when (and (zerop depth) start)
             (push (cons start i) out) (setf start nil)))
          (t (incf i)))))))

(defun %nc-symbol-char-p (c)
  (or (alphanumericp c) (find c "%*+-/<>=!?&$.:_")))

(defun %nc-add-symbols (s table)
  "Every symbol token of S outside strings, comments and character literals,
   upcased and without its package prefix, into TABLE."
  (let ((n (length s)) (i 0))
    (loop
      (when (>= i n) (return table))
      (let ((c (char s i)))
        (cond
          ((char= c #\;) (setf i (or (position #\Newline s :start i) n)))
          ((and (char= c #\#) (< (1+ i) n) (char= (char s (1+ i)) #\|))
           (let ((e (search "|#" s :start2 (+ i 2)))) (setf i (if e (+ e 2) n))))
          ((and (char= c #\#) (< (1+ i) n) (char= (char s (1+ i)) #\\))
           (setf i (%nc-skip-atom-char s i n)))
          ((char= c #\")
           (incf i)
           (loop while (and (< i n) (char/= (char s i) #\"))
                 do (incf i (if (char= (char s i) #\\) 2 1)))
           (incf i))
          ((%nc-symbol-char-p c)
           (let ((j i))
             (loop while (and (< j n) (%nc-symbol-char-p (char s j))) do (incf j))
             (let* ((tok (string-upcase (subseq s i j)))
                    (colon (position #\: tok :from-end t))
                    (name (if colon (subseq tok (1+ colon)) tok)))
               (when (and (plusp (length name)) (not (digit-char-p (char name 0))))
                 (setf (gethash name table) t)))
             (setf i j)))
          (t (incf i)))))))

(defun %nc-head-and-name (form)
  "FORM's head (upcased) and, for a definition, the NAME it defines."
  (let* ((n (length form))
         (tokens (let ((out nil) (i 1))
                   (loop while (and (< i n) (< (length out) 2))
                         do (loop while (and (< i n) (or (member (char form i) '(#\Space #\Tab #\Newline #\Return))
                                                         (char= (char form i) #\()))
                                  do (incf i))
                            (let ((j i))
                              (loop while (and (< j n) (%nc-symbol-char-p (char form j))) do (incf j))
                              (when (> j i) (push (string-upcase (subseq form i j)) out))
                              (setf i (max j (1+ i)))))
                   (nreverse out))))
    (flet ((bare (tok) (let ((c (position #\: tok :from-end t))) (if c (subseq tok (1+ c)) tok))))
      (values (and tokens (bare (first tokens)))
              (and (second tokens) (bare (second tokens)))))))

;;; ---- the filter --------------------------------------------------------------

(defun %nc-filter (compiler-texts outside-texts)
  "COMPILER-TEXTS: list of source texts (the compiler's files, in image order).
   OUTSIDE-TEXTS: everything else the image bakes.  Returns (values FILTERED
   STUB-SOURCE KEPT-COUNT TOTAL-COUNT STUBBED), FILTERED parallel to
   COMPILER-TEXTS."
  (let ((outside (make-hash-table :test #'equal))
        (defs (make-hash-table :test #'equal))   ; NAME -> list of (text-index . span)
        (spans (mapcar #'%nc-form-spans compiler-texts))
        (need (make-hash-table :test #'equal))
        (structs nil)
        (work nil))
    (dolist (o outside-texts) (%nc-add-symbols o outside))
    (loop for text in compiler-texts for k from 0 for sp in spans
          do (dolist (span sp)
               (multiple-value-bind (head name) (%nc-head-and-name (subseq text (car span) (cdr span)))
                 (when (and name (not (member head *nc-always-kept-heads* :test #'string=)))
                   (push (cons k span) (gethash name defs))
                   ;; a DEFSTRUCT is also named by what it generates
                   (when (string= head "DEFSTRUCT")
                     (push name structs)
                     (dolist (alias (list (concatenate 'string "MAKE-" name)
                                          (concatenate 'string "COPY-" name)
                                          (concatenate 'string name "-P")))
                       (push (cons k span) (gethash alias defs))))))))
    (flet ((want (name)
             (unless (gethash name defs)
               ;; NAME-SLOT, a DEFSTRUCT accessor (default conc-name)
               (let ((s (find-if (lambda (st) (and (> (length name) (1+ (length st)))
                                                   (string= st name :end2 (length st))
                                                   (char= (char name (length st)) #\-)))
                                 structs)))
                 (when s (setf name s))))
             (unless (or (gethash name need) (%nc-cut-p name) (not (gethash name defs)))
               (setf (gethash name need) t)
               (push name work))))
      (maphash (lambda (name v) (declare (ignore v)) (when (gethash name outside) (want name))) defs)
      (loop while work
            do (let ((name (pop work)))
                 (dolist (d (gethash name defs))
                   (let ((refs (make-hash-table :test #'equal))
                         (text (nth (car d) compiler-texts)))
                     (%nc-add-symbols (subseq text (cadr d) (cddr d)) refs)
                     (maphash (lambda (r v) (declare (ignore v)) (want r)) refs))))))
    (let ((kept 0) (total 0) (stubbed nil))
      (let ((filtered
              (loop for text in compiler-texts for sp in spans
                    collect (with-output-to-string (out)
                              (dolist (span sp)
                                (let ((form (subseq text (car span) (cdr span))))
                                  (multiple-value-bind (head name) (%nc-head-and-name form)
                                    (incf total)
                                    (when (or (member head *nc-always-kept-heads* :test #'string=)
                                              (and name (gethash name need)))
                                      (incf kept)
                                      (write-string form out)
                                      (terpri out)))))))))
        (let ((refs (make-hash-table :test #'equal)))
          (maphash (lambda (k v) (declare (ignore v)) (setf (gethash k refs) t)) outside)
          (loop for text in compiler-texts for sp in spans
                do (dolist (span sp)
                     (multiple-value-bind (head name) (%nc-head-and-name (subseq text (car span) (cdr span)))
                       (declare (ignore head))
                       (when (and name (gethash name need))
                         (%nc-add-symbols (subseq text (car span) (cdr span)) refs)))))
          (maphash (lambda (name v) (declare (ignore v))
                     (when (and (%nc-cut-p name) (gethash name refs)) (push name stubbed)))
                   defs))
        (setf stubbed (sort stubbed #'string<))
        (values filtered
                (with-output-to-string (out)
                  (dolist (name stubbed)
                    (if (member name *nc-nil-hooks* :test #'string=)
                        (format out "(defun ~A (&rest args) args nil)~%" name)
                        (format out "(defun ~A (&rest args) args (error ~S))~%" name
                                (format nil "~A: this image has no compiler (built with MODUS_NO_COMPILER=1)" name)))))
                kept total stubbed)))))

;;; ---- lean boot ----------------------------------------------------------------

(defun %nc-inner-form (form)
  "Text of the first parenthesized subform of FORM, or NIL."
  (let ((p (position #\( form :start 1)))
    (and p (let ((e (%nc-form-end form p))) (and e (subseq form p e))))))

(defun %nc-form-calls (form)
  "The function a KERNEL-MAIN body form calls: its head; for
   (HANDLER-CASE (F ...) ...) the F it guards; for (WHEN (F ...) ...) the F
   it tests."
  (multiple-value-bind (head) (%nc-head-and-name form)
    (if (and head (member head '("HANDLER-CASE" "WHEN") :test #'string=))
        (let ((inner (%nc-inner-form form)))
          (and inner (values (%nc-head-and-name inner))))
        head)))

(defparameter *nc-lean-boot-replace*
  '(("%INIT-PACKAGES"
     . "(progn (setq modus.runtime::*pkg-tag* 987654321)
       (setq modus.runtime::*sym-tag* 123456789)
       (setq modus.runtime::*all-packages* nil)
       (setq modus.runtime::*pkg-any-local-nicknames* nil)
       (modus.runtime::%init-pkg-by-hash))"))
  "MODUS_LEAN_BOOT: KERNEL-MAIN steps replaced rather than skipped.
   %INIT-PACKAGES builds COMMON-LISP, CL-USER, KEYWORD and MODUS and interns and
   exports their symbols; a lean image keeps only its first lines -- the type
   tags and an EMPTY package-by-hash table -- so the quoted symbols the code
   touches are interned lazily, with no home package.")

(defun %nc-lean-kernel-main (text skip)
  "TEXT with every top-level form of KERNEL-MAIN's body that calls a name in
   SKIP (upcased strings) removed, and every remaining (HANDLER-CASE (F ...)
   ...) reduced to (F ...).  Returns (values TEXT REMOVED)."
  (let* ((start (search "(defun kernel-main ()" text))
         (span (and start (find start (%nc-form-spans text) :key #'car))))
    (unless span (error "lean boot: no (defun kernel-main () ...) in the driver source"))
    (let* ((open (+ start (length "(defun kernel-main ()")))
           (body (subseq text open (1- (cdr span))))
           (removed nil)
           (kept (with-output-to-string (out)
                   (let ((pos 0))
                     (dolist (sp (%nc-form-spans body))
                       (let* ((form (subseq body (car sp) (cdr sp)))
                              (calls (%nc-form-calls form)))
                         (write-string body out :start pos :end (car sp))
                         (cond ((and calls (assoc calls *nc-lean-boot-replace* :test #'string=))
                                (push (format nil "~A(replaced)" calls) removed)
                                (write-string (cdr (assoc calls *nc-lean-boot-replace* :test #'string=)) out))
                               ((and calls (member calls skip :test #'string=))
                                (push calls removed))
                               ;; (HANDLER-CASE (F ...) ...) runs as (F ...): a
                               ;; lean image carries no condition system for it
                               ((string= (or (%nc-head-and-name form) "") "HANDLER-CASE")
                                (write-string (%nc-inner-form form) out))
                               (t (write-string form out)))
                         (setf pos (cdr sp))))
                     (write-string body out :start pos)))))
      (values (concatenate 'string (subseq text 0 open) kept ")" (subseq text (cdr span)))
              (nreverse removed)))))

;;; ---- whole-image shake ---------------------------------------------------------

(defun %nc-shake (texts roots &key exclude)
  "TEXTS: every source text the image bakes, in image order.  ROOTS: names
   (upcased) the image must keep.  Returns TEXTS with each top-level definition
   kept only if reachable from ROOTS -- textually, as %NC-FILTER, and with
   last-DEFUN-wins: of several DEFUNs of one name only the last counts, as in
   the image.  IN-PACKAGE / DEFPACKAGE / DECLAIM are always kept.  A name in
   EXCLUDE is never a root, though it is kept if something kept reaches it."
  (let ((defs (make-hash-table :test #'equal))   ; NAME -> list of (k . span)
        (spans (mapcar #'%nc-form-spans texts))
        (need (make-hash-table :test #'equal))
        (structs nil) (work nil))
    (loop for text in texts for k from 0 for sp in spans
          do (dolist (span sp)
               (multiple-value-bind (head name) (%nc-head-and-name (subseq text (car span) (cdr span)))
                 (when (and name (not (member head *nc-always-kept-heads* :test #'string=)))
                   (if (string= head "DEFUN")
                       (setf (gethash name defs) (list (cons k span)))
                       (push (cons k span) (gethash name defs)))
                   (when (string= head "DEFSTRUCT")
                     (push name structs)
                     (dolist (alias (list (concatenate 'string "MAKE-" name)
                                          (concatenate 'string "COPY-" name)
                                          (concatenate 'string name "-P")))
                       (push (cons k span) (gethash alias defs))))))))
    (flet ((want (name)
             (unless (gethash name defs)
               (let ((s (find-if (lambda (st) (and (> (length name) (1+ (length st)))
                                                   (string= st name :end2 (length st))
                                                   (char= (char name (length st)) #\-)))
                                 structs)))
                 (when s (setf name s))))
             (when (and (gethash name defs) (not (gethash name need)))
               (setf (gethash name need) t)
               (push name work))))
      (flet ((root (name) (unless (member name exclude :test #'string=) (want name))))
        (dolist (r roots)
          (if (consp r)                   ; (:prefix P): every name starting with P
              (let ((p (second r)) (hits nil))
                (maphash (lambda (name v) (declare (ignore v))
                           (when (and (> (length name) (length p)) (string= p name :end2 (length p)))
                             (push name hits)))
                         defs)
                (mapc #'root hits))
              (root r))))
      (loop while work
            do (dolist (d (gethash (pop work) defs))
                 (let ((refs (make-hash-table :test #'equal)))
                   (%nc-add-symbols (subseq (nth (car d) texts) (cadr d) (cddr d)) refs)
                   (maphash (lambda (r v) (declare (ignore v)) (want r)) refs)))))
    (let ((keep (make-hash-table :test #'equal)))   ; (k . start) of every kept definition
      (maphash (lambda (name v) (declare (ignore v))
                 (dolist (d (gethash name defs)) (setf (gethash (cons (car d) (cadr d)) keep) t)))
               need)
      (loop for text in texts for k from 0 for sp in spans
            collect (with-output-to-string (out)
                      (dolist (span sp)
                        (let ((head (%nc-head-and-name (subseq text (car span) (min (cdr span) (+ (car span) 200))))))
                          (when (or (member head *nc-always-kept-heads* :test #'string=)
                                    (gethash (cons k (car span)) keep))
                            (write-string text out :start (car span) :end (cdr span))
                            (terpri out)))))))))

(defparameter *nc-lean-boot-skip*
  '(;; saved cores: a no-compiler image restores none
    "%CORE-REQUESTED-P"
    ;; tables for lookup BY NAME at run time -- EVAL, INTERN of kernel names,
    ;; SYMBOL-NAME of kernel symbols, FUNCALL of a symbol, MACROEXPAND
    "%INIT-SYMBOL-FUNCTION-TABLE" "%INIT-SFT-AUTO" "%INIT-SYM-NAME-AUTO"
    "%INIT-RUNTIME-MACROS" "%INSTALL-RUNTIME-CL-MACROS" "INIT-COMPILER-MACRO-SET"
    ;; the reader, CLOS's protocol GFs and method combinations, condition types
    ;; beyond the built-ins, signals, MAKE-LOAD-FORM, backquote for EVAL
    "%INIT-READER" "%INIT-CONDITION-TYPES" "%INIT-METHOD-COMBINATIONS"
    "%INIT-CLOS-PROTOCOL" "%INIT-SIGNAL-HANDLING" "%INIT-SIGNAL-SYMBOLS"
    "%INIT-MAKE-LOAD-FORM" "%INSTALL-RUNTIME-BACKQUOTE" "%SEAT-BACKQUOTE-MARKERS"
    ;; RT, the MODUS package, the JIT and the deferred surfaces
    "%INSTALL-DEFTEST-MACRO" "%INIT-RTEST" "%INIT-MODUS-PACKAGE" "%JIT-BOOT-INIT"
    "%DEFER-BOOT-SURFACES")
  "MODUS_LEAN_BOOT: the KERNEL-MAIN steps a no-compiler image skips.  Each
   builds something only run-time lookup by name, EVAL or a facility the image
   does not carry would use.")

(defun %nc-form-end (s i)
  "End of the parenthesized form opening at I in S (comment, string and
   character-literal aware), or NIL."
  (let ((n (length s)) (depth 0))
    (loop
      (when (>= i n) (return nil))
      (let ((c (char s i)))
        (cond
          ((char= c #\;) (setf i (or (position #\Newline s :start i) n)))
          ((and (char= c #\#) (< (1+ i) n) (char= (char s (1+ i)) #\|))
           (let ((e (search "|#" s :start2 (+ i 2)))) (setf i (if e (+ e 2) n))))
          ((and (char= c #\#) (< (1+ i) n) (char= (char s (1+ i)) #\\))
           (setf i (%nc-skip-atom-char s i n)))
          ((char= c #\")
           (incf i)
           (loop while (and (< i n) (char/= (char s i) #\"))
                 do (incf i (if (char= (char s i) #\\) 2 1)))
           (incf i))
          ((char= c #\|) (let ((e (position #\| s :start (1+ i)))) (setf i (if e (1+ e) n))))
          ((char= c #\() (incf depth) (incf i))
          ((char= c #\)) (decf depth) (incf i) (when (zerop depth) (return i)))
          (t (incf i)))))))

(defun %nc-emitted-callee-roots (files)
  "Runtime names the BUILD emits calls to without the source naming them:
   every string literal and every quoted symbol in FILES (the compiler, the
   translator, the image builder), and for a FORMAT control naming a family --
   \"%FUNCALL-GF-~D\" -- its prefix, returned as (:prefix \"%FUNCALL-GF-\");
   and every symbol inside a backquote template or a quoted list -- the code,
   and the tables of helper names, the compiler writes from."
  (let ((out nil))
    (dolist (f files out)
      (let* ((s (with-open-file (in f :external-format :utf-8)
                  (let ((str (make-string (file-length in))))
                    (subseq str 0 (read-sequence str in)))))
             (n (length s)) (i 0))
        (loop while (< i n)
              do (let ((c (char s i)))
                   (cond
                     ((char= c #\;) (setf i (or (position #\Newline s :start i) n)))
                     ((and (char= c #\#) (< (1+ i) n) (char= (char s (1+ i)) #\\))
                      (setf i (%nc-skip-atom-char s i n)))
                     ((char= c #\")
                      (let ((j (1+ i)))
                        (loop while (and (< j n) (char/= (char s j) #\"))
                              do (incf j (if (char= (char s j) #\\) 2 1)))
                        (let* ((lit (string-upcase (subseq s (1+ i) (min j n))))
                               (tilde (position #\~ lit)))
                          (cond ((and tilde (> tilde 1) (< (1+ tilde) (length lit))
                                      (char= (char lit (1+ tilde)) #\D)   ; a numbered family
                                      (every #'%nc-symbol-char-p (subseq lit 0 tilde)))
                                 (push (list :prefix (subseq lit 0 tilde)) out))
                                ((and (plusp (length lit)) (every #'%nc-symbol-char-p lit))
                                 (push lit out))))
                        (setf i (1+ j))))
                     ((and (or (char= c #\`) (char= c #\')) (< (1+ i) n) (char= (char s (1+ i)) #\())
                      (let ((e (%nc-form-end s (1+ i))) (h (make-hash-table :test #'equal)))
                        (when e
                          (%nc-add-symbols (subseq s (1+ i) e) h)
                          (maphash (lambda (k v) (declare (ignore v)) (push k out)) h))
                        (incf i)))
                     ((and (char= c #\') (< (1+ i) n) (%nc-symbol-char-p (char s (1+ i))))
                      (let ((j (1+ i)))
                        (loop while (and (< j n) (%nc-symbol-char-p (char s j))) do (incf j))
                        (let* ((tok (string-upcase (subseq s (1+ i) j)))
                               (colon (position #\: tok :from-end t)))
                          (push (if colon (subseq tok (1+ colon)) tok) out))
                        (setf i j)))
                     (t (incf i)))))))))
