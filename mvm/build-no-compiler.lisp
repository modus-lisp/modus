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

(defparameter *nc-always-kept-heads* '("IN-PACKAGE" "DEFPACKAGE" "DECLAIM")
  "Top-level forms kept whatever they name: they set context and cost nothing.")

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
