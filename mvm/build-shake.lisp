;;;; build-shake.lisp -- MODUS_SHAKE=1: leave out the DEFUNs nothing can reach.
;;;;
;;;; A full CL image registers every DEFUN by name in the symbol-function table,
;;;; so nothing in it is ever unreferenced and nothing is left out.  This pass
;;;; runs on the component source texts BEFORE the scanners build those tables,
;;;; and drops each DEFUN that no root reaches, textually:
;;;;
;;;;   - only DEFUN forms are candidates; every other top-level form (DEFVAR,
;;;;     DEFMACRO, DEFSTRUCT, DEFCLASS, a SETF at top level ...) is kept, and
;;;;     every name in it is a root;
;;;;   - a name is any symbol token, AND the text of a short string literal
;;;;     (FIND-SYMBOL "FOO", (EMIT-IR :CALL "%FOO" ...)), AND the tokens of a
;;;;     string literal that is code (the runtime macros' DEFMACRO texts);
;;;;   - the roots also include CL's exported names, KERNEL-MAIN, and the names
;;;;     compiled code calls without naming them (%NC-EMITTED-CALLEE-ROOTS);
;;;;   - a DEFUN whose name the compiler or runtime BUILDS from a prefix or
;;;;     suffix (SETF-x, SET-x, MAKE-x, x-P, %PRINT-x ...) is always kept;
;;;;   - MODUS-INT's export list is not a root: it names internals shared
;;;;     between kernel packages, not an API.  MODUS's is.
;;;;
;;;; Over-approximate by construction: a removed function is one whose name
;;;; appears nowhere a kept form could use it.  Calling one anyway, by a name
;;;; built at run time, is an UNDEFINED-FUNCTION naming it.  MODUS_SHAKE_WHY=N1,N2
;;;; prints what reached each name; MODUS_SHAKE_LIST=FILE writes the removed
;;;; names.  Needs build-no-compiler.lisp's form scanner.

(defparameter *shake-kept-prefixes*
  '("SET-" "SETF-" "%SET-" "MAKE-" "COPY-" "INIT-" "P-" "%PRINT-" "%FUNCALL-IC-"
    "%FUNCALL-GF-" "%KW-REST-" "%DS-" "%CELL-" "DO-" "SAVE-" "SPECPARM-" "SPECTMP-"
    "SPECSUP-" "%%STRUCT-CTOR-" "TOPLEVEL-" "%NAMELESS-")
  "DEFUN names with these prefixes are kept: code builds such names (CONCATENATE
   or FORMAT NIL with the prefix) and calls the result.")

(defun %shake-built-name-p (name)
  (or (some (lambda (p) (and (> (length name) (length p)) (string= p name :end2 (length p))))
            *shake-kept-prefixes*)
      (and (> (length name) 2) (string= "-P" name :start2 (- (length name) 2)))))

(defun %shake-add-tokens (s table)
  "Every symbol token of S outside comments, upcased and without its package
   prefix, into TABLE -- and each short string literal's text, and the tokens
   of a string literal that is code."
  (let ((n (length s)) (i 0))
    (flet ((note (tok)
             (let* ((u (string-upcase tok))
                    (colon (position #\: u :from-end t))
                    (name (if colon (subseq u (1+ colon)) u)))
               (when (and (plusp (length name)) (not (digit-char-p (char name 0))))
                 (setf (gethash name table) t)))))
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
             (let ((start (1+ i)) (escaped nil))
               (incf i)
               (loop while (and (< i n) (char/= (char s i) #\"))
                     do (when (char= (char s i) #\\) (setq escaped t))
                        (incf i (if (char= (char s i) #\\) 2 1)))
               (let ((text (subseq s start (min i n))))
                 (when escaped
                   (setq text (with-output-to-string (o)
                                (let ((k 0))
                                  (loop while (< k (length text))
                                        do (if (and (char= (char text k) #\\) (< (1+ k) (length text)))
                                               (progn (write-char (char text (1+ k)) o) (incf k 2))
                                               (progn (write-char (char text k) o) (incf k))))))))
                 ;; a long string is tokenized whatever it starts with: generated
                 ;; source (a kernel prologue) may open with a comment, and the
                 ;; calls after it are as real.  Docstrings add their words too:
                 ;; over-approximate, never wrong.
                 (cond ((<= (length text) 120)
                        (unless (find-if (lambda (ch) (member ch '(#\Space #\Newline #\Tab #\( #\)))) text)
                          (note text)))
                       (t (%shake-add-tokens text table)))
                 ;; a short code string too: (FORMAT NIL "(defun ~A ..." ...)
                 (when (and (<= (length text) 120) (find #\( text))
                   (%shake-add-tokens text table)))
               (incf i)))
            ((%nc-symbol-char-p c)
             (let ((j i))
               (loop while (and (< j n) (%nc-symbol-char-p (char s j))) do (incf j))
               (note (subseq s i j))
               (setf i j)))
            (t (incf i))))))))

(defun %shake-defuns (texts roots)
  "TEXTS: the component source texts, in image order.  ROOTS: names (upcased).
   Returns (values NEW-TEXTS REMOVED-NAMES): TEXTS without the DEFUNs nothing
   reaches (see the file comment)."
  (let ((defs (make-hash-table :test #'equal))   ; NAME -> list of (k start . end)
        (need (make-hash-table :test #'equal))
        (parent (make-hash-table :test #'equal))
        (work nil) (from nil)
        (spans (mapcar #'%nc-form-spans texts))
        (root-tokens (make-hash-table :test #'equal)))
    (loop for text in texts for k from 0 for sp in spans
          do (dolist (span sp)
               (multiple-value-bind (head name)
                   (%nc-head-and-name (subseq text (car span) (min (cdr span) (+ (car span) 200))))
                 (cond
                   ((and (equal head "DEFUN") name (string/= name "SETF")
                         (not (%shake-built-name-p name)))
                    (push (cons k span) (gethash name defs)))
                   ((and (equal head "DEFPACKAGE") (equal name "MODUS-INT")))
                   (t (%shake-add-tokens (subseq text (car span) (cdr span)) root-tokens))))))
    (flet ((want (n)
             (when (and (gethash n defs) (not (gethash n need)))
               (setf (gethash n need) t (gethash n parent) from)
               (push n work))))
      (dolist (r roots) (want (string-upcase r)))
      (maphash (lambda (n v) (declare (ignore v)) (want n)) root-tokens)
      (loop while work
            do (let ((n (pop work)))
                 (setq from n)
                 (dolist (d (gethash n defs))
                   (let ((refs (make-hash-table :test #'equal)))
                     (%shake-add-tokens (subseq (nth (car d) texts) (cadr d) (cddr d)) refs)
                     (maphash (lambda (r v) (declare (ignore v)) (want r)) refs))))))
    (let ((why (sb-ext:posix-getenv "MODUS_SHAKE_WHY")))
      (when (and why (plusp (length why)))
        (dolist (n (let ((out nil) (st 0))
                     (loop (let ((j (position #\, why :start st)))
                             (push (string-upcase (subseq why st j)) out)
                             (if j (setq st (1+ j)) (return (nreverse out)))))))
          (format t "~&;; SHAKE WHY ~A:~:[ removed~;~{ <- ~A~}~]~%" n (gethash n need)
                  (loop for m = (gethash n parent) then (gethash m parent) while m collect m)))))
    (let ((cut (make-hash-table :test #'equal)) (removed nil))
      (maphash (lambda (n ds)
                 (unless (gethash n need)
                   (push n removed)
                   (dolist (d ds) (setf (gethash (cons (car d) (cadr d)) cut) (cddr d)))))
               defs)
      (values
       (loop for text in texts for k from 0 for sp in spans
             collect (with-output-to-string (out)
                       (let ((pos 0))
                         (dolist (span sp)
                           (when (gethash (cons k (car span)) cut)
                             (write-string text out :start pos :end (car span))
                             (setq pos (cdr span))))
                         (write-string text out :start pos))))
       (sort removed #'string<)))))
