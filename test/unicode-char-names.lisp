;;;; test/unicode-char-names.lisp -- CHAR-NAME, NAME-CHAR and #\name answer exactly as SBCL does.
;;;;   modus --script test/unicode-char-names.lisp   => UNICODE-CHAR-NAMES PASS
;;;;
;;;; The expected answers come from SBCL at test time (test/unicode-char-names-sbcl.lisp, run
;;;; through `sbcl'; or name its output in UNICODE_NAMES_EXPECTED): every code point SBCL names,
;;;; name -> char -> name; a sample of the unnamed (U4E00); the algorithmic ranges (Hangul,
;;;; CJK_COMPATIBILITY_IDEOGRAPH-F900, TANGUT_COMPONENT-001, ...) as SBCL lists them; the Unicode
;;;; 1.0 aliases NAME-CHAR accepts; and #\token for a sample including names SBCL rejects, which
;;;; must be a READER-ERROR naming the token.  Needs an image WITH the table (hosted default);
;;;; test/unicode-char-names-absent.lisp is the other half.
(defvar *fails* 0)
(defvar *checks* 0)
(defun fail (fmt &rest args)
  (setq *fails* (+ *fails* 1))
  (when (< *fails* 40) (format t "FAIL ~?~%" fmt args)))

(defvar *expected*
  (or (sb-ext:posix-getenv "UNICODE_NAMES_EXPECTED")
      (let ((out "/tmp/modus-unicode-char-names-expected.txt")
            (gen (namestring (merge-pathnames "unicode-char-names-sbcl.lisp"
                                              (or *load-truename* "test/unicode-char-names.lisp")))))
        (sb-ext:run-program "/bin/sh" (list "-c" (format nil "sbcl --script '~A' '~A'" gen out))
                            :output nil :error nil :wait t)
        out)))

(defun field-split (line)
  "(KIND CODE REST) of \"K 123 REST\"."
  (let* ((s1 (position #\Space line)) (s2 (position #\Space line :start (+ s1 1))))
    (list (char line 0) (parse-integer line :start (+ s1 1) :end s2) (subseq line (+ s2 1)))))

(defun read-char-token (tok)
  (handler-case (let ((c (read-from-string (concatenate 'string "#\\" tok))))
                  (if (characterp c) (char-code c) (list :not-a-char c)))
    (reader-error (e) (list :reader-error (princ-to-string e)))))

(let ((n 0) (a 0) (r 0))
  (with-open-file (in *expected*)
    (loop
      (let ((line (read-line in nil nil)))
        (unless line (return nil))
        (destructuring-bind (kind code name) (field-split line)
          (setq *checks* (+ *checks* 1))
          (cond
            ((char= kind #\N)
             (setq n (+ n 1))
             (let ((got (char-name (code-char code))))
               (unless (equal got name) (fail "char-name U+~X: ~S, SBCL ~S" code got name)))
             (let ((back (name-char name)))
               (unless (and back (= (char-code back) code))
                 (fail "name-char ~S: ~S, SBCL U+~X" name back code))))
            ((char= kind #\A)
             (setq a (+ a 1))
             (let ((back (name-char name)))
               (unless (eql (if back (char-code back) -1) code)
                 (fail "alias name-char ~S: ~S, SBCL ~D" name back code))))
            ((char= kind #\R)
             (setq r (+ r 1))
             (let ((got (read-char-token name)))
               (if (= code -1)
                   (unless (and (consp got) (eq (car got) :reader-error) (search name (cadr got)))
                     (fail "#\\~A should be a reader error naming it, got ~S" name got))
                   (unless (eql got code) (fail "#\\~A: ~S, SBCL ~D" name got code)))))))))
    (format t "~D names, ~D aliases, ~D reads checked against SBCL~%" n a r)
    (when (< n 40000) (fail "only ~D names in ~A -- did sbcl run?" n *expected*))))

;; the printer writes the non-graphic characters by SBCL's names
(dolist (e (list (cons 0 "#\\Nul") (cons 27 "#\\Esc") (cons 127 "#\\Rubout") (cons 133 "#\\Next-Line")
                 (cons 128 "#\\U80") (cons 7 "#\\Bel") (cons 32 "#\\ ")))
  (let ((got (prin1-to-string (code-char (car e)))))
    (unless (string= got (cdr e)) (fail "prin1 of U+~X: ~S, want ~S" (car e) got (cdr e)))))

(format t "~&UNICODE-CHAR-NAMES ~:[FAIL (~D)~;PASS~]~%" (zerop *fails*) *fails*)
