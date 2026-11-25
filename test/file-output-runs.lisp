;;;; test/file-output-runs.lisp -- ASCII runs into a file's write buffer; names and digits as strings.
;;;;   modus --script test/file-output-runs.lisp   => FILE-OUTPUT-RUNS PASS
;;;;
;;;; A buffered output file takes ASCII straight into its buffer (%FS-WRITE-STRING-RUN, and
;;;; %FS-WRITE-CHAR's fast path); the printer writes a symbol name that prints verbatim, and an
;;;; integer's digits, as one string.  What must hold: the bytes on disk (UTF-8 included, across
;;;; the 4096-byte buffer boundary), FILE-POSITION, FRESH-LINE, and every print-case combination.
;;;; Also: a DEFUN with &OPTIONAL then &REST takes one argument (its required count was &REST's
;;;; position).
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))

(defvar *path* (format nil "/tmp/modus-file-output-runs-~D.txt" (random 1000000)))
(defun slurp ()
  (with-open-file (s *path*)
    (let ((lines nil))
      (loop (let ((l (read-line s nil nil))) (if l (push l lines) (return (nreverse lines))))))))

;; long ASCII lines across the buffer boundary, with non-ASCII in the middle of a run
(let ((line (make-string 1000 :initial-element #\x)))
  (setf (char line 500) (code-char 233))            ; e-acute: two bytes
  (with-open-file (s *path* :direction :output :if-exists :supersede)
    (dotimes (i 10) (write-string line s) (terpri s))
    (write-string "ab" s)
    (check "position" (file-position s) (+ (* 10 1002) 2))
    (fresh-line s) (fresh-line s)
    (write-char #\z s) (write-char (code-char 955) s) (terpri s)))
(let ((lines (slurp)))
  (check "line-count" (length lines) 12)
  (check "line-length" (length (first lines)) 1000)
  (check "utf8-char" (char (fifth lines) 500) (code-char 233))
  (check "tail" (subseq lines 10) (list "ab" (coerce (list #\z (code-char 955)) 'string))))

;; symbols and integers under the print settings
(defpackage :fo-other (:use :cl) (:export #:ext))
(with-open-file (s *path* :direction :output :if-exists :supersede)
  (let ((*print-pretty* nil))
    (prin1 'hello s) (terpri s)
    (let ((*print-case* :downcase)) (prin1 'hello s)) (terpri s)
    (let ((*print-case* :capitalize)) (prin1 'hello-world s)) (terpri s)
    (prin1 '|MiXed| s) (terpri s)
    (prin1 'fo-other:ext s) (terpri s)
    (prin1 'fo-other::int s) (terpri s)
    (prin1 :kw s) (terpri s)
    (princ :kw s) (terpri s)
    (prin1 (list 0 7 -42 123456789 (expt 2 70)) s) (terpri s)
    (let ((*print-base* 16)) (prin1 255 s)) (terpri s)
    (let ((*print-base* 2) (*print-radix* t)) (prin1 5 s)) (terpri s)))
(check "printed" (slurp)
       (list "HELLO" "hello" "Hello-World" "|MiXed|" "FO-OTHER:EXT" "FO-OTHER::INT" ":KW" "KW"
             "(0 7 -42 123456789 1180591620717411303424)" "FF" "#b101"))

(defun opt-rest (a &optional b &rest c) (list a b c))
(check "opt-rest-1" (opt-rest 1) '(1 nil nil))
(check "opt-rest-3" (opt-rest 1 2 3 4) '(1 2 (3 4)))
(check "write-char-arity" (handler-case (funcall #'write-char #\a *standard-output* 3) (program-error () :pe)) :pe)

(delete-file *path*)
(format t "~&FILE-OUTPUT-RUNS ~A~%" (if (zerop *fails*) "PASS" "FAIL"))
