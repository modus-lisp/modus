;;;; test/unicode-char-names-absent.lisp -- an image WITHOUT the Unicode name table
;;;; (MODUS_UNICODE_NAMES=0; the bare-metal default) still reads what it can, as SBCL does, and
;;;; refuses the rest LOUDLY: a READER-ERROR naming the character and saying the table is missing,
;;;; never a guessed character.
;;;;   MODUS_UNICODE_NAMES=0 MODUS_CLI_OUT=/tmp/modus-nonames sbcl --script mvm/build-generic-cli.lisp
;;;;   /tmp/modus-nonames --script test/unicode-char-names-absent.lisp   => UNICODE-CHAR-NAMES-ABSENT PASS
;;;; On an image WITH the table it says SKIP (test/unicode-char-names.lisp covers that one).
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))
(defun rd (tok)
  (handler-case (let ((c (read-from-string (concatenate 'string "#\\" tok))))
                  (if (characterp c) (char-code c) (list :not-a-char c)))
    (reader-error (e) (list :reader-error (princ-to-string e)))))

(if (char-name #\A)
    (format t "UNICODE-CHAR-NAMES-ABSENT SKIP (this image has the table)~%")
    (progn
      ;; what every image reads, with SBCL's answers
      (dolist (e '(("U+FEFF" . #xFEFF) ("UFEFF" . #xFEFF) ("u+41" . 65) ("U10FFFF" . #x10FFFF)
                   ("No-break_space" . #xA0) ("Replacement_Character" . #xFFFD)
                   ("LINE_SEPARATOR" . #x2028) ("Paragraph_Separator" . #x2029)
                   ("ZERO_WIDTH_NO-BREAK_SPACE" . #xFEFF) ("FormFeed" . 12) ("Vt" . 11)
                   ("Nul" . 0) ("Null" . 0) ("^a" . 1) ("Lf" . 10) ("Esc" . 27) ("Escape" . 27)
                   ("Del" . 127) ("Next-Line" . 133) ("U80" . 128) ("Bel" . 7)))
        (check (format nil "#\\~A" (car e)) (rd (car e)) (cdr e)))
      ;; and the rest is an error that names the character and the missing table
      (dolist (tok '("LOWER_ONE_EIGHTH_BLOCK" "Latin_Small_Letter_A" "HANGUL_SYLLABLE_GA" "Bell"
                     "U+110000"))
        (let ((r (rd tok)))
          (check (format nil "#\\~A is a reader error naming it and the missing table" tok)
                 (and (consp r) (eq (car r) :reader-error)
                      (search tok (cadr r)) (search "no Unicode name table" (cadr r)) t)
                 t)))
      (check "name-char of a Unicode name is NIL, not a guess" (name-char "LOWER_ONE_EIGHTH_BLOCK") nil)
      (check "char-name of a graphic character is NIL" (char-name (code-char #x2581)) nil)
      (check "char-name of the controls is SBCL's" (mapcar #'char-name (list (code-char 0) (code-char 27) (code-char 159)))
             '("Nul" "Esc" "Application-Program-Command"))
      (format t "~&UNICODE-CHAR-NAMES-ABSENT ~:[FAIL (~D)~;PASS~]~%" (zerop *fails*) *fails*)))
