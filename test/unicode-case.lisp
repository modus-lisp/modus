;;;; test/unicode-case.lisp -- modus folds case, classifies letters and reads digits as SBCL
;;;; does, for every code point.  Prints lines that must be IDENTICAL under SBCL and modus;
;;;; test/run-unicode-case.sh runs both and compares (no data file: SBCL is the oracle).
;;;;
;;;; Before scripts/gen-unicode-case.lisp's tables, modus stopped at ASCII: (char-upcase
;;;; #\LATIN_SMALL_LETTER_E_WITH_ACUTE) was itself, so (read-from-string "été") was a
;;;; symbol named "été" where SBCL's is "ÉTÉ", string-equal "é" "É" was NIL, and comb's \w
;;;; missed every non-ASCII letter.

(defun s (&rest codes) (map 'string #'code-char codes))

;;; The reference.  modus answers as SBCL does, EXCEPT where SBCL breaks CLHS 13.1.4.3 (the four
;;; titlecase digraphs, U+01C5 U+01C8 U+01CB U+01F2): modus makes those caseless, as
;;; scripts/gen-unicode-case.lisp explains, so ansi-test's BOTH-CASE-P.2 / CHAR-UPCASE.2 /
;;; CHAR-DOWNCASE.2 pass.  Under SBCL the R- functions apply that rule to SBCL's answers; under
;;; modus they are the plain functions.
#+sbcl
(defun violates-case-rules (ch)
  (let ((u (char-upcase ch)) (d (char-downcase ch)))
    (or (and (char/= u ch) (not (lower-case-p ch)))
        (and (char/= d ch) (not (upper-case-p ch)))
        (not (eq (not (both-case-p ch)) (not (or (upper-case-p ch) (lower-case-p ch)))))
        (char/= u (char-upcase u)) (char/= d (char-downcase d))
        (and (upper-case-p ch) (or (char= d ch) (char/= (char-upcase d) ch)))
        (and (lower-case-p ch) (or (char= u ch) (char/= (char-downcase u) ch))))))
#+sbcl (defun caseless (ch) (violates-case-rules ch))
#-sbcl (defun caseless (ch) (declare (ignore ch)) nil)
(defun r-upcase (ch) (if (caseless ch) ch (char-upcase ch)))
(defun r-downcase (ch) (if (caseless ch) ch (char-downcase ch)))
(defun r-upper-p (ch) (and (not (caseless ch)) (upper-case-p ch)))
(defun r-lower-p (ch) (and (not (caseless ch)) (lower-case-p ch)))
(defun r-both-p (ch) (and (not (caseless ch)) (both-case-p ch)))

(defun digest-block (lo hi)
  "An order-sensitive digest of every answer for code points LO..HI-1."
  (let ((h 0))
    (loop for c from lo below hi
          for ch = (code-char c)
          when ch
            do (dolist (v (list (char-code (r-upcase ch)) (char-code (r-downcase ch))
                                (if (r-upper-p ch) 1 0) (if (r-lower-p ch) 1 0)
                                (if (r-both-p ch) 1 0) (if (alpha-char-p ch) 1 0)
                                (if (alphanumericp ch) 1 0) (or (digit-char-p ch 36) 99)
                                (char-code (r-downcase (r-upcase ch)))))
                 (setq h (mod (+ (* h 31) v 7) 1000000007))))
    h))

(format t "~&code points: ~d~%" char-code-limit)
(loop for lo from 0 below char-code-limit by #x10000
      do (format t "~&block ~6,'0x: ~d~%" lo (digest-block lo (min char-code-limit (+ lo #x10000)))))

(defun codes (x) (if (stringp x) (map 'list #'char-code x) x))
(defmacro probe (name form)
  `(format t "~&~a: ~s~%" ,name (handler-case (let ((v ,form)) (if (symbolp v) (list :symbol (codes (symbol-name v))) (codes v)))
                                  (error (e) (list :error (type-of e))))))
(probe "read e-acute symbol" (read-from-string (s #xe9 #x74 #xe9)))
(probe "read arabic-indic 12" (read-from-string (s #x661 #x662)))
(probe "read arabic-indic ratio" (read-from-string (s #x661 #x2f #x662)))
(probe "read fullwidth 12" (read-from-string (s #xff11 #xff12)))
(probe "read #x arabic digit" (read-from-string (s #x23 #x78 #x661 #x46)))
(probe "arabic float is a symbol" (read-from-string (s #x661 #x2e #x665)))
(probe "parse-integer arabic" (parse-integer (s #x661 #x662)))
(probe "prin1 lower-case symbol" (prin1-to-string (intern (s #xe9 #x54))))
(probe "prin1 upper-case symbol" (prin1-to-string (intern (s #xc9 #x54))))
(probe "prin1 digit symbol" (prin1-to-string (intern (s #x661 #x662))))
(probe "print-case downcase" (let ((*print-case* :downcase)) (prin1-to-string (intern (s #xc9 #x54)))))
(probe "format ~(" (format nil "~(~a~)" (s #xc9 #x41 #x3a3)))
(probe "format ~:@(" (format nil "~:@(~a~)" (s #xe9 #x61)))
(probe "format ~@(" (format nil "~@(~a~)" (s #xe9 #x54 #xc9)))
(probe "string-upcase" (string-upcase (s #xe9 #xff #xdf #x3c3 #x3c2)))
(probe "string-downcase" (string-downcase (s #xc9 #x178 #x3a3 #x130)))
(probe "string-capitalize" (string-capitalize (s #xe9 #x74 #xe9 32 #xe9 #x661 #xe9)))
(probe "nstring-upcase" (nstring-upcase (copy-seq (s #xe9 #x74))))
(probe "string-equal" (string-equal (s #xe9 #x3a3) (s #xc9 #x3c3)))
(probe "string-lessp _ a" (string-lessp "_" "a"))
(probe "char-lessp _ a" (char-lessp #\_ #\a))
(probe "char-equal kelvin k" (char-equal (code-char 8490) #\k))
(probe "char-equal long-s s" (char-equal (code-char 383) #\s))
(probe "char-equal sigma" (char-equal (code-char #x3c2) (code-char #x3a3)))
(probe "titlecase digraph is caseless" (list (char-code (r-upcase (code-char #x1c5))) (char-code (r-downcase (code-char #x1c5)))
                                             (r-both-p (code-char #x1c5)) (alpha-char-p (code-char #x1c5))))
(probe "keyword identity" (eq (intern (s #xe9) "KEYWORD") (read-from-string (s #x3a #xe9))))
(probe "keyword name" (symbol-name (read-from-string (s #x3a #xe9))))
(probe "escaped symbol keeps case" (symbol-name (read-from-string (s #x7c #xe9 #x7c))))
(probe "intern lower vs upper distinct" (eq (intern (s #xe9)) (intern (s #xc9))))
(probe "find-symbol after read" (eq (find-symbol (s #xc9 #x54 #xc9)) (read-from-string (s #xe9 #x74 #xe9))))
