;;;; test/unicode-case.lisp -- modus folds case, classifies letters and reads digits as SBCL
;;;; does, for every code point.  Prints lines that must be IDENTICAL under SBCL and modus;
;;;; test/run-unicode-case.sh runs both and compares (no data file: SBCL is the oracle).
;;;;
;;;; Before scripts/gen-unicode-case.lisp's tables, modus stopped at ASCII: (char-upcase
;;;; #\LATIN_SMALL_LETTER_E_WITH_ACUTE) was itself, so (read-from-string "été") was a
;;;; symbol named "été" where SBCL's is "ÉTÉ", string-equal "é" "É" was NIL, and comb's \w
;;;; missed every non-ASCII letter.

(defun s (&rest codes) (map 'string #'code-char codes))

(defun digest-block (lo hi)
  "An order-sensitive digest of every answer for code points LO..HI-1."
  (let ((h 0))
    (loop for c from lo below hi
          for ch = (code-char c)
          when ch
            do (dolist (v (list (char-code (char-upcase ch)) (char-code (char-downcase ch))
                                (if (upper-case-p ch) 1 0) (if (lower-case-p ch) 1 0)
                                (if (both-case-p ch) 1 0) (if (alpha-char-p ch) 1 0)
                                (if (alphanumericp ch) 1 0) (or (digit-char-p ch 36) 99)
                                (char-code (char-downcase (char-upcase ch)))))
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
(probe "keyword identity" (eq (intern (s #xe9) "KEYWORD") (read-from-string (s #x3a #xe9))))
(probe "keyword name" (symbol-name (read-from-string (s #x3a #xe9))))
(probe "escaped symbol keeps case" (symbol-name (read-from-string (s #x7c #xe9 #x7c))))
(probe "intern lower vs upper distinct" (eq (intern (s #xe9)) (intern (s #xc9))))
(probe "find-symbol after read" (eq (find-symbol (s #xc9 #x54 #xc9)) (read-from-string (s #xe9 #x74 #xe9))))
