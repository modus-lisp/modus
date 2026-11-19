;;;; test/unicode-char-names-sbcl.lisp -- the EXPECTED answers for test/unicode-char-names.lisp,
;;;; from SBCL itself.  Run by that test (sbcl --script this-file OUT), or by hand.
;;;;
;;;; Lines:  N <code> <name>   every code point SBCL names, plus a sample of unnamed ones (U<hex>)
;;;;         A <code> <name>   an alias SBCL's NAME-CHAR accepts (Unicode 1.0 names), code it gives
;;;;         R <code> <token>  what SBCL's reader makes of #\<token>; code -1 = reader error
(defun hex (n) (format nil "~X" n))
(defparameter *reads*
  '("LOWER_ONE_EIGHTH_BLOCK" "lower_one_eighth_block" "Replacement_Character" "No-break_space"
    "NO-BREAK_SPACE" "U+FEFF" "UFEFF" "u+feff" "ufeff" "U+41" "U0041" "U+10FFFF" "U10ffff" "UD800"
    "HANGUL_SYLLABLE_GA" "hangul_syllable_gag" "HANGUL_SYLLABLE_HIH" "CJK_COMPATIBILITY_IDEOGRAPH-F900"
    "cjk_compatibility_ideograph-2fa1d" "Tangut_Component-001" "TANGUT_COMPONENT-768"
    "VARIATION_SELECTOR-1" "VARIATION_SELECTOR-17" "VARIATION_SELECTOR-256"
    "KHITAN_SMALL_SCRIPT_CHARACTER-18B00" "NUSHU_CHARACTER-1B170" "GRINNING_FACE"
    "Latin_Small_Letter_A" "LINE_SEPARATOR" "PARAGRAPH_SEPARATOR" "ZERO_WIDTH_NO-BREAK_SPACE"
    "BYTE_ORDER_MARK" "CHARACTER_TABULATION" "Next-Line" "next-line" "Application-Program-Command"
    "^a" "^@" "^[" "Lf" "Nl" "Ff" "Sp" "Del" "Esc" "Escape" "Altmode" "Alt" "Nul" "Null" "Bel"
    "Bell" "Vt" "Page" "Form" "Formfeed" "Linefeed" "Rubout" "Backspace" "Tab" "Return" "U80"
    "Space" "Newline"
    ;; SBCL refuses these
    "CJK_UNIFIED_IDEOGRAPH-4E00" "No_break_space" "No-break-space" "Bogus_Name" "U+110000"
    "UFFFFFFFF" "CJK_COMPATIBILITY_IDEOGRAPH-0F900" "TANGUT_COMPONENT-1" "VARIATION_SELECTOR-017"
    "HANGUL_SYLLABLE_" "HANGUL_SYLLABLE_X" "U+" "U+G" "LATIN_CAPITAL_LETTER_A_"))
(with-open-file (o (or (second sb-ext:*posix-argv*) "/dev/stdout")
                   :direction :output :if-exists :supersede :external-format :utf-8)
  (loop for c from 0 below char-code-limit
        for nm = (char-name (code-char c))
        do (when (or (string/= nm (concatenate 'string "U" (hex c))) (< c 256) (zerop (mod c 997)))
             (format o "N ~D ~A~%" c nm)))
  (loop for c from 0 below char-code-limit
        for u = (sb-unicode:unicode-1-name (code-char c))
        do (when (and u (plusp (length u)))
             (let ((hit (name-char u)))
               (format o "A ~D ~A~%" (if hit (char-code hit) -1) u))))
  (dolist (tok *reads*)
    (let ((c (handler-case (read-from-string (concatenate 'string "#\\" tok))
               (reader-error () nil))))
      (format o "R ~D ~A~%" (if (characterp c) (char-code c) -1) tok))))
