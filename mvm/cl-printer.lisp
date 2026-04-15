;;;; cl-printer.lisp — Printer and format
;;;; Part of the Modus CL runtime. Depends on cl-streams.lisp.

;;; ============================================================
;;; Layer 3: Printer — respects *print-* variables
;;; ============================================================

;;; Write a single char code to stream (may be nil for *standard-output*)
(defun %print-char (code stream)
  (write-char-to-stream code stream))

;;; Write a string to stream
(defun %print-string-raw (str stream)
  (let ((len (array-length str)) (i 0))
    (loop
      (when (= i len) (return nil))
      (%print-char (aref str i) stream)
      (setq i (+ i 1)))))

;;; Write a decimal integer (always base 10, used for radix prefix numbers)
(defun %print-decimal-to-stream (n stream)
  (if (< n 0)
      (progn (%print-char 45 stream)
             (%print-decimal-to-stream (- 0 n) stream))
      (if (= n 0)
          (%print-char 48 stream)
          (let ((digits nil) (tmp n))
            (loop
              (when (= tmp 0) (return nil))
              (setq digits (cons (+ 48 (mod tmp 10)) digits))
              (setq tmp (truncate tmp 10)))
            (dolist (d digits) (%print-char d stream))))))

;;; Digit char for base N (0-9 A-Z)
(defun %digit-char-upper (n)
  (if (< n 10) (+ 48 n) (+ 55 n)))   ; 55 = 65-10

;;; Print integer in given base
(defun %print-integer-in-base (n base stream)
  (if (< n 0)
      (progn (%print-char 45 stream)  ; -
             (%print-integer-in-base (- 0 n) base stream))
      (if (= n 0)
          (%print-char 48 stream)   ; 0
          (let ((digits nil) (tmp n))
            (loop
              (when (= tmp 0) (return nil))
              (setq digits (cons (%digit-char-upper (mod tmp base)) digits))
              (setq tmp (truncate tmp base)))
            (dolist (d digits) (%print-char d stream))))))

;;; Print radix prefix: #b, #o, #x, #Nr
(defun %print-radix-prefix (base stream)
  (cond
    ((= base 2)  (%print-char 35 stream) (%print-char 98 stream))  ; #b
    ((= base 8)  (%print-char 35 stream) (%print-char 111 stream)) ; #o
    ((= base 16) (%print-char 35 stream) (%print-char 120 stream)) ; #x
    (t (%print-char 35 stream)
       (%print-decimal-to-stream base stream)
       (%print-char 114 stream))))  ; #Nr

;;; Apply *print-case* to a symbol name character
(defun %apply-print-case (ch case readtable-case)
  ;; ch is a char code
  ;; CL spec: interaction between readtable-case and *print-case*
  ;; Simple approach: just apply *print-case* directly
  (cond
    ((eq case :upcase)
     (if (and (>= ch 97) (<= ch 122)) (- ch 32) ch))
    ((eq case :downcase)
     (if (and (>= ch 65) (<= ch 90)) (+ ch 32) ch))
    ((eq case :capitalize)
     ch)  ; handled per-word
    (t ch)))  ; :preserve

;;; Print a symbol name applying *print-case*
(defun %print-symbol-name-with-case (name stream case)
  (let ((len (array-length name)) (i 0))
    (if (eq case :capitalize)
        (let ((at-word-start t))
          (loop
            (when (= i len) (return nil))
            (let ((ch (aref name i)))
              (let ((alpha (and (>= ch 65)
                                (or (<= ch 90)
                                    (and (>= ch 97) (<= ch 122))))))
                (if alpha
                    (if at-word-start
                        (progn
                          (%print-char (if (and (>= ch 97) (<= ch 122)) (- ch 32) ch) stream)
                          (setq at-word-start nil))
                        (%print-char (if (and (>= ch 65) (<= ch 90)) (+ ch 32) ch) stream))
                    (progn (%print-char ch stream) (setq at-word-start t)))))
            (setq i (+ i 1))))
        (loop
          (when (= i len) (return nil))
          (let ((ch (aref name i)))
            (%print-char (%apply-print-case ch case :upcase) stream))
          (setq i (+ i 1))))))

;;; Check if a symbol name needs escaping (contains special chars)
(defun %sym-name-needs-escape-p (name)
  (let ((len (array-length name)) (i 0) (needs-escape nil))
    (when (= len 0) (return-from %sym-name-needs-escape-p t))
    (loop
      (when (= i len) (return needs-escape))
      (let ((ch (aref name i)))
        (when (or (= ch 32) (= ch 40) (= ch 41) (= ch 34) (= ch 39)
                  (= ch 96) (= ch 44) (= ch 59) (= ch 35) (= ch 92)
                  (= ch 124) (= ch 58) (= ch 9) (= ch 10) (= ch 13))
          (setq needs-escape t)))
      (setq i (+ i 1)))
    needs-escape))

;;; Print a symbol to stream respecting all print variables
(defun %print-symbol-to-stream (sym stream)
  (let ((escape *print-escape*)
        (case *print-case*)
        (gensym *print-gensym*)
        (readably *print-readably*))
    (let ((name (if (%cl-sym-p sym) (%cl-sym-name sym) (symbol-name sym)))
          (pkg (if (%cl-sym-p sym) (%cl-sym-package sym) nil)))
      ;; Determine if we need package qualifier
      (let ((cur-pkg *package*))
        (let ((need-qualifier
               (if (or escape readably)
                   ;; Need qualifier if symbol not accessible in current pkg
                   (if (null pkg)
                       ;; Uninterned symbol
                       (if (or gensym readably) t nil)
                       ;; Check if symbol is accessible in current package
                       (let ((accessible nil))
                         (when (%pkg-p cur-pkg)
                           (let ((found (%pkg-find-sym name cur-pkg)))
                             (when (and found (eq found sym))
                               (setq accessible t))))
                         (not accessible)))
                   nil)))
          (cond
            ;; Uninterned symbol: print #:name
            ((and (null pkg) (or gensym readably escape))
             (%print-char 35 stream) ; #
             (%print-char 58 stream) ; :
             (%print-symbol-name-with-case name stream case))
            ;; Package-qualified
            (need-qualifier
             (let ((pkg-name (if (%pkg-p pkg) (package-name pkg) "")))
               (%print-string-raw pkg-name stream)
               ;; Check if external: use : else ::
               (let ((ext (if (%pkg-p pkg)
                              (let ((s (%pkg-find-sym name pkg)))
                                (and s (%pkg-sym-external-p pkg s)))
                              nil)))
                 (if ext
                     (%print-char 58 stream)  ; :
                     (progn (%print-char 58 stream)  ; ::
                            (%print-char 58 stream))))
               (%print-symbol-name-with-case name stream case)))
            ;; No qualifier needed
            (t
             (%print-symbol-name-with-case name stream case))))))))

;;; Check if symbol is external in package
(defun %pkg-sym-external-p (pkg sym)
  (let ((name (%cl-sym-name sym)))
    (if (%pkg-p pkg)
        (let ((ext-list (%pkg-external pkg)))
          (let ((found nil))
            (dolist (s ext-list)
              (when (and (%cl-sym-p s) (string-equal (%cl-sym-name s) name))
                (setq found t)))
            found))
        nil)))

;;; Find symbol in package (non-closure version)
(defvar *%find-sym-name* nil)
(defvar *%find-sym-result* nil)
(defun %find-sym-match (s)
  (when (and (%cl-sym-p s) (string-equal (%cl-sym-name s) *%find-sym-name*))
    (setq *%find-sym-result* s)))
(defun %pkg-find-sym (name pkg)
  (if (%pkg-p pkg)
      (progn
        (setq *%find-sym-name* name)
        (setq *%find-sym-result* nil)
        (%do-symbols-fn #'%find-sym-match pkg)
        *%find-sym-result*)
      nil))

;;; Main printer: print OBJ to STREAM respecting all *print-* variables
;;; LEVEL: current nesting level (nil = not tracking)
;;; ESCAPE: current escape setting
(defun %write-obj (obj stream level escape)
  (let ((plen *print-length*)
        (plev *print-level*)
        (pbase *print-base*)
        (pradix *print-radix*)
        (pcase *print-case*)
        (pescape *print-escape*)
        (preadably *print-readably*)
        (pgensym *print-gensym*)
        (parray *print-array*))
    ;; *print-readably* overrides *print-escape*
    (when preadably (setq escape t))
    (cond
      ;; NIL
      ((null obj)
       (%print-char 78 stream) (%print-char 73 stream) (%print-char 76 stream))
      ;; T
      ((eq obj t)
       (%print-char 84 stream))
      ;; Character
      ((characterp obj)
       (if escape
           (let ((code (char-code obj)))
             (%print-char 35 stream)   ; #
             (%print-char 92 stream)   ; backslash
             (cond
               ((= code 32)  (%print-string-raw "Space" stream))
               ((= code 10)  (%print-string-raw "Newline" stream))
               ((= code 9)   (%print-string-raw "Tab" stream))
               ((= code 13)  (%print-string-raw "Return" stream))
               ((= code 12)  (%print-string-raw "Page" stream))
               ((= code 8)   (%print-string-raw "Backspace" stream))
               ((= code 7)   (%print-string-raw "Altmode" stream))
               ((= code 127) (%print-string-raw "Rubout" stream))
               ((= code 0)   (%print-string-raw "Null" stream))
               (t (%print-char code stream))))
           (%print-char (char-code obj) stream)))
      ;; Integer (fixnum)
      ((fixnump obj)
       (when pradix (%print-radix-prefix pbase stream))
       (%print-integer-in-base obj pbase stream))
      ;; Float
      ((floatp-impl obj)
       ;; Use standard float printing
       (%print-float-to-stream obj stream escape))
      ;; Ratio
      ((ratiop obj)
       (%print-integer-in-base (ratio-numerator obj) pbase stream)
       (%print-char 47 stream)  ; /
       (%print-integer-in-base (ratio-denominator obj) pbase stream))
      ;; String
      ((stringp obj)
       (if escape
           (progn
             (%print-char 34 stream)  ; "
             (let ((len (array-length obj)) (i 0))
               (loop
                 (when (= i len) (return nil))
                 (let ((ch (aref obj i)))
                   (when (or (= ch 34) (= ch 92))
                     (%print-char 92 stream))  ; escape " and backslash
                   (%print-char ch stream))
                 (setq i (+ i 1))))
             (%print-char 34 stream))  ; "
           ;; princ-style: no quotes
           (%print-string-raw obj stream)))
      ;; Symbol
      ((or (%cl-sym-p obj) (eq obj t) (null obj))
       (%print-symbol-to-stream obj stream))
      ;; Cons (list)
      ((consp obj)
       ;; Check *print-level*
       (if (and plev (not (null level)) (>= level plev))
           (%print-char 35 stream)   ; #
           (let ((next-level (if (null level) 1 (+ level 1))))
             (%print-char 40 stream)  ; (
             (%write-obj (car obj) stream next-level escape)
             (let ((tail (cdr obj)) (count 1))
               (loop
                 (cond
                   ((null tail) (return nil))
                   ((and plen (>= count plen))
                    (%print-string-raw " ..." stream)
                    (return nil))
                   ((consp tail)
                    (%print-char 32 stream)  ; space
                    (%write-obj (car tail) stream next-level escape)
                    (setq tail (cdr tail))
                    (setq count (+ count 1)))
                   (t
                    (%print-char 32 stream)  ; space
                    (%print-char 46 stream)  ; .
                    (%print-char 32 stream)  ; space
                    (%write-obj tail stream next-level escape)
                    (return nil)))))
             (%print-char 41 stream))))  ; )
      ;; Array/string (non-cons)
      ((arrayp obj)
       (if (not parray)
           ;; Print as unreadable
           (progn
             (%print-char 35 stream)
             (%print-char 60 stream)
             (%print-string-raw "Array" stream)
             (%print-char 62 stream))
           ;; Print #(...)
           (let ((len (array-length obj)))
             (%print-char 35 stream)
             (%print-char 40 stream)   ; #(
             (let ((i 0))
               (loop
                 (when (= i len) (return nil))
                 (when (> i 0) (%print-char 32 stream))
                 (when (and plen (>= i plen))
                   (%print-string-raw "..." stream)
                   (return nil))
                 (%write-obj (aref obj i) stream
                             (if (null level) 1 (+ level 1)) escape)
                 (setq i (+ i 1))))
             (%print-char 41 stream))))  ; )
      ;; Anything else: #<type>
      (t
       (%print-char 35 stream)
       (%print-char 60 stream)
       (%print-char 63 stream)
       (%print-char 62 stream)))))

;;; Float printing helper
(defun float-to-string (f)
  "Convert boxed float to string representation."
  ;; Float stored as array [sign*mantissa, divisor]
  ;; where value = (sign*mantissa) / divisor
  (let ((s (make-string-output-stream)))
    (let ((smant (aref f 0))
          (divisor (aref f 1)))
      ;; Handle sign
      (let ((neg (< smant 0))
            (mant (abs smant)))
        (when neg (%print-char 45 s))  ; -
        ;; Compute integer and fractional parts
        (let ((int-part (truncate mant divisor))
              (frac-num (mod mant divisor)))
          (%print-decimal-to-stream int-part s)
          (%print-char 46 s)  ; .
          ;; Print fractional digits
          (if (= frac-num 0)
              (%print-char 48 s)  ; 0
              ;; Print significant digits of fraction
              (let ((digits nil)
                    (rem frac-num)
                    (div divisor)
                    (max-digits 15))
                (let ((count 0))
                  (loop
                    (when (or (= rem 0) (= count max-digits)) (return nil))
                    (setq rem (* rem 10))
                    (setq digits (cons (truncate rem div) digits))
                    (setq rem (mod rem div))
                    (setq count (+ count 1))))
                ;; Remove trailing zeros
                (let ((d (nreverse digits)))
                  (let ((trimmed nil) (trailing-zero t))
                    ;; Find last non-zero digit
                    (let ((lst (nreverse d)))
                      (loop
                        (when (or (null lst) (not (= (car lst) 0)))
                          (return nil))
                        (setq lst (cdr lst)))
                      (setq trimmed (nreverse lst)))
                    (if (null trimmed)
                        (%print-char 48 s)  ; 0
                        (dolist (digit trimmed)
                          (%print-char (+ 48 digit) s))))))))))
    (get-output-stream-string s)))

(defun %print-float-to-stream (f stream escape)
  (let ((s (float-to-string f)))
    (if (stringp s)
        (%print-string-raw s stream)
        (%print-string-raw "0.0" stream))))

;;; write-to-stream: write OBJ to STREAM (prin1 style, escape=t)
(defun write-to-stream (obj stream)
  (%write-obj obj stream nil t))

;;; princ-to-stream: write OBJ to STREAM (princ style, escape=nil)
(defun princ-to-stream (obj stream)
  (%write-obj obj stream nil nil))

;;; write-to-string: return printed representation as string
(defun write-to-string (obj &rest args)
  "Return string representation of OBJ. Keyword args override *print-* vars."
  (let ((s (make-string-output-stream)))
    ;; Parse keyword args
    (let ((escape *print-escape*)
          (base *print-base*)
          (radix *print-radix*)
          (pcase *print-case*)
          (level *print-level*)
          (length *print-length*)
          (circle *print-circle*)
          (gensym *print-gensym*)
          (array *print-array*)
          (readably *print-readably*)
          (stream-arg nil))
      (let ((rest args))
        (loop
          (when (null rest) (return nil))
          (let ((key (car rest)) (val (cadr rest)))
            (cond
              ((eq key :escape)   (setq escape val))
              ((eq key :base)     (setq base val))
              ((eq key :radix)    (setq radix val))
              ((eq key :case)     (setq pcase val))
              ((eq key :level)    (setq level val))
              ((eq key :length)   (setq length val))
              ((eq key :circle)   (setq circle val))
              ((eq key :gensym)   (setq gensym val))
              ((eq key :array)    (setq array val))
              ((eq key :readably) (setq readably val))
              ((eq key :stream)   (setq stream-arg val))
              ((eq key :pretty)   nil)  ; ignore
              ((eq key :lines)    nil)  ; ignore
              ((eq key :miser-width) nil)
              ((eq key :right-margin) nil)
              ((eq key :pprint-dispatch) nil)))
          (setq rest (cddr rest))))
      ;; Temporarily bind print variables
      (let ((*print-escape* escape)
            (*print-base* base)
            (*print-radix* radix)
            (*print-case* pcase)
            (*print-level* level)
            (*print-length* length)
            (*print-circle* circle)
            (*print-gensym* gensym)
            (*print-array* array)
            (*print-readably* readably))
        (declare (special *print-escape* *print-base* *print-radix* *print-case*
                          *print-level* *print-length* *print-circle* *print-gensym*
                          *print-array* *print-readably*))
        (%write-obj obj s nil (if readably t escape))))
    (get-output-stream-string s)))

(defun prin1-to-string (obj)
  (let ((*print-escape* t))
    (declare (special *print-escape*))
    (write-to-string obj)))

(defun princ-to-string (obj)
  (let ((*print-escape* nil)
        (*print-readably* nil))
    (declare (special *print-escape* *print-readably*))
    (write-to-string obj)))

;;; Write OBJ to STREAM, respecting *print-* vars and keyword args
(defun %write-to-stream-with-keys (obj stream args)
  "Write OBJ to STREAM with keyword arg overrides."
  (let ((escape *print-escape*)
        (base *print-base*)
        (radix *print-radix*)
        (pcase *print-case*)
        (level *print-level*)
        (length *print-length*)
        (circle *print-circle*)
        (gensym *print-gensym*)
        (array *print-array*)
        (readably *print-readably*))
    (let ((rest args))
      (loop
        (when (null rest) (return nil))
        (let ((key (car rest)) (val (cadr rest)))
          (cond
            ((eq key :escape)   (setq escape val))
            ((eq key :base)     (setq base val))
            ((eq key :radix)    (setq radix val))
            ((eq key :case)     (setq pcase val))
            ((eq key :level)    (setq level val))
            ((eq key :length)   (setq length val))
            ((eq key :circle)   (setq circle val))
            ((eq key :gensym)   (setq gensym val))
            ((eq key :array)    (setq array val))
            ((eq key :readably) (setq readably val))
            ((eq key :stream)   nil)  ; already handled
            ((eq key :pretty)   nil)
            ((eq key :lines)    nil)
            ((eq key :miser-width) nil)
            ((eq key :right-margin) nil)
            ((eq key :pprint-dispatch) nil)
            ((eq key :allow-other-keys) nil)))
        (setq rest (cddr rest))))
    (let ((*print-escape* escape)
          (*print-base* base)
          (*print-radix* radix)
          (*print-case* pcase)
          (*print-level* level)
          (*print-length* length)
          (*print-circle* circle)
          (*print-gensym* gensym)
          (*print-array* array)
          (*print-readably* readably))
      (declare (special *print-escape* *print-base* *print-radix* *print-case*
                        *print-level* *print-length* *print-circle* *print-gensym*
                        *print-array* *print-readably*))
      (%write-obj obj stream nil (if readably t escape)))))

(defun prin1 (obj &rest stream-arg)
  (let ((stream (%resolve-output-stream (if stream-arg (car stream-arg) nil)))
        (*print-escape* t))
    (declare (special *print-escape*))
    (%write-obj obj stream nil t)
    obj))

(defun princ (obj &rest stream-arg)
  (let ((stream (%resolve-output-stream (if stream-arg (car stream-arg) nil)))
        (*print-escape* nil)
        (*print-readably* nil))
    (declare (special *print-escape* *print-readably*))
    (%write-obj obj stream nil nil)
    obj))

(defun write (obj &rest args)
  "Write OBJ with keyword args controlling print vars."
  ;; Parse :stream keyword
  (let ((stream *standard-output*))
    (let ((rest args))
      (loop
        (when (null rest) (return nil))
        (when (eq (car rest) :stream)
          (setq stream (cadr rest))
          (return nil))
        (setq rest (cddr rest))))
    (let ((s (%resolve-output-stream stream)))
      ;; Remove :stream from args for %write-to-stream-with-keys
      (let ((filtered-args nil) (rest args))
        (loop
          (when (null rest) (return nil))
          (if (eq (car rest) :stream)
              (setq rest (cddr rest))
              (progn
                (setq filtered-args (cons (car rest) filtered-args))
                (when (cdr rest)
                  (setq filtered-args (cons (cadr rest) filtered-args)))
                (setq rest (cddr rest)))))
        (%write-to-stream-with-keys obj s (nreverse filtered-args))))
    obj))

(defun print (obj &rest stream-arg)
  (let ((stream (%resolve-output-stream (if stream-arg (car stream-arg) nil)))
        (*print-escape* t))
    (declare (special *print-escape*))
    (%write-char-to-stream 10 stream)  ; newline first
    (%write-obj obj stream nil t)
    (%write-char-to-stream 32 stream)  ; trailing space
    obj))

(defun pprint (obj &rest stream-arg)
  "Pretty-print OBJ (stub: same as prin1 + newline)."
  (let ((stream (%resolve-output-stream (if stream-arg (car stream-arg) nil)))
        (*print-escape* t)
        (*print-pretty* t))
    (declare (special *print-escape* *print-pretty*))
    (%write-char-to-stream 10 stream)
    (%write-obj obj stream nil t)
    (values)))

;;; print-unreadable-object macro support
;;; (print-unreadable-object (obj stream :type t :identity t) body...)
;;; is rewritten by SBCL to (%print-unreadable-object obj stream type-p identity-p thunk)
(defun %print-unreadable-object (obj stream type-p identity-p thunk)
  "Implement print-unreadable-object."
  (let ((s (%resolve-output-stream stream)))
    (%print-char 35 s)  ; #
    (%print-char 60 s)  ; <
    (when type-p
      ;; Print type name
      (let ((type-str
             (cond
               ((null obj) "NULL")
               ((eq obj t) "BOOLEAN")
               ((fixnump obj) "FIXNUM")
               ((floatp-impl obj) "FLOAT")
               ((ratiop obj) "RATIO")
               ((stringp obj) "STRING")
               ((characterp obj) "CHARACTER")
               ((%cl-sym-p obj) "SYMBOL")
               ((consp obj) "CONS")
               ((arrayp obj) "ARRAY")
               ((packagep obj) "PACKAGE")
               ((streamp obj) "STREAM")
               (t "T"))))
        (%print-string-raw type-str s)
        (when (and thunk identity-p)
          (%print-char 32 s))))
    (when thunk
      (when (and type-p (not identity-p))
        (%print-char 32 s))
      (funcall thunk))
    (when identity-p
      (when (or thunk type-p) (%print-char 32 s))
      ;; Print a fake address (use object hash or 0)
      (%print-string-raw "{" s)
      (%print-decimal-to-stream 0 s)
      (%print-string-raw "}" s))
    (%print-char 62 s)   ; >
    nil))

;;; Alias: my-with-standard-io-syntax (used in printer-aux tests)
(defun my-with-standard-io-syntax (thunk)
  (%with-standard-io-syntax thunk))

;;; ============================================================
;;; format — comprehensive implementation
;;; ============================================================

;;; Parse a format directive argument (integer or nil)
(defun %fmt-parse-int (str pos end)
  "Parse optional integer at POS in STR. Returns (value . new-pos)."
  (if (>= pos end)
      (cons nil pos)
      (let ((ch (aref str pos)) (neg nil) (n 0) (found nil))
        (when (= ch 45) ; -
          (setq neg t) (setq pos (+ pos 1)))
        (loop
          (when (>= pos end) (return nil))
          (let ((d (aref str pos)))
            (when (or (< d 48) (> d 57)) (return nil))
            (setq n (+ (* n 10) (- d 48)))
            (setq found t)
            (setq pos (+ pos 1))))
        (cons (if found (if neg (- 0 n) n) nil) pos))))

;;; Format ~R directive: radix, english cardinal/ordinal
(defun %format-r (n base colonp atp stream)
  (cond
    ;; ~R with base: print in that base
    (base
     (when atp (%print-char 43 stream))  ; + for positive with @
     (%print-integer-in-base n base stream))
    ;; ~R without base, no modifiers: cardinal English
    ((not colonp)
     (if atp
         (%format-ordinal n stream)
         (%format-cardinal n stream)))
    ;; ~:R: ordinal
    (colonp
     (%format-ordinal n stream))))

(defun %cardinal-ones (n stream)
  (cond
    ((= n 0) (%print-string-raw "zero" stream))
    ((= n 1) (%print-string-raw "one" stream))
    ((= n 2) (%print-string-raw "two" stream))
    ((= n 3) (%print-string-raw "three" stream))
    ((= n 4) (%print-string-raw "four" stream))
    ((= n 5) (%print-string-raw "five" stream))
    ((= n 6) (%print-string-raw "six" stream))
    ((= n 7) (%print-string-raw "seven" stream))
    ((= n 8) (%print-string-raw "eight" stream))
    ((= n 9) (%print-string-raw "nine" stream))
    ((= n 10) (%print-string-raw "ten" stream))
    ((= n 11) (%print-string-raw "eleven" stream))
    ((= n 12) (%print-string-raw "twelve" stream))
    ((= n 13) (%print-string-raw "thirteen" stream))
    ((= n 14) (%print-string-raw "fourteen" stream))
    ((= n 15) (%print-string-raw "fifteen" stream))
    ((= n 16) (%print-string-raw "sixteen" stream))
    ((= n 17) (%print-string-raw "seventeen" stream))
    ((= n 18) (%print-string-raw "eighteen" stream))
    (t (%print-string-raw "nineteen" stream))))

(defun %cardinal-tens (n stream)
  (cond
    ((= n 2) (%print-string-raw "twenty" stream))
    ((= n 3) (%print-string-raw "thirty" stream))
    ((= n 4) (%print-string-raw "forty" stream))
    ((= n 5) (%print-string-raw "fifty" stream))
    ((= n 6) (%print-string-raw "sixty" stream))
    ((= n 7) (%print-string-raw "seventy" stream))
    ((= n 8) (%print-string-raw "eighty" stream))
    (t (%print-string-raw "ninety" stream))))

(defun %format-cardinal (n stream)
  "Print N as English cardinal number."
  (cond
    ((< n 0)
     (%print-string-raw "negative " stream)
     (%format-cardinal (- 0 n) stream))
    ((= n 0) (%print-string-raw "zero" stream))
    ((< n 20) (%cardinal-ones n stream))
    ((< n 100)
     (let ((tens (truncate n 10))
           (ones (mod n 10)))
       (%cardinal-tens tens stream)
       (when (> ones 0)
         (%print-char 45 stream)
         (%cardinal-ones ones stream))))
    ((< n 1000)
     (let ((h (truncate n 100)) (r (mod n 100)))
       (%cardinal-ones h stream)
       (%print-string-raw " hundred" stream)
       (when (> r 0)
         (%print-char 32 stream)
         (%format-cardinal r stream))))
    ((< n 1000000)
     (let ((k (truncate n 1000)) (r (mod n 1000)))
       (%format-cardinal k stream)
       (%print-string-raw " thousand" stream)
       (when (> r 0)
         (%print-char 32 stream)
         (%format-cardinal r stream))))
    ((< n 1000000000)
     (let ((m (truncate n 1000000)) (r (mod n 1000000)))
       (%format-cardinal m stream)
       (%print-string-raw " million" stream)
       (when (> r 0)
         (%print-char 32 stream)
         (%format-cardinal r stream))))
    (t (%print-decimal-to-stream n stream))))

(defun %ordinal-suffix (last-word stream)
  (cond
    ((string-equal last-word "one") (%print-string-raw "first" stream))
    ((string-equal last-word "two") (%print-string-raw "second" stream))
    ((string-equal last-word "three") (%print-string-raw "third" stream))
    ((string-equal last-word "four") (%print-string-raw "fourth" stream))
    ((string-equal last-word "five") (%print-string-raw "fifth" stream))
    ((string-equal last-word "six") (%print-string-raw "sixth" stream))
    ((string-equal last-word "seven") (%print-string-raw "seventh" stream))
    ((string-equal last-word "eight") (%print-string-raw "eighth" stream))
    ((string-equal last-word "nine") (%print-string-raw "ninth" stream))
    ((string-equal last-word "ten") (%print-string-raw "tenth" stream))
    ((string-equal last-word "eleven") (%print-string-raw "eleventh" stream))
    ((string-equal last-word "twelve") (%print-string-raw "twelfth" stream))
    ((string-equal last-word "thirteen") (%print-string-raw "thirteenth" stream))
    ((string-equal last-word "fourteen") (%print-string-raw "fourteenth" stream))
    ((string-equal last-word "fifteen") (%print-string-raw "fifteenth" stream))
    ((string-equal last-word "sixteen") (%print-string-raw "sixteenth" stream))
    ((string-equal last-word "seventeen") (%print-string-raw "seventeenth" stream))
    ((string-equal last-word "eighteen") (%print-string-raw "eighteenth" stream))
    ((string-equal last-word "nineteen") (%print-string-raw "nineteenth" stream))
    ((string-equal last-word "twenty") (%print-string-raw "twentieth" stream))
    ((string-equal last-word "thirty") (%print-string-raw "thirtieth" stream))
    ((string-equal last-word "forty") (%print-string-raw "fortieth" stream))
    ((string-equal last-word "fifty") (%print-string-raw "fiftieth" stream))
    ((string-equal last-word "sixty") (%print-string-raw "sixtieth" stream))
    ((string-equal last-word "seventy") (%print-string-raw "seventieth" stream))
    ((string-equal last-word "eighty") (%print-string-raw "eightieth" stream))
    ((string-equal last-word "ninety") (%print-string-raw "ninetieth" stream))
    ((string-equal last-word "hundred") (%print-string-raw "hundredth" stream))
    ((string-equal last-word "thousand") (%print-string-raw "thousandth" stream))
    ((string-equal last-word "million") (%print-string-raw "millionth" stream))
    ((string-equal last-word "zero") (%print-string-raw "zeroth" stream))
    (t (%print-string-raw last-word stream) (%print-string-raw "th" stream))))

(defun %format-ordinal (n stream)
  "Print N as English ordinal."
  (cond
    ((< n 0)
     (%print-string-raw "negative " stream)
     (%format-ordinal (- 0 n) stream))
    ((= n 0) (%print-string-raw "zeroth" stream))
    (t
     ;; Build cardinal string, then transform last word to ordinal
     (let ((s (make-string-output-stream)))
       (%format-cardinal n s)
       (let ((cardinal (get-output-stream-string s)))
         (let ((last-space -1) (i 0) (len (array-length cardinal)))
           (loop
             (when (= i len) (return nil))
             (when (= (aref cardinal i) 32) (setq last-space i))
             (setq i (+ i 1)))
           (let ((prefix (if (= last-space -1) ""
                             (%substring cardinal 0 (+ last-space 1))))
                 (last-word (if (= last-space -1) cardinal
                                (%substring cardinal (+ last-space 1) len))))
             (%print-string-raw prefix stream)
             (%ordinal-suffix last-word stream))))))))

;;; Roman numeral printing for ~@R
(defun %format-roman (n stream &optional oldp)
  "Print N as Roman numerals. OLDP=t means old-style (IIII not IV)."
  (when (<= n 0) (return-from %format-roman nil))
  (let ((vals (list 1000 900 500 400 100 90 50 40 10 9 5 4 1))
        (strs (if oldp
                  (list "M" "DCCCC" "D" "CCCC" "C" "LXXXX" "L" "XXXX"
                        "X" "VIIII" "V" "IIII" "I")
                  (list "M" "CM" "D" "CD" "C" "XC" "L" "XL"
                        "X" "IX" "V" "IV" "I"))))
    (let ((vs vals) (ss strs) (rem n))
      (loop
        (when (or (null vs) (= rem 0)) (return nil))
        (let ((v (car vs)) (s (car ss)))
          (loop
            (when (< rem v) (return nil))
            (%print-string-raw s stream)
            (setq rem (- rem v)))
          (setq vs (cdr vs))
          (setq ss (cdr ss)))))))

;;; Pad string to minimum column
(defun %fmt-pad (str mincol colinc minpad padchar stream)
  "Write STR padded to MINCOL using PADCHAR, with MINPAD minimum padding."
  (let ((slen (if (stringp str) (array-length str) 0))
        (mc (if mincol mincol 0))
        (ci (if colinc colinc 1))
        (mp (if minpad minpad 0))
        (pc (if padchar padchar 32)))
    (let ((padding mp))
      ;; Increase padding until we meet mincol
      (loop
        (when (>= (+ slen padding) mc) (return nil))
        (setq padding (+ padding ci)))
      ;; Write padding then string (right-align style default)
      (let ((i 0))
        (loop
          (when (= i padding) (return nil))
          (%print-char pc stream)
          (setq i (+ i 1))))
      (when (stringp str) (%print-string-raw str stream)))))

;;; Format ~T: tabulate
(defun %fmt-tabulate (colnum colinc stream)
  (let ((cn (if colnum colnum 1))
        (ci (if colinc colinc 1)))
    ;; We don't track column position, so just emit spaces to next tab stop
    ;; Simplified: emit cn spaces
    (let ((i 0))
      (loop
        (when (= i cn) (return nil))
        (%print-char 32 stream)
        (setq i (+ i 1))))))

;;; Main format implementation
;;; Returns remaining args (for use by formatter)
(defun %format-impl (stream control args)
  "Core format. Returns remaining unused args."
  (let ((len (array-length control))
        (i 0)
        (arg-list args))
    (loop
      (when (>= i len) (return arg-list))
      (let ((ch (aref control i)))
        (if (not (= ch 126))  ; not ~
            (progn
              (%print-char ch stream)
              (setq i (+ i 1)))
            ;; Parse directive
            (let ((pos (+ i 1))
                  (param1 nil) (param2 nil) (param3 nil) (param4 nil)
                  (colonp nil) (atp nil))
              ;; Parse parameters (comma-separated integers or v/V/# placeholders)
              (let ((params nil) (pcount 0))
                (loop
                  (when (>= pos len) (return nil))
                  (let ((c (aref control pos)))
                    (cond
                      ;; v or V: next argument as parameter
                      ((or (= c 118) (= c 86))
                       (setq params (cons (car arg-list) params))
                       (setq arg-list (cdr arg-list))
                       (setq pos (+ pos 1))
                       (setq pcount (+ pcount 1))
                       ;; check for comma
                       (when (and (< pos len) (= (aref control pos) 44))
                         (setq pos (+ pos 1))))
                      ;; # : remaining arg count
                      ((= c 35)
                       (setq params (cons (length arg-list) params))
                       (setq pos (+ pos 1))
                       (setq pcount (+ pcount 1))
                       (when (and (< pos len) (= (aref control pos) 44))
                         (setq pos (+ pos 1))))
                      ;; ' : character parameter
                      ((= c 39)  ; '
                       (setq pos (+ pos 1))
                       (when (< pos len)
                         (setq params (cons (aref control pos) params))
                         (setq pos (+ pos 1))
                         (setq pcount (+ pcount 1)))
                       (when (and (< pos len) (= (aref control pos) 44))
                         (setq pos (+ pos 1))))
                      ;; Integer parameter
                      ((or (= c 45) (and (>= c 48) (<= c 57)))
                       (let ((pr (%fmt-parse-int control pos len)))
                         (setq params (cons (car pr) params))
                         (setq pos (cdr pr))
                         (setq pcount (+ pcount 1)))
                       ;; skip comma
                       (when (and (< pos len) (= (aref control pos) 44))
                         (setq pos (+ pos 1))))
                      ;; Comma alone: nil parameter
                      ((= c 44)
                       (setq params (cons nil params))
                       (setq pos (+ pos 1))
                       (setq pcount (+ pcount 1)))
                      ;; End of params
                      (t (return nil)))))
                ;; params is reversed, get first 4
                (setq params (nreverse params))
                (setq param1 (if (>= pcount 1) (nth 0 params) nil))
                (setq param2 (if (>= pcount 2) (nth 1 params) nil))
                (setq param3 (if (>= pcount 3) (nth 2 params) nil))
                (setq param4 (if (>= pcount 4) (nth 3 params) nil)))
              ;; Parse modifiers : and @
              (loop
                (when (>= pos len) (return nil))
                (let ((c (aref control pos)))
                  (cond
                    ((= c 58) (setq colonp t) (setq pos (+ pos 1)))   ; :
                    ((= c 64) (setq atp t) (setq pos (+ pos 1)))       ; @
                    (t (return nil)))))
              ;; Directive character
              (when (>= pos len) (return arg-list))
              (let ((dir (aref control pos)))
                (setq i (+ pos 1))
                (cond
                  ;; ~A — aesthetic
                  ((or (= dir 65) (= dir 97))
                   (let ((obj (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (let ((s (make-string-output-stream))
                           (*print-escape* nil))
                       (declare (special *print-escape*))
                       (%write-obj obj s nil nil)
                       (let ((str (get-output-stream-string s)))
                         (if (or param1 param2 param3 param4)
                             (%fmt-pad str param1 param2 param3 (if param4 param4 32) stream)
                             (%print-string-raw str stream))))))
                  ;; ~S — standard
                  ((or (= dir 83) (= dir 115))
                   (let ((obj (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (let ((s (make-string-output-stream))
                           (*print-escape* t))
                       (declare (special *print-escape*))
                       (%write-obj obj s nil t)
                       (let ((str (get-output-stream-string s)))
                         (if (or param1 param2 param3 param4)
                             (%fmt-pad str param1 param2 param3 (if param4 param4 32) stream)
                             (%print-string-raw str stream))))))
                  ;; ~W — write (like ~S but respects all print vars)
                  ((or (= dir 87) (= dir 119))
                   (let ((obj (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (%write-obj obj stream nil *print-escape*)))
                  ;; ~D — decimal
                  ((or (= dir 68) (= dir 100))
                   (let ((n (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (let ((s (make-string-output-stream)))
                       (when (and atp (>= n 0)) (%print-char 43 s))
                       (%print-integer-in-base n 10 s)
                       (let ((str (get-output-stream-string s)))
                         (if param1
                             (%fmt-pad str param1 (if param2 param2 1)
                                       (if param3 param3 0)
                                       (if param4 param4 32) stream)
                             (%print-string-raw str stream))))))
                  ;; ~B — binary
                  ((or (= dir 66) (= dir 98))
                   (let ((n (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (let ((s (make-string-output-stream)))
                       (when (and atp (>= n 0)) (%print-char 43 s))
                       (%print-integer-in-base n 2 s)
                       (let ((str (get-output-stream-string s)))
                         (if param1
                             (%fmt-pad str param1 (if param2 param2 1)
                                       (if param3 param3 0)
                                       (if param4 param4 32) stream)
                             (%print-string-raw str stream))))))
                  ;; ~O — octal
                  ((or (= dir 79) (= dir 111))
                   (let ((n (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (let ((s (make-string-output-stream)))
                       (when (and atp (>= n 0)) (%print-char 43 s))
                       (%print-integer-in-base n 8 s)
                       (let ((str (get-output-stream-string s)))
                         (if param1
                             (%fmt-pad str param1 (if param2 param2 1)
                                       (if param3 param3 0)
                                       (if param4 param4 32) stream)
                             (%print-string-raw str stream))))))
                  ;; ~X — hexadecimal
                  ((or (= dir 88) (= dir 120))
                   (let ((n (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (let ((s (make-string-output-stream)))
                       (when (and atp (>= n 0)) (%print-char 43 s))
                       (%print-integer-in-base n 16 s)
                       (let ((str (get-output-stream-string s)))
                         (if param1
                             (%fmt-pad str param1 (if param2 param2 1)
                                       (if param3 param3 0)
                                       (if param4 param4 32) stream)
                             (%print-string-raw str stream))))))
                  ;; ~R — radix
                  ((or (= dir 82) (= dir 114))
                   (let ((n (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (cond
                       ;; ~@R: Roman numerals (new-style)
                       ((and atp (not colonp) (not param1))
                        (%format-roman n stream nil))
                       ;; ~:@R or ~@:R: Roman numerals old-style
                       ((and atp colonp (not param1))
                        (%format-roman n stream t))
                       ;; ~:R: ordinal English
                       ((and colonp (not atp) (not param1))
                        (%format-ordinal n stream))
                       ;; ~R with no params: cardinal English
                       ((and (not colonp) (not atp) (not param1))
                        (%format-cardinal n stream))
                       ;; ~NR: base N
                       (param1
                        (%print-integer-in-base n param1 stream))
                       (t
                        (%format-cardinal n stream)))))
                  ;; ~C — character
                  ((or (= dir 67) (= dir 99))
                   (let ((c (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (let ((code (if (characterp c) (char-code c) c)))
                       (cond
                         ;; ~:C: spell out character name
                         (colonp
                          (cond
                            ((= code 32)  (%print-string-raw "Space" stream))
                            ((= code 10)  (%print-string-raw "Newline" stream))
                            ((= code 9)   (%print-string-raw "Tab" stream))
                            ((= code 13)  (%print-string-raw "Return" stream))
                            ((= code 12)  (%print-string-raw "Page" stream))
                            ((= code 8)   (%print-string-raw "Backspace" stream))
                            ((= code 127) (%print-string-raw "Rubout" stream))
                            ((= code 0)   (%print-string-raw "Null" stream))
                            (t (%print-char code stream))))
                         ;; ~@C: #\Name style
                         (atp
                          (%print-char 35 stream) (%print-char 92 stream)
                          (cond
                            ((= code 32)  (%print-string-raw "Space" stream))
                            ((= code 10)  (%print-string-raw "Newline" stream))
                            ((= code 9)   (%print-string-raw "Tab" stream))
                            ((= code 13)  (%print-string-raw "Return" stream))
                            ((= code 12)  (%print-string-raw "Page" stream))
                            ((= code 8)   (%print-string-raw "Backspace" stream))
                            ((= code 127) (%print-string-raw "Rubout" stream))
                            ((= code 0)   (%print-string-raw "Null" stream))
                            (t (%print-char code stream))))
                         ;; Plain ~C: print char
                         (t (%print-char code stream))))))
                  ;; ~% — newline
                  ((= dir 37)
                   (let ((count (if param1 param1 1)) (j 0))
                     (loop
                       (when (= j count) (return nil))
                       (%print-char 10 stream)
                       (setq j (+ j 1)))))
                  ;; ~& — fresh-line
                  ((= dir 38)
                   (let ((count (if param1 param1 1)))
                     (when (> count 0)
                       (%print-char 10 stream))
                     (let ((j 1))
                       (loop
                         (when (= j count) (return nil))
                         (%print-char 10 stream)
                         (setq j (+ j 1))))))
                  ;; ~~ — tilde
                  ((= dir 126)
                   (let ((count (if param1 param1 1)) (j 0))
                     (loop
                       (when (= j count) (return nil))
                       (%print-char 126 stream)
                       (setq j (+ j 1)))))
                  ;; ~| — page
                  ((= dir 124)
                   (let ((count (if param1 param1 1)) (j 0))
                     (loop
                       (when (= j count) (return nil))
                       (%print-char 12 stream)
                       (setq j (+ j 1)))))
                  ;; ~T — tabulate
                  ((or (= dir 84) (= dir 116))
                   (%fmt-tabulate param1 param2 stream))
                  ;; ~* — goto
                  ((= dir 42)
                   (let ((n (if param1 param1 1)))
                     (if colonp
                         ;; ~:* go back n args (not easily supported without arg array)
                         ;; stub: ignore
                         nil
                         ;; ~* skip n args
                         (let ((j 0))
                           (loop
                             (when (= j n) (return nil))
                             (setq arg-list (cdr arg-list))
                             (setq j (+ j 1)))))))
                  ;; ~? — indirection
                  ((= dir 63)
                   (let ((sub-control (car arg-list))
                         (sub-args (cadr arg-list)))
                     (setq arg-list (cddr arg-list))
                     (if atp
                         ;; ~@?: consume remaining args
                         (setq arg-list (%format-impl stream sub-control arg-list))
                         ;; ~?: use sub-args
                         (%format-impl stream sub-control sub-args))))
                  ;; ~P — plural
                  ((or (= dir 80) (= dir 112))
                   (let ((n (if colonp (car arg-list) (car arg-list))))
                     (unless colonp (setq arg-list (cdr arg-list)))
                     (let ((val (if (integerp n) n 2)))
                       (if atp
                           (if (= val 1) (%print-char 121 stream)  ; y
                               (%print-string-raw "ies" stream))
                           (if (/= val 1) (%print-char 115 stream))))))  ; s
                  ;; ~newline — ignore newline and leading whitespace
                  ((= dir 10)
                   ;; Skip leading whitespace in control string
                   (unless atp  ; ~@newline: keep newline
                     (loop
                       (when (>= i len) (return nil))
                       (let ((wc (aref control i)))
                         (when (not (or (= wc 32) (= wc 9) (= wc 10) (= wc 13)))
                           (return nil)))
                       (setq i (+ i 1)))))
                  ;; ~( ~) — case conversion
                  ((or (= dir 40) (= dir 41))
                   ;; ~(: start case conversion. ~): end.
                   ;; Find matching ~)
                   (when (= dir 40)
                     (let ((end-pos i) (depth 1))
                       (loop
                         (when (>= end-pos len) (return nil))
                         (when (= (aref control end-pos) 126)
                           (let ((nc (if (< (+ end-pos 1) len) (aref control (+ end-pos 1)) 0)))
                             (cond
                               ((or (= nc 40) (= nc 41)) ; nested (  )
                                (setq depth (if (= nc 40) (+ depth 1) (- depth 1)))
                                (when (= depth 0)
                                  ;; Found end: process substring
                                  (let ((sub (%substring control i end-pos))
                                        (s2 (make-string-output-stream)))
                                    (%format-impl s2 sub arg-list)
                                    (let ((result (get-output-stream-string s2)))
                                      (let ((converted
                                             (cond
                                               ((and colonp atp)
                                                (string-upcase result))
                                               (colonp
                                                ;; capitalize each word
                                                (string-capitalize result))
                                               (atp
                                                ;; capitalize first word
                                                (if (> (array-length result) 0)
                                                    (let ((r (%make-string-array (array-length result))))
                                                      (let ((first-upper nil))
                                                        (let ((k 0) (in-word nil))
                                                          (loop
                                                            (when (= k (array-length result)) (return nil))
                                                            (let ((c (aref result k)))
                                                              (let ((alpha (or (and (>= c 65) (<= c 90))
                                                                               (and (>= c 97) (<= c 122)))))
                                                                (if (and alpha (not in-word) (not first-upper))
                                                                    (progn
                                                                      (aset r k (if (and (>= c 97) (<= c 122)) (- c 32) c))
                                                                      (setq first-upper t)
                                                                      (setq in-word t))
                                                                    (progn
                                                                      (aset r k c)
                                                                      (if alpha (setq in-word t)
                                                                          (setq in-word nil))))))
                                                            (setq k (+ k 1)))))
                                                      r)
                                                    result))
                                               (t
                                                (string-downcase result)))))
                                        (%print-string-raw converted stream)))
                                  (setq i (+ end-pos 2))
                                  (return nil)))
                               (t nil))))
                         (setq end-pos (+ end-pos 1))))))
                  ;; ~[ ~] — conditional
                  ((or (= dir 91) (= dir 93))
                   (when (= dir 91)
                     ;; Find matching ~]
                     ;; Parse sections separated by ~;
                     ;; ~[: numeric selection by first arg
                     ;; ~@[: boolean test on first arg (true = process, false = skip + consume)
                     ;; ~:[: boolean test (false=first clause, true=second)
                     (let ((sections (list)) (start i) (depth 1) (pos2 i))
                       (loop
                         (when (>= pos2 len)
                           (setq sections (cons (%substring control start pos2) sections))
                           (return nil))
                         (when (= (aref control pos2) 126)
                           (let ((nc (if (< (+ pos2 1) len) (aref control (+ pos2 1)) 0)))
                             (cond
                               ((= nc 91) (setq depth (+ depth 1)) (setq pos2 (+ pos2 2)))
                               ((= nc 93)
                                (setq depth (- depth 1))
                                (when (= depth 0)
                                  (setq sections (cons (%substring control start pos2) sections))
                                  (setq i (+ pos2 2))
                                  (return nil))
                                (setq pos2 (+ pos2 2)))
                               ((and (= nc 59) (= depth 1))  ; ~;
                                (setq sections (cons (%substring control start pos2) sections))
                                (setq pos2 (+ pos2 2))
                                (setq start pos2))
                               (t (setq pos2 (+ pos2 1)))))
                           (return nil))  ; just in case
                         (setq pos2 (+ pos2 1)))
                       (setq sections (nreverse sections))
                       (cond
                         ;; ~@[: boolean conditional
                         (atp
                          (let ((val (car arg-list)))
                            (if val
                                (progn
                                  ;; Don't consume arg — process section with arg still there
                                  (setq arg-list (%format-impl stream (car sections) arg-list)))
                                (setq arg-list (cdr arg-list)))))
                         ;; ~:[: boolean second-arg style
                         (colonp
                          (let ((val (car arg-list)))
                            (setq arg-list (cdr arg-list))
                            (if (not val)
                                (when sections (%format-impl stream (car sections) arg-list))
                                (when (cdr sections) (%format-impl stream (cadr sections) arg-list)))))
                         ;; ~[: numeric selection
                         (t
                          (let ((idx (car arg-list)))
                            (setq arg-list (cdr arg-list))
                            (if (integerp idx)
                                (let ((selected (nth idx sections)))
                                  (when selected
                                    (setq arg-list (%format-impl stream selected arg-list))))
                                ;; Check for default clause (~;)
                                (let ((default (car (last sections))))
                                  (when default
                                    (setq arg-list (%format-impl stream default arg-list)))))))))))
                  ;; ~{ ~} — iteration
                  ((or (= dir 123) (= dir 125))
                   (when (= dir 123)
                     ;; Find matching ~}
                     (let ((end-pos i) (depth 1))
                       (loop
                         (when (>= end-pos len) (return nil))
                         (when (= (aref control end-pos) 126)
                           (let ((nc (if (< (+ end-pos 1) len) (aref control (+ end-pos 1)) 0)))
                             (cond
                               ((= nc 123) (setq depth (+ depth 1)) (setq end-pos (+ end-pos 2)))
                               ((= nc 125)
                                (setq depth (- depth 1))
                                (when (= depth 0)
                                  (let ((body (%substring control i end-pos))
                                        (max-iter (if param1 param1 -1)))
                                    (setq i (+ end-pos 2))
                                    (if atp
                                        ;; ~@{: use remaining args as list
                                        (let ((count 0))
                                          (loop
                                            (when (or (null arg-list)
                                                      (and (>= max-iter 0) (>= count max-iter)))
                                              (return nil))
                                            (let ((new-args (%format-impl stream body arg-list)))
                                              (when (eq new-args arg-list) (return nil))  ; no progress
                                              (setq arg-list new-args))
                                            (setq count (+ count 1))))
                                        ;; ~{: use next arg as list
                                        (let ((lst (car arg-list))
                                              (count 0))
                                          (setq arg-list (cdr arg-list))
                                          (loop
                                            (when (or (null lst)
                                                      (and (>= max-iter 0) (>= count max-iter)))
                                              (return nil))
                                            (let ((new-lst (%format-impl stream body lst)))
                                              (when (eq new-lst lst) (return nil))
                                              (setq lst new-lst))
                                            (setq count (+ count 1))))))
                                  (return nil)))
                               (t (setq end-pos (+ end-pos 1)))))
                           (return nil))
                         (setq end-pos (+ end-pos 1)))))
                  ;; ~^ — escape upward
                  ((= dir 94)
                   (when (null arg-list) (return arg-list)))
                  ;; ~_ — conditional newline (pprint, ignore)
                  ((= dir 95) nil)
                  ;; ~I — indent (pprint, ignore)
                  ((= dir 73) (setq arg-list (cdr arg-list)))
                  ;; ~/ — call function
                  ((= dir 47)
                   ;; Find end of function name (next /)
                   (let ((fn-start i) (fn-end i))
                     (loop
                       (when (>= fn-end len) (return nil))
                       (when (= (aref control fn-end) 47) (return nil))
                       (setq fn-end (+ fn-end 1)))
                     (setq i (+ fn-end 1))
                     ;; Skip arg
                     (setq arg-list (cdr arg-list))))
                  ;; Unknown directive
                  (t
                   (%print-char 126 stream)
                   (%print-char dir stream))))))))
    arg-list))))

;;; format: the main user-facing function
(defun format (stream control &rest args)
  "Format output. STREAM: nil=return string, t=*standard-output*.
   Returns nil for stream output, string for nil stream."
  (if (null stream)
      ;; Return string
      (let ((s (make-string-output-stream)))
        (%format-impl s control args)
        (get-output-stream-string s))
      ;; Output to stream
      (let ((s (if (eq stream t) (%resolve-output-stream nil) (%resolve-output-stream stream))))
        (%format-impl s control args)
        nil)))

;;; formatter macro: returns a function that takes (stream &rest args)
;;; and applies the format control string
;;; In MVM, defmacro not available, so formatter is a function
;;; that returns a closure at runtime
(defun formatter (control)
  "Return a function (stream &rest args) that formats using CONTROL."
  (lambda (stream &rest args)
    (let ((remaining (%format-impl (%resolve-output-stream stream) control args)))
      remaining)))

(defun terpri (&rest stream-arg)
  (let ((s (%resolve-output-stream (if stream-arg (car stream-arg) nil))))
    (if (and (streamp s) (not (= (%stream-type s) 8)))
        (%write-char-to-stream 10 s)
        (write-char-serial 10)))
  nil)

(defun fresh-line (&rest stream-arg)
  "Write newline only if not at beginning of line. Returns nil if at BOL, non-nil otherwise."
  (let ((s (%resolve-output-stream (if stream-arg (car stream-arg) nil))))
    (if (and (streamp s) (not (= (%stream-type s) 8)))
        (if (%stream-at-bol-p s)
            nil
            (progn (%write-char-to-stream 10 s) t))
        ;; Serial output: always write newline (no column tracking)
        (progn (write-char-serial 10) t))))

(defun %stream-at-bol-p (s)
  "Check if stream is at beginning of line (last char was newline or nothing written)."
  (if (streamp s)
      (let ((ty (%stream-type s)))
        (cond
          ((= ty 2) ;; string-output: check char-list
           (let ((chars (car (%stream-data s))))
             (if (null chars)
                 t  ;; nothing written = at BOL
                 (= (car chars) 10))))  ;; last char was newline
          ((= ty 4) ;; two-way: check output side
           (%stream-at-bol-p (cdr (%stream-data s))))
          ((= ty 3) ;; echo: check output side
           (%stream-at-bol-p (cdr (%stream-data s))))
          ((= ty 5) ;; broadcast: check first stream
           (if (%stream-data s)
               (%stream-at-bol-p (car (%stream-data s)))
               t))
          ((= ty 8) nil) ;; serial: assume not at BOL
          (t nil)))
      nil))

(defun write-string (str &rest args)
  (let ((s (%resolve-output-stream (if args (car args) nil))))
    (if (and (streamp s) (not (= (%stream-type s) 8)))
        (let ((len (length str)) (i 0))
          (loop
            (when (>= i len) (return nil))
            (%write-char-to-stream (aref str i) s)
            (setq i (+ i 1))))
        (write-string-serial str)))
  str)

(defun write-line (str &rest args)
  (let ((s (%resolve-output-stream (if args (car args) nil))))
    (if (and (streamp s) (not (= (%stream-type s) 8)))
        (progn
          (let ((len (length str)) (i 0))
            (loop
              (when (>= i len) (return nil))
              (%write-char-to-stream (aref str i) s)
              (setq i (+ i 1))))
          (%write-char-to-stream 10 s))
        (progn
          (write-string-serial str)
          (write-char-serial 10))))
  str)

(defun %ensure-char-code (x)
  "If x is a character, return char-code. Otherwise return x unchanged.
   Avoids compiler bug with inline characterp + char-code."
  (if (fixnump x) x (char-code x)))

(defun write-char (ch &rest stream-arg)
  "Write character CH to stream. Stream designator: nil=*standard-output*, t=*terminal-io*."
  (let ((saved-ch ch))
    (let ((code (%ensure-char-code saved-ch)))
      (let ((s (if stream-arg
                   (%resolve-output-stream (car stream-arg))
                   (%resolve-output-stream nil))))
        (%write-char-to-stream code s)))
    saved-ch))

(defun finish-output (&rest args) nil)
(defun force-output (&rest args) nil)
(defun clear-output (&rest args) nil)
(defun clear-input (&rest args) nil)
(defun listen (&rest args)
  "Check if input is available on stream."
  (let ((s (%resolve-input-stream (if args (car args) nil))))
    (if (streamp s)
        (let ((ty (%stream-type s)))
          (cond
            ;; String-input: check if there's data or unread char
            ((= ty 1)
             (let ((data (%stream-data s)))
               (let ((str (car data))
                     (pos-cell (cdr data)))
                 (if (cdr pos-cell)
                     t  ;; unread char available
                     (if (< (car pos-cell) (length str)) t nil)))))
            ;; Two-way: check input side
            ((= ty 4) (listen (car (%stream-data s))))
            ;; Echo: check input side
            ((= ty 3) (listen (car (%stream-data s))))
            ;; File stream: check if buffer has data
            ((= ty 9)
             (let ((bpos (%fs-bpos s))
                   (blen (%fs-blen s)))
               (if (< bpos blen) t
                   ;; Would need a non-blocking read to check — return t if fd valid
                   (if (>= (%fs-fd s) 0) t nil))))
            (t nil)))
        nil)))
(defun %substring (str start end)
  "Extract a substring from STR between START and END, preserving string subtag."
  (let ((len (- end start))
        (result (%make-string-array (- end start))))
    (let ((i 0))
      (loop
        (when (= i len) (return result))
        (aset result i (aref str (+ start i)))
        (setq i (+ i 1))))))

(defun read-line (&rest args)
  "Read a line from a stream. Args: [stream [eof-error-p [eof-value [recursive-p]]]]"
  (let ((stream-arg (if args (car args) nil))
        (eof-error-p (if (cdr args) (cadr args) t))
        (eof-value (if (cddr args) (caddr args) nil)))
    (let ((s (%resolve-input-stream stream-arg)))
      ;; Use read-char to read characters until newline or EOF
      (let ((chars nil)
            (found-newline nil)
            (hit-eof nil))
        (loop
          (let ((ch (%read-char-from-stream s nil :eof-sentinel-7770002)))
            (cond
              ((eq ch :eof-sentinel-7770002)
               (setq hit-eof t)
               (return nil))
              ((char= ch #\Newline)
               (setq found-newline t)
               (return nil))
              (t (setq chars (cons (char-code ch) chars))))))
        (if (and hit-eof (null chars))
            ;; EOF with nothing read
            (if eof-error-p
                (values eof-value t)
                (values eof-value t))
            ;; Build string from collected chars
            (let ((result-chars (nreverse chars)))
              (let ((len (list-length result-chars)))
                (let ((str (%make-string-array len))
                      (i 0)
                      (cur result-chars))
                  (loop
                    (when (null cur) (return nil))
                    (aset str i (car cur))
                    (setq cur (cdr cur))
                    (setq i (+ i 1)))
                  (values str (if found-newline nil (if hit-eof t nil)))))))))))

;;; read-sequence: read N elements from stream into seq starting at start
(defun read-sequence (seq stream &rest args)
  "Read elements from STREAM into SEQ. Returns end position."
  (let ((start (if args (car args) 0))
        (end (if (cdr args) (cadr args) nil)))
    (let ((actual-end (if end end (length seq)))
          (i start))
      (loop
        (when (>= i actual-end) (return i))
        (if (stringp seq)
            (let ((ch (%read-char-from-stream
                       (%resolve-input-stream stream) nil :eof-sentinel-7770002)))
              (if (eq ch :eof-sentinel-7770002)
                  (return i)
                  (aset seq i (char-code ch))))
            (let ((b (if (streamp stream)
                         (%fs-read-byte stream nil :eof-sentinel-7770002)
                         :eof-sentinel-7770002)))
              (if (eq b :eof-sentinel-7770002)
                  (return i)
                  (aset seq i b))))
        (setq i (+ i 1))))))

;;; write-sequence: write elements from seq to stream
(defun write-sequence (seq stream &rest args)
  "Write elements from SEQ to STREAM."
  (let ((start (if args (car args) 0))
        (end (if (cdr args) (cadr args) nil)))
    (let ((actual-end (if end end (length seq)))
          (i start)
          (s (%resolve-output-stream stream)))
      (loop
        (when (>= i actual-end) (return seq))
        (if (stringp seq)
            (%write-char-to-stream (aref seq i) s)
            (if (streamp s)
                (%fs-write-byte (aref seq i) s)
                (write-char-serial (aref seq i))))
        (setq i (+ i 1)))))
  seq)

(defun equalp-impl (a b)
  (if (eql a b) t
    (if (consp a)
      (if (consp b)
        (if (equalp-impl (car a) (car b))
          (equalp-impl (cdr a) (cdr b))
          nil)
        nil)
      (if (floatp-impl a)
        (if (floatp-impl b)
          (float-equal a b)
          nil)
        (if (stringp a)
          (if (stringp b) (string-equal a b) nil)
          nil)))))

