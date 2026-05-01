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
  ;; LENGTH (fp-aware) so fp-wrapped strings print only their active prefix.
  (let ((len (length str)) (i 0))
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

;;; Print radix prefix: #b, #o, #x, #Nr. Base 10 with *print-radix* is
;;; conventionally written with a TRAILING dot (not a leading prefix), so
;;; this function skips base 10 — caller is responsible for emitting the
;;; trailing dot in that case.
(defun %print-radix-prefix (base stream)
  (cond
    ((= base 2)  (%print-char 35 stream) (%print-char 98 stream))  ; #b
    ((= base 8)  (%print-char 35 stream) (%print-char 111 stream)) ; #o
    ((= base 10) nil)                                              ; handled by trailing dot
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

;;; Print a multi-dim array: emit "#NA" then a nested-list literal whose
;;; structure follows DIMS, drawing elements from FLAT-DATA in row-major order.
;;; Returns (values).  Forward-declared so %write-obj can recurse into it.
(defun %print-md-array (dims flat-data stream level escape)
  ;; Emit "#NA" prefix
  (%print-char 35 stream)  ; #
  (let ((rank 0) (d dims))
    (loop (when (null d) (return nil))
      (setq rank (+ rank 1))
      (setq d (cdr d)))
    (%print-decimal-to-stream rank stream))
  (%print-char 65 stream)  ; A
  ;; If no dims (0-dim), print the single element
  (cond
    ((null dims)
     (%write-obj (aref flat-data 0) stream
                 (if (null level) 1 (+ level 1)) escape))
    (t
     (%print-md-array-rec dims flat-data 0 stream level escape))))

;;; Print one slice of an N-D array.  Returns the next index into FLAT-DATA
;;; after consuming the slice.  DIMS is the remaining dimension list.
(defun %print-md-array-rec (dims flat-data start stream level escape)
  (cond
    ((null dims)
     ;; Leaf — print the single element at START, return START+1
     (%write-obj (aref flat-data start) stream
                 (if (null level) 1 (+ level 1)) escape)
     (+ start 1))
    (t
     (%print-char 40 stream)  ; (
     (let ((n (car dims))
           (rest-dims (cdr dims))
           (i 0)
           (cur start))
       (loop
         (when (= i n) (return nil))
         (when (> i 0) (%print-char 32 stream))
         (setq cur (%print-md-array-rec rest-dims flat-data cur stream level escape))
         (setq i (+ i 1)))
       (%print-char 41 stream)  ; )
       cur))))

;;; Main printer: print OBJ to STREAM respecting all *print-* variables
;;; LEVEL: current nesting level (nil = not tracking)
;;; ESCAPE: current escape setting
(defun %write-obj (obj stream level escape)
  ;; These *print-* vars are declared special so the let below reads them
  ;; via dynamic (symbol-value) lookup rather than lexical/global. Tests
  ;; that do (let ((*print-base* 2)) (prin1 N)) expect the printer to see
  ;; the dynamic binding — without this declare it read the global.
  (declare (special *print-length* *print-level* *print-base* *print-radix*
                    *print-case* *print-escape* *print-readably*
                    *print-gensym* *print-array*))
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
      ;; NIL — honor *print-case*. NIL is a symbol whose name is "NIL";
      ;; under :downcase / :capitalize the printed form must follow.
      ;; (:capitalize on "NIL" → "Nil", which needs per-word handling.)
      ((null obj)
       (cond
         ((eq pcase :downcase)
          (%print-char 110 stream) (%print-char 105 stream) (%print-char 108 stream))
         ((eq pcase :capitalize)
          (%print-char 78 stream) (%print-char 105 stream) (%print-char 108 stream))
         (t
          (%print-char 78 stream) (%print-char 73 stream) (%print-char 76 stream))))
      ;; T
      ((eq obj t)
       (%print-char (if (eq pcase :downcase) 116 84) stream))
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
       (%print-integer-in-base obj pbase stream)
       ;; Base 10 with *print-radix* uses TRAILING dot, not a prefix.
       (when (and pradix (= pbase 10))
         (%print-char 46 stream)))
      ;; Float
      ((floatp-impl obj)
       ;; Use standard float printing
       (%print-float-to-stream obj stream escape))
      ;; Ratio
      ((ratiop obj)
       (%print-integer-in-base (ratio-numerator obj) pbase stream)
       (%print-char 47 stream)  ; /
       (%print-integer-in-base (ratio-denominator obj) pbase stream))
      ;; String  (also matches fp-wrapped strings — wrapper-aware stringp
      ;; reports T for them.  Use LENGTH (fill-pointer aware) instead of
      ;; ARRAY-LENGTH so the printed form respects the fp truncation.)
      ((stringp obj)
       (if escape
           (progn
             (%print-char 34 stream)  ; "
             (let ((len (length obj)) (i 0))
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
      ;; Adjustable wrapper: (cons 8765432 inner) — peel and recurse
      ((and (consp obj) (eql (car obj) 8765432) (consp (cdr obj)))
       (%write-obj (cdr obj) stream level escape))
      ;; Multi-dim array wrapper: (cons 9867654 (cons DIMS FLAT-ARR))
      ((and (consp obj) (eql (car obj) 9867654) (consp (cdr obj)))
       (cond
         ((not parray)
          (%print-char 35 stream)
          (%print-char 60 stream)
          (%print-string-raw "Array" stream)
          (%print-char 62 stream))
         (t
          (let ((dims (cadr obj)) (data (cddr obj)))
            (%print-md-array dims data stream level escape)))))
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

;;; CLHS-conformant integer formatter for ~D, ~B, ~O, ~X (and ~R with base).
;;; Params (per CLHS):
;;;   mincol   minimum column width (NIL → no padding)
;;;   padchar  fill character (default #\Space). Char OR fixnum (char-code).
;;;   commachar comma character (default #\,). Char OR fixnum.
;;;   commaint comma interval (default 3)
;;;   colonp   T = insert commachar every commaint digits
;;;   atp      T = always print sign (+ for non-negative)
;;; Non-integer falls back to ~A (princ) — no padding/commas.
(defun %fmt-integer (n base mincol padchar commachar commaint colonp atp stream)
  (cond
    ((not (integerp n))
     ;; ANSI: non-integer → print as ~A (princ), no padding
     (let ((*print-escape* nil))
       (declare (special *print-escape*))
       (%write-obj n stream nil nil)))
    (t
     (let ((s (make-string-output-stream)))
       ;; 1. Sign
       (cond
         ((< n 0) (%print-char 45 s) (setq n (- 0 n)))
         (atp     (%print-char 43 s)))
       ;; 2. Digits (no commas yet) — collect into a list to count length
       (let ((digits nil))
         (cond
           ((= n 0) (setq digits (cons 48 nil)))
           (t
            (let ((tmp n))
              (loop
                (when (= tmp 0) (return nil))
                (setq digits (cons (%digit-char-upper (mod tmp base)) digits))
                (setq tmp (truncate tmp base))))))
         ;; 3. With colonp, walk digit list emitting commachar at intervals
         (cond
           (colonp
            (let ((cc (%ensure-char-code (if commachar commachar 44)))
                  (ci (if commaint commaint 3))
                  (dl digits)
                  (total 0))
              ;; total = (length digits)
              (let ((lp digits))
                (loop
                  (when (null lp) (return nil))
                  (setq total (+ total 1))
                  (setq lp (cdr lp))))
              (let ((idx 0))
                (loop
                  (when (null dl) (return nil))
                  (when (and (> idx 0)
                             (= 0 (mod (- total idx) ci)))
                    (%print-char cc s))
                  (%print-char (car dl) s)
                  (setq dl (cdr dl))
                  (setq idx (+ idx 1))))))
           (t
            (let ((dl digits))
              (loop
                (when (null dl) (return nil))
                (%print-char (car dl) s)
                (setq dl (cdr dl)))))))
       ;; 4. Apply mincol padding (left-pad to right-align)
       (let ((str (get-output-stream-string s)))
         (cond
           ((and mincol (> mincol 0))
            (let ((slen (array-length str))
                  (pc (%ensure-char-code (if padchar padchar 32))))
              (let ((pad (- mincol slen)))
                (loop
                  (when (<= pad 0) (return nil))
                  (%print-char pc stream)
                  (setq pad (- pad 1))))
              (%print-string-raw str stream)))
           (t
            (%print-string-raw str stream))))))))

;;; Pad string to minimum column
(defun %fmt-pad-aligned (str mincol colinc minpad padchar stream right-align)
  "Write STR padded to MINCOL using PADCHAR, with MINPAD minimum padding.
   RIGHT-ALIGN: T puts padding first (~@A/~@S/~D-style). NIL puts string first."
  (let ((slen (if (stringp str) (array-length str) 0))
        (mc (if mincol mincol 0))
        (ci (if colinc colinc 1))
        (mp (if minpad minpad 0))
        (pc (if padchar padchar 32)))
    (let ((padding mp))
      (loop
        (when (>= (+ slen padding) mc) (return nil))
        (setq padding (+ padding ci)))
      (cond
        (right-align
         (let ((i 0))
           (loop
             (when (= i padding) (return nil))
             (%print-char pc stream)
             (setq i (+ i 1))))
         (when (stringp str) (%print-string-raw str stream)))
        (t
         (when (stringp str) (%print-string-raw str stream))
         (let ((i 0))
           (loop
             (when (= i padding) (return nil))
             (%print-char pc stream)
             (setq i (+ i 1)))))))))

;;; Compatibility wrapper preserving the old &rest signature so older callers
;;; that pass :right-align as a trailing keyword still work.
(defun %fmt-pad (str mincol colinc minpad padchar stream &rest opts)
  (%fmt-pad-aligned str mincol colinc minpad padchar stream
                    (and opts (eq (car opts) :right-align))))

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

;;; ~^ inside ~{ ~} sets *format-iter-escape* to t — the inner %format-impl
;;; returns immediately and the iteration helper checks the flag to break out
;;; of the iteration loop. CLHS 22.3.9.2: ~^ inside an iteration terminates
;;; that iteration's loop, not the entire format.
(defvar *format-iter-escape* nil)

;;; ~:^ checks the OUTER list (the list-of-sublists for ~:{ / ~:@{), not
;;; the current sublist that's being passed as args. Bound by the
;;; ~:{ / ~:@{ helpers to the remaining outer iterations after the
;;; current one. NIL when not inside ~:{ / ~:@{ — ~:^ then has nothing
;;; useful to check; CLHS says behaviour is unspecified there.
(defvar *format-outer-rest* nil)

;;; ~{...~} helpers. Factored out of %format-impl because inlining the
;;; matching-brace scan + the per-iteration recursive call with all its
;;; nested let/loop/cond state confused the MVM register allocator
;;; (the recursive %format-impl call received the outer's arg-list
;;; instead of the lst being passed).

(defun %format-find-close-brace (control start len)
  "Scan CONTROL from START for the matching ~} (respecting nested ~{~}).
   Skips parameters (digits, commas, V, #, '<char>) and modifiers (:, @)
   between ~ and the directive char so e.g. ~1{ and ~v,3:@{ are recognized.
   Returns the position of ~ in the ~} pair, or NIL if not found.
   The caller can detect a colon-modified close (~:}) by scanning forward
   from result+1 looking for : before the } character."
  (let ((pos start) (depth 1) (result nil))
    (loop
      (when (or result (>= pos len)) (return result))
      (if (/= (aref control pos) 126)
          (setq pos (+ pos 1))
          ;; Found ~ — scan past parameters and modifiers to the directive char.
          (let ((p (+ pos 1)))
            (loop
              (when (>= p len) (return nil))
              (let ((c (aref control p)))
                (cond
                  ;; ' (apostrophe) consumes the next char as a literal param
                  ((= c 39)
                   (setq p (+ p 1))
                   (when (< p len) (setq p (+ p 1))))
                  ;; digits, minus, comma, v/V, #, :, @ — keep scanning
                  ((or (and (>= c 48) (<= c 57))
                       (= c 45) (= c 44)
                       (= c 118) (= c 86)
                       (= c 35) (= c 58) (= c 64))
                   (setq p (+ p 1)))
                  (t (return nil)))))
            (when (>= p len) (return result))
            (let ((dch (aref control p)))
              (cond
                ((= dch 123) (setq depth (+ depth 1)) (setq pos (+ p 1)))
                ((= dch 125)
                 (setq depth (- depth 1))
                 (if (= depth 0)
                     (setq result pos)
                     (setq pos (+ p 1))))
                (t (setq pos (+ p 1))))))))))

(defun %format-close-brace-colon-p (control close-pos len)
  "Return T if the ~} close at CLOSE-POS (the ~ position) had a colon
   modifier — i.e. it was actually ~:}. CLHS 22.3.7.4: ~:} forces at
   least one iteration even when the argument list is empty."
  (let ((p (+ close-pos 1)) (saw-colon nil))
    (loop
      (when (>= p len) (return saw-colon))
      (let ((c (aref control p)))
        (cond
          ((= c 58) (setq saw-colon t) (setq p (+ p 1)))   ; :
          ((= c 125) (return saw-colon))                    ; }
          ;; Skip params/at: digits, comma, V, #, ', @
          ((or (and (>= c 48) (<= c 57))
               (= c 44) (= c 118) (= c 86)
               (= c 35) (= c 64)
               (= c 39))
           (setq p (+ p 1)))
          (t (return saw-colon)))))))

(defun %format-close-brace-end (control close-pos len)
  "Return the position immediately after the closing }. CLOSE-POS points
   to the ~ in ~}. Scans forward through any modifiers (:, @) and params
   to land just past the }."
  (let ((p (+ close-pos 1)))
    (loop
      (when (>= p len) (return len))
      (let ((c (aref control p)))
        (cond
          ((= c 125) (return (+ p 1)))                       ; }
          ((or (= c 58) (= c 64)                             ; : @
               (and (>= c 48) (<= c 57))
               (= c 44) (= c 118) (= c 86)
               (= c 35) (= c 39))
           (setq p (+ p 1)))
          (t (return (+ p 1))))))))

(defun %format-iter-inside (stream body lst max-iter &optional force-once)
  "Iterate BODY over LST (the ~{...~} case). BODY is the template, LST
   is the list to feed as successive args. Stops when LST exhausted, or
   MAX-ITER reached, ~^ fires, or a pass makes no progress.
   FORCE-ONCE (set by ~:}) runs the body at least once even if LST is
   empty, unless MAX-ITER is 0."
  (let ((count 0))
    (declare (special *format-iter-escape*))
    (when (and force-once (null lst) (or (< max-iter 0) (> max-iter 0)))
      (%format-impl stream body nil))
    (loop
      (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
      (when (null lst) (return nil))
      (when (and (>= max-iter 0) (>= count max-iter)) (return nil))
      (let ((new-lst (%format-impl stream body lst)))
        (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
        (when (eq new-lst lst) (return nil))
        (setq lst new-lst))
      (setq count (+ count 1)))))

(defun %format-iter-remaining (stream body arg-list max-iter &optional force-once)
  "Iterate BODY consuming elements from ARG-LIST (the ~@{...~} case).
   Returns the remaining (unconsumed) arg-list. FORCE-ONCE for ~:@}."
  (let ((count 0))
    (declare (special *format-iter-escape*))
    (when (and force-once (null arg-list) (or (< max-iter 0) (> max-iter 0)))
      (%format-impl stream body nil))
    (loop
      (when *format-iter-escape* (setq *format-iter-escape* nil) (return arg-list))
      (when (null arg-list) (return arg-list))
      (when (and (>= max-iter 0) (>= count max-iter)) (return arg-list))
      (let ((new-args (%format-impl stream body arg-list)))
        (when *format-iter-escape* (setq *format-iter-escape* nil) (return new-args))
        (when (eq new-args arg-list) (return arg-list))
        (setq arg-list new-args))
      (setq count (+ count 1)))))

(defun %format-iter-of-lists (stream body lst max-iter &optional force-once)
  "Iterate BODY over LST where each element of LST is itself a list of args
   passed to BODY. The ~:{...~} case. Stops when LST exhausted, MAX-ITER
   reached, or ~^ fires. Binds *format-outer-rest* so ~:^ inside the body
   can check the outer iteration state (CLHS 22.3.9.2)."
  (let ((count 0))
    (declare (special *format-iter-escape* *format-outer-rest*))
    (when (and force-once (null lst) (or (< max-iter 0) (> max-iter 0)))
      (let ((*format-outer-rest* nil))
        (declare (special *format-outer-rest*))
        (%format-impl stream body nil)))
    (loop
      (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
      (when (null lst) (return nil))
      (when (and (>= max-iter 0) (>= count max-iter)) (return nil))
      (let ((*format-outer-rest* (cdr lst)))
        (declare (special *format-outer-rest*))
        (%format-impl stream body (car lst)))
      (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
      (setq lst (cdr lst))
      (setq count (+ count 1)))))

(defun %format-iter-of-lists-rest (stream body arg-list max-iter &optional force-once)
  "Iterate BODY consuming successive args from ARG-LIST, each treated as a
   list passed to BODY as its args. The ~:@{...~} case. Returns the
   remaining (unconsumed) arg-list. Binds *format-outer-rest* for ~:^."
  (let ((count 0))
    (declare (special *format-iter-escape* *format-outer-rest*))
    (when (and force-once (null arg-list) (or (< max-iter 0) (> max-iter 0)))
      (let ((*format-outer-rest* nil))
        (declare (special *format-outer-rest*))
        (%format-impl stream body nil)))
    (loop
      (when *format-iter-escape* (setq *format-iter-escape* nil) (return arg-list))
      (when (null arg-list) (return arg-list))
      (when (and (>= max-iter 0) (>= count max-iter)) (return arg-list))
      (let ((*format-outer-rest* (cdr arg-list)))
        (declare (special *format-outer-rest*))
        (%format-impl stream body (car arg-list)))
      (when *format-iter-escape* (setq *format-iter-escape* nil)
            (return (cdr arg-list)))
      (setq arg-list (cdr arg-list))
      (setq count (+ count 1)))))

(defun %format-dispatch-brace (stream control i len arg-list colonp atp param1)
  "Handle a ~{...~} directive at position i of CONTROL.
   Finds the matching ~}, extracts the body substring, runs the iteration
   (one of ~{, ~@{, ~:{, ~:@{), and returns (cons NEW-I NEW-ARG-LIST).
   If no matching ~} is found, returns (cons i arg-list) unchanged.
   Per CLHS 22.3.7.4:
     ~{...~}    : (car arg-list) is the list, body iterates over it as args
     ~@{...~}   : rest of arg-list is consumed as iteration args
     ~:{...~}   : (car arg-list) is list of sublists; body sees each sublist as args
     ~:@{...~}  : rest of arg-list, each one a sublist; body sees its elements as args"
  (let ((end-pos (%format-find-close-brace control i len)))
    (if (null end-pos)
        (cons i arg-list)
        (let* ((raw-body (%substring control i end-pos))
               (body-empty (= (length raw-body) 0))
               (body raw-body)
               (max-iter (if param1 param1 -1))
               (force-once (%format-close-brace-colon-p control end-pos len))
               (new-i (%format-close-brace-end control end-pos len))
               (use-fn nil))
          ;; CLHS 22.3.7.4: empty ~{~} body consumes the next argument and
          ;; uses it as the body. String → reuse as control. Function (e.g.
          ;; FORMATTER closure) → iterate via %format-iter-via-fn.
          (when body-empty
            (let ((next-body (car arg-list)))
              (setq arg-list (cdr arg-list))
              (cond
                ((stringp next-body) (setq body next-body))
                ((functionp next-body) (setq use-fn next-body)))))
          (cond
            (use-fn
             (%format-iter-via-fn stream use-fn arg-list colonp atp max-iter new-i))
            ((and colonp atp)
             (cons new-i (%format-iter-of-lists-rest stream body arg-list max-iter force-once)))
            (colonp
             (let ((lst (car arg-list))
                   (rest-args (cdr arg-list)))
               (%format-iter-of-lists stream body lst max-iter force-once)
               (cons new-i rest-args)))
            (atp
             (cons new-i (%format-iter-remaining stream body arg-list max-iter force-once)))
            (t
             (let ((lst (car arg-list))
                   (rest-args (cdr arg-list)))
               (%format-iter-inside stream body lst max-iter force-once)
               (cons new-i rest-args))))))))

(defun %format-iter-via-fn (stream fn arg-list colonp atp max-iter new-i)
  "Iterate FN (a formatter-style closure) over args, the empty-body case of
   ~{~} where the body argument was a function. FN should be called as
   (funcall FN stream &rest args) per CLHS 22.3.10.2 — it returns the
   remaining args. Returns (cons NEW-I REMAINING-ARG-LIST)."
  (let ((count 0))
    (declare (special *format-iter-escape* *format-outer-rest*))
    (cond
      ((and colonp atp)
       (loop
         (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
         (when (null arg-list) (return nil))
         (when (and (>= max-iter 0) (>= count max-iter)) (return nil))
         (let ((*format-outer-rest* (cdr arg-list)))
           (declare (special *format-outer-rest*))
           (funcall fn stream (car arg-list)))
         (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
         (setq arg-list (cdr arg-list))
         (setq count (+ count 1)))
       (cons new-i arg-list))
      (colonp
       (let ((lst (car arg-list))
             (rest-args (cdr arg-list)))
         (loop
           (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
           (when (null lst) (return nil))
           (when (and (>= max-iter 0) (>= count max-iter)) (return nil))
           (let ((*format-outer-rest* (cdr lst)))
             (declare (special *format-outer-rest*))
             (funcall fn stream (car lst)))
           (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
           (setq lst (cdr lst))
           (setq count (+ count 1)))
         (cons new-i rest-args)))
      (atp
       (loop
         (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
         (when (null arg-list) (return nil))
         (when (and (>= max-iter 0) (>= count max-iter)) (return nil))
         (let ((rem (apply fn stream arg-list)))
           (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
           (if (eq rem arg-list) (return nil) (setq arg-list rem)))
         (setq count (+ count 1)))
       (cons new-i arg-list))
      (t
       (let ((lst (car arg-list))
             (rest-args (cdr arg-list)))
         (loop
           (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
           (when (null lst) (return nil))
           (when (and (>= max-iter 0) (>= count max-iter)) (return nil))
           (let ((rem (apply fn stream lst)))
             (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
             (if (eq rem lst) (return nil) (setq lst rem)))
           (setq count (+ count 1)))
         (cons new-i rest-args))))))

;;; Main format implementation
;;; Returns remaining args (for use by formatter)
(defun %format-impl (stream control args)
  "Core format. Returns remaining unused args."
  (let ((len (array-length control))
        (i 0)
        (arg-list args)
        ;; Tracks the last arg consumed by the most recent value-printing
        ;; directive — used by ~:P / ~:@P which look BACKWARDS at the
        ;; previous arg (CLHS 22.3.3.4) without consuming a new one.
        (prev-arg nil))
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
                  (param5 nil)
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
                      ;; Integer parameter (supports leading + or -)
                      ((or (= c 43) (= c 45) (and (>= c 48) (<= c 57)))
                       (when (= c 43) (setq pos (+ pos 1)))
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
                (setq param4 (if (>= pcount 4) (nth 3 params) nil))
                (setq param5 (if (>= pcount 5) (nth 4 params) nil)))
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
              (let ((dir (aref control pos))
                    (before-arg-list arg-list))
                (setq i (+ pos 1))
                (cond
                  ;; ~A — aesthetic. The `:` modifier (~:A) prints NIL
                  ;; as "()" instead of "nil"/"NIL"/"Nil".
                  ((or (= dir 65) (= dir 97))
                   (let ((obj (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (let ((s (make-string-output-stream))
                           (*print-escape* nil))
                       (declare (special *print-escape*))
                       (if (and colonp (null obj))
                           (progn (%print-char 40 s) (%print-char 41 s))
                           (%write-obj obj s nil nil))
                       (let ((str (get-output-stream-string s)))
                         (if (or param1 param2 param3 param4)
                             (if atp
                                 (%fmt-pad-aligned str param1 param2 param3 (if param4 param4 32) stream t)
                                 (%fmt-pad-aligned str param1 param2 param3 (if param4 param4 32) stream nil))
                             (%print-string-raw str stream))))))
                  ;; ~S — standard. ~:S also prints NIL as "()".
                  ((or (= dir 83) (= dir 115))
                   (let ((obj (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (let ((s (make-string-output-stream))
                           (*print-escape* t))
                       (declare (special *print-escape*))
                       (if (and colonp (null obj))
                           (progn (%print-char 40 s) (%print-char 41 s))
                           (%write-obj obj s nil t))
                       (let ((str (get-output-stream-string s)))
                         (if (or param1 param2 param3 param4)
                             (if atp
                                 (%fmt-pad-aligned str param1 param2 param3 (if param4 param4 32) stream t)
                                 (%fmt-pad-aligned str param1 param2 param3 (if param4 param4 32) stream nil))
                             (%print-string-raw str stream))))))
                  ;; ~W — write (like ~S but respects all print vars)
                  ((or (= dir 87) (= dir 119))
                   (let ((obj (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (%write-obj obj stream nil *print-escape*)))
                  ;; ~D — decimal. CLHS params: mincol,padchar,commachar,commaint
                  ;; Numeric directives right-align (left-pad). Non-integer falls
                  ;; back to ~A (princ) per ANSI. ":" inserts commas, "@" forces sign.
                  ((or (= dir 68) (= dir 100))
                   (let ((n (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (%fmt-integer n 10 param1 param2 param3 param4
                                   colonp atp stream)))
                  ;; ~B — binary
                  ((or (= dir 66) (= dir 98))
                   (let ((n (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (%fmt-integer n 2 param1 param2 param3 param4
                                   colonp atp stream)))
                  ;; ~O — octal
                  ((or (= dir 79) (= dir 111))
                   (let ((n (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (%fmt-integer n 8 param1 param2 param3 param4
                                   colonp atp stream)))
                  ;; ~X — hexadecimal
                  ((or (= dir 88) (= dir 120))
                   (let ((n (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (%fmt-integer n 16 param1 param2 param3 param4
                                   colonp atp stream)))
                  ;; ~R — radix. ~radix,mincol,padchar,commachar,commaintR
                  ;; mirrors ~D except RADIX is the FIRST parameter (so ~D's
                  ;; param1=mincol shifts to param2 here).
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
                       ;; ~NR / ~N,M,'cR: base N w/ mincol,padchar,commachar,commaint
                       ;; Note: for ~R, params shift left by one (radix is param1).
                       (param1
                        (%fmt-integer n param1 param2 param3 param4 param5
                                      colonp atp stream))
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
                   (cond
                     ;; ~@N* / ~@*: absolute goto — set arg-list to (nthcdr N args)
                     (atp
                      (let ((n (if param1 param1 0)))
                        (let ((cur args) (j 0))
                          (loop
                            (when (or (null cur) (= j n)) (return nil))
                            (setq cur (cdr cur))
                            (setq j (+ j 1)))
                          (setq arg-list cur))))
                     ;; ~:N* / ~:*: go back N args (default 1)
                     (colonp
                      (let ((n (if param1 param1 1)))
                        ;; consumed = (length args) - (length arg-list)
                        ;; new pos = consumed - n  (clamped to 0)
                        (let ((consumed 0) (al args))
                          (loop (when (eq al arg-list) (return nil))
                                (when (null al) (return nil))
                                (setq consumed (+ consumed 1))
                                (setq al (cdr al)))
                          (let ((new-pos (- consumed n)))
                            (when (< new-pos 0) (setq new-pos 0))
                            (let ((cur args) (j 0))
                              (loop
                                (when (or (null cur) (= j new-pos)) (return nil))
                                (setq cur (cdr cur))
                                (setq j (+ j 1)))
                              (setq arg-list cur))))))
                     ;; ~N*: skip N args (default 1)
                     (t
                      (let ((n (if param1 param1 1)) (j 0))
                        (loop
                          (when (or (null arg-list) (= j n)) (return nil))
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
                  ;; ~P — plural. ~:P / ~:@P use the previously printed
                  ;; arg without consuming a new one (CLHS 22.3.3.4).
                  ((or (= dir 80) (= dir 112))
                   (let ((n (if colonp prev-arg (car arg-list))))
                     (unless colonp (setq arg-list (cdr arg-list)))
                     (let ((val (if (integerp n) n 2)))
                       (if atp
                           (if (= val 1) (%print-char 121 stream)  ; y
                               (%print-string-raw "ies" stream))
                           (if (/= val 1) (%print-char 115 stream))))))  ; s
                  ;; ~newline — discard literal newline and following whitespace.
                  ;;   ~newline    : discard the newline AND following whitespace (default)
                  ;;   ~@newline   : KEEP the newline, discard following whitespace
                  ;;   ~:newline   : discard the newline only, KEEP the whitespace
                  ((= dir 10)
                   (when atp
                     (%print-char 10 stream))
                   (unless colonp
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
                                                ;; ~@(...~): uppercase the FIRST alpha
                                                ;; character, lowercase everything else.
                                                (if (> (array-length result) 0)
                                                    (let ((r (%make-string-array (array-length result))))
                                                      (let ((first-done nil) (k 0))
                                                        (loop
                                                          (when (= k (array-length result)) (return nil))
                                                          (let ((c (aref result k)))
                                                            (let ((upper (and (>= c 65) (<= c 90)))
                                                                  (lower (and (>= c 97) (<= c 122))))
                                                              (cond
                                                                ((and (not first-done) (or upper lower))
                                                                 (aset r k (if lower (- c 32) c))
                                                                 (setq first-done t))
                                                                (upper
                                                                 (aset r k (+ c 32)))
                                                                (t
                                                                 (aset r k c)))))
                                                          (setq k (+ k 1))))
                                                      r)
                                                    result))
                                               (t
                                                (string-downcase result)))))
                                        (%print-string-raw converted stream))))
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
                     ;; The default-section marker is ~:; (a ~; with a colon
                     ;; modifier). We treat that specially below.
                     (let ((sections (list)) (default-idx nil) (start i) (depth 1) (pos2 i))
                       (loop
                         (when (>= pos2 len)
                           (setq sections (cons (%substring control start pos2) sections))
                           (return nil))
                         (if (/= (aref control pos2) 126)
                             (setq pos2 (+ pos2 1))
                             ;; At ~: scan past parameters/modifiers to directive char.
                             (let ((p (+ pos2 1)) (saw-colon nil))
                               (loop
                                 (when (>= p len) (return nil))
                                 (let ((c (aref control p)))
                                   (cond
                                     ((= c 39)  ; ' literal-char param
                                      (setq p (+ p 1))
                                      (when (< p len) (setq p (+ p 1))))
                                     ((or (and (>= c 48) (<= c 57))
                                          (= c 45) (= c 44)
                                          (= c 118) (= c 86)
                                          (= c 35) (= c 64))
                                      (setq p (+ p 1)))
                                     ((= c 58)
                                      (setq saw-colon t)
                                      (setq p (+ p 1)))
                                     (t (return nil)))))
                               (if (>= p len)
                                   (setq pos2 p)
                                   (let ((nc (aref control p)))
                                     (cond
                                       ((= nc 91) (setq depth (+ depth 1)) (setq pos2 (+ p 1)))
                                       ((= nc 93)
                                        (setq depth (- depth 1))
                                        (cond
                                          ((= depth 0)
                                           (setq sections (cons (%substring control start pos2) sections))
                                           (setq i (+ p 1))
                                           (return nil))
                                          (t (setq pos2 (+ p 1)))))
                                       ((and (= nc 59) (= depth 1))  ; ~; or ~:;
                                        (when saw-colon
                                          (setq default-idx (length sections)))
                                        (setq sections (cons (%substring control start pos2) sections))
                                        (setq pos2 (+ p 1))
                                        (setq start pos2))
                                       (t (setq pos2 (+ p 1)))))))))
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
                         ;; ~[: numeric selection. If idx is out of range
                         ;; AND the format had a ~:; default-section marker,
                         ;; use that section. Otherwise emit nothing.
                         (t
                          (let ((idx (car arg-list)))
                            (setq arg-list (cdr arg-list))
                            (cond
                              ((and (integerp idx) (>= idx 0) (< idx (length sections)))
                               (let ((selected (nth idx sections)))
                                 (when selected
                                   (setq arg-list (%format-impl stream selected arg-list)))))
                              ;; Out of range with default section
                              (default-idx
                               (let ((selected (nth default-idx sections)))
                                 (when selected
                                   (setq arg-list (%format-impl stream selected arg-list))))))))))))
                  ;; ~{ ~} — iteration (with optional :, @, or :@ flags)
                  ((= dir 123)
                   (let ((new-i-and-args
                          (%format-dispatch-brace stream control i len
                                                  arg-list colonp atp param1)))
                     (setq i (car new-i-and-args))
                     (setq arg-list (cdr new-i-and-args))))
                  ((= dir 125) nil)
                  ;; ~^ — escape upward (CLHS 22.3.9.2)
                  ;; ~^        : exit if no remaining args
                  ;; ~N^       : exit if N is zero
                  ;; ~N,M^     : exit if N = M
                  ;; ~N,M,K^   : exit if N <= M <= K
                  ;; Sets *format-iter-escape* so the surrounding ~{ ~}
                  ;; iteration loop can terminate. If we're not inside an
                  ;; iteration the flag still gets cleared next time.
                  ((= dir 94)
                   (declare (special *format-iter-escape* *format-outer-rest*))
                   (let ((should-escape
                          (cond
                            (param3
                             (and (integerp param1) (integerp param2) (integerp param3)
                                  (<= param1 param2) (<= param2 param3)))
                            (param2
                             (and (integerp param1) (integerp param2)
                                  (= param1 param2)))
                            (param1
                             (and (integerp param1) (= param1 0)))
                            ;; ~:^ — escape if outer iteration list exhausted.
                            ;; CLHS 22.3.9.2: ~:^ checks the list passed to ~:{,
                            ;; not the inner sublist passed to the body.
                            (colonp (null *format-outer-rest*))
                            (t (null arg-list)))))
                     (when should-escape
                       (setq *format-iter-escape* t)
                       (return arg-list))))
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
                   (%print-char dir stream)))
                ;; Update prev-arg if this directive consumed any arg.
                (when (and (consp before-arg-list)
                           (not (eq before-arg-list arg-list)))
                  (setq prev-arg (car before-arg-list))))))))
    arg-list))

;;; format: the main user-facing function
(defun format (stream control &rest args)
  "Format output. STREAM: nil=return string, t=*standard-output*.
   Returns nil for stream output, string for nil stream."
  (declare (special *format-iter-escape*))
  (setq *format-iter-escape* nil)
  (if (null stream)
      ;; Return string
      (let ((s (make-string-output-stream)))
        (%format-impl s control args)
        (setq *format-iter-escape* nil)
        (get-output-stream-string s))
      ;; Output to stream
      (let ((s (if (eq stream t) (%resolve-output-stream nil) (%resolve-output-stream stream))))
        (%format-impl s control args)
        (setq *format-iter-escape* nil)
        nil)))

;;; formatter: returns a closure that takes (stream &rest args) and
;;; applies the format control string.
;;;
;;; The closure mirrors what (formatter STR) is supposed to do per ANSI
;;; CLHS — return a function consuming a stream + arguments and
;;; returning the unused tail.  Test-site calls look like
;;;   (let ((fn (formatter STR))) ... (apply fn stream args) ...)
;;;
;;; Bumped the apply dispatch (above; see prelude.lisp) from 4 args to
;;; 8 so formatter tests with several args succeed.
(defun formatter (control)
  "Return a function (stream &rest args) that formats using CONTROL."
  (declare (special *format-iter-escape*))
  (lambda (stream &rest args)
    (declare (special *format-iter-escape*))
    (setq *format-iter-escape* nil)
    (let ((remaining (%format-impl (%resolve-output-stream stream) control args)))
      (setq *format-iter-escape* nil)
      remaining)))

;;; Extended apply that handles up to 8 spread args (prelude APPLY tops
;;; out at 4, silently dropping trailing args for any formatter call
;;; with 4+ format arguments — which is most of the format-d tests).
(defun apply (fn &rest spread)
  "ANSI apply: build a single arg list and funcall (up to 8 args).

   Detects interp-closures (consp + car=%interp-closure) and routes
   them through %call-interp-closure — compiled funcall doesn't know
   how to call cons-tagged closures, but methods installed by runtime
   (eval `(defmethod ...))) live as interp-closures and need to be
   invoked via apply from %gf-dispatch."
  (let ((all-args
         (if (null spread)
             nil
             (if (null (cdr spread))
                 (car spread)
                 (let ((individual nil) (cur spread))
                   (loop
                     (when (null (cdr cur))
                       (return (append (nreverse individual) (car cur))))
                     (setq individual (cons (car cur) individual))
                     (setq cur (cdr cur))))))))
    ;; Interp-closure dispatch — fast path for runtime-eval'd lambdas.
    (when (and (consp fn) (eq (car fn) '%interp-closure))
      (return-from apply (%call-interp-closure fn all-args)))
    (cond
      ((null all-args)
       (funcall fn))
      ((null (cdr all-args))
       (funcall fn (car all-args)))
      ((null (cddr all-args))
       (funcall fn (car all-args) (cadr all-args)))
      ((null (cdddr all-args))
       (funcall fn (car all-args) (cadr all-args) (caddr all-args)))
      ((null (cddddr all-args))
       (funcall fn (car all-args) (cadr all-args) (caddr all-args) (cadddr all-args)))
      ((null (cdr (cddddr all-args)))
       (funcall fn (car all-args) (cadr all-args) (caddr all-args)
                (cadddr all-args) (car (cddddr all-args))))
      ((null (cddr (cddddr all-args)))
       (funcall fn (car all-args) (cadr all-args) (caddr all-args)
                (cadddr all-args) (car (cddddr all-args))
                (cadr (cddddr all-args))))
      ((null (cdddr (cddddr all-args)))
       (funcall fn (car all-args) (cadr all-args) (caddr all-args)
                (cadddr all-args) (car (cddddr all-args))
                (cadr (cddddr all-args)) (caddr (cddddr all-args))))
      (t
       (funcall fn (car all-args) (cadr all-args) (caddr all-args)
                (cadddr all-args) (car (cddddr all-args))
                (cadr (cddddr all-args)) (caddr (cddddr all-args))
                (cadddr (cddddr all-args)))))))

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

;; ANSI: (write-string str &optional stream &key start end)
;; Honor :start/:end for substring writes.
(defun write-string (str &rest args)
  (let ((stream-arg (if args (car args) nil))
        (start 0)
        (end nil))
    ;; Parse keyword args from args[1..] (post-stream).
    (let ((cur (if args (cdr args) nil)))
      (loop
        (when (null cur) (return nil))
        (let ((k (car cur)) (v (cadr cur)))
          (cond
            ((eq k :start) (setq start v))
            ((eq k :end)   (setq end v))))
        (setq cur (cddr cur))))
    (let* ((s (%resolve-output-stream stream-arg))
           (len (length str))
           (eff-end (if end end len)))
      (if (and (streamp s) (not (= (%stream-type s) 8)))
          (let ((i start))
            (loop
              (when (>= i eff-end) (return nil))
              (%write-char-to-stream (aref str i) s)
              (setq i (+ i 1))))
          ;; Serial fallback only handles whole-string writes; emulate.
          (let ((i start))
            (loop
              (when (>= i eff-end) (return nil))
              (write-char-serial (aref str i))
              (setq i (+ i 1)))))))
  str)

(defun write-line (str &rest args)
  (let ((stream-arg (if args (car args) nil))
        (start 0)
        (end nil))
    (let ((cur (if args (cdr args) nil)))
      (loop
        (when (null cur) (return nil))
        (let ((k (car cur)) (v (cadr cur)))
          (cond
            ((eq k :start) (setq start v))
            ((eq k :end)   (setq end v))))
        (setq cur (cddr cur))))
    (let* ((s (%resolve-output-stream stream-arg))
           (len (length str))
           (eff-end (if end end len)))
      (if (and (streamp s) (not (= (%stream-type s) 8)))
          (progn
            (let ((i start))
              (loop
                (when (>= i eff-end) (return nil))
                (%write-char-to-stream (aref str i) s)
                (setq i (+ i 1))))
            (%write-char-to-stream 10 s))
          (progn
            (let ((i start))
              (loop
                (when (>= i eff-end) (return nil))
                (write-char-serial (aref str i))
                (setq i (+ i 1))))
            (write-char-serial 10)))))
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

(defun %equalp-array-elt (seq i)
  "Read element i from seq (vector or string) — coerce string bytes to chars."
  (let ((v (aref seq i)))
    (if (stringp seq) (code-char v) v)))

(defun %equalp-array-array (a b)
  "Element-wise equalp over two non-cons sequences (vectors or strings).
   Uses array-length and aref/code-char so plain arrays, strings, and
   bit-vectors all compare correctly."
  (let ((la (array-length a))
        (lb (array-length b)))
    (if (= la lb)
        (let ((i 0) (ok t))
          (loop
            (when (or (not ok) (= i la)) (return ok))
            (unless (equalp-impl (%equalp-array-elt a i)
                                  (%equalp-array-elt b i))
              (setq ok nil))
            (setq i (+ i 1))))
        nil)))

(defun equalp-impl (a b)
  (if (eql a b) t
    (if (and (characterp a) (characterp b))
        (char-equal a b)
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
              ;; ANSI equalp: arrays of any type compare element-wise.
              ;; A string and a vector-of-chars compare equalp if their
              ;; chars match. Bit-vectors with bit-vectors etc.
              (if (and (or (stringp a) (arrayp a))
                       (or (stringp b) (arrayp b)))
                  (%equalp-array-array a b)
                  nil))))))

