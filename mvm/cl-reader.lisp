;;;; cl-reader.lisp — Reader, readtable, eval infrastructure
;;;; Part of the Modus CL runtime. Depends on cl-printer.lisp.

;;; ============================================================
;;; Common Lisp Reader (Layer 2)
;;; ============================================================

;;; --- Reader variables ---
;;; NOTE: defvar init-thunks are NOT run (init-all-globals is skipped).
;;; These are initialized explicitly by %init-reader.

(defvar *read-base* nil)
(defvar *read-suppress* nil)
(defvar *read-eval* nil)
(defvar *read-default-float-format* nil)

;; *features* — consulted by #+ / #- via %feature-present-p.  Initialised
;; in %init-reader (defvar init-thunks don't run; see CLAUDE.md).
(defvar *features* nil)

;; Shared-structure label tables for #N= / #N# (CLHS 2.4.8.{15,16}).
;; *sharp-labels* is an alist of (label . placeholder-cons) for each
;; #N= currently being read.  Top-level reader entries rebind it to nil
;; so labels do not leak across reads.  Inside a read, a forward #N#
;; (used before the labelled form completes — self-cycle case) returns
;; the placeholder cons, which is patched in-place by %sharp-label-fixup
;; once the labelled object is known.
(defvar *sharp-labels* nil)

;;; --- Print variables (stubs for with-standard-io-syntax) ---

(defvar *print-array* nil)
(defvar *print-base* nil)
(defvar *print-case* nil)
(defvar *print-circle* nil)
(defvar *print-escape* nil)
(defvar *print-gensym* nil)
(defvar *print-length* nil)
(defvar *print-level* nil)
(defvar *print-lines* nil)
(defvar *print-miser-width* nil)
(defvar *print-pprint-dispatch* nil)
(defvar *print-pretty* nil)
(defvar *print-radix* nil)
(defvar *print-readably* nil)
(defvar *print-right-margin* nil)

;;; --- Readtable data structure ---
;;; Readtable = (cons 7770003 data)
;;; data = array[5]:
;;;   [0] = case (:upcase, :downcase, :preserve, :invert)
;;;   [1] = macro-table: array[128] of (fn . non-terminating-p) or nil
;;;   [2] = dispatch-table: alist of (dispatch-char . sub-table)
;;;         sub-table = array[128] of fn-or-nil
;;;   [3] = syntax-table: array[128] of syntax-type
;;;         :constituent, :whitespace, :single-escape, :multiple-escape, nil(=constituent)
;;;   [4] = reserved

(defun %make-readtable ()
  "Create a new readtable with standard CL syntax."
  (let ((data (make-array 5))
        (macros (make-array 128))
        (syntax (make-array 128))
        (dispatch nil))
    ;; Default: all entries nil (constituent)
    ;; Set whitespace
    (aset syntax 32 :whitespace)   ; space
    (aset syntax 9 :whitespace)    ; tab
    (aset syntax 10 :whitespace)   ; newline
    (aset syntax 13 :whitespace)   ; return
    (aset syntax 12 :whitespace)   ; page (form feed)
    ;; Set escape characters
    (aset syntax 92 :single-escape)    ; backslash
    (aset syntax 124 :multiple-escape) ; |
    ;; Set macro characters (terminating)
    ;; We store (cons fn non-terminating-p)
    ;; fn is a symbol or function; we use keywords as placeholders
    (aset macros 40 (cons :lparen nil))     ; (
    (aset macros 41 (cons :rparen nil))     ; )
    (aset macros 39 (cons :quote nil))      ; '
    (aset macros 34 (cons :string nil))     ; "
    (aset macros 96 (cons :backquote nil))  ; `
    (aset macros 44 (cons :comma nil))      ; ,
    (aset macros 59 (cons :semicolon nil))  ; ;
    ;; # is non-terminating macro character
    (aset macros 35 (cons :sharpsign t))    ; #
    ;; Set up dispatch table for #
    (let ((sharp-table (make-array 128)))
      (setq dispatch (list (cons 35 sharp-table))))
    ;; Store in data array
    (aset data 0 :upcase)
    (aset data 1 macros)
    (aset data 2 dispatch)
    (aset data 3 syntax)
    (aset data 4 nil)
    (cons 7770003 data)))

(defun readtablep (obj)
  "True if OBJ is a readtable."
  (if (consp obj)
      (eql (car obj) 7770003)
      nil))

(defun %rt-data (rt) (cdr rt))
(defun readtable-case (rt) (aref (%rt-data rt) 0))
(defun %set-readtable-case (rt val)
  (aset (%rt-data rt) 0 val)
  val)
(defun %rt-macros (rt) (aref (%rt-data rt) 1))
(defun %rt-dispatch (rt) (aref (%rt-data rt) 2))
(defun %rt-syntax (rt) (aref (%rt-data rt) 3))

(defun %copy-array-128 (src dst)
  "Copy 128 entries from SRC array to DST array."
  (let ((i 0))
    (loop
      (when (>= i 128) (return nil))
      (aset dst i (aref src i))
      (setq i (+ i 1)))))

(defun %copy-macro-entry (entry)
  "Deep-copy a macro table entry."
  (if (consp entry) (cons (car entry) (cdr entry)) nil))

(defun %copy-macro-table (src dst)
  "Copy macro table entries. Uses shallow copy for entries (safe since we
   replace whole entries via set-macro-character, never mutate cons cells)."
  (%copy-array-128 src dst))

(defun %copy-dispatch-tables (from-rt)
  "Copy dispatch tables from FROM-RT, returning new dispatch alist."
  (let ((new-dispatch nil)
        (dl (%rt-dispatch from-rt)))
    (dolist (entry dl)
      (let ((ch (car entry))
            (src-sub (cdr entry))
            (dst-sub (make-array 128)))
        (%copy-array-128 src-sub dst-sub)
        (setq new-dispatch (cons (cons ch dst-sub) new-dispatch))))
    new-dispatch))

(defun copy-readtable (&rest args)
  "Copy a readtable. (copy-readtable) copies *readtable*.
   (copy-readtable nil) copies the standard readtable.
   (copy-readtable from to) copies from into to."
  (let ((from-rt (if args
                     (if (null (car args))
                         *standard-readtable*
                         (car args))
                     *readtable*))
        (to-rt (if (cdr args) (cadr args) nil)))
    (let ((result (if (and to-rt (readtablep to-rt)) to-rt (%make-readtable))))
      ;; Copy case
      (let ((from-data (%rt-data from-rt))
            (to-data (%rt-data result)))
        (aset to-data 0 (aref from-data 0))
        (%copy-macro-table (aref from-data 1) (aref to-data 1))
        (%copy-array-128 (aref from-data 3) (aref to-data 3))
        ;; Share dispatch tables (shallow) — avoids cons-in-loop issue
        (aset to-data 2 (aref from-data 2)))
      result)))

;;; Readtable globals (initialized by %init-reader)
(defvar *standard-readtable* nil)
(defvar *readtable* nil)

(defun %init-reader-vars ()
  "Initialize reader/print variables."
  (setq *read-base* 10)
  (setq *read-suppress* nil)
  (setq *read-eval* t)
  (setq *read-default-float-format* nil)
  (setq *print-array* t)
  (setq *print-base* 10)
  (setq *print-case* :upcase)
  (setq *print-circle* nil)
  (setq *print-escape* t)
  (setq *print-gensym* t)
  (setq *print-length* nil)
  (setq *print-level* nil)
  (setq *print-lines* nil)
  (setq *print-miser-width* nil)
  (setq *print-pprint-dispatch* nil)
  (setq *print-pretty* nil)
  (setq *print-radix* nil)
  (setq *print-readably* nil)
  (setq *print-right-margin* nil)
  ;; Default *features* — built-ins always present.  Tests can rebind
  ;; via (let ((*features* …)) …) since *features* is dynamic.
  (setq *features* (list :common-lisp :cl :ansi-cl :modus)))

(defun %init-reader ()
  "Initialize reader. Must be called after %init-packages."
  (%init-reader-vars)
  (setq *standard-readtable* (%make-readtable))
  ;; Create a separate readtable for *readtable* (not a copy, to avoid cons-in-loop crash)
  (setq *readtable* (%make-readtable)))

;;; --- Macro character API ---

(defun get-macro-character (char &rest args)
  "Get the macro function and non-terminating-p for CHAR."
  (let ((rt (if args (if (car args) (car args) *standard-readtable*) *readtable*)))
    (let ((code (char-code char)))
      (if (>= code 128)
          (values nil nil)
          (let ((entry (aref (%rt-macros rt) code)))
            (if (consp entry)
                (values (car entry) (cdr entry))
                (values nil nil)))))))

(defun set-macro-character (char fn &rest args)
  "Set the macro function for CHAR. Optional non-terminating-p and readtable."
  (let ((non-term-p (if args (car args) nil))
        (rt (if (cdr args) (if (cadr args) (cadr args) *readtable*) *readtable*)))
    (let ((code (char-code char)))
      (when (< code 128)
        (aset (%rt-macros rt) code (cons fn non-term-p))))
    t))

;;; --- Dispatch macro character API ---

(defun %get-dispatch-table (disp-char rt)
  "Get the dispatch sub-table for DISP-CHAR in RT."
  (let ((code (char-code disp-char))
        (dl (%rt-dispatch rt)))
    (let ((found nil))
      (dolist (entry dl)
        (when (= (car entry) code)
          (setq found (cdr entry))))
      found)))

(defun make-dispatch-macro-character (char &rest args)
  "Make CHAR a dispatch macro character."
  (let ((non-term-p (if args (car args) nil))
        (rt (if (cdr args) (if (cadr args) (cadr args) *readtable*) *readtable*)))
    (let ((code (char-code char)))
      ;; Set as macro character
      (when (< code 128)
        (aset (%rt-macros rt) code (cons :dispatch non-term-p)))
      ;; Create dispatch sub-table if not exists
      (unless (%get-dispatch-table char rt)
        (aset (%rt-data rt) 2
              (cons (cons code (make-array 128)) (%rt-dispatch rt)))))
    t))

(defun get-dispatch-macro-character (disp-char sub-char &rest args)
  "Get the dispatch function for DISP-CHAR SUB-CHAR."
  (let ((rt (if args (if (car args) (car args) *standard-readtable*) *readtable*)))
    (let ((sub-table (%get-dispatch-table disp-char rt)))
      (if sub-table
          (let ((code (char-code (char-upcase sub-char))))
            (if (< code 128)
                (aref sub-table code)
                nil))
          nil))))

(defun set-dispatch-macro-character (disp-char sub-char fn &rest args)
  "Set the dispatch function for DISP-CHAR SUB-CHAR."
  (let ((rt (if args (if (car args) (car args) *readtable*) *readtable*)))
    (let ((sub-table (%get-dispatch-table disp-char rt)))
      (unless sub-table
        (make-dispatch-macro-character disp-char nil rt)
        (setq sub-table (%get-dispatch-table disp-char rt)))
      (let ((code (char-code (char-upcase sub-char))))
        (when (< code 128)
          (aset sub-table code fn))))
    t))

(defun set-syntax-from-char (to-char from-char &rest args)
  "Copy syntax from FROM-CHAR to TO-CHAR."
  (let ((to-rt (if args (car args) *readtable*))
        (from-rt (if (cdr args) (cadr args) *standard-readtable*)))
    (let ((to-code (char-code to-char))
          (from-code (char-code from-char)))
      (when (and (< to-code 128) (< from-code 128))
        ;; Copy syntax type
        (aset (%rt-syntax to-rt) to-code (aref (%rt-syntax from-rt) from-code))
        ;; Copy macro character entry
        (let ((from-entry (aref (%rt-macros from-rt) from-code)))
          (aset (%rt-macros to-rt) to-code
                (if (consp from-entry) (cons (car from-entry) (cdr from-entry)) nil))))))
  t)

;;; --- Reader helper: character classification ---

(defun %whitespace-char-p (ch)
  "True if CH is a whitespace character."
  (let ((code (char-code ch)))
    (or (= code 32) (= code 9) (= code 10) (= code 13) (= code 12))))

(defun %terminating-macro-p (ch rt)
  "True if CH is a terminating macro character in RT."
  (let ((code (char-code ch)))
    (if (>= code 128) nil
        (let ((entry (aref (%rt-macros rt) code)))
          (if (consp entry)
              (not (cdr entry))  ; not non-terminating = terminating
              nil)))))

(defun %macro-char-p (ch rt)
  "True if CH is any macro character in RT."
  (let ((code (char-code ch)))
    (if (>= code 128) nil
        (if (consp (aref (%rt-macros rt) code)) t nil))))

(defun %syntax-type (ch rt)
  "Get syntax type for CH in RT."
  (let ((code (char-code ch)))
    (if (>= code 128) :constituent
        (let ((syn (aref (%rt-syntax rt) code)))
          (if syn syn :constituent)))))

;;; --- Core reader implementation ---

(defun %reader-error (msg)
  "Signal a reader error."
  (error msg))


(defun %read-skip-whitespace (stream)
  "Skip whitespace chars AND line comments (semicolon to newline).
   Returns first non-whitespace non-comment char or nil on EOF.
   Without comment-skip, %read-internal hands `;' to the macro dispatch
   which has no :semicolon clause — the test files (every ANSI .lsp
   starts with a `;-*- Mode: Lisp -*-' magic line + author comments)
   would have their first form misread."
  (let ((ch nil))
    (loop
      (setq ch (read-char stream nil nil nil))
      (when (null ch) (return nil))
      (cond
        ((%whitespace-char-p ch) nil)   ; skip whitespace
        ((eql ch (code-char 59))         ; #\;
         ;; skip to end-of-line (or EOF)
         (loop (let ((c (read-char stream nil nil nil)))
                 (when (or (null c) (eql c (code-char 10)) (eql c (code-char 13)))
                   (return nil)))))
        (t (return ch))))))

(defun %invalid-constituent-p (ch)
  "Per CLHS 2.4.4.4: #\\Backspace, #\\Tab (when not whitespace),
   #\\Linefeed, #\\Page, #\\Return, #\\Rubout have constituent trait
   :invalid in standard syntax — using them in a token is a reader
   error.  Modus's whitespace set already includes Tab/Newline/Page/CR,
   so the only chars left here are Backspace (8) and Rubout (127).
   Plus all OTHER control chars below 32 that aren't in the whitespace
   set — they're also :invalid by analogy."
  (let ((code (char-code ch)))
    (and (or (= code 8) (= code 127)
             (and (< code 32)
                  (not (= code 9))   ; tab
                  (not (= code 10))  ; newline
                  (not (= code 12))  ; page
                  (not (= code 13))))  ; return
         t)))

(defun %read-as-token (ch stream rt)
  "Read ch as start of a token.  Invalid-constituent chars signal a
   reader-error per CLHS 2.4.4.4."
  (when (%invalid-constituent-p ch)
    (%reader-error "invalid character in token"))
  (let ((syn (%syntax-type ch rt)))
    (%read-token-from stream ch rt
                      (or (eq syn :single-escape) (eq syn :multiple-escape)))))

(defun %read-check-macro (ch)
  "Check if CH is a macro character. Returns macro fn or nil."
  (let ((code (char-code ch)))
    (if (< code 128)
        (let ((entry (aref (%rt-macros *readtable*) code)))
          (if (consp entry) (car entry) nil))
        nil)))

(defun %read-internal (stream eof-error-p eof-value recursive-p)
  "Internal read function."
  (let ((ch (%read-skip-whitespace stream)))
    (if (null ch)
        (if eof-error-p (%reader-error "end of file during read") eof-value)
        (let ((macro-fn (%read-check-macro ch)))
          (if macro-fn
              (%read-macro-dispatch-simple macro-fn ch stream *readtable*)
              (%read-as-token ch stream *readtable*))))))

(defun %read-after-ws2 (ch stream eof-error-p eof-value)
  "Dispatch after whitespace skip."
  (let ((macro-fn (%read-check-macro ch)))
    (if (null macro-fn)
        (%read-as-token ch stream *readtable*)
        (if (eq macro-fn :semicolon)
            (progn (%skip-line-comment stream)
                   (%read-internal stream eof-error-p eof-value nil))
            (%read-macro-dispatch-simple macro-fn ch stream *readtable*)))))

(defun %read-macro-dispatch-simple (fn ch stream rt)
  "Handle common macro character types."
  (cond
    ((eq fn :lparen) (%read-list stream rt nil))
    ((eq fn :rparen) (%reader-error "unmatched close parenthesis"))
    ((eq fn :quote)
     (let ((obj (%read-internal stream t nil t)))
       (if *read-suppress* nil (list 'quote obj))))
    ((eq fn :string) (%read-string stream))
    ((eq fn :backquote) (%read-backquote stream))
    ((eq fn :comma) (%read-comma stream))
    ((eq fn :sharpsign) (%read-sharpsign stream rt))
    ((eq fn :dispatch) (%read-user-dispatch ch stream rt))
    ((or (functionp fn) (%cl-sym-p fn))
     (let ((result (funcall fn stream ch)))
       (if *read-suppress* nil result)))
    (t nil)))

;;; --- Token reader ---

;;; Token reading state — stored in a simple list to avoid too many local vars
;;; State = (chars all-escaped in-escape has-escape)

(defun %token-process-first (first-char stream rt)
  "Process the first character of a token. Returns (chars all-escaped in-escape has-escape)."
  (let ((syn (%syntax-type first-char rt)))
    (cond
      ((eq syn :single-escape)
       (let ((next (read-char stream t nil t)))
         (list (list (char-code next)) (list t) nil t)))
      ((eq syn :multiple-escape)
       (list nil nil t t))
      (t
       (list (list (char-code first-char)) (list nil) nil nil)))))

(defun %token-add-char (chars all-escaped ch escaped)
  "Add a character to the token accumulator."
  (cons (cons (char-code ch) chars) (cons (cons escaped all-escaped) nil)))

;;; Token state array: [0]=chars, [1]=all-escaped, [2]=in-escape, [3]=has-escape

(defun %token-state-new (chars escaped in-esc has-esc)
  "Create a token state array."
  (let ((st (make-array 4)))
    (aset st 0 chars)
    (aset st 1 escaped)
    (aset st 2 in-esc)
    (aset st 3 has-esc)
    st))

(defun %token-handle-in-escape (ch stream st rt)
  "Handle a char while inside multiple escape."
  (let ((syn (%syntax-type ch rt)))
    (cond
      ((eq syn :multiple-escape)
       (aset st 2 nil))
      ((eq syn :single-escape)
       (let ((next (read-char stream t nil t)))
         (aset st 0 (cons (char-code next) (aref st 0)))
         (aset st 1 (cons t (aref st 1)))))
      (t
       (aset st 0 (cons (char-code ch) (aref st 0)))
       (aset st 1 (cons t (aref st 1)))))))

(defun %token-handle-normal (ch stream st rt)
  "Handle a char in normal (non-escape) mode. Returns :done if token ended, nil otherwise."
  (cond
    ((%whitespace-char-p ch)
     (unread-char ch stream)
     :done)
    ((%terminating-macro-p ch rt)
     (unread-char ch stream)
     :done)
    ((eq (%syntax-type ch rt) :single-escape)
     (aset st 3 t)
     (let ((next (read-char stream t nil t)))
       (aset st 0 (cons (char-code next) (aref st 0)))
       (aset st 1 (cons t (aref st 1))))
     nil)
    ((eq (%syntax-type ch rt) :multiple-escape)
     (aset st 2 t)
     (aset st 3 t)
     nil)
    (t
     (aset st 0 (cons (char-code ch) (aref st 0)))
     (aset st 1 (cons nil (aref st 1)))
     nil)))

(defun %token-add-constituent (st ch)
  "Add a constituent character to token state."
  (let ((new-chars (cons (char-code ch) (aref st 0))))
    (let ((dummy (aset st 0 new-chars))) nil))
  (let ((new-esc (cons nil (aref st 1))))
    (let ((dummy (aset st 1 new-esc))) nil)))

(defun %token-read-char (stream)
  "Read one char from stream, return nil on EOF."
  (read-char stream nil nil nil))

(defun %token-read-loop (stream rt st)
  "Read remaining token characters. Mutates and returns state array ST.
   Honours single-escape (\\) and multiple-escape (|) mid-token per
   CLHS 2.2 (Reader Algorithm).  ST[2]=in-multi-escape, ST[3]=has-escape.
   EOF inside |...| is end-of-file (the multiple-escape was never
   closed)."
  (loop
    (let ((ch (%token-read-char stream)))
      (when (null ch)
        ;; Unterminated |...| → end-of-file.
        (when (aref st 2) (%reader-error "end of file inside | ... |"))
        (return st))
      (cond
        ;; Inside |...| — only multiple-escape closes it; single-escape
        ;; reads next literal; everything else is a literal char.
        ((aref st 2)
         (%token-handle-in-escape ch stream st rt))
        ;; Normal mode
        ((%whitespace-char-p ch)
         (unread-char ch stream)
         (return st))
        ((%terminating-macro-p ch rt)
         (unread-char ch stream)
         (return st))
        ((eq (%syntax-type ch rt) :single-escape)
         (let ((next (read-char stream t nil t)))
           (aset st 0 (cons (char-code next) (aref st 0)))
           (aset st 1 (cons t (aref st 1)))
           (aset st 3 t)))
        ((eq (%syntax-type ch rt) :multiple-escape)
         (aset st 2 t)
         (aset st 3 t))
        (t
         (%token-add-constituent st ch))))))

(defun %read-token-from (stream first-char rt has-escape)
  "Read a token starting with FIRST-CHAR. Handle escapes."
  (let ((first-result (%token-process-first first-char stream rt)))
    (let ((chars (car first-result))
          (all-escaped (cadr first-result))
          (in-escape (caddr first-result))
          (esc (cadddr first-result)))
      (when esc (setq has-escape t))
      (let ((st (%token-read-loop stream rt (%token-state-new chars all-escaped in-escape has-escape))))
        (let ((final-chars (nreverse (aref st 0)))
              (final-escaped (nreverse (aref st 1)))
              (final-has-escape (aref st 3)))
          (if *read-suppress*
              nil
              (%interpret-token final-chars final-escaped final-has-escape rt)))))))

;;; --- Token interpretation ---

(defun %apply-readtable-case (chars all-escaped rt)
  "Apply readtable case to unescaped characters. Returns new char code list."
  (let ((rc (readtable-case rt))
        (result nil))
    (let ((c-cur chars) (e-cur all-escaped))
      (loop
        (when (null c-cur) (return nil))
        (let ((code (car c-cur))
              (esc (car e-cur)))
          (if esc
              ;; Escaped: preserve case
              (setq result (cons code result))
              ;; Unescaped: apply case conversion
              (cond
                ((eq rc :upcase)
                 (setq result (cons (if (and (>= code 97) (<= code 122))
                                        (- code 32) code)
                                    result)))
                ((eq rc :downcase)
                 (setq result (cons (if (and (>= code 65) (<= code 90))
                                        (+ code 32) code)
                                    result)))
                ((eq rc :preserve)
                 (setq result (cons code result)))
                ((eq rc :invert)
                 ;; Invert: if all unescaped letters are same case, invert all
                 ;; Handled below in bulk
                 (setq result (cons code result)))
                (t (setq result (cons code result))))))
        (setq c-cur (cdr c-cur))
        (setq e-cur (cdr e-cur))))
    (setq result (nreverse result))
    ;; Handle :invert case separately
    (when (eq (readtable-case rt) :invert)
      (let ((has-upper nil) (has-lower nil))
        (let ((r-cur result) (e-cur all-escaped))
          (loop
            (when (null r-cur) (return nil))
            (unless (car e-cur)
              (let ((code (car r-cur)))
                (when (and (>= code 65) (<= code 90)) (setq has-upper t))
                (when (and (>= code 97) (<= code 122)) (setq has-lower t))))
            (setq r-cur (cdr r-cur))
            (setq e-cur (cdr e-cur))))
        ;; If all same case, invert
        (when (and has-upper (not has-lower))
          ;; All uppercase unescaped → downcase
          (let ((new-result nil))
            (let ((r-cur result) (e-cur all-escaped))
              (loop
                (when (null r-cur) (return nil))
                (let ((code (car r-cur)))
                  (if (car e-cur)
                      (setq new-result (cons code new-result))
                      (setq new-result (cons (if (and (>= code 65) (<= code 90))
                                                  (+ code 32) code)
                                              new-result))))
                (setq r-cur (cdr r-cur))
                (setq e-cur (cdr e-cur))))
            (setq result (nreverse new-result))))
        (when (and has-lower (not has-upper))
          ;; All lowercase unescaped → upcase
          (let ((new-result nil))
            (let ((r-cur result) (e-cur all-escaped))
              (loop
                (when (null r-cur) (return nil))
                (let ((code (car r-cur)))
                  (if (car e-cur)
                      (setq new-result (cons code new-result))
                      (setq new-result (cons (if (and (>= code 97) (<= code 122))
                                                  (- code 32) code)
                                              new-result))))
                (setq r-cur (cdr r-cur))
                (setq e-cur (cdr e-cur))))
            (setq result (nreverse new-result))))))
    result))

(defun %codes-to-string (codes)
  "Convert a list of char codes to a string."
  (let ((len (list-length codes)))
    (let ((s (%make-string-array len))
          (i 0)
          (cur codes))
      (loop
        (when (null cur) (return s))
        (aset s i (car cur))
        (setq i (+ i 1))
        (setq cur (cdr cur))))))

(defun %try-parse-integer (codes base)
  "Try to parse char codes as an integer in BASE. Returns (value . t) or nil."
  (if (null codes) nil
      (let ((sign 1) (cur codes) (n 0) (got-digit nil))
        ;; Check sign
        (when (= (car cur) 43) (setq cur (cdr cur)))    ; +
        (when (and cur (= (car codes) 45))               ; -
          (setq sign -1) (setq cur (cdr cur)))
        (when (null cur) (return-from %try-parse-integer nil))
        ;; Parse digits
        (loop
          (when (null cur)
            (if got-digit
                (return-from %try-parse-integer
                  (cons (if (bignump n)
                            (if (= sign -1) (bignum-sub 0 n) n)
                            (* sign n))
                        t))
                (return-from %try-parse-integer nil)))
          (let ((code (car cur)))
            (let ((digit (cond
                           ((and (>= code 48) (<= code 57)) (- code 48))
                           ((and (>= code 65) (<= code 90)) (+ 10 (- code 65)))
                           ((and (>= code 97) (<= code 122)) (+ 10 (- code 97)))
                           (t nil))))
              (if (and digit (< digit base))
                  (progn
                    ;; Lazy bignum promote: while n stays small, use fast
                    ;; inline (+ (* n base) digit).  Once n exceeds the
                    ;; safe fixnum*base threshold OR is already a bignum,
                    ;; route through bignum-mul / bignum-add so large
                    ;; literals (e.g. 100000000000000000000) parse to a
                    ;; real bignum instead of silently overflowing :mul.
                    (cond
                      ((or (bignump n) (>= n 230584300921369395))   ; 2^61 / 20 (safe for base ≤ 20)
                       (setq n (bignum-add (bignum-mul n base) digit)))
                      (t
                       (setq n (+ (* n base) digit))))
                    (setq got-digit t) (setq cur (cdr cur)))
                  ;; Check for trailing dot (integer token like "123.")
                  (if (and (= code 46) (null (cdr cur)) got-digit)
                      (return-from %try-parse-integer
                        (cons (if (or (bignump n) (= sign 1))
                                  (if (= sign 1) n (bignum-sub 0 n))
                                  (- 0 n))
                              t))
                      (return-from %try-parse-integer nil)))))))))

;;; The name `%make-float` is intercepted by Modus's compiler as a
;;; primop that allocates a 1-slot uninitialized float object (see
;;; compile-make-float in compiler.lisp ~line 9640).  The 6-arg defun
;;; below NEVER ran from compiled callers — the parser's call site
;;; emitted the alloc primop with no init, returning a 1-slot float
;;; that displayed as 0.0.  Every runtime-EVAL literal float read as
;;; 0.0 with %ieee-float-p = NIL, so generic-add fell to the fixnum
;;; path and produced garbage bit-pattern arithmetic.
;;;
;;; Rename the constructor to `%build-float-from-parts` so the
;;; compiler emits a normal call instead of the primop shortcut.

(defun %build-float-from-parts (sign int-part frac-part frac-div exp-sign exp-part)
  "Create a real IEEE-bit boxed float (subtag #x60) from parsed components.
   value = sign * (int-part + frac-part/frac-div) * 10^(exp-sign*exp-part)
   Built via SSE2 %float-from-int + %float-div, so the result is a true
   #x60 IEEE float that :fadd/:fmul/:fcmp can operate on natively and the
   printer's IEEE-decoder can format."
  (let ((mantissa (+ (* int-part frac-div) frac-part))
        (divisor frac-div)
        (exp-val (* exp-sign exp-part)))
    ;; Apply decimal exponent: positive → multiply mantissa, negative → multiply divisor.
    (let ((i 0))
      (loop
        (when (>= i exp-val) (return nil))
        (setq mantissa (* mantissa 10))
        (setq i (+ i 1))))
    (let ((i 0))
      (loop
        (when (>= i (- 0 exp-val)) (return nil))
        (setq divisor (* divisor 10))
        (setq i (+ i 1))))
    ;; Convert num/den → IEEE via SSE2.
    (let ((signed-mant (* sign mantissa)))
      (if (= divisor 1)
          (%float-from-int signed-mant)
          (%float-div (%float-from-int signed-mant)
                      (%float-from-int divisor))))))

(defun %try-parse-float (codes)
  "Try to parse char codes as a float. Returns float or nil.
   For MVM, we parse but return an integer approximation.

   CL float literals: either a decimal point or an exponent marker
   with at least one digit IN THE INT PART before the marker (so
   `1s2' is a float, `s1' is the symbol S1).  The pre-scan tracks
   whether the exponent marker was preceded by a digit; if not, the
   token is not a float."
  ;; Simple float detection: contains . or exponent marker (e, s, f, d, l)
  ;; but not just a dot
  (let ((has-dot nil) (has-exp nil) (len 0) (saw-pre-exp-digit nil)
        (saw-digit-yet nil))
    (let ((cur codes))
      ;; Skip leading sign for the pre-scan.
      (when (and cur (or (= (car cur) 45) (= (car cur) 43)))
        (setq cur (cdr cur)))
      (loop
        (when (null cur) (return nil))
        (let ((code (car cur)))
          (when (= code 46) (setq has-dot t))
          (when (and (>= code 48) (<= code 57))
            (setq saw-digit-yet t))
          (when (or (= code 69) (= code 101)  ; E e
                    (= code 83) (= code 115)  ; S s
                    (= code 70) (= code 102)  ; F f
                    (= code 68) (= code 100)  ; D d
                    (= code 76) (= code 108)) ; L l
            (setq has-exp t)
            ;; Did we see a digit before this marker?  Without one
            ;; the token isn't a CL float literal.
            (setq saw-pre-exp-digit (or saw-pre-exp-digit saw-digit-yet)))
          (setq len (+ len 1)))
        (setq cur (cdr cur))))
    (when (and (not has-dot) (not has-exp)) (return-from %try-parse-float nil))
    (when (and (= len 1) has-dot) (return-from %try-parse-float nil))
    ;; Exponent without a preceding int digit isn't a float (CL §2.3.2.2).
    (when (and has-exp (not has-dot) (not saw-pre-exp-digit))
      (return-from %try-parse-float nil))
    ;; Parse the float: integer-part.fraction-part[exponent]
    (let ((sign 1) (cur codes) (int-part 0) (frac-part 0) (frac-div 1)
          (exp-sign 1) (exp-part 0) (in-frac nil) (in-exp nil) (got-digit nil))
      ;; Sign
      (when (and cur (= (car cur) 45)) (setq sign -1) (setq cur (cdr cur)))
      (when (and cur (= (car cur) 43)) (setq cur (cdr cur)))
      (loop
        (when (null cur) (return nil))
        (let ((code (car cur)))
          (cond
            ((and (>= code 48) (<= code 57))
             (let ((d (- code 48)))
               (setq got-digit t)
               (cond
                 (in-exp (setq exp-part (+ (* exp-part 10) d)))
                 (in-frac (setq frac-part (+ (* frac-part 10) d))
                          (setq frac-div (* frac-div 10)))
                 (t (setq int-part (+ (* int-part 10) d))))))
            ((= code 46)
             (if in-frac (return-from %try-parse-float nil)
                 (setq in-frac t)))
            ((or (= code 69) (= code 101) (= code 83) (= code 115)
                 (= code 70) (= code 102) (= code 68) (= code 100)
                 (= code 76) (= code 108))
             (if in-exp (return-from %try-parse-float nil))
             (setq in-exp t)
             (setq in-frac nil)
             ;; Check for exponent sign
             (when (cdr cur)
               (let ((next-code (cadr cur)))
                 (when (= next-code 45)
                   (setq exp-sign -1) (setq cur (cdr cur)))
                 (when (= next-code 43)
                   (setq cur (cdr cur))))))
            (t (return-from %try-parse-float nil))))
        (setq cur (cdr cur)))
      (unless got-digit (return-from %try-parse-float nil))
      ;; Build a boxed float.  Use the rename'd builder — `%make-float'
      ;; is the compiler primop that allocates an uninitialized 1-slot
      ;; float, NOT a defun call.
      (%build-float-from-parts sign int-part frac-part frac-div exp-sign exp-part))))

(defun %tag-as-float (arr)
  "Tag an array as a float object (subtag #x60 = 96).
   Retained as a stub for any caller that imported the old reader path —
   the new %make-float returns a real #x60 IEEE object directly so this
   is effectively dead code."
  arr)

(defun %interpret-token (chars all-escaped has-escape rt)
  "Interpret a token as a number, symbol, or package-qualified symbol.
   CLHS 2.3.3: a token consisting solely of unescaped dots is reserved
   — `.', `..', `...' etc. signal a reader-error.  (`.' is consumed by
   the cons-dot mechanism in list reading; reaching %interpret-token
   means we're parsing a standalone token.)"
  ;; Apply readtable case to get the final character codes
  (let ((cased-chars (%apply-readtable-case chars all-escaped rt)))
    ;; Reject all-dots tokens when there are no escapes.
    (when (and (not has-escape) cased-chars)
      (let ((only-dots t) (cur cased-chars))
        (loop (when (null cur) (return nil))
              (unless (= (car cur) 46) (setq only-dots nil) (return nil))
              (setq cur (cdr cur)))
        (when only-dots
          (%reader-error "token consisting only of dots is reserved"))))
    ;; If no escapes, try as number first
    (if (not has-escape)
        (let ((num (%try-parse-integer cased-chars *read-base*)))
          (if num
              (car num)
              ;; Try as float
              (let ((flt (%try-parse-float cased-chars)))
                (if flt flt
                    ;; Try as ratio: N/D
                    (let ((ratio (%try-parse-ratio cased-chars)))
                      (if ratio ratio
                          ;; It's a symbol
                          (%interpret-symbol-token cased-chars all-escaped)))))))
        ;; Has escapes — always a symbol
        (%interpret-symbol-token cased-chars all-escaped))))

(defun %try-parse-ratio (codes)
  "Try to parse char codes as a ratio N/D. Returns value or nil."
  (let ((slash-pos nil) (i 0) (cur codes))
    (loop
      (when (null cur) (return nil))
      (when (= (car cur) 47)  ; /
        (when slash-pos (return-from %try-parse-ratio nil))  ; multiple slashes
        (setq slash-pos i))
      (setq i (+ i 1))
      (setq cur (cdr cur)))
    (unless slash-pos (return-from %try-parse-ratio nil))
    (when (= slash-pos 0) (return-from %try-parse-ratio nil))
    ;; Split at slash
    (let ((num-codes nil) (den-codes nil) (j 0) (cur codes))
      (loop
        (when (null cur) (return nil))
        (if (< j slash-pos)
            (setq num-codes (cons (car cur) num-codes))
            (when (> j slash-pos)
              (setq den-codes (cons (car cur) den-codes))))
        (setq j (+ j 1))
        (setq cur (cdr cur)))
      (setq num-codes (nreverse num-codes))
      (setq den-codes (nreverse den-codes))
      (let ((num (%try-parse-integer num-codes *read-base*))
            (den (%try-parse-integer den-codes *read-base*)))
        (cond
          ;; Well-formed ratio: build it via exact-divide.
          ((and num den (not (= (car den) 0)))
           (exact-divide (car num) (car den)))
          ;; Both halves parse as integers but denominator is zero —
          ;; CLHS 2.3.2.2 says this is a reader-error, not a symbol.
          ((and num den (= (car den) 0))
           (%reader-error "ratio with zero denominator"))
          ;; Otherwise: not a ratio.  Caller falls through to symbol.
          (t nil))))))

(defun %interpret-symbol-token (cased-chars &optional all-escaped)
  "Interpret char codes as a symbol, handling package qualifiers.
   Colons that were escaped (single-escape `\\` or inside `|...|`) are
   treated as constituent chars, not package separators."
  (let ((name-str (%codes-to-string cased-chars)))
    ;; Check for package qualifier — skip escaped colons.
    (let ((colon-pos nil) (double-colon nil) (i 0) (len (length name-str))
          (esc-cur all-escaped))
      ;; Find first UNESCAPED colon
      (loop
        (when (>= i len) (return nil))
        (when (and (= (aref name-str i) 58)
                   (not (and esc-cur (car esc-cur))))
          (setq colon-pos i)
          (return nil))
        (setq i (+ i 1))
        (when esc-cur (setq esc-cur (cdr esc-cur))))
      (cond
        ;; No colon — intern in *package*
        ((null colon-pos)
         ;; Special tokens NIL/T match ONLY when token had no escapes —
         ;; ANSI: `\T` reads as a symbol named "T" in *package*, not the
         ;; boolean T.  Detect any escape via the all-escaped flag list.
         (let ((any-escape nil) (ec all-escaped))
           (loop (when (null ec) (return nil))
             (when (car ec) (setq any-escape t) (return nil))
             (setq ec (cdr ec)))
           (cond
             ((and (not any-escape) (string-equal name-str "NIL")) nil)
             ((and (not any-escape) (string-equal name-str "T")) t)
             (t (intern name-str *package*)))))
        ;; Leading colon — keyword
        ((= colon-pos 0)
         (let ((kw-name (%substring name-str 1 len)))
           (intern kw-name (find-package "KEYWORD"))))
        ;; Package qualifier
        (t
         (let ((pkg-name (%substring name-str 0 colon-pos))
               (sym-start (+ colon-pos 1)))
           ;; Check for double colon
           (when (and (< sym-start len) (= (aref name-str sym-start) 58))
             (setq double-colon t)
             (setq sym-start (+ sym-start 1)))
           (let ((sym-name (%substring name-str sym-start len))
                 (pkg (find-package pkg-name)))
             (if (null pkg)
                 (%reader-error "package not found")
                 (if double-colon
                     ;; pkg::sym — internal access, just intern
                     (intern sym-name pkg)
                     ;; pkg:sym — external access
                     (multiple-value-bind (sym status) (find-symbol sym-name pkg)
                       (if sym sym
                           ;; Not found — intern anyway for robustness
                           (intern sym-name pkg))))))))))))

;;; --- List reader ---

(defun %read-list (stream rt recursive-p)
  "Read a list until closing paren."
  (let ((result nil) (tail nil) (dotted nil))
    (loop
      ;; Skip whitespace
      (let ((ch nil))
        (loop
          (setq ch (read-char stream nil nil t))
          (when (null ch) (%reader-error "end of file reading list"))
          (unless (%whitespace-char-p ch) (return nil)))
        ;; Check for close paren
        (when (eql ch #\))
          (if *read-suppress*
              (return nil)
              (return (if dotted (car result) result))))
        ;; Check for dot (consing dot)
        (when (eql ch #\.)
          ;; Peek at next char to disambiguate dot from symbol starting with dot
          (let ((next (read-char stream nil nil t)))
            (cond
              ((null next) (%reader-error "end of file after dot"))
              ((or (%whitespace-char-p next) (eql next #\)) (%terminating-macro-p next rt))
               ;; It's the consing dot
               (when (null result) (%reader-error "dot at start of list"))
               (unread-char next stream)
               (let ((cdr-val (%read-internal stream t nil t)))
                 ;; Skip whitespace and read closing paren
                 (let ((close-ch nil))
                   (loop
                     (setq close-ch (read-char stream nil nil t))
                     (when (null close-ch) (%reader-error "end of file after dotted pair"))
                     (unless (%whitespace-char-p close-ch) (return nil)))
                   (unless (eql close-ch #\))
                     (%reader-error "more than one object after dot")))
                 ;; Set cdr of last cons
                 (if tail (set-cdr tail cdr-val) (setq result (list cdr-val)))
                 (setq dotted t)
                 ;; Continue to read closing paren — already consumed above
                 (return (if (and (not tail) dotted) cdr-val
                             (progn (when tail (set-cdr tail cdr-val)) result)))))
              (t
               ;; Dot followed by constituent — it's part of a symbol/number
               (unread-char next stream)
               (unread-char ch stream)
               (let ((obj (%read-internal stream t nil t)))
                 (when (not *read-suppress*)
                   (let ((new-cons (list obj)))
                     (if tail (progn (set-cdr tail new-cons) (setq tail new-cons))
                         (progn (setq result new-cons) (setq tail new-cons))))))))))
        ;; Not close paren and not dot — unread and read an object
        (when (not (eql ch #\.))
          (unread-char ch stream)
          (let ((obj (%read-internal stream t nil t)))
            (when (not *read-suppress*)
              (let ((new-cons (list obj)))
                (if tail (progn (set-cdr tail new-cons) (setq tail new-cons))
                    (progn (setq result new-cons) (setq tail new-cons)))))))))))

;;; --- String reader ---

(defun %read-string (stream)
  "Read a string delimited by double-quotes."
  (let ((chars nil))
    (loop
      (let ((ch (read-char stream nil nil t)))
        (when (null ch) (%reader-error "end of file reading string"))
        (cond
          ((eql ch #\")
           ;; End of string
           (if *read-suppress* (return nil)
               (return (%codes-to-string (nreverse chars)))))
          ((eql ch #\\)
           ;; Escape: next char is literal
           (let ((next (read-char stream t nil t)))
             (setq chars (cons (char-code next) chars))))
          (t
           (setq chars (cons (char-code ch) chars))))))))

;;; --- Comment reader ---

(defun %skip-line-comment (stream)
  "Skip to end of line."
  (loop
    (let ((ch (read-char stream nil nil t)))
      (when (null ch) (return nil))
      (when (eql ch #\Newline) (return nil)))))

;;; --- Backquote/comma reader ---

;;; A comma is only legal inside a backquote.  *backquote-depth*
;;; counts how many backquotes are currently open; each comma
;;; consumes one level.  When the reader sees a comma at depth 0 it
;;; signals a reader-error per CLHS 2.4.6.1.  Tracked dynamically so
;;; nested reads (e.g. `(foo ,(bar ',baz))) see the right depth.
(defvar *backquote-depth* 0)

(defun %read-backquote (stream)
  "Read a backquote expression."
  (declare (special *backquote-depth*))
  (let ((*backquote-depth* (+ *backquote-depth* 1)))
    (declare (special *backquote-depth*))
    (let ((obj (%read-internal stream t nil t)))
      (if *read-suppress* nil
          (list 'backquote obj)))))

(defun %read-comma (stream)
  "Read a comma expression.  Outside of a backquote (depth=0) this
   is a reader-error per CLHS 2.4.6.1."
  (declare (special *backquote-depth*))
  (when (<= *backquote-depth* 0)
    (unless *read-suppress*
      (%reader-error "comma outside of a backquote")))
  (let ((*backquote-depth* (- *backquote-depth* 1))
        (next (read-char stream t nil t)))
    (declare (special *backquote-depth*))
    (cond
      ((eql next #\@)
       (let ((obj (%read-internal stream t nil t)))
         (if *read-suppress* nil
             (list 'comma-at obj))))
      ((eql next #\.)
       (let ((obj (%read-internal stream t nil t)))
         (if *read-suppress* nil
             (list 'comma-dot obj))))
      (t
       (unread-char next stream)
       (let ((obj (%read-internal stream t nil t)))
         (if *read-suppress* nil
             (list 'comma obj)))))))

;;; --- Sharpsign dispatch ---

(defun %sharp-label-fixup (root marker obj seen)
  "Walk ROOT (the object that was the target of #N=) and replace every
   occurrence of MARKER (the placeholder cons) with OBJ.  SEEN is an
   alist of conses/vectors we've already visited so we terminate on the
   cycle that #N= itself introduces.

   Walks cons cells and simple vectors.  Symbols, numbers and strings
   never contain markers, so they short-circuit.  This handles the
   syntax.sharp-circle.{1..7} ANSI tests."
  (cond
    ((or (null root) (eq root marker)) nil)
    ((consp root)
     (let ((ent (assoc root seen)))
       (when (and ent (eq (cdr ent) t)) (return-from %sharp-label-fixup nil)))
     (let ((seen2 (cons (cons root t) seen)))
       (when (eq (car root) marker) (set-car root obj))
       (unless (eq (car root) marker) (%sharp-label-fixup (car root) marker obj seen2))
       (when (eq (cdr root) marker) (set-cdr root obj))
       (unless (eq (cdr root) marker) (%sharp-label-fixup (cdr root) marker obj seen2))))
    ((vectorp root)
     ;; Vectors compared by EQ via assoc with cons keys won't dedupe — that
     ;; is fine for the small shapes the tests use.  No SEEN protection
     ;; for vectors because we only descend into them once from inside a
     ;; surrounding cons.
     (let ((len (length root)) (i 0))
       (loop
         (when (>= i len) (return nil))
         (let ((e (aref root i)))
           (cond
             ((eq e marker) (aset root i obj))
             ((consp e)     (%sharp-label-fixup e marker obj seen))
             ((vectorp e)   (%sharp-label-fixup e marker obj seen))))
         (setq i (+ i 1)))))))

(defun %ensure-keyword (x)
  "Coerce X to a keyword symbol if it's a non-keyword symbol/string.
   Used by #S to handle unkeyworded slot names like (a x b y)."
  (cond
    ((null x) (intern "NIL" (find-package "KEYWORD")))
    ((eq x t) (intern "T" (find-package "KEYWORD")))
    ((stringp x) (intern x (find-package "KEYWORD")))
    ((symbolp x)
     (let ((pkg (symbol-package x)))
       (if (and pkg (string= (package-name pkg) "KEYWORD"))
           x
           (intern (symbol-name x) (find-package "KEYWORD")))))
    (t x)))

(defun %build-sharp-s (form)
  "Build a structure from #S(struct-type slot1 val1 ...).
   FORM is the inner list as read.  We construct a call to the
   structure constructor (MAKE-<struct-type>) via eval, normalising
   slot names to keywords first."
  (let ((struct-type (car form))
        (slot-args (cdr form)))
    ;; Normalize slot names: convert non-keyword symbols to keywords.
    ;; Slot list is (slot1 val1 slot2 val2 ...).  Keep vals as-is by
    ;; quoting them so eval doesn't re-evaluate (e.g. quote a symbol
    ;; value like X — the test expects the symbol X, not X's value).
    (let ((normalized nil)
          (cur slot-args))
      (loop
        (when (null cur) (return nil))
        (let ((slot-name (car cur))
              (slot-val (if (cdr cur) (cadr cur) nil)))
          (setq normalized (cons (%ensure-keyword slot-name) normalized))
          (setq normalized (cons (list 'quote slot-val) normalized)))
        (setq cur (if (cdr cur) (cddr cur) nil)))
      (let ((ctor-name (concatenate 'string "MAKE-" (symbol-name struct-type))))
        (let ((ctor-sym (intern ctor-name (symbol-package struct-type))))
          ;; Build: (MAKE-<type> :slot1 'val1 ...) and eval it.
          (let ((call-form (cons ctor-sym (nreverse normalized))))
            (handler-case (eval call-form)
              (error (c) (declare (ignore c)) nil))))))))

(defun %collect-flat-elements (obj acc)
  "Flatten OBJ (a nested list, vector, string, or bit-vector) into a
   list of leaf elements, appending to ACC in reverse order.  Used by
   #A to populate the array with :initial-contents in row-major order
   when ACC is later nreversed."
  (cond
    ((null obj) acc)
    ((stringp obj)
     (let ((i 0) (n (length obj)))
       (loop
         (when (>= i n) (return acc))
         (setq acc (cons (code-char (aref obj i)) acc))
         (setq i (+ i 1)))))
    ((consp obj)
     (let ((cur obj))
       (loop
         (when (null cur) (return acc))
         (setq acc (%collect-flat-elements (car cur) acc))
         (setq cur (cdr cur)))))
    ((vectorp obj)
     (let ((i 0) (n (length obj)))
       (loop
         (when (>= i n) (return acc))
         (setq acc (%collect-flat-elements (aref obj i) acc))
         (setq i (+ i 1)))))
    (t (cons obj acc))))

(defun %compute-dims (rank contents)
  "Compute the dimension list for a rank-RANK array whose top-level
   structure is CONTENTS.  Descends RANK levels: at each level we read
   the length of the sequence and recurse on the first element.  For
   rank 0, returns the empty list.  For strings/bit-vectors at the
   leaf level we treat them as the leaf sequence."
  (cond
    ((<= rank 0) nil)
    (t
     (let ((len (cond
                  ((null contents) 0)
                  ((stringp contents) (length contents))
                  ((consp contents) (list-length contents))
                  ((vectorp contents) (length contents))
                  (t 0)))
           (first-elt (cond
                        ((null contents) nil)
                        ((stringp contents) nil)
                        ((consp contents) (car contents))
                        ((vectorp contents)
                         (if (> (length contents) 0) (aref contents 0) nil))
                        (t nil))))
       (cons len (%compute-dims (- rank 1) first-elt))))))

(defun %build-sharp-a (rank contents)
  "Build a multi-dimensional array from #NA(contents).
   For rank=0: contents is the sole element of a 0-d array.
   For rank>=1: descend to compute dims, pass nested contents directly
   to make-array — its :initial-contents handler descends per-rank.
   Strings as inner contents are converted to char-lists so the array
   stores characters (general T-typed) rather than fixnums.

   IMPORTANT: We use (funcall #'make-array DIMS …) instead of plain
   (make-array DIMS …) for the DIMS-is-runtime-list case.  compile-make-
   array's variable-dim path only handles integer sizes; given a runtime
   LIST it SAR's the pointer and ALLOC-ARRAYs a garbage size, segfaulting
   the kernel.  funcall forces the runtime-defun path which understands
   list dims."
  (cond
    ((<= rank 0)
     (funcall #'make-array nil :initial-element contents))
    ((= rank 1)
     (let ((len (cond
                  ((null contents) 0)
                  ((stringp contents) (length contents))
                  ((consp contents) (list-length contents))
                  ((vectorp contents) (length contents))
                  (t 0)))
           (init (cond
                   ((stringp contents)
                    (let ((acc nil) (i 0) (n (length contents)))
                      (loop
                        (when (>= i n) (return (nreverse acc)))
                        (setq acc (cons (code-char (aref contents i)) acc))
                        (setq i (+ i 1)))))
                   (t contents))))
       (if (= len 0)
           (funcall #'make-array (list 0))
           (funcall #'make-array (list len) :initial-contents init))))
    (t
     ;; Rank > 1: compute dims, pass nested contents directly.
     (let ((dims (%compute-dims rank contents)))
       (let ((total 1))
         (let ((cur dims))
           (loop
             (when (null cur) (return nil))
             (setq total (* total (car cur)))
             (setq cur (cdr cur))))
         (if (= total 0)
             (funcall #'make-array dims)
             (funcall #'make-array dims :initial-contents contents)))))))

(defun %read-sharpsign (stream rt)
  "Read #-dispatched forms."
  (let ((sub-ch (read-char stream t nil t))
        (arg nil))
    ;; Read optional numeric argument
    (when (and (characterp sub-ch) (digit-char-p sub-ch))
      (setq arg 0)
      (loop
        (let ((d (digit-char-p sub-ch)))
          (if d
              (progn (setq arg (+ (* arg 10) d))
                     (setq sub-ch (read-char stream t nil t)))
              (return nil)))))
    ;; Dispatch on sub-char
    (let ((code (if (characterp sub-ch) (char-code sub-ch) 0)))
      (cond
        ;; #' — function
        ((= code 39)  ; '
         (let ((obj (%read-internal stream t nil t)))
           (if *read-suppress* nil
               (list 'function obj))))
        ;; #( — vector
        ((= code 40)  ; (
         (%read-vector stream arg))
        ;; #\ — character
        ((= code 92)  ; \
         (%read-character stream))
        ;; #: — uninterned symbol
        ((= code 58)  ; :
         (%read-uninterned-symbol stream rt))
        ;; #| — block comment
        ((= code 124) ; |
         (%skip-block-comment stream)
         (%read-internal stream t nil nil))
        ;; #+ — feature read
        ((= code 43)  ; +
         (%read-feature stream t))
        ;; #- — feature suppress
        ((= code 45)  ; -
         (%read-feature stream nil))
        ;; #x — hex integer
        ((or (= code 88) (= code 120))  ; X x
         (%read-radix-integer stream 16))
        ;; #o — octal integer
        ((or (= code 79) (= code 111))  ; O o
         (%read-radix-integer stream 8))
        ;; #b — binary integer
        ((or (= code 66) (= code 98))   ; B b
         (%read-radix-integer stream 2))
        ;; #r — radix integer (arg is the radix)
        ((or (= code 82) (= code 114))  ; R r
         (if arg
             (%read-radix-integer stream arg)
             (%reader-error "missing radix for #R")))
        ;; #* — bit vector
        ((= code 42)  ; *
         (%read-bit-vector stream arg))
        ;; #. — read-time eval (CLHS 2.4.8.6).
        ;;   *read-eval* nil → reader-error.
        ;;   *read-suppress* t → swallow the form, return NIL.
        ;;   else → eval the form and return the value.
        ((= code 46)  ; .
         (let ((obj (%read-internal stream t nil t)))
           (cond
             (*read-suppress* nil)
             ((not *read-eval*)
              (%reader-error "#. read-time eval disabled by *read-eval*"))
             (t (eval obj)))))
        ;; #S(struct-type slot1 val1 slot2 val2 ...) — structure literal.
        ;; CLHS 2.4.8.13.  Read the inner list, then call the structure's
        ;; constructor with keyword args.  If a slot symbol is unkeyworded
        ;; (e.g. `a` instead of `:a`), coerce it to a keyword.  Repeated
        ;; slots are allowed (first value wins per CLHS); we delegate to
        ;; the constructor whose &key handler observes the first-wins
        ;; rule.  Bad slots without :allow-other-keys t signal an error,
        ;; but the tests only exercise that via :allow-other-keys nil/t
        ;; which the constructor enforces — we just pass the kwargs
        ;; through.
        ((or (= code 83) (= code 115))  ; S s
         (let ((form (%read-internal stream t nil t)))
           (cond
             (*read-suppress* nil)
             ((not (consp form))
              (%reader-error "#S: expected (struct-type . initargs)"))
             (t
              (%build-sharp-s form)))))
        ;; #NA(initial-contents) — array literal.  CLHS 2.4.8.12.
        ;; ARG is the rank.  Read the contents form and call make-array.
        ;; For rank 0: contents is the single initial element.
        ;; For rank 1: contents is a sequence (list, vector, or string).
        ;; For rank > 1: contents is a nested sequence; compute dims by
        ;; descending the structure.
        ((or (= code 65) (= code 97))  ; A a
         (let ((contents (%read-internal stream t nil t)))
           (cond
             (*read-suppress* nil)
             ((null arg) (%build-sharp-a 1 contents))
             (t (%build-sharp-a arg contents)))))
        ;; #C(r i) — complex literal.  Read the inner (r i) list, then
        ;; build the complex object.
        ((or (= code 67) (= code 99))  ; C c
         (let ((pair (%read-internal stream t nil t)))
           (if (and (consp pair) (consp (cdr pair)))
               (complex (car pair) (cadr pair))
               (car pair))))
        ;; #P"path" — pathname literal.  CLHS 2.4.8.14.  Modus pathnames
        ;; are just strings, so we read the inner form and call pathname
        ;; on it.  Works for "string", #.(parse-namestring ...), and
        ;; #.(make-array ... :element-type 'base-char) shapes the ANSI
        ;; tests use.
        ((or (= code 80) (= code 112))  ; P p
         (let ((inner (%read-internal stream t nil t)))
           (if *read-suppress* nil (pathname inner))))
        ;; #N= — label the next form for later #N# reference.
        ;; CLHS 2.4.8.15.  We register a placeholder marker first so a
        ;; reference inside the form (self-cycle: `#1=(A B . #1#)`) reads
        ;; as the marker; after %read-internal completes we walk the
        ;; result and substitute the marker with the labelled object.
        ((= code 61)  ; =
         (when (null arg) (%reader-error "missing label for #="))
         (let ((marker (cons :sharp-label arg)))
           (setq *sharp-labels* (cons (cons arg marker) *sharp-labels*))
           (let ((obj (%read-internal stream t nil t)))
             ;; Mark the marker as resolved so #N# inside SIBLINGS at the
             ;; same nesting can short-circuit to obj.  We also walk
             ;; (obj) once to splice any embedded marker -> obj for
             ;; self-cycle cases like #1=(A B . #1#).
             (set-car marker obj)
             (set-cdr marker :sharp-resolved)
             (%sharp-label-fixup obj marker obj nil)
             obj)))
        ;; #N# — return the previously-labelled object.
        ((= code 35)  ; #
         (when (null arg) (%reader-error "missing label for ##"))
         (let ((entry (assoc arg *sharp-labels*)))
           (if entry
               (let ((marker (cdr entry)))
                 (if (eq (cdr marker) :sharp-resolved)
                     (car marker)
                     marker))
               (%reader-error "undefined sharp-label"))))
        ;; #< — unreadable object error
        ((= code 60)  ; <
         (%reader-error "unreadable object"))
        ;; #) — error
        ((= code 41)  ; )
         (%reader-error "unmatched #)"))
        ;; Check user dispatch table
        (t
         (let ((sub-table (%get-dispatch-table #\# rt)))
           (if sub-table
               (let ((upper-code (if (and (>= code 97) (<= code 122))
                                     (- code 32) code)))
                 (let ((fn (if (< upper-code 128) (aref sub-table upper-code) nil)))
                   (if fn
                       (funcall fn stream sub-ch arg)
                       (if *read-suppress* nil
                           (%reader-error "unknown # dispatch character")))))
               (if *read-suppress* nil
                   (%reader-error "unknown # dispatch character")))))))))

;;; --- Sharpsign helpers ---

(defun %read-vector (stream len)
  "Read #( ... ) as a simple vector."
  (let ((elements nil))
    (loop
      (let ((ch nil))
        ;; Skip whitespace
        (loop
          (setq ch (read-char stream nil nil t))
          (when (null ch) (%reader-error "end of file reading vector"))
          (unless (%whitespace-char-p ch) (return nil)))
        (when (eql ch #\))
          ;; End of vector
          (if *read-suppress* (return nil)
              (let ((elts (nreverse elements)))
                (let ((actual-len (list-length elts)))
                  (let ((vec-len (if len len actual-len)))
                    (let ((v (make-array vec-len))
                          (i 0)
                          (cur elts)
                          (last-elt nil))
                      (loop
                        (when (>= i vec-len) (return nil))
                        (if cur
                            (progn (aset v i (car cur))
                                   (setq last-elt (car cur))
                                   (setq cur (cdr cur)))
                            ;; Fill with last element if len > actual
                            (aset v i last-elt))
                        (setq i (+ i 1)))
                      (return v)))))))
        ;; Read element
        (unread-char ch stream)
        (let ((obj (%read-internal stream t nil t)))
          (setq elements (cons obj elements)))))))

(defun %read-character (stream)
  "Read #\\ character."
  (let ((ch (read-char stream t nil t)))
    (if *read-suppress* nil
        ;; Check for character name
        (let ((next (read-char stream nil nil t)))
          (if (or (null next) (%whitespace-char-p next)
                  (%terminating-macro-p next *readtable*))
              (progn
                (when next (unread-char next stream))
                ch)
              ;; Multi-character name
              (let ((name-chars (list (char-code ch) (char-code next))))
                (loop
                  (let ((c (read-char stream nil nil t)))
                    (when (or (null c) (%whitespace-char-p c)
                              (%terminating-macro-p c *readtable*))
                      (when c (unread-char c stream))
                      (return nil))
                    (setq name-chars (cons (char-code c) name-chars))))
                (let ((name-str (%codes-to-string (nreverse name-chars))))
                  (cond
                    ((string-equal name-str "Space") #\Space)
                    ((string-equal name-str "Newline") #\Newline)
                    ((string-equal name-str "Tab") #\Tab)
                    ((string-equal name-str "Return") #\Return)
                    ((string-equal name-str "Linefeed") #\Linefeed)
                    ((string-equal name-str "Page") #\Page)
                    ((string-equal name-str "Rubout") (code-char 127))
                    ((string-equal name-str "Backspace") (code-char 8))
                    ((string-equal name-str "Nul") (code-char 0))
                    ((string-equal name-str "Null") (code-char 0))
                    (t (%reader-error "unknown character name"))))))))))

(defun %read-uninterned-symbol (stream rt)
  "Read #:symbol — uninterned symbol."
  (let ((ch (read-char stream nil nil t)))
    (when (null ch) (%reader-error "end of file reading #:"))
    (unread-char ch stream)
    (let ((obj (%read-token-from stream ch rt nil)))
      ;; obj would be interned — we need to make uninterned version
      ;; Extract name from the interned result
      (if *read-suppress* nil
          (let ((name (cond
                        ((null obj) "NIL")
                        ((eq obj t) "T")
                        ((%cl-sym-p obj) (%cl-sym-name obj))
                        ((stringp obj) obj)
                        (t ""))))
            ;; Re-read: we need raw chars for the name
            ;; Actually, let's read the token properly
            (make-symbol name))))))

(defun %skip-block-comment (stream)
  "Skip #| ... |# block comment, handling nesting."
  (let ((depth 1))
    (loop
      (when (= depth 0) (return nil))
      (let ((ch (read-char stream nil nil t)))
        (when (null ch) (%reader-error "end of file in block comment"))
        (cond
          ((eql ch #\#)
           (let ((next (read-char stream nil nil t)))
             (when (and next (eql next #\|))
               (setq depth (+ depth 1)))
             (when (and next (not (eql next #\|)))
               (unread-char next stream))))
          ((eql ch #\|)
           (let ((next (read-char stream nil nil t)))
             (when (and next (eql next #\#))
               (setq depth (- depth 1)))
             (when (and next (not (eql next #\#)))
               (unread-char next stream)))))))))

(defun %read-feature (stream include-if-present)
  "Read #+feature or #-feature.  CLHS 2.4.8.{17,18}: when the feature
   test does NOT match, the next form is skipped (consumed under
   *read-suppress*) and reading continues at the form after.  Returning
   (values) here would make `(read-from-string \"#-cl :good\")` return
   nil instead of recursing past the suppressed form.  When the test
   matches, just read and return the next form.

   CLHS 24.1.2.1.1: feature names in #+ / #- expressions are treated
   as belonging to the keyword package — `#+X' tests for `:X', not
   for whatever package `X' was read in.  We bind *package* to
   :keyword so the inner read interns feature names as keywords."
  (let* ((feature-expr (let ((*read-suppress* nil)
                             (*package* (find-package "KEYWORD")))
                         (%read-internal stream t nil t)))
         (present (%feature-present-p feature-expr)))
    (cond
      ((eq present include-if-present)
       (%read-internal stream t nil t))
      (t
       ;; Skip the next form under *read-suppress*, then recursively
       ;; read the form AFTER it — this is what makes
       ;; `#-X foo bar` return `bar`.
       (let ((*read-suppress* t))
         (%read-internal stream t nil t))
       (%read-internal stream t nil t)))))

(defun %feature-name-eq (a b)
  "Compare two feature designators per CLHS 24.1.2.1.1 / 26.1.2.
   With the keyword-package read of #+ / #- (see %read-feature) the
   LHS is always a keyword.  *features* may contain keywords or
   symbols.  Match by EQ when both sides are keywords; otherwise
   match when names agree only if BOTH sides belong to the keyword
   package — a non-keyword symbol in *features* (test sets
   *features* = '(X) with X interned in cl-test) does NOT satisfy
   #+X per CL semantics."
  (and (symbolp a) (symbolp b)
       (or (eq a b)
           (and (keywordp a) (keywordp b)
                (string= (symbol-name a) (symbol-name b))))))

(defun %feature-present-p (expr)
  "Check if a feature expression EXPR matches *features*.
   - Symbol/keyword: (member EXPR *features* :test name-eq).
   - (AND …): all subforms present.
   - (OR  …): any subform present.
   - (NOT x): x not present.
   Accepts both `:and`/`:or`/`:not` and `and`/`or`/`not` head symbols
   so source written with either keyword or symbol heads matches per
   the typical Lisp convention.  Always-on built-ins (`:common-lisp`,
   `:cl`, `:ansi-cl`) match without consulting *features* — they are
   the de-facto modus identity."
  (cond
    ((symbolp expr)
     (or (%feature-name-eq expr :common-lisp)
         (%feature-name-eq expr :cl)
         (%feature-name-eq expr :ansi-cl)
         (let ((cur *features*) (found nil))
           (loop (when (or found (null cur)) (return found))
             (when (%feature-name-eq expr (car cur)) (setq found t))
             (setq cur (cdr cur))))))
    ((and (consp expr)
          (symbolp (car expr))
          (or (%feature-name-eq (car expr) :and)
              (%feature-name-eq (car expr) 'and)))
     (let ((all t))
       (dolist (sub (cdr expr)) (unless (%feature-present-p sub) (setq all nil)))
       all))
    ((and (consp expr)
          (symbolp (car expr))
          (or (%feature-name-eq (car expr) :or)
              (%feature-name-eq (car expr) 'or)))
     (let ((any nil))
       (dolist (sub (cdr expr)) (when (%feature-present-p sub) (setq any t)))
       any))
    ((and (consp expr)
          (symbolp (car expr))
          (or (%feature-name-eq (car expr) :not)
              (%feature-name-eq (car expr) 'not)))
     (not (%feature-present-p (cadr expr))))
    (t nil)))

(defun %read-radix-integer (stream radix)
  "Read an integer or ratio in the given radix.
   Per CLHS 2.4.8.{3,4,5,6} #B / #O / #X / #nR may also read a ratio
   (e.g. `#B-1/10' → -1/2).  Try ratio N/D first; on failure fall back
   to integer."
  (let ((codes nil) (ch nil))
    (loop
      (setq ch (read-char stream nil nil t))
      (when (null ch) (return nil))
      (when (or (%whitespace-char-p ch) (%terminating-macro-p ch *readtable*))
        (unread-char ch stream)
        (return nil))
      (setq codes (cons (char-code ch) codes)))
    (if *read-suppress* nil
        (let ((forward (nreverse codes)))
          ;; Try ratio first.
          (let ((ratio (%try-parse-ratio-radix forward radix)))
            (if ratio ratio
                (let ((num (%try-parse-integer forward radix)))
                  (if num (car num)
                      (%reader-error "invalid radix integer")))))))))

(defun %try-parse-ratio-radix (codes radix)
  "Try to parse char codes as a ratio N/D in the given RADIX.
   Returns the ratio value or nil.  Mirrors %try-parse-ratio but uses
   RADIX instead of *read-base*."
  (let ((slash-pos nil) (i 0) (cur codes))
    (loop
      (when (null cur) (return nil))
      (when (= (car cur) 47)  ; /
        (when slash-pos (return-from %try-parse-ratio-radix nil))
        (setq slash-pos i))
      (setq i (+ i 1))
      (setq cur (cdr cur)))
    (unless slash-pos (return-from %try-parse-ratio-radix nil))
    (when (= slash-pos 0) (return-from %try-parse-ratio-radix nil))
    (let ((num-codes nil) (den-codes nil) (j 0) (cur codes))
      (loop
        (when (null cur) (return nil))
        (if (< j slash-pos)
            (setq num-codes (cons (car cur) num-codes))
            (when (> j slash-pos)
              (setq den-codes (cons (car cur) den-codes))))
        (setq j (+ j 1))
        (setq cur (cdr cur)))
      (setq num-codes (nreverse num-codes))
      (setq den-codes (nreverse den-codes))
      (let ((num (%try-parse-integer num-codes radix))
            (den (%try-parse-integer den-codes radix)))
        (if (and num den (not (= (car den) 0)))
            (exact-divide (car num) (car den))
            nil)))))

(defun %read-bit-vector (stream len)
  "Read #*bits as a bit vector.  Per CLHS 2.4.8.4: if LEN > actual
   length, the LAST supplied bit is repeated to fill.  When
   *read-suppress* is T, any character is consumed without error
   and the result is NIL."
  (let ((bits nil) (ch nil))
    (loop
      (setq ch (read-char stream nil nil t))
      (when (null ch) (return nil))
      (when (or (%whitespace-char-p ch) (%terminating-macro-p ch *readtable*))
        (unread-char ch stream)
        (return nil))
      (cond
        ;; Suppress mode: consume any character without recording or
        ;; signaling.  This matches CLHS 2.4.4.5 — the entire body of
        ;; a suppressed read is parsed permissively.
        (*read-suppress* nil)
        ((eql ch #\0) (setq bits (cons 0 bits)))
        ((eql ch #\1) (setq bits (cons 1 bits)))
        (t (%reader-error "invalid bit vector character"))))
    (if *read-suppress* nil
        (let ((bit-list (nreverse bits)))
          (let ((actual-len (list-length bit-list)))
            (let ((vec-len (if len len actual-len)))
              ;; CLHS: when LEN > supplied count, the LAST bit repeats.
              ;; (CLHS 2.4.8.4 — "the last bit ... is used to fill".)
              ;; When no bits were supplied at all and LEN > 0, the
              ;; behavior is implementation-defined; fill with 0.
              (let ((fill-bit (if bit-list
                                  (car (last bit-list))
                                  0))
                    (v (make-array vec-len))
                    (i 0) (cur bit-list))
                (loop
                  (when (>= i vec-len) (return nil))
                  (if cur
                      (progn (aset v i (car cur)) (setq cur (cdr cur)))
                      (aset v i fill-bit))
                  (setq i (+ i 1)))
                v)))))))

(defun %read-user-dispatch (disp-char stream rt)
  "Handle user-installed dispatch macro character."
  ;; Read numeric arg
  (let ((sub-ch (read-char stream t nil t))
        (arg nil))
    (when (and (characterp sub-ch) (digit-char-p sub-ch))
      (setq arg 0)
      (loop
        (let ((d (digit-char-p sub-ch)))
          (if d
              (progn (setq arg (+ (* arg 10) d))
                     (setq sub-ch (read-char stream t nil t)))
              (return nil)))))
    ;; Look up function
    (let ((sub-table (%get-dispatch-table disp-char rt)))
      (if sub-table
          (let ((code (if (characterp sub-ch) (char-code (char-upcase sub-ch)) 0)))
            (let ((fn (if (< code 128) (aref sub-table code) nil)))
              (if fn
                  (funcall fn stream sub-ch arg)
                  (%reader-error "unknown dispatch sub-character"))))
          (%reader-error "not a dispatch macro character")))))

;;; --- Public reader API ---

(defun read (&rest args)
  "Read one Lisp object from STREAM."
  (let ((stream (if args (car args) nil))
        (eof-error-p (if (cdr args) (cadr args) t))
        (eof-value (if (cddr args) (caddr args) nil))
        (recursive-p (if (cdddr args) (cadddr args) nil)))
    (let ((s (%resolve-input-stream stream)))
      (if recursive-p
          (%read-internal s eof-error-p eof-value recursive-p)
          (let ((*sharp-labels* nil))
            (%read-internal s eof-error-p eof-value recursive-p))))))

(defun read-preserving-whitespace (&rest args)
  "Read one Lisp object, preserving trailing whitespace."
  (let ((stream (if args (car args) nil))
        (eof-error-p (if (cdr args) (cadr args) t))
        (eof-value (if (cddr args) (caddr args) nil))
        (recursive-p (if (cdddr args) (cadddr args) nil)))
    (let ((s (%resolve-input-stream stream)))
      (if recursive-p
          (%read-internal s eof-error-p eof-value recursive-p)
          (let ((*sharp-labels* nil))
            (%read-internal s eof-error-p eof-value recursive-p))))))

(defun read-delimited-list (char &rest args)
  "Read objects until CHAR is found. Returns a list."
  (let ((stream (if args (car args) nil))
        (recursive-p (if (cdr args) (cadr args) nil)))
    (let ((s (%resolve-input-stream stream))
          (result nil))
      (loop
        ;; Skip whitespace
        (let ((ch nil))
          (loop
            (setq ch (read-char s nil nil t))
            (when (null ch) (%reader-error "end of file in read-delimited-list"))
            (unless (%whitespace-char-p ch) (return nil)))
          (when (eql ch char)
            (return (nreverse result)))
          ;; Read an object
          (unread-char ch s)
          (let ((obj (%read-internal s t nil t)))
            (setq result (cons obj result))))))))

(defun read-from-string (str &rest args)
  "Read one Lisp object from STR."
  (let ((eof-error-p (if args (car args) t))
        (eof-value (if (cdr args) (cadr args) nil))
        (kwargs (cddr args)))
    ;; Parse keyword args
    (let ((start 0) (end nil) (preserve-whitespace nil)
          (allow-other-keys nil)
          (bad-key nil))
      ;; First pass: determine the effective allow-other-keys.  CLHS
      ;; 3.4.1.4.1.1: "value of :ALLOW-OTHER-KEYS is determined by the
      ;; LEFTMOST occurrence … in the property list".  We bind aok-seen
      ;; on first sight so a later :allow-other-keys t can't override
      ;; an earlier :allow-other-keys nil.
      (let ((kw-cur kwargs)
            (aok-seen nil))
        (loop
          (when (null kw-cur) (return nil))
          (when (and (not aok-seen) (eq (car kw-cur) :allow-other-keys))
            (setq aok-seen t)
            (when (cadr kw-cur) (setq allow-other-keys t)))
          (setq kw-cur (cddr kw-cur))))
      ;; Second pass: assign known kwargs and remember the first bad
      ;; key (signalled at the end if allow-other-keys is still nil).
      (let ((kw-cur kwargs))
        (loop
          (when (null kw-cur) (return nil))
          ;; Odd-length kwarg list — value missing.  CLHS 3.4.1.4.1:
          ;; "if the number of keyword arguments and values is not
          ;; equal" → program-error.  Detect (null (cdr kw-cur)) before
          ;; (cadr) faults so we signal cleanly.
          (when (null (cdr kw-cur))
            (%signal-program-error)
            (return nil))
          (let ((key (car kw-cur))
                (val (cadr kw-cur)))
            (cond
              ((eq key :start) (setq start val))
              ((eq key :end) (setq end val))
              ((eq key :preserve-whitespace) (setq preserve-whitespace val))
              ((eq key :allow-other-keys) nil)
              (t (when (null bad-key) (setq bad-key key)))))
          (setq kw-cur (cddr kw-cur))))
      ;; If a bad keyword appeared and :allow-other-keys was not
      ;; supplied (or was supplied as nil), signal program-error.
      (when (and bad-key (not allow-other-keys))
        (%signal-program-error))
      ;; Handle start/end
      (let ((actual-str (if (or (> start 0) end)
                            (%substring str start (if end end (length str)))
                            str)))
        ;; Create string input stream
        (let ((s (make-string-input-stream actual-str)))
          ;; Read from stream — fresh #N= label table per top-level call
          (let* ((*sharp-labels* nil)
                 (result (%read-internal s eof-error-p eof-value nil)))
            ;; Get position
            (let ((pos (car (cdr (%stream-data s)))))
              (values result (+ start pos)))))))))

;;; --- Make-symbol (uninterned) ---

(defun make-symbol (name)
  "Create an uninterned symbol with NAME."
  (let ((sym (%make-cl-symbol (%pkg-string-designator name))))
    sym))

;;; --- with-standard-io-syntax support ---
;;; This is a function that evaluates a thunk with standard I/O bindings.
;;; The SBCL-side rewriter transforms (with-standard-io-syntax body) into
;;; (%with-standard-io-syntax (lambda () body))

(defun %with-standard-io-syntax (thunk)
  "Execute THUNK with standard I/O syntax bindings."
  (let ((saved-package *package*)
        (saved-readtable *readtable*)
        (saved-read-base *read-base*)
        (saved-read-suppress *read-suppress*)
        (saved-read-eval *read-eval*)
        (saved-print-base *print-base*)
        (saved-print-case *print-case*)
        (saved-print-escape *print-escape*)
        (saved-print-gensym *print-gensym*)
        (saved-print-length *print-length*)
        (saved-print-level *print-level*)
        (saved-print-readably *print-readably*)
        (saved-print-array *print-array*)
        (saved-print-circle *print-circle*)
        (saved-print-lines *print-lines*)
        (saved-print-pretty *print-pretty*)
        (saved-print-radix *print-radix*)
        (saved-print-right-margin *print-right-margin*)
        (saved-print-miser-width *print-miser-width*))
    (setq *package* (find-package "CL-USER"))
    (setq *readtable* (copy-readtable nil))
    (setq *read-base* 10)
    (setq *read-suppress* nil)
    (setq *read-eval* t)
    (setq *print-array* t)
    (setq *print-base* 10)
    (setq *print-case* :upcase)
    (setq *print-circle* nil)
    (setq *print-escape* t)
    (setq *print-gensym* t)
    (setq *print-length* nil)
    (setq *print-level* nil)
    (setq *print-lines* nil)
    (setq *print-miser-width* nil)
    (setq *print-pretty* nil)
    (setq *print-radix* nil)
    (setq *print-readably* nil)
    (setq *print-right-margin* nil)
    ;; Preserve all return values from THUNK via MULTIPLE-VALUE-PROG1
    ;; (read-from-string returns 2 values; capturing into single var
    ;; would lose the position value and break tests using
    ;; multiple-value-list around the wrap).
    (multiple-value-prog1
        (handler-case (funcall thunk)
          (error (c)
            (setq *package* saved-package)
            (setq *readtable* saved-readtable)
            (setq *read-base* saved-read-base)
            (setq *read-suppress* saved-read-suppress)
            (setq *read-eval* saved-read-eval)
            (setq *print-base* saved-print-base)
            (setq *print-case* saved-print-case)
            (setq *print-escape* saved-print-escape)
            (setq *print-gensym* saved-print-gensym)
            (setq *print-length* saved-print-length)
            (setq *print-level* saved-print-level)
            (setq *print-readably* saved-print-readably)
            (setq *print-array* saved-print-array)
            (setq *print-circle* saved-print-circle)
            (setq *print-lines* saved-print-lines)
            (setq *print-pretty* saved-print-pretty)
            (setq *print-radix* saved-print-radix)
            (setq *print-right-margin* saved-print-right-margin)
            (setq *print-miser-width* saved-print-miser-width)
            (error c)))
      (setq *package* saved-package)
      (setq *readtable* saved-readtable)
      (setq *read-base* saved-read-base)
      (setq *read-suppress* saved-read-suppress)
      (setq *read-eval* saved-read-eval)
      (setq *print-base* saved-print-base)
      (setq *print-case* saved-print-case)
      (setq *print-escape* saved-print-escape)
      (setq *print-gensym* saved-print-gensym)
      (setq *print-length* saved-print-length)
      (setq *print-level* saved-print-level)
      (setq *print-readably* saved-print-readably)
      (setq *print-array* saved-print-array)
      (setq *print-circle* saved-print-circle)
      (setq *print-lines* saved-print-lines)
      (setq *print-pretty* saved-print-pretty)
      (setq *print-radix* saved-print-radix)
      (setq *print-right-margin* saved-print-right-margin)
      (setq *print-miser-width* saved-print-miser-width))))

;; FIND-CLASS lives in cl-conditions.lisp with the real implementation
;; that consults %find-clos-class + the built-in class proxy table.
;; The reader-stub here unconditionally errored, shadowing the real defun
;; only if the load order ever flipped — keep the comment so future
;; refactors don't re-introduce a stub.
