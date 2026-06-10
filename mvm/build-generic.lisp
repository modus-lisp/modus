;;;; build-generic.lisp — minimal Modus image that LOADs argv[1].
;;;;
;;;; The premise: "this is the whole point of Lisp."  Build a Modus
;;;; image with the full CL runtime but NO baked-in tests.  At boot,
;;;; read a filename from argv[1], LOAD it, exit.  All test runners,
;;;; test files, REPLs, etc. become runtime-loaded source rather than
;;;; build-time-baked code.
;;;;
;;;; Usage:
;;;;   sbcl --script mvm/build-generic.lisp   # → /tmp/modus
;;;;   /tmp/modus path/to/script.lisp         # boot, LOAD it, exit
;;;;
;;;; The build cycle is dramatically shorter than build-ansi-test.lisp
;;;; because we don't read 700+ test .lsp files at SBCL time and emit
;;;; rewritten run-test forms for each.  A typical full rebuild is on
;;;; the order of ~30s instead of ~4-5 minutes.

;;; ============================================================
;;; 1. Load MVM infrastructure (SBCL-side)
;;; ============================================================

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(mvm-load "mvm/repl-source.lisp")

(format t "~%=== Building generic Modus image ===~%")

;;; ============================================================
;;; 2. Read source files as text (SBCL-side)
;;; ============================================================

(defun read-file-text (path)
  (with-open-file (s path :direction :input)
    (let ((text (make-string (file-length s))))
      (subseq text 0 (read-sequence text s)))))

(defun mvm-text (relative-path)
  (let ((path (merge-pathnames relative-path *modus-base*)))
    (modus.mvm::check-parses path)
    (read-file-text path)))

(format t "Reading source files...~%")

(defvar *prelude-source*  (mvm-text "mvm/prelude.lisp"))
(defvar *gc-source*       (mvm-text "mvm/gc.lisp"))
(defvar *rt-source*       (mvm-text "mvm/rt.lisp"))
(defvar *rt-macros-source* (mvm-text "mvm/runtime-cl-macros.lisp"))
(defvar *bridge-source*
  (concatenate 'string
    (mvm-text "mvm/cl-sequences.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-streams.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-fileio.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-printer.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-reader.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-eval.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-clos.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-types.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-packages.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-conditions.lisp")
    (string #\Newline)
    (mvm-text "mvm/ansi-bridge.lisp")))

(format t "  prelude: ~D chars~%" (length *prelude-source*))
(format t "  gc:      ~D chars~%" (length *gc-source*))
(format t "  rt:      ~D chars~%" (length *rt-source*))
(format t "  bridge:  ~D chars~%" (length *bridge-source*))

;;; ============================================================
;;; 3. Build-time scanners (same as build-ansi-test) so runtime LOAD
;;; can find every defun's source — without these the symbol-function
;;; table only knows the ~229 hand-curated entries from
;;; cl-eval.lisp's %init-sft-list and runtime-EVAL of any other name
;;; resolves to %%unresolved-fn.
;;; ============================================================

(format t "Generating sft-auto / sym-name-auto / runtime-macros init...~%")

;; sft-auto: collect every (defun NAME ...) form's NAME across all
;; runtime source and emit (puthash "NAME" *symbol-function-table*
;; #'NAME) calls so the runtime can resolve any defun by name.

(defun scan-defuns (text)
  "Return list of defun names (strings) found in TEXT."
  (let ((names nil)
        (pos 0))
    (loop
      (let ((p (search "(defun " text :start2 pos)))
        (unless p (return (nreverse names)))
        (let* ((start (+ p 7))
               (end (or (position-if (lambda (c)
                                       (or (char= c #\Space)
                                           (char= c #\Newline)
                                           (char= c #\()
                                           (char= c #\)))) text
                                     :start start)
                        (length text))))
          (push (string-upcase (subseq text start end)) names)
          (setq pos end))))))

;; Driver source is concatenated into the image AND scanned for
;; defun / symbol names, so defuns added in the driver (sys-exit,
;; %argv1, runtime-bq-expand, etc.) land in *symbol-function-table*
;; just like the CL runtime ones.  Forward-declared here; the actual
;; content lives below (section 4).  See the trailing setf-via-symbol-
;; value-set trick to overwrite this without re-reading the file.
(defvar *driver-source* "
(defun sys-exit (code)
  (let ((c code))
    (syscall3 60 c 0 0)))
(defun halt ()
  (syscall3 60 1 0 0))
(defun %rbq-sym-name-eq (sym name)
  (and (symbolp sym) (string= (symbol-name sym) name)))
;; Level-tracking backquote expander.  LEVEL counts open backquotes
;; whose commas are still pending; the entry from the macro is LEVEL 1.
;; A COMMA at LEVEL 1 unquotes (its expr stays live); a COMMA at deeper
;; LEVEL is data that drops one level — this is what makes the classic
;; `,',x / `,,x tunnelling work (a `(... `(... ,',def ...)) form).  A
;; nested BACKQUOTE bumps the level by one for its template.
(defun runtime-bq-expand (template) (%rbq template 1))
(defun %rbq (template level)
  (cond
    ((null template) nil)
    ((atom template) (list 'quote template))
    ;; nested COMMA
    ((%rbq-sym-name-eq (car template) \"COMMA\")
     (if (= level 1)
         (cadr template)
         (list 'list (list 'quote 'comma) (%rbq (cadr template) (- level 1)))))
    ((%rbq-sym-name-eq (car template) \"COMMA-AT\")
     (if (= level 1)
         (cadr template)
         (list 'list (list 'quote 'comma-at) (%rbq (cadr template) (- level 1)))))
    ((%rbq-sym-name-eq (car template) \"COMMA-DOT\")
     (if (= level 1)
         (cadr template)
         (list 'list (list 'quote 'comma-dot) (%rbq (cadr template) (- level 1)))))
    ;; nested BACKQUOTE — descend one deeper level, rebuild the marker
    ((%rbq-sym-name-eq (car template) \"BACKQUOTE\")
     (list 'list (list 'quote 'backquote) (%rbq (cadr template) (+ level 1))))
    (t (%rbq-list template level))))
(defun %rbq-list (lst level)
  (cond
    ((null lst) (list 'quote nil))
    ((not (consp lst)) (%rbq lst level))
    ;; a dotted/atom whole-form COMMA tail like `(a . ,b)
    ((%rbq-sym-name-eq (car lst) \"COMMA\")
     (if (= level 1)
         (cadr lst)
         (%rbq lst level)))
    (t
     (let ((first (car lst)) (rest (cdr lst)))
       (cond
         ((and (consp first) (%rbq-sym-name-eq (car first) \"COMMA-AT\") (= level 1))
          (list 'append2 (cadr first) (%rbq-list rest level)))
         ((and (consp first) (%rbq-sym-name-eq (car first) \"COMMA-DOT\") (= level 1))
          (list 'append2 (cadr first) (%rbq-list rest level)))
         (t
          (list 'cons (%rbq first level)
                (%rbq-list rest level))))))))
(defun %install-runtime-backquote ()
  (set-macro-function 'backquote
                      (eval '(lambda (template) (runtime-bq-expand template)))))
(defun %argv-string-at (addr)
  (let ((len 0))
    (let ((i 0))
      (loop
        (let ((b (mem-ref (+ addr i) :u8)))
          (when (= b 0) (return nil))
          (setq i (+ i 1)))
        (setq len i)))
    (if (zerop len) nil
        (let ((s (%make-string-array len)) (i 0))
          (loop
            (when (>= i len) (return s))
            (aset s i (mem-ref (+ addr i) :u8))
            (setq i (+ i 1)))))))
(defun %argv1 () (%argv-string-at #x10000208))
(defun %argv2 () (%argv-string-at #x10000248))
(defun %argc  () (mem-ref #x10000200 :u32))
(defun kernel-main ()
  (init-symbol-table)
  (init-keyword-table)
  (%init-packages)
  (%init-streams)
  (%init-reader)
  (%init-condition-types)
  (%init-method-combinations)
  (%init-symbol-function-table)
  (%init-sft-auto)
  (setq *sym-name-table* (make-hash-table))
  (%init-sym-name-auto)
  (setq *macro-table* (make-hash-table))
  (%init-runtime-macros)
  (setq *cstr-scratch* #x0FE00000)  ; moved below heap base
  (setq *io-buf-addr*  #x0FF00000)  ; moved out of heap semispace 0; see memory note
  (%init-signal-handling)
  (%init-signal-symbols)
  (%init-make-load-form)
  (%install-runtime-backquote)
  ;; Init RT counters + registry (defvar init thunks don't run on bare metal)
  (setq *rt-test-count* 0)
  (setq *rt-pass-count* 0)
  (setq *rt-fail-count* 0)
  (setq *rt-registered-tests* nil)
  (%install-deftest-macro)
  ;; Run all built-in defvar init thunks.  Each is wrapped in
  ;; handler-case at compile time so a thunk that references a not-yet-
  ;; bound symbol can't kill the chain — see CLAUDE.md known limitation
  ;; #7 history.  Most thunks succeed and we get init values for free.
  (init-all-globals)
  ;; AFTER init-all-globals — overrides defvar's init.  *write-object-
  ;; budget* defvars to 0 which immediately exhausts; we want a huge
  ;; budget so test names print fully.
  (setq *write-object-budget* 1000000)
  ;; Runtime CL macros (when/unless/setf/incf/case/dolist/etc.) — must
  ;; come AFTER init-all-globals so *modus-runtime-macros* has its
  ;; defvar value before we walk it.  Outer handler-case in case a
  ;; macro source string fails to parse (the install fn itself doesn't
  ;; wrap — see %install-runtime-cl-macros docstring).
  (handler-case (%install-runtime-cl-macros) (t (c) nil))
  (let ((path (%argv1)))
    (cond
      ((null path)
       (write-string-serial \"usage: modus <script.lisp>\")
       (write-char-serial 10)
       (sys-exit 1))
      (t
       (handler-case
           (progn (load path) (sys-exit 0))
         (t (c)
            (write-string-serial \"unhandled condition while loading: \")
            (handler-case (write-string-serial path) (t (c) nil))
            (write-char-serial 10)
            (sys-exit 1)))))))
")

(defvar *all-runtime-source*
  (concatenate 'string *prelude-source*  (string #\Newline)
                       *gc-source*       (string #\Newline)
                       *rt-source*       (string #\Newline)
                       *rt-macros-source* (string #\Newline)
                       *bridge-source*   (string #\Newline)
                       *driver-source*))

(defvar *all-defun-names*
  ;; Filter to names that look like valid CL identifiers.
  (remove-if-not (lambda (s)
                   (and (stringp s)
                        (> (length s) 0)
                        (every (lambda (c)
                                 (or (alphanumericp c)
                                     (find c "+-*/<=>?!@$%^&_:|.~")))
                               s)))
                 (scan-defuns *all-runtime-source*)))

(format t "  defuns found: ~D~%" (length *all-defun-names*))

;; Emit %init-sft-auto in chunks of ~200 to avoid the compiler's
;; function-size limits.

(defun emit-sft-auto (names chunk-size)
  (with-output-to-string (out)
    (let ((n-chunks (ceiling (length names) chunk-size)))
      (dotimes (c n-chunks)
        (format out "(defun %init-sft-auto-~D ()~%" c)
        (let ((start (* c chunk-size))
              (end (min (* (1+ c) chunk-size) (length names))))
          (loop for i from start below end
                do (format out "  (puthash ~S *symbol-function-table* #'~A)~%"
                          (nth i names) (nth i names))))
        (format out ")~%"))
      (format out "(defun %init-sft-auto ()~%")
      (dotimes (c n-chunks)
        (format out "  (%init-sft-auto-~D)~%" c))
      (format out ")~%"))))

(defvar *sft-auto-source* (emit-sft-auto *all-defun-names* 200))
(format t "  sft-auto: ~D chars~%" (length *sft-auto-source*))

;; sym-name-auto: collect every symbol NAME mentioned in source so
;; runtime symbol-name can recover the name for a native MVM symbol.

(defun scan-symbol-names (text)
  "Return list of distinct symbol-shaped tokens in TEXT."
  (let ((seen (make-hash-table :test 'equal))
        (result nil)
        (pos 0)
        (len (length text)))
    (loop
      (when (>= pos len) (return (nreverse result)))
      (let ((c (char text pos)))
        (cond
          ((or (char= c #\Space) (char= c #\Newline) (char= c #\Tab)
               (char= c #\Return) (char= c #\Page))
           (incf pos))
          ((char= c #\;)
           ;; comment to end of line
           (loop while (and (< pos len)
                            (not (char= (char text pos) #\Newline)))
                 do (incf pos)))
          ((char= c #\")
           ;; string literal — skip to closing quote
           (incf pos)
           (loop while (and (< pos len) (not (char= (char text pos) #\")))
                 do (when (char= (char text pos) #\\) (incf pos))
                    (incf pos))
           (incf pos))
          ;; Sharp dispatch: #\X character literal, #| ... |# block
          ;; comment, #(...) vector literal, #+/#- feature, etc.  Without
          ;; this special-case, #\" / #\; / #\( etc. fool the string- and
          ;; comment-skippers in the branches above and the scanner ends
          ;; up consuming dozens of legitimate tokens as if they were
          ;; inside a string.  In particular COMMA-AT on line 992 of
          ;; cl-reader.lisp was being eaten by the bogus string-skip
          ;; triggered by #\" on line 957.
          ((char= c #\#)
           (incf pos)
           (when (< pos len)
             (let ((next (char text pos)))
               (cond
                 ;; #\X — skip the backslash + next char (which may be
                 ;; any single character) + any trailing word (e.g.
                 ;; #\Newline / #\Space).
                 ((char= next #\\)
                  (incf pos)  ; past the backslash
                  (when (< pos len) (incf pos))  ; the literal char
                  ;; Multi-char names like Newline / Space / Tab — eat
                  ;; the rest of the word.
                  (loop while (and (< pos len)
                                   (alphanumericp (char text pos)))
                        do (incf pos)))
                 ;; #| ... |# block comment
                 ((char= next #\|)
                  (incf pos)
                  (loop while (and (< (1+ pos) len)
                                   (not (and (char= (char text pos) #\|)
                                             (char= (char text (1+ pos)) #\#))))
                        do (incf pos))
                  (when (< (1+ pos) len) (incf pos) (incf pos))))))
           ;; Other # forms (#( vector, #+ feature, etc.) just fall
           ;; through — the next iteration reads them as ordinary
           ;; tokens.
           )
          ((or (char= c #\() (char= c #\)) (char= c #\') (char= c #\`)
               (char= c #\,))
           (incf pos))
          (t
           ;; symbol token
           (let ((start pos))
             (loop while (and (< pos len)
                              (let ((ch (char text pos)))
                                (not (or (char= ch #\Space) (char= ch #\Newline)
                                         (char= ch #\() (char= ch #\))
                                         (char= ch #\Tab) (char= ch #\")
                                         (char= ch #\;)))))
                   do (incf pos))
             (let ((token (string-upcase (subseq text start pos))))
               (unless (or (gethash token seen)
                           (zerop (length token))
                           (every #'digit-char-p token))
                 (setf (gethash token seen) t)
                 (push token result))
               ;; Also register a keyword token's bare name (without the
               ;; leading colon).  compile-keyword uses (normalize-name
               ;; KW) → hash of "FOO" (not ":FOO"); symbol-name at
               ;; runtime looks up that hash in *sym-name-table*.
               ;; Without this, keywords interned at build-time print
               ;; as :|| because the colon-prefixed entry doesn't match.
               (when (and (> (length token) 1)
                          (char= (char token 0) #\:))
                 (let ((bare (subseq token 1)))
                   (unless (or (gethash bare seen)
                               (zerop (length bare)))
                     (setf (gethash bare seen) t)
                     (push bare result))))))))))))

(defvar *all-symbol-names* (scan-symbol-names *all-runtime-source*))

(format t "  symbol names found: ~D~%" (length *all-symbol-names*))

(defun emit-sym-name-auto (names chunk-size)
  (with-output-to-string (out)
    (let ((n-chunks (ceiling (length names) chunk-size)))
      (dotimes (c n-chunks)
        (format out "(defun %init-sym-name-auto-~D ()~%" c)
        (let ((start (* c chunk-size))
              (end (min (* (1+ c) chunk-size) (length names))))
          (loop for i from start below end
                do (format out "  (puthash (compute-name-hash ~S) *sym-name-table* ~S)~%"
                          (nth i names) (nth i names))))
        (format out ")~%"))
      (format out "(defun %init-sym-name-auto ()~%")
      (dotimes (c n-chunks)
        (format out "  (%init-sym-name-auto-~D)~%" c))
      (format out ")~%"))))

(defvar *sym-name-auto-source* (emit-sym-name-auto *all-symbol-names* 200))

;; Macro table — extract every (mvm-define-macro NAME ...) from
;; compiler.lisp at build time so runtime macroexpand-1 can see them.

(defun scan-mvm-define-macro-forms (text)
  "Find (mvm-define-macro \"NAME\" ...) forms and return their names."
  (let ((names nil)
        (pos 0))
    (loop
      (let ((p (search "(mvm-define-macro \"" text :start2 pos)))
        (unless p (return (nreverse names)))
        (let* ((start (+ p (length "(mvm-define-macro \"")))
               (end (position #\" text :start start)))
          (push (subseq text start end) names)
          (setq pos (1+ end)))))))

(defvar *compiler-source* (mvm-text "mvm/compiler.lisp"))
(defvar *macro-names* (scan-mvm-define-macro-forms *compiler-source*))
(format t "  macro names found: ~D~%" (length *macro-names*))

;; Emit %init-runtime-macros — register each macro NAME → a marker
;; (T) in *macro-table*.  This makes macroexpand-1 report "yes I know
;; about that macro" without actually expanding it.  Real expansion
;; (for runtime EVAL of macro forms) requires the actual lambdas which
;; we can't ship as easily; for now mark them as known.
(defvar *runtime-macros-source*
  (with-output-to-string (out)
    (format out "(defun %init-runtime-macros ()~%")
    (dolist (name *macro-names*)
      (format out "  (puthash (compute-name-hash ~S) *macro-table* t)~%" name))
    (format out ")~%")))

;;; ============================================================
;;; 4. Driver — moved to forward-declaration above so the scanner
;;; picks up its defuns (sys-exit / runtime-bq-expand / etc.).
;;; ============================================================

;;; ============================================================
;;; 5. Assemble *full-source*
;;; ============================================================

(format t "Assembling full source...~%")

(defvar *full-source*
  (concatenate 'string
    *prelude-source*
    (string #\Newline)
    *gc-source*
    (string #\Newline)
    *rt-source*
    (string #\Newline)
    *rt-macros-source*
    (string #\Newline)
    *bridge-source*
    (string #\Newline)
    ;; Defvar for *macro-table* — compiler.lisp's defvar isn't here
    ;; since we don't ship the compiler, but runtime macroexpand-1
    ;; references it.
    "(defvar *macro-table* (make-hash-table))
(defvar *sym-name-table* nil)
"
    (string #\Newline)
    *sft-auto-source*
    (string #\Newline)
    *sym-name-auto-source*
    (string #\Newline)
    *runtime-macros-source*
    (string #\Newline)
    *driver-source*))

(format t "Full source: ~D characters~%" (length *full-source*))

;;; ============================================================
;;; 6. Build the image
;;; ============================================================

(mvm-load "boot/boot-linux-x64.lisp")

(in-package :modus.mvm)

(funcall (intern "INSTALL-X64-TRANSLATOR" "MODUS.MVM.X64"))
(setf modus.mvm.x64::*x64-linux-mode* t)
;; Boot preamble for linux-x64 ends 397 bytes into the file (ELF header
;; + entry stub).  Native code starts there, so the fn-entry alignment
;; loop must account for this offset — otherwise `:li-func` + OR-3 +
;; CALL-IND's sub-3 lands one byte before the prologue.  When the
;; preceding function's last byte happens to be RET (0xC3), the
;; misaligned call returns immediately, leaving the caller's RAX
;; intact (silently looks like the fn returned T or whatever else
;; was in RAX).  See reference_append_funcall_bug.md.
(setf modus.mvm.x64::*x64-native-code-offset* 397)

;; Enable the GC trampoline: without this, every :alloc-obj advances R12
;; unchecked and the heap walks past the mapped region in long-running
;; sessions.  build-ansi-test / build-x64-modus-ansi-test set this; we
;; need it too so the generic image survives ANSI sweeps.
(setf modus.mvm.x64::*x64-gc-enabled* t)
;; Bring R14 to the heap midpoint so GC actually fires before the
;; from-space is exhausted.  Default leaves R14 at full heap end which
;; means the gc-check `cmp r12, r14; jl skip` only triggers after
;; allocation walked all the way to the end — too late for a Cheney
;; copy that needs the other half free.
(setf modus.mvm::*linux-x64-r14-offset* modus.mvm::+linux-x64-gc-midpoint+)

(format t "~%Compiling generic image (~D chars)...~%"
        (length cl-user::*full-source*))

(let ((image (build-image :target :linux-x64
                          :source-text cl-user::*full-source*)))
  (let ((path (or #+sbcl (sb-ext:posix-getenv "MODUS_GENERIC_OUT") "/tmp/modus")))
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (format t "~%Wrote ~D bytes to ~A~%"
            (length (kernel-image-image-bytes image)) path)
    (format t "Usage: ~A <script.lisp>~%" path)))
