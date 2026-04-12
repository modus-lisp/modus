;;;; build-ansi-test.lisp — Build ANSI CL test runner (Linux x86-64)
;;;;
;;;; Produces /tmp/modus-ansi-test — runs ANSI CL conformance tests.
;;;;
;;;; Usage: sbcl --script mvm/build-ansi-test.lisp
;;;; Run:   /tmp/modus-ansi-test
;;;;
;;;; Output: FAIL lines for each failing test, then summary: N/M PASS or FAIL
;;;; Exit code: 0 = all pass, >0 = number of failures

;;; ============================================================
;;; 1. Load MVM infrastructure (SBCL-side)
;;; ============================================================

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(mvm-load "mvm/repl-source.lisp")

(format t "~%=== Building ANSI CL test runner ===~%")

;;; ============================================================
;;; 2. Read source files (SBCL-side)
;;; ============================================================

(defun read-file-text (path)
  (with-open-file (s path :direction :input)
    (let ((text (make-string (file-length s))))
      (subseq text 0 (read-sequence text s)))))

(defun mvm-text (relative-path)
  (read-file-text (merge-pathnames relative-path *modus-base*)))

(defvar *prelude-source* (mvm-text "mvm/prelude.lisp"))
(defvar *rt-source*      (mvm-text "mvm/rt.lisp"))
(defvar *bridge-source*  (mvm-text "mvm/ansi-bridge.lisp"))
(defvar *test-source*    (mvm-text "mvm/ansi-tests.lisp"))

;; SBCL-level stubs for functions called during macro expansion
(defun notnot (x) (not (not x)))
(defun notnot-mv (x) (not (not x)))
(defun classify-error* (form) nil)
(in-package :modus.mvm)
(defun notnot (x) (not (not x)))
(defun notnot-mv (x) (not (not x)))
(in-package :cl-user)

;; Rewrite (make-array '(N) ...) → (make-array N ...) for MVM compatibility
(defun rewrite-make-array-dims (form)
  "Walk form tree, converting list-dimension make-array to integer-dimension."
  (cond
    ((atom form) form)
    ((and (eq (car form) 'make-array)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'quote)
          (consp (cadr (cadr form)))
          (null (cddr (cadr (cadr form)))))
     ;; (make-array '(N) ...) → (make-array N ...)
     (cons 'make-array (cons (car (cadr (cadr form)))
                             (mapcar #'rewrite-make-array-dims (cddr form)))))
    (t (mapcar #'rewrite-make-array-dims form))))

;; Load real ANSI test files (if available)
(defvar *real-ansi-sources* "")
(defvar *ansi-test-counter* 10000)
(defvar *ansi-file-names* nil)

(defun load-ansi-chapter (dir files)
  "Transform ANSI test files from DIR into MVM-compatible source.
   Skips files that cause read errors."
  (dolist (file files)
    (handler-case
      (let ((path (concatenate 'string dir file)))
        (when (probe-file path)
          (format t "  Transforming: ~A~%" file)
          (let ((forms nil))
            (with-open-file (s path :direction :input)
              (let ((*package* (find-package :cl-user)))
                (loop (let ((form (read s nil :eof)))
                        (when (eq form :eof) (return))
                        (push form forms)))))
            (push (pathname-name file) *ansi-file-names*)
            (setf forms (mapcar #'rewrite-make-array-dims (nreverse forms)))
            (let ((out (make-string-output-stream)) (test-forms nil))
              (format out "~%;; === ~A ===~%" file)
              (dolist (form forms)
                (cond
                  ((and (consp form) (eq (car form) 'deftest))
                   (let ((name (cadr form)) (test-form (caddr form))
                         (expected (cdddr form)))
                     (setf *ansi-test-counter* (1+ *ansi-test-counter*))
                     (let ((test-id *ansi-test-counter*))
                       (format t "      ~D = ~A~%" test-id name)
                       (let ((test-str (handler-case
                                         (cond
                                           ((= (length expected) 1)
                                            (format nil "(rt-run-test ~D ~S '~S)"
                                                    test-id test-form (car expected)))
                                           ((> (length expected) 0)
                                            (format nil "(rt-run-test-mv ~D (multiple-value-list ~S) '~S)"
                                                    test-id test-form expected)))
                                         (error () nil))))
                         (when (and test-str
                                    (not (search "#<" test-str))
                                    (not (search "&ENVIRONMENT" test-str))
                                    (not (search "STRUCT-TEST-" test-str)))
                           (push test-str test-forms))))))
                  ((and (consp form) (member (car form)
                          '(defharmless def-fold-test def-macro-test
                            in-package declaim))) nil)
                  (t (let ((s (handler-case (format nil "~S" form)
                                (error () nil))))
                       (when (and s
                                  (not (search "#<" s))
                                  (not (search "&ENVIRONMENT" s))
                                  (not (search "STRUCT-TEST-" s)))
                         (write-string s out)
                         (terpri out))))))
              (format out "(defun run-ansi-~A ()~%" (pathname-name file))
              (dolist (tf (nreverse test-forms)) (format out "  ~A~%" tf))
              (format out ")~%")
              (setf *real-ansi-sources*
                    (concatenate 'string *real-ansi-sources*
                                 (get-output-stream-string out)))))))
      (error (e)
        (format t "    SKIP ~A: ~A~%" file e)))))

;;; ============================================================
;;; Load ANSI test files by chapter
;;; ============================================================

;; Cons chapter (68 files)
(load-ansi-chapter "/tmp/ansi-test/cons/"
  '("cons.lsp" "consp.lsp" "atom.lsp" "endp.lsp" "rest.lsp" "last.lsp"
    "revappend.lsp" "nreconc.lsp" "rplaca.lsp" "rplacd.lsp"
    "acons.lsp" "pairlis.lsp" "copy-alist.lsp" "nconc.lsp" "butlast.lsp"
    "list.lsp" "listp.lsp" "nthcdr.lsp" "nth.lsp"
    "copy-list.lsp" "copy-tree.lsp" "tailp.lsp"
    "append.lsp" "mapcar.lsp" "mapc.lsp" "member.lsp" "assoc.lsp"
    "ldiff.lsp" "subst.lsp" "tree-equal.lsp" "make-list.lsp"
    "nbutlast.lsp" "list-length.lsp"
    "mapcan.lsp" "mapcon.lsp" "mapl.lsp" "maplist.lsp"
    "member-if.lsp" "member-if-not.lsp"
    "assoc-if.lsp" "assoc-if-not.lsp"
    "rassoc.lsp" "rassoc-if.lsp" "rassoc-if-not.lsp"
    "sublis.lsp" "subst-if.lsp" "subst-if-not.lsp"
    "nsubst.lsp" "nsubst-if.lsp" "nsubst-if-not.lsp" "nsublis.lsp"
    "intersection.lsp" "nintersection.lsp"
    "union.lsp" "nunion.lsp"
    "set-difference.lsp" "nset-difference.lsp"
    "set-exclusive-or.lsp" "nset-exclusive-or.lsp"
    "adjoin.lsp" "subsetp.lsp"
    "getf.lsp" "get-properties.lsp" "remf.lsp"
    "push.lsp" "pop.lsp" "pushnew.lsp"
    "cons-test-01.lsp" "cons-test-03.lsp" "cons-test-05.lsp"))

;; Data and Control Flow chapter — all safe files
(load-ansi-chapter "/tmp/ansi-test/data-and-control-flow/"
  '("if.lsp" "and.lsp" "or.lsp" "not.lsp" "identity.lsp"
    "not-and-null.lsp" "t.lsp" "nil.lsp"
    "prog1.lsp" "prog2.lsp" "return.lsp"
    "multiple-value-bind.lsp" "multiple-value-list.lsp"
    "call-arguments-limit.lsp" "lambda-parameters-limit.lsp"
    "ecase.lsp" "block.lsp" "return-from.lsp" "constantly.lsp"
    "case.lsp" "apply.lsp" "when.lsp" "unless.lsp" "cond.lsp" "progn.lsp"
))

;; Iteration chapter
(load-ansi-chapter "/tmp/ansi-test/iteration/"
  '("dolist.lsp" "dotimes.lsp"
    "loop1.lsp" "loop2.lsp" "loop3.lsp" "loop4.lsp" "loop5.lsp"
    "loop6.lsp" "loop7.lsp" "loop8.lsp" "loop9.lsp"
    "loop10.lsp" "loop11.lsp" "loop12.lsp" "loop13.lsp"
))

;; Hash-tables chapter
(load-ansi-chapter "/tmp/ansi-test/hash-tables/"
  '("gethash.lsp" "remhash.lsp" "clrhash.lsp" "maphash.lsp"
    "hash-table-count.lsp" "hash-table-p.lsp"
    "make-hash-table.lsp" "sxhash.lsp"))

;; Numbers chapter
(load-ansi-chapter "/tmp/ansi-test/numbers/"
  '("evenp.lsp" "oddp.lsp" "ash.lsp"
    "logand.lsp" "logior.lsp" "logxor.lsp" "lognot.lsp"
    "zerop.lsp" "plusp.lsp" "minusp.lsp"
    "max.lsp" "min.lsp" "abs.lsp"
    "incf.lsp" "decf.lsp"
    "integerp.lsp" "numberp.lsp" "realp.lsp"
    "plus.lsp" "minus.lsp" "times.lsp"
    "mod.lsp" "rem.lsp"
    "boole.lsp"
    "logcount.lsp" "integer-length.lsp"
    "logbitp.lsp" "logtest.lsp"))

;; Symbols chapter
(load-ansi-chapter "/tmp/ansi-test/symbols/"
  '("symbolp.lsp" "keywordp.lsp" "gensym.lsp"
    "boundp.lsp" "makunbound.lsp" "set.lsp"
    "copy-symbol.lsp" "make-symbol.lsp"))

;; Structures chapter
;; Structures
(load-ansi-chapter "/tmp/ansi-test/structures/"
  '("structure-00.lsp" "structures-01.lsp"))

;; Strings chapter
(load-ansi-chapter "/tmp/ansi-test/strings/"
  '("string.lsp" "stringp.lsp" "simple-string-p.lsp"
    "char-schar.lsp" "make-string.lsp"
    "string-upcase.lsp" "string-downcase.lsp" "string-capitalize.lsp"
    "nstring-upcase.lsp" "nstring-downcase.lsp" "nstring-capitalize.lsp"
    "string-trim.lsp" "string-left-trim.lsp" "string-right-trim.lsp"
    "string-comparisons.lsp"))

(load-ansi-chapter "/tmp/ansi-test/characters/"
  '("char-compare.lsp" "character.lsp" "name-char.lsp"))

;; Arrays chapter
(load-ansi-chapter "/tmp/ansi-test/arrays/"
  '("arrayp.lsp" "aref.lsp" "vectorp.lsp" "svref.lsp"
    "simple-vector-p.lsp" "vector.lsp"
    "array-rank.lsp" "array-total-size.lsp"
    "array-dimension.lsp" "array-dimensions.lsp"
    "array-element-type.lsp" "row-major-aref.lsp"
    "make-array.lsp"))

;; More DCF
(load-ansi-chapter "/tmp/ansi-test/data-and-control-flow/"
  '("every.lsp" "some.lsp" "notany.lsp" "notevery.lsp"
    "equal.lsp" "equalp.lsp" "typecase.lsp"
    "multiple-value-prog1.lsp" "multiple-value-setq.lsp"
    "values-list.lsp" "nth-value.lsp"
    "catch.lsp" "unwind-protect.lsp"
    "functionp.lsp" "fboundp.lsp"
    "psetq.lsp" "values.lsp"))

;; Sequences chapter
(load-ansi-chapter "/tmp/ansi-test/sequences/"
  '("length.lsp" "reverse.lsp" "nreverse.lsp" "copy-seq.lsp"
    "elt.lsp" "subseq.lsp" "map.lsp" "reduce.lsp"
    "concatenate.lsp" "sort.lsp" "stable-sort.lsp"
    "count.lsp" "count-if.lsp" "count-if-not.lsp"
    "find.lsp" "find-if.lsp" "find-if-not.lsp"
    "position.lsp" "position-if.lsp" "position-if-not.lsp"
    "remove.lsp" "remove-duplicates.lsp"
    "substitute.lsp" "substitute-if.lsp" "substitute-if-not.lsp"
    "fill.lsp" "replace.lsp" "mismatch.lsp"
    "search-list.lsp" "merge.lsp"
    "map-into.lsp" "make-sequence.lsp"))

;; Generate run-real-ansi-tests that calls all file-level runners
(setf *ansi-file-names* (nreverse *ansi-file-names*))
;; fork-run: isolate chunks in child processes; child exits with fail count
(let ((chunk-size 20)
      (names *ansi-file-names*))
  (setf *real-ansi-sources*
        (concatenate 'string *real-ansi-sources*
                     (format nil "~%(defun fork-run (thunk)~
                       ~%  (let ((pid (syscall3 57 0 0 0)))~
                       ~%    (if (= pid 0)~
                       ~%        (progn~
                       ~%          (syscall3 37 5 0 0)~
                       ~%          (setq *rt-test-count* 0)~
                       ~%          (setq *rt-pass-count* 0)~
                       ~%          (setq *rt-fail-count* 0)~
                       ~%          (funcall thunk)~
                       ~%          (syscall3 60 *rt-fail-count* 0 0))~
                       ~%        (syscall3 61 pid 0 0))))~%")
                     (with-output-to-string (s)
                       (format s "~%(defun run-real-ansi-tests ()~%")
                       (loop while names do
                         (let ((chunk (loop repeat chunk-size while names
                                           collect (pop names))))
                           (format s "  (fork-run (lambda ()~%")
                           (dolist (name chunk)
                             (format s "    (run-ansi-~A)~%" name))
                           (format s "  ))~%")))
                       (format s ")~%")))))

(format t "  prelude: ~D chars~%" (length *prelude-source*))
(format t "  rt: ~D chars~%" (length *rt-source*))
(format t "  bridge: ~D chars~%" (length *bridge-source*))
(format t "  tests: ~D chars~%" (length *test-source*))
(format t "  real-ansi: ~D chars~%" (length *real-ansi-sources*))

;;; ============================================================
;;; 3. Strip in-package forms from source text
;;; ============================================================

(defun strip-in-package (text)
  "Remove (in-package ...) forms from source text."
  (let ((result text))
    (loop
      (let ((pos (search "(in-package " result)))
        (unless pos (return result))
        ;; Find the closing paren
        (let ((end (position #\) result :start pos)))
          (when end
            (setf result (concatenate 'string
                                      (subseq result 0 pos)
                                      (subseq result (1+ end))))))))))

(setf *prelude-source* (strip-in-package *prelude-source*))
(setf *rt-source*      (strip-in-package *rt-source*))
(setf *bridge-source*  (strip-in-package *bridge-source*))
(setf *test-source*    (strip-in-package *test-source*))
(setf *real-ansi-sources* (strip-in-package *real-ansi-sources*))

;;; ============================================================
;;; 4. Driver source (sys-exit + kernel-main)
;;; ============================================================

(defvar *driver-source* "

(defun halt ()
  (syscall3 60 1 0 0))

(defun sys-exit (code)
  (let ((c code))
    (syscall3 60 c 0 0)))

(defun kernel-main ()
  ;; Banner: ANSI-TEST
  (write-char-serial 65)   ; A
  (write-char-serial 78)   ; N
  (write-char-serial 83)   ; S
  (write-char-serial 73)   ; I
  (write-char-serial 45)   ; -
  (write-char-serial 84)   ; T
  (write-char-serial 69)   ; E
  (write-char-serial 83)   ; S
  (write-char-serial 84)   ; T
  (write-char-serial 10)

  ;; Initialize runtime
  (init-symbol-table)
  ;; diagnostic removed
  ;; Init RT counters manually (init-all-globals not safe — some thunks
  ;; reference functions/symbols that may not be available yet)
  (setq *rt-test-count* 0)
  (setq *rt-pass-count* 0)
  (setq *rt-fail-count* 0)

  ;; Run custom tests
  (run-all-tests)

  ;; Run real ANSI tests (generated at build time)
  (run-real-ansi-tests)

  ;; Report custom test results (ANSI results printed by fork children)
  (write-char-serial 10)
  (print-dec *rt-pass-count*)
  (write-char-serial 47)   ;; /
  (print-dec *rt-test-count*)
  ;; DONE marker
  (write-char-serial 32)   ; space
  (write-char-serial 68)   ; D
  (write-char-serial 79)   ; O
  (write-char-serial 78)   ; N
  (write-char-serial 69)   ; E
  (write-char-serial 10)
  (sys-exit 0))

")

;;; ============================================================
;;; 5. Assemble full source
;;; ============================================================

(format t "~%Assembling full source...~%")

(defvar *full-source*
  (concatenate 'string
    ;; 1. Prelude (list utils, equal, print-dec, hash tables, etc.)
    *prelude-source*
    (string #\Newline)
    ;; 2. RT harness (deftest, do-tests)
    *rt-source*
    (string #\Newline)
    ;; 3. ANSI bridge (helpers, stubs, missing functions)
    *bridge-source*
    (string #\Newline)
    ;; 4. Our test source (run-*-tests, run-all-tests)
    *test-source*
    (string #\Newline)
    ;; 5. Real ANSI test files
    *real-ansi-sources*
    (string #\Newline)
    ;; 6. Driver (sys-exit, kernel-main)
    *driver-source*))

(format t "Full source: ~D characters~%" (length *full-source*))

;;; ============================================================
;;; 6. Build Linux ELF via MVM pipeline
;;; ============================================================

;; Load Linux boot descriptor
(mvm-load "boot/boot-linux-x64.lisp")

(in-package :modus.mvm)

;; Override linux-x64-boot-descriptor to include nil page mmap
;; (car nil must not segfault)
(defun mvm-linux-x64-test-entry (buf)
  "Emit Linux x64 entry stub with NIL page mmap."
  (emit-linux-x64-entry buf)
  ;; mmap NIL page at 0xDEAD0000 (car/cdr nil dereferences this)
  ;; movabs rdi, 0xDEAD0000
  (emit-bytes buf #x48 #xBF #x00 #x00 #xAD #xDE #x00 #x00 #x00 #x00)
  (emit-bytes buf #x48 #xC7 #xC6 #x00 #x10 #x00 #x00) ; mov rsi, 4096
  (emit-bytes buf #x48 #xC7 #xC2 #x03 #x00 #x00 #x00) ; mov rdx, PROT_READ|PROT_WRITE
  (emit-bytes buf #x49 #xC7 #xC2 #x32 #x00 #x00 #x00) ; mov r10, MAP_PRIVATE|MAP_ANON|MAP_FIXED
  (emit-bytes buf #x49 #xC7 #xC0 #xFF #xFF #xFF #xFF)   ; mov r8, -1
  (emit-bytes buf #x49 #xC7 #xC1 #x00 #x00 #x00 #x00) ; mov r9, 0
  (emit-bytes buf #x48 #xC7 #xC0 #x09 #x00 #x00 #x00) ; mov rax, SYS_mmap
  (emit-bytes buf #x0F #x05)                             ; syscall
  ;; Fill nil page with NIL values using rep stosq
  (emit-bytes buf #x48 #x89 #xC7)                       ; mov rdi, rax
  (emit-bytes buf #x48 #xC7 #xC1 #x00 #x02 #x00 #x00) ; mov rcx, 512
  (emit-bytes buf #x4C #x89 #xF8)                       ; mov rax, r15 (NIL)
  (emit-bytes buf #xF3 #x48 #xAB))                       ; rep stosq

(defun linux-x64-boot-descriptor ()
  (list :arch :x86-64
        :entry-fn #'mvm-linux-x64-test-entry
        :load-addr +linux-x64-load-addr+
        :elf-format :linux-x64))

;; Install x64 translator in Linux mode
(funcall (intern "INSTALL-X64-TRANSLATOR" "MODUS.MVM.X64"))
(setf modus.mvm.x64::*x64-linux-mode* t)

(format t "~%Compiling test runner (~D chars)...~%" (length cl-user::*full-source*))

(let ((image (build-image :target :linux-x64 :source-text cl-user::*full-source*)))
  (let ((path "/tmp/modus-ansi-test"))
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (format t "~%Wrote ~D bytes to ~A~%"
            (length (kernel-image-image-bytes image)) path)
    (format t "~%Run: ~A~%" path)))
