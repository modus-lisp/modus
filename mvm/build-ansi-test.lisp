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
            (setf forms (nreverse forms))
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
    "loop14.lsp" "loop15.lsp" "loop16.lsp" "loop17.lsp"
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
    "logbitp.lsp" "logtest.lsp"
    "number-comparison.lsp" "parse-integer.lsp"
    "oneplus.lsp" "oneminus.lsp"
    "expt.lsp" "lcm.lsp" "gcd.lsp"
    "byte.lsp" "dpb.lsp" "ldb.lsp" "ldb-test.lsp"
    "deposit-field.lsp" "mask-field.lsp"
    "isqrt.lsp" "signum.lsp" "random.lsp"
    "floor.lsp" "ceiling.lsp" "truncate.lsp" "round.lsp"
    "divide.lsp"
    "sin.lsp" "cos.lsp" "tan.lsp" "exp.lsp" "sqrt.lsp"
    "atan.lsp" "phase.lsp"
    "complexp.lsp" "numberp.lsp" "realp.lsp" "integerp.lsp"
    "float-radix.lsp" "float-digits.lsp"))

;; Symbols chapter
(load-ansi-chapter "/tmp/ansi-test/symbols/"
  '("symbolp.lsp" "keywordp.lsp" "gensym.lsp"
    "boundp.lsp" "makunbound.lsp" "set.lsp"
    "copy-symbol.lsp" "make-symbol.lsp"))

;; Structures chapter
;; Structures
(load-ansi-chapter "/tmp/ansi-test/structures/"
  '("structure-00.lsp" "structures-01.lsp"
    "structures-02.lsp" "structures-03.lsp" "structures-04.lsp"))

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
    "make-array.lsp"
    "array.lsp" "simple-array.lsp" "array-t.lsp" "simple-array-t.lsp"
    "adjust-array.lsp" "adjustable-array-p.lsp"
    "array-displacement.lsp" "array-has-fill-pointer-p.lsp"
    "array-in-bounds-p.lsp" "array-misc.lsp"
    "fill-pointer.lsp" "vector-push.lsp" "vector-push-extend.lsp"
    "vector-pop.lsp"
    "bit-vector-p.lsp" "simple-bit-vector-p.lsp"
    "bit.lsp" "sbit.lsp"
    "bit-and.lsp" "bit-ior.lsp" "bit-xor.lsp" "bit-eqv.lsp"
    "bit-nand.lsp" "bit-nor.lsp" "bit-andc1.lsp" "bit-andc2.lsp"
    "bit-orc1.lsp" "bit-orc2.lsp" "bit-not.lsp"
    "bit-vector.lsp" "simple-bit-vector.lsp"
    "upgraded-array-element-type.lsp"))

;; Eval and Compile chapter
(load-ansi-chapter "/tmp/ansi-test/eval-and-compile/"
  '("constantp.lsp" "eval.lsp" "locally.lsp" "the.lsp" "special.lsp"
    "lambda.lsp" "eval-when.lsp" "macroexpand.lsp" "macroexpand-1.lsp"
    "compile.lsp" "defmacro.lsp" "ignore.lsp" "ignorable.lsp"
    "optimize.lsp" "proclaim.lsp" "declaim.lsp" "type.lsp"
    "eval-and-compile.lsp" "declaration.lsp"))

;; Types and Classes chapter
(load-ansi-chapter "/tmp/ansi-test/types-and-classes/"
  '("typep.lsp" "type-of.lsp" "coerce.lsp" "subtypep.lsp" "deftype.lsp"
    "types-and-class.lsp" "types-and-class-2.lsp"
    "subtypep-integer.lsp" "subtypep-cons.lsp" "subtypep-member.lsp"
    "subtypep-eql.lsp" "subtypep-array.lsp" "subtypep-rational.lsp"
    "subtypep-real.lsp" "subtypep-float.lsp" "subtypep-function.lsp"))

;; Conditions chapter (what we can handle)
(load-ansi-chapter "/tmp/ansi-test/conditions/"
  '("condition.lsp" "error.lsp" "warn.lsp"
    "handler-case.lsp" "handler-bind.lsp" "ignore-errors.lsp"))

;; Reader chapter
(load-ansi-chapter "/tmp/ansi-test/reader/"
  '("readtablep.lsp" "read-from-string.lsp" "read.lsp"))

;; Printer chapter
(load-ansi-chapter "/tmp/ansi-test/printer/"
  '("write.lsp" "write-to-string.lsp"
    "prin1.lsp" "prin1-to-string.lsp"
    "princ.lsp" "princ-to-string.lsp"
    "print.lsp" "pprint.lsp"
    "print-integers.lsp" "print-strings.lsp"
    "print-cons.lsp" "print-characters.lsp"
    "print-vector.lsp" "print-array.lsp"
    "print-symbols.lsp"
    "print-bit-vector.lsp"
    "print-length.lsp" "print-level.lsp"
    "printer-control-vars.lsp"))

;; Streams chapter
(load-ansi-chapter "/tmp/ansi-test/streams/"
  '("streamp.lsp" "write-char.lsp" "terpri.lsp"
    "write-string.lsp" "write-line.lsp"
    "finish-output.lsp" "force-output.lsp" "clear-output.lsp"
    "clear-input.lsp"
    "input-stream-p.lsp" "output-stream-p.lsp" "open-stream-p.lsp"
    "stream-element-type.lsp" "stream-external-format.lsp"
    "interactive-stream-p.lsp"
    "listen.lsp" "fresh-line.lsp"
    "make-string-output-stream.lsp" "get-output-stream-string.lsp"
    "make-string-input-stream.lsp"
    "with-output-to-string.lsp" "with-input-from-string.lsp"
    "make-broadcast-stream.lsp" "broadcast-stream-streams.lsp"
    "make-concatenated-stream.lsp" "concatenated-stream-streams.lsp"
    "make-echo-stream.lsp" "echo-stream-input-stream.lsp"
    "echo-stream-output-stream.lsp"
    "make-synonym-stream.lsp" "synonym-stream-symbol.lsp"
    "make-two-way-stream.lsp" "two-way-stream-input-stream.lsp"
    "two-way-stream-output-stream.lsp"
    "peek-char.lsp" "unread-char.lsp"
    "read-char.lsp" "read-char-no-hang.lsp"
    "read-line.lsp" "read-byte.lsp"
    "read-sequence.lsp" "write-sequence.lsp" "write-byte.lsp"
    "file-length.lsp" "file-position.lsp" "file-string-length.lsp"))

;; More DCF
(load-ansi-chapter "/tmp/ansi-test/data-and-control-flow/"
  '("every.lsp" "some.lsp" "notany.lsp" "notevery.lsp"
    "equal.lsp" "equalp.lsp" "typecase.lsp"
    "multiple-value-prog1.lsp" "multiple-value-setq.lsp"
    "values-list.lsp" "nth-value.lsp"
    "catch.lsp" "unwind-protect.lsp"
    "functionp.lsp" "fboundp.lsp"
    "psetq.lsp" "values.lsp"
    "defun.lsp" "defvar.lsp" "defparameter.lsp" "defconstant.lsp"
    "let.lsp" "letstar.lsp"
    "etypecase.lsp"
    "multiple-value-prog1.lsp" "multiple-value-setq.lsp"))

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
    "sort.lsp" "stable-sort.lsp"
    "map-into.lsp" "make-sequence.lsp"
    "nsubstitute.lsp" "nsubstitute-if.lsp" "nsubstitute-if-not.lsp"))

;; Generate run-real-ansi-tests that calls all file-level runners
(setf *ansi-file-names* (nreverse *ansi-file-names*))
(setf *real-ansi-sources*
      (concatenate 'string *real-ansi-sources*
                   (format nil "~%(defun run-real-ansi-tests ()~%~{  (run-ansi-~A)~%~})~%"
                           *ansi-file-names*)))

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
  ;; Init RT counters manually (init-all-globals not safe — some thunks
  ;; reference functions/symbols that may not be available yet)
  (setq *rt-test-count* 0)
  (setq *rt-pass-count* 0)
  (setq *rt-fail-count* 0)

  ;; Run custom tests
  (run-all-tests)

  ;; Run real ANSI tests (generated at build time)
  (run-real-ansi-tests)

  ;; Report — use simple output to avoid crash in large binaries
  (write-char-serial 10)
  (print-dec *rt-pass-count*)
  (write-char-serial 47)   ;; /
  (print-dec *rt-test-count*)
  (write-char-serial 32)
  (if (= *rt-fail-count* 0)
      (progn (write-char-serial 80) (write-char-serial 65)
             (write-char-serial 83) (write-char-serial 83))
      (progn (write-char-serial 70) (write-char-serial 65)
             (write-char-serial 73) (write-char-serial 76)))
  (write-char-serial 10)
  (sys-exit (if (> *rt-fail-count* 255) 255 *rt-fail-count*)))

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
