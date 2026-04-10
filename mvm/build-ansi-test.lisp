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

;; Load real ANSI test files (if available)
(defvar *real-ansi-sources* "")
(defvar *ansi-test-counter* 10000)
(defvar *ansi-file-names* nil)
(let ((ansi-dir "/tmp/ansi-test/cons/"))
  ;; Real ANSI test files — only those that compile cleanly
  ;; Excluded: push.lsp, pop.lsp (expand-in-current-env macrolet)
  ;;           tree-equal.lsp, append.lsp, make-list.lsp, subst.lsp (complex :test/:key args)
  ;; Real ANSI test files
  (dolist (file '("cons.lsp" "consp.lsp" "atom.lsp" "endp.lsp"
                  "rest.lsp" "last.lsp"
                  "revappend.lsp" "nreconc.lsp"
                  "rplaca.lsp" "rplacd.lsp"
                  "acons.lsp" "pairlis.lsp" "copy-alist.lsp"
                  "nconc.lsp" "butlast.lsp" "list.lsp"
                  "listp.lsp" "nthcdr.lsp" "nth.lsp"
                  "copy-list.lsp" "copy-tree.lsp" "tailp.lsp"
                  "append.lsp" "mapcar.lsp" "mapc.lsp"
                  "member.lsp" "assoc.lsp"
                  "ldiff.lsp" "subst.lsp" "tree-equal.lsp"
                  "make-list.lsp"
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
))
  ;; Data and Control Flow chapter
  (dolist (file '("if.lsp" "and.lsp" "or.lsp" "not.lsp"
                  "identity.lsp"
                  "not-and-null.lsp" "t.lsp" "nil.lsp"
                  "prog1.lsp" "prog2.lsp" "return.lsp"
                  "multiple-value-bind.lsp" "multiple-value-list.lsp"
                  "call-arguments-limit.lsp" "lambda-parameters-limit.lsp"
                  "ecase.lsp" "block.lsp" "return-from.lsp"
                  "constantly.lsp"))
    (let ((path (concatenate 'string "/tmp/ansi-test/data-and-control-flow/" file)))
      (when (probe-file path)
        (format t "  Transforming: dcf/~A~%" file)
        (push (pathname-name file) *ansi-file-names*)
        (let ((forms nil) (test-count *ansi-test-counter*))
          (with-open-file (s path :direction :input)
            (let ((*package* (find-package :cl-user)))
              (loop (let ((form (read s nil :eof)))
                      (when (eq form :eof) (return))
                      (push form forms)))))
          (setf forms (nreverse forms))
          (let ((out (make-string-output-stream)) (test-forms nil))
            (format out "~%;; === dcf/~A ===~%" file)
            (dolist (form forms)
              (cond
                ((and (consp form) (eq (car form) 'deftest))
                 (let ((name (cadr form)) (test-form (caddr form))
                       (expected (cdddr form)))
                   (setf *ansi-test-counter* (1+ *ansi-test-counter*))
                   (let ((test-id *ansi-test-counter*))
                     (format t "      ~D = ~A~%" test-id name)
                     (cond
                       ((= (length expected) 1)
                        (push (format nil "(rt-run-test ~D ~S '~S)"
                                      test-id test-form (car expected)) test-forms))
                       ((> (length expected) 0)
                        (push (format nil "(rt-run-test-mv ~D (multiple-value-list ~S) '~S)"
                                      test-id test-form expected) test-forms))))))
                ((and (consp form) (member (car form)
                        '(defharmless def-fold-test def-macro-test in-package declaim))) nil)
                (t (format out "~S~%" form))))
            (format out "(defun run-ansi-~A ()~%" (pathname-name file))
            (dolist (tf (nreverse test-forms)) (format out "  ~A~%" tf))
            (format out ")~%")
            (setf *real-ansi-sources*
                  (concatenate 'string *real-ansi-sources*
                               (get-output-stream-string out))))))))

  ;; Iteration chapter (placeholder for future)
  (dolist (file '())
    (let ((path (concatenate 'string "/tmp/ansi-test/iteration/" file)))
      (when (probe-file path)
        (format t "  Transforming: iteration/~A~%" file)
        (push (pathname-name file) *ansi-file-names*)
        (let ((forms nil) (test-count *ansi-test-counter*))
          (with-open-file (s path :direction :input)
            (let ((*package* (find-package :cl-user)))
              (loop (let ((form (read s nil :eof)))
                      (when (eq form :eof) (return))
                      (push form forms)))))
          (setf forms (nreverse forms))
          (let ((out (make-string-output-stream)) (test-forms nil))
            (format out "~%;; === iteration/~A ===~%" file)
            (dolist (form forms)
              (cond
                ((and (consp form) (eq (car form) 'deftest))
                 (let ((name (cadr form)) (test-form (caddr form))
                       (expected (cdddr form)))
                   (setf *ansi-test-counter* (1+ *ansi-test-counter*))
                   (let ((test-id *ansi-test-counter*))
                     (format t "      ~D = ~A~%" test-id name)
                     (cond
                       ((= (length expected) 1)
                        (push (format nil "(rt-run-test ~D ~S '~S)"
                                      test-id test-form (car expected)) test-forms))
                       ((> (length expected) 0)
                        (push (format nil "(rt-run-test-mv ~D (multiple-value-list ~S) '~S)"
                                      test-id test-form expected) test-forms))))))
                ((and (consp form) (member (car form)
                        '(defharmless def-fold-test in-package declaim))) nil)
                (t (format out "~S~%" form))))
            (format out "(defun run-ansi-~A ()~%" (pathname-name file))
            (dolist (tf (nreverse test-forms)) (format out "  ~A~%" tf))
            (format out ")~%")
            (setf *real-ansi-sources*
                  (concatenate 'string *real-ansi-sources*
                               (get-output-stream-string out))))))))

  ;; Numbers chapter — integer-only files
  (dolist (file '("evenp.lsp" "oddp.lsp" "ash.lsp"
                  "logand.lsp" "logior.lsp" "logxor.lsp" "lognot.lsp"))
    (let ((path (concatenate 'string "/tmp/ansi-test/numbers/" file)))
      (when (probe-file path)
        (format t "  Transforming: numbers/~A~%" file)
        (push (pathname-name file) *ansi-file-names*)
        (let ((forms nil) (test-count *ansi-test-counter*))
          (with-open-file (s path :direction :input)
            (let ((*package* (find-package :cl-user)))
              (loop (let ((form (read s nil :eof)))
                      (when (eq form :eof) (return))
                      (push form forms)))))
          (setf forms (nreverse forms))
          (let ((out (make-string-output-stream)) (test-forms nil))
            (format out "~%;; === numbers/~A ===~%" file)
            (dolist (form forms)
              (cond
                ((and (consp form) (eq (car form) 'deftest))
                 (let ((name (cadr form)) (test-form (caddr form))
                       (expected (cdddr form)))
                   (setf *ansi-test-counter* (1+ *ansi-test-counter*))
                   (let ((test-id *ansi-test-counter*))
                     (format t "      ~D = ~A~%" test-id name)
                     (cond
                       ((= (length expected) 1)
                        (push (format nil "(rt-run-test ~D ~S '~S)"
                                      test-id test-form (car expected)) test-forms))
                       ((> (length expected) 0)
                        (push (format nil "(rt-run-test-mv ~D (multiple-value-list ~S) '~S)"
                                      test-id test-form expected) test-forms))))))
                ((and (consp form) (member (car form)
                        '(defharmless def-fold-test in-package declaim))) nil)
                (t (format out "~S~%" form))))
            (format out "(defun run-ansi-~A ()~%" (pathname-name file))
            (dolist (tf (nreverse test-forms)) (format out "  ~A~%" tf))
            (format out ")~%")
            (setf *real-ansi-sources*
                  (concatenate 'string *real-ansi-sources*
                               (get-output-stream-string out))))))))
    (let ((path (concatenate 'string ansi-dir file)))
      (when (probe-file path)
        (format t "  Transforming: ~A~%" file)
        (push (pathname-name file) *ansi-file-names*)
        ;; Read forms with SBCL's reader, transform deftests, write back
        (let ((forms nil)
              (test-count *ansi-test-counter*))
          (with-open-file (s path :direction :input)
            (let ((*package* (find-package :cl-user)))
              (loop
                (let ((form (read s nil :eof)))
                  (when (eq form :eof) (return))
                  (push form forms)))))
          (setf forms (nreverse forms))
          ;; Transform each form
          (let ((out (make-string-output-stream)))
            (format out "~%;; === ~A ===~%" file)
            (let ((test-forms nil))
            ;; Pass through defun/defvar at top level, collect deftest calls
            (dolist (form forms)
              (cond
                ;; (deftest name form expected...)
                ((and (consp form) (eq (car form) 'deftest))
                 (let ((name (cadr form))
                       (test-form (caddr form))
                       (expected (cdddr form)))
                   (incf test-count)
                   ;; Use global counter for unique test IDs
                   (setf *ansi-test-counter* (1+ *ansi-test-counter*))
                   (let ((test-id *ansi-test-counter*))
                     (format t "      ~D = ~A~%" test-id name)
                     (cond
                       ((= (length expected) 1)
                        (push (format nil "(rt-run-test ~D (funcall (lambda () ~S)) '~S)"
                                      test-id test-form (car expected))
                              test-forms))
                       ((> (length expected) 0)
                        (push (format nil "(rt-run-test-mv ~D (multiple-value-list (funcall (lambda () ~S))) '~S)"
                                      test-id test-form expected)
                              test-forms))
                       (t nil)))))
                ;; Skip stubs
                ((and (consp form)
                      (member (car form) '(defharmless def-fold-test
                                           in-package declaim)))
                 nil)
                ;; Pass through everything else at top level
                (t
                 (format out "~S~%" form))))
            ;; Generate wrapper function for the test calls
            (format out "(defun run-ansi-~A ()~%" (pathname-name file))
            (dolist (tf (nreverse test-forms))
              (format out "  ~A~%" tf))
            (format out ")~%"))
            (format t "    ~D tests~%" test-count)
            (setf *real-ansi-sources*
                  (concatenate 'string *real-ansi-sources*
                               (get-output-stream-string out)))))))))

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
