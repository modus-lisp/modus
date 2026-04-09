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
(let ((ansi-dir "/tmp/ansi-test/cons/"))
  ;; Real ANSI test files — only those that compile cleanly
  ;; Excluded: push.lsp, pop.lsp (expand-in-current-env macrolet)
  ;;           tree-equal.lsp, append.lsp, make-list.lsp, subst.lsp (complex :test/:key args)
  (dolist (file '("cons.lsp" "consp.lsp" "atom.lsp"
                  "copy-list.lsp" "nth.lsp" "endp.lsp"
                  "rest.lsp" "last.lsp" "nconc.lsp"
                  "revappend.lsp" "nreconc.lsp" "rplaca.lsp" "rplacd.lsp"
                  "acons.lsp" "pairlis.lsp" "copy-tree.lsp"
                  "tailp.lsp" "ldiff.lsp" "copy-alist.lsp"))
    (let ((path (concatenate 'string ansi-dir file)))
      (when (probe-file path)
        (format t "  Transforming: ~A~%" file)
        ;; Read forms with SBCL's reader, transform deftests, write back
        (let ((forms nil)
              (test-count 0))
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
                   (cond
                     ((= (length expected) 1)
                      (push (format nil "(rt-run-test '~S (funcall (lambda () ~S)) '~S)"
                                    name test-form (car expected))
                            test-forms))
                     ((> (length expected) 0)
                      (push (format nil "(rt-run-test-mv '~S (multiple-value-list (funcall (lambda () ~S))) '~S)"
                                    name test-form expected)
                            test-forms))
                     (t nil))))
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

;; Generate run-real-ansi-tests function that calls all file-level runners
(let ((runner-calls ""))
  (let ((ansi-dir "/tmp/ansi-test/cons/"))
    ;; Same file list as above
    (dolist (file '("cons" "consp" "atom"
                    "copy-list" "nth" "endp"
                    "rest" "last" "nconc"
                    "revappend" "nreconc" "rplaca" "rplacd"
                    "acons" "pairlis" "copy-tree"
                    "tailp" "ldiff" "copy-alist"))
      (when (probe-file (concatenate 'string ansi-dir file ".lsp"))
        (setf runner-calls
              (concatenate 'string runner-calls
                           (format nil "  (run-ansi-~A)~%" file))))))
  (setf *real-ansi-sources*
        (concatenate 'string *real-ansi-sources*
                     (format nil "~%(defun run-real-ansi-tests ()~%~A)~%" runner-calls))))

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
  ;; Init defvar'd globals (compiler generates init thunks but they
  ;; aren't auto-called; set them here)
  (setq *rt-test-count* 0)
  (setq *rt-pass-count* 0)
  (setq *rt-fail-count* 0)

  ;; Run custom tests
  (run-all-tests)

  ;; Run real ANSI tests (generated at build time)
  (run-real-ansi-tests)

  ;; Report and exit
  (let ((failures (do-tests)))
    (sys-exit failures)))

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
