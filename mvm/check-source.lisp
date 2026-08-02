;;;; check-source.lisp - Static checker for MVM compiler limitations
;;;;
;;;; Usage: sbcl --script mvm/check-source.lisp [file1.lisp ...]
;;;;        sbcl --script mvm/check-source.lisp --net     (bare-metal net/ sources, default)
;;;;        sbcl --script mvm/check-source.lisp --all     (all source files)
;;;;        sbcl --script mvm/check-source.lisp --info    (also show INFO level)
;;;;
;;;; Checks for known MVM compiler bugs/limitations:
;;;;   1. 3+ arg (+ ...) or (* ...) — works in MVM PUSH/POP path but dangerous
;;;;   2. set-car/set-cdr with function call args — register clobber
;;;;   3. 18+ bindings in a single let/let* — may miscompile
;;;;   4. ~25+ sequential forms in a defun body — crashes/hangs
;;;;   5. Deeply nested arithmetic with function calls — register clobber
;;;;   6. Variable-index aset without let wrapper — clobbers RAX
;;;;
;;;; Note: 3-arg + actually works in the MVM compiler (compiler.lisp) via
;;;; PUSH/POP save/restore patterns. The issue was specific to the bare-metal
;;;; self-hosting compiler's reimplementation. Still flagged as WARN since
;;;; it's a known hazard area.

(defpackage :check-source
  (:use :cl))
(in-package :check-source)

;;; ============================================================
;;; Configuration
;;; ============================================================

(defparameter *let-binding-limit* 17)      ; warn at 18+
(defparameter *body-form-limit* 24)        ; warn at 25+
(defparameter *nesting-depth-limit* 4)     ; arithmetic nesting depth
(defparameter *warnings* nil)
(defparameter *current-file* nil)
(defparameter *current-defun* nil)
(defparameter *show-info* nil)             ; --info flag to show INFO level

;;; ============================================================
;;; Warning collection
;;; ============================================================

(defstruct src-warning
  file defun level message form-hint)

(defun warn-check (level message &optional form-hint)
  (push (make-src-warning :file *current-file*
                          :defun *current-defun*
                          :level level
                          :message message
                          :form-hint form-hint)
        *warnings*))

(defun print-warnings ()
  (let ((sorted (sort (copy-list *warnings*)
                      (lambda (a b)
                        (or (string< (or (src-warning-file a) "")
                                     (or (src-warning-file b) ""))
                            (and (string= (or (src-warning-file a) "")
                                          (or (src-warning-file b) ""))
                                 (string< (or (src-warning-defun a) "")
                                          (or (src-warning-defun b) "")))))))
        (error-count 0)
        (warn-count 0)
        (info-count 0))
    (dolist (w sorted)
      (ecase (src-warning-level w)
        (:error (incf error-count))
        (:warn  (incf warn-count))
        (:info  (incf info-count)))
      (when (or (not (eq (src-warning-level w) :info)) *show-info*)
        (format t "~A  ~A  ~A~%  ~A~@[~%  near: ~A~]~%~%"
                (ecase (src-warning-level w)
                  (:error "ERROR")
                  (:warn  "WARN ")
                  (:info  "INFO "))
                (or (src-warning-file w) "<unknown>")
                (or (src-warning-defun w) "<top-level>")
                (src-warning-message w)
                (when (src-warning-form-hint w)
                  (let ((s (format nil "~S" (src-warning-form-hint w))))
                    (if (> (length s) 120)
                        (concatenate 'string (subseq s 0 117) "~")
                        s))))))
    (format t "=== Summary: ~D error~:P, ~D warning~:P~@[, ~D info~] ===~%"
            error-count warn-count (when (> info-count 0) info-count))
    (+ error-count warn-count)))

;;; ============================================================
;;; Form walkers
;;; ============================================================

(defun function-call-p (form)
  "True if form looks like a function call (not a special form or macro)."
  (and (consp form)
       (symbolp (car form))
       (not (member (car form)
                    '(+ - * / = < > <= >= /=
                      logand logior logxor ash
                      if when unless cond
                      let let* setq setf
                      progn block return return-from
                      lambda defun defmacro
                      quote function
                      car cdr cons list
                      aref aset
                      loop dotimes dolist
                      and or not
                      make-array make-string
                      the declare)))))

(defun abbrev-form (form)
  "Abbreviate a form for display."
  (cond ((not (consp form)) form)
        ((> (length (format nil "~S" form)) 80)
         (list (car form) (intern "...")))
        (t form)))

(defun check-multi-arg-arithmetic (form)
  "Check for (+ a b c) or (* a b c) with 3+ arguments."
  (when (and (consp form)
             (member (car form) '(+ *))
             (> (length (cdr form)) 2))
    (warn-check :warn
                (format nil "~A with ~D args (uses PUSH/POP — works in MVM, breaks in self-hosting compiler)"
                        (car form) (length (cdr form)))
                (abbrev-form form))))

(defun check-set-car-cdr-call-arg (form)
  "Check for (set-car x (fn ...)) or (set-cdr x (fn ...))."
  (when (and (consp form)
             (member (car form) '(set-car set-cdr rplaca rplacd))
             (>= (length (cdr form)) 2))
    (let ((val-arg (caddr form)))
      (when (function-call-p val-arg)
        (warn-check :error
                    (format nil "~A with function call arg ~S — pre-compute to let binding"
                            (car form) (car val-arg))
                    (abbrev-form form))))))

(defun count-let-bindings (form)
  "Count bindings in a let/let* form."
  (when (and (consp form)
             (member (car form) '(let let*))
             (consp (cadr form)))
    (length (cadr form))))

(defun check-let-bindings (form)
  "Check for let/let* with too many bindings."
  (let ((n (count-let-bindings form)))
    (when (and n (> n *let-binding-limit*))
      (warn-check :warn
                  (format nil "~A with ~D bindings (limit ~D) — may miscompile, split into helpers"
                          (car form) n *let-binding-limit*)))))

(defun count-body-forms (body)
  "Count sequential forms in a body, unwrapping progn."
  (let ((count 0))
    (dolist (form body)
      (if (and (consp form) (eq (car form) 'progn))
          (incf count (count-body-forms (cdr form)))
          (incf count)))
    count))

(defun check-defun-body-forms (form)
  "Check for defun with too many sequential body forms."
  (when (and (consp form)
             (eq (car form) 'defun)
             (>= (length form) 4))
    (let* ((name (cadr form))
           (body (cdddr form))
           (n (count-body-forms body)))
      (when (> n *body-form-limit*)
        (warn-check :warn
                    (format nil "defun ~A has ~D sequential forms (limit ~D) — split into helpers"
                            name n *body-form-limit*))))))

(defun arithmetic-op-p (sym)
  (member sym '(+ - * logand logior logxor ash)))

(defun measure-arithmetic-depth (form)
  "Measure nesting depth of arithmetic operations."
  (if (not (consp form))
      0
      (if (arithmetic-op-p (car form))
          (1+ (reduce #'max (mapcar #'measure-arithmetic-depth (cdr form))
                      :initial-value 0))
          0)))

(defun has-function-calls-p (form)
  "Check if form contains function calls (not just arithmetic/variable refs)."
  (cond ((not (consp form)) nil)
        ((function-call-p form) t)
        (t (some #'has-function-calls-p (cdr form)))))

(defun check-deep-arithmetic (form)
  "Check for deeply nested arithmetic with function calls.

   As of the ANF normalizer in compiler.lisp (ANF-NORMALIZE-ARITH-ARGS) this
   is no longer a miscompile: whenever an operand would reach the unsafe
   PUSH/POP nesting that CHECK-ARITH-NESTING guards, the compiler binds it —
   and every non-constant operand to its left — to LET* temporaries and
   compiles that instead.  Kept as INFO because the rewrite costs frame slots
   (see *let-binding-limit*), so hand-hoisting is still cheaper in hot code."
  (when (and (consp form)
             (arithmetic-op-p (car form)))
    (let ((depth (measure-arithmetic-depth form)))
      (when (and (> depth *nesting-depth-limit*)
                 (has-function-calls-p form))
        (warn-check :info
                    (format nil "arithmetic nesting depth ~D with function calls — compiler will ANF-hoist into let* temporaries"
                            depth))))))

(defun check-variable-index-aset (form)
  "Check for (aset array variable-index value) without let wrapper."
  (when (and (consp form)
             (eq (car form) 'aset)
             (>= (length (cdr form)) 3))
    (let ((index-arg (caddr form)))
      (when (and (not (numberp index-arg))
                 (not (and (consp index-arg) (eq (car index-arg) 'quote))))
        (warn-check :info
                    (format nil "aset with variable index ~S — wrap in let if not last form"
                            index-arg))))))

;;; ============================================================
;;; Main walker
;;; ============================================================

(defun walk-form (form)
  "Walk a form and run all checks."
  (when (consp form)
    ;; Run checks on this form
    (check-multi-arg-arithmetic form)
    (check-set-car-cdr-call-arg form)
    (check-let-bindings form)
    (check-deep-arithmetic form)
    (check-variable-index-aset form)

    ;; Track defun for context
    (when (and (eq (car form) 'defun) (>= (length form) 4)
               (or (symbolp (cadr form)) (consp (cadr form))))
      (let ((*current-defun* (format nil "~A" (cadr form))))
        (check-defun-body-forms form)
        (dolist (sub (cdddr form))
          (walk-form sub)))
      (return-from walk-form))

    ;; Recurse into subforms
    (dolist (sub form)
      (when (consp sub)
        (walk-form sub)))))

(defun safe-read (stream)
  "Read a form, handling #. by treating it as a constant."
  (handler-bind ((error (lambda (c)
                          (declare (ignore c))
                          (return-from safe-read (values nil t)))))
    (values (read stream nil :eof) nil)))

(defun check-file (path)
  "Read and check all top-level forms in a file."
  (let ((*current-file* (enough-namestring path))
        (*current-defun* nil))
    (with-open-file (stream path :direction :input)
      (let ((*read-eval* t)
            (*package* (find-package :check-source)))
        ;; Create dummy packages for in-package forms
        (loop for form = (safe-read stream)
              do (cond ((eq form :eof) (return))
                       ((null form) (return))  ; read error, skip rest
                       (t (walk-form form))))))))

(defun check-string (source-string &optional (name "<string>"))
  "Check all top-level forms in a source string."
  (let ((*current-file* name)
        (*current-defun* nil))
    (with-input-from-string (stream source-string)
      (let ((*read-eval* nil)
            (*package* (find-package :check-source)))
        (loop for form = (safe-read stream)
              do (cond ((eq form :eof) (return))
                       ((null form) (return))
                       (t (walk-form form))))))))

;;; ============================================================
;;; Entry point
;;; ============================================================

;; Net source files that run on bare metal (most important to check)
(defparameter *net-files*
  '("net/ip.lisp" "net/crypto.lisp" "net/crypto-32.lisp" "net/crypto-w32.lisp"
    "net/ssh.lisp" "net/http.lisp" "net/http-client.lisp"
    "net/e1000.lisp" "net/dwc2.lisp" "net/dwc2-device.lisp"
    "net/usb.lisp" "net/cdc-ether.lisp" "net/hid.lisp"
    "net/actors.lisp" "net/actors-net-overrides.lisp"
    "net/isolated-net.lisp" "net/ne2000.lisp"
    "net/aarch64-overrides.lisp" "net/32bit-overrides.lisp"
    "net/arch-aarch64.lisp" "net/arch-raspi3b.lisp" "net/arch-x86.lisp"
    "net/arch-i386.lisp" "net/arch-arm32-rpi.lisp"
    "net/x86-ssh-overrides.lisp" "net/uart-bootloader.lisp"))

(defun all-source-files ()
  "Return list of all Lisp source files."
  (let ((files nil))
    (dolist (dir '("net/" "mvm/" "cross/" "boot/"))
      (dolist (f (directory (merge-pathnames "*.lisp" dir)))
        (push f files)))
    (nreverse files)))

(defun ensure-dummy-packages ()
  "Create dummy packages so source files can be read."
  (dolist (pkg '(:modus.mvm :modus.mvm.x64 :modus.mvm.aarch64
                 :modus.mvm.riscv :modus.mvm.ppc :modus.mvm.i386
                 :modus.mvm.68k :modus.mvm.arm32
                 :modus.asm :modus.tags :modus.runtime))
    (unless (find-package pkg)
      (make-package pkg :use '(:cl)))))

(defun main ()
  (ensure-dummy-packages)
  (let* ((args sb-ext:*posix-argv*)
         (net-mode (member "--net" args :test #'string=))
         (all-mode (member "--all" args :test #'string=))
         (info-mode (member "--info" args :test #'string=))
         ;; Collect file args (anything ending in .lisp that's not the script)
         (file-args (remove-if-not (lambda (a)
                                     (and (> (length a) 5)
                                          (string= (subseq a (- (length a) 5)) ".lisp")
                                          (not (search "check-source" a))))
                                   args))
         (files (cond (net-mode (mapcar #'pathname *net-files*))
                      (all-mode (all-source-files))
                      (file-args (mapcar #'pathname file-args))
                      (t (mapcar #'pathname *net-files*)))))
    (setf *show-info* (not (null info-mode)))
    (format t "~%MVM Source Checker — scanning ~D file~:P~A...~%~%"
            (length files) (if net-mode " (bare-metal)" ""))
    (setf *warnings* nil)
    (dolist (f files)
      (when (probe-file f)
        (check-file f)))
    (let ((count (print-warnings)))
      (sb-ext:exit :code (if (> count 0) 1 0)))))

(main)
