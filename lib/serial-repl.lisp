;;;; serial-repl.lisp — the BARE-METAL counterpart of lib/cli-toplevel.lisp.
;;;;
;;;; A read-eval-print loop over the real Common Lisp runtime, with the serial
;;;; port as the character device:
;;;;
;;;;   READ   — the CL reader (READ-FROM-STRING over mvm/cl-reader.lisp)
;;;;   EVAL   — EVAL = MVM-EVAL: compile the form to MVM bytecode with the
;;;;            self-hosted compiler, then run it through MVM-INTERPRET
;;;;   PRINT  — WRITE-OBJECT (mvm/prelude.lisp), which writes to serial
;;;;
;;;; The ONLY bespoke part is the character source: READ-CHAR-SERIAL (compiled
;;;; to TRAP #x0301 — poll the COM1 line-status register at 0x3FD, then IN from
;;;; 0x3F8) and WRITE-CHAR-SERIAL (TRAP #x0300 — OUT to 0x3F8).  Everything
;;;; above that is the one Lisp.  This file deliberately does NOT reimplement a
;;;; reader, a printer or an evaluator — that is what mvm/repl-source.lisp (the
;;;; second Lisp, being retired in task #204) did.
;;;;
;;;; A bare-metal build adopts it with one call from kernel-main:
;;;;
;;;;     (cl-serial-repl)
;;;;
;;;; Depends on: read-char-serial, write-char-serial, write-string-serial,
;;;; %make-string-array, %prim-aset, read-from-string, eval, write-object,
;;;; handler-case.  No argv, no filesystem, no syscalls.

;;; ===========================================================================
;;; The input buffer
;;; ===========================================================================
;;;
;;; One reusable string buffer.  Characters are stored as raw CODES with
;;; %PRIM-ASET (public AREF/ASET on a string coerce to/from CHARACTER objects
;;; since e159986; the reader consumes raw codes via %PRIM-AREF), and the
;;; accumulated text is handed to READ-FROM-STRING with :END so no copy is
;;; needed and stale bytes past the form are ignored.

(defvar *serial-repl-buf* nil)
(defvar *serial-repl-len* 0)
(defvar *serial-repl-cap* 0)

(defun %srepl-init-buf ()
  (setq *serial-repl-cap* 16384)
  (setq *serial-repl-buf* (%make-string-array 16384))
  (setq *serial-repl-len* 0))

(defun %srepl-push (code)
  "Append CODE to the buffer (dropping the char if the buffer is full)."
  (if (< *serial-repl-len* *serial-repl-cap*)
      ;; The store is bound to a variable deliberately: a variable-index ASET
      ;; in non-value position can drop its value (MVM active limitation #2).
      (let ((stored (%prim-aset *serial-repl-buf* *serial-repl-len* code)))
        (setq *serial-repl-len* (+ *serial-repl-len* 1))
        stored)
      nil))

(defun %srepl-echo (code)
  "Echo one input character back to the terminal (CR+LF for a newline)."
  (if (= code 10)
      (progn (write-char-serial 13) (write-char-serial 10))
      (write-char-serial code)))

(defun %srepl-rubout ()
  "Erase the last buffered character and rub it out on the terminal."
  (if (> *serial-repl-len* 0)
      (progn
        (setq *serial-repl-len* (- *serial-repl-len* 1))
        (write-char-serial 8)
        (write-char-serial 32)
        (write-char-serial 8))
      nil))

;;; ===========================================================================
;;; Reading one complete top-level form off the wire
;;; ===========================================================================
;;;
;;; A terminal gives us characters, not forms, so the loop has to know when the
;;; user has finished a form: at a newline, with paren depth back to zero, not
;;; inside a string, and with at least one non-whitespace non-comment character
;;; seen.  That makes multi-line forms work (`(defun f (x)` <newline> `  (* x
;;; 2))`) without the reader ever seeing a partial form.  Returns the number of
;;; characters buffered; the text is *SERIAL-REPL-BUF*[0 .. n).

(defun %srepl-read-form-text ()
  (setq *serial-repl-len* 0)
  (let ((depth 0)          ; open parens
        (instr nil)        ; inside a "string"
        (esc nil)          ; previous char was a backslash
        (incom nil)        ; inside a ; comment
        (any nil))         ; saw real content
    (loop
      (let ((c (read-char-serial)))
        ;; Terminals send CR for Return; normalize to LF.
        (when (= c 13) (setq c 10))
        (cond
          ;; Backspace / DEL edit the buffer instead of entering it.
          ((= c 8)   (%srepl-rubout))
          ((= c 127) (%srepl-rubout))
          (t
           (%srepl-echo c)
           (%srepl-push c)
           (cond
             (esc (setq esc nil))
             (incom (when (= c 10) (setq incom nil)))
             (instr
              (cond ((= c 92) (setq esc t))
                    ((= c 34) (setq instr nil))))
             (t
              (cond ((= c 92) (setq esc t) (setq any t))
                    ((= c 34) (setq instr t) (setq any t))
                    ((= c 59) (setq incom t))
                    ((= c 40) (setq depth (+ depth 1)) (setq any t))
                    ((= c 41) (setq depth (- depth 1)))
                    ((= c 32) nil)
                    ((= c 9)  nil)
                    ((= c 10) nil)
                    (t (setq any t)))))
           ;; End of a complete form?
           (when (= c 10)
             (when (if instr nil (if esc nil (if any (<= depth 0) nil)))
               (return *serial-repl-len*)))))))))

;;; ===========================================================================
;;; Eval + print
;;; ===========================================================================

(defun %srepl-report-condition (c)
  (write-string-serial "ERROR: ")
  (setq *write-object-budget* 400)
  (handler-case (write-object c) (t (c2) (write-string-serial "<condition>")))
  (write-char-serial 10))

(defun %srepl-eval-print (form)
  (handler-case
      (let ((v (eval form)))
        (setq *write-object-budget* 100000)
        (write-object v)
        (write-char-serial 10))
    (t (c) (%srepl-report-condition c))))

;;; ===========================================================================
;;; The loop
;;; ===========================================================================

(defun cl-serial-repl ()
  "Interactive CL read-eval-print loop over the serial port.  Never returns."
  (%srepl-init-buf)
  (write-string-serial "Modus CL REPL (bare metal).  EVAL = MVM-EVAL.")
  (write-char-serial 10)
  (loop
    (write-string-serial "> ")
    (let ((n (%srepl-read-form-text)))
      (let ((ok t)
            (form nil))
        (handler-case (setq form (read-from-string *serial-repl-buf* t nil :end n))
          (t (c)
            (setq ok nil)
            (write-string-serial "READ-ERROR")
            (write-char-serial 10)))
        (when ok (%srepl-eval-print form))))))
