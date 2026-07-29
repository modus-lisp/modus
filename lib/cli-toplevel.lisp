;;;; cli-toplevel.lisp — a shared, SBCL-faithful command-line front end for the
;;;; HOSTED (Linux-ELF) Modus images.
;;;;
;;;; North star: an SBCL user can almost drop in Modus for the portable code we
;;;; support — same flags, same left-to-right semantics, same muscle memory.
;;;;
;;;; A hosted build (x64-ql, aarch64-linux, generic) makes this its entry point
;;;; with essentially ONE call:
;;;;
;;;;     (cli-toplevel)
;;;;
;;;; from kernel-main, in place of the old bespoke `%ql-repl' / argv1-load
;;;; driver.  It:
;;;;   1. reads the FULL argv (argc + every argv[]) off the initial process
;;;;      stack (see %cli-collect-argv), exposing it as *POSIX-ARGV* (a list,
;;;;      element 0 = program name, exactly like SBCL);
;;;;   2. parses SBCL toplevel options left-to-right, interleaving --eval/--load;
;;;;   3. loads the userinit rc (~/.modusrc) before an INTERACTIVE repl only;
;;;;   4. either enters the stdin REPL or exits (--script / --non-interactive).
;;;;
;;;; Bare-metal serial / SSH REPLs are OUT of scope: no argv, no filesystem.
;;;;
;;;; Depends on: the full CL runtime (read, read-from-string, load,
;;;; make-string-input-stream, %sys-stat-exists, %make-file-stream-full,
;;;; write-string-serial, write-char-serial, write-object, sys-exit), plus
;;;; mem-ref and gc.lisp's %gc-stack-base (= the initial RSP, saved by the boot
;;;; stub) — used to walk the live initial process stack for argv and envp.

;;; ===========================================================================
;;; argv / envp — read the FULL vector off the initial process stack
;;; ===========================================================================
;;;
;;; The Linux loader hands the entry stub  RSP -> [argc, argv[0], argv[1], ...,
;;; NULL, envp[0], ..., NULL, auxv...].  boot-linux-x64's stub saves that
;;; initial RSP raw to [0x10000058] (stack_base) and argc to [0x10000200].
;;; The stack pages stay mapped for the whole process, so we walk the original
;;; vectors at runtime with mem-ref — NO boot-stub change is needed to get the
;;; FULL argv; only argv[0..2] were ever copied into fixed BSS.
;;;
;;; mem-ref convention (see mvm/gc.lisp): a :u64 LOAD returns the raw 64-bit
;;; word placed into a fixnum whose machine word IS those bits, so the Lisp
;;; VALUE reads back as raw>>1; symmetrically, a mem-ref ADDRESS operand is
;;; SHR'd by one at runtime, so the Lisp value passed as an address must equal
;;; the REAL byte address.  Hence: to read a pointer stored on the stack we
;;; pass the real slot address and DOUBLE the (halved) pointer we get back
;;; before dereferencing it.  %cli-cstr-at reads a NUL-terminated C string at a
;;; REAL byte address into a fresh Lisp string.

(defun %cli-cstr-at (addr)
  "Read a NUL-terminated C string at raw byte ADDR into a fresh Lisp string.
   Returns \"\" for an empty string; the caller guarantees ADDR is non-NULL."
  (let ((len 0))
    ;; measure
    (let ((i 0))
      (loop
        (let ((b (mem-ref (+ addr i) :u8)))
          (when (= b 0) (return nil))
          (setq i (+ i 1)))
        (setq len i)))
    (let ((s (%make-string-array len)) (i 0))
      (loop
        (when (>= i len) (return s))
        (aset s i (mem-ref (+ addr i) :u8))
        (setq i (+ i 1))))))

(defun %cli-argc ()
  "The process argc, stored by the boot stub at 0x10000200."
  (mem-ref #x10000200 :u32))

(defun %cli-argv-base ()
  "Real byte address of argv[0]'s slot = initial-RSP + 8 (RSP holds argc).
   %gc-stack-base returns the raw RSP right-shifted by one (a u64 load stores
   the raw bits into a fixnum whose machine word IS those bits, so the Lisp
   value = raw>>1).  Double it to recover the real RSP, then +8 past argc.
   The result is the REAL byte address, which is exactly what mem-ref's
   address operand wants (mem-ref SHRs the tagged machine word back)."
  (+ (* 2 (%gc-stack-base)) 8))

(defun %cli-collect-argv ()
  "Return the process arguments as a list of strings — element 0 is the
   program name (argv[0]), like SBCL's *posix-argv*.  Reads every argv[i]
   off the live initial stack, so it is not limited to argv[0..2]."
  (let ((argc (%cli-argc))
        (base (%cli-argv-base))
        (acc nil)
        (i 0))
    (loop
      (when (>= i argc) (return (reverse acc)))
      ;; mem-ref :u64 returns the raw char* right-shifted by one (the u64-load
      ;; convention: raw bits land in a fixnum whose value = raw>>1); double it
      ;; back to the REAL byte address for %cli-cstr-at.
      (let ((ptr (mem-ref (+ base (* 8 i)) :u64)))
        (if (eql ptr 0)
            (return (reverse acc))          ; defensive: NULL before argc
            (push (%cli-cstr-at (* 2 ptr)) acc)))
      (setq i (+ i 1)))))

(defun %cli-getenv (name)
  "Look up environment variable NAME on the initial stack's envp vector
   (which immediately follows argv's NULL terminator).  Returns the value
   string, or NIL if unset.  Modus's prelude GETENV always returns NIL; this
   is the hosted-CLI env reader used for HOME (~ expansion of the rc path)."
  (let* ((argc (%cli-argc))
         (base (%cli-argv-base))
         ;; envp starts after argv[0..argc-1] AND the NULL terminator slot.
         (envp (+ base (* 8 (+ argc 1))))
         (prefix (concatenate 'string name "="))
         (plen (length prefix))
         (i 0))
    (loop
      (let ((ptr (mem-ref (+ envp (* 8 i)) :u64)))
        (when (eql ptr 0) (return nil))     ; envp NULL terminator
        (let ((entry (%cli-cstr-at (* 2 ptr))))
          (when (and entry (>= (length entry) plen)
                     (string= (subseq entry 0 plen) prefix))
            (return (subseq entry plen)))))
      (setq i (+ i 1)))))

;;; *POSIX-ARGV* — the SBCL-equivalent global, readable from user code.
(defvar *posix-argv* nil
  "List of the process's command-line arguments, element 0 = program name.
   Populated by CLI-TOPLEVEL at startup.  Mirrors SB-EXT:*POSIX-ARGV*.")

;;; ===========================================================================
;;; small helpers
;;; ===========================================================================

(defun %cli-msg (s)          (write-string-serial s))
(defun %cli-nl ()            (write-char-serial 10))
(defun %cli-line (s)         (write-string-serial s) (write-char-serial 10))

(defun %cli-flag-p (arg name)
  "True if ARG (a string) equals NAME."
  (and (stringp arg) (string= arg name)))

(defun %cli-home-rc-path (userinit)
  "The rc file path: USERINIT if non-NIL, else $HOME/.modusrc (or ./.modusrc
   if HOME is unavailable)."
  (if userinit
      userinit
      (let ((home (%cli-getenv "HOME")))
        (if (and home (> (length home) 0))
            (concatenate 'string home "/.modusrc")
            ".modusrc"))))

(defun %cli-eval-string (str)
  "Read and evaluate every form in STR (via a string input stream so we get
   correct multi-form advancement), printing nothing (SBCL's --eval is
   silent unless the form itself prints)."
  (let ((in (make-string-input-stream str))
        (eof (list 'eof)))
    (loop
      (let ((form (read in nil eof)))
        (when (eq form eof) (return nil))
        (eval form)))))

(defun %cli-load-file (path)
  "Load PATH (a Lisp source file) via the runtime LOAD (mvm-eval)."
  (load path))

;;; ===========================================================================
;;; version / help banners
;;; ===========================================================================

(defun %cli-print-version ()
  (%cli-line "Modus 0.1 (hosted CLI; self-hosted MVM, mvm-eval)"))

(defun %cli-print-help ()
  (%cli-line "Usage: modus [options] [--end-toplevel-options args...]")
  (%cli-line "")
  (%cli-line "Toplevel options (processed left-to-right):")
  (%cli-line "  --eval FORM / -e FORM   read and evaluate FORM (repeatable)")
  (%cli-line "  --load FILE / -l FILE   load FILE (repeatable; interleaves with --eval)")
  (%cli-line "  --script [FILE]         run FILE (or stdin), no rc, no banner, then exit")
  (%cli-line "  --non-interactive       exit after --eval/--load instead of a REPL")
  (%cli-line "  --quit                  synonym for --non-interactive")
  (%cli-line "  --noinform              suppress the startup banner")
  (%cli-line "  --userinit FILE         load FILE as the user init file (rc)")
  (%cli-line "  --no-userinit           do not load any user init file")
  (%cli-line "  --sysinit FILE          load FILE as the system init file")
  (%cli-line "  --no-sysinit            do not load any system init file")
  (%cli-line "  --version               print version and exit")
  (%cli-line "  --help                  print this help and exit")
  (%cli-line "  --end-toplevel-options  stop parsing; remaining args go to *posix-argv*")
  (%cli-line "")
  (%cli-line "With no options, Modus enters an interactive REPL on stdin."))

(defun %cli-unknown-flag (flag)
  "Report an unknown --flag and exit nonzero, like SBCL."
  (%cli-msg "Modus: unknown toplevel option: ")
  (%cli-line flag)
  (%cli-line "Try --help for a list of options.")
  (sys-exit 1))

;;; ===========================================================================
;;; the REPL (moved here from ql-shim's %ql-repl so it is shared)
;;; ===========================================================================

(defun %cli-repl (banner-p)
  "Interactive read-eval-print loop over stdin (fd 0).  When BANNER-P, print
   the startup banner first.  Ctrl-D (EOF) exits."
  (when banner-p
    (%cli-line "Modus REPL (hosted).  Ctrl-D to exit."))
  (let ((in (%make-file-stream-full 0 0))
        (eof (list 'eof)))
    (loop
      (%cli-msg "> ")
      (let ((form (handler-case (read in nil eof)
                    (t (c)
                      (%cli-line "READ-ERROR")
                      eof))))
        (when (eq form eof) (%cli-nl) (return-from %cli-repl nil))
        (handler-case
            (let ((v (eval form)))
              (write-object v) (%cli-nl))
          (t (c)
            (%cli-msg "ERROR: ")
            (handler-case (write-object c) (t (c2) (%cli-msg "<condition>")))
            (%cli-nl)))))))

;;; ===========================================================================
;;; --script mode
;;; ===========================================================================

(defun %cli-run-script (path)
  "Non-interactive script run.  PATH is the file to load, or NIL / \"-\" for
   stdin.  No rc, no banner.  An uncaught error → exit(1).  Clean → exit(0)."
  (handler-case
      (progn
        (if (or (null path) (string= path "-"))
            ;; stdin: read+eval every form from fd 0 until EOF.
            (let ((in (%make-file-stream-full 0 0))
                  (eof (list 'eof)))
              (loop
                (let ((form (read in nil eof)))
                  (when (eq form eof) (return nil))
                  (eval form))))
            (load path))
        (sys-exit 0))
    (t (c)
      (%cli-msg "Modus: unhandled error in --script")
      (%cli-nl)
      (handler-case (progn (%cli-msg "  ") (write-object c) (%cli-nl))
        (t (c2) nil))
      (sys-exit 1))))

;;; ===========================================================================
;;; the toplevel driver
;;; ===========================================================================
;;;
;;; State collected while parsing (no structs to keep the compiled shape
;;; simple): a reversed list of deferred (:eval . STR) / (:load . PATH) actions
;;; run in left-to-right order, plus the interactive/banner/rc decisions.

(defun %cli-run-actions (actions)
  "Run the deferred --eval/--load ACTIONS in order.  Each is (KIND . ARG)."
  (dolist (a actions)
    (cond
      ((eq (car a) :eval) (%cli-eval-string (cdr a)))
      ((eq (car a) :load) (%cli-load-file (cdr a))))))

(defun cli-toplevel ()
  "The shared hosted-CLI entry point.  Reads argv off the stack, parses
   SBCL-style toplevel options left-to-right, loads the userinit rc before an
   interactive REPL, and either runs the REPL or exits.  Call this from
   kernel-main."
  ;; Expose the full argv as *posix-argv* (element 0 = program name).
  (setq *posix-argv* (%cli-collect-argv))
  (let* ((all *posix-argv*)
         (args (cdr all))                 ; drop program name
         (actions nil)                    ; reversed list of (:eval/:load . x)
         (script-mode nil)
         (script-file nil)
         (interactive t)                  ; enter REPL unless told otherwise
         (banner t)
         (userinit nil)                   ; explicit --userinit path
         (load-rc t)                      ; load ~/.modusrc before interactive
         (rest-args nil))                 ; args after --end-toplevel-options
    ;; ---- parse loop -------------------------------------------------------
    (loop
      (when (null args) (return nil))
      (let ((arg (car args)))
        (cond
          ;; --eval / -e FORM
          ((or (%cli-flag-p arg "--eval") (%cli-flag-p arg "-e"))
           (setq args (cdr args))
           (when (null args)
             (%cli-line "Modus: --eval requires an argument")
             (sys-exit 1))
           (push (cons :eval (car args)) actions)
           (setq args (cdr args)))
          ;; --load / -l FILE
          ((or (%cli-flag-p arg "--load") (%cli-flag-p arg "-l"))
           (setq args (cdr args))
           (when (null args)
             (%cli-line "Modus: --load requires an argument")
             (sys-exit 1))
           (push (cons :load (car args)) actions)
           (setq args (cdr args)))
          ;; --script [FILE]
          ((%cli-flag-p arg "--script")
           (setq script-mode t
                 banner nil
                 load-rc nil
                 interactive nil)
           (setq args (cdr args))
           ;; Optional FILE argument: the next token, unless it starts a flag.
           (when (and args (not (and (> (length (car args)) 0)
                                     (char= (char (car args) 0) #\-))))
             (setq script-file (car args))
             (setq args (cdr args)))
           ;; SBCL: --script's remaining args become script args.  We stop
           ;; option parsing here (the script owns the rest).
           (setq rest-args args)
           (setq args nil))
          ;; --non-interactive / --quit
          ((or (%cli-flag-p arg "--non-interactive") (%cli-flag-p arg "--quit"))
           (setq interactive nil
                 load-rc nil)
           (setq args (cdr args)))
          ;; --noinform
          ((%cli-flag-p arg "--noinform")
           (setq banner nil)
           (setq args (cdr args)))
          ;; --userinit FILE
          ((%cli-flag-p arg "--userinit")
           (setq args (cdr args))
           (when (null args)
             (%cli-line "Modus: --userinit requires an argument")
             (sys-exit 1))
           (setq userinit (car args))
           (setq args (cdr args)))
          ;; --no-userinit
          ((%cli-flag-p arg "--no-userinit")
           (setq load-rc nil)
           (setq args (cdr args)))
          ;; --sysinit FILE — accepted; loaded if present (best-effort).
          ((%cli-flag-p arg "--sysinit")
           (setq args (cdr args))
           (when (null args)
             (%cli-line "Modus: --sysinit requires an argument")
             (sys-exit 1))
           ;; Defer as a load action so it interleaves correctly.
           (push (cons :load (car args)) actions)
           (setq args (cdr args)))
          ;; --no-sysinit — accepted, no-op (Modus has no system rc).
          ((%cli-flag-p arg "--no-sysinit")
           (setq args (cdr args)))
          ;; --version
          ((%cli-flag-p arg "--version")
           (%cli-print-version)
           (sys-exit 0))
          ;; --help
          ((%cli-flag-p arg "--help")
           (%cli-print-help)
           (sys-exit 0))
          ;; --end-toplevel-options / bare --
          ((or (%cli-flag-p arg "--end-toplevel-options")
               (%cli-flag-p arg "--"))
           (setq rest-args (cdr args))
           (setq args nil))
          ;; Unknown --flag → error + nonzero exit.
          ((and (> (length arg) 1) (char= (char arg 0) #\-)
                (not (string= arg "-")))
           (%cli-unknown-flag arg))
          ;; A bare non-flag token: SBCL treats a leftover token as the start
          ;; of the trailing args (implicit end of options).
          (t
           (setq rest-args args)
           (setq args nil)))))
    ;; ---- fold trailing args into *posix-argv* ----------------------------
    ;; SBCL's *posix-argv* keeps program name + the args AFTER the toplevel
    ;; options.  Rebuild it as (program-name . rest-args).
    (setq *posix-argv* (cons (car all) rest-args))
    ;; ---- dispatch ---------------------------------------------------------
    (cond
      ;; --script: honour any preceding --eval/--load (left-to-right), then
      ;; run the file/stdin and exit (no rc, no banner).
      (script-mode
       (%cli-run-actions (reverse actions))
       (%cli-run-script script-file))     ; exits
      ;; Otherwise: run deferred actions, maybe rc, then REPL or exit.
      (t
       (handler-case (%cli-run-actions (reverse actions))
         (t (c)
           ;; An error in --eval/--load is reported; in non-interactive mode
           ;; it is fatal (SBCL exits nonzero), interactively we continue.
           (%cli-msg "Modus: error during --eval/--load: ")
           (handler-case (write-object c) (t (c2) (%cli-msg "<condition>")))
           (%cli-nl)
           (unless interactive (sys-exit 1))))
       (cond
         (interactive
          ;; userinit rc, only interactively and only when not disabled.
          (when load-rc
            (let ((rc (%cli-home-rc-path userinit)))
              (when (%sys-stat-exists rc)
                (handler-case (load rc)
                  (t (c)
                    (%cli-msg "Modus: error loading rc file ")
                    (%cli-line rc))))))
          (handler-case (%cli-repl banner) (t (c) nil))
          (sys-exit 0))
         (t
          ;; --non-interactive / --quit with no script: exit cleanly.
          (sys-exit 0)))))))
