;;;; build.lisp — ONE build entry point for the whole matrix.
;;;;
;;;; The matrix is  arch × mode × board × payload:
;;;;
;;;;   arch     x64 | aarch64 | i386
;;;;   mode     hosted (Linux ELF)  |  bare (kernel image)
;;;;   board    -    (hosted)       |  qemu | rpi | t420   (bare)
;;;;   payload  repl | ssh | cli | ansi | selfhost | ...
;;;;
;;;; Usage:
;;;;   sbcl --script mvm/build.lisp --list
;;;;   sbcl --script mvm/build.lisp x64/bare/qemu/repl
;;;;   sbcl --script mvm/build.lisp aarch64/bare/rpi/ssh
;;;;   sbcl --script mvm/build.lisp x64/hosted/-/ansi
;;;;
;;;; WHY THIS EXISTS
;;;;
;;;; There were 36 build-*.lisp scripts.  Most of the small ones are the SAME
;;;; script with four values changed — compare build-{x64,aarch64,rpi}-repl:
;;;; they differ only in (1) which translator to install, (2) the :TARGET
;;;; keyword, (3) the output path, (4) nothing else.  That is table data
;;;; wearing a filename.
;;;;
;;;; MIGRATION STANCE (deliberate, see the `legacy' slot below)
;;;;
;;;; Every cell in the matrix is reachable from here TODAY.  A cell is either
;;;;   :native — built from this table, no legacy script involved; or
;;;;   :legacy — delegated to its build-*.lisp, unchanged.
;;;; So this is one interface immediately, and the 36 scripts retire one at a
;;;; time as their cell is migrated and proven byte-identical.  No legacy
;;;; build is deleted before its replacement demonstrably produces the same
;;;; image — that is the whole reason the `legacy' slot exists rather than a
;;;; big-bang rewrite.

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))

(in-package :cl-user)

;;; ------------------------------------------------------------------
;;; The matrix.
;;;
;;; Each row:  key  arch mode board payload  status target translator output legacy
;;;
;;;   status     :native (built here) | :legacy (delegate)
;;;   target     the BUILD-IMAGE :target keyword
;;;   translator :x64 | :aarch64 | :i386  (which install-*-translator to call)
;;;   output     image path
;;;   legacy     for a :LEGACY cell, the build-*.lisp it delegates to.
;;;              for a :NATIVE cell whose script has been RETIRED, the old
;;;              filename is kept as a RESOLVABLE ALIAS (find-row matches it)
;;;              so muscle memory and old docs keep working — but there is no
;;;              such file on disk any more.  Retired so far:
;;;              build-x64-repl, build-aarch64-repl, build-rpi-repl.
;;; ------------------------------------------------------------------
(defparameter *matrix*
  '(;; ---------------- hosted (Linux ELF) ----------------
    ("x64/hosted/-/cli"       :x64     :hosted :-    :cli
     :legacy nil :x64     "/tmp/modus"                     "build-generic-cli")
    ("aarch64/hosted/-/cli"   :aarch64 :hosted :-    :cli
     :legacy nil :aarch64 "/tmp/modus-aarch64-cli"         "build-aarch64-cli")
    ("i386/hosted/-/cli"      :i386    :hosted :-    :cli
     :legacy nil :i386    "/tmp/modus-i386-cli"            "build-i386-cli")
    ("x64/hosted/-/ansi"      :x64     :hosted :-    :ansi
     :legacy nil :x64     "/home/claude/modus/tmp/modus-ansi-test" "build-x64-linux")
    ("aarch64/hosted/-/ansi"  :aarch64 :hosted :-    :ansi
     :legacy nil :aarch64 "/tmp/modus-aarch64-ansi"        "build-aarch64-linux")
    ("x64/hosted/-/selfhost"  :x64     :hosted :-    :selfhost
     :legacy nil :x64     "/tmp/modus-selfhost"            "build-modus-selfhost")
    ("x64/hosted/-/generic"   :x64     :hosted :-    :generic
     :legacy nil :x64     "/tmp/modus-generic"             "build-generic")
    ("x64/hosted/-/mvm"       :x64     :hosted :-    :mvm
     :legacy nil :x64     "/tmp/modus-mvm"                 "build-mvm")
    ("x64/hosted/-/bench"     :x64     :hosted :-    :bench
     :legacy nil :x64     "/tmp/modus-bench-x64.bin"       "build-bench")

    ;; ---------------- bare / qemu ----------------
    ;; The four REPL cells are MIGRATED: pure table data, no per-cell code.
    ("x64/bare/qemu/repl"     :x64     :bare   :qemu :repl
     :native :x86-64  :x64     "/tmp/modus-x64.bin"        "build-x64-repl")
    ("aarch64/bare/qemu/repl" :aarch64 :bare   :qemu :repl
     :native :aarch64 :aarch64 "/tmp/modus-aarch64.bin"    "build-aarch64-repl")
    ("i386/bare/qemu/repl"    :i386    :bare   :qemu :repl
     :legacy :i386    :i386    "/tmp/modus-i386.bin"       "build-i386-repl")

    ("x64/bare/qemu/cl-repl"  :x64     :bare   :qemu :cl-repl
     :legacy nil :x64     "/tmp/modus-x64-cl.bin"          "build-x64-cl-repl")
    ("x64/bare/qemu/ssh"      :x64     :bare   :qemu :ssh
     :legacy nil :x64     "/tmp/modus-x64-ssh.bin"         "build-x64-ssh")
    ("aarch64/bare/qemu/ssh"  :aarch64 :bare   :qemu :ssh
     :legacy nil :aarch64 "/tmp/modus-aarch64-ssh.bin"     "build-aarch64-ssh")
    ("aarch64/bare/qemu/actors" :aarch64 :bare :qemu :actors
     :legacy nil :aarch64 "/tmp/modus-aarch64-actors.bin"  "build-aarch64-actors")
    ("aarch64/bare/qemu/isolated" :aarch64 :bare :qemu :isolated
     :legacy nil :aarch64 "/tmp/modus-aarch64-isolated.bin" "build-aarch64-isolated")
    ("i386/bare/qemu/ssh"     :i386    :bare   :qemu :ssh
     :legacy nil :i386    "/tmp/modus-i386-ssh.bin"        "build-i386-ssh")
    ("x64/bare/qemu/ansi"     :x64     :bare   :qemu :ansi
     :legacy nil :x64     "/tmp/modus-x64-ansi.bin"        "build-x64")
    ("aarch64/bare/qemu/ansi" :aarch64 :bare   :qemu :ansi
     :legacy nil :aarch64 "/tmp/modus-aarch64-ansi.bin"    "build-aarch64")

    ;; ---------------- bare / rpi ----------------
    ;; NOTE: the whole RPi cell is BROKEN — one shared type error
    ;; (#S(A64-BUFFER) vs MVM-BUFFER) inside BUILD-IMAGE :TARGET :RPI.
    ;; Plan is migration to the CL/mvm image (#209), not repair.
    ("aarch64/bare/rpi/repl"  :aarch64 :bare   :rpi  :repl
     :native :rpi     :aarch64 "/tmp/kernel8.img"          "build-rpi-repl")
    ("aarch64/bare/rpi/ssh"   :aarch64 :bare   :rpi  :ssh
     :legacy nil :aarch64 "/tmp/kernel8-ssh.img"           "build-rpi-ssh")
    ("aarch64/bare/rpi/hid"   :aarch64 :bare   :rpi  :hid
     :legacy nil :aarch64 "/tmp/kernel8-hid.img"           "build-rpi-hid")
    ("aarch64/bare/rpi/periph" :aarch64 :bare  :rpi  :periph
     :legacy nil :aarch64 "/tmp/kernel8.img"               "build-rpi-periph")
    ("aarch64/bare/rpi/pizero-ssh" :aarch64 :bare :rpi :pizero-ssh
     :legacy nil :aarch64 "/tmp/piboot/kernel8.img"        "build-pizero2w-ssh")
    ("aarch64/bare/rpi/pizero-actors" :aarch64 :bare :rpi :pizero-actors
     :legacy nil :aarch64 "/tmp/piboot/kernel8.img"        "build-pizero2w-actors")

    ;; ---------------- bare / t420 (real hardware) ----------------
    ("x64/bare/t420/uefi-repl" :x64    :bare   :t420 :uefi-repl
     :legacy nil :x64     "/tmp/modus-uefi.efi"            "build-uefi-repl")
    ("i386/bare/t420/diag-ssh" :i386   :bare   :t420 :diag-ssh
     :legacy nil :i386    "/tmp/modus-i386-diag-ssh.img"   "build-i386-diag-ssh")))

;;; Payloads that are NOT in the arch × mode × board matrix the project
;;; targets (arm32 is a 4th arch; fixpoint is superseded by #210).  Kept
;;; reachable so nothing silently disappears, but listed apart.
(defparameter *outside-matrix*
  '(("arm32/bare/qemu/repl" "build-arm32-repl"  "4th arch — not in the x64/aarch64/i386 matrix")
    ("arm32/bare/qemu/ssh"  "build-arm32-ssh"   "4th arch — not in the x64/aarch64/i386 matrix")
    ("multi/fixpoint"       "build-fixpoint"    "BROKEN; superseded by #210 cross-arch fixpoint")
    ("aarch64/bare/rpi/uart-bootloader" "build-uart-bootloader" "deploy tool, not an image")
    ("aarch64/bare/rpi/pizero-hdmi"     "build-pizero2w-hdmi"   "board bring-up variant")
    ("x64/bare/qemu/console-repl"       "build-x64-console-repl" "second-Lisp variant; #204 retires it")
    ("x64/hosted/-/compiler-test"       "build-compiler-test"    "test harness, not an image")))

(defun row-key (r) (first r))
(defun row-status (r) (sixth r))
(defun row-target (r) (seventh r))
(defun row-translator (r) (eighth r))
(defun row-output (r) (ninth r))
(defun row-legacy (r) (tenth r))

(defun print-matrix ()
  (format t "~&Modus build matrix — sbcl --script mvm/build.lisp <key>~%~%")
  (format t "  ~32A ~7A ~40A ~A~%" "KEY" "STATUS" "IMAGE" "FROM")
  (format t "  ~32A ~7A ~40A ~A~%"
          "--------------------------------" "-------"
          "----------------------------------------" "----")
  (dolist (r *matrix*)
    (format t "  ~32A ~7A ~40A ~A~%"
            (row-key r)
            (if (eq (row-status r) :native) "native" "legacy")
            (row-output r)
            (row-legacy r)))
  (format t "~%  native = built from this table.  legacy = delegates to the~%")
  (format t "  named build-*.lisp, unchanged, until its cell is migrated.~%~%")
  (format t "  Outside the x64/aarch64/i386 matrix:~%")
  (dolist (r *outside-matrix*)
    (format t "    ~34A ~24A ~A~%" (first r) (second r) (third r)))
  (format t "~%"))

(defun find-row (key)
  (or (find key *matrix* :key #'row-key :test #'string-equal)
      ;; accept a legacy script name too, so muscle memory keeps working
      (find key *matrix* :key #'row-legacy :test #'string-equal)))

(defun find-outside (key)
  "An *OUTSIDE-MATRIX* entry, by its key or by its build-*.lisp name.
   These are reachable from here so that EVERY build in the tree has one
   entry point, even the ones outside the x64/aarch64/i386 matrix — a
   caller (scripts/run.sh) should never have to know which table a build
   lives in."
  (or (find key *outside-matrix* :key #'first  :test #'string-equal)
      (find key *outside-matrix* :key #'second :test #'string-equal)))

;;; Per-cell environment.  A legacy script may need env vars to build the
;;; REAL image rather than a bring-up rung — see the i386 entry, where the
;;; script's own default is MODUS_I386_LAYER=1 (prelude only, no CL bridge
;;; at all).  Putting the layer here means the matrix cell means what it
;;; says: "i386/hosted/-/cli" builds the i386 CLI, not rung 1 of its
;;; bring-up ladder.
(defparameter *cell-env*
  '(("i386/hosted/-/cli"
     ("MODUS_I386_LAYER"  . "5")
     ("MODUS_I386_OUT"    . "/tmp/modus-i386-cli")
     ("MODUS_I386_SYMMAP" . "/tmp/modus-i386-cli.symmap"))))

(defun apply-cell-env (key)
  (let ((entry (assoc key *cell-env* :test #'string-equal)))
    (when entry
      (require :sb-posix)
      (dolist (kv (cdr entry))
        ;; Do NOT clobber an explicit override from the caller.
        (unless (sb-ext:posix-getenv (car kv))
          (funcall (intern "SETENV" :sb-posix) (car kv) (cdr kv) 1)
          (format t "~&build.lisp: ~A=~A~%" (car kv) (cdr kv)))))))

(defun run-legacy (script &optional key)
  "Delegate to a build-*.lisp, unchanged."
  (let ((path (merge-pathnames (concatenate 'string script ".lisp")
                               (directory-namestring (truename *load-truename*)))))
    (unless (probe-file path)
      (format *error-output* "build.lisp: no such build script: ~A~%" path)
      (sb-ext:exit :code 2))
    (when key (apply-cell-env key))
    (format t "~&build.lisp: delegating to ~A (cell not yet migrated)~%" script)
    (load path)))

(defun install-translator (which)
  (ecase which
    (:x64     (funcall (intern "INSTALL-X64-TRANSLATOR" :modus.mvm.x64)))
    (:aarch64 (funcall (intern "INSTALL-AARCH64-TRANSLATOR" :modus.mvm)))
    (:i386    (funcall (intern "INSTALL-I386-TRANSLATOR" :modus.mvm)))))

(defun payload-source (payload)
  "Source text for a MIGRATED payload.  Only payloads that are pure table
   data live here; anything that composes net/ files or splices an inline
   kernel-main stays with its legacy script until migrated deliberately."
  (ecase payload
    (:repl (symbol-value (intern "*REPL-SOURCE*" :modus.mvm)))))

(defun build-cell (row)
  (let ((build-image  (intern "BUILD-IMAGE" :modus.mvm))
        (write-kernel (intern "WRITE-KERNEL-IMAGE" :modus.mvm)))
    (funcall (intern "MVM-LOAD" :cl-user) "mvm/repl-source.lisp")
    (install-translator (row-translator row))
    (format t "~&build.lisp: ~A -> ~A~%" (row-key row) (row-output row))
    (let ((image (funcall build-image
                          :target (row-target row)
                          :source-text (payload-source (fifth row)))))
      (funcall write-kernel image (row-output row))
      (format t "build.lisp: wrote ~A~%" (row-output row)))))

;;; ------------------------------------------------------------------
(let ((args (cdr sb-ext:*posix-argv*)))
  (cond
    ((or (null args) (member (first args) '("--list" "-l" "list" "--help" "-h")
                             :test #'string=))
     (print-matrix))
    (t
     (let* ((key (first args))
            (row (find-row key)))
       (cond
         (row
          (if (eq (row-status row) :native)
              (build-cell row)
              (run-legacy (row-legacy row) (row-key row))))
         ;; Outside the arch matrix (arm32, fixpoint, console-repl, …) but
         ;; still reachable from the one entry point.
         ((find-outside key)
          (let ((e (find-outside key)))
            (format t "~&build.lisp: ~A — ~A~%" (first e) (third e))
            (run-legacy (second e))))
         (t
          (format *error-output* "build.lisp: unknown cell '~A'~%~%" key)
          (print-matrix)
          (sb-ext:exit :code 2)))))))
