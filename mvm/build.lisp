;;;; build.lisp — ONE build entry point for the whole matrix.
;;;;
;;;; The matrix is  arch × mode × board × payload:
;;;;
;;;;   arch     x64 | aarch64 | i386
;;;;   mode     hosted (Linux ELF)  |  bare (kernel image)
;;;;   board    -    (hosted)       |  qemu | rpi | t420   (bare)
;;;;   payload  cl-repl | cl-net | cl-ssh | cli | selfhost | ...
;;;;
;;;; Usage:
;;;;   sbcl --script mvm/build.lisp --list
;;;;   sbcl --script mvm/build.lisp x64/bare/qemu/cl-repl
;;;;   sbcl --script mvm/build.lisp aarch64/bare/qemu/cl-ssh
;;;;
;;;; Every cell delegates to the build-*.lisp named in its row, with the
;;;; cell's environment (*CELL-ENV*) applied first.  This table used to build
;;;; the mini-Lisp (mvm/repl-source.lisp) images itself; those are gone
;;;; (2026-10-28) -- see docs/mini-lisp-archive.md for what replaced each one
;;;; and the tag that still holds them.

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))

(in-package :cl-user)

;;; ------------------------------------------------------------------
;;; The matrix.
;;;
;;; Each row:  key  arch mode board payload  status target translator output legacy
;;;
;;;   status     :legacy (every cell now delegates; kept so rows stay positional)
;;;   target     the BUILD-IMAGE :target keyword
;;;   translator :x64 | :aarch64 | :i386
;;;   output     image path
;;;   legacy     the build-*.lisp the cell delegates to.
;;; OUTPUT PATHS ARE TRANSCRIBED FROM THE SCRIPTS, not from the docs.  An
;;; audit of all 28 rows found SEVEN wrong — they had been written from
;;; BUILDS.md and the run scripts rather than from what each build actually
;;; writes.  A wrong path here is not cosmetic now that this table is the
;;; interface: `run.sh` boots the path this column names.
;;;
;;; ------------------------------------------------------------------
(defparameter *matrix*
  '(;; ---------------- hosted (Linux ELF) ----------------
    ("x64/hosted/-/cli"       :x64     :hosted :-    :cli
     :legacy nil :x64     "/tmp/modus"                     "build-generic-cli")
    ("aarch64/hosted/-/cli"   :aarch64 :hosted :-    :cli
     :legacy nil :aarch64 "/home/claude/modus-aa64-cli"    "build-aarch64-cli")
    ("i386/hosted/-/cli"      :i386    :hosted :-    :cli
     :legacy nil :i386    "/tmp/modus-i386-cli"            "build-i386-cli")
    ("x64/hosted/-/selfhost"  :x64     :hosted :-    :selfhost
     :legacy nil :x64     "/tmp/modus-selfhost"            "build-modus-selfhost")

    ;; ---------------- bare / qemu ----------------

    ("x64/bare/qemu/cl-repl"  :x64     :bare   :qemu :cl-repl
     :legacy nil :x64     "/tmp/modus-x64-cl-repl.bin"     "build-x64-cl-repl")
    ("i386/bare/qemu/cl-repl" :i386    :bare   :qemu :cl-repl
     :legacy nil :i386    "/tmp/modus-i386-cl-repl.bin"    "build-i386-cl-repl")
    ;; SSH from boot on the PC image (*CELL-ENV*): what the retired mini-Lisp
    ;; x64/bare/qemu/ssh cell was for.
    ("x64/bare/qemu/cl-ssh"   :x64     :bare   :qemu :cl-ssh
     :legacy nil :x64     "/tmp/modus-x64-cl-ssh.bin"      "build-x64-cl-repl")
    ;; ... and the 32-bit one, over the NE2000 (was i386/bare/qemu/ssh).
    ("i386/bare/qemu/cl-ssh"  :i386    :bare   :qemu :cl-ssh
     :legacy nil :i386    "/tmp/modus-i386-cl-ssh.bin"     "build-i386-cl-repl")
    ;; The CLEAN QEMU-virt counterpart of the cl-repl cell below: the SAME
    ;; CL/mvm image (build-cl-repl-common.lisp), booted through
    ;; boot/boot-aarch64.lisp's fixpoint MMU descriptor instead of the Pi's, and
    ;; networked over PCI/E1000 instead of DWC2/USB.  Bakes NO test corpus —
    ;; that is the `ansi' cell directly above, which is a different lineage.
    ("aarch64/bare/qemu/cl-repl" :aarch64 :bare :qemu :cl-repl
     :legacy nil :aarch64 "/tmp/modus-aarch64-cl-repl.bin" "build-aarch64")
    ;; Same script with MODUS_NET_BUILD=1 — see *CELL-ENV*, and see the
    ;; aarch64/bare/rpi/cl-net comment for what the flag turns on.
    ("aarch64/bare/qemu/cl-net" :aarch64 :bare :qemu :cl-net
     :legacy nil :aarch64 "/tmp/modus-aarch64-cl-net.bin" "build-aarch64")
    ;; The SSH server on the same image, started from kernel-main (*CELL-ENV*):
    ;; what the retired mini-Lisp aarch64/bare/qemu/ssh cell was for.
    ("aarch64/bare/qemu/cl-ssh" :aarch64 :bare :qemu :cl-ssh
     :legacy nil :aarch64 "/tmp/modus-aarch64-cl-ssh.bin" "build-aarch64")

    ;; ---------------- bare / rpi ----------------
    ;; #209 rung 1 LANDED: the RPi family's migration onto the CL/mvm image.
    ;; This cell boots on QEMU raspi3b, passes E2SMOKE, and gives a working
    ;; CL REPL (mvm-eval) over PL011 — see build-rpi-cl-repl.lisp.
    ("aarch64/bare/rpi/cl-repl" :aarch64 :bare :rpi :cl-repl
     :legacy nil :aarch64 "/tmp/piboot/kernel8.img"        "build-rpi-cl-repl")
    ;; #209 rung 2: the SAME script with MODUS_NET_BUILD=1, which appends the
    ;; Pi USB net stack (arch-rpi-cl + DWC2 + USB + CDC Ethernet + IP + HTTP
    ;; client) and drives DHCP -> TCP -> HTTP GET from kernel-main before the
    ;; REPL starts.  Flag-off it is byte-identical to the cl-repl cell above,
    ;; so this is a variant row, not a second image lineage.  See *CELL-ENV*.
    ;;
    ;; #263 rung 3 (the PUSH GATE) rides the SAME cell: the flag also bakes in
    ;; lib/tar.lisp + lib/install-tarball.lisp, and the boot pipeline now
    ;; INSTALLS the fetched archive (untar -> .asd -> :components -> read+eval
    ;; each file) and CALLS a function from it.  Verified on QEMU raspi3b with
    ;; the bundled systems/sha1.tar:
    ;;   (sha1:sha1-hex "abc") => "A9993E364706816ABA3E25717850C26C9CD0D89D"
    ;; MODUS_NET_URL picks the tarball (serve a PLAIN .tar — there is no chipz
    ;; in this image, so the gunzip branch is an unresolved sentinel);
    ;; MODUS_LIB_EXPR picks the expression evaluated after the load; and
    ;; MODUS_NET_BUFSZ raises the 32 KB HTTP response cap for a bigger library
    ;; (alexandria's .tar is 276480 bytes and needs e.g. 400000).
    ("aarch64/bare/rpi/cl-net" :aarch64 :bare :rpi :cl-net
     :legacy nil :aarch64 "/tmp/piboot/kernel8-net.img"    "build-rpi-cl-repl")

    ;; ---------------- bare / t420 (real hardware) ----------------
    ("x64/bare/t420/uefi-cl" :x64      :bare   :t420 :uefi-cl
     :legacy nil :x64     "/tmp/modus-uefi-cl.efi"         "build-uefi-cl-repl")))

;;; Payloads that are NOT in the arch × mode × board matrix the project
;;; targets (arm32 is a 4th arch; fixpoint is superseded by #210).  Kept
;;; reachable so nothing silently disappears, but listed apart.
(defparameter *outside-matrix*
  '(("arm32/bare/qemu/cl-repl" "build-arm32-cl-repl" "4th arch — the CL image on QEMU virt (MMU-remapped)")))

(defun row-key (r) (first r))
(defun row-status (r) (sixth r))
(defun row-target (r) (seventh r))
(defun row-translator (r) (eighth r))
(defun row-output (r) (ninth r))
(defun row-legacy (r) (tenth r))

(defun print-matrix ()
  (format t "~&Modus build matrix — sbcl --script mvm/build.lisp <key>~%~%")
  (format t "  ~32A ~40A ~A~%" "KEY" "IMAGE" "FROM")
  (format t "  ~32A ~40A ~A~%"
          "--------------------------------"
          "----------------------------------------" "----")
  (dolist (r *matrix*)
    (format t "  ~32A ~40A ~A~%" (row-key r) (row-output r) (row-legacy r)))
  (format t "~%")
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
;;; REAL image rather than a bring-up rung.
;;;
;;; MODUS_I386_LAYER is GONE (2026-08 CLI convergence): build-i386-cli.lisp is
;;; now a thin arch tail over build-cli-common.lisp and bakes one fixed source
;;; set, exactly like the x64 and aarch64 wrappers, so there is no rung to
;;; select.  What is left here is only the output paths.
(defparameter *cell-env*
  '(("i386/hosted/-/cli"
     ("MODUS_I386_OUT"    . "/tmp/modus-i386-cli")
     ("MODUS_I386_SYMMAP" . "/tmp/modus-i386-cli.symmap"))
    ;; #209 rung 2.  build-rpi-cl-repl.lisp builds the plain rung-1 REPL unless
    ;; MODUS_NET_BUILD=1, so this cell is the only thing that distinguishes the
    ;; net image from the cl-repl one — including its output path, or the two
    ;; cells would overwrite each other at /tmp/piboot/kernel8.img.
    ;; MODUS_NET_URL selects what the boot pipeline fetches.
    ("aarch64/bare/rpi/cl-net"
     ("MODUS_NET_BUILD"   . "1")
     ("MODUS_CL_REPL_OUT" . "/tmp/piboot/kernel8-net.img"))
    ;; The QEMU-virt sibling of the row above.  MODUS_NET_BUFSZ is NOT optional
    ;; for a real library: %net-resp-cap is baked at BUILD time and past the cap
    ;; tcp-rx-copy silently DROPS bytes while the log still prints the full
    ;; "FETCHED bytes=" length, so an 88%-zero-filled tarball presents as a wild
    ;; -pointer data abort deep inside arithmetic.  400000 covers alexandria.
    ("aarch64/bare/qemu/cl-net"
     ("MODUS_NET_BUILD"   . "1")
     ("MODUS_NET_BUFSZ"   . "400000")
     ("MODUS_CL_REPL_OUT" . "/tmp/modus-aarch64-cl-net.bin"))
    ;; SSH from boot: net pipeline (no library fetch), then (ssh-boot).
    ("x64/bare/qemu/cl-ssh"
     ("MODUS_NET_BUILD"     . "1")
     ("MODUS_SSH_BUILD"     . "1")
     ("MODUS_SSH_AUTOSTART" . "1")
     ("MODUS_CL_REPL_OUT"   . "/tmp/modus-x64-cl-ssh.bin"))
    ("i386/bare/qemu/cl-ssh"
     ("MODUS_NET_BUILD"     . "1")
     ("MODUS_SSH_BUILD"     . "1")
     ("MODUS_SSH_AUTOSTART" . "1")
     ("MODUS_CL_REPL_OUT"   . "/tmp/modus-i386-cl-ssh.bin"))
    ("aarch64/bare/qemu/cl-ssh"
     ("MODUS_NET_BUILD"     . "1")
     ("MODUS_SSH_BUILD"     . "1")
     ("MODUS_SSH_AUTOSTART" . "1")
     ("MODUS_CL_REPL_OUT"   . "/tmp/modus-aarch64-cl-ssh.bin"))))

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
    (format t "~&build.lisp: delegating to ~A~%" script)
    (load path)))

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
         (row (run-legacy (row-legacy row) (row-key row)))
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
