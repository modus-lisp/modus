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
;;;; There were 36 build-*.lisp scripts; 28 remain.  Most of the small ones
;;;; are the SAME
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
;;;              build-x64-repl, build-aarch64-repl, build-rpi-repl,
;;;              build-x64-ssh, build-aarch64-ssh, build-aarch64-actors,
;;;              build-aarch64-isolated, build-i386-ssh.   (36 -> 28)
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
    ;; The five SSH/actors cells are MIGRATED — see *COMPOSITES* below.
    ("x64/bare/qemu/ssh"      :x64     :bare   :qemu :ssh
     :native :x86-64  :x64     "/tmp/modus-x64-ssh.bin"    "build-x64-ssh")
    ("aarch64/bare/qemu/ssh"  :aarch64 :bare   :qemu :ssh
     :native :aarch64 :aarch64 "/tmp/modus-aarch64-ssh.bin" "build-aarch64-ssh")
    ("aarch64/bare/qemu/actors" :aarch64 :bare :qemu :actors
     :native :aarch64 :aarch64 "/tmp/modus-aarch64-actors.bin" "build-aarch64-actors")
    ("aarch64/bare/qemu/isolated" :aarch64 :bare :qemu :isolated
     :native :aarch64 :aarch64 "/tmp/modus-aarch64-isolated.bin" "build-aarch64-isolated")
    ("i386/bare/qemu/ssh"     :i386    :bare   :qemu :ssh
     :native :i386    :i386    "/tmp/modus-i386-ssh.bin"   "build-i386-ssh")
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
    ;; NOTE: the i386 translator lives in its OWN package (translate-i386.lisp
    ;; does `(in-package :modus.mvm.i386)`), not in :modus.mvm — interning the
    ;; name in :modus.mvm yields an unbound symbol and a FUNCALL error.
    (:i386    (funcall (intern "INSTALL-I386-TRANSLATOR" :modus.mvm.i386)))))

;;; ------------------------------------------------------------------
;;; COMPOSITE payloads — net/ sources + REPL + an inline kernel-main.
;;;
;;; The REPL cells were pure table data (translator, :TARGET, output).  The
;;; SSH/actors cells are not: each one composes a per-arch list of net/ files
;;; (ORDER IS SEMANTIC — last-defun-wins, so `crypto-w32` after `crypto`
;;; genuinely means "override"), splices an arch-specific kernel-main written
;;; as Lisp source lines, and may set a translator flag.  Rather than force
;;; that into the 4-value shape, a cell may name a COMPOSITE spec here:
;;;
;;;   :net    net/ file names, in load order, verbatim from the old script
;;;   :main   kernel-main source lines (the old script's `ssh-main' list)
;;;   :extra  a further raw source chunk appended after :MAIN (i386 only)
;;;   :parts  the concatenation order of :NET / :REPL / :MAIN / :EXTRA.
;;;           This differs per cell and is load-bearing: the single-threaded
;;;           SSH builds put kernel-main LAST (last-defun-wins makes it the
;;;           entry point), while the actor builds put it FIRST.
;;;   :flags  MODUS.MVM specials to set after the translator is installed
;;;
;;; The kernel-main line lists below are transcribed verbatim (mechanically,
;;; not retyped) from the scripts they replace, and each cell is gated on a
;;; byte-identical image — see GATE-RESULT-ssh-cells.md.
;;; ------------------------------------------------------------------

(defun read-file-text (path)
  "Read entire file as a string."
  (with-open-file (s path :direction :input)
    (let ((text (make-string (file-length s))))
      (let ((n (read-sequence text s)))
        (subseq text 0 n)))))

(defparameter *x64-ssh-main*
  '(
                         ;; Fix ssh-receive-version: (when 0 ...) is truthy on x64 (nil=0xDEAD0001)
                         "(defun ssh-receive-version (ssh)"
                         "  (let ((got-version 0))"
                         "    (let ((tries 0))"
                         "      (loop"
                         "        (when (not (zerop got-version)) (return 1))"
                         "        (when (> tries 50) (return 0))"
                         "        (let ((msg (receive)))"
                         "          (when (zerop msg) (return 0)))"
                         "        (let ((blen (mem-ref (+ ssh #x6D4) :u32)))"
                         "          (when (> blen 8)"
                         "            (let ((buf-base (+ ssh #x6D8)))"
                         "              (when (eq (mem-ref buf-base :u8) 83)"
                         "                (when (eq (mem-ref (+ buf-base 1) :u8) 83)"
                         "                  (when (eq (mem-ref (+ buf-base 2) :u8) 72)"
                         "                    (let ((end 0))"
                         "                      (let ((i 3))"
                         "                        (loop"
                         "                          (when (not (zerop end)) (return 0))"
                         "                          (when (> i blen) (return 0))"
                         "                          (when (eq (mem-ref (+ buf-base i) :u8) 10)"
                         "                            (setq end i))"
                         "                          (setq i (+ i 1))))"
                         "                      (when (not (zerop end))"
                         "                        (let ((vlen end))"
                         "                          (when (> end 0)"
                         "                            (when (eq (mem-ref (+ buf-base (- end 1)) :u8) 13)"
                         "                              (setq vlen (- end 1))))"
                         "                          (let ((ver-base (+ ssh #x650)))"
                         "                            (dotimes (j vlen)"
                         "                              (setf (mem-ref (+ ver-base j) :u8)"
                         "                                    (mem-ref (+ buf-base j) :u8))))"
                         "                          (setf (mem-ref (+ ssh #x6D0) :u32) vlen)"
                         "                          (ssh-buf-consume ssh (+ end 1))"
                         "                          (setq got-version 1))))))))))"
                         "        (setq tries (+ tries 1))))))"
                         ;; Override ssh-copy-host-key: use pre-computed keys from state
                         "(defun ssh-copy-host-key (conn)"
                         "  (let ((ssh (conn-ssh conn)))"
                         "    (let ((state (e1000-state-base)))"
                         "      (let ((src-priv (+ state #x710)))"
                         "        (let ((dst-priv (+ ssh #x110)))"
                         "          (dotimes (i 32)"
                         "            (setf (mem-ref (+ dst-priv i) :u8)"
                         "                  (mem-ref (+ src-priv i) :u8)))))"
                         "      (let ((src-pub (+ state #x730)))"
                         "        (let ((dst-pub (+ ssh #x130)))"
                         "          (dotimes (i 32)"
                         "            (setf (mem-ref (+ dst-pub i) :u8)"
                         "                  (mem-ref (+ src-pub i) :u8))))))))"
                         ;; No-op usb-keepalive for x86 (no USB, E1000 NIC)
                         "(defun usb-keepalive () 0)"
                         ;; Override ssh-handle-kex: safe version with pre-computed keys
                         "(defun ssh-handle-kex (ssh kex-init-payload kex-init-len)"
                         "  (let ((cli-eph (make-array 32)))"
                         "    (let ((ci 0))"
                         "      (loop"
                         "        (when (>= ci 32) (return 0))"
                         "        (let ((src-idx (+ 5 ci)))"
                         "          (let ((val (aref kex-init-payload src-idx)))"
                         "            (let ((dummy (aset cli-eph ci val))) dummy)))"
                         "        (setq ci (+ ci 1))))"
                         "    (let ((state (e1000-state-base)))"
                         "      (let ((srv-priv (make-array 32)))"
                         "        (safe-copy-mem-to-arr srv-priv (+ state #x6C4) 32)"
                         "        (let ((srv-eph (make-array 32)))"
                         "          (safe-copy-mem-to-arr srv-eph (+ state #x6E4) 32)"
                         "          (let ((shared (x25519 srv-priv cli-eph)))"
                         "            (ssh-mem-store (+ ssh #x070) shared 32)"
                         "            (let ((h (ssh-compute-exchange-hash ssh cli-eph srv-eph shared)))"
                         "              (ssh-mem-store (+ ssh #x050) h 32)"
                         "              (when (zerop (mem-ref ssh :u32))"
                         "                (ssh-mem-store (+ ssh #x030) h 32)"
                         "                (setf (mem-ref ssh :u32) 1))"
                         "              (let ((sig (ed25519-sign-fast h 32)))"
                         "                (ssh-send-kex-reply ssh sig srv-eph)))))))))"
                         ;; Override ssh-handle-connection: nil-safe checks (nil != 0 on x64)
                         "(defun ssh-handle-connection (ssh)"
                         "  (let ((cb (- ssh #x20)))"
                         "    (ssh-send-version ssh)"
                         "    (when (zerop (ssh-receive-version ssh)) (return ()))"
                         "    (let ((kexinit (ssh-build-kexinit ssh)))"
                         "      (ssh-send-payload ssh kexinit (array-length kexinit)))"
                         "    (let ((cli-kex (ssh-receive-packet ssh 50000)))"
                         "      (when (not cli-kex) (return ()))"
                         "      (let ((cli-kex-payload (car cli-kex)))"
                         "        (let ((cli-kex-len (cdr cli-kex)))"
                         "          (when (not (eq (aref cli-kex-payload 0) 20)) (return ()))"
                         "          (ssh-mem-store (+ cb #x1F00) cli-kex-payload cli-kex-len)"
                         "          (setf (mem-ref (+ ssh #x20) :u32) cli-kex-len)"
                         "          (let ((kex-init (ssh-receive-packet ssh 50000)))"
                         "            (when (not kex-init) (return ()))"
                         "            (let ((kex-payload (car kex-init)))"
                         "              (when (not (eq (aref kex-payload 0) 30)) (return ()))"
                         "              (ssh-handle-kex ssh kex-payload (cdr kex-init))"
                         "              (ssh-send-newkeys ssh)"
                         "              (let ((nk (ssh-receive-packet ssh 50000)))"
                         "                (when (not nk) (return ()))"
                         "                (when (not (eq (aref (car nk) 0) 21)) (return ()))"
                         "                (ssh-derive-keys ssh)"
                         "                (ssh-message-loop ssh)))))))))"
                         ;; Split kernel-main into phases to stay under ~25 sequential forms limit
                         "(defun km-init-crypto ()"
                         "  (write-byte 91) (write-byte 49) (write-byte 93)"
                         "  (sha256-init)"
                         "  (write-byte 91) (write-byte 50) (write-byte 93)"
                         "  (sha512-init)"
                         "  (write-byte 91) (write-byte 51) (write-byte 93)"
                         "  (ed25519-init)"
                         "  (write-byte 91) (write-byte 52) (write-byte 93)"
                         "  (ssh-seed-random) 0)"
                         "(defun km-init-net ()"
                         "  (write-byte 91) (write-byte 53) (write-byte 93)"
                         "  (dhcp-client)"
                         "  (write-byte 91) (write-byte 54) (write-byte 93)"
                         "  (ssh-seed-random)"
                         "  (ssh-init-strings) 0)"
                         "(defun km-set-host-key ()"
                         "  (let ((state (e1000-state-base)))"
                         "    (setf (mem-ref (+ state #x710) :u64) 0)"
                         "    (setf (mem-ref (+ state #x718) :u64) 0)"
                         "    (setf (mem-ref (+ state #x720) :u64) 0)"
                         "    (setf (mem-ref (+ state #x728) :u64) 0)"
                         "    (setf (mem-ref (+ state #x730) :u32) #xBC276A3B)"
                         "    (setf (mem-ref (+ state #x734) :u32) #x2DA4B6CE)"
                         "    (setf (mem-ref (+ state #x738) :u32) #xD0A8A362)"
                         "    (setf (mem-ref (+ state #x73C) :u32) #x730D6F2A)"
                         "    (setf (mem-ref (+ state #x740) :u32) #x77153265)"
                         "    (setf (mem-ref (+ state #x744) :u32) #xA643E21D)"
                         "    (setf (mem-ref (+ state #x748) :u32) #xA148C03A)"
                         "    (setf (mem-ref (+ state #x74C) :u32) #x29DA598B)"
                         "    (setf (mem-ref (+ state #x624) :u32) 1)) 0)"
                         ;; kernel-main: init hardware + crypto, then delegate to ssh-server
                         "(defun kernel-main ()"
                         "  (write-byte 90) (write-byte 90) (write-byte 90) (write-byte 10)"
                         "  (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 0)"
                         "  (pci-assign-bars)"
                         "  (e1000-probe)"
                         "  (km-init-crypto)"
                         "  (km-init-net)"
                         "  (setup-irq)"
                         "  (setf (mem-ref #x600010 :u32) 1)"
                         "  (km-set-host-key)"
                         "  (pre-compute-host-sign)"
                         "  (pre-compute-server-eph (conn-ssh 0))"
                         "  (ssh-server 22))"
    ))

(defparameter *aarch64-ssh-main*
  '(
                         ;; Split kernel-main to stay under ~25 sequential forms per function
                         "(defun km-init-crypto ()"
                         "  (write-byte 91) (write-byte 49) (write-byte 93) (sha256-init)"
                         "  (write-byte 91) (write-byte 50) (write-byte 93) (sha512-init)"
                         "  (write-byte 91) (write-byte 51) (write-byte 93) (ed25519-init)"
                         "  (write-byte 91) (write-byte 52) (write-byte 93) (ssh-seed-random) 0)"
                         "(defun km-init-net ()"
                         "  (write-byte 91) (write-byte 53) (write-byte 93) (dhcp-client)"
                         "  (write-byte 91) (write-byte 54) (write-byte 93) (ssh-seed-random)"
                         "  (ssh-init-strings) 0)"
                         "(defun km-set-host-key ()"
                         "  (let ((state (e1000-state-base)))"
                         "    (setf (mem-ref (+ state #x710) :u64) 0)"
                         "    (setf (mem-ref (+ state #x718) :u64) 0)"
                         "    (setf (mem-ref (+ state #x720) :u64) 0)"
                         "    (setf (mem-ref (+ state #x728) :u64) 0)"
                         "    (setf (mem-ref (+ state #x730) :u32) #xBC276A3B)"
                         "    (setf (mem-ref (+ state #x734) :u32) #x2DA4B6CE)"
                         "    (setf (mem-ref (+ state #x738) :u32) #xD0A8A362)"
                         "    (setf (mem-ref (+ state #x73C) :u32) #x730D6F2A)"
                         "    (setf (mem-ref (+ state #x740) :u32) #x77153265)"
                         "    (setf (mem-ref (+ state #x744) :u32) #xA643E21D)"
                         "    (setf (mem-ref (+ state #x748) :u32) #xA148C03A)"
                         "    (setf (mem-ref (+ state #x74C) :u32) #x29DA598B)"
                         "    (setf (mem-ref (+ state #x624) :u32) 1)) 0)"
                         "(defun km-set-eph-priv ()"
                         "  (let ((state (e1000-state-base)))"
                         "    (setf (mem-ref (+ state #x6C4) :u8) #x00)"
                         "    (dotimes (i 30)"
                         "      (setf (mem-ref (+ state (+ #x6C5 i)) :u8) #x01))"
                         "    (setf (mem-ref (+ state #x6E3) :u8) #x41)) 0)"
                         ;; Use :u32 writes for ephemeral public key (8 writes instead of 32)
                         "(defun km-set-eph-pub ()"
                         "  (let ((state (e1000-state-base)))"
                         "    (setf (mem-ref (+ state #x6E4) :u32) #x9292E0A4)"
                         "    (setf (mem-ref (+ state #x6E8) :u32) #x78C251B6)"
                         "    (setf (mem-ref (+ state #x6EC) :u32) #x562C77B9)"
                         "    (setf (mem-ref (+ state #x6F0) :u32) #xBBA95F9F)"
                         "    (setf (mem-ref (+ state #x6F4) :u32) #xB406D913)"
                         "    (setf (mem-ref (+ state #x6F8) :u32) #x9D8CB66A)"
                         "    (setf (mem-ref (+ state #x6FC) :u32) #x442BDCF9)"
                         "    (setf (mem-ref (+ state #x700) :u32) #x09A2F809)) 0)"
                         "(defun km-init-conns ()"
                         "  (setf (mem-ref (+ (ssh-ipc-base) #x60438) :u32) 22)"
                         "  (let ((i 0))"
                         "    (loop"
                         "      (when (>= i 4) (return 0))"
                         "      (setf (mem-ref (conn-base i) :u32) 0)"
                         "      (setq i (+ i 1)))) 0)"
                         "(defun kernel-main ()"
                         "  (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 0)"
                         "  (pci-assign-bars)"
                         "  (e1000-probe)"
                         "  (km-init-crypto)"
                         "  (km-init-net)"
                         "  (km-set-host-key)"
                         "  (pre-compute-host-sign)"
                         "  (write-byte 83) (write-byte 83) (write-byte 72)"
                         "  (write-byte 58) (print-dec 22) (write-byte 10)"
                         "  (km-init-conns)"
                         "  (km-set-eph-priv) (km-set-eph-pub)"
                         "  (enable-gic-timer)"
                         "  (net-actor-main))"
    ))

(defparameter *aarch64-actor-main*
  '(
                         "(defun kernel-main ()"
                         "  (smp-init)"
                         "  (actor-init)"
                         ;; Clear eval globals pointer — uninitialized RAM causes hang
                         "  (setf (mem-ref (+ (ssh-ipc-base) #x60000) :u64) 0)"
                         "  (pci-assign-bars)"
                         "  (e1000-probe)"
                         "  (write-byte 91) (write-byte 49) (write-byte 93)"
                         "  (sha256-init)"
                         "  (write-byte 91) (write-byte 50) (write-byte 93)"
                         "  (sha512-init)"
                         "  (write-byte 91) (write-byte 51) (write-byte 93)"
                         "  (ed25519-init)"
                         "  (write-byte 91) (write-byte 52) (write-byte 93)"
                         "  (ssh-seed-random)"
                         "  (write-byte 91) (write-byte 53) (write-byte 93)"
                         "  (dhcp-client)"
                         "  (write-byte 91) (write-byte 54) (write-byte 93)"
                         "  (ssh-seed-random)"
                         "  (ssh-init-strings)"
                         ;; Embed pre-computed Ed25519 host key
                         "  (let ((state (e1000-state-base)))"
                         "    (setf (mem-ref (+ state #x710) :u64) 0)"
                         "    (setf (mem-ref (+ state #x718) :u64) 0)"
                         "    (setf (mem-ref (+ state #x720) :u64) 0)"
                         "    (setf (mem-ref (+ state #x728) :u64) 0)"
                         "    (setf (mem-ref (+ state #x730) :u32) #xBC276A3B)"
                         "    (setf (mem-ref (+ state #x734) :u32) #x2DA4B6CE)"
                         "    (setf (mem-ref (+ state #x738) :u32) #xD0A8A362)"
                         "    (setf (mem-ref (+ state #x73C) :u32) #x730D6F2A)"
                         "    (setf (mem-ref (+ state #x740) :u32) #x77153265)"
                         "    (setf (mem-ref (+ state #x744) :u32) #xA643E21D)"
                         "    (setf (mem-ref (+ state #x748) :u32) #xA148C03A)"
                         "    (setf (mem-ref (+ state #x74C) :u32) #x29DA598B)"
                         "    (setf (mem-ref (+ state #x624) :u32) 1))"
                         ;; Pre-compute ed25519 host key derivatives (s, prefix)
                         "  (pre-compute-host-sign)"
                         "  (write-byte 83) (write-byte 83) (write-byte 72)"
                         "  (write-byte 58) (print-dec 22) (write-byte 10)"
                         "  (setf (mem-ref (+ (ssh-ipc-base) #x60438) :u32) 22)"
                         "  (let ((i 0))"
                         "    (loop"
                         "      (when (>= i 4) (return 0))"
                         "      (setf (mem-ref (conn-base i) :u32) 0)"
                         "      (setq i (+ i 1))))"
                         ;; Pre-compute server ephemeral X25519 key pair
                         "  (pre-compute-server-eph (conn-ssh 0))"
                         ;; Spawn net-actor-main as actor 2
                         "  (actor-spawn (fn-addr net-actor-main))"
                         ;; Primordial actor: idle yield loop
                         "  (loop (yield)))"
    ))

(defparameter *i386-ssh-main*
  '(
                         "(defun kernel-main ()"
                         "  (e1000-probe)"
                         "  (sha256-init)"
                         "  (sha512-init)"
                         "  (ed25519-init)"
                         "  (ssh-seed-random)"
                         "  (dhcp-client)"
                         "  (ssh-seed-random)"
                         "  (ssh-init-strings)"
                         ;; Embed pre-computed Ed25519 host key
                         ;; Private key = 32 zero bytes
                         "  (let ((state (e1000-state-base)))"
                         "    (dotimes (i 32)"
                         "      (setf (mem-ref (+ state (+ #x710 i)) :u8) 0))"
                         ;; Public key stored byte-by-byte (fixnum-safe)
                         ;; 3B 6A 27 BC  CE B6 A4 2D  62 A3 A8 D0  2A 6F 0D 73
                         ;; 65 32 15 77  1D E2 43 A6  3A C0 48 A1  8B 59 DA 29
                         "    (setf (mem-ref (+ state #x730) :u8) #x3B)"
                         "    (setf (mem-ref (+ state #x731) :u8) #x6A)"
                         "    (setf (mem-ref (+ state #x732) :u8) #x27)"
                         "    (setf (mem-ref (+ state #x733) :u8) #xBC)"
                         "    (setf (mem-ref (+ state #x734) :u8) #xCE)"
                         "    (setf (mem-ref (+ state #x735) :u8) #xB6)"
                         "    (setf (mem-ref (+ state #x736) :u8) #xA4)"
                         "    (setf (mem-ref (+ state #x737) :u8) #x2D)"
                         "    (setf (mem-ref (+ state #x738) :u8) #x62)"
                         "    (setf (mem-ref (+ state #x739) :u8) #xA3)"
                         "    (setf (mem-ref (+ state #x73A) :u8) #xA8)"
                         "    (setf (mem-ref (+ state #x73B) :u8) #xD0)"
                         "    (setf (mem-ref (+ state #x73C) :u8) #x2A)"
                         "    (setf (mem-ref (+ state #x73D) :u8) #x6F)"
                         "    (setf (mem-ref (+ state #x73E) :u8) #x0D)"
                         "    (setf (mem-ref (+ state #x73F) :u8) #x73)"
                         "    (setf (mem-ref (+ state #x740) :u8) #x65)"
                         "    (setf (mem-ref (+ state #x741) :u8) #x32)"
                         "    (setf (mem-ref (+ state #x742) :u8) #x15)"
                         "    (setf (mem-ref (+ state #x743) :u8) #x77)"
                         "    (setf (mem-ref (+ state #x744) :u8) #x1D)"
                         "    (setf (mem-ref (+ state #x745) :u8) #xE2)"
                         "    (setf (mem-ref (+ state #x746) :u8) #x43)"
                         "    (setf (mem-ref (+ state #x747) :u8) #xA6)"
                         "    (setf (mem-ref (+ state #x748) :u8) #x3A)"
                         "    (setf (mem-ref (+ state #x749) :u8) #xC0)"
                         "    (setf (mem-ref (+ state #x74A) :u8) #x48)"
                         "    (setf (mem-ref (+ state #x74B) :u8) #xA1)"
                         "    (setf (mem-ref (+ state #x74C) :u8) #x8B)"
                         "    (setf (mem-ref (+ state #x74D) :u8) #x59)"
                         "    (setf (mem-ref (+ state #x74E) :u8) #xDA)"
                         "    (setf (mem-ref (+ state #x74F) :u8) #x29)"
                         "    (setf (mem-ref (+ state #x624) :u32) 1))"
                         ;; Pre-computed Ed25519 host key derivatives
                         ;; SHA-512(zeros32) = 5046adc1...420b6dd3
                         ;; s (clamped): byte[0]&=F8, byte[31]=(byte[31]&7F)|40
                         "  (let ((state (e1000-state-base)))"
                         ;; s at state+0x680 (32 bytes)
                         "    (setf (mem-ref (+ state #x680) :u8) #x50)"
                         "    (setf (mem-ref (+ state #x681) :u8) #x46)"
                         "    (setf (mem-ref (+ state #x682) :u8) #xAD)"
                         "    (setf (mem-ref (+ state #x683) :u8) #xC1)"
                         "    (setf (mem-ref (+ state #x684) :u8) #xDB)"
                         "    (setf (mem-ref (+ state #x685) :u8) #xA8)"
                         "    (setf (mem-ref (+ state #x686) :u8) #x38)"
                         "    (setf (mem-ref (+ state #x687) :u8) #x86)"
                         "    (setf (mem-ref (+ state #x688) :u8) #x7B)"
                         "    (setf (mem-ref (+ state #x689) :u8) #x2B)"
                         "    (setf (mem-ref (+ state #x68A) :u8) #xBB)"
                         "    (setf (mem-ref (+ state #x68B) :u8) #xFD)"
                         "    (setf (mem-ref (+ state #x68C) :u8) #xD0)"
                         "    (setf (mem-ref (+ state #x68D) :u8) #xC3)"
                         "    (setf (mem-ref (+ state #x68E) :u8) #x42)"
                         "    (setf (mem-ref (+ state #x68F) :u8) #x3E)"
                         "    (setf (mem-ref (+ state #x690) :u8) #x58)"
                         "    (setf (mem-ref (+ state #x691) :u8) #xB5)"
                         "    (setf (mem-ref (+ state #x692) :u8) #x79)"
                         "    (setf (mem-ref (+ state #x693) :u8) #x70)"
                         "    (setf (mem-ref (+ state #x694) :u8) #xB5)"
                         "    (setf (mem-ref (+ state #x695) :u8) #x26)"
                         "    (setf (mem-ref (+ state #x696) :u8) #x7A)"
                         "    (setf (mem-ref (+ state #x697) :u8) #x90)"
                         "    (setf (mem-ref (+ state #x698) :u8) #xF5)"
                         "    (setf (mem-ref (+ state #x699) :u8) #x79)"
                         "    (setf (mem-ref (+ state #x69A) :u8) #x60)"
                         "    (setf (mem-ref (+ state #x69B) :u8) #x92)"
                         "    (setf (mem-ref (+ state #x69C) :u8) #x4A)"
                         "    (setf (mem-ref (+ state #x69D) :u8) #x87)"
                         "    (setf (mem-ref (+ state #x69E) :u8) #xF1)"
                         "    (setf (mem-ref (+ state #x69F) :u8) #x56)"
                         ;; prefix at state+0x6A0 (32 bytes)
                         "    (setf (mem-ref (+ state #x6A0) :u8) #x0A)"
                         "    (setf (mem-ref (+ state #x6A1) :u8) #x6A)"
                         "    (setf (mem-ref (+ state #x6A2) :u8) #x85)"
                         "    (setf (mem-ref (+ state #x6A3) :u8) #xEA)"
                         "    (setf (mem-ref (+ state #x6A4) :u8) #xA6)"
                         "    (setf (mem-ref (+ state #x6A5) :u8) #x42)"
                         "    (setf (mem-ref (+ state #x6A6) :u8) #xDA)"
                         "    (setf (mem-ref (+ state #x6A7) :u8) #xC8)"
                         "    (setf (mem-ref (+ state #x6A8) :u8) #x35)"
                         "    (setf (mem-ref (+ state #x6A9) :u8) #x42)"
                         "    (setf (mem-ref (+ state #x6AA) :u8) #x4B)"
                         "    (setf (mem-ref (+ state #x6AB) :u8) #x5D)"
                         "    (setf (mem-ref (+ state #x6AC) :u8) #x7C)"
                         "    (setf (mem-ref (+ state #x6AD) :u8) #x8D)"
                         "    (setf (mem-ref (+ state #x6AE) :u8) #x63)"
                         "    (setf (mem-ref (+ state #x6AF) :u8) #x7C)"
                         "    (setf (mem-ref (+ state #x6B0) :u8) #x00)"
                         "    (setf (mem-ref (+ state #x6B1) :u8) #x40)"
                         "    (setf (mem-ref (+ state #x6B2) :u8) #x8C)"
                         "    (setf (mem-ref (+ state #x6B3) :u8) #x7A)"
                         "    (setf (mem-ref (+ state #x6B4) :u8) #x73)"
                         "    (setf (mem-ref (+ state #x6B5) :u8) #xDA)"
                         "    (setf (mem-ref (+ state #x6B6) :u8) #x67)"
                         "    (setf (mem-ref (+ state #x6B7) :u8) #x2B)"
                         "    (setf (mem-ref (+ state #x6B8) :u8) #x7F)"
                         "    (setf (mem-ref (+ state #x6B9) :u8) #x49)"
                         "    (setf (mem-ref (+ state #x6BA) :u8) #x85)"
                         "    (setf (mem-ref (+ state #x6BB) :u8) #x21)"
                         "    (setf (mem-ref (+ state #x6BC) :u8) #x42)"
                         "    (setf (mem-ref (+ state #x6BD) :u8) #x0B)"
                         "    (setf (mem-ref (+ state #x6BE) :u8) #x6D)"
                         "    (setf (mem-ref (+ state #x6BF) :u8) #xD3)"
                         ;; Mark as pre-computed
                         "    (setf (mem-ref (+ state #x6C0) :u32) 1))"
                         ;; Set SSH port
                         "  (setf (mem-ref (+ (ssh-ipc-base) #x60438) :u32) 22)"
                         "  (let ((i 0))"
                         "    (loop"
                         "      (when (>= i 4) (return 0))"
                         "      (setf (mem-ref (conn-base i) :u32) 0)"
                         "      (setq i (+ i 1))))"
                         ;; Pre-computed server ephemeral X25519 key pair
                         ;; Private (clamped): 00 01 01 .. 01 41
                         ;; Public: A4 E0 92 92 B6 51 C2 78 B9 77 2C 56 9F 5F A9 BB
                         ;;         13 D9 06 B4 6A B6 8C 9D F9 DC 2B 44 09 F8 A2 09
                         "  (let ((state (e1000-state-base)))"
                         "    (setf (mem-ref (+ state #x6C4) :u8) #x00)"
                         "    (dotimes (i 30)"
                         "      (setf (mem-ref (+ state (+ #x6C5 i)) :u8) #x01))"
                         "    (setf (mem-ref (+ state #x6E3) :u8) #x41)"
                         "    (setf (mem-ref (+ state #x6E4) :u8) #xA4)"
                         "    (setf (mem-ref (+ state #x6E5) :u8) #xE0)"
                         "    (setf (mem-ref (+ state #x6E6) :u8) #x92)"
                         "    (setf (mem-ref (+ state #x6E7) :u8) #x92)"
                         "    (setf (mem-ref (+ state #x6E8) :u8) #xB6)"
                         "    (setf (mem-ref (+ state #x6E9) :u8) #x51)"
                         "    (setf (mem-ref (+ state #x6EA) :u8) #xC2)"
                         "    (setf (mem-ref (+ state #x6EB) :u8) #x78)"
                         "    (setf (mem-ref (+ state #x6EC) :u8) #xB9)"
                         "    (setf (mem-ref (+ state #x6ED) :u8) #x77)"
                         "    (setf (mem-ref (+ state #x6EE) :u8) #x2C)"
                         "    (setf (mem-ref (+ state #x6EF) :u8) #x56)"
                         "    (setf (mem-ref (+ state #x6F0) :u8) #x9F)"
                         "    (setf (mem-ref (+ state #x6F1) :u8) #x5F)"
                         "    (setf (mem-ref (+ state #x6F2) :u8) #xA9)"
                         "    (setf (mem-ref (+ state #x6F3) :u8) #xBB)"
                         "    (setf (mem-ref (+ state #x6F4) :u8) #x13)"
                         "    (setf (mem-ref (+ state #x6F5) :u8) #xD9)"
                         "    (setf (mem-ref (+ state #x6F6) :u8) #x06)"
                         "    (setf (mem-ref (+ state #x6F7) :u8) #xB4)"
                         "    (setf (mem-ref (+ state #x6F8) :u8) #x6A)"
                         "    (setf (mem-ref (+ state #x6F9) :u8) #xB6)"
                         "    (setf (mem-ref (+ state #x6FA) :u8) #x8C)"
                         "    (setf (mem-ref (+ state #x6FB) :u8) #x9D)"
                         "    (setf (mem-ref (+ state #x6FC) :u8) #xF9)"
                         "    (setf (mem-ref (+ state #x6FD) :u8) #xDC)"
                         "    (setf (mem-ref (+ state #x6FE) :u8) #x2B)"
                         "    (setf (mem-ref (+ state #x6FF) :u8) #x44)"
                         "    (setf (mem-ref (+ state #x700) :u8) #x09)"
                         "    (setf (mem-ref (+ state #x701) :u8) #xF8)"
                         "    (setf (mem-ref (+ state #x702) :u8) #xA2)"
                         "    (setf (mem-ref (+ state #x703) :u8) #x09))"
                         ;; Enable PIT timer for HLT-based io-delay (after all crypto init)
                         "  (enable-pit-timer)"
                         "  (net-actor-main))"
                         ;; Override net-actor-main to not call yield (unresolved on i386)
                         "(defun net-actor-main ()"
                         "  (loop"
                         "    (io-delay)"
                         "    (let ((pkt-len (e1000-receive)))"
                         "      (when (not (zerop pkt-len))"
                         "        (let ((buf (e1000-rx-buf)))"
                         "          (let ((et-hi (mem-ref (+ buf 12) :u8)))"
                         "            (when (eq et-hi #x08)"
                         "              (let ((et-lo (mem-ref (+ buf 13) :u8)))"
                         "                (if (eq et-lo #x06)"
                         "                    (let ((arp-op (buf-read-u16-mem buf 20)))"
                         "                      (when (eq arp-op 1) (arp-reply buf)))"
                         "                    (when (eq et-lo 0)"
                         "                      (let ((st (e1000-state-base)))"
                         "                        (dotimes (m 6)"
                         "                          (setf (mem-ref (+ st (+ #x28 m)) :u8)"
                         "                                (mem-ref (+ buf (+ 6 m)) :u8))))"
                         "                      (let ((proto (mem-ref (+ buf 23) :u8)))"
                         "                        (if (eq proto 17)"
                         "                            (udp-receive buf pkt-len)"
                         "                            (when (eq proto 6)"
                         "                              (net-handle-tcp buf pkt-len))))))))))))))"
    ))

;;; The i386 SSH image appends one more override chunk AFTER kernel-main:
;;; ssh-handle-connection with a higher receive timeout (i386 crypto is slow).
(defparameter *i386-ssh-overrides* "
(defun ssh-handle-connection (ssh)
  (let ((cb (- ssh #x20)))
    (ssh-send-version ssh)
    (when (zerop (ssh-receive-version ssh))
      (return ()))
    (let ((kexinit (ssh-build-kexinit ssh)))
      (ssh-send-payload ssh kexinit (array-length kexinit)))
    (let ((cli-kex (ssh-receive-packet ssh 500)))
      (when (zerop cli-kex) (return ()))
      (let ((cli-kex-payload (car cli-kex)))
        (when (not (eq (aref cli-kex-payload 0) 20)) (return ()))
        (ssh-mem-store (+ cb #x1F00) cli-kex-payload (cdr cli-kex))
        (setf (mem-ref (+ ssh #x20) :u32) (cdr cli-kex))
        (let ((kex-init (ssh-receive-packet ssh 500)))
          (when (zerop kex-init) (return ()))
          (let ((kex-payload (car kex-init)))
            (when (not (eq (aref kex-payload 0) 30)) (return ()))
            (ssh-handle-kex ssh kex-payload (cdr kex-init))
            (ssh-send-newkeys ssh)
            (let ((nk (ssh-receive-packet ssh 500)))
              (when (zerop nk) (return ()))
              (when (not (eq (aref (car nk) 0) 21)) (return ()))
              (ssh-derive-keys ssh)
              (ssh-message-loop ssh))))))))
")

(defparameter *composites*
  `(("x64/bare/qemu/ssh"
     ;; arch-x86 → e1000 → ip → crypto → ssh → aarch64-overrides →
     ;; mvm-ssh-fixes.  mvm-ssh-fixes MUST load last (last-defun-wins): it
     ;; carries the variable-index ASET/AREF workarounds and the arena-based
     ;; fe-pow-sqrt/ed-recover-x.
     :net ("arch-x86.lisp" "e1000.lisp" "ip.lisp" "crypto.lisp" "ssh.lisp"
           "aarch64-overrides.lisp" "mvm-ssh-fixes.lisp")
     :parts (:net :repl :main)
     :main ,*x64-ssh-main*)

    ("aarch64/bare/qemu/ssh"
     :net ("arch-aarch64.lisp" "e1000.lisp" "ip.lisp" "crypto.lisp"
           "crypto-fast.lisp" "ssh.lisp" "ssh-profile.lisp"
           "aarch64-overrides.lisp")
     :parts (:net :repl :main)
     ;; GICv2 + virtual timer init for setup-irq
     :flags (("*AARCH64-SETUP-IRQ-ENABLE*" . t))
     :main ,*aarch64-ssh-main*)

    ;; actors and isolated share ONE kernel-main verbatim; isolated differs
    ;; only by appending isolated-net.lisp (the Qubes-like overrides, LAST).
    ("aarch64/bare/qemu/actors"
     :net ("arch-aarch64.lisp" "actors.lisp" "e1000.lisp" "ip.lisp"
           "crypto.lisp" "crypto-fast.lisp" "ssh.lisp" "http.lisp"
           "http-client.lisp" "aarch64-overrides.lisp"
           "actors-net-overrides.lisp")
     :parts (:main :net :repl)
     :flags (("*AARCH64-SCHED-LOCK-ADDR*" . #x41200200))
     :main ,*aarch64-actor-main*)

    ("aarch64/bare/qemu/isolated"
     :net ("arch-aarch64.lisp" "actors.lisp" "e1000.lisp" "ip.lisp"
           "crypto.lisp" "crypto-fast.lisp" "ssh.lisp" "http.lisp"
           "http-client.lisp" "aarch64-overrides.lisp"
           "actors-net-overrides.lisp" "isolated-net.lisp")
     :parts (:main :net :repl)
     :flags (("*AARCH64-SCHED-LOCK-ADDR*" . #x41200200))
     :main ,*aarch64-actor-main*)

    ("i386/bare/qemu/ssh"
     ;; crypto-w32 MUST follow crypto + crypto-32: it overrides sha256 /
     ;; sha512 / chacha20 with (hi16 . lo16) pair arithmetic.
     :net ("arch-i386.lisp" "ne2000.lisp" "ip.lisp" "crypto.lisp"
           "crypto-32.lisp" "crypto-w32.lisp" "ssh.lisp" "http.lisp"
           "aarch64-overrides.lisp" "32bit-overrides.lisp"
           "crypto-32-fast.lisp")
     :parts (:net :repl :main :extra)
     :main ,*i386-ssh-main*
     :extra ,*i386-ssh-overrides*)))

(defun net-source (files)
  "Concatenate net/ FILES, each followed by a newline — byte-for-byte what
   the old scripts' (format nil \"~A~%~A~%...\") produced."
  (let ((dir (merge-pathnames "net/" (symbol-value (intern "*MODUS-BASE*" :cl-user)))))
    (apply #'concatenate 'string
           (loop for f in files
                 collect (read-file-text (merge-pathnames f dir))
                 collect (string #\Newline)))))

(defun composite-source (spec)
  (let ((net  nil) (main nil) (extra nil) (repl nil))
    (apply #'concatenate 'string
           (loop for part in (getf spec :parts)
                 collect (ecase part
                           (:net   (or net  (setf net (net-source (getf spec :net)))))
                           (:main  (or main (setf main (format nil "~{~A~%~}"
                                                               (getf spec :main)))))
                           (:extra (or extra (setf extra (getf spec :extra ""))))
                           (:repl  (or repl (setf repl (symbol-value
                                                        (intern "*REPL-SOURCE*" :modus.mvm))))))))))

(defun cell-composite (row)
  (cdr (assoc (row-key row) *composites* :test #'string-equal)))

(defun payload-source (row)
  "Source text for a MIGRATED cell: either a COMPOSITE spec (net/ files +
   REPL + inline kernel-main) or a pure-table-data payload."
  (let ((spec (cell-composite row)))
    (if spec
        (composite-source spec)
        (ecase (fifth row)
          (:repl (symbol-value (intern "*REPL-SOURCE*" :modus.mvm)))))))

(defun apply-cell-flags (spec)
  (dolist (kv (getf spec :flags))
    (let ((sym (intern (car kv) :modus.mvm)))
      (setf (symbol-value sym) (cdr kv))
      (format t "~&build.lisp: MODUS.MVM::~A = ~A~%" (car kv) (cdr kv)))))

(defun build-cell (row)
  (let ((build-image  (intern "BUILD-IMAGE" :modus.mvm))
        (write-kernel (intern "WRITE-KERNEL-IMAGE" :modus.mvm)))
    (funcall (intern "MVM-LOAD" :cl-user) "mvm/repl-source.lisp")
    (install-translator (row-translator row))
    ;; Flags go on AFTER the translator is installed — same order as the
    ;; scripts these cells replace.
    (apply-cell-flags (cell-composite row))
    (format t "~&build.lisp: ~A -> ~A~%" (row-key row) (row-output row))
    (let* ((source (payload-source row))
           (image (funcall build-image
                           :target (row-target row)
                           :source-text source)))
      (format t "build.lisp: combined source ~D chars~%" (length source))
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
