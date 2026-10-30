;;;; kernel-packages.lisp -- the kernel's packages beyond MODUS-INT
;;;;
;;;; Builder packages step 3: net, crypto, ssh and the boards move out of
;;;; MODUS-INT into packages of their own, one gate cycle at a time.  This file
;;;; is the ONE list of those packages.  The build host loads it (so the reader
;;;; can read source into them) and every image bakes it (so %INIT-PACKAGES
;;;; makes the same packages at boot, for the REPL and for the self-host build,
;;;; which re-reads this source in the image).  Plain CL on both sides.
;;;;
;;;; A seam between packages is an INTERFACE function bound per image with
;;;; (setf fdefinition) at boot (user, 2026-10-29): MODUS.NET:NIC-SEND is an
;;;; ordinary function whose definition the image's composition sets from its
;;;; driver's -- see net/nic.lisp.

(in-package :modus.mvm)

(defun %kernel-package-specs ()
  "Each entry is (NAME USE EXPORTS), all strings.  Order matters: a package
   comes after the ones it uses."
  (quote
   (;; the implementation of COMMON-LISP: mvm/prelude.lisp and mvm/cl-*.lisp
    ;; (user, 2026-10-29).  What the rest of the kernel calls of it -- and
    ;; what it calls of the rest -- is homed in MODUS-INT and exported
    ;; (%MODUS-INT-RUNTIME-EXPORTS), as SB-IMPL uses SB-INT; everything else
    ;; in it is its own.
    ("MODUS.RUNTIME" ("COMMON-LISP" "MODUS-INT") ())
    ("MODUS.NET" ("COMMON-LISP")
     ;; the NIC interface: every driver implements it, ip.lisp calls it
     ("NIC-SEND" "NIC-RECEIVE" "NIC-RX-BUF" "NIC-PROBE" "NIC-READ-REG"))
    ;; the drivers.  They use MODUS-INT for the board and console functions
    ;; it exports to them (%MODUS-INT-DRIVER-EXPORTS).
    ("MODUS.NET.NE2000" ("COMMON-LISP" "MODUS-INT")
     ("NE2K-SEND" "NE2K-RECEIVE" "NE2K-RX-BUF" "NE2K-PROBE"))
    ("MODUS.NET.E1000" ("COMMON-LISP" "MODUS-INT")
     ("E1000-HW-SEND" "E1000-HW-RECEIVE" "E1000-HW-RX-BUF" "E1000-HW-PROBE"
      "E1000-READ-REG" "E1000-WRITE-REG"))
    ("MODUS.NET.VIRTIO" ("COMMON-LISP" "MODUS-INT")
     ("VIRTIO-NET-PROBE" "VIRTIO-NET-SEND" "VIRTIO-NET-RECEIVE"
      "VIRTIO-NET-RX-BUF" "VIRTIO-NET-ACTIVE-P" "VIRTIO-NET-STATS"))
    ;; SHA-256/512, ChaCha20, Poly1305, X25519, Ed25519 (net/crypto*.lisp):
    ;; what SSH, the SNP attestation and the isolated net domain call
    ("MODUS.CRYPTO" ("COMMON-LISP" "MODUS-INT")
     ("SHA256-INIT" "SHA256" "SHA512-INIT" "SHA512" "SHA512-BLOCK"
      "CHACHA-BLOCK" "CHACHA20-CRYPT" "POLY1305" "X25519" "X25519-PUBLIC-KEY"
      "ED25519-PUBLIC-KEY" "ED25519-VERIFY" "ED-BASE-MULT" "ED-ENCODE-POINT"
      "ED-REDUCE-SCALAR" "ED-SCALAR-ADD" "ED-SCALAR-MULT-MOD-L"
      "CONCAT-BYTES" "CONCAT3-BYTES"))
    ;; the SSH-2 server (net/ssh.lisp): what the images' boot text, the
    ;; adapters (hosted-ssh, isolated-cl, ssh-x64-cl) and ip.lisp call
    ("MODUS.SSH" ("COMMON-LISP" "MODUS-INT" "MODUS.CRYPTO")
     ("SSH-SERVER" "SSH-HANDLE-CONNECTION" "SSH-HANDLE-KEX" "SSH-DISPATCH-MSG"
      "SSH-RECEIVE-PACKET" "SSH-NET-POLL" "SSH-SEND-STRING" "SSH-SEND-PROMPT"
      "SSH-GET-U32" "SSH-INIT-STRINGS" "SSH-SEED-RANDOM" "SSH-RANDOM"
      "SSH-SET-HOST-KEY" "SSH-USE-DEFAULT-KEY" "SSH-AUTH-KEY-ADDR"
      "SSH-MEM-STORE" "SSH-REFRESH-EPHEMERAL" "SSH-PEER-CLOSED-P"
      "SSH-PREAUTH-CLOCK" "SSH-PREAUTH-IDLE-LIMIT" "SSH-WAIT-EXPIRED-P"
      "PRE-COMPUTE-HOST-SIGN" "PRE-COMPUTE-SERVER-EPH" "%DRBG-WEAK-KEY")))))

(defun %modus-int-driver-exports ()
  "What MODUS-INT exports to the step-3 packages: the console
   printers, PCI configuration access and the BOARD's address hooks (each
   arch-*-cl.lisp defines these for its machine).  The board hooks move to a
   MODUS.BOARD package when the boards get one.  Exported on the build host
   and in the image alike (%ENSURE-KERNEL-PACKAGES), so the image reads
   driver source as the host did."
  (quote ("PRINT-DEC" "PRINT-HEX-BYTE" "PRINT-HEX32" "WRITE-STRING-SERIAL"
          "%SERIAL-BYTE" "IO-DELAY" "PCI-CONFIG-READ" "PCI-CONFIG-WRITE"
          "E1000-STATE-BASE" "E1000-RX-DESC-BASE" "E1000-RX-BUF-BASE"
          "E1000-TX-DESC-BASE" "E1000-TX-BUF-BASE" "NE2K-RX-HOST"
          "VIRTIO-NET-RING-BASE" "VIRTIO-NET-STATE-BASE" "VIRTIO-NET-PT-PAGE"
          "SNP-MMIO-P" "SNP-MMIO-REAL-P" "SNP-MMIO-READ" "SNP-MMIO-WRITE"
          ;; for MODUS.CRYPTO: byte-buffer access, its scratch, and the
          ;; keep-alive a long computation calls (a seam the images rebind)
          "BUF-READ-U32" "BUF-WRITE-U32" "FE-SCRATCH-BASE" "USB-KEEPALIVE"
          ;; for MODUS.SSH: the TCP/IP stack it rides on, its connection and
          ;; IPC blocks, the REPL's line editor and the shell hooks the image
          ;; supplies (SSH-EVAL-LINE and friends: build text or an adapter)
          "ARCH-SEED-RANDOM" "ARP-REPLY" "ICMP-HANDLE" "NET-HANDLE-TCP"
          "NET-ACTOR-MAIN" "TCP-SEND-CONN" "BUF-READ-U16-MEM" "CONN-BASE"
          "CONN-SSH" "SSH-CONN-BASE" "SSH-IPC-BASE" "FIXNUMP"
          "EDIT-CURSOR-POS" "EDIT-LINE-LEN" "EDIT-SET-CURSOR-POS"
          "EDIT-SET-LINE-LEN" "EMIT-PROMPT" "EVAL-LINE-EXPR" "PRINT-OBJ"
          "HASH-OF" "NFN-LOOKUP" "RT-COMPILE-DEFUN" "SSH-EVAL-LINE"
          "SSH-DO-EVAL-EXPR" "SSH-HANDLE-CHANNEL-DATA")))

(defun %kernel-package-name-p (name)
  "True when NAME is one of %KERNEL-PACKAGE-SPECS' packages."
  (let ((specs (%kernel-package-specs)) (hit nil))
    (loop
      (when (or hit (null specs)) (return hit))
      (when (string= name (car (car specs))) (setq hit t))
      (setq specs (cdr specs)))))

(defun %modus-int-runtime-exports ()
  "The names MODUS-INT exports for MODUS.RUNTIME: functions, globals and
   quoted markers the runtime and the rest of the kernel share, as the build's
   package checks (calls, globals, quotes, function collisions) named
   them."
  (quote ("%ANY-RUNTIME-BORN-PKG-P" "%ANY-TO-FLOAT" "%AREF-MULTI-PUBLIC"
          "%AREF-SLOW" "%AS-RESULT-FLOAT" "%ASET-SLOW" "%BB-LIMB"
          "%BB-NLIMBS" "%BB-SIGN" "%BULK-COPY" "%BULK-COPY-U8" "%C32-AREF"
          "%C32-ASET" "%CAB" "%CAB-FD-OPEN" "%CAB-ON" "%CALL-INTERP-CLOSURE"
          "%CATCH-RECEIVE" "%CHAR2=" "%CL-SYM-NAME" "%CL-SYM-P"
          "%CL-SYM-PACKAGE" "%CLI-GETENV" "%CLOCK-GETTIME-NS"
          "%CLOS-INSTANCE" "%COMPILE-FILE-TOPLEVEL-EFFECTS" "%COMPLEX-ABS"
          "%COMPLEX-IMAG" "%COMPLEX-P" "%COMPLEX-REAL"
          "%COND-INSTALL-READER-IF-GENERIC" "%CONDITION-P" "%CONDITION-SLOT"
          "%CONDITION-TYPE-NAME" "%DBL" "%DECLAIM-NOTE-INLINE"
          "%DECLAIM-NOTE-TYPE" "%DEFERRED-PACKAGE-HOOK" "%DEFERRED-PENDING-P"
          "%DEFINE-CONDITION" "%DEFINE-SEAM" "%DEFPACKAGE-IMPL"
          "%DEFTYPE-LOOKUP" "%DEFTYPE-WARM" "%DO-FUNCALL"
          "%DYNB-FILTER-CLEAR" "%DYNBIND-C" "%DYNUNBIND-C"
          "%E2-NAME-SET-FUNCTION-P" "%ENSURE-KERNEL-PACKAGES"
          "%EVAL-SYM-NAME" "%EXPAND-DEFTYPE" "%EXPORT-STANDARD-CL-SYMBOLS"
          "%F32-AREF" "%F32-AREF-RT" "%F32-ASET" "%F32-ASET-RT"
          "%F32-BITS-REF-NARROW" "%F32-BITS-REF-RT" "%F32-BITS-SET-NARROW"
          "%F32-BITS-SET-RT" "%FASL-CLOSE" "%FASL-COMPILE-FILE"
          "%FASL-COMPILED-FX" "%FASL-EVAL" "%FASL-LOAD-BEGIN"
          "%FASL-LOAD-END" "%FASL-OPEN" "%FASL-READ" "%FASL-STREAM-TEXT"
          "%FASL-TAINT" "%FAULT-FRAMES" "%FD-INPUT-READY-P" "%FILL-POS"
          "%FILL-U8" "%FILL-WORDS" "%FIXNUM-SHL-OR-NIL" "%FLAT-STRING"
          "%FLOAT-HI32" "%FLOAT-LO32" "%FLOAT-RESULT-TYPE" "%FLOOR2"
          "%FORCE-DEFERRED-INSTALLS" "%FS-FD" "%FS-FLUSH-ALL" "%FS-IO-PAGE"
          "%GC-COUNT" "%GC-READ64" "%GC-REGION" "%GC-STAT-COUNT"
          "%GC-STAT-LAST-BYTES" "%GC-STAT-LAST-TICKS" "%GC-STAT-MAX-TICKS"
          "%GC-STAT-TOTAL-BYTES" "%GC-STAT-TOTAL-TICKS" "%GC-WRITE64"
          "%GENERIC-FUNCTION-P" "%GET-NARGS" "%GETHASH-PAIR" "%GF-DISPATCH"
          "%GV-SET" "%HB-HANDLER-ENTRY" "%HT-PAIRS" "%IEEE-FLOAT-P"
          "%INIT-BOOLE-CONSTANTS" "%INIT-CLOS-PROTOCOL"
          "%INIT-CONDITION-TYPES" "%INIT-MAKE-LOAD-FORM"
          "%INIT-METHOD-COMBINATIONS" "%INIT-PACKAGES" "%INIT-READER"
          "%INIT-SIGNAL-HANDLING" "%INIT-SIGNAL-SYMBOLS" "%INIT-STREAMS"
          "%INIT-SYMBOL-FUNCTION-TABLE" "%INTERN-SYMBOL-PKG"
          "%INTERP-CLOSURE-P" "%INTERP-FAULT-CONDITION" "%INTERSECTION-IMPL"
          "%IRT-NS" "%IRUN-NS" "%JIT-DRAIN-WANTED"
          "%JIT-FNADDR-THUNK-INVALIDATE" "%JIT-X64-STUB-RETARGET"
          "%KERNEL-GLOBAL-SHADOW-P" "%KERNEL-HOME-PKG-NAME-P"
          "%KERNEL-PACKAGE-NAME-P" "%KERNEL-SHADOW-KEY" "%KEY-LOOKUP"
          "%KEY-PRESENT-P" "%LDB-RT" "%LOOP-GE" "%LOOP-GT" "%LOOP-LE"
          "%LOOP-LT" "%MAKE-BASE-STRING" "%MAKE-F32-VECTOR"
          "%MAKE-FILE-STREAM-FULL" "%MAKE-STRING32" "%MAPHASH-IMPL"
          "%MARKED-VECTOR-P" "%MAYBE-REPORT-UNHANDLED-HC" "%MDA-ARRAY-LENGTH"
          "%MEXP-MEMO-INVALIDATE" "%MLL-NAME-EQ" "%NARROW-COPY"
          "%NARROW-STRINGS-P" "%NATIVE-SYM-P" "%NEGATE-NUMBER" "%PKG-NAME"
          "%PKG-P" "%PROCLAIM-SPECIAL-NAME" "%PROGV-RESTORE" "%PROGV-SAVE"
          "%PROGV-SET" "%PUBLISH-FENCE" "%RATIONAL-DIVIDE"
          "%RAW-MACRO-EXPANDER" "%RC-CATCH-CLEANUP" "%RC-CLEAR-INVOKE"
          "%RC-ENTER-ASSOCIATING" "%RC-EXIT" "%RC-INVOKED-ARGS"
          "%RC-INVOKED-INDEX" "%RC-INVOKED-MINE-P" "%RC-INVOKED-P"
          "%RC-PASS-ON" "%REG-PKG-ALIST-GET" "%REG-PKG-ALIST-PUT"
          "%REG-SYM-PKG-OBJ" "%REGISTER-GF-FN" "%REGISTER-SETF-FUNCTION"
          "%REGISTER-STRUCT-PRINTER" "%REGISTER-STRUCT-TYPE" "%REPLACE-POS"
          "%RESTART-FRAME-UNWIND" "%ROOM-REPORT" "%ROUND-TO-SINGLE-LISP"
          "%RT-CONSP" "%RT-ENTER" "%RT-ENTER-LOCKED" "%RT-LEAVE"
          "%RT-LEAVE-LOCKED" "%RT-OWN-ALLOC-BEGIN" "%RT-OWN-ALLOC-END"
          "%RT-RESERVE" "%RT-THREADS-LIVE-P" "%RUNTIME-BORN-PKG-P"
          "%SAFE-CAR" "%SAFE-CDR" "%SEAT-BACKQUOTE-MARKERS"
          "%SEED-STATIC-KEYWORD" "%SET-EXCLUSIVE-OR-IMPL"
          "%SIGNAL-TYPE-ERROR" "%SNAPSHOT-KERNEL-FN-NAMES"
          "%SNAPSHOT-KERNEL-GLOBAL-NAMES" "%SQRT-F-NEWTON" "%STREAM-TYPE"
          "%STRING-TO-CSTR" "%STRUCT-INSTANCE" "%STRUCT-INSTANCE-TYPEP"
          "%SVREF-SLOW" "%SYM-GLOBAL-PKG-KEY" "%SYM-HASH" "%SYS-CLOSE"
          "%SYS-FSTAT-SIZE" "%SYS-GETCWD-NR" "%SYS-GETDENTS64" "%SYS-GETPID"
          "%SYS-LSEEK" "%SYS-MKDIR" "%SYS-OPEN-APPEND"
          "%SYS-OPEN-CREATE-EXCL" "%SYS-OPEN-RDONLY" "%SYS-OPEN-RDWR"
          "%SYS-OPEN-WRONLY" "%SYS-OPEN-WRONLY-EXISTING" "%SYS-READ-RAW"
          "%SYS-READLINK" "%SYS-RENAME" "%SYS-STAT-EXISTS" "%SYS-STAT-MTIME"
          "%SYS-STAT-SIZE" "%SYS-UNLINK" "%SYS-WRITE-RAW"
          "%TAKE-PENDING-FAULT" "%TARGET-BIG-ENDIAN-P" "%THROW-TO"
          "%TIMER-UNIVERSAL-TIME" "%TO-DOUBLE" "%TRUNC1-GENERIC"
          "%TRUNCATE2-GENERIC" "%UNION-IMPL" "%UTF8-MEM-TO-STRING"
          "%VALIDATE-KW-LIST" "%WITH-HANDLER-BIND" "%WRAPPER-ARRAY-LENGTH"
          "%WRAPPER-ARRAYP" "%WRAPPER-STRINGP" "*%ARITH-STRICT*"
          "*%CURRENT-GF*" "*%CURRENT-GF-ARGS*" "*%DEFTYPE-CLOSURES*"
          "*%DIRENT-BUF-ADDR*" "*%EVAL-ESCAPE-STACK*" "*%FN-NAMES*"
          "*%LOAD-DEPTH*" "*%NEXT-METHODS*" "*%NO-CONS-ARRAY-WRAPPERS*"
          "*CATCH-ACTIVE*" "*CATCH-TAG*" "*CATCH-TAGS*" "*CATCH-VALUE*"
          "*CATCH-VALUES*" "*CSTR-SCRATCH*" "*CURRENT-CONDITION*"
          "*E2-ACTIVE-DEFUN-NAMES*" "*E2-GF-NAME-MEMO*" "*FILESYSTEM*"
          "*GENTEMP-COUNTER*" "*GF-FN-TO-NAME*" "*IO-BUF-ADDR*"
          "*JIT-HOT-CALLS*" "*JIT-SCRIPT-DEF-NAMES*" "*KERNEL-FN-NAMES*"
          "*KERNEL-GLOBAL-NAMES*" "*LOAD-ABORT-ON-ERROR*"
          "*LOAD-ERROR-CONDITION*" "*MACRO-TABLE*" "*MVM-EMIT-HALVES*"
          "*MVM-EVAL-RUNTIME-P*" "*NATIVE-SYM-FUNCTION-TABLE*"
          "*READER-MISSING-PACKAGE-LENIENT*" "*RUNTIME-BORN-ANSWERS*"
          "*RUNTIME-BORN-PKGS*" "*SCRATCH-MMAPPED*" "*STANDARD-READTABLE*"
          "*SYM-NAME-TABLE*" "*SYMBOL-FUNCTION-TABLE*"
          "*WRITE-OBJECT-BUDGET*" "+BN-SENTINEL+" "+CLOCK-MONOTONIC+"
          "+CLOCK-PROCESS-CPUTIME+" "+FIXNUM-BITS+" "+FIXNUM-HALF+"
          "+FIXNUM-HALF-MAX+" "+FIXNUM-LIMIT+" "+FIXNUM-MAX+"
          "+FIXNUM-NEG-HALF+" "+FIXNUM-NEG-LIMIT+" "+FIXNUM-READ-GUARD+"
          "+HALF-LIMB-BITS+" "+HALF-LIMB-MASK+" "+LIMB-BITS+" "+LIMB-BITS-1+"
          "+LIMB-SPLIT-BITS+" "+MV-COUNT-ADDR+" "+MV-VALUES-ADDR+"
          "+TARGET-BIG-ENDIAN+" "..." "BACKQUOTE" "BIG-BIGNUM-P" "BIGNUM-ASH"
          "BIGNUM-HI" "BIGNUM-LO" "COMMA" "COMMA-AT" "COMMA-DOT"
          "COMPLEX-DIV" "CONCATENATE-STRINGS" "EOF" "EQLT" "EQT" "EQUALT"
          "EXACT-DIVIDE" "EXPECTED" "FLOAT-NEGATIVE-P"
          "FLOAT-TRUNCATE-TO-INTEGER" "FLOATP-IMPL" "GENERIC-ADD"
          "GENERIC-FUNCTION-LAMBDA-LIST" "GENERIC-FUNCTION-METHODS"
          "GENERIC-FUNCTION-NAME" "GENERIC-MULTIPLY" "GENERIC-NEGATE-INT"
          "GENERIC-SUBTRACT" "GOT" "HALT" "INIT-COMPILER-MACRO-SET"
          "INIT-KEYWORD-TABLE" "INIT-SYMBOL-TABLE" "JIT-EAGER" "MVM-EVAL"
          "MVM-TYPE-ERROR" "NAME" "NOTNOT" "NUMERIC-EQUAL-P" "NV" "OBJ"
          "OPERATION" "PUTHASH" "RANDOM-FROM-INTERVAL" "SET-CHAR" "SET-ELT"
          "SET-FDEFINITION" "SET-GETF" "SET-MACRO-FUNCTION"
          "SET-SYMBOL-FUNCTION" "SET-SYMBOL-PLIST" "SET-SYMBOL-VALUE"
          "SUBTYPEP*" "WRITE-OBJECT")))

(defun %ensure-kernel-packages ()
  "Make every package in %KERNEL-PACKAGE-SPECS that does not exist yet, and
   export its names.  Answers the list of packages."
  (let ((out nil)
        (int (find-package "MODUS-INT")))
    (dolist (n (%modus-int-driver-exports))
      (export (intern n int) int))
    (dolist (n (%modus-int-runtime-exports))
      (export (intern n int) int))
    (dolist (s (%kernel-package-specs))
      (let ((p (or (find-package (car s))
                   (make-package (car s) :use (cadr s)))))
        (dolist (e (caddr s))
          (export (intern e p) p))
        (push p out)))
    (nreverse out)))
