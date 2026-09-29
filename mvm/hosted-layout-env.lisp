;;;; hosted-layout-env.lisp — the hosted AArch64 layout knobs, read ONCE.
;;;;
;;;; docs/macos-hosting.md.  A hosted image's fixed mappings are link-time
;;;; constants, chosen by environment variables (hex):
;;;;
;;;;   MODUS_CONV_DELTA      runtime-data region: real = virtual + delta   (0)
;;;;   MODUS_CODE_BASE       where the code is linked                (0x400000)
;;;;   MODUS_HEAP_BASE       the fixed heap mapping              (0x2000000000)
;;;;   MODUS_JIT_ARENA_BASE  the fixed JIT arena                 (0x3000000000)
;;;;   MODUS_NO_X18=1        Darwin's register rule: no x18 base, boot poisons x18
;;;;   MODUS_DARWIN=1        a Darwin image: every syscall calls the host shim's
;;;;                         stub through a word one 16 KB page below the code
;;;;                         base (implies MODUS_NO_X18; needs MODUS_CODE_BASE)
;;;;   MODUS_NO_THREADS=1    a CLI without native threads (see ENABLE-LAYOUT-THREADS)
;;;;   MODUS_PCREL=1         every layout address PC-relative (ADRP+ADD), so the
;;;;                         whole layout can be mapped at a uniform slide
;;;;                         (translate-aarch64 *A64-PCREL*)
;;;;
;;;; Every value has TWO homes that must agree: the HOST build that compiles
;;;; and translates the image's fixed code, and the image's JIT co-init that
;;;; configures code compiled at runtime.  Both the CLI (build-cli-common) and
;;;; the ANSI gate (build-ansi-common) load this file, so they read the knobs
;;;; the same way; unset, every build is byte-identical to before.

(in-package :cl-user)

(defun %layout-env-hex (name)
  (let ((v (sb-ext:posix-getenv name)))
    (and v (plusp (length v)) (parse-integer v :radix 16))))

(defvar *layout-conv-delta* (or (%layout-env-hex "MODUS_CONV_DELTA") 0))

(defvar *layout-plist*
  (let ((out nil))
    (dolist (kv '(("MODUS_CODE_BASE" . :code-base)
                  ("MODUS_HEAP_BASE" . :heap-base)
                  ("MODUS_JIT_ARENA_BASE" . :jit-arena-base)))
      (let ((v (%layout-env-hex (car kv))))
        (when v (setq out (list* (cdr kv) v out)))))
    out))

(defvar *layout-no-x18*
  (let ((v (sb-ext:posix-getenv "MODUS_NO_X18")))
    (and v (plusp (length v)) (string/= v "0"))))

(defvar *layout-darwin*
  (let ((v (sb-ext:posix-getenv "MODUS_DARWIN")))
    (and v (plusp (length v)) (string/= v "0"))))

(when *layout-darwin*
  (setq *layout-no-x18* t)                          ; Darwin zeroes x18
  ;; runtime source asks (%layout :darwin 0) where Darwin needs a different
  ;; shape (gc.lisp's bitmaps: RW data, not the MAP_JIT arena).
  (setq *layout-plist* (list* :darwin 1 *layout-plist*)))

(defvar *layout-pcrel*
  (let ((v (sb-ext:posix-getenv "MODUS_PCREL")))
    (and v (plusp (length v)) (string/= v "0") t)))

(defvar *layout-threads* nil
  "Native threads (translate-aarch64.lisp, THE PER-THREAD WINDOW, AARCH64).  Set
   by ENABLE-LAYOUT-THREADS, which only the CLI calls: the ANSI gate bakes no
   thread support and keeps its historic code.")

(defconstant +layout-darwin-tsd-key+ 300
  "The pthread key a Darwin image keeps its per-thread-window delta in.  The
   host shim creates keys until it holds this one (host/macos/modus-shim.c
   MODUS_TSD_KEY); the image reads it at TPIDRRO_EL0 + 8*key.")

(defun enable-layout-threads ()
  "Turn native threads on for this build unless MODUS_NO_THREADS is set.  The
   layout key :A64-THREADS is what runtime source tests, with %LAYOUT-IF."
  (let ((v (sb-ext:posix-getenv "MODUS_NO_THREADS")))
    (unless (and v (plusp (length v)) (string/= v "0"))
      (setq *layout-threads* t)
      (unless (getf *layout-plist* :a64-threads)
        (setq *layout-plist* (list* :a64-threads 1 *layout-plist*)))
      ;; Stop-the-world region-0 collection (translate-aarch64).
      (unless (getf *layout-plist* :stw)
        (setq *layout-plist* (list* :stw 1 *layout-plist*)))
      ;; x86-64's heap geometry: two 896 MB semispaces plus the 16 MB overshoot
      ;; guard (boot-linux-aarch64 +LINUX-AARCH64-GC-GUARD+).  Every thread's
      ;; region, the actor band and the lock arena are carved out of region
      ;; 0's semispace; at 432 MB that affords 12 regions and leaves region 0
      ;; small enough to collect inside the thread tests' measurement windows.
      (unless (getf *layout-plist* :heap-size)
        (setq *layout-plist* (list* :heap-size #x71000000 *layout-plist*))))))

(defun check-layout-overlaps ()
  "Refuse a layout whose heap mapping runs into the JIT arena."
  (let ((heap (or (getf *layout-plist* :heap-base) #x2000000000))
        (size (or (getf *layout-plist* :heap-size) #x38000000))
        (arena (or (getf *layout-plist* :jit-arena-base) #x3000000000)))
    (when (and (< heap arena) (> (+ heap size) arena))
      (error "hosted layout: the heap [#x~X, #x~X) overlaps the JIT arena at #x~X ~
              — move MODUS_JIT_ARENA_BASE to #x~X or above"
             heap (+ heap size) arena (+ heap size)))))

(defun layout-tsd-offset ()
  "Byte offset of the delta in the pthread TSD array (Darwin), or NIL (Linux:
   TPIDR_EL0)."
  (and *layout-darwin* (* 8 +layout-darwin-tsd-key+)))

(defun layout-darwin-syscall-slot ()
  "The fixed VA of the word holding the shim's syscall-stub address: one
   16 KB page below the code base.  NIL for a non-Darwin image."
  (when *layout-darwin*
    (let ((code (getf *layout-plist* :code-base)))
      (unless (and code (>= code (ash 1 32)))
        (error "MODUS_DARWIN needs MODUS_CODE_BASE above 4 GB (macOS maps nothing lower)"))
      (- code #x4000))))

(defparameter +layout-slid-keys+ '(:code-base :heap-base :jit-arena-base)
  "The layout keys that are ADDRESSES, and so move with a PC-relative image.")

(defun layout-slid-text (slide-var)
  "Under MODUS_PCREL: source text for *CONV-DELTA*, *HOSTED-LAYOUT* and the
   syscall slot as they are NOW, SLIDE-VAR bytes from where they were linked.
   Every address the image's own code forms is PC-relative, so it moved with
   the layout; these three are the values code compiled at RUNTIME is built
   from, which must move the same way."
  (let ((slot (layout-darwin-syscall-slot)))
    ;; Every integer is (%LINK-INT K): a PC-relative build would otherwise
    ;; take a K that equals a layout address for one, and slide it too.
    (format nil "  (setq *conv-delta* (+ (%link-int ~D) ~A))
  (setq *hosted-layout* (list~{ ~A~}))
  (setq *aarch64-darwin-syscall-slot* ~A)
"
            *layout-conv-delta* slide-var
            (loop for (k v) on *layout-plist* by #'cddr
                  collect (format nil ":~A" (symbol-name k))
                  collect (if (member k +layout-slid-keys+)
                              (format nil "(+ (%link-int ~D) ~A)" v slide-var)
                              (format nil "(%link-int ~D)" v)))
            (if slot (format nil "(+ (%link-int ~D) ~A)" slot slide-var) "nil"))))

(defun layout-coinit-text ()
  "Source text for an image's JIT co-init: the runtime twin of
   APPLY-LAYOUT-HOST.  Spliced inside a DEFUN body, so no double quotes.
   A PC-relative image measures its slide first: (%CONV-ADDR #x10000000)
   compiles PC-relative (compiler.lisp PCREL-LAYOUT-ADDR-P), so it is the
   region's base where it is mapped THIS run."
  (let ((*print-base* 10) (*print-radix* nil))
    (format nil "  (setq *a64-x18-base* ~A)
  (setq *conv-relative* t)
~A~A"
            (if *layout-no-x18* "nil" "t")
            (if *layout-pcrel*
                (format nil "  (let ((%slide (- (%conv-addr #x10000000) (%link-int ~D))))
~A  )
"
                        (+ #x10000000 *layout-conv-delta*)
                        (layout-slid-text "%slide"))
                (format nil "  (setq *conv-delta* ~D)
  (setq *hosted-layout* (quote ~S))
  (setq *aarch64-darwin-syscall-slot* ~A)
"
                        *layout-conv-delta*
                        *layout-plist*
                        (let ((slot (layout-darwin-syscall-slot)))
                          (if slot (format nil "~D" slot) "nil"))))
            (layout-coinit-rest-text))))

(defun layout-coinit-rest-text ()
  (let ((*print-base* 10) (*print-radix* nil))
    (format nil "  (setq *a64-tls-window* ~A)
  (setq *tls-window* ~:*~A)
  (setq *tls-window-a64* ~:*~A)
  (setq *a64-tls-tsd-offset* ~A)
  (setq *aarch64-sched-lock-addr* ~A)
"
            (if *layout-threads* "t" "nil")
            (let ((o (and *layout-threads* (layout-tsd-offset)))) (if o (format nil "~D" o) "nil"))
            (if *layout-threads* "268439488" "nil"))))   ; +HOSTED-SCHED-LOCK-ADDR+ #x10000FC0

(defun check-layout-pcrel ()
  "A PC-relative build recognises layout addresses BY VALUE (compiler.lisp
   PCREL-LAYOUT-ADDR-P), which is only sound where the runtime source writes no
   ordinary numbers: refuse a layout that is not wholly above 4 GB.  The stock
   Linux layout's region (0x0F000000..0x40000000) would claim masks like
   #x3FFFFFFF as addresses and slide them."
  (when *layout-pcrel*
    (let ((low (+ #x0F000000 *layout-conv-delta*))
          (code (getf *layout-plist* :code-base)))
      (unless (and (>= low (ash 1 32)) code (>= code (ash 1 32))
                   (getf *layout-plist* :heap-base) (getf *layout-plist* :jit-arena-base))
        (error "MODUS_PCREL needs the whole layout above 4 GB: set MODUS_CONV_DELTA, ~
                MODUS_CODE_BASE, MODUS_HEAP_BASE and MODUS_JIT_ARENA_BASE ~
                (region low now #x~X)" low)))))

(defun apply-layout-host ()
  "Set the host translator and compiler to this build's layout.  Call after
   boot-linux-aarch64.lisp is loaded and the AArch64 translator installed."
  (check-layout-overlaps)
  (check-layout-pcrel)
  (flet ((put (name value)
           (setf (symbol-value (find-symbol name :modus.mvm)) value)))
    (put "*CONV-RELATIVE*" t)
    (put "*CONV-DELTA*" *layout-conv-delta*)
    (put "*HOSTED-LAYOUT*" *layout-plist*)
    (let ((code (funcall (find-symbol "HOSTED-LAYOUT" :modus.mvm) :code-base
                         (symbol-value (find-symbol "+LINUX-AARCH64-LOAD-ADDR+" :modus.mvm)))))
      (put "*A64-CODE-ADDR-WIDE*" (>= code (ash 1 32))))
    (when *layout-no-x18* (put "*A64-X18-BASE*" nil))
    (put "*A64-PCREL*" *layout-pcrel*)
    (put "*PCREL-LAYOUT*" *layout-pcrel*)
    (put "*AARCH64-DARWIN-SYSCALL-SLOT*" (layout-darwin-syscall-slot))
    (put "*A64-TLS-WINDOW*" *layout-threads*)
    (put "*TLS-WINDOW*" *layout-threads*)
    (put "*TLS-WINDOW-A64*" *layout-threads*)
    (put "*A64-TLS-TSD-OFFSET*" (and *layout-threads* (layout-tsd-offset)))
    ;; RESTORE-CTX releases the hosted scheduler lock, as x86-64's does
    ;; (build-generic-cli.lisp *X64-SCHED-LOCK-ADDR*).
    (when *layout-threads*
      (put "*AARCH64-SCHED-LOCK-ADDR*"
           (symbol-value (find-symbol "+HOSTED-SCHED-LOCK-ADDR+" :modus.mvm))))
    (let ((real (funcall (find-symbol "CONV-REAL" :modus.mvm)
                         (symbol-value (find-symbol "+CONV-REGION-BASE+" :modus.mvm)))))
      (format t "~&  Hosted layout: code #x~X  region #x~X (delta #x~X)  heap #x~X (#x~X)  arena #x~X~A~%"
              (funcall (find-symbol "LINUX-AARCH64-CODE-BASE" :modus.mvm))
              real *layout-conv-delta*
              (or (getf *layout-plist* :heap-base) #x2000000000)
              (or (getf *layout-plist* :heap-size) #x38000000)
              (or (getf *layout-plist* :jit-arena-base) #x3000000000)
              (if *layout-no-x18* "  x18: NOT used (poisoned)" ""))
      (when *layout-pcrel*
        (format t "  PC-relative layout addresses (ADRP+ADD): the layout may slide~%"))
      (when *layout-threads*
        (format t "  Native threads: per-thread window via ~A~%"
                (if (layout-tsd-offset)
                    (format nil "pthread key ~D (TPIDRRO_EL0+~D)" +layout-darwin-tsd-key+ (layout-tsd-offset))
                    "TPIDR_EL0")))
      (when *layout-darwin*
        (format t "  DARWIN image: syscalls call the shim stub via slot #x~X~%"
                (layout-darwin-syscall-slot))))))
