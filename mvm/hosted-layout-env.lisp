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

(defun layout-coinit-text ()
  "Source text for an image's JIT co-init: the runtime twin of
   APPLY-LAYOUT-HOST.  Spliced inside a DEFUN body, so no double quotes."
  (let ((*print-base* 10) (*print-radix* nil))
    (format nil "  (setq *a64-x18-base* ~A)
  (setq *conv-relative* t)
  (setq *conv-delta* ~D)
  (setq *hosted-layout* (quote ~S))
"
            (if *layout-no-x18* "nil" "t")
            *layout-conv-delta*
            *layout-plist*)))

(defun apply-layout-host ()
  "Set the host translator and compiler to this build's layout.  Call after
   boot-linux-aarch64.lisp is loaded and the AArch64 translator installed."
  (flet ((put (name value)
           (setf (symbol-value (find-symbol name :modus.mvm)) value)))
    (put "*CONV-RELATIVE*" t)
    (put "*CONV-DELTA*" *layout-conv-delta*)
    (put "*HOSTED-LAYOUT*" *layout-plist*)
    (let ((code (funcall (find-symbol "HOSTED-LAYOUT" :modus.mvm) :code-base
                         (symbol-value (find-symbol "+LINUX-AARCH64-LOAD-ADDR+" :modus.mvm)))))
      (put "*A64-CODE-ADDR-WIDE*" (>= code (ash 1 32))))
    (when *layout-no-x18* (put "*A64-X18-BASE*" nil))
    (let ((real (funcall (find-symbol "CONV-REAL" :modus.mvm)
                         (symbol-value (find-symbol "+CONV-REGION-BASE+" :modus.mvm)))))
      (format t "~&  Hosted layout: code #x~X  region #x~X (delta #x~X)  heap #x~X  arena #x~X~A~%"
              (funcall (find-symbol "LINUX-AARCH64-CODE-BASE" :modus.mvm))
              real *layout-conv-delta*
              (or (getf *layout-plist* :heap-base) #x2000000000)
              (or (getf *layout-plist* :jit-arena-base) #x3000000000)
              (if *layout-no-x18* "  x18: NOT used (poisoned)" "")))))
