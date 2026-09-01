;;; ============================================================
;;; 32-bit crypto dispatch: rename overridden defuns, generate wrappers
;;; ============================================================

;; Replace all exact occurrences of OLD with NEW in STRING
(defun fp-replace-all (string old new)
  (with-output-to-string (out)
    (let ((pos 0) (old-len (length old)) (slen (length string)))
      (loop
        (let ((found (search old string :start2 pos)))
          (unless found
            (write-string string out :start pos :end slen)
            (return))
          (write-string string out :start pos :end found)
          (write-string new out)
          (setf pos (+ found old-len)))))))

;; Rename (defun NAME ...) to (defun PREFIX-NAME ...) for each NAME in FN-NAMES
(defun fp-rename-defuns (source fn-names prefix)
  (let ((result source))
    ;; Sort longest first to avoid partial matches (sha256-block before sha256)
    (dolist (name (sort (copy-list fn-names) #'> :key #'length) result)
      (setf result (fp-replace-all result
                                   (format nil "(defun ~A " name)
                                   (format nil "(defun ~A~A " prefix name))))))

;; Functions that exist in BOTH 64-bit and 32-bit source.
;; Format: (name max-arity (arg-names...) &optional c64-arity)
;; c64-arity is only needed when 64-bit version has fewer args (sha512-set-k).
(defvar *override-fns*
  '(;; crypto.lisp overrides
    ("sha256-init" 0 ())
    ("sha256-ch" 3 (x y z))
    ("sha256-maj" 3 (x y z))
    ("sha256-bsig0" 1 (x))
    ("sha256-bsig1" 1 (x))
    ("sha256-lsig0" 1 (x))
    ("sha256-lsig1" 1 (x))
    ("sha256-block" 3 (block bo h))
    ("sha256" 1 (msg))
    ("chacha-qr" 5 (s a b c d))
    ("chacha-rotl7" 1 (x))
    ("chacha-rotl8" 1 (x))
    ("chacha-rotl12" 1 (x))
    ("chacha-rotl16" 1 (x))
    ("chacha-inner" 1 (state))
    ("chacha-setup" 3 (key nonce counter))
    ("chacha-block" 3 (key nonce counter))
    ("chacha20-crypt" 5 (key nonce data data-len counter))
    ;; buf-read-u32-le: bare override in extra source (config-mask at 0x48006C)
    ("poly-from-17" 2 (block limbs))
    ("poly-to-16" 2 (limbs result))
    ("poly-mul" 2 (a r))
    ("fe-from-bytes" 1 (bytes))
    ("fe-carry" 1 (h))
    ("fe-reduce" 1 (h))
    ("fe-mul" 3 (dst f g))
    ("fe-mul-split" 3 (dst f g))
    ("fe-mul-precomp-f" 2 (ff f))
    ("fe-mul-precomp-g" 2 (gg g))
    ("ff-read" 2 (ff i))
    ("fe-mul-h0" 3 (h ff gg))
    ("fe-mul-h1" 3 (h ff gg))
    ("fe-mul-h2" 3 (h ff gg))
    ("fe-mul-h3" 3 (h ff gg))
    ("fe-mul-h4" 3 (h ff gg))
    ("fe-mul-h5" 3 (h ff gg))
    ("fe-mul-h6" 3 (h ff gg))
    ("fe-mul-h7" 3 (h ff gg))
    ("fe-mul-h8" 3 (h ff gg))
    ("fe-mul-h9" 3 (h ff gg))
    ("fe-mul-lo" 3 (h ff gg))
    ("fe-mul-hi" 3 (h ff gg))
    ("fe-mul-carry-lo" 1 (h))
    ("fe-mul-carry-hi" 2 (dst h))
    ("fe-mul-copy" 2 (dst h))
    ("fe-sq" 2 (dst f))
    ("fe-sq-iter" 2 (dst n))
    ("fe-carry" 1 (h))
    ("fe-reduce" 1 (h))
    ("fe-reduce-check" 1 (h))
    ("fe-reduce-check2" 2 (h c2))
    ("fe-reduce-check3" 2 (h c5))
    ("fe-reduce-apply" 1 (h))
    ("fe-reduce-apply2" 2 (h c1))
    ("fe-reduce-apply3" 2 (h c3))
    ("fe-reduce-apply4" 2 (h c5))
    ("fe-invert" 1 (z))
    ("fe-invert-lo" 6 (z z2 z9 z11 t0 t1))
    ("fe-invert-hi" 5 (z11 z2 z9 t0 t1))
    ("fe-to-bytes" 1 (fe))
    ("fe-to-bytes-lo" 2 (r fe))
    ("fe-to-bytes-lo2" 3 (r l3 l4))
    ("fe-to-bytes-hi" 2 (r fe))
    ("fe-to-bytes-hi2" 3 (r l8 l9))
    ("x25519" 2 (k u))
    ("x25519-init" 9 (kc k x1 u x2 z2 x3 z3 a24))
    ("x25519-step" 19 (kc x1 x2 z2 x3 z3 a aa b bb fe-e c d da cb t1 t2 a24 swap pos))
    ("x25519-step2" 17 (x1 x2 z2 x3 z3 a aa b bb fe-e c d da cb t1 t2 a24))
    ("x25519-step3" 12 (x1 x2 z2 z3 t1 t2 da cb fe-e aa a24 bb))
    ("x25519-ladder" 18 (kc x1 x2 z2 x3 z3 a aa b bb fe-e c d da cb t1 t2 a24))
    ("sha512-ch" 3 (x y z))
    ("sha512-maj" 3 (x y z))
    ("sha512-sigma0" 1 (x))
    ("sha512-sigma1" 1 (x))
    ("sha512-lsig0" 1 (x))
    ("sha512-lsig1" 1 (x))
    ("sha512-set-k" 9 (i b0 b1 b2 b3 b4 b5 b6 b7) 3)  ;; 64-bit: 3 args (i hi lo)
    ("sha512-init" 0 ())
    ("sha512-block" 4 (block bo h w))
    ("sha512" 1 (msg))
    ;; ed-add, ed-add-compute, ed-add-finish: NOT dispatched.
    ;; Only defined in text-64 (crypto.lisp), no text-32 version exists.
    ;; ed-add calls fe-mul/fe-sub/fe-add which ARE dispatched correctly.
    ("ed-scalar-mult" 2 (scalar point))
    ("ed-reduce-scalar" 1 (hash))
    ;; htonl, buf-read-u32, buf-read-u32-mem, ssh-get-u32:
    ;; bare overrides in extra source (config-mask at 0x48006C)
    ("ssh-random" 1 (ssh))
    ("ssh-buf-to-array" 2 (ssh len))
    ("ssh-buf-consume" 2 (ssh n))
    ("ssh-copy-host-key" 1 (conn))
    ("ssh-compute-exchange-hash" 4 (ssh cli-eph srv-eph shared-secret))
    ("ssh-encrypt-packet" 3 (ssh payload payload-len))
    ("ssh-decrypt-packet" 3 (ssh data data-len))
    ("ssh-make-packet" 3 (ssh payload payload-len))
    ("ssh-parse-packet" 3 (ssh data data-len))
    ("ssh-receive-packet" 2 (ssh timeout))
    ("ssh-receive-version" 1 (ssh))
    ("ssh-dispatch-msg" 4 (ssh payload plen flag-addr))
    ("ssh-message-loop" 1 (ssh))
    ;; ssh-eh-write-arr/mem/u32: only defined in 32bit-overrides.lisp
    ;; (no 64-bit version exists). NOT dispatched — used directly by
    ;; the bare ssh-compute-exchange-hash override in extra source.
    ;; actor stubs (32-bit returns 0, 64-bit has real implementation)
    ("actor-spawn" 1 (fn))
    ("actor-exit" 0 ())
    ("yield" 0 ())
    ("spin-lock" 1 (addr))
    ("spin-unlock" 1 (addr))
    ;; Architecture adapter base addresses: NOT dispatched.
    ;; Replaced by boot-time config-block readers (td-read-u32 #x500040+)
    ;; in *fixpoint-extra-source*. Do not add to *override-fns*.
    ;; NOTE: make-array/aref/aset/array-length/try-alloc-obj/tag-as-object
    ;; are NOT dispatched. The MVM compiler recognizes make-array/aref/aset/
    ;; array-length as builtins and always compiles them to opcodes (ALLOC-OBJ,
    ;; OBJ-REF, OBJ-SET, AREF, ASET, ARRAY-LEN). These opcodes use word-sized
    ;; slots (4 bytes on 32-bit, 8 bytes on 64-bit) with architecture-correct
    ;; headers. Dispatching would create a byte-vs-word mismatch between
    ;; dispatched code (function calls → byte access) and non-dispatched code
    ;; (opcodes → word access). try-alloc-obj/tag-as-object are only called
    ;; by the Lisp make-array function (which is never called since the compiler
    ;; always uses opcodes).
    ("numberp" 1 (obj))
    ;; I/O (different UART/address on each arch)
    ("%serial-byte" 1 (b))
    ("io-delay" 0 ())
    ("arch-seed-random" 0 ())
    ;; DWC2 register helpers (bit 30/31 overflow on 31-bit fixnum)
    ("hcchar-chena" 0 ())
    ("hcchar-chdis" 0 ())
    ("grstctl-ahbidl" 0 ())
    ("hctsiz-pid-data0" 0 ())
    ("hctsiz-pid-data1" 0 ())
    ("hctsiz-pid-setup" 0 ())
    ("dwc2-hprt0-mask" 0 ())
    ;; DWC2 functions with 32-bit safe overrides
    ("dwc2-init" 0 ())
    ("dwc2-core-reset" 0 ())
    ("dwc2-start-transfer" 3 (ch hctsiz-val dma-addr))
    ("dwc2-halt-channel" 1 (ch))
    ("dwc2-poll-channel" 1 (ch))
    ("dwc2-poll-bulk-in" 1 (ch))
    ("dwc2-setup-channel" 2 (ch hcchar-val))
    ("dwc2-port-reset" 0 ())
    ;; NIC driver (E1000 on 64-bit, CDC-Ether on 32-bit)
    ("e1000-send" 2 (buf len))
    ("e1000-receive" 0 ())
    ("e1000-rx-buf" 0 ())
    ;; Line editor (different ssh-ipc-base addresses)
    ("edit-line-len" 0 ())
    ("edit-set-line-len" 1 (v))
    ("edit-cursor-pos" 0 ())
    ("edit-set-cursor-pos" 1 (v))
    ;; Other arch-specific
    ("pci-config-read" 4 (bus dev fn reg))
    ("pci-config-write" 5 (bus dev fn reg val))
    ("pci-assign-bars" 0 ())
    ("init-gc-helper" 0 ())
    ("hash-of" 1 (name))
    ("nfn-lookup" 1 (hash))
    ("emit-prompt" 0 ())
    ("print-dec" 1 (n))
    ("receive" 0 ())
    ("native-eval" 1 (form))
    ("eval-line-expr" 1 (line))
    ("rt-compile-defun" 3 (name args body))
))

;; Generate dispatch wrappers: (defun NAME (args) (if (>= flag 1) (c32-NAME args) (c64-NAME args)))
;; Flag is at 0x48006D, read via mem-ref :u8 (compiler intrinsic, no function call).
;; CRITICAL: Previous version used (td-read-u32 #x500008) which is a FUNCTION CALL that
;; clobbers argument registers (RSI, RDI, R8, R9 on x64) before passing them to the
;; actual c64/c32 function. mem-ref :u8 is an opcode — no register clobber.
;; TODO: Replace with boot-time JMP patching once all translators support FN-ADDR opcode.
;; Generate dispatch wrappers that save args to let bindings before reading
;; the config flag. This is CRITICAL for ARM32 where VR (accumulator, r0)
;; aliases V0 (first arg, r0). Without let bindings, (mem-ref #x50006D :u8)
;; writes to r0, clobbering the first argument before it reaches the c32/c64 call.
;; On x64 (VR=RAX, V0=RSI) and i386 (VR=EAX, V0=ESI) this isn't needed but
;; the let bindings are harmless and make the wrapper correct on all architectures.
(defun fp-gen-dispatch-wrappers ()
  (with-output-to-string (s)
    (format s "~%;;; Architecture dispatch wrappers (32-bit vs 64-bit)~%")
    (dolist (fn *override-fns*)
      (let* ((name (first fn))
             (max-arity (second fn))
             (arg-names (third fn))
             (c64-arity (fourth fn))  ;; nil means same as max-arity
             (args (mapcar (lambda (a) (string-downcase (symbol-name a))) arg-names))
             (c64-args (if c64-arity (subseq args 0 c64-arity) args))
             ;; Generate saved-arg names: _a0, _a1, _a2, ...
             (saved-args (loop for i below max-arity
                               collect (format nil "_a~D" i)))
             (c64-saved (if c64-arity (subseq saved-args 0 c64-arity) saved-args)))
        (if (zerop max-arity)
            (format s "(defun ~A () (if (>= (mem-ref #x50006D :u8) 1) (c32-~A) (c64-~A)))~%"
                    name name name)
            (progn
              ;; Emit: (defun NAME (args...) (let ((_a0 arg0)) (let ((_a1 arg1)) ...
              ;;           (if (>= (mem-ref #x50006D :u8) 1)
              ;;               (c32-NAME _a0 _a1 ...) (c64-NAME _a0 _a1 ...)))))
              (format s "(defun ~A (~{~A~^ ~})~%" name args)
              ;; Nested let bindings to save all args to frame slots
              (loop for arg in args
                    for sa in saved-args
                    do (format s "  (let ((~A ~A))~%" sa arg))
              ;; The dispatch check — now safe because args are in frame slots
              (format s "  (if (>= (mem-ref #x50006D :u8) 1)~%")
              (format s "      (c32-~A~{ ~A~})~%" name saved-args)
              (format s "      (c64-~A~{ ~A~})" name c64-saved)
              ;; Close: if + all let forms + defun = 1 + max-arity + 1
              (format s ")")  ;; close if
              (dotimes (i max-arity) (format s ")"))  ;; close let forms
              (format s ")~%")))))))  ;; close: format, progn, if, let*, dolist, w-o-t-s, defun
