;;;; build-i386-diag-ssh.lisp — Build i386 diagnostic REPL with SSH over E1000
;;;; LEGACY MINI-LISP PATH.  This image runs mvm/repl-source.lisp — a separate
;;;; 708-line toy Lisp with its own reader, printer and evaluator, NOT the real
;;;; Common Lisp stack.  The real CL image on i386 is mvm/build-i386-cli.lisp
;;;; (prelude + gc + rt + the cl-*.lisp bridge + crypto), driven by
;;;; scripts/run-i386.sh.  Retiring this second Lisp is its own workstream.
;;;;
;;;; Usage: sbcl --script mvm/build-i386-diag-ssh.lisp
;;;;
;;;; Produces:
;;;;   /tmp/modus-i386-diag-ssh.bin   — kernel (Multiboot, works with qemu -kernel)
;;;;   /tmp/modus-i386-diag-ssh.img   — disk image (boot sector + kernel, USB boot)
;;;;
;;;; Features:
;;;;   - VGA text mode + PS/2 keyboard (boot messages)
;;;;   - SSH server over Intel E1000/E1000e PCI NIC
;;;;   - All compiled functions callable from SSH REPL (auto-dispatch)
;;;;   - PCI config space reads, GPU MMIO, NIC diagnostics from REPL
;;;;   - Direct boot from USB (no GRUB needed)
;;;;
;;;; Test in QEMU:
;;;;   qemu-system-i386 -m 512 -nographic -no-reboot \
;;;;     -kernel /tmp/modus-i386-diag-ssh.bin \
;;;;     -device e1000,netdev=net0 \
;;;;     -netdev 'user,id=net0,hostfwd=tcp::2222-:22'
;;;;
;;;;   echo '(+ 1 2)' | ssh -p 2222 -o StrictHostKeyChecking=no test@localhost
;;;;
;;;; Real hardware (ThinkPad T420):
;;;;   sudo dd if=/tmp/modus-i386-diag-ssh.img of=/dev/sdX bs=512 status=progress
;;;;   # Boot from USB, SSH to the machine's DHCP-assigned IP

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(mvm-load "mvm/repl-source.lisp")

(defun read-file-text (path)
  "Read entire file as a string."
  (with-open-file (s path :direction :input)
    (let ((text (make-string (file-length s))))
      (let ((n (read-sequence text s)))
        (subseq text 0 n)))))

(defvar *net-dir*
  (merge-pathnames "net/" *modus-base*))

(defvar *console-source*
  (read-file-text (merge-pathnames "net/i386-console.lisp" *modus-base*)))

(defvar *e1000-raw-source*
  (read-file-text (merge-pathnames "net/i386-e1000-raw.lisp" *modus-base*)))

(defvar *ehci-source*
  (read-file-text (merge-pathnames "net/i386-ehci.lisp" *modus-base*)))

;; Load the direct boot code (boot sector emitter + console entry)
(load (merge-pathnames "boot/boot-direct-i386.lisp" *modus-base*))

;;; ============================================================
;;; SSH networking source (same as build.lisp i386/bare/qemu/ssh but with e1000)
;;; ============================================================
;;; Source load order:
;;;   arch-i386 → e1000 → ip → crypto → crypto-32 → crypto-w32 → ssh →
;;;   http → aarch64-overrides → 32bit-overrides → crypto-32-fast

(defvar *net-source*
  (format nil "~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%"
          (read-file-text (merge-pathnames "arch-i386.lisp" *net-dir*))
          (read-file-text (merge-pathnames "e1000.lisp" *net-dir*))
          (read-file-text (merge-pathnames "ip.lisp" *net-dir*))
          (read-file-text (merge-pathnames "crypto.lisp" *net-dir*))
          (read-file-text (merge-pathnames "crypto-32.lisp" *net-dir*))
          (read-file-text (merge-pathnames "crypto-w32.lisp" *net-dir*))
          (read-file-text (merge-pathnames "ssh.lisp" *net-dir*))
          (read-file-text (merge-pathnames "http.lisp" *net-dir*))
          (read-file-text (merge-pathnames "aarch64-overrides.lisp" *net-dir*))
          (read-file-text (merge-pathnames "32bit-overrides.lisp" *net-dir*))
          (read-file-text (merge-pathnames "crypto-32-fast.lisp" *net-dir*))))

;;; ============================================================
;;; Auto-generate REPL dispatch for ALL compiled functions
;;; ============================================================
;;; Same as build-i386-diag.lisp.

(defun extract-defun-info (source-text)
  "Extract (NAME-STRING . PARAM-COUNT) for all defuns in SOURCE-TEXT.
   Deduplicates keeping last occurrence (last-defun-wins)."
  (let ((defuns '())
        (pos 0)
        (len (length source-text))
        (*read-eval* nil)
        (*package* (find-package :cl-user)))
    (loop
      (when (>= pos len) (return))
      (handler-case
          (multiple-value-bind (form new-pos)
              (read-from-string source-text nil :eof :start pos)
            (if (eq form :eof)
                (return)
                (progn
                  (setf pos new-pos)
                  (when (and (consp form)
                             (eq (car form) 'defun)
                             (symbolp (second form))
                             (listp (third form)))
                    (push (cons (symbol-name (second form))
                                (length (third form)))
                          defuns)))))
        (error ()
          (let ((next (position #\( source-text :start (min (1+ pos) len))))
            (if next (setf pos next) (return))))))
    ;; Deduplicate: keep last occurrence of each name
    (let ((seen (make-hash-table :test #'equal))
          (order '()))
      (dolist (pair (nreverse defuns))
        (unless (gethash (car pair) seen)
          (push (car pair) order))
        (setf (gethash (car pair) seen) (cdr pair)))
      (mapcar (lambda (name) (cons name (gethash name seen)))
              (nreverse order)))))

(defun gen-char-codes (name)
  "Generate (cons C1 (cons C2 ... nil)) for NAME string."
  (if (zerop (length name))
      "nil"
      (with-output-to-string (s)
        (loop for c across name do (format s "(cons ~D " (char-code c)))
        (write-string "nil" s)
        (dotimes (i (length name)) (write-char #\) s)))))

(defun gen-nth-cdr (n)
  "Generate nested cdr to reach nth position in list 'a'."
  (if (<= n 0) "a"
      (format nil "(cdr ~A)" (gen-nth-cdr (1- n)))))

(defun gen-nth-car (n)
  "Generate (car (cdr ...)) for nth element of list 'a'."
  (format nil "(car ~A)" (gen-nth-cdr n)))

(defun gen-call-wrapper (name param-count)
  "Generate (defun rcall-NAME (a) ...) wrapper that extracts args and calls NAME."
  (let ((lname (string-downcase name)))
    (with-output-to-string (s)
      (format s "(defun rcall-~A (a) " lname)
      (if (zerop param-count)
          (format s "(let ((r (~A))) (cons r nil))" lname)
          (progn
            (dotimes (i param-count)
              (format s "(let ((a~D ~A)) " i (gen-nth-car i)))
            (format s "(let ((r (~A" lname)
            (dotimes (i param-count) (format s " a~D" i))
            (format s "))) (cons r nil)")
            (dotimes (i (1+ param-count)) (write-char #\) s))))
      (format s ")~%"))))

(defun group-list (lst n)
  "Split LST into sublists of at most N elements."
  (loop for i from 0 below (length lst) by n
        collect (subseq lst i (min (+ i n) (length lst)))))

(defun gen-dispatch-source (defuns)
  "Generate eval-platform-call dispatch source from DEFUNS list."
  (let* ((filtered (remove-if
                     (lambda (d)
                       (or (string= (car d) "EVAL-PLATFORM-CALL")
                           (> (cdr d) 5)
                           ;; Exclude compiler intrinsics that require constant args
                           ;; (io-out-byte etc. embed port as imm16, can't take variables)
                           (member (car d)
                                   '("IO-OUT-BYTE" "IO-IN-BYTE"
                                     "IO-OUT-DWORD" "IO-IN-DWORD"
                                     "MMIO-DO-READ32" "MMIO-DO-WRITE32"
                                     "IO-IN-DWORD-RAW" "PCI-CONFIG-READ-RAW"
                                     "WBINVD" "MEMORY-BARRIER"
                                     "HALT-LOOP" "STI-HLT"
                                     "SETUP-NIC-IDT" "NIC-IRQ-UNMASK")
                                   :test #'string=)
                           ;; Exclude EHCI/RTL internals but keep EHCI-PROBE and EHCI-DIAG-*
                           (and (> (length (car d)) 4)
                                (or (string= (car d) "EHCI-" :end1 5)
                                    (string= (car d) "RTL-" :end1 4))
                                (not (string= (car d) "EHCI-PROBE"))
                                (not (and (>= (length (car d)) 10)
                                          (string= (car d) "EHCI-DIAG-" :end1 10))))))
                     defuns))
         (groups (group-list filtered 8)))
    (with-output-to-string (s)
      (format s "~%;;; === Auto-generated REPL dispatch (~D functions) ===~%"
              (length filtered))
      (dolist (fn filtered)
        (write-string (gen-call-wrapper (car fn) (cdr fn)) s))
      (loop for group in groups
            for i from 0
            do (format s "(defun try-repl-~D (nm args) (let ((n nm) (a args)) " i)
               (dolist (fn group)
                 (format s "(if (symbol-eq n ~A) (rcall-~A a) "
                         (gen-char-codes (car fn))
                         (string-downcase (car fn))))
               (if (< (1+ i) (length groups))
                   (format s "(try-repl-~D n a)" (1+ i))
                   (format s "nil"))
               (dotimes (j (length group)) (write-char #\) s))
               (format s "))~%"))
      (if groups
          (format s "(defun eval-platform-call (name evaled-args) (let ((n name) (a evaled-args)) (try-repl-0 n a)))~%")
          (format s "(defun eval-platform-call (name evaled-args) nil)~%")))))

;;; ============================================================
;;; String literal expansion
;;; ============================================================

(defun gen-chars-cons (text)
  "Generate (cons C1 (cons C2 ... nil)) for TEXT string."
  (if (zerop (length text))
      "nil"
      (with-output-to-string (s)
        (loop for c across text do (format s "(cons ~D " (char-code c)))
        (write-string "nil" s)
        (dotimes (i (length text)) (write-char #\) s)))))

(defun expand-print-str (text)
  (format nil "(write-string-codes ~A)" (gen-chars-cons text)))

(defun expand-print-ln (text)
  (format nil "(progn (write-string-codes ~A) (write-char-output 10) 0)"
          (gen-chars-cons text)))

(defun expand-print-reg-raw (name-text)
  (format nil "(progn (write-string-codes ~A) (write-char-output 58) (write-char-output 32) (mmio-print-result) (write-char-output 10) 0)"
          (gen-chars-cons name-text)))

(defun expand-one-pattern (source pattern expander)
  "Replace all occurrences of (PATTERN \"TEXT\"...) using EXPANDER function."
  (let ((result source)
        (pat (concatenate 'string "(" pattern " \"")))
    (loop
      (let ((pos (search pat result)))
        (unless pos (return result))
        (let* ((str-start (+ pos (length pat)))
               (str-end (position #\" result :start str-start)))
          (unless str-end (return result))
          (let* ((close-paren (position #\) result :start (1+ str-end)))
                 (text (subseq result str-start str-end))
                 (expansion (funcall expander text)))
            (setf result (concatenate 'string
                                      (subseq result 0 pos)
                                      expansion
                                      (subseq result (1+ close-paren))))))))))

(defun expand-string-calls (source-text)
  "Replace string-literal print calls with inline character writes."
  (let ((result source-text))
    (setf result (expand-one-pattern result "print-reg-raw" #'expand-print-reg-raw))
    (setf result (expand-one-pattern result "print-ln" #'expand-print-ln))
    (setf result (expand-one-pattern result "print-str" #'expand-print-str))
    result))

;; Expand string literals in console + e1000 raw sources
(setf *console-source* (expand-string-calls *console-source*))
(setf *e1000-raw-source* (expand-string-calls *e1000-raw-source*))

;;; ============================================================
;;; Build
;;; ============================================================

(in-package :modus.mvm)

;; Install the i386 translator
(modus.mvm.i386:install-i386-translator)

;; SSH kernel-main and overrides
(let* ((ssh-main (format nil "~{~A~%~}"
                        (list
                         "(defun kernel-main ()"
                         "  (vga-clear-screen)"
                         "  (print-str \"v3\")"
                         "  (write-char-output 49)"
                         "  (e1000-probe)"
                         "  (write-char-output 50)"
                         "  (sha256-init)"
                         "  (sha512-init)"
                         "  (setf (mem-ref (+ (e1000-state-base) #x5D0) :u32) 0)"
                         "  (ed25519-init)"
                         "  (write-char-output 51)"
                         "  (ssh-seed-random)"
                         "  (write-char-output 52)"
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
                         ;; Pre-computed Ed25519 host key derivatives (s + prefix)
                         "  (let ((state (e1000-state-base)))"
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
                         ;; prefix at state+0x6A0
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
                         "    (setf (mem-ref (+ state #x6C0) :u32) 1))"
                         ;; SSH port
                         "  (setf (mem-ref (+ (ssh-ipc-base) #x60438) :u32) 22)"
                         ;; Clear connection table
                         "  (let ((i 0))"
                         "    (loop"
                         "      (when (>= i 4) (return 0))"
                         "      (setf (mem-ref (conn-base i) :u32) 0)"
                         "      (setq i (+ i 1))))"
                         ;; Pre-computed X25519 ephemeral key pair
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
                         ;; Enable PIT timer for HLT-based io-delay
                         "  (enable-pit-timer)"
                         "  (kernel-main-ready))"
                         "(defun diag-print-key-bytes ()"
                         "  (let ((state (e1000-state-base)))"
                         "    (write-char-output 10)"
                         "    (write-char-output 115)"
                         "    (write-char-output 61)"
                         "    (vga-hex-byte (mem-ref (+ state #x680) :u8))"
                         "    (vga-hex-byte (mem-ref (+ state #x681) :u8))"
                         "    (vga-hex-byte (mem-ref (+ state #x682) :u8))"
                         "    (vga-hex-byte (mem-ref (+ state #x683) :u8))))"
                         ";; Check c constant at state+0x5C0 (should be ED D3 F5 5C)"
                         "(defun diag-print-c-bytes ()"
                         "  (let ((state (e1000-state-base)))"
                         "    (write-char-output 10)"
                         "    (write-char-output 99)"
                         "    (write-char-output 61)"
                         "    (vga-hex-byte (mem-ref (+ state #x5C0) :u8))"
                         "    (vga-hex-byte (mem-ref (+ state #x5C1) :u8))"
                         "    (vga-hex-byte (mem-ref (+ state #x5C2) :u8))"
                         "    (vga-hex-byte (mem-ref (+ state #x5C3) :u8))))"
                         "(defun diag-fe-test ()"
                         "  (let ((a (fe-from-int 7)))"
                         "    (let ((r (fe-from-int 0)))"
                         "      (fe-mul r a a)"
                         "      (let ((bytes (fe-to-bytes r)))"
                         "        (write-char-output 10)"
                         "        (write-char-output 70)"
                         "        (write-char-output 61)"
                         "        (vga-hex-byte (aref bytes 0))"
                         "        (vga-hex-byte (aref bytes 1))"
                         "        (vga-hex-byte (aref bytes 2))"
                         "        (vga-hex-byte (aref bytes 3))))))"
                         ";; Test ed-reduce-scalar: reduce SHA-512 of prefix||msg mod L"
                         "(defun diag-reduce-test ()"
                         "  (let ((state (e1000-state-base)))"
                         "    (let ((prefix (make-array 32)))"
                         "      (dotimes (i 32)"
                         "        (aset prefix i (mem-ref (+ state (+ #x6A0 i)) :u8)))"
                         "      (let ((msg (make-array 4)))"
                         "        (aset msg 0 1)"
                         "        (aset msg 1 2)"
                         "        (aset msg 2 3)"
                         "        (aset msg 3 4)"
                         "        (let ((input (concat-bytes prefix 32 msg 4)))"
                         "          (let ((r (ed-reduce-scalar (sha512 input))))"
                         "            (write-char-output 10)"
                         "            (write-char-output 82)"
                         "            (write-char-output 61)"
                         "            (vga-hex-byte (aref r 0))"
                         "            (vga-hex-byte (aref r 1))"
                         "            (vga-hex-byte (aref r 2))"
                         "            (vga-hex-byte (aref r 3))))))))"
                         "(defun diag-sha512-test ()"
                         "  (let ((msg (make-array 1)))"
                         "    (aset msg 0 0)"
                         "    (let ((h (sha512 msg)))"
                         "      (write-char-output 10)"
                         "      (write-char-output 72)"
                         "      (write-char-output 61)"
                         "      (vga-hex-byte (aref h 0))"
                         "      (vga-hex-byte (aref h 1))"
                         "      (vga-hex-byte (aref h 2))"
                         "      (vga-hex-byte (aref h 3)))))"
                         "(defun diag-sign-test ()"
                         "  (let ((msg (make-array 4)))"
                         "    (aset msg 0 1)"
                         "    (aset msg 1 2)"
                         "    (aset msg 2 3)"
                         "    (aset msg 3 4)"
                         "    (let ((sig (ed25519-sign-fast msg 4)))"
                         "      (write-char-output 10)"
                         "      (write-char-output 83)"
                         "      (write-char-output 61)"
                         "      (vga-hex-byte (aref sig 0))"
                         "      (vga-hex-byte (aref sig 1))"
                         "      (vga-hex-byte (aref sig 2))"
                         "      (vga-hex-byte (aref sig 3))"
                         "      (vga-hex-byte (aref sig 32))"
                         "      (vga-hex-byte (aref sig 33))"
                         "      (vga-hex-byte (aref sig 34))"
                         "      (vga-hex-byte (aref sig 35)))))"
                         "(defun kernel-main-ready ()"
                         "  (diag-print-key-bytes)"
                         "  (diag-print-c-bytes)"
                         "  (diag-fe-test)"
                         "  (diag-reduce-test)"
                         "  (diag-sha512-test)"
                         "  (diag-sign-test)"
                         "  (vga-nic-dump)"
                         "  (write-char-output 10)"
                         "  (write-char-output 62)"
                         "  (write-char-output 32)"
                         "  (net-actor-main))"
                         ;; REPL with network polling in read-char-input
                         "(defun net-actor-main ()"
                         "  (let ((globals (cons nil nil)))"
                         "    (repl globals)))"
)))
       ;; SSH connection handler with higher timeout for i386 crypto
       (ssh-overrides "
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
       ;; Combine all source: networking + REPL + console + EHCI + e1000 raw + SSH main
       ;; EHCI loaded BEFORE e1000-raw so E1000 overrides win for networking.
       ;; EHCI's send/receive renamed to ehci-net-* to avoid shadowing.
       (combined-source (concatenate 'string
                                      cl-user::*net-source*
                                      *repl-source*
                                      cl-user::*console-source*
                                      cl-user::*ehci-source*
                                      cl-user::*e1000-raw-source*
                                      ssh-main
                                      ssh-overrides))
       ;; Generate auto-dispatch for ALL compiled functions
       (defuns (cl-user::extract-defun-info combined-source))
       (dispatch-source (cl-user::gen-dispatch-source defuns))
       (final-source (concatenate 'string combined-source dispatch-source)))
  (format t "Building i386 diagnostic SSH image (E1000 PCI)...~%")
  (format t "Combined source: ~D chars (~D from dispatch)~%"
          (length final-source) (length dispatch-source))
  (format t "Functions callable from REPL: ~D~%"
          (length (remove-if (lambda (d)
                               (or (string= (car d) "EVAL-PLATFORM-CALL")
                                   (> (cdr d) 5)))
                             defuns)))
  (let ((image (build-image :target :i386-console :source-text final-source)))
    (format t "Entry point offset: ~A~%" (kernel-image-entry-point image))
    (format t "Native code size: ~D~%" (length (kernel-image-native-code image)))
    (format t "Boot code size: ~D~%" (length (kernel-image-boot-code image)))
    (let* ((kernel-bytes (kernel-image-image-bytes image))
           (kernel-size (length kernel-bytes))
           (kernel-path "/tmp/modus-i386-diag-ssh.bin")
           (image-path "/tmp/modus-i386-diag-ssh.img"))

      ;; Write kernel binary
      (write-kernel-image image kernel-path)

      ;; Generate boot sector
      (format t "Generating boot sector (kernel: ~D bytes, ~D sectors)...~%"
              kernel-size (ceiling kernel-size 512))
      (let* ((boot-sector (generate-boot-sector kernel-size))
             (padded-kernel-size (* (ceiling kernel-size 512) 512))
             (raw-size (+ 512 padded-kernel-size))
             (total-size (max raw-size (* 2880 512)))
             (disk-image (make-array total-size
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0)))
        ;; Copy boot sector
        (replace disk-image boot-sector)
        ;; Copy kernel after boot sector
        (replace disk-image kernel-bytes :start1 512)

        ;; Write disk image
        (with-open-file (out image-path :direction :output
                                        :element-type '(unsigned-byte 8)
                                        :if-exists :supersede)
          (write-sequence disk-image out))
        (format t "Wrote ~D bytes to ~A~%~%" total-size image-path)
        (format t "Kernel:     ~A~%" kernel-path)
        (format t "Disk image: ~A~%~%" image-path)
        (format t "QEMU test:~%")
        (format t "  qemu-system-i386 -m 512 -nographic -no-reboot \\~%")
        (format t "    -kernel ~A \\~%" kernel-path)
        (format t "    -device e1000,netdev=net0 \\~%")
        (format t "    -netdev 'user,id=net0,hostfwd=tcp::2222-:22'~%~%")
        (format t "SSH test:~%")
        (format t "  echo '(+ 1 2)' | ssh -p 2222 -o StrictHostKeyChecking=no test@localhost~%~%")
        (format t "Real hardware:~%")
        (format t "  sudo dd if=~A of=/dev/sdX bs=512 status=progress~%" image-path)))))
