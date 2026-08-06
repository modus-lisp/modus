;;;; rung1.lisp — the #210 cross-arch fixpoint rung-1 probe program.
;;;;
;;;; Deliberately trivial: no heap allocation, no runtime library.  It uses
;;;; only two compiler primitives that every Linux target implements —
;;;; WRITE-CHAR-SERIAL (trap #x0300 → write(2) on fd 1) and SYS-EXIT
;;;; (trap #x0500 → exit_group(2)) — so what it proves is the PIPELINE
;;;; (compile → MVM bytecode → native translate → ELF wrap), not the runtime.
;;;;
;;;; Expected behaviour on any Linux target:
;;;;   stdout: "MODUS XARCH RUNG1 OK\n"
;;;;   exit status: 42   (SYS-EXIT passes the *tagged* fixnum, so 21 → 42)
(defun kernel-main ()
  (write-char-serial 77)   ; M
  (write-char-serial 79)   ; O
  (write-char-serial 68)   ; D
  (write-char-serial 85)   ; U
  (write-char-serial 83)   ; S
  (write-char-serial 32)
  (write-char-serial 88)   ; X
  (write-char-serial 65)   ; A
  (write-char-serial 82)   ; R
  (write-char-serial 67)   ; C
  (write-char-serial 72)   ; H
  (write-char-serial 32)
  (write-char-serial 82)   ; R
  (write-char-serial 85)   ; U
  (write-char-serial 78)   ; N
  (write-char-serial 71)   ; G
  (write-char-serial 49)   ; 1
  (write-char-serial 32)
  (write-char-serial 79)   ; O
  (write-char-serial 75)   ; K
  (write-char-serial 10)
  (sys-exit 21))
