;;;; hosted-ctx-switch.lisp — STEP A: THE CONTEXT SWITCH, ON HOSTED x86-64.
;;;;
;;;;   ./modus --script test/hosted-ctx-switch.lisp
;;;;
;;;; translate-x64.lisp has implemented +op-save-ctx+ and +op-restore-ctx+ all
;;;; along — a real setjmp/longjmp-style switch over RSP/RBX/RBP plus a
;;;; continuation RIP.  Nothing outside net/actors.lisp has ever called them,
;;;; and net/actors.lisp is bare-metal-only and in no hosted image, so that code
;;;; had never once executed on x86-64.  This file makes it execute.
;;;;
;;;; TWO COROUTINES, N round trips, driven through SAVE-CONTEXT/RESTORE-CONTEXT
;;;; and nothing else — no scheduler, no actor table, no per-CPU data.  Both
;;;; sides must make progress (a switch that silently fell through would leave
;;;; one counter at zero), and both sides must find their own locals intact
;;;; afterwards: a sentinel fixnum, a loop counter, and a HEAP CONS, all bound
;;;; before the switch and read after it.
;;;;
;;;; WHY THE LOCALS CHECK IS THE INTERESTING ONE.  x64's SAVE-CTX saves RSP,
;;;; RBX and RBP.  It does NOT save V0/V1/V2/V3/V5/V6/V7/V8 (RSI/RDI/R8/R9/RCX/
;;;; RDX/R10/R11) the way aarch64's equivalent pushes x20-x23/x29/x30, and it
;;;; deliberately does not save R12/R14/R15 (the allocator pointer, limit and
;;;; NIL — global mutator state that must not roll back).  So the switch is
;;;; only sound because the compiler keeps let-bound locals in FRAME SLOTS, on
;;;; the stack RSP restores.  That is a claim about the compiler, and this test
;;;; is what makes it a measurement.

(defvar *fail* 0)
(defvar *checks* 0)

(defun w (res off) (%gc-read64 (+ res off)))

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (= got want)
      (format t "  ok   ~A = ~D~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~D (expected ~D)~%" name got want))))

(defun chk-true (name got)
  (setq *checks* (+ *checks* 1))
  (if got
      (format t "  ok   ~A~%" name)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A~%" name))))

(defvar *rounds* 200)

(let ((res (%ha-ctx-selftest *rounds*)))
  (if (= res 0)
      (format t "~%SKIP: the active region is too small to carve an actor band from.~%")
      (let* ((n         (w res #x00))
             (driver    (w res #x08))
             (coro      (w res #x10))
             (resumes   (w res #x18))
             (badsent   (w res #x20))
             (badcons   (w res #x28))
             (drv-rsp   (w res #x30))
             (co-rsp    (w res #x38))
             (drv-rip   (w res #x40))
             (co-stk    (w res #x48))
             (g0-before (w res #x50))
             (g0-after  (w res #x58)))

        (format t "~%=== LAYOUT ===============================================~%")
        (format t "  coroutine stack top                         ~X~%" co-stk)
        (format t "  driver RSP recorded by save-ctx             ~X~%" drv-rsp)
        (format t "  driver continuation RIP recorded by save-ctx ~X~%" drv-rip)
        (format t "  coroutine RSP recorded by save-ctx          ~X~%" co-rsp)

        (format t "~%=== BOTH SIDES RAN =======================================~%")
        (chk "round trips requested" n *rounds*)
        (chk "driver iterations" driver *rounds*)
        (chk "coroutine iterations" coro *rounds*)
        (chk "times save-context returned NON-zero (a real resume)"
             resumes *rounds*)

        (format t "~%=== VALUES LIVED ACROSS EVERY SWITCH =====================~%")
        (chk "driver sentinel fixnum mismatches" badsent 0)
        (chk "driver heap-cons mismatches" badcons 0)
        (format t "  (the coroutine's own sentinel gates ITS counter: a~%")
        (format t "   clobbered coroutine frame zeroes the count above)~%")

        (format t "~%=== SAVE-CTX WROTE REAL MACHINE STATE ====================~%")
        (chk-true "driver RSP is non-zero" (> drv-rsp 0))
        (chk-true "driver continuation RIP is non-zero" (> drv-rip 0))
        (chk-true "coroutine RSP is below its own stack top" (< co-rsp co-stk))
        (chk-true "coroutine RSP is within 64 KB of that top"
                  (> co-rsp (- co-stk 65536)))
        (chk-true "the two coroutines are on DIFFERENT stacks"
                  (or (> drv-rsp co-stk) (< drv-rsp (- co-stk 65536))))

        (format t "~%=== AND NOTHING COLLECTED UNDERNEATH IT ==================~%")
        (format t "  A collection while the coroutine is live would scan~%")
        (format t "  [its band stack, the PROCESS stack base) — terabytes of~%")
        (format t "  unmapped VA.  Region 0 owning every actor's roots is what~%")
        (format t "  step D fixes; here it is simply asserted not to happen.~%")
        (chk "region 0 collections during the switching" g0-after g0-before)

        (format t "~%=== VERDICT ==============================================~%")
        (if (= *fail* 0)
            (format t "HOSTED x64 CONTEXT SWITCH: PASS (~D checks)~%" *checks*)
            (format t "HOSTED x64 CONTEXT SWITCH: FAIL (~D of ~D checks)~%"
                    *fail* *checks*)))))
