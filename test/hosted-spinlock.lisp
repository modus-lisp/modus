;;;; hosted-spinlock.lisp — STEP 1: A REAL SCHEDULER LOCK ON HOSTED x86-64.
;;;;
;;;;   ./modus --script test/hosted-spinlock.lisp
;;;;
;;;; net/actors.lisp has always shipped a real TTAS spinlock built on the MVM's
;;;; XCHG-MEM — `(if (zerop (xchg-mem addr 1)) …)' — and translate-x64 has
;;;; always implemented +OP-ATOMIC-XCHG+ as a genuine `XCHG [mem], reg', which
;;;; x86 locks implicitly whether or not you write the LOCK prefix.  So the
;;;; primitive was never the obstacle.
;;;;
;;;; THE OBSTACLE WAS WHO RELEASES.  net/actors.lisp takes the lock, does the
;;;; context switch, and hands the RELEASE to RESTORE-CONTEXT: YIELD's resume
;;;; arm is commented "lock already released by restore-context" and does
;;;; nothing, and RECEIVE's resume arm re-ACQUIRES.  translate-aarch64 honours
;;;; that (it stores zero to *AARCH64-SCHED-LOCK-ADDR* right after the SP
;;;; switch); translate-x64 did not, so a real lock would be held across every
;;;; switch and the very next acquire would spin forever.  That is why
;;;; net/hosted-actors-post.lisp used to override both operations to `0'.
;;;;
;;;; THE FIX IS THE AARCH64 ONE, ON x64.  translate-x64 now has
;;;; *X64-SCHED-LOCK-ADDR*, and +OP-RESTORE-CTX+ stores zero to it AFTER
;;;; `mov rsp,[base]' and BEFORE the `jmp'.  That instant is not arbitrary:
;;;;
;;;;   EARLIER IS UNSAFE.  By the time YIELD reaches RESTORE-CONTEXT it has
;;;;   already re-enqueued the OUTGOING actor, so that actor is visible to
;;;;   every other CPU while this CPU is still executing on its stack.  Release
;;;;   before the stack switch and a second thread can dequeue it and
;;;;   RESTORE-CONTEXT onto a stack we have not left — two CPUs, one stack.
;;;;
;;;;   LATER IS NOT AVAILABLE.  After the jump we are running arbitrary actor
;;;;   code — including a FRESHLY SPAWNED actor entered at its raw entry point,
;;;;   which has no idea a lock is outstanding.  A Lisp-level "the arriving code
;;;;   releases" protocol would need a trampoline, an extra actor-struct slot,
;;;;   and edits to both resume arms; releasing inside the switch needs NO EDIT
;;;;   to net/actors.lisp at all.
;;;;
;;;;   NO FENCE, deliberately.  x86-64 is TSO: the releasing store cannot be
;;;;   reordered before an earlier load or store, so a plain MOV is a release.
;;;;   aarch64 needs its DMB because it is not TSO.
;;;;
;;;; The lock is a FIXED BSS WORD (+HOSTED-SCHED-LOCK-ADDR+ in mvm/compiler.lisp)
;;;; rather than an offset into the carved actor band, because the release is
;;;; baked into the instruction stream at TRANSLATE time and cannot be a number
;;;; the image works out at runtime.  BSS zero-fill means it starts unlocked.
;;;;
;;;; WHAT WOULD FAIL WITHOUT THE TRANSLATOR CHANGE: the last three checks.  The
;;;; lock is HELD across the switch, the coroutine on the other side reports
;;;; what it sees, and both it and the returning driver must see 0.  With the
;;;; old +OP-RESTORE-CTX+ they would both see 1 — and the test would FAIL rather
;;;; than hang, because every other acquire/release pair here is balanced.

(defvar *fail* 0)
(defvar *checks* 0)
(defvar *n* 1000)

(defun w (res off) (%gc-read64 (+ res off)))

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (= got want)
      (format t "  ok   ~A = ~D~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~D (expected ~D)~%" name got want))))

(let ((res (%ha-lock-selftest *n*)))
  (if (= res 0)
      (format t "~%SKIP: the active region is too small to carve an actor band from.~%")
      (let* ((w0    (w res #x00))
             (held  (w res #x08))
             (oheld (w res #x10))
             (free  (w res #x18))
             (ofree (w res #x20))
             (n     (w res #x28))
             (bad   (w res #x30))
             (pre   (w res #x38))
             (seen  (w res #x40))
             (post  (w res #x48))
             (addr  (w res #x50)))

        (format t "~%=== THE LOCK IS A FIXED BSS WORD =========================~%")
        (format t "  scheduler lock address ~X~%" addr)
        (chk "and it is +HOSTED-SCHED-LOCK-ADDR+, the word translate-x64 bakes in"
             addr #x10000FC0)
        (chk "BSS zero-fill left it UNLOCKED" w0 0)

        (format t "~%=== ACQUIRE AND RELEASE ==================================~%")
        (chk "the word while SPIN-LOCK holds it" held 1)
        (chk "an XCHG landing on a HELD lock sees 1" oheld 1)
        (chk "the word after SPIN-UNLOCK" free 0)
        (chk "an XCHG landing on a FREE lock sees 0" ofree 0)
        (format t "  (both XCHG probes matter: an oracle that can only ever~%")
        (format t "   answer 0 would pass the release check by accident.)~%")

        (format t "~%=== ~D UNCONTENDED ROUND TRIPS ===========================~%" n)
        (chk "round trips requested" n *n*)
        (chk "round trips where the word read wrong" bad 0)

        (format t "~%=== THE CONTEXT SWITCH RELEASES IT =======================~%")
        (format t "  The driver takes the lock and then RESTORE-CONTEXTs into a~%")
        (format t "  coroutine on a different stack.  Nothing in Lisp releases~%")
        (format t "  it; only +OP-RESTORE-CTX+'s new store can.~%")
        (chk "held immediately before the switch" pre 1)
        (chk "what the ARRIVING side saw" seen 0)
        (chk "what the driver sees when control comes back" post 0)

        (format t "~%=== VERDICT ==============================================~%")
        (if (= *fail* 0)
            (format t "HOSTED x64 SCHEDULER LOCK: PASS (~D checks)~%" *checks*)
            (format t "HOSTED x64 SCHEDULER LOCK: FAIL (~D of ~D checks)~%"
                    *fail* *checks*)))))
