;;;; hosted-percpu.lisp — STEP B: PER-CPU STORAGE ON HOSTED x86-64.
;;;;
;;;;   ./modus --script test/hosted-percpu.lisp
;;;;
;;;; net/actors.lisp keeps its current-actor, idle-flag, reduction counter and
;;;; object-space pointers in PER-CPU slots, reached through PERCPU-REF /
;;;; PERCPU-SET.  translate-x64.lisp emits those as `GS:[disp32]`.  On hosted
;;;; Linux the GS base is 0, so every one of them reads or writes an absolute
;;;; low address and takes SIGSEGV — which is exactly why the actor system has
;;;; never run in a hosted image.
;;;;
;;;; TWO WAYS TO FIX IT, AND WHY THIS ONE.  The alternative was to override the
;;;; four accessors with plain memory.  That does not work: PERCPU-REF and
;;;; PERCPU-SET are COMPILER INTRINSICS — mvm/compiler.lisp dispatches on the
;;;; op-name hash before it ever looks a function up — so no `defun' can shadow
;;;; them, and an override would mean editing every percpu call site inside
;;;; net/actors.lisp, i.e. modifying the file whose unmodified behaviour is the
;;;; thing under test.  Setting the segment base with arch_prctl(ARCH_SET_GS)
;;;; costs one syscall, leaves net/actors.lisp alone, and runs the SAME
;;;; instructions the bare-metal image runs (GS: on x64 is TPIDR_EL1 on
;;;; aarch64) rather than a parallel code path that could diverge.
;;;;
;;;; WHAT MAKES THIS A REAL TEST RATHER THAN A ROUND TRIP.  A value written and
;;;; read back through the same GS:[disp32] pair would also round-trip if the
;;;; segment base pointed at some unrelated mapped page.  So every slot is ALSO
;;;; read straight out of the per-CPU block with %GC-READ64, which does not go
;;;; through GS at all.  PERCPU-SET stores the value still TAGGED, so that word
;;;; must be exactly 2x the value — which ties the segment base to the block
;;;; this image owns.
;;;;
;;;; NOTE ON *X64-GC-REGION-PERCPU*: setting a GS base is a NECESSARY condition
;;;; for that flag, not the same decision.  It stays off.  Flipping it makes the
;;;; GC's active-region CELL a GS:-relative read from both the collector and the
;;;; mutator, and both sides must flip together or they read different cells.

(defvar *fail* 0)
(defvar *checks* 0)

(defun w (res off) (%gc-read64 (+ res off)))

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (= got want)
      (format t "  ok   ~A = ~D~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~D (expected ~D)~%" name got want))))

(defun chk-slot (idx off readback memword)
  (chk (format nil "percpu-ref ~D read back" off) readback (+ 700000 idx))
  (chk (format nil "  and the word at base+~D is 2x that" off)
       memword (* 2 (+ 700000 idx))))

(let ((res (%ha-percpu-selftest)))
  (if (= res 0)
      (format t "~%SKIP: the active region is too small to carve an actor band from.~%")
      (let* ((rc      (w res #x00))
             (pb      (w res #x08))
             (gsbase  (w res #x10))
             (big-rd  (w res #xA0))
             (big-want (w res #xA8)))

        (format t "~%=== THE SEGMENT BASE ======================================~%")
        (format t "  per-CPU block                               ~X~%" pb)
        (format t "  GS base recorded after arch_prctl           ~X~%" gsbase)
        (chk "arch_prctl(ARCH_SET_GS) returned 0" rc 0)
        (chk "and the recorded base is the block" gsbase pb)

        (format t "~%=== EVERY SLOT net/actors.lisp USES =======================~%")
        (format t "  (0 self-ptr, 8 reduction, 16 cpu-id, 24 current-actor,~%")
        (format t "   32 idle-flag, 40 obj-alloc, 48 obj-limit, 56 idle-stack)~%")
        (chk-slot 0  0 (w res #x20) (w res #x28))
        (chk-slot 1  8 (w res #x30) (w res #x38))
        (chk-slot 2 16 (w res #x40) (w res #x48))
        (chk-slot 3 24 (w res #x50) (w res #x58))
        (chk-slot 4 32 (w res #x60) (w res #x68))
        (chk-slot 5 40 (w res #x70) (w res #x78))
        (chk-slot 6 48 (w res #x80) (w res #x88))
        (chk-slot 7 56 (w res #x90) (w res #x98))

        (format t "~%=== A 47-BIT VALUE, NOT A SMALL INTEGER ==================~%")
        (format t "  obj-alloc/obj-limit carry heap addresses; a lost high half~%")
        (format t "  or a sign-extension would only show up on a real one.~%")
        (format t "  allocation pointer stored                   ~X~%" big-want)
        (chk "percpu-ref 40 returned it unchanged" big-rd big-want)

        (format t "~%=== VERDICT ==============================================~%")
        (if (= *fail* 0)
            (format t "HOSTED x64 PER-CPU STORAGE: PASS (~D checks)~%" *checks*)
            (format t "HOSTED x64 PER-CPU STORAGE: FAIL (~D of ~D checks)~%"
                    *fail* *checks*)))))
