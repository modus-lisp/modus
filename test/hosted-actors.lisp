;;;; hosted-actors.lisp — STEP C: net/actors.lisp RUNNING ON HOSTED x86-64.
;;;;
;;;;   ./modus --script test/hosted-actors.lisp
;;;;
;;;; net/actors.lisp — spawn, yield, send, receive, term serialisation, the
;;;; cooperative scheduler — has always been bare-metal-only, so its
;;;; %GC-REGION-SWITCH call sites (per-region GC stage 3) had never executed
;;;; anywhere.  With the address hooks (net/hosted-actors.lisp) and the GS base
;;;; (step B) in place, this is the same file running as an ordinary Linux
;;;; process.
;;;;
;;;; EVERY ACTOR STILL ALLOCATES IN REGION 0.  That is the point of doing C
;;;; before D: this measures the scheduler, the mailboxes and the serialiser
;;;; with the GC arrangement unchanged, so that when step D moves each actor
;;;; into its own region, anything that breaks is the region change.
;;;;
;;;; THE PIPELINE.  The primordial actor (id 1, on the process stack) spawns
;;;; actor 2 and actor 3, each on its own 64 KB band stack, then YIELDs once so
;;;; both reach a blocking RECEIVE.  It then sends NMSG messages to actor 2;
;;;; actor 2 checks each one's shape and forwards it UNCHANGED to actor 3;
;;;; actor 3 records its three fixnums and acknowledges to actor 1.  Every
;;;; message therefore crosses the term serialiser TWICE (encoded into 2's
;;;; staging buffer, decoded into 2's heap, encoded into 3's, decoded into 3's)
;;;; and the acknowledgements — plain fixnums — take SEND's fast path instead.
;;;;
;;;; WHAT THE SWITCHES ACTUALLY ARE.  YIELD and RECEIVE call SAVE-CONTEXT and
;;;; RESTORE-CONTEXT, i.e. the +op-save-ctx+/+op-restore-ctx+ pair step A proved
;;;; out, on three different stacks.  If any of it silently no-op'd, the actors
;;;; would never run and the counts below would be zero.
;;;;
;;;; MESSAGES ARE DOTTED TREES OF NON-ZERO FIXNUMS, and that is a finding, not a
;;;; preference.  net/actors.lisp's TERM-SIZE begins `(if (zerop val) 1 …)',
;;;; which is how it recognises the end of a list on bare metal, WHERE NIL IS
;;;; ZERO.  In a hosted CL image NIL is the immediate #xDEAD0001: (zerop nil) is
;;;; false, (consp nil) is false, (numberp nil) is false, and TERM-SIZE falls
;;;; through to SOFT-SUBTAG, which dereferences it.  So a NIL-terminated LIST
;;;; cannot be sent by this serialiser in a hosted image.  Dotted pairs can, and
;;;; they exercise the same cons path.
;;;;
;;;; AND ONE THING IT ASSERTS RATHER THAN FIXES: region 0 must not collect while
;;;; an actor is running.  Region 0's stack_base is the PROCESS stack base, but
;;;; actors 2 and 3 run on band stacks, so a collection during one of them would
;;;; scan from a band address up to the process stack base — terabytes of
;;;; unmapped VA.  Every actor sharing region 0 is exactly what step D ends.

(defvar *fail* 0)
(defvar *checks* 0)
(defvar *nmsg* 24)

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

(let ((res (%ha-actors-selftest *nmsg*)))
  (if (= res 0)
      (format t "~%SKIP: the active region is too small to carve an actor band from.~%")
      (let* ((n        (w res #x00))
             (ida      (w res #x08))
             (idb      (w res #x10))
             (fwd      (w res #x18))
             (logged   (w res #x20))
             (badA     (w res #x28))
             (badB     (w res #x30))
             (badack   (w res #x38))
             (idle     (w res #x40))
             (g0-bef   (w res #x48))
             (g0-aft   (w res #x50))
             (gsfail   (w res #x58))
             (acount   (w res #x60))
             (logbase  (w res #x68))
             (spA      (w res #x70))
             (spB      (w res #x78))
             (topA     (w res #x80))
             (topB     (w res #x88))
             (cur      (w res #x90))
             (stA      (w res #x98))
             (stB      (w res #xA0))
             (sp1      (w res #xA8))
             (region   (w res #xB0))
             (region0  (w res #xB8)))

        (format t "~%=== THE ACTOR SYSTEM CAME UP =============================~%")
        (chk "arch_prctl set the GS base (0 = yes)" gsfail 0)
        (chk "actor A's id" ida 2)
        (chk "actor B's id" idb 3)
        (chk "next free actor id after two spawns" acount 4)
        (format t "  actor 2 stack top ~X ; recorded SP ~X~%" topA spA)
        (format t "  actor 3 stack top ~X ; recorded SP ~X~%" topB spB)
        (format t "  primordial actor's recorded SP ~X~%" sp1)
        (chk-true "actor 2's SP is inside its own 64 KB stack"
                  (if (< spA topA) (> spA (- topA 65536)) nil))
        (chk-true "actor 3's SP is inside its own 64 KB stack"
                  (if (< spB topB) (> spB (- topB 65536)) nil))
        (chk-true "the primordial actor is on a DIFFERENT stack (the process's)"
                  (or (> sp1 topB) (< sp1 (- topA 65536))))

        (format t "~%=== BOTH ACTORS RAN ======================================~%")
        (chk "messages sent" n *nmsg*)
        (chk "actor 2 forwarded" fwd *nmsg*)
        (chk "actor 3 logged" logged *nmsg*)
        (chk "acknowledgements the primordial actor got back"
             (- *nmsg* badack) *nmsg*)
        (chk "actor 2 structural errors" badA 0)
        (chk "actor 3 structural errors" badB 0)
        (chk "current actor at the end is the primordial one" cur 1)
        (chk "actor 2 is parked in RECEIVE (status 4 = blocked)" stA 4)
        (chk "actor 3 is parked in RECEIVE (status 4 = blocked)" stB 4)
        (chk "the hosted AP-SCHEDULER was never reached" idle 0)

        (format t "~%=== EVERY MESSAGE ARRIVED INTACT =========================~%")
        (format t "  actor 3's log, read straight out of the band: message i~%")
        (format t "  must carry (i . (7i . i+1000)) after TWO serialisation~%")
        (format t "  round trips through two different staging buffers.~%")
        (let ((bad 0) (i 1))
          (loop
            (when (> i *nmsg*) (return 0))
            (let ((e (+ logbase (* (- i 1) 32))))
              (if (= (%gc-read64 (+ e 24)) 1) 0 (setq bad (+ bad 1)))
              (if (= (%gc-read64 e) i) 0 (setq bad (+ bad 1)))
              (if (= (%gc-read64 (+ e 8)) (* i 7)) 0 (setq bad (+ bad 1)))
              (if (= (%gc-read64 (+ e 16)) (+ i 1000)) 0 (setq bad (+ bad 1))))
            (setq i (+ i 1)))
          (chk "log entries wrong in any field" bad 0))

        (format t "~%=== ALL THREE STILL ALLOCATE IN REGION 0 =================~%")
        (format t "  active region control block ~X (region 0 is ~X)~%"
                region region0)
        (chk "the active region is region 0's block" region region0)
        (chk "region 0 collections during the whole run" g0-aft g0-bef)

        (format t "~%=== VERDICT ==============================================~%")
        (if (= *fail* 0)
            (format t "HOSTED x64 ACTORS: PASS (~D checks)~%" *checks*)
            (format t "HOSTED x64 ACTORS: FAIL (~D of ~D checks)~%"
                    *fail* *checks*)))))
