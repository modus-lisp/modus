;;;; hosted-actor-regions.lisp — STEP D: EACH ACTOR OWNS ITS GC REGION.
;;;;
;;;;   ./modus --script test/hosted-actor-regions.lisp
;;;;
;;;; Per-region GC stage 3 gave the actor struct a region slot at +0x68, made
;;;; ZERO MEAN REGION 0, and wired ACTOR-REGION-HOP into net/actors.lisp's YIELD
;;;; and RECEIVE.  Nothing had ever written a NON-ZERO value into that slot in a
;;;; running system, so those calls had never executed.  Here they do: actor 2
;;;; and actor 3 each get a 16 MB carved region whose STACK_BASE IS THAT ACTOR'S
;;;; OWN STACK TOP, and every scheduler hop parks the region it leaves — heap
;;;; pointer, limit and root window together — and enters the arriving one's.
;;;;
;;;; THE ORDERING THIS RESTS ON, stated because it is the real hazard.  x64's
;;;; SAVE-CTX does NOT save R12/R14 (the allocation pointer and limit): they are
;;;; global mutator state and rolling them back would hand the allocator a
;;;; pointer it has already allocated past.  That is exactly what makes the hop
;;;; work here.  ACTOR-REGION-HOP runs AFTER SAVE-CONTEXT has recorded the
;;;; outgoing actor's SP and IMMEDIATELY BEFORE RESTORE-CONTEXT, so
;;;; %GC-REGION-ENTER's load of the arriving region's allocation pointer and
;;;; limit SURVIVES the RESTORE-CONTEXT that follows — restore only moves
;;;; RSP/RBX/RBP.  Between the two there is a window in which the CPU is still
;;;; on the outgoing actor's stack with the ARRIVING region's registers, and it
;;;; is safe only because nothing in it allocates.  (On aarch64 the same window
;;;; is closed differently: its RESTORE-CTX reloads x24/x25 from the arriving
;;;; actor's own save area, so there the struct's +0x10/+0x18 and the region's
;;;; parked pair must agree.  On x64 the question does not arise.)
;;;;
;;;; FIVE CLAIMS, and every collection here is FORCED — a 16 MB region and a few
;;;; thousand conses would not trip a :gc-check in this lifetime:
;;;;
;;;;   (a) EACH ACTOR'S LIVE DATA SURVIVES COLLECTIONS OF ITS OWN REGION, in the
;;;;       RUNNING window.  Each worker holds an NLINKS-cons chain live in a
;;;;       frame slot on its own stack and forces a collection after every
;;;;       message; the chain must still walk each time.  That window only
;;;;       exists because the region's stack_base is this actor's stack top.
;;;;   (b) THE MESSAGE SURVIVES TOO.  Worker A re-checks and re-sends the
;;;;       message AFTER collecting the region it was decoded into; worker B
;;;;       logs it AFTER collecting the region it was decoded into.  That is the
;;;;       term-serialisation soundness claim being run in a live system for the
;;;;       first time.
;;;;   (c) COLLECTING ONE REGION LEAVES THE OTHER ALONE, in both directions:
;;;;       heap checksum, control-block checksum and collection count identical
;;;;       across the other region's collections — and region 0 likewise.
;;;;   (d) EACH REGION'S COUNT RISES INDEPENDENTLY.  The driver collects region
;;;;       2 twice and region 3 once from the far side of the switch, so the two
;;;;       counts must end at NMSG+2 and NMSG+1 while region 0 stays at 0.
;;;;   (e) %GC-COUNT-FOREIGN-REFS IS ZERO IN BOTH DIRECTIONS, with TWO positive
;;;;       controls — an exact synthetic one (a zeroed window holding exactly
;;;;       one cons-tagged pointer, which must count 1) and a real one (actor
;;;;       2's PARKED WINDOW, the span the collector actually scanned, which
;;;;       must hold at least one pointer into its region because that is where
;;;;       its chain is).  An oracle that can only answer zero is worth nothing.

(defvar *fail* 0)
(defvar *checks* 0)
(defvar *nmsg* 16)
(defvar *nlinks* 2000)

(defun w (res off) (%gc-read64 (+ res off)))

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (= got want)
      (format t "  ok   ~A = ~D~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~D (expected ~D)~%" name got want))))

(defun chk-x (name got want)
  (setq *checks* (+ *checks* 1))
  (if (= got want)
      (format t "  ok   ~A = ~X~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~X (expected ~X)~%" name got want))))

(defun chk-true (name got)
  (setq *checks* (+ *checks* 1))
  (if got
      (format t "  ok   ~A~%" name)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A~%" name))))

(let ((res (%ha-regions-selftest *nmsg* *nlinks*)))
  (if (= res 0)
      (format t "~%SKIP: the active region is too small to carve an actor band from.~%")
      (let* ((n       (w res #x00))
             (nl      (w res #x08))
             (ida     (w res #x10))
             (idb     (w res #x18))
             (rcb2    (w res #x20))
             (rcb3    (w res #x28))
             (r0      (w res #x30))
             (fwd     (w res #x38))
             (logged  (w res #x40))
             (badA    (w res #x48))
             (badB    (w res #x50))
             (chainA  (w res #x58))
             (chainB  (w res #x60))
             (badack  (w res #x68))
             (idle    (w res #x70))
             (g2park  (w res #x78))
             (g3park  (w res #x80))
             (g0start (w res #x88))
             (logbase (w res #x90))
             (g2b     (w res #x98))
             (g2a     (w res #xA0))
             (g3b     (w res #xA8))
             (g3mid   (w res #xB0))
             (g0b     (w res #xB8))
             (g0mid   (w res #xC0))
             (sum3b   (w res #xC8))
             (sum3a   (w res #xD0))
             (rcb3sb  (w res #xD8))
             (rcb3sa  (w res #xE0))
             (sum2b   (w res #xE8))
             (sum2a   (w res #xF0))
             (rcb2sb  (w res #xF8))
             (rcb2sa  (w res #x100))
             (live2   (w res #x108))
             (f2old   (w res #x110))
             (f2new   (w res #x118))
             (live3   (w res #x120))
             (refs23  (w res #x128))
             (refs32  (w res #x130))
             (refs20  (w res #x138))
             (ctl-hit (w res #x140))
             (ctl-mis (w res #x148))
             (realA   (w res #x150))
             (realB   (w res #x158))
             (spA     (w res #x160))
             (topA    (w res #x168))
             (spB     (w res #x170))
             (topB    (w res #x178))
             (pk2     (w res #x180))
             (pk3     (w res #x188))
             (pk0     (w res #x190))
             (active  (w res #x198))
             (slotA   (w res #x1A0))
             (slotB   (w res #x1A8))
             (sp2rcb  (w res #x1B0))
             (sp3rcb  (w res #x1B8))
             (sb2     (w res #x1C0))
             (sb3     (w res #x1C8))
             (gsfail  (w res #x1D0))
             (g0end   (w res #x1D8))
             (g3a     (w res #x1E0))
             (g2end   (w res #x1E8)))

        (format t "~%=== LAYOUT ===============================================~%")
        (format t "  region 0 control block                      ~X~%" r0)
        (format t "  actor 2's region control block              ~X~%" rcb2)
        (format t "  actor 3's region control block              ~X~%" rcb3)
        (format t "  actor 2 stack ~X..~X ; parked SP ~X~%" (- topA 65536) topA spA)
        (format t "  actor 3 stack ~X..~X ; parked SP ~X~%" (- topB 65536) topB spB)
        (format t "  region 2 from-space before/after 2 collections ~X ~X~%"
                f2old f2new)

        (format t "~%=== AN ACTOR NAMES ITS REGION (+0x68) ====================~%")
        (chk "arch_prctl set the GS base (0 = yes)" gsfail 0)
        (chk "messages" n *nmsg*)
        (chk "chain links each worker holds live" nl *nlinks*)
        (chk-x "actor 2's +0x68 reads back as its control block" slotA rcb2)
        (chk-x "actor 3's +0x68 reads back as its control block" slotB rcb3)
        (chk-x "region 2's stack_base IS actor 2's stack top" sb2 topA)
        (chk-x "region 3's stack_base IS actor 3's stack top" sb3 topB)
        (chk "region 2 is PARKED now that actor 2 is off-CPU" pk2 1)
        (chk "region 3 is PARKED now that actor 3 is off-CPU" pk3 1)
        (chk "region 0 is RUNNING — the driver is on its stack" pk0 0)
        (chk-x "and it parked at actor 2's OWN recorded SP" sp2rcb spA)
        (chk-x "and it parked at actor 3's OWN recorded SP" sp3rcb spB)
        (chk-x "the active region is region 0's block again" active r0)

        (format t "~%=== (a) LIVE DATA SURVIVED EVERY RUNNING COLLECTION ======~%")
        (format t "  Each worker forced ONE collection of its own region per~%")
        (format t "  message with a ~D-link chain live in its own frame.~%" *nlinks*)
        (chk "actor 2 chain-walk failures" chainA 0)
        (chk "actor 3 chain-walk failures" chainB 0)
        (chk "actor 2's own region collected once per message" g2park *nmsg*)
        (chk "actor 3's own region collected once per message" g3park *nmsg*)

        (format t "~%=== (b) AND SO DID THE MESSAGES ==========================~%")
        (chk "messages actor 2 forwarded AFTER collecting its region" fwd *nmsg*)
        (chk "messages actor 3 logged AFTER collecting its region" logged *nmsg*)
        (chk "actor 2 structural errors (checked before AND after its GC)" badA 0)
        (chk "actor 3 structural errors" badB 0)
        (chk "acknowledgements the primordial actor got back"
             (- *nmsg* badack) *nmsg*)
        (chk "the hosted AP-SCHEDULER was never reached" idle 0)
        (let ((bad 0) (i 1))
          (loop
            (when (> i *nmsg*) (return 0))
            (let ((e (+ logbase (* (- i 1) 32))))
              (if (= (%gc-read64 (+ e 24)) 1) 0 (setq bad (+ bad 1)))
              (if (= (%gc-read64 e) i) 0 (setq bad (+ bad 1)))
              (if (= (%gc-read64 (+ e 8)) (* i 7)) 0 (setq bad (+ bad 1)))
              (if (= (%gc-read64 (+ e 16)) (+ i 1000)) 0 (setq bad (+ bad 1))))
            (setq i (+ i 1)))
          (chk "log fields wrong after TWO serialisation round trips" bad 0))

        (format t "~%=== (c)+(d) COUNTS RISE INDEPENDENTLY, HEAPS DO NOT MOVE =~%")
        (format t "  The driver then collects region 2 TWICE and region 3 ONCE~%")
        (format t "  from the far side of the switch — the PARKED-window path,~%")
        (format t "  scanning each actor's own recorded frames.~%")
        (chk "region 2 collections before the driver's" g2b *nmsg*)
        (chk "region 2 collections after two forced ones" g2a (+ *nmsg* 2))
        (chk "region 3 collections before" g3b *nmsg*)
        (chk "region 3 was NOT collected by region 2's collections" g3mid *nmsg*)
        (chk "region 3 collections after its own forced one" g3a (+ *nmsg* 1))
        (chk "region 2 was NOT collected by region 3's collection"
             g2end (+ *nmsg* 2))
        (format t "  -- region 3 bit-for-bit across region 2's collections --~%")
        (chk "region 3 heap checksum" sum3a sum3b)
        (chk "region 3 control-block checksum" rcb3sa rcb3sb)
        (format t "  -- region 2 bit-for-bit across region 3's collection --~%")
        (chk "region 2 heap checksum" sum2a sum2b)
        (chk "region 2 control-block checksum" rcb2sa rcb2sb)
        (format t "  -- and region 0 throughout --~%")
        (chk "region 0 collections at the start" g0start 0)
        (chk "region 0 collections before the audit" g0b 0)
        (chk "region 0 collections mid-audit" g0mid 0)
        (chk "region 0 collections at the end" g0end 0)
        (chk-x "region 2's semispaces flipped TWICE, back to where they were"
               f2new f2old)

        (format t "~%=== LIVE BYTES SAY THE PARKED WINDOW WAS SCANNED =========~%")
        (format t "  region 2 live bytes after its parked collections  ~D~%" live2)
        (format t "  region 3 live bytes after its parked collection   ~D~%" live3)
        (format t "  A window that missed the actor's frames would leave only~%")
        (format t "  the junk cons that tripped the check (16 bytes).~%")
        (chk-true "region 2 retained at least its whole chain"
                  (>= live2 (* 16 *nlinks*)))
        (chk-true "region 3 retained at least its whole chain"
                  (>= live3 (* 16 *nlinks*)))

        (format t "~%=== (e) THE ASSUMPTION, AUDITED IN EVERY DIRECTION =======~%")
        (format t "  Per-actor collection is sound only because no actor can~%")
        (format t "  hold a pointer into another actor's region — messages are~%")
        (format t "  term-serialised, copied and never shared.  The collector~%")
        (format t "  does NOT enforce that and there is no write barrier.~%")
        (chk "region 2's live data -> either of region 3's semispaces" refs23 0)
        (chk "region 3's live data -> either of region 2's semispaces" refs32 0)
        (chk "region 2's live data -> region 0's live space" refs20 0)
        (format t "  (positive controls — the oracle can answer non-zero)~%")
        (chk "exact control: one planted pointer into region 2 counts" ctl-hit 1)
        (chk "same window, asked about region 3 instead" ctl-mis 0)
        (format t "  real control: actor 2's parked window -> region 2 = ~D~%" realA)
        (format t "  real control: actor 3's parked window -> region 3 = ~D~%" realB)
        (chk-true "actor 2's parked window holds live pointers into its region"
                  (> realA 0))
        (chk-true "actor 3's parked window holds live pointers into its region"
                  (> realB 0))

        (format t "~%=== VERDICT ==============================================~%")
        (if (= *fail* 0)
            (format t "AN ACTOR OWNS ITS GC REGION: PASS (~D checks)~%" *checks*)
            (format t "AN ACTOR OWNS ITS GC REGION: FAIL (~D of ~D checks)~%"
                    *fail* *checks*)))))
