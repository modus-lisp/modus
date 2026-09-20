;;;; region-gc-actors.lisp — STAGE-3 PER-REGION GC ACCEPTANCE: THE ACTOR.
;;;;
;;;;   ./modus --script test/region-gc-actors.lisp
;;;;
;;;; Stage 1 made the HEAP a property of the region, stage 2 made the ROOTS one.
;;;; Stage 3 makes THE REGION a property of an ACTOR: an actor names its region
;;;; by pointer in its own struct at +0x68, ZERO MEANS REGION 0, and a scheduler
;;;; hop parks the region it leaves — allocation pointer, limit AND root window —
;;;; and makes the arriving actor's region the running one.
;;;;
;;;; %gc-region-actors-selftest (mvm/gc.lisp) lays THREE 128-byte actor structs
;;;; out in the carved guard band, in net/actors.lisp's own layout.  Actor 0's
;;;; region slot is left ZERO — it is the actor of today, owning no region — and
;;;; must resolve to region 0's block.  Actors 1 and 2 own carved 16 MB regions.
;;;; Each hop reads exactly what a scheduler reads: the outgoing actor's SP from
;;;; its struct at +0x08 (where SAVE-CONTEXT writes it) and the incoming actor's
;;;; region from +0x68.  Then 1, 2, 1, 2, with a FORCED collection at every stop
;;;; while the region sits parked on its own 512-byte root window.
;;;;
;;;; THE FOUR CLAIMS, all judged here by reading memory back out:
;;;;   (a) each region's OWN collection count rose at each of its stops.  The
;;;;       collections are FORCED, never assumed — stage 1 measured that an
;;;;       ordinary 200,000-allocation stress collects ZERO times on this heap.
;;;;   (b) each region's live data survived its own collection: the chain still
;;;;       walks from its parked slot, and the live byte count is 16*N + 16 —
;;;;       the chain plus the one junk cons that trips the :gc-check.
;;;;   (c) collecting region A leaves region B bit-for-bit alone, in BOTH
;;;;       directions, and leaves region 0 alone as well.
;;;;   (d) %gc-count-foreign-refs is ZERO in both directions across real
;;;;       collections — plus a POSITIVE CONTROL on each parked window, a span
;;;;       known to hold exactly ONE pointer into its region, because an oracle
;;;;       that can only answer zero is worth nothing.
;;;;
;;;; WHAT THIS DOES NOT TEST, said plainly: net/actors.lisp's own call sites.
;;;; That file is bare-metal only (it needs percpu-ref, actor-table-base and the
;;;; rest of the arch hooks) and is not part of the hosted x64 CLI, so YIELD's
;;;; and RECEIVE's %gc-region-switch calls cannot be executed here.  What this
;;;; file exercises is the mechanism they call, driven through the same struct
;;;; offsets and the same zero-means-region-0 encoding.

(defvar *nlinks* 4000)
(defvar *fail* 0)
(defvar *checks* 0)

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

(let ((res (%gc-region-actors-selftest *nlinks*)))
  (if (= res 0)
      (format t "~%SKIP: the active region is too small to carve two regions from.~%")
      (let* ((rcb1 (w res 0))   (rcb2 (w res 8))   (r0 (w res 16))
             (k (w res 24))     (at (w res 32))
             (f1 (w res 40))    (t1 (w res 48))
             (f2 (w res 56))    (t2 (w res 64))
             (w1lo (w res 72))  (w1hi (w res 80))
             (w2lo (w res 88))  (w2hi (w res 96))
             (from0 (w res 104)) (new0 (w res 112)) (alloc0 (w res 120))
             (areg0 (w res 128)) (areg1 (w res 136)) (areg2 (w res 144))
             (asp0 (w res 152))  (asp1 (w res 160))  (asp2 (w res 168))
             (fill1 (w res 176)) (fill2 (w res 184))
             (chkA-before (w res 192)) (chkB-before (w res 200))
             (parked1 (w res 208)) (parksp1 (w res 216))
             (parked2 (w res 224)) (parksp2 (w res 232))
             (ctl1 (w res 240)) (ctl2 (w res 248))
             (g1-before (w res 256))
             (sum2-before (w res 264)) (rcb2sum-before (w res 272))
             (g2-before (w res 280))
             (sum0-before (w res 288)) (rcb0sum-before (w res 296))
             (g0-before (w res 304))
             (g1-after (w res 312)) (f1-after (w res 320)) (t1-after (w res 328))
             (alloc1-after (w res 336)) (chkA-after1 (w res 344))
             (refs1-new (w res 352)) (refs1-old (w res 360))
             (sum2-after (w res 368)) (rcb2sum-after (w res 376))
             (g2-after (w res 384))
             (sum0-after (w res 392)) (rcb0sum-after (w res 400))
             (g0-after (w res 408))
             (g2-before2 (w res 416)) (sum1-before2 (w res 424))
             (g1-before2 (w res 432)) (g2-after2 (w res 440))
             (f2-after (w res 448)) (alloc2-after (w res 456))
             (chkB-after1 (w res 464)) (refs2-new (w res 472))
             (sum1-after2 (w res 480)) (g1-after2b (w res 488))
             (g1-after3 (w res 496)) (chkA-after3 (w res 504))
             (alloc1-after3 (w res 512))
             (g2-after4 (w res 520)) (chkB-after4 (w res 528))
             (alloc2-after4 (w res 536))
             (refs-1-to-2 (w res 544)) (refs-2-to-1 (w res 552))
             (refs-1-to-0 (w res 560)) (refs-0-to-12 (w res 568))
             (pos-1 (w res 576)) (pos-2 (w res 584))
             (n (w res 592)) (s (w res 600))
             (back-home (w res 608)) (g0-final (w res 616))
             (r0-parked (w res 624))
             (wordA (w res 648)) (wordB (w res 656))
             (w0lo (w res 664))  (w0hi (w res 672))
             (want-chain (+ 1 (ash (* n (- n 1)) -1))))

        (format t "~%=== LAYOUT ===============================================~%")
        (format t "  metadata scale (1 = raw words, 2 = SHL'd)   ~D~%" k)
        (format t "  region 0 control block                      ~X~%" r0)
        (format t "  actor table (3 x 128 bytes)                 ~X~%" at)
        (format t "  actor 1's region / from / to                ~X ~X ~X~%" rcb1 f1 t1)
        (format t "  actor 2's region / from / to                ~X ~X ~X~%" rcb2 f2 t2)
        (format t "  actor 1's parked window                     [~X,~X)~%" w1lo w1hi)
        (format t "  actor 2's parked window                     [~X,~X)~%" w2lo w2hi)
        (format t "  actor 0's parked window                     [~X,~X)~%" w0lo w0hi)
        (format t "  region 0 semispace size after carve         ~X (~D MB)~%"
                new0 (ash new0 -20))
        (format t "  region 0 live bytes checksummed             ~D~%" (- alloc0 from0))
        (format t "  chain A head before ~X ; chain B head before ~X~%" wordA wordB)

        (format t "~%=== AN ACTOR NAMES ITS REGION (+0x68), 0 = REGION 0 ======~%")
        (chk-x "actor 0 owns no region, so it resolves to region 0" areg0 r0)
        (chk-x "actor 1's region" areg1 rcb1)
        (chk-x "actor 2's region" areg2 rcb2)
        (chk-x "actor 0's SP is its window's low end" asp0 w0lo)
        (chk-x "actor 1's SP is its window's low end" asp1 w1lo)
        (chk-x "actor 2's SP is its window's low end" asp2 w2lo)

        (format t "~%=== THE HOPS PARKED WHAT THEY LEFT =======================~%")
        (chk "region 1 reports parked after the hop away" parked1 1)
        (chk-x "and it parked at actor 1's OWN recorded SP" parksp1 asp1)
        (chk "region 2 reports parked after the hop away" parked2 1)
        (chk-x "and it parked at actor 2's OWN recorded SP" parksp2 asp2)
        (chk "chain A checksum before" chkA-before want-chain)
        (chk "chain B checksum before" chkB-before want-chain)
        (chk "bytes allocated in region 1" (- fill1 f1) (* 32 n))
        (chk "bytes allocated in region 2" (- fill2 f2) (* 32 n))
        (format t "  (positive controls for the foreign-ref oracle)~%")
        (chk "actor 1's window holds 1 ptr into region 1" ctl1 1)
        (chk "actor 2's window holds 1 ptr into region 2" ctl2 1)

        (format t "~%=== STOP 1: COLLECT REGION 1, PARKED ON ITS OWN WINDOW ===~%")
        (chk "region 1 collections before" g1-before 0)
        (chk "region 1 collections after " g1-after 1)
        (chk-x "semispaces flipped: new from = old to" f1-after t1)
        (chk-x "semispaces flipped: new to   = old from" t1-after f1)
        (chk "chain A walks, read back from the parked slot" chkA-after1 want-chain)
        (chk "the window now holds 1 ptr into the NEW from-space" refs1-new 1)
        (chk "the window holds 0 ptrs into the OLD one" refs1-old 0)
        (chk "live bytes after = chain A + 1 junk cons"
             (- alloc1-after t1) (+ (* 16 n) 16))
        (format t "  info reclaimed ~D of ~D bytes~%"
                (- (- fill1 f1) (- alloc1-after t1)) (- fill1 f1))
        (format t "  -- and region 2 must be untouched by it --~%")
        (chk "region 2 heap checksum" sum2-after sum2-before)
        (chk "region 2 control block checksum" rcb2sum-after rcb2sum-before)
        (chk "region 2 collections" g2-after g2-before)
        (format t "  -- and so must region 0 --~%")
        (chk "region 0 heap checksum" sum0-after sum0-before)
        (chk "region 0 control block checksum" rcb0sum-after rcb0sum-before)
        (chk "region 0 collections" g0-after g0-before)

        (format t "~%=== STOP 2: HOP TO ACTOR 2, COLLECT REGION 2 =============~%")
        (chk "region 2 collections before" g2-before2 0)
        (chk "region 2 collections after " g2-after2 1)
        (chk-x "semispaces flipped: new from = old to" f2-after t2)
        (chk "chain B walks, read back from the parked slot" chkB-after1 want-chain)
        (chk "the window now holds 1 ptr into the NEW from-space" refs2-new 1)
        (chk "live bytes after = chain B + 1 junk cons"
             (- alloc2-after t2) (+ (* 16 n) 16))
        (format t "  -- and now the OTHER direction: region 1 untouched --~%")
        (chk "region 1 live-range checksum" sum1-after2 sum1-before2)
        (chk "region 1 collections" g1-after2b g1-before2)

        (format t "~%=== STOPS 3 AND 4: ROUND ROBIN AGAIN =====================~%")
        (chk "region 1 collections" g1-after3 2)
        (chk "chain A still walks" chkA-after3 want-chain)
        (chk "region 1 live bytes (back in its original from-space)"
             (- alloc1-after3 f1) (+ (* 16 n) 16))
        (chk "region 2 collections" g2-after4 2)
        (chk "chain B still walks" chkB-after4 want-chain)
        (chk "region 2 live bytes (back in its original from-space)"
             (- alloc2-after4 f2) (+ (* 16 n) 16))
        (chk "actor 1's window still holds exactly 1 live ptr" pos-1 1)
        (chk "actor 2's window still holds exactly 1 live ptr" pos-2 1)

        (format t "~%=== THE ASSUMPTION, AUDITED IN EVERY DIRECTION ===========~%")
        (format t "  Per-actor collection is sound only because no actor can~%")
        (format t "  hold a pointer into another actor's region (messages are~%")
        (format t "  term-serialized).  The collector does NOT enforce that,~%")
        (format t "  and there is no write barrier.~%")
        (chk "region 1's live data -> either of region 2's semispaces" refs-1-to-2 0)
        (chk "region 2's live data -> either of region 1's semispaces" refs-2-to-1 0)
        (chk "region 1's live data -> region 0's from-space" refs-1-to-0 0)
        (chk "region 0's live data -> any semispace of 1 or 2" refs-0-to-12 0)

        (format t "~%=== HOP HOME: THE ACTOR THAT OWNS NO REGION ==============~%")
        (chk-x "active region is region 0's block again" back-home r0)
        (chk "region 0 is RUNNING again, not parked" r0-parked 0)
        (chk "region 0 still never collected" g0-final g0-before)

        (format t "~%=== VERDICT ==============================================~%")
        (if (= *fail* 0)
            (format t "PER-ACTOR REGIONS: PASS (~D checks)~%" *checks*)
            (format t "PER-ACTOR REGIONS: FAIL (~D of ~D checks)~%" *fail* *checks*)))))

;; Region 0 must still collect correctly after the carve and after two other
;; regions have been round-robin collected out from under it.
(let ((keep nil) (n 0))
  (dotimes (i 100000)
    (let ((c (cons i (list i (+ i 1) "after-carve"))))
      (when (= 0 (mod i 1000))
        (setq keep (cons c keep))
        (setq n (+ n 1)))))
  (let ((sum 0) (bad 0))
    (dolist (c keep)
      (setq sum (+ sum (car c)))
      (unless (and (= (car (cdr c)) (car c))
                   (= (car (cdr (cdr c))) (+ (car c) 1))
                   (string= (car (cdr (cdr (cdr c)))) "after-carve"))
        (setq bad (+ bad 1))))
    (format t "~%REGION 0 AFTER CARVE: retained=~D checksum=~D (expect 4950000) bad=~D~%"
            n sum bad)))
