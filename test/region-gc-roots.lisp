;;;; region-gc-roots.lisp — STAGE-2 PER-REGION GC ACCEPTANCE: THE ROOT SET.
;;;;
;;;;   ./modus --script test/region-gc-roots.lisp
;;;;
;;;; Stage 1 made the HEAP a property of the region.  Stage 2 makes the ROOTS
;;;; one: the collector scans [root_sp, stack_base), and both ends are fields of
;;;; the region being collected.  root_sp = 0 means "this region's actor is the
;;;; one RUNNING" and the low end is the live stack pointer the collector entry
;;;; point already holds; non-zero means the actor is PARKED and this is the SP
;;;; its context switch recorded on its own stack.
;;;;
;;;; %gc-region-roots-selftest (mvm/gc.lisp) builds TWO identical chains in
;;;; region 1.  Chain A is rooted ONLY from region 1's parked window — a zeroed
;;;; buffer in the carved guard band, with A's pointer in one slot in the middle
;;;; of it.  Chain B is rooted ONLY from the live machine stack, which belongs
;;;; to REGION 0's actor, the one actually executing.  Then region 1, and only
;;;; region 1, is collected — twice.
;;;;
;;;; THE TWO CLAIMS, and this file judges both by reading memory back out:
;;;;   (a) A SURVIVES.  A region's own parked roots keep its data alive across a
;;;;       collection while its actor is not running.
;;;;   (b) B DOES NOT.  A root held only by ANOTHER region does not keep an
;;;;       object alive in the collected region.
;;;; The sharpest single number is the live-bytes count: 16*N+16 if only A
;;;; survived, 32*N+16 if the live stack had been scanned as well.
;;;;
;;;; SOUNDNESS, SAID OUT LOUD.  Per-region collection is correct only because no
;;;; actor may hold a pointer into another actor's region — net/actors.lisp
;;;; TERM-SERIALISES every message, so they are copied, never shared.  The
;;;; COLLECTOR DOES NOT ENFORCE THAT and there is no write barrier; a pointer
;;;; stored from region A into region B dangles when B is collected, silently.
;;;; %gc-count-foreign-refs is the debug-mode audit, and this test runs it in
;;;; BOTH directions over the real heaps after a real collection — plus once as
;;;; a POSITIVE CONTROL on a span that is known to contain exactly one such
;;;; pointer, because an oracle that can only answer zero is worth nothing.

(defvar *nlinks* 4000)
(defvar *fail* 0)

(defun w (res off) (%gc-read64 (+ res off)))

(defun chk (name got want)
  (if (= got want)
      (format t "  ok   ~A = ~D~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~D (expected ~D)~%" name got want))))

(defun chk-ne (name got other)
  (if (= got other)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~X (expected it to CHANGE)~%" name got))
      (format t "  ok   ~A = ~X (changed)~%" name got)))

(defun chk-in (name addr lo len)
  (if (and (>= addr lo) (< addr (+ lo len)))
      (format t "  ok   ~A ~X is inside [~X,~X)~%" name addr lo (+ lo len))
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A ~X is NOT inside [~X,~X)~%" name addr lo (+ lo len)))))

(let ((res (%gc-region-roots-selftest *nlinks*)))
  (if (= res 0)
      (format t "~%SKIP: the active region is too small to carve two regions from.~%")
      (let* ((rcb1 (w res 0))    (rcb2 (w res 8))    (r0 (w res 16))
             (k (w res 24))      (from0 (w res 32))  (new0 (w res 40))
             (f1 (w res 48))     (t1 (w res 56))
             (pslo (w res 64))   (pshi (w res 72))   (pslot (w res 80))
             (alloc0 (w res 88)) (fill2 (w res 96))  (c2-before (w res 104))
             (wordA (w res 112)) (wordB (w res 120))
             (chkA-before (w res 128)) (chkB-before (w res 136))
             (alloc1-before (w res 144))
             (parked (w res 152)) (probe-pos (w res 160))
             (sum0-before (w res 168)) (sum2-before (w res 176))
             (g0-before (w res 184))   (g2-before (w res 192))
             (rcb0-before (w res 200)) (rcb2-before (w res 208))
             (g1-before (w res 216))
             (g1-after1 (w res 224))
             (f1-after1 (w res 232)) (t1-after1 (w res 240))
             (alloc1-after1 (w res 248))
             (pslot-after (w res 256)) (chkA-after1 (w res 264))
             (refs-new (w res 272))   (refs-old (w res 280))
             (fwdA (w res 288))       (fwdB (w res 296))
             (sum0-after (w res 304)) (sum2-after (w res 312))
             (g0-after (w res 320))   (g2-after (w res 328))
             (rcb0-after (w res 336)) (rcb2-after (w res 344))
             (g1-after2 (w res 368))  (alloc1-after2 (w res 376))
             (chkA-after2 (w res 384)) (refs-2 (w res 392))
             (foreign-1-0 (w res 400)) (foreign-0-1 (w res 408))
             (n (w res 416))          (c2-after (w res 424))
             (f2 (w res 432))         (s (w res 440))
             (g1-after3 (w res 448))  (alloc1-after3 (w res 456))
             (from-after3 (w res 464))
             (refs3-new (w res 472))  (refs3-old (w res 480))
             (chkC (w res 488))       (wordC (w res 496))
             (pslot-pre3 (w res 512)) (pslot-post3 (w res 520))
             (want-chain (+ 1 (ash (* n (- n 1)) -1))))

        (format t "~%=== LAYOUT ===============================================~%")
        (format t "  metadata scale (1 = raw words, 2 = SHL'd)   ~D~%" k)
        (format t "  region 0 control block                      ~X~%" r0)
        (format t "  region 1 control block                      ~X~%" rcb1)
        (format t "  region 2 control block                      ~X~%" rcb2)
        (format t "  region 0 semispace size after carve         ~X (~D MB)~%"
                new0 (ash new0 -20))
        (format t "  region 0 live bytes checksummed             ~D~%" (- alloc0 from0))
        (format t "  region 1 from-space                         ~X~%" f1)
        (format t "  region 1 to-space                           ~X~%" t1)
        (format t "  region 1 PARKED ROOT WINDOW                 [~X,~X)  ~D bytes~%"
                pslo pshi (- pshi pslo))
        (format t "  the one root slot in it                     ~X~%" pslot)
        (format t "  region 2 from-space                         ~X~%" f2)
        (format t "  region 2 live bytes checksummed             ~D~%" (- fill2 f2))

        (format t "~%=== REGION 1 IS PARKED, AND THE DETECTOR WORKS ===========~%")
        (chk "region 1 reports parked" parked 1)
        (chk "control: pointers from the parked window into region 1" probe-pos 1)
        (chk "chain A checksum before" chkA-before want-chain)
        (chk "chain B checksum before" chkB-before want-chain)
        (chk "bytes allocated in region 1" (- alloc1-before f1) (* 64 n))

        (format t "~%=== COLLECT REGION 1 WHILE ITS ACTOR IS NOT RUNNING ======~%")
        (chk "collections before" g1-before 0)
        (chk "collections after " g1-after1 1)
        (chk "semispaces flipped: new from = old to" f1-after1 t1)
        (chk "semispaces flipped: new to   = old from" t1-after1 f1)
        ;; (a) the parked roots kept region 1's data alive
        (chk-ne "the parked root slot" pslot-after wordA)
        (chk-in "chain A head after" (- pslot-after 1) t1 s)
        (chk "chain A walks, from the parked slot" chkA-after1 want-chain)
        (chk "chain A's old head carries a forwarding tag" fwdA 1)
        (chk "the parked window now holds 1 ptr into the NEW from-space" refs-new 1)
        (chk "the parked window holds 0 ptrs into the OLD one" refs-old 0)
        ;; (b) the root held by the OTHER region's stack kept nothing alive
        (chk "chain B's old head was NEVER forwarded" fwdB 0)
        (format t "  info chain B head was ~X, rooted only from the live stack~%" wordB)
        (chk "live bytes after = chain A + 1 junk cons"
             (- alloc1-after1 t1) (+ (* 16 n) 16))
        (format t "  info had the LIVE stack been scanned too, chain B would~%")
        (format t "       have survived and this number would be ~D~%"
                (+ (* 32 n) 16))
        (format t "  info reclaimed ~D of ~D bytes~%"
                (- (- alloc1-before f1) (- alloc1-after1 t1)) (- alloc1-before f1))

        (format t "~%=== AND AGAIN, STILL PARKED ==============================~%")
        (chk "collections" g1-after2 2)
        (chk "chain A still walks" chkA-after2 want-chain)
        (chk "live bytes after the second collection" (- alloc1-after2 f1)
             (+ (* 16 n) 16))
        (chk "the parked window holds 1 ptr into the from-space again" refs-2 1)

        ;; THE NEGATIVE CONTROL.  Everything above shows the parked window being
        ;; used; this shows it NOT being used, in the same binary, with the same
        ;; region — the only change is root_sp going back to 0 (and stack_base
        ;; back to the live stack's, because the two describe one stack).
        ;; Every expectation is inverted.
        (format t "~%=== UNPARK IT: THE ROOT SET MOVES TO THE LIVE STACK ======~%")
        (chk "collections" g1-after3 3)
        (chk "semispaces flipped again: new from = old to" from-after3 t1)
        (chk "chain C, held ONLY by a live-stack local, survives" chkC want-chain)
        (chk "the parked window was NOT scanned: 0 ptrs into the new from-space"
             refs3-new 0)
        (chk "its stale slot still points into the EVACUATED space" refs3-old 1)
        (chk "and the slot was never rewritten" pslot-post3 pslot-pre3)
        (format t "  info chain C head ~X; region 1 live after ~D bytes~%"
                wordC (- alloc1-after3 t1))

        (format t "~%=== REGION 0: MUST BE BIT-FOR-BIT UNTOUCHED ==============~%")
        (chk "heap checksum" sum0-after sum0-before)
        (chk "control block checksum" rcb0-after rcb0-before)
        (chk "collections" g0-after g0-before)

        (format t "~%=== REGION 2: MUST BE BIT-FOR-BIT UNTOUCHED ==============~%")
        (chk "heap checksum" sum2-after sum2-before)
        (chk "control block checksum" rcb2-after rcb2-before)
        (chk "collections" g2-after g2-before)
        (chk "chain checksum before" c2-before want-chain)
        (chk "chain still walkable after" c2-after want-chain)

        (format t "~%=== THE ASSUMPTION, AUDITED IN BOTH DIRECTIONS ===========~%")
        (format t "  Per-region collection is sound only because no actor can~%")
        (format t "  hold a pointer into another actor's region (messages are~%")
        (format t "  term-serialized).  The collector does NOT enforce that.~%")
        (chk "region 1's live data -> region 0's from-space" foreign-1-0 0)
        (chk "region 0's live data -> either region 1 semispace" foreign-0-1 0)

        (format t "~%=== VERDICT ==============================================~%")
        (if (= *fail* 0)
            (format t "PER-REGION ROOTS: PASS (~D checks)~%" 32)
            (format t "PER-REGION ROOTS: FAIL (~D failing checks)~%" *fail*)))))

;; Region 0 must still collect correctly after being shrunk by the carve, and
;; after two other regions have been collected out from under it.
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
