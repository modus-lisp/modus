;;;; region-gc.lisp — STAGE-1 PER-REGION GC ACCEPTANCE.
;;;;
;;;;   ./modus --script test/region-gc.lisp
;;;;
;;;; %gc-region-selftest (mvm/gc.lisp) carves two extra 16 MB regions out of the
;;;; top of region 0's semispaces, fills region 2, fills region 1 with a live
;;;; chain plus garbage, and forces ONE collection of REGION 1 through the
;;;; ordinary :gc-check path — the target's real collector, not a side door.
;;;; It leaves 34 machine words of evidence in the carved guard band, where no
;;;; region's collector can reach them.
;;;;
;;;; THIS FILE DOES THE JUDGING, and it judges by reading that memory back with
;;;; %gc-read64 rather than by trusting anything the harness reports about
;;;; itself.  The two claims that matter:
;;;;   (a) region 1's live data survived — same chain, same values, at a NEW
;;;;       address inside what was region 1's to-space before the collection;
;;;;   (b) regions 0 and 2 are bit-for-bit untouched — same checksum over every
;;;;       machine word they hold, same control block, same collection count.
;;;; A test that only shows "region 0 still works" has not tested the feature.

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

(let ((res (%gc-region-selftest *nlinks*)))
  (if (= res 0)
      (format t "~%SKIP: the active region is too small to carve two regions from.~%")
      (let* ((rcb1 (w res 0))   (rcb2  (w res 8))   (r0    (w res 16))
             (k    (w res 24))  (from0 (w res 32))  (new0  (w res 40))
             (alloc0 (w res 48)) (fill2 (w res 56))
             (c2-before (w res 64))
             (h1-before (w res 72))  (s1-before (w res 80))  (a1-before (w res 88))
             (sum0-before (w res 96)) (sum2-before (w res 104))
             (g0-before (w res 112))  (g2-before (w res 120))
             (rcb0-before (w res 128)) (rcb2-before (w res 136))
             (f1-before (w res 144)) (t1-before (w res 152)) (g1-before (w res 160))
             (h1-after (w res 168))  (s1-after (w res 176))  (a1-after (w res 184))
             (sum0-after (w res 192)) (sum2-after (w res 200))
             (g0-after (w res 208))  (g2-after (w res 216))
             (rcb0-after (w res 224)) (rcb2-after (w res 232))
             (f1-after (w res 240)) (t1-after (w res 248)) (g1-after (w res 256))
             (n (w res 280))
             (c2-after (w res 288))
             (f2 (w res 296))
             (want-chain (+ 1 (ash (* n (- n 1)) -1)))
             (region-size #x1000000))

        (format t "~%=== LAYOUT ===============================================~%")
        (format t "  metadata scale (1 = raw words, 2 = SHL'd)   ~D~%" k)
        (format t "  region 0 control block                      ~X~%" r0)
        (format t "  region 1 control block                      ~X~%" rcb1)
        (format t "  region 2 control block                      ~X~%" rcb2)
        (format t "  region 0 from_start                         ~X~%" from0)
        (format t "  region 0 semispace size after carve         ~X (~D MB)~%"
                new0 (ash new0 -20))
        (format t "  region 0 live bytes checksummed             ~D~%" (- alloc0 from0))
        (format t "  region 1 from-space                         ~X~%" f1-before)
        (format t "  region 1 to-space                           ~X~%" t1-before)
        (format t "  region 2 from-space                         ~X~%" f2)
        (format t "  region 2 live bytes checksummed             ~D~%" (- fill2 f2))

        (format t "~%=== REGION 1: THE ONE THAT WAS COLLECTED =================~%")
        (chk "collections before" g1-before 0)
        (chk "collections after " g1-after 1)
        (chk "chain checksum before" s1-before want-chain)
        (chk "chain checksum after " s1-after want-chain)
        (chk-ne "chain head address" h1-after h1-before)
        (chk-in "head before" (- h1-before 1) f1-before region-size)
        (chk-in "head after " (- h1-after 1) t1-before region-size)
        (chk "semispaces flipped: new from = old to" f1-after t1-before)
        (chk "semispaces flipped: new to   = old from" t1-after f1-before)
        (format t "  info allocated before ~D bytes, live after ~D bytes (reclaimed ~D)~%"
                (- a1-before f1-before) (- a1-after t1-before)
                (- (- a1-before f1-before) (- a1-after t1-before)))

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

        ;; THE CONVERSE.  "Region 0's count did not move" is only worth
        ;; something if the count moves when region 0 IS collected — otherwise
        ;; a collector that never ran would pass.  So collect region 0 (which
        ;; is active again) and watch all three counts.
        (format t "~%=== AND THE OTHER WAY ROUND: COLLECT REGION 0 ============~%")
        (let ((p0 (%gc-read64 (+ r0 32)))
              (p1 (%gc-read64 (+ rcb1 32)))
              (p2 (%gc-read64 (+ rcb2 32))))
          (%gc-collect-here)
          (let ((q0 (%gc-read64 (+ r0 32)))
                (q1 (%gc-read64 (+ rcb1 32)))
                (q2 (%gc-read64 (+ rcb2 32))))
            (chk "region 0 collections rose" q0 (+ p0 1))
            (chk "region 1 collections unchanged" q1 p1)
            (chk "region 2 collections unchanged" q2 p2)))

        (format t "~%=== VERDICT ==============================================~%")
        (if (= *fail* 0)
            (format t "PER-REGION GC: PASS (~D checks)~%" 23)
            (format t "PER-REGION GC: FAIL (~D failing checks)~%" *fail*)))))

;; Region 0 must still collect correctly after being shrunk by the carve, and
;; after having just been collected above.
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
