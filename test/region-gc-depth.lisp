;;;; region-gc-depth.lisp — THE COLLECTOR ENTRY POINT MUST LEAVE NO TRACE.
;;;;
;;;;   ./modus --script test/region-gc-depth.lisp
;;;;
;;;; Every other forced-collection test in this tree drives %gc-collect-here
;;;; from ONE call site, so every collection sees the SAME stack pointer.  That
;;;; makes a STALE saved_sp indistinguishable from a fresh one, and a stale
;;;; saved_sp is silent heap corruption: real collections trigger from arbitrary
;;;; allocation sites at arbitrary depths, and if a collector entry point writes
;;;; the live SP into the region's +0x28 and never puts back what it found, the
;;;; SECOND collection scans [SP-of-the-FIRST, stack_base).  Stacks grow down,
;;;; so a deeper second collection gets a window that starts ABOVE its own live
;;;; frames — including the entry point's own register-save area.  Those roots
;;;; are never forwarded, from-space is reclaimed under them, and every
;;;; collection from the second onward corrupts the heap.
;;;;
;;;; %gc-region-depth-selftest (mvm/gc.lisp) carves ONE region, collects it at
;;;; the harness's own depth, then descends DEPTH real machine frames (the
;;;; recursive call is not in tail position, so the SP genuinely falls), builds
;;;; a chain rooted ONLY by that deep frame's local, and collects again.
;;;;
;;;; TWO ORACLES:
;;;;   (a) saved_sp AFTER a collection is exactly what it was BEFORE — 0 for a
;;;;       running region.  Exact, deterministic, and the direct statement of
;;;;       the invariant.
;;;;   (b) the deep frame's chain survives the collection made from that frame.
;;;;       A stale window makes it unreachable and the region's live bytes
;;;;       collapse to the one junk cons %gc-collect-here allocates.  Bounded
;;;;       rather than equated: a RUNNING region's window is the whole live
;;;;       stack, and a conservative scan may retain a few dead conses off stale
;;;;       slots, so the assertion is >= the chain and < the chain + slack.
;;;;
;;;; WHERE THIS DISCRIMINATES.  translate-x64's trampoline and translate-i386's
;;;; collector only ever READ +0x28, so this passes there by construction and is
;;;; a regression guard.  translate-aarch64's shim is the one entry point in the
;;;; tree that WRITES the field, so it is the one this is actually about — and
;;;; it can only be executed on a bare-metal aarch64 image built on the shim
;;;; path (*aarch64-gc-native-mcgc* NIL).

(defvar *nlinks* 4000)
(defvar *depth* 200)
(defvar *fail* 0)
(defvar *checks* 0)

(defun w (res off) (%gc-read64 (+ res off)))

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (= got want)
      (format t "  ok   ~A = ~D~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~D (expected ~D)~%" name got want))))

(defun chk-range (name got lo hi)
  (setq *checks* (+ *checks* 1))
  (if (and (>= got lo) (< got hi))
      (format t "  ok   ~A = ~D  (in [~D,~D))~%" name got lo hi)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~D (expected it in [~D,~D))~%" name got lo hi))))

(let ((res (%gc-region-depth-selftest *nlinks* *depth*)))
  (if (= res 0)
      (format t "~%SKIP: the active region is too small to carve a region from.~%")
      (let* ((rcb (w res 0))   (r0 (w res 8))    (k (w res 16))
             (f1 (w res 24))   (t1 (w res 32))   (new0 (w res 40))
             (sp-before-1 (w res 64)) (g-after-1 (w res 72))
             (sp-after-1 (w res 80))  (alloc-after-1 (w res 88))
             (sp-before-2 (w res 96)) (g-after-2 (w res 104))
             (from-after-2 (w res 112)) (alloc-after-2 (w res 120))
             (chain-after-2 (w res 128))
             (n (w res 136)) (depth (w res 144)) (s (w res 152))
             (want-chain (+ 1 (ash (* n (- n 1)) -1))))

        (format t "~%=== LAYOUT ===============================================~%")
        (format t "  metadata scale (1 = raw words, 2 = SHL'd)   ~D~%" k)
        (format t "  region 0 control block                      ~X~%" r0)
        (format t "  carved region control block                 ~X~%" rcb)
        (format t "  its semispaces                              ~X / ~X  (~D MB each)~%"
                f1 t1 (ash s -20))
        (format t "  region 0 semispace size after carve         ~X (~D MB)~%"
                new0 (ash new0 -20))
        (format t "  chain links / extra frames for GC #2        ~D / ~D~%" n depth)

        (format t "~%=== COLLECTION #1, AT THE HARNESS'S OWN DEPTH ============~%")
        (chk "saved_sp before it (0 = RUNNING)" sp-before-1 0)
        (chk "collections after it" g-after-1 1)
        (format t "  info alloc pointer after it ~X (the region flipped, so~%"
                alloc-after-1)
        (format t "       this is in what was to-space at ~X)~%" t1)
        (format t "  THE INVARIANT: a collector entry point must put back what~%")
        (format t "  it found.  A non-zero value here is a stale SP that the~%")
        (format t "  NEXT collection will scan from, wherever it happens.~%")
        (chk "saved_sp AFTER it is still 0" sp-after-1 0)

        (format t "~%=== COLLECTION #2, ~D FRAMES DEEPER ====================~%" depth)
        (chk "saved_sp as seen at that depth, before it" sp-before-2 0)
        (chk "collections after it" g-after-2 2)
        (chk "the deep frame's chain still walks" chain-after-2 want-chain)
        (chk-range "live bytes after it"
                   (- alloc-after-2 from-after-2)
                   (* 16 n) (+ (* 16 n) 4096))
        (format t "  info a stale window would make this ~D — the junk cons alone~%" 16)

        (format t "~%=== VERDICT ==============================================~%")
        (if (= *fail* 0)
            (format t "DEPTH-VARYING COLLECTION: PASS (~D checks)~%" *checks*)
            (format t "DEPTH-VARYING COLLECTION: FAIL (~D of ~D checks)~%"
                    *fail* *checks*)))))
