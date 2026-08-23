;;;; hosted-thread-gc-stress.lisp — LONG-RUNNING CONCURRENT COLLECTION.
;;;;
;;;;   ./modus --script test/hosted-thread-gc-stress.lisp
;;;;
;;;; test/hosted-thread-gc-concurrent.lisp proves the two collections OVERLAP
;;;; IN TIME and that the isolation audit holds for one 48-message run.  This
;;;; one asks the other question: does it keep holding.  ROUNDS whole two-thread
;;;; runs, each with a large live chain on EVERY actor and a forced collection
;;;; per message on BOTH threads, and after each round the same audit — nothing
;;;; corrupted, the counts rose independently, the other region bit-for-bit
;;;; unchanged, no cross-region reference in either direction.
;;;;
;;;; NMSG IS 64 AND NOT MORE, deliberately.  The message log is 0x800 bytes at
;;;; band+0x800 — sixty-four 32-byte entries — and the next thing above it is
;;;; PERCPU-DATA-BASE, the block the GS base points at.  A longer single run
;;;; would log through current-actor, idle-flag and cpu-id.  %HA-LOG-CAP now
;;;; caps the writers so it cannot, and this test gets its length from ROUNDS
;;;; instead — which is the better shape anyway, because each round re-runs the
;;;; whole bring-up: fresh actors, a fresh second thread, freshly initialised
;;;; regions, and therefore fresh alignment checks.
;;;;
;;;; WHAT "NO CORRUPTION" MEANS HERE, precisely.  Each actor builds an
;;;; NLINKS-cons chain in its own frame and re-walks it after every collection,
;;;; comparing against an exact expected sum; a single moved-and-not-forwarded
;;;; cons changes that sum.  The message in flight is re-checked AFTER the
;;;; collection of the region it was decoded into.  And after the join, thread
;;;; 1 collects its own region twice while thread 2's live heap and control
;;;; block must not move a single byte — with the checksum asserted NON-ZERO
;;;; first, because a checksum over an empty range is a check that can only
;;;; answer 0.

(defvar *fail* 0)
(defvar *checks* 0)
(defvar *rounds* 24)
(defvar *nmsg* 64)
(defvar *nlinks* 2000)
(defvar *budget* 200000000)
(defvar *barrier* 20000000)

(defvar *tot-witness* 0)
(defvar *tot-met* 0)
(defvar *tot-collections* 0)

(defun w (res off) (%gc-read64 (+ res off)))

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (= got want)
      0
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~D (expected ~D)~%" name got want))))

(defun chk-pos (name got)
  (setq *checks* (+ *checks* 1))
  (if (> got 0)
      0
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~D (expected > 0)~%" name got))))

(format t "~%LONG-RUNNING CONCURRENT COLLECTION~%")
(format t "  ~D rounds x ~D messages, ~D-cons chain live on EVERY actor,~%"
        *rounds* *nmsg* *nlinks*)
(format t "  a forced collection per message on BOTH threads, no lock.~%~%")

(let ((r 1))
  (loop
    (when (> r *rounds*) (return 0))
    (let ((out (%ha-mt-conc-selftest *nmsg* *budget* *nlinks* 0 *barrier*)))
      (if (zerop out)
          (progn (setq *fail* (+ *fail* 1))
                 (format t "FAIL round ~D: no band~%" r))
          (let ((res (w out #x28)))
            (if (zerop res)
                (progn (setq *fail* (+ *fail* 1))
                       (format t "FAIL round ~D: selftest returned 0~%" r))
                (progn
                  ;; ---- the round ran ----
                  (chk "thread 2 joined"        (w res #x00) 0)
                  (chk "acks collected"         (w res #x20) *nmsg*)
                  (chk "actor 2 forwarded"      (w res #x30) *nmsg*)
                  (chk "actor 3 logged"         (w res #x38) *nmsg*)
                  (chk "actor 2 struct errors"  (w res #x40) 0)
                  (chk "actor 3 struct errors"  (w res #x48) 0)
                  (chk "driver bad acks"        (w res #x50) 0)
                  (chk "ap-scheduler entries"   (w res #x58) 0)
                  ;; ---- both threads collected, independently ----
                  (chk "thread 1 collections"   (w out #x40) *nmsg*)
                  (chk "thread 2 collections"   (w out #x48) *nmsg*)
                  (chk "thread 2 region count"  (w res #x168) *nmsg*)
                  (chk "thread 1 region count"  (w res #x170) *nmsg*)
                  ;; ---- nothing was corrupted ----
                  (chk "actor 1 chain failures" (w res #x150) 0)
                  (chk "actor 2 chain failures" (w res #x140) 0)
                  (chk "actor 3 chain failures" (w res #x148) 0)
                  (chk-pos "thread 2 heap checksum is real"  (w res #x1D0))
                  (chk "thread 2 heap unchanged" (w res #x1D8) (w res #x1D0))
                  (chk-pos "thread 2 ctl checksum is real"   (w res #x1E0))
                  (chk "thread 2 ctl unchanged"  (w res #x1E8) (w res #x1E0))
                  (chk "thread 2 count unchanged" (w res #x1F8) (w res #x1F0))
                  (chk "thread 1 count +2" (w res #x208) (+ (w res #x200) 2))
                  (chk "region 0 frontier restored" (w res #x1C0) (w res #x1B8))
                  ;; ---- isolation, with the positive control ----
                  (chk "refs t2 -> t1" (w res #x188) 0)
                  (chk "refs t1 -> t2" (w res #x190) 0)
                  (chk "positive control" (w res #x198) 1)
                  ;; ---- the alignment invariant, re-checked every round ----
                  (chk "alignment violations" (w out #x18) 0)
                  (chk "region 0 alignment mask" (w out #x60) 0)
                  ;; ---- the collectors are all out ----
                  (chk "collectors still inside" (w out #x10) 0)
                  (setq *tot-witness* (+ *tot-witness* (w out #x00)))
                  (setq *tot-met* (+ *tot-met* (w out #x08)))
                  (setq *tot-collections*
                        (+ *tot-collections*
                           (+ (w out #x40) (+ (w out #x48) 2))))
                  (format t "  round ~D: ok  (~D collections, ~D witnesses, ~D met)~%"
                          r (+ (w out #x40) (+ (w out #x48) 2))
                          (w out #x00) (w out #x08)))))))
    (setq r (+ r 1))))

(format t "~%TOTALS~%")
(format t "  collections            ~D~%" *tot-collections*)
(format t "  overlap witnesses      ~D~%" *tot-witness*)
(format t "  barrier meetings       ~D~%" *tot-met*)
(chk "total collections" *tot-collections*
     (* *rounds* (+ (* 2 *nmsg*) 2)))
(chk-pos "overlap witnessed across the run" *tot-witness*)
(chk-pos "barrier met across the run" *tot-met*)

(format t "~%")
(if (zerop *fail*)
    (format t "CONCURRENT COLLECTION STRESS: PASS (~D checks)~%" *checks*)
    (format t "CONCURRENT COLLECTION STRESS: FAIL (~D of ~D checks)~%"
            *fail* *checks*))
