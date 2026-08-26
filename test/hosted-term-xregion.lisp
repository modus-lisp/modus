;;;; hosted-term-xregion.lisp — DECODE IN ANOTHER REGION, AND AUDIT BOTH WAYS.
;;;;
;;;;   ./modus --script test/hosted-term-xregion.lisp
;;;;
;;;; ============================================================
;;;; THE CLAIM UNDER TEST
;;;; ============================================================
;;;;
;;;; The serialiser was made to copy rather than share so that a message could
;;;; cross a region boundary without leaving a pointer behind.
;;;; test/hosted-term-roundtrip.lisp proves the COPY half — values survive and
;;;; are not the sender's objects — but it sends to SELF, one actor in one
;;;; region, so it cannot see a cross-region pointer even in principle.
;;;;
;;;; This is the other half.  The SENDER is the main thread, which owns region
;;;; 0.  The RECEIVER is a worker thread, which owns its own region — measured
;;;; here, not assumed, and the test refuses to grade itself if the two turn out
;;;; to be one region.
;;;;
;;;; ============================================================
;;;; THE TWO DIRECTIONS ARE NOT THE SAME REQUIREMENT
;;;; ============================================================
;;;;
;;;; This file originally demanded ZERO both ways.  That was wrong, and the
;;;; measurement said so: the backward direction reports ~150 for a worker that
;;;; has merely started up.
;;;;
;;;;   region 0 -> the worker's region     MUST BE ZERO.  This is the FORBIDDEN
;;;;       direction and the one that is fatal: those pointers are not on the
;;;;       worker's stack, so the worker's own collector never updates them, and
;;;;       they dangle the moment its region collects.  That is the mechanism
;;;;       behind the SIGSEGV in test/run-worker-intern.sh.
;;;;
;;;;   the worker's region -> region 0     IS EXPECTED TO BE NON-ZERO and is not
;;;;       a violation of anything today.  Every symbol, every global, every
;;;;       shared table entry a worker touches is a region-0 object, and a
;;;;       worker that referenced none of them could not run Lisp.  What it DOES
;;;;       mean is that region 0 must never collect out from under a live worker
;;;;       — an invariant this tree relies on and does not enforce, and which no
;;;;       test yet exercises.  Reported here as a number so it is at least
;;;;       visible.
;;;;
;;;; ============================================================
;;;; WHAT WOULD MAKE THIS A LIE
;;;; ============================================================
;;;;
;;;;   THE AUDIT MUST BE ABLE TO ANSWER NON-ZERO IN THE DIRECTION BEING
;;;;   ASSERTED.  A scan over an empty range reports 0 and looks like success.
;;;;   Here the STRING case and the SYMBOL case are the same audit over the same
;;;;   spans in the same direction, and they disagree — 0 against 29 — so the
;;;;   instrument is demonstrably not blind.  (A dedicated control that interned
;;;;   400 fresh symbols on the worker was tried and REMOVED: it takes the
;;;;   process down, because that is precisely the defect, so it could never
;;;;   report its own result.)
;;;;
;;;;   THE VALUE MUST REALLY HAVE BEEN DECODED, so the worker returns it and the
;;;;   main thread checks it.  An audit taken after a decode that quietly
;;;;   produced nothing is not a clean audit.

(defvar *fail* 0)
(defvar *checks* 0)

(defun chk-true (name got)
  (setq *checks* (+ *checks* 1))
  (if got
      (format t "ok   ~a~%" name)
      (progn (setq *fail* (+ *fail* 1)) (format t "FAIL ~a~%" name))))

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "ok   ~a = ~s~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "FAIL ~a: got ~s want ~s~%" name got want))))

(%ha-actors-bringup 4 0)

;;; --- the three things a worker does, kept as separate small functions -------
;;; Everything is passed in: reading a global on the worker would take the
;;; runtime lock and hop the active region, which is a mechanism under test.

(defun xr-decode (buf)
  (setf (mem-ref (decode-ptr-addr) :u64) buf)
  (term-decode-step))

(defun xr-fwd (r0)
  "region 0's live span AND the lock arena -> this worker's region.  MUST be
   zero.  THE ARENA IS PART OF REGION 0'S ADDRESS SPACE and is where every
   locked-section allocation lands since B-LITE (net/hosted-sync.lisp) — an
   audit that swept only [from, parked) would go green the moment the objects
   moved into a span it never looked at, which is exactly the wrong reason to
   go green.  %RT-ARENA-* answer 0 on a pre-arena binary, so there this sweep
   is byte-for-byte the old one."
  (let* ((k (%gc-meta-scale)) (rw (%gc-region))
         (wfrom (%gc-meta-read (+ rw #x00) k))
         (wsize (%gc-meta-read (+ rw #x10) k))
         (r0from (%gc-meta-read (+ r0 #x00) k))
         (r0alloc (%gc-meta-read (+ r0 #x30) k))
         (ab (%rt-arena-base))
         (aa (%rt-arena-alloc)))
    (+ (%gc-count-foreign-refs r0from r0alloc wfrom wsize)
       (if (> aa ab) (%gc-count-foreign-refs ab aa wfrom wsize) 0))))

(defun xr-bwd (r0)
  "This worker's live span -> region 0.  Expected non-zero; reported, not asserted."
  (let* ((k (%gc-meta-scale)) (rw (%gc-region))
         (wfrom (%gc-meta-read (+ rw #x00) k))
         (walloc (%gc-meta-read (+ rw #x30) k))
         (r0from (%gc-meta-read (+ r0 #x00) k))
         (r0size (%gc-meta-read (+ r0 #x10) k)))
    (%gc-count-foreign-refs wfrom walloc r0from r0size)))

(defun xr-worker (buf r0)
  (let ((got (xr-decode buf)))
    (list got (xr-fwd r0) (xr-bwd r0) (%gc-region))))

(defun xr-planted-control (r0 scr)
  "THE INSTRUMENT'S POSITIVE CONTROL, run ON THE WORKER: plant exactly one
   pointer to a worker-region cons in a raw scratch word and count refs over
   just that word toward this worker's region — the same function, the same
   direction, the same target span as the assertion.  Must answer exactly 1,
   and 0 after the word is cleared.  Needed because the fix drives BOTH the
   string and the symbol case to 0, which retires the old two-case
   non-blindness argument this file used to make."
  r0
  (let* ((k (%gc-meta-scale)) (rw (%gc-region))
         (wfrom (%gc-meta-read (+ rw #x00) k))
         (wsize (%gc-meta-read (+ rw #x10) k))
         (c (cons 11 22))
         (w (%gc-word-of c (+ scr 512))))
    (%gc-write64 (+ scr 520) w)
    (let ((hit (%gc-count-foreign-refs (+ scr 520) (+ scr 528) wfrom wsize)))
      (%gc-write64 (+ scr 520) 0)
      (list hit (%gc-count-foreign-refs (+ scr 520) (+ scr 528) wfrom wsize)))))

(defvar *r0* 0)
(setq *r0* (%gc-region-0))

;;; ============================================================
;;; ONE CASE PER PROCESS — because the AUDIT SWEEPS GARBAGE TOO
;;; ============================================================
;;;
;;; This file used to run the string case and then the symbol case in ONE
;;; process, and reported 29 for the symbol.  DECOMPOSED (2026-08-25, measured
;;; on the pre-fix binary): symbol case alone = 2 — the real defect, the
;;; worker-allocated symbol and its name — and the other 27 were POLLUTION:
;;; the STRING case's worker hands its result list back through the join box
;;; BY POINTER, main's evaluator traffics those worker-region pointers through
;;; its own heap, and the audit sweeps [from, parked) of region 0 — LIVE AND
;;; DEAD ALIKE — while the second worker REUSES the first's region span.  So
;;; case two's audit counted case one's leftovers.  Post-fix: symbol alone =
;;; 0; the 27 remain if the cases share a process.
;;;
;;; TWO CONSEQUENCES, kept honest here:
;;;   * XREGION_CASE=string|symbol runs ONE case; test/run-term-xregion.sh
;;;     runs each in its own process and both must audit 0.
;;;   * XREGION_CASE=joinshare DEMONSTRATES the residual defect the pollution
;;;     exposed — A THREAD'S RETURN VALUE CROSSES THE REGION BOUNDARY BY
;;;     POINTER (the join box is the same class of channel term-encode used to
;;;     be).  It runs the string case, then a second worker that decodes
;;;     NOTHING and merely audits: every ref it counts is the first worker's
;;;     result, held by main, pointing into the span the second worker now
;;;     owns.  EXPECTED NON-ZERO — it is a reproducer, and it doubles as the
;;;     in-vivo control that the audit can answer non-zero.

(defvar *case*
  (let ((s (%cli-getenv "XREGION_CASE")))
    (if (and s (> (length s) 0)) s "symbol")))

(defun xr-audit-only (r0)
  "A worker that decodes NOTHING: whatever it counts was already dangling
   over its span when it was born."
  (list nil (xr-fwd r0) (xr-bwd r0) (%gc-region)))

(when (or (string= *case* "string") (string= *case* "joinshare"))
  (format t "~&=== A STRING: encoded in region 0, decoded in a worker's region ===~%")
  (let* ((buf (staging-base-addr))
         (r0 *r0*))
    (term-encode "hello cross region" buf)
    (let* ((res (sb-thread:join-thread
                 (sb-thread:make-thread (lambda () (xr-worker buf r0)) :name "xr-str")))
           (got (nth 0 res)) (fwd (nth 1 res)) (bwd (nth 2 res)) (rw (nth 3 res)))
      (format t "~&worker region ~x   region 0 ~x~%" rw r0)
      (format t "~&foreign refs:  region0 -> worker = ~d    worker -> region0 = ~d~%" fwd bwd)
      (chk-true "the worker really has its OWN region (else every audit is trivial)"
                (not (eql rw r0)))
      (chk "the string really was decoded" got "hello cross region")
      (chk "NO pointer from region 0 into the worker's region" fwd 0))))

(when (string= *case* "symbol")
  (format t "~&=== A SYMBOL: encoded in region 0, decoded in a worker's region ===~%")
  (let* ((buf (staging-base-addr))
         (r0 *r0*))
    (term-encode (intern "XRT-DEMO-SYMBOL" "COMMON-LISP-USER") buf)
    (let* ((res (sb-thread:join-thread
                 (sb-thread:make-thread (lambda () (xr-worker buf r0)) :name "xr-sym")))
           (got (nth 0 res)) (fwd (nth 1 res)) (bwd (nth 2 res)))
      (format t "~&foreign refs:  region0 -> worker = ~d    worker -> region0 = ~d~%" fwd bwd)
      (chk-true "the symbol really was decoded" (symbolp got))
      (chk "its name survived" (symbol-name got) "XRT-DEMO-SYMBOL")
      ;; THIS WAS 2 ON THE PRE-FIX BINARY (single-process: 29, of which 27
      ;; were the pollution above) and its going to 0 was named as the
      ;; acceptance criterion for co-locating a worker's fresh interns with
      ;; the tables.  The fix that landed: CL:INTERN runs under the runtime
      ;; lock with the name COPIED there, so both land in the LOCK ARENA —
      ;; region-0 address space main cannot reach (B-LITE,
      ;; net/hosted-sync.lisp).  The sweep covers the arena, so this 0 is
      ;; measured where the objects actually are.
      (chk "NO pointer from region 0 into the worker's region" fwd 0))))

(when (string= *case* "joinshare")
  (format t "~&=== JOINSHARE: a second worker inherits the first's span ===~%")
  (let ((r0 *r0*))
    (let* ((res (sb-thread:join-thread
                 (sb-thread:make-thread (lambda () (xr-audit-only r0)) :name "xr-js")))
           (fwd (nth 1 res)))
      (format t "~&foreign refs:  region0 -> worker = ~d~%" fwd)
      (format t "~&Every one of those is the FIRST worker's result, handed to main~%")
      (format t "~&BY POINTER through the join box, now aimed at a span this worker~%")
      (format t "~&owns.  The residual defect, demonstrated rather than asserted.~%")
      (chk-true "the join-by-pointer channel is VISIBLE (expected non-zero)"
                (> fwd 0)))))

(format t "~&~%=== THE INSTRUMENT IS NOT BLIND ===~%")
(format t "~&The explicit control, on the worker: plant one pointer to a~%")
(format t "~&worker-region cons in a raw scratch word, count over exactly that~%")
(format t "~&word toward this worker's region — same function, same direction,~%")
(format t "~&same target span as the assertions — then clear it and count again.~%")
(let ((ctl (sb-thread:join-thread
            (sb-thread:make-thread
             (let ((r0 *r0*) (scr (%thr-scratch)))
               (lambda () (xr-planted-control r0 scr)))
             :name "xr-ctl"))))
  (chk "the planted forbidden pointer is COUNTED" (nth 0 ctl) 1)
  (chk "and cleared, the same span answers zero" (nth 1 ctl) 0))

(format t "~&~%~d checks, ~d failed~%" *checks* *fail*)
(if (= *fail* 0)
    (format t "CROSS-REGION MESSAGE AUDIT: PASS~%")
    (format t "CROSS-REGION MESSAGE AUDIT: FAIL~%"))
(finish-output)

;;; TOPLEVEL — SYS-EXIT from inside a nested LET*/IF in a --script does not take
;;; effect; see test/hosted-worker-xregion.lisp.
(sys-exit (if (= *fail* 0) 0 1))
