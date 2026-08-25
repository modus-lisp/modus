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
  "region 0's live span -> this worker's region.  MUST be zero."
  (let* ((k (%gc-meta-scale)) (rw (%gc-region))
         (wfrom (%gc-meta-read (+ rw #x00) k))
         (wsize (%gc-meta-read (+ rw #x10) k))
         (r0from (%gc-meta-read (+ r0 #x00) k))
         (r0alloc (%gc-meta-read (+ r0 #x30) k)))
    (%gc-count-foreign-refs r0from r0alloc wfrom wsize)))

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

(defvar *r0* 0)
(setq *r0* (%gc-region-0))

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
    (chk "NO pointer from region 0 into the worker's region" fwd 0)))

(format t "~&~%=== A SYMBOL: same shape ===~%")
(let* ((buf (staging-base-addr))
       (r0 *r0*))
  (term-encode (intern "XRT-DEMO-SYMBOL" "COMMON-LISP-USER") buf)
  (let* ((res (sb-thread:join-thread
               (sb-thread:make-thread (lambda () (xr-worker buf r0)) :name "xr-sym")))
         (got (nth 0 res)) (fwd (nth 1 res)) (bwd (nth 2 res)))
    (format t "~&foreign refs:  region0 -> worker = ~d    worker -> region0 = ~d~%" fwd bwd)
    (chk-true "the symbol really was decoded" (symbolp got))
    (chk "its name survived" (symbol-name got) "XRT-DEMO-SYMBOL")
    ;; THIS IS THE ONE THAT IS STILL RED, AND IT IS THE POINT OF THE FILE.
    ;; "The receiver interns, so the symbol and the table that points at it are
    ;; co-located by construction" — that argument is WRONG, and this number is
    ;; how we know.  The receiver does intern, and the symbol IS allocated in
    ;; the receiver's region; but the intern table is the SHARED one at
    ;; 0x10000088, which lives in REGION 0.  So the forbidden pointer is not
    ;; avoided, only moved: region 0's table now points at the RECEIVER's
    ;; symbol instead of the sender's.
    ;;
    ;; Serialising was still necessary — the string above is 0, where before
    ;; the fix it shipped a raw pointer — but it is not sufficient for symbols.
    ;; This goes to 0 when fresh interns are delegated to the region's owner,
    ;; and that is the acceptance criterion for that work.
    (chk "NO pointer from region 0 into the worker's region" fwd 0)))

(format t "~&~%=== THE INSTRUMENT IS NOT BLIND ===~%")
(format t "~&No separate positive control is needed, and one was REMOVED rather than~%")
(format t "~&kept: interning 400 fresh symbols on a worker to force the violation~%")
(format t "~&takes the process down (that IS the defect — test/run-worker-intern.sh),~%")
(format t "~&so it could never report.  The SYMBOL case above is the control: it is~%")
(format t "~&the same audit, in the same direction, over the same spans, and it~%")
(format t "~&answers NON-ZERO while the STRING case answers ZERO.  An audit that~%")
(format t "~&distinguishes those two cannot be reporting 0 because it is looking at~%")
(format t "~&nothing.~%")

(format t "~&~%~d checks, ~d failed~%" *checks* *fail*)
(if (= *fail* 0)
    (format t "CROSS-REGION MESSAGE AUDIT: PASS~%")
    (format t "CROSS-REGION MESSAGE AUDIT: FAIL~%"))
(finish-output)

;;; TOPLEVEL — SYS-EXIT from inside a nested LET*/IF in a --script does not take
;;; effect; see test/hosted-worker-xregion.lisp.
(sys-exit (if (= *fail* 0) 0 1))
