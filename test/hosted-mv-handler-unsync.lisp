;;;; hosted-mv-handler-unsync.lisp — THE NEGATIVE CONTROL, IN THE SAME BINARY.
;;;;
;;;;   ./modus --script test/hosted-mv-handler-unsync.lisp
;;;;
;;;; test/hosted-mv-handler.lisp's workload, with ONE word changed: MODE 1
;;;; makes thread 2 skip %TLS-INSTALL, so it keeps the FS base it was cloned
;;;; with — zero, the main thread's window.  The multiple-value buffer and the
;;;; handler-frame stack go back to being one copy for both threads.  Same
;;;; binary, same iterations, same collections, same everything else.
;;;;
;;;; IT IS A SEPARATE SCRIPT BECAUSE THE FAILURE IS NOT OBLIGED TO BE POLITE.
;;;; A wrong multiple value is a failed comparison and gets counted; a handler
;;;; frame restored from the other thread's stack is a JUMP, and where it lands
;;;; is not this script's to decide.  So this one asserts the thing that is
;;;; true either way: THE CONTROL MUST NOT COME BACK CLEAN.  A process that
;;;; dies here is evidence; a process that reports non-zero failure counters is
;;;; evidence; a process that reports everything zero means the fix under test
;;;; is not what makes the real test pass, and that is a FAIL.
;;;;
;;;; IT IS PROBABILISTIC, NOT DETERMINISTIC, AND THAT MATTERS TO WHOEVER READS
;;;; ITS RESULT.  The damage is a RACE between two threads over one window, so
;;;; a run in which the two never interleave badly comes back CLEAN.  MEASURED
;;;; INDEPENDENTLY, 2026-08-23: 11 of 12 runs detected the missing fix and ONE
;;;; SURVIVED.  A single clean run of this control is therefore not evidence
;;;; that the fix is unnecessary; only a batch is.  Run it at least a dozen
;;;; times and read the RATE.  (A later 12-run batch scored 12 of 12 — which is
;;;; the same statement about the same coin.)

(defvar *n* 400)
(defvar *gcevery* 25)

(format t "~%=== THE NEGATIVE CONTROL: ONE WINDOW, TWO THREADS =========~%")
(format t "  Thread 2 does NOT install its own window.  Expect damage.~%")
(format t "  (A crash here is a PASS for this script's purpose — it is~%")
(format t "   evidence the shared window is what the real test's fix fixes.)~%")

(let ((ctl (%mvhc-selftest 1 *n* *gcevery*)))
  (if (zerop ctl)
      (format t "  the selftest could not start (carve/mmap) — inconclusive~%")
      (let* ((s0 (+ ctl #x100))
             (s1 (+ ctl #x140))
             (bad (+ (+ (%gc-read64 (+ s0 #x08)) (%gc-read64 (+ s1 #x08)))
                     (+ (+ (%gc-read64 (+ s0 #x10)) (%gc-read64 (+ s1 #x10)))
                        (+ (%gc-read64 (+ s0 #x18)) (%gc-read64 (+ s1 #x18))))))
             (depth (+ (%gc-read64 (+ s0 #x30)) (%gc-read64 (+ s1 #x30))))
             (iters (+ (%gc-read64 s0) (%gc-read64 s1))))
        (format t "~%  thread 1 iterations ~D, thread 2 iterations ~D (of ~D each)~%"
                (%gc-read64 s0) (%gc-read64 s1) *n*)
        (format t "  multiple-value failures  t1 ~D  t2 ~D~%"
                (%gc-read64 (+ s0 #x08)) (%gc-read64 (+ s1 #x08)))
        (format t "  handler-case failures    t1 ~D  t2 ~D~%"
                (%gc-read64 (+ s0 #x10)) (%gc-read64 (+ s1 #x10)))
        (format t "  nested-handler failures  t1 ~D  t2 ~D~%"
                (%gc-read64 (+ s0 #x18)) (%gc-read64 (+ s1 #x18)))
        (format t "  net handler-stack depth change    t1 ~D  t2 ~D~%"
                (%gc-read64 (+ s0 #x30)) (%gc-read64 (+ s1 #x30)))
        (format t "~%=== VERDICT ==============================================~%")
        (if (or (> bad 0) (> depth 0) (< iters (* 2 *n*)))
            (format t "THE CONTROL FAILED, AS IT MUST: PASS~%")
            (format t "THE CONTROL CAME BACK CLEAN: FAIL — the shared window~%~
                      was not the thing being fixed, or this workload does not~%~
                      reach it.~%")))))
