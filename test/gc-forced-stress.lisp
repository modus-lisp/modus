;;;; gc-forced-stress.lisp — 20 FORCED collections of region 0, 20,000 live.
;;;;
;;;;   ./modus --script test/gc-forced-stress.lisp
;;;;
;;;; The ordinary 200,000-allocation stress collects ZERO times on an 896 MB
;;;; semispace (measured, with and without MODUS_GC_R14), so it exercises the
;;;; allocator and not the collector.  This forces the collections and ASSERTS
;;;; THE COUNT ROSE, then checks that 20,000 structures — cons + list + string —
;;;; survived every one of them intact.
;;;;
;;;; The work is done by %gc-forced-stress in mvm/gc.lisp rather than here: a
;;;; --script that calls %gc-collect-here at toplevel breaks the interpreted
;;;; toplevel's next FORMAT (pre-existing, reproduced on an unmodified HEAD
;;;; build), so a scripted version would measure that instead of the collector.

(defvar *fail* 0)
(defun chk (name got want)
  (if (= got want)
      (format t "  ok   ~A = ~D~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~D (expected ~D)~%" name got want))))

(let ((r (%gc-forced-stress 20000 20)))
  (let ((g0 (car r)) (g1 (car (cdr r)))
        (n (car (cdr (cdr r))))
        (sum (car (cdr (cdr (cdr r)))))
        (bad (car (cdr (cdr (cdr (cdr r))))))
        (calls (car (cdr (cdr (cdr (cdr (cdr r))))))))
    (format t "~%=== FORCED-COLLECTION STRESS =============================~%")
    (format t "  collections ~D -> ~D  (~D calls to %gc-collect-here)~%"
            g0 g1 calls)
    (chk "collections performed" (- g1 g0) 20)
    (chk "structures retained" n 20000)
    (chk "checksum" sum 199990000)
    (chk "corrupted" bad 0)
    (format t "~%~A~%"
            (if (= *fail* 0) "FORCED-GC STRESS: PASS (4 checks)"
                "FORCED-GC STRESS: FAIL"))))
