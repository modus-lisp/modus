;;;; hosted-thread-lisp-unsync.lisp — THE NEGATIVE CONTROL.
;;;;
;;;;   ./modus --script test/hosted-thread-lisp-unsync.lisp
;;;;
;;;; THIS SCRIPT IS SUPPOSED TO FAIL.  It runs test/hosted-thread-lisp.lisp's
;;;; workload — two threads interning fresh symbols, calling FORMAT, consing,
;;;; defining and calling functions, with forced collections on both sides —
;;;; with the threads-live gate left OFF, which makes %RT-ENTER and %RT-LEAVE
;;;; the no-ops they are in a single-threaded image.  Same binary, same
;;;; workload, one word apart: it is this work REMOVED.
;;;;
;;;; WHY THE CONTROL HAS ITS OWN SCRIPT.  A test that passes with and without
;;;; the fix proves nothing, so the control has to run — but a corrupted intern
;;;; table is not obliged to fail politely.  Without the lock, two threads that
;;;; both miss the same GETHASH both allocate a symbol and both store it; the
;;;; symbols are allocated in each thread's OWN region, so a table in region 0
;;;; ends up holding pointers into a heap that thread's own collector is about
;;;; to evacuate; and PUTHASH's rehash can be walked mid-flight by the other
;;;; thread.  Any of those can take the process down.  A process that dies IS
;;;; the demonstration, and it must not take the acceptance test's report with
;;;; it.
;;;;
;;;; SO: A NON-ZERO EXIT, A CRASH, OR THE `CONTROL FAILED AS REQUIRED' LINE
;;;; BELOW ALL MEAN THE SAME THING.  What would falsify the whole exercise is
;;;; this script printing `CONTROL PASSED', because that would mean the
;;;; synchronised arm was passing for some reason other than the
;;;; synchronisation.

(defvar *n* 300)
(defvar *gcevery* 25)

(defun w (res off) (%gc-read64 (+ res off)))

(format t "~%=== THE SAME WORKLOAD WITH THE LOCK GATE OFF =============~%")
(format t "  If this reaches the end at all, these are the numbers.~%")

(let ((res (%tl-selftest 1 *n* *gcevery*)))
  (if (= res 0)
      (format t "~%SKIP: no actor band, no thread page, or the arrays would not map.~%")
      (let ((bad  (w res #x30))
            (drv  (w res #x38))
            (dup  (w res #x40))
            (zero (w res #x48))
            (own  (w res #xB0))
            (fmt  (w res #xB8))
            (chn  (w res #xC0))
            (glb  (w res #xC8))
            (fnc  (w res #xD0))
            (idc  (w res #xD8)))
        (format t "  iterations                       ~D / ~D~%"
                (w res #xA0) (w res #xA8))
        (format t "  barrier timeouts                 ~D / ~D~%"
                (w res #x80) (w res #x78))
        (format t "  forced collections               ~D / ~D~%"
                (w res #xE0) (w res #xE8))
        (format t "  region collection counts         ~D / ~D~%"
                (w res #x10) (w res #x18))
        (format t "  region 0 collections             ~D -> ~D~%"
                (w res #x20) (w res #x28))
        (format t "  runtime-lock acquisitions        ~D (0 = the gate really was off)~%"
                (w res #x88))
        (format t "~%  THE DAMAGE:~%")
        (format t "    shared names, threads disagreed    ~D~%" bad)
        (format t "    shared names, driver disagreed     ~D~%" drv)
        (format t "    recorded words that were zero      ~D~%" zero)
        (format t "    consecutive names, SAME object     ~D~%" dup)
        (format t "    own-name re-intern failures        ~D~%" own)
        (format t "    FORMAT failures                    ~D~%" fmt)
        (format t "    chain-survival failures            ~D~%" chn)
        (format t "    global read-back failures          ~D~%" glb)
        (format t "    function-table failures            ~D~%" fnc)
        (format t "    identity changed across a GC       ~D~%" idc)
        (format t "~%=== VERDICT ==============================================~%")
        (if (> (+ bad (+ drv (+ dup (+ zero (+ own (+ fmt (+ chn (+ glb (+ fnc idc)))))))))
               0)
            (format t "CONTROL FAILED AS REQUIRED: the unsynchronised arm is corrupt.~%")
            (format t "CONTROL PASSED — WHICH IS ITSELF A FAILURE OF THE EXERCISE.~%")))))
