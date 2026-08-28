;;;; hosted-sleep.lisp — SLEEP IS REAL, MEASURED AGAINST THE WALL CLOCK.
;;;;
;;;;   ./modus --script test/hosted-sleep.lisp
;;;;
;;;; mvm/ansi-bridge.lisp shipped `(defun sleep (n) nil)'.  Nothing in the tree
;;;; could pace anything.  net/hosted-sync.lisp replaces it with a restarting
;;;; nanosleep(2).
;;;;
;;;; WHAT WOULD MAKE THIS TEST A LIE, and what stops it.
;;;;
;;;;   "SLEEP returned" proves nothing — the no-op returned too, instantly.  So
;;;;   every claim here is a DIFFERENCE OF TWO CLOCK_MONOTONIC READINGS taken
;;;;   around the call, and the assertions are two-sided: a sleep must take AT
;;;;   LEAST its duration (the no-op fails this) and NOT MUCH MORE (a hang or a
;;;;   thousand-fold unit error fails this).
;;;;
;;;;   "The clock advanced" proves nothing on its own either, because a busy
;;;;   loop advances it as well.  So the same interval is measured a SECOND
;;;;   time in CPU-time (CLOCK_PROCESS_CPUTIME_ID), which only advances while a
;;;;   thread is on a core.  A sleeping process burns essentially none of it;
;;;;   the spin loop that is measured beside it, for the same wall duration,
;;;;   burns nearly all of it.  That pair is the difference between blocking
;;;;   and spinning, and it is the same instrument the blocking-receive test
;;;;   uses.

(defvar *fail* 0)
(defvar *checks* 0)

(defun chk-true (name got)
  (setq *checks* (+ *checks* 1))
  (if got
      (format t "  ok   ~A~%" name)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A~%" name))))

(defun chk-range (name got lo hi)
  (setq *checks* (+ *checks* 1))
  (if (and (>= got lo) (<= got hi))
      (format t "  ok   ~A = ~D  (want ~D .. ~D)~%" name got lo hi)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~D  (want ~D .. ~D)~%" name got lo hi))))

(defun ms-of (ns) (floor ns 1000000))

(defun timed-sleep-ms (ms)
  "Wall milliseconds actually consumed by (%SLEEP-MS MS)."
  (let ((t0 (%monotonic-ns)))
    (%sleep-ms ms)
    (ms-of (- (%monotonic-ns) t0))))

(format t "~%=== THE CLOCK ITSELF =====================================~%")
(let ((a (%monotonic-ns))
      (b (%monotonic-ns)))
  (chk-true "CLOCK_MONOTONIC reads non-zero" (> a 0))
  (chk-true "and is monotonic across two reads" (>= b a)))

(format t "~%=== %SLEEP-MS TAKES THE TIME IT SAYS =====================~%")
(format t "  Each row is (monotonic after) - (monotonic before), in ms.~%")
(format t "  The OLD `(defun sleep (n) nil)' scores 0 on every one.~%")
(let ((a (timed-sleep-ms 50))
      (b (timed-sleep-ms 200))
      (c (timed-sleep-ms 500)))
  (chk-range "sleep 50ms  measured" a 45 400)
  (chk-range "sleep 200ms measured" b 195 700)
  (chk-range "sleep 500ms measured" c 495 1200))

(format t "~%=== SLEEP, THE CL FUNCTION, ON EACH NUMERIC TOWER ========~%")
(let ((a (let ((t0 (%monotonic-ns))) (sleep 1) (ms-of (- (%monotonic-ns) t0)))))
  (chk-range "(sleep 1)     — integer seconds" a 995 1600))
(let ((b (let ((t0 (%monotonic-ns))) (sleep 0.25) (ms-of (- (%monotonic-ns) t0)))))
  (chk-range "(sleep 0.25)  — float seconds" b 240 800))
(let ((c (let ((t0 (%monotonic-ns))) (sleep 1/10) (ms-of (- (%monotonic-ns) t0)))))
  (chk-range "(sleep 1/10)  — ratio seconds" c 95 600))
(let ((d (let ((t0 (%monotonic-ns))) (sleep 0) (ms-of (- (%monotonic-ns) t0)))))
  (chk-range "(sleep 0)     — returns at once" d 0 50))
(let ((e (let ((t0 (%monotonic-ns))) (sleep -5) (ms-of (- (%monotonic-ns) t0)))))
  (chk-range "(sleep -5)    — negative is not a hang" e 0 50))

(format t "~%=== SLEEPING IS NOT SPINNING =============================~%")
(format t "  The same wall interval, measured twice: once on the wall~%")
(format t "  clock and once on CLOCK_PROCESS_CPUTIME_ID, which only~%")
(format t "  advances while a thread is ON A CORE.~%")
(let* ((w0 (%monotonic-ns))
       (c0 (%cpu-ns)))
  (%sleep-ms 400)
  (let ((wall (ms-of (- (%monotonic-ns) w0)))
        (cpu  (ms-of (- (%cpu-ns) c0))))
    (format t "  sleeping 400ms:  wall ~Dms   cpu ~Dms~%" wall cpu)
    (chk-range "wall time passed" wall 395 1200)
    (chk-range "and CPU time did NOT" cpu 0 60)))

(let* ((w0 (%monotonic-ns))
       (c0 (%cpu-ns))
       (n 0))
  ;; A spin of comparable duration, for the contrast.  The bound is wall-clock
  ;; driven so the comparison holds on a slow box as well as a fast one.
  (loop
    (setq n (+ n 1))
    (when (>= (- (%monotonic-ns) w0) 400000000) (return 0)))
  (let ((wall (ms-of (- (%monotonic-ns) w0)))
        (cpu  (ms-of (- (%cpu-ns) c0))))
    (format t "  spinning 400ms:  wall ~Dms   cpu ~Dms  (~D iterations)~%"
            wall cpu n)
    (chk-true "a spin of the same wall length burns the CPU time a sleep did not"
              (> cpu 300))))

(format t "~%=== VERDICT ==============================================~%")
(if (= *fail* 0)
    (format t "HOSTED x64 SLEEP: PASS (~D checks)~%" *checks*)
    (format t "HOSTED x64 SLEEP: FAIL (~D of ~D checks)~%" *fail* *checks*))
