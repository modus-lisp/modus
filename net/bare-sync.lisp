;;;; bare-sync.lisp - the synchronisation primitives net/sb-thread-shim.lisp
;;;; calls, for a bare-metal image: ONE thread, so ONE honest answer each.
;;;;
;;;; The SB-THREAD shim is boot-evaluated in the bare CL images too (it is how
;;;; portable `#+sb-thread' code -- glass/fb's framebuffer locks -- takes the
;;;; arm it is tested on), but its primitives live in net/hosted-sync.lisp,
;;;; which only the hosted x64 image carries.  Without these, the first
;;;; MAKE-MUTEX on the Zero died with UNDEFINED-FUNCTION %SYNC-CELL.
;;;;
;;;;   - A mutex is a cell at a raw address that never moves (the collector
;;;;     copies).  The arena is the board's: %BARE-SYNC-ARENA returns
;;;;     (values start end), 0 0 here, so a board without one gets the shim's
;;;;     own "arena exhausted" error rather than a bad address.  The arena's
;;;;     first word counts cells and its second is the clock; the board's
;;;;     kernel prologue zeroes both (Device RAM survives a reset).
;;;;   - With one thread a lock is a flag: lock and unlock are stores.
;;;;   - A wait with a timeout waits; a wait with NONE fails loudly -- nothing
;;;;     could ever signal it.  Signal and wake have nobody to wake.
;;;;   - Making a thread fails loudly.
;;;;   - Time is approximate: a busy-wait millisecond and a call-counting
;;;;     monotonic clock, enough for the shim's timeout loops to terminate.

(defun %bare-sync-arena () (values 0 0))

(defun %sync-cell ()
  "A zeroed 64-byte cell at a fixed address, or 0 when the arena is full."
  (multiple-value-bind (start end) (%bare-sync-arena)
    (if (zerop start)
        0
        (let* ((n (mem-ref start :u32))
               (a (+ start 64 (* n 64))))
          (if (> (+ a 64) end)
              0
              (progn
                (dotimes (i 8) (%gc-write64 (+ a (* i 8)) 0))
                (setf (mem-ref start :u32) (+ n 1))
                a))))))

(defun %sync-cells-handed-out ()
  (multiple-value-bind (start end) (%bare-sync-arena)
    (declare (ignore end))
    (if (zerop start) 0 (mem-ref start :u32))))
(defun %sync-cells-exhausted () 0)

(defun %thr-cpu () 0)
(defun %tls-self-base () 0)

(defun %mutex-trylock (cell)
  (if (zerop (%gc-read64 cell)) (progn (%gc-write64 cell 1) 1) 0))
(defun %mutex-lock (cell) (%gc-write64 cell 1) 1)
(defun %mutex-unlock (cell) (%gc-write64 cell 0) 0)

;; The clock is the arena's second word (a Lisp global's DEFVAR value is not
;; set at boot on every bare image -- Active Limitation 7).  No arena, no
;; mutex, so nothing waits on it.
(defun %bare-clock-add (ns)
  (multiple-value-bind (start end) (%bare-sync-arena)
    (declare (ignore end))
    (if (zerop start)
        0
        (let ((v (+ (%gc-read64 (+ start 8)) ns)))
          (%gc-write64 (+ start 8) v)
          v))))

(defun %monotonic-ns ()
  ;; Each call is a microsecond, so a deadline loop that calls this and
  ;; %SLEEP-MS 1 per iteration expires.
  (%bare-clock-add 1000))

(defun %sleep-ms (ms)
  (let ((i 0) (n (* ms 20000)))
    (loop (when (>= i n) (return nil)) (setq i (+ i 1))))
  (%bare-clock-add (* ms 1000000))
  nil)

(defun %cond-wait-ms (cv mtx ms)
  (declare (ignore cv mtx))
  (if (> ms 0)
      (progn (%sleep-ms ms) 1)
      (error "condition-wait with no timeout on a single-threaded bare-metal image: ~
              nothing could ever wake it")))
(defun %cond-bump (cv) (declare (ignore cv)) 0)
(defun %futex-wake (addr n) (declare (ignore addr n)) 0)

(defun %make-native-thread (fn &rest args)
  (declare (ignore fn args))
  (error "make-thread: a bare-metal image has no threads"))
(defun %native-thread-alive-p (h) (declare (ignore h)) nil)
(defun %join-native-thread (h) (declare (ignore h)) nil)
