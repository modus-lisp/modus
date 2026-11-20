;;;; u32-loop-parks.lisp — A LOOP OVER AN (UNSIGNED-BYTE 32) ARRAY PARKS FOR A
;;;; STOP-THE-WORLD COLLECTION, AND A COLLECTION THAT MOVES THE ARRAY MID-LOOP
;;;; LEAVES THE LOOP WRITING THE NEW COPY.
;;;;
;;;;   timeout 300 ./modus --script test/u32-loop-parks.lisp
;;;;
;;;; A JIT'd loop parks at its back-edges only when its function holds no raw
;;;; word (compiler.lisp *PARK-SAFEPOINTS*).  Declared word-vector access
;;;; (COMPILE-WORD-AREF / -ASET) dropped a :RAW-MARK although it holds none, and
;;;; on AArch64 Linux -- no packed u32 vectors -- a (simple-array (unsigned-byte
;;;; 32) (*)) is exactly such a word vector.  So glass's framebuffer loops never
;;;; parked: measured on the Pi 5 (origin/main 0d8ff428), main's first
;;;; collection waited 4.5 s for a worker running 1500 passes of a 640x480
;;;; pixel swizzle in one function.  x86-64 packs u32 (%C32-REF, never marked)
;;;; and parked already.
;;;;
;;;; Checked, for arrays MAIN allocated (region 0, moved by every one of main's
;;;; collections), arrays the WORKER allocated, and one of each:
;;;;   VALUES   the worker's passes both read and write both arrays, and each
;;;;            pass depends on the last, so a store that went to an array's
;;;;            OLD copy, or a load from it, changes the final checksums; they
;;;;            must equal an unthreaded run from the same start.
;;;;   PARKS    main's collections while the worker is mid-loop each take
;;;;            under a quarter of the worker's loop (~3 s, at least 1 s);
;;;;            unparked, the first one waits for the whole loop.
;;;;   MOVED    (positive control) main's array really moved -- its address
;;;;            changed across a collection -- while the worker was in its loop.
;;;; and the pass rate is printed with and without collections, for the
;;;; speed comparison.

(defvar *fail* 0)
(defvar *checks* 0)
(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "  ok   ~A = ~S~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A: got ~S want ~S~%" name got want))))
(defun chk-true (name v)
  (setq *checks* (+ *checks* 1))
  (if v (format t "  ok   ~A~%" name)
      (progn (setq *fail* (+ *fail* 1)) (format t "  FAIL ~A~%" name))))

(defvar *n* (* 640 480))
(defun now-ms () (floor (get-internal-real-time) 1000))
(defun addr-of (x) (%val->word x))

(defun fresh (seed)
  (let ((a (make-array *n* :element-type '(unsigned-byte 32))))
    (dotimes (i *n*) (setf (aref a i) (logand (+ (* i 2654435761) seed) #xffffffff)))
    a))

(defun passes (src dst n)
  "N dependent passes: every element of SRC steps an LCG, every element of DST
   folds in the pixel swizzle of the old SRC value.  All in ONE function: the
   only back-edges between the first pass and the last are its own."
  (declare (type (simple-array (unsigned-byte 32) (*)) src dst))
  (dotimes (k n)
    (dotimes (i (length src))
      (let ((p (aref src i)))
        (setf (aref src i) (logand (+ (* p 69069) 12345 k) #xffffffff))
        (setf (aref dst i)
              (logand (logxor (* (aref dst i) 31)
                              (logior (logand p #xff00ff00) (ash (logand p #xff) 16)
                                      (logand (ash p -16) #xff)))
                      #xffffffff)))))
  nil)

(defun u32-sum (a)
  (declare (type (simple-array (unsigned-byte 32) (*)) a))
  (let ((s 0)) (dotimes (i (length a)) (setq s (logand (+ (* s 3) (aref a i)) #xffffffffffff))) s))

;;; ---- the pass count: a loop of ~3 s --------------------------------------
(defun ms-per-pass ()
  (let ((a (fresh 1)) (b (fresh 2)))
    ;; warm: the first calls may run before the JIT has compiled PASSES
    (dotimes (i 3) (passes a b 1))
    (let ((t0 (now-ms)))
      (passes a b 20)
      (/ (max 1 (- (now-ms) t0)) 20.0))))
(defvar *ms-per-pass* (ms-per-pass))
(defvar *npasses* (max 50 (ceiling 3000 *ms-per-pass*)))
(format t "~%unthreaded: ~,2F ms per 640x480 pass; ~D passes per arm~%" *ms-per-pass* *npasses*)

(defun reference ()
  (let ((a (fresh 1)) (b (fresh 2)))
    (passes a b *npasses*)
    (list (u32-sum a) (u32-sum b))))
(defvar *want* (reference))
(format t "  reference checksums ~S~%" *want*)

;;; ---- one arm --------------------------------------------------------------
(defvar *done* nil)
(defvar *m-src* nil)
(defvar *m-dst* nil)

(defun collect-while-running ()
  "Collect region 0 until the worker says it is done.  Returns (times-ms
   moves), MOVES counting collections across which main's array (*M-SRC*, else
   *M-DST*; neither in the arm where the worker owns both, which does not
   check MOVES) changed address."
  (let ((times nil) (moves 0))
    (loop
      (when *done* (return nil))
      (let ((a0 (addr-of (or *m-src* *m-dst*))) (t0 (now-ms)))
        (modus-int::%gc-collect-region-0)
        ;; Every collection STARTED while the worker looped counts -- one that
        ;; waited for the loop to end, above all.
        (setq times (cons (- (now-ms) t0) times))
        (unless (= a0 (addr-of (or *m-src* *m-dst*))) (setq moves (+ moves 1))))
      (modus-int::%sleep-ms 100))
    (list (nreverse times) moves)))

(defun run-arm (name main-src main-dst)
  (format t "~%=== ~A ===~%" name)
  (setq *done* nil)
  (setq *m-src* (if main-src (fresh 1) nil))
  (setq *m-dst* (if main-dst (fresh 2) nil))
  (let* ((t0 (now-ms))
         (th (sb-thread:make-thread
              (lambda ()
                (let ((src (or *m-src* (fresh 1)))
                      (dst (or *m-dst* (fresh 2))))
                  (let ((s0 (now-ms)))
                    (passes src dst *npasses*)
                    (let ((ms (- (now-ms) s0)))
                      (setq *done* t)
                      (list (u32-sum src) (u32-sum dst) ms))))))))
    (let* ((c (collect-while-running))
           (r (sb-thread:join-thread th))
           (times (car c)) (moves (cadr c)) (ms (caddr r)))
      (format t "  worker loop ~D ms (~,2F ms/pass); ~D collections during it, median ~D ms, max ~D ms~%"
              ms (/ ms (* 1.0 *npasses*)) (length times)
              (if times (nth (floor (length times) 2) (sort (copy-list times) #'<)) 0)
              (if times (reduce #'max times) 0))
      (chk "checksums equal the unthreaded run" (list (car r) (cadr r)) *want*)
      (chk-true "the worker's loop lasted at least 1 s" (>= ms 1000))
      (chk-true "POSITIVE CONTROL: >= 5 collections while the worker looped"
                (>= (length times) 5))
      (chk-true "every collection during the loop took < 1/4 of it (parked, not waited for)"
                (every (lambda (x) (< (* 4 x) ms)) times))
      (when (or main-src main-dst)
        (chk-true (format nil "POSITIVE CONTROL: main's array moved mid-loop (~D times)" moves)
                  (>= moves 3)))
      (when main-src
        (chk "main's source, read back by main" (u32-sum *m-src*) (car *want*)))
      (when main-dst
        (chk "main's destination, read back by main" (u32-sum *m-dst*) (cadr *want*))))
    (- (now-ms) t0)))

(run-arm "MAIN'S ARRAYS (moved by every collection)" t t)
(run-arm "THE WORKER'S OWN ARRAYS" nil nil)
(run-arm "MAIN'S SOURCE, THE WORKER'S DESTINATION" t nil)

(format t "~%~D checks, ~D failed~%" *checks* *fail*)
(format t "~A~%" (if (zerop *fail*) "U32 LOOP PARKS: PASS" "U32 LOOP PARKS: FAIL"))
