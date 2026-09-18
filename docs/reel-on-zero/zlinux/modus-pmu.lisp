;;; modus-pmu.lisp — A53 PMU counters from hosted Modus (Linux) via perf_event_open, no C.
;;; Syscall numbers are x86-64 style (the aarch64 translator remaps): 298 perf_event_open, 16 ioctl, 0 read, 3 close.
(setq *jit-hot-only* nil)
(defvar *pmu-type* 4)
(defvar *pmu-smoke-events* (list #x11 #x08))
(defvar *pmu-attr* (+ *cstr-scratch* 2048))          ; perf_event_attr (128 bytes) in the C-string scratch page
(defun %pmu-attr-clear () (dotimes (i 128) (setf (mem-ref (+ *pmu-attr* i) :u8) 0)))
(defun %pmu-put32 (off v) (dotimes (i 4) (setf (mem-ref (+ *pmu-attr* off i) :u8) (logand (ash v (* -8 i)) 255))))
(defun pmu-open (event &optional (group -1))
  "perf_event_open(PERF_TYPE_RAW event, pid 0, cpu -1, GROUP, flags 0): disabled, user-only, read_format=GROUP.
   GROUP = -1 opens a leader; pass the leader's fd to add members. Returns fd or -errno."
  (%pmu-attr-clear)
  (%pmu-put32 0 *pmu-type*)                          ; type: 4 = RAW (A53 codes), 0 = HARDWARE (portable)
  (%pmu-put32 4 128)                                 ; size
  (%pmu-put32 8 event)                               ; config (low word)
  (%pmu-put32 32 8)                                  ; read_format = PERF_FORMAT_GROUP: one read() returns nr + all values
  (%pmu-put32 40 (if (< group 0) 97 96))             ; flags: disabled(1, leader only) | exclude_kernel(32) | exclude_hv(64)
  (syscall6 298 *pmu-attr* 0 -1 group 0 0))
(defun pmu-ctl (fd op) (syscall3 16 fd op 0))        ; ops: reset #x2403 enable #x2400 disable #x2401
(defun pmu-read (fd)
  (syscall3 0 fd *io-buf-addr* 8)
  ;; two shifts <=30 stay inline (:shl); a constant count >30 routes to runtime bignum-ash, which ALLOCATES per call
  (logior (mem-ref *io-buf-addr* :u32) (ash (ash (mem-ref (+ *io-buf-addr* 4) :u32) 2) 30)))
(defun pmu-close (fd) (syscall3 3 fd 0 0))
(defun %u64-at (a) (logior (mem-ref a :u32) (ash (ash (mem-ref (+ a 4) :u32) 2) 30)))
(defvar *pmu-fds* nil)          ; (leader member ...) — the leader's ioctls act on the whole group
(defvar *pmu-n* 0)
(defun pmu-start (events)
  "Open EVENTS as one perf group (first = leader), reset + enable it. Returns the fds."
  (let ((leader (pmu-open (car events))))
    (setq *pmu-fds* (cons leader (mapcar (lambda (e) (pmu-open e leader)) (cdr events))))
    (setq *pmu-n* (length events))
    (pmu-ctl leader #x2403) (pmu-ctl leader #x2400)
    *pmu-fds*))
(defun pmu-snap (into)
  "ONE read(2) on the leader: {u64 nr; u64 value[nr]} into INTO (a vector of >= *pmu-n* fixnums). No allocation."
  (syscall3 0 (car *pmu-fds*) *io-buf-addr* (* 8 (+ 1 *pmu-n*)))
  (dotimes (i *pmu-n*) (setf (aref into i) (%u64-at (+ *io-buf-addr* 8 (* 8 i))))))
(defun pmu-stop ()
  (pmu-ctl (car *pmu-fds*) #x2401)
  (let ((v (make-array *pmu-n* :initial-element 0)))
    (pmu-snap v)
    (dolist (fd *pmu-fds*) (pmu-close fd)) (setq *pmu-fds* nil)
    (let ((l nil)) (dotimes (i *pmu-n*) (setq l (cons (aref v i) l))) (nreverse l))))
;; smoke: cycles + instructions around a fixnum loop
(defun spin (n) (let ((s 0)) (declare (type fixnum s)) (dotimes (i n) (setq s (+ s i))) s))
(format t "fds=~a~%" (pmu-start *pmu-smoke-events*))
(spin 1000000)
(let ((v (pmu-stop))) (format t "PMU-SMOKE cycles=~a inst=~a ipc=~,2f~%" (first v) (second v) (/ (second v) (max 1 (first v)))))
