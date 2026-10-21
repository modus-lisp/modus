;;;; hosted-pmu.lisp — hardware performance counters and a sampling profiler on
;;;; hosted x86-64, from Lisp: raw perf_event_open(2), no C, no FFI.
;;;;
;;;; COUNTING (the API of lib/a64-pmu.lisp, the bare-metal A53 counters):
;;;;   (pmu-measure (lambda () (work)))                    ; :summary preset
;;;;   (pmu-measure (lambda () (work)) :frontend)
;;;;   (pmu-measure (lambda () (work)) :instructions :l1d-read-miss #x0c2)
;;;;   => (values RESULT COUNTS), COUNTS printed: cycles, IPC, each event with
;;;;      its rate per 1000 instructions
;;;;   (pmu-start ...) ... (pmu-read) (pmu-stop)    ; by hand
;;;;   (pmu-events)                                 ; the names
;;;; An integer event is a RAW one (perf type 4): on AMD Zen the PMCx code with
;;;; its unit mask in bits 8-15, e.g. #x0c2 retired branches.
;;;;
;;;; SAMPLING:
;;;;   (pmu-profile (lambda () (work)))             ; every 100000 cycles
;;;;   (pmu-profile thunk :period 20000 :event :instructions :top 40)
;;;;   => (values RESULT PROFILE), the table printed: per function the SELF
;;;;      samples (the instruction pointer was in it) and the INCLUSIVE ones (it
;;;;      was anywhere on the stack -- the kernel walks the RBP chain every
;;;;      compiled function keeps).  Names come from net/hosted-backtrace.lisp's
;;;;      tables: image functions by the ELF symbols, JIT code by
;;;;      *SYMBOL-FUNCTION-TABLE*.
;;;;   (pmu-annotate PROFILE "NAME" "/path/prefix")
;;;;      writes PREFIX.bin (the function's machine code) and PREFIX.hits
;;;;      ("offset count" per sampled instruction); scripts/pmu-annotate.py
;;;;      runs objdump over the code and prints each instruction's samples.
;;;;
;;;; WHERE IT WORKS.  perf_event_open needs kernel.perf_event_paranoid <= 2 for a
;;;; process's own user-space events, or CAP_PERFMON / CAP_SYS_ADMIN.  Debian's
;;;; default is 4 (nothing at all); this box's is 4, so measure inside a
;;;; container with --cap-add SYS_ADMIN (and --cap-add IPC_LOCK for a sample
;;;; ring above kernel.perf_event_mlock_kb, 516 KB by default).  A refusal is
;;;; an ERROR naming the errno, never silent zeros.
;;;;
;;;; WHAT IS COUNTED: the calling thread, user space only (exclude_kernel and
;;;; exclude_hv), so the numbers are the program's, not the kernel's.  The
;;;; events are one perf GROUP, all on the PMU together; with Zen 3's six
;;;; counters less the NMI watchdog's that is cycles plus four, and a group too
;;;; big to schedule is reported "not counted" rather than multiplexed into
;;;; estimates.
;;;;
;;;; BAKED SOURCE (build-cli-common, after hosted-backtrace.lisp): a DEFVAR's
;;;; initform never runs in the image, so buffers are made on first use.  Not
;;;; reentrant: one measurement at a time per process.

;;; --- raw memory ---------------------------------------------------------------
;;; Every 64-bit field goes through two :U32 halves: a :U64 load hands back the
;;; machine word as a TAGGED value (see gc.lisp), and these are raw numbers.

(defun %pmu-put32 (a v) (setf (mem-ref a :u32) (logand v #xffffffff)))
(defun %pmu-put64 (a v)
  (setf (mem-ref a :u32) (logand v #xffffffff))
  (setf (mem-ref (+ a 4) :u32) (logand (ash v -32) #xffffffff)))
(defun %pmu-get64 (a)
  (logior (mem-ref a :u32) (ash (mem-ref (+ a 4) :u32) 32)))

(defun %pmu-mmap (bytes)
  "BYTES of fresh anonymous read/write memory, or an error."
  (let ((p (syscall6 9 0 bytes 3 #x22 -1 0)))   ; PROT_READ|WRITE, MAP_PRIVATE|ANONYMOUS
    (if (< p 0) (error "pmu: mmap(~d) failed: errno ~d" bytes (- p)) p)))

(defvar *pmu-scratch*)     ; one page: the perf_event_attr at +0, read buffers at +256
(defun %pmu-scratch ()
  (unless (and (boundp '*pmu-scratch*) (integerp *pmu-scratch*))
    (setq *pmu-scratch* (%pmu-mmap 8192)))
  *pmu-scratch*)

;;; --- events -------------------------------------------------------------------

(defun pmu-events ()
  "The named events: (name type . config).  Type 0 = generic hardware, 3 = the
   generic cache events (cache | op<<8 | result<<16), 1 = software.  Any
   integer is also accepted, as a RAW event (type 4)."
  '((:cycles 0 . 0) (:instructions 0 . 1) (:cache-references 0 . 2)
    (:cache-misses 0 . 3) (:branches 0 . 4) (:branch-misses 0 . 5)
    (:stalled-cycles-frontend 0 . 7) (:stalled-cycles-backend 0 . 8)
    (:ref-cycles 0 . 9)
    (:l1d-read-access 3 . #x00000) (:l1d-read-miss 3 . #x10000)
    (:l1d-write-access 3 . #x00100) (:l1i-read-miss 3 . #x10001)
    (:llc-read-access 3 . #x00002) (:llc-read-miss 3 . #x10002)
    (:dtlb-read-miss 3 . #x10003) (:itlb-read-miss 3 . #x10004)
    (:task-clock 1 . 1) (:page-faults 1 . 2) (:context-switches 1 . 3)
    ;; AMD Instruction-Based Sampling, sampling only (PMU-PROFILE): the IP is
    ;; the exact instruction, not wherever a cycle sample skidded to.  :IBS-OP
    ;; tags every PERIODth DISPATCHED op (cnt_ctl), so a stalled load is
    ;; sampled itself and its tag-to-retire latency is its stall; :IBS-OP-CYCLES
    ;; tags the op dispatched when PERIOD cycles have passed, which during a
    ;; stall is the op AFTER it.  The PMU type comes from sysfs.
    (:ibs-op :ibs . #x80000) (:ibs-op-cycles :ibs . 0)))

(defun %pmu-ibs-type ()
  "The perf type number of AMD's IBS op PMU (it varies by kernel), or an error."
  (let ((line (ignore-errors
               (with-open-file (i "/sys/bus/event_source/devices/ibs_op/type")
                 (read-line i nil nil)))))
    (or (and line (ignore-errors (parse-integer (string-trim " " line))))
        (error "pmu: no IBS op PMU here (/sys/bus/event_source/devices/ibs_op): IBS is AMD-only"))))

(defun pmu-preset (name)
  "Event sets of four, which fit beside the cycle counter: Zen 3 has six
   general counters and the kernel's NMI watchdog (kernel.nmi_watchdog = 1)
   holds one, so a group of six is never scheduled (measured: :NOT-COUNTED).
   Zen also has no generic backend-stall, ref-cycles, L1D-write or LLC event.
   :SUMMARY   where instructions go and what misses
   :BRANCHES  the branch side
   :FRONTEND  why the front end starves
   :MEMORY    the data side"
  (case name
    (:summary (list :instructions :branch-misses :l1d-read-miss :l1i-read-miss))
    (:branches (list :instructions :branches :branch-misses :stalled-cycles-frontend))
    (:frontend (list :instructions :stalled-cycles-frontend :l1i-read-miss :itlb-read-miss))
    (:memory (list :instructions :l1d-read-access :l1d-read-miss :dtlb-read-miss))
    (t nil)))

(defun %pmu-event (e)
  "(type . config) for event designator E."
  (cond ((integerp e) (cons 4 e))
        ((assoc e (pmu-events))
         (let ((te (cdr (assoc e (pmu-events)))))
           (if (eq (car te) :ibs) (cons (%pmu-ibs-type) (cdr te)) te)))
        (t (error "pmu: unknown event ~s (see (pmu-events))" e))))

;;; --- perf_event_open ----------------------------------------------------------

(defun %pmu-attr (type config &key (leader t) (period 0) (sample-type 0) (exclude t))
  "Fill the perf_event_attr at the scratch page; return its address."
  (let ((a (%pmu-scratch)))
    (dotimes (i 32) (setf (mem-ref (+ a (* 4 i)) :u32) 0))
    (%pmu-put32 a type)
    (%pmu-put32 (+ a 4) 128)                       ; size
    (%pmu-put64 (+ a 8) config)
    (%pmu-put64 (+ a 16) period)                   ; sample_period
    (%pmu-put64 (+ a 24) sample-type)
    ;; read_format: GROUP | TOTAL_TIME_ENABLED | TOTAL_TIME_RUNNING
    (%pmu-put64 (+ a 32) (logior 8 1 2))
    ;; flags: disabled (leader only) | exclude_kernel | exclude_hv -- not for
    ;; IBS, whose driver refuses the exclusions (EINVAL); its kernel samples
    ;; are named <kernel> when the ring is read
    (%pmu-put64 (+ a 40) (logior (if leader 1 0) (if exclude (logior 32 64) 0)))
    (%pmu-put32 (+ a 48) 1)                        ; wakeup_events
    a))

(defun %pmu-open (type config &rest keys)
  "perf_event_open on this thread, any CPU; the fd, or an error naming why."
  (let* ((group (or (getf keys :group) -1))
         (attr (%pmu-attr type config :leader (< group 0)
                          :period (or (getf keys :period) 0)
                          :sample-type (or (getf keys :sample-type) 0)
                          :exclude (if (member :exclude keys) (getf keys :exclude) t)))
         (fd (syscall6 298 attr 0 -1 group 8 0)))  ; flags: PERF_FLAG_FD_CLOEXEC
    (when (< fd 0)
      (error "pmu: perf_event_open(type ~d config #x~x) failed: errno ~d~a" type config (- fd)
             (case (- fd)
               ((1 13) " -- not permitted: kernel.perf_event_paranoid is above 2; run with CAP_PERFMON / --cap-add SYS_ADMIN")
               (2 " -- no such event on this CPU")
               (95 " -- not supported by this PMU")
               (t ""))))
    fd))

(defun %pmu-ioctl (fd op) (syscall3 16 fd op 1))   ; PERF_IOC_FLAG_GROUP
(defun %pmu-close (fd) (syscall3 3 fd 0 0))

;;; --- counting -----------------------------------------------------------------

(defvar *pmu-fds*)          ; (leader member ...): cycles first
(defvar *pmu-current*)      ; the event designators, in counter order after cycles

(defun pmu-start (&rest events)
  "Open :CYCLES and EVENTS (names, raw integers, or one preset keyword) as one
   group on this thread, reset and start it."
  (when (and events (null (cdr events)) (keywordp (car events)) (pmu-preset (car events)))
    (setq events (pmu-preset (car events))))
  (when (and (boundp '*pmu-fds*) *pmu-fds*) (pmu-stop))
  (let* ((leader (%pmu-open 0 0))
         (fds (list leader)))
    (handler-case
        (dolist (e events)
          (let ((te (%pmu-event e)))
            (setq fds (append fds (list (%pmu-open (car te) (cdr te) :group leader))))))
      (error (c) (dolist (fd fds) (%pmu-close fd)) (error c)))
    (setq *pmu-fds* fds *pmu-current* events)
    (%pmu-ioctl leader #x2403)                     ; RESET
    (%pmu-ioctl leader #x2400)                     ; ENABLE
    events))

(defun pmu-read ()
  "The counts since PMU-START, as a plist (:CYCLES n EVENT n ...).  When the
   group was not on the PMU the whole time, :RUNNING-FRACTION says how much of
   it was, and the counts are NOT scaled; when it never was, :NOT-COUNTED T."
  (let* ((buf (+ (%pmu-scratch) 256))
         (n (length *pmu-fds*))
         (got (syscall3 0 (car *pmu-fds*) buf (* 8 (+ 3 n)))))
    (when (< got 0) (error "pmu: read failed: errno ~d" (- got)))
    (let* ((enabled (%pmu-get64 (+ buf 8)))
           (running (%pmu-get64 (+ buf 16)))
           (out (list :cycles (%pmu-get64 (+ buf 24))))
           (i 1))
      (dolist (e *pmu-current*)
        (setq out (append out (list e (%pmu-get64 (+ buf 24 (* 8 i))))))
        (setq i (+ i 1)))
      (cond ((zerop running) (append out (list :not-counted t)))
            ((< running enabled) (append out (list :running-fraction (/ running (float enabled)))))
            (t out)))))

(defun pmu-stop ()
  (when (and (boundp '*pmu-fds*) *pmu-fds*)
    (%pmu-ioctl (car *pmu-fds*) #x2401)            ; DISABLE
    (dolist (fd *pmu-fds*) (%pmu-close fd))
    (setq *pmu-fds* nil))
  nil)

(defun pmu-report (counts &optional (stream t))
  "Print COUNTS (from PMU-READ): cycles, IPC, and each event per 1000
   instructions when :INSTRUCTIONS was counted."
  (let ((cyc (getf counts :cycles)) (inst (getf counts :instructions)))
    (format stream "~&  cycles ~18:d~@[  IPC ~,2f~]~%" cyc
            (and inst cyc (> cyc 0) (/ inst (float cyc))))
    (loop for (k v) on counts by #'cddr
          unless (member k '(:cycles :not-counted :running-fraction))
            do (format stream "  ~24a ~18:d~@[  ~,2f/k-inst~]~@[  ~,1f% of cycles~]~%"
                       (if (integerp k) (format nil "raw #x~x" k) (string-downcase (symbol-name k)))
                       v
                       (and inst (> inst 0) (not (eq k :instructions)) (/ (* 1000.0 v) inst))
                       (and cyc (> cyc 0)
                            (member k '(:stalled-cycles-frontend :stalled-cycles-backend))
                            (/ (* 100.0 v) cyc))))
    (when (getf counts :not-counted)
      (format stream "  !! the group was never on the PMU: too many events for the counters~%"))
    (when (getf counts :running-fraction)
      (format stream "  !! counted ~,1f% of the time (unscaled)~%" (* 100 (getf counts :running-fraction))))
    counts))

(defun pmu-measure (thunk &rest events)
  "Run THUNK with the counters on; print the table; (values RESULT COUNTS).
   EVENTS as for PMU-START; none means :SUMMARY."
  (apply #'pmu-start (or events (list :summary)))
  (let ((r nil) (counts nil))
    (unwind-protect
         (progn (setq r (funcall thunk))
                (setq counts (pmu-read)))
      (pmu-stop))
    (pmu-report counts)
    (values r counts)))

;;; --- sampling -----------------------------------------------------------------
;;; One sampling event (cycles by default) with PERF_SAMPLE_IP | CALLCHAIN into a
;;; ring of 1 + 2^k pages, read after THUNK returns.  The kernel never
;;; overwrites unread data: when the ring fills it drops samples and says so in
;;; PERF_RECORD_LOST, which is reported -- raise :PAGES (needs IPC_LOCK past the
;;; mlock limit) or :PERIOD if it fires.

(defun %pmu-ring (fd pages)
  (let ((p (syscall6 9 0 (* 4096 (+ 1 pages)) 3 1 fd 0)))   ; PROT_READ|WRITE, MAP_SHARED
    (if (< p 0)
        (error "pmu: mmap of the sample ring (~d pages) failed: errno ~d~a" pages (- p)
               (if (= (- p) 1) " -- past kernel.perf_event_mlock_kb: fewer :PAGES, or CAP_IPC_LOCK" ""))
        p)))

(defvar *pmu-jit-code* nil)   ; %BT-JIT-CODE-TABLE for the profile being drained

(defun %pmu-fn-name (addr code jit)
  "The name of the function containing ADDR, without an offset (aggregation key).
   JIT code by its exact recorded extent (lambdas and toplevel forms included),
   else -- code from before the map existed -- the nearest DEFUN below it."
  (if (>= addr #x3000000000)
      (or (%bt-jit-code-name addr *pmu-jit-code*)
          (let ((e (and jit (%bt-lookup addr jit))))
            (if (and e (< (- addr (car e)) 65536)) (cadr e) "<jit: unnamed>")))
      (let ((e (%bt-lookup addr code)))
        (cond ((null e) "<outside the image>")
              ((and (cddr e) (>= (- addr (car e)) (cddr e)))
               (format nil "<native past ~a>" (cadr e)))
              (t (cadr e))))))

(defun %pmu-bump (table key n)
  (setf (gethash key table) (+ n (gethash key table 0))))

(defun pmu-profile (thunk &key (period 100000) (event :cycles) (pages 4096) (top 30)
                               (stream t) (callchain t))
  "Run THUNK sampling EVENT every PERIOD occurrences; print the hottest
   functions; (values RESULT PROFILE).  PROFILE is a plist: :SAMPLES :LOST
   :SELF and :INCLUSIVE ((name . samples) ...) sorted, and :HITS, a table of
   sampled addresses -> count (for PMU-ANNOTATE).  With an IBS EVENT
   (:IBS-OP, :IBS-OP-CYCLES) also :DC-MISS ((name . data-cache misses) ...),
   from each sample's raw IBS registers, and :LATENCY (tag-to-retire cycles
   summed per function -- NOT a time profile: ops retire in order, so every
   op queued behind a missing load carries its wait; test/pmu-calibrate.lisp
   measures a 94% pointer chase at ~53% that way).  For WHERE TIME GOES use
   the default :CYCLES (calibrated within a few points); IBS says exactly
   WHICH instructions miss the data cache."
  (let* ((te (%pmu-event event))
         (ibs (and (member event '(:ibs-op :ibs-op-cycles)) t))
         (fd (%pmu-open (car te) (cdr te) :period period :exclude (not ibs)
                        ;; IP | CALLCHAIN, and RAW (the IBS registers) for IBS
                        :sample-type (logior 1 (if callchain 32 0) (if ibs 1024 0))))
         (ring nil) (r nil))
    (unwind-protect
         (progn
           (setq ring (%pmu-ring fd pages))
           (%pmu-ioctl fd #x2403)
           (%pmu-ioctl fd #x2400)
           (setq r (funcall thunk))
           (%pmu-ioctl fd #x2401)
           (let ((profile (%pmu-drain ring pages callchain ibs)))
             (%pmu-print-profile profile top stream)
             (setq r (list r profile))))
      (when ring (syscall3 11 ring (* 4096 (+ 1 pages)) 0))      ; munmap
      (%pmu-close fd))
    (values (first r) (second r))))

(defun %pmu-kernel-ip-p (rec-addr)
  "The 64-bit IP stored at REC-ADDR is a kernel address (top half), read by its
   high word so no bignum is made."
  (>= (mem-ref (+ rec-addr 4) :u32) #xffff8000))

(defun %pmu-drain (ring pages callchain &optional ibs)
  "Walk the ring's records into a profile plist."
  (let* ((head (%pmu-get64 (+ ring 1024)))          ; data_head
         (size (* 4096 pages))
         (data (+ ring 4096))
         (code (%bt-code-table))
         (jit (%bt-jit-table))
         (jcode (setq *pmu-jit-code* (%bt-jit-code-table)))
         (self (make-hash-table :test 'equal))
         (incl (make-hash-table :test 'equal))
         (hits (make-hash-table))
         (lat (make-hash-table :test 'equal))
         (miss (make-hash-table :test 'equal))
         (samples 0) (lost 0)
         (pos (max 0 (- head size))))                ; older data is overwritten only if dropped
    (loop while (< pos head) do
      (let* ((rec (+ data (mod pos size)))
             (type (mem-ref rec :u32))
             (rsize (logand (ash (mem-ref (+ rec 4) :u32) -16) #xffff)))
        (when (zerop rsize) (return))
        (cond
          ((= type 9)                                ; PERF_RECORD_SAMPLE
           (let* ((kernel (%pmu-kernel-ip-p (+ data (mod (+ pos 8) size))))
                  (ip (if kernel 0 (%pmu-get64 (+ data (mod (+ pos 8) size)))))
                  (nm (if kernel "<kernel>" (%pmu-fn-name ip code jit))))
             (setq samples (+ samples 1))
             (unless kernel (%pmu-bump hits ip 1))
             (%pmu-bump self nm 1)
             ;; IBS: after IP (and the callchain) comes PERF_SAMPLE_RAW -- a
             ;; u32 size, then a u32 capability word and the op registers:
             ;; OP_CTL, OP_RIP, OP_DATA, OP_DATA2, OP_DATA3, ...  OP_DATA bits
             ;; 31:16 are the tag-to-retire cycles; OP_DATA3 bit 7 is a
             ;; data-cache miss.
             (when ibs
               (let* ((raw (+ pos 16 (if callchain
                                         (+ 8 (* 8 (%pmu-get64 (+ data (mod (+ pos 16) size)))))
                                         0)))
                      (regs (+ raw 8))
                      (opdata (mem-ref (+ data (mod (+ regs 16) size)) :u32))
                      (opdata3 (mem-ref (+ data (mod (+ regs 32) size)) :u32)))
                 (%pmu-bump lat nm (logand (ash opdata -16) #xffff))
                 (when (logbitp 7 opdata3) (%pmu-bump miss nm 1))))
             (when callchain
               (let* ((nr (%pmu-get64 (+ data (mod (+ pos 16) size))))
                      (seen nil))
                 (dotimes (i (min nr 512))
                   (let* ((a (+ data (mod (+ pos 24 (* 8 i)) size)))
                          (hi (mem-ref (+ a 4) :u32)))
                     ;; PERF_CONTEXT_* markers are the top 4 KB of the address
                     ;; space, kernel frames the top half (IBS samples the
                     ;; kernel too); skip both without making the bignum
                     (unless (>= hi #xffff8000)
                       (let ((nm (%pmu-fn-name (%pmu-get64 a) code jit)))
                         (unless (member nm seen :test #'string=)
                           (setq seen (cons nm seen))
                           (%pmu-bump incl nm 1))))))
                 (unless (member nm seen :test #'string=)
                   (%pmu-bump incl nm 1))))))
          ((= type 2)                                ; PERF_RECORD_LOST: id, lost
           (setq lost (+ lost (%pmu-get64 (+ data (mod (+ pos 16) size)))))))
        (setq pos (+ pos rsize))))
    (%pmu-put64 (+ ring 1032) head)                  ; data_tail: consumed
    (flet ((sorted (h) (let ((l nil))
                         (maphash (lambda (k v) (setq l (cons (cons k v) l))) h)
                         (sort l #'> :key #'cdr))))
      (append (list :samples samples :lost lost :self (sorted self)
                    :inclusive (sorted incl) :hits hits)
              (when ibs (list :latency (sorted lat) :dc-miss (sorted miss)))))))

(defun %pmu-print-profile (p top stream)
  (let ((n (max 1 (getf p :samples))))
    (format stream "~&  ~:d samples~@[, ~:d LOST (raise :pages or :period)~]~%"
            (getf p :samples) (and (plusp (getf p :lost)) (getf p :lost)))
    (if (getf p :latency)
        ;; IBS: DC-MISS% is each function's share of the sampled data-cache
        ;; misses, SELF% its share of the sampled OPS (not of time -- see
        ;; PMU-PROFILE); sorted by misses, then ops.
        (let* ((miss (getf p :dc-miss)) (incl (getf p :inclusive))
               (mtot (max 1 (let ((s 0)) (dolist (e miss s) (setq s (+ s (cdr e)))))))
               (rows (sort (copy-list (getf p :self))
                           (lambda (a b)
                             (let ((ma (or (cdr (assoc (car a) miss :test #'string=)) 0))
                                   (mb (or (cdr (assoc (car b) miss :test #'string=)) 0)))
                               (if (= ma mb) (> (cdr a) (cdr b)) (> ma mb))))))
               (i 0))
          (format stream "  ~:d sampled data-cache misses~%" (if miss mtot 0))
          (format stream "  ~8a ~6a ~6a  ~a~%" "dc-miss%" "self%" "incl%" "function")
          (dolist (e rows)
            (when (>= i top) (return))
            (format stream "  ~8,1f ~6,1f ~6,1f  ~a~%"
                    (/ (* 100.0 (or (cdr (assoc (car e) miss :test #'string=)) 0)) mtot)
                    (/ (* 100.0 (cdr e)) n)
                    (/ (* 100.0 (or (cdr (assoc (car e) incl :test #'string=)) 0)) n) (car e))
            (setq i (+ i 1))))
        (progn
          (format stream "  ~6a ~6a  ~a~%" "self%" "incl%" "function")
          (let ((incl (getf p :inclusive)) (i 0))
            (dolist (e (getf p :self))
              (when (>= i top) (return))
              (format stream "  ~6,1f ~6,1f  ~a~%" (/ (* 100.0 (cdr e)) n)
                      (/ (* 100.0 (or (cdr (assoc (car e) incl :test #'string=)) 0)) n) (car e))
              (setq i (+ i 1))))))))

;;; --- annotation ---------------------------------------------------------------

(defun %pmu-name-keys (name)
  "The table keys NAME may be under: a symbol's bare name and its PKG::NAME
   (runtime-born packages are qualified), or a string as given."
  (if (symbolp name)
      (let ((p (symbol-package name)))
        (list (symbol-name name)
              (if p (concatenate 'string (package-name p) "::" (symbol-name name)) "")))
      (list name)))

(defun pmu-annotate (profile name prefix)
  "NAME is a symbol or a table name string.  Write NAME's machine code to PREFIX.bin and its sampled offsets to
   PREFIX.hits (\"offset count\" lines), from PROFILE's :HITS.  The code ends
   where the next known function starts (at most 64 KB).  Returns the entry
   address.  Then: scripts/pmu-annotate.py PREFIX"
  (let* ((code (%bt-code-table)) (jit (%bt-jit-table))
         (names (%pmu-name-keys name))
         (entry nil) (end nil))
    (dolist (tab (list code jit))
      (let ((v tab))
        (dotimes (i (length v))
          (when (and (null entry) (member (cadr (aref v i)) names :test #'string=))
            (setq entry (car (aref v i))
                  end (cond ((cddr (aref v i)) (+ entry (cddr (aref v i))))
                            ((< (+ i 1) (length v)) (min (car (aref v (+ i 1))) (+ entry 65536)))
                            (t (+ entry 65536))))))))
    (unless entry (error "pmu-annotate: no function named ~s" name))
    (with-open-file (s (concatenate 'string prefix ".bin") :direction :output
                       :if-exists :supersede :element-type '(unsigned-byte 8))
      (loop for a from entry below end do (write-byte (mem-ref a :u8) s)))
    (with-open-file (s (concatenate 'string prefix ".hits") :direction :output :if-exists :supersede)
      (format s "# ~a entry #x~x size ~d~%" name entry (- end entry))
      (maphash (lambda (ip c) (when (and (>= ip entry) (< ip end))
                                (format s "~d ~d~%" (- ip entry) c)))
               (getf profile :hits)))
    entry))
