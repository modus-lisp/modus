;;;; trace.lisp - MVM-compatible tracing and profiling utilities
;;;;
;;;; MVM port of lib/trace.lisp using mvm-rdtsc for cycle counting.
;;;; On bare metal, output goes to serial console.
;;;;

(in-package :cl-user)

(defpackage :modus.mvm.trace
  (:use :cl)
  (:export
   #:*trace-enabled*
   #:*trace-output-serial*
   #:mtime
   #:mtrace
   #:mprofile
   #:show-profile
   #:reset-profile
   #:benchmark))

(in-package :modus.mvm.trace)

;;; ============================================================
;;; Configuration
;;; ============================================================

(defvar *trace-enabled* t
  "When NIL, tracing macros become no-ops")

(defvar *trace-output-serial* t
  "When T, output goes to serial console. Otherwise uses write-char based output.")

(defvar *tsc-per-microsecond* 2000
  "TSC ticks per microsecond. Default assumes 2 GHz. Adjust for your hardware.")

;;; ============================================================
;;; Low-level cycle counting
;;;
;;; The rdtsc function is a builtin compiled by the MVM compiler.
;;; Calling (rdtsc) at runtime emits TRAP #x0310 which executes
;;; the RDTSC instruction on native hardware, returning 64-bit cycles.
;;;
;;; In interpreter mode, this returns 0.
;;; ============================================================

(declaim (optimize (speed 3) (safety 0)))

(defun read-cycle-count ()
  "Read timestamp counter, return 64-bit cycle count.
   On native MVM: uses RDTSC instruction via TRAP #x0310.
   In interpreter: returns 0."
  (the fixnum (rdtsc)))

;;; ============================================================
;;; Serial output helpers
;;; ============================================================

(defun print-decimal (n)
  "Print decimal number to serial console."
  (declare (optimize (speed 3) (safety 0)))
  (when (zerop n)
    (write-char-serial 48)  ; "0"
    (return-from print-decimal nil))
  (when (minusp n)
    (write-char-serial 45)  ; "-"
    (setq n (- 0 n)))
  (let ((buf nil))
    (loop while (plusp n)
          do (push (logand n 15) buf)
             (setq n (ash n -4)))
    (dolist (d (reverse buf))
      (write-char-serial (if (< d 10)
                             (+ 48 d)
                             (+ 55 d))))))

(defun write-string-serial (s)
  "Write string to serial console."
  (declare (optimize (speed 3) (safety 0)))
  (dotimes (i (length s))
    (write-char-serial (char-code (char s i)))))

(defun write-newline-serial ()
  (write-char-serial 10))

;;; ============================================================
;;; Time measurement
;;; ============================================================

(defun cycles-to-microseconds (cycles)
  (floor cycles *tsc-per-microsecond*))

;;; ============================================================
;;; Simple timing macro
;;; ============================================================

(defmacro mtime (form)
  "Time the execution of FORM, printing elapsed cycles and microseconds."
  (let ((start (gensym "START-"))
        (end (gensym "END-"))
        (result (gensym "RESULT-"))
        (cycles (gensym "CYCLES-")))
    `(if (not *trace-enabled*)
         ,form
         (let ((,start (rdtsc)))
           (let ((,result ,form))
             (let ((,end (rdtsc)))
               (let ((,cycles (if (>= ,end ,start)
                                  (- ,end ,start)
                                  (- ,end ,start #x100000000))))
                 (write-string-serial "; Time: ")
                 (print-decimal ,cycles)
                 (write-string-serial " cycles (")
                 (print-decimal (cycles-to-microseconds ,cycles))
                 (write-string-serial " us)")
                 (write-newline-serial))
               ,result))))))

;;; ============================================================
;;; Trace printing utilities
;;; ============================================================

(defvar *trace-depth* 0)

(defun trace-indent ()
  "Print indentation for current trace depth"
  (dotimes (i *trace-depth*)
    (write-string-serial "  ")))

(defun trace-entry (name &rest args)
  "Print trace entry message"
  (trace-indent)
  (write-string-serial "> ")
  (write-string-serial (string name))
  (dolist (a args)
    (write-char-serial 32)
    (print-decimal a))
  (write-newline-serial))

(defun trace-exit (name result elapsed-cycles)
  "Print trace exit message with result and timing"
  (trace-indent)
  (write-string-serial "< ")
  (write-string-serial (string name))
  (write-string-serial " = ")
  (print-decimal result)
  (write-string-serial " (")
  (print-decimal elapsed-cycles)
  (write-string-serial " cycles, ")
  (print-decimal (cycles-to-microseconds elapsed-cycles))
  (write-string-serial " us)")
  (write-newline-serial))

;;; ============================================================
;;; Tracing macro for individual calls
;;; ============================================================

(defmacro mtrace (name form)
  "Trace a single form, printing entry/exit and timing."
  (let ((start (gensym "START-"))
        (end (gensym "END-"))
        (result (gensym "RESULT-"))
        (cycles (gensym "CYCLES-")))
    `(if (not *trace-enabled*)
         ,form
         (progn
           (trace-entry ',name)
           (let ((*trace-depth* (1+ *trace-depth*)))
             (let ((,start (rdtsc)))
               (let ((,result ,form))
                 (let ((,end (rdtsc)))
                   (let ((,cycles (if (>= ,end ,start)
                                      (- ,end ,start)
                                      (- ,end ,start #x100000000))))
                     (let ((*trace-depth* (1- *trace-depth*)))
                       (trace-exit ',name ,result ,cycles))
                     ,result)))))))))

;;; ============================================================
;;; Simple profiling with timing accumulation
;;;;
;;;; Note: Hash tables may not be available on bare metal.
;;;; Use simple arrays when profiling a known set of functions.
;;; ============================================================

(defvar *profile-data* nil
  "Hash table mapping names to (count . total-cycles).
   Set to NIL if hash tables unavailable.")

(defun ensure-profile-data ()
  "Initialize profile data if needed."
  (when (null *profile-data*)
    ;; Try to use hash table, fall back to nil
    (setq *profile-data* nil)))

(defun reset-profile ()
  "Clear all profiling data"
  (setq *profile-data* nil)
  t)

(defun record-profile (name cycles)
  "Record timing data for NAME"
  (when (null *profile-data*)
    (return-from record-profile nil))
  (let ((entry (gethash name *profile-data*)))
    (if entry
        (progn
          (incf (car entry))
          (incf (cdr entry) cycles))
        (setf (gethash name *profile-data*) (cons 1 cycles)))))

(defun record-profile-simple (index cycles)
  "Record timing data by simple index (for array-based profiling)."
  (when (null *profile-data*)
    (return-from record-profile-simple nil))
  (let ((entry (aref *profile-data* index)))
    (if entry
        (progn
          (incf (car entry))
          (incf (cdr entry) cycles))
        (setf (aref *profile-data* index) (cons 1 cycles)))))

(defmacro mprofile (name form)
  "Profile FORM, accumulating timing data under NAME.
   Use (show-profile) to display results."
  (let ((start (gensym "START-"))
        (end (gensym "END-"))
        (result (gensym "RESULT-"))
        (cycles (gensym "CYCLES-")))
    `(if (not *trace-enabled*)
         ,form
         (let ((,start (rdtsc)))
           (let ((,result ,form))
             (let ((,end (rdtsc)))
               (let ((,cycles (if (>= ,end ,start)
                                  (- ,end ,start)
                                  (- ,end ,start #x100000000))))
                 (record-profile ',name ,cycles)
                 ,result)))))))

(defmacro mprofile-simple (index form)
  "Profile FORM using simple index instead of name (for arrays)."
  (let ((start (gensym "START-"))
        (end (gensym "END-"))
        (result (gensym "RESULT-"))
        (cycles (gensym "CYCLES-")))
    `(if (not *trace-enabled*)
         ,form
         (let ((,start (rdtsc)))
           (let ((,result ,form))
             (let ((,end (rdtsc)))
               (let ((,cycles (if (>= ,end ,start)
                                  (- ,end ,start)
                                  (- ,end ,start #x100000000))))
                 (record-profile-simple ,index ,cycles)
                 ,result)))))))

(defun show-profile ()
  "Display accumulated profile data.
   Note: Requires hash tables - may not work on bare metal."
  (when (null *profile-data*)
    (write-string-serial "; Profile data not initialized (hash tables unavailable)")
    (write-newline-serial)
    (return-from show-profile nil))
  (write-newline-serial)
  (write-string-serial "=== Profile Results ===")
  (write-newline-serial)
  (maphash (lambda (name data)
             (let* ((count (car data))
                    (total-cycles (cdr data))
                    (total-us (cycles-to-microseconds total-cycles))
                    (avg-us (if (zerop count) 0 (floor total-us count))))
               (write-string-serial (string name))
               (write-char-serial 32)
               (write-string-serial "count=")
               (print-decimal count)
               (write-string-serial " total=")
               (print-decimal total-us)
               (write-string-serial "us avg=")
               (print-decimal avg-us)
               (write-string-serial "us")
               (write-newline-serial)))
           *profile-data*)
  (write-newline-serial)
  (values))

;;; ============================================================
;;; Quick benchmark utility
;;; ============================================================

(defmacro benchmark (n form)
  "Run FORM N times, reporting total and average time."
  (let ((i (gensym "I-"))
        (start (gensym "START-"))
        (end (gensym "END-"))
        (cycles (gensym "CYCLES-")))
    `(let ((,start (rdtsc)))
       (dotimes (,i ,n)
         (declare (ignore ,i))
         ,form)
       (let ((,end (rdtsc)))
         (let ((,cycles (if (>= ,end ,start)
                            (- ,end ,start)
                            (- ,end ,start #x100000000))))
           (let ((total-us (cycles-to-microseconds ,cycles))
                 (avg-us (cycles-to-microseconds (floor ,cycles ,n))))
             (write-string-serial "; Benchmark: ")
             (print-decimal ,n)
             (write-string-serial " iterations")
             (write-newline-serial)
             (write-string-serial ";   Total: ")
             (print-decimal ,cycles)
             (write-string-serial " cycles (")
             (print-decimal total-us)
             (write-string-serial " us)")
             (write-newline-serial)
             (write-string-serial ";   Average: ")
             (print-decimal avg-us)
             (write-string-serial " us per iteration")
             (write-newline-serial)
             (values ,cycles ,n)))))))

;;; ============================================================
;;; CPU frequency calibration
;;; ============================================================

(defun calibrate-tsc (&optional (iterations 10000000))
  "Attempt to calibrate TSC by busy-waiting.
   Returns estimated cycles per iteration.
   Note: Time on host must be measured externally."
  (write-string-serial "; Running busy loop for calibration...")
  (write-newline-serial)
  (write-string-serial "; Count to ")
  (print-decimal iterations)
  (write-string-serial " and note elapsed time on host...")
  (write-newline-serial)
  (let ((start (rdtsc)))
    (let ((count 0))
      (dotimes (i iterations)
        (incf count)))
    (let ((end (rdtsc)))
      (let ((cycles (if (>= end start)
                        (- end start)
                        (- end start #x100000000))))
        (write-string-serial "; ")
        (print-decimal cycles)
        (write-string-serial " cycles for ")
        (print-decimal iterations)
        (write-string-serial " iterations")
        (write-newline-serial)
        (write-string-serial "; Set *tsc-per-microsecond* based on your host timing")
        (write-newline-serial)
        cycles))))