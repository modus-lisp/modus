;;;; main-jobs.lisp - JOBS: evaluations on the main thread, blocking or detached, by name.
;;;;
;;;; The main thread is where evaluation is correct today (off it, compiling at runtime loses
;;;; SETQs and can wedge core 0), so everything typed at the board -- an SSH command, a desk
;;;; started over SSH -- runs there, as a JOB:
;;;;
;;;;   (detach FORM :name "desk")   queue FORM, return the job's name at once
;;;;   (jobs)                       every job: name, status, seconds since it started
;;;;   (attach "desk")              what it printed since the last ATTACH, and its status and
;;;;                                result (:ALL T for everything)
;;;;   (stop-job "desk")            ask it to end
;;;;   (serve-jobs)                 for a job that runs long (the desk): call it at a safe point
;;;;                                and queued jobs run there; a stop asked of the job calling it
;;;;                                unwinds it from there, through its UNWIND-PROTECTs
;;;;
;;;; An SSH command is a blocking job: the session waits for its result.  Jobs run one at a
;;;; time, oldest first, from the console's wait (the REPL idle) or from a SERVE-JOBS call
;;;; inside a running job -- one level deep: a job that runs inside another runs to its end
;;;; before the outer one continues, so it should be short (the desk's frame waits for it).
;;;;
;;;; Loaded at runtime with the network service (net/net-service.lisp sends SSH commands here).

(in-package :modus-int)

(defstruct (job (:constructor %make-job))
  name form text (status :queued) (out (make-string-output-stream)) (shown "") result
  reply-slot (started 0) (ended 0) stop)

(defvar *jobs* nil "Every job, newest first (finished ones are trimmed to the last 32).")
(defvar *job-queue* nil "Jobs waiting to run, oldest first.")
(defvar *job-current* nil "The innermost job running.")
(defvar *job-depth* 0)
(defvar *job-counter* 0)

(defun %job-now () (get-universal-time))

(defun %job-name (want)
  (let ((base (or want (format nil "job-~D" (incf *job-counter*)))))
    (if (find base *jobs* :key #'job-name :test #'string=)
        (loop for k from 2 for n = (format nil "~A-~D" base k)
              unless (find n *jobs* :key #'job-name :test #'string=) return n)
        base)))

(defun %job-trim ()
  (let ((done 0))
    (setq *jobs* (remove-if (lambda (j) (and (member (job-status j) '(:done :failed :stopped))
                                             (> (incf done) 32)))
                            *jobs*))))

(defun %job-add (j)
  (push j *jobs*)
  (setq *job-queue* (append *job-queue* (list j)))
  (%job-trim)
  j)

(defun job-submit (form &key name text reply-slot)
  "Queue a job (FORM, or TEXT to read when it runs).  Its name."
  (job-name (%job-add (%make-job :name (%job-name name) :form form :text text
                                 :reply-slot reply-slot))))

(defmacro detach (form &key name)
  "Run FORM as a job on the main thread and return at once: the job's name."
  `(job-submit ',form :name ,name))

(defun %job-find (name)
  (or (find name *jobs* :key #'job-name :test #'string-equal)
      (error "no job named ~S" name)))

(defun jobs ()
  "(name status seconds) for every job, newest first."
  (mapcar (lambda (j) (list (job-name j) (job-status j)
                            (if (zerop (job-started j)) 0
                                (- (if (zerop (job-ended j)) (%job-now) (job-ended j)) (job-started j)))))
          *jobs*))

(defun attach (name &key all)
  "Print what job NAME printed since the last ATTACH (everything with ALL); its status and
   result as values."
  (let* ((j (%job-find name))
         (s (get-output-stream-string (job-out j)))
         (text (concatenate 'string (job-shown j) s)))
    ;; GET-OUTPUT-STREAM-STRING empties the stream: keep what was said, mark what was shown
    (setf (job-shown j) text)
    (write-string (if all text s))
    (values (job-status j) (job-result j))))

(defun stop-job (name)
  "Ask job NAME to end: a queued one is dropped; a running one unwinds at its next SERVE-JOBS."
  (let ((j (%job-find name)))
    (if (eq (job-status j) :queued)
        (progn (setq *job-queue* (remove j *job-queue*))
               (setf (job-status j) :stopped (job-ended j) (%job-now)))
        (setf (job-stop j) t))
    (job-status j)))

(defun job-stop-requested-p ()
  (and *job-current* (job-stop *job-current*) t))

(defun %job-reply-bytes (out result)
  (let* ((text (concatenate 'string out "= " result (string #\Newline)))
         (v (make-array (length text) :element-type '(unsigned-byte 8))))
    (dotimes (i (length text) v) (setf (aref v i) (logand (char-code (char text i)) 255)))))

(defun %job-run (j)
  (setf (job-status j) :running (job-started j) (%job-now))
  (let ((outer *job-current*) (status :done) (result nil))
    (setq *job-current* j *job-depth* (+ *job-depth* 1))
    (unwind-protect
         (let ((stopped t))
           (catch '%job-stop
             (handler-case
                 (let ((v (let ((*standard-output* (job-out j)))
                            (eval (or (job-form j) (read-from-string (job-text j)))))))
                   (setq result (handler-case (prin1-to-string v) (error () "#<unprintable>"))))
               (error (c)
                 (setq status :failed
                       ;; (ERROR TYPE "message"): the condition's report, not just its type
                       result (handler-case
                                  (prin1-to-string (list 'error (type-of c) (princ-to-string c)))
                                (error () (handler-case (prin1-to-string (list 'error c))
                                            (error () "(ERROR #<unprintable>)")))))))
             (setq stopped nil))
           (when stopped (setq status :stopped result ":STOPPED")))
      (setq *job-current* outer *job-depth* (- *job-depth* 1))
      (setf (job-status j) status (job-result j) result (job-ended j) (%job-now))
      (when (job-reply-slot j)
        (let ((out (get-output-stream-string (job-out j))))
          (setf (job-shown j) out)
          (%ns-send (svref *ns-slot-ch* (job-reply-slot j))
                    (list :result (%job-reply-bytes out (or result "NIL")))))))
    j))

(defun %jobs-take-commands ()
  "Evaluations waiting on the channel become jobs: (SLOT TEXT), where SLOT is an SSH handler's
   reply channel (a blocking job) or -1 for none -- a thread on another core asking the main
   thread to run something.  A number, not NIL: a thread sends plain data, and a symbol in a
   message is encoded by name through the package tables, which a thread may not touch."
  (when *ns-main-jobs*
    (loop
      (let ((m (%ns-try *ns-main-jobs*)))
        (unless m (return))
        (let ((slot (car m)))
          (%job-add (%make-job :name (%job-name (if (and (integerp slot) (>= slot 0))
                                                    (format nil "ssh-~D" (incf *job-counter*))
                                                    nil))
                               :text (cadr m)
                               :reply-slot (and (integerp slot) (>= slot 0) slot))))))))

(defun %jobs-step (max-depth)
  "Run the next queued job if this depth allows one.  T when one ran."
  (when (and (zerop (percpu-ref 16)) (< *job-depth* max-depth))
    (%jobs-take-commands)
    (let ((j (pop *job-queue*)))
      (when j (%job-run j) t))))

;; THE CONSOLE'S WAIT (net/bare-threads.lisp %CONSOLE-IDLE): the REPL is idle, nothing runs.
(defun %bt-main-jobs () (%jobs-step 1))

(defun serve-jobs ()
  "From a long-running job, at a safe point: run what is queued (one level deep), and unwind
   this job if it was asked to stop."
  (when (job-stop-requested-p) (throw '%job-stop nil))
  (loop (unless (%jobs-step 2) (return)))
  (when (job-stop-requested-p) (throw '%job-stop nil))
  nil)

;; the user's names, in MODUS (SB-EXT's role), so CL-USER sees them unqualified
(dolist (n '("DETACH" "JOBS" "ATTACH" "STOP-JOB" "SERVE-JOBS" "JOB-STOP-REQUESTED-P"))
  (let ((sym (intern n "MODUS-INT")))
    (handler-case (progn (import sym "MODUS") (export sym "MODUS"))
      (error (c) (kmsg 2 "JOBX" (princ-to-string c))))))
