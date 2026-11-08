;;;; hosted-run-program.lisp — SB-EXT:RUN-PROGRAM, a pseudo-terminal, termios, and a
;;;; READ-CHAR-NO-HANG that does not wait.
;;;;
;;;;   ./modus --script test/hosted-run-program.lisp
;;;;
;;;; What a terminal emulator needs from the Lisp it runs on, driven the way glass/term
;;;; drives it on SBCL: a shell on a pty (:PTY T :WAIT NIL), its output read with
;;;; READ-CHAR-NO-HANG from the thread that owns the screen (no reader thread), keys
;;;; written to the same stream, the window size told with TIOCSWINSZ through
;;;; SB-ALIEN + SB-POSIX::IOCTL-WITH-POINTER-ARG, echo turned on with TCGETATTR /
;;;; TCSETATTR, and the shell hung up with PROCESS-KILL :PROCESS-GROUP.  Plus the
;;;; plain shapes: :WAIT T with an exit code, :OUTPUT :STREAM, :ENVIRONMENT,
;;;; :DIRECTORY, :SEARCH, and a Latin-1 stream returning bytes as themselves.
;;;;
;;;; Runs on a worker thread as well as main: glass's compositor, which starts the
;;;; shell, is not the main thread.

(defvar *fail* 0)
(defvar *checks* 0)

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "  ok   ~A = ~S~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A: got ~S want ~S~%" name got want))))

(defun chk-true (name got) (chk name (and got t) t))

(defun drain (stream until &optional (secs 10))
  "Characters read without waiting from STREAM until the text contains UNTIL (or SECS
   pass, or end of file).  Returns (values text polls-that-found-nothing).  Local state
   only: on a worker thread a global may not hold this thread's string."
  (let ((text (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)) (idle 0)
        (end (+ (get-internal-real-time) (* secs internal-time-units-per-second))))
    (loop
      (let ((c (read-char-no-hang stream nil :eof)))
        (cond ((eq c :eof) (return))
              ((null c) (setq idle (+ idle 1))
               (when (search until text) (return))
               (when (> (get-internal-real-time) end) (return))
               (sleep 1/100))
              (t (vector-push-extend c text)))))
    (values (coerce text 'simple-string) idle)))

(defun run-checks (where)
  (format t "~%-- ~A ---------------------------------------------~%" where)
  ;; :WAIT T, an exit code, :OUTPUT :STREAM, :ENVIRONMENT, :DIRECTORY, :SEARCH
  (let ((p (sb-ext:run-program "/bin/sh" (list "-c" "exit 3"))))
    (chk "exit code of sh -c 'exit 3'" (sb-ext:process-exit-code p) 3)
    (chk "status after :wait t" (sb-ext:process-status p) :exited))
  (let* ((p (sb-ext:run-program "sh" (list "-c" "echo $GREETING; pwd") :search t :output :stream
                                     :environment (list "GREETING=hello from env")
                                     :directory "/tmp/"))
         (text (with-output-to-string (o)
                 (loop for line = (read-line (sb-ext:process-output p) nil nil)
                       while line do (write-line line o)))))
    (chk "output :stream, environment, directory" text (format nil "hello from env~%/tmp~%")))
  ;; A Latin-1 stream returns bytes as themselves: printf writes c3 a9 (é in UTF-8).
  (let* ((p (sb-ext:run-program "/usr/bin/printf" (list "\\303\\251") :output :stream
                                :external-format :latin-1))
         (s (sb-ext:process-output p)))
    (chk "latin-1 reads bytes" (list (char-code (read-char s)) (char-code (read-char s))) (list #xc3 #xa9)))
  ;; THE PTY.
  (let* ((p (sb-ext:run-program "/bin/sh" (list "-i") :pty t :wait nil :external-format :latin-1
                                :environment (list "PS1=$ " "TERM=dumb" "PATH=/usr/bin:/bin")))
         (pty (sb-ext:process-pty p)))
    (chk-true "process-pty is a stream" (streamp pty))
    (chk-true "alive after :wait nil" (sb-ext:process-alive-p p))
    (multiple-value-bind (text idle) (drain pty "$ ")
      (chk-true "the shell's prompt arrives" (search "$ " text))
      (chk-true "read-char-no-hang said NIL while nothing was there (did not wait)" (> idle 0)))
    ;; the window size, as glass/term tells it
    (sb-alien:with-alien ((ws (sb-alien:array sb-alien:unsigned-short 4)))
      (setf (sb-alien:deref ws 0) 33 (sb-alien:deref ws 1) 101
            (sb-alien:deref ws 2) 0 (sb-alien:deref ws 3) 0)
      (chk "TIOCSWINSZ through ioctl-with-pointer-arg"
           (sb-posix::ioctl-with-pointer-arg (sb-sys:fd-stream-fd pty) #x5414 (sb-alien:addr ws)) 0))
    ;; echo on, through termios
    (let ((tio (sb-posix:tcgetattr pty)))
      (setf (sb-posix:termios-lflag tio) (logior (sb-posix:termios-lflag tio) sb-posix:echo))
      (sb-posix:tcsetattr pty sb-posix:tcsanow tio)
      (chk-true "ECHO is set after tcsetattr"
                (logtest (sb-posix:termios-lflag (sb-posix:tcgetattr pty)) sb-posix:echo)))
    (write-string (format nil "stty size; tty; echo done-$((6*7))~c" (code-char 13)) pty)
    (force-output pty)
    (let ((text (drain pty "done-42")))
      (chk-true "the typed line is echoed" (search "stty size; tty" text))
      (chk-true "stty size sees the winsize" (search "33 101" text))
      (chk-true "the shell's terminal is a pty" (search "/dev/pts/" text))
      (chk-true "the command ran" (search "done-42" text)))
    (chk-true "a session leader: its pgid is its pid (kill of the group works)"
              (sb-ext:process-kill p 1 :process-group))
    (sb-ext:process-wait p)
    (chk "status after the hangup" (sb-ext:process-status p) :signaled)
    (chk "the hangup was signal 1" (sb-ext:process-exit-code p) 1)
    (chk "end of file on the pty after exit"
         (loop repeat 100
               for c = (read-char-no-hang pty nil :eof)
               do (when (eq c :eof) (return :eof)) (unless c (sleep 1/100)))
         :eof)
    (sb-ext:process-close p)))

(run-checks "main thread")
(sb-thread:join-thread (sb-thread:make-thread (lambda () (run-checks "a worker thread") nil)))

(format t "~%=== VERDICT ==============================================~%")
(if (zerop *fail*)
    (format t "RUN-PROGRAM AND THE PTY: PASS (~A checks)~%" *checks*)
    (format t "RUN-PROGRAM AND THE PTY: FAIL (~A of ~A checks)~%" (- *checks* *fail*) *checks*))
