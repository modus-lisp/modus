;;;; hosted-process.lisp — run programs from the HOSTED x86-64 modus CLI.
;;;;
;;;; vfork-style clone + execve, as posix_spawn does it.  The uiop shim
;;;; (net/asdf-interface.lisp) builds UIOP:RUN-PROGRAM / LAUNCH-PROGRAM on
;;;; these; nothing here knows about streams or keywords.
;;;;
;;;; THE CHILD RUNS ALMOST NOTHING.  A child gets only the calling thread, so
;;;; whatever another thread was doing -- holding the runtime lock, half-way
;;;; through a collection -- is frozen in the child forever.  The child must
;;;; therefore not allocate, not take a lock, not signal.  Everything it needs
;;;; (the path, argv, the fds, the directory) is laid out in raw memory BEFORE
;;;; the fork, and %PROC-CHILD is nothing but syscalls on fixnums.
;;;;
;;;; THE CHILD'S SIGNAL STATE IS RESET.  modus ignores SIGPIPE, and an ignored
;;;; disposition survives execve: a shell pipeline (`yes | head`) would then
;;;; see EPIPE instead of dying quietly.  The child sets SIGPIPE back to SIG_DFL
;;;; and clears its signal mask; handled signals reset on exec by themselves.
;;;;
;;;; ENVIRONMENT: the child gets this process's own envp -- the vector on the
;;;; initial stack (lib/cli-toplevel.lisp), which fork copies like any memory.
;;;;
;;;; x86-64 syscalls: read 0, write 1, open 2, close 3, poll 7, mmap via
;;;; %mmap-shared-page, munmap 11, rt_sigaction 13, rt_sigprocmask 14,
;;;; access 21, dup2 33, clone 56, execve 59, wait4 61, kill 62, chdir 80,
;;;; exit_group 231, pipe2 293.

(defun %proc-o-cloexec () #x80000)

;;; 64-bit stores as two exact 32-bit halves (a :u64 store is not exact).
(defun %proc-put64 (addr v)
  (setf (mem-ref addr :u32) (logand v #xFFFFFFFF))
  (setf (mem-ref (+ addr 4) :u32) (logand (ash v -32) #xFFFFFFFF)))

(defun %proc-utf8-len (s)
  (let ((n 0) (i 0) (len (length s)))
    (loop
      (when (>= i len) (return n))
      (let ((c (char-code (char s i))))
        (setq n (+ n (cond ((< c #x80) 1) ((< c #x800) 2) ((< c #x10000) 3) (t 4)))))
      (setq i (+ i 1)))))

(defun %proc-put-cstr (s addr)
  "Write S as NUL-terminated UTF-8 at ADDR; return the address after the NUL."
  (let ((i 0) (len (length s)) (p addr))
    (loop
      (when (>= i len) (return nil))
      (let ((c (char-code (char s i))))
        (cond
          ((< c #x80) (setf (mem-ref p :u8) c) (setq p (+ p 1)))
          ((< c #x800)
           (setf (mem-ref p :u8) (logior #xC0 (ash c -6)))
           (setf (mem-ref (+ p 1) :u8) (logior #x80 (logand c #x3F)))
           (setq p (+ p 2)))
          ((< c #x10000)
           (setf (mem-ref p :u8) (logior #xE0 (ash c -12)))
           (setf (mem-ref (+ p 1) :u8) (logior #x80 (logand (ash c -6) #x3F)))
           (setf (mem-ref (+ p 2) :u8) (logior #x80 (logand c #x3F)))
           (setq p (+ p 3)))
          (t
           (setf (mem-ref p :u8) (logior #xF0 (ash c -18)))
           (setf (mem-ref (+ p 1) :u8) (logior #x80 (logand (ash c -12) #x3F)))
           (setf (mem-ref (+ p 2) :u8) (logior #x80 (logand (ash c -6) #x3F)))
           (setf (mem-ref (+ p 3) :u8) (logior #x80 (logand c #x3F)))
           (setq p (+ p 4)))))
      (setq i (+ i 1)))
    (setf (mem-ref p :u8) 0)
    (+ p 1)))

(defun %proc-envp ()
  "This process's envp vector: on the initial stack, after argv's NULL."
  (+ (%cli-argv-base) (* 8 (+ (%cli-argc) 1))))

(defun %proc-scratch (bytes)
  "A fresh zeroed mapping of at least BYTES (page-rounded), or 0 on failure.
   Per call, so two threads spawning at once never share one."
  (let* ((n (* 4096 (+ 1 (floor (+ bytes 4095) 4096))))
         (m (%mmap-shared-page n)))
    (if (< m 4096) 0 m)))

(defun %proc-unmap (addr bytes)
  (when (> addr 0)
    (syscall3 11 addr (* 4096 (+ 1 (floor (+ bytes 4095) 4096))) 0)))

(defun %proc-executable-p (path)
  "access(PATH, X_OK) = 0."
  (let* ((buf (%proc-scratch (+ 1 (%proc-utf8-len path))))
         (ok (and (> buf 0)
                  (progn (%proc-put-cstr path buf) (= 0 (syscall3 21 buf 1 0))))))
    (%proc-unmap buf (+ 1 (%proc-utf8-len path)))
    ok))

(defun %proc-find-program (name)
  "NAME itself when it contains a /, else the first executable NAME on $PATH
   (execvp's rule), else NIL."
  (if (find #\/ name)
      name
      (let* ((path (or (%cli-getenv "PATH") "/usr/local/bin:/usr/bin:/bin"))
             (start 0)
             (len (length path)))
        (loop
          (when (> start len) (return nil))
          (let* ((end (or (position #\: path :start start) len))
                 (dir (subseq path start end))
                 (cand (concatenate 'string (if (= (length dir) 0) "." dir) "/" name)))
            (when (%proc-executable-p cand) (return cand))
            (setq start (+ end 1)))))))

(defun %proc-pipe ()
  "pipe2(O_CLOEXEC): (read-fd . write-fd), or NIL."
  (let ((buf (%proc-scratch 8)))
    (if (= buf 0)
        nil
        (let ((r (syscall3 293 buf (%proc-o-cloexec) 0)))
          (let ((res (if (< r 0) nil (cons (mem-ref buf :u32) (mem-ref (+ buf 4) :u32)))))
            (%proc-unmap buf 8)
            res)))))

(defun %proc-open (path mode)
  "Open PATH for the child: MODE :input, :supersede or :append.  fd or -errno."
  (let* ((n (+ 1 (%proc-utf8-len path)))
         (buf (%proc-scratch n)))
    (if (= buf 0)
        -12
        (let ((flags (cond ((eq mode :input) 0)
                           ((eq mode :append) (logior 1 64 1024))   ; WRONLY|CREAT|APPEND
                           (t (logior 1 64 512)))))                 ; WRONLY|CREAT|TRUNC
          (%proc-put-cstr path buf)
          (let ((fd (syscall3 2 buf (logior flags (%proc-o-cloexec)) 420)))
            (%proc-unmap buf n)
            fd)))))

(defun %proc-close (fd) (when (>= fd 0) (syscall3 3 fd 0 0)))

(defun %proc-child (path-addr argv-addr envp dir-addr in out err sig-addr)
  "In the forked child: wire fds 0-2, chdir, reset signals, exec.  Syscalls on
   fixnums only -- see the header."
  (when (>= in 0) (syscall3 33 in 0 0))
  (when (>= out 0) (syscall3 33 out 1 0))
  (when (>= err 0) (syscall3 33 err 2 0))
  (when (> dir-addr 0)
    (when (< (syscall3 80 dir-addr 0 0) 0) (syscall3 231 127 0 0)))
  ;; sig-addr: a zeroed 32-byte area = sigaction{SIG_DFL} and an empty sigset.
  (syscall6 13 13 sig-addr 0 8 0 0)        ; SIGPIPE -> SIG_DFL
  (syscall6 14 2 sig-addr 0 8 0 0)         ; SIG_SETMASK, empty
  (syscall3 59 path-addr argv-addr envp)
  (syscall3 231 127 0 0))                  ; exec failed: 127, as a shell says

(defun %proc-spawn (path args dir in out err)
  "fork+exec PATH (a resolved program path) with argv ARGS (strings, ARGS[0]
   included), in directory DIR (string or NIL), stdin/stdout/stderr = IN, OUT,
   ERR (fds, or -1 to inherit).  Returns the pid, or -errno."
  (let* ((nargs (length args))
         (bytes (+ 64 (* 8 (+ nargs 1)) (+ 1 (%proc-utf8-len path))
                   (if dir (+ 1 (%proc-utf8-len dir)) 0)
                   (let ((s 0)) (dolist (a args s) (setq s (+ s 1 (%proc-utf8-len a)))))))
         (buf (%proc-scratch bytes)))
    (if (= buf 0)
        -12
        (let* ((sig buf)                                  ; 32 zero bytes
               (argv (+ buf 64))
               (p (+ argv (* 8 (+ nargs 1))))
               (path-addr p)
               (dir-addr 0))
          (setq p (%proc-put-cstr path p))
          (when dir (setq dir-addr p) (setq p (%proc-put-cstr dir p)))
          (let ((i 0))
            (dolist (a args)
              (%proc-put64 (+ argv (* 8 i)) p)
              (setq p (%proc-put-cstr a p))
              (setq i (+ i 1)))
            (%proc-put64 (+ argv (* 8 i)) 0))
          (let ((envp (%proc-envp)))
            ;; clone(CLONE_VFORK|SIGCHLD), not fork: the parent sleeps until
            ;; the child has exec'd.  An actor's stack is in a SHARED mapping,
            ;; which a forked child does not get a copy of -- parent and child
            ;; then ran on the same stack at once and corrupted each other
            ;; (memory faults, exec'ing garbage).  With the parent asleep the
            ;; child alone uses it, and only below the parent's SP.
            (let ((pid (syscall6 56 #x4011 0 0 0 0 0)))
              (when (= pid 0)
                (%proc-child path-addr argv envp dir-addr in out err sig))
              (%proc-unmap buf bytes)
              pid))))))

(defun %proc-wait (pid hang)
  "wait4 PID.  Returns the exit code (128+N for death by signal N), or NIL when
   HANG is false and it is still running.  EINTR is retried."
  (let ((buf (%proc-scratch 8)))
    (if (= buf 0)
        nil
        (let ((res nil))
          (loop
            (let ((r (syscall6 61 pid buf (if hang 0 1) 0 0 0)))
              (cond
                ((= r -4) nil)                            ; EINTR: again
                ((< r 0) (setq res -1) (return nil))      ; ECHILD etc.
                ((= r 0) (setq res nil) (return nil))     ; WNOHANG, running
                (t (let ((st (mem-ref buf :u32)))
                     (setq res (if (= (logand st #x7F) 0)
                                   (logand (ash st -8) #xFF)
                                   (+ 128 (logand st #x7F))))
                     (return nil))))))
          (%proc-unmap buf 8)
          res))))

(defun %proc-kill (pid sig) (syscall3 62 pid sig 0))

(defun %proc-drain (fd1 fd2)
  "Read FD1 and FD2 (-1 = none) to EOF together, so a child filling one pipe
   never deadlocks against us reading the other.  Returns (bytes1 . bytes2),
   each an (unsigned-byte 8) vector."
  (let* ((cap 65536)
         (buf (%proc-scratch (+ cap 16)))
         (pfd (+ buf cap))
         (o1 (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (o2 (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (a fd1) (b fd2))
    (when (= buf 0) (return-from %proc-drain (cons o1 o2)))
    (loop
      (when (and (< a 0) (< b 0)) (return nil))
      ;; struct pollfd {int fd; short events; short revents} x2; POLLIN = 1.
      ;; A negative fd is ignored by poll(2).
      (setf (mem-ref pfd :u32) (logand a #xFFFFFFFF))
      (setf (mem-ref (+ pfd 4) :u32) 1)
      (setf (mem-ref (+ pfd 8) :u32) (logand b #xFFFFFFFF))
      (setf (mem-ref (+ pfd 12) :u32) 1)
      (let ((r (syscall3 7 pfd 2 -1)))
        (when (>= r 0)
          (let ((ra (mem-ref (+ pfd 6) :u16))
                (rb (mem-ref (+ pfd 14) :u16)))
            (when (and (>= a 0) (> ra 0))
              (let ((n (syscall3 0 a buf cap)))
                (cond ((> n 0) (dotimes (i n) (vector-push-extend (mem-ref (+ buf i) :u8) o1)))
                      ((= n -4) nil)
                      (t (setq a -1)))))
            (when (and (>= b 0) (> rb 0))
              (let ((n (syscall3 0 b buf cap)))
                (cond ((> n 0) (dotimes (i n) (vector-push-extend (mem-ref (+ buf i) :u8) o2)))
                      ((= n -4) nil)
                      (t (setq b -1)))))))))
    (%proc-unmap buf (+ cap 16))
    (cons o1 o2)))
