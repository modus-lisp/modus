;;; net/hosted-fd-ready-a64.lisp -- LISTEN's question, asked of the kernel, on hosted AArch64.
;;;
;;; mvm/cl-fileio.lisp's %FD-INPUT-READY-P is the conservative T (a seam, declared
;;; overridable); hosted-sockets-post.lisp overrides it with poll(2) on x86-64.  On AArch64
;;; nothing did, so LISTEN on a DRAINED socket said T on every AArch64 image -- Linux, and
;;; macOS through the Darwin shim -- and a reader looping "while LISTEN, read a message"
;;; blocked in its next read: glass's one-thread RFB server read a viewer's first three
;;; messages, then hung in READ-BYTE and never sent a frame (`modus.com desk' on a Mac).
;;;
;;; COMPILED INTO THE IMAGE, not evaluated at boot: a boot-time DEFUN reaches the image's own
;;; callers (LISTEN) only by rewriting the old function's call pad, and on macOS the image
;;; runs from signed, read-only __TEXT, where that rewrite cannot happen.  Here it is
;;; last-defun-wins at build time, so LISTEN calls this directly on every AArch64 target.
;;;
;;; ppoll (73; AArch64 has no poll) on one pollfd with a zero timeout, both in this CPU's
;;; socket scratch (%SOCKADDR-SCRATCH, net/hosted-sync.lisp) at the offsets
;;; hosted-sockets-a64.lisp's SOCKET-WAIT-READABLE uses.  No scratch, or an error from
;;; ppoll (a descriptor it cannot ask): the conservative answer, T.

(defun %fd-input-ready-p (fd)
  (let ((a (%sockaddr-scratch)))
    (if (zerop a)
        t
        (let ((p (+ a 200)) (ts (+ a 216)))
          (setf (mem-ref p :u32) fd)
          (setf (mem-ref (+ p 4) :u32) 1)         ; events = POLLIN, revents = 0
          (setf (mem-ref ts :u32) 0) (setf (mem-ref (+ ts 4) :u32) 0)       ; tv_sec
          (setf (mem-ref (+ ts 8) :u32) 0) (setf (mem-ref (+ ts 12) :u32) 0) ; tv_nsec
          ;; ppoll(fds, 1, &zero, NULL sigmask, 0): %GC-SAFE-SYSCALL6 takes the number and SIX
          ;; arguments (five passed a wrong count, compiled into a PROGRAM-ERROR)
          (if (> (%gc-safe-syscall6 73 p 1 ts 0 0 0) 0) t nil)))))
