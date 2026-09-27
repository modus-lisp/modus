;;;; syscall-clobber.lisp -- a syscall must not clobber live registers.
;;;;
;;;; Run: ./modus --script test/syscall-clobber.lisp   (x64, i386, aarch64, riscv)
;;;;
;;;; x86-64's SYSCALL instruction overwrites RCX (return RIP) and R11 (RFLAGS),
;;;; both ordinary allocatable registers in the x64 back end (V5, V8).  The
;;;; syscall traps saved neither, so compiled code holding a value in RCX
;;;; across a syscall lost it.  It showed as a SYSCALL3 in interpreted code (a
;;;; top-level form of this script) "returning" its own syscall number: the
;;;; interpreter's trap arm does (setf (svref regs 0) (syscall3 ...)) with the
;;;; store index in RCX, so the result went to regs + <return RIP>*4 -- a wild
;;;; store -- and V0 kept the number.  Separately, the interpreter used to skip
;;;; syscall traps altogether.  Either bug makes this fail.
;;;;
;;;; getpid is 39 on x86-64, 20 on i386 and 172 in the generic ABI (aarch64,
;;;; riscv).  *FEATURES* does not name the architecture, so every candidate is
;;;; tried; the others are harmless with zero arguments (writev(0,NULL,0),
;;;; mkdir(NULL), iopl(0) / prctl(0)).  Exactly one must return the pid.

(let* ((pid (%sys-getpid))
       (got (list (syscall3 39 0 0 0) (syscall3 20 0 0 0) (syscall3 172 0 0 0)))
       (hits (count pid got)))
  (format t "~&syscall-clobber: pid ~D, candidates ~S -> ~D match~%" pid got hits)
  (unless (= hits 1)
    (error "syscall-clobber: interpreted SYSCALL3 did not return the pid")))
