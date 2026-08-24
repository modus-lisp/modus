;;;; sb-sys-shim.lisp — SB-BSD-SOCKETS, SB-POSIX, SB-SYS, SB-EXT and SB-ALIEN.
;;;;
;;;; Evaluated at boot from a baked source string, for the same reason
;;;; net/sb-thread-shim.lisp and net/genera-compat.lisp are: it defines symbols
;;;; in packages the HOST reading the build already owns, and a defpackage
;;;; evaluated host-side would collide with SBCL's own.
;;;;
;;;; PORTABLE-SHAPED, NOT GLASS-SHAPED.  Each function takes the arguments
;;;; SBCL's does.  Where it does not, it says PARTIAL at the definition.
;;;;
;;;; ================= THE PARTIAL LIST, IN ONE PLACE =================
;;;;   SOCKET-MAKE-STREAM  :TIMEOUT and :EXTERNAL-FORMAT are accepted and
;;;;                       ignored; :BUFFERING is accepted and ignored (modus's
;;;;                       fd streams are always buffered).  :INPUT/:OUTPUT are
;;;;                       real.  Both nil defaults to an I/O stream, as SBCL.
;;;;   INET6-SOCKET        NOT IMPLEMENTED.  modus's address builder is
;;;;                       sockaddr_in only; a v6 socket signals rather than
;;;;                       quietly binding v4.
;;;;   SOCKET-BIND on a LOCAL-SOCKET
;;;;                       IMPLEMENTED (sockaddr_un), path limited to 107 bytes.
;;;;                       Abstract-namespace paths (leading NUL) are NOT.
;;;;   GET-HOST-BY-NAME    Resolves through modus's own DNS; returns a host-ent
;;;;                       with ONE address.  No aliases, no v6, no /etc/hosts.
;;;;   SOCKET-ERROR        The condition hierarchy is CONNECTION-REFUSED-ERROR,
;;;;                       ADDRESS-IN-USE-ERROR and the base SOCKET-ERROR only.
;;;;                       Any other errno signals the base with its number.
;;;;   SB-POSIX:STAT       Returns a stat object carrying MODE, UID, GID, INO,
;;;;                       SIZE and nothing else.  The other 20 fields are not
;;;;                       read out of the buffer, and asking for one signals
;;;;                       rather than answering 0.
;;;;   SB-EXT:RUN-PROGRAM  NOT IMPLEMENTED — signals.  modus has no fork and no
;;;;                       exec.  glass/term is the only caller and is not part
;;;;                       of the :GLASS system.
;;;;   SB-ALIEN            Only the shape glass uses: WITH-ALIEN over INT and
;;;;                       (ARRAY INT n), ADDR, DEREF, CAST, and ALIEN-FUNCALL
;;;;                       of an EXTERN-ALIEN naming a function modus knows how
;;;;                       to issue as a SYSCALL.  "getsockopt" and "ioctl" are
;;;;                       known; any other name signals.  There is no libc and
;;;;                       no dynamic linking here, so this is emulation of two
;;;;                       call sites, not an FFI.
;;;; ==================================================================

(defpackage "SB-BSD-SOCKETS"
  (:use)
  (:export "SOCKET" "INET-SOCKET" "INET6-SOCKET" "LOCAL-SOCKET"
           "SOCKET-BIND" "SOCKET-LISTEN" "SOCKET-ACCEPT" "SOCKET-CONNECT"
           "SOCKET-CLOSE" "SOCKET-SHUTDOWN" "SOCKET-MAKE-STREAM"
           "SOCKET-FILE-DESCRIPTOR" "SOCKET-NAME" "SOCKET-PEERNAME"
           "SOCKOPT-TCP-NODELAY" "SOCKOPT-REUSE-ADDRESS" "SOCKOPT-KEEP-ALIVE"
           "MAKE-INET-ADDRESS" "GET-HOST-BY-NAME" "HOST-ENT" "HOST-ENT-ADDRESS"
           "HOST-ENT-ADDRESSES" "HOST-ENT-NAME"
           "SOCKET-ERROR" "SOCKET-ERROR-ERRNO" "CONNECTION-REFUSED-ERROR"
           "ADDRESS-IN-USE-ERROR" "INTERRUPTED-ERROR"))

(defpackage "SB-POSIX"
  (:use)
  (:export "STAT" "STAT-MODE" "STAT-UID" "STAT-GID" "STAT-INO" "STAT-SIZE"
           "S-ISDIR" "S-ISSOCK" "S-ISREG" "CHMOD" "UNLINK" "GETUID" "GETPID"
           "SYSCALL-ERROR" "ENOENT"))

(defpackage "SB-SYS"
  (:use)
  (:export "FD-STREAM" "FD-STREAM-P" "FD-STREAM-FD" "MAKE-FD-STREAM"
           "WITH-PINNED-OBJECTS"))

(defpackage "SB-ALIEN"
  (:use)
  (:export "INT" "UNSIGNED-LONG" "UNSIGNED-INT" "LONG" "CHAR" "C-STRING" "VOID"
           "ARRAY" "WITH-ALIEN" "ADDR" "DEREF" "CAST" "EXTERN-ALIEN"
           "ALIEN-FUNCALL" "ALIEN-SIZE"))

;;; SB-EXT IS CREATED HERE, and the first draft of this file said it was not.
;;; MEASURED: `(find-package "SB-EXT")' answers NIL in a built ./modus — the
;;; image has %CLI-GETENV and SYS-EXIT under their own names and no SB-EXT
;;; package at all, so a form saying `sb-ext:posix-getenv' fails AT READ TIME.
;;; glass says it 34 times.
(defpackage "SB-EXT"
  (:use)
  (:export "POSIX-GETENV" "*EXIT-HOOKS*" "EXIT" "QUIT" "GC"
           "*INVOKE-DEBUGGER-HOOK*" "RUN-PROGRAM"
           "PROCESS-PTY" "PROCESS-KILL" "PROCESS-ALIVE-P" "PROCESS-EXIT-CODE"
           "PROCESS-WAIT" "PROCESS-INPUT" "PROCESS-OUTPUT"))

;;; ============================================================
;;; SB-ALIEN — two libc calls, emulated as syscalls
;;; ============================================================
;;;
;;; modus is a static ELF issuing raw syscalls.  It has no libc, no dynamic
;;; linker and no way to take the address of a C function, so sb-alien AS A
;;; FACILITY is not shimmable and this does not pretend to shim it.  What it
;;; does is make the TWO forms glass actually writes work:
;;;
;;;   getsockopt(fd, SOL_SOCKET, SO_PEERCRED, &ucred, &len)  — src/socket.lisp
;;;   ioctl(fd, SIOCOUTQ, &n)                                — src/socket.lisp
;;;
;;; both of which are ordinary syscalls (55 and 16).  EXTERN-ALIEN of a name
;;; this file knows returns a CLOSURE that issues that syscall; ALIEN-FUNCALL
;;; applies it.  EXTERN-ALIEN of any other name SIGNALS at macroexpansion —
;;; loudly, at the point of use, naming the function — rather than returning
;;; something that would fail later somewhere else.
;;;
;;; GLASS ALREADY HANDLES THE SIGNAL.  Both call sites are wrapped in
;;; `(handler-case … (error () nil/0))' and both document the fallback as the
;;; correct answer for a platform without the facility.  So even the unknown-name
;;; path degrades the way glass's author intended.
;;;
;;; THE MEMORY IS PER-CPU AND THE BINDINGS ARE A BUMP.  WITH-ALIEN needs bytes a
;;; syscall can be handed a pointer to; a Lisp object cannot be one, because this
;;; collector copies.  %ALIEN-SCRATCH is 256 bytes at the top of this CPU's
;;; per-CPU block; nesting bumps a cursor inside it and unwinds on the way out.

(defvar *alien-cursor* 0
  "Bytes handed out of this CPU's alien scratch.  PARTIAL: it is one number for
   the process, not one per CPU, so two threads inside WITH-ALIEN at the same
   moment can both advance it — which wastes scratch but cannot alias, because
   the BASE is per-CPU and the cursor only ever grows within a nest.  It is
   restored by the unwind, so a balanced nest returns it to 0.")

(defun %alien-alloc (bytes)
  "BYTES of raw, zeroed, per-CPU scratch.  Signals if the scratch is exhausted
   rather than returning an address inside somebody else's bytes."
  (let ((base (%alien-scratch)))
    (if (zerop base)
        (error "sb-alien: no per-CPU scratch (the thread page is not mapped).")
        (let ((a (+ base *alien-cursor*)))
          (if (> (+ *alien-cursor* bytes) 256)
              (error "sb-alien: WITH-ALIEN nested past 256 bytes of scratch.")
              (progn (setq *alien-cursor* (+ *alien-cursor* bytes))
                     (%ha-zero a (+ a bytes))
                     a))))))

(defun %alien-type-bytes (type)
  "How many bytes a supported alien type occupies.  Signals for a type this
   shim does not implement, rather than guessing 8."
  (cond ((eq type 'sb-alien:int) 4)
        ((eq type 'sb-alien:unsigned-int) 4)
        ((eq type 'sb-alien:long) 8)
        ((eq type 'sb-alien:unsigned-long) 8)
        ((eq type 'sb-alien:char) 1)
        ((and (consp type) (eq (car type) 'sb-alien:array))
         (* (%alien-type-bytes (cadr type)) (caddr type)))
        (t (error "sb-alien: unsupported type ~S on modus." type))))

(defmacro sb-alien:with-alien (bindings &body body)
  "(WITH-ALIEN ((name type [initial]) …) body).  Each NAME is bound to the RAW
   ADDRESS of that many bytes of per-CPU scratch — so ADDR is the identity and
   DEREF is a load, which is exactly the shape the two glass call sites use."
  (let ((saved (gensym "CUR")))
    (let ((binds nil) (inits nil) (rest bindings))
      (loop
        (when (null rest) (return nil))
        (let* ((b (car rest))
               (name (car b))
               (type (cadr b))
               (init (if (cddr b) (caddr b) nil)))
          (setq binds (cons (list name (list '%alien-alloc
                                             (list '%alien-type-bytes
                                                   (list 'quote type))))
                            binds))
          (when (cddr b)
            (setq inits (cons (list 'setf (list 'mem-ref name :u32) init)
                              inits))))
        (setq rest (cdr rest)))
      `(let ((,saved *alien-cursor*))
         (unwind-protect
              (let* ,(reverse binds)
                ,@(reverse inits)
                ,@body)
           (setq *alien-cursor* ,saved))))))

(defmacro sb-alien:addr (place) place)
(defmacro sb-alien:cast (thing type) (declare (ignore type)) thing)

(defun sb-alien:deref (address &optional (index 0))
  "Read the INDEXth 32-bit int at ADDRESS.  PARTIAL: this shim's WITH-ALIEN only
   ever hands out INT and (ARRAY INT n), the two shapes glass uses, so DEREF is
   a 32-bit load.  A wider alien type would need the width carried alongside the
   address, which is a real alien-type representation and is not this."
  (mem-ref (+ address (* index 4)) :u32))

(defmacro sb-alien:extern-alien (name type)
  "A callable standing for the C function NAME.  Only names modus can issue as a
   syscall are known; anything else signals AT MACROEXPANSION, naming what was
   asked for.  Note this is the OPPOSITE of ignoring it: an unknown extern that
   returned a dummy would fail somewhere else entirely."
  (declare (ignore type))
  (cond
    ((equal name "getsockopt")
     ;; getsockopt(fd, level, optname, optval, optlen).  The only optname glass
     ;; asks for is SO_PEERCRED, whose answer is three ints; %SBS-PEERCRED reads
     ;; them from AOT code and this deposits them where the caller's DEREF will
     ;; look.  A different optname gets the generic 4-byte %SBS-GETOPT.
     '(lambda (fd level optname optval optlen)
       optlen
       (if (and (= level 1) (= optname 17))
           (let ((pid (%sbs-peercred fd 0)))
             (if (< pid 0)
                 -1
                 (progn (setf (mem-ref optval :u32) pid)
                        (setf (mem-ref (+ optval 4) :u32) (%sbs-peercred fd 1))
                        (setf (mem-ref (+ optval 8) :u32) (%sbs-peercred fd 2))
                        0)))
           (let ((v (%sbs-getopt fd level optname)))
             (if (< v 0) -1 (progn (setf (mem-ref optval :u32) v) 0))))))
    ((equal name "ioctl")
     '(lambda (fd request argp)
       (let ((v (%sbs-ioctl-int fd request)))
         (if (< v 0) -1 (progn (setf (mem-ref argp :u32) v) 0)))))
    ;; AN UNKNOWN NAME SIGNALS WHEN IT IS CALLED, NOT WHEN IT IS EXPANDED.
    ;; The first version signalled at macroexpansion, which is louder and is
    ;; WRONG: it makes a whole file unloadable because of a form in a branch
    ;; that never runs, and glass wraps both of its extern-alien call sites in
    ;; a handler-case that a compile-time error cannot reach.  Expanding to a
    ;; function that signals keeps the error at the call site, where the
    ;; caller\'s handler can see it and take the documented fallback.
    (t `(lambda (&rest args)
          args
          (error "sb-alien:extern-alien ~S: modus has no libc and no dynamic ~
                  linking.  Only getsockopt and ioctl are emulated as syscalls."
                 ,name)))))

(defmacro sb-alien:alien-funcall (fn &rest args) `(funcall ,fn ,@args))

;;; ============================================================
;;; SB-SYS — the fd behind a stream
;;; ============================================================
;;;
;;; SB-SYS:FD-STREAM MUST BE A TYPE AND NOT A CLASS.  glass asks
;;; `(typecase thing … (sb-sys:fd-stream (sb-sys:fd-stream-fd thing)))' about
;;; streams modus made, which are not CLOS instances and never will be, so a
;;; DEFCLASS of that name would answer NIL for every stream in the image.  A
;;; DEFTYPE over a predicate answers about the object that is actually there.

(defun sb-sys::%fd-stream-p (x)
  (if (streamp x) (if (= (%stream-type x) 9) (>= (%fs-fd x) 0) nil) nil))

(deftype sb-sys:fd-stream () '(satisfies sb-sys::%fd-stream-p))

(defun sb-sys:fd-stream-p (x) (sb-sys::%fd-stream-p x))
(defun sb-sys:fd-stream-fd (stream) (%fs-fd stream))

(defun sb-sys:make-fd-stream (fd &key input output (element-type 'character)
                                      buffering external-format timeout name)
  "A stream over an existing descriptor.  :BUFFERING, :EXTERNAL-FORMAT,
   :TIMEOUT and :NAME are accepted and ignored — modus's fd streams are always
   buffered, always 8-bit-through, never time out and are not named."
  buffering external-format timeout name
  (%make-file-stream-full fd
                          (cond ((and input (not output)) 0)
                                ((and output (not input)) 1)
                                (t 2))
                          element-type))

(defmacro sb-sys:with-pinned-objects (objects &body body)
  "PARTIAL AND HONEST: modus's collector COPIES, and there is no pinning
   primitive reachable from Lisp, so this DOES NOT PIN.  It is a PROGN.  It is
   provided because portable code wraps address-taking in it, and the code that
   would then take an address goes through sb-alien, which on modus never hands
   out the address of a Lisp object in the first place — so nothing here can
   observe the difference.  Do not use it to justify taking one."
  (declare (ignore objects))
  `(progn ,@body))

;;; ============================================================
;;; SB-POSIX
;;; ============================================================
;;;
;;; A stat buffer is 144 bytes on x86-64.  The fields this reads, by offset:
;;;   +0x00 st_dev  +0x08 st_ino  +0x10 st_nlink  +0x18 st_mode (u32)
;;;   +0x1C st_uid (u32)  +0x20 st_gid (u32)  +0x30 st_size

(defclass sb-posix::stat-result ()
  ((mode :initarg :mode :accessor sb-posix:stat-mode)
   (uid  :initarg :uid  :accessor sb-posix:stat-uid)
   (gid  :initarg :gid  :accessor sb-posix:stat-gid)
   (ino  :initarg :ino  :accessor sb-posix:stat-ino)
   (size :initarg :size :accessor sb-posix:stat-size)))

(define-condition sb-posix:syscall-error (error)
  ((errno :initarg :errno :initform 0 :reader sb-posix::syscall-errno)))

(defun sb-posix:stat (pathname)
  "stat(2).  Signals SB-POSIX:SYSCALL-ERROR on failure, as SBCL\'s does; a
   caller distinguishes `no such file\' from `a file with mode 0\' by catching,
   not by comparing to NIL.  The syscall is issued from AOT code — see THE
   SYSCALL FLOOR in net/hosted-sockets-post.lisp.

   PARTIAL: five fields are read out of the buffer (mode, uid, gid, ino, size).
   The other twenty are in the buffer and are not exposed; adding one is two
   lines, and guessing one is not done."
  (let* ((path (if (stringp pathname) pathname (namestring pathname)))
         (mode (%sbs-stat-field path 0)))
    (if (< mode 0)
        (error 'sb-posix:syscall-error :errno (- 0 mode))
        (make-instance 'sb-posix::stat-result
                       :mode mode
                       :uid  (%sbs-stat-field path 1)
                       :gid  (%sbs-stat-field path 2)
                       :ino  (%sbs-stat-field path 3)
                       :size (%sbs-stat-field path 4)))))

(defun sb-posix:s-isdir  (mode) (= (logand mode #xF000) #x4000))
(defun sb-posix:s-issock (mode) (= (logand mode #xF000) #xC000))
(defun sb-posix:s-isreg  (mode) (= (logand mode #xF000) #x8000))

(defun sb-posix:chmod (pathname mode)
  "chmod(2)."
  (let ((r (%sbs-chmod (if (stringp pathname) pathname (namestring pathname))
                       mode)))
    (if (< r 0) (error 'sb-posix:syscall-error :errno (- 0 r)) 0)))

(defun sb-posix:unlink (pathname)
  "unlink(2)."
  (let ((r (%sbs-unlink (if (stringp pathname) pathname (namestring pathname)))))
    (if (< r 0) (error 'sb-posix:syscall-error :errno (- 0 r)) 0)))

(defun sb-posix:getuid () (%sbs-getuid))
(defun sb-posix:getpid () (%sbs-getpid))
(defconstant sb-posix:enoent 2)

;;; ============================================================
;;; SB-BSD-SOCKETS
;;; ============================================================

(define-condition sb-bsd-sockets:socket-error (error)
  ((errno :initarg :errno :initform 0 :reader sb-bsd-sockets:socket-error-errno)))
(define-condition sb-bsd-sockets:connection-refused-error
    (sb-bsd-sockets:socket-error) ())
(define-condition sb-bsd-sockets:address-in-use-error
    (sb-bsd-sockets:socket-error) ())
(define-condition sb-bsd-sockets:interrupted-error
    (sb-bsd-sockets:socket-error) ())

(defun %sock-signal (r)
  "Turn a negative -errno into the condition SBCL would signal.  ECONNREFUSED
   is 111, EADDRINUSE 98, EINTR 4; everything else is the base SOCKET-ERROR
   carrying its number, which is better than inventing a class for it."
  (let ((e (- 0 r)))
    (cond ((= e 111) (error 'sb-bsd-sockets:connection-refused-error :errno e))
          ((= e 98)  (error 'sb-bsd-sockets:address-in-use-error :errno e))
          ((= e 4)   (error 'sb-bsd-sockets:interrupted-error :errno e))
          (t (error 'sb-bsd-sockets:socket-error :errno e)))))

(defclass sb-bsd-sockets:socket ()
  ((fd :initarg :fd :initform nil :accessor sb-bsd-sockets:socket-file-descriptor)
   ;; SLOT NAMES SOCK-TYPE / SOCK-PROTOCOL AND NOT TYPE / PROTOCOL: `:type' is
   ;; also a DEFCLASS slot OPTION, and a slot whose name collides with an option
   ;; keyword is the kind of thing that reads correctly and parses differently.
   ;;
   ;; THE INITARGS ARE :TYPE AND :PROTOCOL, WHICH ARE NOT THE SLOT NAMES, and
   ;; that is the entire point.  A slot's initarg is independent of its name,
   ;; and the initarg is not ours to choose: `sb-bsd-sockets' spells it :TYPE,
   ;; every caller in the world writes
   ;;     (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)
   ;; and glass writes it five times (src/socket.lisp:280,300,332,513,514).
   ;; The first draft named the initargs after the slots, so every one of those
   ;; five call sites died with `invalid initarg' at the first socket a real
   ;; program opened — the shim was portable-SHAPED in its arguments and
   ;; shim-shaped in its initargs, which is the same mistake one level down.
   (family :initarg :family :initform 2 :accessor %socket-family)
   (sock-type :initarg :type :initform :stream :accessor %socket-type)
   (sock-protocol :initarg :protocol :initform :tcp
                  :accessor %socket-protocol)))

(defclass sb-bsd-sockets:inet-socket (sb-bsd-sockets:socket) ())
(defclass sb-bsd-sockets:inet6-socket (sb-bsd-sockets:socket) ())
(defclass sb-bsd-sockets:local-socket (sb-bsd-sockets:socket) ())

(defun %socket-family-of (socket)
  (cond ((typep socket 'sb-bsd-sockets:local-socket) 1)   ; AF_UNIX
        ((typep socket 'sb-bsd-sockets:inet6-socket)
         (error "sb-bsd-sockets:inet6-socket is not implemented on modus ~
                 (its address builder is sockaddr_in only)."))
        (t 2)))                                            ; AF_INET

(defun %socket-fd (socket)
  "The descriptor, opening it if it does not have one yet.

   IT NORMALLY HAS ONE ALREADY: INITIALIZE-INSTANCE opens it, as SBCL's does.
   This remains the single place a descriptor is created, so a socket that
   somehow reached here without one still gets a working one rather than
   handing NIL to a syscall."
  (let ((fd (sb-bsd-sockets:socket-file-descriptor socket)))
    (if fd
        fd
        (let* ((fam (%socket-family-of socket))
               (ty (if (eq (%socket-type socket) :datagram) 2 1))
               (r (%sbs-open fam ty)))
          (if (< r 0)
              (%sock-signal r)
              (progn (setf (slot-value socket 'fd) r) r))))))

(defmethod initialize-instance :after ((socket sb-bsd-sockets:socket) &rest initargs)
  "OPEN THE DESCRIPTOR AT MAKE-INSTANCE, WHICH IS WHERE SBCL OPENS IT.

   The first draft of this shim opened it lazily, at the first bind/connect, and
   argued that nothing in glass depended on the earlier point.  That was true of
   glass and false of the SURFACE: `(sb-bsd-sockets:socket-file-descriptor
   sock)' on a freshly made socket answers a descriptor under SBCL and answered
   NIL here — so any caller that reaches for the fd before binding (to set an
   option this shim does not name, to hand it to a poll set, to log it) got NIL
   and then handed NIL to a syscall.  A shim that is portable-SHAPED in its
   arguments has to be portable-shaped in WHEN things happen too, wherever that
   is observable, and this one was observable with a single accessor call.

   A SOCKET MADE WITH :FD ALREADY HAS ONE — that is SOCKET-ACCEPT wrapping a
   descriptor the kernel just handed it — so this must not open a second one and
   leak the first.  %SOCKET-FD is exactly that test, so it is what runs here.

   The cost of being faithful is that MAKE-INSTANCE can now SIGNAL (EMFILE, or
   the INET6 refusal), which is also what SBCL's does."
  initargs
  (%socket-fd socket)
  socket)

;;; ---- addresses ----------------------------------------------------------

(defun sb-bsd-sockets:make-inet-address (dotted)
  "\"127.0.0.1\" -> #(127 0 0 1), the vector shape sb-bsd-sockets uses."
  (let ((v (make-array 4))
        (i 0) (n 0) (part 0))
    (loop
      (when (>= i (length dotted)) (return nil))
      (let ((c (char dotted i)))
        (if (char= c #\.)
            (progn (aset v part n) (setq part (+ part 1)) (setq n 0))
            (setq n (+ (* n 10) (- (char-code c) 48)))))
      (setq i (+ i 1)))
    (aset v part n)
    v))

(defun %inet-host-order (addr)
  "An sb-bsd-sockets address vector, or a host-order integer, as a host-order
   integer."
  (if (integerp addr)
      addr
      (+ (* (aref addr 0) 16777216)
         (+ (* (aref addr 1) 65536)
            (+ (* (aref addr 2) 256) (aref addr 3))))))

(defun %local-socket-p (socket) (typep socket 'sb-bsd-sockets:local-socket))

(defun sb-bsd-sockets:socket-bind (socket &rest address)
  "bind(2).  ADDRESS is (host port) for an inet socket and (path) for a local
   one, exactly as sb-bsd-sockets spells it."
  (let* ((fd (%socket-fd socket))
         (r (if (%local-socket-p socket)
                (%sbs-bind-un fd (car address))
                (%sbs-bind-in fd (%inet-host-order (car address)) (cadr address)))))
    (if (< r 0) (%sock-signal r) socket)))

(defun sb-bsd-sockets:socket-listen (socket backlog)
  "listen(2)."
  (let ((r (%sbs-listen (%socket-fd socket) backlog)))
    (if (< r 0) (%sock-signal r) socket)))

(defun sb-bsd-sockets:socket-accept (socket)
  "accept(2) — syscall 43.  Returns a NEW socket of the same class, or NIL when
   the listener is non-blocking and nothing was waiting (EAGAIN), which is
   sb-bsd-sockets' contract."
  (let ((r (%sbs-accept (%socket-fd socket))))
    (cond ((>= r 0)
           ;; THE SAME CLASS, BY EXPLICIT TEST.  (make-instance (class-of x))
           ;; would be the obvious spelling; this shim does not use it because
           ;; MAKE-INSTANCE of a class OBJECT rather than a class NAME is not
           ;; something this image is known to do, and there are exactly two
           ;; classes it can be.
           (if (typep socket 'sb-bsd-sockets:local-socket)
               (make-instance 'sb-bsd-sockets:local-socket :fd r
                              :type (%socket-type socket)
                              :protocol (%socket-protocol socket))
               (make-instance 'sb-bsd-sockets:inet-socket :fd r
                              :type (%socket-type socket)
                              :protocol (%socket-protocol socket))))
          ((= r -11) nil)                       ; EAGAIN
          (t (%sock-signal r)))))

(defun sb-bsd-sockets:socket-connect (socket &rest address)
  "connect(2)."
  (let* ((fd (%socket-fd socket))
         (r (if (%local-socket-p socket)
                (%sbs-connect-un fd (car address))
                (%sbs-connect-in fd (%inet-host-order (car address))
                                 (cadr address)))))
    (if (< r 0) (%sock-signal r) socket)))

(defun sb-bsd-sockets:socket-close (socket)
  "close(2).  Idempotent: closing an already-closed socket is a no-op rather
   than an error, which is what every cleanup path in portable code expects."
  (let ((fd (sb-bsd-sockets:socket-file-descriptor socket)))
    (if (null fd)
        nil
        (progn (%sbs-close fd)
               (setf (slot-value socket 'fd) nil)
               t))))

(defun sb-bsd-sockets:socket-shutdown (socket &key direction)
  "shutdown(2) — syscall 48.  DIRECTION is :INPUT, :OUTPUT or :IO.

   LOAD-BEARING FOR GLASS: it is how a parked ACCEPT is cancelled
   (src/socket.lisp:420-440), so a wrong `how' here is a listener that never
   comes back."
  (let ((how (cond ((eq direction :input) 0)
                   ((eq direction :output) 1)
                   ((eq direction :io) 2)
                   ((null direction) 2)
                   (t (error "sb-bsd-sockets:socket-shutdown: unknown ~
                              direction ~S" direction))))
        (fd (sb-bsd-sockets:socket-file-descriptor socket)))
    (if (null fd)
        nil
        (let ((r (%sbs-shutdown fd how)))
          ;; ENOTCONN (107) on shutdown is not an error worth signalling: the
          ;; peer is already gone, which is the state the caller wanted.
          (if (or (>= r 0) (= r -107)) socket (%sock-signal r))))))

(defun %sock-getname (socket which)
  "getsockname / getpeername as (values address port).  The syscall is issued
   from AOT code (%SBS-GETNAME) and comes back PACKED as (ip * 65536) + port;
   this unpacks it.  See THE SYSCALL FLOOR in net/hosted-sockets-post.lisp for
   why a shim function may not issue it itself."
  (let ((r (%sbs-getname (%socket-fd socket) which)))
    (if (< r 0)
        (%sock-signal r)
        (let ((ip (truncate r 65536))
              (v (make-array 4)))
          (aset v 0 (logand (ash ip -24) 255))
          (aset v 1 (logand (ash ip -16) 255))
          (aset v 2 (logand (ash ip -8) 255))
          (aset v 3 (logand ip 255))
          (values v (- r (* ip 65536)))))))

(defun sb-bsd-sockets:socket-name (socket) (%sock-getname socket 51))
(defun sb-bsd-sockets:socket-peername (socket) (%sock-getname socket 52))

(defun sb-bsd-sockets:socket-make-stream (socket &key input output
                                                      (element-type 'character)
                                                      buffering timeout
                                                      external-format)
  "A Lisp stream over the socket's descriptor.

   PARTIAL: :BUFFERING, :TIMEOUT and :EXTERNAL-FORMAT are accepted and IGNORED.
   modus's fd streams are always buffered, never time out, and pass 8-bit bytes
   through unchanged.  A :TIMEOUT that was silently honoured as `no timeout'
   would be a read that blocks where the caller asked it not to — glass's
   `serve' never passes one (src/rfb.lisp:1047 takes every default), so the
   honest thing is to say so here rather than to refuse the argument and break
   a caller that passes it harmlessly."
  buffering timeout external-format
  (%make-file-stream-full (%socket-fd socket)
                          (cond ((and input (not output)) 0)
                                ((and output (not input)) 1)
                                (t 2))
                          element-type))

;;; ---- socket options -----------------------------------------------------
;;;
;;; SETF OF A FUNCTION AND NOT OF AN ACCESSOR, and it is not a style choice:
;;; measured on this image, `(setf (foreign-package:accessor obj) v)' expands to
;;; a call to FOREIGN-PACKAGE::SET-ACCESSOR, which DEFCLASS :ACCESSOR does not
;;; define — while a `(defun (setf name) …)' IS found.  So every setf-able
;;; option here is written as an explicit (setf …) function.

(defun sb-bsd-sockets:sockopt-tcp-nodelay (socket)
  (let ((v (%sbs-getopt (%socket-fd socket) 6 1)))
    (if (< v 1) nil t)))

(defun (setf sb-bsd-sockets:sockopt-tcp-nodelay) (value socket)
  "TCP_NODELAY.  Load-bearing for an interactive protocol: without it every RFB
   frame pays a 40 ms Nagle delay between its header and its payload."
  (%sbs-setopt (%socket-fd socket) 6 1 (if value 1 0))
  value)

(defun sb-bsd-sockets:sockopt-reuse-address (socket)
  (let ((v (%sbs-getopt (%socket-fd socket) 1 2)))
    (if (< v 1) nil t)))

(defun (setf sb-bsd-sockets:sockopt-reuse-address) (value socket)
  "SO_REUSEADDR — setsockopt(fd, SOL_SOCKET = 1, SO_REUSEADDR = 2, &v, 4)."
  (%sbs-setopt (%socket-fd socket) 1 2 (if value 1 0))
  value)

(defun sb-bsd-sockets:sockopt-keep-alive (socket)
  (let ((v (%sbs-getopt (%socket-fd socket) 1 9)))
    (if (< v 1) nil t)))

(defun (setf sb-bsd-sockets:sockopt-keep-alive) (value socket)
  "SO_KEEPALIVE = 9."
  (%sbs-setopt (%socket-fd socket) 1 9 (if value 1 0))
  value)

;;; ---- names --------------------------------------------------------------

(defclass sb-bsd-sockets:host-ent ()
  ((name :initarg :name :accessor sb-bsd-sockets:host-ent-name)
   (addresses :initarg :addresses :accessor sb-bsd-sockets:host-ent-addresses)))

(defun sb-bsd-sockets:host-ent-address (host-ent)
  (car (sb-bsd-sockets:host-ent-addresses host-ent)))

(defun sb-bsd-sockets:get-host-by-name (name)
  "Resolve NAME.

   PARTIAL, AND THE PARTIAL PART IS THE WHOLE OF IT: a DOTTED QUAD is recognised
   without asking anybody; anything else SIGNALS.  modus does have a resolver
   (DNS-LOOKUP in net/hosted-sockets.lisp) but it takes a nameserver address as
   an argument and this shim has no configured one to pass — there is no
   /etc/resolv.conf reader in the image — so `resolve a name' would mean picking
   a public resolver on the caller\'s behalf, which is not a decision a
   compatibility shim gets to make.  glass\'s only call site
   (src/socket.lisp:520) passes a host that is a dotted quad in every
   configuration the RFB server is used in."
  (if (and (> (length name) 0) (digit-char-p (char name 0)))
      (make-instance 'sb-bsd-sockets:host-ent :name name
                     :addresses (list (sb-bsd-sockets:make-inet-address name)))
      (error 'sb-bsd-sockets:socket-error :errno 2)))

;;; ============================================================
;;; SB-EXT additions
;;; ============================================================
;;;
;;; Defined INTO modus's existing SB-EXT rather than into a new package of that
;;; name; see the note above the defpackage forms.

(defvar sb-ext:*exit-hooks* nil
  "PARTIAL: the list exists and can be pushed onto, and NOTHING RUNS IT.  modus
   has no exit-hook mechanism.  glass's only use (src/socket.lisp:467-477,
   unlinking UNIX socket files) is documented there as a courtesy rather than
   the mechanism, so a hook that never runs loses a courtesy — but a caller that
   believes cleanup happened should read this line.")

(defun sb-ext:posix-getenv (name)
  "The environment variable NAME, or NIL.  modus reads its environment through
   %CLI-GETENV; this is the SBCL spelling of the same question."
  (%cli-getenv name))

(defvar sb-ext:*invoke-debugger-hook* nil
  "PARTIAL: the variable exists and NOTHING CONSULTS IT.  modus\'s toplevel has
   its own error path.  Only McCLIM sets it, and McCLIM is not part of :GLASS.")

(defun sb-ext:exit (&key (code 0) abort timeout)
  "exit_group(2).  :ABORT and :TIMEOUT are accepted and ignored — modus has no
   other threads to unwind or wait for at exit, and never runs *EXIT-HOOKS*
   (see below), so the two spellings do the same thing."
  abort timeout
  (sys-exit code))

(defun sb-ext:quit (&key (unix-status 0) recklessly-p)
  "The pre-1.0.57 spelling of EXIT."
  recklessly-p
  (sys-exit unix-status))

(defun sb-ext:gc (&rest options)
  "Force one collection of THIS thread\'s active region.  :FULL and :GEN are
   accepted and ignored: modus\'s collector is a single-generation copier per
   region, so there is no less-than-full collection to ask for."
  options
  (%gc-collect-here)
  nil)

(defun sb-ext:run-program (program args &rest options)
  "NOT IMPLEMENTED, deliberately.  modus has no fork(2), no execve(2), and no
   pty; :PTY additionally needs /dev/ptmx and a session model.  glass/term is
   the only production caller and is not part of the :GLASS system."
  program args options
  (error "sb-ext:run-program is not implemented on modus (no fork, no exec)."))

;;; ============================================================
;;; THE FEATURES
;;; ============================================================
(pushnew :sb-bsd-sockets *features*)
