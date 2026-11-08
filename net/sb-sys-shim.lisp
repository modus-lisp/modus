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
;;;;   SB-EXT:RUN-PROGRAM  hosted x86-64 only (net/hosted-process.lisp).  :PTY T,
;;;;                       :WAIT, :ENVIRONMENT, :DIRECTORY, :SEARCH, and NIL / T /
;;;;                       :STREAM / a file for :INPUT :OUTPUT :ERROR.  A Lisp
;;;;                       stream to copy through, :STATUS-HOOK, :PRESERVE-FDS
;;;;                       signal.  No SIGCHLD: status is collected when asked.
;;;;   SB-ALIEN            Only the shape glass uses: WITH-ALIEN over INT and
;;;;                       (ARRAY INT n), ADDR, DEREF, CAST, and ALIEN-FUNCALL
;;;;                       of an EXTERN-ALIEN naming a function modus knows how
;;;;                       to issue as a SYSCALL.  "getsockopt" and "ioctl" are
;;;;                       known; any other name signals.  There is no libc and
;;;;                       no dynamic linking here, so this is emulation of two
;;;;                       call sites, not an FFI.
;;;; ==================================================================

(defpackage "SB-POSIX"
  (:use)
  (:export "STAT" "STAT-MODE" "STAT-UID" "STAT-GID" "STAT-INO" "STAT-SIZE"
           "S-ISDIR" "S-ISSOCK" "S-ISREG" "CHMOD" "UNLINK" "GETUID" "GETPID"
           "SYSCALL-ERROR" "ENOENT"
           "TERMIOS" "TCGETATTR" "TCSETATTR" "TERMIOS-IFLAG" "TERMIOS-OFLAG"
           "TERMIOS-CFLAG" "TERMIOS-LFLAG" "TERMIOS-CC"
           "TCSANOW" "TCSADRAIN" "TCSAFLUSH" "ECHO" "ICANON" "ISIG" "IEXTEN" "ECHOE"
           "ICRNL" "OPOST" "ONLCR" "VMIN" "VTIME" "VINTR" "VEOF"))

(defpackage "SB-SYS"
  (:use)
  (:export "FD-STREAM" "FD-STREAM-P" "FD-STREAM-FD" "MAKE-FD-STREAM"
           "WITH-PINNED-OBJECTS"))

(defpackage "SB-ALIEN"
  (:use)
  (:export "INT" "UNSIGNED-LONG" "UNSIGNED-INT" "LONG" "CHAR" "C-STRING" "VOID"
           "SHORT" "UNSIGNED-SHORT" "UNSIGNED-CHAR"
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
           "PROCESS-WAIT" "PROCESS-INPUT" "PROCESS-OUTPUT" "PROCESS-ERROR"
           "PROCESS-PID" "PROCESS-STATUS" "PROCESS-CLOSE" "PROCESS-P"
           "OCTETS-TO-STRING" "STRING-TO-OCTETS"
           "DOUBLE-FLOAT-POSITIVE-INFINITY" "DOUBLE-FLOAT-NEGATIVE-INFINITY"
           "SINGLE-FLOAT-POSITIVE-INFINITY" "SINGLE-FLOAT-NEGATIVE-INFINITY"
           "SEED-RANDOM-STATE" "SPIN-LOOP-HINT"))

;;; SB-KERNEL and SB-INT: only the float-bits and float-trap names portable numeric code reaches
;;; for (mill reads ONNX tensors with them; ieee-floats-style code does the same).
(defpackage "SB-KERNEL"
  (:use)
  (:export "MAKE-SINGLE-FLOAT" "MAKE-DOUBLE-FLOAT" "SINGLE-FLOAT-BITS"
           "DOUBLE-FLOAT-HIGH-BITS" "DOUBLE-FLOAT-LOW-BITS" "%FUN-NAME"))

(defpackage "SB-INT"
  (:use)
  (:export "WITH-FLOAT-TRAPS-MASKED"))

;;; OCTETS-TO-STRING / STRING-TO-OCTETS — SBCL's byte<->text conversions, for
;;; UTF-8 (the default) and Latin-1.  cassette reads WebM tag strings with the
;;; first.  UTF-8 decoding is lenient, as for library sources (lib/install-
;;; tarball.lisp %IT-UTF8-BYTES-TO-STRING): a malformed byte reads as Latin-1.
(defun sb-ext::octets-to-string (octets &key (external-format :utf-8) (start 0) end)
  (let* ((end (or end (length octets)))
         (bytes (subseq octets start end)))
    (if (member external-format '(:latin-1 :latin1 :iso-8859-1 :ascii))
        (map 'string (function code-char) bytes)
        (%it-utf8-bytes-to-string bytes))))

(defun sb-ext::string-to-octets (string &key (external-format :utf-8) (start 0) end null-terminate)
  (let* ((end (or end (length string)))
         (latin (member external-format '(:latin-1 :latin1 :iso-8859-1 :ascii)))
         (out nil))
    (loop for i from start below end
          do (let ((c (char-code (char string i))))
               (cond ((or latin (< c #x80)) (push (logand c 255) out))
                     ((< c #x800) (push (logior #xC0 (ash c -6)) out)
                                  (push (logior #x80 (logand c #x3F)) out))
                     ((< c #x10000) (push (logior #xE0 (ash c -12)) out)
                                    (push (logior #x80 (logand (ash c -6) #x3F)) out)
                                    (push (logior #x80 (logand c #x3F)) out))
                     (t (push (logior #xF0 (ash c -18)) out)
                        (push (logior #x80 (logand (ash c -12) #x3F)) out)
                        (push (logior #x80 (logand (ash c -6) #x3F)) out)
                        (push (logior #x80 (logand c #x3F)) out)))))
    (when null-terminate (push 0 out))
    (coerce (nreverse out) '(vector (unsigned-byte 8)))))

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
        ((eq type 'sb-alien:short) 2)
        ((eq type 'sb-alien:unsigned-short) 2)
        ((eq type 'sb-alien:unsigned-char) 1)
        ((eq type 'sb-alien:long) 8)
        ((eq type 'sb-alien:unsigned-long) 8)
        ((eq type 'sb-alien:char) 1)
        ((and (consp type) (eq (car type) 'sb-alien:array))
         (* (%alien-type-bytes (cadr type)) (caddr type)))
        (t (error "sb-alien: unsupported type ~S on modus." type))))

(defun %alien-load (a type)
  "The value of the scalar alien TYPE stored at A: INT and LONG sign-extended, as C reads them."
  (cond ((eq type 'sb-alien:int)
         (let ((v (mem-ref a :u32))) (if (>= v #x80000000) (- v #x100000000) v)))
        ((eq type 'sb-alien:unsigned-int) (mem-ref a :u32))
        ((eq type 'sb-alien:long)
         (let ((v (mem-ref a :u64))) (if (>= v #x8000000000000000) (- v #x10000000000000000) v)))
        ((eq type 'sb-alien:unsigned-long) (mem-ref a :u64))
        ((eq type 'sb-alien:char) (let ((v (mem-ref a :u8))) (if (>= v 128) (- v 256) v)))
        ((eq type 'sb-alien:unsigned-char) (mem-ref a :u8))
        ((eq type 'sb-alien:short) (let ((v (mem-ref a :u16))) (if (>= v 32768) (- v 65536) v)))
        ((eq type 'sb-alien:unsigned-short) (mem-ref a :u16))
        (t (error "sb-alien: unsupported scalar type ~S on modus." type))))

(defun %alien-store (a type v)
  (cond ((or (eq type 'sb-alien:int) (eq type 'sb-alien:unsigned-int))
         (setf (mem-ref a :u32) (logand v #xFFFFFFFF)))
        ((or (eq type 'sb-alien:long) (eq type 'sb-alien:unsigned-long))
         (setf (mem-ref a :u64) (logand v #xFFFFFFFFFFFFFFFF)))
        ((or (eq type 'sb-alien:char) (eq type 'sb-alien:unsigned-char))
         (setf (mem-ref a :u8) (logand v #xFF)))
        ((or (eq type 'sb-alien:short) (eq type 'sb-alien:unsigned-short))
         (setf (mem-ref a :u16) (logand v #xFFFF)))
        (t (error "sb-alien: unsupported scalar type ~S on modus." type)))
  v)

(defsetf %alien-load (a type) (v) `(%alien-store ,a ,type ,v))

(defmacro sb-alien:with-alien (bindings &body body)
  "(WITH-ALIEN ((name type [initial]) …) body), with SBCL's meaning: a SCALAR alien
   variable (INT, LONG, …) READS AS ITS VALUE and SETF stores into it, and (ADDR name)
   is the address of its bytes.  An ARRAY variable stands for its address, so DEREF
   with an index is a load from it.

   IT USED TO BIND EVERY NAME TO THE RAW ADDRESS.  That suited this file's own tests,
   which wrote (DEREF n), but SBCL code reads an int variable as plain N -- cl-transport's
   SOCKET-UNSENT-BYTES returns N -- and on modus that N was a pointer: every send queue
   read as ~1.4e14 bytes, so session audio and the microphone judged themselves backed
   up on every send, and a window manager keyed on the backlog would never draw a window."
  (let ((saved (gensym "CUR")) (binds nil) (inits nil) (macros nil) (addrs nil) (arrays nil)
        (rest bindings))
    (loop
      (when (null rest) (return nil))
      (let* ((b (car rest)) (name (car b)) (type (cadr b))
             (scalar (not (and (consp type) (eq (car type) 'sb-alien:array))))
             (a (if scalar (gensym (symbol-name name)) name)))
        (setq binds (cons (list a (list '%alien-alloc (list '%alien-type-bytes (list 'quote type))))
                          binds))
        (if scalar
            (progn
              (setq macros (cons (list name (list '%alien-load a (list 'quote type))) macros))
              (setq addrs (cons (cons name a) addrs)))
            (setq arrays (cons (cons name (cadr type)) arrays)))
        (when (cddr b)
          (setq inits (cons (if scalar
                                (list '%alien-store a (list 'quote type) (caddr b))
                                (list 'setf (list 'mem-ref a :u32) (caddr b)))
                            inits))))
      (setq rest (cdr rest)))
    `(let ((,saved *alien-cursor*))
       (unwind-protect
            (let* ,(reverse binds)
              ,@(reverse inits)
              (macrolet ((sb-alien:addr (place)
                           (let ((hit (assoc place ',addrs)))
                             (if hit (cdr hit) place)))
                         ;; DEREF of an array bound HERE knows its element type: an
                         ;; (ARRAY UNSIGNED-SHORT 4) is read and SETF'd 2 bytes at a
                         ;; time (struct winsize).  Anything else is the 32-bit DEREF.
                         (sb-alien:deref (place &optional (index 0))
                           (let ((hit (assoc place ',arrays)))
                             (if hit
                                 (list '%alien-load
                                       (list '+ place (list '* index (%alien-type-bytes (cdr hit))))
                                       (list 'quote (cdr hit)))
                                 (list '%alien-deref32 place index)))))
                (symbol-macrolet ,(reverse macros)
                  ,@body)))
         (setq *alien-cursor* ,saved)))))

(defmacro sb-alien:addr (place) place)
(defmacro sb-alien:cast (thing type) (declare (ignore type)) thing)

(defun %alien-deref32 (address index) (mem-ref (+ address (* index 4)) :u32))
(defun %alien-set-deref32 (address index v)
  (setf (mem-ref (+ address (* index 4)) :u32) (logand v #xFFFFFFFF))
  v)
(defsetf %alien-deref32 (address index) (v) `(%alien-set-deref32 ,address ,index ,v))
;;; Inside WITH-ALIEN, SETF reaches the local DEREF macro (which knows the element
;;; width) and not this: a local macro shadows a global setf expander.  modus's
;;; SETF did not know that until mvm/compiler.lisp's *MACROLET-NAMES*, and an
;;; (ARRAY UNSIGNED-SHORT 4)'s element 1 was stored 32 bits wide at byte 4.
(defsetf sb-alien:deref (address &optional (index 0)) (v) `(%alien-set-deref32 ,address ,index ,v))

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

(defun sb-sys:wait-until-fd-usable (fd direction &optional timeout serve-events)
  "Wait until FD is ready for DIRECTION (:input or :output), at most TIMEOUT
   seconds (NIL: no limit).  T when ready, NIL on timeout.  Input is poll(2);
   output is reported ready at once (sockets here block on write instead).
   SERVE-EVENTS is accepted and ignored -- there is no serve-event loop."
  serve-events
  (if (eq direction :output)
      t
      (> (%sbs-poll-in fd (if timeout (max 0 (round (* timeout 1000))) -1)) 0)))

(defun sb-sys:make-fd-stream (fd &key input output (element-type 'character)
                                      buffering external-format timeout name)
  "A stream over an existing descriptor.  :EXTERNAL-FORMAT is UTF-8 (the default)
   or :LATIN-1 (a byte per character).  :BUFFERING, :TIMEOUT and :NAME are
   accepted and ignored — modus's fd streams are always buffered, never time out
   and are not named."
  buffering timeout name
  (%make-file-stream-full fd
                          (cond ((and input (not output)) 0)
                                ((and output (not input)) 1)
                                (t 2))
                          element-type
                          external-format))

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
;;; ---- termios, and ioctl on a pointer ---------------------------------------------
;;;
;;; TCGETS / TCSETS{,W,F} (#x5401..#x5404) on the kernel's struct termios: four
;;; u32 flag words (iflag oflag cflag lflag), c_line, then 19 control characters
;;; -- 36 bytes, x86-64 Linux.  A TERMIOS is that, as Lisp values; the ioctls are
;;; AOT (net/hosted-process.lisp, %PROC-IOCTL-BUF), as every syscall here must be.

(defclass sb-posix:termios ()
  ((iflag :initarg :iflag :initform 0 :accessor sb-posix:termios-iflag)
   (oflag :initarg :oflag :initform 0 :accessor sb-posix:termios-oflag)
   (cflag :initarg :cflag :initform 0 :accessor sb-posix:termios-cflag)
   (lflag :initarg :lflag :initform 0 :accessor sb-posix:termios-lflag)
   (line :initarg :line :initform 0 :accessor %termios-line)
   (cc :initarg :cc :initform (make-array 19 :initial-element 0) :accessor sb-posix:termios-cc)))

(defconstant sb-posix:tcsanow 0)
(defconstant sb-posix:tcsadrain 1)
(defconstant sb-posix:tcsaflush 2)
(defconstant sb-posix:isig #o1)
(defconstant sb-posix:icanon #o2)
(defconstant sb-posix:echo #o10)
(defconstant sb-posix:echoe #o20)
(defconstant sb-posix:iexten #o100000)
(defconstant sb-posix:icrnl #o400)
(defconstant sb-posix:opost #o1)
(defconstant sb-posix:onlcr #o4)
(defconstant sb-posix:vintr 0)
(defconstant sb-posix:veof 4)
(defconstant sb-posix:vtime 5)
(defconstant sb-posix:vmin 6)

(defun %posix-fd (fd)
  (cond ((integerp fd) fd)
        ((streamp fd) (sb-sys:fd-stream-fd fd))
        (t (error "sb-posix: ~S is not a file descriptor" fd))))

(defun %termios-u32 (b i)
  (+ (aref b i) (* 256 (aref b (+ i 1))) (* 65536 (aref b (+ i 2))) (* 16777216 (aref b (+ i 3)))))

(defun sb-posix:tcgetattr (fd)
  "tcgetattr(3): FD's terminal settings as a TERMIOS.  Signals SYSCALL-ERROR (ENOTTY
   for something that is not a terminal), as SBCL's does."
  (unless (fboundp '%proc-ioctl-buf)
    (error "sb-posix:tcgetattr: this modus image has no terminal layer (hosted x86-64 only)."))
  (let* ((r (%proc-ioctl-buf (%posix-fd fd) #x5401 36 nil))
         (b (cdr r)))
    (when (< (car r) 0) (error 'sb-posix:syscall-error :errno (- 0 (car r))))
    (let ((cc (make-array 19)))
      (dotimes (i 19) (setf (aref cc i) (aref b (+ 17 i))))
      (make-instance 'sb-posix:termios
                     :iflag (%termios-u32 b 0) :oflag (%termios-u32 b 4)
                     :cflag (%termios-u32 b 8) :lflag (%termios-u32 b 12)
                     :line (aref b 16) :cc cc))))

(defun sb-posix:tcsetattr (fd actions termios)
  "tcsetattr(3): ACTIONS is TCSANOW, TCSADRAIN or TCSAFLUSH."
  (let ((b (make-array 36 :element-type '(unsigned-byte 8) :initial-element 0)))
    (flet ((put (i v) (dotimes (k 4) (setf (aref b (+ i k)) (logand (ash v (* -8 k)) 255)))))
      (put 0 (sb-posix:termios-iflag termios)) (put 4 (sb-posix:termios-oflag termios))
      (put 8 (sb-posix:termios-cflag termios)) (put 12 (sb-posix:termios-lflag termios))
      (setf (aref b 16) (%termios-line termios))
      (dotimes (i 19) (setf (aref b (+ 17 i)) (logand (aref (sb-posix:termios-cc termios) i) 255))))
    (let ((r (car (%proc-ioctl-buf (%posix-fd fd) (+ #x5402 (case actions (1 1) (2 2) (t 0))) 36 b))))
      (if (< r 0) (error 'sb-posix:syscall-error :errno (- 0 r)) 0))))

(defun sb-posix::ioctl-with-pointer-arg (fd request pointer)
  "ioctl(2) with a pointer argument: an address SB-ALIEN handed out (WITH-ALIEN, ADDR).
   SBCL's own non-variadic entry point, which code reaches for because ioctl is variadic
   (see glass's term-pty.lisp)."
  (let ((r (%proc-ioctl-addr (%posix-fd fd) request pointer)))
    (if (< r 0) (error 'sb-posix:syscall-error :errno (- 0 r)) r)))

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
   (see below), so the two spellings do the same thing.  The standard streams
   are flushed first, as SBCL's unwind does (not on :ABORT): output still
   buffered at exit_group(2) is lost."
  timeout
  (unless abort
    (ignore-errors (finish-output *standard-output*))
    (ignore-errors (finish-output *error-output*)))
  (%cli-exit code))

(defun sb-ext:quit (&key (unix-status 0) recklessly-p)
  "The pre-1.0.57 spelling of EXIT."
  (unless recklessly-p
    (ignore-errors (finish-output *standard-output*))
    (ignore-errors (finish-output *error-output*)))
  (%cli-exit unix-status))

(defun sb-ext:gc (&rest options)
  "Force one collection of THIS thread\'s active region.  :FULL and :GEN are
   accepted and ignored: modus\'s collector is a single-generation copier per
   region, so there is no less-than-full collection to ask for."
  options
  (%gc-collect-here)
  nil)

;;; ---- floats as bits (SB-KERNEL), SBCL's spellings ------------------------------------------
;;;
;;; modus boxes every float with an IEEE-64 payload (a single-float is rounded to single
;;; precision but stored wide), so these go through the prelude's converters: %BITS->SINGLE and
;;; %SINGLE->BITS for the 32-bit pattern, %FLOAT-HI32/LO32 for a double's halves.  Signedness is
;;; SBCL's: SINGLE-FLOAT-BITS and DOUBLE-FLOAT-HIGH-BITS are signed 32-bit, the low half unsigned.

(defun %sb-signed32 (u) (if (logbitp 31 u) (- u 4294967296) u))

;;; The name a function reports (backtraces, documentation tools, test
;;; frameworks ask for it): mvm/cl-eval.lisp %FUNCTION-NAME.
(defun sb-kernel:%fun-name (f) (%function-name f))

(defun sb-kernel:make-single-float (bits)
  "The single-float whose IEEE-32 pattern is BITS (a signed or unsigned 32-bit integer)."
  (%bits->single (logand bits 4294967295)))

(defun sb-kernel:single-float-bits (f)
  "F's IEEE-32 pattern as a signed 32-bit integer."
  (%sb-signed32 (%single->bits f)))

(defun sb-kernel:make-double-float (hi lo)
  "The double-float with IEEE-64 high half HI (signed 32-bit) and low half LO (unsigned)."
  (%make-typed-float hi lo 'double-float))

(defun sb-kernel:double-float-high-bits (f)
  (%sb-signed32 (%float-hi32 (if (typep f 'double-float) f (float f 1d0)))))

(defun sb-kernel:double-float-low-bits (f)
  (%float-lo32 (if (typep f 'double-float) f (float f 1d0))))

(defvar sb-ext:double-float-positive-infinity (%make-typed-float #x7FF00000 0 'double-float))
(defvar sb-ext:double-float-negative-infinity (%make-typed-float #xFFF00000 0 'double-float))
(defvar sb-ext:single-float-positive-infinity (%make-typed-float #x7FF00000 0 'single-float))
(defvar sb-ext:single-float-negative-infinity (%make-typed-float #xFFF00000 0 'single-float))

(defmacro sb-int:with-float-traps-masked ((&rest traps) &body body)
  "BODY.  modus runs with every IEEE exception masked (MXCSR is never unmasked), so an overflow
   or a 0/0 already yields the infinity or NaN that masking asks for; TRAPS is accepted and
   ignored."
  (declare (ignore traps))
  `(progn ,@body))

(defun sb-ext:seed-random-state (&optional state)
  "A random state seeded by STATE: an integer seeds it, T or NIL gives a fresh one, a random
   state is copied.  modus's random state is (7770005 seed): see RANDOM-STATE-P."
  (cond ((integerp state) (list 7770005 (logand state #x3FFFFFFF)))
        ((or (null state) (eq state t)) (make-random-state t))
        (t (make-random-state state))))

(defun sb-ext:spin-loop-hint ()
  "A spin-wait's courtesy to the CPU (PAUSE on x86).  Nothing here: correctness never rests on it."
  nil)

;;; ---- RUN-PROGRAM ---------------------------------------------------------------
;;;
;;; SBCL's process interface over net/hosted-process.lisp's fork/exec (hosted
;;; x86-64; an image without it signals, saying so).  What is here:
;;;   PROGRAM        a path; with :SEARCH T, looked up on $PATH as execvp does
;;;   ARGS           strings (argv[0] is PROGRAM, as SBCL makes it)
;;;   :WAIT          T (default): return when it has exited; NIL: at once
;;;   :PTY T         a new pseudo-terminal: the child is a session leader with
;;;                  the slave as controlling terminal and as fds 0-2;
;;;                  PROCESS-PTY is an I/O fd-stream on the master
;;;   :INPUT :OUTPUT :ERROR
;;;                  NIL (/dev/null), T (inherit this process's), :STREAM (a
;;;                  pipe: PROCESS-INPUT / -OUTPUT / -ERROR), a pathname or
;;;                  string (a file; :IF-OUTPUT-EXISTS :APPEND appends);
;;;                  :ERROR :OUTPUT joins stderr to stdout.  Under :PTY,
;;;                  anything left NIL is the pty, as on SBCL.
;;;   :ENVIRONMENT   a list of "NAME=value" (default: this process's)
;;;   :DIRECTORY     the child's working directory
;;;   :EXTERNAL-FORMAT  :LATIN-1 / :UTF-8 for the streams made here
;;; PARTIAL: :PTY as a stream, :INPUT/:OUTPUT as a Lisp stream to copy through,
;;; :STATUS-HOOK and :PRESERVE-FDS signal rather than being dropped.  There is
;;; no SIGCHLD handler: a child's status is collected when asked for
;;; (PROCESS-ALIVE-P, -STATUS, -EXIT-CODE, -WAIT), and a child nobody asks about
;;; stays a zombie until then.  Not a registry the asking could be automatic
;;; from: only the thread that started a child may wait for it, and on modus a
;;; thread cannot keep its own objects anywhere global.

;;; The accessors are %PROCESS-*, and SBCL's names are functions over them: a
;;; :CONC-NAME of SB-EXT::PROCESS- would make the accessor and the exported
;;; function one symbol, and the first draft's (defun sb-ext:process-pid (p)
;;; (sb-ext::process-pid p)) was a function calling itself.
(defstruct (sb-ext::%process (:constructor %make-process) (:predicate sb-ext:process-p)
                             (:conc-name %process-))
  pid pty input output error (status :running) exit-code)

(defun %sb-proc-poll (proc hang)
  "Collect PROC's exit if it has exited (or, with HANG, when it does)."
  (when (eq (%process-status proc) :running)
    (let ((code (%proc-wait (%process-pid proc) hang)))
      (when code
        (setf (%process-exit-code proc) (if (>= code 128) (- code 128) code)
              (%process-status proc) (cond ((< code 0) :exited)
                                                  ((>= code 128) :signaled)
                                                  (t :exited))))))
  proc)

(defun %sb-fd-stream (fd dir external-format)
  (sb-sys:make-fd-stream fd :input (member dir '(:input :io)) :output (member dir '(:output :io))
                            :external-format (or external-format :default)))

(defun sb-ext:run-program (program args &key (wait t) pty input output (error nil error-given)
                                        environment directory search external-format
                                        (if-input-does-not-exist :error)
                                        (if-output-exists :supersede)
                                        (if-error-exists :supersede)
                                        status-hook preserve-fds
                                        &allow-other-keys)
  if-input-does-not-exist
  (unless (fboundp '%proc-spawn-2)
    (error "sb-ext:run-program: this modus image has no process layer (only the hosted ~
            x86-64 image starts programs)."))
  (when (or status-hook preserve-fds (and pty (not (eq pty t))))
    (error "sb-ext:run-program: :STATUS-HOOK, :PRESERVE-FDS and a stream :PTY are not ~
            implemented on modus."))
  (let* ((program (namestring program))
         (path (if search (%proc-find-program program) program))
         (args (mapcar (lambda (a) (if (stringp a) a (princ-to-string a))) args))
         (closes '()) (keep '()) (master nil) (tty nil)
         (in -1) (out -1) (err -1) (in-s nil) (out-s nil) (err-s nil))
    (unless path (error "sb-ext:run-program: ~S not found on PATH" program))
    (labels ((fail (fmt &rest a) (dolist (fd closes) (%proc-close fd))
               (dolist (fd keep) (%proc-close fd))
               (apply (function error) fmt a))
             (open-fd (spec mode)
               (let ((fd (%proc-open (namestring spec) mode)))
                 (when (< fd 0) (fail "sb-ext:run-program: cannot open ~A (errno ~D)" spec (- fd)))
                 (push fd closes) fd))
             (child-fd (spec dir exists)
               ;; (values fd-for-the-child parent-end-of-a-pipe-or-NIL)
               (cond ((eq spec t) (values -1 nil))
                     ((null spec) (if master (values -1 nil)
                                      (values (open-fd "/dev/null" (if (eq dir :input) :input :supersede)) nil)))
                     ((eq spec :stream)
                      (let ((pp (%proc-pipe)))
                        (unless pp (fail "sb-ext:run-program: pipe failed"))
                        (if (eq dir :input)
                            (progn (push (car pp) closes) (push (cdr pp) keep) (values (car pp) (cdr pp)))
                            (progn (push (cdr pp) closes) (push (car pp) keep) (values (cdr pp) (car pp))))))
                     ((or (stringp spec) (pathnamep spec))
                      (values (open-fd spec (cond ((eq dir :input) :input)
                                                  ((eq exists :append) :append)
                                                  (t :supersede)))
                              nil))
                     (t (fail "sb-ext:run-program: ~S as ~(~A~) is not implemented on modus" spec dir)))))
      (when pty
        (let ((pt (%proc-openpt)))
          (unless (consp pt) (fail "sb-ext:run-program: no pseudo-terminal (errno ~D)" (- pt)))
          (setq master (car pt) tty (format nil "/dev/pts/~D" (cdr pt)))
          (push master keep)))
      (multiple-value-bind (fd pe) (child-fd input :input nil)
        (setq in fd) (when pe (setq in-s (%sb-fd-stream pe :output external-format))))
      (multiple-value-bind (fd pe) (child-fd output :output if-output-exists)
        (setq out fd) (when pe (setq out-s (%sb-fd-stream pe :input external-format))))
      (if (and error-given (eq error :output))
          (setq err (if (>= out 0) out 1))
          (multiple-value-bind (fd pe) (child-fd error :error if-error-exists)
            (setq err fd) (when pe (setq err-s (%sb-fd-stream pe :input external-format)))))
      (let ((pid (%proc-spawn-2 path (cons program args) environment
                                (and directory (namestring directory)) in out err tty)))
        ;; The child holds its own copies now; ours would keep pipes open.
        (dolist (fd closes) (%proc-close fd))
        (when (< pid 0)
          (dolist (fd keep) (%proc-close fd))
          (error "sb-ext:run-program: fork failed (errno ~D)" (- pid)))
        (let ((proc (%make-process :pid pid :input in-s :output out-s :error err-s
                                   :pty (and master (%sb-fd-stream master :io external-format)))))
          (when wait (%sb-proc-poll proc t))
          proc)))))

(defun sb-ext:process-pid (p) (%process-pid p))
(defun sb-ext:process-pty (p) (%process-pty p))
(defun sb-ext:process-input (p) (%process-input p))
(defun sb-ext:process-output (p) (%process-output p))
(defun sb-ext:process-error (p) (%process-error p))

(defun sb-ext:process-status (p) (%process-status (%sb-proc-poll p nil)))
(defun sb-ext:process-alive-p (p) (eq (sb-ext:process-status p) :running))
(defun sb-ext:process-exit-code (p) (%process-exit-code (%sb-proc-poll p nil)))

(defun sb-ext:process-wait (p &optional check-for-stopped)
  "Wait for P to exit; P."
  check-for-stopped
  (%sb-proc-poll p t))

(defun sb-ext:process-kill (p signal &optional (whom :pid))
  "Send SIGNAL to P (:PID), or to its process group (:PROCESS-GROUP / :PTY-PROCESS-GROUP:
   a :PTY child leads its own).  T if the kernel took it."
  (and (eq (%process-status p) :running)
       (>= (%proc-kill (if (eq whom :pid) (%process-pid p) (- (%process-pid p)))
                       signal)
           0)))

(defun sb-ext:process-close (p)
  "Close P's streams."
  (dolist (s (list (%process-pty p) (%process-input p)
                   (%process-output p) (%process-error p)))
    (when s (ignore-errors (close s))))
  p)

;;; ============================================================
;;; THE FEATURES
;;; ============================================================

;;; ---- SB-DEBUG: backtraces ---------------------------------------------------------
;;;
;;; The frame chain, named (net/hosted-backtrace.lisp).  Called from a HANDLER-BIND handler
;;; it shows the stack at the error, as on SBCL:
;;;   (handler-bind ((error (lambda (c) (sb-debug:print-backtrace :count 20)))) ...)
;;; An image without the walker (not hosted x86-64) prints nothing rather than failing.

(unless (find-package "SB-DEBUG") (make-package "SB-DEBUG" :use nil))

(defun sb-debug::list-backtrace (&key (count 60) &allow-other-keys)
  (if (fboundp '%backtrace-list) (%backtrace-list count) nil))

(defun sb-debug::print-backtrace (&key (stream *error-output*) (count 60) &allow-other-keys)
  (when (fboundp '%print-backtrace) (%print-backtrace stream count))
  nil)

(defun modus::backtrace (&optional (stream *error-output*) (count 60))
  "Print the stack -- each frame as NAME+OFFSET for code in the image, JIT@ADDR for code
   compiled at run time -- innermost first."
  (sb-debug::print-backtrace :stream stream :count count))

(defun modus::condition-backtrace (&optional condition (stream *error-output*))
  "Print the stack CONDITION came from, innermost first: a hardware fault's (a trap from
   compiled code: a memory fault, a type check, the shared-store guard) always, a signalled
   error's when MODUS:*CAPTURE-BACKTRACES* was on.  With no condition, this thread's last
   fault.  A HANDLER-CASE has unwound by the time its clause runs, so MODUS:BACKTRACE there
   shows the handler's stack; this shows the error's."
  (when (fboundp '%print-fault-backtrace) (%print-fault-backtrace stream condition))
  nil)

(defun modus::condition-backtrace-list (&optional condition)
  (if (fboundp '%fault-backtrace-list) (%fault-backtrace-list condition) nil))

(export (list (intern "PRINT-BACKTRACE" "SB-DEBUG") (intern "LIST-BACKTRACE" "SB-DEBUG")) "SB-DEBUG")
(export (list (intern "BACKTRACE" "MODUS") (intern "CONDITION-BACKTRACE" "MODUS")
              (intern "CONDITION-BACKTRACE-LIST" "MODUS"))
        "MODUS")
;; The switch is HOSTED-BACKTRACE's special; MODUS:*CAPTURE-BACKTRACES* is that symbol.
(when (find-symbol "*CAPTURE-BACKTRACES*" (symbol-package '%backtrace-list))
  (import (find-symbol "*CAPTURE-BACKTRACES*" (symbol-package '%backtrace-list)) "MODUS")
  (export (find-symbol "*CAPTURE-BACKTRACES*" (symbol-package '%backtrace-list)) "MODUS"))
