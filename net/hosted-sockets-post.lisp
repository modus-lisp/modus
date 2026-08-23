;;;; hosted-sockets-post.lisp — the socket layer on TWO CPUs, and a TCP server
;;;; that is multiplexed rather than one-thread-per-connection.
;;;;
;;;; net/hosted-sockets.lisp is arch-neutral and is baked into every hosted
;;;; image.  THIS file is x86-64-hosted only and is baked AFTER
;;;; net/hosted-sync.lisp, which is what lets it use %THR-CPU, SPIN-LOCK,
;;;; %SLEEP-MS and %MONOTONIC-NS — none of which exist on the aarch64 or i386
;;;; CLI.  It overrides the three seam functions
;;;; (%SOCK-ADDR-BUF / %SOCK-IO-BUF / %SOCK-IO-CAP) by last-defun-wins, the
;;;; same mechanism net/hosted-actors-post.lisp uses for SPIN-LOCK, so every
;;;; transfer already written against the seam picks the new buffers up without
;;;; being touched.
;;;;
;;;; ============================================================
;;;; 1. WHY PER-*CPU* BUFFERS AND NOT PER-*CONNECTION* ONES
;;;; ============================================================
;;;;
;;;; The obvious reading of "the shared buffer is a hazard" is that each
;;;; connection should get its own.  That is the wrong partition, and the
;;;; reason is worth writing down because it is what makes the fix small.
;;;;
;;;; THE BOUNCE BUFFER'S LIVE RANGE IS ONE SYSCALL.  Bytes enter it, one
;;;; read(2) or write(2) happens, and they leave.  Nothing is retained across
;;;; a call.  So the only thing that can overlap a buffer's live range is
;;;; ANOTHER CPU executing another call at the same instant — never a second
;;;; connection on the same CPU, because this ISA has no preemption and no
;;;; signal that re-enters Lisp, so a thread is inside exactly one socket call
;;;; at a time.  Partition by the thing that can actually overlap: the CPU.
;;;;
;;;; It is also strictly cheaper.  A per-connection buffer is an allocation per
;;;; fd with an ownership question attached (who frees it, and what happens to
;;;; a buffer whose connection died inside a handler); a per-CPU buffer is a
;;;; base plus a shift of a number the thread already has in a register, and
;;;; there are exactly as many of them as there are CPUs.  It is the same
;;;; answer %THR-TS gives for timespecs and %GC-SCRATCH-CELL gives for the
;;;; collector's per-collection state, for the same reason.
;;;;
;;;; AND IT FIXES A SECOND HAZARD THAT WAS NEVER ABOUT THREADS.  The old
;;;; buffer was *IO-BUF-ADDR*, which is mvm/cl-fileio.lisp's file-I/O staging
;;;; page.  Any program that read a file while a socket transfer was in
;;;; progress corrupted one of them, on ONE CPU.  Moving sockets to their own
;;;; mapping ends that whether or not a second thread ever exists.
;;;;
;;;; SIZE IS NOT THE FIX AND IS NOT PRETENDING TO BE.  Each CPU gets 64 KB
;;;; because that is a reasonable syscall chunk, not because 64 KB is enough
;;;; for anything.  What makes a transfer unbounded is the CHUNKING LOOP in
;;;; net/hosted-sockets.lisp, which is written against %SOCK-IO-CAP and would
;;;; be equally correct at 64 bytes.
;;;;
;;;; ============================================================
;;;; 2. WHY poll(2), AND NOT select / epoll / A THREAD PER CONNECTION
;;;; ============================================================
;;;;
;;;; RECOMMENDATION: poll(2), level-triggered, one poll set per serving CPU,
;;;; with every fd — listener and connections — in O_NONBLOCK.
;;;;
;;;;   * A THREAD PER CONNECTION IS NOT AVAILABLE, not merely undesirable.
;;;;     net/hosted-actors-post.lisp has ONE thread stack (*HA-T2-STACK*), one
;;;;     thread block and one TID word: the image can run exactly TWO OS
;;;;     threads.  A thread-per-connection server would therefore serve one
;;;;     client.  Even with an unbounded clone(2), %RT-ENTER serializes every
;;;;     thread that touches the runtime tables, so threads are not the axis
;;;;     that buys concurrency here.
;;;;   * select(2) needs three fd_set bitmaps that the kernel REWRITES in
;;;;     place, so the set has to be rebuilt from scratch every pass anyway,
;;;;     and it caps out at FD_SETSIZE.  It is poll with more bookkeeping.
;;;;   * epoll is genuinely better at thousands of fds and worse here: it is a
;;;;     kernel object with a lifetime (epoll_create1/epoll_ctl/epoll_wait,
;;;;     three more syscall numbers and a registration protocol), and its
;;;;     edge-triggered mode has a failure mode — a socket you did not drain
;;;;     completely goes quiet forever — that is exactly the bug class this
;;;;     layer cannot afford while it is new.  A compositor serves a handful
;;;;     of viewers, which is the range where poll's O(n) scan is free.
;;;;   * poll(2) IS SYSCALL 7 WITH EXACTLY THREE ARGUMENTS — fds, nfds,
;;;;     timeout_ms — so it rides the `syscall3' trap that is already there.
;;;;     It adds no compiler surface at all: no new opcode, no syscall6, no
;;;;     translator change on any of the nine back-ends.  A struct pollfd is
;;;;     8 bytes and the array is built with the mem-ref stores this file
;;;;     already uses.
;;;;
;;;; The two mechanisms compose rather than compete: each CPU runs its own
;;;; poll set over a DISJOINT set of fds, so the poll loops need no lock
;;;; between them, and the only shared state is the connection table.
;;;;
;;;; ============================================================
;;;; 3. NOTHING LISTENS UNTIL A CALLER SAYS SO
;;;; ============================================================
;;;;
;;;; Loading this file opens no socket.  There is no autostart, no default
;;;; port, no environment variable and no hook.  %SK-SERVER-OPEN is the only
;;;; thing here that ever calls bind(2), it takes the address as an argument
;;;; with no default, and every entry point that reaches it is named for what
;;;; it does.  The address decision is TWO decisions and they have different
;;;; names (net/hosted-sockets.lisp: SOCKET-LISTEN is 127.0.0.1,
;;;; SOCKET-LISTEN-ON is wherever you say), so serving the network shows up in
;;;; a diff instead of being the value of an omitted argument.

;;; ============================================================
;;; THE SOCKET PAGE
;;; ============================================================
;;;
;;; ONE anonymous mapping, addressed from TWO BSS words, mapped on first use.
;;; The same shape as net/hosted-sync.lisp's thread page and for the same
;;; reasons: not the carved actor band, because sockets must work in a
;;; `./modus' that never starts an actor system; and not a Lisp global,
;;; because a global lives in the shared globals hash table, which is the
;;; structure a second thread must not be mutating.
;;;
;;; The two words are 0x10000DF0 and 0x10000DF8 — the last two free words
;;; between the runtime-lock counters (…DE0/…DE8) and the MCGC config block at
;;; 0x10000E00.  A grep of every `#x10000xxx' literal in the tree finds
;;; nothing between 0x10000DE8 and 0x10000E00.
;;;
;;; LAYOUT (offsets from the page base):
;;;   +0x00000  per-CPU ADDRESS scratch, 64 bytes x 16 CPUs
;;;             +0x00 sockaddr_in (16)   +0x10 setsockopt value (4)
;;;             +0x20 socklen_t in/out for getsockname (4)
;;;   +0x00400  per-CPU POLL region, 1024 bytes x 16 CPUs
;;;             +0x000 struct pollfd[64]  (8 bytes each)
;;;             +0x200 slot map[64] — which connection each pollfd entry is
;;;             Entry 63 is RESERVED for the nested "wait until writable"
;;;             poll, so a blocked write never disturbs the loop's own set.
;;;   +0x04400  the SERVER CONTROL BLOCK (see below), 0x1C00 bytes
;;;   +0x06000  per-CPU IO STAGING, 64 KB x 16 CPUs
;;;
;;; MAP_ANONYMOUS is zero-filled and demand-faulted, so the 1 MB of staging
;;; costs one page per CPU that actually moves bytes, not 1 MB of RAM.

(defun %sk-page-slot () #x10000DF0)
(defun %sk-page-lock () #x10000DF8)

(defun %sk-cpus ()      16)
(defun %sk-buf-bytes () 65536)
(defun %sk-data-off ()  #x6000)
(defun %sk-cb-off ()    #x4400)
(defun %sk-page-bytes () 1073152)          ; 0x106000

(defun %sk-page ()
  "Raw byte address of the socket page, mapping it on first use.  0 if the
   mmap failed, in which case every accessor below falls back to the
   single-buffer behaviour net/hosted-sockets.lisp ships — degraded, not
   broken.  Double-checked under a spinlock so two threads racing on the first
   call cannot map two pages and disagree about which is the real one."
  (let ((p (%gc-read64 (%sk-page-slot))))
    (if (> p 0)
        p
        (progn
          (spin-lock (%sk-page-lock))
          (let ((q (%gc-read64 (%sk-page-slot))))
            (if (> q 0)
                (progn (spin-unlock (%sk-page-lock)) q)
                (let ((m (%mmap-shared-page (%sk-page-bytes))))
                  ;; A failed mmap comes back as a small negative (-errno).
                  (if (< m 4096)
                      (progn (spin-unlock (%sk-page-lock)) 0)
                      (progn
                        ;; Only the control area, not the 1 MB of staging: the
                        ;; mapping is already zero and touching it would fault
                        ;; in every page for nothing.
                        (%ha-zero m (+ m (%sk-data-off)))
                        (%gc-write64 (%sk-page-slot) m)
                        (spin-unlock (%sk-page-lock))
                        m)))))))))

(defun %sk-cpu ()
  "This thread's CPU id, clamped to the table.  0 when per-CPU storage has
   never been switched on, which is exactly right: a single-CPU image is CPU 0
   and gets CPU 0's buffers."
  (let ((c (%thr-cpu)))
    (if (>= c (%sk-cpus)) (- (%sk-cpus) 1) c)))

(defun %sk-cb ()
  "The server control block, or 0 if the page could not be mapped."
  (let ((p (%sk-page)))
    (if (zerop p) 0 (+ p (%sk-cb-off)))))

;;; ---- THE NEGATIVE CONTROL, in the same binary --------------------------
;;;
;;; Word CB+0xF0.  0 = per-CPU buffers (the shipping path).  1 = every CPU
;;; copies through the ONE page again, which is this whole file removed.  It
;;; exists so "the shared buffer was really a hazard" can be DEMONSTRATED on
;;; the same workload rather than argued from the source, the same way
;;; test/hosted-thread-lisp-unsync.lisp is the control for the runtime lock.
;;;
;;; The fallback addresses are CACHED into the control block by the driver
;;; thread (%SK-CACHE-FALLBACKS) rather than read from *IO-BUF-ADDR* at use
;;; time, because reading a Lisp global goes through SYMBOL-VALUE and the
;;; shared globals table, which is precisely what a second thread must not
;;; touch on a hot path.

(defun %sk-shared-mode ()
  (let ((c (%sk-cb))) (if (zerop c) 1 (%gc-read64 (+ c #xF0)))))

(defun %sk-set-shared-mode (v)
  (let ((c (%sk-cb))) (if (zerop c) 0 (progn (%gc-write64 (+ c #xF0) v) v))))

(defun %sk-cache-fallbacks ()
  "Record the single-buffer addresses in the control block.  DRIVER THREAD
   ONLY — it reads two Lisp globals."
  (let ((c (%sk-cb)))
    (if (zerop c)
        0
        (progn
          (%gc-write64 (+ c #xF8) *io-buf-addr*)
          (%gc-write64 (+ c #x100) (+ *cstr-scratch* 3072))
          1))))

(defun %sk-fallback-io ()
  (let ((c (%sk-cb)))
    (let ((v (if (zerop c) 0 (%gc-read64 (+ c #xF8)))))
      (if (> v 0) v *io-buf-addr*))))

(defun %sk-fallback-addr ()
  (let ((c (%sk-cb)))
    (let ((v (if (zerop c) 0 (%gc-read64 (+ c #x100)))))
      (if (> v 0) v (+ *cstr-scratch* 3072)))))

;;; ---- THE SEAM, OVERRIDDEN ----------------------------------------------

(defun %sock-addr-buf ()
  (let ((p (%sk-page)))
    (if (zerop p)
        (+ *cstr-scratch* 3072)
        (if (zerop (%gc-read64 (+ (+ p (%sk-cb-off)) #xF0)))
            (+ p (* 64 (%sk-cpu)))
            (%sk-fallback-addr)))))

(defun %sock-io-buf ()
  (let ((p (%sk-page)))
    (if (zerop p)
        *io-buf-addr*
        (if (zerop (%gc-read64 (+ (+ p (%sk-cb-off)) #xF0)))
            (+ p (+ (%sk-data-off) (* (%sk-buf-bytes) (%sk-cpu))))
            (%sk-fallback-io)))))

(defun %sock-io-cap ()
  (let ((p (%sk-page)))
    (if (zerop p)
        4096
        (if (zerop (%gc-read64 (+ (+ p (%sk-cb-off)) #xF0)))
            (%sk-buf-bytes)
            4096))))

;;; ============================================================
;;; NON-BLOCKING FDS AND SOCKET OPTIONS
;;; ============================================================

;;; O_NONBLOCK (04000 = 2048) via fcntl(72): F_GETFL = 3, F_SETFL = 4.  Read
;;; the flags rather than assuming them, because clearing a flag the kernel set
;;; (O_RDWR, O_CLOEXEC) would be a different bug every time.
(defun socket-set-nonblock (fd)
  (let ((fl (syscall3 72 fd 3 0)))
    (if (< fl 0)
        fl
        (if (zerop (logand fl 2048)) (syscall3 72 fd 4 (+ fl 2048)) 0))))

(defun socket-set-block (fd)
  (let ((fl (syscall3 72 fd 3 0)))
    (if (< fl 0)
        fl
        (if (zerop (logand fl 2048)) 0 (syscall3 72 fd 4 (- fl 2048))))))

;;; TCP_NODELAY: setsockopt(fd, IPPROTO_TCP = 6, TCP_NODELAY = 1, &1, 4).
;;; Five arguments, so syscall6 with a6 ignored.  Glass sets this on every
;;; accepted RFB connection; an interactive protocol that writes a small header
;;; and then a payload pays a 40 ms Nagle delay per frame without it.
(defun socket-set-nodelay (fd)
  (let ((opt (+ (%sock-addr-buf) 16)))
    (setf (mem-ref opt :u32) 1)
    (syscall6 54 fd 6 1 opt 4 0)))

;;; accept(2) WITHOUT the -1 flattening.  socket-accept maps every failure to
;;; -1, which throws away the one distinction a poll loop needs: -11 (EAGAIN)
;;; means "nothing was waiting, come back later" and every other negative means
;;; the listener is in trouble.
(defun socket-accept-raw (lfd) (syscall3 43 lfd 0 0))

;;; ============================================================
;;; poll(2)
;;; ============================================================
;;;
;;; struct pollfd { int fd; short events; short revents; } — 8 bytes.
;;; POLLIN 1, POLLOUT 4, POLLERR 8, POLLHUP 16, POLLNVAL 32.

(defun %sk-pollin ()  1)
(defun %sk-pollout () 4)
(defun %sk-pollbad () 56)                  ; POLLERR | POLLHUP | POLLNVAL
(defun %sk-poll-max () 63)                 ; entry 63 is the write-wait slot

(defun %sk-pollfds ()
  (let ((p (%sk-page)))
    (if (zerop p) 0 (+ p (+ #x400 (* 1024 (%sk-cpu)))))))

(defun %sk-slotmap-at (i) (+ (+ (%sk-pollfds) #x200) (* i 8)))

(defun %sk-poll-set (i fd events)
  (let ((a (+ (%sk-pollfds) (* i 8))))
    (setf (mem-ref a :u32) fd)
    (setf (mem-ref (+ a 4) :u16) events)
    (setf (mem-ref (+ a 6) :u16) 0)
    0))

(defun %sk-poll-revents (i)
  (mem-ref (+ (+ (%sk-pollfds) (* i 8)) 6) :u16))

;;; poll(fds, nfds, timeout_ms).  Returns the number of ready fds, 0 on
;;; timeout, or a negative -errno.
(defun %sk-poll (n ms) (syscall3 7 (%sk-pollfds) n ms))

;;; Wait until FD accepts a write, using the RESERVED entry 63 so the caller's
;;; own poll set is untouched.  Passing the address of entry 63 with nfds = 1
;;; means the kernel looks at that one struct and nothing else.
(defun %sk-wait-writable (fd ms)
  (let ((a (+ (%sk-pollfds) (* 63 8))))
    (setf (mem-ref a :u32) fd)
    (setf (mem-ref (+ a 4) :u16) 4)
    (setf (mem-ref (+ a 6) :u16) 0)
    (syscall3 7 a 1 ms)))

;;; Wait for FD to become readable — the one-fd case, for a client that has no
;;; poll set of its own.  Returns 1 if it is readable, 0 on timeout, negative
;;; on error.
(defun socket-wait-readable (fd ms)
  (let ((a (+ (%sk-pollfds) (* 63 8))))
    (if (zerop (%sk-pollfds))
        1
        (progn
          (setf (mem-ref a :u32) fd)
          (setf (mem-ref (+ a 4) :u16) 1)
          (setf (mem-ref (+ a 6) :u16) 0)
          (syscall3 7 a 1 ms)))))

;;; ============================================================
;;; THE SERVER CONTROL BLOCK
;;; ============================================================
;;;
;;;   CB+0x000 listening fd            CB+0x080 short writes
;;;   CB+0x008 bound port              CB+0x088 cpu0 poll returns
;;;   CB+0x010 stop flag               CB+0x090 cpu1 poll returns
;;;   CB+0x018 connections accepted    CB+0x098 cpu0 read events
;;;   CB+0x020 connections closed      CB+0x0A0 cpu1 read events
;;;   CB+0x028 service steps           CB+0x0A8 cpu0 bytes echoed
;;;   CB+0x030 handlers INSIDE now     CB+0x0B0 cpu1 bytes echoed
;;;   CB+0x038 max handlers at once    CB+0x0B8 cpu0 connections closed
;;;   CB+0x040 OVERLAP WITNESSES       CB+0x0C0 cpu1 connections closed
;;;   CB+0x048 the table lock word     CB+0x0C8 (unused)
;;;   CB+0x050 interleave-log index    CB+0x0D0 echo write errors
;;;   CB+0x058 accept errors           CB+0x0D8 max slots in use
;;;   CB+0x060 thread 2 started        CB+0x0E0 thread 2's own CPU id
;;;   CB+0x068 thread 2 finished       CB+0x0E8 per-event spin (widens the race)
;;;   CB+0x070 deadline, monotonic ns  CB+0x0F0 SHARED-BUFFER MODE (control)
;;;   CB+0x078 stop after N closed     CB+0x0F8 cached *io-buf-addr*
;;;   CB+0x100 cached sockaddr scratch
;;;   CB+0x108 slots in use
;;;   CB+0x110 poll errors
;;;   CB+0x118 WRONG-CPU SERVICES — must be 0
;;;   CB+0x120 number of serving CPUs (1 or 2)
;;;   CB+0x128 EAGAIN reads (poll said ready, read had nothing)
;;;   CB+0x130 write-wait rounds (send queue was full)
;;;   CB+0x200 CONNECTION TABLE — 32 slots x 32 bytes
;;;              +0x00 fd (0 = free; written LAST, so it is the ready flag)
;;;              +0x08 owner CPU        +0x10 bytes echoed   +0x18 events
;;;   CB+0x600 INTERLEAVE LOG — 256 entries x 16 bytes
;;;              +0x00 cpu + 1 (0 = empty)   +0x08 slot

(defun %sk-slots () 32)
(defun %sk-log-cap () 256)
(defun %sk-slot (i) (+ (%sk-cb) (+ #x200 (* i 32))))
(defun %sk-log (i)  (+ (%sk-cb) (+ #x600 (* i 16))))
(defun %sk-lock ()  (+ (%sk-cb) #x48))
(defun %sk-stat (off) (let ((c (%sk-cb))) (if (zerop c) 0 (%gc-read64 (+ c off)))))

;;; Bump a control-block counter.  CALL ONLY WITH %SK-LOCK HELD: this ISA has
;;; an unconditional atomic exchange and no atomic add, so a counter is
;;; ordinary protected code, exactly as net/hosted-sync.lisp's condvar
;;; sequence number is.
(defun %sk-bump (off n)
  (let ((c (%sk-cb)))
    (%gc-write64 (+ c off) (+ (%gc-read64 (+ c off)) n))
    0))

(defun %sk-locked-bump (off n)
  (spin-lock (%sk-lock))
  (%sk-bump off n)
  (spin-unlock (%sk-lock))
  0)

;;; A bounded arithmetic delay.  Not a sleep: the point is to hold bytes IN
;;; the staging buffer while the other CPU is also inside its handler, so a
;;; SHARED buffer really does corrupt instead of merely being able to.
(defun %sk-spin (n)
  (let ((i 0) (s 0))
    (loop
      (when (>= i n) (return s))
      (setq s (+ s i))
      (setq i (+ i 1)))))

;;; ============================================================
;;; THE CONNECTION TABLE
;;; ============================================================

;;; Publish FD into a free slot.  Returns the slot, or -1 if the table is full
;;; (the caller closes FD; refusing a connection is not a leak).
;;;
;;; ORDERING: the counters and the owner are written BEFORE the fd, and the fd
;;; is the ready flag.  A serving thread reading the table without the lock
;;; therefore sees either 0 or a fully-formed entry — x86-64 does not reorder
;;; stores, and the reader only ever reads fields of an entry whose fd it has
;;; already seen non-zero.
(defun %sk-assign (fd)
  (let ((c (%sk-cb)) (i 0) (r -1))
    (spin-lock (%sk-lock))
    (loop
      (when (>= i (%sk-slots)) (return nil))
      (when (zerop (%gc-read64 (%sk-slot i))) (setq r i) (return nil))
      (setq i (+ i 1)))
    (when (>= r 0)
      (let ((s (%sk-slot r))
            (ncpu (%gc-read64 (+ c #x120)))
            (n (%gc-read64 (+ c #x18))))
        (%gc-write64 (+ s #x10) 0)
        (%gc-write64 (+ s #x18) 0)
        ;; Round-robin the owner so both CPUs get work.  With one serving CPU
        ;; this is always 0, which is what makes the same loop correct
        ;; single-threaded.
        (%gc-write64 (+ s #x08) (if (< ncpu 2) 0 (logand n 1)))
        (%gc-write64 s fd)
        (%sk-bump #x18 1)
        (%sk-bump #x108 1)
        (when (> (%gc-read64 (+ c #x108)) (%gc-read64 (+ c #xD8)))
          (%gc-write64 (+ c #xD8) (%gc-read64 (+ c #x108))))))
    (spin-unlock (%sk-lock))
    r))

;;; Close SLOT's fd and free the slot.  Idempotent: a slot whose fd is already
;;; 0 is left alone, so two paths racing to retire the same connection close
;;; the fd exactly once.
(defun %sk-close-slot (cpu slot)
  (let ((c (%sk-cb)) (s (%sk-slot slot)) (fd 0))
    (spin-lock (%sk-lock))
    (setq fd (%gc-read64 s))
    (when (> fd 0)
      (%gc-write64 s 0)
      (%sk-bump #x20 1)
      (%sk-bump (if (zerop cpu) #xB8 #xC0) 1)
      (%gc-write64 (+ c #x108) (- (%gc-read64 (+ c #x108)) 1)))
    (spin-unlock (%sk-lock))
    (when (> fd 0) (socket-close fd))
    0))

;;; ============================================================
;;; THE HANDLER
;;; ============================================================

;;; Write N bytes from THIS CPU's staging buffer to a non-blocking FD, waiting
;;; on POLLOUT whenever the send queue fills.  Returns the byte count, or a
;;; negative -errno if nothing went out.  BUDGET bounds the number of POLLOUT
;;; waits so a peer that stops reading cannot wedge a serving thread forever.
(defun %sk-write-all (fd n budget)
  (let ((io (%sock-io-buf)) (sent 0) (bad 0) (waits 0))
    (loop
      (when (>= sent n) (return nil))
      (let ((w (syscall3 1 fd (+ io sent) (- n sent))))
        (if (< w 0)
            (if (= w -11)
                (progn
                  (setq waits (+ waits 1))
                  (when (> waits budget) (setq bad w) (return nil))
                  (%sk-wait-writable fd 50))
                (progn (setq bad w) (return nil)))
            (if (zerop w)
                (progn (setq bad -32) (return nil))     ; -EPIPE-ish: no progress
                (setq sent (+ sent w))))))
    (when (> waits 0) (%sk-locked-bump #x130 waits))
    (if (> sent 0) sent bad)))

;;; One readable event on SLOT, serviced by CPU.  Reads what is there into
;;; THIS CPU's staging buffer and writes it straight back out — no Lisp array
;;; is involved at all, which is deliberate: it means the handler allocates
;;; nothing (so a second thread with a borrowed allocation pointer cannot
;;; double-hand-out), and it means the ONLY thing standing between the two
;;; threads' bytes is the per-CPU staging buffer this file installs.  Turn the
;;; shared-buffer control on and this same code corrupts.
;;;
;;; Returns 1 if the connection is still open, 0 if it has been retired.
(defun %sk-service (cpu slot)
  (let ((c (%sk-cb)) (s (%sk-slot slot)) (fd 0) (alive 1))
    (setq fd (%gc-read64 s))
    (if (zerop fd)
        0
        (progn
          ;; ---- entry: the overlap witness, under the lock ----------------
          (spin-lock (%sk-lock))
          (%sk-bump #x30 1)
          (let ((live (%gc-read64 (+ c #x30))))
            (when (> live (%gc-read64 (+ c #x38))) (%gc-write64 (+ c #x38) live))
            (when (> live 1) (%sk-bump #x40 1)))
          (%sk-bump #x28 1)
          (%sk-bump (if (zerop cpu) #x98 #xA0) 1)
          (when (> (- (%gc-read64 (+ s #x08)) cpu) 0) (%sk-bump #x118 1))
          (when (< (- (%gc-read64 (+ s #x08)) cpu) 0) (%sk-bump #x118 1))
          (let ((li (%gc-read64 (+ c #x50))))
            (when (< li (%sk-log-cap))
              (%gc-write64 (%sk-log li) (+ cpu 1))
              (%gc-write64 (+ (%sk-log li) 8) slot)
              (%gc-write64 (+ c #x50) (+ li 1))))
          (spin-unlock (%sk-lock))
          ;; ---- the work ---------------------------------------------------
          (let ((n (%sock-read fd (%sock-io-cap))))
            (if (< n 1)
                (if (= n -11)
                    (%sk-locked-bump #x128 1)       ; spurious wakeup, still alive
                    (setq alive 0))                 ; 0 = peer closed; <0 = broken
                (progn
                  ;; Hold the bytes in the buffer while the other CPU is also
                  ;; inside its handler.  This is what makes the negative
                  ;; control fail rather than merely be able to.
                  (%sk-spin (%gc-read64 (+ c #xE8)))
                  (let ((w (%sk-write-all fd n 200)))
                    (spin-lock (%sk-lock))
                    (when (< w n) (%sk-bump #x80 1))
                    (when (< w 0) (%sk-bump #xD0 1))
                    (when (> w 0)
                      (%sk-bump (if (zerop cpu) #xA8 #xB0) w)
                      (%gc-write64 (+ s #x10) (+ (%gc-read64 (+ s #x10)) w)))
                    (%gc-write64 (+ s #x18) (+ (%gc-read64 (+ s #x18)) 1))
                    (spin-unlock (%sk-lock))
                    (when (< w 0) (setq alive 0))))))
          ;; ---- exit -------------------------------------------------------
          (spin-lock (%sk-lock))
          (%gc-write64 (+ c #x30) (- (%gc-read64 (+ c #x30)) 1))
          (spin-unlock (%sk-lock))
          (when (zerop alive) (%sk-close-slot cpu slot))
          alive))))

;;; Drain the listener: accept until EAGAIN.  Every accepted fd is put in
;;; O_NONBLOCK and TCP_NODELAY BEFORE it is published to a slot, so a serving
;;; thread never sees a blocking fd.  GUARD bounds one drain so a flood cannot
;;; starve the connections already in the poll set.
(defun %sk-accept-ready ()
  (let ((c (%sk-cb)) (guard 0))
    (loop
      (when (> guard 64) (return nil))
      (setq guard (+ guard 1))
      (let ((fd (socket-accept-raw (%gc-read64 c))))
        (if (< fd 0)
            (progn
              (when (> fd -11) (%sk-locked-bump #x58 1))
              (return nil))
            (progn
              (socket-set-nonblock fd)
              (socket-set-nodelay fd)
              (when (< (%sk-assign fd) 0) (socket-close fd))))))
    0))

;;; ============================================================
;;; THE POLL LOOP
;;; ============================================================

;;; Fill this CPU's pollfd array from the connection table.  On CPU 0 entry 0
;;; is the listener, marked in the slot map with the out-of-range index
;;; (%SK-SLOTS).  Returns the number of entries.
(defun %sk-build-poll (cpu)
  (let ((c (%sk-cb)) (n 0) (i 0))
    (when (zerop cpu)
      (let ((l (%gc-read64 c)))
        (when (> l 0)
          (%sk-poll-set 0 l (%sk-pollin))
          (%gc-write64 (%sk-slotmap-at 0) (%sk-slots))
          (setq n 1))))
    (loop
      (when (>= i (%sk-slots)) (return nil))
      (when (< n (%sk-poll-max))
        (let ((s (%sk-slot i)))
          (let ((fd (%gc-read64 s)))
            (when (> fd 0)
              (when (= (%gc-read64 (+ s #x08)) cpu)
                (%sk-poll-set n fd (%sk-pollin))
                (%gc-write64 (%sk-slotmap-at n) i)
                (setq n (+ n 1)))))))
      (setq i (+ i 1)))
    n))

(defun %sk-dispatch (cpu n)
  (let ((i 0))
    (loop
      (when (>= i n) (return nil))
      (let ((re (%sk-poll-revents i)))
        (when (> re 0)
          (let ((slot (%gc-read64 (%sk-slotmap-at i))))
            (if (= slot (%sk-slots))
                (%sk-accept-ready)
                ;; POLLIN first, ALWAYS: a peer that sent data and then closed
                ;; arrives as POLLIN|POLLHUP in one revents, and closing on the
                ;; HUP without reading would drop the last message.  %SK-SERVICE
                ;; retires the slot itself when the read comes back 0.
                (if (> (logand re (%sk-pollin)) 0)
                    (%sk-service cpu slot)
                    (when (> (logand re (%sk-pollbad)) 0)
                      (%sk-close-slot cpu slot)))))))
      (setq i (+ i 1)))
    0))

;;; The server is done when it has been told to stop, or its deadline passed,
;;; or it has retired the number of connections it was told to expect AND none
;;; is still open.  The deadline is not a nicety: it is what guarantees that a
;;; test which fails to connect ends by itself rather than leaving a listener
;;; behind.
(defun %sk-stop-p ()
  (let ((c (%sk-cb)))
    (if (zerop c)
        t
        (if (> (%gc-read64 (+ c #x10)) 0)
            t
            (if (>= (%monotonic-ns) (%gc-read64 (+ c #x70)))
                t
                (let ((want (%gc-read64 (+ c #x78))))
                  (if (zerop want)
                      nil
                      (if (>= (%gc-read64 (+ c #x20)) want)
                          (zerop (%gc-read64 (+ c #x108)))
                          nil))))))))

;;; One serving CPU's whole life: rebuild the poll set, poll, dispatch.
;;; Level-triggered, so a connection this pass did not fully drain simply
;;; comes back ready next pass — the property that makes this safe to get
;;; slightly wrong, which epoll's edge mode does not have.
(defun %sk-serve (cpu)
  (let ((c (%sk-cb)))
    (if (zerop c)
        0
        (progn
          (loop
            (when (%sk-stop-p) (return nil))
            (let ((n (%sk-build-poll cpu)))
              (if (zerop n)
                  (%sleep-ms 2)
                  (let ((r (%sk-poll n 20)))
                    (if (< r 0)
                        (%sk-locked-bump #x110 1)
                        (progn
                          (%sk-locked-bump (if (zerop cpu) #x88 #x90) 1)
                          (when (> r 0) (%sk-dispatch cpu n))))))))
          0))))

;;; ============================================================
;;; START AND STOP — THE ONLY PLACES THAT BIND(2)
;;; ============================================================

;;; Open the listener and arm the control block.  IP is a host-order address
;;; and HAS NO DEFAULT — pass (%SOCK-LOOPBACK) for a server this machine can
;;; reach and nothing else, or an address you wrote down on purpose for one
;;; the network can.  PORT 0 asks the kernel to choose.  SECONDS is the
;;; deadline; EXPECT, when non-zero, additionally stops the server once it has
;;; retired that many connections.  NCPU is 1 or 2.
;;;
;;; Returns the bound port, or -1.  THE CALLER OWNS THE LISTENER FROM HERE:
;;; %SK-SERVER-CLOSE is what gives it back, and every path in this file that
;;; can fail closes what it opened before returning.
(defun %sk-server-open (ip port backlog seconds expect ncpu spin)
  (let ((c (%sk-cb)))
    (if (zerop c)
        -1
        (progn
          (%ha-zero c (+ c #x1600))
          (%sk-cache-fallbacks)
          (let ((lfd (socket-listen-on ip port backlog)))
            (if (< lfd 0)
                -1
                (let ((bound (socket-local-port lfd)))
                  (if (< bound 0)
                      (progn (socket-close lfd) -1)
                      (progn
                        (socket-set-nonblock lfd)
                        (%gc-write64 (+ c #x08) bound)
                        (%gc-write64 (+ c #x70)
                                     (+ (%monotonic-ns) (* seconds 1000000000)))
                        (%gc-write64 (+ c #x78) expect)
                        (%gc-write64 (+ c #x120) ncpu)
                        (%gc-write64 (+ c #xE8) spin)
                        ;; The fd goes in LAST: while it is 0 the poll loop
                        ;; simply has no listener, so an incomplete control
                        ;; block is never served from.
                        (%gc-write64 c lfd)
                        bound)))))))))

;;; Ask every serving loop to wind up.  Cooperative — the loops check it once
;;; per poll timeout — so a caller that needs the fds gone must also join.
(defun %sk-server-stop ()
  (let ((c (%sk-cb))) (if (zerop c) 0 (progn (%gc-write64 (+ c #x10) 1) 1))))

;;; TEARDOWN.  Closes the listener first (so nothing new arrives) and then
;;; every connection still in the table, and returns the number of fds it
;;; closed.  A server that ends any other way — deadline, EXPECT, an error —
;;; still ends here, because every entry point below calls it on the way out.
(defun %sk-server-close ()
  (let ((c (%sk-cb)) (n 0) (i 0))
    (if (zerop c)
        0
        (progn
          (let ((l (%gc-read64 c)))
            (when (> l 0)
              (%gc-write64 c 0)
              (socket-close l)
              (setq n 1)))
          (loop
            (when (>= i (%sk-slots)) (return nil))
            (when (> (%gc-read64 (%sk-slot i)) 0)
              (%sk-close-slot 0 i)
              (setq n (+ n 1)))
            (setq i (+ i 1)))
          n))))

;;; ---- A SERVER YOU CAN ACTUALLY CALL ------------------------------------
;;;
;;; One call, one thread, poll-multiplexed over as many concurrent connections
;;; as the table holds, echoing every byte back.  It binds 127.0.0.1 and it
;;; ends at the deadline whatever happens, closing the listener and every
;;; connection.  Returns the bytes echoed, or -1 if it could not start.
;;;
;;; It is here because the primitives above are only worth something if the
;;; shortest path to a working server is short.  Nothing calls it.
(defun socket-echo-server (port seconds)
  (let ((bound (%sk-server-open (%sock-loopback) port 16 seconds 0 1 0)))
    (if (< bound 0)
        -1
        (progn
          (%sk-serve 0)
          (%sk-server-close)
          (+ (%sk-stat #xA8) (%sk-stat #xB0))))))

;;; The same thing on the address the caller names.  A SEPARATE FUNCTION, so
;;; that serving the network is a thing somebody wrote rather than an argument
;;; somebody left off.
(defun socket-echo-server-on (ip port seconds)
  (let ((bound (%sk-server-open ip port 16 seconds 0 1 0)))
    (if (< bound 0)
        -1
        (progn
          (%sk-serve 0)
          (%sk-server-close)
          (+ (%sk-stat #xA8) (%sk-stat #xB0))))))

;;; ============================================================
;;; THE SECOND SERVING THREAD
;;; ============================================================

(defun %sk-t2-body ()
  "THREAD 2.  Its own GS base, its own CPU id, its own GC region — in that
   order — and only then anything that reads %THR-CPU, because every per-CPU
   address in this file is derived from it and a freshly cloned thread reads
   CPU 0 until %HA-PERCPU-INIT-CPU has run."
  (let ((tb (%ha-thread-block)) (c (%sk-cb)))
    (%ha-percpu-init-cpu (%ha-cpu1-percpu-base) 1)
    (set-current-actor 0)
    (set-idle-flag 0)
    (%ha-thread-adopt-region (%gc-read64 (+ tb #x4D0)) (%gc-read64 (+ tb #x340)))
    (%gc-write64 (+ c #xE0) (%thr-cpu))
    (%gc-write64 (+ c #x60) 1)
    (%sk-serve 1)
    (%ha-thread-park-region (%gc-read64 (+ tb #x4D0)) (%gc-read64 (+ tb #x340)))
    (%gc-write64 (+ c #x68) 1)
    0))

(defun %sk-t2-entry ()
  (- (%gc-word-of (fn-addr %sk-t2-body) (+ (%ha-base) #x80)) 3))

;;; A fresh socket's fd number, immediately closed again.  Linux hands out the
;;; LOWEST free descriptor, so comparing this before and after a server run is
;;; a proof about fds rather than a count of them: if anything below it had
;;; leaked, the number would come back smaller-or-equal in a way it cannot.
(defun %sk-fd-probe ()
  (let ((f (%sock-open 1)))
    (if (< f 0) -1 (progn (socket-close f) f))))

;;; TWO SERVING THREADS.  The caller must have opened the server already
;;; (%SK-SERVER-OPEN with NCPU 2) and printed its port, because the client
;;; cannot connect to a port nobody told it.  Returns the control block's raw
;;; address, or 0.
;;;
;;; The shape is net/hosted-sync.lisp's %TL-SELFTEST: carve the band, give each
;;; thread its own GC region, switch per-CPU storage on, take THIS thread's
;;; region first (so %GC-REGION-ENTER records region 0's parked frontier), then
;;; clone.  %RT-THREADS-ON is deliberately NOT called: the serving loops touch
;;; the runtime tables nowhere — no INTERN, no FORMAT, no symbol literal, no
;;; global read on any hot path — so the lock would only serialize them.
(defun %sk-run-two-threads (budget)
  (if (zerop (%ha-carve))
      0
      (let ((c (%sk-cb)))
        (if (zerop c)
            0
            (let ((band (%ha-base))
                  (rcb2 (+ (%ha-base) #x200))
                  (rcb3 (+ (%ha-base) #x240))
                  (r0 (%gc-region-0))
                  (k 0) (mode0 0) (tid 0))
              (if (zerop (%ha-thread-stack))
                  0
                  (progn
                    (%ha-zero (%ha-cpu1-percpu-base)
                              (+ (%ha-cpu1-percpu-base) #x4000))
                    (setq mode0 (%ha-percpu-mode))
                    (%ha-percpu-init-cpu (%ha-percpu-base) 0)
                    (setq k (%gc-meta-scale))
                    (%gc-write64 (+ (%ha-thread-block) #x340) k)
                    (%gc-region-init rcb2 *ha-r1-from* *ha-r1-to* *ha-rsize*
                                     (%gc-meta-read (+ r0 #x18) k) k)
                    (%gc-region-init rcb3 *ha-r2-from* *ha-r2-to* *ha-rsize*
                                     (+ *ha-t2-stack* *ha-t2-stack-size*) k)
                    (%gc-write64 (+ (%ha-thread-block) #x4D0) rcb3)
                    (%ha-set-percpu-mode 1)
                    (%gc-region-enter rcb2)
                    (setq tid (%ha-spawn-t2 (%sk-t2-entry)))
                    (%sk-serve 0)
                    (%gc-write64 (+ c #xC8) (%ha-join-t2 budget))
                    (%gc-region-enter r0)
                    (%ha-set-percpu-mode mode0)
                    (%gc-write64 (+ c #x138) tid)
                    (%gc-write64 (+ c #x140) band)
                    c)))))))
