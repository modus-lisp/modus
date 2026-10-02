;;;; genera-compat.lisp — make Modus present as :GENERA.
;;;;
;;;; Modus is an operating system written in Lisp.  Of every implementation
;;;; a portable CL library knows how to reader-conditionalise for, exactly
;;;; two others were ever that: Symbolics Genera and Mezzano.  Every other
;;;; implementation's "-specific" surface is ultimately `ffi:' — a door out
;;;; to C that Modus does not have and does not want.  So when a library
;;;; asks "which Lisp am I on?", the least-wrong answer Modus can give is
;;;; :GENERA: a Lisp that owned its machine, had no C underneath it, and
;;;; whose implementation-specific surface is therefore *scheduler and
;;;; storage* primitives rather than foreign-function glue.
;;;;
;;;; Why not the alternatives (measured over the 69-system local Quicklisp
;;;; corpus, 1211 files):
;;;;   :sbcl     — worst possible.  71 files of SBCL-specific code and 53
;;;;               distinct foreign packages (sb-alien, sb-bsd-sockets,
;;;;               sb-kernel, sb-sys …) inside #+sbcl blocks.
;;;;   :scl/:mkcl/:clasp — best recognition-to-special-casing ratios, but
;;;;               each drags in its parent's internals (CMU / ECL).
;;;;   :mezzano  — family-free and an OS, but bordeaux-threads v0.9.4 gates
;;;;               atomics behind #+(or allegro ccl clasp ecl genera
;;;;               lispworks sbcl) and mezzano is NOT in that list.
;;;;   :genera   — family-free, an OS, and IS in that list.
;;;;
;;;; ---------------------------------------------------------------------
;;;; WHAT GENERA MEANS HERE, AND WHAT IT DOES NOT
;;;;
;;;; Advertising :GENERA is a claim about SHAPE, not about history.  Modus
;;;; is not bit-compatible with a 3600.  Everything in this file is a
;;;; deliberately minimal, honestly-degenerate shim: where Genera had a
;;;; real facility and Modus has none, the shim does the harmless thing and
;;;; says so in a comment rather than pretending.  The specific
;;;; degeneracies are listed at the bottom of this file under KNOWN
;;;; DEGENERACIES — read that before trusting a Genera branch.
;;;; ---------------------------------------------------------------------
;;;;
;;;; !!! SMP LANDMINE — see net/cooperative-atomics.lisp !!!
;;;; The atomic operations reached through this file are atomic only under
;;;; Modus's cooperative single-core scheduler.  The full argument (and the
;;;; three facts it rests on) lives in cooperative-atomics.lisp; every site
;;;; that depends on it is tagged COOPERATIVE-ATOMIC-PRECONDITION.

;;; =====================================================================
;;; 1.  Packages
;;;
;;; Genera's namespace, as the portable corpus actually uses it:
;;;   SCL      Symbolics Common Lisp extensions   (locf, let-globally,
;;;            make-hash-table with storage options)
;;;   SYS      low-level system                   (store-conditional,
;;;            gc-immediately)
;;;   SI       system internals                   (GC reporting knobs)
;;;   PROCESS  the scheduler                      (atomic-incf/-decf)
;;;   CLI      command-loop / table internals     (basic-table-options)
;;;   GRAY-STREAMS   Genera's Gray-stream package (trivial-gray-streams)
;;;   FUTURE-COMMON-LISP  Genera's name for the ANSI CL package
;;;
;;; These are created with :USE NIL — they are namespaces, not CL-using
;;; packages.  Nothing in them shadows a CL symbol.
;;; =====================================================================

;;; SCL on a real Genera is Common Lisp PLUS the Symbolics extensions — every
;;; CL symbol is accessible as `scl:foo'.  This stub is not that; it is the
;;; three extension operators the portable corpus actually reaches for.  That
;;; makes it the wrong SHAPE, and the shape shows: vendored ASDF's Genera
;;; branch does `(:shadowing-import-from :scl :boolean)', and BOOLEAN is a CL
;;; type that SCL would inherit for free on a real Genera.  Exported here as a
;;; correctness fix — SCL:BOOLEAN now names CL:BOOLEAN, which is what it means
;;; on the machine this package is imitating.  IMPORT before EXPORT: without
;;; it, EXPORT would intern a fresh SCL::BOOLEAN and `scl:boolean' would name
;;; a symbol with no type, which is worse than not exporting it at all.
(defpackage "SCL"
  (:use)
  (:import-from "COMMON-LISP" "BOOLEAN" "SHIFTF")
  (:export "LOCF" "LET-GLOBALLY" "MAKE-HASH-TABLE" "BOOLEAN"
           "SHIFTF" "NCONS" "*CURRENT-PROCESS*" "PROCESS-ALLOW-SCHEDULE"))

(defpackage "SYS"
  (:use)
  (:export "STORE-CONDITIONAL" "GC-IMMEDIATELY"
           "VALUE-CELL-LOCATION" "LOCATION-CONTENTS"))

(defpackage "SI"
  (:use)
  (:export "GC-REPORT-STREAM" "GC-REPORTS-ENABLE" "GC-EPHEMERAL-REPORTS-ENABLE"
           "GC-WARNINGS-ENABLE" "EPHEMERAL-GC-FLIP" "PROCESS-SPARE-SLOT-4"))

(defpackage "PROCESS"
  (:use)
  (:export "ATOMIC-INCF" "ATOMIC-DECF"
           ;; section 8: processes, locks, waits, over SB-THREAD
           "PROCESS" "PROCESS-P" "PROCESS-RUN-FUNCTION" "PROCESS-NAME"
           "PROCESS-ACTIVE-P" "PROCESS-WAIT" "PROCESS-INTERRUPT" "PROCESS-KILL"
           "*ALL-PROCESSES*" "WAKEUP" "BLOCK-WITH-TIMEOUT" "WITH-TIMEOUT"
           "MAKE-LOCK" "MAKE-LOCK-ARGUMENT" "LOCK" "UNLOCK" "LOCK-LOCKABLE-P"
           "WITH-NO-OTHER-PROCESSES"
           "ATOMIC-UPDATEF" "ATOMIC-POP" "ATOMIC-REPLACEF"))

;;; CLI is referenced only as `cli::basic-table-options' (double colon), so
;;; the name need not be external — but the PACKAGE must exist or the form
;;; is a READ error, which in Modus drops the whole enclosing toplevel form.
(defpackage "CLI"
  (:use)
  (:export "BASIC-TABLE-OPTIONS"))

;;; =====================================================================
;;; 2.  FUTURE-COMMON-LISP
;;;
;;; On a real Genera, LISP is CLtL1 and FUTURE-COMMON-LISP is the ANSI CL
;;; package.  On Modus, COMMON-LISP *is* the ANSI package — there is no
;;; CLtL1 package to be distinct from.  So FUTURE-COMMON-LISP is not a
;;; separate namespace here; it is another name for COMMON-LISP, installed
;;; as a nickname.  cl-ppcre's
;;;     (:use #-:genera :cl #+:genera :future-common-lisp)
;;;     #+:genera (:shadowing-import-from :common-lisp :lambda :simple-string :string)
;;; then means exactly what the #-:genera branch meant, and the
;;; shadowing-import is a no-op because the three symbols it imports are
;;; already the very symbols inherited.
;;;
;;; A separate package that :USEs CL would NOT work: :USE is not transitive,
;;; so a package using FUTURE-COMMON-LISP would inherit only FCL's own
;;; externals, and every CL symbol would have to be re-exported by hand.
;;; The nickname is both simpler and semantically the truth.
;;; =====================================================================

(defun %genera-add-cl-nickname ()
  (let ((cl (find-package "COMMON-LISP")))
    (when (and cl (not (find-package "FUTURE-COMMON-LISP")))
      (rename-package cl "COMMON-LISP"
                      (cons "FUTURE-COMMON-LISP" (package-nicknames cl))))))

;;; =====================================================================
;;; 3.  GRAY-STREAMS
;;;
;;; trivial-gray-streams' package.lisp does
;;;     (:import-from #+(or abcl genera) :gray-streams  <28 symbols>)
;;; and streams.lisp reads `gray-streams:stream-read-sequence' with a
;;; SINGLE colon, so the 28 symbols must exist AND be external.
;;;
;;; They are plain symbols, not generic functions: trivial-gray-streams
;;; defines its own mirror CLASSES in its own package and only needs the
;;; FUNCTION symbols to be shared, so `defmethod gray-streams:stream-…'
;;; creates the generic function on first use.  Creating them here as
;;; anything else would be a lie about a facility Modus does not have.
;;; See KNOWN DEGENERACIES.
;;; =====================================================================

(defpackage "GRAY-STREAMS"
  (:use)
  (:export
   ;; classes
   "FUNDAMENTAL-STREAM"
   "FUNDAMENTAL-INPUT-STREAM" "FUNDAMENTAL-OUTPUT-STREAM"
   "FUNDAMENTAL-CHARACTER-STREAM" "FUNDAMENTAL-BINARY-STREAM"
   "FUNDAMENTAL-CHARACTER-INPUT-STREAM" "FUNDAMENTAL-CHARACTER-OUTPUT-STREAM"
   "FUNDAMENTAL-BINARY-INPUT-STREAM" "FUNDAMENTAL-BINARY-OUTPUT-STREAM"
   ;; functions
   "STREAM-READ-CHAR" "STREAM-UNREAD-CHAR" "STREAM-READ-CHAR-NO-HANG"
   "STREAM-PEEK-CHAR" "STREAM-LISTEN" "STREAM-READ-LINE"
   "STREAM-CLEAR-INPUT" "STREAM-WRITE-CHAR" "STREAM-LINE-COLUMN"
   "STREAM-START-LINE-P" "STREAM-WRITE-STRING" "STREAM-TERPRI"
   "STREAM-FRESH-LINE" "STREAM-FINISH-OUTPUT" "STREAM-FORCE-OUTPUT"
   "STREAM-CLEAR-OUTPUT" "STREAM-ADVANCE-TO-COLUMN"
   "STREAM-READ-BYTE" "STREAM-WRITE-BYTE"
   ;; the three trivial-gray-streams extends the proposal with, which its
   ;; #+genera branch defines methods on
   "STREAM-READ-SEQUENCE" "STREAM-WRITE-SEQUENCE" "STREAM-FILE-POSITION"))

;;; =====================================================================
;;; 4.  Locatives
;;;
;;; Genera's LOCF returns a LOCATIVE: a first-class pointer to a place.
;;; STORE-CONDITIONAL is its only consumer in the portable corpus
;;; (bordeaux-threads apiv2/atomics.lisp:14 is the ONLY call site), so
;;; Modus defines BOTH ends and the representation is a free choice.
;;;
;;; REPRESENTATION CHOSEN: a locative is ONE closure of two arguments —
;;; a read/write dispatcher over the place's subforms.
;;;
;;;   (scl:locf (svref v 0))
;;;     => (lambda (op val)
;;;          (if (eq op :read) (svref v 0) (setf (svref v 0) val)))
;;;
;;;   read  = (funcall loc :read nil)
;;;   write = (funcall loc :write new)
;;;
;;; Why a closure and not a raw address:
;;;   - Modus's collector COPIES (Cheney semispace).  A raw interior
;;;     address handed out to Lisp code would be invalidated by the next
;;;     GC, silently.  A closure is an ordinary heap object the collector
;;;     already traces and forwards correctly.
;;;   - It works for ANY setf-able place, not just slots the runtime knows
;;;     how to take an address of — locf's whole point.
;;;   - It needs no new object subtag, so it cannot collide with
;;;     runtime/tags.lisp (a documented crash class in this project).
;;;
;;; Why ONE dispatcher closure and not the more obvious
;;; (cons READER WRITER) pair of closures:
;;;
;;;   THE PAIR SHAPE HITS A PRE-EXISTING MODUS COMPILER BUG.  Measured on
;;;   this tree (e4d26a8) with a plain `./modus', no Genera code involved:
;;;
;;;       (defvar *v* (make-array 1))
;;;       (defun k () (cons (lambda () (svref *v* 0)) (lambda () 1)))
;;;       (funcall (car (k)))          ; => UNHANDLED-ESCAPE, swallowed
;;;
;;;   The trigger is TWO closures constructed inside ONE compiled DEFUN
;;;   body where at least one of them references a GLOBAL variable;
;;;   funcalling either of the resulting closures escapes.  ONE closure
;;;   referencing a global is fine; two closures referencing only
;;;   lexicals are fine; the identical cons-of-two-lambdas written at
;;;   toplevel (not inside a defun) is fine.  This matters because LOCF
;;;   expands INSIDE the caller's defun — bordeaux's
;;;   ATOMIC-INTEGER-COMPARE-AND-SWAP — so the pair shape would have put
;;;   two closures in one defun body at exactly the wrong place.
;;;
;;;   The dispatcher shape allocates one closure and sidesteps it
;;;   entirely.  It is also cheaper.  (The underlying compiler bug is
;;;   real and independent of this work; it is reported separately.)
;;;
;;; NOTE the double evaluation: PLACE's subforms are evaluated on every
;;; read and every write, exactly as `setf' of the same place would.
;;; =====================================================================

(defmacro scl::locf (place)
  (let ((op (gensym "LOCOP")) (v (gensym "LOCV")))
    `(lambda (,op ,v)
       (if (eq ,op :read) ,place (setf ,place ,v)))))

(defun %genera-locative-read (loc) (funcall loc :read nil))
(defun %genera-locative-write (loc value) (funcall loc :write value))

;;; COOPERATIVE-ATOMIC-PRECONDITION: no LOOP => no YIELD => no interleaving.
;;; Genera contract: returns T if the store happened, NIL if it did not.
;;; bordeaux uses the result directly as ATOMIC-INTEGER-COMPARE-AND-SWAP's
;;; return value, which is documented "Returns T if the replacement was
;;; successful, otherwise NIL".
(defun sys::store-conditional (locative old new)
  (if (eql (%genera-locative-read locative) old)
      (progn (%genera-locative-write locative new) t)
      nil))

;;; =====================================================================
;;; 5.  PROCESS:ATOMIC-INCF / ATOMIC-DECF
;;;
;;; RETURN VALUE IS THE **NEW** VALUE.  bordeaux-threads uses these
;;; UNWRAPPED —
;;;     #+genera `(process:atomic-incf ,place ,delta)
;;; — where its #+sbcl / #+ecl branches wrap the call in (+ … delta) /
;;; (- … delta) because those implementations return the PRIOR value.
;;; ATOMIC-INTEGER-INCF is documented "Returns the new value".  Do not
;;; "fix" these to return the prior value without also changing the
;;; advertised feature.
;;; =====================================================================

;;; These are thin Genera spellings of the primitives in
;;; net/cooperative-atomics.lisp — load that file FIRST.  The atomicity
;;; argument and the SMP landmine warning live there.
;;; COOPERATIVE-ATOMIC-PRECONDITION: no LOOP => no YIELD => no interleaving.
(defmacro process::atomic-incf (place &optional (delta 1))
  `(%atomic-incf ,place ,delta))

;;; COOPERATIVE-ATOMIC-PRECONDITION: no LOOP => no YIELD => no interleaving.
(defmacro process::atomic-decf (place &optional (delta 1))
  `(%atomic-decf ,place ,delta))

;;; =====================================================================
;;; 6.  Storage / GC surface (trivial-garbage's #+genera branches)
;;; =====================================================================

;;; Genera's SCL:MAKE-HASH-TABLE accepts storage options CL:MAKE-HASH-TABLE
;;; does not — trivial-garbage passes :GC-PROTECT-VALUES.  Modus's
;;; collector has no weak references at all, so the option is accepted and
;;; ignored, and the table is an ordinary strong hash table.  This is the
;;; SAME strength trivial-garbage gets on Modus today (its non-genera path
;;; refuses to make a weak table too), so nothing is weakened; it just
;;; stops being an error.
(defun scl::make-hash-table (&rest args)
  (let ((clean nil) (rest args))
    (loop
      (when (null rest) (return nil))
      (if (member (car rest) '(:gc-protect-values :gc-protect-keys
                               :store-hash-code :rehash-before-cold
                               :growth-factor :area :locking))
          (setq rest (cddr rest))
          (progn (setq clean (cons (cadr rest) (cons (car rest) clean)))
                 (setq rest (cddr rest)))))
    (apply #'cl:make-hash-table (reverse clean))))

;;; Genera's GC reporting knobs.  Modus's collector is fully automatic and
;;; reports nothing, so these are inert specials that exist only so
;;; SCL:LET-GLOBALLY has something to bind.
(defvar si::gc-report-stream nil)
(defvar si::gc-reports-enable nil)
(defvar si::gc-ephemeral-reports-enable nil)
(defvar si::gc-warnings-enable nil)

;;; SCL:LET-GLOBALLY binds the *global* value of each variable for the
;;; dynamic extent of the body (Genera's answer to "bind a special that
;;; other processes should also see").  Modus is single-core and
;;; cooperative, so a dynamic binding is the whole of that semantics.
;;; PROGV is used rather than LET because the variable names come from the
;;; caller's source and need not be known-special here.
(defmacro scl::let-globally (bindings &rest body)
  `(progv (list ,@(mapcar (lambda (b) (list 'quote (car b))) bindings))
       (list ,@(mapcar #'cadr bindings))
     ,@body))

;;; Modus's collector is triggered by allocation, not by request: there is
;;; no user-callable "collect now" entry point in the shipping image.  Both
;;; of these therefore do nothing and return NIL.  A caller asking for a
;;; full GC gets one on its next allocation, which is the honest answer.
(defun sys::gc-immediately (&optional full) (declare (ignore full)) nil)
(defun si::ephemeral-gc-flip () nil)

;;; Genera hash tables carry a plist of storage options; trivial-garbage
;;; reads :GC-PROTECT-VALUES out of it to answer HASH-TABLE-WEAKNESS.
;;; Modus tables carry no such plist and are never weak, so returning NIL
;;; makes trivial-garbage's
;;;     (if (null (getf (cli::basic-table-options ht) :gc-protect-values t)) …)
;;; take the (getf … t) => T => NIL branch: "this table has no weakness",
;;; which is correct.
(defun cli::basic-table-options (ht) (declare (ignore ht)) nil)

;;; =====================================================================
;;; 7.  Feature advertisement
;;;
;;; Done LAST, and only after every package and operator above exists, so
;;; that no reader-conditional can ever select a Genera branch whose
;;; support has not been installed yet.
;;;
;;; :64-BIT / :32-BIT is not a Genera claim — it is the machine word size —
;;; but it is installed here because it only becomes load-bearing once :GENERA
;;; is on: bordeaux-threads'
;;;     (deftype %atomic-integer-value () #+32-bit … #+64-bit …)
;;; is only ever reached down a recognised-implementation path, and with
;;; neither feature present the deftype body is empty, which makes the type
;;; NIL and every CHECK-TYPE against it fail.
;;;
;;; It used to push :64-BIT unconditionally ("simply true of this runtime"),
;;; so the 30-bit tower (i386, RV32, ppc32, arm32, 68k) claimed :64-BIT and
;;; bordeaux-threads chose its 64-bit atomic integer type there.  The word
;;; size is read off MOST-POSITIVE-FIXNUM: 2^62-1 on the 64-bit ports,
;;; 2^30-1 on the 32-bit ones.
;;; =====================================================================

(defun %genera-install-features ()
  (when (boundp '*features*)
    (let ((word (if (> most-positive-fixnum #xFFFFFFFF) :64-bit :32-bit)))
      (unless (or (member :64-bit *features*) (member :32-bit *features*))
        (setq *features* (cons word *features*))))
    (unless (member :genera *features*)
      (setq *features* (cons :genera *features*)))))

(defun %init-genera-compat ()
  (handler-case (%genera-add-cl-nickname) (t (c) nil))
  (%genera-install-features)
  t)

(%init-genera-compat)

;;; =====================================================================
;;; 8.  Processes and locks (bordeaux-threads' Genera backends)
;;;
;;; bordeaux-threads 0.9.4 selects impl-genera.lisp on :GENERA, in BOTH its
;;; APIs -- and its v1 API (BT:MAKE-LOCK, what almost every library calls)
;;; now sits on v2.  Until this section, PROCESS exported only the two
;;; atomics, so every BT:MAKE-LOCK in the image died UNDEFINED-FUNCTION
;;; PROCESS::MAKE-LOCK.  This is the surface those two files name, each
;;; operator given its Genera meaning over Modus's real threads (the
;;; SB-THREAD surface, net/sb-thread-shim.lisp).
;;;
;;; SB-THREAD IS LOOKED UP WHEN CALLED, never named in source: this file is
;;; installed BEFORE the sb-thread shim, and on a target without threads the
;;; package does not exist at all (a READ of `sb-thread:x' would drop the
;;; rest of this file).  On such a target the operators below signal.
;;;
;;; THE DEGENERACIES, in one place (each repeated where it lives):
;;;   WITH-NO-OTHER-PROCESSES  a PROGN.  Genera stopped the scheduler; real
;;;       threads cannot be stopped, and a global lock would deadlock the
;;;       way bordeaux uses it (it BLOCKS in PROCESS:LOCK inside one).  Each
;;;       operator here is thread-safe on its own instead.
;;;   LOCK-LOCKABLE-P  answers by TRYING the lock: on T the caller now holds
;;;       it, and the PROCESS:LOCK bordeaux always issues next completes the
;;;       claim.  A caller that asks and then does not lock leaks the lock.
;;;   WITH-TIMEOUT     bounds only the waits made through this section (a
;;;       PROCESS:LOCK, PROCESS-WAIT, BLOCK-WITH-TIMEOUT); a body that is
;;;       computing, not waiting, runs to completion.  Modus cannot
;;;       interrupt another thread (see INTERRUPT-THREAD in the shim).
;;;   PROCESS-WAIT / BLOCK-WITH-TIMEOUT  poll their predicate every 1 ms;
;;;       WAKEUP is therefore a no-op.  Latency 1 ms, no lost wakeups: the
;;;       state a waiter watches is in the predicate, not in the wakeup.
;;;   PROCESS-INTERRUPT / PROCESS-KILL  signal, as the shim's do.
;;;
;;; Written around the closure bug recorded in section 4: no DEFUN below
;;; builds more than one closure.
;;; =====================================================================

(defun %genera-sbt (name)
  "SB-THREAD's function NAME, found now; signals if this image has none."
  (let ((s (and (find-package "SB-THREAD") (find-symbol name "SB-THREAD"))))
    (if (and s (fboundp s))
        (symbol-function s)
        (error "PROCESS: this Modus image has no threads (SB-THREAD:~A)" name))))

(defun %genera-current ()
  "The current process: SB-THREAD:*CURRENT-THREAD*, or :MAIN without threads."
  (let ((s (and (find-package "SB-THREAD")
                (find-symbol "*CURRENT-THREAD*" "SB-THREAD"))))
    (if (and s (boundp s)) (symbol-value s) :main)))

(define-symbol-macro scl::*current-process* (%genera-current))

;;; One global mutex serialises the short critical sections below (the
;;; atomic place updates, the per-process table).  Nothing blocks while
;;; holding it.  Made on first use -- the shim is not installed yet when
;;; this file is.
;;;
;;; WORKER ALLOCATIONS DIE WITH THE WORKER.  Each thread allocates in its own
;;; GC region, reclaimed when it exits, so an object a worker creates must not
;;; be stored anywhere global -- the next thread to touch it finds garbage.
;;; First use may be on a worker, so the mutex is made inside %RT-ENTER, whose
;;; locked sections allocate in the immortal arena (mvm/prelude.lisp).
(defvar %genera-atomic-mutex nil)

(defun %genera-make-atomic-mutex ()
  (unless %genera-atomic-mutex
    (setq %genera-atomic-mutex
          (funcall (%genera-sbt "MAKE-MUTEX") :name "genera atomic")))
  %genera-atomic-mutex)

(defun %genera-atomically (thunk)
  (when (and (null %genera-atomic-mutex) (find-package "SB-THREAD"))
    (%rt-enter)
    (%genera-make-atomic-mutex)
    (%rt-leave))
  (if (null %genera-atomic-mutex)
      (funcall thunk)
      (progn
        (funcall (%genera-sbt "GRAB-MUTEX") %genera-atomic-mutex)
        (unwind-protect (funcall thunk)
          (funcall (%genera-sbt "RELEASE-MUTEX") %genera-atomic-mutex)))))

(defmacro process::atomic-updatef (place function)
  `(%genera-atomically (lambda () (setf ,place (funcall ,function ,place)))))
(defmacro process::atomic-pop (place)
  `(%genera-atomically (lambda () (pop ,place))))
(defmacro process::atomic-replacef (place new)
  `(%genera-atomically (lambda () (shiftf ,place ,new))))

(defun scl::ncons (x) (list x))

;;; --- processes ---

(deftype process::process ()
  (let ((s (and (find-package "SB-THREAD") (find-symbol "THREAD" "SB-THREAD"))))
    (or s t)))

(define-symbol-macro process::*all-processes* (%genera-all-processes))
(defun %genera-all-processes () (funcall (%genera-sbt "ALL-THREADS")))

(defun process::process-p (x) (funcall (%genera-sbt "THREADP") x))
(defun process::process-name (p) (funcall (%genera-sbt "THREAD-NAME") p))
(defun process::process-active-p (p) (funcall (%genera-sbt "THREAD-ALIVE-P") p))
(defun scl::process-allow-schedule () (funcall (%genera-sbt "THREAD-YIELD")))
(defun process::wakeup (p) p)            ; waits poll; see the header

(defun process::process-run-function (name-or-options function &rest args)
  "Genera: NAME-OR-OPTIONS is a name or a keyword list with :NAME."
  (let ((name (if (consp name-or-options)
                  (or (getf name-or-options :name) "Anonymous")
                  name-or-options)))
    (funcall (%genera-sbt "MAKE-THREAD") function
             :name (string name) :arguments args)))

(defun process::process-interrupt (p function &rest args)
  (funcall (%genera-sbt "INTERRUPT-THREAD") p
           (lambda () (apply function args))))

(defun process::process-kill (p &rest options)
  (declare (ignore options))
  (funcall (%genera-sbt "TERMINATE-THREAD") p))

;;; SI:PROCESS-SPARE-SLOT-4 -- where bordeaux v1 parks a thread's return
;;; values.  A table keyed by process, made at install (on main).  Stores go
;;; through %RT-ENTER so the table's own entries land in the immortal arena
;;; rather than the storing worker's region, and the value list is copied
;;; there as well (its elements are still the worker's).
(defvar %genera-spare-slots (make-hash-table :test 'eq))
(defun si::process-spare-slot-4 (p)
  (%rt-enter)
  (let ((v (gethash p %genera-spare-slots)))
    (%rt-leave)
    v))
(defun (setf si::process-spare-slot-4) (value p)
  ;; The VALUE is copied here, under the lock, so its spine lives in region 0
  ;; too: bordeaux joins by waiting for the process to die and reading this
  ;; slot afterwards, and by then the worker's region may be another thread's.
  (%rt-enter)
  (puthash p %genera-spare-slots (copy-tree value))
  (%rt-leave)
  value)

;;; --- timeouts and waiting ---
;;;
;;; The deadline of the innermost PROCESS:WITH-TIMEOUT is a special, bound
;;; per thread (a worker's binding lives on its own binding stack, and a
;;; THROW out of the binding unwinds it -- %DYNB-UNWIND, mvm/prelude.lisp).
;;; A table keyed by thread would have had workers storing into a global.

(defvar %genera-deadline-now nil)
(defun %genera-deadline () %genera-deadline-now)

(defun %genera-seconds-left (deadline)
  (max 1/1000 (/ (- deadline (get-internal-real-time))
                 internal-time-units-per-second)))

(defun %genera-expired-p (deadline)
  (and deadline (>= (get-internal-real-time) deadline)))

;;; RATIOS, NOT FLOAT LITERALS, anywhere in this file.  It is evaluated at boot
;;; in every image, the web (JS MVM) one included, and reading a float literal
;;; calls %ROUND-TO-SINGLE, whose opcode the JS engine does not implement: one
;;; 0.001 here stopped the web image booting at step 0.
(defun %genera-pause () (sleep 1/1000))

(defmacro process::with-timeout ((seconds &rest options) &body body)
  "Genera: the value of BODY, or NIL if SECONDS pass first.  See the header:
   only waits made through this section are bounded."
  (declare (ignore options))
  `(%genera-call-with-timeout ,seconds (lambda () ,@body)))

(defun %genera-call-with-timeout (seconds thunk)
  (if (null seconds)
      (funcall thunk)
      (let* ((outer (%genera-deadline))
             (mine (+ (get-internal-real-time)
                      (round (* seconds internal-time-units-per-second))))
             (dl (if (and outer (< outer mine)) outer mine)))
        (catch '%genera-timeout
          (let ((%genera-deadline-now dl))
            (funcall thunk))))))

(defun process::process-wait (whostate predicate &rest args)
  "Return when (apply PREDICATE ARGS) is true.  Polls; see the header."
  (declare (ignore whostate))
  (let ((dl (%genera-deadline)))
    (loop
      (when (apply predicate args) (return t))
      (when (%genera-expired-p dl) (throw '%genera-timeout nil))
      (%genera-pause))))

(defun process::block-with-timeout (timeout whostate predicate &rest args)
  "The predicate's value once true, or NIL after TIMEOUT seconds (NIL:
   no limit)."
  (declare (ignore whostate))
  (let ((end (and timeout (+ (get-internal-real-time)
                             (round (* timeout internal-time-units-per-second))))))
    (loop
      (let ((v (apply predicate args)))
        (when v (return v)))
      (when (%genera-expired-p end) (return nil))
      (%genera-pause))))

;;; SYS:VALUE-CELL-LOCATION of a (quoted) variable -- bordeaux passes a LEXICAL
;;; one -- is a locative to it, which is exactly section 4's SCL:LOCF.
(defmacro sys::value-cell-location (form)
  (%genera-locf-var-form form))
(defun %genera-locf-var-form (form)
  (let ((var (if (and (consp form) (eq (car form) 'quote)) (cadr form) form)))
    (list 'scl::locf var)))
(defun sys::location-contents (loc) (%genera-locative-read loc))
(defun (setf sys::location-contents) (value loc)
  (%genera-locative-write loc value)
  value)

;;; --- locks ---
;;;
;;; A lock is a vector, not a struct, so this file needs nothing of DEFSTRUCT:
;;;   0 tag   1 name   2 mutex   3 recursive-p   4 owner   5 depth   6 claimed
;;; OWNER is only written by the thread holding MUTEX (set after taking it,
;;; cleared before giving it back), so reading it to ask "is it mine?" is safe.

(defun process::make-lock (name &rest options &key recursive &allow-other-keys)
  (declare (ignore options))
  (vector '%genera-lock name
          (funcall (%genera-sbt "MAKE-MUTEX") :name (string name))
          recursive nil 0 nil))

(defun process::make-lock-argument (lock &rest options)
  "A constant, not a fresh list: PROCESS:LOCK ignores it, and a caller keeps it
   in the lock object (bordeaux-threads' Genera backend stores it on every
   acquire).  A list made on a worker thread is that thread's object, and
   storing it into a lock other threads share is the store modus's shared-store
   guard refuses -- every bordeaux-threads lock taken off the main thread
   failed."
  (declare (ignore lock options))
  :lock-argument)

(defun %genera-take (lock me)
  (setf (svref lock 4) me (svref lock 5) 1 (svref lock 6) nil)
  t)

(defun process::lock (lock &optional lock-argument)
  "Take LOCK, waiting as long as the enclosing PROCESS:WITH-TIMEOUT allows."
  (declare (ignore lock-argument))
  (let ((me (%genera-current)))
    (cond
      ((and (eq (svref lock 4) me) (svref lock 6))    ; LOCK-LOCKABLE-P took it
       (setf (svref lock 6) nil)
       t)
      ((eq (svref lock 4) me)
       (if (svref lock 3)
           (progn (setf (svref lock 5) (+ 1 (svref lock 5))) t)
           (error "PROCESS:LOCK: ~A is already held by this process" (svref lock 1))))
      (t
       (let ((dl (%genera-deadline)))
         (if (null dl)
             (funcall (%genera-sbt "GRAB-MUTEX") (svref lock 2))
             (unless (funcall (%genera-sbt "GRAB-MUTEX") (svref lock 2)
                              :timeout (%genera-seconds-left dl))
               (throw '%genera-timeout nil)))
         (%genera-take lock me))))))

(defun process::lock-lockable-p (lock)
  "Genera: could LOCK be taken now?  Here: TAKE it if possible (see the header)."
  (let ((me (%genera-current)))
    (cond
      ((eq (svref lock 4) me) (and (svref lock 3) t))
      ((funcall (%genera-sbt "GRAB-MUTEX") (svref lock 2) :waitp nil)
       (%genera-take lock me)
       (setf (svref lock 6) t)
       t)
      (t nil))))

(defun process::unlock (lock &optional lock-argument)
  (declare (ignore lock-argument))
  (when (eq (svref lock 4) (%genera-current))
    (setf (svref lock 5) (- (svref lock 5) 1))
    (when (<= (svref lock 5) 0)
      (setf (svref lock 4) nil (svref lock 5) 0 (svref lock 6) nil)
      (funcall (%genera-sbt "RELEASE-MUTEX") (svref lock 2))))
  t)

(defmacro process::with-no-other-processes (&body body)
  "A PROGN; see the header."
  `(progn ,@body))

;;; =====================================================================
;;; KNOWN DEGENERACIES  (what a Genera branch will and will not get)
;;;
;;;  * WEAK REFERENCES DO NOT EXIST.  SCL:MAKE-HASH-TABLE ignores
;;;    :GC-PROTECT-VALUES; trivial-garbage's genera weak-pointer path keeps
;;;    its objects alive forever through a strong hash table.  This is not a
;;;    regression — Modus's non-genera trivial-garbage path has no weak
;;;    references either — but a program that RELIES on weakness to bound
;;;    memory will grow without limit.
;;;
;;;  * GC IS NOT REQUESTABLE.  SYS:GC-IMMEDIATELY and SI:EPHEMERAL-GC-FLIP
;;;    are no-ops.
;;;
;;;  * GRAY-STREAMS IS A NAMESPACE, NOT AN IMPLEMENTATION.  The 28 symbols
;;;    exist so trivial-gray-streams can load; they are not wired into
;;;    Modus's own stream dispatch.  A Gray stream class defined through
;;;    trivial-gray-streams will therefore NOT be usable by CL:READ-CHAR
;;;    and friends.  Wiring Modus's stream system to dispatch through these
;;;    generic functions is the real fix and is out of scope here.
;;;
;;;  * FINALIZERS.  trivial-garbage's #+genera branch signals
;;;    "Finalizers are not available in Genera." — which is TRUE of Modus
;;;    as well, and is a better outcome than the silent empty body the
;;;    unrecognised-implementation path produces.
;;;
;;;  * PROCESSES ARE SB-THREAD THREADS, LOCKS ARE ITS MUTEXES.  Section 8
;;;    lists what that does not give a Genera program: no scheduler freeze
;;;    (WITH-NO-OTHER-PROCESSES is a PROGN), timeouts that bound only waits,
;;;    1 ms polling for PROCESS-WAIT, no interrupt or kill.
;;;
;;;  * GENERA PREDATES ANSI.  Some #+genera branches in the wild are
;;;    decades stale.  Anything that loads down a Genera path should be
;;;    behaviour-probed, not assumed.  The per-library evidence for this
;;;    tree is in the task #237 report.
;;; =====================================================================
