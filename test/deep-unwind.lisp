;;;; deep-unwind.lisp — A NON-LOCAL EXIT RUNS EVERY CLEANUP, AT ANY DEPTH.
;;;;
;;;;   ./modus --script test/deep-unwind.lisp
;;;;
;;;; Every CATCH / UNWIND-PROTECT / HANDLER-CASE keeps one frame on the
;;;; per-thread handler-frame stack while its body runs, and a THROW, a handled
;;;; ERROR, a RETURN-FROM or a GO unwinds by longjmping frame to frame.  That
;;;; stack was a fixed array (512 frames on hosted x86-64) whose overflow was a
;;;; TRANSPARENT NO-OP: past the cap a frame was simply not pushed, so an exit
;;;; from 601 / 801 / 2001 nested UNWIND-PROTECTs ran only the outermost ~498
;;;; cleanups and skipped the rest, without a word.  On a worker each skipped
;;;; cleanup was also a LET of a special whose restore never ran, so the
;;;; special read 99 / 299 (a dead inner binding) after the exit instead of
;;;; its outer value.  SBCL runs all of them.  Measured on 11f01c47 (the
;;;; parent of the fix): depth 100 passes, and at 497 -- past the cap once
;;;; the script loader's own frames are counted -- a THROW escaped every
;;;; CATCH (an unhandled %THROW-IN-FLIGHT) and aborted the file.
;;;;
;;;; AArch64 had no bound check at all: its window holds 170 frames and the
;;;; 171st was stored past it -- on a worker into the NEXT thread's window.
;;;; Measured on the Pi 5 (origin/main 82fa6e7e): a worker 200 deep made its
;;;; neighbour's HANDLER-CASE receive a MEMORY-FAULT-ERROR instead of its own
;;;; ERROR, and 400 deep killed the process with SIGSEGV.
;;;;
;;;; Now frames past the window's 512 (AArch64: 170) go to a lazily reserved
;;;; per-thread extension (translate-x64 / translate-aarch64, THE
;;;; HANDLER-FRAME STACK HAS NO SILENT CEILING),
;;;; and a push that still cannot be stored is LOUD: the enclosing frame
;;;; receives a HANDLER-STACK-EXHAUSTED (a STORAGE-CONDITION).  The last
;;;; section reaches that through the test-only per-window limit, since no
;;;; native stack gets to the real capacity.
;;;;
;;;; Checked on MAIN and on a WORKER thread, for N from 100 to 5000:
;;;;   CLEANUPS   N nested UNWIND-PROTECTs, exited by THROW, a handled ERROR,
;;;;              a handled hardware fault (the signal stub's own pop),
;;;;              RETURN-FROM and GO from the bottom: all N cleanups run, in
;;;;              innermost-first order, and the frame depth comes back.
;;;;   CATCH      N nested CATCHes with distinct tags: a THROW from the bottom
;;;;              to a deep tag, a middle one and a shallow one is caught by
;;;;              exactly that CATCH, with every cleanup inside it run.  On a
;;;;              worker a CATCH also holds a dynamic binding (its tag list),
;;;;              and the worker's binding stack holds 960: past 850 the
;;;;              worker checks instead that the loud binding-stack error is
;;;;              an exit that runs every established cleanup.
;;;;   SPECIALS   a LET of a special N deep restores the outer value after
;;;;              each exit kind (the worker symptom).  A worker's binding
;;;;              stack holds 960 bindings (a fresh worker already has ~60), so past 850
;;;;              the worker checks
;;;;              instead that the loud binding-stack error is an exit that
;;;;              restores the outer value too.
;;;;   HANDLERS   HANDLER-CASEs nested N deep with alternating condition types
;;;;              dispatch to the innermost one of the right type; HANDLER-BIND
;;;;              handlers N deep all run, innermost first, and one that exits
;;;;              non-locally still runs every cleanup.
;;;;   NEIGHBOURS a worker nesting N deep leaves a concurrently parked
;;;;              worker's window alone (its HANDLER-CASE, special, values).
;;;;   OVERFLOW   past the (lowered) limit the push signals a STORAGE-CONDITION
;;;;              to the enclosing handler, every frame that WAS pushed runs its
;;;;              cleanup, and the stack is usable again afterwards.
;;;;
;;;; WHAT WOULD MAKE A PASS MEANINGLESS: the nesting running in the
;;;; interpreter, whose frames never touch the native stack.  Every nest
;;;; records the native frame depth at its bottom and the test checks it grew
;;;; by at least N.

(defvar *fail* 0)
(defvar *checks* 0)
(defvar *quiet-ok* t)

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (unless *quiet-ok* (format t "  ok   ~A~%" name))
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A: got ~S want ~S~%" name
                     (if (and (consp got) (> (length got) 12)) (list :len (length got) :head (subseq got 0 6)) got)
                     (if (and (consp want) (> (length want) 12)) (list :len (length want) :head (subseq want 0 6)) want)))))

(defun depth () (modus-int::%hc-depth))

(defvar *trace* nil)          ; cleanup levels, pushed as they run
(defvar *bottom-depth* 0)     ; native frame depth seen at the bottom
(defvar *sp* :outer)          ; the special the SPECIALS section binds
(defvar *bad-address* 8)      ; unmapped: reading it is a SIGSEGV

(define-condition deep-a (error) ())
(define-condition deep-b (error) ())
(define-condition deep-note (condition) ())

(defun bottom-exit (how k)
  (setq *bottom-depth* (depth))
  (ecase how
    (:throw (throw 'deep :thrown))
    (:error (error "deep bottom"))
    ;; a hardware fault: the signal stub pops the handler stack itself
    ;; (an inline pop, not the pop helper), so it is its own exit path
    (:fault (modus-int::%gc-read64 *bad-address*))
    (:fn (funcall k))))

;;; CLEANUPS: N nested UNWIND-PROTECTs, each cleanup pushing its level.
(defun nest-uwp (n lim how k)
  (unwind-protect
       (if (< n lim) (nest-uwp (+ n 1) lim how k) (bottom-exit how k))
    (push n *trace*)))

(defun run-uwp (lim how)
  "Exit LIM+1 nested UNWIND-PROTECTs by HOW; returns (value trace depth-ok)."
  (setq *trace* nil)
  (let* ((d0 (depth))
         (v (ecase how
              (:throw (catch 'deep (nest-uwp 0 lim :throw nil)))
              (:error (handler-case (nest-uwp 0 lim :error nil)
                        (error () :handled)))
              (:fault (handler-case (nest-uwp 0 lim :fault nil)
                        (error (e) (if (string= (symbol-name (type-of e)) "MEMORY-FAULT-ERROR") :faulted e))))
              (:return-from (block out
                              (nest-uwp 0 lim :fn (lambda () (return-from out :returned)))))
              (:go (let ((r nil))
                     (tagbody
                        (nest-uwp 0 lim :fn (lambda () (go out)))
                      out
                        (setq r :went))
                     r)))))
    (list v (nreverse *trace*) (= (depth) d0) (>= *bottom-depth* (+ d0 lim)))))

(defun iota (n) (loop for i from 0 below n collect i))

(defun check-cleanups (where lim)
  (dolist (how '(:throw :error :fault :return-from :go))
    (let ((r (run-uwp lim how)))
      (chk (format nil "~A ~D ~(~A~) value" where lim how) (first r)
           (ecase how (:throw :thrown) (:error :handled) (:fault :faulted)
                  (:return-from :returned) (:go :went)))
      ;; *trace* is pushed innermost-first, so reversed it must read
      ;; LIM, LIM-1, ... 0 -- i.e. the reverse of 0..LIM.
      (chk (format nil "~A ~D ~(~A~) every cleanup, innermost first" where lim how)
           (second r) (reverse (iota (+ lim 1))))
      (chk (format nil "~A ~D ~(~A~) frame depth restored" where lim how) (third r) t)
      (chk (format nil "~A ~D ~(~A~) nested natively" where lim how) (fourth r) t))))

;;; CATCH: tags are the levels; the bottom throws to level TARGET.
(defun nest-catch (n lim target)
  (let ((r (catch n
             (unwind-protect
                  (if (< n lim)
                      (nest-catch (+ n 1) lim target)
                      (progn (setq *bottom-depth* (depth))
                             (throw target (list :to target))))
               (push n *trace*)))))
    ;; The catch at level N returns R either because its own tag was thrown
    ;; to (then it is the receiver) or because a deeper catch returned R
    ;; normally.  Record who received it: the first level to see (:to N).
    (if (and (consp r) (eq (car r) :to) (eql (cadr r) n))
        (list :caught-at n)
        r)))

(defun check-catch (where lim)
  (dolist (target (remove-duplicates (list lim (- lim 3) (floor lim 2) 3 0)))
    (setq *trace* nil)
    (let* ((d0 (depth))
           (v (nest-catch 0 lim target)))
      (chk (format nil "~A ~D catch tag ~D receives" where lim target) v (list :caught-at target))
      ;; levels TARGET..LIM unwound (cleanups LIM down to TARGET); the
      ;; levels above TARGET then returned normally, running theirs too.
      (chk (format nil "~A ~D catch tag ~D cleanups" where lim target)
           (nreverse *trace*) (reverse (iota (+ lim 1))))
      (chk (format nil "~A ~D catch tag ~D depth" where lim target) (= (depth) d0) t)
      (chk (format nil "~A ~D catch nested natively" where lim) (>= *bottom-depth* (+ d0 lim)) t))))

;;; SPECIALS: a LET of *SP* at every level.
(defun nest-special (n lim how k)
  (let ((*sp* n))
    (if (< n lim) (nest-special (+ n 1) lim how k) (bottom-exit how k))))

(defun check-specials (where lim)
  (dolist (how '(:throw :error :return-from :go))
    (ecase how
      (:throw (catch 'deep (nest-special 0 lim :throw nil)))
      (:error (handler-case (nest-special 0 lim :error nil) (error () nil)))
      (:return-from (block out (nest-special 0 lim :fn (lambda () (return-from out nil)))))
      (:go (tagbody (nest-special 0 lim :fn (lambda () (go out))) out)))
    (chk (format nil "~A ~D special restored after ~(~A~)" where lim how) *sp* :outer)
    ;; and binding still works afterwards
    (chk (format nil "~A ~D special rebinds after ~(~A~)" where lim how)
         (let ((*sp* :again)) *sp*) :again)))

;;; HANDLERS.  Even levels handle DEEP-A, odd levels DEEP-B; the bottom
;;; signals one of them, so the innermost level of the right parity catches.
(defun nest-hc (n lim which)
  (if (evenp n)
      (handler-case (nest-hc-body n lim which)
        (deep-a () (list :a-at n)))
      (handler-case (nest-hc-body n lim which)
        (deep-b () (list :b-at n)))))

(defun nest-hc-body (n lim which)
  (unwind-protect
       (if (< n lim)
           (nest-hc (+ n 1) lim which)
           (progn (setq *bottom-depth* (depth)) (error which)))
    (push n *trace*)))

(defun check-handler-case (where lim)
  (dolist (which '(deep-a deep-b))
    (setq *trace* nil)
    (let* ((d0 (depth))
           (v (nest-hc 0 lim which))
           ;; innermost level of the matching parity
           (at (if (eq which 'deep-a)
                   (if (evenp lim) lim (- lim 1))
                   (if (oddp lim) lim (- lim 1)))))
      (chk (format nil "~A ~D handler-case ~(~A~) dispatch" where lim which)
           v (list (if (eq which 'deep-a) :a-at :b-at) at))
      (chk (format nil "~A ~D handler-case ~(~A~) cleanups" where lim which)
           (nreverse *trace*) (reverse (iota (+ lim 1))))
      (chk (format nil "~A ~D handler-case depth" where lim) (= (depth) d0) t)
      (chk (format nil "~A ~D handler-case nested natively" where lim) (>= *bottom-depth* (+ d0 lim)) t))))

(defvar *seen* nil)

(defun nest-hb (n lim exit)
  (handler-bind ((deep-note (lambda (c) (declare (ignore c)) (push n *seen*)))
                 (deep-a (lambda (c) (declare (ignore c))
                           (when (= n 0) (funcall exit)))))
    (unwind-protect
         (if (< n lim)
             (nest-hb (+ n 1) lim exit)
             (progn (setq *bottom-depth* (depth))
                    (signal 'deep-note)          ; declined by everyone
                    (error 'deep-a)))            ; level 0's handler exits
      (push n *trace*))))

(defun check-handler-bind (where lim)
  (setq *trace* nil *seen* nil)
  (let* ((d0 (depth))
         (v (block out (nest-hb 0 lim (lambda () (return-from out :exited))))))
    (chk (format nil "~A ~D handler-bind exit" where lim) v :exited)
    (chk (format nil "~A ~D handler-bind handlers innermost first" where lim)
         (nreverse *seen*) (reverse (iota (+ lim 1))))
    (chk (format nil "~A ~D handler-bind cleanups" where lim)
         (nreverse *trace*) (reverse (iota (+ lim 1))))
    (chk (format nil "~A ~D handler-bind depth" where lim) (= (depth) d0) t)))

;;; OVERFLOW, through the per-window test limit (+HSTACK-EXT-LIMIT-ADDR+,
;;; window +0x5608, a raw count of extension frames -- the same word on
;;; AArch64, +A64-HSTACK-EXT-LIMIT-ADDR+).
(defvar *win-frames* (if (string= (machine-type) "aarch64") 170 512)
  "Handler frames the per-thread window holds before the extension.")
(defvar *entered* 0)
(defvar *d1* 0)               ; frame depth on entering level 0
(defun limit-addr () (+ (modus-int::%tls-self-base) #x10005608))
(defun set-ext-limit (n) (modus-int::%gc-write64 (limit-addr) n))

(defun nest-count (n lim)
  (setq *entered* (+ *entered* 1))
  (when (= n 0) (setq *d1* (depth)))
  (unwind-protect
       (if (< n lim) (nest-count (+ n 1) lim) :bottom)
    (push n *trace*)))

(defun check-overflow (where)
  (setq *trace* nil *entered* 0)
  (set-ext-limit 100)
  (let* ((d0 (depth))
         (v (handler-case (nest-count 0 2000)
              (storage-condition (c) (list :storage (symbol-name (type-of c))
                                           (typep c 'error))))))
    (set-ext-limit 0)
    (chk (format nil "~A overflow signals a storage-condition" where)
         v '(:storage "HANDLER-STACK-EXHAUSTED" nil))
    ;; the level whose push overflowed never ran its body (nor established
    ;; its cleanup); every level before it did.
    (chk (format nil "~A overflow: every pushed frame's cleanup ran" where)
         (length *trace*) (- *entered* 1))
    (chk (format nil "~A overflow: cleanups innermost first" where)
         (nreverse *trace*) (reverse (iota (- *entered* 1))))
    ;; frames 0..611 (512 in the window + the 100 allowed) were stored.
    ;; Level I pushes at depth *D1*+I, so the push that overflowed is the
    ;; one at depth 612: level 612-*D1*, the (613-*D1*)th entered.  (AArch64:
    ;; 170 + 100, so depth 270.)
    (chk (format nil "~A overflow at exactly window + limit" where)
         *entered* (- (+ *win-frames* 101) *d1*))
    (chk (format nil "~A overflow depth restored" where) (= (depth) d0) t))
  ;; and the stack works again, deeper than the old limit, afterwards
  (let ((r (run-uwp 1500 :throw)))
    (chk (format nil "~A after overflow 1500 deep" where) (second r) (reverse (iota 1501)))))

(defun check-specials-loud (where lim)
  "A worker's dynamic-binding stack holds 960 bindings (PER-THREAD DYNAMIC
   BINDINGS, mvm/prelude.lisp): deeper is a LOUD error, not this file's
   subject -- but it is itself an ERROR exit out of ~960 bindings, so the
   outer value must come back after it."
  (let ((v (handler-case (progn (nest-special 0 lim :throw nil) :no-error)
             (error (e) (if (search "binding stack exhausted" (princ-to-string e))
                            :binding-stack-exhausted
                            (princ-to-string e))))))
    (chk (format nil "~A ~D special: binding stack says so" where lim) v :binding-stack-exhausted)
    (chk (format nil "~A ~D special restored after the binding-stack error" where lim) *sp* :outer)
    (chk (format nil "~A ~D special rebinds after it" where lim)
         (let ((*sp* :again)) *sp*) :again)))

(defun check-catch-loud (where lim)
  "CATCH keeps a binding per level on a worker (its tag list), so past the
   binding stack a deep CATCH nest is the same loud error -- an exit that
   must still run every established level's cleanup, innermost first."
  (setq *trace* nil)
  (let* ((d0 (depth))
         (v (handler-case (nest-catch 0 lim 0)
              (error (e) (if (search "binding stack exhausted" (princ-to-string e))
                             :binding-stack-exhausted
                             (princ-to-string e)))))
         (tr (nreverse *trace*)))
    (chk (format nil "~A ~D catch: binding stack says so" where lim) v :binding-stack-exhausted)
    (chk (format nil "~A ~D catch: established levels' cleanups ran" where lim)
         tr (reverse (iota (length tr))))
    (chk (format nil "~A ~D catch: got deep before it" where lim) (> (length tr) 700) t)
    (chk (format nil "~A ~D catch loud depth" where lim) (= (depth) d0) t)))

(defun run-all (where depths)
  (dolist (lim depths)
    (check-cleanups where lim)
    (if (and (equal where "worker") (> lim 850))
        (progn (check-catch-loud where lim)
               (check-specials-loud where lim))
        (progn (check-catch where lim)
               (check-specials where lim)))
    (check-handler-case where lim)
    (check-handler-bind where lim)
    (format t "  ~A ~D: ~D checks so far, ~D failed~%" where lim *checks* *fail*))
  (check-overflow where))

;;; Around both windows' ends: AArch64's 170, x86-64's 512.
(defvar *depths* '(100 169 170 171 497 498 511 512 513 600 800 900 1000 2000 5000))

(format t "~&-- main~%")
(run-all "main" *depths*)

(format t "~&-- worker~%")
(let ((r (sb-thread:join-thread
          (sb-thread:make-thread
           (lambda ()
             ;; The worker's own bindings of the recording specials: a
             ;; worker may not store its objects into main's global cells
             ;; (the shared-store guard).  *SP* is deliberately NOT bound
             ;; here -- its outer value is main's global, as in the symptom.
             (handler-case (let ((*trace* nil) (*seen* nil) (*bottom-depth* 0)
                                 (*entered* 0) (*d1* 0))
                             (run-all "worker" *depths*)
                             :done)
               (error (e) (list :died (princ-to-string e)))))))))
  (chk "worker finished" r :done))

;;; Main's specials are untouched by the worker's.
(chk "main special after worker" *sp* :outer)

;;; NEIGHBOURS.  Worker A nests N deep while worker B, started right after it
;;; (normally the next window block), is parked inside a HANDLER-CASE with a
;;; special bound; then B signals an error its own HANDLER-CASE must catch.
;;; A frame stored past A's window lands in B's block: before the AArch64
;;; fix, N = 200 gave B a MEMORY-FAULT-ERROR and N = 400 killed the process.
(defvar *go* 0)
(defvar *b-ready* 0)
(defvar *nb-ran* 0)
(defun spin-until (sym) (loop (when (eql (symbol-value sym) 1) (return)) (sleep 0.001)))
(defun nest-nb (n lim)
  (unwind-protect (if (< n lim) (nest-nb (+ n 1) lim) (throw 'nb :done))
    (setq *nb-ran* (+ *nb-ran* 1))))

(format t "~&-- neighbours~%")
(dolist (lim '(100 200 400 1000 3000))
  (setq *go* 0 *b-ready* 0)
  (let* ((a (sb-thread:make-thread
             (lambda ()
               (handler-case
                   (let ((*nb-ran* 0))
                     (spin-until '*b-ready*)
                     (let ((v (catch 'nb (nest-nb 0 lim))))
                       (setq *go* 1)
                       (list v *nb-ran*)))
                 (error (e) (setq *go* 1) (list :a-died (princ-to-string e)))))))
         (b (sb-thread:make-thread
             (lambda ()
               (handler-case
                   (let ((*sp* :b))
                     (handler-case
                         (progn (setq *b-ready* 1)
                                (spin-until '*go*)
                                (error "b's own error"))
                       (error () (list :b-caught *sp* (multiple-value-list (values 1 2 3))))))
                 (error (e) (list :b-died (princ-to-string e))))))))
    (chk (format nil "neighbours ~D: A ran every cleanup" lim)
         (sb-thread:join-thread a) (list :done (+ lim 1)))
    (chk (format nil "neighbours ~D: B's own handler caught its error" lim)
         (sb-thread:join-thread b) '(:b-caught :b (1 2 3)))))

(format t "~&~D checks, ~D failed~%" *checks* *fail*)
(format t "~A~%" (if (= *fail* 0) "DEEP-UNWIND: PASS" "DEEP-UNWIND: FAIL"))
