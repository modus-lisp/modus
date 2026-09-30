;;;; hosted-arena-evac.lisp — THE LOCK ARENA IS COLLECTED, NOT IMMORTAL.
;;;;
;;;;   ./modus --script test/hosted-arena-evac.lisp
;;;;
;;;; Locked runtime sections (INTERN, the symbol tables) allocate in per-CPU
;;;; slices of a lock arena carved off region 0 (net/hosted-sync.lisp, B-LITE).
;;;; Nothing used to collect it: ~31-95 bytes of it went per locked INTERN, and
;;;; a worker interning in a loop filled the 31 MB arena and then corrupted
;;;; memory on the fallback path.  Now a stop-the-world region-0 collection
;;;; EVACUATES it (translate-aarch64 / translate-x64, EVACUATING THE LOCK
;;;; ARENA), and a worker that finds it low collects region 0 on main's behalf
;;;; (%GC-COLLECT-REGION-0) — main picks up its new frontier when it wakes.
;;;;
;;;; WHAT WOULD MAKE A PASS MEANINGLESS, AND WHAT STOPS IT.
;;;;   - No evacuation happened: every collection is checked to have REWOUND
;;;;     the arena (used bytes after < used bytes before), and region 0's
;;;;     collection count must rise.
;;;;   - The symbols moved but nothing looked them up again: every symbol the
;;;;     worker interned is re-interned after every collection and must come
;;;;     back EQ, and main checks them too at the end.
;;;;   - Main's heap was the one broken: main allocates all through the
;;;;     worker's collections of ITS region, and checks a list built before.

(defvar *fail* 0)
(defvar *checks* 0)
(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "  ok   ~A = ~A~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A: got ~A want ~A~%" name got want))))
(defun chk-true (name v)
  (setq *checks* (+ *checks* 1))
  (if v (format t "  ok   ~A~%" name)
      (progn (setq *fail* (+ *fail* 1)) (format t "  FAIL ~A~%" name))))

(%sb-threads-up)
(defun r0gcs () (%gc-meta-read (+ (%gc-region-0) #x20) (%gc-meta-scale)))
(defun arena-used () (- (%rt-arena-alloc) (%rt-arena-base)))
(defun list-sum (l) (let ((s 0)) (dolist (x l s) (setq s (+ s x)))))

(defvar *worker-syms* nil)

(defun worker ()
  "Intern 3000 fresh symbols; after every 1000, collect region 0 and check
   that every one of them still interns to itself."
  (let ((mine nil) (bad 0) (log nil))
    (dotimes (i 3000)
      (setq mine (cons (intern (format nil "AEV-S~D" i) :cl-user) mine))
      (when (zerop (mod (+ i 1) 1000))
        (let ((before (arena-used)))
          (%gc-collect-region-0)
          (setq log (cons (list before (arena-used)) log)))
        (dolist (s mine)
          (unless (eq (intern (symbol-name s) :cl-user) s) (setq bad (+ bad 1))))))
    ;; RETURNED, not SETQ'd into a global: a worker storing its own objects
    ;; into shared memory is refused on x86-64 (the shared-store guard);
    ;; JOIN-THREAD carries the result back as a message.
    (list bad (reverse log) mine)))

(format t "~%=== A WORKER COLLECTS REGION 0, EVACUATING THE ARENA ====~%")
(let ((keep (let ((l nil)) (dotimes (i 500) (setq l (cons i l))) l))
      (g0 (r0gcs))
      (th (sb-thread:make-thread
            (lambda () (handler-case (worker) (error (e) (list :err (%escape-describe e))))))))
  ;; main allocates in region 0 all the while
  (loop
    (when (not (sb-thread:thread-alive-p th)) (return nil))
    (let ((junk nil)) (dotimes (j 200) (setq junk (cons j junk)))))
  (let ((r (sb-thread:join-thread th)))
    (when (and (consp r) (integerp (car r))) (setq *worker-syms* (caddr r)))
    (format t "  worker: ~S~%" (if (consp r) (list (car r) (cadr r)) r))
    (chk-true "the worker finished without an error" (and (consp r) (integerp (car r))))
    (when (and (consp r) (integerp (car r)))
      (chk "symbols that no longer intern to themselves" (car r) 0)
      (chk "collections the worker made" (length (cadr r)) 3)
      (chk-true "every collection rewound the arena"
                (every (lambda (p) (< (cadr p) (car p))) (cadr r))))
    (chk-true "region 0 collected (its count rose)" (> (r0gcs) g0))
    (chk "main's list, built before, still sums right" (list-sum keep) 124750)
    (chk-true "main finds every symbol the worker interned"
              (every (lambda (s) (eq (intern (symbol-name s) :cl-user) s)) *worker-syms*))))

(format t "~%=== THE ARENA REFILLS ITSELF UNDER A WORKER'S INTERNS ===~%")
;; No explicit collection: the lock's own refill check (%RT-ARENA-REFILL-CHECK)
;; must keep a long run of locked interns off the fallback path.
(let ((fb0 (%rt-arena-fallbacks))
      (th (sb-thread:make-thread
            (lambda ()
              (let ((bad 0) (last nil))
                (dotimes (i 60000)
                  (let ((s (intern (format nil "AEV-R~D" (mod i 4000)) :cl-user)))
                    (when (and last (zerop (mod i 4000)))
                      (unless (eq (intern (symbol-name last) :cl-user) last) (setq bad (+ bad 1))))
                    (setq last s)))
                bad)))))
  (loop
    (when (not (sb-thread:thread-alive-p th)) (return nil))
    (let ((junk nil)) (dotimes (j 200) (setq junk (cons j junk)))))
  (chk "bad lookups over 60000 locked interns" (sb-thread:join-thread th) 0)
  (chk "arena fallbacks (the corrupting path)" (- (%rt-arena-fallbacks) fb0) 0)
  (chk-true "the arena is not full" (< (arena-used) (- (%rt-arena-end) (%rt-arena-base)))))

(format t "~%~D checks, ~D failed~%" *checks* *fail*)
(format t "~A~%" (if (zerop *fail*) "LOCK ARENA EVACUATION: PASS" "LOCK ARENA EVACUATION: FAIL"))
