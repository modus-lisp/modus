;;;; test/special-site.lisp -- specials read and bound through a per-site cache
;;;; (compiler.lisp %SITE-FAST) keep CL semantics.
;;;;
;;;;   ./modus --script test/special-site.lisp
;;;;
;;;; A special declared with no value (DEFVAR *X*), or one defined after the
;;;; code that uses it, has no globals cell when that code is compiled; its
;;;; reads and LET bindings go through a per-site cache, whose common case is
;;;; now inline.  These are the behaviours that path has to keep.
;;;;
;;;; Unbound is a value a cell can hold (prelude %UNBOUND-MARKER): MAKUNBOUND
;;;; writes it, and a LET or PROGV of an unbound special saves it and puts it
;;;; back.  Before that, such a LET left the variable bound to NIL and
;;;; MAKUNBOUND did nothing; the checks that say "unbound" test exactly that.

(defvar *fails* 0)
(defun chk (name got want)
  (format t "~&  ~a ~a~%" (if (equal got want) "ok  " "FAIL") name)
  (unless (equal got want) (format t "      got ~s want ~s~%" got want) (setq *fails* (+ *fails* 1))))
(defvar *a*)                         ; no value, no cell at compile time
(defvar *b*)
(defun read-a () *a*)
(defun bind-a (v f) (let ((*a* v)) (funcall f)))
(defun nested () (let ((*a* 1)) (let ((*a* (+ *a* 1))) (let ((*a* (* *a* 10))) *a*))))
(defun after-nested () (list (nested) (boundp '*a*)))
(defun thrower () (let ((*a* :inner)) (throw 'out (read-a))))
(defun swap () (let ((*a* *b*) (*b* *a*)) (list *a* *b*)))
(defun later-ref () *later*)        ; *LATER* is defined below, after this compiles
(defun bind-later (v) (let ((*later* v)) (declare (special *later*)) (later-ref)))

(chk "unbound: reading signals UNBOUND-VARIABLE"
     (handler-case (progn (read-a) :no-error) (unbound-variable () :unbound)) :unbound)
(chk "a binding is seen by a callee" (bind-a 42 #'read-a) 42)
(chk "and is gone after it (unbound again)" (boundp '*a*) nil)
(chk "nested bindings read the inner value, then restore" (after-nested) '(20 nil))
(dotimes (i 200) (bind-a i #'read-a))           ; hot: JIT, site filled
(chk "hot: still right" (bind-a :hot #'read-a) :hot)
(chk "hot: still restored" (boundp '*a*) nil)
(setq *a* :global)
(chk "a global value once set" (read-a) :global)
(chk "a binding shadows it" (bind-a :bound #'read-a) :bound)
(chk "and restores it" (read-a) :global)
(chk "THROW out of a binding returns the inner value" (catch 'out (thrower)) :inner)
(chk "and restores the outer one" (read-a) :global)
(chk "an error out of a binding restores it"
     (progn (handler-case (bind-a :err (lambda () (error "x"))) (error () nil)) (read-a)) :global)
(setq *b* :bee)
(chk "parallel LET of two specials sees the old values" (swap) '(:bee :global))
(chk "and restores both" (list *a* *b*) '(:global :bee))
(makunbound '*a*)
(chk "MAKUNBOUND makes it unbound to compiled reads"
     (handler-case (progn (read-a) :no-error) (unbound-variable () :unbound)) :unbound)
(chk "and a binding still works after it" (bind-a 7 #'read-a) 7)
(defvar *later* :late)
(chk "a special defined after its reader was compiled" (later-ref) :late)
(chk "and bound" (bind-later 5) 5)
(chk "restored" (later-ref) :late)
(let ((th (sb-thread:make-thread (lambda () (bind-a :worker #'read-a)))))
  (chk "a worker's binding is its own" (sb-thread:join-thread th) :worker))
(chk "main is unaffected" (boundp '*a*) nil)
(let ((th (sb-thread:make-thread (lambda () (let ((*a* :w)) (sleep 0.2) (read-a))))))
  (sleep 0.05)
  (chk "main does not see a worker's live binding" (boundp '*a*) nil)
  (chk "the worker reads its own" (sb-thread:join-thread th) :w))
(chk "and main binds normally after threads are armed" (bind-a :after #'read-a) :after)
;;; Unbound through every other door.
(defvar *c*)
(defvar *d* :dee)
(defun read-c () *c*)
(defun read-d () *d*)                ; *D* has a cell: a baked read
(chk "PROGV with too few values leaves the rest unbound"
     (progv '(*c* *d*) '(1)
       (list (read-c) (boundp '*d*)
             (handler-case (progn (read-d) :no-error) (unbound-variable () :unbound))))
     '(1 nil :unbound))
(chk "and restores both" (list (boundp '*c*) (read-d)) '(nil :dee))
(makunbound '*d*)
(chk "MAKUNBOUND of a baked-cell special: BOUNDP" (boundp '*d*) nil)
(chk "a baked read signals"
     (handler-case (progn (read-d) :no-error) (unbound-variable () :unbound)) :unbound)
(chk "naming the variable"
     (handler-case (read-d) (unbound-variable (e) (cell-error-name e))) '*d*)
(defvar *never*)
(chk "SYMBOL-VALUE signals, as for a never-bound one"
     (mapcar (lambda (s) (handler-case (progn (symbol-value s) :no-error)
                           (unbound-variable () :unbound)))
             '(*d* *never*))
     '(:unbound :unbound))
(chk "a LET of it binds, then it is unbound again"
     (list (let ((*d* 3)) (read-d)) (boundp '*d*)) '(3 nil))
(defvar *d* :again)
(chk "DEFVAR initialises it again" (read-d) :again)
(setq *c* 5)
(chk "SETQ makes it bound" (list (boundp '*c*) (read-c)) '(t 5))
(chk "MAKUNBOUND returns the symbol" (makunbound '*c*) '*c*)
(chk "MAKUNBOUND of an already-unbound special is harmless"
     (progn (makunbound '*c*) (boundp '*c*)) nil)
(dotimes (i 200) (let ((*c* i)) (read-c)))      ; hot LET of an unbound special
(chk "a hot LET of an unbound special leaves it unbound" (boundp '*c*) nil)
(let ((th (sb-thread:make-thread (lambda () (let ((*c* :w)) (list (boundp '*c*) (read-c)))))))
  (chk "a worker's binding of an unbound special is bound on the worker"
       (sb-thread:join-thread th) '(t :w)))
(chk "and main still sees it unbound" (boundp '*c*) nil)
(chk "a printer special survives a LET" (let ((*print-base* 16)) (format nil "~a" 255)) "FF")
(format t "~&SPECIAL-SITE: ~a failed~%" *fails*)
(sys-exit (if (zerop *fails*) 0 1))
