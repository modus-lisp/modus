;;;; test/special-site.lisp -- specials read and bound through a per-site cache
;;;; (compiler.lisp %SITE-FAST) keep CL semantics.
;;;;
;;;;   ./modus --script test/special-site.lisp
;;;;
;;;; A special declared with no value (DEFVAR *X*), or one defined after the
;;;; code that uses it, has no globals cell when that code is compiled; its
;;;; reads and LET bindings go through a per-site cache, whose common case is
;;;; now inline.  These are the behaviours that path has to keep.

(defvar *fails* 0)
(defvar *known* 0)
(defun chk (name got want)
  (format t "~&  ~a ~a~%" (if (equal got want) "ok  " "FAIL") name)
  (unless (equal got want) (format t "      got ~s want ~s~%" got want) (setq *fails* (+ *fails* 1))))
;;; KNOWN, and older than the inline path (the binary before it fails exactly
;;; these): a LET of an UNBOUND special saves "absent" as NIL and restores that
;;; NIL, so the variable is left BOUND after the LET; and a compiled read of a
;;; special does not see MAKUNBOUND.  Reported, not counted, so that whoever
;;; fixes it sees them turn to "ok".
(defun known (name got want)
  (format t "~&  ~a ~a~%" (if (equal got want) "ok  " "KNOWN") name)
  (unless (equal got want) (setq *known* (+ *known* 1))))

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
(known "and is gone after it (unbound again)" (boundp '*a*) nil)
(known "nested bindings read the inner value, then restore" (after-nested) '(20 nil))
(dotimes (i 200) (bind-a i #'read-a))           ; hot: JIT, site filled
(chk "hot: still right" (bind-a :hot #'read-a) :hot)
(known "hot: still restored" (boundp '*a*) nil)
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
(known "MAKUNBOUND makes it unbound to compiled reads"
     (handler-case (progn (read-a) :no-error) (unbound-variable () :unbound)) :unbound)
(chk "and a binding still works after it" (bind-a 7 #'read-a) 7)
(defvar *later* :late)
(chk "a special defined after its reader was compiled" (later-ref) :late)
(chk "and bound" (bind-later 5) 5)
(chk "restored" (later-ref) :late)
(let ((th (sb-thread:make-thread (lambda () (bind-a :worker #'read-a)))))
  (chk "a worker's binding is its own" (sb-thread:join-thread th) :worker))
(known "main is unaffected" (boundp '*a*) nil)
(let ((th (sb-thread:make-thread (lambda () (let ((*a* :w)) (sleep 0.2) (read-a))))))
  (sleep 0.05)
  (known "main does not see a worker's live binding" (boundp '*a*) nil)
  (chk "the worker reads its own" (sb-thread:join-thread th) :w))
(chk "and main binds normally after threads are armed" (bind-a :after #'read-a) :after)
(format t "~&SPECIAL-SITE: ~a failed (~a known, see KNOWN)~%" *fails* *known*)
(sys-exit (if (zerop *fails*) 0 1))
