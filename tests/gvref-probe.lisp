;;;; gvref-probe.lisp — the probe that gates the AOT special-variable cell cache.
;;;;
;;;; Two questions, and they are not the same question.  The FIRST is whether
;;;; caching a global's cell changed what a read MEANS: unbound stays unbound
;;;; and does not become NIL, BOUNDP still answers about the binding rather
;;;; than about the cache, and a global born at runtime by SETQ is found.  The
;;;; SECOND is whether a dynamic binding still reaches a callee, still unwinds
;;;; on an error or a THROW, and still absorbs a SETQ made inside it.
;;;;
;;;; Run it against SBCL and against Modus and diff the two outputs.  Four of
;;;; the unbound answers differ from SBCL and did so before this work as well
;;;; -- SYMBOL-VALUE of an absent name returns NIL, MAKUNBOUND is a no-op, and
;;;; PROGV leaves its bindings behind.  What the probe gates is that the list
;;;; of differences does not GROW: Modus-before and Modus-after must be
;;;; identical, line for line.
;;;;
;;;;   sbcl --script tests/gvref-probe.lisp
;;;;   ./modus --load tests/gvref-probe.lisp
;;;;   MODUS_NO_JIT=1 ./modus --load tests/gvref-probe.lisp   (x64; on the
;;;;     aarch64 CLI that variable does not take -- use (setq *use-jit* nil))
(defun try (th) (handler-case (list :val (funcall th))
                  (unbound-variable (c) (list :unbound (cell-error-name c)))
                  (error (c) (list :error (type-of c)))))
(defvar *bound-1* 17)
(format t "P1 bound-read      ~A~%" (try (lambda () *bound-1*)))
(format t "P2 boundp-bound    ~A~%" (boundp '*bound-1*))
(format t "P3 boundp-unbound  ~A~%" (boundp '*never-ever-set-xyz*))
(format t "P4 unbound-read    ~A~%" (try (lambda () (symbol-value '*never-ever-set-xyz*))))
(format t "P5 name-eq         ~A~%"
        (let ((r (try (lambda () (symbol-value '*never-ever-set-xyz*)))))
          (if (eq (car r) :unbound) (eq (cadr r) '*never-ever-set-xyz*) :no-signal)))
(setq *runtime-born-abc* 99)
(format t "P6 setq-born-read  ~A~%" (try (lambda () (symbol-value '*runtime-born-abc*))))
(format t "P7 setq-born-bndp  ~A~%" (boundp '*runtime-born-abc*))
(defvar *mk* 5)
(makunbound '*mk*)
(format t "P8 after-makunbound boundp=~A read=~A~%" (boundp '*mk*) (try (lambda () (symbol-value '*mk*))))
(format t "P9 progv           ~A~%"
        (progv '(*pv-a* *pv-b*) '(1 2) (list (symbol-value '*pv-a*) (symbol-value '*pv-b*))))
(format t "P10 progv-after    ~A~%" (boundp '*pv-a*))

(defvar *x* :outer)
(defun rd () *x*)
(defun rd2 () (list *x* *x*))
(format t "D1 plain           ~A~%" (rd))
(format t "D2 let-callee      ~A~%" (let ((*x* :inner)) (rd)))
(format t "D3 nested          ~A~%" (let ((*x* :a)) (let ((*x* :b)) (rd))))
(format t "D4 after-nested    ~A~%" (let ((*x* :a)) (let ((*x* :b)) (rd)) (rd)))
(format t "D5 restored        ~A~%" (rd))
(format t "D6 setq-in-binding ~A~%"
        (list (let ((*x* :i)) (setq *x* :set) (rd)) (rd)))
(format t "D7 unwind-by-error ~A~%"
        (list (handler-case (let ((*x* :err)) (error "boom")) (error (c) :caught)) (rd)))
(format t "D8 progv-callee    ~A~%" (progv '(*x*) '(:pv) (rd)))
(format t "D9 after-progv     ~A~%" (rd))
(format t "D10 two-reads      ~A~%" (let ((*x* :two)) (rd2)))
(format t "D11 throw-unwind   ~A~%"
        (list (catch 'tag (let ((*x* :thr)) (throw 'tag :thrown))) (rd)))
(defvar *acc* nil)
(defun push-acc (v) (setq *acc* (cons v *acc*)))
(dolist (i '(1 2 3)) (push-acc i))
(format t "D12 setq-accum     ~A~%" *acc*)
(format t "D13 mv             ~A~%" (multiple-value-list (rd)))
(format t "D14 mv-bind        ~A~%" (multiple-value-bind (a b) (rd) (list a b)))
