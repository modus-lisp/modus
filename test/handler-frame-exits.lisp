;;;; test/handler-frame-exits.lisp -- a lexical exit out of a frame-establishing
;;;; form must pop that form's handler frame.
;;;;
;;;;   ./modus --script test/handler-frame-exits.lisp
;;;;
;;;; HANDLER-CASE (and CATCH, IGNORE-ERRORS, RESTART-CASE, which compile through
;;;; it) pushes a frame on the runtime handler stack and pops it on the normal
;;;; path.  A RETURN-FROM / RETURN / GO compiled inside the body is a direct
;;;; branch to the block's exit label, and it used to skip the pop: each
;;;; execution leaked one frame.  On bare aarch64 a worker thread's frames sit
;;;; just below the next thread's per-thread window, so a background git sync on
;;;; the Pi Zero (~300 leaked frames per repository) wrote over another
;;;; thread's MV buffer and binding-stack words and the board faulted.
;;;;
;;;; Each shape runs N times and the depth must come back where it started --
;;;; interpreted, through COMPILE, and as a DEFUN called often enough to go
;;;; native.  The values must still be right, and an error signalled after the
;;;; exits must still reach the handler that is really armed (a leaked frame
;;;; is a stale one, and an unwind into it lands on a dead stack).
;;;;
;;;; Measured on the pre-fix x64 CLI: every exit shape below leaked exactly N
;;;; (the control, a plain normal exit, 0).

(defvar *fails* 0)
(defvar *checks* 0)
(defvar *n* 100)

(defun check (name ok &optional detail)
  (incf *checks*)
  (unless ok (incf *fails*))
  (format t "~:[FAIL~;ok  ~] ~a~@[ ~s~]~%" ok name (unless ok detail)))

(defun depth () (modus-int::%hc-depth))

(defmacro shapes ()
  ''((:normal         (handler-case 1 (error () 2)) 1)
     (:hc-return-from (block b (handler-case (return-from b 1) (error () 2))) 1)
     (:hc-values      (multiple-value-list
                       (block b (handler-case (return-from b (values 1 2 3)) (error () 2))))
                      (1 2 3))
     (:hc-loop-return (loop (handler-case (return 1) (error () 2))) 1)
     (:hc-dolist      (dolist (x '(1 2)) (handler-case (return x) (error () 2))) 1)
     (:hc-go          (let ((r 0)) (tagbody (handler-case (go out) (error () 2)) out (setq r 1)) r) 1)
     (:hc-nested      (block b (handler-case (handler-case (return-from b 1) (type-error () 3))
                                 (error () 2))) 1)
     (:hc-in-uwp      (let ((c 0)) (block b (unwind-protect
                                                 (handler-case (return-from b c) (error () 2))
                                              (setq c 9)))) 0)
     (:hc-special     (block b (let ((*print-base* 10)) (handler-case (return-from b 1) (error () 2)))) 1)
     (:catch          (block b (catch 'x (return-from b 1))) 1)
     (:ignore-errors  (block b (ignore-errors (return-from b 1))) 1)
     (:restart-case   (block b (restart-case (return-from b 1) (r () 2))) 1)
     (:simple-restart (block b (with-simple-restart (r "r") (return-from b 1))) 1)
     (:after-error    (handler-case (progn (block b (handler-case (return-from b 1) (error () 2)))
                                           (error "late"))
                        (error () :caught-outside)) :caught-outside)))

;;; 1. Interpreted: each shape as its own toplevel form.
(dolist (s (shapes))
  (destructuring-bind (name form want) s
    (let* ((before (depth))
           (got (let ((r nil)) (dotimes (i *n* r) (setq r (eval form)))))
           (leak (- (depth) before)))
      (check (format nil "eval    ~(~a~) leak ~d" name leak) (= leak 0) leak)
      (check (format nil "eval    ~(~a~) value" name) (equal got want) got))))

;;; 2. COMPILE: the whole shape inside a compiled function.
(dolist (s (shapes))
  (destructuring-bind (name form want) s
    (let* ((f (compile nil `(lambda (n) (let ((r nil)) (dotimes (i n r) (setq r ,form))))))
           (before (depth))
           (got (funcall f *n*))
           (leak (- (depth) before)))
      (check (format nil "compile ~(~a~) leak ~d" name leak) (= leak 0) leak)
      (check (format nil "compile ~(~a~) value" name) (equal got want) got))))

;;; 3. A DEFUN called often enough for the hot JIT to build it.
(defun hot-exit (x) (block b (handler-case (if x (return-from b x) 0) (error () 2))))
(let ((before (depth)))
  (dotimes (i 500) (hot-exit i))
  (check (format nil "defun   hot-exit leak ~d" (- (depth) before)) (= (depth) before)))

;;; 4. After all of that, an error at toplevel still reaches its handler.
(check "outer handler still armed"
       (eq (handler-case (error "x") (error () :ok)) :ok))

(format t "~&handler-frame-exits: ~d checks, ~d failed~%" *checks* *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
