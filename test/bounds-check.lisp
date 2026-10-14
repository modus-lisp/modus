;;;; test/bounds-check.lisp -- out-of-range element access signals and never
;;;; touches memory past the array.
;;;;
;;;;   ./modus --script test/bounds-check.lisp
;;;;   BOUNDS_MODE=interp ./modus --script test/bounds-check.lisp
;;;;
;;;; Before compiler.lisp's %EMIT-INDEX-CHECK every one of these READ whatever
;;;; followed the vector and every write landed on it.  Each case runs inside
;;;; HANDLER-CASE and must come back :TYPE-ERROR; a GUARD vector allocated
;;;; immediately after the target must be bit-for-bit unchanged afterwards (a
;;;; write that went through anyway would show there), and in-range accesses
;;;; must still return the right values.

(defvar *interp* (equal (sb-ext:posix-getenv "BOUNDS_MODE") "interp"))
(when *interp* (setq *jit-inhibit* t))

(defvar *checks* 0)
(defvar *fails* 0)
(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (unless (equal got want)
    (setq *fails* (+ *fails* 1))
    (format t "~&FAIL ~a~%   got  ~s~%   want ~s~%" name got want)))

(defmacro trap (form)
  `(handler-case (progn ,form :no-error)
     (type-error () :type-error)
     (error () :other-error)))

;;; One function per access shape, so each is compiled through its own path.
(defun rd (v i) (aref v i))
(defun wr (v i x) (setf (aref v i) x))
(defun rd-sv (v i) (svref v i))
(defun rd-ch (s i) (char s i))
(defun rd-k7 (v) (aref v 7))                 ; constant index
(defun wr-k7 (v x) (setf (aref v 7) x))
(defun rd-u8 (v i) (declare (type (simple-array (unsigned-byte 8) (*)) v)) (aref v i))
(defun wr-u8 (v i x) (declare (type (simple-array (unsigned-byte 8) (*)) v)) (setf (aref v i) x))
(defun rd-gen (v i) (declare (type (simple-array t (*)) v)) (aref v i))
(defun wr-gen (v i x) (declare (type (simple-array t (*)) v)) (setf (aref v i) x))
(defun rd-local (i) (let ((w (make-array 4 :element-type '(unsigned-byte 32) :initial-element 9))) (aref w i)))

(defun guarded (make)
  "A target and the guard allocated right after it."
  (let* ((target (funcall make)) (guard (make-array 8 :initial-element 12345)))
    (list target guard)))

(defun guard-ok-p (g) (every (lambda (x) (eql x 12345)) g))

(defun run-all ()
  (let* ((p (guarded (lambda () (make-array 4 :initial-element 1))))
         (v (first p)) (g (second p)))
    (chk "word rd in range" (rd v 3) 1)
    (chk "word rd 4" (trap (rd v 4)) :type-error)
    (chk "word rd 100" (trap (rd v 100)) :type-error)
    (chk "word rd -1" (trap (rd v -1)) :type-error)
    (chk "word rd bignum" (trap (rd v (expt 2 70))) :type-error)
    (chk "word rd non-integer" (trap (rd v :x)) :type-error)
    (chk "word wr 4" (trap (wr v 4 99)) :type-error)
    (chk "word wr 9" (trap (wr v 9 99)) :type-error)
    (chk "word wr -1" (trap (wr v -1 99)) :type-error)
    (chk "word wr in range" (progn (wr v 0 5) (rd v 0)) 5)
    (chk "svref 4" (trap (rd-sv v 4)) :type-error)
    (chk "const 7 rd" (trap (rd-k7 v)) :type-error)
    (chk "const 7 wr" (trap (wr-k7 v 99)) :type-error)
    (chk "word guard untouched" (guard-ok-p g) t))
  (let* ((p (guarded (lambda () (make-array 5 :element-type '(unsigned-byte 8) :initial-element 7))))
         (v (first p)) (g (second p)))
    (chk "u8 rd in range" (rd v 4) 7)
    (chk "u8 rd 5" (trap (rd v 5)) :type-error)
    (chk "u8 wr 5" (trap (wr v 5 1)) :type-error)
    (chk "u8 wr 40" (trap (wr v 40 1)) :type-error)
    (chk "u8 declared rd 5" (trap (rd-u8 v 5)) :type-error)
    (chk "u8 declared wr 5" (trap (wr-u8 v 5 1)) :type-error)
    (chk "u8 declared in range" (progn (wr-u8 v 2 200) (rd-u8 v 2)) 200)
    (chk "u8 guard untouched" (guard-ok-p g) t))
  (let* ((p (guarded (lambda () (make-array 3 :initial-element 0))))
         (v (first p)) (g (second p)))
    (chk "declared generic rd 3" (trap (rd-gen v 3)) :type-error)
    (chk "declared generic wr 3" (trap (wr-gen v 3 1)) :type-error)
    (chk "declared generic in range" (progn (wr-gen v 2 8) (rd-gen v 2)) 8)
    (chk "generic guard untouched" (guard-ok-p g) t))
  (let ((s (make-string 3 :initial-element #\a)))
    (chk "char in range" (rd-ch s 2) #\a)
    (chk "char 3" (trap (rd-ch s 3)) :type-error))
  (chk "inferred local rd 4" (trap (rd-local 4)) :type-error)
  (chk "inferred local in range" (rd-local 3) 9)
  (let ((f (make-array 2 :element-type 'single-float :initial-element 1.5)))
    (chk "f32 in range" (rd f 1) 1.5)
    (chk "f32 rd 2" (trap (rd f 2)) :type-error)
    (chk "f32 wr 2" (trap (wr f 2 2.5)) :type-error))
  (list *checks* *fails*))

(defvar *result* (if *interp* (run-all) (jit-eager '(run-all))))
(format t "~&BOUNDS ~a: ~a checks, ~a failed~%" (if *interp* "interp" "jit")
        (car *result*) (cadr *result*))
(sys-exit (if (zerop (cadr *result*)) 0 1))
