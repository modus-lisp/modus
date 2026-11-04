;;;; test/struct-slot-value.lisp -- SLOT-VALUE and friends on DEFSTRUCT instances.
;;;;
;;;; They answered NIL for every slot of a structure (and SETF did nothing), so a program that
;;;; did arithmetic on what it read crashed: on the bare-metal board a CPU fault, every core.
;;;;   ./modus --script test/struct-slot-value.lisp      ; exit 0 = pass
(defstruct ssv x (y 4))
(defstruct (ssv2 (:include ssv) (:conc-name s2-)) z)
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want) (format t "ok    ~a~%" name)
      (progn (incf *fails*) (format t "FAIL  ~a: got ~s want ~s~%" name got want))))
(let ((a (make-ssv :x 3)) (b (make-ssv2 :x 1 :z 9)))
  (check "read" (list (slot-value a 'x) (slot-value a 'y)) '(3 4))
  (check "read through :include" (list (slot-value b 'x) (slot-value b 'y) (slot-value b 'z)) '(1 4 9))
  (setf (slot-value a 'x) 30 (slot-value b 'z) 90)
  (check "setf writes the slot" (list (ssv-x a) (s2-z b)) '(30 90))
  (check "slot-boundp" (slot-boundp a 'y) t)
  (check "slot-exists-p" (list (slot-exists-p b 'z) (slot-exists-p a 'z)) '(t nil))
  (check "a missing slot signals" (handler-case (progn (slot-value a 'nope) :no-error) (error () :error)) :error)
  (check "setf of a missing slot signals"
         (handler-case (progn (setf (slot-value a 'nope) 1) :no-error) (error () :error)) :error))
(format t "~&struct-slot-value: ~a~%" (if (zerop *fails*) "PASS" "FAIL"))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
