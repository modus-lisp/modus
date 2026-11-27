;;;; test/undefined-function-warning.lisp — EVAL and COMPILE warn, as SBCL
;;;; does, of a call to a function that is not defined.
;;;; Run: ./modus --load test/undefined-function-warning.lisp
;;;; Prints one line per check and ALL PASS or N FAILED last.
;;;;
;;;; A loader that fetches definitions on demand needs the warning BEFORE the
;;;; form runs: a compiled call to a name that is later defined as a MACRO stays
;;;; a call, and evaluates the macro's arguments.  The warning is SBCL's shape --
;;;; a SIMPLE-CONDITION STYLE-WARNING whose format arguments are (:FUNCTION name).

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*))
  (finish-output))

(defun undefined-names (thunk)
  "The names THUNK's compiles warned of, in order, and whether THUNK's body ran."
  (let ((names '()))
    (handler-bind ((style-warning
                     (lambda (w)
                       (let ((args (and (typep w 'simple-condition)
                                        (simple-condition-format-arguments w))))
                         (when (and (consp args) (eq (first args) :function))
                           (push (second args) names)))
                       (muffle-warning w))))
      (ignore-errors (funcall thunk)))
    (reverse names)))

(defun uw-defined () 1)

(check "a DEFUN calling an undefined function warns of it"
       (equal (undefined-names (lambda () (eval '(defun uw-a () (uw-nowhere-1 2)))))
              '(uw-nowhere-1)))
(check "once per name, in the order called"
       (equal (undefined-names (lambda () (eval '(defun uw-b () (uw-nowhere-2) (uw-nowhere-3) (uw-nowhere-2)))))
              '(uw-nowhere-2 uw-nowhere-3)))
(check "a defined function is not warned of"
       (null (undefined-names (lambda () (eval '(defun uw-c () (uw-defined)))))))
(check "nor the function the form defines, called from itself"
       (null (undefined-names (lambda () (eval '(defun uw-d (n) (if (> n 0) (uw-d (- n 1)) 0)))))))
(check "nor one a later form of the same FILE defines once that form is in"
       (null (undefined-names (lambda () (eval '(defun uw-e () 2)) (eval '(defun uw-e2 () (uw-e)))))))
(check "nor a FLET or LABELS function"
       (null (undefined-names (lambda () (eval '(defun uw-f () (flet ((uw-local () 3)) (uw-local))))))))
(check "COMPILE of a lambda warns too"
       (equal (undefined-names (lambda () (compile nil '(lambda () (uw-nowhere-4)))))
              '(uw-nowhere-4)))
(check "the warning comes BEFORE the form runs"
       (let ((ran nil))
         (declare (special ran))
         (handler-case
             (handler-bind ((style-warning (lambda (w) (declare (ignore w)) (throw 'before (symbol-value 'ran)))))
               (catch 'before
                 (eval '(let () (setq ran t) (uw-nowhere-5)))))
           (error () :error))
         (not (symbol-value 'ran))))
(check "it is a STYLE-WARNING and a SIMPLE-CONDITION"
       (let ((w nil))
         (handler-bind ((style-warning (lambda (c) (setq w c) (muffle-warning c))))
           (eval '(defun uw-g () (uw-nowhere-6))))
         (and (typep w 'style-warning) (typep w 'simple-condition))))
(check "and muffled, the form still runs and defines its function"
       (progn (handler-bind ((style-warning #'muffle-warning))
                (eval '(defun uw-h () (uw-nowhere-7))))
              (fboundp 'uw-h)))

(check "WITH-COMPILATION-UNIT holds its warnings to the end, for names still undefined then"
       (equal (undefined-names (lambda ()
                                 (with-compilation-unit ()
                                   (eval '(defun uw-i () (uw-j) (uw-nowhere-8)))
                                   (eval '(defun uw-j () 1)))))
              '(uw-nowhere-8)))
(check "and nested, says nothing until the outer one ends"
       (let ((inner-end :unset))
         (and (equal (undefined-names
                      (lambda ()
                        (with-compilation-unit ()
                          (setq inner-end
                                (undefined-names
                                 (lambda () (with-compilation-unit () (eval '(defun uw-k () (uw-nowhere-9))))))))))
                     '(uw-nowhere-9))
              (null inner-end))))
(check "a unit returns its body's values"
       (equal (multiple-value-list (with-compilation-unit () (values 1 2))) '(1 2)))
(check "LOAD of source warns form by form, a forward reference included"
       (let ((path (format nil "/tmp/uw-fwd-~d.lisp" (random 1000000))))
         (with-open-file (o path :direction :output :if-exists :supersede)
           (write-string "(defun uw-l () (uw-m) (uw-nowhere-10)) (defun uw-m () 1)" o))
         (prog1 (equal (undefined-names (lambda () (load path))) '(uw-m uw-nowhere-10))
           (delete-file path))))
(check "COMPILE-FILE is a unit: only what is still undefined at its end"
       (let* ((path (format nil "/tmp/uw-fwd-~d.lisp" (random 1000000)))
              (out (concatenate 'string path ".fasl")))
         (with-open-file (o path :direction :output :if-exists :supersede)
           (write-string "(defun uw-n () (uw-o) (uw-nowhere-11)) (defun uw-o () 1)" o))
         (prog1 (equal (undefined-names (lambda () (compile-file path :output-file out))) '(uw-nowhere-11))
           (delete-file path) (ignore-errors (delete-file out)))))

(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
