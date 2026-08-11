(eval-when (:compile-toplevel :load-toplevel :execute)

(defun list-of-forms? (x)
  (and (consp x) (consp (car x))
       (not (eq (caar x) 'lambda))))

) ;end eval-when

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SharpL.
;;;
;;; the #L reader macro is an abbreviation for lambdas with numbered
;;; arguments, with the last argument being the greatest numbered
;;; argument that is used in the body.  Arguments which are not used
;;; in the body are (declare ignore)d.
;;;
;;; e.g. #L(list !2 !3 !5) is equivalent to:
;;;      (lambda (!1 !2 !3 !4 !5) (declare (ignore !1 !4)) (list !2 !3 !5))

(eval-when (:compile-toplevel :execute)

  (defun sharpL-reader (stream subchar n-args)
    (declare (ignore subchar))
    ;; Depending how an implementation chooses to expand `(,!1 (get-free-temp))
    ;; at read-time, it might be a macro that must be expanded before groveling
    ;; the resultant sexpr. Here it gets expanded in the null environment for
    ;; lack of anything better. If the macro is sensitive to its lexical
    ;; environment, it suggests perhaps an inappropriate use of #L.
    ;; However, to support unforseen cases, we will use the original form as
    ;; read for the resulting lambda's body. Moreover, rather than stuff new
    ;; atoms into the body which is impossible if the representation is opaque,
    ;; redirect "!" vars onto gensyms using SYMBOL-MACROLET.
    (let* ((form (read stream t nil t))
	   (refd-!vars (sort (bang-vars (macroexpand form))
                             #'< :key #'bang-var-num))
	   (bang-var-nums (mapcar #'bang-var-num refd-!vars))
	   (max-bv-num (if refd-!vars (car (last bang-var-nums)) 0)))
      (cond ((null n-args)
             (setq n-args max-bv-num))
            ((< n-args max-bv-num)
             (error "#L: digit-string ~d specifies too few arguments" n-args)))
      (let* ((all-!vars (loop for i from 1 to n-args collect (make-bang-var i)))
	     (formals (mapcar (lambda (x) (declare (ignore x)) (gensym))
                              all-!vars)))
	`#'(lambda ,formals
             ,@(let ((ignore (mapcan (lambda (!var tempvar)
                                       (unless (member !var refd-!vars)
                                         (list tempvar)))
                                     all-!vars formals)))
                 (if ignore `((declare (ignore ,@ignore)))))
             (symbol-macrolet ,(mapcan (lambda (!var tempvar)
                                         (when (member !var refd-!vars)
                                           (list (list !var tempvar))))
                                       all-!vars formals)
               ,@(if (list-of-forms? form) form (list form)))))))

  (defun make-bang-var (n)
    (intern (format nil "!~d" n)))

  (defun bang-vars (form)
    (delete-duplicates (bang-vars-1 form '()) :test #'eq))

  (defun bang-vars-1 (form vars)
    (cond
      ((consp form)
       (bang-vars-1 (cdr form)
		    (bang-vars-1 (car form) vars)))
      ((and (symbolp form) (bang-var? form)) (cons form vars))
      (t vars)))

  (defun bang-var? (sym)
    (char= (char (symbol-name sym) 0) #\!))

  (defun bang-var-num (sym)
    (let ((num (read-from-string (subseq (symbol-name sym) 1))))
      (if (not (and (integerp num) (> num 0)))
	  (error "#L: ~a is not a valid variable specifier" sym)
	  num)))

  (defun enable-sharpL-reader ()
    (set-dispatch-macro-character #\# #\L #'sharpL-reader))

  ;; According to CLHS, *readtable* must be rebound when compiling
  ;; so we are free to reassign it to a copy and modify that copy.
  (setf *readtable* (copy-readtable *readtable*))
  (enable-sharpL-reader)

  ) ; end eval-when
