;;;-*- syntax:COMMON-LISP; Package: (ITERATE :use "COMMON-LISP" :colon-mode :external) -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     ITERATE, An Iteration Macro
;;;
;;;                 Copyright 1989 by Jonathan Amsterdam
;;;         Adapted to ANSI Common Lisp in 2003 by Andreas Fuchs
;;;
;;; Permission to use, copy, modify, and distribute this software and its
;;; documentation for any purpose and without fee is hereby granted,
;;; provided that this copyright and permission notice appear in all
;;; copies and supporting documentation, and that the name of M.I.T. not
;;; be used in advertising or publicity pertaining to distribution of the
;;; software without specific, written prior permission. M.I.T. makes no
;;; representations about the suitability of this software for any
;;; purpose.  It is provided "as is" without express or implied warranty.

;;; M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;;; ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;;; M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;;; ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;;; WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;;; ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;;; SOFTWARE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  FIXES.
;;; (v. 1.2-ansi)
;;;  2004-11-30 - Joerg Hoehle: a dozen small fixes to various functions
;;;  2003-12-16 - Tested a bit more, implemented FOR-HASHTABLE and
;;;               FOR-PACKAGES (FOR-PACKAGE) iteration CLtS-style
;;;               using (with-{package,hashtable}-iterator)
;;;  2003-12-16 - ported iterate-1.2 to ANSI Common Lisp (in the form
;;;               of SBCL). Extremely untested. Works for simple
;;;               examples, though.
;;; (v. 1.2)
;;;  6/14/91  - fixed generation of previous code
;;;  5/6/91   - improved code generated for COLLECT and ADJOINING
;;;  4/10/91  - added *binding-context?* to correctly determine when inside
;;;	        a binding context
;;;  12/20/90 - fixed ,. bug in IN-HASHTABLE
;;;  3/3/91 - no longer generates loop-end and loop-step tags if they're not
;;;           used, to avoid compiler warnings from some compilers (Allegro)
;;;  3/4/91 - treat cond as a special form for allegro
;;;  (v. 1.1.1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OUTSTANDING PROBLEMS & QUESTIONS:
;;; - What happens if there are two contradictory declarations
;;;   about a variable's type?  We just take the second one. CLM
;;;   doesn't say, but presumably this is an error. Let's say it is.
;;;
;;; - Is there a more general way to do synonyms that still allows
;;;   some specificity to particular clauses?  Right now, all we allow
;;;   is for the first words of clauses to have synonyms.
;;;
;;; - We should look at function type declarations, at least at the
;;;   result type, and record them.
;;;
;;; - Consider adding an if-never keyword to find...max/min
;;;
;;; - Consider allowing accumulation variables to be generalized
;;;   variables, acceptable to setf.
;;;
;;; - Consider parsing type declarations of the form (vector * integer),
;;;   to generate types for internal variables.
;;;
;;; - Vector destructuring?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TO DO:
;;;  - do I walk &optional and &key code in lambda-lists?
;;;  - try binding *macroexpand-hook* in walk
;;;  - track down PREVIOUS bug in Symbolics and sparc lucid

;;;  - reducing and accum: RESULT-TYPE
;;;  - rethink types
;;;  - how to type result var?
;;;  - (for var concatenate (from 1 to 10) (in '(a b c)) (next (gensym)))
;;;  -       (if (< var 10)
;;;		 (next [from-to])
;;;		 (if lst
;;;		     (next [in])
;;;		     (gensym)))
;;;  - for var choose, for var repeatedly

;;; For CL version 2:
;;;  - variable info from environments
;;;  - macro info     "     " (so we can support macrolet)
;;;  - use errors for EOF
;;;  - change WALK and FREE-VARIABLES to take symbol macros into account
;;;  - array indices are fixnums
;;;  - type REAL for extremum clauses

;;; Maybe:
;;;  - decls can appear not at top level, as long as they appear before use.
;;;  - extremum and find-extremum should do reductions when possible
;;;  - optimize collections, hashtables, packages for lispms
;;;  - fix :using-type-of to check for supplied ???
;;;  - for-in should allow numerical keywords (from, to, etc.)...?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TO TEST:
;;;  - leaving driver code where it is
;;;  - typing
;;;  - macroexpand & walk after-each
;;;  - check for duplicate keywords in defclause, defmacro-clause
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TO DOCUMENT:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package #:iterate)

(declaim (declaration declare-variables))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants and global variables.
(defconst version "1.4" "Current version of Iterate")



(defconst standard-type-symbols ; of CLtL2
  '(array atom bignum bit bit-vector boolean character compiled-function
    complex cons double-float fixnum float function hash-table integer
    keyword list long-float nil null number package pathname random-state
    ratio rational readtable real sequence short-float signed-byte simple-array
    simple-bit-vector simple-string simple-vector single-float standard-char
    stream string string-char symbol t unsigned-byte vector)
  "Table 4-1 of the Common Lisp Manual")


;;; These next two can be used for maximizing and minimizing.

#+nil ;; unused
(defconst smallest-number-alist
  `((fixnum . ,most-negative-fixnum)
    (float . ,most-negative-long-float)
    (long-float . ,most-negative-long-float)
    (short-float . ,most-negative-short-float)
    (double-float . ,most-negative-double-float)
    (single-float . ,most-negative-single-float)))

#+nil ;; unused
(defconst largest-number-alist
  `((fixnum . ,most-positive-fixnum)
    (float . ,most-positive-long-float)
    (long-float . ,most-positive-long-float)
    (short-float . ,most-positive-short-float)
    (double-float . ,most-positive-double-float)
    (single-float . ,most-positive-single-float)))


;;; This is like (declare (declare-variables)).

(defvar *always-declare-variables* nil)

;;; *result-var* is bound to a gensym before the clauses of an iterate
;;; form are processed.  In the generated code, the gensym is bound
;;; to nil before any other bindings are performed.  Clauses are free
;;; to generate code that sets the value of *result-var*.

(defvar *result-var*)

;;; Iterate binds *type-alist* to an alist of variables and their
;;; types before processing clauses.  It does this by looking at
;;; (declare (type ...)) forms in the clauses and recording the information
;;; there.  (Just variable type information, not function.)

(defvar *type-alist*)

;;; *declare-variables* is bound to T iff the
;;;            (declare (iterate:declare-variables))
;;; declaration was seen at top-level, or if
;;; *always-declare-variables* is non-nil.  This indicates that variables
;;; that haven't been declared by the user should be declared to have
;;; the appropriate types.  What "appropriate" means depends on the
;;; context.

(defvar *declare-variables*)

;;; *clause* is bound to each entire iterate clause before the clause
;;; is processed.  Mostly for error output (see clause-error).

(defvar *clause*)

;;; *top-level?* is bound to T at top-level (i.e. before any forms that
;;; contain clauses inside them, like IF, LET, etc.) and to NIL
;;; inside such forms.  It is useful to ensure that certain forms
;;; (particularly iteration drivers) occur only at top-level.

(defvar *top-level?*)

;;; *binding-context?* a misnomer, should be named *declaration-context*, is
;;; bound to T inside a form that allows declarations (flet, labels).  We used
;;; to just see if *internal-variables* was non-nil, but that's wrong--you can
;;; be inside a binding context that binds no variables.

(defvar *binding-context?*)

;;; For the use of make-binding-internal, to pass back bindings.
;;; if-1st-time also uses it to create first-time variables.

(defvar *bindings*)


;;; This is a list of variable-lists containing the variables made by
;;; internal let's or other binding forms.  It is used to check for
;;; the error of having iterate try to bind one of these variables at
;;; top-level.  E.g.
;;;   (iterate (for i from 1 to 10)
;;;            (let ((a nil))
;;;              (collect i into a)))
;;; is an error.

(defvar *internal-variables*)


;;; For functions (like make-binding) that don't want to or can't pass
;;; declarations normally.  These are really decl-specs, not full
;;; declarations.

(defvar *declarations*)


;;; This is how we get multiple accumulations into the same variable
;;; to come out right.  See make-accum-var-binding.
;;; It's an alist of (accum-var kind <possibly other info>).
;;; The currently used kinds are:
;;;   :collect     for collect, nconc, append, etc.
;;;   :increment   for count, sum and multiply
;;;   :max         for maximize
;;;   :min         for minimize
;;;   :if-exists   for always/never/thereis and finding such-that
;;; Note that we do not check for type conflict in the re-use of these
;;; variables.

(defvar *accum-var-alist*)

;;; Shared variables created by make-shared-binding.
;;; It's an alist of (name gensym-var <possibly other info>).
;;; Tipical use is FIRST-ITERATION-P.

(defvar *shared-bindings-alist*)

;;; Name of the block for this iterate form.  Used in generating
;;; return statements.

(defvar *block-name*)
(defvar *loop-name*)

;;; The index of standard clauses (a discrimination tree).  This is a
;;; defvar so that reloading doesn't clobber existing defs (though it
;;; will clobber those clauses that are defined in this file, of
;;; course).

(defvar *clause-info-index* (list :index))

(eval-when (:compile-toplevel)
  ;; This is so the variable has a value when we compile this file, since
  ;; the process of compilation results in actually setting things up.
  (if (not (boundp '*clause-info-index*))
      (setq *clause-info-index* (list :index))))


;;; An alist of lisp special forms and the functions for handling them.
;;; nil as function means leave form as-is.

(defparameter *special-form-alist*
  '(;; First the special operators that every code walker must recognize
    (block . 		    walk-cddr)
    (catch . 		    walk-cdr)
    (declare . 	 	    walk-declare)
    (eval-when .  	    walk-cddr)
    (flet . 		    walk-flet)
    (function . 	    walk-function)
    (go . 		    nil)
    (if . 		    walk-cdr) ; also walk test form
    (labels . 		    walk-flet)
    (let . 		    walk-let)
    (let* . 		    walk-let)
    (load-time-value .      nil)
    (locally .              walk-cdr-with-declarations)
    ;(macrolet . 	    walk-macrolet) ; uncomment to raise error
    (multiple-value-call .  walk-cdr)
    (multiple-value-prog1 . walk-cdr)
    (progn . 		    walk-progn)
    (progv . 		    walk-cdr)
    (quote . 		    nil)
    (return-from . 	    walk-cddr)
    (setq . 		    walk-setq)
    (symbol-macrolet . 	    walk-cddr-with-declarations)
    (tagbody . 		    walk-tagbody)
    (the . 		    walk-cddr)
    (throw . 		    walk-cdr)
    (unwind-protect . 	    walk-cdr)

    ;; Next some special cases:
    ;; m-v-b is a macro, not a special form, but we want to recognize bindings.
    ;; Furthermore, Lispworks macroexpands m-v-b into some unknown m-v-BIND-call special form.
    (multiple-value-bind .  walk-multiple-value-bind)
    ;; Allegro treats cond as a special form, it does not macroexpand.
    #+allegro (cond .	    walk-cond)
    ;; Prior to 2005, CLISP expanded handler-bind into some
    ;; sys::%handler-bind syntax not declared as a special operator.
    #+clisp (handler-bind . walk-cddr) ; does not recognize clauses in handlers
    ;; pretty sure the bindings in compiler-let should NOT be transformed,
    ;; hence use of WALK-CDDR instead of WALK-LET
    #+ccl (ccl:compiler-let . walk-cddr)
    #+allegro (excl:compiler-let . walk-cddr) ; fix from Phoebe Goldman
    ;; A suitable generalization would be a pattern language that describes
    ;; which car/cdr are forms to be walked, declarations or structure.
    ;; Walk with-*-iterator ourselves in order to avoid macrolet warnings.
    ;; Note that walk-cddr-with-declarations won't walk the
    ;; package/hash-table descriptor argument, but it's good enough for now.
    (with-package-iterator    . walk-cddr-with-declarations)
    (with-hash-table-iterator . walk-cddr-with-declarations)

    ;; Finally some cases where code compiled from the macroexpansion
    ;; may not be as good as code compiled from the original form:
    ;; -- and iterate's own expansion becomes more readable
    (and .		    walk-cdr)
    (ignore-errors .	    walk-cdr) ; expands to handler-bind in CLISP
    (multiple-value-list .  walk-cdr)
    (multiple-value-setq .  walk-cddr)
    (nth-value .	    walk-cdr)
    (or .		    walk-cdr)
    (prog1 .		    walk-cdr)
    (prog2 .		    walk-cdr)
    (psetq . 		    walk-setq)))


;;; For clauses that are "special" in the sense that they don't conform to the
;;; keyword-argument syntax of Iterate clauses.

(defvar *special-clause-alist* nil)


;;; These two are for conserving temporaries.  *temps* is a list
;;; of temporaries that have already been created and given bindings.
;;; *temps-in-use* is a list of temporaries that are currently being used.
;;; See with-temporary, with-temporaries.
;;; This seems to stem from a time where it was more efficient to use
;;; (prog (temp)
;;;    ... (setq temp #) ; somewhere deep inside the body
;;;        (foo temp)
;;;        (bar temp)
;;;    ...)
;;; than using a local let deep inside that body, as in
;;; (tagbody ... (let ((temp #)) (foo temp) (bar temp)) ...)
;;; which may be be easier for compiler data flow and lifetime analysis.

(defvar *temps*)
(defvar *temps-in-use*)

;;; This is the environment, for macroexpand.

(defvar *env*)

;;; This is a list of information about drivers, for use by the NEXT
;;; mechanism.

(defvar *driver-info-alist*)

;;; This is used by the PREVIOUS mechanism.

(defvar *previous-vars-alist*)


;;; Loop labels

(defvar *loop-top*)
(defvar *loop-step*)
(defvar *loop-end*)

;;; Whether a label was used, to avoid generating them.  This is so we don't
;;; get a warning from compilers that check for unused tags.

(defvar *loop-step-used?*)
(defvar *loop-end-used?*)

;;; Things that we should wrap the loop's body in

(defvar *loop-body-wrappers*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(eval-when (:compile-toplevel :load-toplevel :execute)

;;; Clause-info structures, which are put in the clause index.
  (defstruct clause-info
    function
    keywords
    req-keywords
    doc-string
    generator?)

;;; Driver-info structures, for information about driver variables--used by
;;; NEXT.

  (defstruct driver-info
    next-code
    generator?
    (used nil))

;;; Previous-info structures, used by the PREVIOUS mechanism.

  (defstruct previous-info
    var
    save-info-list
    code
    (class :step))

  (defstruct save-info
    save-var
    save-vars
    iv-ref)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros.

(eval-when (:compile-toplevel :load-toplevel :execute)  ;; Allegro needs this

#+nil ;; unused
(defmacro assertion (test)
  `(if (not ,test) (bug "Assertion ~a failed" ',test)))

(defmacro augment (var stuff)
  `(setf ,var (nconc ,var ,stuff)))

(defmacro prepend (stuff var)
  `(setf ,var (nconc ,stuff ,var)))

) ;end eval-when

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
