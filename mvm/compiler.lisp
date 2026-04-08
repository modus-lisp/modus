;;;; compiler.lisp - MVM Compiler: Source Lisp -> MVM Bytecode
;;;;
;;;; The main compiler for the Modus Virtual Machine. Replaces both
;;;; cross-compile.lisp (x86-64 cross-compiler) and the rt-compile-*
;;;; runtime compilation modules. Produces target-independent MVM bytecode
;;;; that is later translated to native code by the AOT or JIT backends.
;;;;
;;;; Three phases:
;;;;   Phase 1: Frontend    - Source forms -> AST (macro expansion, recognition)
;;;;   Phase 2: IR Gen      - AST -> MVM IR (register allocation, control flow)
;;;;   Phase 3: Bytecode    - MVM IR -> compact bytecode (encoding, fixups)
;;;;
;;;; Register usage:
;;;;   V0-V3:  argument passing (caller places args here before call)
;;;;   V4-V8:  temporaries for expression evaluation
;;;;   V9-V15: available for spill / longer-lived values
;;;;   VR:     return value register
;;;;   VN:     NIL constant register
;;;;   VA:     allocation pointer
;;;;   VL:     allocation limit
;;;;   VSP:    stack pointer
;;;;   VFP:    frame pointer

(in-package :modus.mvm)

;;; ============================================================
;;; Tagging Constants (mirror cross-compile.lisp)
;;; ============================================================
;;;
;;; xxx0 = Fixnum (63-bit signed, shifted left 1)
;;; 0001 = Cons pointer
;;; 1001 = Object pointer (general heap object)
;;; 0101 = Immediate (char, single-float)
;;; 1111 = GC forwarding pointer

(defconstant +tag-fixnum+    #b0000)
(defconstant +tag-cons+      #b0001)
(defconstant +tag-object+    #b1001)
(defconstant +tag-immediate+ #b0101)
(defconstant +tag-forward+   #b1111)

(defconstant +fixnum-shift+ 1)
(defconstant +char-shift+ 8)
(defconstant +char-tag+ #x05)

;;; Subtag constants for heap objects
(defconstant +subtag-bignum+ #x30)
(defconstant +subtag-string+ #x31)
(defconstant +subtag-array+  #x32)

;;; Placeholder addresses for NIL and T (patched during image build)
(defconstant +nil-value+ #xDEAD0001)
(defconstant +t-value+   #xDEAD1009)

;;; Maximum number of register arguments
(defconstant +max-reg-args+ 4)

;;; ============================================================
;;; Compilation State
;;; ============================================================

(defvar *ir-buffer* nil
  "Current IR instruction list being built (in reverse order)")

(defvar *functions* (make-hash-table :test 'equal)
  "Map from function name (string) to function-info")

(defvar *function-table* nil
  "Ordered list of function-info structs for the compiled module")

(defvar *constant-table* nil
  "List of constants needing allocation in the image")

(defvar *current-function-name* nil
  "Name of the function currently being compiled")

(defvar *macro-table* (make-hash-table :test 'eql)
  "Hash table of macro-name (hash integer) -> expander function")

(defvar *label-counter* 0
  "Monotonic counter for generating unique labels")

(defvar *loop-exit-label* nil
  "Label for (return) to jump to in a loop")

(defvar *block-labels* nil
  "Alist of (name . exit-label) for block/return-from")

(defvar *tagbody-tags* nil
  "Alist of (tag . label) for tagbody/go")

(defvar *function-return-label* nil
  "Label for early return from function body (return outside loop)")

(defvar *globals* (make-hash-table :test 'eql)
  "Set of known global variable names (hash -> t)")

(defvar *constants* (make-hash-table :test 'eql)
  "Map from constant name (hash) to compile-time value")

(defvar *temp-reg-counter* 0
  "Next temporary register to allocate (cycles through V4-V15)")

(defvar *arith-push-depth* 0
  "Depth of arithmetic PUSH/POP nesting. When > 0, we are inside an
   arithmetic operation that has pushed an intermediate result.
   Compiling a function call at depth > 0 risks register clobber
   from the call's save/restore interacting with the arithmetic stack.")

(defvar *pending-flet-ir* nil
  "Collects (info . ir) pairs from flet/labels function compilations.
   These are drained by mvm-compile-all into all-ir after each top-level form.")

(defvar *current-source-location* nil
  "Current source location string, set by mvm-compile-all for each form.")

;;; ============================================================
;;; Structures
;;; ============================================================

(defstruct function-info
  name            ; symbol or string
  param-count     ; number of formal parameters
  bytecode-offset ; offset in the module bytecode vector
  bytecode-length ; length of this function's bytecode
  stack-frame-size ; number of stack slots used
  source-location) ; string describing where defined (e.g. "form#123")

(defstruct compile-env
  (bindings nil)       ; list of binding structs
  (stack-depth 0)      ; current stack depth (in slots)
  (parent nil))        ; parent environment for nested scopes

(defstruct binding
  name               ; symbol name
  location           ; :reg or :stack
  reg                ; virtual register number if :reg
  stack-slot)        ; stack slot index if :stack

(defstruct compiled-module
  (bytecode (make-array 0 :element-type '(unsigned-byte 8)))
  (function-table nil)   ; list of function-info
  (constant-table nil))  ; list of constant values

;;; ============================================================
;;; Label Generation
;;; ============================================================

(defun make-compiler-label ()
  "Generate a unique label ID for the compiler"
  (incf *label-counter*))

;;; ============================================================
;;; Temporary Register Allocation
;;; ============================================================
;;;
;;; Simple linear allocation of V4-V8 for expression temporaries.
;;; Resets at the start of each expression statement.

(defun reset-temp-regs ()
  "Reset the temporary register counter"
  (setf *temp-reg-counter* 0))

(defun alloc-temp-reg ()
  "Allocate the next temporary register (V4-V15).
   V4-V8 map to physical registers; V9-V15 are spill slots that the
   translator automatically maps to stack frame locations."
  (let ((reg (+ +vreg-v4+ *temp-reg-counter*)))
    (when (> reg +vreg-v15+)
      (error "MVM compiler: out of temporary registers (need >12)"))
    (incf *temp-reg-counter*)
    reg))

(defun free-temp-reg ()
  "Free the most recently allocated temporary register"
  (when (> *temp-reg-counter* 0)
    (decf *temp-reg-counter*)))

(defun current-temp-count ()
  "Return how many temp regs are currently in use"
  *temp-reg-counter*)

;;; ============================================================
;;; Arithmetic nesting safety check
;;; ============================================================
;;;
;;; The PUSH/POP pattern used by arithmetic ops (+, *, logand, etc.)
;;; interacts badly with function call save/restore when nested.
;;; Example of what breaks:
;;;   (+ (* (f x) (g y))        ; outer + pushes first arg
;;;      (+ (* (h a) (k b))     ; inner +,* also push/pop
;;;         (* (m c) (n d))))   ; call save/restore corrupts stack
;;;
;;; The fix: bind each multiply to a let variable, then sum variables.
;;; We detect this at compile time and error out with a clear message.

(defparameter *arith-ops*
  '(+ - * logand logior logxor ash)
  "Operators that use PUSH/POP for multi-arg evaluation")

(defun form-contains-call-p (form)
  "Return T if FORM contains a function call (not just arithmetic/variables/constants).
   Used to detect dangerous nesting patterns."
  (cond
    ((not (consp form)) nil)
    ((not (symbolp (car form))) nil)
    ;; Known safe forms that don't generate calls — but recurse into subforms
    ((member (car form) '(quote function)) nil)
    ;; Arithmetic ops: recurse into their args
    ((member (car form) *arith-ops*)
     (some #'form-contains-call-p (cdr form)))
    ;; Inline ops: safe themselves but recurse into arg forms
    ((member (car form) '(car cdr cons list
                          cadr cdar cddr
                          aref aset
                          not null
                          1+ 1-
                          = < > <= >= /= eq eql equal
                          zerop
                          length
                          mem-ref %setf-mem-ref))
     (some #'form-contains-call-p (cdr form)))
    ;; Control flow: recurse into all subforms
    ((member (car form) '(if when unless cond and or
                          progn block return return-from
                          setq setf))
     (some #'form-contains-call-p (cdr form)))
    ;; let/let*: recurse into binding values and body
    ((member (car form) '(let let*))
     (or (and (consp (cadr form))
              (some (lambda (b)
                      (and (consp b) (cdr b)
                           (form-contains-call-p (cadr b))))
                    (cadr form)))
         (some #'form-contains-call-p (cddr form))))
    ;; the: recurse into value
    ((member (car form) '(the declare)) nil)
    ;; Everything else is a function call
    (t t)))

(defun form-arith-call-depth (form)
  "Return the nesting depth of arithmetic ops containing function calls.
   Returns 0 if no function calls, or the max depth at which a call appears.
   Depth 1 = call inside one arithmetic op (safe — save-count stays <= 1).
   Depth 2+ = call inside nested arithmetic (DANGEROUS — stack corruption)."
  (cond
    ((not (consp form)) 0)
    ((not (symbolp (car form))) 0)
    ((member (car form) '(quote function)) 0)
    ;; Arithmetic op with multiple args: each non-first arg is inside a PUSH/POP
    ((and (member (car form) *arith-ops*)
          (cddr form))  ; has 2+ args
     ;; The first arg is NOT inside a push/pop, only subsequent args are
     (max (form-arith-call-depth (car (cdr form)))  ; first arg: no depth increase
          (reduce #'max (mapcar (lambda (arg)
                                  (let ((d (form-arith-call-depth arg)))
                                    (if (form-contains-call-p arg)
                                        (1+ d)  ; call is inside this arithmetic's push/pop
                                        d)))
                                (cddr form))
                  :initial-value 0)))
    ;; Single-arg arithmetic: no push/pop
    ((member (car form) *arith-ops*)
     (if (cdr form) (form-arith-call-depth (cadr form)) 0))
    ;; Recurse into control flow / inline ops
    ((member (car form) '(if when unless cond and or not null
                          progn block return return-from
                          setq setf
                          car cdr cons list cadr cdar cddr
                          aref aset 1+ 1-
                          = < > <= >= /= eq eql equal zerop length
                          mem-ref %setf-mem-ref))
     (reduce #'max (mapcar #'form-arith-call-depth (cdr form))
             :initial-value 0))
    ;; let/let*
    ((member (car form) '(let let*))
     (max (if (consp (cadr form))
              (reduce #'max
                      (mapcar (lambda (b)
                                (if (and (consp b) (cdr b))
                                    (form-arith-call-depth (cadr b))
                                    0))
                              (cadr form))
                      :initial-value 0)
              0)
          (reduce #'max (mapcar #'form-arith-call-depth (cddr form))
                  :initial-value 0)))
    ;; Function call: leaf — depth 0 (the call itself is counted by parent)
    (t 0)))

(defun check-arith-nesting (op operand)
  "Check that OPERAND is safe to compile inside an arithmetic PUSH/POP.
   At *arith-push-depth* >= 1, the operand must not itself push again
   with a function call inside (which would reach effective depth 2+)."
  (let ((inner-depth (form-arith-call-depth operand)))
    ;; Only error if the operand actually contains function calls.
    ;; Pure arithmetic nesting without calls is safe — no save/restore interference.
    (when (and (>= (+ *arith-push-depth* inner-depth) 2)
               (form-contains-call-p operand))
      (error "MVM compiler: nested arithmetic with function calls will miscompile.~%~
              In function ~A: operator ~A at depth ~D, operand adds ~D more levels.~%~
              Effective depth ~D >= 2: stack corruption will occur.~%~
              Fix: bind function call results to let variables first.~%~
              Offending operand: ~S"
             *current-function-name* op *arith-push-depth* inner-depth
             (+ *arith-push-depth* inner-depth) operand))))

(defparameter *let-binding-limit* 120
  "Maximum number of bindings in a single let/let* form.
   The x64 frame has 128 slots; leave headroom for nested scopes.
   Beyond this, stack slots overflow the frame causing corruption.")

(defun check-frame-overflow (n-bindings form-type env)
  "Error if total stack depth would exceed the frame slot limit."
  (let ((current-depth (if env (compile-env-stack-depth env) 0))
        (new-depth (+ (if env (compile-env-stack-depth env) 0) n-bindings)))
    (when (> new-depth *let-binding-limit*)
      (error "MVM compiler: ~A with ~D bindings at depth ~D would use ~D frame slots (limit ~D).~%~
              In function ~A.~%~
              Fix: split into helper functions to reduce total nesting depth."
             form-type n-bindings current-depth new-depth *let-binding-limit*
             *current-function-name*))))

;;; ============================================================
;;; IR Instruction Representation
;;; ============================================================
;;;
;;; IR instructions are simple lists:
;;;   (:op arg1 arg2 ...)
;;; Labels are represented as:
;;;   (:label label-id)
;;; Register operands are integers (virtual register numbers).
;;; Immediates are tagged with :imm:
;;;   (:imm value)

(defun emit-ir (op &rest args)
  "Emit an IR instruction to the current buffer"
  (push (cons op args) *ir-buffer*))

(defun emit-ir-label (label-id)
  "Emit a label marker in the IR stream"
  (push (list :label label-id) *ir-buffer*))

(defun get-ir-instructions ()
  "Return the IR instructions in forward order"
  (nreverse *ir-buffer*))

;;; ============================================================
;;; Environment Operations
;;; ============================================================

(defun make-empty-env ()
  "Create a fresh empty compilation environment"
  (make-compile-env))

(defun env-lookup (env name)
  "Find binding for NAME in ENV, searching parent chain. Returns binding or nil."
  (when env
    (or (find name (compile-env-bindings env)
              :key #'binding-name :test #'equal)
        (env-lookup (compile-env-parent env) name))))

(defun env-extend-reg (env name reg)
  "Add a register binding for NAME to ENV"
  (make-compile-env
   :bindings (cons (make-binding :name name :location :reg :reg reg)
                   (compile-env-bindings env))
   :stack-depth (compile-env-stack-depth env)
   :parent (compile-env-parent env)))

(defun env-extend-stack (env name)
  "Allocate a stack slot for NAME, return (values new-env slot-index)"
  (let* ((slot (compile-env-stack-depth env))
         (new-env (make-compile-env
                   :bindings (cons (make-binding :name name
                                                  :location :stack
                                                  :stack-slot slot)
                                   (compile-env-bindings env))
                   :stack-depth (1+ slot)
                   :parent (compile-env-parent env))))
    (cons new-env slot)))

(defun env-child (env)
  "Create a child environment inheriting from ENV"
  (make-compile-env
   :bindings nil
   :stack-depth (compile-env-stack-depth env)
   :parent env))

;;; ============================================================
;;; Name Normalization
;;; ============================================================
;;;
;;; Cross-package compatibility: we compare operator names as
;;; name hashes (dual FNV-1a), matching cross.lisp's compute-name-hash.

(defun compute-name-hash (name-string)
  "Compute dual-FNV-1a hash for a name string. 60-bit collision-resistant."
  (let ((name (string-upcase (string name-string)))
        (h1 2166136261) (h2 3735928559))
    (loop for c across name
          do (setq h1 (logand (* (logxor h1 (char-code c)) 16777619) #xFFFFFFFF))
             (setq h2 (logand (* (logxor h2 (char-code c)) 805306457) #xFFFFFFFF)))
    (let ((combined (logior (ash (logand h1 #x3FFFFFFF) 30)
                            (logand h2 #x3FFFFFFF))))
      (if (zerop combined) 1 combined))))

(defun normalize-name (sym)
  "Convert a symbol to its name hash for comparison"
  (if (integerp sym)
      sym
      (compute-name-hash (symbol-name sym))))

(defun name-eq (sym name-string)
  "Check if SYM's name matches NAME-STRING via hash comparison"
  (and (symbolp sym)
       (= (compute-name-hash (symbol-name sym))
          (compute-name-hash name-string))))

;;; ============================================================
;;; Phase 1: Frontend (Source -> AST)
;;; ============================================================
;;;
;;; The frontend reads already-parsed s-expressions, expands macros,
;;; and produces an AST. For this compiler the AST is simply the
;;; expanded s-expression itself -- special forms, builtins, and calls
;;; are recognized during IR generation. Macro expansion is the key
;;; transformation in this phase.

(defun macroexpand-1-mvm (form)
  "Expand one level of macro in FORM, using the MVM macro table.
   Returns (values expanded-form expanded-p)."
  (if (and (consp form) (symbolp (car form)))
      (let* ((name (normalize-name (car form)))
             (expander (gethash name *macro-table*)))
        (if expander
            (cons (funcall expander form) t)
            (cons form nil)))
      (cons form nil)))

(defun macroexpand-mvm (form)
  "Fully expand macros in FORM"
  (loop
    (let* ((result (macroexpand-1-mvm form))
           (expanded (car result))
           (expanded-p (cdr result)))
      (unless expanded-p
        (return form))
      (setf form expanded))))

(defun mvm-define-macro (name expander)
  "Register a macro with NAME (string or hash) and EXPANDER function.
   EXPANDER takes the whole form (including the operator) and returns
   the expansion."
  (setf (gethash (if (integerp name) name (compute-name-hash name))
                 *macro-table*)
        expander))

(defun register-mvm-bootstrap-macros ()
  "Register standard CL macros needed to compile *runtime-functions*."
  ;; COND → nested IF
  (mvm-define-macro "COND"
    (lambda (form)
      (let ((clauses (cdr form)))
        (if (null clauses) nil
            (let ((clause (car clauses)))
              (if (and (symbolp (car clause))
                       (= (compute-name-hash (symbol-name (car clause))) 307092296168853251))
                  `(progn ,@(cdr clause))
                  `(if ,(car clause)
                       (progn ,@(cdr clause))
                       (cond ,@(cdr clauses)))))))))
  ;; AND → nested IF
  (mvm-define-macro "AND"
    (lambda (form)
      (let ((args (cdr form)))
        (cond ((null args) t)
              ((null (cdr args)) (car args))
              (t `(if ,(car args) (and ,@(cdr args)) nil))))))
  ;; OR → LET + IF
  (mvm-define-macro "OR"
    (lambda (form)
      (let ((args (cdr form)))
        (cond ((null args) nil)
              ((null (cdr args)) (car args))
              (t (let ((tmp (gensym "OR")))
                   `(let ((,tmp ,(car args)))
                      (if ,tmp ,tmp (or ,@(cdr args))))))))))

  ;; CASE → LET + COND + EQL
  (mvm-define-macro "CASE"
    (lambda (form)
      (let ((keyform (cadr form))
            (clauses (cddr form))
            (tmp (gensym "CASE")))
        `(let ((,tmp ,keyform))
           (cond ,@(mapcar (lambda (clause)
                             (let ((keys (car clause))
                                   (body (cdr clause)))
                               (cond
                                 ((or (eq keys t)
                                      (and (symbolp keys)
                                           (= (compute-name-hash (symbol-name keys)) 351744830753626451)))
                                  `(t ,@body))
                                 ((listp keys)
                                  `((or ,@(mapcar (lambda (k) `(eql ,tmp ',k)) keys))
                                    ,@body))
                                 (t `((eql ,tmp ',keys) ,@body)))))
                           clauses))))))

  ;; ECASE → CASE (same behavior for now; no error on mismatch)
  (mvm-define-macro "ECASE"
    (lambda (form)
      `(case ,@(cdr form))))

  ;; DOLIST → LET + LOOP
  (mvm-define-macro "DOLIST"
    (lambda (form)
      (let ((spec (cadr form))
            (body (cddr form)))
        (let ((var (car spec))
              (list-form (cadr spec))
              (tmp (gensym "DL")))
          `(let ((,tmp ,list-form))
             (loop
               (if (null ,tmp)
                   (return nil)
                   (let ((,var (car ,tmp)))
                     ,@body
                     (setq ,tmp (cdr ,tmp))))))))))

  ;; INCF → SETQ + +
  (mvm-define-macro "INCF"
    (lambda (form)
      (let ((place (cadr form))
            (delta (or (caddr form) 1)))
        (if (symbolp place)
            `(setq ,place (+ ,place ,delta))
            `(setf ,place (+ ,place ,delta))))))

  ;; DECF → SETQ + -
  (mvm-define-macro "DECF"
    (lambda (form)
      (let ((place (cadr form))
            (delta (or (caddr form) 1)))
        (if (symbolp place)
            `(setq ,place (- ,place ,delta))
            `(setf ,place (- ,place ,delta))))))

  ;; PLUSP → (> x 0)
  (mvm-define-macro "PLUSP"
    (lambda (form)
      `(> ,(cadr form) 0)))

  ;; MINUSP → (< x 0)
  (mvm-define-macro "MINUSP"
    (lambda (form)
      `(< ,(cadr form) 0)))

  ;; LOGNOT → LOGXOR with -1
  (mvm-define-macro "LOGNOT"
    (lambda (form)
      `(logxor ,(cadr form) -1)))

  ;; MAX → IF + comparison
  (mvm-define-macro "MAX"
    (lambda (form)
      (if (null (cddr form))
          (cadr form)
          (let ((tmp (gensym "MAX")))
            `(let ((,tmp ,(cadr form)))
               (if (> ,tmp ,(caddr form)) ,tmp ,(caddr form)))))))

  ;; MIN → IF + comparison
  (mvm-define-macro "MIN"
    (lambda (form)
      (if (null (cddr form))
          (cadr form)
          (let ((tmp (gensym "MIN")))
            `(let ((,tmp ,(cadr form)))
               (if (< ,tmp ,(caddr form)) ,tmp ,(caddr form)))))))

  ;; ABS → IF + negate
  (mvm-define-macro "ABS"
    (lambda (form)
      (let ((tmp (gensym "ABS")))
        `(let ((,tmp ,(cadr form)))
           (if (< ,tmp 0) (- 0 ,tmp) ,tmp)))))

  ;; PROG1 → LET + body + return first value
  (mvm-define-macro "PROG1"
    (lambda (form)
      (let ((tmp (gensym "P1")))
        `(let ((,tmp ,(cadr form)))
           ,@(cddr form)
           ,tmp))))

  ;; DEFPARAMETER → DEFVAR
  (mvm-define-macro "DEFPARAMETER"
    (lambda (form)
      `(defvar ,(cadr form) ,(caddr form))))

  ;; PUSH → (setq place (cons val place))
  (mvm-define-macro "PUSH"
    (lambda (form)
      (let ((val (cadr form))
            (place (caddr form)))
        (if (symbolp place)
            `(setq ,place (cons ,val ,place))
            `(setf ,place (cons ,val ,place))))))

  ;; POP → extract car, advance cdr
  (mvm-define-macro "POP"
    (lambda (form)
      (let ((place (cadr form))
            (tmp (gensym "POP")))
        (if (symbolp place)
            `(let ((,tmp (car ,place)))
               (setq ,place (cdr ,place))
               ,tmp)
            `(let ((,tmp (car ,place)))
               (setf ,place (cdr ,place))
               ,tmp)))))

  ;; DESTRUCTURING-BIND → LET* with car/cdr decomposition
  (mvm-define-macro "DESTRUCTURING-BIND"
    (lambda (form)
      (let ((pattern (cadr form))
            (expr (caddr form))
            (body (cdddr form))
            (tmp (gensym "DB")))
        (let ((bindings nil)
              (cur tmp))
          ;; Walk the flat pattern, handling &rest and &body
          (let ((rest-mode nil))
            (dolist (elt pattern)
              (cond
                ((member elt '(&rest &body &optional))
                 (setf rest-mode t))
                (rest-mode
                 ;; Bind rest of list
                 (push (list elt cur) bindings)
                 (setf rest-mode nil))
                (t
                 ;; Bind car, advance to cdr
                 (let ((next-cur (gensym "D")))
                   (push (list elt `(car ,cur)) bindings)
                   (push (list next-cur `(cdr ,cur)) bindings)
                   (setf cur next-cur))))))
          `(let* ((,tmp ,expr)
                  ,@(nreverse bindings))
             ,@body)))))

  ;; FIRST, SECOND, THIRD, FOURTH, FIFTH → car of nthcdr
  (mvm-define-macro "FIRST"
    (lambda (form) `(car ,(cadr form))))
  (mvm-define-macro "SECOND"
    (lambda (form) `(car (cdr ,(cadr form)))))
  (mvm-define-macro "THIRD"
    (lambda (form) `(car (cdr (cdr ,(cadr form))))))
  (mvm-define-macro "FOURTH"
    (lambda (form) `(car (cdr (cdr (cdr ,(cadr form)))))))
  (mvm-define-macro "FIFTH"
    (lambda (form) `(car (cdr (cdr (cdr (cdr ,(cadr form))))))))

  ;; WHEN → (if test (progn body...) nil)
  (mvm-define-macro "WHEN"
    (lambda (form)
      (let ((test (cadr form))
            (body (cddr form)))
        `(if ,test (progn ,@body) nil))))

  ;; UNLESS → (if test nil (progn body...))
  (mvm-define-macro "UNLESS"
    (lambda (form)
      (let ((test (cadr form))
            (body (cddr form)))
        `(if ,test nil (progn ,@body)))))

  ;; VECTOR → (let ((v (make-array N))) (aset v 0 a0) ... v)
  (mvm-define-macro "VECTOR"
    (lambda (form)
      (let ((args (cdr form))
            (n (length (cdr form)))
            (var (gensym "VEC")))
        `(let ((,var (make-array ,n)))
           ,@(loop for arg in args
                   for i from 0
                   collect `(aset ,var ,i ,arg))
           ,var))))

  ;; SETF expansion for complex places (car, cdr, aref, gethash)
  ;; Note: mem-ref SETF is handled directly in compile-setf
  (mvm-define-macro "SETF"
    (lambda (form)
      (let ((args (cdr form)))
        ;; Multi-place: (setf p1 v1 p2 v2 ...) → (progn (setf p1 v1) (setf p2 v2) ...)
        (if (> (length args) 2)
            (let ((pairs nil)
                  (rest args))
              (loop while rest
                    do (push `(setf ,(first rest) ,(second rest)) pairs)
                       (setq rest (cddr rest)))
              `(progn ,@(nreverse pairs)))
            ;; Single-place: (setf place value)
            (let ((place (car args))
                  (value (cadr args)))
              (cond
                ;; (setf var value) → (setq var value)
                ((symbolp place)
                 `(setq ,place ,value))
                ;; (setf (car x) v) → (set-car x v)
                ((and (consp place) (name-eq (car place) "CAR"))
                 `(set-car ,(cadr place) ,value))
                ;; (setf (cdr x) v) → (set-cdr x v)
                ((and (consp place) (name-eq (car place) "CDR"))
                 `(set-cdr ,(cadr place) ,value))
                ;; (setf (aref a i) v) → (aset a i v)
                ((and (consp place) (name-eq (car place) "AREF"))
                 `(aset ,(cadr place) ,(caddr place) ,value))
                ;; (setf (gethash k h) v) → (puthash k h v)
                ((and (consp place) (name-eq (car place) "GETHASH"))
                 `(puthash ,(cadr place) ,(caddr place) ,value))
                ;; (setf (mem-ref ...) v) → keep as %setf-mem-ref for compile-setf
                ((and (consp place) (name-eq (car place) "MEM-REF"))
                 `(%setf-mem-ref ,place ,value))
                ;; (setf (nth n lst) v) → (set-car (nthcdr n lst) v)
                ((and (consp place) (name-eq (car place) "NTH"))
                 `(set-car (nthcdr ,(cadr place) ,(caddr place)) ,value))
                ;; (setf (svref a i) v) → (aset a i v)
                ((and (consp place) (name-eq (car place) "SVREF"))
                 `(aset ,(cadr place) ,(caddr place) ,value))
                ;; Generic struct accessor: (setf (foo-bar x) v) → (set-foo-bar x v)
                ((consp place)
                 (let ((setter (intern (format nil "SET-~A" (symbol-name (car place)))
                                       :modus.mvm)))
                   `(,setter ,(cadr place) ,value)))))))))

  ;; LDB — extract byte field from integer
  ;; (ldb (byte size position) integer) → (logand (ash integer (- position)) mask)
  (mvm-define-macro "LDB"
    (lambda (form)
      (let ((bytespec (cadr form))
            (integer (caddr form)))
        (if (and (consp bytespec)
                 (symbolp (car bytespec))
                 (string= (symbol-name (car bytespec)) "BYTE"))
            (let* ((size (cadr bytespec))
                   (pos (caddr bytespec))
                   (mask (1- (ash 1 size))))
              (if (zerop pos)
                  `(logand ,integer ,mask)
                  `(logand (ash ,integer ,(- pos)) ,mask)))
            (error "MVM ldb: only (ldb (byte s p) n) supported, got ~S" bytespec)))))

  ;; EMIT-BYTES — expand to individual emit-byte calls (avoids &rest)
  (mvm-define-macro "EMIT-BYTES"
    (lambda (form)
      (let ((buf (cadr form))
            (bytes (cddr form)))
        `(progn ,@(mapcar (lambda (b) `(emit-byte ,buf ,b)) bytes)))))

  ;; LIST — expand to nested cons (MVM has no &rest)
  ;; (list) → nil, (list a) → (cons a nil), (list a b c) → (cons a (cons b (cons c nil)))
  (mvm-define-macro "LIST"
    (lambda (form)
      (let ((args (cdr form)))
        (if (null args)
            nil
            (let ((result nil))
              (dolist (a (reverse args))
                (setf result `(cons ,a ,result)))
              result)))))

  ;; REST — alias for CDR
  (mvm-define-macro "REST"
    (lambda (form)
      `(cdr ,(cadr form))))

  ;; /= — not equal
  (mvm-define-macro "/="
    (lambda (form)
      `(not (= ,(cadr form) ,(caddr form)))))

  ;; CADDR, CDDDR, CADDDR — extended car/cdr compositions
  (mvm-define-macro "CADDR"
    (lambda (form)
      `(car (cddr ,(cadr form)))))
  (mvm-define-macro "CDDDR"
    (lambda (form)
      `(cdr (cddr ,(cadr form)))))
  (mvm-define-macro "CADDDR"
    (lambda (form)
      `(car (cdr (cddr ,(cadr form))))))
  )

;;; ============================================================
;;; Declare Form Stripping
;;; ============================================================
;;;
;;; CL allows (declare ...) forms at the start of let/defun bodies.
;;; MVM ignores declarations; this helper strips them.

(defun strip-declares (body)
  "Remove leading (declare ...) forms from BODY."
  (loop while (and (consp body)
                   (consp (car body))
                   (symbolp (caar body))
                   (= (compute-name-hash (symbol-name (caar body))) 524150358979133175))
        do (setf body (cdr body)))
  body)

;;; ============================================================
;;; Phase 2: IR Generation (AST -> MVM IR)
;;; ============================================================
;;;
;;; Walks the AST (expanded s-expressions) and emits MVM IR instructions.
;;; The result of every expression ends up in a destination register,
;;; which defaults to VR (the return value register).

;;; ------ Backquote Expansion ------
;;;
;;; SBCL represents `(a ,b ,@c) as:
;;;   (SB-INT:QUASIQUOTE (a #S(COMMA :EXPR b :KIND 0) #S(COMMA :EXPR c :KIND 2)))
;;; We expand this to explicit list/cons/append calls before compiling.

(defun bq-comma-p (x)
  "Check if X is an SBCL comma struct"
  (typep x 'sb-impl::comma))

(defun bq-comma-expr (x)
  "Get the expression from an SBCL comma struct"
  (sb-impl::comma-expr x))

(defun bq-comma-kind (x)
  "Get the kind from an SBCL comma struct (0=unquote, 2=splice)"
  (sb-impl::comma-kind x))

(defun expand-backquote (template)
  "Expand a backquote template into explicit list-building code.
   Handles ,x (unquote) and ,@x (splice)."
  (cond
    ;; Atom (no unquoting needed): quote it
    ((null template) nil)
    ((bq-comma-p template)
     ;; Bare ,x at top level
     (bq-comma-expr template))
    ((atom template)
     (list 'quote template))
    ;; List — process element by element
    (t (expand-backquote-list template))))

(defun expand-backquote-list (lst)
  "Expand a backquote list template. Handles splice and nested backquote."
  (let ((segments nil)   ; list of (kind . form) — :list or :splice
        (current nil))   ; accumulator for consecutive non-splice elements
    ;; Process each element
    (let ((remaining lst))
      (loop while (consp remaining)
            do (let ((elt (car remaining)))
                 (cond
                   ;; ,@x — splice
                   ((and (bq-comma-p elt) (= (bq-comma-kind elt) 2))
                    ;; Flush current accumulator
                    (when current
                      (push (cons :list (nreverse current)) segments)
                      (setf current nil))
                    (push (cons :splice (bq-comma-expr elt)) segments))
                   ;; ,x — unquote
                   ((bq-comma-p elt)
                    (push (bq-comma-expr elt) current))
                   ;; Nested backquote
                   ((and (consp elt) (eq (car elt) 'sb-int:quasiquote))
                    (push (expand-backquote (cadr elt)) current))
                   ;; Nested list
                   ((consp elt)
                    (push (expand-backquote elt) current))
                   ;; Literal atom
                   (t
                    (push (list 'quote elt) current))))
                 (setf remaining (cdr remaining)))
      ;; Handle dotted pair tail
      (when remaining
        ;; Dotted tail
        (when current
          (push (cons :list (nreverse current)) segments)
          (setf current nil))
        (if (bq-comma-p remaining)
            (push (cons :tail (bq-comma-expr remaining)) segments)
            (push (cons :tail (list 'quote remaining)) segments))))
    ;; Flush final accumulator
    (when current
      (push (cons :list (nreverse current)) segments))
    ;; Build result from segments (in reverse order)
    (setf segments (nreverse segments))
    ;; Optimize: single :list segment → just (list ...)
    (cond
      ((null segments) nil)
      ((and (null (cdr segments))
            (eq (caar segments) :list))
       `(list ,@(cdar segments)))
      (t
       ;; Multiple segments → append them
       (let ((parts (mapcar (lambda (seg)
                              (case (car seg)
                                (:list `(list ,@(cdr seg)))
                                (:splice (cdr seg))
                                (:tail (cdr seg))))
                            segments)))
         (if (null (cdr parts))
             (car parts)
             `(append ,@parts)))))))

;;; ------ Main Dispatch ------

(defun compile-form (form env dest)
  "Compile FORM in environment ENV, placing result in register DEST.
   DEST is a virtual register number."
  ;; Expand backquote before macro expansion
  (let ((form (if (and (consp form) (eq (car form) 'sb-int:quasiquote))
                  (expand-backquote (cadr form))
                  form)))
  ;; Macro expand
  (let ((form (macroexpand-mvm form)))
    (cond
      ;; NIL
      ((null form)
       (compile-nil dest))

      ;; T
      ((eq form t)
       (compile-t dest))

      ;; Integer literal
      ((integerp form)
       (compile-integer form dest))

      ;; Character literal
      ((characterp form)
       (compile-character form dest))

      ;; String literal (docstrings become no-ops; no heap strings in bare-metal)
      ((stringp form)
       (compile-nil dest))

      ;; Keyword (self-evaluating)
      ((keywordp form)
       (compile-keyword form dest))

      ;; Variable reference
      ((symbolp form)
       (compile-variable-ref form env dest))

      ;; Compound forms (special forms, builtins, calls)
      ((consp form)
       (compile-compound form env dest))

      ;; Float literal → compile as 0 (bare metal has no floats)
      ((floatp form)
       (compile-integer 0 dest))

      ;; Vector literal #(...) → (make-array-from elt0 elt1 ...)
      ((and (vectorp form) (not (stringp form)))
       (let ((n (length form)))
         (compile-form `(let ((arr (make-array ,n)))
                          ,@(loop for i from 0 below n
                                  collect `(aset arr ,i ,(aref form i)))
                          arr)
                       env dest)))

      ;; Unrecognized
      (t
       (error "MVM compiler: cannot compile ~S" form))))))

;;; ------ Self-Evaluating Literals ------

(defun compile-nil (dest)
  "Load NIL into DEST"
  (emit-ir :mov dest +vreg-vn+))

(defun compile-t (dest)
  "Load T into DEST"
  (emit-ir :li dest +t-value+))

(defun compile-integer (value dest)
  "Load an integer literal (pre-tagged as fixnum) into DEST"
  (let ((tagged (ash value +fixnum-shift+)))
    (if (zerop tagged)
        (emit-ir :li dest 0)
        (emit-ir :li dest tagged))))

(defun compile-character (ch dest)
  "Load a character literal into DEST"
  (let ((tagged (logior (ash (char-code ch) +char-shift+) +char-tag+)))
    (emit-ir :li dest tagged)))

(defun compile-keyword (kw dest)
  "Load a keyword (as its tagged name hash) into DEST.
   Uses normalize-name (not sxhash) so keywords match other symbol
   representations and integer-encoded symbol IDs consistently."
  (emit-ir :li dest (ash (normalize-name kw) +fixnum-shift+)))

;;; ------ Variable Reference ------

(defun compile-variable-ref (name env dest)
  "Compile a variable reference, placing result in DEST"
  (let ((binding (env-lookup env name)))
    (cond
      (binding
       (ecase (binding-location binding)
         (:reg
          (let ((src (binding-reg binding)))
            (unless (= src dest)
              (emit-ir :mov dest src))))
         (:stack
          ;; Load from stack slot: load [VFP - (slot+1)*8]
          (emit-ir :stack-load dest (binding-stack-slot binding)))))
      ;; Compile-time constant: fold to literal
      ((let ((const-val (gethash (normalize-name name) *constants* :not-found)))
         (unless (eq const-val :not-found)
           ;; Quote symbol constants to prevent them being treated as variables
           (if (and (symbolp const-val)
                    (not (null const-val))
                    (not (eq const-val t))
                    (not (keywordp const-val)))
               (compile-form (list 'quote const-val) nil dest)
               (compile-form const-val nil dest))
           t)))
      ;; Global variable: emit call to symbol-value with name hash
      ((gethash (normalize-name name) *globals*)
       (let ((hash (normalize-name name)))
         (emit-ir :li +vreg-v0+ (ash hash +fixnum-shift+))
         (emit-ir :call "SYMBOL-VALUE" 1)
         (unless (= dest +vreg-vr+)
           (emit-ir :mov dest +vreg-vr+))))
      (t
       (error "MVM compiler: undefined variable ~A" name)))))

;;; ------ Compound Form Dispatch ------

(defun compile-compound (form env dest)
  "Compile a compound form (operator . args)"
  (let* ((op (car form))
         (op-name (cond ((integerp op) op) ((symbolp op) (normalize-name op)) (t nil))))
    (cond
      ;; Non-symbol operator (lambda call, etc.)
      ((null op-name)
       (compile-call op (cdr form) env dest))
      ;; --- Special Forms ---
      ((= op-name 518921307293258709)    (compile-quote (cadr form) dest))
      ((= op-name 448736678201786992)       (compile-if (cdr form) env dest))
      ((= op-name 87505416312042891)    (compile-progn (cdr form) env dest))
      ((= op-name 347164158959663450)      (compile-let (cadr form) (cddr form) env dest))
      ((= op-name 115433002357585904)     (compile-let* (cadr form) (cddr form) env dest))
      ((= op-name 565254038635891948)     (compile-setq (cadr form) (caddr form) env dest))
      ((= op-name 527981956251550024)   (compile-lambda (cadr form) (cddr form) env dest))
      ((= op-name 89559098115627243)     (compile-when (cdr form) env dest))
      ((= op-name 123360604517422061)   (compile-unless (cdr form) env dest))
      ((= op-name 502185558679326091)     (compile-loop (cdr form) env dest))
      ((= op-name 732905726022713733)   (compile-return (cadr form) env dest))
      ((= op-name 1062346144843286510)    (compile-block (cadr form) (cddr form) env dest))
      ((= op-name 54884900767456285)  (compile-tagbody (cdr form) env dest))
      ((= op-name 609179962647778703)       (compile-go (cadr form) env dest))
      ((= op-name 1080561289491153610)  (compile-dotimes (cadr form) (cddr form) env dest))
      ((= op-name 113179339635393781) (compile-function-ref (cadr form) env dest))
      ((= op-name 59431251605330656)  (compile-funcall (cdr form) env dest))
      ;; FLET/LABELS — compile local functions as named lambdas
      ((or (= op-name 230909053785822708) (= op-name 176230696681611090))
       (compile-flet (cadr form) (cddr form) env dest))
      ;; RETURN-FROM — treat as return (ignore block name)
      ((= op-name 102326962717880022)
       (compile-return (caddr form) env dest))
      ;; HANDLER-CASE — compile body only, skip handler clauses
      ((= op-name 362314411895974678)
       (compile-form (cadr form) env dest))
      ;; IGNORE-ERRORS — compile body only
      ((= op-name 1140402238842668217)
       (compile-progn (cdr form) env dest))
      ;; MACROLET — register local macros, compile body, then unregister
      ((= op-name 36999051998272136)
       (let ((saved-macros nil)
             (macro-defs (cadr form))
             (body (cddr form)))
         ;; Register macrolet macros
         (dolist (mdef macro-defs)
           (let* ((mname (normalize-name (car mdef)))
                  (mparams (cadr mdef))
                  (mbody (cddr mdef))
                  (old (gethash mname *macro-table*))
                  (expander (eval `(lambda (form)
                                     (destructuring-bind (,@mparams) (cdr form)
                                       ,@mbody)))))
             (push (cons mname old) saved-macros)
             (mvm-define-macro mname expander)))
         ;; Compile body
         (compile-progn body env dest)
         ;; Restore previous macro bindings
         (dolist (saved saved-macros)
           (if (cdr saved)
               (setf (gethash (car saved) *macro-table*) (cdr saved))
               (remhash (car saved) *macro-table*)))))
      ;; WITH-OPEN-FILE — compile as let binding stream var to nil, then body
      ((= op-name 258734651587197007)
       (let ((spec (cadr form))
             (body (cddr form)))
         (compile-form `(let ((,(car spec) nil)) ,@body) env dest)))
      ;; WITH-OUTPUT-TO-STRING — compile as let binding stream var to nil
      ((= op-name 884158782725716889)
       (let ((spec (cadr form))
             (body (cddr form)))
         (compile-form `(let ((,(car spec) nil)) ,@body) env dest)))
      ;; WITH-INPUT-FROM-STRING — compile as let binding stream var to nil
      ((= op-name 778706583216373557)
       (let ((spec (cadr form))
             (body (cddr form)))
         (compile-form `(let ((,(car spec) nil)) ,@body) env dest)))
      ;; MULTIPLE-VALUE-BIND — compile as let* with car/cdr destructuring
      ((= op-name 544225037749651317)
       (let ((vars (cadr form))
             (expr (caddr form))
             (body (cdddr form)))
         (let ((tmp (gensym "MVB")))
           (compile-form `(let* ((,tmp ,expr)
                                 ,@(loop for var in vars
                                         for i from 0
                                         collect (if (zerop i)
                                                     `(,var (car ,tmp))
                                                     `(,var (cdr ,tmp)))))
                            ,@body)
                         env dest))))

      ;; --- Arithmetic ---
      ((= op-name 829550095445217828)        (compile-add (cdr form) env dest))
      ((= op-name 721461107543724402)        (compile-sub (cdr form) env dest))
      ((= op-name 847564926404219517)        (compile-mul (cdr form) env dest))
      ((= op-name 757490770535469248)        (compile-div (cdr form) env dest))
      ((= op-name 701100176259851453)       (compile-1+ (cadr form) env dest))
      ((= op-name 593011189432099851)       (compile-1- (cadr form) env dest))
      ((= op-name 219259789038689217) (compile-truncate (cdr form) env dest))
      ((= op-name 1047143422370414916) (compile-mul26lo (cdr form) env dest))
      ((= op-name 3053449675996246) (compile-mul26hi (cdr form) env dest))
      ((= op-name 13026604224746835194) (compile-mul64lo (cdr form) env dest))
      ((= op-name 12591721202407133616) (compile-mul64hi (cdr form) env dest))
      ((= op-name 920227542902379435) (compile-acc128 (cdr form) env dest))
      ((= op-name 654425922550660137)      (compile-mod (cdr form) env dest))

      ;; --- Comparisons ---
      ;; Handle 3-arg comparisons: (<= a b c) → (and (<= a b) (<= b c))
      ((and (member op-name '(1027713239215462235 1063742901133465257 1009698407182718722 377678312869028470 305990259964713332))
            (= (length (cdr form)) 3))
       (let ((a (cadr form)) (b (caddr form)) (c (cadddr form)))
         (compile-form `(and (,(car form) ,a ,b) (,(car form) ,b ,c)) env dest)))
      ((= op-name 1027713239215462235)        (compile-compare :blt (cdr form) env dest))
      ((= op-name 1063742901133465257)        (compile-compare :bgt (cdr form) env dest))
      ((= op-name 1009698407182718722)        (compile-compare :beq (cdr form) env dest))
      ((= op-name 377678312869028470)       (compile-compare :ble (cdr form) env dest))
      ((= op-name 305990259964713332)       (compile-compare :bge (cdr form) env dest))
      ((= op-name 644866047583222547)       (compile-eq (cdr form) env dest))

      ;; --- List Operations ---
      ((= op-name 131620339109781567)      (compile-car (cadr form) env dest))
      ((= op-name 960859484116883722)      (compile-cdr (cadr form) env dest))
      ((= op-name 658831809041752574)     (compile-cons (cadr form) (caddr form) env dest))
      ((= op-name 643626177239181368)  (compile-set-car (cadr form) (caddr form) env dest))
      ((= op-name 680584020244584045)  (compile-set-cdr (cadr form) (caddr form) env dest))
      ((= op-name 599790875489715846)     (compile-caar (cadr form) env dest))
      ((= op-name 492519292879068819)     (compile-cadr (cadr form) env dest))
      ((= op-name 779194256552149755)     (compile-cdar (cadr form) env dest))
      ((= op-name 455511896952479694)     (compile-cddr (cadr form) env dest))

      ;; --- Bitwise Operations ---
      ((= op-name 245376457710419216)   (compile-logand (cdr form) env dest))
      ((= op-name 444641700551290191)   (compile-logior (cdr form) env dest))
      ((= op-name 91997575206662710)   (compile-logxor (cdr form) env dest))
      ((= op-name 498596602025227109)      (compile-ash (cadr form) (caddr form) env dest))
      ((= op-name 707618725562015373)      (compile-ldb (cadr form) (caddr form) env dest))

      ;; --- Type Predicates ---
      ((= op-name 1034692707450833644)     (compile-null (cadr form) env dest))
      ((= op-name 791386596785250882)      (compile-null (cadr form) env dest))
      ((= op-name 193192138738169214)    (compile-consp (cadr form) env dest))
      ((= op-name 1084402973118869726)  (compile-fixnump (cadr form) env dest))
      ((= op-name 105613410085771328)     (compile-atom-p (cadr form) env dest))
      ((= op-name 197121891723777229)    (compile-listp (cadr form) env dest))
      ((= op-name 1091515641497713485)  (compile-bignump (cadr form) env dest))
      ((= op-name 1024588698656382250)  (compile-stringp (cadr form) env dest))
      ((= op-name 959229030243575902)   (compile-arrayp (cadr form) env dest))
      ((= op-name 467922512990154729) (compile-integerp (cadr form) env dest))
      ((= op-name 641752649465622469)    (compile-zerop (cadr form) env dest))
      ((= op-name 322465010757792166) (compile-characterp (cadr form) env dest))

      ;; --- Character Operations ---
      ((= op-name 511431138979586071) (compile-char-code (cadr form) env dest))
      ((= op-name 632535660519644111) (compile-code-char (cadr form) env dest))

      ;; --- EQL (same as EQ for fixnums/chars/symbols) ---
      ((= op-name 743927193407775751)      (compile-eq (cdr form) env dest))
      ((= op-name 777630921077348411)    (compile-eq (cdr form) env dest))
      ((= op-name 674674239141986683) (compile-call op (cdr form) env dest))

      ;; --- Memory Operations ---
      ((= op-name 900047298083458158)  (compile-mem-ref (cadr form) (caddr form) env dest))
      ((= op-name 61397303667544258) (compile-setf (cadr form) (caddr form) env dest))

      ;; --- I/O Port Operations ---
      ((= op-name 951008440734391765)  (compile-io-out-byte (cadr form) (caddr form) env dest))
      ((= op-name 64505486828081420)   (compile-io-in-byte (cadr form) env dest))
      ((= op-name 402197317922113957) (compile-io-out-dword (cadr form) (caddr form) env dest))
      ((= op-name 157364223757942884)  (compile-io-in-dword (cadr form) env dest))

      ;; --- Serial Console ---
      ((= op-name 821056500804198866) (compile-write-char-serial (cdr form) env dest))
      ((= op-name 602746553318600181)  (compile-read-char-serial dest))

      ;; --- SAP (System Area Pointer) ---
      ((= op-name 613080895778544554) (compile-make-sap (cdr form) env dest))
      ((= op-name 848425955895126923) (compile-make-sap-raw (cdr form) env dest))
      ((= op-name 815904472968259812) (compile-sap-ref (cdr form) env dest :u8))
      ((= op-name 333410446086126141) (compile-sap-ref (cdr form) env dest :u32))
      ((= op-name 622121571885883806) (compile-sap-ref (cdr form) env dest :u64))
      ((= op-name 922421810905006511) (compile-sap-set (cdr form) env :u8))
      ((= op-name 550869834617056174) (compile-sap-set (cdr form) env :u32))
      ((= op-name 334162645828734861) (compile-sap-set (cdr form) env :u64))
      ((= op-name 124167155243180718) (compile-sap-addr (cdr form) env dest))

      ;; --- Linux Syscalls ---
      ((= op-name 874449673647888811) (compile-sys-exit (cdr form) env dest))
      ((= op-name 385320872711688559) (compile-syscall3 (cdr form) env dest))
      ((= op-name 84019503938880062)  (compile-syscall3-raw (cdr form) env dest))

      ;; --- Timestamp Counter ---
      ((= op-name 580098868411189197) (compile-rdtsc dest))

      ;; --- Wait For Interrupt ---
      ((= op-name 703562642750212015) (compile-wfi dest))

      ;; --- Setup IRQ ---
      ((= op-name 208317008853653791)  (compile-setup-irq dest))
      ;; --- Timer Rearm ---
      ((= op-name 590227155880225484) (compile-timer-rearm dest))

      ;; --- NIC Interrupt Setup ---
      ((= op-name 1009685354534069733) (compile-setup-nic-idt dest))
      ((= op-name 739607750214719398)  (compile-nic-irq-unmask dest))

      ;; --- MMIO (raw 32-bit address at 0x600140, result at 0x600148) ---
      ((= op-name 372079205816461105)  (compile-mmio-do-read32 dest))
      ((= op-name 186965853563265998) (compile-mmio-do-write32 dest))
      ;; --- Raw I/O port read (port in low 16 of [0x600140], result at 0x600148) ---
      ((= op-name 581371924726892981) (compile-io-in-dword-raw dest))
      ;; --- PCI config read (V0=addr without enable, result at 0x600148) ---
      ((= op-name 587268234776988492) (compile-pci-config-read-raw (cadr form) env dest))

      ;; --- Memory Barrier ---
      ((= op-name 1082210422183761822) (compile-memory-barrier dest))
      ;; --- Cache Flush (WBINVD on x86, NOP on others) ---
      ((= op-name 70198493141306239) (compile-wbinvd dest))

      ;; --- System Registers ---
      ((= op-name 756709414635220786)   (compile-get-alloc-ptr dest))
      ((= op-name 1055755022150105225) (compile-get-alloc-limit dest))
      ((= op-name 193475663400074726)   (compile-set-alloc-ptr (cadr form) env dest))
      ((= op-name 831645445086829693) (compile-set-alloc-limit (cadr form) env dest))
      ((= op-name 541448696650310846)           (compile-untag (cadr form) env dest))

      ;; --- Actor/Context Primitives ---
      ((= op-name 746185050329267356)    (compile-save-context (cadr form) env dest))
      ((= op-name 876713729717888613) (compile-restore-context (cadr form) env dest))
      ((= op-name 949162595018862897)     (compile-call-native (cdr form) env dest))

      ;; --- SMP Primitives ---
      ((= op-name 64976006036515571) (compile-xchg-mem (cadr form) (caddr form) env dest))
      ((= op-name 133047071382386485)    (compile-pause dest))
      ((= op-name 532818990203984097)   (compile-mfence dest))
      ((= op-name 930330168574267847)      (compile-hlt dest))
      ((= op-name 637964639327971374)    (compile-wrmsr (cdr form) env dest))
      ((= op-name 2665441512406489)      (compile-sti dest))
      ((= op-name 295712735144528609)      (compile-cli dest))
      ((= op-name 535690985964426756)  (compile-sti-hlt dest))

      ;; --- Per-CPU Data ---
      ((= op-name 1049169163874840266)       (compile-percpu-ref (cadr form) env dest))
      ((= op-name 815670105998589857)       (compile-percpu-set (cadr form) (caddr form) env dest))
      ((= op-name 76399844366031519) (compile-switch-idle-stack dest))
      ((= op-name 796316490043394273)          (compile-set-rsp (cadr form) env dest))
      ((= op-name 1011033367071895394)             (compile-lidt (cadr form) env dest))

      ;; --- Jump ---
      ((= op-name 659104475066268328)  (compile-jump-to-address (cadr form) env dest))

      ;; --- Function Address ---
      ((= op-name 532864888570260201)          (compile-fn-addr (cadr form) dest))

      ;; --- Array Operations ---
      ((= op-name 686483400154579705)       (compile-make-array (cadr form) env dest))
      ((= op-name 568601634040735695)             (compile-aref (cadr form) (caddr form) env dest))
      ((= op-name 216456113736582507)            (compile-aref (cadr form) (caddr form) env dest))
      ((= op-name 416706424900304020)             (compile-aset (cadr form) (caddr form) (cadddr form) env dest))
      ((= op-name 728795624198454423)     (compile-array-length (cadr form) env dest))

      ;; --- Function Call (default) ---
      (t (compile-call op (cdr form) env dest)))))

;;; ============================================================
;;; Quote
;;; ============================================================

(defun compile-quote (value dest)
  "Compile (quote VALUE)"
  (cond
    ((null value)
     (compile-nil dest))
    ((eq value t)
     (compile-t dest))
    ((integerp value)
     (compile-integer value dest))
    ((characterp value)
     (compile-character value dest))
    ((keywordp value)
     (compile-keyword value dest))
    ;; Non-keyword symbol: store as tagged name hash
    ((symbolp value)
     (emit-ir :li dest (ash (normalize-name value) +fixnum-shift+)))
    ;; Cons cell: proper lists built iteratively, dotted pairs recursively
    ((consp value)
     (if (and (listp (cdr (last value)))  ; proper list check
              (> (length value) 4))       ; optimize lists of 5+ elements
         ;; Iterative: build in reverse with single temp reg
         (let ((elems (reverse value)))
           (compile-nil dest)
           (dolist (elem elems)
             (emit-ir :push dest)
             (let ((temp (alloc-temp-reg)))
               (compile-quote elem temp)
               (emit-ir :pop dest)
               (emit-ir :gc-check)
               (emit-ir :cons dest temp dest)
               (free-temp-reg))))
         ;; Short lists / dotted pairs: recursive
         (compile-cons `(quote ,(car value)) `(quote ,(cdr value)) nil dest)))
    ;; String or other: use constant table
    (t
     (let ((idx (length *constant-table*)))
       (push value *constant-table*)
       (emit-ir :li-const dest idx)))))

;;; ============================================================
;;; If
;;; ============================================================

(defun compile-if (args env dest)
  "Compile (if test then &optional else)"
  (destructuring-bind (test then &optional else) args
    (let ((else-label (make-compiler-label))
          (end-label (make-compiler-label)))
      ;; Compile test into dest
      (compile-form test env dest)
      ;; Branch to else if nil
      (emit-ir :bnull dest else-label)
      ;; Then branch
      (compile-form then env dest)
      (emit-ir :br end-label)
      ;; Else branch
      (emit-ir-label else-label)
      (if else
          (compile-form else env dest)
          (compile-nil dest))
      ;; Join
      (emit-ir-label end-label))))

;;; ============================================================
;;; Progn
;;; ============================================================

(defun compile-progn (forms env dest)
  "Compile (progn form*). Result of last form goes to DEST."
  (if (null forms)
      (compile-nil dest)
      (let ((remaining forms))
        (loop while remaining
              do (let ((form (car remaining))
                       (rest (cdr remaining)))
                   (if rest
                       ;; Not the last form: compile for effect, result discarded
                       (compile-form form env dest)
                       ;; Last form: result goes to DEST
                       (compile-form form env dest))
                   (setq remaining rest))))))

;;; ============================================================
;;; Let / Let*
;;; ============================================================

(defun compile-let (bindings body env dest)
  "Compile (let ((var val)*) body*).
   All values are evaluated in the outer environment, then bound."
  (check-frame-overflow (length bindings) "let" env)
  (let ((body (strip-declares body))
        (n-bindings (length bindings))
        (new-env env)
        (save-temps nil))
    ;; Phase 1: Evaluate all values in original env, store to temp regs
    ;; We use a set of temp regs (or stack slots for > 5 bindings)
    (when (> n-bindings 0)
      ;; Allocate stack frame space for local variables
      (emit-ir :frame-alloc n-bindings))
    ;; Evaluate each binding value and store it to a stack slot
    (let ((i 0))
      (dolist (binding bindings)
        (let ((val (if (consp binding) (cadr binding) nil))
              (temp (alloc-temp-reg)))
          (compile-form val env temp)
          (let ((slot (+ (compile-env-stack-depth env) i)))
            (emit-ir :stack-store temp slot))
          (free-temp-reg)
          (setq i (+ i 1)))))
    ;; Phase 2: Build new environment with stack bindings
    (let ((i 0))
      (dolist (binding bindings)
        (let ((var (if (consp binding) (car binding) binding)))
          (setq new-env
                (make-compile-env
                 :bindings (cons (make-binding
                                  :name var
                                  :location :stack
                                  :stack-slot (+ (compile-env-stack-depth env) i))
                                (compile-env-bindings new-env))
                 :stack-depth (+ (compile-env-stack-depth env) n-bindings)
                 :parent (compile-env-parent new-env)))
          (setq i (+ i 1)))))
    ;; Fix stack-depth in the final env
    (setf (compile-env-stack-depth new-env)
          (+ (compile-env-stack-depth env) n-bindings))
    ;; Compile body in new environment
    (compile-progn body new-env dest)
    ;; Deallocate frame space
    (when (> n-bindings 0)
      (emit-ir :frame-free n-bindings))))

(defun compile-let* (bindings body env dest)
  "Compile (let* ((var val)*) body*).
   Values are evaluated sequentially; each can see earlier bindings."
  (check-frame-overflow (length bindings) "let*" env)
  (let ((body (strip-declares body))
        (n-bindings (length bindings))
        (new-env env))
    (when (> n-bindings 0)
      (emit-ir :frame-alloc n-bindings))
    ;; Evaluate sequentially, extending env each time
    (let ((i 0))
      (dolist (binding bindings)
        (let ((var (if (consp binding) (car binding) binding))
              (val (if (consp binding) (cadr binding) nil))
              (temp (alloc-temp-reg))
              (slot (+ (compile-env-stack-depth env) i)))
          (compile-form val new-env temp)
          (emit-ir :stack-store temp slot)
          (free-temp-reg)
          ;; Extend environment with this new binding
          (setq new-env
                (make-compile-env
                 :bindings (cons (make-binding
                                  :name var
                                  :location :stack
                                  :stack-slot slot)
                                (compile-env-bindings new-env))
                 :stack-depth (+ (compile-env-stack-depth env) (+ i 1))
                 :parent (compile-env-parent new-env)))
          (setq i (+ i 1)))))
    ;; Final env has correct stack depth
    (setf (compile-env-stack-depth new-env)
          (+ (compile-env-stack-depth env) n-bindings))
    ;; Compile body
    (compile-progn body new-env dest)
    ;; Deallocate
    (when (> n-bindings 0)
      (emit-ir :frame-free n-bindings))))

;;; ============================================================
;;; Setq
;;; ============================================================

(defun compile-setq (var val env dest)
  "Compile (setq var val)"
  (compile-form val env dest)
  (let ((binding (env-lookup env var)))
    (cond
      (binding
       (ecase (binding-location binding)
         (:reg
          (unless (= (binding-reg binding) dest)
            (emit-ir :mov (binding-reg binding) dest)))
         (:stack
          (emit-ir :stack-store dest (binding-stack-slot binding)))))
      ;; Global variable: emit call to set-symbol-value
      ((gethash (normalize-name var) *globals*)
       (let ((hash (normalize-name var)))
         ;; Push value, load hash into V0, pop value into V1
         (emit-ir :push dest)
         (emit-ir :li +vreg-v0+ (ash hash +fixnum-shift+))
         (emit-ir :pop +vreg-v1+)
         (emit-ir :call "SET-SYMBOL-VALUE" 2)
         ;; Result is in VR, move back to dest if needed
         (unless (= dest +vreg-vr+)
           (emit-ir :mov dest +vreg-vr+))))
      (t
       (error "MVM compiler: undefined variable ~A in setq" var)))))

;;; ============================================================
;;; Lambda
;;; ============================================================

(defun compile-lambda (params body env dest)
  "Compile (lambda (params) body*).
   Creates a closure-like function. Passes outer environment through
   so closure variables can be resolved (they'll reference outer stack slots)."
  (let* ((pp (preprocess-params params body))
         (lambda-name (format nil "~A$$LAMBDA~D"
                               (or *current-function-name* "ANON")
                               (make-compiler-label)))
         (fn-info (mvm-compile-function-internal lambda-name (car pp) (cdr pp) env)))
    ;; Load function reference into dest
    ;; This will be resolved to a bytecode offset during linking
    (emit-ir :li-func dest lambda-name)))

;;; ============================================================
;;; Flet / Labels
;;; ============================================================

(defun compile-flet (defs body env dest)
  "Compile (flet ((name (params) body) ...) body).
   Each local function is compiled as a named global function.
   The local name is used for calls within the body.
   The compiled IR is collected in *pending-flet-ir* for later emission."
  (dolist (def defs)
    (let ((name (car def))
          (params (cadr def))
          (fbody (cddr def)))
      (let ((pp (preprocess-params params fbody)))
        ;; Use mvm-compile-function-internal with parent env (for closure access),
        ;; then manually register and collect IR (like mvm-compile-function does).
        (let* ((fname (if (symbolp name) (symbol-name name) (string name)))
               (result (mvm-compile-function-internal fname (car pp) (cdr pp) env))
               (info (car result)))
          ;; Register in function table so CALL resolution works
          (setf (gethash (function-info-name info) *functions*) info)
          (push info *function-table*)
          ;; Save IR for collection by mvm-compile-all
          (push result *pending-flet-ir*)))))
  ;; Compile body in same environment
  (compile-progn (strip-declares body) env dest))

;;; ============================================================
;;; When / Unless
;;; ============================================================

(defun compile-when (args env dest)
  "Compile (when test body...) -> (if test (progn body...) nil)"
  (let ((test (car args))
        (body (cdr args)))
    (compile-if (list test (cons 'progn body)) env dest)))

(defun compile-unless (args env dest)
  "Compile (unless test body...) -> (if test nil (progn body...))"
  (let ((test (car args))
        (body (cdr args)))
    (compile-if (list test nil (cons 'progn body)) env dest)))

;;; ============================================================
;;; Loop / Return
;;; ============================================================

(defun cl-loop-keyword-p (sym)
  "Check if SYM is a CL loop keyword"
  (and (symbolp sym)
       (member (normalize-name sym)
               '(861144843042936108 1113883427174140325 468563938978316688
                 666095121438175797 32547421316216284 942546142429891564
                 204640710178503481 1066799008902276193
                 579297982844014476 820203232253031873 647934184416839188
                 146808687552856964 891107942385378521 646649243001235175
                 676158121401459048 264837417035531413 89559098115627243
                 123360604517422061 448736678201786992 732905726022713733
                 744661507158602198 340376721697683628 1091564327776232814
                 870389735836749037 212607784983936827
                 195734683635763289 682179722204096129
                 876035653932002648 1018827631117520136))))


(defun compile-loop (body env dest)
  "Compile (loop forms...) - either simple infinite loop or CL-style loop"
  (if (and (consp body) (cl-loop-keyword-p (car body)))
      ;; CL-style loop: expand to basic forms, then compile
      (compile-form (expand-cl-loop body) env dest)
      ;; Simple infinite loop
      (let* ((loop-label (make-compiler-label))
             (exit-label (make-compiler-label))
             (*loop-exit-label* exit-label))
        ;; Loop entry
        (emit-ir-label loop-label)
        ;; Compile loop body
        (compile-progn body env dest)
        ;; Yield/preemption check
        (emit-ir :yield)
        ;; Jump back to loop start
        (emit-ir :br loop-label)
        ;; Exit label (target of return)
        (emit-ir-label exit-label))))

;;; ============================================================
;;; CL-Style Loop Expansion
;;; ============================================================
;;;
;;; Expands (loop for/while/collect ...) to basic forms using
;;; let, block, tagbody, go, and return-from.
;;;
;;; Supported patterns:
;;;   (loop for VAR from START [to|below] END [by STEP] do BODY...)
;;;   (loop for VAR in LIST do BODY...)
;;;   (loop for VAR across ARRAY do BODY...)
;;;   (loop while COND do BODY...)
;;;   (loop for VAR = INIT then STEP [until COND] do BODY...)
;;;   (loop for VAR on LIST do BODY...)
;;;   (loop ... collect EXPR)
;;;   (loop ... sum EXPR)
;;;   (loop ... count EXPR)
;;;   (loop ... when COND do BODY)
;;;   (loop ... finally EXPR)
;;;   (loop ... return EXPR)

(defun expand-cl-loop (body)
  "Expand a CL-style loop body into basic Lisp forms."
  (let ((state (parse-cl-loop body)))
    (generate-loop-code state)))

(defstruct loop-state
  ;; Iteration variables: list of (var init step test)
  (iterations nil)
  ;; Body forms
  (body-forms nil)
  ;; Accumulator: nil or (:collect var) or (:sum var) or (:count var)
  (accumulator nil)
  ;; Finally forms
  (finally-forms nil)
  ;; With-bindings: list of (var init)
  (with-bindings nil))

(defstruct loop-iter
  kind            ; :from, :in, :across, :on, :general, :while, :repeat
  var             ; iteration variable
  init-form       ; initial value
  step-form       ; step expression
  end-form        ; end value (for :from)
  end-test        ; :to, :below, :above, :downto (for :from)
  by-form         ; step amount (for :from)
  list-var)       ; internal temp var (for :in, :on, :across)

(defun parse-cl-loop (body)
  "Parse loop clauses into a loop-state struct."
  (let ((state (make-loop-state))
        (rest body))
    (loop while rest do
      (let ((kw (normalize-name (car rest))))
        (cond
          ;; FOR var FROM start [TO|BELOW end] [BY step]
          ((or (= kw 861144843042936108) (= kw 1113883427174140325))
           (let ((var (cadr rest)))
             (setf rest (cddr rest))
             (let ((iter-kw (normalize-name (car rest))))
               (cond
                 ;; FOR var FROM start ...
                 ((= iter-kw 355693237506394641)
                  (setf rest (cdr rest))
                  (let* ((start-form (car rest))
                         (end-form nil) (end-test :to) (by-form nil))
                    (setf rest (cdr rest))
                    ;; Parse optional TO/BELOW/DOWNTO/ABOVE and BY
                    (loop while (and rest (symbolp (car rest))
                                    (member (normalize-name (car rest))
                                            '(611742951095832940 708656842296756988
                                              962879967384500096 223271319558938470
                                              934319717393949980)))
                          do (let ((sub-kw (normalize-name (car rest))))
                               (cond
                                 ((= sub-kw 611742951095832940)
                                  (setf end-test :to end-form (cadr rest) rest (cddr rest)))
                                 ((= sub-kw 708656842296756988)
                                  (setf end-test :below end-form (cadr rest) rest (cddr rest)))
                                 ((= sub-kw 962879967384500096)
                                  (setf end-test :above end-form (cadr rest) rest (cddr rest)))
                                 ((= sub-kw 223271319558938470)
                                  (setf end-test :downto end-form (cadr rest) rest (cddr rest)))
                                 ((= sub-kw 934319717393949980)
                                  (setf by-form (cadr rest) rest (cddr rest))))))
                    (push (make-loop-iter :kind :from :var var
                                          :init-form start-form
                                          :end-form end-form
                                          :end-test end-test
                                          :by-form by-form)
                          (loop-state-iterations state))))

                 ;; FOR var IN list
                 ((= iter-kw 592855328021284152)
                  (setf rest (cdr rest))
                  (let ((list-form (car rest))
                        (tmp (gensym "LI")))
                    (setf rest (cdr rest))
                    (push (make-loop-iter :kind :in :var var
                                          :init-form list-form
                                          :list-var tmp)
                          (loop-state-iterations state))))

                 ;; FOR var ACROSS array
                 ((= iter-kw 1027666347502942664)
                  (setf rest (cdr rest))
                  (let ((array-form (car rest))
                        (idx (gensym "LI"))
                        (arr (gensym "LA")))
                    (setf rest (cdr rest))
                    (push (make-loop-iter :kind :across :var var
                                          :init-form array-form
                                          :list-var idx
                                          :step-form arr)
                          (loop-state-iterations state))))

                 ;; FOR var ON list
                 ((= iter-kw 16092538585173950)
                  (setf rest (cdr rest))
                  (let ((list-form (car rest)))
                    (setf rest (cdr rest))
                    (push (make-loop-iter :kind :on :var var
                                          :init-form list-form)
                          (loop-state-iterations state))))

                 ;; FOR var = init [THEN step]
                 ((= iter-kw 1009698407182718722)
                  (setf rest (cdr rest))
                  (let ((init (car rest))
                        (step nil))
                    (setf rest (cdr rest))
                    (when (and rest (symbolp (car rest))
                               (= (normalize-name (car rest)) 712293789701165160))
                      (setf step (cadr rest) rest (cddr rest)))
                    (push (make-loop-iter :kind :general :var var
                                          :init-form init
                                          :step-form (or step init))
                          (loop-state-iterations state))))

                 (t (error "MVM loop: unknown FOR clause ~A" iter-kw))))))

          ;; WHILE condition
          ((= kw 468563938978316688)
           (push (make-loop-iter :kind :while :init-form (cadr rest))
                 (loop-state-iterations state))
           (setf rest (cddr rest)))

          ;; UNTIL condition
          ((= kw 666095121438175797)
           (push (make-loop-iter :kind :until :init-form (cadr rest))
                 (loop-state-iterations state))
           (setf rest (cddr rest)))

          ;; REPEAT n
          ((= kw 676158121401459048)
           (let ((n-form (cadr rest))
                 (counter (gensym "RC")))
             (push (make-loop-iter :kind :repeat :var counter
                                    :init-form n-form)
                   (loop-state-iterations state))
             (setf rest (cddr rest))))

          ;; WITH var = init
          ((= kw 264837417035531413)
           (let ((var (cadr rest)))
             (setf rest (cddr rest))
             ;; Skip optional =
             (when (and rest (symbolp (car rest))
                        (= (normalize-name (car rest)) 1009698407182718722))
               (setf rest (cdr rest))
               (push (list var (car rest)) (loop-state-with-bindings state))
               (setf rest (cdr rest)))))

          ;; DO body...
          ((or (= kw 32547421316216284) (= kw 942546142429891564))
           (setf rest (cdr rest))
           ;; Collect body forms until next loop keyword
           (loop while (and rest (not (and (symbolp (car rest))
                                           (cl-loop-keyword-p (car rest)))))
                 do (push (car rest) (loop-state-body-forms state))
                    (setf rest (cdr rest))))

          ;; COLLECT expr
          ((or (= kw 204640710178503481) (= kw 1066799008902276193))
           (let ((expr (cadr rest)))
             (setf (loop-state-accumulator state) (list :collect expr))
             (setf rest (cddr rest))))

          ;; SUM expr
          ((or (= kw 579297982844014476) (= kw 820203232253031873))
           (let ((expr (cadr rest)))
             (setf (loop-state-accumulator state) (list :sum expr))
             (setf rest (cddr rest))))

          ;; COUNT expr
          ((or (= kw 647934184416839188) (= kw 146808687552856964))
           (let ((expr (cadr rest)))
             (setf (loop-state-accumulator state) (list :count expr))
             (setf rest (cddr rest))))

          ;; WHEN/IF cond DO body | COLLECT expr
          ((or (= kw 89559098115627243) (= kw 448736678201786992))
           (let ((cond-form (cadr rest)))
             (setf rest (cddr rest))
             ;; Next should be DO or COLLECT
             (let ((action-kw (and rest (normalize-name (car rest)))))
               (cond
                 ((or (= action-kw 32547421316216284) (= action-kw 942546142429891564))
                  (setf rest (cdr rest))
                  (let ((action-form (car rest)))
                    (push `(when ,cond-form ,action-form) (loop-state-body-forms state))
                    (setf rest (cdr rest))))
                 ((or (= action-kw 204640710178503481) (= action-kw 1066799008902276193))
                  (let ((expr (cadr rest)))
                    (setf (loop-state-accumulator state) (list :collect-when cond-form expr))
                    (setf rest (cddr rest))))
                 (t
                  ;; Bare when: the next form is the body
                  (push `(when ,cond-form ,(car rest)) (loop-state-body-forms state))
                  (setf rest (cdr rest)))))))

          ;; FINALLY form...
          ((= kw 744661507158602198)
           (setf rest (cdr rest))
           (loop while (and rest (not (and (symbolp (car rest))
                                           (cl-loop-keyword-p (car rest)))))
                 do (push (car rest) (loop-state-finally-forms state))
                    (setf rest (cdr rest))))

          ;; RETURN expr
          ((= kw 732905726022713733)
           (push `(return ,(cadr rest)) (loop-state-body-forms state))
           (setf rest (cddr rest)))

          ;; Unknown keyword — treat as body form
          (t
           (push (car rest) (loop-state-body-forms state))
           (setf rest (cdr rest))))))

    ;; Reverse accumulated lists
    (setf (loop-state-iterations state) (nreverse (loop-state-iterations state)))
    (setf (loop-state-body-forms state) (nreverse (loop-state-body-forms state)))
    (setf (loop-state-finally-forms state) (nreverse (loop-state-finally-forms state)))
    (setf (loop-state-with-bindings state) (nreverse (loop-state-with-bindings state)))
    state))

(defun generate-loop-code (state)
  "Generate Lisp code from a parsed loop-state."
  (let* ((iters (loop-state-iterations state))
         (body (loop-state-body-forms state))
         (acc (loop-state-accumulator state))
         (finally (loop-state-finally-forms state))
         (with-binds (loop-state-with-bindings state))
         (acc-var (when acc (gensym "ACC")))
         (bindings nil)
         (init-stmts nil)
         (test-forms nil)
         (step-stmts nil))

    ;; WITH bindings
    (dolist (wb with-binds)
      (push wb bindings))

    ;; Accumulator binding
    (when acc
      (push (list acc-var (case (car acc)
                            (:collect nil)
                            (:collect-when nil)
                            (:sum 0)
                            (:count 0)
                            (t nil)))
            bindings))

    ;; Process iterations
    (dolist (iter iters)
      (ecase (loop-iter-kind iter)
        (:from
         (let ((var (loop-iter-var iter))
               (by (or (loop-iter-by-form iter) 1)))
           (push (list var (loop-iter-init-form iter)) bindings)
           (when (loop-iter-end-form iter)
             (let ((end-var (gensym "END")))
               (push (list end-var (loop-iter-end-form iter)) bindings)
               (push (ecase (loop-iter-end-test iter)
                       (:to    `(if (> ,var ,end-var) (return nil)))
                       (:below `(if (>= ,var ,end-var) (return nil)))
                       (:downto `(if (< ,var ,end-var) (return nil)))
                       (:above `(if (<= ,var ,end-var) (return nil))))
                     test-forms)))
           (if (and (loop-iter-end-test iter)
                    (member (loop-iter-end-test iter) '(:downto :above)))
               (push `(setq ,var (- ,var ,by)) step-stmts)
               (push `(setq ,var (+ ,var ,by)) step-stmts))))

        (:in
         (let ((var (loop-iter-var iter))
               (tmp (loop-iter-list-var iter)))
           (push (list tmp (loop-iter-init-form iter)) bindings)
           (push (list var nil) bindings)
           (push `(if (null ,tmp) (return nil)) test-forms)
           (push `(setq ,var (car ,tmp)) init-stmts)
           (push `(setq ,tmp (cdr ,tmp)) step-stmts)))

        (:on
         (let ((var (loop-iter-var iter)))
           (push (list var (loop-iter-init-form iter)) bindings)
           (push `(if (null ,var) (return nil)) test-forms)
           (push `(setq ,var (cdr ,var)) step-stmts)))

        (:across
         (let ((var (loop-iter-var iter))
               (idx (loop-iter-list-var iter))
               (arr (loop-iter-step-form iter)))
           (push (list arr (loop-iter-init-form iter)) bindings)
           (push (list idx 0) bindings)
           (push (list var nil) bindings)
           (push `(if (>= ,idx (array-length ,arr)) (return nil)) test-forms)
           (push `(setq ,var (aref ,arr ,idx)) init-stmts)
           (push `(setq ,idx (1+ ,idx)) step-stmts)))

        (:general
         (let ((var (loop-iter-var iter)))
           (if (eq (loop-iter-init-form iter) (loop-iter-step-form iter))
               ;; No THEN clause: re-evaluate each iteration in init-stmts.
               ;; This ensures correct ordering when referencing other loop
               ;; variables (e.g., "for entry in list for name = (first entry)").
               (progn
                 (push (list var nil) bindings)
                 (push `(setq ,var ,(loop-iter-init-form iter)) init-stmts))
               ;; Has THEN clause: init from binding, step from step-form
               (progn
                 (push (list var (loop-iter-init-form iter)) bindings)
                 (push `(setq ,var ,(loop-iter-step-form iter)) step-stmts)))))

        (:while
         (push `(if (null ,(loop-iter-init-form iter)) (return nil)) test-forms))

        (:until
         (push `(if ,(loop-iter-init-form iter) (return nil)) test-forms))

        (:repeat
         (let ((var (loop-iter-var iter)))
           (push (list var (loop-iter-init-form iter)) bindings)
           (push `(if (<= ,var 0) (return nil)) test-forms)
           (push `(setq ,var (1- ,var)) step-stmts)))))

    ;; Build accumulation body
    (let ((acc-body
            (when acc
              (case (car acc)
                (:collect
                 (list `(setq ,acc-var (cons ,(cadr acc) ,acc-var))))
                (:collect-when
                 (list `(when ,(cadr acc)
                          (setq ,acc-var (cons ,(caddr acc) ,acc-var)))))
                (:sum
                 (list `(setq ,acc-var (+ ,acc-var ,(cadr acc)))))
                (:count
                 (list `(when ,(cadr acc)
                          (setq ,acc-var (+ ,acc-var 1)))))))))

      ;; Construct the final form
      ;; (let* (bindings...)
      ;;   (loop
      ;;     tests...
      ;;     init-stmts...
      ;;     body...
      ;;     acc-body...
      ;;     step-stmts...
      ;;   )
      ;;   finally...
      ;;   acc-var or nil)
      (let* ((loop-body (append (nreverse test-forms)
                                (nreverse init-stmts)
                                body
                                acc-body
                                (nreverse step-stmts)))
             (inner `(loop ,@loop-body))
             (result (cond
                       ;; Collect returns reversed list (nreverse in prelude)
                       ((and acc (member (car acc) '(:collect :collect-when)))
                        `(progn ,inner ,@finally (nreverse ,acc-var)))
                       ;; Sum/count returns accumulator
                       ((and acc (member (car acc) '(:sum :count)))
                        `(progn ,inner ,@finally ,acc-var))
                       ;; No accumulator
                       (finally
                        `(progn ,inner ,@finally nil))
                       (t inner))))
        (if bindings
            `(let* ,(nreverse bindings) ,result)
            result)))))

(defun compile-return (value env dest)
  "Compile (return value) - exit from enclosing loop or function.
   If inside a loop, jumps to loop exit. Otherwise, compiles the value
   into VR and jumps to the function return epilogue."
  (cond
    (*loop-exit-label*
     (compile-form value env dest)
     (emit-ir :br *loop-exit-label*))
    (*function-return-label*
     ;; Function-level return: result goes to VR, jump to epilogue
     (compile-form value env +vreg-vr+)
     (emit-ir :br *function-return-label*))
    (t
     (error "MVM compiler: RETURN outside of LOOP or function"))))

;;; ============================================================
;;; Block / Tagbody / Go
;;; ============================================================

(defun compile-block (name body env dest)
  "Compile (block name forms...)"
  (let* ((exit-label (make-compiler-label))
         (*block-labels* (cons (cons name exit-label) *block-labels*)))
    (compile-progn body env dest)
    (emit-ir-label exit-label)))

(defun compile-tagbody (body env dest)
  "Compile (tagbody {tag | form}*)"
  (let ((*tagbody-tags* nil))
    ;; First pass: collect tags and create labels
    (dolist (item body)
      (when (symbolp item)
        (push (cons item (make-compiler-label)) *tagbody-tags*)))
    ;; Second pass: compile
    (dolist (item body)
      (if (symbolp item)
          ;; It's a tag: emit label
          (emit-ir-label (cdr (assoc item *tagbody-tags*)))
          ;; It's a form: compile it
          (compile-form item env dest)))
    ;; Tagbody returns nil
    (compile-nil dest)))

(defun compile-go (tag env dest)
  "Compile (go tag)"
  (declare (ignore env dest))
  (let ((entry (assoc tag *tagbody-tags*)))
    (unless entry
      (error "MVM compiler: unknown GO tag ~A" tag))
    (emit-ir :br (cdr entry))))

;;; ============================================================
;;; Dotimes
;;; ============================================================

(defun compile-dotimes (spec body env dest)
  "Compile (dotimes (var count) body...).
   Expands to: (let ((var 0)) (loop (if (< var count) (progn body (setq var (1+ var))) (return nil))))"
  (let* ((var (car spec))
         (count-form (cadr spec)))
    (compile-let
     (list (list var 0))
     (list (list 'loop
                 (list 'if (list '< var count-form)
                       (append (list 'progn)
                               body
                               (list (list 'setq var (list '1+ var))))
                       (list 'return nil))))
     env dest)))

;;; ============================================================
;;; Higher-Order: function / funcall
;;; ============================================================

(defun compile-function-ref (name env dest)
  "Compile (function fname) - return function address as tagged value"
  (declare (ignore env))
  ;; Emit a load-function-ref IR that will be resolved during linking
  (emit-ir :li-func dest (if (symbolp name) (symbol-name name) (string name))))

(defun compile-funcall (args env dest)
  "Compile (funcall f arg1 arg2 ...) - indirect function call"
  (let ((fn-form (car args))
        (call-args (cdr args))
        (nargs (length (cdr args)))
        (save-count (min *temp-reg-counter* 5)))
    ;; Save caller-saved temp registers (V5 through V(4+save-count-1))
    ;; Skip dest register — it will be overwritten with the CALL result.
    (when (> save-count 1)
      (loop for r from (+ +vreg-v4+ 1) below (+ +vreg-v4+ save-count)
            do (unless (= r dest)
                 (emit-ir :push r))))
    ;; Push overflow args FIRST (before populating V0-V3), because
    ;; evaluating overflow args may involve function calls that clobber V0-V3.
    (when (> nargs +max-reg-args+)
      (dolist (arg (reverse (nthcdr +max-reg-args+ call-args)))
        (let ((temp (alloc-temp-reg)))
          (compile-form arg env temp)
          (emit-ir :push temp)
          (free-temp-reg))))
    ;; Compile function expression into a temp, push on top of overflow args
    (let ((fn-reg (alloc-temp-reg)))
      (compile-form fn-form env fn-reg)
      (emit-ir :push fn-reg)
      (free-temp-reg))
    ;; Now compile register args using push/pop pattern (safe from clobbering)
    (let ((reg-count (min nargs +max-reg-args+)))
      (dotimes (i reg-count)
        (let ((temp (alloc-temp-reg)))
          (compile-form (nth i call-args) env temp)
          (emit-ir :push temp)
          (free-temp-reg)))
      (loop for i from (1- reg-count) downto 0
            do (emit-ir :pop (+ +vreg-v0+ i))))
    ;; Pop function address (on top of overflow args) and call indirect
    (let ((fn-call-reg (alloc-temp-reg)))
      (emit-ir :pop fn-call-reg)
      (emit-ir :call-indirect fn-call-reg nargs)
      (free-temp-reg))
    ;; Move result to dest
    (unless (= dest +vreg-vr+)
      (emit-ir :mov dest +vreg-vr+))
    ;; Clean up overflow args with POP (frame-free is NOP in translator)
    (when (> nargs +max-reg-args+)
      (let ((temp (alloc-temp-reg)))
        (dotimes (i (- nargs +max-reg-args+))
          (emit-ir :pop temp))
        (free-temp-reg)))
    ;; Restore caller-saved temp registers (reverse order, skip dest)
    (when (> save-count 1)
      (loop for r from (+ +vreg-v4+ save-count -1) downto (+ +vreg-v4+ 1)
            do (unless (= r dest)
                 (emit-ir :pop r))))))

;;; ============================================================
;;; Arithmetic Operations
;;; ============================================================

(defun compile-add (args env dest)
  "Compile (+ args...). Fixnum addition preserves tags for 2-arg case.
   Push/pop dest around each operand to survive function calls."
  (cond
    ((null args)
     ;; (+) = 0
     (compile-integer 0 dest))
    ((null (cdr args))
     ;; (+ x) = x
     (compile-form (car args) env dest))
    (t
     ;; (+ a b ...) -- evaluate pairwise, push/pop to preserve accumulator
     ;; across function calls in later args.
     (compile-form (car args) env dest)
     (dolist (arg (cdr args))
       (check-arith-nesting '+ arg)
       (let ((temp (alloc-temp-reg))
             (*arith-push-depth* (1+ *arith-push-depth*)))
         (emit-ir :push dest)
         (compile-form arg env temp)
         (emit-ir :pop dest)
         ;; Tagged fixnum add: (a<<1) + (b<<1) = (a+b)<<1
         (emit-ir :add dest dest temp)
         (free-temp-reg))))))

(defun compile-sub (args env dest)
  "Compile (- args...). Handles unary negation and multi-arg subtraction.
   Push/pop dest around each operand to survive function calls."
  (cond
    ((null args)
     (compile-integer 0 dest))
    ((null (cdr args))
     ;; Unary minus: (- x) = negate
     (compile-form (car args) env dest)
     (emit-ir :neg dest dest))
    (t
     ;; (- a b ...) -- subtract pairwise, push/pop to preserve accumulator
     ;; across function calls in later args.
     (compile-form (car args) env dest)
     (dolist (arg (cdr args))
       (check-arith-nesting '- arg)
       (let ((temp (alloc-temp-reg))
             (*arith-push-depth* (1+ *arith-push-depth*)))
         (emit-ir :push dest)
         (compile-form arg env temp)
         (emit-ir :pop dest)
         (emit-ir :sub dest dest temp)
         (free-temp-reg))))))

(defun compile-mul (args env dest)
  "Compile (* args...). For tagged fixnums: result = (a*b)>>1 to fix double-tag.
   Push/pop dest around each operand to survive function calls."
  (cond
    ((null args)
     ;; (*) = 1
     (compile-integer 1 dest))
    ((null (cdr args))
     ;; (* x) = x
     (compile-form (car args) env dest))
    (t
     ;; (* a b ...) -- multiply pairwise, push/pop to preserve accumulator
     ;; across function calls in later args.
     (compile-form (car args) env dest)
     (dolist (arg (cdr args))
       (check-arith-nesting '* arg)
       (let ((temp (alloc-temp-reg))
             (*arith-push-depth* (1+ *arith-push-depth*)))
         (emit-ir :push dest)
         (compile-form arg env temp)
         (emit-ir :pop dest)
         (emit-ir :mul dest dest temp)
         (free-temp-reg))))))

(defun compile-mul26lo (args env dest)
  "Compile (mul26lo a b) — low 26 bits of untag(a)*untag(b), tagged.
   Uses MUL26LO opcode for hardware wide multiply on 32-bit targets."
  (compile-form (car args) env dest)
  (let ((temp (alloc-temp-reg)))
    (emit-ir :push dest)
    (compile-form (cadr args) env temp)
    (emit-ir :pop dest)
    (emit-ir :mul26lo dest dest temp)
    (free-temp-reg)))

(defun compile-mul26hi (args env dest)
  "Compile (mul26hi a b) — bits 26+ of untag(a)*untag(b), tagged.
   Uses MUL26HI opcode for hardware wide multiply on 32-bit targets."
  (compile-form (car args) env dest)
  (let ((temp (alloc-temp-reg)))
    (emit-ir :push dest)
    (compile-form (cadr args) env temp)
    (emit-ir :pop dest)
    (emit-ir :mul26hi dest dest temp)
    (free-temp-reg)))

(defun compile-mul64lo (args env dest)
  "Compile (mul64lo a b) — low 64 bits of raw a*b. No tag/untag."
  (compile-form (car args) env dest)
  (let ((temp (alloc-temp-reg)))
    (emit-ir :push dest)
    (compile-form (cadr args) env temp)
    (emit-ir :pop dest)
    (emit-ir :mul64lo dest dest temp)
    (free-temp-reg)))

(defun compile-mul64hi (args env dest)
  "Compile (mul64hi a b) — high 64 bits of raw a*b. No tag/untag."
  (compile-form (car args) env dest)
  (let ((temp (alloc-temp-reg)))
    (emit-ir :push dest)
    (compile-form (cadr args) env temp)
    (emit-ir :pop dest)
    (emit-ir :mul64hi dest dest temp)
    (free-temp-reg)))

(defun compile-acc128 (args env dest)
  "Compile (acc128 addr lo hi) — mem128[addr] += hi:lo. Raw u64 values."
  (let ((addr-reg (alloc-temp-reg))
        (lo-reg (alloc-temp-reg))
        (hi-reg (alloc-temp-reg)))
    (compile-form (car args) env addr-reg)
    (compile-form (cadr args) env lo-reg)
    (compile-form (caddr args) env hi-reg)
    (emit-ir :acc128 addr-reg lo-reg hi-reg)
    (free-temp-reg)
    (free-temp-reg)
    (free-temp-reg)))

(defun compile-div (args env dest)
  "Compile (/ a b). Truncating integer division for tagged fixnums.
   Push/pop dest around divisor to survive function calls."
  (when (null args) (error "/ requires at least one argument"))
  (if (null (cdr args))
      ;; (/ x) = 1/x (not meaningful for integers, just return x)
      (compile-form (car args) env dest)
      ;; (/ a b ...) -- divide pairwise
      (progn
        (compile-form (car args) env dest)
        (dolist (arg (cdr args))
          (let ((temp (alloc-temp-reg)))
            (emit-ir :push dest)
            (compile-form arg env temp)
            (emit-ir :pop dest)
            (emit-ir :div dest dest temp)
            (free-temp-reg))))))

(defun compile-1+ (arg env dest)
  "Compile (1+ x) -> add tagged 1 (which is 2)"
  (compile-form arg env dest)
  (emit-ir :inc dest))

(defun compile-1- (arg env dest)
  "Compile (1- x) -> subtract tagged 1 (which is 2)"
  (compile-form arg env dest)
  (emit-ir :dec dest))

(defun compile-truncate (args env dest)
  "Compile (truncate a b) - integer division, quotient to DEST.
   Push/pop dest around second operand to survive function calls."
  (destructuring-bind (a b) args
    (let ((temp (alloc-temp-reg)))
      (compile-form a env dest)
      (emit-ir :push dest)
      (compile-form b env temp)
      (emit-ir :pop dest)
      (emit-ir :div dest dest temp)
      (free-temp-reg))))

(defun compile-mod (args env dest)
  "Compile (mod a b) - integer modulus, remainder to DEST.
   Push/pop dest around second operand to survive function calls."
  (destructuring-bind (a b) args
    (let ((temp (alloc-temp-reg)))
      (compile-form a env dest)
      (emit-ir :push dest)
      (compile-form b env temp)
      (emit-ir :pop dest)
      (emit-ir :mod dest dest temp)
      (free-temp-reg))))

;;; ============================================================
;;; Comparison Operations
;;; ============================================================

(defun compile-compare (branch-op args env dest)
  "Compile a comparison (<, >, =, <=, >=) producing T or NIL.
   BRANCH-OP is the branch instruction keyword to use when the comparison
   is true (:blt, :bgt, :beq, :ble, :bge).
   Push/pop dest around second operand to survive function calls."
  (destructuring-bind (a b) args
    (let ((temp (alloc-temp-reg))
          (true-label (make-compiler-label))
          (end-label (make-compiler-label)))
      (compile-form a env dest)
      (emit-ir :push dest)
      (compile-form b env temp)
      (emit-ir :pop dest)
      ;; Compare dest vs temp
      (emit-ir :cmp dest temp)
      ;; Branch if true
      (emit-ir branch-op true-label)
      ;; False: load NIL
      (compile-nil dest)
      (emit-ir :br end-label)
      ;; True: load T
      (emit-ir-label true-label)
      (compile-t dest)
      ;; Join
      (emit-ir-label end-label)
      (free-temp-reg))))

(defun compile-eq (args env dest)
  "Compile (eq a b) - pointer equality.
   Push/pop dest around second operand to survive function calls."
  (destructuring-bind (a b) args
    (let ((temp (alloc-temp-reg))
          (true-label (make-compiler-label))
          (end-label (make-compiler-label)))
      (compile-form a env dest)
      (emit-ir :push dest)
      (compile-form b env temp)
      (emit-ir :pop dest)
      (emit-ir :cmp dest temp)
      (emit-ir :beq true-label)
      ;; Not equal: NIL
      (compile-nil dest)
      (emit-ir :br end-label)
      ;; Equal: T
      (emit-ir-label true-label)
      (compile-t dest)
      (emit-ir-label end-label)
      (free-temp-reg))))

;;; ============================================================
;;; List Operations
;;; ============================================================

(defun compile-car (arg env dest)
  "Compile (car x) -> MVM car instruction"
  (compile-form arg env dest)
  (emit-ir :car dest dest))

(defun compile-cdr (arg env dest)
  "Compile (cdr x) -> MVM cdr instruction"
  (compile-form arg env dest)
  (emit-ir :cdr dest dest))

(defun compile-cons (car-arg cdr-arg env dest)
  "Compile (cons x y) -> MVM cons instruction (allocating).
   Saves car result to stack before evaluating cdr to prevent
   temp register exhaustion with deeply nested cons expressions."
  ;; Evaluate car into dest, save to stack
  (compile-form car-arg env dest)
  (emit-ir :push dest)
  ;; Evaluate cdr into dest (car is safe on stack)
  (compile-form cdr-arg env dest)
  ;; Move cdr to temp, restore car
  (let ((temp (alloc-temp-reg)))
    (emit-ir :mov temp dest)
    (emit-ir :pop dest)
    ;; GC check before allocation
    (emit-ir :gc-check)
    ;; Cons: dest = cons(dest, temp)
    (emit-ir :cons dest dest temp)
    (free-temp-reg)))

(defun compile-set-car (cell-arg value-arg env dest)
  "Compile (set-car cell value).
   Cell goes into a callee-saved temp (V4/RBX), value into dest.
   This avoids VR clobber when value-arg is a function call."
  (let ((cell-reg (alloc-temp-reg)))
    (compile-form cell-arg env cell-reg)
    (compile-form value-arg env dest)
    (emit-ir :setcar cell-reg dest)
    (emit-ir :write-barrier cell-reg)
    (free-temp-reg)))

(defun compile-set-cdr (cell-arg value-arg env dest)
  "Compile (set-cdr cell value).
   Cell goes into a callee-saved temp (V4/RBX), value into dest."
  (let ((cell-reg (alloc-temp-reg)))
    (compile-form cell-arg env cell-reg)
    (compile-form value-arg env dest)
    (emit-ir :setcdr cell-reg dest)
    (emit-ir :write-barrier cell-reg)
    (free-temp-reg)))

;; Compound accessors

(defun compile-caar (arg env dest)
  "Compile (caar x) = (car (car x))"
  (compile-form arg env dest)
  (emit-ir :car dest dest)
  (emit-ir :car dest dest))

(defun compile-cadr (arg env dest)
  "Compile (cadr x) = (car (cdr x))"
  (compile-form arg env dest)
  (emit-ir :cdr dest dest)
  (emit-ir :car dest dest))

(defun compile-cdar (arg env dest)
  "Compile (cdar x) = (cdr (car x))"
  (compile-form arg env dest)
  (emit-ir :car dest dest)
  (emit-ir :cdr dest dest))

(defun compile-cddr (arg env dest)
  "Compile (cddr x) = (cdr (cdr x))"
  (compile-form arg env dest)
  (emit-ir :cdr dest dest)
  (emit-ir :cdr dest dest))

;;; ============================================================
;;; Bitwise Operations
;;; ============================================================

(defun flatten-arith-args (op args)
  "Flatten nested associative arithmetic: (op A (op B C)) → (op A B C).
   Only flattens when the nested form uses the SAME operator.
   This prevents push/pop stack corruption from deeply nested arithmetic
   with function call operands."
  (let ((result nil))
    (dolist (arg args)
      (if (and (consp arg)
               (symbolp (car arg))
               (eq (car arg) op))
          ;; Recursively flatten: (logior A (logior B (logior C D))) → (A B C D)
          (dolist (inner (flatten-arith-args op (cdr arg)))
            (push inner result))
          (push arg result)))
    (nreverse result)))

(defun compile-logand (args env dest)
  "Compile (logand args...).
   Flattens nested logand to prevent push/pop stack corruption.
   Push/pop dest around each operand to survive function calls."
  (let ((flat-args (flatten-arith-args 'logand args)))
    (cond
      ((null flat-args)
       ;; (logand) = -1
       (emit-ir :li dest -1))
      ((null (cdr flat-args))
       (compile-form (car flat-args) env dest))
      (t
       (compile-form (car flat-args) env dest)
       (dolist (arg (cdr flat-args))
         (check-arith-nesting 'logand arg)
         (let ((temp (alloc-temp-reg))
               (*arith-push-depth* (1+ *arith-push-depth*)))
           (emit-ir :push dest)
           (compile-form arg env temp)
           (emit-ir :pop dest)
           (emit-ir :and dest dest temp)
           (free-temp-reg)))))))

(defun compile-logior (args env dest)
  "Compile (logior args...).
   Flattens nested logior to prevent push/pop stack corruption.
   Push/pop dest around each operand to survive function calls."
  (let ((flat-args (flatten-arith-args 'logior args)))
    (cond
      ((null flat-args)
       (compile-integer 0 dest))
      ((null (cdr flat-args))
       (compile-form (car flat-args) env dest))
      (t
       (compile-form (car flat-args) env dest)
       (dolist (arg (cdr flat-args))
         (check-arith-nesting 'logior arg)
         (let ((temp (alloc-temp-reg))
               (*arith-push-depth* (1+ *arith-push-depth*)))
           (emit-ir :push dest)
           (compile-form arg env temp)
           (emit-ir :pop dest)
           (emit-ir :or dest dest temp)
           (free-temp-reg)))))))

(defun compile-logxor (args env dest)
  "Compile (logxor args...).
   Flattens nested logxor to prevent push/pop stack corruption.
   Push/pop dest around each operand to survive function calls."
  (let ((flat-args (flatten-arith-args 'logxor args)))
    (cond
      ((null flat-args)
       (compile-integer 0 dest))
      ((null (cdr flat-args))
       (compile-form (car flat-args) env dest))
      (t
       (compile-form (car flat-args) env dest)
       (dolist (arg (cdr flat-args))
         (check-arith-nesting 'logxor arg)
         (let ((temp (alloc-temp-reg))
               (*arith-push-depth* (1+ *arith-push-depth*)))
           (emit-ir :push dest)
           (compile-form arg env temp)
           (emit-ir :pop dest)
           (emit-ir :xor dest dest temp)
           (free-temp-reg)))))))

(defun compile-ash (value-form count-form env dest)
  "Compile (ash value count) - arithmetic shift.
   Positive count = left shift, negative = right shift.
   Handles both constant and variable shift counts."
  (compile-form value-form env dest)
  (cond
    ;; Constant shift count
    ((integerp count-form)
     (if (>= count-form 0)
         ;; Left shift
         (emit-ir :shl dest dest count-form)
         ;; Right shift, then fix tag
         (progn
           (emit-ir :sar dest dest (- count-form))
           ;; AND with ~1 to ensure fixnum tag (low bit 0)
           (let ((temp (alloc-temp-reg)))
             (emit-ir :li temp -2)
             (emit-ir :and dest dest temp)
             (free-temp-reg)))))
    ;; Variable shift count
    (t
     (check-arith-nesting 'ash count-form)
     (let ((count-reg (alloc-temp-reg))
           (pos-label (make-compiler-label))
           (neg-label (make-compiler-label))
           (done-label (make-compiler-label))
           (*arith-push-depth* (1+ *arith-push-depth*)))
       ;; Push/pop dest around count evaluation to survive function calls
       (emit-ir :push dest)
       (compile-form count-form env count-reg)
       (emit-ir :pop dest)
       ;; Untag both: sar by 1
       (emit-ir :sar dest dest 1)
       (emit-ir :sar count-reg count-reg 1)
       ;; Test sign of count
       (let ((zero-reg (alloc-temp-reg)))
         (emit-ir :li zero-reg 0)
         (emit-ir :cmp count-reg zero-reg)
         (free-temp-reg))
       (emit-ir :bge pos-label)
       ;; Negative (right shift): negate count, sar
       (emit-ir :neg count-reg count-reg)
       (emit-ir :sar-var dest dest count-reg)
       (emit-ir :br done-label)
       ;; Positive (left shift): shl
       (emit-ir-label pos-label)
       (emit-ir :shl-var dest dest count-reg)
       ;; Done: re-tag
       (emit-ir-label done-label)
       (emit-ir :shl dest dest 1)
       (free-temp-reg)))))

(defun compile-ldb (bytespec value-form env dest)
  "Compile (ldb (byte size pos) value) - extract bit field"
  (unless (and (consp bytespec)
               (name-eq (car bytespec) "BYTE"))
    (error "MVM compiler: LDB requires (byte size pos) form, got ~S" bytespec))
  (let ((size (cadr bytespec))
        (pos (caddr bytespec)))
    (compile-form value-form env dest)
    ;; Shift right by position
    (when (and (integerp pos) (> pos 0))
      (emit-ir :shr dest dest pos))
    ;; Mask to size bits
    (when (integerp size)
      (let ((mask (1- (ash 1 size)))
            (temp (alloc-temp-reg)))
        (emit-ir :li temp mask)
        (emit-ir :and dest dest temp)
        (free-temp-reg)))))

;;; ============================================================
;;; Type Predicates
;;; ============================================================
;;;
;;; Each predicate compiles to: test + conditional branch -> T or NIL.

(defun compile-type-predicate-branch (dest true-label end-label)
  "Helper: emit the false->NIL, true->T pattern for type predicates"
  ;; Fall through = false
  (compile-nil dest)
  (emit-ir :br end-label)
  ;; True path
  (emit-ir-label true-label)
  (compile-t dest)
  (emit-ir-label end-label))

(defun compile-null (arg env dest)
  "Compile (null x) or (not x) - true if x is NIL"
  (let ((true-label (make-compiler-label))
        (end-label (make-compiler-label)))
    (compile-form arg env dest)
    (emit-ir :bnull dest true-label)
    ;; Not nil -> return NIL
    (compile-nil dest)
    (emit-ir :br end-label)
    ;; Was nil -> return T
    (emit-ir-label true-label)
    (compile-t dest)
    (emit-ir-label end-label)))

(defun compile-consp (arg env dest)
  "Compile (consp x) - true if x has cons tag.
   The MVM consp instruction produces a tagged boolean (T or NIL) in dest."
  (compile-form arg env dest)
  ;; MVM consp: dest = (consp? src) -> T or NIL
  (emit-ir :consp dest dest))

(defun compile-fixnump (arg env dest)
  "Compile (fixnump x) - true if low bit is 0"
  (let ((temp (alloc-temp-reg))
        (true-label (make-compiler-label))
        (end-label (make-compiler-label)))
    (compile-form arg env dest)
    ;; Test low bit: AND with 1
    (emit-ir :li temp 1)
    (emit-ir :test dest temp)
    ;; If zero flag set (low bit is 0), it's a fixnum
    (emit-ir :beq true-label)
    (compile-type-predicate-branch dest true-label end-label)
    (free-temp-reg)))

(defun compile-atom-p (arg env dest)
  "Compile (atom x) - true if x is not a cons.
   The MVM atom instruction produces a tagged boolean (T or NIL) in dest."
  (compile-form arg env dest)
  ;; MVM atom: dest = (atom? src) -> T or NIL
  (emit-ir :atom dest dest))

(defun compile-listp (arg env dest)
  "Compile (listp x) - true if x is NIL or a cons"
  (let ((true-label (make-compiler-label))
        (check-cons-label (make-compiler-label))
        (end-label (make-compiler-label)))
    (compile-form arg env dest)
    ;; Check for nil first
    (emit-ir :bnull dest true-label)
    ;; Not nil: check if cons
    (let ((temp (alloc-temp-reg)))
      (emit-ir :consp temp dest)
      (emit-ir :bnnull temp true-label)
      (free-temp-reg))
    ;; Neither nil nor cons
    (compile-nil dest)
    (emit-ir :br end-label)
    ;; Is a list (nil or cons)
    (emit-ir-label true-label)
    (compile-t dest)
    (emit-ir-label end-label)))

(defun compile-bignump (arg env dest)
  "Compile (bignump x) - true if object tag with bignum subtag"
  (let ((true-label (make-compiler-label))
        (end-label (make-compiler-label))
        (false-label (make-compiler-label))
        (temp (alloc-temp-reg))
        (temp2 (alloc-temp-reg)))
    (compile-form arg env dest)
    ;; Extract tag: AND with #x0F
    (emit-ir :li temp #x0F)
    (emit-ir :and temp dest temp)
    ;; Compare to object tag (#x09)
    (emit-ir :li temp2 +tag-object+)
    (emit-ir :cmp temp temp2)
    (emit-ir :bne false-label)  ; wrong tag -> false
    ;; Has object tag: check subtag
    ;; Extract subtag from header at [obj & ~0xF]
    (emit-ir :obj-subtag temp dest)
    (emit-ir :li temp2 (ash +subtag-bignum+ +fixnum-shift+))
    (emit-ir :cmp temp temp2)
    (emit-ir :beq true-label)
    ;; False
    (emit-ir-label false-label)
    (compile-nil dest)
    (emit-ir :br end-label)
    ;; True
    (emit-ir-label true-label)
    (compile-t dest)
    (emit-ir-label end-label)
    (free-temp-reg)
    (free-temp-reg)))

(defun compile-object-subtype-p (arg env dest expected-subtag)
  "Helper: compile a predicate that checks for object tag + specific subtag"
  (let ((true-label (make-compiler-label))
        (end-label (make-compiler-label))
        (false-label (make-compiler-label))
        (temp (alloc-temp-reg))
        (temp2 (alloc-temp-reg)))
    (compile-form arg env dest)
    ;; Check object tag
    ;; OBJ-TAG/OBJ-SUBTAG return tagged fixnums (value << fixnum-shift),
    ;; so comparison values must also be tagged.
    (emit-ir :obj-tag temp dest)
    (emit-ir :li temp2 (ash +tag-object+ +fixnum-shift+))
    (emit-ir :cmp temp temp2)
    (emit-ir :bne false-label)
    ;; Check subtag
    (emit-ir :obj-subtag temp dest)
    (emit-ir :li temp2 (ash expected-subtag +fixnum-shift+))
    (emit-ir :cmp temp temp2)
    (emit-ir :beq true-label)
    ;; False
    (emit-ir-label false-label)
    (compile-nil dest)
    (emit-ir :br end-label)
    ;; True
    (emit-ir-label true-label)
    (compile-t dest)
    (emit-ir-label end-label)
    (free-temp-reg)
    (free-temp-reg)))

(defun compile-stringp (arg env dest)
  "Compile (stringp x)"
  (compile-object-subtype-p arg env dest +subtag-string+))

(defun compile-arrayp (arg env dest)
  "Compile (arrayp x)"
  (compile-object-subtype-p arg env dest +subtag-array+))

(defun compile-integerp (arg env dest)
  "Compile (integerp x) - true if fixnum or bignum"
  (let ((check-bignum-label (make-compiler-label))
        (true-label (make-compiler-label))
        (false-label (make-compiler-label))
        (end-label (make-compiler-label))
        (temp (alloc-temp-reg))
        (temp2 (alloc-temp-reg)))
    (compile-form arg env dest)
    ;; Check fixnum: low bit 0
    (emit-ir :li temp 1)
    (emit-ir :test dest temp)
    (emit-ir :bne check-bignum-label)
    ;; Low bit 0: fixnum if not nil
    (emit-ir :bnull dest false-label)
    (emit-ir :br true-label)
    ;; Check bignum
    (emit-ir-label check-bignum-label)
    (emit-ir :obj-tag temp dest)
    (emit-ir :li temp2 (ash +tag-object+ +fixnum-shift+))
    (emit-ir :cmp temp temp2)
    (emit-ir :bne false-label)
    ;; Check subtag
    (emit-ir :obj-subtag temp dest)
    (emit-ir :li temp2 (ash +subtag-bignum+ +fixnum-shift+))
    (emit-ir :cmp temp temp2)
    (emit-ir :beq true-label)
    ;; False
    (emit-ir-label false-label)
    (compile-nil dest)
    (emit-ir :br end-label)
    ;; True
    (emit-ir-label true-label)
    (compile-t dest)
    (emit-ir-label end-label)
    (free-temp-reg)
    (free-temp-reg)))

(defun compile-zerop (arg env dest)
  "Compile (zerop x) - true if x is fixnum zero"
  (let ((true-label (make-compiler-label))
        (end-label (make-compiler-label))
        (temp (alloc-temp-reg)))
    (compile-form arg env dest)
    ;; Tagged fixnum 0 is 0
    (emit-ir :li temp 0)
    (emit-ir :cmp dest temp)
    (emit-ir :beq true-label)
    ;; Not zero
    (compile-nil dest)
    (emit-ir :br end-label)
    ;; Zero
    (emit-ir-label true-label)
    (compile-t dest)
    (emit-ir-label end-label)
    (free-temp-reg)))

(defun compile-characterp (arg env dest)
  "Compile (characterp x) - true if low byte = #x05"
  (let ((true-label (make-compiler-label))
        (end-label (make-compiler-label))
        (temp (alloc-temp-reg))
        (temp2 (alloc-temp-reg)))
    (compile-form arg env dest)
    ;; Extract low byte
    (emit-ir :li temp #xFF)
    (emit-ir :and temp dest temp)
    ;; Compare to char tag
    (emit-ir :li temp2 +char-tag+)
    (emit-ir :cmp temp temp2)
    (emit-ir :beq true-label)
    ;; Not character
    (compile-nil dest)
    (emit-ir :br end-label)
    ;; Is character
    (emit-ir-label true-label)
    (compile-t dest)
    (emit-ir-label end-label)
    (free-temp-reg)
    (free-temp-reg)))

;;; ============================================================
;;; Character Operations
;;; ============================================================

(defun compile-char-code (arg env dest)
  "Compile (char-code c) - extract character code as fixnum.
   Character: code in bits 8+. Fixnum: value << 1.
   Net shift: right by (char-shift - fixnum-shift) = 7."
  (compile-form arg env dest)
  (emit-ir :sar dest dest (- +char-shift+ +fixnum-shift+)))

(defun compile-code-char (arg env dest)
  "Compile (code-char n) - create character from code.
   Input: fixnum (code << 1). Output: (code << 8) | #x05.
   Net shift: left by 7, then OR with char tag."
  (compile-form arg env dest)
  (emit-ir :shl dest dest (- +char-shift+ +fixnum-shift+))
  (let ((temp (alloc-temp-reg)))
    (emit-ir :li temp +char-tag+)
    (emit-ir :or dest dest temp)
    (free-temp-reg)))

;;; ============================================================
;;; Memory Operations
;;; ============================================================

(defun memory-width-code (type-form)
  "Convert a type keyword to an MVM memory width code.
   Returns (cons width-code needs-tag-p)"
  (let ((actual-type (if (and (consp type-form)
                               (name-eq (car type-form) "QUOTE"))
                          (cadr type-form)
                          type-form)))
    (cond
      ((or (eq actual-type :u8)  (name-eq actual-type "U8"))
       (cons +width-u8+ t))
      ((or (eq actual-type :u16) (name-eq actual-type "U16"))
       (cons +width-u16+ t))
      ((or (eq actual-type :u32) (name-eq actual-type "U32"))
       (cons +width-u32+ t))
      ((or (eq actual-type :u64) (name-eq actual-type "U64"))
       (cons +width-u64+ nil))
      (t
       ;; Default: u64, no tagging (raw pointer)
       (cons +width-u64+ nil)))))

(defun compile-mem-ref (addr-form type-form env dest)
  "Compile (mem-ref addr type) - raw memory read.
   Address is a tagged fixnum. Type controls width and tagging."
  (compile-form addr-form env dest)
  ;; Untag address: logical shift right by 1
  ;; Must use SHR (not SAR) because on 32-bit targets, addresses >= 0x40000000
  ;; have the sign bit set in their tagged representation, and SAR would
  ;; sign-extend to the wrong address.
  (emit-ir :shr dest dest +fixnum-shift+)
  ;; Load from memory
  (let* ((wt (memory-width-code type-form))
         (width (car wt))
         (needs-tag (cdr wt)))
    (emit-ir :load dest dest width)
    ;; Tag result as fixnum if needed (u8/u16/u32)
    (when needs-tag
      (emit-ir :shl dest dest +fixnum-shift+))))

(defun compile-setf (place value-form env dest)
  "Compile (setf place value)"
  (cond
    ;; (setf (mem-ref addr type) value)
    ((and (consp place) (name-eq (car place) "MEM-REF"))
     (let ((addr-form (cadr place))
           (type-form (caddr place)))
       (let* ((wt2 (memory-width-code type-form))
              (width (car wt2))
              (needs-untag (cdr wt2)))
         (let ((addr-reg (alloc-temp-reg)))
           ;; Compile value first
           (compile-form value-form env dest)
           ;; Save value across address evaluation (may involve function calls
           ;; that clobber caller-saved regs including dest)
           (emit-ir :push dest)
           ;; Compile address
           (compile-form addr-form env addr-reg)
           ;; Restore value
           (emit-ir :pop dest)
           ;; Untag address (logical shift right, not arithmetic — see compile-mem-ref)
           (emit-ir :shr addr-reg addr-reg +fixnum-shift+)
           ;; Untag value for sub-64-bit stores
           (when needs-untag
             (emit-ir :sar dest dest +fixnum-shift+))
           ;; Store
           (emit-ir :store addr-reg dest width)
           ;; Re-tag value in dest if we untagged it (for return value)
           (when needs-untag
             (emit-ir :shl dest dest +fixnum-shift+))
           (free-temp-reg)))))

    ;; (setf var value) = (setq var value)
    ((symbolp place)
     (compile-setq place value-form env dest))

    (t
     (error "MVM compiler: unsupported SETF place ~S" place))))

;;; ============================================================
;;; I/O Port Operations
;;; ============================================================

(defun compile-io-out-byte (port-form value-form env dest)
  "Compile (io-out-byte port value) - write byte to I/O port.
   Port must be a compile-time constant (embedded as imm16 in bytecode)."
  (unless (integerp port-form)
    (error "MVM compiler: io-out-byte requires constant port, got ~S" port-form))
  (compile-form value-form env dest)
  ;; io-write: port(imm16), value(reg), width(u8)
  (emit-ir :io-write port-form dest +width-u8+)
  ;; Return 0
  (emit-ir :li dest 0))

(defun compile-io-in-byte (port-form env dest)
  "Compile (io-in-byte port) - read byte from I/O port.
   Port must be a compile-time constant (embedded as imm16 in bytecode)."
  (unless (integerp port-form)
    (error "MVM compiler: io-in-byte requires constant port, got ~S" port-form))
  ;; io-read: dest(reg), port(imm16), width(u8)
  (emit-ir :io-read dest port-form +width-u8+))

(defun compile-io-out-dword (port-form value-form env dest)
  "Compile (io-out-dword port value) - write dword to I/O port.
   Port must be a compile-time constant."
  (unless (integerp port-form)
    (error "MVM compiler: io-out-dword requires constant port, got ~S" port-form))
  (compile-form value-form env dest)
  (emit-ir :io-write port-form dest +width-u32+)
  (emit-ir :li dest 0))

(defun compile-io-in-dword (port-form env dest)
  "Compile (io-in-dword port) - read dword from I/O port.
   Port must be a compile-time constant."
  (unless (integerp port-form)
    (error "MVM compiler: io-in-dword requires constant port, got ~S" port-form))
  (emit-ir :io-read dest port-form +width-u32+))

;;; ============================================================
;;; System Register Operations
;;; ============================================================

(defun compile-get-alloc-ptr (dest)
  "Compile (get-alloc-ptr) - return VA tagged as fixnum"
  (emit-ir :mov dest +vreg-va+)
  (emit-ir :shl dest dest +fixnum-shift+))

(defun compile-get-alloc-limit (dest)
  "Compile (get-alloc-limit) - return VL tagged as fixnum"
  (emit-ir :mov dest +vreg-vl+)
  (emit-ir :shl dest dest +fixnum-shift+))

(defun compile-set-alloc-ptr (form env dest)
  "Compile (set-alloc-ptr value) - set VA from tagged fixnum"
  (compile-form form env dest)
  (emit-ir :shr dest dest +fixnum-shift+)
  (emit-ir :mov +vreg-va+ dest))

(defun compile-set-alloc-limit (form env dest)
  "Compile (set-alloc-limit value) - set VL from tagged fixnum"
  (compile-form form env dest)
  (emit-ir :shr dest dest +fixnum-shift+)
  (emit-ir :mov +vreg-vl+ dest))

(defun compile-untag (form env dest)
  "Compile (untag value) - remove fixnum tag (logical shift, unsigned)"
  (compile-form form env dest)
  (emit-ir :shr dest dest +fixnum-shift+))

;;; ============================================================
;;; Actor/Context Primitives
;;; ============================================================

(defun compile-save-context (addr-form env dest)
  "Compile (save-context addr) - save registers to actor struct.
   Returns 0 on initial save, 1 when resumed via restore-context.
   Untags the address before passing to save-ctx."
  (compile-form addr-form env dest)
  ;; Untag: address is a tagged fixnum, shift right by 1 to get raw address
  ;; Must use SHR (not SAR) for 32-bit targets where high addresses set sign bit
  (emit-ir :shr dest dest +fixnum-shift+)
  ;; The save-ctx MVM instruction handles the actual save.
  ;; It stores the continuation point internally.
  (emit-ir :save-ctx dest)
  ;; Result is in dest: 0 for initial save, tagged 1 for resume
  )

(defun compile-restore-context (addr-form env dest)
  "Compile (restore-context addr) - restore registers from actor struct.
   This never returns to the caller.
   Untags the address before passing to restore-ctx."
  (compile-form addr-form env dest)
  ;; Untag: address is a tagged fixnum, shift right by 1 to get raw address
  ;; Must use SHR (not SAR) for 32-bit targets where high addresses set sign bit
  (emit-ir :shr dest dest +fixnum-shift+)
  (emit-ir :restore-ctx dest)
  ;; restore-ctx never returns, but we need dest for type consistency
  )

(defun compile-call-native (args env dest)
  "Compile (call-native addr arg1 arg2) - call native code at address.
   Address is a tagged fixnum. Up to 2 arguments supported."
  (let ((addr-form (first args))
        (arg-forms (rest args)))
    ;; Compile address
    (compile-form addr-form env dest)
    ;; Untag address (SHR for 32-bit safety)
    (emit-ir :shr dest dest +fixnum-shift+)
    ;; Place arguments in V0, V1
    (loop for arg-form in arg-forms
          for i from 0
          for areg = (+ +vreg-v0+ i)
          while (< i 2)
          do (let ((temp (alloc-temp-reg)))
               (compile-form arg-form env temp)
               (emit-ir :mov areg temp)
               (free-temp-reg)))
    ;; Indirect call
    (emit-ir :call-native dest (length arg-forms))
    ;; Result is in VR
    (unless (= dest +vreg-vr+)
      (emit-ir :mov dest +vreg-vr+))))

(defun compile-fn-addr (name dest)
  "Compile (fn-addr name) - load tagged native function address.
   Resolves function name at link time via FN-ADDR opcode."
  (let ((fn-name (if (symbolp name) (symbol-name name) (string name))))
    (emit-ir :fn-addr dest fn-name)))

;;; ============================================================
;;; SMP Primitives
;;; ============================================================

(defun compile-xchg-mem (addr-form val-form env dest)
  "Compile (xchg-mem addr val) - atomic exchange.
   Both addr and val are tagged fixnums."
  (let ((addr-reg (alloc-temp-reg))
        (val-reg (alloc-temp-reg)))
    (compile-form addr-form env addr-reg)
    (compile-form val-form env val-reg)
    ;; Untag both (SHR for address, SAR for value to preserve sign)
    (emit-ir :shr addr-reg addr-reg +fixnum-shift+)
    (emit-ir :sar val-reg val-reg +fixnum-shift+)
    ;; Atomic exchange: dest = old value at [addr], [addr] = val
    (emit-ir :atomic-xchg dest addr-reg val-reg)
    ;; Re-tag result
    (emit-ir :shl dest dest +fixnum-shift+)
    (free-temp-reg)
    (free-temp-reg)))

(defun compile-pause (dest)
  "Compile (pause) - spin-wait hint. Returns 0."
  (emit-ir :nop)  ; PAUSE maps to NOP in the VM (native backend converts to PAUSE)
  (emit-ir :li dest 0))

(defun compile-mfence (dest)
  "Compile (mfence) - full memory barrier. Returns 0."
  (emit-ir :fence)
  (emit-ir :li dest 0))

(defun compile-hlt (dest)
  "Compile (hlt) - halt CPU. Returns 0."
  (emit-ir :halt)
  (emit-ir :li dest 0))

;; --- SAP (System Area Pointer) ---

(defun compile-make-sap (args env dest)
  "Compile (make-sap addr) — allocate SAP wrapping an address.
   addr is a tagged fixnum (Lisp integer of the byte address).
   Untagged (SHR 1) before storing in the SAP."
  (compile-form (car args) env +vreg-v0+)
  (emit-ir :shr +vreg-v0+ +vreg-v0+ +fixnum-shift+)
  (emit-ir :sap-new dest +vreg-v0+))

(defun compile-make-sap-raw (args env dest)
  "Compile (make-sap-raw raw-u64) — allocate SAP wrapping a raw u64 address.
   raw-u64 is already untagged (e.g., from mem-ref :u64 or syscall result).
   Stored directly without untagging."
  (compile-form (car args) env +vreg-v0+)
  (emit-ir :sap-new dest +vreg-v0+))

(defun compile-sap-ref (args env dest width)
  "Compile (sap-ref-N sap offset) — read from SAP address + offset."
  (compile-form (car args) env +vreg-v0+)
  (compile-form (cadr args) env +vreg-v1+)
  (ecase width
    (:u8  (emit-ir :sap-ref8  dest +vreg-v0+ +vreg-v1+))
    (:u32 (emit-ir :sap-ref32 dest +vreg-v0+ +vreg-v1+))
    (:u64 (emit-ir :sap-ref64 dest +vreg-v0+ +vreg-v1+))))

(defun compile-sap-set (args env width)
  "Compile (sap-set-N sap offset val) — write to SAP address + offset."
  (compile-form (car args) env +vreg-v0+)
  (compile-form (cadr args) env +vreg-v1+)
  (compile-form (caddr args) env +vreg-v2+)
  (ecase width
    (:u8  (emit-ir :sap-set8  +vreg-v0+ +vreg-v1+ +vreg-v2+))
    (:u32 (emit-ir :sap-set32 +vreg-v0+ +vreg-v1+ +vreg-v2+))
    (:u64 (emit-ir :sap-set64 +vreg-v0+ +vreg-v1+ +vreg-v2+))))

(defun compile-sap-addr (args env dest)
  "Compile (sap-address sap) — extract raw address from SAP."
  (compile-form (car args) env +vreg-v0+)
  (emit-ir :sap-addr dest +vreg-v0+))

;; --- Serial Console ---

(defun compile-write-char-serial (args env dest)
  "Compile (write-char-serial char-code) — write character to serial port.
   The argument is a fixnum containing the ASCII code.
   Uses TRAP #x0300 with the value in V0."
  (compile-form (car args) env +vreg-v0+)
  (emit-ir :trap #x0300)
  (emit-ir :li dest 0))

(defun compile-sys-exit (args env dest)
  "Compile (sys-exit code) — exit the process (Linux).
   Uses TRAP #x0500 with exit code in V0."
  (compile-form (car args) env +vreg-v0+)
  (emit-ir :trap #x0500)
  (emit-ir :li dest 0))

(defun compile-syscall3 (args env dest)
  "Compile (syscall3 num arg1 arg2 arg3) — 3-arg Linux syscall.
   All arguments are tagged fixnums, untagged before syscall.
   Result is tagged fixnum in V0.
   Evaluates args to V4-V7 first, then MOVs to V0-V3 to avoid clobber."
  (compile-form (car args) env +vreg-v4+)
  (compile-form (cadr args) env +vreg-v5+)
  (compile-form (caddr args) env +vreg-v6+)
  (compile-form (cadddr args) env +vreg-v7+)
  (emit-ir :mov +vreg-v0+ +vreg-v4+)
  (emit-ir :mov +vreg-v1+ +vreg-v5+)
  (emit-ir :mov +vreg-v2+ +vreg-v6+)
  (emit-ir :mov +vreg-v3+ +vreg-v7+)
  (emit-ir :trap #x0502)
  (emit-ir :mov dest +vreg-v0+))

(defun compile-syscall3-raw (args env dest)
  "Compile (syscall3-raw num arg1 arg2 arg3) — 3-arg Linux syscall.
   V0 (syscall#) is untagged. V1-V3 passed as-is (raw).
   Result is raw (untagged) in V0."
  (compile-form (car args) env +vreg-v4+)
  (compile-form (cadr args) env +vreg-v5+)
  (compile-form (caddr args) env +vreg-v6+)
  (compile-form (cadddr args) env +vreg-v7+)
  (emit-ir :mov +vreg-v0+ +vreg-v4+)
  (emit-ir :mov +vreg-v1+ +vreg-v5+)
  (emit-ir :mov +vreg-v2+ +vreg-v6+)
  (emit-ir :mov +vreg-v3+ +vreg-v7+)
  (emit-ir :trap #x0503)
  (emit-ir :mov dest +vreg-v0+))

(defun compile-read-char-serial (dest)
  "Compile (read-char-serial) — read a character from the serial port.
   Uses TRAP #x0301; result is a tagged fixnum char code in V0."
  (emit-ir :trap #x0301)
  (emit-ir :mov dest +vreg-v0+))

(defun compile-rdtsc (dest)
  "Compile (rdtsc) — read timestamp counter, return 64-bit cycles.
   Uses TRAP #x0310; result is in VR (RAX)."
  (emit-ir :trap #x0310)
  (emit-ir :mov dest +vreg-vr+))

(defun compile-wfi (dest)
  "Compile (wfi) — wait for interrupt. CPU sleeps until next IRQ.
   On ARM32: WFI instruction. On other archs: NOP (TRAP falls through to SWI).
   Returns 0."
  (emit-ir :trap #x0304)
  (emit-ir :li dest 0))

(defun compile-setup-irq (dest)
  "Compile (setup-irq) - architecture-specific timer interrupt setup. Returns 0."
  (emit-ir :trap #x0320)
  (emit-ir :li dest 0))

(defun compile-timer-rearm (dest)
  "Compile (timer-rearm) - re-arm virtual timer for WFI wake. Returns 0."
  (emit-ir :trap #x0321)
  (emit-ir :li dest 0))

(defun compile-setup-nic-idt (dest)
  "Compile (setup-nic-idt) — install NIC IDT entry and unmask NIC IRQ.
   NIC IRQ number must be stored at [0x600024] before calling. Returns 0."
  (emit-ir :trap #x0322)
  (emit-ir :li dest 0))

(defun compile-nic-irq-unmask (dest)
  "Compile (nic-irq-unmask) — re-enable NIC IRQ in PIC after servicing. Returns 0."
  (emit-ir :trap #x0323)
  (emit-ir :li dest 0))

(defun compile-mmio-do-read32 (dest)
  "Compile (mmio-do-read32) — read 32-bit value from raw address at 0x600140,
   store result at 0x600148. Returns 0. Used for MMIO above 2GB on i386."
  (emit-ir :trap #x0330)
  (emit-ir :li dest 0))

(defun compile-mmio-do-write32 (dest)
  "Compile (mmio-do-write32) — write 32-bit value from 0x600148 to raw address
   at 0x600140. Returns 0. Used for MMIO above 2GB on i386."
  (emit-ir :trap #x0331)
  (emit-ir :li dest 0))

(defun compile-io-in-dword-raw (dest)
  "Compile (io-in-dword-raw) — read 32-bit I/O port (port in low 16 of [0x600140]),
   store raw result at 0x600148. Returns 0. Used for PCI reads with bit 31."
  (emit-ir :trap #x0332)
  (emit-ir :li dest 0))

(defun compile-pci-config-read-raw (addr-form env dest)
  "Compile (pci-config-read-raw ADDR) — native PCI config cycle.
   ADDR is PCI address without enable bit (fits in fixnum).
   Native code: untag, OR 0x80000000, outl 0xCF8, inl 0xCFC, store at 0x600148.
   Returns byte 0 of result."
  (compile-form addr-form env +vreg-v0+)
  (emit-ir :trap #x0333)
  (emit-ir :mov dest +vreg-vr+))

(defun compile-memory-barrier (dest)
  "Compile (memory-barrier) — full system DSB.
   On AArch64 without MMU, peripheral registers are Normal Non-cacheable,
   so writes to different 4KB pages can be reordered by the write buffer.
   This forces all pending writes to complete before proceeding."
  (emit-ir :trap #x0302)
  (emit-ir :li dest 0))

(defun compile-wbinvd (dest)
  "Compile (wbinvd) — flush all CPU caches (write-back, invalidate).
   Required on real x86 hardware so NIC DMA sees descriptor writes."
  (emit-ir :trap #x0334)
  (emit-ir :li dest 0))

(defun compile-sti (dest)
  "Compile (sti) - enable interrupts. Returns 0."
  (emit-ir :sti)
  (emit-ir :li dest 0))

(defun compile-cli (dest)
  "Compile (cli) - disable interrupts. Returns 0."
  (emit-ir :cli)
  (emit-ir :li dest 0))

(defun compile-sti-hlt (dest)
  "Compile (sti-hlt) - atomic STI+HLT. Returns 0."
  (emit-ir :sti)
  (emit-ir :halt)
  (emit-ir :li dest 0))

(defun compile-wrmsr (args env dest)
  "Compile (wrmsr ecx-val eax-val edx-val) - write to MSR.
   All args are tagged fixnums. Emits as a trap with args in V0-V2."
  (loop for arg in args
        for i from 0
        for areg = (+ +vreg-v0+ i)
        while (< i 3)
        do (compile-form arg env areg))
  ;; WRMSR is a privileged system operation, emit as trap
  (emit-ir :trap 1)  ; trap code 1 = WRMSR
  (emit-ir :li dest 0))

;;; ============================================================
;;; Per-CPU Data
;;; ============================================================

(defun compile-percpu-ref (offset-form env dest)
  "Compile (percpu-ref offset) - read per-CPU data.
   Offset must be a compile-time constant (embedded as imm16 in bytecode)."
  (unless (integerp offset-form)
    (error "MVM compiler: percpu-ref requires constant offset, got ~S" offset-form))
  ;; Read per-CPU slot with constant offset
  (emit-ir :percpu-ref dest offset-form))

(defun compile-percpu-set (offset-form val-form env dest)
  "Compile (percpu-set offset value) - write per-CPU data.
   Offset must be a compile-time constant."
  (unless (integerp offset-form)
    (error "MVM compiler: percpu-set requires constant offset, got ~S" offset-form))
  (compile-form val-form env dest)
  ;; Write per-CPU slot (value stays tagged)
  (emit-ir :percpu-set offset-form dest))

(defun compile-switch-idle-stack (dest)
  "Compile (switch-idle-stack) - switch to per-CPU idle stack. Returns 0."
  ;; This is implemented as a special percpu-ref that loads the stack pointer
  (emit-ir :trap #x0400)  ; trap code 0x400 = switch-idle-stack (above frame-enter range)
  (emit-ir :li dest 0))

(defun compile-jump-to-address (addr-form env dest)
  "Compile (jump-to-address addr) — untag fixnum, branch to it. Never returns."
  (compile-form addr-form env +vreg-v0+)
  (emit-ir :trap #x0303))

(defun compile-set-rsp (addr-form env dest)
  "Compile (set-rsp addr) - set stack pointer from tagged fixnum"
  (compile-form addr-form env dest)
  (emit-ir :shr dest dest +fixnum-shift+)
  ;; Move to stack pointer register
  (emit-ir :mov +vreg-vsp+ dest)
  (emit-ir :li dest 0))

(defun compile-lidt (addr-form env dest)
  "Compile (lidt addr) - load IDT register. Returns 0."
  (compile-form addr-form env dest)
  (emit-ir :shr dest dest +fixnum-shift+)
  (emit-ir :trap 3)  ; trap code 3 = LIDT
  (emit-ir :li dest 0))

;;; ============================================================
;;; Array Operations
;;; ============================================================

(defun compile-make-array (size-form env dest)
  "Compile (make-array size).
   Constant size <= 65535: ALLOC-OBJ with imm16 element count.
   Constant size > 65535 or variable size: ALLOC-ARRAY (register-based)."
  (if (and (integerp size-form) (<= size-form 65535))
      ;; Small constant size — emit ALLOC-OBJ directly
      ;; imm16 = element count, translator computes allocation size
      (emit-ir :alloc-obj dest size-form +subtag-array+)
      ;; Large constant or variable size — compile to register, ALLOC-ARRAY
      (progn
        (if (integerp size-form)
            ;; Large constant: load as immediate, already untagged
            (emit-ir :li dest size-form)
            ;; Variable: compile and untag
            (progn
              (compile-form size-form env dest)
              (emit-ir :sar dest dest +fixnum-shift+)))
        (emit-ir :alloc-array dest dest))))

(defun compile-aref (arr-form idx-form env dest)
  "Compile (aref array index).
   Constant index uses OBJ-REF; variable index uses AREF opcode."
  (if (integerp idx-form)
      ;; Constant index — use OBJ-REF
      (let ((arr-reg (alloc-temp-reg)))
        (compile-form arr-form env arr-reg)
        (emit-ir :obj-ref dest arr-reg idx-form)
        (free-temp-reg))
      ;; Variable index — use AREF opcode
      ;; AREF expects tagged fixnum index: SHL 2 with tagged gives real_idx*8
      (let ((arr-reg (alloc-temp-reg))
            (idx-reg (alloc-temp-reg)))
        (compile-form arr-form env arr-reg)
        (compile-form idx-form env idx-reg)
        (emit-ir :aref dest arr-reg idx-reg)
        (free-temp-reg)
        (free-temp-reg))))

(defun compile-aset (arr-form idx-form val-form env dest)
  "Compile (aset array index value).
   Constant index uses OBJ-SET; variable index uses ASET opcode."
  (if (integerp idx-form)
      ;; Constant index — use OBJ-SET
      (let ((arr-reg (alloc-temp-reg))
            (val-reg (alloc-temp-reg)))
        (compile-form arr-form env arr-reg)
        (compile-form val-form env val-reg)
        (emit-ir :obj-set arr-reg idx-form val-reg)
        (emit-ir :mov dest val-reg)
        (free-temp-reg)
        (free-temp-reg))
      ;; Variable index — use ASET opcode
      ;; ASET expects tagged fixnum index: SHL 2 with tagged gives real_idx*8
      ;; Use dedicated val-reg to avoid VR=scratch(RAX) clobber in translator
      (let ((arr-reg (alloc-temp-reg))
            (idx-reg (alloc-temp-reg))
            (val-reg (alloc-temp-reg)))
        (compile-form arr-form env arr-reg)
        (compile-form idx-form env idx-reg)
        (compile-form val-form env val-reg)
        (emit-ir :aset arr-reg idx-reg val-reg)
        (emit-ir :mov dest val-reg)
        (free-temp-reg)
        (free-temp-reg)
        (free-temp-reg))))

(defun compile-array-length (arr-form env dest)
  "Compile (array-length array). Extracts element count from header."
  (compile-form arr-form env dest)
  (emit-ir :array-len dest dest))

;;; ============================================================
;;; Function Call
;;; ============================================================

(defun compile-call (fn args env dest)
  "Compile a function call (fn arg1 arg2 ...).
   Register args are saved to the stack during evaluation to avoid
   exhausting temp registers when args contain nested function calls.
   Caller-saved temp registers (V5-V8) are saved/restored around the CALL
   to prevent clobbering live variables in those registers."
  (let ((nargs (length args))
        ;; Save the current temp count BEFORE arg evaluation.
        ;; V4 (RBX) is callee-saved, V9+ are spill slots (on stack, safe).
        ;; We need to save V5..V(4+save-count-1) where save-count is the
        ;; number of temps currently in use, but only those in V5-V8 range
        ;; (the caller-saved physical registers).
        (save-count (min *temp-reg-counter* 5)))  ; at most V4..V8 = 5 regs
    ;; Save caller-saved temp registers (V5 through V(4+save-count-1))
    ;; V4 (RBX) is callee-saved, so skip it — start from V5.
    ;; Skip dest register: it will be overwritten with the CALL result,
    ;; so restoring its old value would clobber the result.
    (when (> save-count 1)
      (loop for r from (+ +vreg-v4+ 1) below (+ +vreg-v4+ save-count)
            do (unless (= r dest)
                 (emit-ir :push r))))

    ;; Push overflow args FIRST (before populating V0-V3), because
    ;; evaluating overflow args may involve function calls that clobber V0-V3.
    ;; These end up deeper on the stack, which is correct: after CALL+frame-enter,
    ;; they'll be at [RBP+16+k*8] where the callee expects them.
    (when (> nargs +max-reg-args+)
      (dolist (arg (reverse (nthcdr +max-reg-args+ args)))
        (let ((temp (alloc-temp-reg)))
          (compile-form arg env temp)
          (emit-ir :push temp)
          (free-temp-reg))))

    (let ((reg-count (min nargs +max-reg-args+)))
      ;; Evaluate each register arg into a temp, push to stack, free temp.
      ;; This uses only 1 temp at a time, preventing temp exhaustion when
      ;; args are themselves function calls (which allocate their own temps).
      (dotimes (i reg-count)
        (let ((temp (alloc-temp-reg)))
          (compile-form (nth i args) env temp)
          (emit-ir :push temp)
          (free-temp-reg)))
      ;; Pop into arg registers (LIFO: last pushed = highest reg, pop first)
      (loop for i from (1- reg-count) downto 0
            do (emit-ir :pop (+ +vreg-v0+ i))))

    ;; Emit the call
    (cond
      ;; Direct call to named function
      ((symbolp fn)
       (let ((fn-name (symbol-name fn)))
         (emit-ir :call fn-name nargs)))
      ;; Other callable expression (rare in this system)
      (t
       (let ((fn-reg (alloc-temp-reg)))
         (compile-form fn env fn-reg)
         (emit-ir :call-indirect fn-reg nargs)
         (free-temp-reg))))

    ;; Result is in VR, move to dest
    (unless (= dest +vreg-vr+)
      (emit-ir :mov dest +vreg-vr+))

    ;; Clean up overflow stack args with POP (frame-free is NOP in translator)
    (when (> nargs +max-reg-args+)
      (let ((temp (alloc-temp-reg)))
        (dotimes (i (- nargs +max-reg-args+))
          (emit-ir :pop temp))
        (free-temp-reg)))

    ;; Restore caller-saved temp registers (reverse order, skip dest)
    (when (> save-count 1)
      (loop for r from (+ +vreg-v4+ save-count -1) downto (+ +vreg-v4+ 1)
            do (unless (= r dest)
                 (emit-ir :pop r))))))

;;; ============================================================
;;; Parameter List Preprocessing
;;; ============================================================
;;;
;;; Transforms &optional and &key parameter lists into simple required params
;;; with default value initialization code prepended to the body.

(defun preprocess-params (params body)
  "Transform a CL parameter list with &optional/&key into simple required params.
   Returns (cons new-params new-body)."
  (let ((mode :required)
        (required nil)
        (optional nil)
        (keys nil))
    ;; Parse parameter list
    (dolist (p params)
      (cond
        ((eq p '&optional) (setq mode :optional))
        ((eq p '&key)      (setq mode :key))
        ((eq p '&rest)     (setq mode :rest))
        ((eq p '&body)     (setq mode :rest))
        ((eq p '&allow-other-keys) nil) ; skip
        ((eq mode :required) (push p required))
        ((eq mode :optional)
         (if (consp p)
             (push (list (car p) (cadr p)) optional)
             (push (list p nil) optional)))
        ((eq mode :key)
         (if (consp p)
             (push (list (car p) (cadr p)) keys)
             (push (list p nil) keys)))
        ((eq mode :rest)
         ;; &rest param — just treat as regular param for now
         (push p required))))
    (setf required (nreverse required))
    (setf optional (nreverse optional))
    (setf keys (nreverse keys))
    ;; If no &optional or &key, return unchanged
    (if (and (null optional) (null keys))
        (cons params body)
        ;; Build new parameter list: required + optional param names
        (let ((new-params (append required (mapcar #'car optional)))
              (new-body body))
          ;; For &key params, add key params as regular params
          (when keys
            (dolist (k keys)
              (push (car k) new-params)))
          ;; Prepend default value checks for optional params
          (let ((defaults nil))
            (dolist (opt optional)
              (when (cadr opt)
                (push `(when (null ,(car opt))
                         (setq ,(car opt) ,(cadr opt)))
                      defaults)))
            (dolist (k keys)
              (when (cadr k)
                (push `(when (null ,(car k))
                         (setq ,(car k) ,(cadr k)))
                      defaults)))
            (when defaults
              (setf new-body (append (nreverse defaults) body))))
          (cons new-params new-body)))))

;;; ============================================================
;;; Phase 2.5: Internal Function Compilation
;;; ============================================================
;;;
;;; Compiles a single function's body, producing IR instructions.

(defun mvm-compile-function-internal (name params body &optional parent-env)
  "Compile a single function into IR. Returns function-info.
   Does NOT produce bytecode; that happens in phase 3.
   PARENT-ENV, if provided, allows closure variable references."
  (let* ((*ir-buffer* nil)
         (*current-function-name* (if (symbolp name) (symbol-name name)
                                      (string name)))
         (*temp-reg-counter* 0)
         (return-label (make-compiler-label))
         (*function-return-label* return-label))
    ;; Function prologue: push frame pointer, set up frame
    (emit-ir :frame-enter (length params))

    ;; Build initial environment with parameter bindings.
    ;; Parameters arrive in V0-V3 (for the first 4), rest on stack.
    ;; Register params must be saved to stack since they get clobbered
    ;; during function body execution.
    (let* ((nreg-params (min (length params) +max-reg-args+))
           (env (make-compile-env :stack-depth nreg-params
                                  :bindings nil
                                  :parent parent-env)))
      ;; Save register params to stack and build environment
      (loop for param in params
            for i from 0
            while (< i +max-reg-args+)
            for areg = (+ +vreg-v0+ i)
            do ;; Store arg register to stack slot
               (emit-ir :stack-store areg i)
               ;; Add binding to environment
               (push (make-binding :name param
                                    :location :stack
                                    :stack-slot i)
                     (compile-env-bindings env)))

      ;; Handle excess arguments (already on caller's stack)
      (loop for param in (nthcdr +max-reg-args+ params)
            for i from +max-reg-args+
            do (push (make-binding :name param
                                    :location :stack
                                    :stack-slot i)
                     (compile-env-bindings env))
               (setf (compile-env-stack-depth env) (1+ i)))

      ;; Compile body (strip any declarations), result goes to VR
      (compile-progn (strip-declares body) env +vreg-vr+))

    ;; Function return label (for early return via (return value))
    (emit-ir-label return-label)

    ;; Function epilogue
    (emit-ir :frame-leave)
    (emit-ir :ret)

    ;; Build function-info
    (let ((ir (get-ir-instructions)))
      ;; Store the IR on the function-info for later bytecode emission
      (let ((info (make-function-info
                    :name (if (symbolp name) (symbol-name name) (string name))
                    :param-count (length params)
                    :bytecode-offset 0
                    :bytecode-length 0
                    :stack-frame-size 0)))
        ;; Stash IR in the constant table temporarily (will be consumed by phase 3)
        ;; Actually, we return a pair: (info . ir)
        (cons info ir)))))

;;; ============================================================
;;; Phase 3: Bytecode Emission (MVM IR -> Bytecode)
;;; ============================================================
;;;
;;; Two-pass approach:
;;;   Pass 1: Measure instruction sizes, compute label positions
;;;   Pass 2: Emit bytecode with resolved branch offsets

;;; ------ Instruction Size Calculation ------

(defun ir-instruction-size (insn)
  "Return the encoded bytecode size (in bytes) for a single IR instruction"
  (let ((op (car insn)))
    (case op
      ;; Labels take 0 bytes
      (:label 0)

      ;; No-operand instructions: 1 byte opcode
      (:nop   1)
      (:ret   1)
      (:halt  1)
      (:fence 1)
      (:cli   1)
      (:sti   1)
      (:gc-check 1)
      (:yield 1)

      ;; 1-reg instructions: 1 opcode + 1 reg = 2 bytes
      (:push  2)
      (:pop   2)
      (:inc   2)
      (:dec   2)
      (:write-barrier 2)

      ;; 2-reg instructions: 1 opcode + 2 regs = 3 bytes
      (:mov   3)
      (:car   3)
      (:cdr   3)
      (:setcar 3)
      (:setcdr 3)
      (:consp 3)
      (:atom  3)
      (:neg   3)
      (:cmp   3)
      (:test  3)
      (:obj-tag 3)
      (:obj-subtag 3)
      (:array-len 3)
      (:alloc-array 3)  ;; 2-reg: 1 opcode + 2 regs = 3 bytes
      (:sap-new  3)    ;; 2-reg
      (:sap-addr 3)    ;; 2-reg

      ;; Object allocation: 1 opcode + 1 reg + 2 imm16 + 1 imm8 = 5 bytes
      (:alloc-obj 5)

      ;; Object slot access (immediate index): 1 opcode + 2 regs + 1 imm8 = 4 bytes
      (:obj-ref 4)
      (:obj-set 4)

      ;; 3-reg instructions: 1 opcode + 3 regs = 4 bytes
      (:aref  4)
      (:aset  4)
      (:add   4)
      (:sub   4)
      (:mul   4)
      (:mul26lo 4)
      (:mul26hi 4)
      (:mul64lo 4)
      (:mul64hi 4)
      (:acc128  4)
      (:sap-ref8  4)   ;; 3-reg
      (:sap-ref32 4)
      (:sap-ref64 4)
      (:sap-set8  4)
      (:sap-set32 4)
      (:sap-set64 4)
      (:div   4)
      (:mod   4)
      (:and   4)
      (:or    4)
      (:xor   4)
      (:cons  4)
      (:atomic-xchg 4)

      ;; reg + imm8 shift: 1 opcode + 2 regs + 1 imm8 = 4 bytes
      (:shl   4)
      (:shr   4)
      (:sar   4)

      ;; Variable shift: 2 regs + 1 reg (shift amount) = 4 bytes
      (:shl-var 4)
      (:sar-var 4)

      ;; Load immediate: 1 opcode + 1 reg + 8 imm64 = 10 bytes
      (:li    10)
      (:li-const 10)
      ;; li-func now emits FN-ADDR (1 opcode + 1 reg + 4 imm32 = 6 bytes)
      (:li-func 6)

      ;; Function address: 1 opcode + 1 reg + 4 imm32 = 6 bytes
      (:fn-addr 6)

      ;; Branch (unconditional): 1 opcode + 4 off32 = 5 bytes
      (:br    5)

      ;; Conditional branches: 1 opcode + 4 off32 = 5 bytes
      (:beq   5)
      (:bne   5)
      (:blt   5)
      (:bge   5)
      (:ble   5)
      (:bgt   5)

      ;; Branch-null: 1 opcode + 1 reg + 4 off32 = 6 bytes
      (:bnull  6)
      (:bnnull 6)

      ;; Call: 1 opcode + 4 imm32 = 5 bytes
      (:call  5)

      ;; Call indirect: 1 opcode + 1 reg = 2 bytes
      (:call-indirect 2)

      ;; Call native: 1 opcode + 1 reg = 2 bytes
      (:call-native 2)

      ;; Memory load/store: 1 opcode + 2 regs + 1 width = 4 bytes
      (:load  4)
      (:store 4)

      ;; I/O: 1 opcode + 1 reg + 2 port + 1 width = 5 bytes
      ;; For register-based port, use different encoding:
      (:io-read  5)
      (:io-write 5)

      ;; Per-CPU: 1 opcode + 1 reg + 2 imm16 = 4 bytes
      (:percpu-ref 4)
      (:percpu-set 4)

      ;; Frame: 1 opcode + 2 imm16 = 3 bytes
      (:frame-enter 3)
      (:frame-leave 1)
      (:frame-alloc 3)
      (:frame-free  3)

      ;; Stack load/store: 1 opcode + 1 reg + 2 imm16 = 4 bytes
      (:stack-load  4)
      (:stack-store 4)

      ;; Context save/restore: 1 opcode + 1 reg = 2 bytes
      (:save-ctx  2)
      (:restore-ctx 2)

      ;; Trap: 1 opcode + 2 imm16 = 3 bytes
      (:trap  3)

      ;; Default (unknown): assume 4 bytes
      (otherwise
       (warn "MVM compiler: unknown IR instruction ~A, assuming 4 bytes" op)
       4))))

;;; ------ Pass 1: Measure and Compute Label Positions ------

(defun compute-label-positions (ir-list)
  "Compute the byte offset for each label in the IR instruction list.
   Returns a hash table mapping label-id -> byte offset."
  (let ((labels (make-hash-table :test 'eql))
        (offset 0))
    (dolist (insn ir-list)
      (if (eq (car insn) :label)
          ;; Label: record position, takes 0 bytes
          (setf (gethash (second insn) labels) offset)
          ;; Instruction: advance offset
          (incf offset (ir-instruction-size insn))))
    labels))

;;; ------ Pass 2: Emit Bytecode ------

(defun emit-bytecode-for-ir (buf ir-list label-positions)
  "Emit MVM bytecode for a list of IR instructions.
   BUF is an mvm-buffer. LABEL-POSITIONS maps label-id -> byte offset."
  (let ((current-offset 0))
    (dolist (insn ir-list)
      (unless (eq (car insn) :label)
      (let ((op (car insn)))
        (case op
          ;; ---- No-operand instructions ----
          (:nop
           (mvm-nop buf))
          (:ret
           (mvm-ret buf))
          (:halt
           (mvm-halt buf))
          (:fence
           (mvm-fence buf))
          (:cli
           (mvm-cli buf))
          (:sti
           (mvm-sti buf))
          (:gc-check
           (mvm-gc-check buf))
          (:yield
           (mvm-yield buf))
          (:frame-leave
           ;; Frame teardown: the native backend handles restoring VSP/VFP.
           ;; In bytecode, emit as NOP (frame management is a higher-level concept).
           (mvm-nop buf))

          ;; ---- 1-reg instructions ----
          (:push
           (mvm-push buf (second insn)))
          (:pop
           (mvm-pop buf (second insn)))
          (:inc
           (mvm-inc buf (second insn)))
          (:dec
           (mvm-dec buf (second insn)))
          (:write-barrier
           (mvm-write-barrier buf (second insn)))

          ;; ---- 2-reg instructions ----
          (:mov
           (mvm-mov buf (second insn) (third insn)))
          (:car
           (mvm-car buf (second insn) (third insn)))
          (:cdr
           (mvm-cdr buf (second insn) (third insn)))
          (:setcar
           (mvm-setcar buf (second insn) (third insn)))
          (:setcdr
           (mvm-setcdr buf (second insn) (third insn)))
          (:consp
           (mvm-consp buf (second insn) (third insn)))
          (:atom
           (mvm-atom buf (second insn) (third insn)))
          (:neg
           (mvm-neg buf (second insn) (third insn)))
          (:cmp
           (mvm-cmp buf (second insn) (third insn)))
          (:test
           (mvm-test buf (second insn) (third insn)))
          (:obj-tag
           (mvm-obj-tag buf (second insn) (third insn)))
          (:obj-subtag
           (mvm-obj-subtag buf (second insn) (third insn)))
          (:array-len
           (mvm-array-len buf (second insn) (third insn)))
          (:alloc-array
           (mvm-alloc-array buf (second insn) (third insn)))

          ;; ---- SAP operations ----
          (:sap-new
           (mvm-sap-new buf (second insn) (third insn)))
          (:sap-addr
           (mvm-sap-addr buf (second insn) (third insn)))
          (:sap-ref8
           (mvm-sap-ref8 buf (second insn) (third insn) (fourth insn)))
          (:sap-ref32
           (mvm-sap-ref32 buf (second insn) (third insn) (fourth insn)))
          (:sap-ref64
           (mvm-sap-ref64 buf (second insn) (third insn) (fourth insn)))
          (:sap-set8
           (mvm-sap-set8 buf (second insn) (third insn) (fourth insn)))
          (:sap-set32
           (mvm-sap-set32 buf (second insn) (third insn) (fourth insn)))
          (:sap-set64
           (mvm-sap-set64 buf (second insn) (third insn) (fourth insn)))

          ;; ---- Object allocation and slot access ----
          (:alloc-obj
           ;; (alloc-obj dest size subtag)
           (mvm-alloc-obj buf (second insn) (third insn) (fourth insn)))
          (:obj-ref
           ;; (obj-ref dest obj idx)
           (mvm-obj-ref buf (second insn) (third insn) (fourth insn)))
          (:obj-set
           ;; (obj-set obj idx src)
           (mvm-obj-set buf (second insn) (third insn) (fourth insn)))

          ;; ---- Variable-index array access ----
          (:aref
           ;; (aref dest obj idx)
           (mvm-aref buf (second insn) (third insn) (fourth insn)))
          (:aset
           ;; (aset obj idx src)
           (mvm-aset buf (second insn) (third insn) (fourth insn)))

          ;; ---- 3-reg instructions ----
          (:add
           (mvm-add buf (second insn) (third insn) (fourth insn)))
          (:sub
           (mvm-sub buf (second insn) (third insn) (fourth insn)))
          (:mul
           (mvm-mul buf (second insn) (third insn) (fourth insn)))
          (:mul26lo
           (mvm-mul26lo buf (second insn) (third insn) (fourth insn)))
          (:mul26hi
           (mvm-mul26hi buf (second insn) (third insn) (fourth insn)))
          (:mul64lo
           (mvm-mul64lo buf (second insn) (third insn) (fourth insn)))
          (:mul64hi
           (mvm-mul64hi buf (second insn) (third insn) (fourth insn)))
          (:acc128
           (mvm-acc128 buf (second insn) (third insn) (fourth insn)))
          (:div
           (mvm-div buf (second insn) (third insn) (fourth insn)))
          (:mod
           (mvm-mod buf (second insn) (third insn) (fourth insn)))
          (:and
           (mvm-and buf (second insn) (third insn) (fourth insn)))
          (:or
           (mvm-or buf (second insn) (third insn) (fourth insn)))
          (:xor
           (mvm-xor buf (second insn) (third insn) (fourth insn)))
          (:cons
           (mvm-cons buf (second insn) (third insn) (fourth insn)))
          (:atomic-xchg
           (mvm-atomic-xchg buf (second insn) (third insn) (fourth insn)))

          ;; ---- Shift instructions (reg + reg + imm8) ----
          (:shl
           (mvm-shl buf (second insn) (third insn) (fourth insn)))
          (:shr
           (mvm-shr buf (second insn) (third insn) (fourth insn)))
          (:sar
           (mvm-sar buf (second insn) (third insn) (fourth insn)))

          ;; ---- Variable shift (reg + reg + reg) ----
          (:shl-var
           (mvm-shlv buf (second insn) (third insn) (fourth insn)))
          (:sar-var
           (mvm-sarv buf (second insn) (third insn) (fourth insn)))

          ;; ---- Load Immediate ----
          (:li
           (mvm-li buf (second insn) (third insn)))
          (:li-const
           ;; Load constant by index (placeholder, resolved during linking)
           (mvm-li buf (second insn) (third insn)))
          (:li-func
           ;; Load function address (resolved during translation to native)
           ;; Use FN-ADDR opcode so the translator can map bytecode offset
           ;; to native code address. Plain LI would load the bytecode offset
           ;; which is NOT a valid native address for CALL-IND.
           (let* ((fn-name (third insn))
                  (fn-info (gethash fn-name *functions*)))
             (mvm-fn-addr buf (second insn)
                          (if fn-info
                              (function-info-bytecode-offset fn-info)
                              0))))

          ;; ---- Branches (1 opcode + 4 off32 = 5 bytes) ----
          (:br
           (let* ((target-label (second insn))
                  (target-pos (gethash target-label label-positions))
                  (insn-end (+ current-offset 5))
                  (rel-offset (- target-pos insn-end)))
             (mvm-br buf rel-offset)))

          (:beq
           (let* ((target-label (second insn))
                  (target-pos (gethash target-label label-positions))
                  (insn-end (+ current-offset 5))
                  (rel-offset (- target-pos insn-end)))
             (mvm-beq buf rel-offset)))
          (:bne
           (let* ((target-label (second insn))
                  (target-pos (gethash target-label label-positions))
                  (insn-end (+ current-offset 5))
                  (rel-offset (- target-pos insn-end)))
             (mvm-bne buf rel-offset)))
          (:blt
           (let* ((target-label (second insn))
                  (target-pos (gethash target-label label-positions))
                  (insn-end (+ current-offset 5))
                  (rel-offset (- target-pos insn-end)))
             (mvm-blt buf rel-offset)))
          (:bge
           (let* ((target-label (second insn))
                  (target-pos (gethash target-label label-positions))
                  (insn-end (+ current-offset 5))
                  (rel-offset (- target-pos insn-end)))
             (mvm-bge buf rel-offset)))
          (:ble
           (let* ((target-label (second insn))
                  (target-pos (gethash target-label label-positions))
                  (insn-end (+ current-offset 5))
                  (rel-offset (- target-pos insn-end)))
             (mvm-ble buf rel-offset)))
          (:bgt
           (let* ((target-label (second insn))
                  (target-pos (gethash target-label label-positions))
                  (insn-end (+ current-offset 5))
                  (rel-offset (- target-pos insn-end)))
             (mvm-bgt buf rel-offset)))

          ;; ---- Branch-null (1 opcode + 1 reg + 4 off32 = 6 bytes) ----
          (:bnull
           (let* ((reg (second insn))
                  (target-label (third insn))
                  (target-pos (gethash target-label label-positions))
                  (insn-end (+ current-offset 6))
                  (rel-offset (- target-pos insn-end)))
             (mvm-bnull buf reg rel-offset)))
          (:bnnull
           (let* ((reg (second insn))
                  (target-label (third insn))
                  (target-pos (gethash target-label label-positions))
                  (insn-end (+ current-offset 6))
                  (rel-offset (- target-pos insn-end)))
             (mvm-bnnull buf reg rel-offset)))

          ;; ---- Call ----
          (:call
           (let* ((fn-name (second insn))
                  (fn-info (gethash fn-name *functions*))
                  (target (if fn-info
                              (function-info-bytecode-offset fn-info)
                              0)))
             (unless fn-info
               (format t "  WARN: unresolved CALL ~S~%" fn-name))
             (mvm-call buf target)))

          (:call-indirect
           (mvm-call-ind buf (second insn)))

          (:call-native
           ;; Emit as call-indirect
           (mvm-call-ind buf (second insn)))

          ;; ---- Function address ----
          (:fn-addr
           (let* ((dest-reg (second insn))
                  (fn-name (third insn))
                  (fn-info (gethash fn-name *functions*))
                  (target (if fn-info
                              (function-info-bytecode-offset fn-info)
                              0)))
             (mvm-fn-addr buf dest-reg target)))

          ;; ---- Memory ----
          (:load
           (mvm-load buf (second insn) (third insn) (fourth insn)))
          (:store
           (mvm-store buf (second insn) (third insn) (fourth insn)))

          ;; ---- I/O ----
          (:io-read
           ;; (io-read dest port-imm16 width)
           (mvm-io-read buf (second insn) (third insn) (fourth insn)))
          (:io-write
           ;; (io-write port-imm16 value-reg width)
           (mvm-io-write buf (second insn) (third insn) (fourth insn)))

          ;; ---- Per-CPU ----
          (:percpu-ref
           ;; (percpu-ref dest offset-imm16)
           (mvm-percpu-ref buf (second insn) (third insn)))
          (:percpu-set
           ;; (percpu-set offset-imm16 value-reg)
           (mvm-percpu-set buf (second insn) (third insn)))

          ;; ---- Frame management ----
          (:frame-enter
           ;; Emit as: push VFP; mov VFP, VSP; sub VSP, N*8
           ;; Encoded as trap with frame size
           (mvm-trap buf (second insn)))
          (:frame-alloc
           ;; sub VSP, N*8
           (mvm-trap buf (+ #x100 (second insn))))
          (:frame-free
           ;; add VSP, N*8
           (mvm-trap buf (+ #x200 (second insn))))

          ;; ---- Stack load/store ----
          (:stack-load
           ;; load dest from stack slot
           ;; Encoded as: load dest, VFP, slot-offset
           ;; Use obj-ref as a proxy: dest = [VFP + slot*8]
           (let ((dest-reg (second insn))
                 (slot (third insn)))
             (mvm-obj-ref buf dest-reg +vreg-vfp+ slot)))
          (:stack-store
           ;; store src to stack slot
           (let ((src-reg (second insn))
                 (slot (third insn)))
             (mvm-obj-set buf +vreg-vfp+ slot src-reg)))

          ;; ---- Context ----
          (:save-ctx
           (mvm-save-ctx buf (second insn)))
          (:restore-ctx
           (mvm-restore-ctx buf (second insn)))

          ;; ---- Trap ----
          (:trap
           (mvm-trap buf (second insn)))

          ;; ---- Unknown ----
          (otherwise
           (warn "MVM bytecode: unknown IR op ~A, emitting NOP" op)
           (mvm-nop buf))))

      ;; Advance offset
      (incf current-offset (ir-instruction-size insn))))))


;;; ============================================================
;;; Top-Level API
;;; ============================================================

(defun mvm-compile-function (name params body)
  "Compile a named function to MVM bytecode.
   Returns function-info with bytecode embedded in the module buffer."
  (let ((result (mvm-compile-function-internal name params body)))
    ;; result is (function-info . ir-list)
    (let ((info (car result))
          (ir (cdr result)))
      ;; Record source location
      (setf (function-info-source-location info) *current-source-location*)
      ;; Warn on redefinition with source locations
      (let ((existing (gethash (function-info-name info) *functions*)))
        (when existing
          (format t "  NOTE: redefining ~A  (old: ~A, new: ~A)~%"
                  (function-info-name info)
                  (or (function-info-source-location existing) "?")
                  (or *current-source-location* "?"))))
      ;; Register in function table
      (setf (gethash (function-info-name info) *functions*) info)
      (push info *function-table*)
      ;; Return the info and IR for later bytecode emission
      (cons info ir))))

(defun mvm-compile-toplevel (form)
  "Compile a top-level form.
   Handles defun, defvar, defconstant, defmacro, and bare expressions."
  ;; Macro-expand top-level forms first
  (let ((expanded (macroexpand-mvm form)))
    (unless (eq expanded form)
      (return-from mvm-compile-toplevel (mvm-compile-toplevel expanded))))
  (cond
    ;; (progn form*) at top level — process each sub-form
    ((and (consp form) (name-eq (car form) "PROGN"))
     (let ((last-result nil))
       (dolist (sub-form (cdr form))
         (let ((result (mvm-compile-toplevel sub-form)))
           (when result (setf last-result result))))
       last-result))

    ;; (defun name (params) body...)
    ((and (consp form) (name-eq (car form) "DEFUN"))
     (destructuring-bind (name params &body body) (cdr form)
       (let ((pp (preprocess-params params body)))
         (mvm-compile-function name (car pp) (cdr pp)))))

    ;; (defvar name &optional value)
    ((and (consp form) (name-eq (car form) "DEFVAR"))
     (let* ((name (cadr form))
            (value (caddr form))
            (name-hash (normalize-name name)))
       ;; Register as global variable
       (setf (gethash name-hash *globals*) t)
       ;; Compile as a thunk that initializes the variable
       ;; IMPORTANT: 1) Use raw name-hash (NOT pre-shifted), because
       ;; compile-integer will apply fixnum-shift. Pre-shifting causes
       ;; double-tagging: init stores at hash*4 but reads look up hash*2.
       ;; 2) Wrap value in let to avoid register clobber when value is
       ;; a function call (which would clobber V0 holding the hash).
       (when value
         (let ((tmp-var (gensym "INIT-TMP")))
           (mvm-compile-function
            (format nil "INIT-~A" (symbol-name name))
            nil
            (list `(let ((,tmp-var ,value))
                     (set-symbol-value ,name-hash ,tmp-var))))))))

    ;; (defparameter name value) — same as defvar
    ((and (consp form) (name-eq (car form) "DEFPARAMETER"))
     (let* ((name (cadr form))
            (value (caddr form))
            (name-hash (normalize-name name)))
       ;; Register as global variable
       (setf (gethash name-hash *globals*) t)
       (when value
         (let ((tmp-var (gensym "INIT-TMP")))
           (mvm-compile-function
            (format nil "INIT-~A" (symbol-name name))
            nil
            (list `(let ((,tmp-var ,value))
                     (set-symbol-value ,name-hash ,tmp-var))))))))

    ;; (defpackage ...) — skip, package system is SBCL-side only
    ((and (consp form) (name-eq (car form) "DEFPACKAGE"))
     nil)

    ;; (in-package ...) — skip, package system is SBCL-side only
    ((and (consp form) (name-eq (car form) "IN-PACKAGE"))
     nil)

    ;; (eval-when (situations...) body...) — compile body as top-level forms
    ;; MVM treats all compilation situations as :execute
    ((and (consp form) (name-eq (car form) "EVAL-WHEN"))
     (dolist (subform (cddr form))
       (mvm-compile-toplevel subform))
     nil)

    ;; (defconstant name value)
    ((and (consp form) (name-eq (car form) "DEFCONSTANT"))
     ;; Fold constants at compile time: evaluate the value and store.
     ;; Also define in host environment so dependent defconstants can eval.
     (let ((name (cadr form))
           (value-form (caddr form)))
       (when value-form
         (let ((value (eval value-form)))
           (setf (gethash (normalize-name name) *constants*) value)
           ;; Make available for subsequent eval calls (skip if already a constant)
           (when (and (symbolp name)
                      (not (constantp name)))
             (proclaim `(special ,name))
             (set name value)))))
     nil)

    ;; (defmacro name (params) body...)
    ((and (consp form) (name-eq (car form) "DEFMACRO"))
     (let ((name (cadr form))
           (params (caddr form))
           (body (cdddr form)))
       ;; Register macro expander
       ;; The expander is a host-side function (runs at compile time)
       (let ((expander (eval `(lambda (form)
                                 (destructuring-bind (,@params) (cdr form)
                                   ,@body)))))
         (mvm-define-macro (normalize-name name) expander))
       nil))

    ;; (defstruct name slot1 slot2 ...)
    ;; Generates constructor (make-name), accessors (name-slot), setters (set-name-slot)
    ((and (consp form) (name-eq (car form) "DEFSTRUCT"))
     (let* ((struct-name (cadr form))
            (struct-str (symbol-name struct-name))
            (raw-slots (cddr form))
            ;; Parse slot specs: plain symbol or (symbol default)
            (slot-names (mapcar (lambda (s)
                                  (if (consp s) (car s) s))
                                raw-slots))
            (slot-defaults (mapcar (lambda (s)
                                     (if (consp s) (cadr s) nil))
                                   raw-slots))
            (nslots (length slot-names))
            ;; Generate all defun forms
            (forms-to-compile nil))
       ;; Constructor: takes keyword-value pairs as flat args
       ;; e.g., (make-foo :bar 1 :baz 2) → called with args (:bar 1 :baz 2)
       ;; We generate a function that creates an array and fills slots
       ;; But since MVM doesn't support &rest, we generate a fixed-arity
       ;; constructor with (* 2 nslots) args (key1 val1 key2 val2 ...)
       (let ((ctor-name (format nil "MAKE-~A" struct-str))
             (internal-ctor-name (format nil "%MAKE-~A" struct-str))
             (ctor-params nil)
             (ctor-body nil))
         ;; Positional constructor with internal name to avoid macro recursion
         (setf ctor-params (loop for s in slot-names
                                 collect (intern (format nil "P-~A" (symbol-name s))
                                                 :modus.mvm)))
         (setf ctor-body
               `(let ((obj (make-array ,nslots)))
                  ,@(loop for i from 0
                          for p in ctor-params
                          collect `(aset obj ,i ,p))
                  obj))
         (push `(defun ,(intern internal-ctor-name :modus.mvm)
                    ,ctor-params
                  ,ctor-body)
               forms-to-compile)

         ;; Register macro that expands (make-name ...) to (%make-name ...)
         ;; with keyword args reordered to positional
         (let ((slot-kw-names (mapcar (lambda (s) (normalize-name s)) slot-names))
               (defaults slot-defaults)
               (internal-ctor-sym (intern internal-ctor-name :modus.mvm)))
           (mvm-define-macro ctor-name
             (lambda (form)
               (let ((args (cdr form))
                     (positional (make-list nslots :initial-element nil)))
                 ;; Fill defaults
                 (loop for i from 0 for d in defaults
                       do (setf (nth i positional) d))
                 ;; Parse keyword args
                 (loop while args
                       do (let ((key (car args))
                                (val (cadr args)))
                            (let ((idx (position (normalize-name key)
                                                 slot-kw-names
                                                 :test #'=)))
                              (when idx
                                (setf (nth idx positional) val)))
                            (setf args (cddr args))))
                 `(,internal-ctor-sym ,@positional))))))

       ;; Accessors
       (loop for slot in slot-names
             for i from 0
             do (let ((acc-name (format nil "~A-~A" struct-str (symbol-name slot))))
                  (push `(defun ,(intern acc-name :modus.mvm) (obj)
                           (aref obj ,i))
                        forms-to-compile)
                  ;; Register SETF handler for this accessor
                  (let ((setter-name (format nil "SET-~A-~A" struct-str (symbol-name slot))))
                    (push `(defun ,(intern setter-name :modus.mvm) (obj val)
                             (aset obj ,i val)
                             val)
                          forms-to-compile)
                    ;; Register setf macro: (setf (foo-bar x) v) → (set-foo-bar x v)
                    (let ((setter-sym (intern setter-name :modus.mvm)))
                      (let ((setf-key (compute-name-hash (format nil "SETF-~A" acc-name))))
                        (mvm-define-macro setf-key
                          (lambda (form)
                            (declare (ignore form))
                            nil))
                        ;; Add to SETF expansion table
                        (setf (gethash setf-key *macro-table*)
                              setter-sym))))))

       ;; Type predicate (name-p) — always returns t for any array (simple check)
       (let ((pred-name (format nil "~A-P" struct-str)))
         (push `(defun ,(intern pred-name :modus.mvm) (obj)
                  (arrayp obj))
               forms-to-compile))

       ;; Compile all generated forms and return ALL results
       (let ((results nil))
         (dolist (gen-form (nreverse forms-to-compile))
           (let ((result (mvm-compile-toplevel gen-form)))
             (when result (push result results))))
         ;; Return multi-result so mvm-compile-all collects all of them
         (cons :multi-result (nreverse results)))))

    ;; Other top-level forms: wrap in anonymous function
    (t
     (let ((thunk-name (format nil "TOPLEVEL-~D" (make-compiler-label))))
       (mvm-compile-function thunk-name nil (list form))))))

(defun mvm-compile-all (forms &key source-lines)
  "Compile a list of top-level forms into a complete MVM module.
   Returns a compiled-module containing bytecode, function table,
   and constant table.
   SOURCE-LINES: optional vector mapping form index to source line number."
  (let ((*functions* (make-hash-table :test 'equal))
        (*function-table* nil)
        (*constant-table* nil)
        (*label-counter* 0)
        (*macro-table* (make-hash-table :test 'eql))
        (*globals* (make-hash-table :test 'eql))
        (*constants* (make-hash-table :test 'eql))
        (*loop-exit-label* nil)
        (*block-labels* nil)
        (*tagbody-tags* nil)
        (*pending-flet-ir* nil)
        (all-ir nil))

    ;; Register standard macros (cond, and, or) for this compilation
    (register-mvm-bootstrap-macros)

    ;; Phase 1 & 2: Compile all forms to IR
    (let ((form-index 0))
    (dolist (form forms)
      (setf *pending-flet-ir* nil)
      (setf *current-source-location*
            (if (and source-lines (< form-index (length source-lines)))
                (format nil "line ~D" (aref source-lines form-index))
                (format nil "form#~D" form-index)))
      (incf form-index)
      (let* ((result (mvm-compile-toplevel form)))
        (cond
          ;; Multi-result from defstruct: collect all sub-results
          ((and (consp result) (eq (car result) :multi-result))
           (dolist (sub-result (cdr result))
             (let ((info (car sub-result))
                   (ir (cdr sub-result)))
               (when (and info ir)
                 (push (cons info ir) all-ir)))))
          ;; Single result
          (t
           (let ((info (car result))
                 (ir (cdr result)))
             (when (and info ir)
               (push (cons info ir) all-ir))))))
      ;; Drain any flet/labels IR collected during this form's compilation
      (dolist (flet-result *pending-flet-ir*)
        (let ((info (car flet-result))
              (ir (cdr flet-result)))
          (when (and info ir)
            (push (cons info ir) all-ir))))))

    ;; Reverse to get compilation order
    (setf all-ir (nreverse all-ir))

    ;; Auto-generate init-all-globals: calls every INIT-* thunk
    (let ((init-calls nil))
      (dolist (entry all-ir)
        (let ((name (function-info-name (car entry))))
          (when (and (stringp name)
                     (>= (length name) 5)
                     (string= name "INIT-" :end1 5))
            (format t "  init thunk: ~A~%" name)
            (push (list (intern name :modus.mvm)) init-calls))))
      (when init-calls
        (let* ((result (mvm-compile-toplevel
                         `(defun init-all-globals ()
                            ,@(nreverse init-calls))))
               (info (car result))
               (ir (cdr result)))
          (when (and info ir)
            (setf all-ir (nconc all-ir (list (cons info ir))))))))

    ;; Phase 3: Emit bytecode
    (let ((buf (make-mvm-buffer)))
      ;; First pass: compute label positions for each function and assign
      ;; bytecode offsets
      (let ((global-offset 0))
        (dolist (entry all-ir)
          (let* ((info (car entry))
                 (ir (cdr entry))
                 (label-positions (compute-label-positions ir))
                 ;; Compute total size of this function's bytecode
                 (fn-size (loop for insn in ir
                                sum (ir-instruction-size insn))))
            ;; Record bytecode offset and length
            (setf (function-info-bytecode-offset info) global-offset)
            (setf (function-info-bytecode-length info) fn-size)
            ;; Update function in hash table so calls can resolve
            (setf (gethash (function-info-name info) *functions*) info)
            (incf global-offset fn-size))))

      ;; Debug: show bytecode offsets for key functions
      (dolist (name '("ED-SCALAR-MULT" "C64-ED-SCALAR-MULT" "ED-BASE-MULT"
                      "ED-ADD" "ED-DOUBLE" "USB-KEEPALIVE" "ED25519-SIGN-FAST"))
        (let ((fi (gethash name *functions*)))
          (when fi
            (format t "  FN ~A: offset=~D len=~D (~A)~%"
                    name (function-info-bytecode-offset fi)
                    (function-info-bytecode-length fi)
                    (or (function-info-source-location fi) "?")))))

      ;; Second pass: emit bytecode with resolved offsets
      ;; Note: label-positions are LOCAL to each function (starting at 0).
      ;; This matches current-offset in emit-bytecode-for-ir which also
      ;; starts at 0, so branch offsets are computed correctly.
      ;; CALL targets use global offsets from *functions*, not label-positions.
      (dolist (entry all-ir)
        (let* ((ir (cdr entry))
               (label-positions (compute-label-positions ir)))
          (emit-bytecode-for-ir buf ir label-positions)))

      ;; Build module
      (make-compiled-module
       :bytecode (mvm-buffer-used-bytes buf)
       :function-table (nreverse *function-table*)
       :constant-table (nreverse *constant-table*)))))

;;; ============================================================
;;; Disassembly / Debug Support
;;; ============================================================

(defun disassemble-module (module)
  "Print a human-readable disassembly of a compiled MVM module"
  (format t "~&=== MVM Module ===~%")
  (format t "Bytecode size: ~D bytes~%" (length (compiled-module-bytecode module)))
  (format t "Functions: ~D~%" (length (compiled-module-function-table module)))
  (format t "Constants: ~D~%~%" (length (compiled-module-constant-table module)))

  ;; Print function table
  (dolist (fn (compiled-module-function-table module))
    (format t "Function ~A (~D params) @ offset ~D (~D bytes):~%"
            (function-info-name fn)
            (function-info-param-count fn)
            (function-info-bytecode-offset fn)
            (function-info-bytecode-length fn))
    ;; Disassemble this function's bytecode
    (disassemble-mvm (compiled-module-bytecode module)
                     :start (function-info-bytecode-offset fn)
                     :end (+ (function-info-bytecode-offset fn)
                             (function-info-bytecode-length fn)))
    (format t "~%"))

  ;; Print constant table
  (when (compiled-module-constant-table module)
    (format t "Constants:~%")
    (loop for const in (compiled-module-constant-table module)
          for i from 0
          do (format t "  [~D] ~S~%" i const))))

(defun dump-ir (ir-list)
  "Print IR instructions in a human-readable format for debugging"
  (dolist (insn ir-list)
    (if (eq (car insn) :label)
        (format t "L~D:~%" (second insn))
        (format t "  ~{~A~^ ~}~%" insn))))

;;; ============================================================
;;; Testing
;;; ============================================================

(defun test-mvm-compiler ()
  "Basic test of the MVM compiler"
  ;; Test 1: Simple function
  (format t "~%=== Test 1: (defun add1 (x) (1+ x)) ===~%")
  (let ((module (mvm-compile-all
                 '((defun add1 (x) (1+ x))))))
    (disassemble-module module))

  ;; Test 2: Fibonacci
  (format t "~%=== Test 2: fibonacci ===~%")
  (let ((module (mvm-compile-all
                 '((defun fib (n)
                     (if (< n 2)
                         n
                         (+ (fib (1- n))
                            (fib (- n 2)))))))))
    (disassemble-module module))

  ;; Test 3: List manipulation
  (format t "~%=== Test 3: list operations ===~%")
  (let ((module (mvm-compile-all
                 '((defun list-length (lst)
                     (let ((count 0))
                       (loop
                         (if (null lst)
                             (return count)
                             (progn
                               (setq count (1+ count))
                               (setq lst (cdr lst)))))))))))
    (disassemble-module module))

  ;; Test 4: Multiple functions
  (format t "~%=== Test 4: multiple functions ===~%")
  (let ((module (mvm-compile-all
                 '((defun double (x) (* x 2))
                   (defun quadruple (x) (double (double x)))))))
    (disassemble-module module))

  (format t "~%All MVM compiler tests passed.~%")
  t)
