;;;; eval2.lisp — in-image runtime evaluator: compile a list of top-level
;;;; forms to ONE MVM bytecode module and execute the trailing expression via
;;;; mvm-interpret.  Extracted verbatim from build-generic.lisp's
;;;; *stage2-test-source* so the same source feeds BOTH the generic oracle
;;;; binary and the ANSI image (WS3: self-host the compiler + retire the
;;;; tree-walker).  Depends on mvm.lisp (ISA), interp.lisp (mvm-interpret),
;;;; and compiler.lisp (mvm-compile-toplevel / emit pipeline) being loaded
;;;; earlier in the image source.
;; eval2-forms: compile a LIST of top-level forms (helper defuns followed by a
;; trailing expression) into ONE bytecode module and run the expression.  CALLs
;; between the functions resolve in-module (bytecode->bytecode) so NO native
;; bridge / value marshalling is needed — one representation throughout.  This
;; is the 'drop native' model: the interpreter runs everything as bytecode.
(defun eval2-forms (forms)
  ;; In-image: emit integer literals as fixnum-safe :li-halves (set the GLOBAL,
  ;; not a let-binding — compiled LET of a special may not establish a dynamic
  ;; binding the compiler's compile-integer reads).  Native builds never call
  ;; eval2-forms, so the global stays NIL there.
  (setq *mvm-emit-halves* t)
  (let ((*functions* (make-hash-table :test (quote equal)))
        (*function-table* nil)
        (*constant-table* nil)
        (*label-counter* 0)
        (*unresolved-calls* (make-hash-table :test (quote equal)))
        (*macro-table* (make-hash-table :test (quote eql)))
        (*globals* (make-hash-table :test (quote eql)))
        (*constants* (make-hash-table :test (quote eql)))
        (*loop-exit-label* nil)
        (*block-labels* nil)
        (*tagbody-tags* nil)
        (*pending-flet-ir* nil)
        (*init-thunk-names* nil)
        (all-ir nil)
        (buf nil)
        (global-offset 0)
        (entry nil)
        (rt-table (make-hash-table))
        (rt-next #x40000000))
    (register-mvm-bootstrap-macros)
    ;; Split: last form is the expression; preceding forms are definitions.
    (let* ((rforms (reverse forms))
           (expr (car rforms))
           (defs (reverse (cdr rforms)))
           (toplevel (append defs (list (list (quote defun) (quote %eval2-thunk) nil expr)))))
      (dolist (f toplevel)
        (let ((result (mvm-compile-toplevel f)))
          (cond
            ((null result) nil)
            ((and (consp result) (eq (car result) :multi-result))
             (dolist (sub (cdr result))
               (when (and (car sub) (cdr sub))
                 (setq all-ir (cons (cons (car sub) (cdr sub)) all-ir)))))
            (t (when (and (car result) (cdr result))
                 (setq all-ir (cons (cons (car result) (cdr result)) all-ir)))))))
      ;; Drain *pending-flet-ir*: captureless lambdas, flet/labels bodies, and
      ;; nested defuns register their (info . ir) here instead of returning it
      ;; from mvm-compile-toplevel.  Without emitting these, a `#'(lambda ...)`
      ;; (funcall lambda) would resolve its li-func to an UN-OFFSET function and
      ;; jump to bytecode 0 → infinite recursion.  Append so they get Pass-1
      ;; offsets and Pass-2 bytecode like any other function.
      (dolist (pend *pending-flet-ir*)
        (when (and (consp pend) (car pend) (cdr pend))
          (setq all-ir (cons pend all-ir))))
      (setq all-ir (reverse all-ir)))
    ;; Small buffer (the default 128MB byte array blows the in-image heap).
    (setq buf (make-mvm-buffer :bytes (make-array 65536)))
    ;; Pass 1: size each function, assign cumulative bytecode offsets, register
    ;; in *functions* so emit-time CALL resolution finds them.
    (dolist (e all-ir)
      (let* ((info (car e)) (ir (cdr e))
             (fn-size (let ((s 0)) (dolist (insn ir) (setq s (+ s (ir-instruction-size insn)))) s)))
        (setf (function-info-bytecode-offset info) global-offset)
        (setf (function-info-bytecode-length info) fn-size)
        (setf (gethash (function-info-name info) *functions*) info)
        (setq global-offset (+ global-offset fn-size))))
    ;; Pass 1.5: any CALL to a name NOT defined in this module is a RUNTIME call
    ;; (a native function in the image).  Register a synthetic stub at a high
    ;; offset so emit resolves the CALL there; the interpreter's runtime-table
    ;; maps that offset back to the name and funcalls the native fn (WS1 bridge).
    ;; LI-FUNC (#'NAME / (function NAME)) to an out-of-module name is also a
    ;; runtime reference: the FN-ADDR opcode reads function-info-bytecode-offset,
    ;; so registering the name at a runtime stub offset lets the interp's
    ;; op-FN-ADDR map that offset back to the name and load the REAL native
    ;; function OBJECT.  The subsequent CALL-INDIRECT then bridge-calls it —
    ;; this is the higher-order eval2 path (funcall/apply/mapcar #'NAME).
    (dolist (e all-ir)
      (dolist (insn (cdr e))
        ;; The fn NAME is operand 1 for :call ((:call name nargs)) but operand 2
        ;; for :li-func ((:li-func dest name)) — pick the right slot per op.
        (let ((name (cond ((eq (car insn) :call) (cadr insn))
                          ((eq (car insn) :li-func) (caddr insn))
                          (t nil))))
          (when (and name (stringp name) (not (gethash name *functions*)))
            (let ((info (make-function-info :name name :bytecode-offset rt-next
                                            :bytecode-length 0)))
              (setf (gethash name *functions*) info)
              (setf (gethash rt-next rt-table) name)
              (setq rt-next (+ rt-next 1)))))))
    ;; Pass 2: emit (CALLs resolve to in-module OR synthetic offsets via *functions*).
    (dolist (e all-ir)
      (let* ((ir (cdr e)) (lp (compute-label-positions ir)))
        (emit-bytecode-for-ir buf ir lp)))
    (dolist (e all-ir)
      (when (string-equal (string (function-info-name (car e))) \"%EVAL2-THUNK\")
        (setq entry (function-info-bytecode-offset (car e)))))
    (let ((bc (mvm-buffer-used-bytes buf)))
      (if entry
          (let ((fn-table (make-array (length all-ir))) (i 0)
                ;; LAMBDA-OFFSETS: the bytecode entry offsets of in-module LAMBDA /
                ;; CLOSURE bodies (named *$$LAMBDA* / *$$CLOSURE* by compile-lambda).
                ;; The interp's native-bridge uses this to recognise an eval2 lambda
                ;; VALUE escaping to a native higher-order fn (mapcar/reduce/…) and
                ;; wrap it in a re-entrant trampoline.  Keyed by offset; ONLY genuine
                ;; lambda bodies are recorded (never the %eval2-thunk / helper defuns
                ;; / the fn at offset 0), so an ordinary fixnum DATA argument — a loop
                ;; counter 0/1/2, an index — is never mistaken for a callable.
                (lam-offsets (make-hash-table)))
            (dolist (e all-ir)
              (aset fn-table i (function-info-bytecode-offset (car e)))
              (setq i (+ i 1))
              (let ((nm (string (function-info-name (car e)))))
                (when (or (search \"$$LAMBDA\" nm) (search \"$$CLOSURE\" nm))
                  (puthash (function-info-bytecode-offset (car e)) lam-offsets t))))
            (handler-case (mvm-interpret bc :entry-point entry :function-table fn-table
                                         :runtime-table rt-table :return-raw nil
                                         :lambda-offsets lam-offsets)
              (error (e) (list :interp-err e))))
          :no-entry))))
;; Single-expression convenience.
(defun eval2 (form) (eval2-forms (list form)))
