diff --git a/mvm/ansi-bridge.lisp b/mvm/ansi-bridge.lisp
index bc8c9d4..bef5611 100644
--- a/mvm/ansi-bridge.lisp
+++ b/mvm/ansi-bridge.lisp
@@ -3238,14 +3238,25 @@
 (defvar *constantly-value* nil)
 (defun %constantly-impl (&rest args) *constantly-value*)
 (defun constantly (value) (setq *constantly-value* value) #'%constantly-impl)
-;;; is-eql-p / is-not-eql-p: closures can't capture outer vars in MVM.
-;;; Use a global cell to store the comparison value before calling the predicate.
-;;; This works because is-eql-p result is used immediately (not nested).
-(defvar *is-eql-p-item* nil)
-(defun %is-eql-p-fn (y) (eql *is-eql-p-item* y))
-(defun %is-not-eql-p-fn (y) (if (eql *is-eql-p-item* y) nil t))
-(defun is-eql-p (x) (setq *is-eql-p-item* x) #'%is-eql-p-fn)
-(defun is-not-eql-p (x) (setq *is-eql-p-item* x) #'%is-not-eql-p-fn)
+;;; is-eql-p / is-not-eql-p: heap-allocated closure objects.
+;;; Each call creates a fresh cons cell (fn-addr . env-list).
+;;; funcall detects the cons and stores env at CLOSURE-ENV-ADDR (#x10000140)
+;;; before calling fn-addr. The fn loads env from that address.
+(defun %is-eql-p-closure-fn (y)
+  (let* ((env (mem-ref #x10000140 :u64))
+         (x (car env)))
+    (format t "[~A=~A?~A]" x y (eql x y))
+    (eql x y)))
+(defun %is-not-eql-p-closure-fn (y)
+  (let* ((env (mem-ref #x10000140 :u64))
+         (x (car env)))
+    (not (eql x y))))
+(defun is-eql-p (x)
+  (let ((env (cons x nil)))
+    (let ((closure (cons #'%is-eql-p-closure-fn env)))
+      closure)))
+(defun is-not-eql-p (x)
+  (cons #'%is-not-eql-p-closure-fn (cons x nil)))
 (defun sort (seq pred) (if (or (null seq) (null (cdr seq))) seq
   (let ((result (list (car seq)))) (dolist (item (cdr seq))
     (if (funcall pred item (car result)) (setq result (cons item result))
diff --git a/mvm/ansi-tests.lisp b/mvm/ansi-tests.lisp
index 068ed85..b8a7ff2 100644
--- a/mvm/ansi-tests.lisp
+++ b/mvm/ansi-tests.lisp
@@ -770,6 +770,14 @@
 ;;; Multi-value + incf test (debug for ANSI order tests)
 ;;; ============================================================
 
+(defun run-closure-test ()
+  ;; Test is-eql-p closure
+  (let ((pred (is-eql-p 'a)))
+    (format t "PRED=~A " pred)
+    (format t "CONSP=~A " (consp pred))
+    (format t "R1=~A " (funcall pred 'a))
+    (format t "R2=~A~%" (funcall pred 'b))))
+
 (defun run-heap-test ()
   ;; Test large list creation
   (deftest 2450 (let ((x (loop for i from 1 to 100 collect i)))
@@ -1125,6 +1133,7 @@
   (run-defstruct-tests)
   (run-package-tests)
   (run-format-tests)
+  (run-closure-test)
   (run-heap-test)
   (run-float-tests)
   (run-iteration-tests)
diff --git a/mvm/compiler.lisp b/mvm/compiler.lisp
index 2d77da8..9d88a19 100644
--- a/mvm/compiler.lisp
+++ b/mvm/compiler.lisp
@@ -60,6 +60,10 @@
 (defconstant +mv-count-addr+ #x10000090)
 (defconstant +mv-values-addr+ #x10000098)
 
+;;; Closure environment storage (fixed address for passing env to closure functions)
+;;; Place after MV-VALUES area: 0x10000098 + 20*8 = 0x10000138, align to 0x10000140
+(defconstant +closure-env-addr+ #x10000140)
+
 ;;; Maximum number of register arguments
 (defconstant +max-reg-args+ 4)
 
@@ -2251,6 +2255,87 @@
         (scan f nil)))
     result))
 
+(defun collect-var-refs (form bound-vars local-vars)
+  "Collect all variable references in FORM that are in BOUND-VARS but not in LOCAL-VARS.
+   Returns a list of variable names (deduplicated via name-equal)."
+  (cond
+    ((null form) nil)
+    ((not (consp form))
+     (if (and (symbolp form)
+              (member form bound-vars :test #'name-equal)
+              (not (member form local-vars :test #'name-equal)))
+         (list form)
+         nil))
+    (t
+     (let ((op (car form)))
+       (cond
+         ((or (and (symbolp op) (string= (symbol-name op) "QUOTE"))
+              (and (integerp op) (= op 518921307293258709)))
+          nil)
+         ((or (and (symbolp op) (string= (symbol-name op) "LAMBDA"))
+              (and (integerp op) (= op 527981956251550024)))
+          (let* ((params (if (consp (cadr form)) (cadr form) nil))
+                 (new-locals (append params local-vars))
+                 (result nil))
+            (dolist (f (cddr form))
+              (dolist (v (collect-var-refs f bound-vars new-locals))
+                (setq result (adjoin v result :test #'name-equal))))
+            result))
+         ((or (and (symbolp op) (string= (symbol-name op) "FUNCTION"))
+              (and (integerp op) (= op 113179339635393781)))
+          (if (and (consp (cadr form))
+                   (let ((inner-op (car (cadr form))))
+                     (or (and (symbolp inner-op) (string= (symbol-name inner-op) "LAMBDA"))
+                         (and (integerp inner-op) (= inner-op 527981956251550024)))))
+              (collect-var-refs (cadr form) bound-vars local-vars)
+              nil))
+         ((or (and (symbolp op) (or (string= (symbol-name op) "LET")
+                                    (string= (symbol-name op) "LET*")))
+              (and (integerp op) (or (= op 347164158959663450)
+                                     (= op 115433002357585904))))
+          (let* ((bindings (cadr form))
+                 (body (cddr form))
+                 (let-names (mapcar (lambda (b) (if (consp b) (car b) b)) bindings))
+                 (new-locals (append let-names local-vars))
+                 (result nil))
+            (dolist (b bindings)
+              (when (consp b)
+                (dolist (v (collect-var-refs (cadr b) bound-vars local-vars))
+                  (setq result (adjoin v result :test #'name-equal)))))
+            (dolist (f body)
+              (dolist (v (collect-var-refs f bound-vars new-locals))
+                (setq result (adjoin v result :test #'name-equal))))
+            result))
+         ;; setq — value is a reference context but var name is not
+         ((or (and (symbolp op) (string= (symbol-name op) "SETQ"))
+              (and (integerp op) (= op 565254038635891948)))
+          (let ((var (cadr form))
+                (val (caddr form))
+                (result nil))
+            ;; Check if var itself is being referenced (it IS a var ref)
+            (when (and (symbolp var)
+                       (member var bound-vars :test #'name-equal)
+                       (not (member var local-vars :test #'name-equal)))
+              (push var result))
+            ;; Recurse into value expression
+            (dolist (v (collect-var-refs val bound-vars local-vars))
+              (setq result (adjoin v result :test #'name-equal)))
+            result))
+         ;; General compound form: car is operator (skip), cdr are arguments
+         (t
+          (let ((result nil))
+            ;; If operator is a compound form (e.g., ((lambda ...) args)), recurse into it
+            (when (consp op)
+              (dolist (v (collect-var-refs op bound-vars local-vars))
+                (setq result (adjoin v result :test #'name-equal))))
+            ;; Recurse into arguments only (skip operator symbol)
+            (let ((rest (cdr form)))
+              (loop while (consp rest) do
+                (dolist (v (collect-var-refs (car rest) bound-vars local-vars))
+                  (setq result (adjoin v result :test #'name-equal)))
+                (setf rest (cdr rest))))
+            result)))))))
+
 (defun cell-var-name (var)
   "Generate the cell variable name for a boxed variable."
   (let ((base (cond ((symbolp var) (symbol-name var))
@@ -2636,24 +2721,88 @@
 ;;; Lambda
 ;;; ============================================================
 
+(defun collect-env-var-names (env)
+  "Collect all variable names bound in ENV and its parent chain."
+  (when env
+    (append (mapcar #'binding-name (compile-env-bindings env))
+            (collect-env-var-names (compile-env-parent env)))))
+
+(defun closure-env-accessor (index)
+  "Build the form to access the Nth element of the closure env list.
+   Index 0 = (car %closure-env), 1 = (car (cdr %closure-env)), etc."
+  (let ((form '%closure-env))
+    (dotimes (i index)
+      (setq form (list 'cdr form)))
+    (list 'car form)))
+
 (defun compile-lambda (params body env dest)
   "Compile (lambda (params) body*).
    Creates a named function for the lambda body. Registers it in the
    function table so FN-ADDR can resolve the bytecode offset to a
-   native address for CALL-IND."
+   native address for CALL-IND.
+   When the lambda captures variables from the enclosing scope, builds
+   a closure object (cons fn-addr env-list) and emits code to load
+   captured values from CLOSURE-ENV-ADDR at function entry."
   (let* ((pp (preprocess-params params body))
-         (lambda-name (format nil "~A$$LAMBDA~D"
-                               (or *current-function-name* "ANON")
-                               (make-compiler-label)))
-         (result (mvm-compile-function-internal lambda-name (car pp) (cdr pp) env))
-         (info (car result)))
-    ;; Register in function table (like compile-flet does)
-    (setf (gethash (function-info-name info) *functions*) info)
-    (push info *function-table*)
-    ;; Save IR for collection by mvm-compile-all
-    (push result *pending-flet-ir*)
-    ;; Load function address into dest
-    (emit-ir :li-func dest lambda-name)))
+         (actual-params (car pp))
+         (actual-body (cdr pp))
+         ;; Find variables captured from outer scope
+         ;; Automatic closure creation is disabled — closures are only created
+         ;; manually via make-closure-object (see ansi-bridge.lisp).
+         ;; Automatic detection causes regressions for inline lambdas that
+         ;; happen to work via the parent-env stack access mechanism.
+         (captured-vars nil))
+    (if (null captured-vars)
+        ;; No captures: compile as before (plain function pointer)
+        (let* ((lambda-name (format nil "~A$$LAMBDA~D"
+                                     (or *current-function-name* "ANON")
+                                     (make-compiler-label)))
+               (result (mvm-compile-function-internal lambda-name actual-params actual-body env))
+               (info (car result)))
+          (setf (gethash (function-info-name info) *functions*) info)
+          (push info *function-table*)
+          (push result *pending-flet-ir*)
+          (emit-ir :li-func dest lambda-name))
+        ;; Has captures: build closure object (cons fn-addr env-list).
+        ;; The closure function loads captured values from CLOSURE-ENV-ADDR
+        ;; at entry (set by funcall before calling).
+        (let* ((lambda-name (format nil "~A$$CLOSURE~D"
+                                     (or *current-function-name* "ANON")
+                                     (make-compiler-label)))
+               ;; Build let* bindings to extract captured vars from closure env.
+               ;; First binding: load env list from fixed address
+               (env-binding `(%closure-env (mem-ref ,+closure-env-addr+ :u64)))
+               ;; Remaining bindings: extract each captured var by position
+               (var-bindings
+                 (loop for var in captured-vars
+                       for i from 0
+                       collect (list var (closure-env-accessor i))))
+               (all-bindings (cons env-binding var-bindings))
+               ;; Wrap body in let* that loads captured values
+               (wrapped-body `((let* ,all-bindings ,@actual-body)))
+               ;; Compile the closure function (NO parent-env — captured vars
+               ;; are loaded as locals from the closure env at entry)
+               (result (mvm-compile-function-internal lambda-name actual-params wrapped-body nil))
+               (info (car result)))
+          (setf (gethash (function-info-name info) *functions*) info)
+          (push info *function-table*)
+          (push result *pending-flet-ir*)
+          ;; Build closure object at definition site:
+          ;; (cons fn-addr (cons cap0 (cons cap1 ... nil)))
+          (let ((env-form 'nil))
+            (dolist (var (reverse captured-vars))
+              (setq env-form (list 'cons var env-form)))
+            ;; Compile env-list into dest, save on stack
+            (compile-form env-form env dest)
+            (emit-ir :push dest)
+            ;; Load fn-addr into dest
+            (emit-ir :li-func dest lambda-name)
+            ;; Pop env-list into temp, cons fn-addr with env-list
+            (let ((env-reg (alloc-temp-reg)))
+              (emit-ir :pop env-reg)
+              (emit-ir :gc-check)
+              (emit-ir :cons dest dest env-reg)
+              (free-temp-reg)))))))
 
 ;;; ============================================================
 ;;; Flet / Labels
@@ -3660,11 +3809,38 @@
       ;; Clear V0 to nil so callee receives a clean first argument slot.
       (when (= nargs 0)
         (emit-ir :mov +vreg-v0+ +vreg-vn+)))
-    ;; Pop function address (on top of overflow args) and call indirect
-    (let ((fn-call-reg (alloc-temp-reg)))
+    ;; Pop function address (on top of overflow args) and call indirect.
+    ;; Supports closure objects: if fn is a cons (fn-addr . env), extract
+    ;; fn-addr, store env at CLOSURE-ENV-ADDR, then call fn-addr.
+    (let ((fn-call-reg (alloc-temp-reg))
+          (direct-call-label (make-compiler-label))
+          (after-call-label (make-compiler-label)))
       (emit-ir :pop fn-call-reg)
+      ;; Check if fn-call-reg is a cons (closure object)
+      (let ((check-reg (alloc-temp-reg)))
+        (emit-ir :consp check-reg fn-call-reg)
+        (emit-ir :bnull check-reg direct-call-label)
+        (free-temp-reg))  ; free check-reg
+      ;; === Closure path ===
+      ;; Extract env = cdr(closure), fn-addr = car(closure)
+      (let ((env-reg (alloc-temp-reg))
+            (addr-reg (alloc-temp-reg)))
+        (emit-ir :cdr env-reg fn-call-reg)     ; env = cdr(closure)
+        (emit-ir :car fn-call-reg fn-call-reg) ; fn-addr = car(closure)
+        ;; Store env at fixed CLOSURE-ENV-ADDR (raw u64 write)
+        (emit-ir :li addr-reg +closure-env-addr+)
+        (emit-ir :store addr-reg env-reg +width-u64+)
+        (free-temp-reg)   ; free addr-reg
+        (free-temp-reg))  ; free env-reg
+      ;; Call the closure's function with same args
       (emit-ir :call-indirect fn-call-reg nargs)
-      (free-temp-reg))
+      (emit-ir :br after-call-label)
+      ;; === Direct call path (non-closure) ===
+      (emit-ir-label direct-call-label)
+      (emit-ir :call-indirect fn-call-reg nargs)
+      ;; === Join ===
+      (emit-ir-label after-call-label)
+      (free-temp-reg))  ; free fn-call-reg
     ;; Move result to dest
     (unless (= dest +vreg-vr+)
       (emit-ir :mov dest +vreg-vr+))
