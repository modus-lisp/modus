;;;; runtime-backquote.lisp — the LOAD-TIME backquote expander, shared by every
;;;; hosted Modus image.
;;;;
;;;; Backquote inside a macro body is handled at BUILD time by compiler.lisp,
;;;; so a baked `(defmacro m (x) `(+ ,x 1))' just works.  A macro defined at
;;;; RUNTIME — which is what every real library does, and what alexandria is
;;;; almost entirely made of (with-gensyms, once-only, if-let, when-let,
;;;; switch/eswitch, define-constant) — instead reaches EVAL with the reader's
;;;; (BACKQUOTE template) marker still in the body.  Without the expander below
;;;; that marker's COMMA sub-markers survive into the expansion and the first
;;;; call signals UNDEFINED-FUNCTION NAME="COMMA".
;;;;
;;;; %install-runtime-backquote registers the expander as BACKQUOTE's macro
;;;; function; call it from kernel-main.  Requires: set-macro-function, mapcar,
;;;; symbol-name, string=, append2.
;;;;
;;;; Extracted verbatim from mvm/build-generic-cli.lisp's *driver-source* (WS5
;;;; #203, 2026-07-31) so the aarch64 and x64 hosted CLIs share ONE copy
;;;; instead of each carrying the text inside its own build wrapper.  It is
;;;; concatenated INTO each build's driver source, so the existing driver
;;;; defun-name / symbol-name scanners still see every defun here.
;;;; (mvm/build-generic.lisp, build-modus-selfhost.lisp and build-i386-cli.lisp
;;;; still carry their own copies; folding those in is follow-up work.)

(defun %rbq-sym-name-eq (sym name)
  (and (symbolp sym) (string= (symbol-name sym) name)))
;; UNQUOTE*-equivalent: a nested splice `,,@x / `,,.x evaluates X in the OUTER
;; pass and must present X's value to the INNER (pass-2) backquote as spliceable
;; template elements.  Wrap each element in a fresh COMMA marker so pass-2 re-
;; processes it (a template element re-expands; a plain datum self-quotes).  This
;; is what makes alexandria once-only's ``(,,g ,,(cdr n)) inside `(let (,,@...))
;; collapse correctly instead of leaving a raw (BACKQUOTE ...) as a LET binding.
(defun %rbq-splice-commas (lst)
  (mapcar (lambda (%sc-e) (list 'comma %sc-e)) lst))
;; Level-tracking backquote expander.  LEVEL counts open backquotes
;; whose commas are still pending; the entry from the macro is LEVEL 1.
;; A COMMA at LEVEL 1 unquotes (its expr stays live); a COMMA at deeper
;; LEVEL is data that drops one level — this is what makes the classic
;; `,',x / `,,x tunnelling work (a `(... `(... ,',def ...)) form).  A
;; nested BACKQUOTE bumps the level by one for its template.
(defun runtime-bq-expand (template) (%rbq template 1))
(defun %rbq (template level)
  (cond
    ((null template) nil)
    ((atom template) (list 'quote template))
    ;; nested COMMA
    ((%rbq-sym-name-eq (car template) "COMMA")
     (if (= level 1)
         (cadr template)
         (list 'list (list 'quote 'comma) (%rbq (cadr template) (- level 1)))))
    ((%rbq-sym-name-eq (car template) "COMMA-AT")
     (if (= level 1)
         (cadr template)
         (list 'list (list 'quote 'comma-at) (%rbq (cadr template) (- level 1)))))
    ((%rbq-sym-name-eq (car template) "COMMA-DOT")
     (if (= level 1)
         (cadr template)
         (list 'list (list 'quote 'comma-dot) (%rbq (cadr template) (- level 1)))))
    ;; nested BACKQUOTE — descend one deeper level, rebuild the marker
    ((%rbq-sym-name-eq (car template) "BACKQUOTE")
     (list 'list (list 'quote 'backquote) (%rbq (cadr template) (+ level 1))))
    (t (%rbq-list template level))))
(defun %rbq-list (lst level)
  (cond
    ((null lst) (list 'quote nil))
    ((not (consp lst)) (%rbq lst level))
    ;; a dotted/atom whole-form COMMA tail like `(a . ,b)
    ((%rbq-sym-name-eq (car lst) "COMMA")
     (if (= level 1)
         (cadr lst)
         (%rbq lst level)))
    (t
     (let ((first (car lst)) (rest (cdr lst)))
       (cond
         ((and (consp first) (%rbq-sym-name-eq (car first) "COMMA-AT") (= level 1))
          (list 'append2 (cadr first) (%rbq-list rest level)))
         ((and (consp first) (%rbq-sym-name-eq (car first) "COMMA-DOT") (= level 1))
          (list 'append2 (cadr first) (%rbq-list rest level)))
         ;; nested splice `,,@E / `,,.E (outer COMMA + inner COMMA-AT/DOT) at
         ;; level 2: outer comma is active THIS pass, so evaluate E now, then
         ;; hand each element to pass-2 wrapped in a COMMA marker (%rbq-splice-
         ;; commas) so pass-2 re-processes it.  Splicing raw would leave un-
         ;; expanded (BACKQUOTE ...) elements (once-only's LET-binding bug).
         ((and (consp first) (= level 2)
               (%rbq-sym-name-eq (car first) "COMMA")
               (consp (cadr first))
               (%rbq-sym-name-eq (car (cadr first)) "COMMA-AT"))
          (list 'append2 (list '%rbq-splice-commas (cadr (cadr first)))
                (%rbq-list rest level)))
         ((and (consp first) (= level 2)
               (%rbq-sym-name-eq (car first) "COMMA")
               (consp (cadr first))
               (%rbq-sym-name-eq (car (cadr first)) "COMMA-DOT"))
          (list 'append2 (list '%rbq-splice-commas (cadr (cadr first)))
                (%rbq-list rest level)))
         (t
          (list 'cons (%rbq first level)
                (%rbq-list rest level))))))))
;; COMPILE-TIME closure, NOT (eval '(lambda …)): this install runs at boot in
;; kernel-main, and under WS3 Phase-3 production EVAL is mvm-eval — a boot-time
;; mvm-eval of the lambda runs before init-all-globals (mvm-eval's state defvars all
;; NIL) and silently produced a broken expander (callee resolution fell into
;; the :li-func offset-0 fallback), so every runtime defmacro with a backquote
;; body "expanded" to its raw (BACKQUOTE …) template — uiop define-package
;; became a silent no-op and the asdf gauntlet died at the first read-time #.
;; that depended on an earlier defparameter.  The historical eval-based install
;; only worked because boot-time eval used to be the tree-walker (flag NIL).
;; Convention: non-interp-closure *macro-function-table* entries are funcalled
;; with the WHOLE form — (cadr mform) is the template.
(defun %install-runtime-backquote ()
  (set-macro-function 'backquote
                      (function (lambda (mform) (runtime-bq-expand (cadr mform))))))
