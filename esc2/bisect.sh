#!/bin/bash
# usage: bisect.sh <startline> <endline>
SC=/tmp/claude-1002/-home-claude-modus/9e874ffc-83a3-493c-9640-e84a344b598b/scratchpad
S=$1; E=$2
sed -n "${S},${E}p" $SC/it/iterate-release-d27d7ff4-git/iterate.lisp > /home/claude/ws-iter/esc2/slice.lisp
cat > /home/claude/ws-iter/esc2/dbis.lisp <<'PYEOF'
(defun say (k v) (princ (concatenate (quote string) k "=" (princ-to-string v))) (terpri) (finish-output))
(defun ldf (path pkgname)
  (let ((p *package*) (r nil))
    (setq *package* (find-package pkgname))
    (setq r (handler-case (%it-eval-source (tar-bytes-to-string (%it-slurp-bytes path)) path)
              (t (c) (list :ERR c))))
    (setq *package* p) r))
(say "L1" (ldf "/home/claude/ws-iter/esc2/itpkg.lisp" "COMMON-LISP-USER"))
(say "L2" (ldf "/home/claude/ws-iter/esc2/slice.lisp" "ITERATE"))
(say "FB" (handler-case (eval (read-from-string "(if (fboundp 'iterate::sharpL-reader) 1 0)")) (t (c) (list :ERR c))))
(say "X.nobang" (handler-case (eval (read-from-string "(funcall (function iterate::sharpL-reader) (make-string-input-stream \"(list 1)\") #\\L nil)")) (t (c) (list :ERR c))))
(say "X.bang"   (handler-case (eval (read-from-string "(funcall (function iterate::sharpL-reader) (make-string-input-stream \"(list iterate::!1)\") #\\L nil)")) (t (c) (list :ERR c))))
(say "END" "ok")
PYEOF
timeout 600 /home/claude/ws-iter/modus --load /home/claude/ws-iter/esc2/dbis.lisp --quit 2>&1 | grep -aE "^(L1|L2|FB|X|END)"
