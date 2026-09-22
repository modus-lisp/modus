;;; pkgindex-probe.lisp -- FIND-PACKAGE's contract, with the three cases the
;;; index's verification guard exists for: the CL-TEST alias, a DELETEd
;;; package, and a RENAMEd one.  Run on the pre-change binary, on the indexed
;;; one, and on SBCL (all but the CL-TEST line, which is a modus alias).
(defun p (l v) (format t "~a ~s~%" l v))
(defun nm (x) (and x (package-name x)))

;; primary names, nicknames, case folding, designators
(p "cl"        (nm (find-package "COMMON-LISP")))
(p "cl-nick"   (nm (find-package "CL")))
(p "lisp-nick" (nm (find-package "LISP")))
(p "cluser"    (nm (find-package "COMMON-LISP-USER")))
(p "cluser-n"  (nm (find-package "CL-USER")))
(p "kw"        (nm (find-package "KEYWORD")))
(p "lower"     (nm (find-package "common-lisp")))
(p "mixed"     (nm (find-package "Cl-UsEr")))
(p "sym"       (nm (find-package 'cl-user)))
(p "kwdesig"   (nm (find-package :keyword)))
(p "char"      (nm (find-package #\A)))
(p "obj"       (nm (find-package (find-package "CL"))))
(p "missing"   (nm (find-package "NO-SUCH-PKG-QQQ")))
(p "empty"     (nm (find-package "")))

;; a package made at runtime: found by name and by nickname
(let ((q (make-package "PIX-NEW" :nicknames (list "PIX-N1" "PIX-N2") :use nil)))
  (p "new"      (nm (find-package "PIX-NEW")))
  (p "new-n1"   (nm (find-package "PIX-N1")))
  (p "new-n2"   (nm (find-package "PIX-N2")))
  (p "new-low"  (nm (find-package "pix-n1")))
  (p "new-eq"   (eq q (find-package "PIX-NEW"))))

;; RENAME: the old name must stop resolving, the new one must start
(let ((q (find-package "PIX-NEW")))
  (rename-package q "PIX-RENAMED" (list "PIX-R1"))
  (p "ren-old"   (nm (find-package "PIX-NEW")))
  (p "ren-oldn"  (nm (find-package "PIX-N1")))
  (p "ren-new"   (nm (find-package "PIX-RENAMED")))
  (p "ren-newn"  (nm (find-package "PIX-R1")))
  (p "ren-eq"    (eq q (find-package "PIX-RENAMED"))))

;; DELETE: neither the name nor any nickname may resolve afterwards
(let ((q (make-package "PIX-DOOMED" :nicknames (list "PIX-D1") :use nil)))
  (p "del-pre"   (nm (find-package "PIX-DOOMED")))
  (p "del-pren"  (nm (find-package "PIX-D1")))
  (delete-package q)
  (p "del-post"  (nm (find-package "PIX-DOOMED")))
  (p "del-postn" (nm (find-package "PIX-D1")))
  (p "del-again" (find-package "PIX-DOOMED")))

;; the CL-TEST alias (modus-specific: resolves to CL-USER without being a
;; nickname of it) -- the index must NOT answer this; FIND-PACKAGE does
(p "cl-test"     (nm (find-package "CL-TEST")))
(p "clu-nicks"   (package-nicknames (find-package "COMMON-LISP-USER")))

;; things built on find-package
(p "intern-hit"  (multiple-value-list (intern "CAR" "COMMON-LISP")))
(p "find-sym"    (multiple-value-list (find-symbol "CAR" "CL")))
(p "in-pkg"      (nm (let ((*package* (find-package "KEYWORD"))) *package*)))
(p "pkg-use"     (mapcar #'package-name (package-use-list (find-package "CL-USER"))))
(p "all-count"   (numberp (length (list-all-packages))))
(p "done" t)
