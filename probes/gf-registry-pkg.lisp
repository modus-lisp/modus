;;; Probe: is the CLOS generic-function registry package-blind?
(defpackage "PGA" (:use "CL"))
(defpackage "PGB" (:use "CL"))

(format t "~%=== D1: same name, DIFFERENT lambda lists (congruence) ===~%")
(format t "d1a defgeneric pga::gfx (a b c d) -> ~a~%"
        (handler-case (progn (eval '(defgeneric pga::gfx (a b c d))) :ok)
          (t (c) (list :err (type-of c)))))
(format t "d1b defmethod  pga::gfx 4 args    -> ~a~%"
        (handler-case (progn (eval '(defmethod pga::gfx ((a t) (b t) (c t) (d t)) :pga)) :ok)
          (t (c) (list :err (type-of c)))))
(format t "d1c defgeneric pgb::gfx (a)       -> ~a~%"
        (handler-case (progn (eval '(defgeneric pgb::gfx (a))) :ok)
          (t (c) (list :err (type-of c)))))
(format t "d1d defmethod  pgb::gfx 1 arg     -> ~a~%"
        (handler-case (progn (eval '(defmethod pgb::gfx ((a t)) :pgb)) :ok)
          (t (c) (list :err (type-of c)))))

(format t "~%=== D2: identity of the two GF objects ===~%")
(let ((ga (%find-gf 'pga::gfx))
      (gb (%find-gf 'pgb::gfx)))
  (format t "d2a (eq 'pga::gfx 'pgb::gfx) -> ~a  [want NIL]~%" (eq 'pga::gfx 'pgb::gfx))
  (format t "d2b gfa non-nil ~a  gfb non-nil ~a~%" (not (null ga)) (not (null gb)))
  (format t "d2c (eq gfa gfb) -> ~a  [want NIL]~%" (eq ga gb))
  (format t "d2d gfa lambda-list ~a~%" (and ga (%gf-lambda-list ga)))
  (format t "d2e gfb lambda-list ~a  [want (A)]~%" (and gb (%gf-lambda-list gb)))
  (format t "d2f gfa nmethods ~a  gfb nmethods ~a  [want 1 and 1]~%"
          (and ga (length (%gf-methods ga))) (and gb (length (%gf-methods gb)))))

(format t "~%=== D3: MIS-DISPATCH direction (same arity, two packages) ===~%")
(handler-case (progn (eval '(defgeneric pga::gfy (x))) (eval '(defmethod pga::gfy ((x integer)) :from-pga)) :ok)
  (t (c) (format t "d3-setupA err ~a~%" (type-of c))))
(handler-case (progn (eval '(defgeneric pgb::gfy (x))) (eval '(defmethod pgb::gfy ((x integer)) :from-pgb)) :ok)
  (t (c) (format t "d3-setupB err ~a~%" (type-of c))))
(format t "d3a (pga::gfy 1) -> ~a  [want :FROM-PGA]~%"
        (handler-case (eval '(pga::gfy 1)) (t (c) (list :err (type-of c)))))
(format t "d3b (pgb::gfy 1) -> ~a  [want :FROM-PGB]~%"
        (handler-case (eval '(pgb::gfy 1)) (t (c) (list :err (type-of c)))))
(let ((ga (%find-gf 'pga::gfy)) (gb (%find-gf 'pgb::gfy)))
  (format t "d3c (eq gfa gfb) -> ~a  [want NIL]~%" (eq ga gb))
  (format t "d3d nmethods a=~a b=~a  [want 1 1]~%"
          (and ga (length (%gf-methods ga))) (and gb (length (%gf-methods gb)))))

(format t "~%=== D4: gray-streams shape (defmethod on a NEW pkg's name, existing 4-arg GF elsewhere) ===~%")
(handler-case (progn (eval '(defgeneric pga::srs (s seq start end))) :ok)
  (t (c) (format t "d4-setup err ~a~%" (type-of c))))
(format t "d4a defmethod pgb::srs with 2 specializers -> ~a  [want :OK]~%"
        (handler-case (progn (eval '(defmethod pgb::srs ((s t) (seq t)) :pgb-srs)) :ok)
          (t (c) (list :err (type-of c)))))
(format t "d4b (pgb::srs 1 2) -> ~a  [want :PGB-SRS]~%"
        (handler-case (eval '(pgb::srs 1 2)) (t (c) (list :err (type-of c)))))

(format t "~%PROBE-GF-DONE~%")
