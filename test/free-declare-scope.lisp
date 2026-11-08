;;;; free-declare-scope.lisp — a free type declaration scopes over its form, and a
;;;; local macro shadows a global setf expander.
;;;;
;;;;   ./modus --script test/free-declare-scope.lisp
;;;;
;;;; 1. (locally (declare (type (simple-array (unsigned-byte 8) (*)) d)) ...) in one
;;;;    branch of an IF used to stamp the type on D's OWN binding, so the other branch
;;;;    compiled its AREF as a simple-array access too.  Unmasked when TYPEP stopped
;;;;    calling a fill-pointered vector simple (bf8745f3): cram's ADLER-STEP then read
;;;;    an adjustable vector's header as data, and every ZRLE frame glass sent failed
;;;;    ("The value 7 is not of type (INTEGER 0 6)").
;;;; 2. SETF of a place whose operator a MACROLET binds expands the local macro, even
;;;;    when a DEFSETF of that name exists (CLHS 5.1.2.7).  SB-ALIEN's WITH-ALIEN DEREF
;;;;    of an (ARRAY UNSIGNED-SHORT 4) relies on it.

(defvar *fail* 0)
(defun chk (name got want)
  (if (equal got want)
      (format t "  ok   ~A = ~S~%" name got)
      (progn (setq *fail* (+ *fail* 1)) (format t "  FAIL ~A: got ~S want ~S~%" name got want))))

(defun branchy (data i)
  (if (typep data '(simple-array (unsigned-byte 8) (*)))
      (locally (declare (type (simple-array (unsigned-byte 8) (*)) data)
                        (optimize (speed 3) (safety 0)))
        (list :simple (aref data i)))
      (list :general (aref data i))))

(defvar *adj* (make-array 10 :element-type '(unsigned-byte 8) :adjustable t
                             :fill-pointer 10 :initial-element 5))
(defvar *simple* (make-array 10 :element-type '(unsigned-byte 8) :initial-element 9))
(chk "the general branch reads an adjustable vector" (branchy *adj* 7) '(:general 5))
(chk "the declared branch reads a simple one" (branchy *simple* 7) '(:simple 9))

(defun after-locally (data)
  (locally (declare (type (simple-array (unsigned-byte 8) (*)) data)) (aref data 0))
  (aref data 3))
(chk "a declaration ends with its LOCALLY" (after-locally *adj*) 5)

(defvar *v* (make-array 4 :initial-element 0))
(defun gl (i) (aref *v* i))
(defsetf gl (i) (x) `(setf (aref *v* (+ 2 ,i)) ,x))
(defun shadowed (v)
  (macrolet ((gl (i) (list 'aref 'v i))) (setf (gl 0) 50 (gl 1) 70)) v)
(chk "SETF of a MACROLET name expands the local macro"
     (coerce (shadowed (make-array 2 :initial-element 0)) 'list) '(50 70))
(chk "the global DEFSETF is untouched" (progn (setf (gl 0) 1) (coerce *v* 'list)) '(0 0 1 0))

(format t "~%FREE-DECLARE-SCOPE: ~:[FAIL~;PASS~]~%" (zerop *fail*))
